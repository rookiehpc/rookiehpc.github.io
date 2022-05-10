!> @brief Illustrates how to resize an MPI datatype.
!> @details This program is meant to be run with 3 processes. The MPI process 0
!> holds a 2D array of 12 integers 4 rows times 3 columns, and scatters columns
!> across all three MPI processes as illustrated below:
!>
!>  Elements of a row
!> <----------------->
!> +-----+-----+-----+ ^
!> |  0  |  1  |  2  | |
!> +-----+-----+-----+ |
!> |  3  |  4  |  5  | | Elements of a column
!> +-----+-----+-----+ | 
!> |  6  |  7  |  8  | |
!> +-----+-----+-----+ |
!> |  9  |  10 |  11 | |
!> +-----+-----+-----+ v
!>
!> MPI process 0     MPI process 1     MPI process 2
!>    +-----+           +-----+           +-----+
!>    |  0  |           |  1  |           |  2  |
!>    +-----+           +-----+           +-----+
!>    |  3  |           |  4  |           |  5  |
!>    +-----+           +-----+           +-----+
!>    |  6  |           |  7  |           |  8  |
!>    +-----+           +-----+           +-----+
!>    |  9  |           |  10 |           |  11 |
!>    +-----+           +-----+           +-----+
!>
!> The global array held on MPI process 0 has been declared such that elements
!> of the same row are contiguous in memory, in other words, here is the memory
!> layout of this global array:
!>
!> +-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+
!> |  0  |  1  |  2  |  3  |  4  |  5  |  6  |  7  |  8  |  9  |  10 |  11 |
!> +-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+
!>
!> However the objective here is to scatter columns across the 3 MPI processes.
!> If we identify columns with letters, here is how they are scattered in
!> memory:
!>
!> +-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+
!> |  A  |  B  |  C  |  A  |  B  |  C  |  A  |  B  |  C  |  A  |  B  |  C  |
!> +-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+
!>
!> To extract a column, we need the following vector:
!> +-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+
!> |  A  |  B  |  C  |  A  |  B  |  C  |  A  |  B  |  C  |  A  |  B  |  C  |
!> +-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+
!> <----->           <----->           <----->           <----->
!> 1st block        2nd block         3rd block        4th block
!>      
!> ^                  <--------------->                        ^
!> | Start of          Stride: distance                 End of |
!> | 1st block       between the start of           last block |
!> |                two consecutive blocks                     |
!> |                                                           |
!> <----------------------------------------------------------->
!>          Extent, as calculated by MPI_Type_vector
!>
!> 
!> When a vector is repeated, as part of a MPI collective for instance, the
!> vector extent is used to determine the start position of the next vector. 
!> This vector extent is calculated by MPI_Type_vector as the distance between
!> the start of the 1st block and the end of the last block. This means that the
!> second vector begins only after the last block of the first vector. Although
!> this makes sense when vectors are made of contiguous data, in our case it
!> does not generate the repeating pattern we want:
!>
!> +-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+
!> |  A  |  B  |  C  |  A  |  B  |  C  |  A  |  B  |  C  |  A  |  B  |  C  |
!> +-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+
!> ^                                                           ^
!> |                                                           |
!> +-----------------------------------------------------------+
!>                 First vector effectively                    ^
!>             extracting all cells of column A                |
!>                                                             +---------... 
!>                                                              Second vector,
!>                                                         supposed to extract
!>                                                       all cells of column B
!>
!> Not only did the second vector missed some of the cells of column B, but
!> having started much further away in the buffer than it was meant to, it is
!> likely to result in a segmentation fault, similarly for C. In our case, we
!> want an vectors to have an interleaved pattern and begin after the first
!> block of the previous vector, as follows:
!> 
!> +-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+
!> |  A  |  B  |  C  |  A  |  B  |  C  |  A  |  B  |  C  |  A  |  B  |  C  |
!> +-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+
!> ^     ^     ^
!> |     |     |
!> |     |     | Start of 3rd vector
!> |     |
!> |     | Start of 2nd vector
!> |      
!> | Start of 1st vector
!>
!> We therefore need to modify the vector extent accordingly and set it to the
!> size of 1 block, this is where we need the MPI_Type_create_resized routine.
PROGRAM main
    USE mpi

    IMPLICIT NONE

    INTEGER :: ierror
    INTEGER :: size
    ! Number of cells per column.
    INTEGER, PARAMETER :: CELLS_PER_COLUMN = 4
    ! Number of cells per row.
    INTEGER, PARAMETER :: CELLS_PER_ROW = 3
    INTEGER :: column_not_resized
    INTEGER :: column_resized
    INTEGER :: my_rank
    INTEGER, DIMENSION(0:CELLS_PER_COLUMN-1) :: my_column
    INTEGER, DIMENSION(0:CELLS_PER_ROW-1, 0:CELLS_PER_COLUMN-1) :: full_array
    INTEGER :: i
    INTEGER :: j
    INTEGER(KIND=MPI_ADDRESS_KIND) :: lower_bound
    INTEGER(KIND=MPI_ADDRESS_KIND) :: extent

    CALL MPI_Init(ierror)

    ! Get the number of processes and check only 3 processes are used
    CALL MPI_Comm_size(MPI_COMM_WORLD, size, ierror)
    IF (size .NE. 3) THEN
        WRITE(*, '(A)') 'This application is meant to be run with 3 processes.'
        CALL MPI_Abort(MPI_COMM_WORLD, -1, ierror)
    END IF

    ! Create the vector datatype
    CALL MPI_Type_vector(CELLS_PER_COLUMN, 1, CELLS_PER_ROW, MPI_INTEGER, column_not_resized, ierror)

    ! Resize it to make sure it is interleaved when repeated
    lower_bound = 0
    extent = SIZEOF(i)
    CALL MPI_Type_create_resized(column_not_resized, lower_bound, extent, column_resized, ierror)
    CALL MPI_Type_commit(column_resized, ierror)

    ! Get my rank and do the corresponding job
    CALL MPI_Comm_rank(MPI_COMM_WORLD, my_rank, ierror)
    IF (my_rank == 0) THEN
        ! Declare and initialise the full array
        DO i = 0, CELLS_PER_ROW - 1
            DO j = 0, CELLS_PER_COLUMN - 1
                full_array(i,j) = j * CELLS_PER_ROW + i
            END DO
        END DO

        ! Send the column
        CALL MPI_Scatter(full_array, 1, column_resized, my_column, CELLS_PER_COLUMN, MPI_INTEGER, 0, MPI_COMM_WORLD, ierror)
    ELSE
        ! Receive the column
        CALL MPI_Scatter(full_array, 1, column_resized, my_column, CELLS_PER_COLUMN, MPI_INTEGER, 0, MPI_COMM_WORLD, ierror)
    END IF

    WRITE(*, '(A,I0,A,I0,A,I0,A,I0,A,I0)') 'MPI process ', my_rank, ' received column made of cells ', &
          my_column(0), ', ', my_column(1), ', ', my_column(2), ', ', my_column(3)

CALL     MPI_Finalize(ierror)
END PROGRAM main