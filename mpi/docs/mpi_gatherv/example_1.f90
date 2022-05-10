!> @brief Illustrates how to use the variable version of a gather.
!> @details Every MPI process begins with a value, the MPI process 0 will gather
!> all these values and print them. The example is designed to cover all cases:
!> - Different displacements
!> - Different receive counts
!> It can be visualised as follows:
!> This application is meant to be run with 3 processes.
!>
!> +-----------+ +-----------+ +-------------------+ 
!> | Process 0 | | Process 1 | |     Process 2     |
!> +-+-------+-+ +-+-------+-+ +-+-------+-------+-+
!>   | Value |     | Value |     | Value | Value |
!>   |  100  |     |  101  |     |  102  |  103  |
!>   +-------+     +-------+     +-------+-------+
!>      |                |            |     |
!>      |                |            |     |
!>      |                |            |     |
!>      |                |            |     |
!>      |                |            |     |
!>      |                |            |     |
!>   +-----+-----+-----+-----+-----+-----+-----+
!>   | 100 |  0  |  0  | 101 |  0  | 102 | 103 |
!>   +-----+-----+-----+-----+-----+-----+-----+
!>   |                Process 0                |
!>   +-----------------------+-----+-----+-----+
PROGRAM main
    USE mpi

    IMPLICIT NONE

    INTEGER :: ierror
    INTEGER :: size
    INTEGER :: my_rank
    ! Determine root's process rank
    INTEGER, PARAMETER :: root_rank = 0
    INTEGER, ALLOCATABLE :: my_values(:)
    INTEGER :: my_values_count
    INTEGER :: counts(0:2)
    INTEGER :: displacements(0:2)
    INTEGER :: buffer(0:6) = 0
    INTEGER :: i

    CALL MPI_Init(ierror)

    ! Get number of processes and check only 3 processes are used
    CALL MPI_Comm_size(MPI_COMM_WORLD, size, ierror)
    IF (size .NE. 3) THEN
        WRITE(*,'(A)') 'This application is meant to be run with 3 processes.'
        CALL MPI_Abort(MPI_COMM_WORLD, -1, ierror)
    END IF

    ! Get my rank
    CALL MPI_Comm_rank(MPI_COMM_WORLD, my_rank, ierror)

    SELECT CASE(my_rank)
        CASE (0)
            ! Define my value
            my_values_count = 1
            ALLOCATE(my_values(0:my_values_count-1))
            my_values(0) = 100

            ! Define the receive counts
            counts = (/1, 1, 2/)

            ! Define the displacements
            displacements = (/0, 3, 5/)

            WRITE(*,'(A,I0,A,I0,A)') 'Process ', my_rank, ', my value = ', my_values(0), '.'
            
        CASE (1)
            ! Define my value
            my_values_count = 1
            ALLOCATE(my_values(0:my_values_count-1))
            my_values(0) = 101

            WRITE(*,'(A,I0,A,I0,A)') 'Process ', my_rank, ', my value = ', my_values(0), '.'
        CASE (2)
            ! Define my values
            my_values_count = 2
            ALLOCATE(my_values(0:my_values_count-1))
            my_values = (/102, 103/)

            WRITE(*,'(A,I0,A,I0,A,I0,A)') 'Process ', my_rank, ', my values are ', my_values(0), ' and ', my_values(1), '.'
    END SELECT

    CALL MPI_Gatherv(my_values, my_values_count, MPI_INTEGER, buffer, counts, displacements, &
                     MPI_INTEGER, root_rank, MPI_COMM_WORLD, ierror)
    DEALLOCATE(my_values)

    IF (my_rank .EQ. 0) THEN
        WRITE(*,'(A,I0,A)', advance='no') 'Values gathered in the buffer on process ', my_rank, ':'
        DO i = 0, 6
            WRITE(*,'(A,I0)', advance='no') ' ', buffer(i)
        END DO
        WRITE(*,'(A)') ''
    END IF

    CALL MPI_Finalize(ierror)
END PROGRAM main