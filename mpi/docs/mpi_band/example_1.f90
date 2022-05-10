!> @brief Illustrates how to use a bit-wise and reduction operation.
!> @details This application consists of a bit-wise and reduction; every MPI process
!> sends its value for reduction before the bit-wise and of these values is stored in
!> the root MPI process. It can be visualised as follows, with MPI process 0 as
!> root:
!>
!> +-----------+ +-----------+ +-----------+ +-----------+
!> | Process 0 | | Process 1 | | Process 2 | | Process 3 |
!> +-+-------+-+ +-+-------+-+ +-+-------+-+ +-+-------+-+
!>   | Value |     | Value |     | Value |     | Value |
!>   |   0   |     |   1   |     |   3   |     |   8   |
!>   +-------+     +-------+     +-------+     +-------+
!>            \         |           |         /
!>             \        |           |        /
!>              \       |           |       /
!>               \      |           |      /
!>                +-----+-----+-----+-----+
!>                            |
!>                        +---+---+
!>                        |  BAND |
!>                        +---+---+
!>                            |
!>                        +---+---+
!>                        |   0   |
!>                      +-+-------+-+
!>                      | Process 0 |
!>                      +-----------+
PROGRAM main    
    USE mpi

    IMPLICIT NONE

    INTEGER :: ierror
    INTEGER :: size
    INTEGER :: my_rank
    ! Determine root's rank
    INTEGER, PARAMETER :: root_rank = 0
    INTEGER :: all_values(0:3)
    INTEGER :: my_value
    INTEGER :: reduction_result

    CALL MPI_Init(ierror)

    ! Get the number of processes and check only 4 are used.
    CALL MPI_Comm_size(MPI_COMM_WORLD, size, ierror)
    IF (size .NE. 4) THEN
        WRITE(*,'(A)') 'This application is meant to be run with 4 processes.'
        CALL MPI_Abort(MPI_COMM_WORLD, -1, ierror)
    END IF

    ! Get my rank
    CALL MPI_Comm_rank(MPI_COMM_WORLD, my_rank, ierror)

    ! Each MPI process sends its rank to reduction, root MPI process collects the result
    all_values = (/0, 1, 3, 8/)
    my_value = all_values(my_rank)
    CALL MPI_Reduce(my_value, reduction_result, 1, MPI_INTEGER, MPI_BAND, root_rank, MPI_COMM_WORLD, ierror)

    IF (my_rank .EQ. root_rank) THEN
        WRITE(*,'(A,I0,A)') 'The bit-wise and of all values is ', reduction_result, '.'
    END IF

    CALL MPI_Finalize(ierror)
END PROGRAM main