!> @brief Illustrates how to use a non-blocking all-reduce.
!> @details This application consists of a sum all-reduction every MPI process
!> sends its rank for reduction before the sum of these ranks is stored in the
!> receive buffer of each MPI process. It can be visualised as follows:
!>
!> +-----------+ +-----------+ +-----------+ +-----------+
!> | Process 0 | | Process 1 | | Process 2 | | Process 3 |
!> +-+-------+-+ +-+-------+-+ +-+-------+-+ +-+-------+-+
!>   | Value |     | Value |     | Value |     | Value |
!>   |   0   |     |   1   |     |   2   |     |   3   |
!>   +-------+     +----+--+     +--+----+     +-------+
!>            \         |           |         /
!>             \        |           |        /
!>              \       |           |       /
!>               \      |           |      /
!>                +-----+-----+-----+-----+
!>                            |
!>                        +---+---+
!>                        |  SUM  |
!>                        +---+---+
!>                        |   6   |
!>                        +-------+
!>                            |
!>                +-----+-----+-----+-----+
!>               /      |           |      \
!>              /       |           |       \
!>             /        |           |        \
!>            /         |           |         \
!>   +-------+     +----+--+     +--+----+     +-------+  
!>   |   6   |     |   6   |     |   6   |     |   6   |  
!> +-+-------+-+ +-+-------+-+ +-+-------+-+ +-+-------+-+
!> | Process 0 | | Process 1 | | Process 2 | | Process 3 |
!> +-----------+ +-----------+ +-----------+ +-----------+
PROGRAM main
    USE mpi

    IMPLICIT NONE

    INTEGER :: ierror
    INTEGER :: size
    INTEGER :: my_rank
    INTEGER :: reduction_result = 0
    INTEGER :: request

    CALL MPI_Init(ierror)

    ! Get the size of the communicator
    CALL MPI_Comm_size(MPI_COMM_WORLD, size, ierror)
    IF (size .NE. 4) THEN
        WRITE(*,'(A)') 'This application is meant to be run with 4 MPI processes.'
        CALL MPI_Abort(MPI_COMM_WORLD, 0, ierror)
    END IF

    ! Get my rank
    CALL MPI_Comm_rank(MPI_COMM_WORLD, my_rank, ierror)

    ! Each MPI process sends its rank to reduction, root MPI process collects the result
    CALL MPI_Iallreduce(my_rank, reduction_result, 1, MPI_INTEGER, MPI_SUM, MPI_COMM_WORLD, request, ierror)

    ! Do some other job
    WRITE(*,'(A,I0,A)') 'Process ', my_rank, ' issued the MPI_Iallreduce and has moved on, printing this message.'

    ! Wait for the MPI_Iallreduce to complete
    CALL MPI_Wait(request, MPI_STATUS_IGNORE, ierror)

    WRITE(*,'(A,I0,A,I0,A)') '[MPI Process ', my_rank, '] The sum of all ranks is ', reduction_result, '.'

    CALL MPI_Finalize(ierror)
END PROGRAM main