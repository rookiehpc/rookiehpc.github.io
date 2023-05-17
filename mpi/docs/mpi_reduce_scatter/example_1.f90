!> @brief Illustrates how to use a reduce scatter.
!> @details This application is meant to be run with 3 MPI processes. It
!> consists of a sum reduction every MPI process has four values to send for
!> reduction. The first values from the MPI process will be reduced and stored 
!> on the MPI process 0. The second and third values will be reduced separately 
!> and stored on MPI process 1, similarly with the fourth values on MPI process
!> 2. It can be visualised as follows:
!>
!>      +---------------+  +---------------+  +---------------+
!>      |   Process 0   |  |   Process 1   |  |   Process 2   |
!>      +---------------+  +---------------+  +---------------+
!>      |     Values    |  |     Values    |  |     Values    |
!>      +---+---+---+---+  +---+---+---+---+  +---+---+---+---+
!>      | 0 | 1 | 2 | 3 |  | 4 | 5 | 6 | 7 |  | 8 | 9 | 10| 11|
!>      +---+---+---+---+  +---+---+---+---+  +---+---+---+---+
!>        |    \   \   \     /  |     |  \      /   /   /   |
!>        | ____\___\___\___/___|_____|___\____/   /   /    |
!>        |/     \   \   \      |     |    \      /   /     |
!>        |       \___\___\____ | ____|_____\____/   /      |
!>        |            \   \   \|/    |      \      /       |
!>        |             \___\___|____ | ______\____/        |
!>        |                  \  |    \|/       \            |
!>        |                   \_|_____|_________\__________ |
!>        |                     |     |                    \|
!>        |                     |     |                     |
!>     +--+--+                +-+---+-+---+              +--+--+
!>     | SUM |                | SUM | SUM |              | SUM |
!>     +-----+                +-----+-----+              +-----+
!>     |  12 |                |  15 |  18 |              |  21 |
!>     +--+--+                +--+--+--+--+              +--+--+
!>        |                      |     |                    |      
!>  +-----+-----+             +--+-----+--+           +-----+-----+
!>  | Process 0 |             | Process 1 |           | Process 2 |
!>  +-----------+             +-----------+           +-----------+
!>  |   Value   |             |   Values  |           |   Value   |
!>  +-----------+             +-----+-----+           +-----------+
!>  |     12    |             |  15 |  18 |           |     21    |
!>  +-----------+             +-----+-----+           +-----------+
PROGRAM main
    USE mpi

    INTEGER :: ierror
    INTEGER :: size
    INTEGER :: my_rank
    INTEGER :: values(4)
    INTEGER :: counts(3)
    INTEGER, ALLOCATABLE :: reduction_results(:)

    CALL MPI_Init(ierror)

    ! Get the size of the communicator
    CALL MPI_Comm_size(MPI_COMM_WORLD, size, ierror)
    IF (size .NE. 3) THEN
        WRITE(*,'(A)') 'This application is meant to be run with 3 MPI processes.'
        CALL MPI_Abort(MPI_COMM_WORLD, -1, ierror)
    END IF

    ! Get my rank
    CALL MPI_Comm_rank(MPI_COMM_WORLD, my_rank, ierror)

    ! Defines my values
    values = (/4 * my_rank, 4 * my_rank + 1, 4 * my_rank + 2, 4 * my_rank + 3/)

    ! Define the block lengths
    counts = (/1, 2, 1/)

    IF (my_rank .EQ. 1) THEN
        ! Each MPI process sends its values and the buffer to receive the corresponding reduction results
        ALLOCATE(reduction_results(2))
        CALL MPI_Reduce_scatter(values, reduction_results, counts, MPI_INTEGER, MPI_SUM, MPI_COMM_WORLD, ierror)
        WRITE(*,'(A,I0,A,I0,A,I0,A)') '(MPI process ', my_rank ,') The sum I received are ', reduction_results(1) ,' and ', &
                                      reduction_results(2) ,'.'
        DEALLOCATE(reduction_results)
    ELSE
        ! Each MPI process sends its values and the buffer to receive the corresponding reduction results
        ALLOCATE(reduction_results(1))
        CALL MPI_Reduce_scatter(values, reduction_results, counts, MPI_INTEGER, MPI_SUM, MPI_COMM_WORLD, ierror)
        WRITE(*,'(A,I0,A,I0,A)') '(MPI process ', my_rank, ') The sum I received is ', reduction_results, '.'
        DEALLOCATE(reduction_results)
    END IF

    CALL MPI_Finalize(ierror)
END PROGRAM main
