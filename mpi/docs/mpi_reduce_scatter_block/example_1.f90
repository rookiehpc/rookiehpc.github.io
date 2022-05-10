!> @brief Illustrates how to use a reduce scatter block.
!> @details This application is meant to be run with 3 MPI processes. It
!> consists of a sum reduction every MPI process has three values to send for
!> reduction. The first values from the MPI process will be reduced and stored 
!> on the MPI process 0. The second values will be reduced separately and stored
!> on MPI process 1, similarly with the third values on MPI process 2. It can be
!> visualised as follows:
!>
!>      +-----------+  +-----------+  +-----------+
!>      | Process 0 |  | Process 1 |  | Process 2 |
!>      +-----------+  +-----------+  +-----------+
!>      |   Values  |  |   Values  |  |   Values  |
!>      +---+---+---+  +---+---+---+  +---+---+---+ 
!>      | 0 | 1 | 2 |  | 3 | 4 | 5 |  | 6 | 7 | 8 |
!>      +---+---+---+  +---+---+---+  +---+---+---+
!>        |    \   \     /   |    \    /   /    |
!>        | ____\___\___/____|_____\__/   /     |
!>        |/     \   \       |      \    /      |
!>        |       \___\_____ | ______\__/       |
!>        |            \    \|/       \         |
!>        |             \____|_________\_______ |
!>        |                  |                 \|
!>        |                  |                  |
!>     +--+--+            +--+--+            +-----+
!>     | SUM |            | SUM |            | SUM |
!>     +-----+            +-----+            +-----+
!>     |  9  |            |  12 |            |  15 |
!>     +--+--+            +--+--+            +--+--+
!>        |                  |                  |      
!>  +-----+-----+      +-----+-----+      +-----+-----+
!>  | Process 0 |      | Process 1 |      | Process 2 |
!>  +-----------+      +-----------+      +-----------+
!>  |   Value   |      |   Value   |      |   Value   |
!>  |     9     |      |     12    |      |     15    |
!>  +-----------+      +-----------+      +-----------+
PROGRAM main
    USE mpi

    INTEGER :: ierror
    INTEGER :: size = 0
    INTEGER :: my_rank
    INTEGER :: values(3)
    INTEGER :: reduction_result

    CALL MPI_Init(ierror)

    ! Get the size of the communicator
    CALL MPI_Comm_size(MPI_COMM_WORLD, size, ierror)
    IF (size .NE. 3) THEN
        WRITE(*,'(A)') 'This application is meant to be run with 3 MPI processes.'
        CALL MPI_Abort(MPI_COMM_WORLD, 0, ierror)
    END IF

    ! Get my rank
    CALL MPI_Comm_rank(MPI_COMM_WORLD, my_rank, ierror)

    ! Defines my values
    values = (/3 * my_rank, 3 * my_rank + 1, 3 * my_rank + 2/)

    ! Each MPI process sends its values and the buffer to receive the corresponding reduction results
    CALL MPI_Reduce_scatter_block(values, reduction_result, 1, MPI_INTEGER, MPI_SUM, MPI_COMM_WORLD, ierror)

    WRITE(*,'(A,I0,A,I0,A)') '(MPI process ', my_rank, ') The sum I received is ', reduction_result, '.'

    CALL MPI_Finalize(ierror)

END PROGRAM main