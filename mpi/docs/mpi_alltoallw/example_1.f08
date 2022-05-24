!> @brief Illustrates how to use a variable all to all.
!> @details This application is meant to be run with 3 MPI processes. Each
!> process has an arbitrary number of elements to send and receive, at different
!> positions. To demonstrate the great flexibility of the MPI_Alltoallv routine,
!> the data exchange designed is rather irregular, so it is extra detailed in
!> this description.
!> 
!> It can be described as follows:
!> - Process 0:
!>     - has 3 integers to send, as follows, it sends:
!>         - to process 0: the first integer
!>         - to process 1: the last 2 integers
!>         - to process 2: nothing
!>     - has 2 integers to receive, as follows, it receives:
!>         - from process 0: 1 integer, stores it at the end
!>         - from process 1: nothing
!>         - from process 2: 1 integer, stores it at the beginning
!> - Process 1:
!>     - has 3 integers to send, as follows, it sends:
!>         - nothing to process 0
!>         - nothing to itself
!>         - 3 integers to process 2
!>     - has 2 integers to receive, as follows, it receives:
!>         - 2 integers rom process 0
!>         - nothing from itself
!>         - nothing from process 2
!> - Process 2:
!>     - has 1 integer to send, as follows, it sends:
!>         - 1 integer to process 0
!>         - nothing to process 1
!>         - nothing to itself
!>     - has 3 integers to receive, as follows, it receives:
!>         - nothing from process 0
!>         - 3 integers from process 1
!>         - nothing from itself
!>
!> In addition to the above, it can be visualised as follows:
!>
!> +-----------------------+ +-----------------------+ +-----------------------+
!> |       Process 0       | |       Process 1       | |       Process 2       |
!> +-------+-------+-------+ +-------+-------+-------+ +-------+-------+-------+
!> | Value | Value | Value | | Value | Value | Value |         | Value |
!> |   0   |  100  |  200  | |  300  |  400  |  500  |         |  600  |
!> +-------+-------+-------+ +-------+-------+-------+         +-------+
!>     |       |       |        |        |       |_________________|_______
!>     |       |       |        |        |_________________________|_      |
!>     |       |       |        |______________________________    | |     |
!>     |       |       |_____________________                  |   | |     |
!>     |       |_______________________      |                 |   | |     | 
!>     |   ____________________________|_____|_________________|___| |     |
!>     |__|_____                       |     |                 |     |     | 
!>        |     |                      |     |                 |     |     | 
!>     +-----+-----+                +-----+-----+           +-----+-----+-----+
!>     | 600 |  0  |                | 100 | 200 |           | 300 | 400 | 500 |
!>  +--+-----+-----+--+         +---+-----+-----+-+         +-----+-----+-----+
!>  |    Process 0    |         |    Process 1    |         |    Process 2    |
!>  +-----------------+         +-----------------+         +-----------------+
PROGRAM main
    USE mpi_f08

    IMPLICIT NONE

    INTEGER :: size
    INTEGER :: my_rank   
    INTEGER, ALLOCATABLE :: buffer_send(:)
    INTEGER :: buffer_send_length
    INTEGER :: counts_send(3)
    INTEGER :: displacements_send(3)
    INTEGER, ALLOCATABLE :: buffer_recv(:)
    INTEGER :: buffer_recv_length
    INTEGER :: counts_recv(3)
    INTEGER :: displacements_recv(3)
    INTEGER :: i

    CALL MPI_Init()

    ! Get number of processes and check that 3 processes are used
    CALL MPI_Comm_size(MPI_COMM_WORLD, size)
    IF (size .NE. 3) THEN
        WRITE(*,'(A)') 'This application is meant to be run with 3 MPI processes.'
        CALL MPI_Abort(MPI_COMM_WORLD, -1)
    END IF

    ! Get my rank
    CALL MPI_Comm_rank(MPI_COMM_WORLD, my_rank)

    ! Define the buffer containing the values to send
    SELECT CASE(my_rank)
        CASE (0)
            buffer_send_length = 3
            ALLOCATE(buffer_send(buffer_send_length))
            buffer_send(1) = 0
            buffer_send(2) = 100
            buffer_send(3) = 200
            WRITE(*,'(A,I0,A,I0,A,I0,A,I0,A)') 'Process ', my_rank, ', my values = ', &
                                               buffer_send(1), ', ', buffer_send(2), ', ', buffer_send(3), '.'
        CASE (1)
            buffer_send_length = 3
            ALLOCATE(buffer_send(buffer_send_length))
            buffer_send(1) = 300
            buffer_send(2) = 400
            buffer_send(3) = 500
            WRITE(*,'(A,I0,A,I0,A,I0,A,I0,A)') 'Process ', my_rank, ', my values = ', &
                                               buffer_send(1), ', ', buffer_send(2), ', ', buffer_send(3), '.'
        CASE (2)
            buffer_send_length = 1
            ALLOCATE(buffer_send(buffer_send_length))
            buffer_send(1) = 600
            WRITE(*,'(A,I0,A,I0,A)') 'Process ', my_rank, ', my value = ', buffer_send(1)
    END SELECT

    ! Define my counts for sending (how many integers do I send to each process?)
    SELECT CASE (my_rank)
        CASE (0)
            counts_send = [1, 2, 0]
        CASE (1)
            counts_send = [0, 0, 3]
        CASE (2)
            counts_send = [1, 0, 0]
    END SELECT

    ! Define my displacements for sending (where is located in the buffer each message to send?)
    SELECT CASE (my_rank)
        CASE (0)
            displacements_send = [0, 1, 0]
        CASE (1)
            displacements_send = [0, 0, 0]
        CASE (2)
            displacements_send = [0, 0, 0]
    END SELECT

    ! Define the buffer for reception
    SELECT CASE (my_rank)
        CASE (0)
            buffer_recv_length = 2
            ALLOCATE(buffer_recv(buffer_recv_length))
        CASE (1)
            buffer_recv_length = 2
            ALLOCATE(buffer_recv(buffer_recv_length))
        CASE (2)
            buffer_recv_length = 3
            ALLOCATE(buffer_recv(buffer_recv_length))
    END SELECT

    ! Define my counts for receiving (how many integers do I receive from each process?)
    SELECT CASE (my_rank)
        CASE (0)
            counts_recv = [1, 0, 1]
        CASE (1)
            counts_recv = [2, 0, 0]
        CASE (2)
            counts_recv = [0, 3, 0]
    END SELECT

    ! Define my displacements for reception (where to store in buffer each message received?)
    SELECT CASE (my_rank)
        CASE (0)
            displacements_recv = [1, 0, 0]
        CASE (1)
            displacements_recv = [0, 0, 0]
        CASE (2)
            displacements_recv = [0, 0, 0]
    END SELECT

    CALL MPI_Alltoallv(buffer_send, counts_send, displacements_send, MPI_INTEGER, &
                       buffer_recv, counts_recv, displacements_recv, MPI_INTEGER, MPI_COMM_WORLD)
    
    WRITE(*,'(A,I0,A)',advance='no') 'Values received on process ', my_rank, ':'
    DO i = 1, buffer_recv_length
        WRITE(*,'(A,I0)',advance='no') ' ', buffer_recv(i)
    END DO
    WRITE(*,'(A)') ''

    DEALLOCATE(buffer_send)
    DEALLOCATE(buffer_recv)

    CALL MPI_Finalize()
END PROGRAM main
