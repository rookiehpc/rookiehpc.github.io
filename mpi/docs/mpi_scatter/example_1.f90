!> @brief Illustrates how to use a scatter.
!> @details This application is meant to be run with 4 processes. Process 0 is
!> designed as root and begins with a buffer containing all values, and prints
!> them. It then dispatches these values to all the processes in the same
!> communicator. Other processes just receive the dispatched value meant for 
!> them. Finally, everybody prints the value received.
!>
!>                +-----------------------+
!>                |       Process 0       |
!>                +-----+-----+-----+-----+
!>                |  0  | 100 | 200 | 300 |
!>                +-----+-----+-----+-----+
!>                 /      |       |      \
!>                /       |       |       \
!>               /        |       |        \
!>              /         |       |         \
!>             /          |       |          \
!>            /           |       |           \
!> +-----------+ +-----------+ +-----------+ +-----------+
!> | Process 0 | | Process 1 | | Process 2 | | Process 3 |
!> +-+-------+-+ +-+-------+-+ +-+-------+-+ +-+-------+-+ 
!>   | Value |     | Value |     | Value |     | Value |   
!>   |   0   |     |  100  |     |  200  |     |  300  |   
!>   +-------+     +-------+     +-------+     +-------+   
!>                
PROGRAM main
    USE mpi

    IMPLICIT NONE

    INTEGER :: ierror
    INTEGER :: size
    ! Determine root's rank
    INTEGER, PARAMETER :: root_rank = 0
    INTEGER :: my_rank
    INTEGER :: my_value
    INTEGER :: buffer(0:3)

    CALL MPI_Init(ierror)

    ! Get number of processes and check that 4 processes are used
    CALL MPI_Comm_size(MPI_COMM_WORLD, size, ierror)
    IF (size .NE. 4) THEN
        WRITE(*,'(A)') 'This application is meant to be run with 4 processes.'
        CALL MPI_Abort(MPI_COMM_WORLD, -1, ierror)
    END IF

    ! Get my rank
    CALL MPI_Comm_rank(MPI_COMM_WORLD, my_rank, ierror)

    IF (my_rank .EQ. root_rank) THEN
        buffer = (/0, 100, 200, 300/)
        WRITE(*,'(A,I0,A,A,I0,A,I0,A,I0,A,I0,A)') 'Values to scatter from process ', my_rank, ':', &
                                                ' ', buffer(0) ,', ', buffer(1) ,', ', buffer(2) ,', ', buffer(3) ,'.'
    END IF

    CALL MPI_Scatter(buffer, 1, MPI_INTEGER, my_value, 1, MPI_INTEGER, root_rank, MPI_COMM_WORLD, ierror)

    WRITE(*,'(A,I0,A,I0,A)') 'Process ', my_rank ,' received value = ', my_value ,'.'

    CALL MPI_Finalize(ierror)
END PROGRAM main