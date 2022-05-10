!> @brief Illustrates how to use the variable version of a scatter.
!> @details A process is designed as root and begins with a buffer containig all
!> values, and prints them. It then dispatches these values to all the processes
!> in the same communicator. Other process just receive the dispatched value(s)
!> meant for them. Finally, everybody prints the value received. This
!> application is designed to cover all cases:
!> - Different send counts
!> - Different displacements
!> This application is meant to be run with 3 processes.
!>
!>       +-----------------------------------------+
!>       |                Process 0                |
!>       +-----+-----+-----+-----+-----+-----+-----+
!>       | 100 |  0  | 101 | 102 |  0  |  0  | 103 |
!>       +-----+-----+-----+-----+-----+-----+-----+
!>         |            |     |                |
!>         |            |     |                |
!>         |            |     |                |
!>         |            |     |                |
!>         |            |     |                |
!>         |            |     |                |
!> +-----------+ +-------------------+ +-----------+
!> | Process 0 | |    Process 1      | | Process 2 |
!> +-+-------+-+ +-+-------+-------+-+ +-+-------+-+
!>   | Value |     | Value | Value |     | Value |
!>   |  100  |     |  101  |  102  |     |  103  |
!>   +-------+     +-------+-------+     +-------+ 
!>                
PROGRAM main
    USE mpi

    IMPLICIT NONE

    INTEGER :: ierror
    INTEGER :: size
    ! Determine root's rank
    INTEGER, PARAMETER :: root_rank = 0
    INTEGER :: my_rank
    INTEGER, ALLOCATABLE :: my_values(:)
    INTEGER :: my_values_count
    INTEGER :: buffer(0:6)
    INTEGER :: counts(0:2)
    INTEGER :: displacements(0:2)
    INTEGER :: i

    CALL MPI_Init(ierror)

    ! Get number of processes and check that 3 processes are used
    CALL MPI_Comm_size(MPI_COMM_WORLD, size, ierror)
    IF (size .NE. 3) THEN
        WRITE(*,'(A)') 'This application is meant to be run with 3 processes.'
        CALL MPI_Abort(MPI_COMM_WORLD, -1, ierror)
    END IF

    ! Get my rank
    CALL MPI_Comm_rank(MPI_COMM_WORLD, my_rank, ierror)

    SELECT CASE (my_rank)
        CASE (0)
            my_values_count = 1
            ALLOCATE(my_values(0:my_values_count-1))

            ! Define the buffer
            buffer = (/100, 0, 101, 102, 0, 0, 103/)

            ! Define the counts
            counts = (/1, 2, 1/)

            ! Define the displacements
            displacements = (/0, 2, 6/)

            WRITE(*,'(A)',advance='no') 'Values in the buffer of root process:'
            DO i = 0, 6
                WRITE(*,'(A,I0)',advance='no') ' ', buffer(i)
            END DO
            WRITE(*,'(A)') ''
        CASE (1)
            my_values_count = 2
            ALLOCATE(my_values(0:my_values_count-1))
        CASE (2)
            my_values_count = 1
            ALLOCATE(my_values(0:my_values_count-1))
    END SELECT

    CALL MPI_Scatterv(buffer, counts, displacements, MPI_INTEGER, &
                      my_values, my_values_count, MPI_INTEGER, root_rank, MPI_COMM_WORLD, ierror)
    WRITE(*,'(A,I0,A)',advance='no') 'Process ', my_rank, ' received values:'
    DO i = 0, my_values_count-1
        WRITE(*,'(A,I0)',advance='no') ' ', my_values(i)
    END DO
    WRITE(*,'(A)') ''
    DEALLOCATE(my_values)

    CALL MPI_Finalize(ierror)
END PROGRAM main