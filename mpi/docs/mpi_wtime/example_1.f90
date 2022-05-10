!> @brief Illustrates how to use MPI_Wtime.
!> @details This application consists of 4 MPI processes which will forward a
!> message from MPI process 0 onwards. Every MPI process will introduce a
!> latency of 250ms by waiting, using MPI_Wtime to see how much time has been
!> waited already. The job of each MPI process is to simulate this latency, then
!> send a message to the next MPI process, if any. Also, each MPI process will
!> have checked the time before and after its job using MPI_Wtime, and will 
!> print the timing obtained (effectively being the difference between the two
!> timings). The execution flow can be visualised as follows:
!>
!> +------------------------------------------------------------------+ 
!> | MPI process 0                                                    |
!> | | Start clock                                                    |
!> | | Wait 250ms                                                     |
!> | +--------------> MPI process 1                                   |
!> | |                | Start clock                                   |
!> | |                | Wait 250ms                                    |
!> | |                +--------------> MPI process 2                  |
!> | |                |                | Start clock                  |
!> | |                |                | Wait 250ms                   |
!> | |                |                +--------------> MPI process 3 |
!> | |                |                |                | Start clock |
!> | |                |                |                | Wait 250ms  |
!> | V                V                V                V             |
!> +-+----------------+----------------+----------------+-------------+ 
!> |                             MPI BARRIER                          |
!> +-+----------------+----------------+----------------+-------------+
!> | | Stop clock     | Stop clock     | Stop clock     | Stop clock  |
!> | X Print time     X Print time     X Print time     X Print time  |
!> +------------------------------------------------------------------+
PROGRAM main
    USE mpi

    IMPLICIT NONE

    INTEGER :: ierror
    INTEGER :: comm_size
    INTEGER :: my_rank
    ! Time to wait before processing, in seconds
    DOUBLE PRECISION, PARAMETER :: waiting_time = 0.250
    INTEGER :: message
    DOUBLE PRECISION :: start
    DOUBLE PRECISION :: end

    CALL MPI_Init(ierror)

    ! Check that 4 MPI processes are used
    CALL MPI_Comm_size(MPI_COMM_WORLD, comm_size, ierror)
    IF (comm_size .NE. 4) THEN
        WRITE(*, '(A,I0,A)') 'This application is meant to be run with 4 MPI processes, not ', comm_size, '.'
        CALL MPI_Abort(MPI_COMM_WORLD, -1, ierror)
    END IF

    ! Get my rank
    CALL MPI_Comm_rank(MPI_COMM_WORLD, my_rank, ierror)

    IF (my_rank == 0) THEN
        ! If I am the first MPI process, I send the message to the next MPI process and wait for reception

        ! Job begins for me, check the clock
        start = MPI_Wtime()

        ! I simulate the latency
        DO WHILE (MPI_Wtime() - start < waiting_time)
            ! We keep looping until <waiting_time> seconds have elapsed
        END DO

        CALL MPI_Send(message, 1, MPI_INTEGER, 1, 0, MPI_COMM_WORLD, ierror)
    ELSE
        ! If am not the first MPI process, I receive the message from the previous MPI process first
        CALL MPI_Recv(message, 1, MPI_INTEGER, my_rank - 1, 0, MPI_COMM_WORLD, MPI_STATUS_IGNORE, ierror)

        ! Job begins for me, check the clock
        start = MPI_Wtime()

        ! I simulate the latency
        DO WHILE (MPI_Wtime() - start < waiting_time)
            ! We keep looping until <waiting_time> seconds have elapsed
        END DO

        ! If I am not the last MPI process
        IF (my_rank .NE. comm_size - 1) THEN
            ! I forward the message to the next MPI process
            CALL MPI_Send(message, 1, MPI_INTEGER, my_rank + 1, 0, MPI_COMM_WORLD, ierror)
        END IF
    END IF

    ! Wait for the very last one to 
    CALL MPI_Barrier(MPI_COMM_WORLD, ierror)
    end = MPI_Wtime()

    WRITE(*, '(A,I0,A,F4.2,A)') '[MPI process ', my_rank, '] time elapsed during the job: ', end - start, 's.'

    CALL MPI_Finalize(ierror)
END PROGRAM main