!> @brief Exercise to broadcast a value.
!> @details This exercises is made of 4 MPI processes. One, the MPI process 1,
!> will broadcast the value 12345. All three other processes will receive this
!> value. The broadcast must not be achieved using individual sends.
PROGRAM main
    USE mpi

    IMPLICIT NONE

    INTEGER :: ierror
    INTEGER :: comm_size

    ! 1) Tell MPI to start

    ! 2) Check that at least 4 MPI processes are used
    CALL MPI_Comm_size(MPI_COMM_WORLD, comm_size, ierror)
    IF (comm_size < 4) THEN
        WRITE(*, '(A,I0,A)') 'This application must be run with at least 4 MPI processes, not ', comm_size, '.'
        CALL MPI_Abort(MPI_COMM_WORLD, -1, ierror)
    END IF

    ! 3) Get my rank

    ! 4) If my rank is 0, I am the broadcast emitter
        ! 4.1) Print the value to broadcast
        ! 4.2) Broadcast the value

    ! 5) Otherwise, I am a broadcast receiver
        ! 5.1) Receive the value by broadcast
        ! 5.2) Print the value received by broadcast

    ! 6) Tell MPI to stop
END PROGRAM main