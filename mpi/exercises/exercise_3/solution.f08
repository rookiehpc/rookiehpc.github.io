!> @brief Solution to the 'Shout it to the whole world' exercise.
PROGRAM main
    USE mpi_f08

    IMPLICIT NONE

    INTEGER :: comm_size
    INTEGER :: my_rank
    INTEGER :: value

    ! 1) Tell MPI to start
    CALL MPI_Init()

    ! 2) Check that at least 4 MPI processes are used
    CALL MPI_Comm_size(MPI_COMM_WORLD, comm_size)
    IF (comm_size .NE. 4) THEN
        WRITE(*, '(A,I0,A)') 'This application must be run with at least 4 MPI processes, not ', comm_size, '.'
        CALL MPI_Abort(MPI_COMM_WORLD, -1)
    END IF

    ! 3) Get my rank
    CALL MPI_Comm_rank(MPI_COMM_WORLD, my_rank)

    ! 4) If my rank is 0, I am the broadcast emitter
    IF (my_rank .EQ. 0) THEN
        ! 4.1) Print the value to broadcast
        value = 12345
        WRITE(*, '(A,I0,A,I0,A)') 'I am process ', my_rank ,' and I broadcast value ', value ,'.'

        ! 4.2) Broadcast the value
        CALL MPI_Bcast(value, 1, MPI_INTEGER, 0, MPI_COMM_WORLD)
    ! 5) Otherwise, I am a broadcast receiver
    ELSE
        ! 5.1) Receive the value by broadcast
        CALL MPI_Bcast(value, 1, MPI_INTEGER, 0, MPI_COMM_WORLD)

        ! 5.2) Print the value received by broadcast
        WRITE(*, '(A,I0,A,I0,A)') 'I am process ', my_rank, ' and I received value ', value, ' via broadcast.'
    END IF

    ! 6) Tell MPI to stop
    CALL MPI_Finalize()
END PROGRAM main