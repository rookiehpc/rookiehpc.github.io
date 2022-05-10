!> @brief Sends a message as soon as possible in a blocking synchronous fashion.
!> @details This program is meant to be run with 2 processes: a sender and a
!> receiver.
PROGRAM main
    USE mpi_f08

    IMPLICIT NONE

    INTEGER :: size
    INTEGER, PARAMETER :: sender_rank = 0
    INTEGER, PARAMETER :: receiver_rank = 1
    INTEGER :: my_rank
    INTEGER :: buffer
    TYPE(MPI_Request) :: request
    INTEGER :: i

    CALL MPI_Init()

    ! Get the number of processes and check only 2 processes are used
    CALL MPI_Comm_size(MPI_COMM_WORLD, size)
    IF (size .NE. 2) THEN
        WRITE(*,'(A)') 'This application is meant to be run with 2 processes.'
        CALL MPI_Abort(MPI_COMM_WORLD, -1)
    END IF

    ! Get my rank and do the corresponding job
    CALL MPI_Comm_rank(MPI_COMM_WORLD, my_rank)
    SELECT CASE (my_rank)
        CASE (sender_rank)
            ! Prepare the ready send
            CALL MPI_Rsend_init(buffer, 1, MPI_INTEGER, receiver_rank, 0, MPI_COMM_WORLD, request)
            DO i = 0, 2
                WRITE(*,'(A,I0,A)') 'MPI process ', my_rank, ' hits the barrier to wait for the matching MPI_Recv to be posted.'
                CALL MPI_Barrier(MPI_COMM_WORLD)
                WRITE(*,'(A)') 'The barrier unlocked, which means the MPI_Recv is already posted so the MPI_Rsend can be issued.'

                buffer = 12345 + i
                WRITE(*,'(A,I0,A,I0,A,I0,A)') 'MPI process ', my_rank ,' sends value ', buffer, ' for message ', i, '.'
                ! Launch the ready send
                CALL MPI_Start(request)
                ! Wait for the ready send to complete
                CALL MPI_Wait(request, MPI_STATUS_IGNORE)
            END DO
        CASE (receiver_rank)
            DO i = 0, 2
                CALL MPI_Irecv(buffer, 1, MPI_INTEGER, sender_rank, 0, MPI_COMM_WORLD, request)

                WRITE(*,'(A,I0,A)') 'MPI process ', my_rank ,' issued the MPI_Irecv, moved on and hit the barrier.'
                CALL MPI_Barrier(MPI_COMM_WORLD)

                ! Wait for the underlying MPI_Recv to complete.
                CALL MPI_Wait(request, MPI_STATUS_IGNORE)
                WRITE(*,'(A,I0,A,I0,A,I0,A)') 'MPI process ', my_rank ,' receives value ', buffer ,' for message ', i, '.'
            END DO
    END SELECT

    CALL MPI_Finalize()
END PROGRAM main
