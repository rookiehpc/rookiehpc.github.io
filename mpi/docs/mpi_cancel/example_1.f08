!> @brief Illustrates how to cancel a request.
!> @details This program is meant to be run with 2 processes: a sender and a
!> receiver. The sender issues a non-blocking send to communicate the value
!> 12345 to the receiver, and cancels it with MPI_Cancel. Both processes
!> display what they sent / received depending on whether the MPI_Cancel
!> happened before or after the message could be exchanged.
PROGRAM main
    USE mpi_f08

    IMPLICIT NONE

    INTEGER :: size
    INTEGER, PARAMETER :: sender_rank = 0
    INTEGER, PARAMETER :: receiver_rank = 1
    INTEGER :: my_rank
    INTEGER, PARAMETER :: buffer_sent = 12345
    INTEGER :: received = 0
    TYPE(MPI_Request) :: request
    TYPE(MPI_Status) :: status
    LOGICAL :: flag

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
            CALL MPI_Isend(buffer_sent, 1, MPI_INTEGER, 1, 0, MPI_COMM_WORLD, request)
            
            ! Cancel that request
            CALL MPI_Cancel(request)

            ! The request is marked for cancellation, but the MPI_Cancel operation is local therefore the MPI_Wait is still needed
            CALL MPI_Wait(request, status)

            ! Check whether the underlying communication had already taken place
            CALL MPI_Test_cancelled(status, flag)

            IF (flag .EQV. .TRUE.) THEN
                ! Successful cancellation
                WRITE(*, '(A,I0,A,A)') 'MPI process ', my_rank, ': the cancellation happened before ', &
                    'I could send the message, therefore nothing was sent.'
            ELSE
                ! Successful communication
                WRITE(*, '(A,I0,A,A,I0,A)') 'MPI process ', my_rank, ': the cancellation happened after ', &
                    'I sent the message containing value ', buffer_sent, '.'
            END IF
        CASE (receiver_rank)
            CALL MPI_Recv(received, 1, MPI_INTEGER, 0, 0, MPI_COMM_WORLD, status)

            ! Check whether the underlying communication had already taken place
            CALL MPI_Test_cancelled(status, flag)

            IF (flag .EQV. .TRUE.) THEN
                ! Successful cancellation
                WRITE(*, '(A,I0,A,A,A,I0,A)') 'MPI process ', my_rank, ': the cancellation happened before ', &
                    'I sent the message, therefore I received nothing and my buffer', &
                    ' still contains its original value of ', received, '.'
            ELSE
                ! Successful communication
                WRITE(*, '(A,I0,A,A,I0,A,I0,A)') 'MPI process ', my_rank, ': the cancellation happened after ', &
                    'MPI process ', sender_rank, ' sent the message, therefore I received value ', received, ' as normal.'
            END IF
    END SELECT

    CALL MPI_Finalize()
END PROGRAM main