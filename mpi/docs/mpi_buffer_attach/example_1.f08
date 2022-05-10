!> @brief Illustrate how to attach a memory buffer to MPI so MPI_Bsend issued
!> use this buffer to copy the messages to send.
!> @details This application requires 2 processes: 1 sender and 1 receiver.
PROGRAM main
	USE mpi_f08
    USE, INTRINSIC ::  ISO_C_BINDING, &
         ONLY : C_PTR, C_LOC

	IMPLICIT NONE

	INTEGER :: size
	INTEGER, PARAMETER :: sender_rank = 0
	INTEGER, PARAMETER :: receiver_rank = 1
	INTEGER :: my_rank
	INTEGER :: message
	INTEGER :: buffer_size_bytes
	INTEGER :: buffer_size_elements
	INTEGER, ALLOCATABLE, TARGET :: buffer(:)
	TYPE(C_PTR) :: buffer_attached_C_pointer
    INTEGER, POINTER :: buffer_attached_F_pointer(:)

	CALL MPI_Init()

	! Check that 2 processes are used
	CALL MPI_Comm_size(MPI_COMM_WORLD, size)
	IF (size .NE. 2) THEN
		WRITE(*,'(A)') 'This application is meant to be run with 2 processes.'
		CALL MPI_Abort(MPI_COMM_WORLD, -1)
	END IF

	! Get my rank and do the corresponding job
	CALL MPI_Comm_rank(MPI_COMM_WORLD, my_rank)
	SELECT CASE (my_rank)
		CASE (sender_rank)
			! The message to send
			message = 1234;

			! Allocate enough space to issue the buffered send
			buffer_size_bytes = (MPI_BSEND_OVERHEAD + SIZEOF(message))
			buffer_size_elements = (buffer_size_bytes - MODULO(buffer_size_bytes, SIZEOF(message))) / SIZEOF(message)
			IF (MODULO(buffer_size_bytes, SIZEOF(message)) .NE. 0) THEN
				buffer_size_elements = buffer_size_elements + 1
			END IF
			ALLOCATE(buffer(0:buffer_size_elements-1))

			! Pass the buffer allocated to MPI so it uses it when we issue MPI_Bsend
			CALL MPI_Buffer_attach(buffer, buffer_size_bytes)

			! Issue the buffered send
			WRITE(*,'(A,I0,A,I0,A,I0,A)') '[Process ', my_rank, '] I send value ', message, ' to process ', receiver_rank, '.'
			CALL MPI_Bsend(message, 1, MPI_INTEGER, receiver_rank, 0, MPI_COMM_WORLD)

			! Detach the buffer no-longer used (it will wait for MPI_Bsend message to be sent first)
			CALL MPI_Buffer_detach(buffer_attached_C_pointer, buffer_size_bytes)
			CALL C_F_POINTER(buffer_attached_C_pointer, buffer_attached_F_pointer, SHAPE=[buffer_size_elements])
            DEALLOCATE(buffer_attached_F_pointer)
		CASE (receiver_rank)
			ALLOCATE(buffer(1))
			CALL MPI_Recv(buffer, 1, MPI_INTEGER, sender_rank, 0, MPI_COMM_WORLD, MPI_STATUS_IGNORE)
			WRITE(*,'(A,I0,A,I0,A)') '[Process ', my_rank, '] I received value ', buffer, '.'
			DEALLOCATE(buffer)
	END SELECT

	CALL MPI_Finalize()
END PROGRAM main
