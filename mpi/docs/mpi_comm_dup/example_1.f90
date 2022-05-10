!> @brief Illustrates how to duplicate a communicator with MPI_Comm_dup.
!> @details This application is meant to run with 2 MPI processes, one acting as
!> the sender in the use-case, one acting as the receiver. This application
!> illustrates how to duplicate a communicator, and also provides a use-case
!> for MPI_Comm_dup.
!> 
!> To that end, we imagine an user code that would:
!>     1.1) send an MPI_INTEGER from MPI process 0 to MPI process 1 with tag 0
!>     1.2) issue the corresponding MPI_Recv on MPI process 1 with tag 0
!> 
!> We also imagine an MPI library running in parallel in the background, which
!> would:
!>     2.1) send an MPI_INTEGER from MPI process 0 to MPI process 1 with tag 0
!>     2.2) issue the corresponding MPI_Recv on MPI process 1 with tag 0
!> 
!> Both sends and receives would thus be identical same sender rank, same
!> receiver rank, same element datatype, same element count, same tag and same
!> communicator. Therefore, the MPI_Recv of the MPI library could very well be
!> mismatched with the MPI_Send from the user code in the event of an
!> unfortunate timing.
!>
!> To avoid this risk, a duplicated communicator will be passed to that library.
!> It results that, albeit having a communicator identical to the original one,
!> the MPI calls issued by the MPI library will be on a different communicator,
!> hence cannot be mismatched with those that the user issued on the original
!> communicator.
PROGRAM main
    USE mpi

    IMPLICIT NONE

    INTEGER :: ierror
    INTEGER :: my_rank
    INTEGER :: duplicated_communicator
    INTEGER :: some_message_in_user_code = 1234
    INTEGER :: some_message_in_library_code = 5678
    INTEGER :: buffer_for_user_message
    INTEGER :: buffer_for_library_message

    CALL MPI_Init(ierror)

    ! Get my rank in the MPI_COMM_WORLD communicator
    CALL MPI_Comm_rank(MPI_COMM_WORLD, my_rank, ierror)

    ! Duplicate the MPI_COMM_WORLD communicator; everything is preserved, ranks included
    CALL MPI_Comm_dup(MPI_COMM_WORLD, duplicated_communicator, ierror)

    ! The actual communicator duplication with MPI_Comm_dup is complete by now.
    ! Below is a use case illustrating the usefulness of MPI_Comm_dup.

    ! An MPI_Send issued from the user code
    IF (my_rank .EQ. 0) THEN
        CALL MPI_Send(some_message_in_user_code, 1, MPI_INTEGER, 1, 0, MPI_COMM_WORLD, ierror)
        WRITE(*, '(A,I0,A)') '(MPI process 0) Sent ', some_message_in_user_code, ' in user code.'
    END IF

    ! An MPI_Send issued by an MPI library running in parallel
    IF (my_rank .EQ. 0) THEN
        CALL MPI_Send(some_message_in_library_code, 1, MPI_INTEGER, 1, 0, duplicated_communicator, ierror)
        WRITE(*, '(A,I0,A)') '(MPI process 0) Sent ', some_message_in_library_code, ' in library code.'
    END IF

    ! Same emitter rank, same receiver rank, same element datatype, same
    ! element count but different communicators, they cannot be mismatched.

    ! The user code can safely issue its MPI_Recv
    IF (my_rank .EQ. 1) THEN
        CALL MPI_Recv(buffer_for_user_message, 1, MPI_INTEGER, 0, 0, MPI_COMM_WORLD, MPI_STATUS_IGNORE, ierror)
        WRITE(*, '(A,I0,A)') '(MPI process 1) Received ', buffer_for_user_message, ' in user code.'
    END IF

    ! The MPI library can safely issue its MPI_Recv
    IF (my_rank .EQ. 1) THEN
        CALL MPI_Recv(buffer_for_library_message, 1, MPI_INTEGER, 0, 0, duplicated_communicator, MPI_STATUS_IGNORE, ierror)
        WRITE(*, '(A,I0,A)') '(MPI process 1) Received ', buffer_for_library_message, ' in library code.'
    END IF

    CALL MPI_Finalize(ierror)
END PROGRAM main