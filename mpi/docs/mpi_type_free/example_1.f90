!> @brief Illustrates how to free an MPI datatype.
!> @details This program is meant to be run with 2 processes: a sender and a
!> receiver. These two MPI processes will exchange a message made of two
!> integers. To that end, they each create a datatype representing that layout.
!> The corresponding MPI_Datatype is created, then committed before being used
!> in communications, to eventually be freed.
PROGRAM main
    USE mpi

    IMPLICIT NONE

    INTEGER :: ierror
    INTEGER :: size
    INTEGER :: double_int_type
    INTEGER, PARAMETER :: sender_rank = 0
    INTEGER, PARAMETER :: receiver_rank = 1
    INTEGER :: my_rank
    INTEGER :: buffer(0:1)

    CALL MPI_Init(ierror)

    ! Get the number of processes and check only 2 processes are used
    CALL MPI_Comm_size(MPI_COMM_WORLD, size, ierror)
    IF (size .NE. 2) THEN
        WRITE(*,'(A)') 'This application is meant to be run with 2 processes.'
        CALL MPI_Abort(MPI_COMM_WORLD, -1, ierror)
    END IF

    ! Create the datatype
    CALL MPI_Type_contiguous(2, MPI_INTEGER, double_int_type, ierror)

    ! Commit the datatype so we can use it in communications
    CALL MPI_Type_commit(double_int_type, ierror)

    ! Get my rank and do the corresponding job
    CALL MPI_Comm_rank(MPI_COMM_WORLD, my_rank, ierror)
    SELECT CASE (my_rank)
        CASE (sender_rank)
            buffer = (/12345, 67890/)
            WRITE(*,'(A,I0,A,I0,A,I0,A)') 'MPI process ', my_rank, ' sends values ', buffer(0), ' and ', buffer(1), '.'
            CALL MPI_Send(buffer, 1, double_int_type, receiver_rank, 0, MPI_COMM_WORLD, ierror)
        CASE (receiver_rank)
            CALL MPI_Recv(buffer, 1, double_int_type, sender_rank, 0, MPI_COMM_WORLD, MPI_STATUS_IGNORE, ierror)
            WRITE(*,'(A,I0,A,I0,A,I0,A)') 'MPI process ', my_rank, ' received values ', buffer(0), ' and ', buffer(1), '.'
    END SELECT

    ! Free the datatype created
    CALL MPI_Type_free(double_int_type, ierror)

    CALL MPI_Finalize(ierror)
END PROGRAM main