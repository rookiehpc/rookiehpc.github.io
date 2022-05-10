!> @brief Illustrates the use MPI_COMM_SELF.
!> @details This application is meant to be used with 2 MPI processes. A server
!> opening a connection and a client connecting to the server process. To accept
!> a connection, the server must issue a collective operation where 
!> MPI_COMM_SELF can be passed to restrict that collective operation to the
!> server process only.
PROGRAM main
    USE mpi_f08

    IMPLICIT NONE

    INTEGER :: size
    INTEGER, PARAMETER :: client_rank = 0
    INTEGER, PARAMETER :: server_rank = 1
    INTEGER :: my_rank
    TYPE(MPI_Comm) :: server
    TYPE(MPI_Comm) :: client
    CHARACTER(LEN=MPI_MAX_PORT_NAME) :: port_name

    CALL MPI_Init()

    ! Size of the default communicator
    CALL MPI_Comm_size(MPI_COMM_WORLD, size)
    IF (size .NE. 2) THEN
        WRITE(*,'(A)') 'This application is meant to be run with 2 MPI processes.'
        CALL MPI_Abort(MPI_COMM_WORLD, -1)
    END IF

    ! Get my rank and do the corresponding job
    CALL MPI_Comm_rank(MPI_COMM_WORLD, my_rank)
    SELECT CASE (my_rank)
        CASE (client_rank)
            WRITE(*,'(A)') 'Enter port name: '
            READ(*,'(A)') port_name
            CALL MPI_Comm_connect(port_name, MPI_INFO_NULL, 0, MPI_COMM_SELF, server)
            WRITE(*,'(A)') 'I am connected!'
            CALL MPI_Comm_disconnect(server)
        CASE (server_rank)
            CALL MPI_Open_port(MPI_INFO_NULL, port_name )
            WRITE(*, '(A,A)') 'Port name is: ', port_name
            CALL MPI_Comm_accept(port_name, MPI_INFO_NULL, 0, MPI_COMM_SELF, client)
            WRITE(*,'(A)') 'Incoming connection accepted!'
            CALL MPI_Comm_disconnect(client)
            CALL MPI_Close_port(port_name)
    END SELECT

    CALL MPI_Finalize()
END PROGRAM main
