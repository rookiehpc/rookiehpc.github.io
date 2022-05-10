!> @brief Illustrates how to initialise the MPI environment.
PROGRAM main
    USE mpi_f08

    IMPLICIT NONE
 
    INTEGER :: my_rank
 
    ! Initilialise MPI and check its completion
    CALL MPI_INIT()
 
    ! Get my rank
    CALL MPI_COMM_RANK(MPI_COMM_WORLD, my_rank)
 
    WRITE (*,'(A,I0,A)') 'Process ', my_rank, ' has initialised its MPI environment.'
 
    ! Tell MPI to shut down.
    CALL MPI_FINALIZE()
END PROGRAM main
