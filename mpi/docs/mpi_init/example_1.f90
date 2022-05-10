!> @brief Illustrates how to initialise the MPI environment.
PROGRAM main
    USE mpi

    IMPLICIT NONE
 
    INTEGER :: my_rank
    INTEGER :: ierror
 
    ! Initilialise MPI and check its completion
    CALL MPI_INIT(ierror)
 
    ! Get my rank
    CALL MPI_COMM_RANK(MPI_COMM_WORLD, my_rank, ierror)
 
    WRITE (*,'(A,I0,A)') 'Process ', my_rank, ' has initialised its MPI environment.'
 
    ! Tell MPI to shut down.
    CALL MPI_FINALIZE(ierror)
END PROGRAM main