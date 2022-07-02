!> @brief Solution to the hello world in MPI.
PROGRAM main
    USE mpi
 
    IMPLICIT NONE
 
    INTEGER :: ierror
    INTEGER :: my_rank
    INTEGER :: comm_size
 
    ! 1) Tell MPI to start
    CALL MPI_Init(ierror)
 
    ! 2) Get my rank
    CALL MPI_Comm_rank(MPI_COMM_WORLD, my_rank, ierror)
 
    ! 3) Get the number of MPI processes
    CALL MPI_Comm_size(MPI_COMM_WORLD, comm_size, ierror)
 
    ! 4) Print everything
    WRITE(*, '(A,I0,A,I0,A)') '"Hello World!" from MPI process ', my_rank, '. We are ', comm_size, ' MPI processes.'
 
    ! 5) Tell MPI to end
    CALL MPI_Finalize(ierror)
END PROGRAM main