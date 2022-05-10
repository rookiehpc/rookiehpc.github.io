!> @brief Illustrates how to create a communicator representing a 2D torus
!> topology.
PROGRAM main
    USE mpi

    IMPLICIT NONE

    INTEGER :: ierror
    INTEGER :: size
    INTEGER :: dims(0:1)
    LOGICAL :: periods(0:1)
    LOGICAL :: reorder
    INTEGER :: new_communicator
    INTEGER :: my_rank
    INTEGER :: my_coords(0:1)

    CALL MPI_Init(ierror)

    ! Size of the default communicator
    CALL MPI_Comm_size(MPI_COMM_WORLD, size, ierror)

    ! Ask MPI to decompose our processes in a 2D cartesian grid for us
    dims = (/0,0/)
    CALL MPI_Dims_create(size, 2, dims, ierror)

    ! Make both dimensions periodic
    periods = (/.TRUE., .TRUE./)

    ! Let MPI assign arbitrary ranks if it deems it necessary
    reorder = .TRUE.

    ! Create a communicator given the 2D torus topology.
    CALL MPI_Cart_create(MPI_COMM_WORLD, 2, dims, periods, reorder, new_communicator, ierror)

    ! My rank in the new communicator
    CALL MPI_Comm_rank(new_communicator, my_rank, ierror)

    ! Get my coordinates in the new communicator
    CALL MPI_Cart_coords(new_communicator, my_rank, 2, my_coords, ierror)

    ! Print my location in the 2D torus.
    WRITE(*,'(A,I0,A,I0,A,I0,A)') '[MPI process ', my_rank, '] I am located at (', my_coords(0), ', ', my_coords(1), ').'

    CALL MPI_Finalize(ierror)
END PROGRAM main