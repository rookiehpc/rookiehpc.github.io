!> @brief Illustrates how to print a process location in a cartesian grid.
!> @details This code creates a new communicator given a 2D torus topology. It
!> then makes each process print its coordinates in the 2D torus communicator.
PROGRAM main
    USE mpi_f08

    IMPLICIT NONE

    INTEGER :: size
    INTEGER :: dims(0:1)
    LOGICAL :: periods(0:1)
    LOGICAL :: reorder
    TYPE(MPI_Comm) :: new_communicator
    INTEGER :: my_rank
    INTEGER :: my_coords(0:1)

    CALL MPI_Init()

    ! Size of the default communicator
    CALL MPI_Comm_size(MPI_COMM_WORLD, size)

    ! Ask MPI to decompose our processes in a 2D cartesian grid for us
    dims = [0, 0]
    CALL MPI_Dims_create(size, 2, dims)

    ! Make both dimensions periodic
    periods = [.TRUE., .TRUE.]

    ! Let MPI assign arbitrary ranks if it deems it necessary
    reorder = .TRUE.

    ! Create a communicator given the 2D torus topology.
    CALL MPI_Cart_create(MPI_COMM_WORLD, 2, dims, periods, reorder, new_communicator)

    ! My rank in the new communicator
    CALL MPI_Comm_rank(new_communicator, my_rank)

    ! Get my coordinates in the new communicator
    CALL MPI_Cart_coords(new_communicator, my_rank, 2, my_coords)

    ! Print my location in the 2D torus.
    WRITE(*,'(A,I0,A,I0,A,I0,A)') '[MPI process ', my_rank, '] I am located at (', my_coords(0), ', ', my_coords(1), ').'

    CALL MPI_Finalize()
END PROGRAM main
