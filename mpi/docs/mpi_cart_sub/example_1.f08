!> @brief Illustrates how to partition a cartesian topology created with
!> MPI_Cart_create.
!> @details This program is meant to be run with 6 MPI processes. It consists in
!> partitioning a 2 x 3 2D cartesian topology along its first dimension to
!> obtain 2 1D subgrids of 3 MPI processes each.
!> The initial 2D cartesian topology can be visualised as follows:
!>
!>                  Second dimension
!> ------------------------------------------------->
!>
!> +---------------+---------------+---------------+   |
!> | MPI process 0 | MPI process 1 | MPI process 2 |   |
!> |     (0,0)     |     (0,1)     |     (0,2)     |   |
!> +---------------+---------------+---------------+   |  First dimension
!> | MPI process 3 | MPI process 4 | MPI process 5 |   |
!> |     (1,0)     |     (1,1)     |     (1,2)     |   |
!> +---------------+---------------+---------------+   v
!>
!> And the final subgrids can be visualised as follows:
!>
!> +---------------+---------------+---------------+  \
!> | MPI process 0 | MPI process 1 | MPI process 2 |   } First subgrid
!> +---------------+---------------+---------------+  /
!>
!> +---------------+---------------+---------------+  \
!> | MPI process 3 | MPI process 4 | MPI process 5 |   } Second subgrid
!> +---------------+---------------+---------------+  /
!>
PROGRAM main
    USE mpi_f08

    IMPLICIT NONE

    INTEGER :: size
    ! Dimensions of the 2D cartesian topology to partition
    INTEGER, DIMENSION(0:1) :: dims = [2, 3]
    LOGICAL, DIMENSION(0:1) :: periods = [.FALSE., .FALSE.]
    LOGICAL :: reorder = .TRUE.
    TYPE(MPI_Comm) :: cartesian_communicator
    INTEGER :: my_rank
    INTEGER, DIMENSION(0:1) :: my_coords
    LOGICAL, DIMENSION(0:1) :: remain_dims = [.FALSE., .TRUE.]
    TYPE(MPI_Comm) :: subgrid_communicator
    INTEGER, DIMENSION(0:2) :: subgrid_ranks

    CALL MPI_Init()

    ! Size of the default communicator
    CALL MPI_Comm_size(MPI_COMM_WORLD, size)

    IF (size .NE. 6) THEN
        WRITE(*, '(A)') 'This application is meant to be run with 6 processes.'
        CALL MPI_Abort(MPI_COMM_WORLD, -1)
    END IF

    ! Create a communicator given the 2D torus topology.
    CALL MPI_Cart_create(MPI_COMM_WORLD, 2, dims, periods, reorder, cartesian_communicator)

    ! My rank in the new communicator
    CALL MPI_Comm_rank(cartesian_communicator, my_rank)

    ! Get my coordinates in the new communicator
    CALL MPI_Cart_coords(cartesian_communicator, my_rank, 2, my_coords)

    ! Print my location in the 2D cartesian topology.
    WRITE(*, '(A,I0,A,I0,A,I0,A)') '[MPI process ', my_rank, '] I am located at &
                                    (', my_coords(0), ', ', my_coords(1), ') in the initial 2D cartesian topology.'

    ! Partition the 2D cartesian topology along the first dimension, by preserving the second dimension
    CALL MPI_Cart_sub(cartesian_communicator, remain_dims, subgrid_communicator)

    ! Get the ranks of all MPI processes in my subgrid and print it
    CALL MPI_Allgather(my_rank, 1, MPI_INTEGER, subgrid_ranks, 1, MPI_INTEGER, subgrid_communicator)
    WRITE(*, '(A,I0,A,I0,A,I0,A,I0,A)') '[MPI process ', my_rank, '] I am in the 1D subgrid that contains MPI processes ', &
                                        subgrid_ranks(0), ', ', subgrid_ranks(1), ' and ', subgrid_ranks(2), '.'

    CALL MPI_Finalize()
END PROGRAM main