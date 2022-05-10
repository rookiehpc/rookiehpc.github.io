!> @brief Illustrates how to get the dimensions of a graph.
!> @details This application consists of 3 MPI processes that form a graph
!> that can be visualised as follows:
!>
!> +-----+              +-----+
!> |     |              |     |
!> |  0  |              |  1  |
!> |     |              |     |
!> +-----+              +-----+
!>  ^   |                    ^
!>  |   |                    |
!>  |   |    +-----+         |
!>  |   +--->|     |         |
!>  |        |  2  |         |
!>  +--------|     |---------+
!>           +-----+
!>
!> After creating the graph, each MPI process retrieves its number of neighbours
!> and prints it.
PROGRAM main
    USE mpi_f08

    IMPLICIT NONE

    ! Size of the default communicator
    INTEGER :: comm_size
    ! My rank in the default communicator
    INTEGER :: my_rank
    ! Declare the total number of neighbours until each MPI process (= the ones before + its own)
    INTEGER, DIMENSION(0:2) :: indexes = [1, 1, 3]
    ! Declare the endpoint of each edge
    INTEGER, DIMENSION(0:2) :: edges = [2, 0, 1]
    ! Allow MPI to reorder ranks if it deems it necessary
    LOGICAL :: reorder = .TRUE.
    ! The new communicator with the graph topology
    TYPE(MPI_Comm) :: new_communicator
    INTEGER :: number_of_neighbours_retrieved

    CALL MPI_Init()

    CALL MPI_Comm_size(MPI_COMM_WORLD, comm_size)

    IF (comm_size .NE. 3) THEN
        WRITE(*, '(A,I0,A)') 'This application is meant to be run with 3 MPI processes, not ', comm_size, '.'
        CALL MPI_Abort(MPI_COMM_WORLD, -1)
    END IF

    CALL MPI_Comm_rank(MPI_COMM_WORLD, my_rank)

    ! Create a communicator given the graph topology.
    CALL MPI_Graph_create(MPI_COMM_WORLD, comm_size, indexes, edges, reorder, new_communicator)

    ! Get my number of neighbours and print it
    CALL MPI_Graph_neighbors_count(new_communicator, my_rank, number_of_neighbours_retrieved)
    WRITE(*, '(A,I0,A,I0,A)') '[MPI process ', my_rank, '] I have ', &
                                   number_of_neighbours_retrieved, ' neighbours.'

    CALL MPI_Finalize()
END PROGRAM main