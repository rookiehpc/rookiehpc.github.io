!> @brief Illustrates how to get the dimensions of a graph.
!> @details This application consists of 3 MPI processes that form a fully
!> connected graph that can be visualised as follows:
!>
!> +-----+              +-----+
!> |     |------------->|     |
!> |  0  |              |  1  |
!> |     |<-------------|     |
!> +-----+              +-----+
!>  ^   |                |   ^
!>  |   |                |   |
!>  |   |    +-----+     |   |
!>  |   +--->|     |<----+   |
!>  |        |  2  |         |
!>  +--------|     |---------+
!>           +-----+
!>
!> After creating the graph, each MPI process retrieves the graph dimensions and
!> prints them.
PROGRAM main
    USE mpi_f08

    IMPLICIT NONE

    ! Size of the default communicator
    INTEGER :: comm_size
    ! My rank in the default communicator
    INTEGER :: my_rank
    ! Declare the total number of neighbours until each MPI process (= the ones before + its own)
    INTEGER, DIMENSION(0:2) :: indexes = [2, 4, 6]
    ! Declare the endpoint of each edge
    INTEGER, DIMENSION(0:5) :: edges = [1, 2, 0, 2, 0, 1]
    ! Allow MPI to reorder ranks if it deems it necessary
    LOGICAL :: reorder = .TRUE.
    ! The new communicator with the graph topology
    TYPE(MPI_Comm) :: new_communicator
    INTEGER :: number_of_indexes_retrieved
    INTEGER :: number_of_edge_retrieved

    CALL MPI_Init()

    CALL MPI_Comm_size(MPI_COMM_WORLD, comm_size)

    IF (comm_size .NE. 3) THEN
        WRITE(*, '(A,I0,A)') 'This application is meant to be run with 3 MPI processes, not ', comm_size, '.'
        CALL MPI_Abort(MPI_COMM_WORLD, -1)
    END IF

    CALL MPI_Comm_rank(MPI_COMM_WORLD, my_rank)

    ! Create a communicator given the graph topology.
    CALL MPI_Graph_create(MPI_COMM_WORLD, comm_size, indexes, edges, reorder, new_communicator)

    ! Get the graph dimensions and print them
    CALL MPI_Graphdims_get(new_communicator, number_of_indexes_retrieved, number_of_edge_retrieved)
    WRITE(*, '(A,I0,A,I0,A,I0,A)') '[MPI process ', my_rank, '] The graph communicator created contains ', &
                                   number_of_indexes_retrieved, ' nodes and ', number_of_edge_retrieved, ' edges.'

    CALL MPI_Finalize()
END PROGRAM main