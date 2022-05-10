!> @brief Illustrates how to get the indexes and edges of a graph.
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
    USE mpi

    IMPLICIT NONE

    INTEGER :: ierror
    ! Size of the default communicator
    INTEGER :: comm_size
    ! My rank in the default communicator
    INTEGER :: my_rank
    ! Declare the total number of neighbours until each MPI process (= the ones before + its own)
    INTEGER, DIMENSION(0:2) :: indexes = (/2, 4, 6/)
    ! Declare the endpoint of each edge
    INTEGER, DIMENSION(0:5) :: edges = (/1, 2, 0, 2, 0, 1/)
    ! Allow MPI to reorder ranks if it deems it necessary
    LOGICAL :: reorder = .TRUE.
    ! The new communicator with the graph topology
    INTEGER :: new_communicator
    ! The number of indexes in the graph, as retrieved from MPI_Graphdims_get
    INTEGER :: number_of_indexes_retrieved
    ! The number of edges in the graph, as retrieved from MPI_Graphdims_get
    INTEGER :: number_of_edges_retrieved
    ! The indexes of the graph, as retrieved from MPI_Graph_get
    INTEGER, DIMENSION(:), ALLOCATABLE :: indexes_retrieved
    ! The edges of the graph, as retrieved from MPI_Graph_get
    INTEGER, DIMENSION(:), ALLOCATABLE :: edges_retrieved
    ! Iterator used in do loop
    INTEGER :: i

    CALL MPI_Init(ierror)

    CALL MPI_Comm_size(MPI_COMM_WORLD, comm_size, ierror)

    IF (comm_size .NE. 3) THEN
        WRITE(*, '(A,I0,A)') 'This application is meant to be run with 3 MPI processes, not ', comm_size, '.'
        CALL MPI_Abort(MPI_COMM_WORLD, -1, ierror)
    END IF

    CALL MPI_Comm_rank(MPI_COMM_WORLD, my_rank, ierror)

    ! Create a communicator given the graph topology.
    CALL MPI_Graph_create(MPI_COMM_WORLD, comm_size, indexes, edges, reorder, new_communicator, ierror)

    ! Get the graph dimensions
    CALL MPI_Graphdims_get(new_communicator, number_of_indexes_retrieved, number_of_edges_retrieved, ierror)

    ! Retrieve the indexes and edges
    ALLOCATE(indexes_retrieved(0:number_of_indexes_retrieved-1))
    ALLOCATE(edges_retrieved(0:number_of_edges_retrieved-1))
    CALL MPI_Graph_get(new_communicator, number_of_indexes_retrieved, number_of_edges_retrieved, &
                       indexes_retrieved, edges_retrieved, ierror)

    ! Print all information retrieved
    WRITE(*, '(A,I0,A,I0,A)', advance="no") '[MPI process ', my_rank, '] ', number_of_indexes_retrieved, ' indexes retrieved: {'
    DO i = 0, number_of_indexes_retrieved - 1
        WRITE(*, '(I0)', advance="no") indexes_retrieved(i)
        IF (i .LT. number_of_indexes_retrieved - 1) THEN
            WRITE(*, '(A)', advance="no") ', '
        END IF
    END DO
    WRITE(*, '(A,I0,A)', advance="no") '}, and ', number_of_edges_retrieved, ' edges retrieved: {'
    DO i = 0, number_of_edges_retrieved - 1
        WRITE(*, '(I0)', advance="no") edges_retrieved(i)
        IF (i .LT. number_of_edges_retrieved - 1) THEN
            WRITE(*, '(A)', advance="no") ', '
        END IF
    END DO
    WRITE(*, '(A)') '}.'

    CALL MPI_Finalize(ierror)
END PROGRAM main