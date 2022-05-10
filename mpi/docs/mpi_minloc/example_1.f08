!> @brief Illustrates how to use a minloc reduction operation.
!> @details This application consists in every MPI process sending its value
!> along with its rank. The value sent will be used in the min reduction while
!> the rank will be the locator data. This allows for the locator to represent
!> the rank which has the minimum value found. The result of the reduction will
!> be stored on MPI process 2. It can be visualised as follows:
!>
!> +--------------+ +--------------+ +--------------+ +--------------+
!> |   Process 0  | |   Process 1  | |   Process 2  | |   Process 3  |
!> +--------------+ +--------------+ +--------------+ +--------------+
!> | Value:    12 | | Value:    34 | | Value:    56 | | Value:    78 |
!> | Location:  0 | | Location:  1 | | Location:  2 | | Location:  3 |
!> +--------------+ +--------------+ +--------------+ +--------------+
!>            \             |               |             /
!>             \            |               |            /
!>              \           |               |           /
!>               \          |               |          /
!>                +---------+------+--------+---------+
!>                                 |
!>                          +--------------+
!>                          |     MIN      |
!>                          +--------------+
!>                          | Value:    12 |
!>                          | Location:  0 |
!>                          +--------------+
!>                                 |
!>                          +--------------+
!>                          |   Process 2  |
!>                          +--------------+
!>                          | Value:    12 |
!>                          | Location:  0 |
!>                          +--------------+
!> 
!> In order to pass the value and the locator data, an MPI datatype containing
!> both will be created.
PROGRAM main
    USE mpi_f08

    IMPLICIT NONE

    INTEGER :: size
    ! Determine root's rank
    INTEGER :: root_rank = 2
    INTEGER :: my_rank
    ! The data pair that will be used.
    ! (0): the value (12 / 34 / 56 / 78)
    ! (1): the locator (the MPI rank)
    INTEGER, DIMENSION(0:1) :: data_pair
    INTEGER, DIMENSION(0:1) :: reduction_result

    CALL MPI_Init()

    ! Get the number of processes and check only 4 are used.
    CALL MPI_Comm_size(MPI_COMM_WORLD, size)
    IF (size .NE. 4) THEN
        WRITE(*, '(A)') 'This application is meant to be run with 4 processes.'
        CALL MPI_Abort(MPI_COMM_WORLD, -1)
    END IF

    ! Get my rank
    CALL MPI_Comm_rank(MPI_COMM_WORLD, my_rank)

    ! Initialise the value
    SELECT CASE (my_rank)
        CASE (0)
            data_pair(0) = 12
        CASE (1)
            data_pair(0) = 34
        CASE (2)
            data_pair(0) = 56
        CASE (3)
            data_pair(0) = 78
    END SELECT

    ! Initialise the locator
    data_pair(1) = my_rank

    ! Each MPI process sends its rank to reduction, root MPI process collects the result
    CALL MPI_Reduce(data_pair, reduction_result, 1, MPI_2INTEGER, MPI_MINLOC, root_rank, MPI_COMM_WORLD)

    IF (my_rank == root_rank) THEN
        WRITE(*, '(A,I0,A,I0,A)') 'The minimum value is ', reduction_result(0), &
                                  ' and is held by MPI process ', reduction_result(1), '.'
    END IF

    CALL MPI_Finalize()
END PROGRAM
