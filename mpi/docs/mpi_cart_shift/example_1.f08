!> @brief Illustrates how to use the MPI_Cart_shift routine.
!> @details This code creates a cartesian topology, then retrieves the rank of
!> up/down left/right neighbours via a shift.
!> For readability reasons, it is advised to run this code with 4 processes. The
!> toplogy created, given 4 processes, can be visualised as:
!>      +-----------+-----------+
!>      |           |           |
!>    ^ | process 1 | process 3 |
!>    | |           |           |
!> UP | +-----------+-----------+
!>    | |           |           |
!>    | | process 0 | process 2 |
!>      |           |           |
!>      +-----------------------+
!>        ------------------->
!>                RIGHT
PROGRAM main
    USE mpi_f08

    IMPLICIT NONE

    INTEGER :: ierror
    INTEGER :: size
    INTEGER :: dims(0:1)
    LOGICAL :: periods(0:1)
    LOGICAL :: reorder
    TYPE(MPI_Comm) :: new_communicator
    INTEGER, PARAMETER :: DOWN = 0
    INTEGER, PARAMETER :: UP = 1
    INTEGER, PARAMETER :: LEFT = 2
    INTEGER, PARAMETER :: RIGHT = 3
    CHARACTER(len=5) :: neighbours_names(0:3)
    INTEGER :: neighbours_ranks(0:3)
    INTEGER :: my_rank
    INTEGER :: i

    CALL MPI_Init(ierror)

    ! Size of the default communicator
    CALL MPI_Comm_size(MPI_COMM_WORLD, size, ierror)
    IF (size .NE. 4) THEN
        WRITE(*,'(A,I0,A)') 'This application is meant to be run with 4 processes, not ', size, '.'
        CALL MPI_Abort(MPI_COMM_WORLD, -1, ierror)
    END IF

    ! Ask MPI to decompose our processes in a 2D cartesian grid for us
    dims = [0, 0]
    CALL MPI_Dims_create(size, 2, dims, ierror)

    ! Make both dimensions non-periodic
    periods = [.FALSE., .FALSE.]

    ! Let MPI assign arbitrary ranks if it deems it necessary
    reorder = .TRUE.

    ! Create a communicator with a cartesian topology.
    CALL MPI_Cart_create(MPI_COMM_WORLD, 2, dims, periods, reorder, new_communicator, ierror)

    ! Declare our neighbours
    neighbours_names = [' down', '   up', ' left', 'right']

    ! Let consider dims[0] = X, so the shift tells us our left and right neighbours
    CALL MPI_Cart_shift(new_communicator, 0, 1, neighbours_ranks(LEFT), neighbours_ranks(RIGHT), ierror)

    ! Let consider dims[1] = Y, so the shift tells us our up and down neighbours
    CALL MPI_Cart_shift(new_communicator, 1, 1, neighbours_ranks(DOWN), neighbours_ranks(UP), ierror)

    ! Get my rank in the new communicator
    CALL MPI_Comm_rank(new_communicator, my_rank, ierror)

    DO i = 0, 3
        IF (neighbours_ranks(i) .EQ. MPI_PROC_NULL) THEN
            WRITE(*,'(A,I0,A,A,A)') '[MPI process ', my_rank, '] I have no ', neighbours_names(i), ' neighbour.'
        ELSE
            WRITE(*,'(A,I0,A,A,A,A,I0,A)') '[MPI process ', my_rank, '] I have a ', neighbours_names(i), ' neighbour:', &
                                           ' process ', neighbours_ranks(i), '.'
        END IF
    END DO

    CALL MPI_Finalize(ierror)
END PROGRAM main
