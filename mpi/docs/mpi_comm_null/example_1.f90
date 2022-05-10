!> @brief Illustrates how to check whether a process is part of a new
!> communicator.
!> @details This code creates a new communicator from a cartesian grid designed
!> to contain only 1 process. By running this application with multiple
!> processes, all but one will not belong to the new communicator created.
PROGRAM main
    USE mpi

    IMPLICIT NONE

    INTEGER :: ierror
    INTEGER :: size
    INTEGER :: my_rank
    INTEGER :: dims(0:1)
    LOGICAL :: periods(0:1)
    LOGICAL :: reorder
    INTEGER :: new_communicator

    CALL MPI_Init(ierror)

    ! Size of the default communicator
    CALL MPI_Comm_size(MPI_COMM_WORLD, size, ierror)

    ! Rank in the default communicator
    CALL MPI_Comm_rank(MPI_COMM_WORLD, my_rank, ierror)

    ! Use a cartesian grid of 1 process in total
    dims = (/1, 1/)

    ! Make both dimensions periodic
    periods = (/.TRUE., .TRUE./)

    ! Let MPI assign arbitrary ranks if it deems it necessary
    reorder = .TRUE.

    ! Create a communicator given the 2D torus topology.
    CALL MPI_Cart_create(MPI_COMM_WORLD, 2, dims, periods, reorder, new_communicator, ierror)

    ! Check if I am part of the new communicator
    IF (new_communicator .EQ. MPI_COMM_NULL) THEN
        WRITE(*,'(A,I0,A)') 'Process ', my_rank, ' is not part of the new communicator.'
    ELSE
        WRITE(*,'(A,I0,A)') 'Process ', my_rank, ' is part of the new communicator.'
    END IF

    CALL MPI_Finalize(ierror)
END PROGRAM main