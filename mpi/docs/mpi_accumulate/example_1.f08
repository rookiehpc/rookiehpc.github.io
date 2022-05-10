!> @brief Illustrate how to accumulate data.
!> @details This application consists of two MPI processes. MPI process 0
!> exposes a window containing an integer initialised to 0. All the other MPI
!> processes add their rank to that value. After the MPI_Accumulate is issued, 
!> each MPI process calls on MPI_Win_fence to synchronise. Finally, MPI process
!> 0 prints the total value.
PROGRAM main
    USE mpi_f08

    IMPLICIT NONE

    INTEGER :: ierror
    INTEGER :: my_rank
    INTEGER :: window_buffer = 0
    TYPE(MPI_Win) :: window
    INTEGER :: integer_size
    INTEGER(KIND=MPI_ADDRESS_KIND) :: window_buffer_size
    INTEGER(KIND=MPI_ADDRESS_KIND) :: target_displacement

    CALL MPI_Init(ierror)

    CALL MPI_Type_size(MPI_INTEGER, integer_size, ierror)
    window_buffer_size = integer_size

    ! Get my rank
    CALL MPI_Comm_rank(MPI_COMM_WORLD, my_rank, ierror)

    ! Create the window
    CALL MPI_Win_create(window_buffer, window_buffer_size, integer_size, MPI_INFO_NULL, MPI_COMM_WORLD, window, ierror)
    IF (my_rank .EQ. 0) THEN
        WRITE(*, '(A,I0,A)') '(MPI process 0) Value in my window_buffer before MPI_Accumulate: ', window_buffer, '.'
    END IF
    CALL MPI_Win_fence(0, window, ierror)

    IF (my_rank .GT. 0) THEN
        ! Push my value into the first integer in MPI process 0 window
        target_displacement = 0
        CALL MPI_Accumulate(my_rank, 1, MPI_INTEGER, 0, target_displacement, 1, MPI_INTEGER, MPI_SUM, window, ierror)
        WRITE(*, '(A,I0,A,I0,A)') '[MPI process ', my_rank ,'] I accumulate data ', my_rank, &
            ' in MPI process 0 window via MPI_Accumulate.'
    END IF

    ! Wait for the MPI_Accumulate issued to complete before going any further
    CALL MPI_Win_fence(0, window, ierror)

    IF (my_rank .EQ. 0) THEN
        WRITE(*, '(A,I0,A)') '[MPI process 0] Value in my window_buffer after MPI_Accumulate: ', window_buffer, '.'
    END IF

    ! Destroy the window
    CALL MPI_Win_free(window, ierror)

    CALL MPI_Finalize(ierror)
END PROGRAM main
