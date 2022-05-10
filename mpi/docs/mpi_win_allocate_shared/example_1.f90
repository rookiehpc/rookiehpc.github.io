!> @brief Illustrate how to create a shared window.
!> @details This application consists in creating a shared window and interact
!> with it using direct memory accesses. Two MPI processes are used, each will
!> hold an integer initialised to 100. MPI process 0 will increment MPI process
!> 1's variable, and MPI process 1 will decrement MPI process 0's variable.
!> In this application, the overall shared window uses the default configuration
!> where it is made of contiguous data.
!>
!> This can be visualised as follows:
!>
!> - Start situation:
!>         Held on MPI process 0 | Held on MPI process 1
!>                         +-----+-----+
!>                         | 100 | 100 |
!>                         +-----+-----+
!>         My element = array(0) | My element = array(0)
!>       Peer element = array(1) | Peer element = array(-1)
!>
!> - End situation:
!>         Held on MPI process 0 | Held on MPI process 1
!>                         +-----+-----+
!>                         |  99 | 101 |
!>                         +-----+-----+
!>         My element = array(0) | My element = array(0)
!>       Peer element = array(1) | Peer element = array(-1)
!>
!> This code assumes MPI processes must be able to physically share memory.
PROGRAM main
    USE mpi
    USE, INTRINSIC :: ISO_C_BINDING

    IMPLICIT NONE

    INTEGER :: ierror
    INTEGER :: comm_size
    INTEGER :: my_rank
    INTEGER, PARAMETER :: ARRAY_SIZE = 1
    TYPE(C_PTR) :: c_window_ptr
    INTEGER(KIND=MPI_ADDRESS_KIND) :: window_buffer_size
    INTEGER, POINTER :: f_window_ptr(:)
    INTEGER :: window
    INTEGER :: integer_size

    CALL MPI_Init(ierror)
    CALL MPI_Type_size(MPI_INTEGER, integer_size, ierror)
    window_buffer_size = integer_size * ARRAY_SIZE

    ! Check that only 2 MPI processes are spawn
    CALL MPI_Comm_size(MPI_COMM_WORLD, comm_size, ierror)
    IF (comm_size .NE. 2) THEN
        WRITE(*, '(A,I0,A)') 'This application is meant to be run with 2 MPI processes, not ', comm_size, '.'
        CALL MPI_Abort(MPI_COMM_WORLD, -1, ierror)
    END IF

    ! Get my rank
    CALL MPI_Comm_rank(MPI_COMM_WORLD, my_rank, ierror)

    ! Create the window
    CALL MPI_Win_allocate(window_buffer_size, integer_size, MPI_INFO_NULL, MPI_COMM_WORLD, c_window_ptr, window, ierror)
    WRITE(*, '(A,I0,A)') '[MPI process ', my_rank, '] Window created.'

    CALL C_F_POINTER(c_window_ptr, f_window_ptr, SHAPE = [2])

    ! Do the C-FORTRAN pointer grab
    f_window_ptr(0) = 100
    WRITE(*, '(A,I0,A,I0,A,I0,A)') "[MPI process", my_rank, "] Value before direct write from MPI process", &
                                   comm_size - 1 - my_rank, ": ", f_window_ptr(0), "."

    ! Initialise my element
    CALL MPI_BARRIER(MPI_COMM_WORLD, ierror)
    IF(my_rank .EQ. 0) THEN
        f_window_ptr(1) = f_window_ptr(1) + 1
    ELSE
        f_window_ptr(-1) = f_window_ptr(-1) - 1
    END IF
    CALL MPI_BARRIER(MPI_COMM_WORLD, ierror)

    WRITE(*, '(A,I0,A,I0,A,I0,A)') "[MPI process", my_rank, "] Value after direct write from MPI process ", &
                                   comm_size - 1 - my_rank, ": ", f_window_ptr(0), "."

    ! Destroy the window
    WRITE(*, '(A,I0,A)') '[MPI process ', my_rank, '] Window destroyed.'
    CALL MPI_Win_free(window, ierror)

    CALL MPI_Finalize(ierror)
END PROGRAM main
