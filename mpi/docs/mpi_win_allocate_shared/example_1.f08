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
    USE mpi_f08
    USE, INTRINSIC :: ISO_C_BINDING, ONLY : C_PTR

    IMPLICIT NONE

    INTEGER :: comm_size
    INTEGER :: my_rank
    INTEGER, PARAMETER :: ARRAY_SIZE = 1
    TYPE(C_PTR) :: c_window_ptr
    INTEGER(KIND=MPI_ADDRESS_KIND) :: window_buffer_size
    INTEGER, POINTER :: f_window_ptr(:)
    TYPE(MPI_Win) :: window
    INTEGER :: integer_size

    CALL MPI_Init()
    CALL MPI_Type_size(MPI_INTEGER, integer_size)
    window_buffer_size = integer_size * ARRAY_SIZE

    ! Check that only 2 MPI processes are spawn
    CALL MPI_Comm_size(MPI_COMM_WORLD, comm_size)
    IF (comm_size .NE. 2) THEN
        WRITE(*, '(A,I0,A)') 'This application is meant to be run with 2 MPI processes, not ', comm_size, '.'
        CALL MPI_Abort(MPI_COMM_WORLD, -1)
    END IF

    ! Get my rank
    CALL MPI_Comm_rank(MPI_COMM_WORLD, my_rank)

    ! Create the window
    CALL MPI_Win_allocate(window_buffer_size, integer_size, MPI_INFO_NULL, MPI_COMM_WORLD, c_window_ptr, window)
    WRITE(*, '(A,I0,A)') '[MPI process ', my_rank, '] Window created.'

    ! Do the C-FORTRAN pointer grab
    CALL C_F_POINTER(c_window_ptr, f_window_ptr, SHAPE = [2])

    ! Initialise my element
    f_window_ptr(0) = 100
    WRITE(*, '(A,I0,A,I0,A,I0,A)') "[MPI process", my_rank, "] Value before direct write from MPI process", &
                                   comm_size - 1 - my_rank, ": ", f_window_ptr(0), "."

    ! Modify peer's element                                   
    CALL MPI_BARRIER(MPI_COMM_WORLD)
    IF(my_rank .EQ. 0) THEN
        f_window_ptr(1) = f_window_ptr(1) + 1
    ELSE
        f_window_ptr(-1) = f_window_ptr(-1) - 1
    END IF
    CALL MPI_BARRIER(MPI_COMM_WORLD)

    WRITE(*, '(A,I0,A,I0,A,I0,A)') "[MPI process", my_rank, "] Value after direct write from MPI process ", &
                                   comm_size - 1 - my_rank, ": ", f_window_ptr(0), "."

    ! Destroy the window
    WRITE(*, '(A,I0,A)') '[MPI process ', my_rank, '] Window destroyed.'
    CALL MPI_Win_free(window)

    CALL MPI_Finalize()
END PROGRAM main
