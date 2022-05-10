!> @brief Illustrates how to use a non-blocking barrier.
!> @details This application provides a basic example about a non-blocking
!> barrier. Although this example may not be very realistic, it does illustrate
!> how to manipulate a non-blocking barrier.
!>
!> This application consists of three jobs for each process:
!> 1) Get my rank
!> 2) When I have my rank, I print it
!> 3) Once all processes obtained their rank, I say it
!>
!> Rather than using a classic barrier after 2), we can issue a non-blocking
!> barrier after 1) so that we overlap the synchronisation of the barrier with
!> the task 2). Then, once 2) is complete, we can block until the non-blocking
!> barrier completes to progress to 3).
!>
!> The difference can be visualised as follows:
!>
!>               +--------------+
!>               | Barrier sync |
!> +------+------+--------------+------+
!> | Task | Task |              | Task |
!> |   1  |   2  |              |   3  |
!> +------+------+--------------+------+
!>                                     |
!>        +--------------+             |
!>        | Barrier sync |             |
!> +------+------+-------+------+      |
!> | Task | Task |       | Task |      |
!> |  1   |   2  |       |   3  |      |
!> +------+------+--------------+      |
!>                              |      |
!>                              V      V
!> ---------------- TIME -------+------+----->
!>                               <---->
!>                                gain
PROGRAM main
    USE mpi_f08

    IMPLICIT NONE

    INTEGER :: my_rank
    TYPE(MPI_Request) :: request

    CALL MPI_Init()

    ! Get my rank, this is task 1
    CALL MPI_Comm_rank(MPI_COMM_WORLD, my_rank)
    CALL MPI_Ibarrier(MPI_COMM_WORLD, request)

    ! Task 2
    WRITE(*,'(A,I0,A,I0,A)') '[MPI process ', my_rank, '] I got my rank, it is ', my_rank, ', I now call MPI_Ibarrier.'

    ! Task 3
    CALL MPI_Wait(request, MPI_STATUS_IGNORE)
    WRITE(*,'(A,I0,A)') '[MPI process ', my_rank, '] The MPI_Ibarrier is complete all processes got their rank.'

    CALL MPI_Finalize()
END PROGRAM main
