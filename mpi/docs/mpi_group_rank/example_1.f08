!> @brief For each process in the group of the default communicator
!> MPI_COMM_WORLD, show their rank.
PROGRAM main
    USE mpi_f08

    IMPLICIT NONE

    TYPE(MPI_Group) :: group
    INTEGER :: my_rank

    CALL MPI_Init()

    ! Get the group of processes from the default communicator
    CALL MPI_Comm_group(MPI_COMM_WORLD, group)

    ! Get my rank in that group.
    CALL MPI_Group_rank(group, my_rank)

    ! Print my rank in that group.
    WRITE(*,'(A,I0,A)') 'I am MPI process ', my_rank, ' in the group.'

    CALL MPI_Finalize()
END PROGRAM main
