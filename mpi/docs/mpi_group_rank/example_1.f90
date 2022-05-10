!> @brief For each process in the group of the default communicator
!> MPI_COMM_WORLD, show their rank.
PROGRAM main
    USE mpi

    IMPLICIT NONE

    INTEGER :: ierror
    INTEGER :: group
    INTEGER :: my_rank

    CALL MPI_Init(ierror)

    ! Get the group of processes from the default communicator
    CALL MPI_Comm_group(MPI_COMM_WORLD, group, ierror)

    ! Get my rank in that group.
    CALL MPI_Group_rank(group, my_rank, ierror)

    ! Print my rank in that group.
    WRITE(*,'(A,I0,A)') 'I am MPI process ', my_rank, ' in the group.'

    CALL MPI_Finalize(ierror)
END PROGRAM main