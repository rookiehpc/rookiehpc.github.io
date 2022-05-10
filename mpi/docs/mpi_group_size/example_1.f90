!> @brief Display the number of MPI processes in the group of the default
!> communicator MPI_COMM_WORLD.
!> @details This code obtains the group from the default communicator, then
!> queries that group to know how many processes are part of it.
PROGRAM main
    USE mpi

    IMPLICIT NONE

    INTEGER :: ierror
    INTEGER :: group
    INTEGER :: size

    CALL MPI_Init(ierror)

    ! Get the group from the default communicator
    CALL MPI_Comm_group(MPI_COMM_WORLD, group, ierror)

    ! Get the size of the group
    CALL MPI_Group_size(group, size, ierror)

    ! Each process prints the number of processes in that group
    WRITE(*,'(A,I0,A)') 'We are ', size, ' MPI processes in the group of the default communicator MPI_COMM_WORLD.'

    CALL MPI_Finalize(ierror)
END PROGRAM main