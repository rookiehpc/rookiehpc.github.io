!> @brief Illustrates what is a group.
!> @details This code obtains the group of processes making the default
!> communicator, then queries that group to know how many processes are part of
!> it.
PROGRAM main
    USE mpi_f08

    IMPLICIT NONE

    TYPE(MPI_Group) :: group
    INTEGER :: size

    CALL MPI_Init()

    ! Get the group from the default communicator
    CALL MPI_Comm_group(MPI_COMM_WORLD, group)

    ! Get the size of the group
    CALL MPI_Group_size(group, size)

    ! Each process prints the number of processes in that group
    WRITE(*,'(A,I0,A)') 'We are ', size, ' MPI processes in the group of the default communicator MPI_COMM_WORLD.'

    CALL MPI_Finalize()
END PROGRAM main
