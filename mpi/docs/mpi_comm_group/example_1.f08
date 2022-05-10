!> @brief Illustrates how to obtain the group of processes of a communicator.
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
