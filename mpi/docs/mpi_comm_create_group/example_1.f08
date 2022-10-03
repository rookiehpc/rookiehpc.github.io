!> @brief Illustrates how to create communicators with MPI_Comm_create_group.
!> @details This code extracts the group of processes in the default
!> communicator, then only two processes call MPI_Comm_create_group to create
!> a new communicator that contains only them. Finally, it makes two broadcast
!> calls: one in the global communicator and one in the new communicator and
!> each process reports if it participated in any of these broadcasts.
!> Note: this is the same as the example given for MPI_Comm_create but with
!> using MPI_Comm_create_group instead of MPI_Comm_create.
!> This application is meant to be run with 3 processes.
PROGRAM main
    USE mpi_f08

    IMPLICIT NONE

    INTEGER :: size
    TYPE(MPI_Group) :: world_group
    INTEGER :: ranks(0:1)
    TYPE(MPI_Group) :: new_group
    TYPE(MPI_Comm) :: new_communicator
    INTEGER :: my_rank
    INTEGER :: value

    CALL MPI_Init()

    ! Check that the application is run with at least 3 processes.
    CALL MPI_Comm_size(MPI_COMM_WORLD, size)
    IF (size .LT. 3) THEN
        WRITE(*,'(A)') 'Please run this application with at least 3 processes.'
        CALL MPI_Abort(MPI_COMM_WORLD, -1)
    END IF

    ! Get the group or processes of the default communicator
    CALL MPI_Comm_group(MPI_COMM_WORLD, world_group)

    ! Keep only the processes 0 and 1 in the new group.
    ranks = [0, 1]
    CALL MPI_Group_incl(world_group, 2, ranks, new_group)

    new_communicator = MPI_COMM_NULL

    CALL MPI_Comm_rank(MPI_COMM_WORLD, my_rank)
    IF (my_rank .LT. 2) THEN
       ! Only processes 0 and 1 call MPI_Comm_create_group
       CALL MPI_Comm_create_group(MPI_COMM_WORLD, new_group, 0, new_communicator)
    ENDIF

    ! Do a broadcast between all processes
    CALL MPI_Bcast(value, 1, MPI_INTEGER, 0, MPI_COMM_WORLD)
    WRITE(*,'(A,I0,A)') 'Process ', my_rank, ' took part to the global communicator broadcast.'

    ! Let's wait all processes before proceeding to the second phase.
    CALL MPI_Barrier(MPI_COMM_WORLD)

    ! Do a broadcast only between the processes of the new communicator.
    IF (new_communicator .EQ. MPI_COMM_NULL) THEN
        ! I am not part of the new communicator, I can't participate to that broadcast.
        WRITE(*,'(A,I0,A)') 'Process ', my_rank, ' did not take part to the new communicator broadcast.'
    ELSE
        ! I am part of the new communicator, I can participate to that broadcast.
        CALL MPI_Bcast(value, 1, MPI_INTEGER, 0, new_communicator)
        WRITE(*,'(A,I0,A)') 'Process ', my_rank, ' took part to the new communicator broadcast.'
    END IF

    CALL MPI_Finalize()
END PROGRAM main
