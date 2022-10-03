!> @brief Illustrates how to create a communicator.
!> @details This code extracts the group of processes in the default
!> communicator, then it keeps only the first 2 processes and creates a group
!> containing only these two. Finally, it creates a communicator based on that
!> group of 2, and proceeds to two broadcasts: one in the global communicator,
!> one in the communicator just created.
!> This application is meant to be run with at least 3 processes.
PROGRAM main
    USE mpi

    IMPLICIT NONE

    INTEGER :: ierror
    INTEGER :: size
    INTEGER :: world_group
    INTEGER :: ranks(0:1)
    INTEGER :: new_group
    INTEGER :: new_communicator
    INTEGER :: my_rank
    INTEGER :: value

    CALL MPI_Init(ierror)

    ! Check that the application is run with at least 3 processes.
    CALL MPI_Comm_size(MPI_COMM_WORLD, size, ierror)
    IF (size .LT. 3) THEN
        WRITE(*,'(A)') 'Please run this application with at least 3 processes.'
        CALL MPI_Abort(MPI_COMM_WORLD, -1, ierror)
    END IF

    ! Get the group or processes of the default communicator
    CALL MPI_Comm_group(MPI_COMM_WORLD, world_group, ierror)

    ! Keep only the processes 0 and 1 in the new group.
    ranks = (/0, 1/)
    CALL MPI_Group_incl(world_group, 2, ranks, new_group, ierror)

    new_communicator = MPI_COMM_NULL

    CALL MPI_Comm_rank(MPI_COMM_WORLD, my_rank, ierror)
    IF ( my_rank .LT. 2 ) THEN
       ! Only processes 0 and 1 call MPI_Comm_create_group
       CALL MPI_Comm_create_group(MPI_COMM_WORLD, new_group, 0, new_communicator, ierror)
       ! The commented call below is INCORRECT - not called by all processes
       ! CALL MPI_Comm_create(MPI_COMM_WORLD, new_group, new_communicator)
    ELSE ! There is no actual need for this else statement
      CALL MPI_Comm_create_group(MPI_COMM_WORLD, MPI_GROUP_EMPTY, 0, new_communicator, ierror)
    ENDIF

    ! Do a broadcast between all processes
    CALL MPI_Comm_rank(MPI_COMM_WORLD, my_rank, ierror)
    CALL MPI_Bcast(value, 1, MPI_INTEGER, 0, MPI_COMM_WORLD, ierror)
    WRITE(*,'(A,I0,A)') 'Process ', my_rank, ' took part to the global communicator broadcast.'

    ! Let's wait all processes before proceeding to the second phase.
    CALL MPI_Barrier(MPI_COMM_WORLD, ierror)

    ! Do a broadcast only between the processes of the new communicator.
    IF (new_communicator .EQ. MPI_COMM_NULL) THEN
        ! I am not part of the new communicator, I can't participate to that broadcast.
        WRITE(*,'(A,I0,A)') 'Process ', my_rank, ' did not take part to the new communicator broadcast.'
    ELSE
        ! I am part of the new communicator, I can participate to that broadcast.
        CALL MPI_Bcast(value, 1, MPI_INTEGER, 0, new_communicator, ierror)
        WRITE(*,'(A,I0,A)') 'Process ', my_rank, ' took part to the new communicator broadcast.'
    END IF

    CALL MPI_Finalize(ierror)
END PROGRAM main
