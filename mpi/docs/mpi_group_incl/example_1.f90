!> @brief Illustrates how to create a group by including processes from another
!> group.
!> @details This application is meant to be run with 4 processes. It begins with
!> the default group made of all processes and then creates another group from
!> it, keeping only processes 1 and 3.
PROGRAM main
    USE mpi

    IMPLICIT NONE

    INTEGER :: ierror
    INTEGER :: comm_size
    INTEGER :: world_group
    INTEGER :: my_world_group_rank
    INTEGER :: small_group
    INTEGER :: small_group_ranks(0:1)
    INTEGER :: my_small_group_rank

    CALL MPI_Init(ierror)

    ! Check that 4 processes are used
    CALL MPI_Comm_size(MPI_COMM_WORLD, comm_size, ierror)
    IF (comm_size .NE. 4) THEN
        WRITE(*,'(A)') 'This application is meant to be run with 4 processes.'
        CALL MPI_Abort(MPI_COMM_WORLD, -1, ierror)
    END IF

    ! Get the group or processes of the default communicator
    CALL MPI_Comm_group(MPI_COMM_WORLD, world_group, ierror)

    ! Get my rank in the world group
    CALL MPI_Group_rank(world_group, my_world_group_rank, ierror)

    ! Create the small group by including only processes 1 and 3 from the world group
    small_group_ranks = (/1, 3/)
    CALL MPI_Group_incl(world_group, 2, small_group_ranks, small_group, ierror)

    ! Get my rank in the small group
    CALL MPI_Group_rank(small_group, my_small_group_rank, ierror)

    ! Continue only if we are part of the small group
    IF (my_small_group_rank .NE. MPI_UNDEFINED) THEN
        WRITE(*,'(A,I0,A,I0,A)') 'I am process ', my_world_group_rank, ' in world group and ', &
                                  my_small_group_rank, ' in small group.'
    ELSE
        WRITE(*,'(A,I0,A)') 'I am process ', my_world_group_rank, ' in world group but I am not part of the small group.'
    END IF

    CALL MPI_Finalize(ierror)
END PROGRAM main