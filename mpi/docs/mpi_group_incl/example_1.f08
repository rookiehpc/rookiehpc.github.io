!> @brief Illustrates how to create a group by including processes from another
!> group.
!> @details This application is meant to be run with 4 processes. It begins with
!> the default group made of all processes and then creates another group from
!> it, keeping only processes 1 and 3.
PROGRAM main
    USE mpi_f08

    IMPLICIT NONE

    INTEGER :: comm_size
    TYPE(MPI_Group) :: world_group
    INTEGER :: my_world_group_rank
    TYPE(MPI_Group) :: small_group
    INTEGER :: small_group_ranks(0:1)
    INTEGER :: my_small_group_rank

    CALL MPI_Init()

    ! Check that 4 processes are used
    CALL MPI_Comm_size(MPI_COMM_WORLD, comm_size)
    IF (comm_size .NE. 4) THEN
        WRITE(*,'(A)') 'This application is meant to be run with 4 processes.'
        CALL MPI_Abort(MPI_COMM_WORLD, -1)
    END IF

    ! Get the group or processes of the default communicator
    CALL MPI_Comm_group(MPI_COMM_WORLD, world_group)

    ! Get my rank in the world group
    CALL MPI_Group_rank(world_group, my_world_group_rank)

    ! Create the small group by including only processes 1 and 3 from the world group
    small_group_ranks = [1, 3]
    CALL MPI_Group_incl(world_group, 2, small_group_ranks, small_group)

    ! Get my rank in the small group
    CALL MPI_Group_rank(small_group, my_small_group_rank)

    ! Continue only if we are part of the small group
    IF (my_small_group_rank .NE. MPI_UNDEFINED) THEN
        WRITE(*,'(A,I0,A,I0,A)') 'I am process ', my_world_group_rank, ' in world group and ', &
                                  my_small_group_rank, ' in small group.'
    ELSE
        WRITE(*,'(A,I0,A)') 'I am process ', my_world_group_rank, ' in world group but I am not part of the small group.'
    END IF

    CALL MPI_Finalize()
END PROGRAM main
