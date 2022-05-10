!> @brief Illustrates how to create a group by including ranges of processes
!> from another group.
!> @details This application is meant to be run with 4 processes. It begins with
!> the default group made of all processes and then creates another group from
!> it, keeping only odd process ranks.
PROGRAM main
    USE mpi

    IMPLICIT NONE

    INTEGER :: ierror
    INTEGER :: comm_size
    INTEGER :: world_group
    INTEGER :: my_world_group_rank
    INTEGER :: odd_group
    INTEGER, DIMENSION(0:2, 1) :: rank_ranges
    INTEGER :: my_odd_group_rank

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

    ! Selecting odd ranks means selecting every 2 ranks starting from 1, until the max rank <comm_size - 1>.
    rank_ranges = reshape([1, comm_size-1, 2], shape(rank_ranges))
    CALL MPI_Group_range_incl(world_group, 1, rank_ranges, odd_group, ierror)

    ! Get my rank in the odd group
    CALL MPI_Group_rank(odd_group, my_odd_group_rank, ierror)

    ! Continue only if we are part of the odd group
    IF (my_odd_group_rank .NE. MPI_UNDEFINED) THEN
        WRITE(*,'(A,I0,A,I0,A)') 'I am process ', my_world_group_rank, ' in world group and ', &
                                  my_odd_group_rank, ' in the odd group.'
    ELSE
        WRITE(*,'(A,I0,A)') 'I am process ', my_world_group_rank, ' in world group but I am not part of the odd group.'
    END IF

    CALL MPI_Finalize(ierror)
END PROGRAM main