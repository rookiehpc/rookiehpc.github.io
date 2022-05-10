!> @brief Illustrate how to create a group by difference.
!> @details This application is meant to be run with 4 processes. It creates two 
!> groups, named A and B, which contain processes 0,1 and 1,3 respectively. It
!> then creates a group that is the difference of groups A and B (all processes
!> belong to the first group that are not in the second one). It can be 
!> visualised as follows:
!> 
!>                    +---+---+---+---+
!>                    | 0 | 1 | 2 | 3 |
!> +------------------+---+---+---+---+
!> | Group A          | X | X |   |   |
!> | Group B          |   | X |   | X |
!> | Difference group | X |   |   |   |
!> +------------------+---+---+---+---+
PROGRAM main
    USE mpi

    IMPLICIT NONE

    INTEGER :: ierror
    INTEGER :: comm_size
    INTEGER :: world_group
    INTEGER :: my_world_group_rank
    INTEGER :: group_a
    INTEGER :: ranks_group_a(0:1)
    INTEGER :: group_b
    INTEGER :: ranks_group_b(0:1)
    INTEGER :: difference_group
    INTEGER :: my_difference_group_rank

    CALL MPI_Init(ierror)

    ! Check that 4 MPI processes are used
    CALL MPI_Comm_size(MPI_COMM_WORLD, comm_size, ierror)
    IF (comm_size .NE. 4) THEN
        WRITE(*,'(A)') 'This application is meant to be run with 4 MPI processes.'
        CALL MPI_Abort(MPI_COMM_WORLD, -1, ierror)
    END IF

    ! Get the group or processes of the default communicator
    CALL MPI_Comm_group(MPI_COMM_WORLD, world_group, ierror)

    ! Create the group A
    ranks_group_a = (/0, 1/)
    CALL MPI_Group_incl(world_group, 2, ranks_group_a, group_a, ierror)

    ! Create the group B
    ranks_group_b = (/1, 3/)
    CALL MPI_Group_incl(world_group, 2, ranks_group_b, group_b, ierror)

    ! Create the difference of groups A and B
    CALL MPI_Group_difference(group_a, group_b, difference_group, ierror)

    ! Get my rank in the world group and the difference group
    CALL MPI_Group_rank(world_group, my_world_group_rank, ierror)
    CALL MPI_Group_rank(difference_group, my_difference_group_rank, ierror)

    IF (my_difference_group_rank .EQ. MPI_UNDEFINED) THEN
        WRITE(*,'(A,I0,A)') 'I am process ', my_world_group_rank, ' in world group but I am not part of the difference group.'
    ELSE
        WRITE(*,'(A,I0,A,A,I0,A)') 'I am process ', my_world_group_rank, ' in world group ', &
                                   'and process ', my_difference_group_rank, ' in difference group.'
    END IF

    CALL MPI_Finalize(ierror)
END PROGRAM main