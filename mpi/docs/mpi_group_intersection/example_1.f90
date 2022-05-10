!> @brief Illustrates how to get the intersection of two groups of processes.
!> @details This code gets all processes of the default communicator and splits
!> them in two groups, designed to cover all cases: processes that belong to
!> both groups, one group or none.
!> It then gets the intersection of these two groups and creates a communicator
!> containing the processes of the intersection group. Each process then prints
!> whether it belongs to the communicator of the intersection group or not.
!> 
!> This application is meant to be run with 4 processes. The intersection can
!> be visualised as follows:
!>
!> +--------------+---+---+---+---+
!> | Processes    | 0 | 1 | 2 | 3 |
!> +--------------+---+---+---+---+
!> | Group A      | X |   | X |   |
!> | Group B      |   |   | X | X |
!> | Intersection |   |   | X |   |
!> +--------------+---+---+---+---+
PROGRAM main
    USE mpi

    IMPLICIT NONE

    INTEGER :: ierror
    INTEGER :: size
    INTEGER :: world_group
    INTEGER :: group_a
    INTEGER :: group_a_processes(0:1)
    INTEGER :: group_b
    INTEGER :: group_b_processes(0:1)
    INTEGER :: intersection_group
    INTEGER :: my_rank
    INTEGER :: new_communicator

    CALL MPI_Init(ierror)

    ! Check that the application is run with 4 processes.
    CALL MPI_Comm_size(MPI_COMM_WORLD, size, ierror)
    IF (size .NE. 4) THEN
        WRITE(*,'(A)') 'Please run this application with 4 processes.'
        CALL MPI_Abort(MPI_COMM_WORLD, -1, ierror)
    END IF

    ! Get the group from the default communicator
    CALL MPI_Comm_group(MPI_COMM_WORLD, world_group, ierror)

    ! Keep the processes 0 and 2 in the group A
    group_a_processes = (/0, 2/)
    CALL MPI_Group_incl(world_group, 2, group_a_processes, group_a, ierror)

    ! Keep the processes 2 and 3 in the group B
    group_b_processes = (/2, 3/)
    CALL MPI_Group_incl(world_group, 2, group_b_processes, group_b, ierror)

    ! Get the intersection of both groups
    CALL MPI_Group_intersection(group_a, group_b, intersection_group, ierror)

    ! Get my rank in the communicator
    CALL MPI_Comm_rank(MPI_COMM_WORLD, my_rank, ierror)

    ! Create a communicator made of the processes in the intersection group
    CALL MPI_Comm_create(MPI_COMM_WORLD, intersection_group, new_communicator, ierror)
    IF (new_communicator .EQ. MPI_COMM_NULL) THEN
        ! I am not part of the communicator created, so I am not part of the intersection group
        WRITE(*,'(A,I0,A)') 'Process ', my_rank, ' is not part of the intersection group.'
    ELSE
        ! I am part of the communicator created, so I am part of the intersection group
        WRITE(*,'(A,I0,A)') 'Process ', my_rank, ' is part of the intersection group.'
    END IF

    CALL MPI_Finalize(ierror)
END PROGRAM main