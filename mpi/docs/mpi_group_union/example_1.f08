!> @brief Illustrates how to get the union of two groups of processes.
!> @details This code gets all processes of the default communicator and splits
!> them in two groups, designed to cover all cases: processes that belong to
!> both groups, one group or none.
!> It then gets the union of these two groups and creates a communicator
!> containing the processes of the union group. Each process then prints whether
!> it belongs to the communicator of the union group or not.
!> 
!> This application is meant to be run with 4 processes. The union can
!> be visualised as follows:
!>
!> +-----------+---+---+---+---+
!> | Processes | 0 | 1 | 2 | 3 |
!> +-----------+---+---+---+---+
!> | Group A   | X |   | X |   |
!> | Group B   |   |   | X | X |
!> | Union     | X |   | X | X |
!> +-----------+---+---+---+---+
PROGRAM main
    USE mpi_f08

    IMPLICIT NONE

    INTEGER :: size
    TYPE(MPI_Group) :: world_group
    TYPE(MPI_Group) :: group_a
    INTEGER :: group_a_processes(0:1)
    TYPE(MPI_Group) :: group_b
    INTEGER :: group_b_processes(0:1)
    TYPE(MPI_Group) :: union_group
    INTEGER :: my_rank
    TYPE(MPI_Comm) :: new_communicator

    CALL MPI_Init()

    ! Check that the application is run with 4 processes.
    CALL MPI_Comm_size(MPI_COMM_WORLD, size)
    IF (size .NE. 4) THEN
        WRITE(*,'(A)') 'Please run this application with 4 processes.'
        CALL MPI_Abort(MPI_COMM_WORLD, -1)
    END IF

    ! Get the group from the default communicator
    CALL MPI_Comm_group(MPI_COMM_WORLD, world_group)

    ! Keep the processes 0 and 2 in the group A
    group_a_processes = [0, 2]
    CALL MPI_Group_incl(world_group, 2, group_a_processes, group_a)

    ! Keep the processes 2 and 3 in the group B
    group_b_processes = [2, 3]
    CALL MPI_Group_incl(world_group, 2, group_b_processes, group_b)

    ! Get the union of both groups
    CALL MPI_Group_union(group_a, group_b, union_group)

    ! Get my rank in the communicator
    CALL MPI_Comm_rank(MPI_COMM_WORLD, my_rank)

    ! Create a communicator made of the processes in the union group
    CALL MPI_Comm_create(MPI_COMM_WORLD, union_group, new_communicator)
    IF (new_communicator .EQ. MPI_COMM_NULL) THEN
        ! I am not part of the communicator created, so I am not part of the union group
        WRITE(*,'(A,I0,A)') 'Process ', my_rank, ' is not part of the union group.'
    ELSE
        ! I am part of the communicator created, so I am part of the union group
        WRITE(*,'(A,I0,A)') 'Process ', my_rank, ' is part of the union group.'
    END IF

    CALL MPI_Finalize()
END PROGRAM main
