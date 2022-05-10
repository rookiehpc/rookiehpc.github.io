!> @brief Illustrates
!> @details MPI processes split into two groups depending on whether their rank
!> is even.
!>
!> +----------------+---+---+---+---+
!> | MPI processes  | 0 | 1 | 2 | 3 |
!> +----------------+---+---+---+---+
!> | MPI_COMM_WORLD | X | X | X | X |
!> | Subgroup A     | X |   | X |   |
!> | Subgroup B     |   | X |   | X |
!> +----------------+---+---+---+---+
!>
!> In subcommunicator A, MPI processes are assigned ranks in the same order as
!> their rank in the global communicator.
!> In subcommunicator B, MPI processes are assigned ranks in the opposite order
!> as their rank in the global communicator.
PROGRAM main
    USE mpi

    IMPLICIT NONE 

    INTEGER :: ierror
    INTEGER :: comm_size
    INTEGER :: my_rank
    CHARACTER(LEN=1) :: subcommunicator
    INTEGER :: colour
    INTEGER :: key
    INTEGER :: new_comm
    INTEGER :: my_new_comm_rank

    CALL MPI_Init(ierror)

    ! Check that 4 MPI processes are used
    CALL MPI_Comm_size(MPI_COMM_WORLD, comm_size, ierror)
    IF (comm_size .NE. 4) THEN
        WRITE(*, '(A,I0,A)') 'This application is meant to be run with 4 MPI processes, not ', comm_size, '.'
        CALL MPI_Abort(MPI_COMM_WORLD, -1, ierror)
    END IF

    ! Get my rank in the global communicator
    CALL MPI_Comm_rank(MPI_COMM_WORLD, my_rank, ierror)

    ! Determine the colour and key based on whether my rank is even.
    IF (MODULO(my_rank, 2) .EQ. 0) THEN
        subcommunicator = 'A'
        colour = 0
        key = my_rank
    ELSE
        subcommunicator = 'B'
        colour = 1
        key = comm_size - my_rank
    END IF

    ! Split de global communicator
    CALL MPI_Comm_split(MPI_COMM_WORLD, colour, key, new_comm, ierror)

    ! Get my rank in the new communicator
    CALL MPI_Comm_rank(new_comm, my_new_comm_rank, ierror)

    ! Print my new rank and new communicator
    WRITE(*, '(A,I0,A,I0,A,A1,A)') '[MPI process ', my_rank, '] I am now MPI process ', my_new_comm_rank, &
        ' in subcommunicator ', subcommunicator, '.'

    CALL MPI_Finalize(ierror)
END PROGRAM main