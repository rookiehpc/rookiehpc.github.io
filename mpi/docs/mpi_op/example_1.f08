!> @brief Illustrates how to use an MPI_Op handle.
!> @details This application consists of multiple reductions applied in turn.
!> An array of MPI_Op handles contains the reduction operations to run. All MPI
!> processes perform an MPI reduction for each of the operations held in that
!> array: a sum, a product and a max.
PROGRAM main
    USE mpi_f08

    IMPLICIT NONE

    INTEGER :: size
    INTEGER :: root_rank = 0
    INTEGER :: my_rank
    INTEGER, PARAMETER :: OPERATION_COUNT = 3
    TYPE(MPI_Op) :: operations(OPERATION_COUNT)
    CHARACTER(LEN=4) :: operations_label(OPERATION_COUNT)
    INTEGER :: reduction_result
    INTEGER :: i

    CALL MPI_Init()

    ! Get the number of processes and check only 4 are used.
    CALL MPI_Comm_size(MPI_COMM_WORLD, size)
    IF (size .NE. 4) THEN
        WRITE(*, '(A)') 'This application is meant to be run with 4 processes.'
        CALL MPI_Abort(MPI_COMM_WORLD, -1)
    END IF

    ! Get my rank
    CALL MPI_Comm_rank(MPI_COMM_WORLD, my_rank)

    ! Initialise the operations to apply
    operations = [ MPI_SUM, MPI_PROD, MPI_MAX ]
    operations_label = [ ' sum', 'prod', ' max' ]

    ! Each MPI process sends its rank to reduction, root MPI process collects the result
    DO i = 1, OPERATION_COUNT
        reduction_result = 0
        CALL MPI_Reduce(my_rank, reduction_result, 1, MPI_INTEGER, operations(i), root_rank, MPI_COMM_WORLD)

        IF (my_rank .EQ. root_rank) THEN
            WRITE(*, '(A,A,A,I0,A)') 'The ', operations_label(i), ' of all ranks is ', reduction_result, '.'
        END IF
    END DO

    CALL MPI_Finalize()
END PROGRAM main
