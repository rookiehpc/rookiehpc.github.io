!> @brief Illustrates how to run a user-defined operation for reduction.
!> @details This application consists of 3 MPI processes that participate to a
!> sum reduction using a user-defined function for the sum. Each MPI process 
!> sends two integers for reduction:
!> 1) its rank
!> 2) its rank plus the communicator size.
!> It can be visualised as follows:
!>
!>         'inputBuffer'     'inputBuffer'     'inputBuffer'
!>              on                on                on
!>         MPI process 0     MPI process 1     MPI process 2     'outputBuffer'                    
!>     ^ +---------------+ +---------------+ +---------------+ +---------------+
!>     | |       0       | |       1       | |       2       | |   0+1+2 = 3   |
!> len | +---------------+ +---------------+ +---------------+ +---------------+
!>     | |       3       | |       4       | |       5       | |   3+4+5 = 12  |
!>     v +---------------+ +---------------+ +---------------+ +---------------+
PROGRAM main
    USE mpi_f08

    IMPLICIT NONE

    INTERFACE
        !> @brief User-defined version of a sum function for reduction.
        !> @param(in) inputBuffer A pointer on the buffer providing the inputs of an
        !> MPI process.
        !> @param(inout) outputBuffer A pointer on the buffer in which write the
        !> reduction results.
        !> @param(in) len The number of elements on which the reduction applies. This is
        !> not the number of MPI processes in the communicator but the 'count' argument
        !> passed to the reduction call.
        SUBROUTINE my_sum_function_template(inputBuffer, outputBuffer, len, datatype)
            USE, INTRINSIC :: ISO_C_BINDING, ONLY : C_PTR, C_F_POINTER
            USE mpi_f08

            IMPLICIT NONE

            TYPE(C_PTR), VALUE :: inputBuffer, outputBuffer
            INTEGER :: len
            TYPE(MPI_Datatype) :: datatype
            INTEGER, POINTER :: inputBuffer_r(:), outputBuffer_r(:)
        END SUBROUTINE my_sum_function_template
    END INTERFACE

    INTEGER :: size
    INTEGER :: root_rank = 0
    INTEGER :: my_rank
    TYPE(MPI_Op) :: operation
    INTEGER :: data(2)
    INTEGER :: reduction_results(2)
    PROCEDURE(my_sum_function_template), POINTER :: my_function

    CALL MPI_Init()

    ! Get the number of processes and check only 4 are used.
    CALL MPI_Comm_size(MPI_COMM_WORLD, size)
    IF (size .NE. 3) THEN
        WRITE(*, '(A)') 'This application is meant to be run with 3 processes.'
        CALL MPI_Abort(MPI_COMM_WORLD, -1)
    END IF

    ! Get my rank
    CALL MPI_Comm_rank(MPI_COMM_WORLD, my_rank)

    ! Create the operation handle
    my_function => my_sum_function
    CALL MPI_Op_create(my_function, .TRUE., operation)

    ! Initialise the data to send
    data = [ my_rank, my_rank + size ]

    ! Each MPI process sends its rank to reduction, root MPI process collects the result
    reduction_results = [ 0, 0 ]
    CALL MPI_Reduce(data, reduction_results, 2, MPI_INTEGER, operation, root_rank, MPI_COMM_WORLD)

    IF (my_rank .EQ. root_rank) THEN
        WRITE(*, '(A,I0,A)') 'The sum of first elements of data is ', reduction_results(1), '.'
        WRITE(*, '(A,I0,A)') 'The sum of second elements of data is ', reduction_results(2), '.'
    END IF

    ! Free the operation handle created
    CALL MPI_Op_free(operation)

    CALL MPI_Finalize()

CONTAINS

    SUBROUTINE my_sum_function(inputBuffer, outputBuffer, len, datatype)
        USE mpi_f08

        IMPLICIT NONE

        TYPE(C_PTR), VALUE :: inputBuffer, outputBuffer
        INTEGER :: len
        TYPE(MPI_Datatype) :: datatype
        INTEGER, POINTER :: input(:), output(:)

        IF (datatype%MPI_VAL .EQ. MPI_INTEGER%MPI_VAL) THEN
            CALL C_F_POINTER(inputBuffer, input, [ len ] )
            CALL C_F_POINTER(outputBuffer, output, [ len ] )
            output = input + output
        END IF
    END SUBROUTINE my_sum_function
END PROGRAM main
