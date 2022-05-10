!> @brief Illustrate how to use the MPI_Count datatype.
!> @details This application gets the size of an MPI Datatype via MPI_Count.
PROGRAM main
    USE mpi_f08

    IMPLICIT NONE

    TYPE(MPI_Datatype) :: my_type
    INTEGER(KIND=MPI_COUNT_KIND) :: count

    CALL MPI_Init()

    ! Create the MPI datatype
    CALL MPI_Type_contiguous(10, MPI_INTEGER, my_type)
    CALL MPI_Type_commit(my_type)

    ! Retrieve the size of the MPI datatype created
    CALL MPI_Type_size_x(my_type, count)
    WRITE(*,'(A,I0,A)') 'The type created would generate a message of ', count, ' bytes.'

    CALL MPI_Finalize()
END PROGRAM main
