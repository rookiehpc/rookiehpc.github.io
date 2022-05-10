!> @brief Illustrate how to use the MPI_Count datatype.
!> @details This application gets the size of an MPI Datatype via MPI_Count.
PROGRAM main
    USE mpi

    IMPLICIT NONE

    INTEGER :: ierror
    INTEGER :: my_type
    INTEGER(KIND=MPI_COUNT_KIND) :: count

    CALL MPI_Init(ierror)

    ! Create the MPI datatype
    CALL MPI_Type_contiguous(10, MPI_INTEGER, my_type, ierror)
    CALL MPI_Type_commit(my_type, ierror)

    ! Retrieve the size of the MPI datatype created
    CALL MPI_Type_size_x(my_type, count, ierror)
    WRITE(*,'(A,I0,A)') 'The type created would generate a message of ', count, ' bytes.'

    CALL MPI_Finalize(ierror)
END PROGRAM main