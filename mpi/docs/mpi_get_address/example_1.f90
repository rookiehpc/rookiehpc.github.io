!> @brief Illustrate how to obtain the address of an element.
PROGRAM main
    USE mpi

    IMPLICIT NONE

    INTEGER :: ierror
    INTEGER(KIND=MPI_ADDRESS_KIND) :: addr
    INTEGER :: a(0:9)

    CALL MPI_Init(ierror)

    CALL MPI_Get_address(a(2), addr, ierror)
    WRITE(*,'(A,I0,A)') 'The address of the third element is ', addr, '.'

    CALL MPI_Finalize(ierror)
END PROGRAM main