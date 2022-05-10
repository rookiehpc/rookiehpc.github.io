!> @brief Illustrate how to obtain the address of an element.
PROGRAM main
    USE mpi_f08

    IMPLICIT NONE

    INTEGER(KIND=MPI_ADDRESS_KIND) :: addr
    INTEGER :: a(0:9)

    CALL MPI_Init()

    CALL MPI_Get_address(a(2), addr)
    WRITE(*,'(A,I0,A)') 'The address of the third element is ', addr, '.'

    CALL MPI_Finalize()
END PROGRAM main
