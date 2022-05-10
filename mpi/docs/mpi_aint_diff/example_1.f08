!> @brief Illustrate how to get the difference between addresses.
!> @details This application takes the address of two elements in an array and
!> calculates the difference between the two before printing the difference.
PROGRAM main
    USE mpi_f08

    IMPLICIT NONE

    INTEGER :: a(0:9)
    INTEGER(KIND=MPI_ADDRESS_KIND) :: addr_1
    INTEGER(KIND=MPI_ADDRESS_KIND) :: addr_2
    INTEGER(KIND=MPI_ADDRESS_KIND) :: addr_gap

    CALL MPI_Init()

    CALL MPI_Get_address(a(2), addr_1)
    CALL MPI_Get_address(a(8), addr_2)
    addr_gap = MPI_Aint_diff(addr_2, addr_1)

    WRITE(*,'(A,A,I0,A)') 'Difference between the address of the 3rd int and 9th int', &
                        ' is ', addr_gap, ' bytes.'

    CALL MPI_Finalize()
END PROGRAM main
