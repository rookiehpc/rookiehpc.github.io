!> @brief Illustrate how to apply a displacement to an address.
!> @details This application takes the address of an element in an array, then
!> gets the address of another element by applying a displacement to the address
!> of the first element taken.
PROGRAM main
    USE mpi_f08

    IMPLICIT NONE

    INTEGER :: a(0:9)
    INTEGER(KIND=MPI_ADDRESS_KIND) :: address_third_element
    INTEGER(KIND=MPI_ADDRESS_KIND) :: displacement
    INTEGER(KIND=MPI_ADDRESS_KIND) :: address_fifth_element

    CALL MPI_Init()

    CALL MPI_Get_address(a(2), address_third_element)
    displacement = SIZEOF(a(0)) * 2
    address_fifth_element = MPI_Aint_add(address_third_element, displacement)

    WRITE(*,'(A,I0,A)') 'The address of the 3th element is ', address_third_element, '.'
    WRITE(*,'(A,I0,A)') 'The address of the 5th element is ', address_fifth_element, '.'

    CALL MPI_Finalize()
END PROGRAM main
