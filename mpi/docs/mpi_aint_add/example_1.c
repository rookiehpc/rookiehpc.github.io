#include <stdio.h>
#include <stdlib.h>
#include <mpi.h>

/**
 * @brief Illustrate how to apply a displacement to an address.
 * @details This application takes the address of an element in an array, then
 * gets the address of another element by applying a displacement to the address
 * of the first element taken.
 **/
int main(int argc, char* argv[])
{
    MPI_Init(&argc, &argv);

    int a[10];

    MPI_Aint address_third_element;
    MPI_Get_address(&a[2], &address_third_element);

    MPI_Aint displacement = sizeof(int) * 2;
    MPI_Aint address_fifth_element;
    address_fifth_element = MPI_Aint_add(address_third_element, displacement);

    printf("The address of the 3th element is %ld.\n", address_third_element);
    printf("The address of the 5th element is %ld.\n", address_fifth_element);

    MPI_Finalize();

    return EXIT_SUCCESS;
}
