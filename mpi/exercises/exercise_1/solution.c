#include <stdio.h>
#include <mpi.h>
 
/**
 * @brief Solution to the hello world in MPI.
 **/
int main(int argc, char* argv[])
{
    // 1) Tell MPI to start
    MPI_Init(&argc, &argv); // or MPI_Init(NULL, NULL);
 
    // 2) Get my rank
    int my_rank;
    MPI_Comm_rank(MPI_COMM_WORLD, &my_rank);
 
    // 3) Get the number of MPI processes
    int comm_size;
    MPI_Comm_size(MPI_COMM_WORLD, &comm_size);
 
    // 4) Print everything
    printf("\"Hello World!\" from MPI process %d. We are %d MPI processes.\n", my_rank, comm_size);
 
    // 5) Tell MPI to end
    MPI_Finalize();
 
    return 0;
}