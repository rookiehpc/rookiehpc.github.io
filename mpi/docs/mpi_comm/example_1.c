#include <stdio.h>
#include <stdlib.h>
#include <mpi.h>

/**
 * @brief Get information of a communicator.
 **/
int main(int argc, char* argv[])
{
    MPI_Init(&argc, &argv);

    MPI_Comm comm = MPI_COMM_WORLD;
    int comm_size;
    MPI_Comm_size(comm, &comm_size);
    char name[MPI_MAX_OBJECT_NAME];
    int name_length;
    MPI_Comm_get_name(comm, name, &name_length);
    printf("The communicator %s contains %d MPI processes.\n", name, comm_size);

    MPI_Finalize();

    return EXIT_SUCCESS;
}
