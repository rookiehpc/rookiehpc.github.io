#include <stdio.h>
#include <stdlib.h>
#include <mpi.h>

/**
 * @brief Illustrates the use MPI_COMM_SELF.
 * @details This application is meant to be used with 2 MPI processes. A server
 * opening a connection and a client connecting to the server process. To accept
 * a connection, the server must issue a collective operation where 
 * MPI_COMM_SELF can be passed to restrict that collective operation to the
 * server process only.
 **/
int main(int argc, char* argv[])
{
    MPI_Init(&argc, &argv);

    // Size of the default communicator
    int size;
    MPI_Comm_size(MPI_COMM_WORLD, &size);
    if(size != 2)
    {
        printf("This application is meant to be run with 2 MPI processes.\n");
        MPI_Abort(MPI_COMM_WORLD, EXIT_FAILURE);
    }

    // Get my rank and do the corresponding job
    enum role_ranks { CLIENT, SERVER };
    int my_rank;
    MPI_Comm_rank(MPI_COMM_WORLD, &my_rank);
    switch(my_rank)
    {
        case CLIENT:
        {
            MPI_Comm server; 
            char name[MPI_MAX_PORT_NAME]; 
            printf("Enter port name: ");  
            scanf("%s", name); 
            MPI_Comm_connect(name, MPI_INFO_NULL, 0, MPI_COMM_SELF, &server);
            printf("I am connected!\n");
            MPI_Comm_disconnect(&server);
            break;
        }
        case SERVER:
        {
            char my_port[MPI_MAX_PORT_NAME]; 
            MPI_Comm client; 
            MPI_Open_port(MPI_INFO_NULL, my_port); 
            printf("Port name is: %s\n", my_port);
            MPI_Comm_accept(my_port, MPI_INFO_NULL, 0, MPI_COMM_SELF, &client);
            printf("Incoming connection accepted!\n");
            MPI_Comm_disconnect(&client);
            MPI_Close_port(my_port);
            break;
        }
    }

    MPI_Finalize();

    return EXIT_SUCCESS;
}