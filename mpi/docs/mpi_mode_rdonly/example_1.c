#include <stdio.h>
#include <stdlib.h>
#include <mpi.h>

/**
 * @brief Illustrates how to specify that a file must be open in read-only mode.
 * @details Since MPI_MODE_RDONLY is incompatible with MPI_MODE_CREATE and
 * MPI_MODE_EXCL, the file to open must be existing already. Therefore, this
 * application contains a preparatory phase where the MPI process 0 creates an
 * empty file so that the file exists by the time the MPI_File_open is issued.
 **/
int main(int argc, char* argv[])
{
    MPI_Init(&argc, &argv);

    int my_rank;
    MPI_Comm_rank(MPI_COMM_WORLD, &my_rank);

    // PREPARATORY PHASE: MPI process 0 creates the file
    if(my_rank == 0)
    {
        // If the file does not exist, it will be created automatically
        FILE* tmp_file = fopen("file.tmp", "w");
        if(!tmp_file)
        {
            printf("[MPI process 0] Cannot create the file 'file.tmp'.\n");
            MPI_Abort(MPI_COMM_WORLD, EXIT_FAILURE);
        }
        fclose(tmp_file);
    }
    MPI_Barrier(MPI_COMM_WORLD);
    // END OF PREPARATORY PHASE

    MPI_File handle;
    int access_mode = MPI_MODE_RDONLY; /* With read-only access */
    if(MPI_File_open(MPI_COMM_WORLD, "file.tmp", access_mode, MPI_INFO_NULL, &handle) != MPI_SUCCESS)
    {
        printf("[MPI process %d] Failure in opening the file.\n", my_rank);
        MPI_Abort(MPI_COMM_WORLD, EXIT_FAILURE);
    }

    printf("[MPI process %d] File opened successfully.\n", my_rank);

    if(MPI_File_close(&handle) != MPI_SUCCESS)
    {
        printf("[MPI process %d] Failure in closing the file.\n", my_rank);
        MPI_Abort(MPI_COMM_WORLD, EXIT_FAILURE);
    }

    printf("[MPI process %d] File closed successfully.\n", my_rank);

    MPI_Finalize();

    return EXIT_SUCCESS;
}