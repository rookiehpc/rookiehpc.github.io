#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <mpi.h>

/**
 * @brief Illustrates how to create an indexed MPI datatype.
 * @details This program is meant to be run with 2 processes: a sender and a
 * receiver. These two MPI processes will exchange a message made of a
 * structure representing a person.
 *
 * Structure of a person:
 * - age: int
 * - height: double
 * - name: char[10]
 *
 * How to represent such a structure with an MPI struct:
 *   
 *           +----------------- displacement for
 *           |        block 2: sizeof(int) + sizeof(double)
 *           |               (+ potential padding)
 *           |                         |
 *           +----- displacement for   |
 *           |    block 2: sizeof(int) |
 *           |   (+ potential padding) |
 *           |            |            |
 *  displacement for      |            |
 *    block 1: 0          |            |
 * (+ potential padding)  |            |
 *           |            |            |
 *           V            V            V
 *           +------------+------------+------------+
 *           |     age    |   height   |    name    |
 *           +------------+------------+------------+
 *            <----------> <----------> <---------->
 *               block 1      block 2      block 3
 *              1 MPI_INT  1 MPI_DOUBLE  10 MPI_CHAR
 **/

struct person_t
{
	int age;
	double height;
	char name[10];
};

int main(int argc, char* argv[])
{
	MPI_Init(&argc, &argv);

	// Get the number of processes and check only 2 processes are used
	int size;
	MPI_Comm_size(MPI_COMM_WORLD, &size);
	if(size != 2)
	{
		printf("This application is meant to be run with 2 processes.\n");
		MPI_Abort(MPI_COMM_WORLD, EXIT_FAILURE);
	}

	// Create the datatype
	MPI_Datatype person_type;
	int lengths[3] = { 1, 1, 10 };

	// Calculate displacements
	// In C, by default padding can be inserted between fields. MPI_Get_address will allow
	// to get the address of each struct field and calculate the corresponding displacement
	// relative to that struct base address. The displacements thus calculated will therefore
	// include padding if any.
	MPI_Aint displacements[3];
	struct person_t dummy_person;
	MPI_Aint base_address;
	MPI_Get_address(&dummy_person, &base_address);
	MPI_Get_address(&dummy_person.age, &displacements[0]);
	MPI_Get_address(&dummy_person.height, &displacements[1]);
	MPI_Get_address(&dummy_person.name[0], &displacements[2]);
	displacements[0] = MPI_Aint_diff(displacements[0], base_address);
	displacements[1] = MPI_Aint_diff(displacements[1], base_address);
	displacements[2] = MPI_Aint_diff(displacements[2], base_address);

	MPI_Datatype types[3] = { MPI_INT, MPI_DOUBLE, MPI_CHAR };
	MPI_Type_create_struct(3, lengths, displacements, types, &person_type);
	MPI_Type_commit(&person_type);

	// Get my rank and do the corresponding job
	enum rank_roles { SENDER, RECEIVER };
	int my_rank;
	MPI_Comm_rank(MPI_COMM_WORLD, &my_rank);
	switch(my_rank)
	{
		case SENDER:
		{
			// Send the message
			struct person_t buffer;
			buffer.age = 20;
			buffer.height = 1.83;
			strncpy(buffer.name, "Tom", 9);
			buffer.name[9] = '\0';
			printf("MPI process %d sends person:\n\t- age = %d\n\t- height = %f\n\t- name = %s\n", my_rank, buffer.age, buffer.height, buffer.name);
			MPI_Send(&buffer, 1, person_type, RECEIVER, 0, MPI_COMM_WORLD);
			break;
		}
		case RECEIVER:
		{
			// Receive the message
			struct person_t received;
			MPI_Recv(&received, 1, person_type, SENDER, 0, MPI_COMM_WORLD, MPI_STATUS_IGNORE);
			printf("MPI process %d received person:\n\t- age = %d\n\t- height = %f\n\t- name = %s\n", my_rank, received.age, received.height, received.name);
			break;
		}
	}

	MPI_Finalize();

	return EXIT_SUCCESS;
}
