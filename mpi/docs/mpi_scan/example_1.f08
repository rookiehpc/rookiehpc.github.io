!> @brief Illustrate how to use an MPI_Scan.
!> @details This program uses MPI_Scan to compute a progressive sum of ranks. It
!> can be visualised as follows:
!>
!> +---------------+   +---------------+   +---------------+   +---------------+
!> | MPI process 0 |   | MPI process 1 |   | MPI process 2 |   | MPI process 3 |
!> +---------------+   +---------------+   +---------------+   +---------------+
!> |       0       |   |       1       |   |       2       |   |       3       |
!> +-------+-------+   +-------+-------+   +-------+-------+   +-------+-------+
!>         |                   |                   |                   |
!>         |                +--+--+                |                   |
!>         +----------------| SUM |                |                   |
!>         |                +--+--+                |                   |
!>         |                   |                +--+--+                |
!>         |                   +----------------| SUM |                |
!>         |                   |                +--+--+                |
!>         |                   |                   |                +--+--+
!>         |                   |                   +----------------| SUM |
!>         |                   |                   |                +--+--+
!>         |                   |                   |                   |
!> +-------+-------+   +-------+-------+   +-------+-------+   +-------+-------+
!> |       0       |   |       1       |   |       3       |   |       6       |
!> +---------------+   +---------------+   +---------------+   +---------------+
!> | MPI process 0 |   | MPI process 1 |   | MPI process 2 |   | MPI process 3 |
!> +---------------+   +---------------+   +---------------+   +---------------+
!>                                       
PROGRAM main
	USE mpi_f08

	IMPLICIT NONE

	INTEGER :: my_rank
	INTEGER :: total

	CALL MPI_Init()

	! Get my rank
	CALL MPI_Comm_rank(MPI_COMM_WORLD, my_rank)

	! Get the sum of all ranks up to mine and print it
	CALL MPI_Scan(my_rank, total, 1, MPI_INTEGER, MPI_SUM, MPI_COMM_WORLD)
	WRITE(*, '(A,I0,A,I0,A)') '[MPI process ', my_rank, '] Total = ', total, '.'

	CALL MPI_Finalize()
END PROGRAM main