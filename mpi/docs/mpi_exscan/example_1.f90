!> @brief Illustrate how to use an MPI_Exscan.
!> @details This program uses MPI_Exscan to compute a progressive sum of ranks. It
!> can be visualised as follows:
!>
!> +---------------+   +---------------+   +---------------+   +---------------+
!> | MPI process 0 |   | MPI process 1 |   | MPI process 2 |   | MPI process 3 |
!> +---------------+   +---------------+   +---------------+   +---------------+
!> |       0       |   |       1       |   |       2       |   |       3       |
!> +-------+-------+   +-------+-------+   +-------+-------+   +-------+-------+
!>         |                   |                   |                    
!>         |                +--+--+                |                    
!>         +----------------| SUM |                |                    
!>         |                +--+--+                |                    
!>         |                   |                +--+--+                 
!>         |                   +----------------| SUM |                 
!>         |                   |                +--+--+                 
!>         |                   |                   |                    
!>         |                   |                   |                    
!>          \                   \                   \                      
!>           \                   \                   \                  
!>            \                   \                   \                 
!>             \_______________    \_______________    \_______________ 
!>                             |                   |                   |
!>                             |                   |                   |
!> +-------+-------+   +-------+-------+   +-------+-------+   +-------+-------+
!> |   undefined   |   |       0       |   |       1       |   |       3       |
!> +---------------+   +---------------+   +---------------+   +---------------+
!> | MPI process 0 |   | MPI process 1 |   | MPI process 2 |   | MPI process 3 |
!> +---------------+   +---------------+   +---------------+   +---------------+
!>                                       
PROGRAM main
    USE mpi

    IMPLICIT NONE

    INTEGER :: ierror
    INTEGER :: my_rank
    INTEGER :: total

    CALL MPI_Init(ierror)

    !  Get my rank
    CALL MPI_Comm_rank(MPI_COMM_WORLD, my_rank, ierror)

    !  Get the sum of all ranks up to the one before mine and print it
    CALL MPI_Exscan(my_rank, total, 1, MPI_INTEGER, MPI_SUM, MPI_COMM_WORLD, ierror)

    !  The result on MPI process 0 is undefined, do not print it
    IF (my_rank .EQ. 0) THEN
        WRITE(*, '(A)') '[MPI process 0] Total = undefined.'
    ELSE
        WRITE(*,'(A,I0,A,I0,A)') '[MPI process ', my_rank, '] Total = ', total, '.'
    END IF

    CALL MPI_Finalize(ierror)
END PROGRAM main