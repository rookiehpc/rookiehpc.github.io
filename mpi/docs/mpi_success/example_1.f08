!> @brief Uses the MPI_SUCCESS constant to check the success of any MPI routine.
PROGRAM main
    USE mpi_f08

    IMPLICIT NONE

    INTEGER :: ierror
    INTEGER :: size

    CALL MPI_Init(ierror)
    IF (ierror .EQ. MPI_SUCCESS) THEN
        WRITE(*,'(A)') 'The MPI routine MPI_Init succeeded.'
    ELSE
        WRITE(*,'(A)') 'The MPI routine MPI_Init failed.'
    END IF

    CALL MPI_Comm_size(MPI_COMM_WORLD, size, ierror)
    IF (ierror .EQ. MPI_SUCCESS) THEN
        WRITE(*,'(A)') 'The MPI routine MPI_Comm_size succeeded.'
    ELSE
        WRITE(*,'(A)') 'The MPI routine MPI_Comm_size failed.'
    END IF

    CALL MPI_Finalize(ierror)
    IF (ierror .EQ. MPI_SUCCESS) THEN
        WRITE(*,'(A)') 'The MPI routine MPI_Finalize succeeded.'
    ELSE
        WRITE(*,'(A)') 'The MPI routine MPI_Finalize failed.'
    END IF
END PROGRAM main
