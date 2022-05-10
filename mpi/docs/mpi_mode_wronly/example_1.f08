!> @brief Illustrates how to specify that a file must be open in write-only mode.
PROGRAM main
    USE mpi_f08

    IMPLICIT NONE

    INTEGER :: ierror
    INTEGER :: my_rank
    TYPE(MPI_File) :: handle
    INTEGER :: access_mode

    CALL MPI_Init()

    CALL MPI_Comm_rank(MPI_COMM_WORLD, my_rank)

    access_mode = MPI_MODE_CREATE ! Create the file if it does not exist
    access_mode = access_mode + MPI_MODE_WRONLY ! With write-only access

    CALL MPI_File_open(MPI_COMM_WORLD, 'file.tmp', access_mode, MPI_INFO_NULL, handle, ierror)
    IF (ierror .NE. MPI_SUCCESS) THEN
        WRITE(*, '(A,I0,A)') '[MPI process ', my_rank, '] Failure in opening the file.'
        CALL MPI_Abort(MPI_COMM_WORLD, -1)
    END IF

    WRITE(*, '(A,I0,A)') '[MPI process ', my_rank, '] File opened successfully.'

    CALL MPI_File_close(handle, ierror)
    IF (ierror .NE. MPI_SUCCESS) THEN
        WRITE(*, '(A,I0,A)') '[MPI process ', my_rank, '] Failure in closing the file.'
        CALL MPI_Abort(MPI_COMM_WORLD, -1)
    END IF

    WRITE(*, '(A,I0,A)') '[MPI process ', my_rank, '] File closed successfully.'

    CALL MPI_Finalize()
END PROGRAM main