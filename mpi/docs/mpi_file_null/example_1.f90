!> @brief Illustrates the reset to MPI_FILE_NULL of MPI file handles.
!> @details This application consists in opening a file, closing it and checking
!> that the file handle is reset to MPI_FILE_NULL.
PROGRAM main
    USE mpi

    IMPLICIT NONE

    INTEGER :: ierror
    INTEGER :: my_rank
    INTEGER :: handle
    INTEGER :: access_mode

    CALL MPI_Init(ierror)

    CALL MPI_Comm_rank(MPI_COMM_WORLD, my_rank, ierror)

    access_mode = MPI_MODE_CREATE ! Create the file if it does not exist
    access_mode = access_mode + MPI_MODE_RDWR ! With read / write access

    CALL MPI_File_open(MPI_COMM_WORLD, 'file.tmp', access_mode, MPI_INFO_NULL, handle, ierror)
    IF (ierror .NE. MPI_SUCCESS) THEN
        WRITE(*, '(A,I0,A)') '[MPI process ', my_rank, '] Failure in opening the file.'
        CALL MPI_Abort(MPI_COMM_WORLD, -1, ierror)
    END IF

    WRITE(*, '(A,I0,A)') '[MPI process ', my_rank, '] File opened successfully.'

    CALL MPI_File_close(handle, ierror)
    IF (ierror .NE. MPI_SUCCESS) THEN
        WRITE(*, '(A,I0,A)') '[MPI process ', my_rank, '] Failure in closing the file.'
        CALL MPI_Abort(MPI_COMM_WORLD, -1, ierror)
    END IF

    WRITE(*, '(A,I0,A)') '[MPI process ', my_rank, '] File closed successfully.'

    IF (handle .EQ. MPI_FILE_NULL) THEN
        WRITE(*, '(A,I0,A)') '[MPI process ', my_rank, '] File handle now equal to MPI_FILE_NULL.'
    ELSE
        WRITE(*, '(A,I0,A)') '[MPI process ', my_rank, '] File handle different from MPI_FILE_NULL.'
    END IF

    CALL MPI_Finalize(ierror)
END PROGRAM main