!> @brief Illustrates how to specify that a file must be open in read-only mode.
!> @details Since MPI_MODE_RDONLY is incompatible with MPI_MODE_CREATE and
!> MPI_MODE_EXCL, the file to open must be existing already. Therefore, this
!> application contains a preparatory phase where the MPI process 0 creates an
!> empty file so that the file exists by the time the MPI_File_open is issued.
PROGRAM main
    USE mpi

    IMPLICIT NONE

    INTEGER :: ierror
    INTEGER :: my_rank
    INTEGER :: handle
    INTEGER :: access_mode = MPI_MODE_RDONLY ! With read-only access
    LOGICAL :: file_exists

    CALL MPI_Init(ierror)

    CALL MPI_Comm_rank(MPI_COMM_WORLD, my_rank, ierror)

    ! PREPARATORY PHASE: MPI process 0 creates the file
    IF (my_rank .EQ. 0) THEN
        INQUIRE(FILE = "file.tmp", EXIST = file_exists)
        IF (.NOT. file_exists) THEN
            ! If the file does not exist, it will be created automatically
            OPEN(9, file = 'file.tmp', status = 'new')
            CLOSE(9) 
        END IF
    END IF
    CALL MPI_Barrier(MPI_COMM_WORLD, ierror);
    ! END OF PREPARATORY PHASE

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

    CALL MPI_Finalize(ierror)
END PROGRAM main