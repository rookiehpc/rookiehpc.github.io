!> @brief Illustrate how to decompose MPI processes in a cartesian grid.
!> @details This application is to be run with 60 processes. It attempts to
!> decompose these 12 MPI processes in 3 dimensions (i.e: a cube).
PROGRAM main
    USE mpi_f08

    IMPLICIT NONE

    INTEGER :: size
    INTEGER :: my_rank
    INTEGER :: dims(0:2)

    CALL MPI_Init()

    ! Get total number of MPI processes
    CALL MPI_Comm_size(MPI_COMM_WORLD, size)
    IF (size .NE. 12) THEN
        WRITE(*,'(A)') 'This application is meant to be run with 12 processes.'
        CALL MPI_Abort(MPI_COMM_WORLD, -1)
    END IF

    ! Get my MPI process identifier
    CALL MPI_Comm_rank(MPI_COMM_WORLD, my_rank)

    IF (my_rank .EQ. 0) THEN
        WRITE(*,'(A,I0,A)') 'Attempting to decompose ', size, ' processes into a cube:'

        ! Give total freedom to MPI
        dims = [0, 0, 0]
        WRITE(*,'(A,I0,A,I0,A,I0,A,I0,A,I0,A,I0,A)', advance='no') &
            '- Restrictions (', dims(0), ', ', dims(1), ', ', dims(2), ') give decomposition '
        CALL MPI_Dims_create(size, 3, dims)
        WRITE(*,'(A,I0,A,I0,A,I0,A)') '(', dims(0), ', ', dims(1), ', ', dims(2), ') give decomposition '

        ! Restrict 6 processes in the first dimension
        dims = [6, 0, 0]
        WRITE(*,'(A,I0,A,I0,A,I0,A,I0,A,I0,A,I0,A)', advance='no') &
            '- Restrictions (', dims(0), ', ', dims(1), ', ', dims(2), ') give decomposition '
        CALL MPI_Dims_create(size, 3, dims)
        WRITE(*,'(A,I0,A,I0,A,I0,A)') '(', dims(0), ', ', dims(1), ', ', dims(2), ') give decomposition '

        ! Restrict 4 processes in the second dimension and 1 in the third one
        dims = [0, 4, 1]
        WRITE(*,'(A,I0,A,I0,A,I0,A,I0,A,I0,A,I0,A)', advance='no') &
            '- Restrictions (', dims(0), ', ', dims(1), ', ', dims(2), ') give decomposition '
        CALL MPI_Dims_create(size, 3, dims)
        WRITE(*,'(A,I0,A,I0,A,I0,A)') '(', dims(0), ', ', dims(1), ', ', dims(2), ') give decomposition '
    END IF

    CALL MPI_Finalize()
END PROGRAM main
