!> @brief Illustrates how to retrieve the cartesian topology information of a
!> communicator.
!> @details This code creates a new communicator with a cartesian topology. It
!> then retrieves the cartesian topology information and compares them with the
!> information given at creation.
PROGRAM main
    USE mpi_f08

    IMPLICIT NONE

    INTEGER :: size
    INTEGER :: dimsGiven(0:1)
    LOGICAL :: periodsGiven(0:1)
    LOGICAL :: reorderGiven
    TYPE(MPI_Comm) :: new_communicator
    INTEGER :: dimsRetrieved(0:1)
    LOGICAL :: periodsRetrieved(0:1)
    INTEGER :: my_coords(0:1)
    INTEGER :: my_rank

    CALL MPI_Init()

    ! Size of the default communicator
    CALL MPI_Comm_size(MPI_COMM_WORLD, size)

    ! Ask MPI to decompose our processes in a 2D cartesian grid for us
    dimsGiven = [0, 0]
    CALL MPI_Dims_create(size, 2, dimsGiven)

    ! Make both dimensions periodic
    periodsGiven = [.TRUE., .FALSE.]

    ! Let MPI assign arbitrary ranks if it deems it necessary
    reorderGiven = .TRUE.

    ! Create a communicator given the 2D torus topology.
    CALL MPI_Cart_create(MPI_COMM_WORLD, 2, dimsGiven, periodsGiven, reorderGiven, new_communicator)

    ! Retrieve cartesian topology information
    CALL MPI_Cart_get(new_communicator, 2, dimsRetrieved, periodsRetrieved, my_coords)

    ! Master prints a comparison between dims / periods given and retrieved.
    CALL MPI_Comm_rank(MPI_COMM_WORLD, my_rank)
    IF (my_rank .EQ. 0) THEN
        WRITE(*,'(A,I0,A,I0,A,A,I0,A,I0,A)') 'Dims given (', dimsGiven(0), ', ', dimsGiven(1), ')', &
                                           ' vs (', dimsRetrieved(0), ', ', dimsRetrieved(1), ') retrieved.'
        WRITE(*,'(A,L0,A,L0,A,A,L0,A,L0,A)') 'Periods given (', periodsGiven(0), ', ', periodsGiven(1), ')', &
                                           ' vs (', periodsRetrieved(0), ', ', periodsRetrieved(1), ') retrieved.'
    END IF

    CALL MPI_Finalize()
END PROGRAM main
