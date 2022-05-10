!> @brief Illustrates how to create an homogeneous indexed block MPI datatype.
!> @details This program is meant to be run with 2 processes: a sender and a
!> receiver. These two MPI processes will exchange a message made of four
!> integers. On the sender, that message is in fact made of the four corners of
!> an array it holds, which will be represented by an MPI indexed type.
!>
!>
!>     Full array          What we want
!>                            to send
!> +-----+-----+-----+  +-----+-----+-----+
!> |  0  |  1  |  2  |  |  0  |  -  |  2  |
!> +-----+-----+-----+  +-----+-----+-----+
!> |  3  |  4  |  5  |  |  -  |  -  |  -  |
!> +-----+-----+-----+  +-----+-----+-----+
!> |  6  |  7  |  8  |  |  6  |  -  |  8  |
!> +-----+-----+-----+  +-----+-----+-----+
!>
!> How to extract these elements with an homogeneous indexed block type:
!>    
!>            +---------------------------------------- displacement for
!>            |                                      block 3: 8 sizeof(INTEGER)
!>            |                                               |
!>            +---------------------------- displacement for  |
!>            |                    block 2: 6 sizeof(INTEGER) |
!>            |                                   |           |
!>            +---- displacement for              |           |
!>            |  block 2: 2 sizeof(INTEGER)       |           |
!>            |           |                       |           |
!> displacement block 1:  |                       |           |
!>    0 sizeof(INTEGER)   |                       |           |
!>            |           |                       |           |
!>            V           V                       V           |
!>            +-----+-----+-----+-----+-----+-----+-----+-----+-----+
!>            |  0  |  -  |  2  |  -  |  -  |  -  |  6  |  -  |  8  |
!>            +-----+-----+-----+-----+-----+-----+-----+-----+-----+
!>             <--->       <--->                   <--->       <--->
!>            block 1     block 2                 block 3     block 4
!>           1 element   1 element               1 element   1 element
!> 
!> Element: MPI_INTEGER
PROGRAM main
    USE mpi

    IMPLICIT NONE

    INTEGER :: ierror
    INTEGER :: size
    INTEGER, PARAMETER :: sender_rank = 0
    INTEGER, PARAMETER :: receiver_rank = 1
    INTEGER :: my_rank
    TYPE(MPI_Datatype) :: corner_type
    INTEGER(KIND=MPI_ADDRESS_KIND) :: displacements(0:3)
    INTEGER :: buffer(0:2,0:2)
    INTEGER :: received(0:3)

    CALL MPI_Init(ierror)

    ! Get the number of processes and check only 2 processes are used
    CALL MPI_Comm_size(MPI_COMM_WORLD, size, ierror)
    IF (size .NE. 2) THEN
        WRITE(*,'(A)') 'This application is meant to be run with 2 processes.'
        CALL MPI_Abort(MPI_COMM_WORLD, -1, ierror)
    END IF

    ! Get my rank and do the corresponding job
    CALL MPI_Comm_rank(MPI_COMM_WORLD, my_rank, ierror)
    SELECT CASE (my_rank)
        CASE (sender_rank)
            ! Create the datatype
            displacements = (/0 * SIZEOF(my_rank), 2 * SIZEOF(my_rank), 6 * SIZEOF(my_rank), 8 * SIZEOF(my_rank)/)
            CALL MPI_Type_create_hindexed_block(4, 1, displacements, MPI_INTEGER, corner_type, ierror)
            CALL MPI_Type_commit(corner_type, ierror)

            ! Send the message
            buffer = RESHAPE((/0, 1, 2, 3, 4, 5, 6, 7, 8/), SHAPE(buffer))
            WRITE(*,'(A,I0,A)') 'MPI process ', my_rank, ' sends values:'
            WRITE(*,'(I0,A,I0)') buffer(0,0), ' ', buffer(2,0)
            WRITE(*,'(I0,A,I0)') buffer(0,2), ' ', buffer(2,2)
            CALL MPI_Send(buffer, 1, corner_type, receiver_rank, 0, MPI_COMM_WORLD, ierror)
        CASE (receiver_rank)
            ! Receive the message
            CALL MPI_Recv(received, 4, MPI_INTEGER, sender_rank, 0, MPI_COMM_WORLD, MPI_STATUS_IGNORE, ierror)
            WRITE(*,'(A,I0,A)') 'MPI process ', my_rank, ' received values:'
            WRITE(*,'(I0,A,I0)') received(0), ' ', received(1)
            WRITE(*,'(I0,A,I0)') received(2), ' ', received(3)
    END SELECT

    CALL MPI_Finalize(ierror)
END PROGRAM main