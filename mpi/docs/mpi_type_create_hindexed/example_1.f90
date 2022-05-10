!> @brief Illustrates how to create a heterogeneous indexed MPI datatype.
!> @details This program is meant to be run with 2 processes: a sender and a
!> receiver. These two MPI processes will exchange a message made of six
!> integers. On the sender, that message is in fact the lower triangle of an
!> array it holds, which will be represented by an MPI indexed type.
!>
!>
!>     Full array          What we want
!>                            to send
!> +-----+-----+-----+  +-----+-----+-----+
!> |  0  |  1  |  2  |  |  0  |  -  |  -  |
!> +-----+-----+-----+  +-----+-----+-----+
!> |  3  |  4  |  5  |  |  3  |  4  |  -  |
!> +-----+-----+-----+  +-----+-----+-----+
!> |  6  |  7  |  8  |  |  6  |  7  |  8  |
!> +-----+-----+-----+  +-----+-----+-----+
!>
!> How to extract the lower triangle with a heterogeneous indexed type:
!>
!>   
!>        +---------------------------- displacement for
!>        |                          block 2: 6 sizeof(INTEGER)
!>        |                                   |
!>        +---------- displacement for        |
!>        |        block 2: 3 sizeof(INTEGER) |
!>        |                 |                 |
!>  displacement for        |                 |
!>    block 1: 0            |                 |
!>        |                 |                 |
!>        V                 V                 V
!>        +-----+-----+-----+-----+-----+-----+-----+-----+-----+
!>        |  0  |  -  |  -  |  3  |  4  |  -  |  6  |  7  |  8  |
!>        +-----+-----+-----+-----+-----+-----+-----+-----+-----+
!>         <--->             <--------->       <--------------->
!>        block 1              block 2              block 3
!>       1 element            2 elements           3 elements
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
    INTEGER :: triangle_type
    INTEGER :: lengths(0:2)
    INTEGER(KIND=MPI_ADDRESS_KIND) :: displacements(0:2)
    INTEGER :: buffer(0:2,0:2)
    INTEGER :: received(0:5)

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
        case (sender_rank)
            ! Create the datatype
            lengths = (/1, 2, 3/)
            displacements = (/0 * SIZEOF(my_rank), 3 * SIZEOF(my_rank), 6 * SIZEOF(my_rank)/)
            CALL MPI_Type_create_hindexed(3, lengths, displacements, MPI_INTEGER, triangle_type, ierror)
            CALL MPI_Type_commit(triangle_type, ierror)

            ! Send the message
            buffer = reshape((/0, 1, 2, 3, 4, 5, 6, 7, 8/), shape(buffer))
            WRITE(*,'(A,I0,A)') 'MPI process ', my_rank, ' sends values:'
            WRITE(*,'(I0)') buffer(0,0)
            WRITE(*,'(I0,A,I0)') buffer(0,1), ' ', buffer(1,1)
            WRITE(*,'(I0,A,I0,A,I0)') buffer(0,2), ' ', buffer(1,2), ' ', buffer(2,2)
            CALL MPI_Send(buffer, 1, triangle_type, receiver_rank, 0, MPI_COMM_WORLD, ierror)
        case (receiver_rank)
            ! Receive the message
            CALL MPI_Recv(received, 6, MPI_INTEGER, sender_rank, 0, MPI_COMM_WORLD, MPI_STATUS_IGNORE, ierror)
            WRITE(*,'(A,I0,A)') 'MPI process ', my_rank, ' received values:'
            WRITE(*,'(I0)') received(0)
            WRITE(*,'(I0,A,I0)') received(1), ' ', received(2)
            WRITE(*,'(I0,A,I0,A,I0)') received(3), ' ', received(4), ' ', received(5)
    END SELECT

    CALL MPI_Finalize(ierror)
END PROGRAM main