!> @brief Illustrates how to create a vector MPI datatype.
!> @details This program is meant to be run with 2 processes: a sender and a
!> receiver. These two MPI processes will exchange a message made of three
!> integers. On the sender, that message is in fact the middle column of an
!> array it holds, which will be represented by an MPI homogeneous vector.
!>
!>
!>     Full array          What we want
!>                            to send
!> +-----+-----+-----+  +-----+-----+-----+
!> |  0  |  1  |  2  |  |  -  |  1  |  -  |
!> +-----+-----+-----+  +-----+-----+-----+
!> |  3  |  4  |  5  |  |  -  |  4  |  -  |
!> +-----+-----+-----+  +-----+-----+-----+
!> |  6  |  7  |  8  |  |  -  |  7  |  -  |
!> +-----+-----+-----+  +-----+-----+-----+
!>
!> How to extract a column with a vector type:
!>
!>                 distance between the
!>            start of each block: 3 elements
!>          <---------------> <--------------->
!>         |                 |                 |
!>      start of          start of          start of
!>      block 1           block 2           block 3
!>         |                 |                 |
!>         V                 V                 V
!>   +-----+-----+-----+-----+-----+-----+-----+-----+-----+
!>   |  -  |  1  |  -  |  -  |  4  |  -  |  -  |  7  |  -  |
!>   +-----+-----+-----+-----+-----+-----+-----+-----+-----+
!>          <--->             <--->             <--->
!>         block 1           block 2           block 3
!> 
!> Block length: 1 element
!> Element: MPI_INTEGER
PROGRAM main
    USE mpi_f08

    IMPLICIT NONE

    INTEGER :: size
    INTEGER, PARAMETER :: sender_rank = 0
    INTEGER, PARAMETER :: receiver_rank = 1
    INTEGER :: my_rank
    TYPE(MPI_Datatype) :: column_type
    INTEGER :: buffer (0:2,0:2)
    INTEGER :: received(0:2)

    CALL MPI_Init()

    ! Get the number of processes and check only 2 processes are used
    CALL MPI_Comm_size(MPI_COMM_WORLD, size)
    IF (size .NE. 2) THEN
        WRITE(*,'(A)') 'This application is meant to be run with 2 processes.'
        CALL MPI_Abort(MPI_COMM_WORLD, -1)
    END IF

    ! Get my rank and do the corresponding job
    CALL MPI_Comm_rank(MPI_COMM_WORLD, my_rank)
    SELECT CASE (my_rank)
        CASE (sender_rank)
            ! Create the datatype
            CALL MPI_Type_vector(3, 1, 3, MPI_INTEGER, column_type)
            CALL MPI_Type_commit(column_type)

            ! Send the message
            buffer = RESHAPE([0, 1, 2, 3, 4, 5, 6, 7, 8], SHAPE(buffer))
            WRITE(*,'(A,I0,A,I0,A,I0,A,I0,A)') 'MPI process ', my_rank, ' sends values ', &
                                                 buffer(1,0), ', ', buffer(1,1), ' and ', buffer(1,2), '.'
            CALL MPI_Send(buffer(1,0), 1, column_type, receiver_rank, 0, MPI_COMM_WORLD)
        CASE (receiver_rank)
            ! Receive the message
            CALL MPI_Recv(received, 3, MPI_INTEGER, sender_rank, 0, MPI_COMM_WORLD, MPI_STATUS_IGNORE)
            WRITE(*,'(A,I0,A,I0,A,I0,A,I0,A)') 'MPI process ', my_rank, ' received values ', &
                                                 received(0), ', ', received(1), ' and ', received(2), '.'
    END SELECT

    CALL MPI_Finalize()
END PROGRAM main
