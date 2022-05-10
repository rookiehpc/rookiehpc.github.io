!> @brief Illustrates how to create an indexed MPI datatype.
!> @details This program is meant to be run with 2 processes: a sender and a
!> receiver. These two MPI processes will exchange a message made of a
!> structure representing a person.
!>
!> Structure of a person:
!> - age: INTEGER
!> - height: REAL
!> - name: CHARACTER(LEN=10)
!>
!> How to represent such a structure with an MPI struct:
!>   
!>        +------------------ displacement for
!>        |         block 2: sizeof(INTEGER) + sizeof(REAL)
!>        |                             |
!>        +----- displacement for       |
!>        |    block 2: sizeof(INTEGER) |
!>        |            |                |
!>  displacement for   |                |
!>    block 1: 0       |                |
!>        |            |                |
!>        V            V                V
!>        +------------+----------------+------------+
!>        |    age     |     height     |    name    |
!>        +------------+----------------+------------+
!>         <----------> <--------------> <---------->
!>            block 1        block 2        block 3
!>        1 MPI_INTEGER    1 MPI_REAL  10 MPI_CHARACTER
PROGRAM main
    USE mpi

    IMPLICIT NONE

    TYPE person_t
        INTEGER :: age
        REAL :: height
        CHARACTER(LEN=10) :: name
    END TYPE

    INTEGER :: ierror
    INTEGER :: size
    INTEGER :: person_type
    INTEGER :: lengths(0:2)
    INTEGER(KIND=MPI_ADDRESS_KIND) :: displacements(0:2)
    INTEGER :: types(0:2)
    INTEGER, PARAMETER :: sender_rank = 0
    INTEGER, PARAMETER :: receiver_rank = 1
    INTEGER :: my_rank
    TYPE(person_t) :: buffer
    INTEGER :: dummy_age
    REAL :: dummy_height

    CALL MPI_Init(ierror)

    ! Get the number of processes and check only 2 processes are used
    CALL MPI_Comm_size(MPI_COMM_WORLD, size, ierror)
    IF (size .NE. 2) THEN
        WRITE(*,'(A)') 'This application is meant to be run with 2 processes.'
        CALL MPI_Abort(MPI_COMM_WORLD, -1, ierror)
    END IF

    ! Create the datatype
    lengths = (/1, 1, 10/)
    displacements = (/0 * SIZEOF(dummy_age), sizeof(dummy_age), sizeof(dummy_age) + sizeof(dummy_height)/)
    types = (/MPI_INTEGER, MPI_REAL, MPI_CHARACTER/)
    CALL MPI_Type_create_struct(3, lengths, displacements, types, person_type, ierror)
    CALL MPI_Type_commit(person_type, ierror)

    ! Get my rank and do the corresponding job
    CALL MPI_Comm_rank(MPI_COMM_WORLD, my_rank, ierror)
    SELECT CASE (my_rank)
        CASE (sender_rank)
            ! Send the message
            buffer % age = 20
            buffer % height = 1.83
            buffer % name = 'Tom'
            WRITE(*,'(A,I0,A)') 'MPI process ', my_rank, ' sends person:'
            WRITE(*,'(A,I0)') '- age = ', buffer % age
            WRITE(*,'(A,F0.0)') '- height = ', buffer % height
            WRITE(*,'(A,A)') '- name = ', buffer % name
            CALL MPI_Send(buffer, 1, person_type, receiver_rank, 0, MPI_COMM_WORLD, ierror)
        CASE (receiver_rank)
            ! Receive the message
            CALL MPI_Recv(buffer, 1, person_type, sender_rank, 0, MPI_COMM_WORLD, MPI_STATUS_IGNORE, ierror)
            WRITE(*,'(A,I0,A)') 'MPI process ', my_rank, ' received person:'
            WRITE(*,'(A,I0)') '- age = ', buffer % age
            WRITE(*,'(A,F0.0)') '- height = ', buffer % height
            WRITE(*,'(A,A)') '- name = ', buffer % name
    END SELECT

    CALL MPI_Finalize(ierror)
END PROGRAM main
