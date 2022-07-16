!> @brief Illustrates how to retreive the lower bound and extent of an MPI
!> datatype.
!> @details Each process in this program creates a contiguous MPI_Datatype made 
!> of two MPI_INTEGER, and then prints its lower bound and extent. This can be
!> visually described as follows:
!>
!> lower bound = 0 bytes
!> |
!> V
!> +-------------+-------------+
!> | MPI_INTEGER | MPI_INTEGER |
!> +-------------+-------------+
!>  <------------------------->
!>             extent
!> 
!>   (assuming 4-byte integers,
!>  this is an extent of 8 bytes)
PROGRAM main
    USE mpi_f08

    IMPLICIT NONE

    INTEGER :: ierror
    INTEGER :: my_rank
    TYPE(MPI_Datatype) :: double_int_type
    INTEGER(KIND=MPI_COUNT_KIND) :: lower_bound
    INTEGER(KIND=MPI_COUNT_KIND) :: extent

    CALL MPI_Init(ierror)

    ! Get my rank
    CALL MPI_Comm_rank(MPI_COMM_WORLD, my_rank, ierror)

    ! Create the datatype
    CALL MPI_Type_contiguous(2, MPI_INTEGER, double_int_type, ierror)
    CALL MPI_Type_commit(double_int_type, ierror)

    ! Get the lower bound and extent of the datatype created
    CALL MPI_Type_get_extent_x(double_int_type, lower_bound, extent, ierror)
    WRITE(*,'(A,I0,A,I0,A,I0,A)') '(MPI Process ', my_rank, ') Lower-bound = ', lower_bound, ' bytes, extent = ', extent, ' bytes.'

    CALL MPI_Finalize(ierror)
END PROGRAM main
