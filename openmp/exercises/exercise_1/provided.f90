!> @brief Hello world in OpenMP.
!> @details The object is to make each thread print its identifier and the total
!> number of threads spawned.
PROGRAM main
    USE omp_lib

    IMPLICIT NONE

    ! 1) Declare the variables needed
    ! 1.1) The one for the thread identifier
    ! 1.2) The one for the total number of threads

    ! 2) Create the OpenMP parallel region and make sure each thread gets its own copy of the two variables declared above
        ! 2.1) Get my thread number
        ! 2.2) Get the number of threads inside that parallel region
        ! 2.3) Print everything
END PROGRAM main