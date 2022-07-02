!> @brief Solution to the Hello world exercise in OpenMP.
PROGRAM main
    USE omp_lib

    IMPLICIT NONE

    ! 1) Declare the variables needed
    ! 1.1) The one for the thread identifier
    INTEGER :: my_id
    ! 1.2) The one for the total number of threads
    INTEGER :: thread_number

    ! 1) Create the OpenMP parallel region
    !$OMP parallel default(none) private(my_id, thread_number)
        ! 1.1) Get my thread number
        my_id = omp_get_thread_num()

        ! 1.2) Get the number of threads inside that parallel region
        thread_number = omp_get_num_threads()

        ! 1.3) Print everything
        WRITE(*, '(A,I0,A,I0,A)') '"Hello world!" from thread ', my_id, ', we are ', thread_number, ' threads.'
    !$OMP END parallel
END PROGRAM main