!> @brief Illustrates the OpenMP none policy.
!> @details This example shows that the usage of the 'none' default, by
!> comparing a version using implicit data-sharing clauses against that using
!> explicit data-sharing clauses. Both yield the same result, but one requires
!> the explicit usage of data-sharing clauses.
PROGRAM main
    USE omp_lib

    IMPLICIT NONE

    ! Relying on the implicit default(shared)
    INTEGER :: implicitly_shared = 0
    ! Forcing the usage of explicit data-sharing closes
    INTEGER :: explicitly_shared = 0

    ! Use 2 OpenMP threads
    CALL omp_set_num_threads(2)

    !$omp parallel
        !$omp atomic
        implicitly_shared = implicitly_shared + 1
    !$omp end parallel
    WRITE(*,'(A,I0,A)') 'Value with implicit shared: ', implicitly_shared, '.'

    !$omp parallel default(none) shared(explicitly_shared)
        !$omp atomic
        explicitly_shared = explicitly_shared + 1
    !$omp end parallel
    WRITE(*,'(A,I0,A)') 'Value with explicit shared: ', explicitly_shared, '.'
END PROGRAM main