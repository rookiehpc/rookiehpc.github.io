!> @brief Illustrates how to change the dynamic adjustment.
PROGRAM main
    USE omp_lib

    IMPLICIT NONE

    IF (omp_get_dynamic()) THEN
        WRITE(*,'(A)') 'By default, dynamic adjustment is allowed.'
    ELSE
        WRITE(*,'(A)') 'By default, dynamic adjustment is not allowed.'
    END IF

    ! Invert the dynamic adjustement
    CALL omp_set_dynamic(MERGE(.FALSE., .TRUE., omp_get_dynamic()))

    IF (omp_get_dynamic()) THEN
        WRITE(*,'(A)') 'Dynamic adjustment is now allowed.'
    ELSE
        WRITE(*,'(A)') 'Dynamic adjustment is no longer allowed.'
    END IF
END PROGRAM main