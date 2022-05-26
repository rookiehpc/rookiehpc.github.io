!> @brief Illustrates the use the omp_get_schedule function.
!> @details The application retrieves the schedule and chunk size applied when
!> a runtime schedule is encountered.
PROGRAM main
    USE omp_lib

    IMPLICIT NONE

    INTEGER(KIND=omp_sched_kind) :: kind
    INTEGER :: chunk_size

    ! Get the schedule to apply when a runtime schedule is encountered
    CALL omp_get_schedule(kind, chunk_size)

    WRITE(*,'(A)',advance='no') 'In case a runtime schedule is encountered, the '
    SELECT CASE (kind)
        CASE (omp_sched_static)
            WRITE(*,'(A)',advance='no') 'static'
        CASE (omp_sched_dynamic)
            WRITE(*,'(A)',advance='no') 'dynamic'
        CASE (omp_sched_guided)
            WRITE(*,'(A)',advance='no') 'guided'
        CASE (omp_sched_auto)
            WRITE(*,'(A)',advance='no') 'auto'
        CASE DEFAULT
            WRITE(*,'(A)',advance='no') 'other (implementation specific)'
    END SELECT
    WRITE(*,'(A,I0,A,A)') ' schedule is applied, with chunks made of ', &
                            chunk_size, ' iteration', MERGE('s.', '. ', chunk_size .GT. 1)
END PROGRAM main