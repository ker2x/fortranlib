! NOT TESTED YET
! --------------

MODULE lib_mandelbrot
    
    IMPLICIT NONE
    
    CONTAINS
    
    ! Why *NOT* in Mandelbrol set instead of *IS* in Mandelbrot set ?
    ! Because we can't now for sure if it really is inside the set 
    ! until we iterate an infinite number of time.
    ! but we now for sure it's not in the mandelbrot set if it escape before n_max
    !   c = the complex point we want to check
    !   n_max = the maximum iteration
    PURE FUNCTION notInMset(c, n_max)
        COMPLEX, INTENT(IN) :: c
        INTEGER, INTENT(IN) :: n_max
        INTEGER :: n
        COMPLEX :: z
        LOGICAL :: notInMSet
        z = CMPLX(0,0) 
        n = 0
        
        ! early escape attempts. if it's in the main cardioid or largest bulbs
        ! then it's proven it's in the mandelbrot set
        IF(((ABS(c - CMPLX(-1,0) )) < 0.25) .OR. ((ABS( 1.0 - SQRT(1-(4*c)) ))  < 1.0 ) ) THEN
            notInMset = .FALSE.

        ! else do the regular iteration
        ELSE
            ! iterate until we escape or reach mac iteration
            DO WHILE (ABS(z) < 2 .AND. (n < n_max))
                z = z**2 + c
                n = n + 1
            END DO
            
            IF (n >= n_max) THEN
                ! we reached max iteration without escaping, it may be in mandelbrot set so we return false
                notInMset = .FALSE.
            ELSE
                ! it escaped before reaching max iteration, it's not in mandelbrot set for sure
                notInMset = .TRUE.
            END IF
        END IF
    END FUNCTION notInMset
    
END MODULE lib_mandelbrot
