C
C***********************************************************************
C*=================================                                    *
C* SUBROUTINE BSTIME( TIME, IFLG )                                     *
C*=================================                                    *
C*((Purpose))                                                          *
C*        Interface routine to get used CPU time from FORTRAN          *
C*        Library routine CLOCK etc.                                   *
C*((Input))                                                            *
C*        IFLG  : Flag                                                 *
C*          IFLG = 0 : Initialization of clock routine.                *
C*          IFLG = 1 : Get used CPU time.                              *
C*((Output))                                                           *
C*        TIME  : Used CPU time in second.                             *
C*                                                                     *
C*       Coded by S.Kawabata        Oct. '85                           *
C*                                                                     *
C***********************************************************************
      SUBROUTINE BSTIME( TIME, IFLG )

      IMPLICIT NONE

      REAL TIME
      INTEGER IFLG

      REAL TIME0
      SAVE TIME0

      REAL TARRAY(2),RESULT
C
      IF( IFLG .EQ. 0 ) THEN

         CALL ETIME(TARRAY, RESULT)
         TIME0 = TARRAY(1)

      ELSE

         CALL ETIME(TARRAY, RESULT)
         TIME = TARRAY(1)
         TIME = TIME-TIME0

      ENDIF
C
      RETURN
      END
