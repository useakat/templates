      SUBROUTINE BSUTIM( JOB, ID )

      IMPLICIT NONE

      INTEGER JOB,ID

      INCLUDE 'btime.h'

      REAL DTIME


*  Prior to call thisroutine, BSTIME( TIME0, 1 ) should be called
*  for initialize the time offset TIME0.
*
      CALL BSTIME( RTIME, 1)
      DTIME      = RTIME - TIME0

      IF( JOB .EQ. 0 ) THEN
*       For BASES computing time
*         ID  = 0  : Grid defining step
*               1  : Integration step
*               2  : Others

          TIMEBS(ID) = TIMEBS(ID) + DTIME

          IF( ID .LE. 1 ) THEN
              TIMINT = TIMINT + DTIME
          ENDIF
      ELSE
*       For SPRING computing time
*         ID  = 0  : Event generation
*               1  : Overhead
*               2  : Others

          TIMESP(ID) = TIMESP(ID) + DTIME

      ENDIF

      TIME0      = RTIME

      RETURN
      END
