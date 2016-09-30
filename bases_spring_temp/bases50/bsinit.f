************************************************************************
*    ===================                                               *
      SUBROUTINE BSINIT(ISEED)
*    ===================                                               *
* ((Purpose))                                                          *
*     Initialization of BASE50/SPRING50.                               *
*     Function of this routine is                                      *
*       (0) Set the size of histogram and scatter plot buffers         *
*       (1) Set the parameters INTV and IPNT                           *
*             INTV = ( 0 / 1 / any )                                   *
*                  = ( Batch / Batch(Unix) / Interactive )             *
*             IPNT = ( 0 / any )                                       *
*                  = ( IBM Type / Ascii printer )                      *
*       (2) Set the acceleration factor ALPHA by 1.5                   *
*            The range of this value is from 0.0 to 2.0.               *
*            ALPHA = 0.0 results in no grid-optimization.              *
*       (3) Set the grid-optimization flag IGOPT ( Default value 0 )   *
*             IGOPT = 0  :  The grid is optimized by VEGAS algorithm   *
*             IGOPT = 1  :  The grid is optimized so that the accuracy *
*                           of each iteration be minimized.            *
*       (4) Set Node-ID number NODEID and the number of nodes NUMNOD   *
*       (5) Set seed of radom number                                   *
*       (6) Set the values of BASES paremeters with default ones.      *
*       (7) Set the values of parameters with non-sense values,        *
*            which should be set again with the true values by User    *
*            before running BASES.                                     *
*                                                                      *
*        Coded by S.Kawabata         March '94                         *
*                                                                      *
************************************************************************

      IMPLICIT NONE
C     IMPLICIT REAL*8 (A-H,O-Z)

      INCLUDE 'base0.h'
      INCLUDE 'base1.h'
      INCLUDE 'base4.h'
      INCLUDE 'base6.h'

      INCLUDE 'bparm.h'

      INCLUDE 'bscntl.h'

      INCLUDE 'ninfo.h'
      INCLUDE 'btime.h'

      INTEGER LU
      INTEGER IGOPT,ISEED

      INTEGER I

*=========================================================
* (0) Initialization of timer and Histogram buffer
*     Timer initialization
      CALL BSTIME( TIME0, 0 )
      TIMEB1 = TIME0
      TIMINT = 0

*     Histogram buffer initialization
      LU  = 6
CCCCC LU  = 0
      CALL BHINIT( LU )

*=========================================================

* (1) Set the parameters INTV and IPNT
      INTV  = 0
      IPNT  = 0
* (2) Set the acceleration factor ALPHA by 1.5
      ALPH  = 1.5D0
* (3) Set the grid-optimization flag IGOPT
      IGOPT = 0
* (4) Set Node-ID number NODEID and the number of nodes NUMNOD
      IF( INTV .EQ. 0 ) THEN
         NODEID = 0
         NUMNOD = 1
      ELSE
         NODEID = 0
         NUMNOD = 1
C        CALL BSUNIX(1)
      ENDIF

C---------------------------------------------------------------
C (5)  Set initial seeds of random number generator
C---------------------------------------------------------------
c       ISEED = 12345
C
       CALL DRNSET( ISEED )
C ---------------------------------------------------------------
C (6),(7)  Set BASES parameters equal to default values
C ---------------------------------------------------------------
C
       NDIMT   = -1
       NWILDT  =  1
       ITMX1T  = 15
       ITMX2T  = 100
       NCALLT  = 1000
       ACC1T   = 0.2D0
       ACC2T   = 0.01D0
       DO 100 I = 1,MXDIM
          IGT(I) = 1
          XUT(I)  = -1.0D60
  100  CONTINUE

*    Initialization of computing time table of BASES
       DO 200 I = 0, 2
          TIMEBS(I) = 0.0
  200  CONTINUE

*-------------------------------------------
*      Don't change IBASES from this value
*-------------------------------------------
       IBASES =  1

       RETURN
       END
