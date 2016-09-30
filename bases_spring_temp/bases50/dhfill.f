************************************************************************
*    =======================================                           *
      SUBROUTINE DHFILL( ID, DX, DY, FX )
*    =======================================                           *
* ((Function))                                                         *
*     To fill scatter plot                                             *
*   This routine identifies the bin number which is to be updated      *
*   with weight FX*WGT.  Up to five points per plot are able to        *
*   be stacked before calling BHUPDT or SHUPDT.                        *
* ((Input))                                                            *
*   ID    : Histogram identification number                            *
*   DX    : Input x value                                              *
*   DY    : Input y value                                              *
*   FX    : Input value of the function                                *
* ((Author))                                                           *
*   S.Kawabata         June '90 at KEK                                 *
*                                                                      *
************************************************************************

      IMPLICIT NONE

      INTEGER ID
      DOUBLE PRECISION DX, DY, FX

      INCLUDE 'base0.h'
      INCLUDE 'base3.h'

      INCLUDE 'plot.h'

      INTEGER ISCAT,IP1,IP2,IP3
      REAL X,Y
      REAL XMIN,XMAX
      REAL YMIN,YMAX
      INTEGER MXBIN,MYBIN
      REAL DEV
      INTEGER IX,IY

      INTEGER I,K

*                                                                      *
*--------------------------- Entry point ------------------------------*
*                                                                      *
*======================================================================*
*               Find the scatter plot ID in the table                  *
*======================================================================*
*                                                                      *
      IF( NSCAT .GT. 0 ) THEN
          I  = IABS(MOD( ID, 13 )) + 1
          IF( DHASH(1, I) .EQ. 1 ) THEN
            IF( ID .EQ. MAPD( 1, DHASH(2,I))) THEN
                ISCAT = DHASH(2,I)
                GO TO 200
            ENDIF
          ELSEIF( DHASH(1, I) .GT. 1 ) THEN
            DO 100 K = 2, DHASH(1,I)+1
               IF( ID .EQ. MAPD( 1, DHASH(K,I))) THEN
                   ISCAT = DHASH(K,I)
                   GO TO 200
               ENDIF
  100       CONTINUE
          ENDIF
      ENDIF
C     IF( LU .GT. 0 ) THEN
C         WRITE(LU,9000) ID
C9000     FORMAT(1X,'No Scat_Plot corresponds to ID =',I5,
C    .          /1X,' This call is neglected ]]]')
C     ENDIF
      RETURN

*                                                                      *
*======================================================================*
*               Determine the bin numbers for x and y                  *
*======================================================================*
*                                                                      *
  200 X     = DX*1.0
      Y     = DY*1.0

          IP1   = MAPD(2,ISCAT)
          XMIN  = BUFF(IP1)
          XMAX  = BUFF(IP1+1)
          MXBIN = IBUF(IP1+2)
          DEV   = BUFF(IP1+3)
          IX    =   0
          IY    =   0
          IF( X .GE. XMIN .AND. X .LE. XMAX ) THEN
              IX   = INT( (X - XMIN)/DEV+ 1.0 )
              IF( IX .GT. MXBIN ) IX =   0
          ENDIF
C
          IF( IX .GT. 0 ) THEN
              YMIN  = BUFF(IP1+4)
              YMAX  = BUFF(IP1+5)
              MYBIN = IBUF(IP1+6)
              DEV   = BUFF(IP1+7)
              IF( Y .GE. YMIN .AND. Y .LE. YMAX ) THEN
                  IY   = INT((Y - YMIN)/DEV + 1.0)
                 IF( IY .GT. MYBIN ) THEN
                     IX  =  0
                     IY  =  0
                 ENDIF
              ENDIF
          ENDIF
*                                                                      *
*======================================================================*
*               Fill the scatter plot ID                               *
*======================================================================*
*----------------------------------------------------------------------*
*               For BASES                                              *
*----------------------------------------------------------------------*
*                                                                      *
      IF( IBASES .EQ. 1 ) THEN
          IF( IY .GT. 0 ) THEN

              IP2       = MAPD(3,ISCAT)
              IBUF(IP2) = SCALLS
              IP2       = IX + MXBIN*(IY - 1) + IP2
              BUFF(IP2) = BUFF(IP2) + FX*WGT

          ENDIF

*----------------------------------------------------------------------*
*               For SPRING                                             *
*----------------------------------------------------------------------*
*                                                                      *
      ELSE

          IP3         = MAPD(4,ISCAT)
          IBUF(IP3)   = IX
          IBUF(IP3+1) = IY

      ENDIF

      RETURN
      END
