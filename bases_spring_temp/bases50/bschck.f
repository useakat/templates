************************************************************************
*    ===================                                               *
      SUBROUTINE BSCHCK
*    ===================                                               *
* ((Purpose))                                                          *
*     To check user's initialization parameters.                       *
*                                                                      *
*        Coded by S.Kawabata        Oct. '85                           *
*                                                                      *
************************************************************************

      IMPLICIT NONE
C     IMPLICIT REAL*8 (A-H,O-Z)

      INCLUDE 'base0.h'
      INCLUDE 'base1.h'
      INCLUDE 'base2.h'

      INCLUDE 'bparm.h'
      INCLUDE 'bscntl.h'

      INCLUDE 'xhcntl.h'

      INTEGER NWILDO

      INTEGER I

      LOCK  = 1

      IF( IBASES .NE.  1 ) THEN
          WRITE(6,9000)
 9000     FORMAT(
     .     5X,'*************************************************',
     .    /5X,'*                                               *',
     .    /5X,'*   BSINIT was not called before calling BASES  *',
     .    /5X,'*                                               *',
     .    /5X,'*   Process was terminated due to this error.   *',
     .    /5X,'*                                               *',
     .    /5X,'*************************************************')
          STOP
      ENDIF

C     IF( INTV .NE. 0 ) CALL BSUNIX( 2 )

      IF( NDIMT .LT. 1) THEN
          WRITE(6,9100)
 9100     FORMAT(
     .     5X,'*************************************************',
     .    /5X,'*                                               *',
     .    /5X,'*   NDIM was not set before calling BASES.      *',
     .    /5X,'*                                               *',
     .    /5X,'*   Process was terminated due to this error.   *',
     .    /5X,'*                                               *',
     .    /5X,'*************************************************')
          STOP
      ENDIF

      NDIM = NDIMT

      DO 200 I = 1,NDIM
         IF( XUT(I) .LE. -1.0D60) THEN
             WRITE(6,9200) I,I
 9200        FORMAT(
     .        5X,'*************************************************',
     .       /5X,'*                                               *',
     .       /5X,'*   XL(',I6,' ).  XU(',I6,' ) were not set      *',
     .       /5X,'*    before calling BASES.                      *',
     .       /5X,'*   Process was terminated due to this error.   *',
     .       /5X,'*                                               *',
     .       /5X,'*************************************************')
             STOP
         ENDIF

         IG(I)  = IGT(I)
         XL(I)  = XLT(I)
         XU(I)  = XUT(I)

  200 CONTINUE
C
C  Change the maximum number of the wild variables
C 10 ===> 15
      IF( NWILDT .LT.  0) THEN
          NWILDT = MIN( NDIM, 15)
          WRITE(6,9300) NWILDT
 9300     FORMAT(
     .     5X,'*************************************************',
     .    /5X,'*                                               *',
     .    /5X,'*   NWILD was not set before calling BASES.     *',
     .    /5X,'*                                               *',
     .    /5X,'*   NWILD is set equal to the value(',I6,' ).   *',
     .    /5X,'*                                               *',
     .    /5X,'*************************************************')
      ELSE
     .IF( NWILDT .GT. 15) THEN
          NWILDO = NWILDT
          NWILDT  = MIN( NDIM, 15)
          WRITE(6,9400) NWILDO, NWILDT
 9400     FORMAT(
     .     5X,'*************************************************',
     .    /5X,'*                                               *',
     .    /5X,'*   NWILD(',I6,' ) was too large number.        *',
     .    /5X,'*                                               *',
     .    /5X,'*   NWILD is set equal to the value(',I6,' ).   *',
     .    /5X,'*                                               *',
     .    /5X,'*************************************************')
      ENDIF

      NWILD = NWILDT
      NCALL = NCALLT

      ITMX1 = ITMX1T
      ITMX2 = ITMX2T
      ACC1  = ACC1T
      ACC2  = ACC2T
C
      RETURN
      END
