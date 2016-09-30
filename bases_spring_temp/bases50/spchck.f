************************************************************************
*    ===================                                               *
      SUBROUTINE SPCHCK
*    ===================                                               *
* ((Purpose))                                                          *
*     To check user's initialization parameters.                       *
*                                                                      *
*        Coded by S.Kawabata      April  '94                           *
*                                                                      *
************************************************************************

      IMPLICIT NONE
C     IMPLICIT REAL*8 (A-H,O-Z)

      INCLUDE 'base0.h'
      INCLUDE 'base1.h'
      INCLUDE 'base2.h'
      INCLUDE 'bparm.h'

      INCLUDE 'bscntl.h'

      INTEGER I


      IF( NDIMT .NE. NDIM ) THEN
          WRITE(6,9100) NDIMT,NDIM
 9100     FORMAT(
     .     5X,'*************************************************',
     .    /5X,'*                                               *',
     .    /5X,'*   Given NDIM(',I6,' ) does not match          *',
     .    /5X,'*      to NDIM(',I6,' ) in BASES.               *',
     .    /5X,'*                                               *',
     .    /5X,'*   Process was terminated due to this error.   *',
     .    /5X,'*                                               *',
     .    /5X,'*************************************************')
          STOP
      ENDIF

      IF( NWILDT .NE. NWILD ) THEN
          WRITE(6,9110) NWILDT,NWILD
 9110     FORMAT(
     .     5X,'*************************************************',
     .    /5X,'*                                               *',
     .    /5X,'*   Given NWILD(',I6,' ) does not match         *',
     .    /5X,'*      to NWILD(',I6,' ) in BASES.              *',
     .    /5X,'*                                               *',
     .    /5X,'*   Process was terminated due to this error.   *',
     .    /5X,'*                                               *',
     .    /5X,'*************************************************')
          STOP
      ENDIF

      DO 200 I = 1,NDIM
         IF( XLT(I) .NE. XL(I) ) THEN
             WRITE(6,9200) I,XLT(I),I,XL(I)
 9200        FORMAT(
     .     5X,'*************************************************',
     .    /5X,'*                                               *',
     .    /5X,'*   Given XL(',I3,' ) = ',D15.8,'            *',
     .    /5X,'*      does not match to                        *',
     .    /5X,'*      to XL(',I3,' ) = ',D15.8,' in BASES   *',
     .    /5X,'*                                               *',
     .    /5X,'*   Process was terminated due to this error.   *',
     .    /5X,'*                                               *',
     .    /5X,'*************************************************')
             STOP
         ENDIF
         IF( XUT(I) .NE. XU(I) ) THEN
             WRITE(6,9210) I,XUT(I),I,XU(I)
 9210        FORMAT(
     .     5X,'*************************************************',
     .    /5X,'*                                               *',
     .    /5X,'*   Given XU(',I3,' ) = ',D15.8,'            *',
     .    /5X,'*      does not match to                        *',
     .    /5X,'*      to XU(',I3,' ) = ',D15.8,' in BASES   *',
     .    /5X,'*                                               *',
     .    /5X,'*   Process was terminated due to this error.   *',
     .    /5X,'*                                               *',
     .    /5X,'*************************************************')
             STOP
         ENDIF
  200 CONTINUE

      RETURN
      END
