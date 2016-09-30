************************************************************************
*    ===================                                               *
      SUBROUTINE BSSETP(NCALL0,ITMX10,ITMX20,ACC10,ACC20)
*    ===================                                               *
* ((Purpose))                                                          *
*                                                                      *
*        Coded by J.Kanzaki           July '94                         *
*                                                                      *
************************************************************************

      IMPLICIT NONE
C     IMPLICIT REAL*8 (A-H,O-Z)

      INTEGER NCALL0,ITMX10,ITMX20
      DOUBLE PRECISION ACC10,ACC20

      INCLUDE 'base1.h'
      INCLUDE 'base2.h'
      INCLUDE 'bparm.h'


      IF(NCALL0.GT.0) THEN
         NCALL  = NCALL0
         NCALLT = NCALL0
      END IF
      IF(ITMX10.GT.0) THEN
         ITMX1  = ITMX10
         ITMX1T = ITMX10
      END IF
      IF(ITMX20.GT.0) THEN
         ITMX2  = ITMX20
         ITMX2T = ITMX20
      END IF
      IF(ACC10.GT.0.D0) THEN
         ACC1  = ACC10
         ACC1T = ACC10
      END IF
      IF(ACC20.GT.0.D0) THEN
         ACC2  = ACC20
         ACC2T = ACC20
      END IF

      RETURN
      END
