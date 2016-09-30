************************************************************************
*     ====================                                             *
      SUBROUTINE BHSAVE
*     ====================                                             *
* ((Purpose))                                                          *
*     To save contents of temporary buffers to the histogram buffers,  *
*     in order to avoid the precision problem.                         *
* ((Author))                                                           *
*     S.Kawabata  June '90 at KEK                                      *
************************************************************************

      IMPLICIT NONE

      INCLUDE 'plot.h'

      INTEGER IP2,NC
      INTEGER IB1,IB2
      INTEGER I1,I2

      INTEGER I,J
*                                                                      *
*--------------------------- Entry point ------------------------------*
*                                                                      *
      DO 200 J = 1, NHIST
         IP2   = MAPL(3,J)
         NC    = IBUF( MAPL(2,J)+2 ) + 1
         IB1   = IP2 + 52
         IB2   = IB1 + 52
         DO 100 I = 0,NC
            I1    = I + IB1
            I2    = I1 + 104
            BUFF(I2)  = BUFF(I2) + BUFF(I1)
            BUFF(I1)  = 0.0
            I1    = I + IB2
            I2    = I1 + 104
            BUFF(I2)  = BUFF(I2) + BUFF(I1)
            BUFF(I1)  = 0.0
  100    CONTINUE
  200 CONTINUE
C
      RETURN
      END
