      subroutine JGLUO3(W1,W2,AMP_NGLUONS) 
      IMPLICIT NONE

C     
C     LOCAL VARIABLES
C     
      INTEGER I
      COMPLEX*16 W(6,2,2),W1(6),W2(6)
      COMPLEX*16 AMP_NGLUONS(6)
C     
C     GLOBAL VARIABLES
C     
      real*8 G2
      COMMON /CFQCD_COUPL/ g2

      DO i=1,6
         W(i,1,1) = W1(i)
         W(i,2,2) = W2(i)
      ENDDO

      CALL JGGXXX(W(1,1,1),W(1,2,2), G2,AMP_NGLUONS)
    
      RETURN
      END
