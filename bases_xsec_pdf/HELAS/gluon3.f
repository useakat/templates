      subroutine GLUON3(W1,W2,W3,AMP_NGLUONS) 
      IMPLICIT NONE

C     
C     LOCAL VARIABLES
C     
      INTEGER I
      COMPLEX*16 W(6,3,3),W1(6),W2(6),W3(6)
      COMPLEX*16 AMP_NGLUONS
C     
C     GLOBAL VARIABLES
C     

      REAL*8 G2
      double complex G2I
      COMMON /CFQCD_COUPL/ g2,g2i
          
      AMP_NGLUONS=(0D0,0D0)
      
      DO i=1,6
         W(i,1,1) = W1(i)
         W(i,2,2) = W2(i)
         W(i,3,3) = W3(i)
      ENDDO

      CALL GGGXXX(W(1,1,1),W(1,2,2),W(1,3,3),G2,AMP_NGLUONS)
  
      RETURN
      END
