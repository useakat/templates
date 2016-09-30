      subroutine GLUON4(W1,W2,W3,W4,AMP_NGLUONS) 
      IMPLICIT NONE
C
C     PARAMETERS
C
c      include 'maxgluons.inc'
c     maxgluons= 4 5  6  7   8   9  10  11  12  13  14  15
c     maxdiag  = 3 6 10 15  21  28  36  45  55  66  78  91
c     ncalls   =     41 77 134 219 340 506 727
C
      REAL*8     ZERO
      PARAMETER (ZERO=0D0)

      INTEGER   MAXGLUONS,   MAXDIAG
      PARAMETER(MAXGLUONS=12,MAXDIAG=55)
c
c     ARGUMENTS
c     
      REAL*8 P(0:3,MAXGLUONS)
      INTEGER NHEL(MAXGLUONS),IC(MAXGLUONS)
      INTEGER NGLUONS
C     
C     LOCAL VARIABLES
C     
      INTEGER I,K
      COMPLEX*16 W(6,MAXGLUONS,MAXGLUONS),W1(6),W2(6),W3(6),W4(6)
      COMPLEX*16 Z(MAXDIAG),WX(6,MAXDIAG),AMP_NGLUONS
C     
C     GLOBAL VARIABLES
C     
      double complex G2I
      REAL*8 G2
      COMMON /CFQCD_COUPL/ g2,g2i
      
      
      AMP_NGLUONS=(0D0,0D0)

      ngluons = 4
***************************************************************************
      do i=1,6
         W(i,1,1) = W1(i)
         W(i,2,2) = W2(i)
         W(i,3,3) = W3(i)
         W(i,4,4) = W4(i)
      enddo

      DO i=1,ngluons-2
         CALL JGGXXX(W(1,I,I),W(1,I+1,I+1), G2,W(1,I,I+1))     
      ENDDO

***************************************************************************4

              
      IF(ngluons.eq.4) then
         I=1
         CALL GGGXXX(W(1,I,I+1),      W(1,I+2,I+2),W(1,I+3,I+3),G2,Z(1))
         CALL GGGXXX(W(1,I,I)  ,      W(1,I+1,I+2),W(1,I+3,I+3),G2,Z(2))  
         CALL GGGGOR(W(1,I,I),W(1,I+1,I+1),W(1,I+2,I+2),W(1,I+3,I+3),G2I
     &    ,Z(3))          
         
         DO K=1,3
            AMP_NGLUONS=AMP_NGLUONS+Z(K)
         ENDDO  
         RETURN
      ENDIF

      END


      
