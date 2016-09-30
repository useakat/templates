      subroutine GLUON6(W1,W2,W3,W4,W5,W6,AMP_NGLUONS) 
      IMPLICIT NONE
C
C     PARAMETERS
C
      INTEGER   MAXGLUONS,   MAXDIAG
      PARAMETER(MAXGLUONS=12,MAXDIAG=55)
c
c     ARGUMENTS
c     
      INTEGER NGLUONS
C     
C     LOCAL VARIABLES
C     
      INTEGER I,K
      COMPLEX*16 W(6,MAXGLUONS,MAXGLUONS),W1(6),W2(6),W3(6),W4(6),W5(6)
      COMPLEX*16 W6(6)
      COMPLEX*16 Z(MAXDIAG),WX(6,MAXDIAG),AMP_NGLUONS
C     
C     GLOBAL VARIABLES
C     
      double complex G2I
      REAL*8 G2
      COMMON /CFQCD_COUPL/ g2,g2i
      
      
      AMP_NGLUONS=(0D0,0D0)
      ngluons = 6
      do i=1,6
         W(i,1,1) = W1(i)
         W(i,2,2) = W2(i)
         W(i,3,3) = W3(i)
         W(i,4,4) = W4(i)
         W(i,5,5) = W5(i)
         W(i,6,6) = W6(i)
      enddo

***************************************************************************

      DO i=1,ngluons-2
         CALL JGGXXX(W(1,I,I),W(1,I+1,I+1), G2,W(1,I,I+1))     
      ENDDO

***************************************************************************

       DO i=1,ngluons-3  
          CALL JGGXXX(W(1,I,I+1),W(1,I+2,I+2),              G2,WX(1,1))              
          CALL JGGXXX(W(1,I,I)  ,W(1,I+1,I+2),              G2,WX(1,2))              
          CALL JGGGOR(W(1,I,I)  ,W(1,I+1,I+1),W(1,I+2,I+2), G2I,WX(1,3))              
          CALL SUMW(WX,3,W(1,I,I+2))
       ENDDO
          
***************************************************************************
     
      DO i=1,ngluons-4  
         CALL JGGXXX(W(1,I,I+2),W(1,I+3,I+3),             G2,WX(1,1))                
         CALL JGGXXX(W(1,I,I+1),W(1,I+2,I+3),             G2,WX(1,2))                
         CALL JGGXXX(W(1,I,I)  ,W(1,I+1,I+3),             G2,WX(1,3))                
         CALL JGGGOR(W(1,I,I+1),W(1,I+2,I+2),W(1,I+3,I+3),G2I,WX(1,4))                
         CALL JGGGOR(W(1,I,I)  ,W(1,I+1,I+2),W(1,I+3,I+3),G2I,WX(1,5))                
         CALL JGGGOR(W(1,I,I)  ,W(1,I+1,I+1),W(1,I+2,I+3),G2I,WX(1,6))                
         CALL SUMW(WX,6,W(1,I,I+3))
      ENDDO
      
***************************************************************************

         i=1
C 3-point
         CALL GGGXXX(W(1,I,I+3),W(1,I+4,I+4),W(1,I+5,I+5),     G2,Z( 1))      
         CALL GGGXXX(W(1,I,I+2),W(1,I+3,I+4),W(1,I+5,I+5),     G2,Z( 2))      
         CALL GGGXXX(W(1,I,I+1),W(1,I+2,I+4),W(1,I+5,I+5),     G2,Z( 3))      
         CALL GGGXXX(W(1,I,I)  ,W(1,I+1,I+4),W(1,I+5,I+5),     G2,Z( 4))      
C 4-point
         CALL GGGGOR(W(1,I,I+2),W(1,I+3,I+3),W(1,I+4,I+4),W(1,I+5,I+5),
     &    G2I,Z( 5))     
         CALL GGGGOR(W(1,I,I+1),W(1,I+2,I+2),W(1,I+3,I+4),W(1,I+5,I+5),
     &    G2I,Z( 6))     
         CALL GGGGOR(W(1,I,I+1),W(1,I+2,I+3),W(1,I+4,I+4),W(1,I+5,I+5),
     &    G2I,Z( 7))     
         CALL GGGGOR(W(1,I,I)  ,W(1,I+1,I+1),W(1,I+2,I+4),W(1,I+5,I+5),
     &    G2I,Z( 8))     
         CALL GGGGOR(W(1,I,I)  ,W(1,I+1,I+2),W(1,I+3,I+4),W(1,I+5,I+5),
     &    G2I,Z( 9))     
         CALL GGGGOR(W(1,I,I)  ,W(1,I+1,I+3),W(1,I+4,I+4),W(1,I+5,I+5),
     &    G2I,Z(10))     
         DO K=1,10
            AMP_NGLUONS=AMP_NGLUONS+Z(K)
         ENDDO 
c         DO K=5,10
c            AMP_NGLUONS=AMP_NGLUONS+Z(K)
c         ENDDO 
         RETURN

      END
