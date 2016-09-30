      subroutine JGLUO5(W1,W2,W3,W4,AMP_NGLUONS) 
      IMPLICIT NONE
C
C     PARAMETERS
C
      INTEGER   MAXGLUONS,   MAXDIAG
      PARAMETER(MAXGLUONS=12,MAXDIAG=55)
C     
C     LOCAL VARIABLES
C     
      INTEGER I,K,NGLUONS
      COMPLEX*16 W(6,MAXGLUONS,MAXGLUONS),W1(6),W2(6),W3(6),W4(6)
      COMPLEX*16 Z(MAXDIAG),WX(6,MAXDIAG),AMP_NGLUONS(6)
C     
C     GLOBAL VARIABLES
C     
      double complex G2I
      REAL*8 G2
      COMMON /CFQCD_COUPL/ g2, g2i
            

      ngluons = 5

      DO i=1,6
         AMP_NGLUONS(I) = (0D0,0D0)
         W(i,1,1) = W1(i)
         W(i,2,2) = W2(i)
         W(i,3,3) = W3(i)
         W(i,4,4) = W4(i)
      ENDDO
     
***************************************************************************

      DO i=1,3
         CALL JGGXXX(W(1,I,I),W(1,I+1,I+1), G2,W(1,I,I+1))     
      ENDDO

***************************************************************************

       DO i=1,2 
          CALL JGGXXX(W(1,I,I+1),W(1,I+2,I+2),              G2,WX(1,1))              
          CALL JGGXXX(W(1,I,I)  ,W(1,I+1,I+2),              G2,WX(1,2))              
          CALL JGGGOR(W(1,I,I)  ,W(1,I+1,I+1),W(1,I+2,I+2), G2I,WX(1,3))             
          CALL SUMW(WX,3,W(1,I,I+2))
       ENDDO
      
      
***************************************************************************

      i=1 
         CALL JGGXXX(W(1,I,I+2),W(1,I+3,I+3),             G2,WX(1,1))                
         CALL JGGXXX(W(1,I,I+1),W(1,I+2,I+3),             G2,WX(1,2))                
         CALL JGGXXX(W(1,I,I)  ,W(1,I+1,I+3),             G2,WX(1,3))                
         CALL JGGGOR(W(1,I,I+1),W(1,I+2,I+2),W(1,I+3,I+3),G2I,WX(1,4))               
         CALL JGGGOR(W(1,I,I)  ,W(1,I+1,I+2),W(1,I+3,I+3),G2I,WX(1,5))               
         CALL JGGGOR(W(1,I,I)  ,W(1,I+1,I+1),W(1,I+2,I+3),G2I,WX(1,6))               
         CALL SUMW(WX,6,AMP_NGLUONS)
 
      RETURN
      END
      
