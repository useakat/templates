      subroutine JGLUO6(W1,W2,W3,W4,W5,AMP_NGLUONS) 
      IMPLICIT NONE
C
C     PARAMETERS
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
      INTEGER I
      COMPLEX*16 W(6,MAXGLUONS,MAXGLUONS),W1(6),W2(6),W3(6),W4(6),W5(6)
      COMPLEX*16 Z(MAXDIAG),WX(6,MAXDIAG),AMP_NGLUONS(6)
C     
C     GLOBAL VARIABLES
C     
      double complex G2I
      REAL*8 G2
      COMMON /CFQCD_COUPL/ g2,g2i
      
      DO I = 1,6
         AMP_NGLUONS(I) = (0D0,0D0)
      ENDDO
      ngluons = 6
      do i=1,6
         W(i,1,1) = W1(i)
         W(i,2,2) = W2(i)
         W(i,3,3) = W3(i)
         W(i,4,4) = W4(i)
         W(i,5,5) = W5(i)
      enddo

***************************************************************************

      DO i=1,ngluons-2
         CALL JGGXXX(W(1,I,I),W(1,I+1,I+1), G2,W(1,I,I+1))     
      ENDDO

***************************************************************************

       DO i=1,ngluons-3  
          CALL JGGXXX(W(1,I,I+1),W(1,I+2,I+2),              G2,WX(1,1))              
          CALL JGGXXX(W(1,I,I)  ,W(1,I+1,I+2),              G2,WX(1,2))              
          CALL JGGGOR(W(1,I,I)  ,W(1,I+1,I+1),W(1,I+2,I+2), G2,WX(1,3))              
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

      DO i=1,ngluons-5  
C 3-point
         CALL JGGXXX(W(1,I,I+3),W(1,I+4,I+4),             G2,WX(1, 1))               
         CALL JGGXXX(W(1,I,I+2),W(1,I+3,I+4),             G2,WX(1, 2))               
         CALL JGGXXX(W(1,I,I+1),W(1,I+2,I+4),             G2,WX(1, 3))               
         CALL JGGXXX(W(1,I,I)  ,W(1,I+1,I+4),             G2,WX(1, 4))               
C 4-point
         CALL JGGGOR(W(1,I,I+2),W(1,I+3,I+3), W(1,I+4,I+4),G2I,WX(1, 5))             
         CALL JGGGOR(W(1,I,I+1),W(1,I+2,I+2), W(1,I+3,I+4),G2I,WX(1, 6))             
         CALL JGGGOR(W(1,I,I+1),W(1,I+2,I+3), W(1,I+4,I+4),G2I,WX(1, 7))             
         CALL JGGGOR(W(1,I,I)  ,W(1,I+1,I+1), W(1,I+2,I+4),G2I,WX(1, 8))             
         CALL JGGGOR(W(1,I,I)  ,W(1,I+1,I+2), W(1,I+3,I+4),G2I,WX(1, 9))             
         CALL JGGGOR(W(1,I,I)  ,W(1,I+1,I+3), W(1,I+4,I+4),G2I,WX(1,10))             
         CALL SUMW(WX,10,aMP_NGLUONS)
      ENDDO

      RETURN
      END

