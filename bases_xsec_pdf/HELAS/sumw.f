      SUBROUTINE SUMW(WX,NDIAG,WOUT)
c************************************************************************

      IMPLICIT NONE
c
c      INCLUDE 'maxgluons.inc'
C
C     ARGUMENTS
C
      INTEGER   MAXGLUONS,   MAXDIAG
      PARAMETER(MAXGLUONS=12,MAXDIAG=55)
      COMPLEX*16 WX(6,MAXDIAG)
      INTEGER NDIAG
      COMPLEX*16 WOUT(6)
C 
C     LOCAL
C
      INTEGER I,J
      
C-- first contribution included component 5 and 6 which do not change       

      DO J=1,6
         WOUT(J)=WX(J,1)
      ENDDO

c-- all the others which are summed over

      DO I=2,NDIAG
         DO J=1,4
            WOUT(J)=WOUT(J)+WX(J,I)
         ENDDO
      ENDDO


      RETURN
      END
      
