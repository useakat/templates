************************************************************************
*    ===================                                               *
      SUBROUTINE SHCLER
*    ===================                                               *
* ((FUNCTION))                                                         *
*     To cancel the update of histograms and scatter plots in case     *
*   of the trial was rejected.                                         *
* ((Author))                                                           *
*     S.Kawabata June '90 at KEK                                       *
*                                                                      *
************************************************************************

      IMPLICIT NONE

      INCLUDE 'plot.h'

      INTEGER IP3
      INTEGER J,K


      IF( NHIST .GT. 0 ) THEN
         DO 200  J   = 1, NHIST
           IP3       = MAPL(3,J)
           IBUF(IP3) = -1
  200    CONTINUE
      ENDIF
C
      IF( NSCAT .GT. 0 ) THEN
         DO 500   K    = 1, NSCAT
           IP3         = MAPD(4,K)
           IBUF(IP3)   =  0
           IBUF(IP3+1) =  0
  500    CONTINUE
      ENDIF
C
      RETURN
      END
