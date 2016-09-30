************************************************************************
*     =========================                                        *
      SUBROUTINE BHPLOT( LUN )
*     =========================                                        *
* ((Purpose))                                                          *
*     Interface routine to print histograms and scatter plots.         *
*     Routines XHPLOT and DHPLOT are called to print them.             *
* ((Author))                                                           *
*     S.Kawabata  June '90  at KEK                                     *
************************************************************************

      IMPLICIT NONE

      INTEGER LUN

      INCLUDE 'plot.h'

      INTEGER J
*                                                                      *
*--------------------------- Entry point ------------------------------*
*    ===================                                               *
      CALL XHCHCK( LUN )
*    ===================

      IF( NHIST .LE. 0 ) THEN
         WRITE(LUN,9000)
 9000    FORMAT(1X,'No Histogram')
      ELSE
         DO 500 J = 1, NHIST
            IFBASE(J) = 1
*          =====================
            CALL XHPLOT(LUN, 0, J )
*          =====================
  500    CONTINUE
      ENDIF

*    ===================
      CALL DHPLOT( LUN )
*    ===================

      RETURN
      END
