************************************************************************
*    ====================                                              *
      SUBROUTINE SHRSET
*    ====================                                              *
* ((Function))                                                         *
*     To reset the content of histograms and scatter plots.            *
* ((Author))                                                           *
*     S.Kawabata   June '90 at KEK                                     *
*                                                                      *
* **********************************************************************

      IMPLICIT NONE

      INCLUDE 'plot.h'

      INTEGER IHIST,ISCAT
      INTEGER IP2,IP3

      INTEGER I

      IF( NHIST .GT. 0 ) THEN
         DO 100 IHIST = 1, NHIST
            IP2       = MAPL(3,IHIST) + 52
            IP3       = MAPL(4,IHIST)
            IBUF(IP3) = -1
            DO 100 I = 0,51
               BUFF(I+IP2) = 0.0
  100      CONTINUE
      ENDIF
C
      IF( NSCAT .GT. 0 ) THEN
         DO 400   ISCAT = 1, NSCAT
            IP3         = MAPD(4,ISCAT)
            IBUF(IP3)   = 0
            IBUF(IP3+1) = 0
            IP2         = MAPD(3,ISCAT)
            IBUF(IP2)   = 0
            DO 400   I  = IP2+1,IP2+2500
               BUFF(I)  = 0.0
  400      CONTINUE
      ENDIF
C
      RETURN
      END
