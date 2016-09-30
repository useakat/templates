************************************************************************
*                                                                      *
*    ==========================                                        *
      SUBROUTINE BSGETI( AVGI, SD)
*    ==========================                                        *
* ((Function))                                                         *
*     Read temporary result from the logocal unit LUN                  *
* ((Auther))                                                           *
*     S.Kawabata    June '90 at KEK                                    *
*                                                                      *
************************************************************************


      IMPLICIT NONE

      DOUBLE PRECISION AVGI,SD

      INCLUDE 'base5.h'
      
      INTEGER ISTEP,I,ITX


      ISTEP = 1

      ITX = 0
      DO 10 I=1,ITM
      IF(ITRAT(I,ISTEP).EQ.0) GO TO 20
      ITX = ITRAT(I,ISTEP)
   10 CONTINUE

   20 CONTINUE
      IF(ITX.NE.ITRAT(ITX,ISTEP)) THEN
         PRINT *,'SPRSLT;ITX,ITRAT = ',ITX,ITRAT(ITX,ISTEP)
         RETURN
      END IF

      AVGI = RESLT(ITX,ISTEP)
      SD   = ACSTD(ITX,ISTEP)

      RETURN
      END
