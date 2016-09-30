************************************************************************
*    ==================================                                *
      SUBROUTINE SPRING(FUNC, MXTRY0 )
*    ==================================                                *
*         Main Program for the Event generation program SPRING.        *
*                                                                      *
*        Coded by S.Kawabata        September '84                      *
*                                                                      *
************************************************************************

      IMPLICIT NONE
C     IMPLICIT REAL*8 (A-H,O-Z)

      DOUBLE PRECISION FUNC
      EXTERNAL FUNC
      INTEGER MXTRY0

      INCLUDE 'base0.h'
      INCLUDE 'base1.h'
      INCLUDE 'base4.h'
      INCLUDE 'bdate.h'
      INCLUDE 'bscntl.h'

      INCLUDE 'sprng1.h'
      INCLUDE 'sprng2.h'

      INCLUDE 'btime.h'

      INTEGER MISFLG
      INTEGER NBIN,MXTRY1,IRET

      INTEGER I
*                                                                      *
*----------------------------- Entry point ----------------------------*
*                                                                      *
*======================================================================*
*                  Initialization of the program                       *
*======================================================================*
*----------------------------------------------------------------------*
*                     initialize timer etc.                            *
*----------------------------------------------------------------------*
*                                                                      *
       IF( IBASES .GT. 0 ) THEN

C          CALL SPCHCK

           CALL BSTIME( TIME0, 0 )
           TIMES1 = TIME0

           MXTRY  = MXTRY0
           INTV   = 0
           IBASES = 0
           MISFLG = 0

           CALL BSDATE

           DO 10 I = 0,2
              TIMESP(I) = 0.0
   10      CONTINUE
*                                                                      *
            IF( MXTRY .LT. 10 ) MXTRY = 50
            NBIN    = MXTRY
            IF( MXTRY .GT. 50) NBIN = 50
            MXTRY1  = MXTRY + 1
            MISS    = 0
            NEVENT  = 0
            NTRIAL  = 0

            CALL SHINIT( MXTRY1 )

*           -------------
             CALL SHRSET
*            -------------
*----------------------------------------------------------------------*
*             Make the cumulative probability distribution             *
*----------------------------------------------------------------------*
*                                                                      *
            XND     = ND
            DXG     = XND/NG
            NSP     = NG**NWILD

*///// DEBUG
*       MCALL   = NSP*NPG
*       CALL BSPRNT( 4, MCALL, IDUM2, IDUM3, IDUM4 )
*
            XJAC    = 1.0
            DO 50 I = 1, NDIM
               XJAC = XJAC*DX(I)
   50       CONTINUE
            DXMAX   = 0.0D0
            DO 100  I = 1,NSP
               IF( DXD( I ) .LT. 0.0D0 ) THEN
                   WRITE(6,9100) I
 9100              FORMAT(
     .             /5X,'********** FATAL ERROR IN SPRING **********',
     .             /5X,'*     Negative probability was found      *',
     .             /5X,'*        in the ',I6,'-th Hypercube.      *',
     .             /5X,'*******************************************')
                   STOP
               ENDIF

               DXMAX    = DXMAX + DXD( I )
               DXD(I)   = DXMAX
  100       CONTINUE
*        =====================
          CALL BSUTIM( 1, 1 )
*        =====================
      ENDIF
*     =====================
       CALL BSUTIM( 1, 2 )
*     =====================
      IF( IBASES .EQ. 1 ) THEN
          WRITE(6,9000)
 9000     FORMAT(
     .      1X,'**************************************************',
     .     /1X,'*    Flag IBASES was not equal to "0".           *',
     .     /1X,'*                                                *',
     .     /1X,'*   Process was terminated by this error.        *',
     .     /1X,'*   Call S.Kawabata.                             *',
     .     /1X,'**************************************************')
           STOP
       ENDIF
*                                                                      *
*======================================================================*
*                       Event generation                               *
*======================================================================*
*     =====================
  500  CALL BSUTIM( 1, 1 )
*     =====================

*     ==================================
        CALL SPRGEN( FUNC, MXTRY, IRET)
*     ==================================

*     =====================
       CALL BSUTIM( 1, 0 )
*     =====================

      CALL SHFILL( IRET )

      IF( IRET .LE. MXTRY ) THEN
          NTRIAL =NTRIAL + IRET
          NEVENT = NEVENT + 1
          CALL SHUPDT
      ELSE
          NTRIAL =NTRIAL + IRET - 1
          MISS = MISS + 1
          IF( MISFLG .EQ. 0 .AND. MISS .GT. MXTRY ) THEN
              WRITE(6,9600) MXTRY
 9600         FORMAT(1X,'****************************************',
     .                  '****************************************',
     .              /1X,'* (((( Warning ))))                     ',
     .                  '                                       *',
     .              /1X,'*                                       ',
     .                  '                                       *',
     .              /1X,'*  The number of mis-generations is foun',
     .                  'd more than',I3,' times.                  *')
              WRITE(6,9610)
 9610         FORMAT(1X,'*                                       ',
     .                  '                                       *',
     .              /1X,'*(( Suggestion ))                       ',
     .                  '                                       *',
     .              /1X,'* (1) Try integration again with larger ',
     .                  'number of sample points than this job. *',
     .              /1X,'* or                                    ',
     .                  '                                       *',
     .              /1X,'* (2) The integral variables are not sui',
     .                  'ted for the function.                  *',
     .              /1X,'*     Take another integral variables !!',
     .                  '                                       *',
     .              /1X,'*                                       ',
     .                  '                                       *',
     .              /1X,'****************************************',
     .                  '****************************************')
            MISFLG = 1
          ENDIF
          GO TO 500
      ENDIF
*     =====================
  600  CALL BSUTIM( 1, 1 )
*     =====================

      RETURN
      END
