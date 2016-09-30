***********************************************************************
*    =======================================                          *
      SUBROUTINE BSPRNT( LU, ID, IP1, IP2 )
*    =======================================                          *
* ((purpose))                                                         *
*     Print out routine of BASES.                                     *
*  (Argument)                                                         *
*     ID  : Identity number of printouts.                             *
*     IP1... IP2 : Integer                                            *
*  (Author)                                                           *
*     S. Kawabata   May 1992                                          *
*     Last update   March 1994                                        *
***********************************************************************

      IMPLICIT NONE
C     IMPLICIT REAL*8 (A-H,O-Z)

      INTEGER LU,ID,IP1,IP2

      INCLUDE 'base0.h'
      INCLUDE 'base1.h'
      INCLUDE 'base2.h'
      INCLUDE 'base3.h'
      INCLUDE 'base4.h'
      INCLUDE 'base5.h'

      INCLUDE 'bsrslt.h'

      INCLUDE 'bscntl.h'

      INCLUDE 'bdate.h'
      INCLUDE 'btime.h'

      REAL XTIME

      INCLUDE 'ninfo.h'

      CHARACTER*51 ICH(0:1)
      DATA  ICH / 'Convergency Behavior for the Grid Optimization Step',
     .            'Convergency Behavior for the Integration Step      '/
      CHARACTER*1 CN

      INTEGER IH,MN,IS1,IS2,NSP,MCALL
      DOUBLE PRECISION EXTIM

      INTEGER ISTEP,ITX,ITJ,NDEV,ITFN,ITMN,ITST,LOOP
      DOUBLE PRECISION RE,AC,ARE,F2,ORDER
      INTEGER IORDR,IT1

      INTEGER I,J


      IF( NODEID .NE. 0 ) RETURN
      CN = CHAR(12)

      GO TO ( 100, 200, 300, 400, 500, 600, 700, 800, 900, 1000 ), ID
C----------------------------------------------------------- BSMAIN

  100 IF( IPNT .EQ. 0 ) THEN
          WRITE(LU,9600)
 9600     FORMAT(/1H1,/1H )
      ELSE
          WRITE(LU,9610) CN
 9610     FORMAT(A1)
      ENDIF
      WRITE(LU,9620) (IDATE0(I),I=1,3),(ITIME0(J),J=1,2)
 9620 FORMAT(55X,'Date: ',I2,'/',I2,'/',I2,2X,I2.2,':',I2.2)
      WRITE(LU,9050)
 9050 FORMAT(
     . 8X,'**********************************************************',
     ./8X,'*                                                        *',
     ./8X,'*     BBBBBBB     AAAA     SSSSSS   EEEEEE   SSSSSS      *',
     ./8X,'*     BB    BB   AA  AA   SS    SS  EE      SS    SS     *',
     ./8X,'*     BB    BB  AA    AA  SS        EE      SS           *',
     ./8X,'*     BBBBBBB   AAAAAAAA   SSSSSS   EEEEEE   SSSSSS      *',
     ./8X,'*     BB    BB  AA    AA        SS  EE            SS     *',
     ./8X,'*     BB    BB  AA    AA  SS    SS  EE      SS    SS     *',
     ./8X,'*     BBBB BB   AA    AA   SSSSSS   EEEEEE   SSSSSS      *',
     ./8X,'*                                                        *',
     ./8X,'*                   BASES Version 5.1                    *',
     ./8X,'*           coded by S.Kawabata KEK, March 1994          *',
     ./8X,'**********************************************************')

          RETURN
C----------------------------------------------------------- BSMAIN

  200     IF( IPNT .EQ. 0 ) THEN
              WRITE(LU,9600)
          ELSE
              WRITE(LU,9610) CN
          ENDIF
          WRITE(LU,9300)
 9300     FORMAT(20X,
     .         '****** END OF BASES *********')

C----------------------------------------------------------- BSMAIN

  300 CONTINUE
      WRITE(LU,9305)
 9305 FORMAT(
     .//5X,'<<   Computing Time Information   >>')

*     WRITE(LU,9310) (IDATE0(I),I=1,3),(ITIME0(J),J=1,2)
*9310 FORMAT(/15X,'Start at: ',I2,'/',I2,'/',I2,2X,I2.2,':',I2.2)
*     CALL BSDATE
*     WRITE(LU,9320) (IDATE0(I),I=1,3),(ITIME0(J),J=1,2)
*9320 FORMAT(15X,'End   at: ',I2,'/',I2,'/',I2,2X,I2.2,':',I2.2)
      WRITE(LU,9330)
 9330 FORMAT(/15X,'(1) For BASES              H: M:  Sec')
      CALL BSTCNV(TIMEBS(2),IH,MN,IS1,IS2)
      WRITE(LU,9340) IH, MN, IS1, IS2
 9340 FORMAT(19X,'Overhead           : ',I3,':',I2,':',I2,'.',I2.2)
      CALL BSTCNV(TIMEBS(0),IH,MN,IS1,IS2)
      WRITE(LU,9350) IH, MN, IS1, IS2
 9350 FORMAT(19X,'Grid Optim. Step   : ',I3,':',I2,':',I2,'.',I2.2)
      CALL BSTCNV(TIMEBS(1),IH,MN,IS1,IS2)
      WRITE(LU,9360) IH, MN, IS1, IS2
 9360 FORMAT(19X,'Integration Step   : ',I3,':',I2,':',I2,'.',I2.2)
      XTIME = TIMEB2 - TIMEB1
      CALL BSTCNV(XTIME,IH,MN,IS1,IS2)
      WRITE(LU,9365) IH, MN, IS1, IS2
 9365 FORMAT(19X,'Go time for all    : ',I3,':',I2,':',I2,'.',I2.2)
      EXTIM  = TIMEBS(1)*1000.0/SCALLS/0.7
      WRITE(LU,9375)
 9375 FORMAT(/15X,'(2) Expected event generation time')
      WRITE(LU,9376) EXTIM
 9376 FORMAT(19X,'Expected time for 1000 events :',F10.2,' Sec')
      RETURN

C----------------------------------------------------------- BASES

  400 NSP   = NG**NWILD
      MCALL = NSP*NPG
      WRITE(LU,9400) NDIM,NWILD,MCALL,NCALL,ND,NG,NSP
 9400 FORMAT(
     .//5X,'<<   Parameters for BASES    >>',
     .//5X,' (1) Dimensions of integration etc.',
     . /5X,'     # of dimensions :    Ndim    =',I9,3X,'( 50 at max.)',
     . /5X,'     # of Wilds      :    Nwild   =',I9,3X,'( 15 at max.)',
     . /5X,'     # of sample points : Ncall   =',I9,'(real)',
     .                                         I9,'(given)',
     . /5X,'     # of subregions    : Ng      =',I9,' / variable',
     . /5X,'     # of regions       : Nregion =',I9,' / variable',
     . /5X,'     # of Hypercubes    : Ncube   =',I9,
     .//5X,' (2) About the integration variables')
      WRITE(LU,9405)
 9405 FORMAT(10X,'------',2('+---------------'),'+-------+-------')
      WRITE(LU,9410)
 9410 FORMAT(10X,'    i       XL(i)           XU(i)     ',
     .           '  IG(i)   Wild')
      WRITE(LU,9405)
       DO 450 I = 1,NDIM
          IF( I .LE. NWILD ) THEN
          WRITE(LU,9420) I,XL(I),XU(I),IG(I)
 9420     FORMAT(10X,I5,1P,2('  ',E14.6),'  ',3X,0P,I1,3X,
     .                       '   yes')
          ELSE
          WRITE(LU,9421) I,XL(I),XU(I),IG(I)
 9421     FORMAT(10X,I5,1P,2('  ',E14.6),'  ',3X,0P,I1,3X,
     .                        '    no')
          ENDIF
  450  CONTINUE
       WRITE(LU,9405)
       WRITE(LU,9450) ITMX1,ACC1,ITMX2,ACC2
 9450  FORMAT(
     . /5X,' (3) Parameters for the grid optimization step',
     . /5X,'     Max.# of iterations: ITMX1 =',I9,
     . /5X,'     Expected accuracy  : Acc1  =',F9.4,' %',
     .//5X,' (4) Parameters for the integration step',
     . /5X,'     Max.# of iterations: ITMX2 =',I9,
     . /5X,'     Expected accuracy  : Acc2  =',F9.4,' %')

          RETURN
C----------------------------------------------------------- BASES

  500    IF( INTV .LE. 1 )    RETURN
         ISTEP  = IP1
         IF( IPNT .EQ. 0 ) THEN
             WRITE(LU,9600)
         ELSE
             WRITE(LU,9610) CN
         ENDIF
         WRITE(LU,9620) (IDATE0(I),I=1,3),(ITIME0(J),J=1,2)
         WRITE(LU,9500) ICH(ISTEP)
 9500    FORMAT(15X,A)
         WRITE(LU,9570)
         WRITE(LU,9550)
 9550    FORMAT(1X,'<- Result of  each iteration ->',
     .          2X,'<-     Cumulative Result     ->',
     .          1X,'< CPU  time >',
     .         /1X,' IT Eff R_Neg   Estimate  Acc %',
     .          2X,'Estimate(+- Error )order  Acc %',
     .          1X,'( H: M: Sec )')
         WRITE(LU,9570)
 9570    FORMAT(1X,7('----------'),'--------')
         RETURN

C----------------------------------------------------------- BASES

  600    IF( INTV .LE. 1 ) RETURN
         ISTEP  = IP1
         ITX = MOD( IT, ITM)
         IF( ITX .EQ. 0 ) ITX = ITM

         CALL BSLIST( LU, ITX, ISTEP )

         RETURN

  700    IF( INTV .LE. 1 ) RETURN
         WRITE(LU,9570)

         RETURN
C----------------------------------------------------------- BASES

  800    ITJ    = IP1
         ISTEP  = IP2
         ITX  = MOD( ITJ, ITM )
         IF( ITX .EQ. 0 ) ITX = ITM

         IF( ITRAT(1,ISTEP) .EQ. 1 ) THEN
             NDEV   = 1
         ELSE
             NDEV   = 2
             ITFN   = ITM
             ITMN   = 10000
             DO 610 I = 1,ITM
                IF( ITRAT(I,ISTEP) .LT. ITMN ) THEN
                    ITST = I
                    ITMN = ITRAT(I,ISTEP)
                ENDIF
  610        CONTINUE
             IF( ITST .EQ. 1 ) NDEV = 1
         ENDIF

         IF( IPNT .EQ. 0 ) THEN
             WRITE(LU,9600)
         ELSE
             WRITE(LU,9610) CN
         ENDIF
         WRITE(LU,9620) (IDATE0(I),I=1,3),(ITIME0(J),J=1,2)
         WRITE(LU,9500) ICH(ISTEP)
         WRITE(LU,9570)
         WRITE(LU,9550)
         WRITE(LU,9570)

  625    IF( NDEV .EQ. 1 ) THEN
             ITST = 1
             ITFN = ITX
         ENDIF

         DO 650 I = ITST, ITFN

            CALL BSLIST( LU, I, ISTEP )

  650    CONTINUE
         NDEV  = NDEV - 1
         IF( NDEV .GT. 0 ) GO TO 625
         WRITE(LU,9570)

      RETURN

C----------------------------------------------------------- BASES

  900 WRITE(LU,9950)
 9950 FORMAT(1X,'******** FATAL ERROR IN BASES **************',
     .      /1X,'There are no enough good points in this iteration.',
     .      /1X,'Process was terminated due to this error.')

      RETURN

C-----------------------------------------------------------------
 1000 LOOP = IP1
      IF( IP2 .NE. 0 ) THEN
          IF( IPNT .EQ. 0 ) THEN
              WRITE(LU,9600)
           ELSE
              WRITE(LU,9610) CN
           ENDIF
           WRITE(LU,9620) (IDATE0(I),I=1,3),(ITIME0(J),J=1,2)
           WRITE(LU,9650)
 9650      FORMAT(
     .      20X,'Results of Integration',
     .     /10X,5('----------'),'------',
     .     /10X,' Loop#  Estimate(+- Error )order',
     .                     '  It1  It2 ( H: M: Sec )',
     .     /10X,5('----------'),'------')
      ENDIF

      RE  = AVGI
      AC  = ABS(SD)
      ARE = ABS(RE)
      IF( ARE .GE. AC) THEN
          CALL BSORDR( ARE, F2, ORDER, IORDR)
      ELSE
          CALL BSORDR(  AC, F2, ORDER, IORDR )
      ENDIF
      RE  = RE/ORDER
      AC  = AC/ORDER
      CALL BSTCNV( STIME, IH, MN, IS1, IS2)
      WRITE(LU,9660) LOOP,RE,AC,IORDR,IT1,IT,IH,MN,IS1,IS2
 9660 FORMAT(10X,I6,F10.6,'(+-',F8.6,')E',I3.2,2I5,
     .        1X,I3,':',I2,':',I2,'.',I2.2,
     .      /10X,5('----------'),'------')

      RETURN
      END
