************************************************************************
*    ===============================                                   *
      SUBROUTINE XHSAVE( LUNIT, ID )
*    ===============================                                   *
* ((Purpose))                                                          *
*      To write the ID-th histogram on the unit LUNIT.                 *
* ((Input))                                                            *
*      LUNIT: Logical unit number                                      *
*      ID   : Historgram ID                                            *
*                                                                      *
* ((Author))                                                           *
*       S.Kawabata   June '90 at KEK                                   *
*                                                                      *
************************************************************************

      IMPLICIT NONE

      INTEGER LUNIT,ID

      INCLUDE 'base3.h'

      INCLUDE 'plot.h'

      INTEGER IHIST,NTOTAL
      INTEGER IP1,IP2,IP3,IPF,IPF2
      REAL XMIN,XMAX,DEV
      INTEGER NXBIN,NX
      REAL FACT,TX,VLS,DEV2,VER,XX

      INTEGER I,K

      IF( NHIST .GT. 0 ) THEN

          I  = IABS(MOD( ID, 13 )) + 1
          IF( XHASH(1, I) .EQ. 1 ) THEN
            IF( ID .EQ. MAPL( 1, XHASH(2,I))) THEN
                IHIST = XHASH(2,I)
                GO TO 200
            ENDIF
          ELSEIF( XHASH(1, I) .GT. 1 ) THEN
            DO 100 K = 2, XHASH(1,I)+1
               IF( ID .EQ. MAPL( 1, XHASH(K,I))) THEN
                   IHIST = XHASH(K,I)
                   GO TO 200
               ENDIF
  100       CONTINUE
          ENDIF
      ENDIF

      WRITE(LU,9000) ID
 9000 FORMAT(1X,'************* Warning from XHSAVE *************',
     .      /1X,' Histogram ID(',I5,' ) is illegal.',
     .      /1X,' This call is neglected.',
     .      /1X,'**********************************************')
      RETURN

  200 NTOTAL= SCALLS
      IP1   = MAPL(2,IHIST)
      XMIN  = BUFF(IP1)
      XMAX  = BUFF(IP1+1)
      NXBIN = IBUF(IP1+2)
      DEV   = BUFF(IP1+3)
      IP2   = MAPL(3,IHIST)
      IP3   = MAPL(4,IHIST)

      write(lunit,*) 'SET DEVICE POSTSCRIPT ORIENT=3'
c      WRITE(LUNIT,9200) ID,LUNIT,(BUFF(I),I=IP3+1,IP3+15),
c     .              NTOTAL,NXBIN,DEV
c 9200 FORMAT(/1H1,
c     .      1X, 'SET DEVICE POSTSCRIPT ORIENT=3')
c     .      1X,'** Histogram ID(',I5,' ) was saved in Unit(',I2,') **',
c     .      /1X,'Title : ',15A4,
c     .      /1X,'Entries     =',I10,
c     .      /1X,'No. of bins =',I10,'  Width =',G13.4)

c      WRITE( LUNIT, 9300) ID
c 9300 FORMAT('(...... ID =',I5,' ......')

      WRITE( LUNIT, 9400)
c 9400 FORMAT(1X,'NEW FRAME',
 9400 FORMAT(1X,'SET FONT DUPLEX')
      WRITE( LUNIT, 9500) (BUFF(I), I=IP3+1,IP3+15)
c 9500 FORMAT(1X,'TITLE TOP ','''',15A4,'''')
 9500 FORMAT(1X,'TITLE TOP ')

      WRITE( LUNIT, 9600) XMIN,XMAX
 9600 FORMAT(1X,'SET LIMIT X FROM ',G12.4,'TO ',G12.4)

      WRITE( LUNIT, 9700)
 9700 FORMAT(1X,'SET INTENSITY 4',
     .      /1X,'SET TICK SIZE 0.04',
     .      /1X,'SET ORDER X Y DY')

      IPF   = IP2 + 156
      IPF2  = IPF + 52
      FACT       = 1./(NTOTAL*DEV)
c      FACT       = 1d0
      DO 400 I = 1, NXBIN
         TX     = BUFF(I+IPF)
         NX     = IBUF(I+IP2)
         VLS    = TX*FACT
         IF( NX .GT. 1 ) THEN
             DEV2   =  NX*BUFF(I+IPF2)-TX*TX
             IF( DEV2 .LE. 0.0 ) THEN
                 VER = 0.0
             ELSE
                 VER = FACT*SQRT( DEV2/( NX-1 ))
             ENDIF
         ELSEIF( NX .EQ. 1 ) THEN
             VER = VLS
         ELSE
             VER = 0.0
         ENDIF
         XX     = XMIN + DEV*(FLOAT(I) - 0.5)
         WRITE( LUNIT, 9800) XX, VLS, VER, 0d0
 9800    FORMAT(1X,E11.4,3X,E14.7,3X,E14.7,3X,E14.7)
  400 CONTINUE

      WRITE( LUNIT, 9900)
 9900 FORMAT( 1X,'HIST')

      RETURN
      END
