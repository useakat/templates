************************************************************************
*    =========================                                         *
      SUBROUTINE SHPLOT( LUN )
*    =========================                                         *
C*((Function))                                                         *
C*    To print histograms and scatter plots defined by XHINIT and      *
C*  DHINIT.                                                            *
C*    For the original histograms, a special histograms are printd     *
C*  by this routine. For the additional histograms and scatter plots   *
C*  routines XHPLOT and DHPLOT are called.                             *
C*((Author))                                                           *
C*    S.Kawabata   June '90 at KEK                                     *
C*                                                                     *
C***********************************************************************

      IMPLICIT NONE

      INTEGER LUN

      INCLUDE 'base3.h'

      INCLUDE 'plot.h'

      INCLUDE 'bscntl.h'

      INTEGER IP1,IP2,IP3
      INTEGER IPX,IPF,IPF2

      REAL YMAX
      PARAMETER (YMAX=50.)

      INTEGER NXBIN,NX
      REAL XMIN,XMAX,DEV,DEV2
      REAL VMAX,VORG,VEVT,FACT,VEVT1,TX,VLS

      INTEGER IHIST,NTOT
      REAL VNORM,XNORM,VLMIN,VLMAX,VLSTP
      REAL VXMIN,VXMAX
      REAL UNITL,VX,F2,ORD
      INTEGER IORD,NUMBL,NUMB
      REAL XL,XM,ERL,DERL,ORDER,RNORM
      INTEGER NERUP,NERLW,IORDR

      INTEGER I,K


      CHARACTER*50 CHARR,CHR1
      CHARACTER*52 SCALE
      REAL  VAL(0:51),VLOG(0:51)
      REAL  VERR(0:51)
      CHARACTER*1  BLNK,STAR,CROS,AI,CN
      DATA  BLNK /' '/, STAR /'*'/, CROS /'O'/
      DATA  AI /'I'/

      CN  = CHAR(12)

      CALL XHCHCK( LUN )

      IF( NHIST .GT. 0 ) THEN
*                 add March 1994
         CALL SHUPDT
*
C        NTOTAL= SCALLS
         DO 500 IHIST = 1, NHIST
          IF(IFBASE(IHIST) .EQ. 1 ) THEN
            IP3  = MAPL(4,IHIST)
            IF( IPNT .EQ. 0 ) THEN
                WRITE(LUN,9010)
            ELSE
                WRITE(LUN,9020) CN
            ENDIF
 9010       FORMAT(/1H1)
 9020       FORMAT(A1)
            WRITE(LUN,9050) MAPL(1,IHIST),(BUFF(I), I=IP3+1,IP3+15)
 9050       FORMAT(1X,'Original Histogram (ID =',I3,' ) for ',15A4)

            IP1   = MAPL(2,IHIST)
            XMIN  = BUFF(IP1)
            XMAX  = BUFF(IP1+1)
            NXBIN = IBUF(IP1+2) + 1
            DEV   = BUFF(IP1+3)
            VMAX  = 0.0
            VORG  = 0.0
            VEVT  = 0.0
C           FACT       = 1./(NTOTAL*DEV)
            FACT       = 1./(SCALLS*DEV)
            IP2   = MAPL(3,IHIST)
            IPX   = IP2 + 52
            IPF   = IP2 + 156
            IPF2  = IPF + 52
C           VAL(0)     = BUFF(IPF)/NTOTAL
            VAL(0)     = BUFF(IPF)/SCALLS
C           VAL(NXBIN) = BUFF(IPF+NXBIN)/NTOTAL
            VAL(NXBIN) = BUFF(IPF+NXBIN)/SCALLS
            VEVT1 = BUFF(IPX) + BUFF(IPX+NXBIN)
            DO  50 I   = 1,NXBIN-1
                TX     = BUFF(I+IPF)
                NX     = IBUF(I+IP2)
                VLS    = TX*FACT
                IF( VMAX .LT. VLS ) VMAX = VLS
                VAL(I) = VLS
                IF( NX .GT. 1 ) THEN
                  DEV2   =  NX*BUFF(I+IPF2)-TX*TX
                  IF( DEV2 .LE. 0.0 ) THEN
                      VERR(I)= 0.0
                  ELSE
                      VERR(I)= FACT*SQRT( DEV2/( NX-1 ))
                  ENDIF
                ELSEIF( NX .EQ. 1 ) THEN
                  VERR(I)= VLS
                ELSE
                  VERR(I)= 0.0
                ENDIF
                VORG   = VLS + VORG
                VEVT   = BUFF(I+IPX) + VEVT
   50       CONTINUE
            NTOT   = INT(VEVT+VEVT1)
            IF( VMAX .LE. 0.0 .AND. VEVT .GT. 0.0 ) THEN
                  WRITE(LUN,9060) MAPL(1,IHIST)
 9060             FORMAT(/5X,'***************************************',
     .                   /5X,'* Since BASES has no entry            *',
     .                   /5X,'*     in the histogram ID(',I6,' ),   *',
     .                   /5X,'*  an additional hist. is given       *',
     .                   /5X,'*     in the next page in stead.      *',
     .                   /5X,'***************************************')
C
                  CALL XHPLOT( LUN, 1, IHIST )
C
                  GO TO 500
            ELSEIF( VEVT .LE. 0) THEN
                  WRITE(LUN,9070) IHIST
 9070             FORMAT(/5X,'***************************************',
     .                   /5X,'*    SPRING has no entry              *',
     .                   /5X,'*     in the histogram ID(',I6,' )    *',
     .                   /5X,'***************************************')
                  GO TO 500
            ENDIF
            VNORM = VORG/VEVT
            XNORM = VNORM*DEV
            VLMAX = ALOG10(VMAX)
            VLMIN = VLMAX
            DO  60 I = 0,NXBIN
              IF( VAL(I) .GT. 0.0 ) THEN
                  VLS   = ALOG10( VAL(I) )
                 IF( I .GT. 0 .AND. I .LT. NXBIN ) THEN
                    IF( VLS .LT. VLMIN ) VLMIN = VLS
                 ENDIF
                 VLOG(I)  = VLS
              ELSE
                 VLOG(I)  = 0.0
              ENDIF
   60       CONTINUE
C
             VXMAX = VLMAX
             IF( VLMIN .LT. 0.0) THEN
                VXMIN = IFIX(VLMIN) - 1.0
             ELSE
                VXMIN = IFIX(VLMIN)
             ENDIF
             CALL XHRNGE( 1, VXMIN, VXMAX, VLMIN, VLMAX, VLSTP)
             UNITL = (VLMAX-VLMIN)/YMAX
C
             CALL XHSCLE( 1, VLMIN, VLMAX, VLSTP, UNITL, SCALE, CHR1)
C
C
             WRITE(LUN,9150) NTOT
 9150        FORMAT(1X,'Total =',I10,' events',
     .              3X,'"*" : Orig. Dist. in Log Scale.')
             VXMIN = 10.0**VLMIN
             WRITE(LUN,9200) SCALE
 9200        FORMAT(1X,'    x      d(Sig/dx) dN/dx',A52)
             WRITE(LUN,9250) CHR1
 9250        FORMAT(1X,
     .             '+--------+----------+------+',
     .       A50 )
C

            VX    = ABS(XMAX)
            XM    = ABS(XMIN)
            IF( XM .GT. VX ) VX = XM

            CALL XHORDR( VX, F2, ORD, IORD )

            DO 200 I = 0,NXBIN
              RNORM = VNORM
              IF( I .EQ. 0 .OR. I .EQ. NXBIN ) RNORM = XNORM
              VX    = VAL(I)
              XL     = BUFF( I + IPX )
              NX     = XL
              IF( VX .GT. 0.0 ) THEN
                 NUMBL  = (VLOG(I) - VLMIN)/UNITL + 1.0
              ELSE
                 NUMBL  = 0
              ENDIF
              IF( NX .GT. 0 ) THEN
                 NUMB   = ( LOG10( XL*RNORM ) - VLMIN)/UNITL + 1.0
                 ERL    = SQRT(XL)
                 DERL   = (XL + ERL)*RNORM
                 NERUP  = ( LOG10( DERL ) - VLMIN)/UNITL + 1.0
                 DERL   = (XL - ERL)*RNORM
                 IF( DERL .GT. 0.0 ) THEN
                     NERLW  = ( LOG10( DERL ) - VLMIN)/UNITL + 1.0
                 ELSE
                     NERLW  = 0
                 ENDIF
              ELSE
                 NUMB   = 0
                 NERUP  = 0
                 NERLW  = 0
              ENDIF
              IF( NUMB  .GT. 50 ) NUMB = 50
              IF( NUMBL .GT. 50 ) NUMBL= 50
              DO 100 K = 1,50
                IF( K .LE. NUMBL) THEN
                  CHARR(K:K) = STAR
                ELSE
                  IF( K .EQ. 50 ) THEN
                    CHARR(K:K) = AI
                  ELSE
                    CHARR(K:K) = BLNK
                  ENDIF
                ENDIF
C
                IF(     K .EQ. NUMB ) THEN
                        CHARR(K:K) = CROS
                        IF( K .EQ. NERUP .AND. K .EQ. NERLW ) GO TO 100
                ENDIF
                IF(     K .EQ. NERUP ) THEN
                        CHARR(K:K) = '>'
                ELSEIF( K .EQ. NERLW ) THEN
                        CHARR(K:K) = '<'
                ENDIF

  100         CONTINUE

              CALL XHORDR( VX, F2, ORDER, IORDR )

             IF( I .EQ. 0 .OR. I .EQ. NXBIN ) THEN
                 WRITE(LUN,9300) IORD,F2,IORDR,NX,CHARR
 9300            FORMAT(1X,'I   E',I3,' I',F6.3,'E',I3,'I',
     .                                            I6,'I',A50)
             ELSE
                   XM    = (XMIN + DEV*(I-1))/ORD
                   WRITE(LUN,9340) XM,F2,IORDR,NX,CHARR
 9340              FORMAT(1X,'I ',F6.3,' I',F6.3,'E',I3,'I',
     .                                        I6,'I',A50)
             ENDIF
  200       CONTINUE
             WRITE(LUN,9250) CHR1
             WRITE(LUN,9260)
 9260    FORMAT(1X,
     .       '    x      d(Sig/dx) dN/dx',4X,
     .       '"O" : Generated Events.',
     .       '( Arbitrary unit in Log )')
C
           ELSE
C
              CALL XHPLOT( LUN, 1, IHIST )
C
           ENDIF
  500    CONTINUE
      ENDIF
C
      CALL DHPLOT( LUN )
C
      RETURN
      END
