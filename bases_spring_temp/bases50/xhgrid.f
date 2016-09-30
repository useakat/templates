C   =====================================
      SUBROUTINE XHGRID( LUNIT )
C   =====================================

      IMPLICIT NONE
C     IMPLICIT REAL*8 (A-H,O-Z)

      INTEGER LUNIT

      INCLUDE 'base1.h'
      INCLUDE 'base4.h'
      INCLUDE 'base6.h'

      INTEGER NO,MPAGE,NPAGE,NFIGS,NF
      DOUBLE PRECISION XND,XJ

      INTEGER I

      CHARACTER*50 WINDOW(8)
      DATA WINDOW /'SET WINDOW X FROM  1.0 TO  5.0 Y FROM  8.0 TO 11.5',
     .             'SET WINDOW X FROM  5.5 TO  9.5 Y FROM  8.0 TO 11.5',
     .             'SET WINDOW X FROM 10.0 TO 14.0 Y FROM  8.0 TO 11.5',
     .             'SET WINDOW X FROM 14.5 TO 18.5 Y FROM  8.0 TO 11.5',
     .             'SET WINDOW X FROM  1.0 TO  5.0 Y FROM  3.0 TO  6.5',
     .             'SET WINDOW X FROM  5.5 TO  9.5 Y FROM  3.0 TO  6.5',
     .             'SET WINDOW X FROM 10.0 TO 14.0 Y FROM  3.0 TO  6.5',
     .             'SET WINDOW X FROM 14.5 TO 18.5 Y FROM  3.0 TO  6.5'/
      REAL X(8),Y(8)
      DATA X/  2.0, 6.5, 11.0, 15.5, 2.0, 6.5, 11.0, 15.5 /
      DATA Y/  4*7.5, 4*2.5/

      NO     = 0
      XND    = FLOAT(ND)
      MPAGE  = (NDIM-1)/8 + 1
      DO 1000 NPAGE = 1, MPAGE

         NFIGS = 8
         IF( NPAGE .EQ. MPAGE ) NFIGS = MOD(NDIM, 8)
         IF( NFIGS .GT. 0 ) THEN

            WRITE( LUNIT, 9000)
 9000       FORMAT(1X,'NEW FRAME')
            WRITE( LUNIT, 9001)
 9001       FORMAT(1X,'SET FONT DUPLEX')
            WRITE( LUNIT, 9002)
 9002       FORMAT(1X,'SET SIZE 20 BY 14.00')

            DO 500 NF = 1, NFIGS
               NO    = NO + 1
               WRITE( LUNIT, 9003) WINDOW(NF)
 9003          FORMAT(1X,A)
               WRITE( LUNIT, 9004) XND
 9004          FORMAT(1X,'SET LIMIT X FROM 0.0 TO ',G12.4,
     .               /1X,'SET LABEL LEFT OFF')
               WRITE( LUNIT, 9005)
 9005          FORMAT(1X,'SET INTENSITY 4',
     .               /1X,'SET TICK SIZE 0.04',
     .               /1X,'SET ORDER X Y ')
               DO 200 I = 1, ND
                  WRITE( LUNIT, 9006) FLOAT(I),D(I,NO)
 9006             FORMAT(1X,2G12.4)
  200          CONTINUE
               WRITE( LUNIT, 9007)
 9007          FORMAT(1X,'HIST')
               WRITE( LUNIT, 9008 ) X(NF),Y(NF),NF
 9008          FORMAT(1X,'TITLE ',2G12.4,''' Dim # ', I3)
  500       CONTINUE
         ENDIF
 1000 CONTINUE

      NO     = 0
      DO 2000 NPAGE = 1, MPAGE

         NFIGS = 8
         IF( NPAGE .EQ. MPAGE ) NFIGS = MOD(NDIM, 8)
         IF( NFIGS .GT. 0 ) THEN

            WRITE( LUNIT, 9000)
            WRITE( LUNIT, 9001)
            WRITE( LUNIT, 9002)

            DO 1500 NF = 1, NFIGS
               NO    = NO + 1
               WRITE( LUNIT, 9003) WINDOW(NF)
               WRITE( LUNIT, 9004) XND
               WRITE( LUNIT, 9005)
               I = 1
               WRITE( LUNIT, 9006) FLOAT(I),XI(1,NO)
               DO 1200 I = 2, ND
                  WRITE( LUNIT, 9006) FLOAT(I),XI(I,NO)-XI(I-1,NO)
 1200          CONTINUE
               WRITE( LUNIT, 9007)
               WRITE( LUNIT, 9108 ) X(NF),Y(NF),NF
 9108          FORMAT(1X,'TITLE ',2G12.4,''' dXI For Dim # ', I3)
 1500       CONTINUE
         ENDIF
 2000 CONTINUE

      NO     = 0
      DO 3000 NPAGE = 1, MPAGE

         NFIGS = 8
         IF( NPAGE .EQ. MPAGE ) NFIGS = MOD(NDIM, 8)
         IF( NFIGS .GT. 0 ) THEN

            WRITE( LUNIT, 9000)
            WRITE( LUNIT, 9001)
            WRITE( LUNIT, 9002)

            DO 2500 NF = 1, NFIGS
               NO    = NO + 1
               WRITE( LUNIT, 9003) WINDOW(NF)
               XJ = 1.0
               WRITE( LUNIT, 9004) XJ
               WRITE( LUNIT, 9005)
               DO 2200 I = 1, ND
                  WRITE( LUNIT, 9006) XI(I,NO),D(I,NO)
 2200          CONTINUE
               WRITE( LUNIT, 9007)
               WRITE( LUNIT, 9208 ) X(NF),Y(NF),NF
 9208          FORMAT(1X,'TITLE ',2G12.4,''' XI-D For Dim # ', I3)
 2500       CONTINUE
         ENDIF
 3000 CONTINUE

      RETURN
      END
