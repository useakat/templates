***********************************************************************
*============================                                         *
      SUBROUTINE SPINFO( LU )
*============================                                         *
*((Purpose))                                                          *
*    Print the information for                                        *
*        (1) BASES parameters                                         *
*        (2) Computer time information                                *
*        (3) Convergency behavior of the Grid optimization step       *
*        (4) Convergency behavior of the integration step             *
*(( Input ))                                                          *
*    LU  :  Logical unit number of printer                            *
*                                                                     *
*           by S.Kawabata    March 1994 at KEK
*                                                                     *
***********************************************************************

      IMPLICIT NONE
C     IMPLICIT REAL*8 (A-H,O-Z)

      INTEGER LU

      INCLUDE 'bdate.h'
      INCLUDE 'bscntl.h'

      INCLUDE 'sprng2.h'

      INCLUDE 'btime.h'

      REAL XTIME

      CHARACTER*1 CN

      DOUBLE PRECISION EFF

      INTEGER I,J

       IF( IPNT .EQ. 0 ) THEN
           WRITE(LU,9300)
       ELSE
           CN     = CHAR(12)
           WRITE(LU,9350) CN
       ENDIF
 9300  FORMAT(/1H1,////1H )
 9350  FORMAT(A1,////1X)
       WRITE(LU,9360) (IDATE0(I),I=1,3),(ITIME0(J),J=1,2)
 9360  FORMAT(55X,'Date: ',I2,'/',I2,'/',I2,2X,I2.2,':',I2.2)
       WRITE(LU,9400)
 9400 FORMAT(
     . 8X,'**********************************************************',
     ./8X,'*                                                        *',
     ./8X,'*    SSSSS   PPPPPP   RRRRRR   IIIII  N    NN   GGGGG    *',
     ./8X,'*   SS   SS  PP   PP  RR   RR   III   NN   NN  GG   GG   *',
     ./8X,'*   SS       PP   PP  RR   RR   III   NNN  NN  GG        *',
     ./8X,'*    SSSSS   PPPPPP   RRRRR     III   NNNN NN  GG  GGGG  *',
     ./8X,'*        SS  PP       RR  RR    III   NN NNNN  GG   GG   *',
     ./8X,'*   SS   SS  PP       RR   RR   III   NN  NNN  GG   GG   *',
     ./8X,'*    SSSSS   PP       RR    RR IIIII  NN   NN   GGGGG    *',
     ./8X,'*                                                        *',
     ./8X,'*                  SPRING Version 5.1                    *',
     ./8X,'*           coded by S.Kawabata KEK, March 1994          *',
     ./8X,'**********************************************************')
*                                                                      *
          EFF   = FLOAT(NEVENT)/FLOAT(NTRIAL)*100.D0
          CALL BSTIME( RTIME, 1 )
          XTIME = RTIME - TIMES1
          WRITE(LU,9500) NEVENT,EFF,(TIMESP(I),I=0,2),XTIME,MXTRY,MISS
 9500     FORMAT(/5X,'Number of generated events    =',I10,
     .         /5X,'Generation efficiency         =',F10.3,' Percent',
     .         /5X,'Computing time for generation =',F10.3,' Seconds',
     .         /5X,'               for Overhead   =',F10.3,' Seconds',
     .         /5X,'               for Others     =',F10.3,' Seconds',
     .         /5X,'GO time for event generation  =',F10.3,' Seconds',
     .         /5X,'Max. number of trials MXTRY   =',I10,' per event',
     .         /5X,'Number of miss-generation     =',I10,' times')

      CALL SPHIST( LU )

      RETURN
      END
