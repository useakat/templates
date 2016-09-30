      SUBROUTINE BSUNIX( ID )

      IMPLICIT NONE
C     IMPLICIT REAL*8 ( A-H, O-Z )

      INTEGER ID

      INCLUDE 'base0.h'
      INCLUDE 'base1.h'
      INCLUDE 'base2.h'
      INCLUDE 'ninfo.h'

      INCLUDE 'bscntl.h'

C**** The following 2 variables should be declared in a common block
C**** J.Kanzaki 96-08-28
      INTEGER NPRINT
      REAL SETIM


      IF( ID .EQ. 1 ) THEN
C**** for single CPU machine.
         NODEID   = 0
         NUMNOD   = 1
         READ( 5, * ) NLOOP, MLOOP
         READ( 5, * ) NPRINT
         READ( 5, * ) JFLAG
         READ( 5, * ) SETIM
      ELSE
      ENDIF

      RETURN
      END
