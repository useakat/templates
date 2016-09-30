C***********************************************************************
C*=======================                                              *
      SUBROUTINE BSDATE
C*=======================                                              *
C*((Purpose))                                                          *
C*    Changethe format of the time stamp.                              *
C*    This program should be modified according to the machine.        *
C*((Author))                                                           *
C*    S.Kawabata  Nov. '91 at KEK                                      *
C***********************************************************************

      IMPLICIT NONE
      
      INCLUDE 'bdate.h'
      
      INTEGER ISL
      COMMON /SLATE/ISL(40)
      
      INTEGER ID,IT
      
      INTEGER TARRAY(3)
 
      CALL IDATE(TARRAY)
      
      IF (TARRAY(3).LT.2000) THEN
         IDATE0(1) = TARRAY(3)-1900
      ELSE
         IDATE0(1) = TARRAY(3)-2000
      ENDIF
      IDATE0(2) = TARRAY(2)
      IDATE0(3) = TARRAY(1)

      CALL ITIME(TARRAY)
      ITIME0(1) = TARRAY(1)
      ITIME0(2) = TARRAY(2)
      
      RETURN
      END
