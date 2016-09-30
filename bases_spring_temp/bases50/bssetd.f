************************************************************************
*    ============================================
      SUBROUTINE BSSETD(NDIM0,NWILD0,XL0,XU0,IG0)
*    ============================================
* ((Purpose))                                                          *
*                                                                      *
*        Coded by J.Kanzaki           July '94                         *
*                                                                      *
************************************************************************

      IMPLICIT NONE

      INTEGER NDIM0,NWILD0,IG0(NDIM0)
      DOUBLE PRECISION XL0(NDIM0),XU0(NDIM0)

      INCLUDE 'base1.h'
      INCLUDE 'base2.h'
      INCLUDE 'bparm.h'

      INTEGER I


      IF(NDIM0.GT.0) THEN
         NDIM  = NDIM0
         NDIMT = NDIM0
      END IF
      IF(NWILD0.GT.0) THEN
         NWILD  = NWILD0
         NWILDT = NWILD0
      END IF

      DO 100 I=1,NDIM0
      XL(I)  = XL0(I)
      XU(I)  = XU0(I)
      IG(I)  = IG0(I)
      XLT(I) = XL0(I)
      XUT(I) = XU0(I)
      IGT(I) = IG0(I)
  100 CONTINUE

      RETURN
      END
