      double precision function bfunc(z)
      implicit none

      include 'nexternal.inc'

      real*8 x,y,z(2),i
      real*8 total_energy,mass(10),P(0:3,10),wgt
      common /event/ mass,total_energy,P,wgt
      real*8 prambo(4,10)

      x = z(1)
      y = z(2)

      total_energy = 1000d0
      do i = 1,nexternal
         mass(i) = 0d0
      enddo

      P(0,1) = total_energy/2d0
      P(1,1) = 0d0
      P(2,1) = 0d0
      P(3,1) = total_energy/2d0
      P(0,2) = total_energy/2d0
      P(1,2) = 0d0
      P(2,2) = 0d0
      P(3,2) = total_energy/2d0
      call rambo(nexternal-2,total_energy,mass(3),prambo,wgt)

      DO I=3,nexternal
         P(0,I)=PRAMBO(4,I-2)
         P(1,I)=PRAMBO(1,I-2)
         P(2,I)=PRAMBO(2,I-2)
         P(3,I)=PRAMBO(3,I-2)
      ENDDO

      bfunc = (x-0.5d0)**2 +1d0 +(y-0.2)**2 +0.4d0

      call xhfill(1,x,bfunc)
      call xhfill(2,y,bfunc)
      call dhfill(3,x,y,bfunc)

      return
      end
