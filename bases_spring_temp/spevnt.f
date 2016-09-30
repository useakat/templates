      subroutine spevnt(iret)
      implicit none

      include 'nexternal.inc'
      
      integer iret
      integer ievent/ 0/
      save ievent

      integer i
      real*8 total_energy,mass(10),P(0:3,10),wgt
      common /event/ mass,total_energy,P,wgt

      iret = 0
      ievent = ievent + 1

      write(33,*) "<event>" 
      do i=1,nexternal
         write(33,*) i,P(0,i),P(1,i),P(2,i),P(3,i),mass(i) 
      enddo
      write(33,*) "</event>"      

      return
      end
