      subroutine userin
      implicit none

      include 'base1.h'
      include 'base2.h'

      integer i

      external bfunc

      ncall = 10000
      itmx1 = 5
      itmx2 = 5
      acc1 = 0.1d0
      acc2 = 0.1d0

      ndim = 2
      nwild = ndim
      do i=1,ndim
         xl(i) = 0d0
         xu(i) = 1d0
         ig(i) = 1
      enddo

      call bssetp(ncall,itmx1,itmx2,acc1,acc2)
      call bssetd(ndim,nwild,xl,xu,ig)
      
      call xhinit(1,0d0,1d0,50,'dbfunc/dx')
      call xhinit(2,0d0,1d0,50,'dbfunc/dy')
      call dhinit(3,0d0,1d0,30,0d0,1d0,30,'dbfunc/dxdy')

      return
      end
