      program main
      implicit none

      integer i,seed
      double precision bfunc
      external bfunc
      integer iret,nevents,ievt
      integer mxtry
      parameter (mxtry=10000)

***********************
* initialization
***********************
      nevents = 100
      seed = 12345
      call bsinit(seed)
      call userin
      open(23, file="rslt.bases",status='old',form='unformatted')
      call bsread(23)
      close(23)
*******************************
* read process information
*******************************
      open(33,file="events.dat",status="unknown")
      do ievt = 1, nevents
         call spring(bfunc,mxtry)
         call spevnt(iret)
      enddo
      close(33)
      call spinfo(6)
      call shplot(6)

      end
