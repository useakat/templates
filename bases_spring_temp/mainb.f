      program main
      implicit none

      integer i,seed,it1,it2
      real*8 estim,sigma,ctime
      common /bsresults/ estim,sigma,ctime,it1,it2

      real*8 bfunc
      external bfunc

***********************
* initialization
***********************
      seed = 12345
      call bsinit(seed)
      call userin

*************************
* integration
*************************
      call bases(bfunc, estim,sigma,ctime,it1,it2)
      call bsinfo(6)
      call bhplot(6)
***************
* save grids
***************
      open(23, file="rslt.bases",status='unknown',form='unformatted')
      call bswrit(23)
      close(23)

**********************************
* write histograms
**********************************

      open(1,file="x.top",status="replace")
      open(2,file="y.top",status="replace")
      call xhsave(1,1)
      call xhsave(2,2)
      close(1)
      close(2)

**********************************
* write bases information
**********************************
      call usrout

      end
