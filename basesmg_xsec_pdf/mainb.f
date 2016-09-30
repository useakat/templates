C   18/10/90 109050545  MEMBER NAME  USERIN   (S)        M  FORTRAN
      program main
      implicit none

      double precision func
      external func

      include 'hmunit.inc'
      include 'bsfile.inc'
      include 'creslt.inc'

      call bsinit

      call hmsetf('bases')

      call userin

      call bases(func, estim,sigma,ctime,it1,it2)

      call bsinfo(lustdo)

      call bhplot(lustdo)

      call bswrit(lubsf)

      call xhsave(luhist,0)
      
      call usrout

      stop
      end
