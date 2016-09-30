      subroutine usrout
      implicit none

      include 'base1.h'
      include 'base2.h'

      integer lustdo,it1,it2
      real*8 estim,sigma,ctime
      common /bsresults/ estim,sigma,ctime,it1,it2

      lustdo = 6
      write(lustdo,'()')
      write(lustdo,'(5x,a)') '================================='
      write(lustdo,'(5x,a)') '  Bases Parameters'
      write(lustdo,'(5x,a)') '================================='
      write(lustdo,'()')
      write(lustdo,'(9x,a,i10)')         'Ncall     = ',ncall
      write(lustdo,'(9x,a,i8,a,i8)')     'ITMX1,2   = ',itmx1,', ',itmx2
      write(lustdo,'(9x,a,f8.4,a,f8.4)') 'ACC1,ACC2 = ',acc1,', ',acc2
      write(lustdo,'()')
      
      write(lustdo,600) estim,sigma,100.d0*sigma/estim,ctime,it1,it2
 600  format(/
     &     5x,' ================================',/,
     &     5x,'    Results',/,
     &     5x,' ================================',//,
     &     5x,'  Integration: ',g13.6,' +- ',g13.6,
     &                                  ' (',f8.4,'%)'/,
     &     5x,'   CPU time [sec]     = ',g14.3,/,
     &     5x,'   # of iteration.    = ',2i5,/)


      return
      end
