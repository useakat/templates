C   18/10/90 109050545  MEMBER NAME  USERIN   (S)        M  FORTRAN
      subroutine usrout
      implicit none

      include 'hmparm.inc'
      include 'hmunit.inc'
      include 'bsffcm.inc'
      include 'smptab.inc'
      include 'gnbeam.inc'

      include 'cparam.inc'
      include 'cinit.inc'
      include 'cfinal.inc'
      include 'ccuts.inc'
      include 'creslt.inc'


      write(lustdo,9000)
 9000 format(//
     &     5x,'==============================',/
     &     5x,'      Cross Section [fb]      ',/
     &     5x,'          g g --> 2g        ',/
     &     5x,'==============================')
      write(lustdo,9050)
 9050 format(1x,
     &     '+-----------+---------------+---------------+',
     &     '---------------+-----+-----+',
     &     /1x,
     &     '!   Ebeam   ! Cross Section !     Error     !',
     &     '     Stime     ! IT1 ! IT2 !')
      write(lustdo,9100)
 9100 format(1x,
     1      '+-----------+---------------+---------------+',
     2      '---------------+-----+-----+')

      write(lustdo,9200) eb0,estim,sigma,ctime,it1,it2
 9200 format(1x,'!',f10.1,' !',2(g14.6,' !'),g14.3,' !',i4,' !',
     &           i4,' !')
      write(lustdo,9100)

      write(lustdo,'()')
      write(lustdo,'(5x,a)') '================================='
      write(lustdo,'(5x,a)') '  g g --> 2g (Input Parameters)'
      write(lustdo,'(5x,a)') '================================='
      write(lustdo,'()')
      write(lustdo,'(9x,a,i5,a,i5)') 'Nini,  Nfin  = ',nini,', ',nfin
      write(lustdo,'(9x,a,i5,a,i5)') 'Ndiag, NHtot = ',ngraphs,
     &     ', ',nhtot
      write(lustdo,'(9x,a,i5,a,i5)') 'Ndim,  Nwild = ',ndim,', ',nwild

      write(lustdo,'()')
      write(lustdo,'(5x,a)') '================================='
      write(lustdo,'(5x,a)') '  g g --> 2g (Bases Parameters)'
      write(lustdo,'(5x,a)') '================================='
      write(lustdo,'()')
      write(lustdo,'(9x,a,i10)')         'Ncall     = ',ncall
      write(lustdo,'(9x,a,i8,a,i8)')     'ITMX1,2   = ',itmx1,', ',itmx2
      write(lustdo,'(9x,a,f8.4,a,f8.4)') 'ACC1,ACC2 = ',acc1,', ',acc2
      write(lustdo,'()')
      write(lustdo,'(5x,a)') '================================'
      write(lustdo,'(5x,a)') '  g g --> 2g (Kinematic Cuts)'
      write(lustdo,'(5x,a)') '================================'
      write(lustdo,'()')
      write(lustdo,'(9x,a,f8.1)') 'Pt      = ',ptcut
      write(lustdo,'(9x,a,f8.1)') 'Eta     = ',etacut
      write(lustdo,'(9x,a,f8.1)') 'Pt_jj   = ',ptcut
c      write(lustdo,'(9x,a,f8.1)') 'Delta_R = ',drcut
      write(lustdo,'()')
      write(lustdo,'(5x,a)') '=============================='
      write(lustdo,'(5x,a)') '  g g --> 2g (PS variables)'
      write(lustdo,'(5x,a)') '=============================='
      write(lustdo,'()')
      write(lustdo,'(9x,a,f8.1)') 'What_min     = ',whatmin
      write(lustdo,'(9x,a,f8.1)') 'log(tau)_min = ',taulmin
      write(lustdo,'()')
      
      
      write(lustdo,600) w0,estim,sigma,100.d0*sigma/estim,ctime,it1,it2
 600  format(/
     &     5x,' ================================',/,
     &     5x,'    g g --> 2g (Cross section)',/,
     &     5x,' ================================',//,
     &     5x,'   Ecm [GeV]          = ',f10.1,/,
     &     5x,'   Cross section [fb] = ',g13.6,' +- ',g13.6,
     &                                  ' (',f8.4,'%)'/,
     &     5x,'   CPU time [sec]     = ',g14.3,/,
     &     5x,'   # of iteration.    = ',2i5,/)

c      write(lustdo,610) w,estim,sigma
c  610 format(1h ,'ANSWER ',f9.3,2x,g15.6,2x,g15.6)

      return
      end
