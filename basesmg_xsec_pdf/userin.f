c   18/10/90 109050545  member name  userin   (s)        m  fortran
      subroutine userin
      implicit none
      external func

      integer maxdim
      parameter (maxdim=50)
      double precision xl(maxdim),xu(maxdim)
      integer ig(maxdim)

      include 'bsffcm.inc'
      include 'hmparm.inc'
      include 'hmunit.inc'
      include 'smptab.inc'
      include 'gnbeam.inc'

      include 'nexternal.inc'
      include 'cinit.inc'
      include 'cfinal.inc'
      include 'ccuts.inc'

      include 'coupl.inc'
      
      character*80 ctit
      character*1 char
      
      double precision spnave,colave,stat,color
      integer nfact

      integer i,id,idim
      
c     call coupsm(0)
      
      zmass = 91.188d0
      gfermi = 1.16639d-5
      alpha = 1.d0/1.3250698d+2
c g = sqrt(4pi*alphas)
c alfas = 0.118
c#define hg 1.2177158f
c fixed at mz
c alfas = 0.130000
c#define hg 1.27813465f
c fixed at 20gev
      alfas = 0.171202
c#define hg 1.46676145f
c fixed at 50gev
c      alfas = 0.143696816
c#define hg 1.3437813f
c fixed at 100gev
c      alfas = 0.128125194
c#define hg 1.2688481f
c fixed at 150GeV
c      alfas = 0.120487592
c#define hG 1.23048435f
c fixed at 200GeV
c      alfas = 0.115598437
c#define hG 1.20526047f
      
      call coupsm(1)
      
      call hminit

      call smprnt

      call pdfwrap

      do idim=1,ndim
         xl(idim) = 0.d0
         xu(idim) = 1.d0
         ig(idim) = 1
      enddo
      
      whatmin = nfin * ptcut
      taulmin = 2.d0*log(whatmin/w0)
      
      call bssetd(ndim,nwild,xl,xu,ig)
      
      do i=1,nfin
         write(char,'(i1)') i
         id = 10*i
         call xhinit(id+1,-5.d0,5.d0,50,'d sigma / d eta_a'//char)
         call xhinit(id+2,0.d0,2.d0*pi,50,'d sigma / d phi_a'//char)
         call xhinit(id+3,0.d0,1000.d0,50,'d sigma / d pt_a'//char)
         call xhinit(id+4,0.d0,100.d0,50,'d sigma / d pt_a'//char)
         call xhinit(id+5,-1.d0,1.d0,50,'d sigma / d pt_cos'//char)
      enddo
      
      call xhinit(111,0.d0, 4.d0, 4,'helicity(ee)')

      do idim=1,ndim
         write(ctit,1010) idim,xl(idim),xu(idim)
 1010    format('x(',i2,') ; ',e12.6,' - ',e12.6)
         call xhinit(idim+1000,xl(idim),xu(idim),50,ctit)
      enddo

      return
      end
