C   05/11/88 208061312  MEMBER NAME  BFUNC    (S)        M  FORTRAN
      double precision function func(z)
      implicit none

      double precision z(*)

      include 'hmparm.inc'
      include 'smptab.inc'
      include 'gnbeam.inc'
      include 'nexternal.inc'
      include 'cinit.inc'
      include 'cfinal.inc'
      include 'ccuts.inc'

      double complex wf(6,10000)

      double precision xpq(-7:7)
      integer lp
      
      double precision p1,p2

      double precision amp2

      double precision phffin

      double precision fluxf,phfact

      double precision pt,eta,dr
      double precision ene,cos,phi
      
      double precision pti,ptj,ptij

      double precision ptmin
      
c-- functions
      double precision psdrap, deltar, ptvec

      double precision phfpdf
      double precision tau,taul
      double precision y, ymin, ymax

      double precision zero
      parameter (zero = 0.d0)

      integer id,ierr,pdflag,parton1,parton2
      integer i,j
C******************************************************C
C     pdflag = 0 : No PDF
C             1 : with PDF
C     parton  = 0 : gluon
C               1 : d quark  -1 : d~
C               2 : u quark  -2 : u~
C               3 : s quark  -3 : s~
C               4 : c quark  -4 : c~
C               5 : b quark  -5 : b~
C******************************************************C
      pdflag = 1
      parton1 = 0
      parton2 = 0
      
      func = 0.d0

      if (pdflag.eq.0) then
      elseif ((parton1.eq.0).or.(parton2.eq.0)) then      ! gx -> case
         taul = taulmin * (1.d0 - z(1))
         tau = exp(taul)

         phfpdf = - tau * taulmin

         ymax = min( -0.5d0*taul,  etacut)
         ymin = max(  0.5d0*taul, -etacut)

         y = ymin + (ymax-ymin) * z(2)
         phfpdf = phfpdf * (ymax-ymin)

         xbk(1) = sqrt(tau) * exp( y)
         xbk(2) = sqrt(tau) * exp(-y)

         call gnstpdf

         call pcopy(pini1,p(0,1))
         call pcopy(pini2,p(0,2))

         call ph2bdt(p(0,1), p(0,2),
     &        0.d0, 0.d0, ptcut, etacut,
     &        z(3), z(4), 
     &        p(0,3), p(0,4),
     &        phffin, ierr)
         if (ierr.ne.0) return

      elseif(parton1.ne.parton2) then ! qq' -> case
          call pshk0(s,nfin,z,ptcut,etacut,xbk(1),xbk(2),phffin,ierr)
         if (ierr.ne.0) return
      elseif (parton1.eq.parton2) then !qq -> case
         call pshkn0(s,nfin,z,ptcut,etacut,xbk(1),xbk(2),phffin,ierr)
         if (ierr.ne.0) return
      endif

      fluxf = 1.d0/(2.d0*s)
      
      ptmin = 0.d0
      do i=1,nfin
         pt = ptvec(pfin(1,i))
         if (pt.lt.ptcut) return
         eta = psdrap(pfin(1,i))
         if (abs(eta).gt.etacut) return
         if (ptmin.eq.0.d0 .or. pt.lt.ptmin) ptmin = pt
      enddo

      do i=1,nfin-1
         pti = ptvec(pfin(1,i))
         do j=i+1,nfin
            ptj = ptvec(pfin(1,j))
            dr = deltar(pfin(1,i),pfin(1,j))
            ptij = min(pti,ptj)*dr
            if (ptij.lt.ptcut) return
         enddo
      enddo

      q2fact(1) = ptcut**2
      q2fact(2) = q2fact(1)

      call pftopdg(abs(lpp(1)),xbk(1),dsqrt(q2fact(1)),xpq)
      lp = sign(1,lpp(1))
      p1 = xpq(parton1*lp)
      call pftopdg(abs(lpp(2)),xbk(2),dsqrt(q2fact(2)),xpq)
      lp = sign(1,lpp(2))
      p2 = xpq(parton2*lp)

      if (pdflag.eq.0) then
         phfpdf = 1d0
      elseif ((parton1.eq.0).or.(parton2.eq.0)) then
         phfpdf = phfpdf * p1 * p2
      else
         phfpdf = p1 * p2
      endif

      call smatrix(p, amp2)

      func=phffin*phfpdf*fluxf*amp2*gev2fb
      
      do i=1,nfin
         call fvcomp(p(0,nini+i), ene,cos,eta,phi,pt)
         id = 10*i
         call xhfill(id+1,eta, func)
         call xhfill(id+2,phi, func)
         call xhfill(id+3,pt , func)
         call xhfill(id+4,pt , func)
         call xhfill(id+5,cos, func)
      enddo
      
      call xhfill(111,dfloat(nhel(1)+1+(nhel(2)+1)/2),func)

      do i=1,ndim
         call xhfill(i+1000,z(i),func)
      enddo

      return
      end
