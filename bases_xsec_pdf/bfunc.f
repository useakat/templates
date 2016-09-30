C   05/11/88 208061312  MEMBER NAME  BFUNC    (S)        M  FORTRAN
      double precision function func(z)
      implicit none

      double precision z(*)

      include 'hmparm.inc'
      include 'smptab.inc'
      include 'gnbeam.inc'
      include 'cparam.inc'
      include 'cinit.inc'
      include 'cfinal.inc'
      include 'ccuts.inc'

      double complex wf(6,nwavefuncs)

      double precision xpq(-7:7)
      integer lp
      
      double precision g1,g2

      double precision amp2

      double precision phffin
      integer nh
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

      integer id,ierr
      integer i,j

      func = 0.d0

      taul = taulmin * (1.d0 - z(1))
      tau = exp(taul)
      
      phfpdf = - tau * taulmin

      ymax = min( -0.5d0*taul,  etacut)
      ymin = max(  0.5d0*taul, -etacut)

      y = ymin + (ymax-ymin) * z(2)
      phfpdf = phfpdf * (ymax-ymin)

      xbk(1) = sqrt(tau) * exp( y)
      xbk(2) = sqrt(tau) * exp(-y)

c      print *,'bfunc; xbk = ',xbk(1),', ',xbk(2)

c      print *,'bfunc: call gnstpdf'
      call gnstpdf

      call pcopy(pini1,p(0,1))
      call pcopy(pini2,p(0,2))

      fluxf = 1.d0/(2.d0*s)

      call ph2bdt(p(0,1), p(0,2),
     &     0.d0, 0.d0, ptcut, etacut,
     &     z(3), z(4), 
     &     p(0,3), p(0,4),
     &     phffin, ierr)
      if (ierr.ne.0) return

c      do i=1,nfin
c         print *,'pfin(',i,')= ',(pfin(j,i),j=0,3)
c      enddo
      
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
c      q2fact(1) = ptmin**2

      q2fact(2) = q2fact(1)

c      print *,'bfunc: call pftopdf-1: ',lpp(1), xbk(1), q2fact(1)
      call pftopdg(abs(lpp(1)),xbk(1),dsqrt(q2fact(1)),xpq)
c      print *,'bfunc: lp-1 = ',lp
      g1 = xpq(0)
c      print *,'bfunc: uq = ',uq
c      print *,'bfunc: call pftopdf-2: ',lpp(2), xbk(2), q2fact(2)
      call pftopdg(abs(lpp(2)),xbk(2),dsqrt(q2fact(2)),xpq)
c      print *,'bfunc: lp-2 = ',lp
      g2 = xpq(0)
c      print *,'bfunc: ux = ',ux
      phfpdf = phfpdf * g1 * g2
      
      nh = int(dble(nhtot)*z(ndim))
      nh = mod(nh,nhtot)

      do i=1,nexternal
         nhel(i) = 2*mod(nh/2**(i-1),2)-1
      enddo

      call vxxxxx(p(0,1   ),zero ,nhel(1   ),-1,wf(1,1   ))        
      call vxxxxx(p(0,2   ),zero ,nhel(2   ),-1,wf(1,2   ))        

      do i=1,nfin
         call vxxxxx(p(0,i+2   ),zero ,nhel(i+2   ),+1,wf(1,i+2   ))
      enddo
      
      call ggng(wf, amp2)

c      print *,'bfunc;ampsq   = ',amp2
c      print *,'     ;nhtot,fluxf   = ',nhtot,fluxf
c      print *,'     ;phfini,phffin = ',phfini,phffin
      func=phfini*phffin*phfpdf*fluxf*amp2
c      func=phfini*fluxf*amp2*dble(nhtot)
      
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

c      do i=1,ndiag
c         amptmp = dreal(amplst(i)*dconjg(amplst(i)))
cc      print *,'bfunc;amptmp = ',amptmp
c         call xhfill(110,dfloat(i-1)+0.5d0,amptmp)
c      enddo

      do i=1,ndim
c         print *,'i,z(i) = ',i,z(i)
         call xhfill(i+1000,z(i),func)
      enddo

      return
      end
