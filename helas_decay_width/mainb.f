      program mainb

      implicit none

      double precision estim, sigma, ctime
      integer it1, it2
      common /paramd/ estim,sigma,ctime,it1,it2
      integer ndim, nwild
      common /params/ ndim,nwild
      save /paramd/,/params/

      integer lustdo
      parameter (lustdo=6)
      double precision bfunc

	real*8 smass,rmass,pi,ans,estim2,ans2,fmass
 
      external bfunc

	pi=dacos(-1d0)

	smass=200d0
	rmass=100d0
	fmass=10d0

      call bsinit     
      call userin     
      call bases(bfunc, estim,sigma,ctime,it1,it2)

	ans=1/(24*pi)*0.01**2*smass**5/rmass**2*(1-rmass**2/smass**2)**4
	ans2=(smass**2-rmass**2-fmass**2)**4/(24*pi*rmass**2*smass**3)
     & *0.01**2
     &     *(1-(4*rmass**2*fmass**2)/(smass**2-rmass**2-fmass**2)**2)
     &     **1.5d0
	estim2=estim

      write(*,*) ans2,estim, sigma
   
      stop
      end

ccccccccccccc bfunc cccccccccccccccccccccccccccccccc
	double precision function bfunc(z)

      implicit none

      double precision estim, sigma, ctime
      integer it1, it2
      common /paramd/ estim,sigma,ctime,it1,it2
      integer ndim, nwild
      common /params/ ndim,nwild
      save /paramd/,/params/

      double precision z(2)
      double precision x,y
      REAL*8 ZERO
      PARAMETER (ZERO=0D0)

	real*8 p(0:3),smass,fmass,rmass,r(0:3),q(0:3)
	real*8 MM,a,b,beta,pi
	integer nss,rhel,fhel,nsr,nsf
	double complex sc(3),ro(18),fi(6),vertex,gr(2)
	double complex vertex2,ri(18),fo(6)

      x = z(1)
      y=z(2)

	pi=dacos(-1d0)

	smass=200d0
	fmass=10
	rmass=100d0

	nss=-1
      nsr=1
	nsf=-1

	gr(1)=1d-2
	gr(2)=0
	

	p(0)=smass
	p(1)=0
	p(2)=0
	p(3)=0

	call mom2cx(smass,fmass,rmass,x,y,q,r)

      MM=0d0

      do rhel=-3,3,2
       do fhel=-1,1,2
        call sxxxxx(p,nss,sc)
	  call orxxxx(r,rmass,rhel,nsr,ro)
        call ixxxxx(q,fmass,fhel,nsf,fi)
	  call irxxxx(r,rmass,rhel,-1,ri)
	  call oxxxxx(q,fmass,fhel,1,fo)
	  call iorsxx(fi,ro,sc,gr,vertex)
	  call irosxx(ri,fo,sc,gr,vertex2)

c	  MM=MM+vertex*dconjg(vertex)
	  MM=MM+vertex2*dconjg(vertex2)
	  
	!print*,rhel,fhel,vertex

       enddo
      enddo
	
	!stop	

	a=(rmass/smass)**2
	b=(fmass/smass)**2
      beta=sqrt(1d0+a**2+b**2-2*(a+b+a*b))

c	write(*,*) beta
c      bfunc=1d0
	bfunc=1d0/(2*smass)*MM*beta/(8*pi)/2d0/(2*pi)


      return
      end

cccccccccccccc userin cccccccccccccccccccccccccccccc
      subroutine userin

      implicit none

      double precision estim, sigma, ctime
      integer it1, it2
      common /paramd/ estim,sigma,ctime,it1,it2
      integer ndim, nwild
      common /params/ ndim,nwild
      save /paramd/,/params/

      integer maxdim   
      parameter (maxdim=50)
      double precision xl(maxdim),xu(maxdim)
 
      integer ig(maxdim)

      integer ncall, itmx1, itmx2
      double precision acc1, acc2

	real*8 pi

      integer idim

	pi = dacos(-1d0)

      ndim = 2
      nwild = 0
                
      xl(1) = -1
      xu(1) = 1
      ig(1) = 1

	xl(2) = 0
	xu(2) = 2*pi
	ig(2) = 1

      call bssetd(ndim,nwild,xl,xu,ig)

      ncall = 10000
      itmx1 = 7
      itmx2 = 7
      acc1 = 0.001d0
      acc2 = 0.001d0      
      call bssetp(ncall, itmx1, itmx2, acc1, acc2)

*      call xhinit(1,-1.d0,1.d0,50,'d sigma / d x1')
*      call xhinit(2,-1.d0,1.d0,50,'d sigma / d x2')

*      call dhinit(100,-1.d0,1.d0,50,-1.d0,1.d0,50, 
*     &     'd sigma/ d x1/d x2')

      return
      end
