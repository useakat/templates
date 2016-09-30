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
      double precision bfunc,dif

	real*8 vmass,rmass,pi,ans,fmass,smass
	common /mass/ vmass,smass,fmass,rmass
 
      external bfunc

	pi=dacos(-1d0)

      call bsinit     
      call userin     
      call bases(bfunc, estim,sigma,ctime,it1,it2)

	ans=1d0/pi*fmass**5/rmass**2*(1-rmass**2/fmass**2)**3
     & *(1d0/3d0+rmass**2/fmass**2)

	dif=ans-estim

      write(*,*) estim,sigma,dif
   
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

	real*8 p(0:3),vmass,fmass,rmass,smass,r(0:3),q(0:3)
	real*8 MM,a,b,beta,pi
	integer nsv,rhel,fhel,nsr,nsf,vhel
	double complex sc(3),ro(18),fi(6),vertex,gr(2),vc(6)
	double complex vertex2,ri(18),fo(6)
	common /mass/ vmass,smass,fmass,rmass

      x = z(1)
      y = z(2)

	pi=dacos(-1d0)

	vmass=0d0
	fmass=200
	rmass=100d0

	nsv=1
      nsr=1
	nsf=1

	gr(1)=1
	gr(2)=gr(1)
	

	p(0)=fmass
	p(1)=0
	p(2)=0
	p(3)=0

	call mom2cx(fmass,vmass,rmass,x,y,q,r)

      MM=0d0

      do NHEL(2)=-1,-1
       do NHEL(3)=-1,1,2
        do NHEL(4)=-3,3,2
      CALL SXXXXX(P(0,1   ),-1*IC(1   ),W(1,1   ))                         
      CALL OXXXXX(P(0,2   ),LMASS ,NHEL(2   ),+1*IC(2   ),W(1,2   ))       
      CALL VXXXXX(P(0,3   ),ZERO ,NHEL(3   ),+1*IC(3   ),W(1,3   ))        
      CALL IRXXXX(P(0,4   ),MR ,NHEL(4   ),-1*IC(4   ),W(1,4   ))          
      CALL FVOXXX(W(1,2   ),W(1,3   ),GAL ,LMASS   ,ZERO    ,W(1,5   ))    
      CALL IROSXX(W(1,4   ),W(1,5   ),W(1,1   ),GFRS ,AMP(1   ))           
      CALL HVSXXX(W(1,3   ),W(1,1   ),-GAELEL ,ML1     ,WL1     ,W(1,      
     &     6   ))                                                          
      CALL IROSXX(W(1,4   ),W(1,2   ),W(1,6   ),GFRS ,AMP(2   ))           
      CALL FSOXXX(W(1,2   ),W(1,1   ),GL1N1P ,MN1     ,WN1     ,W(1,       
     &     7   ))                                                          
      CALL IROVXX(W(1,4   ),W(1,7   ),W(1,3   ),GN1RA ,AMP(3   ))          
      CALL FSOXXX(W(1,2   ),W(1,1   ),GL1N2P ,MN2     ,WN2     ,W(1,       
     &     8   ))                                                          
      CALL IROVXX(W(1,4   ),W(1,8   ),W(1,3   ),GFRV ,AMP(4   ))           
      CALL FSOXXX(W(1,2   ),W(1,1   ),GL1N3P ,MN3     ,WN3     ,W(1,       
     &     9   ))                                                          
      CALL IROVXX(W(1,4   ),W(1,9   ),W(1,3   ),GFRV ,AMP(5   ))           
      CALL FSOXXX(W(1,2   ),W(1,1   ),GL1N4P ,MN4     ,WN4     ,W(1,       
     &     10  ))                                                          
      CALL IROVXX(W(1,4   ),W(1,10  ),W(1,3   ),GFRV ,AMP(6   ))           
      CALL IROVSX(W(1,4   ),W(1,2   ),W(1,3   ),W(1,1   ),GFRVS ,          
     &     AMP(7   ))                                                      
      JAMP(   1) = JAMP(   1)
     &             -AMP(   1)-AMP(   2)-AMP(   3)-AMP(   4)-AMP(   5)
     &             -AMP(   6)-AMP(   7)

c	  MM=MM+vertex*dconjg(vertex)
	  MM=MM+jamp(1)*dconjg(jamp(1))
	  
       enddo
      enddo
	enddo

	MM = 1d0/2d0*MM

	a=(rmass/fmass)**2
	b=(vmass/fmass)**2
      beta=sqrt(1d0+a**2+b**2-2*(a+b+a*b))

	bfunc=1d0/(2*fmass)*MM*beta/(8*pi)/4d0*pi

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
