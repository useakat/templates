      PROGRAM DRIVER
C**************************************************************************
C     THIS IS THE DRIVER FOR CHECKING THE STANDALONE MATRIX ELEMENT.
C     IT USES A SIMPLE PHASE SPACE GENERATOR
C     Fabio Maltoni - 3rd Febraury 2007
C**************************************************************************
      IMPLICIT NONE

      REAL*8 ZERO
      PARAMETER (ZERO=0D0) 

      INCLUDE "coupl.inc"
      INCLUDE "nexternal.inc" 
      REAL*8 PMASS(NEXTERNAL)	
      INCLUDE "ngraphs.inc"

      double precision estim, sigma, ctime
      integer it1, it2
      common /paramd/ estim,sigma,ctime,it1,it2
      integer ndim, nwild
      common /params/ ndim,nwild
      save /paramd/,/params/

      integer lustdo,i,n,h
      parameter (lustdo=6)
      real*8 costh,phi,m1,m2,m3,m4,emin,E(2000)
      double precision bfunc,sqrts(2000)
      
      external bfunc

      common /pa/ i,n,sqrts,h
      
C-----
C     BEGIN CODE
C-----
	n=100
	h=0

	call setpara('param_card.dat',.true.)  !first call to setup the paramaters
        include "pmass.inc"             !set up masses
 
 	m1=pmass(1)
        m2=pmass(2)
        m3=pmass(3)
        m4=pmass(4)

	if (h.eq.0) then 
	  
        open(1,file="total_cross_cm.dat",status="unknown")

        do i=1,n
c        sqrts(i)=10**((Log10(1d2)-Log10(1d0))/n*i+Log10(1d0))
         sqrts

        call bsinit     
        call userin     
        call bases(bfunc, estim,sigma,ctime,it1,it2)

        write(1,*) sqrts(i),estim, sigma

        enddo
	close(1)

        elseif (h.eq.1) then
	open(2,file="total_cross_lab.dat",status="unknown")

	emin=((m3+m4)**2-m1**2-m2**2)/(2*m2)
	do i=1,n
        E(i)=10**((Log10(1d16)-Log10(emin))/n*i+Log10(emin))
        sqrts(i)=sqrt(m2**2+2*E(i)*m2+m1**2)
	call bsinit     
        call userin     
        call bases(bfunc, estim,sigma,ctime,it1,it2)
	write(2,*) E(i),estim, sigma
        enddo
	close(2)

	else
	call bsinit     
        call userin     
        call bases(bfunc, estim,sigma,ctime,it1,it2)
	write(*,*) estim, sigma
	 
	endif
   
      stop
      end

C************************************************
	double precision function bfunc(z)
C************************************************
      implicit none

      REAL*8 ZERO,costh,phi,m1,m2,m3,m4,beta(2000),F(2000)
      PARAMETER (ZERO=0D0) 

      INCLUDE "coupl.inc"
      INCLUDE "nexternal.inc" 
      REAL*8 PMASS(NEXTERNAL)	
      INCLUDE "ngraphs.inc"           
	
      double precision estim, sigma, ctime,sqrts(2000)
      integer it1, it2
      common /paramd/ estim,sigma,ctime,it1,it2
      integer ndim, nwild,h
      common /params/ ndim,nwild
      save /paramd/,/params/        
      INTEGER I,J,K,n
      REAL*8 P(0:3,NEXTERNAL)   ! four momenta. Energy is the zeroth component.
      REAL*8 ME,pi          ! sqrt(s) = center of mass energy 
      REAL*8 PIN(0:3), POUT(0:3)
      CHARACTER*120 BUFF(NEXTERNAL)
      double precision z(50)
      common /pa/ i,n,sqrts,h
      REAL*8 DOT
      EXTERNAL DOT

      real*8 smass,fmass,vmass,rmass,MM,a,b,beta1
      double complex vc(6),fi(6),ro(18),vertex
      integer nhel1,nhel2,nhel3,flug

      pi=dacos(-1d0)

      costh = z(1)
      phi = z(2)

      flug=1

c      CALL GET_MOMENTA(SQRTS,PMASS,P)

      if (h.eq.0.or.h.eq.1) then
C**********************************************************************
c	cross section 2 -> 2
C**********************************************************************
      m1=pmass(1)
      m2=pmass(2)
      m3=pmass(3)
      m4=pmass(4)	

      call mom2cx(sqrts(i),m1,m2,1d0,zero,P(0,1),P(0,2))
      call mom2cx(sqrts(i),m3,m4,costh,phi,P(0,3),P(0,4))
      CALL SMATRIX(P,ME)

	beta(i)=sqrt(1d0+(m3**2/sqrts(i)**2)**2+(m4**2/sqrts(i)**2)**2-2
     & *(m3**2/sqrts(i)**2+m4**2/sqrts(i)**2+m3**2/sqrts(i)**2*m4**2/
     &  sqrts(i)**2))

      F(i)=sqrt((sqrts(i)**2-m1**2-m2**2)**2/4d0-m1**2*m2**2)

      bfunc=1d0/(4*F(i))*ME/(8*pi)*beta(i)/(4*pi)*sqrts(i)**2
C***********************************************************************

      elseif (h.eq.2) then
C***********************************************************************
c	decay width 1 -> 2
C***********************************************************************
	fmass=200
	rmass=100
	vmass=0
	
	m1=fmass
	m2=rmass
	m3=vmass
	
	p(0,1)=m1
	p(1,1)=0
	p(2,1)=0
	p(3,1)=0

	call mom2cx(m1,m2,m3,costh,phi,P(0,2),P(0,3))

        MM=0d0

        do nhel2=-3,3,2
         do nhel1=-1,1,2
          do nhel3=-1,1,2
           call vxxxxx(P(0,3),m3,nhel3,1,vc)
	   call orxxxx(P(0,2),m2,nhel2,1,ro)
           call ixxxxx(P(0,1),m1,nhel1,1,fi)
	   call iorvxx(fi,ro,vc,gc,vertex)

	   MM=MM+vertex*dconjg(vertex)
	 
          enddo
         enddo
        enddo

	MM = 1d0/2d0*MM

	a=(m2/m1)**2
	b=(m3/m1)**2
        beta1=dsqrt(1d0+a**2+b**2-2*(a+b+a*b))

	bfunc=1d0/(2*m1)*MM*beta1/(8*pi)/4d0*pi
C*******************************************************
	endif

      end

C*******************************************************
	subroutine userin 
C******************************************************* 	
	implicit none

      double precision estim, sigma, ctime,pi
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

      integer idim

	pi=dacos(-1d0)

      ndim = 2
      nwild = 0
                
      xl(1) = -1d0
      xu(1) = 1d0
      ig(1) = 1

      xl(2) = 0
      xu(2) = 2*pi
      ig(2) = 1      

      call bssetd(ndim,nwild,xl,xu,ig)

      ncall = 800
      itmx1 = 10
      itmx2 = 10
      acc1 = 0.1d0
      acc2 = 0.1d0      
      call bssetp(ncall, itmx1, itmx2, acc1, acc2)

*      call xhinit(1,-1.d0,1.d0,50,'d sigma / d x1')
*      call xhinit(2,-1.d0,1.d0,50,'d sigma / d x2')

*      call dhinit(100,-1.d0,1.d0,50,-1.d0,1.d0,50, 
*     &     'd sigma/ d x1/d x2')

      return
      end


 	double precision function dot(p1,p2)
C****************************************************************************
C     4-Vector Dot product
C****************************************************************************
      implicit none
      double precision p1(0:3),p2(0:3)
      dot=p1(0)*p2(0)-p1(1)*p2(1)-p1(2)*p2(2)-p1(3)*p2(3)
      end


	  SUBROUTINE GET_MOMENTA(ENERGY,PMASS,P)
C---- auxiliary function to change convention between madgraph and rambo
c---- four momenta. 	  
	  IMPLICIT NONE
	  INCLUDE "nexternal.inc"
C	  ARGUMENTS
	  REAL*8 ENERGY,PMASS(NEXTERNAL),P(0:3,NEXTERNAL),PRAMBO(4,10),WGT
C         LOCAL
	  INTEGER I

	  P(0,1)=energy/2
	  P(1,1)=0d0
	  P(2,1)=0d0
	  P(3,1)=energy/2

	  P(0,2)=energy/2
	  P(1,2)=0d0
	  P(2,2)=0d0
	  P(3,2)=-energy/2
	  	
	  call rambo(nexternal-2,energy,pmass(3),prambo,WGT)
	  DO I=3, NEXTERNAL
	  	P(0,I)=PRAMBO(4,I-2)	
	    P(1,I)=PRAMBO(1,I-2)
	  	P(2,I)=PRAMBO(2,I-2)
	    P(3,I)=PRAMBO(3,I-2)	
      ENDDO

	  RETURN
	  END


      SUBROUTINE RAMBO(N,ET,XM,P,WT)
***********************************************************************
*                       RAMBO                                         *
*    RA(NDOM)  M(OMENTA)  B(EAUTIFULLY)  O(RGANIZED)                  *
*                                                                     *
*    A DEMOCRATIC MULTI-PARTICLE PHASE SPACE GENERATOR                *
*    AUTHORS:  S.D. ELLIS,  R. KLEISS,  W.J. STIRLING                 *
*    THIS IS VERSION 1.0 -  WRITTEN BY R. KLEISS                      *
*    -- ADJUSTED BY HANS KUIJF, WEIGHTS ARE LOGARITHMIC (20-08-90)    *
*                                                                     *
*    N  = NUMBER OF PARTICLES                                         *
*    ET = TOTAL CENTRE-OF-MASS ENERGY                                 *
*    XM = PARTICLE MASSES ( DIM=NEXTERNAL-2 )                         *
*    P  = PARTICLE MOMENTA ( DIM=(4,NEXTERNAL-2) )                    *
*    WT = WEIGHT OF THE EVENT                                         *
***********************************************************************
      IMPLICIT REAL*8(A-H,O-Z)
	  INCLUDE "nexternal.inc"
      DIMENSION XM(NEXTERNAL-2),P(4,NEXTERNAL-2)
      DIMENSION Q(4,NEXTERNAL-2),Z(NEXTERNAL-2),R(4),
     .   B(3),P2(NEXTERNAL-2),XM2(NEXTERNAL-2),
     .   E(NEXTERNAL-2),V(NEXTERNAL-2),IWARN(5)
      SAVE ACC,ITMAX,IBEGIN,IWARN
      DATA ACC/1.D-14/,ITMAX/6/,IBEGIN/0/,IWARN/5*0/
*
* INITIALIZATION STEP: FACTORIALS FOR THE PHASE SPACE WEIGHT
      IF(IBEGIN.NE.0) GOTO 103
      IBEGIN=1
      TWOPI=8.*DATAN(1.D0)
      PO2LOG=LOG(TWOPI/4.)
      Z(2)=PO2LOG
      DO 101 K=3,10
  101 Z(K)=Z(K-1)+PO2LOG-2.*LOG(DFLOAT(K-2))
      DO 102 K=3,10
  102 Z(K)=(Z(K)-LOG(DFLOAT(K-1)))
*
* CHECK ON THE NUMBER OF PARTICLES
  103 IF(N.GT.1.AND.N.LT.101) GOTO 104
      PRINT 1001,N
      STOP
*
* CHECK WHETHER TOTAL ENERGY IS SUFFICIENT; COUNT NONZERO MASSES
  104 XMT=0.
      NM=0
      DO 105 I=1,N
      IF(XM(I).NE.0.D0) NM=NM+1
  105 XMT=XMT+ABS(XM(I))
      IF(XMT.LE.ET) GOTO 201
      PRINT 1002,XMT,ET
      STOP
*
* THE PARAMETER VALUES ARE NOW ACCEPTED
*
* GENERATE N MASSLESS MOMENTA IN INFINITE PHASE SPACE
  201 DO 202 I=1,N
         r1=rn(1)
      C=2.*r1-1.
      S=SQRT(1.-C*C)
      F=TWOPI*RN(2)
      r1=rn(3)
      r2=rn(4)
      Q(4,I)=-LOG(r1*r2)
      Q(3,I)=Q(4,I)*C
      Q(2,I)=Q(4,I)*S*COS(F)
  202 Q(1,I)=Q(4,I)*S*SIN(F)
*
* CALCULATE THE PARAMETERS OF THE CONFORMAL TRANSFORMATION
      DO 203 I=1,4
  203 R(I)=0.
      DO 204 I=1,N
      DO 204 K=1,4
  204 R(K)=R(K)+Q(K,I)
      RMAS=SQRT(R(4)**2-R(3)**2-R(2)**2-R(1)**2)
      DO 205 K=1,3
  205 B(K)=-R(K)/RMAS
      G=R(4)/RMAS
      A=1./(1.+G)
      X=ET/RMAS
*
* TRANSFORM THE Q'S CONFORMALLY INTO THE P'S
      DO 207 I=1,N
      BQ=B(1)*Q(1,I)+B(2)*Q(2,I)+B(3)*Q(3,I)
      DO 206 K=1,3
  206 P(K,I)=X*(Q(K,I)+B(K)*(Q(4,I)+A*BQ))
  207 P(4,I)=X*(G*Q(4,I)+BQ)
*
* CALCULATE WEIGHT AND POSSIBLE WARNINGS
      WT=PO2LOG
      IF(N.NE.2) WT=(2.*N-4.)*LOG(ET)+Z(N)
      IF(WT.GE.-180.D0) GOTO 208
      IF(IWARN(1).LE.5) PRINT 1004,WT
      IWARN(1)=IWARN(1)+1
  208 IF(WT.LE. 174.D0) GOTO 209
      IF(IWARN(2).LE.5) PRINT 1005,WT
      IWARN(2)=IWARN(2)+1
*
* RETURN FOR WEIGHTED MASSLESS MOMENTA
  209 IF(NM.NE.0) GOTO 210
* RETURN LOG OF WEIGHT
      WT=WT
      RETURN
*
* MASSIVE PARTICLES: RESCALE THE MOMENTA BY A FACTOR X
  210 XMAX=SQRT(1.-(XMT/ET)**2)
      DO 301 I=1,N
      XM2(I)=XM(I)**2
  301 P2(I)=P(4,I)**2
      ITER=0
      X=XMAX
      ACCU=ET*ACC
  302 F0=-ET
      G0=0.
      X2=X*X
      DO 303 I=1,N
      E(I)=SQRT(XM2(I)+X2*P2(I))
      F0=F0+E(I)
  303 G0=G0+P2(I)/E(I)
      IF(ABS(F0).LE.ACCU) GOTO 305
      ITER=ITER+1
      IF(ITER.LE.ITMAX) GOTO 304
      PRINT 1006,ITMAX
      GOTO 305
  304 X=X-F0/(X*G0)
      GOTO 302
  305 DO 307 I=1,N
      V(I)=X*P(4,I)
      DO 306 K=1,3
  306 P(K,I)=X*P(K,I)
  307 P(4,I)=E(I)
*
* CALCULATE THE MASS-EFFECT WEIGHT FACTOR
      WT2=1.
      WT3=0.
      DO 308 I=1,N
      WT2=WT2*V(I)/E(I)
  308 WT3=WT3+V(I)**2/E(I)
      WTM=(2.*N-3.)*LOG(X)+LOG(WT2/WT3*ET)
*
* RETURN FOR  WEIGHTED MASSIVE MOMENTA
      WT=WT+WTM
      IF(WT.GE.-180.D0) GOTO 309
      IF(IWARN(3).LE.5) PRINT 1004,WT
      IWARN(3)=IWARN(3)+1
  309 IF(WT.LE. 174.D0) GOTO 310
      IF(IWARN(4).LE.5) PRINT 1005,WT
      IWARN(4)=IWARN(4)+1
* RETURN LOG OF WEIGHT
  310 WT=WT
      RETURN
*
 1001 FORMAT(' RAMBO FAILS: # OF PARTICLES =',I5,' IS NOT ALLOWED')
 1002 FORMAT(' RAMBO FAILS: TOTAL MASS =',D15.6,' IS NOT',
     . ' SMALLER THAN TOTAL ENERGY =',D15.6)
 1004 FORMAT(' RAMBO WARNS: WEIGHT = EXP(',F20.9,') MAY UNDERFLOW')
 1005 FORMAT(' RAMBO WARNS: WEIGHT = EXP(',F20.9,') MAY  OVERFLOW')
 1006 FORMAT(' RAMBO WARNS:',I3,' ITERATIONS DID NOT GIVE THE',
     . ' DESIRED ACCURACY =',D15.6)
      END

      FUNCTION RN(IDUMMY)
      REAL*8 RN,RAN
      SAVE INIT
      DATA INIT /1/
      IF (INIT.EQ.1) THEN
        INIT=0
        CALL RMARIN(1802,9373)
      END IF
*
  10  CALL RANMAR(RAN)
      IF (RAN.LT.1D-16) GOTO 10
      RN=RAN
*
      END



      SUBROUTINE RANMAR(RVEC)
*     -----------------
* Universal random number generator proposed by Marsaglia and Zaman
* in report FSU-SCRI-87-50
* In this version RVEC is a double precision variable.
      IMPLICIT REAL*8(A-H,O-Z)
      COMMON/ RASET1 / RANU(97),RANC,RANCD,RANCM
      COMMON/ RASET2 / IRANMR,JRANMR
      SAVE /RASET1/,/RASET2/
      UNI = RANU(IRANMR) - RANU(JRANMR)
      IF(UNI .LT. 0D0) UNI = UNI + 1D0
      RANU(IRANMR) = UNI
      IRANMR = IRANMR - 1
      JRANMR = JRANMR - 1
      IF(IRANMR .EQ. 0) IRANMR = 97
      IF(JRANMR .EQ. 0) JRANMR = 97
      RANC = RANC - RANCD
      IF(RANC .LT. 0D0) RANC = RANC + RANCM
      UNI = UNI - RANC
      IF(UNI .LT. 0D0) UNI = UNI + 1D0
      RVEC = UNI
      END
 
      SUBROUTINE RMARIN(IJ,KL)
*     -----------------
* Initializing routine for RANMAR, must be called before generating
* any pseudorandom numbers with RANMAR. The input values should be in
* the ranges 0<=ij<=31328 ; 0<=kl<=30081
      IMPLICIT REAL*8(A-H,O-Z)
      COMMON/ RASET1 / RANU(97),RANC,RANCD,RANCM
      COMMON/ RASET2 / IRANMR,JRANMR
      SAVE /RASET1/,/RASET2/
* This shows correspondence between the simplified input seeds IJ, KL
* and the original Marsaglia-Zaman seeds I,J,K,L.
* To get the standard values in the Marsaglia-Zaman paper (i=12,j=34
* k=56,l=78) put ij=1802, kl=9373
      I = MOD( IJ/177 , 177 ) + 2
      J = MOD( IJ     , 177 ) + 2
      K = MOD( KL/169 , 178 ) + 1
      L = MOD( KL     , 169 )
      DO 300 II = 1 , 97
        S =  0D0
        T = .5D0
        DO 200 JJ = 1 , 24
          M = MOD( MOD(I*J,179)*K , 179 )
          I = J
          J = K
          K = M
          L = MOD( 53*L+1 , 169 )
          IF(MOD(L*M,64) .GE. 32) S = S + T
          T = .5D0*T
  200   CONTINUE
        RANU(II) = S
  300 CONTINUE
      RANC  =   362436D0 / 16777216D0
      RANCD =  7654321D0 / 16777216D0
      RANCM = 16777213D0 / 16777216D0
      IRANMR = 97
      JRANMR = 33
      END






