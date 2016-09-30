      subroutine rvixxx(fi,vc,gc,rmass,rwidth , rvi)
c
c This subroutine computes an off-shell Rarita Schwinger fermion wavefunction from a
c flowing-In external fermion and a vector boson.
c
c
c input:
c       complex fi(6)          : flow-in fermion                           |fi>
c       complex vc(6)          : input    vector                            v
c       complex gc(2)          : coupling constants                        gvf
c       real    rmass          : mass  of OUTPUT Rarita Schwinger fermion   r'
c       real    rwidth         : width of OUTPUT Rarita Schwinger fermion   r'
c
c output:
c       complex rvi(18)        : off-shell Rarita Schwinger fermion       |r',v,fi>
c
c- by Y.Takaesu - 2010/01/10
c
      implicit none
      double complex fi(6),vc(6),gc(2),rvi(18),d
      double precision rmass,rwidth

      double precision rZero, rOne
      parameter( rZero = 0.0d0, rOne = 1.0d0 )
      double complex cImag, cZero
      parameter( cImag = ( 0.0d0, 1.0d0 ), cZero = ( 0.0d0, 0.0d0 ) )

      integer i,j,k,l,m

      double precision q(0:3),p(0:3),pdv,pdq,a,p2
      double complex sgp(0:3,2,2),sgm(0:3,2,2),sq1(2,2),sq2(2,2)
      double complex sp2(2,2),sv1(2,2),sv2(2,2),sqsv1(2,2),sqsv2(2,2)
      double complex spsq1(2,2),spsq2(2,2),spsv1(2,2),spsv2(2,2)
      double complex qv1(2,2),qv2(2,2),svsq1(2,2),svsq2(2,2),sp1(2,2)
      double complex gqv1(0:3,2,2),gqv2(0:3,2,2),gpqv1(0:3,2,2)
      double complex gpqv2(0:3,2,2),gq1(0:3,2,2),gq2(0:3,2,2)
      double complex gv1(0:3,2,2),gv2(0:3,2,2),gpq1(0:3,2,2)
      double complex gpq2(0:3,2,2),gpv1(0:3,2,2),gpv2(0:3,2,2),rc(0:3,4)
      double complex pqv1(2,2),pqv2(2,2),pqvp1(2,2),pqvp2(2,2)
      

#ifdef HELAS_CHECK
      integer stdo
      parameter( stdo = 6 )
#endif

#ifdef HELAS_CHECK
      if ( abs(fi(1))+abs(fi(2))+abs(fi(3))+abs(fi(4))).eq.rZero ) then
         write(stdo,*) ' helas-warn  : fi in rvixxx is zero spinor'
      endif
      if ( abs(fi(5))+abs(fi(6)).eq.rZero ) then
         write(stdo,*)
     &        ' helas-error : fi in rvixxx has zero momentum'
      endif
      if ( abs(vc(1))+abs(vc(2)+abs(vc(3))+abs(vc(4)).eq.rZero ) then
         write(stdo,*) ' helas-warn  : vc in rvixxx is zero vector'
      endif
      if ( abs(vc(5))+abs(vc(6)).eq.rZero ) then
         write(stdo,*)
     &        ' helas-error : vc in rvixxx has zero momentum'
      endif
      if ( gc(1).eq.cZero .and. gc(2).eq.cZero ) then
         write(stdo,*)
     &        ' helas-error : gc in rvixxx is zero coupling'
      endif
      if ( rmass.lt.rZero ) then
         write(stdo,*) ' helas-error : rmass in rvixxx is negative'
         write(stdo,*) '             : rmass = ',rmass
      endif
      if ( rwidth.lt.rZero ) then
         write(stdo,*) ' helas-error : rwidth in rvixxx is negative'
         write(stdo,*) '             : rwidth = ',rwidth
      endif
#endif

      rvi(17) = -vc(5)+fi(5)
      rvi(18) = -vc(6)+fi(6)

      p(0) = dble( rvi(17))
      p(1) = dble( rvi(18))
      p(2) = dimag(rvi(18))
      p(3) = dimag(rvi(17))
      p2 = p(0)**2-(p(1)**2+p(2)**2+p(3)**2)

#ifdef HELAS_CHECK
      if ( abs(rvi(17))+abs(rvi(18)).eq.rZero ) then
          write(stdo,*)
     &        ' helas-error : rvi in rvixxx has zero momentum'
       endif
       if ( rwidth.eq.rZero .and. pf2.eq.rmass**2 ) then
          write(stdo,*)
     &         ' helas-error : rvi in rvixxx is on rmass pole'
          write(stdo,*)
     &         '             : p     = ',pf(0),pf(1),pf(2),pf(3)
          write(stdo,*)
     &         '             : abs(p)= ',sqrt(abs(pf2))

         do i=1,16
          rvi(i) = cZero
         enddo
         return
      endif
#endif

      do i=1,16
       rvi(i) = cZero
      enddo

      q(0) = dble( vc(5))
      q(1) = dble( vc(6))
      q(2) = dimag(vc(6))
      q(3) = dimag(vc(5))

******* Pauli sigma matrix *********
      sgp(0,1,1) = rOne
      sgp(0,1,2) = rZero
      sgp(0,2,1) = rZero
      sgp(0,2,2) = rOne
      sgp(1,1,1) = rZero
      sgp(1,1,2) = rOne
      sgp(1,2,1) = rOne
      sgp(1,2,2) = rZero
      sgp(2,1,1) = rZero
      sgp(2,1,2) = -cImag
      sgp(2,2,1) = cImag
      sgp(2,2,2) = rZero
      sgp(3,1,1) = rOne
      sgp(3,1,2) = rZero
      sgp(3,2,1) = rZero
      sgp(3,2,2) = -rOne

      sgm(0,1,1) = rOne
      sgm(0,1,2) = rZero
      sgm(0,2,1) = rZero
      sgm(0,2,2) = rOne
      sgm(1,1,1) = rZero
      sgm(1,1,2) = -rOne
      sgm(1,2,1) = -rOne
      sgm(1,2,2) = rZero
      sgm(2,1,1) = rZero
      sgm(2,1,2) = cImag
      sgm(2,2,1) = -cImag
      sgm(2,2,2) = rZero
      sgm(3,1,1) = -rOne
      sgm(3,1,2) = rZero
      sgm(3,2,1) = rZero
      sgm(3,2,2) = rOne
******* Pauli sigma matrix ***********

      sq1(1,1) = q(0)-q(3)
      sq1(1,2) = -(q(1)-cImag*q(2))
      sq1(2,1) = -(q(1)+cImag*q(2))
      sq1(2,2) = q(0)+q(3)

      sq2(1,1) = q(0)+q(3)
      sq2(1,2) = q(1)-cImag*q(2)
      sq2(2,1) = q(1)+cImag*q(2)
      sq2(2,2) = q(0)-q(3)

      sp1(1,1) = p(0)-p(3)
      sp1(1,2) = -(p(1)-cImag*p(2))
      sp1(2,1) = -(p(1)+cImag*p(2))
      sp1(2,2) = p(0)+p(3)

      sp2(1,1) = p(0)+p(3)
      sp2(1,2) = p(1)-cImag*p(2)
      sp2(2,1) = p(1)+cImag*p(2)
      sp2(2,2) = p(0)-p(3)
      
      sv1(1,1) = vc(1)-vc(4)
      sv1(1,2) = -(vc(2)-cImag*vc(3))
      sv1(2,1) = -(vc(2)+cImag*vc(3))
      sv1(2,2) = vc(1)+vc(4)

      sv2(1,1) = vc(1)+vc(4)
      sv2(1,2) = vc(2)-cImag*vc(3)
      sv2(2,1) = vc(2)+cImag*vc(3)
      sv2(2,2) = vc(1)-vc(4)

      pdv = p(0)*vc(1)-p(1)*vc(2)-p(2)*vc(3)-p(3)*vc(4)
      pdq = p(0)*q(0)-p(1)*q(1)-p(2)*q(2)-p(3)*q(3)

      d = cImag/dcmplx( p2-rmass**2, rmass*rwidth )

      do i=1,2
         do j=1,2
      sqsv1(i,j) = sq1(i,1)*sv2(1,j)+sq1(i,2)*sv2(2,j)
      sqsv2(i,j) = sq2(i,1)*sv1(1,j)+sq2(i,2)*sv1(2,j)

      svsq1(i,j) = sv1(i,1)*sq2(1,j)+sv1(i,2)*sq2(2,j)
      svsq2(i,j) = sv2(i,1)*sq1(1,j)+sv2(i,2)*sq1(2,j)

      spsq1(i,j) = sp1(i,1)*sq2(1,j)+sp1(i,2)*sq2(2,j)
      spsq2(i,j) = sp2(i,1)*sq1(1,j)+sp2(i,2)*sq1(2,j)

      spsv1(i,j) = sp1(i,1)*sv2(1,j)+sp1(i,2)*sv2(2,j)
      spsv2(i,j) = sp2(i,1)*sv1(1,j)+sp2(i,2)*sv1(2,j)

      qv1(i,j) = sqsv1(i,j)-svsq1(i,j)
      qv2(i,j) = sqsv2(i,j)-svsq2(i,j)
     
         enddo
      enddo

      do i = 1,2
         do j = 1,2
             pqv1(i,j) = sp1(i,1)*qv2(1,j)+sp1(i,2)*qv2(2,j)
             pqv2(i,j) = sp2(i,1)*qv1(1,j)+sp2(i,2)*qv1(2,j)             
         enddo
      enddo

      do i=1,2
         do j=1,2
            pqvp1(i,j) = pqv1(i,1)*sp2(1,j)+pqv1(i,2)*sp2(2,j)
            pqvp2(i,j) = pqv2(i,1)*sp1(1,j)+pqv2(i,2)*sp1(2,j)
         enddo
      enddo

      do k=0,3
       do i=1,2
        do j=1,2
      gqv1(k,i,j) = sgp(k,i,1)*qv2(1,j)+sgp(k,i,2)*qv2(2,j)     
      gqv2(k,i,j) = sgm(k,i,1)*qv1(1,j)+sgm(k,i,2)*qv1(2,j)
      
      gpqv1(k,i,j) = sgp(k,i,1)*pqv2(1,j)+sgp(k,i,2)*pqv2(2,j)
      gpqv2(k,i,j) = sgm(k,i,1)*pqv1(1,j)+sgm(k,i,2)*pqv1(2,j)
     
      gq1(k,i,j) = sgp(k,i,1)*sq2(1,j)+sgp(k,i,2)*sq2(2,j)
      gq2(k,i,j) = sgm(k,i,1)*sq1(1,j)+sgm(k,i,2)*sq1(2,j)  

      gv1(k,i,j) = sgp(k,i,1)*sv2(1,j)+sgp(k,i,2)*sv2(2,j)
      gv2(k,i,j) = sgm(k,i,1)*sv1(1,j)+sgm(k,i,2)*sv1(2,j)
      
      gpq1(k,i,j) = sgp(k,i,1)*spsq2(1,j)+sgp(k,i,2)*spsq2(2,j)
      gpq2(k,i,j) = sgm(k,i,1)*spsq1(1,j)+sgm(k,i,2)*spsq1(2,j)

      gpv1(k,i,j) = sgp(k,i,1)*spsv2(1,j)+sgp(k,i,2)*spsv2(2,j)
      gpv2(k,i,j) = sgm(k,i,1)*spsv1(1,j)+sgm(k,i,2)*spsv1(2,j)
        enddo
       enddo
      enddo    
           
      if ( gc(1).ne.cZero ) then
 
         do i=0,3

       rvi(4*i+1) = gc(1)*cImag*d*(2d0*p(i)/(3d0*rmass**2)
     &           *(pqvp1(1,1)*fi(1)+pqvp1(1,2)*fi(2))
     &           +2d0*p(i)/(3d0*rmass**2)
     &           *(-3d0*rmass**2)*(qv1(1,1)*fi(1)+qv1(1,2)*fi(2))
     &           +4d0/3d0*(gpqv1(i,1,1)*fi(1)+gpqv1(i,1,2)*fi(2))
     &           +4d0*(-vc(i+1))
     &           *(spsq1(1,1)*fi(1)+spsq1(1,2)*fi(2))
     &           -4d0*(-q(i))
     &           *(spsv1(1,1)*fi(1)+spsv1(1,2)*fi(2))
     &           +4d0*pdv/3d0*(gq1(i,1,1)*fi(1)+gq1(i,1,2)*fi(2))
     &           -4d0*pdq/3d0*(gv1(i,1,1)*fi(1)+gv1(i,1,2)*fi(2)))

       rvi(4*i+2) = gc(1)*cImag*d*(2d0*p(i)/(3d0*rmass**2)
     &           *(pqvp1(2,1)*fi(1)+pqvp1(2,2)*fi(2))
     &           +2d0*p(i)/(3d0*rmass**2)
     &           *(-3d0*rmass**2)*(qv1(2,1)*fi(1)+qv1(2,2)*fi(2))
     &           +4d0/3d0*(gpqv1(i,2,1)*fi(1)+gpqv1(i,2,2)*fi(2))
     &           +4d0*(-vc(i+1))
     &           *(spsq1(2,1)*fi(1)+spsq1(2,2)*fi(2))
     &           -4d0*(-q(i))
     &           *(spsv1(2,1)*fi(1)+spsv1(2,2)*fi(2))
     &           +4d0*pdv/3d0*(gq1(i,2,1)*fi(1)+gq1(i,2,2)*fi(2))
     &           -4d0*pdq/3d0*(gv1(i,2,1)*fi(1)+gv1(i,2,2)*fi(2)))
                    
      rvi(4*i+3) = gc(1)*cImag*d*(-(p2+3d0*rmass**2)/(3d0*rmass)
     &          *(gqv2(i,1,1)*fi(1)+gqv2(i,1,2)*fi(2))
     &          +4d0*p(i)/(3d0*rmass)*(pqv2(1,1)*fi(1)+pqv2(1,2)*fi(2))
     &          +4d0*(-rmass*vc(i+1)+4d0*p(i)*pdv/(3d0*rmass))
     &          *(sq2(1,1)*fi(1)+sq2(1,2)*fi(2))
     &          -4d0*(-rmass*q(i)+4d0*p(i)*pdq/(3d0*rmass))
     &          *(sv2(1,1)*fi(1)+sv2(1,2)*fi(2))
     &        -4d0*pdv/(3d0*rmass)*(gpq2(i,1,1)*fi(1)+gpq2(i,1,2)*fi(2))
     &       +4d0*pdq/(3d0*rmass)*(gpv2(i,1,1)*fi(1)+gpv2(i,1,2)*fi(2)))

      rvi(4*i+4) = gc(1)*cImag*d*(-(p2+3d0*rmass**2)/(3d0*rmass)
     &          *(gqv2(i,2,1)*fi(1)+gqv2(i,2,2)*fi(2))
     &          +4d0*p(i)/(3d0*rmass)*(pqv2(2,1)*fi(1)+pqv2(2,2)*fi(2))
     &          +4d0*(-rmass*vc(i+1)+4d0*p(i)*pdv/(3d0*rmass))
     &          *(sq2(2,1)*fi(1)+sq2(2,2)*fi(2))
     &          -4d0*(-rmass*q(i)+4d0*p(i)*pdq/(3d0*rmass))
     &          *(sv2(2,1)*fi(1)+sv2(2,2)*fi(2))
     &        -4d0*pdv/(3d0*rmass)*(gpq2(i,2,1)*fi(1)+gpq2(i,2,2)*fi(2))
     &       +4d0*pdq/(3d0*rmass)*(gpv2(i,2,1)*fi(1)+gpv2(i,2,2)*fi(2)))

     
      enddo
      endif

      if ( gc(2).ne.cZero ) then 
      do i=0,3
        
      rvi(4*i+1) =rvi(4*i+1)
     &          +gc(2)*cImag*d*(-(p2+3d0*rmass**2)/(3d0*rmass)
     &          *(gqv1(i,1,1)*fi(3)+gqv1(i,1,2)*fi(4))
     &          +4d0*p(i)/(3d0*rmass)*(pqv1(1,1)*fi(3)+pqv1(1,2)*fi(4))
     &          +4d0*(-rmass*vc(i+1)+4d0*p(i)*pdv/(3d0*rmass))
     &          *(sq1(1,1)*fi(3)+sq1(1,2)*fi(4))
     &          -4d0*(-rmass*q(i)+4d0*p(i)*pdq/(3d0*rmass))
     &          *(sv1(1,1)*fi(3)+sv1(1,2)*fi(4))
     &        -4d0*pdv/(3d0*rmass)*(gpq1(i,1,1)*fi(3)+gpq1(i,1,2)*fi(4))
     &       +4d0*pdq/(3d0*rmass)*(gpv1(i,1,1)*fi(3)+gpv1(i,1,2)*fi(4)))

      rvi(4*i+2) =rvi(4*i+2)
     &          +gc(2)*cImag*d*(-(p2+3d0*rmass**2)/(3d0*rmass)
     &          *(gqv1(i,2,1)*fi(3)+gqv1(i,2,2)*fi(4))
     &          +4d0*p(i)/(3d0*rmass)*(pqv1(2,1)*fi(3)+pqv1(2,2)*fi(4))
     &          +4d0*(-rmass*vc(i+1)+4d0*p(i)*pdv/(3d0*rmass))
     &          *(sq1(2,1)*fi(3)+sq1(2,2)*fi(4))
     &          -4d0*(-rmass*q(i)+4d0*p(i)*pdq/(3d0*rmass))
     &          *(sv1(2,1)*fi(3)+sv1(2,2)*fi(4))
     &        -4d0*pdv/(3d0*rmass)*(gpq1(i,2,1)*fi(3)+gpq1(i,2,2)*fi(4))
     &       +4d0*pdq/(3d0*rmass)*(gpv1(i,2,1)*fi(3)+gpv1(i,2,2)*fi(4)))

        rvi(4*i+3) = gc(2)*cImag*d*(2d0*p(i)/(3d0*rmass**2)
     &           *(pqvp2(1,1)*fi(3)+pqvp2(1,2)*fi(4))
     &           +2d0*p(i)/(3d0*rmass**2)
     &           *(-3d0*rmass**2)*(qv2(1,1)*fi(3)+qv2(1,2)*fi(4))
     &           +4d0/3d0*(gpqv2(i,1,1)*fi(2)+gpqv2(i,1,2)*fi(4))
     &           +4d0*(-vc(i+1))
     &           *(spsq2(1,1)*fi(3)+spsq2(1,2)*fi(4))
     &           -4d0*(-q(i))
     &           *(spsv2(1,1)*fi(3)+spsv2(1,2)*fi(4))
     &           +4d0*pdv/3d0*(gq2(i,1,1)*fi(3)+gq2(i,1,2)*fi(4))
     &           -4d0*pdq/3d0*(gv2(i,1,1)*fi(3)+gv2(i,1,2)*fi(4)))

       rvi(4*i+4) = gc(2)*cImag*d*(2d0*p(i)/(3d0*rmass**2)
     &           *(pqvp2(2,1)*fi(3)+pqvp2(2,2)*fi(4))
     &           +2d0*p(i)/(3d0*rmass**2)
     &           *(-3d0*rmass**2)*(qv2(2,1)*fi(3)+qv2(2,2)*fi(4))
     &           +4d0/3d0*(gpqv2(i,2,1)*fi(3)+gpqv2(i,2,2)*fi(4))
     &           +4d0*(-vc(i+1))
     &           *(spsq2(2,1)*fi(3)+spsq2(2,2)*fi(4))
     &           -4d0*(-q(i))
     &           *(spsv2(2,1)*fi(3)+spsv2(2,2)*fi(4))
     &           +4d0*pdv/3d0*(gq2(i,2,1)*fi(3)+gq2(i,2,2)*fi(4))
     &           -4d0*pdq/3d0*(gv2(i,2,1)*fi(3)+gv2(i,2,2)*fi(4)))

      enddo
      

      endif
 

      Return
      end
