      subroutine orxxxx(p,rmass,nhel,nsr , ro)
c
c This subroutine computes a Rarita-Schwinger wavefunction of spin-3/2 fermion.
c
c input:
c       real    p(0:3)         : four-momentum of fermion
c       real    rmass          : mass          of fermion
c       integer nhel = -3,-1,1,3 : helicity      of fermion
c       integer nsf  = -1 or 1 : +1 for particle, -1 for anti-particle
c
c output:
c       complex ro(18)          : Rarita-Schwinger wavefunction               <ro|
c    

      implicit none
      double precision p(0:3), rmass
      integer nhel, nsr
      double complex ro(18)
      
      integer stdo
      parameter( stdo = 6 )

      double precision rZero, rHalf, rOne, rTwo, pi
      parameter( rZero = 0.0d0, rHalf = 0.5d0 )
      parameter( rOne = 1.0d0, rTwo = 2.0d0 )

      double complex ft(6,4), ep(4), em(4), e0(4)
      double precision pt, pt2, pp, pzpt, emp, sqh, sqs
      integer i, j

      double precision sqm, sf(2),sfomeg(2),omega(2),pp3,sqp0p3
      double precision phase
      integer hel
      double complex fip(6), fim(6), chi, chip, chim
     
#ifdef HELAS_CHECK
      double precision p2, pp1
      double precision epsi
      parameter( epsi = 2.0d-5 )
      
#endif

#ifdef HELAS_CHECK
      pp1 = sqrt(p(1)**2+p(2)**2+p(3)**2)
      if ( abs(p(0))+pp1.eq.rZero ) then
         write(stdo,*)
     &        ' helas-error : p(0:3) in ixxxxx is zero momentum'
      endif
      if ( p(0).le.rZero ) then
         write(stdo,*)
     &        ' helas-error : p(0:3) in ixxxxx has non-positive energy'
         write(stdo,*)
     &        '             : p(0) = ',p(0)
      endif
      p2 = (p(0)-pp1)*(p(0)+pp1)
      if ( abs(p2-rmass**2).gt.p(0)**2*epsi ) then
         write(stdo,*)
     &        ' helas-error : p(0:3) in ixxxxx has inappropriate mass'
         write(stdo,*)
     &        '             : p**2 = ',p2,' : rmass**2 = ',rmass**2
      endif
      if (abs(nhel).ne.3) then
         if (abs(nhel).ne.1) then
            write(stdo,*) ' helas-error : nhel in ixxxxx is not -1,1'
            write(stdo,*) '             : nhel = ',nhel
         endif
      endif
      if (abs(nsr).ne.1) then
         write(stdo,*) ' helas-error : nsr in ixxxxx is not -1,1'
         write(stdo,*) '             : nsr = ',nsr
      endif
#endif

      pi = acos(-1.0d0)

      sqh = sqrt(rHalf)
      sqs = sqrt(rHalf/3.d0)

      pt2 = p(1)**2 + p(2)**2
      pp = min(p(0),sqrt(pt2+p(3)**2))
      pt = min(pp,sqrt(pt2))

      ft(5,1) = dcmplx(p(0),p(3))*nsr
      ft(6,1) = dcmplx(p(1),p(2))*nsr
      
      if ( nhel.ge.0 ) then 
c construct eps+
         if ( pp.eq.rZero ) then
            ep(1) = dcmplx( rZero )
            ep(2) = dcmplx( -sqh )
            ep(3) = dcmplx( rZero , -1*sqh )
            ep(4) = dcmplx( rZero )
         else
            ep(1) = dcmplx( rZero )
            ep(4) = dcmplx( pt/pp*sqh )
            if ( pt.ne.rZero ) then
               pzpt = p(3)/(pp*pt)*sqh
               ep(2) = dcmplx( -p(1)*pzpt , p(2)/pt*sqh )
               ep(3) = dcmplx( -p(2)*pzpt ,  -1*p(1)/pt*sqh )
            else
               ep(2) = dcmplx( -sqh )
               ep(3) = dcmplx( rZero , -1*sign(sqh,p(3)) )
            endif
         endif
      end if

      if ( nhel.le.0 ) then 
c construct eps-
         if ( pp.eq.rZero ) then
            em(1) = dcmplx( rZero )
            em(2) = dcmplx( sqh )
            em(3) = dcmplx( rZero , -1*sqh )
            em(4) = dcmplx( rZero )
         else
            em(1) = dcmplx( rZero )
            em(4) = dcmplx( -pt/pp*sqh )
            if ( pt.ne.rZero ) then
               pzpt = -p(3)/(pp*pt)*sqh
               em(2) = dcmplx( -p(1)*pzpt , p(2)/pt*sqh )
               em(3) = dcmplx( -p(2)*pzpt ,  -1*p(1)/pt*sqh )
            else
               em(2) = dcmplx( sqh )
               em(3) = dcmplx( rZero , -1*sign(sqh,p(3)) )
            endif
         endif
      end if

      if ( abs(nhel).le.1 ) then  
c construct eps0
         if ( pp.eq.rZero ) then
            e0(1) = dcmplx( rZero )
            e0(2) = dcmplx( rZero )
            e0(3) = dcmplx( rZero )
            e0(4) = dcmplx( rOne )
         else
            emp = p(0)/(rmass*pp)
            e0(1) = dcmplx( pp/rmass )
            e0(4) = dcmplx( p(3)*emp )
            if ( pt.ne.rZero ) then
               e0(2) = dcmplx( p(1)*emp )
               e0(3) = dcmplx( p(2)*emp )
            else
               e0(2) = dcmplx( rZero )
               e0(3) = dcmplx( rZero )
            endif
         end if
      end if

c construct fermion wavefunctions
      pp = min(p(0),dsqrt(p(1)**2+p(2)**2+p(3)**2))
                 
         if ( rmass.ne.rZero ) then

             if ( pp.eq.rZero ) then
            
                sqm = dsqrt(rmass)
                
                fip(1) = 1 * sqm * nsr
                fip(2) = 0               
                fip(3) = 1 * sqm
                fip(4) = 0
                
                fim(1) = 0
                fim(2) = 1 * sqm 
                fim(3) = 0
                fim(4) = 1 * sqm * nsr
                        
       
             else

                omega(1) = dsqrt(p(0)+pp)
                omega(2) = rmass/omega(1)
                pp3 = max(pp+p(3),rZero)
                chi = dcmplx( dsqrt(pp3*rHalf/pp) )
                if ( pp3.eq.rZero ) then
                   chip = dcmplx(-1)
                   chim = dcmplx(1)
                else
                   chip = dcmplx(p(1) , -p(2) )/dsqrt(rTwo*pp*pp3)
                   chim = dcmplx(-p(1) , -p(2) )/dsqrt(rTwo*pp*pp3)
                endif
            
                fip(1) = omega(1)*chi*nsr
                fip(2) = omega(1)*chip*nsr
                fip(3) = omega(2)*chi
                fip(4) = omega(2)*chip
                
                fim(1) = omega(2)*chim
                fim(2) = omega(2)*chi
                fim(3) = omega(1)*chim*nsr
                fim(4) = omega(1)*chi*nsr
                
            
             endif
          else
         
            sqp0p3 = dsqrt(max(p(0)+p(3),rZero))*nsr
            chi = dcmplx( sqp0p3 )
            if ( sqp0p3.eq.rZero ) then
               chip = dcmplx(-1)*dsqrt(rTwo*p(0))
               chim = dcmplx(1)*dsqrt(rTwo*p(0))
            else
               chip = dcmplx( p(1), -p(2) )/sqp0p3
               chim = dcmplx( -p(1), -p(2) )/sqp0p3
            endif
            
            fip(1) = chi*nsr
            fip(2) = chip*nsr
            fip(3) = dcmplx( rZero )
            fip(4) = dcmplx( rZero )
               
            fim(1) = dcmplx( rZero )
            fim(2) = dcmplx( rZero )
            fim(3) = chim*nsr
            fim(4) = chi*nsr

          endif

c construct Rarita-Schwinger wavefunction
      if (P(1).eq.0) then
         if (P(2).gt.0) then
            phase = pi/2
         elseif (P(2).eq.0) then
            if (P(3).ge.0) then
               phase = 0
            else 
               phase = pi
            endif
         else 
            phase = -pi/2
         endif
      else 
         phase = atan(P(2)/P(1))
      endif

      if (nsr.eq. 1 ) then

         if ( nhel.eq.3 ) then
            do j = 1,4
               do i = 1,4
                  ft(i,j) = conjg(ep(i))*fip(j)
               end do
            end do
         else if ( nhel.eq.1 ) then
            do j = 1,4
               do i = 1,4
                  ft(i,j) = dsqrt(2/3.d0)*conjg(e0(i))*fip(j)+
     &                      dsqrt(1/3.d0)*conjg(ep(i))*fim(j)
     &                      *cexp(cmplx(0,-phase))
               end do
            end do
         else if ( nhel.eq.-1 ) then
            do j = 1,4
               do i = 1,4
                  ft(i,j) = dsqrt(2/3.d0)*conjg(e0(i))*fim(j)
     &                      *cexp(cmplx(0,-phase))
     &                      +dsqrt(1/3.d0)*conjg(em(i))*fip(j)
               end do
            end do
         else if ( nhel.eq.-3 ) then
            do j = 1,4
               do i = 1,4
                  ft(i,j) = conjg(em(i))*fim(j)*cexp(cmplx(0,-phase))
               end do
            end do
         else
            write(stdo,*) 'invalid helicity in TXXXXX'
            stop
         end if

      else
        
         if ( nhel.eq.3 ) then
            do j = 1,4
               do i = 1,4
                  ft(i,j) = ep(i)*fim(j)
               end do
            end do
         else if ( nhel.eq.1 ) then
            do j = 1,4
               do i = 1,4
                  ft(i,j) = dsqrt(2/3.d0)*e0(i)*fim(j)+
     &                      dsqrt(1/3.d0)*ep(i)*fip(j)
     &                      *cexp(cmplx(0,phase))
               end do
            end do
         else if ( nhel.eq.-1 ) then
            do j = 1,4
               do i = 1,4
                  ft(i,j) = dsqrt(2/3.d0)*e0(i)*fip(j)
     &                      *cexp(cmplx(0,phase))
     &                      +dsqrt(1/3.d0)*em(i)*fim(j)
               end do
            end do
         else if ( nhel.eq.-3 ) then
            do j = 1,4
               do i = 1,4
                  ft(i,j) = em(i)*fip(j)
     &                      *cexp(cmplx(0,phase))
               end do
            end do
         else
            write(stdo,*) 'invalid helicity in TXXXXX'
            stop
         end if
      endif

      ro(1) = ft(1,1)
      ro(2) = ft(1,2)
      ro(3) = ft(1,3)
      ro(4) = ft(1,4)
      ro(5) = ft(2,1)
      ro(6) = ft(2,2)
      ro(7) = ft(2,3)
      ro(8) = ft(2,4)
      ro(9) = ft(3,1)
      ro(10) = ft(3,2)
      ro(11) = ft(3,3)
      ro(12) = ft(3,4)
      ro(13) = ft(4,1)
      ro(14) = ft(4,2)
      ro(15) = ft(4,3)
      ro(16) = ft(4,4)
      ro(17) = ft(5,1)
      ro(18) = ft(6,1)
      

      return
      end
