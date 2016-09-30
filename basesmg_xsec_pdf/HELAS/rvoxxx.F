      subroutine rvoxxx(fo,vc,gc,rmass,rwidth , rvo)
c
c This subroutine computes an off-shell Rarita Schwinger fermion wavefunction from a
c flowing-Out external fermion and a vector boson.
c
c
c input:
c       complex fo(6)          : flow-out fermion                          |fo>
c       complex vc(6)          : input    vector                            v
c       complex gc(2)          : coupling constants                        gvf
c       real    rmass          : mass  of OUTPUT Rarita Schwinger fermion   r'
c       real    rwidth         : width of OUTPUT Rarita Schwinger fermion   r'
c
c output:
c       complex rvo(18)        : off-shell Rarita Schwinger fermion       |r',v,fo>
c
c- by Y.Takaesu - 2010/01/09
c
      implicit none
      double complex fo(6),vc(6),gc(2),rvo(18),d
      double precision rmass,rwidth

      double precision rZero, rOne
      parameter( rZero = 0.0d0, rOne = 1.0d0 )
      double complex cImag, cZero
      parameter( cImag = ( 0.0d0, 1.0d0 ), cZero = ( 0.0d0, 0.0d0 ) )

      integer i,j,k,l,m

      double precision q(0:3),p(0:3),p2
      double complex sq1(2,2),sq2(2,2),sp1(2,2)
      double complex sp2(2,2),sv1(2,2),sv2(2,2),sqsv1(2,2),sqsv2(2,2)
      double complex qv1(2,2),qv2(2,2),svsq1(2,2),svsq2(2,2)
      double complex pqv1(2,2),pqv2(2,2),pqvp1(2,2),pqvp2(2,2)
      

#ifdef HELAS_CHECK
      integer stdo
      parameter( stdo = 6 )
#endif

#ifdef HELAS_CHECK
      if ( abs(fo(1))+abs(fo(2))+abs(fo(3))+abs(fo(4))).eq.rZero ) then
         write(stdo,*) ' helas-warn  : fo in rvoxxx is zero spinor'
      endif
      if ( abs(fo(5))+abs(fo(6)).eq.rZero ) then
         write(stdo,*)
     &        ' helas-error : fo in rvoxxx has zero momentum'
      endif
      if ( abs(vc(1))+abs(vc(2)+abs(vc(3))+abs(vc(4)).eq.rZero ) then
         write(stdo,*) ' helas-warn  : vc in rvoxxx is zero vector'
      endif
      if ( abs(vc(5))+abs(vc(6)).eq.rZero ) then
         write(stdo,*)
     &        ' helas-error : vc in rvoxxx has zero momentum'
      endif
      if ( gc(1).eq.cZero .and. gc(2).eq.cZero ) then
         write(stdo,*)
     &        ' helas-error : gc in rvoxxx is zero coupling'
      endif
      if ( rmass.lt.rZero ) then
         write(stdo,*) ' helas-error : rmass in rvoxxx is negative'
         write(stdo,*) '             : rmass = ',rmass
      endif
      if ( rwidth.lt.rZero ) then
         write(stdo,*) ' helas-error : rwidth in rvoxxx is negative'
         write(stdo,*) '             : rwidth = ',rwidth
      endif
#endif

      rvo(17) = vc(5)+fo(5)
      rvo(18) = vc(6)+fo(6)

      p(0) = dble( rvo(17))
      p(1) = dble( rvo(18))
      p(2) = dimag(rvo(18))
      p(3) = dimag(rvo(17))
      p2 = p(0)**2-(p(1)**2+p(2)**2+p(3)**2)

#ifdef HELAS_CHECK
      if ( abs(rvo(17))+abs(rvo(18)).eq.rZero ) then
          write(stdo,*)
     &        ' helas-error : rvo in rvoxxx has zero momentum'
       endif
       if ( rwidth.eq.rZero .and. pf2.eq.rmass**2 ) then
          write(stdo,*)
     &         ' helas-error : rvo in rvoxxx is on rmass pole'
          write(stdo,*)
     &         '             : p     = ',pf(0),pf(1),pf(2),pf(3)
          write(stdo,*)
     &         '             : abs(p)= ',sqrt(abs(pf2))

         do i=1,16
          rvo(i) = cZero
         enddo
         return
      endif
#endif

      do i=1,16
       rvo(i) = cZero
      enddo

      q(0) = dble( vc(5))
      q(1) = dble( vc(6))
      q(2) = dimag(vc(6))
      q(3) = dimag(vc(5))

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

      d = cImag/(p2-rmass**2+cImag*rwidth*rmass)
     
      sqsv1(1,1) = sq1(1,1)*sv2(1,1)+sq1(1,2)*sv2(2,1)
      sqsv1(1,2) = sq1(1,1)*sv2(1,2)+sq1(1,2)*sv2(2,2)
      sqsv1(2,1) = sq1(2,1)*sv2(1,1)+sq1(2,2)*sv2(2,1)
      sqsv1(2,2) = sq1(2,1)*sv2(1,2)+sq1(2,2)*sv2(2,2)
      sqsv2(1,1) = sq2(1,1)*sv1(1,1)+sq2(1,2)*sv1(2,1)
      sqsv2(1,2) = sq2(1,1)*sv1(1,2)+sq2(1,2)*sv1(2,2)
      sqsv2(2,1) = sq2(2,1)*sv1(1,1)+sq2(2,2)*sv1(2,1)
      sqsv2(2,2) = sq2(2,1)*sv1(1,2)+sq2(2,2)*sv1(2,2)

      svsq1(1,1) = sv1(1,1)*sq2(1,1)+sv1(1,2)*sq2(2,1)
      svsq1(1,2) = sv1(1,1)*sq2(1,2)+sv1(1,2)*sq2(2,2)
      svsq1(2,1) = sv1(2,1)*sq2(1,1)+sv1(2,2)*sq2(2,1)
      svsq1(2,2) = sv1(2,1)*sq2(1,2)+sv1(2,2)*sq2(2,2)
      svsq2(1,1) = sv2(1,1)*sq1(1,1)+sv2(1,2)*sq1(2,1)
      svsq2(1,2) = sv2(1,1)*sq1(1,2)+sv2(1,2)*sq1(2,2)
      svsq2(2,1) = sv2(2,1)*sq1(1,1)+sv2(2,2)*sq1(2,1)
      svsq2(2,2) = sv2(2,1)*sq1(1,2)+sv2(2,2)*sq1(2,2)

      qv1(1,1) = sqsv1(1,1)-svsq1(1,1)
      qv1(1,2) = sqsv1(1,2)-svsq1(1,2)
      qv1(2,1) = sqsv1(2,1)-svsq1(2,1)
      qv1(2,2) = sqsv1(2,2)-svsq1(2,2)
      qv2(1,1) = sqsv2(1,1)-svsq2(1,1)
      qv2(1,2) = sqsv2(1,2)-svsq2(1,2)
      qv2(2,1) = sqsv2(2,1)-svsq2(2,1)
      qv2(2,2) = sqsv2(2,2)-svsq2(2,2)

      pqv1(1,1) = sp1(1,1)*qv2(1,1)+sp1(1,2)*qv2(2,1)
      pqv1(1,2) = sp1(1,1)*qv2(1,2)+sp1(1,2)*qv2(2,2)
      pqv1(2,1) = sp1(2,1)*qv2(1,1)+sp1(2,2)*qv2(2,1)
      pqv1(2,2) = sp1(2,1)*qv2(1,2)+sp1(2,2)*qv2(2,2) 
      pqv2(1,1) = sp2(1,1)*qv1(1,1)+sp2(1,2)*qv1(2,1)
      pqv2(1,2) = sp2(1,1)*qv1(1,2)+sp2(1,2)*qv1(2,2)      
      pqv2(2,1) = sp2(2,1)*qv1(1,1)+sp2(2,2)*qv1(2,1)      
      pqv2(2,2) = sp2(2,1)*qv1(1,2)+sp2(2,2)*qv1(2,2)

      pqvp1(1,1) = pqv1(1,1)*sp2(1,1)+pqv1(1,2)*sp2(2,1)
      pqvp1(1,2) = pqv1(1,1)*sp2(1,2)+pqv1(1,2)*sp2(2,2)
      pqvp1(2,1) = pqv1(2,1)*sp2(1,1)+pqv1(2,2)*sp2(2,1)
      pqvp1(2,2) = pqv1(2,1)*sp2(1,2)+pqv1(2,2)*sp2(2,2)
      pqvp2(1,1) = pqv2(1,1)*sp1(1,1)+pqv2(1,2)*sp1(2,1)      
      pqvp2(1,2) = pqv2(1,1)*sp1(1,2)+pqv2(1,2)*sp1(2,2)      
      pqvp2(2,1) = pqv2(2,1)*sp1(1,1)+pqv2(2,2)*sp1(2,1)      
      pqvp2(2,2) = pqv2(2,1)*sp1(1,2)+pqv2(2,2)*sp1(2,2)

      if ( gc(2).ne.cZero ) then
 
         do i=0,3
        rvo(4*i+1) = -2*dconjg(gc(2))/(3*rmass**2)*p(i)/p2
     &               *( fo(1)*pqvp1(1,1) + fo(2)*pqvp1(2,1) )
        rvo(4*i+2) = -2*dconjg(gc(2))/(3*rmass**2)*p(i)/p2
     &               *( fo(1)*pqvp1(1,2) + fo(2)*pqvp1(2,2) )
        rvo(4*i+3) = cZero
        rvo(4*i+4) = cZero
      enddo
      endif

      if ( gc(1).ne.cZero ) then 
      do i=0,3
        rvo(4*i+1) = rvo(4*i+1)
        rvo(4*i+2) = rvo(4*i+2)
        rvo(4*i+3) = -2*dconjg(gc(1))/(3*rmass**2)*p(i)/p2
     &               *( fo(3)*pqvp2(1,1) + fo(4)*pqvp2(2,1) )
        rvo(4*i+4) = -2*dconjg(gc(1))/(3*rmass**2)*p(i)/p2
     &               *( fo(3)*pqvp2(1,2) + fo(4)*pqvp2(2,2) )

      enddo
      

      endif

      return
      end
