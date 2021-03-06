      subroutine fvirxx(ri,vc,gc,fmass,fwidth , fvir)
c
c This subroutine computes an off-shell fermion wavefunction from a
c flowing-IN external fermion and a vector boson.
c
c input:
c       complex ri(18)         : flow-in  Rarita-Schwinger-fermion   |ri>
c       complex vc(6)          : input    vector                      v
c       complex gc(2)          : coupling constants                  gvf
c       real    fmass          : mass  of output fermion f'
c       real    fwidth         : width of output fermion f'
c
c output:
c       complex fvir(6)        : off-shell fermion             |f',v,ri>
c
c- by Y.Takaesu - 2009/11/04
c
      implicit none
      double complex ri(18),vc(6),gc(2),fvir(6),d
      double precision fmass,fwidth
      
      double precision rZero, rOne
      parameter( rZero = 0.0d0, rOne = 1.0d0 )
      double complex cImag, cZero
      parameter( cImag = ( 0.0d0, 1.0d0 ), cZero = ( 0.0d0, 0.0d0 ) )

      integer i
      double precision pf(0:3),pf2,sabsri,pv(0:3)
      double complex svcl(2,2),svcr(2,2),spvl(2,2),spvr(2,2),rc(0:3,4)
      double complex eprc(0:3,4),geprc(4),sssl(2,2),sssr(2,2)
      double complex spfl(2,2),spfr(2,2)

#ifdef HELAS_CHECK
      integer stdo
      parameter( stdo = 6 )
#endif
c
#ifdef HELAS_CHECK
      sabsri=0
      do i=1,16
       sabsri=abs(ri(i))+sabsri
      enddo
      if ( sabsri.eq.rZero ) then
         write(stdo,*)
     & ' helas-warn  : ri in fvirxx is zero vector-spinor'
      endif
      if ( abs(ri(17))+abs(ri(18)).eq.rZero ) then
         write(stdo,*)
     &        ' helas-error : ri in fvirxx has zero momentum'
      endif
      if ( abs(vc(1))+abs(vc(2))+abs(vc(3))+abs(vc(4)).eq.rZero ) then
         write(stdo,*) ' helas-warn  : vc in fvirxx is zero vector'
      endif
      if ( abs(vc(5))+abs(vc(6)).eq.rZero ) then
         write(stdo,*)
     &        ' helas-error : vc in fvirxx has zero momentum'
      endif
      if ( gc(1).eq.cZero .and. gc(2).eq.cZero ) then
         write(stdo,*)
     &        ' helas-error : gc in fvirxx is zero coupling'
      endif
      if ( fmass.lt.rZero ) then
         write(stdo,*) ' helas-error : fmass in fvirxx is negative'
         write(stdo,*) '             : fmass = ',fmass
      endif
      if ( fwidth.lt.rZero ) then
         write(stdo,*) ' helas-error : fwidth in fvirxx is negative'
         write(stdo,*) '             : fwidth = ',fwidth
      endif
#endif

      fvir(5) = -ri(17)+vc(5)
      fvir(6) = -ri(18)+vc(6)

      pf(0) = dble( fvir(5))
      pf(1) = dble( fvir(6))
      pf(2) = dimag(fvir(6))
      pf(3) = dimag(fvir(5))
      pf2 = pf(0)**2-(pf(1)**2+pf(2)**2+pf(3)**2)

#ifdef HELAS_CHECK
      if ( abs(fvir(5))+abs(fvir(6)).eq.rZero ) then
         write(stdo,*)
     &        ' helas-error : fvir in fvirxx has zero momentum'
      endif
      if ( fwidth.eq.rZero .and. pf2.eq.fmass**2 ) then
         write(stdo,*)
     &        ' helas-error : fvir in fvirxx is on fmass pole'
         write(stdo,*)
     &        '             : p     = ',pf(0),pf(1),pf(2),pf(3)
         write(stdo,*)
     &        '             : abs(p)= ',dsqrt(dabs(pf2))
         fvir(1) = cZero
         fvir(2) = cZero
         fvir(3) = cZero
         fvir(4) = cZero
         return
      endif
#endif

      pv(0) = dble( vc(5))
      pv(1) = dble( vc(6))
      pv(2) = dimag(vc(6))
      pv(3) = dimag(vc(5))

      rc(0,1) = ri(1)
      rc(0,2) = ri(2)
      rc(0,3) = ri(3)
      rc(0,4) = ri(4)
      rc(1,1) = ri(5)
      rc(1,2) = ri(6)
      rc(1,3) = ri(7)
      rc(1,4) = ri(8)
      rc(2,1) = ri(9)
      rc(2,2) = ri(10)
      rc(2,3) = ri(11)
      rc(2,4) = ri(12)
      rc(3,1) = ri(13)
      rc(3,2) = ri(14)
      rc(3,3) = ri(15)
      rc(3,4) = ri(16)

      fvir(1) = cZero
      fvir(2) = cZero
      fvir(3) = cZero
      fvir(4) = cZero

      svcl(1,1) = vc(1)+vc(4)
      svcl(1,2) = vc(2)-cImag*vc(3)
      svcl(2,1) = vc(2)+cImag*vc(3)
      svcl(2,2) = vc(1)-vc(4)

      svcr(1,1) = vc(1)-vc(4)
      svcr(1,2) = -(vc(2)-cImag*vc(3))
      svcr(2,1) = -(vc(2)+cImag*vc(3))
      svcr(2,2) = vc(1)+vc(4)
        
      spvl(1,1) = pv(0)-pv(3)
      spvl(1,2) = -(pv(1)-cImag*pv(2))
      spvl(2,1) = -(pv(1)+cImag*pv(2))
      spvl(2,2) = pv(0)+pv(3)

      spvr(1,1) = pv(0)+pv(3)
      spvr(1,2) = pv(1)-cImag*pv(2)
      spvr(2,1) = pv(1)+cImag*pv(2)
      spvr(2,2) = pv(0)-pv(3)

      spfl(1,1) = pf(0)+pf(3)
      spfl(1,2) = pf(1)-cImag*pf(2)
      spfl(2,1) = pf(1)+cImag*pf(2)
      spfl(2,2) = pf(0)-pf(3)

      spfr(1,1) = pf(0)-pf(3)
      spfr(1,2) = -(pf(1)-cImag*pf(2))
      spfr(2,1) = -(pf(1)+cImag*pf(2))
      spfr(2,2) = pf(0)+pf(3)

      d = 1d0/dcmplx(pf2-fmass**2,fmass*fwidth)

      if ( gc(2).ne.cZero ) then
        
       sssl(1,1) = svcl(1,1)*spvl(1,1)+svcl(1,2)*spvl(2,1)
       sssl(1,2) = svcl(1,1)*spvl(1,2)+svcl(1,2)*spvl(2,2) 
       sssl(2,1) = svcl(2,1)*spvl(1,1)+svcl(2,2)*spvl(2,1) 
       sssl(2,2) = svcl(2,1)*spvl(1,2)+svcl(2,2)*spvl(2,2) 
        
       do i=0,3
        eprc(i,1) = cZero
        eprc(i,2) = cZero
        eprc(i,3) = sssl(1,1)*rc(i,3)+sssl(1,2)*rc(i,4)
        eprc(i,4) = sssl(2,1)*rc(i,3)+sssl(2,2)*rc(i,4)
       enddo

       geprc(1) = eprc(0,3)-eprc(1,4)+cImag*eprc(2,4)-eprc(3,3)
       geprc(2) = eprc(0,4)-eprc(1,3)-cImag*eprc(2,3)+eprc(3,4)
       geprc(3) = cZero
       geprc(4) = cZero

       fvir(1) = fvir(1)-d*dconjg(gc(2))*fmass*geprc(1)
       fvir(2) = fvir(2)-d*dconjg(gc(2))*fmass*geprc(2)
       fvir(3) = fvir(3)-d*dconjg(gc(2))*(spfl(1,1)*geprc(1)
     &           +spfl(1,2)*geprc(2))
       fvir(4) = fvir(4)-d*dconjg(gc(2))*(spfl(2,1)*geprc(1)
     &           +spfl(2,2)*geprc(2))

       sssl(1,1) = spvr(1,1)*svcr(1,1)+spvr(1,2)*svcr(2,1)
       sssl(1,2) = spvr(1,1)*svcr(1,2)+spvr(1,2)*svcr(2,2) 
       sssl(2,1) = spvr(2,1)*svcr(1,1)+spvr(2,2)*svcr(2,1) 
       sssl(2,2) = spvr(2,1)*svcr(1,2)+spvr(2,2)*svcr(2,2)

       do i=0,3
        eprc(i,1) = cZero
        eprc(i,2) = cZero
        eprc(i,3) = sssl(1,1)*rc(i,3)+sssl(1,2)*rc(i,4)
        eprc(i,4) = sssl(2,1)*rc(i,3)+sssl(2,2)*rc(i,4)
       enddo

       geprc(1) = eprc(0,3)-eprc(1,4)+cImag*eprc(2,4)-eprc(3,3)
       geprc(2) = eprc(0,4)-eprc(1,3)-cImag*eprc(2,3)+eprc(3,4)
       geprc(3) = cZero
       geprc(4) = cZero  

       fvir(1) = fvir(1)+d*dconjg(gc(2))*fmass*geprc(1)
       fvir(2) = fvir(2)+d*dconjg(gc(2))*fmass*geprc(2)
       fvir(3) = fvir(3)+d*dconjg(gc(2))*(spfl(1,1)*geprc(1)
     &          +spfl(1,2)*geprc(2))
       fvir(4) = fvir(4)+d*dconjg(gc(2))*(spfl(2,1)*geprc(1)
     &          +spfl(2,2)*geprc(2))
     
      endif

      if ( gc(1).ne.cZero ) then

       sssr(1,1) = svcr(1,1)*spvr(1,1)+svcr(1,2)*spvr(2,1)
       sssr(1,2) = svcr(1,1)*spvr(1,2)+svcr(1,2)*spvr(2,2) 
       sssr(2,1) = svcr(2,1)*spvr(1,1)+svcr(2,2)*spvr(2,1) 
       sssr(2,2) = svcr(2,1)*spvr(1,2)+svcr(2,2)*spvr(2,2)

       do i=0,3
        eprc(i,1) = sssr(1,1)*rc(i,1)+sssr(1,2)*rc(i,2)
        eprc(i,2) = sssr(2,1)*rc(i,1)+sssr(2,2)*rc(i,2)
        eprc(i,3) = cZero
        eprc(i,4) = cZero
       enddo

       geprc(1) = cZero
       geprc(2) = cZero
       geprc(3) = eprc(0,1)+eprc(1,2)-cImag*eprc(2,2)+eprc(3,1)
       geprc(4) = eprc(0,2)+eprc(1,1)+cImag*eprc(2,1)-eprc(3,2)  

       fvir(1) = fvir(1)-d*dconjg(gc(1))*(spfr(1,1)*geprc(3)
     &          +spfr(1,2) *geprc(4))
       fvir(2) = fvir(2)-d*dconjg(gc(1))*(spfr(2,1)*geprc(3)
     &           +spfr(2,2)*geprc(4))
       fvir(3) = fvir(3)-d*dconjg(gc(1))*fmass*geprc(3)
       fvir(4) = fvir(4)-d*dconjg(gc(1))*fmass*geprc(4)

       sssr(1,1) = spvl(1,1)*svcl(1,1)+spvl(1,2)*svcl(2,1)
       sssr(1,2) = spvl(1,1)*svcl(1,2)+spvl(1,2)*svcl(2,2) 
       sssr(2,1) = spvl(2,1)*svcl(1,1)+spvl(2,2)*svcl(2,1) 
       sssr(2,2) = spvl(2,1)*svcl(1,2)+spvl(2,2)*svcl(2,2)

       do i=0,3
        eprc(i,1) = sssr(1,1)*rc(i,1)+sssr(1,2)*rc(i,2)
        eprc(i,2) = sssr(2,1)*rc(i,1)+sssr(2,2)*rc(i,2)
        eprc(i,3) = cZero
        eprc(i,4) = cZero
       enddo

       geprc(1) = cZero
       geprc(2) = cZero
       geprc(3) = eprc(0,1)+eprc(1,2)-cImag*eprc(2,2)+eprc(3,1)
       geprc(4) = eprc(0,2)+eprc(1,1)+cImag*eprc(2,1)-eprc(3,2)  

       fvir(1) = fvir(1)+d*dconjg(gc(1))*(spfr(1,1)*geprc(3)
     &           +spfr(1,2)*geprc(4))
       fvir(2) = fvir(2)+d*dconjg(gc(1))*(spfr(2,1)*geprc(3)
     &           +spfr(2,2)*geprc(4))
       fvir(3) = fvir(3)+d*dconjg(gc(1))*fmass*geprc(3)
       fvir(4) = fvir(4)+d*dconjg(gc(1))*fmass*geprc(4)

      endif
     
      return
      end
