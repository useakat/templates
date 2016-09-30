      implicit none
*
* f1 f1bar -> f2 f2bar
*
      complex*16 fi(6),fo(6),fif(6),fof(6),jio(6),amp,gal(2)
      real*8 p1(0:3),p2(0:3),p3(0:3),p4(0:3),f2mass,f1mass
      real*8 pi,roots,alpha,MM
      integer nhel1,nhel2,nhel3,nhel4,i
      
      integer n,wt,lw
      parameter (n=2)
      real*8 px(4,n),mx(n)
      

      pi=dacos(-1.0d0)
      alpha=1.0d0/137.0d0

      gal(1) = 1
      gal(2) = gal(1)

      roots = 500.0d0
      f1mass = 0
      f2mass=0
      mx(1)=f2mass
      mx(2)=f2mass

      call mom2cx(roots,f1mass,f1mass,1.0d0,0.0d0, p1,p2)
      call rambo(n,roots,mx,px,wt,lw)

      p3(0) = px(4,1)
      p3(1) = px(3,1)
      p3(2) = px(2,1)
      p3(3) = px(1,1)
	p4(0) = px(4,2)
      p4(1) = px(3,2)
      p4(2) = px(2,2)
      p4(3) = px(1,2)

      MM = 0.0d0

      do nhel1=-1,1,2
       do nhel2= -1,1,2  
        do nhel3= -1,1,2
         do nhel4= -1, 1,2

           CALL IXXXXX(P1,f1mass,NHEL1, 1 , fi )
           CALL OXXXXX(P2,f1mass,NHEL2,-1 , fo )
           CALL OXXXXX(P3,f2mass,NHEL3, 1 , fof)
           CALL IXXXXX(P4,f2mass,NHEL4,-1 , fif)
           CALL JIOXXX(fi,fo,GAL,0.0d0,0.0d0 , jio)
           CALL IOVXXX(fif,fof,jio,GAL , AMP)

           MM = MM + amp*dconjg(amp)

         enddo
        enddo 
       enddo
      enddo

      write(*,*) MM
 
      end
