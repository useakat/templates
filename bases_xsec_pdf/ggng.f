C   14/06/90 208061130  MEMBER NAME  EEZZ     (S)        M  FORTRAN
      subroutine ggng (w,  amp2)
      implicit none

      include 'cparam.inc'
      include 'ccolor.inc'
      
      double complex w(6,nwavefuncs)

      double complex amp(ngraphs),jamp(ncolor)

      double precision amp2
      
c      double complex ampsum,amplst(ndiag)
c     double precision amp2sum,amp2

      double complex ztemp

      include 'hmparm.inc'
      include 'smptab.inc'
      include 'coupl.inc'

      double precision zero
      parameter (zero = 0.d0)

      integer iflag
      integer i,j

      do i=1,ngraphs
         amp(i) = (0.d0,0.d0)
      enddo

      call jvvxxx(w(1,3   ),w(1,2   ),g ,zero    ,zero    ,w(1,5   ))      
      call vvvxxx(w(1,4   ),w(1,1   ),w(1,5   ),g ,amp(1   ))              
      call jvvxxx(w(1,1   ),w(1,3   ),g ,zero    ,zero    ,w(1,6   ))      
      call vvvxxx(w(1,4   ),w(1,2   ),w(1,6   ),g ,amp(2   ))              
      call jvvxxx(w(1,1   ),w(1,2   ),g ,zero    ,zero    ,w(1,7   ))      
      call vvvxxx(w(1,4   ),w(1,3   ),w(1,7   ),g ,amp(3   ))              
      call ggggxx(w(1,1   ),w(1,2   ),w(1,3   ),w(1,4   ),g ,amp(4   ))    
      call ggggxx(w(1,3   ),w(1,1   ),w(1,2   ),w(1,4   ),g ,amp(5   ))    
      call ggggxx(w(1,2   ),w(1,3   ),w(1,1   ),w(1,4   ),g ,amp(6   ))    

      jamp(   1) =      2*( +amp(   1)-amp(   2)+amp(   5)-amp(   6))
      jamp(   2) =      2*( -amp(   1)-amp(   3)-amp(   4)+amp(   6))
      jamp(   3) =      2*( -amp(   1)-amp(   3)-amp(   4)+amp(   6))
      jamp(   4) =      2*( +amp(   1)-amp(   2)+amp(   5)-amp(   6))
      jamp(   5) =      2*( +amp(   2)+amp(   3)+amp(   4)-amp(   5))
      jamp(   6) =      2*( +amp(   2)+amp(   3)+amp(   4)-amp(   5))

      amp2 = 0.d0 
      do i = 1, ncolor
          ztemp = (0.d0,0.d0)
          do j = 1, ncolor
              ztemp = ztemp + cf(j,i)*jamp(j)
          enddo
          amp2 =amp2+ztemp*dconjg(jamp(i))/denom(i)   
      enddo
      
      return
      end
