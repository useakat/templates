      subroutine perm2(h)
      implicit none
      integer i,h(2),th
      th = h(1)
      h(1) = h(2)
      h(2) = th
      return
      end

      subroutine perm2s(h1,h2)
      implicit none
      integer i,h1,h2,th
      th = h1
      h1 = h2
      h2 = th
      return
      end

      subroutine permc2(h)
      implicit none
      character*3 h(2),th
      th = h(1)
      h(1) = h(2)
      h(2) = th
      return
      end

      subroutine permo2(p)
      implicit none
      integer i
      real*8 p(0:3,2),tp(0:3)
      do i = 0,3
         tp(i) = p(i,1)
         p(i,1) = p(i,2)
         p(i,2) = tp(i)
      enddo
      return
      end

      subroutine permw2(w)
      implicit none
      integer i
      complex*16 w(18,2),tw(18)
      do i = 1,18
         tw(i) = w(i,1)
         w(i,1) = w(i,2)
         w(i,2) = tw(i)
      enddo
      return
      end

      subroutine perm3(h,j)
      implicit none
      integer j,h(3)
      if (mod(j,2).eq.0) then
         call perm2(h(2))        
      elseif (j.ne.1) then
         call perm2(h(2))
         call perm2(h)
         call perm2(h(2))
      endif
      return
      end

      subroutine perm3s(h1,h2,h3,j)
      implicit none
      integer j,h1,h2,h3
      if (mod(j,2).eq.0) then
         call perm2s(h2,h3)        
      elseif (j.ne.1) then
         call perm2s(h2,h3)
         call perm2s(h1,h2)
         call perm2s(h2,h3)
      endif
      return
      end

      subroutine permc3(h,j)
      implicit none
      integer j
      character*3 h(3)
      if (mod(j,2).eq.0) then
         call permc2(h(2))        
      elseif (j.ne.1) then
         call permc2(h(2))
         call permc2(h)
         call permc2(h(2))
      endif
      return
      end
     
      subroutine permo3(p,j)
      implicit none
      integer j
      real*8 p(0:3,3)
      if (mod(j,2).eq.0) then
         call permo2(p(0,2))        
      elseif (j.ne.1) then
         call permo2(p(0,2))
         call permo2(p)
         call permo2(p(0,2))
      endif
      return
      end

      subroutine permw3(w,j)
      implicit none
      integer j
      complex*16 w(18,3)
      if (mod(j,2).eq.0) then
         call permw2(w(1,2))        
      elseif (j.ne.1) then
         call permw2(w(1,2))
         call permw2(w)
         call permw2(w(1,2))
      endif
      return
      end

      subroutine perm4(h,i)
      implicit none
      integer i,h(4)
      if (mod(i,6).ne.1) then
         call perm3(h(2),mod(i,6))
      elseif ((i.ne.19).and.(i.ne.1)) then
         call perm2(h)
      elseif (i.eq.19) then
         call perm3(h,3)
      endif
      return
      end

      subroutine permc4(h,i)
      implicit none
      integer i
      character*3 h(4)
      if (mod(i,6).ne.1) then
         call permc3(h(2),mod(i,6))
      elseif ((i.ne.19).and.(i.ne.1)) then
         call permc2(h)
      elseif (i.eq.19) then
         call permc3(h,3)
      endif
      return
      end

      subroutine permo4(p,i)
      implicit none
      integer i
      real*8 p(0:3,4)
      if (mod(i,6).ne.1) then
         call permo3(p(0,2),mod(i,6))
      elseif ((i.ne.19).and.(i.ne.1)) then
         call permo2(p)
      elseif (i.eq.19) then
         call permo3(p,3)
      endif
      return
      end

      subroutine permw4(w,i)
      implicit none
      integer i
      complex*16 w(18,4)
      if (mod(i,6).ne.1) then
         call permw3(w(1,2),mod(i,6))
      elseif ((i.ne.19).and.(i.ne.1)) then
         call permw2(w)
      elseif (i.eq.19) then
         call permw3(w,3)
      endif
      return
      end

      subroutine perm5(h,i)
      implicit none
      integer i,h(5)
      if (mod(i,24).ne.1) then
         call perm4(h(2),mod(i,24))
      elseif (i.ne.1) then
         call perm2(h)
      endif
      return
      end

      subroutine permc5(h,i)
      implicit none
      integer i
      character*3 h(5)
      if (mod(i,24).ne.1) then
         call permc4(h(2),mod(i,24))
      elseif ((mod(i,24).eq.1).and.(i.ne.1)) then
         call permc2(h)
      endif
      return
      end

      subroutine permo5(p,i)
      implicit none
      integer i
      real*8 p(0:3,5)
      if (mod(i,24).ne.1) then
         call permo4(p(0,2),mod(i,24))
      elseif ((mod(i,24).eq.1).and.(i.ne.1)) then
         call permo2(p)
      endif
      return
      end

      subroutine permw5(w,i)
      implicit none
      integer i
      complex*16 w(18,5)
      if (mod(i,24).ne.1) then
         call permw4(w(1,2),mod(i,24))
      elseif ((mod(i,24).eq.1).and.(i.ne.1)) then
         call permw2(w)
      endif
      return
      end

      subroutine perm20(p,h,c)
      implicit none
      double precision p(0:3,2)
      integer i,h(2),c(2)
      call permo2(p)
      call perm2(h)
      call perm2(c)
      return
      end

      subroutine perm21(p,h,c)
      implicit none
      double precision p(0:3,2)
      integer i,h(2),c(2)
      call perm20(p,h,c)
      c(1) = -c(1)
      c(2) = -c(2)
      end

      subroutine perm30(p,h,c,j)
      implicit none
      double precision p(0:3,3)
      integer j,h(3),c(3)
      call permo3(p,j)
      call perm3(h,j)
      call perm3(c,j)
      return
      end
      
      subroutine perm31(p,h,c,j)
      implicit none
      double precision p(0:3,3)
      integer j,h(3),c(3)
      call perm30(p,h,c,j)
      if ((j.eq.3).or.(j.eq.5)) then
         c(1) = -c(1)
         c(3) = -c(3)
      endif
      return
      end
      
      subroutine perm32(p,h,c,j)
      implicit none
      double precision p(0:3,3)
      integer j,h(3),c(3)
      call perm30(p,h,c,j)
      if ((j .eq. 2) .or. (j .eq. 4) .or. (j .eq. 6)) then
         c(2) = -c(2)
         c(3) = -c(3)
      elseif ((j .eq. 3) .or. (j .eq. 5)) then
         c(1) = -c(1)
         c(3) = -c(3)
      endif
      end

      subroutine perm40(p,h,c,i)
      implicit none
      double precision p(0:3,4)
      integer i,h(4),c(4)
      call permo4(p,i)
      call perm4(h,i)
      call perm4(c,i)
      end

      subroutine perm41(p,h,c,i)
      implicit none
      double precision p(0:3,4)
      integer i,h(4),c(4)
      call perm40(p,h,c,i)
      if ((i.eq.7).or.(i.eq.13)) then
         c(1) = -c(1)
         c(2) = -c(2)
      elseif (i.eq.19) then
         c(1) = -c(1)
         c(3) = -c(3)
      endif
      end

      subroutine perm50(p,h,c,i)
      implicit none
      double precision p(0:3,5)
      integer i,h(5),c(5)
      call permo5(p,i)
      call perm5(h,i)
      call perm5(c,i)
      end

      subroutine perm51(p,h,c,i)
      implicit none
      double precision p(0:3,5)
      integer i,h(5),c(5)
      call perm50(p,h,c,i)
      if ((mod(i,24).eq.1).and.(i.ne.1)) then
         c(1) = -c(1)
         c(2) = -c(2)
      endif
      end
