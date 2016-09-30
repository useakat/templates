      integer function fact(n)
      implicit none
      integer n,i      
      fact = 1
      do i = 1, n
         fact = fact*i
      enddo
      return
      end
