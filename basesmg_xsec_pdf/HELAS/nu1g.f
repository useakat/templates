CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
CCCCCCCCCCCCCCCCC computation of the # of U(1) gluons CCCCCCCCCCCC

      subroutine nu1g(n,cf,nu1)
      implicit none
      integer n,cf(100),nu1,j
      
         nu1 = 0
         do j = 3, n-1, 2
         if ( cf(j+1).eq.(cf(j)+1) ) then
               nu1 = nu1 +1
         endif
         enddo
         

      return 
      end
