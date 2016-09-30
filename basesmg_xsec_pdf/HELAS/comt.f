CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
CCCCCCCCCCCCC computation of the color matrix elements CCCCCCCCCCC
      
      subroutine comt(n,cf1,cf2, com)
      implicit none
      integer n,i,cf1(100),cf2(100),com,f(100),a,b,nn

          do i = 1,n
           f(i) = 0
          enddo
          a = cf1(1)
          b = cf1(2)
          nn = 0
 10      do i=1,n-1,2
          if( b .eq. cf2(i) ) then
             b = cf2(i+1) 
             goto 15
          elseif ( b .eq. cf2(i+1) ) then
             b = cf2(i)
             goto 15
          endif
         enddo
         write(*,*) 'ERROR'
         stop

 15       if ( a .eq. b ) then 
             nn = nn + 1
             do i=3,n-1,2
                if ( f(i) .eq. 0) then
                   a = cf1(i)
                   b = cf1(i+1)
                   f(i) = 1
                   f(i+1) = 1
                   goto 10
                endif
             enddo
             goto 20
           write(*,*) 'ERROR2' 
           stop
         
          else
             do i = 1,n-1,2
                if ((f(i) .eq. 0) .and. ( b .eq. cf1(i) )) then
                   b = cf1(i+1)
                   f(i) = 1
                   f(i+1) = 1
                   goto 10
                elseif ((f(i+1) .eq. 0) .and. ( b .eq. cf1(i+1) )) then
                   b = cf1(i)
                   f(i) = 1
                   f(i+1) = 1
                   goto 10
                endif
             enddo
             write(*,*) 'ERROR3'
             stop
          endif
         
 20       com = 3**nn
      
       return
       end
