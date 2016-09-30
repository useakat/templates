      subroutine cofl(proc, m,n,cf)
      implicit none
      
      integer n,m,nq,ng,fact,p(2,40),pi(40)
      integer cf(100,1000),i,j,k,nw,ct,ni,no
      character inpart(40),outpart(40),part(40)
      character*3 pnn(40,1000),pn(40)
      character*40 proc,inproc,outproc

CCCCCCCCC input of quark # and gluon # CCCCCCCCCCCCCCCCCCCCC

      inproc = proc(1:index(proc,'>')-1)
      outproc = proc(index(proc,'>')+1:len(proc))
      nw = 0
      ni = 0
      nq = 0
      ng = 0
      ct = 1
      do while (ct.le.40)
         if (inproc(ct:ct).eq.'u') then
            nw = nw +1
            ni = ni +1
            nq = nq +1
            inpart(ni) = 'q'
            part(nw) = 'q'
            ct = ct +1
         elseif (inproc(ct:ct).eq.'g') then
            nw = nw+1
            ni = ni+1
            ng = ng+1
            inpart(ni) = 'g'
            part(nw) = 'g'
            ct = ct +1
         else
            ct = ct +1
         endif
      enddo
 
      no = 0
      ct = 1
      do while (ct.le.40)
         if (outproc(ct:ct).eq.'u') then
            nw= nw +1
            no = no +1
            nq = nq +1
            outpart(no) = 'q'
            part(nw) = 'q'
            ct = ct +1
         elseif (outproc(ct:ct).eq.'g') then
            nw = nw+1
            no = no+1
            ng = ng+1
            outpart(nw) = 'g'
            part(nw) = 'g'
            ct = ct +1
         else
            ct = ct +1
         endif
      enddo

       
      if (( nq.eq.0 ).and.( ng.eq.0 )) then
         write(*,*) 'There is no particle.'
         stop
      elseif ( mod(nq,2).ne.0) then
         write(*,*) 'The number of quarks should be even.'
         stop
      endif   

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
CCCCC calculation of # of color flow and # of particle CCCCC
      
      n = nq + ng*2
      if ( nq.eq. 0) then
        m = fact(ng-1)
      elseif (nq.eq.2) then
         m = fact(n/2)-fact(nq/2)
      else
         write(*,*) 'Sorry, the max number of quark is 2.'
         stop
      endif
      if ( nw.lt.4 ) then
         write(*,*) 'Please enter more than 4 particles.'
         stop
      endif

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
CCCCCCCCCCCCCCCCCCCC setting color flow CCCCCCCCCCCCCCCCCCCCC

       do i = 1, n
        cf(i,1) = i
       enddo
       
      if ( nq.eq.0 ) then   
            do j = 1, ng
               do i = 1, 2
                  p(i,j) = cf(2*j-mod(i,2),1)
               enddo
            enddo
            do i = 1, ng
               pi(i) = i
            enddo
            
            if (ng.eq.4) then
               pn(1) = 'g12'
               pn(2) = 'g23'
               pn(3) = 'g34'
               pn(4) = 'g41'
               do i = 1, m
                  call perm3(pi(2),i)  
                  call permc3(pn(2),i)
                  do j = 1, ng-1
                     cf(2*j-1,i) = p(1,pi(j))
                     cf(2*j,i) = p(2,pi(j+1))
                  enddo               
                  cf(2*ng-1,i) = p(1,pi(ng))
                  cf(2*ng,i) = p(2,pi(1))    
                do j = 1, 4
                   pnn(j,i) = pn(j)
                enddo
                
               enddo
            elseif (ng.eq.5) then
               pn(1) = 'g12'
               pn(2) = 'g23'
               pn(3) = 'g34'
               pn(4) = 'g45'
               pn(5) = 'g51'
                do i = 1, 24
                      call perm4(pi(2),i) 
                      call permc4(pn(2),i)
                      do k = 1, ng-1
                         cf(2*k-1,i) = p(1,pi(k))
                         cf(2*k,i) = p(2,pi(k+1))
                      enddo               
                      cf(2*ng-1,i) = p(1,pi(ng))
                      cf(2*ng,i) = p(2,pi(1))
                      do k =1,5
                         pnn(k,i) = pn(k)
                      enddo
                enddo
               
            elseif (ng.eq.6) then
                do i=1,m
                  call perm5(pi(2),i)               
                  do j=1,ng-1
                     cf(2*j-1,i) = p(1,pi(j))
                     cf(2*j,i) = p(2,pi(j+1))
                  enddo               
                  cf(2*ng-1,i) = p(1,pi(ng))
                  cf(2*ng,i) = p(2,pi(1))  
               enddo
             else 
                write(*,*) 'Sorry, the max number of gluons is 6.'
                stop
             endif
                         
      elseif ( nq.eq.2 ) then
         if (nw .eq. 4) then
c            if (part(1).eq.'q') then
c               pn(1) = 'q1'
c               if (part(2).eq.'g') then
c                  pn(2) = 'g12'
c               else
c                  pn(2) = 
c            elseif (part(2).eq.'q') then
c               pn(2) = 'q1'
c            elseif (part(3).eq.'q') then
c               pn(3) = 'q1'
c            endif
c            if 
            do i = 3, n
               call perm3s(cf(1,2),cf(1,4),cf(1,6),i)
               do j = 1, n
                  cf(j,i-1) = cf(j,1)
               enddo 
            enddo
            do i = 1, n
               cf(i,1) = i
            enddo
          else
             write(*,*) 'error.....'
          endif
      
      else 
         write(*,*) 'Sorry, the number of quarks should be 2.'
      endif
      
**********************************************************************
      return
 50   end

