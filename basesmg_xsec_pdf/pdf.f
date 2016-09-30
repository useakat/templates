      subroutine pftopdg(ih,x,q,pdf)
c***************************************************************************
c     Wrapper for calling the pdf of MCFM
c***************************************************************************
      implicit none
c
c     Arguments
c
      DOUBLE  PRECISION x,q,pdf(-7:7)
      INTEGER IH
C
C     Include
C
      include 'pdf.inc'
C      
      call fdist(ih,x, q, pdf)
      
      return	
      end

      subroutine fdist(ih,x,xmu,fx)
C***********************************************************************
C     MCFM PDF CALLING ROUTINE
C***********************************************************************
      implicit none
      integer ih
      double precision fx(-7:7),x,xmu
      double precision u_val,d_val,u_sea,d_sea,s_sea,c_sea,b_sea,gluon
      double precision Ctq3df,Ctq4Fn,Ctq5Pdf,Ctq6Pdf,Ctq5L
      double precision q2max
      double precision epa_electron,epa_proton
      include 'pdf.inc'

      integer mode,Iprtn,Irt

      do Iprtn=-7,7
         fx(Iprtn)=0d0
      enddo

C---  set to zero if x out of range
      if (x .ge. 1d0) then
          return
      endif
 	  	 
C         
      fx(-5)=Ctq6Pdf(-5,x,xmu)
      fx(-4)=Ctq6Pdf(-4,x,xmu)
      fx(-3)=Ctq6Pdf(-3,x,xmu)
      
      fx(0)=Ctq6Pdf(0,x,xmu)
      
      fx(+3)=Ctq6Pdf(+3,x,xmu)
      fx(+4)=Ctq6Pdf(+4,x,xmu)
      fx(+5)=Ctq6Pdf(+5,x,xmu)
      
      fx(1)=Ctq6Pdf(+2,x,xmu)
      fx(2)=Ctq6Pdf(+1,x,xmu)
      fx(-1)=Ctq6Pdf(-2,x,xmu)
      fx(-2)=Ctq6Pdf(-1,x,xmu)
      
      return
      end
