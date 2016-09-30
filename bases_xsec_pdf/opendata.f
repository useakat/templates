      Integer Function NextUnopen()
c********************************************************************
C     Returns an unallocated FORTRAN i/o unit.
c********************************************************************

      Logical EX
C
      Do 10 N = 10, 300
         INQUIRE (UNIT=N, OPENED=EX)
         If (.NOT. EX) then
            NextUnopen = N
            Return
         Endif
 10   Continue
      Stop ' There is no available I/O unit. '
C               *************************
      End



      subroutine OpenData(Tablefile)
c********************************************************************
c generic subroutine to open the table files in the right directories
c********************************************************************
      implicit none
c
      Character Tablefile*40,up*3,dir*8
      data up,dir/'../','Pdfdata/'/
      Integer IU,NextUnopen
      External NextUnopen
      common/IU/IU
c
c--   start
c
      IU=NextUnopen()

c     first try in the current directory

c      print*, 'tried ',Tablefile
      Open(IU, File=Tablefile, Status='OLD', Err=100)
c      print*,'PDF Datafile :',Tablefile,' found'    
      return
      
 100  continue

c     move up one step

c      print*, 'tried ',dir//Tablefile  
      Open(IU, File=dir//Tablefile, Status='OLD', Err=101)
c      print*,'PDF Datafile :',Tablefile,' found'    
      return
      
 101  continue
c     move up one step

c      print*, 'tried ',up//dir//Tablefile  
      Open(IU, File=up//dir//Tablefile, Status='OLD', Err=102)
c      print*,'PDF Datafile :',Tablefile,' found'    
      return
      
 102  continue

c     move up one more step

c      print*, 'tried ',up//up//dir//Tablefile  
      Open(IU, File=up//up//dir//Tablefile, Status='OLD', Err=103)
c      print*,'PDF Datafile :',Tablefile,' found'    
      return
      
 103  continue

      print*,'table for the pdf NOT found!!!'
      
      return
      end

