
Module VirtualMesh
use MeshVar
implicit none

integer                                   ::NFv
integer                                   ::NCv
integer                                   ::NPv
integer,allocatable,dimension(:,:)        ::IDSv
integer,allocatable,dimension(:)          ::FaceTypev
real(8),allocatable,dimension(:)          ::NXv
real(8),allocatable,dimension(:)          ::NYv
real(8),allocatable,dimension(:)          ::NZv
real(8),allocatable,dimension(:)          ::Volv
integer,allocatable,dimension(:)          ::nFaceCellv
integer,allocatable,dimension(:,:)        ::iFaceCellv
integer,allocatable,dimension(:,:)        ::DirFaceCellv

real(8),allocatable,dimension(:,:)        ::LocalFacValuv
contains
   
   

   
!SSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
!*********************************************************************************************
 subroutine AllocatVirtualMesh(NFv) 
 implicit none
 integer,value::NFv
!********************************************************************************************* 

NCv = NF
NPv = NP + NC

allocate( IDSv(1:6,1:NFv)          )
allocate( FaceTypev(1:NFv)         )
allocate( NXv(1:NFv)               )
allocate( NYv(1:NFv)               )
allocate( NZv(1:NFv)               )
allocate( Volv(1:NCv)              )
allocate( nFaceCellv(1:NCv)        ) 
allocate( iFaceCellv(1:NCv,1:8)    ) 
allocate( DirFaceCellv(1:NCv,1:8)  )

allocate( LocalFacValuv(1:NFv,1:18)          )
!*********************************************************************************************
 end
!###########################################################################################
    
    

    end module
    
   
   
  

   
         
    
    
        


  
     
 

     
    
      

 
        
    

    
   
    