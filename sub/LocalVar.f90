Module LocalVar

real(8),dimension(:,:),allocatable,target::FacVal
     
real(8),dimension(:,:),allocatable::FacVal2D  
 
real(8),dimension(:,:),allocatable::LocalFacValu

real(8),dimension(:,:),allocatable,target::Array2D

contains
    
    
subroutine AllocateLocalVar(NF,NC)
implicit none


integer::NC
integer::NF

allocate( FacVal(1:NF,1:7)    ) 
allocate( FacVal2D(1:NF,1:2)  ) 
allocate (Array2D(1:NC,1:2)   )
allocate( LocalFacValu(1:NF,1:18) )


end subroutine


end module

