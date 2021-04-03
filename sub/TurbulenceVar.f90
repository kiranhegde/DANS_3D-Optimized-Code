   
Module TurbulenceVar
implicit none

real(8)::PrT
real(8)::Mut0

real(8),dimension(:),allocatable::Mut  
real(8),dimension(:),allocatable::MutFac  

contains

 
subroutine AllocateTurbulenceVar(NC,NF)
implicit none

integer::NC
integer::NF

allocate( Mut(1:NC)    )
allocate( MutFac(1:NF) )

end subroutine
    
end module
    
    
    
