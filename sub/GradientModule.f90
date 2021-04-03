module Gradient 
implicit none
    
real(8),dimension(:,:),allocatable,target::CellGrad
real(8),dimension(:,:),allocatable,target::FaceGrad

real(8),dimension(:,:),allocatable::PhiFace
real(8),dimension(:,:),allocatable::PhiCell

contains
    
subroutine AllocateGradient(nPhiGradient,NC,NF)
implicit none

integer::NC
integer::NF
integer::nPhiGradient


allocate( PhiFace(1:NF,1:6) )

allocate( PhiCell(1:NC,1:6) )

allocate( CellGrad(1:NC,1:18) )
allocate( FaceGrad(1:NF,1:18) )

end subroutine


end module