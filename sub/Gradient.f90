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


allocate( PhiFace(1:6,1:NF) )

allocate( PhiCell(1:6,1:NC) )

allocate( CellGrad(1:18,1:NC) )
allocate( FaceGrad(1:18,1:NF) )

end subroutine


end module