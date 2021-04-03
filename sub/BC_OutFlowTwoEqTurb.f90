subroutine BC_OutFlowTwoEqTurb(FacInx)
use MeshVar
use MeanFlowVar
use TurbulenceVar
use KwSSTVariables
implicit none
!*********************************************************************************************
integer::FacInx,Left,Rigt
real(8)::U,V,W,Q
!*********************************************************************************************

Left       = IDS(1,FacInx)
KFac(FacInx) = RK(Left)/Ro(Left)
OmgFac(FacInx) = ROmg(Left)/Ro(Left)
    
MutFac(FacInx) = Mut(Left)
    
!*********************************************************************************************
end
!###########################################################################################

