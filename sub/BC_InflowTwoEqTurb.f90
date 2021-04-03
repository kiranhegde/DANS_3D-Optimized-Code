subroutine BC_InflowTwoEqTurb(FacInx)
use MeshVar
use KwSSTVariables
use MeanFlowVar
use TurbulenceVar
implicit none
!*********************************************************************************************
integer::FacInx,Left,Rigt
real(8)::U,V,W,Q
!*********************************************************************************************

Left       = IDS(1,FacInx)
KFac(FacInx) = 1.0e-6
OmgFac(FacInx) = 5.0 
    
MutFac(FacInx) = Mut(Left)
    
!*********************************************************************************************
end
!###########################################################################################
