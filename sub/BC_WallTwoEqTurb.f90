subroutine BC_WallTwoEqTurb(FacInx)
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
KFac(FacInx) = 0.0
OmgFac(FacInx) = MR * (60.0*Mu(Left)/ (Ro(Left)*0.075*DisWall(Left)*DisWall(Left)) ) 
    
MutFac(FacInx) = 0.0
    
!*********************************************************************************************
end
!###########################################################################################

