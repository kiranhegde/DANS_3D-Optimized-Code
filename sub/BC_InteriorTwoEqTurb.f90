subroutine BC_InteriorTwoEqTurb(FacInx)
use MeshVar
use MeanFlowVar
use TurbulenceVar
use KwSSTVariables
implicit none
!*********************************************************************************************
integer::FacInx,Left,Rigt
real(8)::U,V,W,Q
!*********************************************************************************************

Left = IDS(1,FacInx)
Rigt = IDS(2,FacInx)
    
KFac(FacInx)   = 0.5*( RK(Left)/Ro(Left)   + RK(Rigt)/Ro(Rigt)   )
OmgFac(FacInx) = 0.5*( ROmg(Left)/Ro(Left) + ROmg(Rigt)/Ro(Rigt) )
    
MutFac(FacInx) = 0.5*( Mut(Left) + Mut(Rigt) )
    
  
!*********************************************************************************************
end
!###########################################################################################
