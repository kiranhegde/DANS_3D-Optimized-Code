subroutine BC_FarFieldTwoEqTurb(FacInx)
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
Rigt       = IDS(2,FacInx)
    
U = UFac(FacInx)
V = VFac(FacInx)
W = WFac(FacInx)
    
Q  = U*NX(FacInx)+V*NY(FacInx)+W*NZ(FacInx)
        
if( Q<=0. )then  
 	KFac(FacInx) = 1.0e-6
    OmgFac(FacInx) = 5.0
Else
KFac(FacInx) = RK(Left)/Ro(Left)
OmgFac(FacInx) = ROmg(Left)/Ro(Left)   
end if
    
MutFac(FacInx) = Mut(Left)
    
!*********************************************************************************************
end
!###########################################################################################
