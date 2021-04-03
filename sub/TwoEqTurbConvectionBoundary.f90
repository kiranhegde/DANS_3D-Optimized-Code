
!*********************************************************************************************
 subroutine TwoEqTurbConvectionBoundary(FacInx,NXX,NYY,NZZ,U,V,W,FirstEqConv,ScondEqConv)
 use MeshVar
 use MeanFlowVar
 use TurbulenceVar
 use KwSSTVariables
 use LocalVar
 implicit none
!**********************************************************************************************

integer::FacInx,Left,Rigt,P1,P2
real(8)::NXX,NYY,NZZ,U,V,W,DX,DY,F1,F2,Q,R,RFi,Rho,FirstEqConv,ScondEqConv
!**********************************************************************************************	

Rho= RFac(FacInx)

Q  = U*NXX + V*NYY + W*NZZ

FirstEqConv = Q*Rho*KFac(FacInx)
ScondEqConv = Q*Rho*OmgFac(FacInx)
    
!*********************************************************************************************
 end
!###########################################################################################
