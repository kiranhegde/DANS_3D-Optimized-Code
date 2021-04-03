!*********************************************************************************************
 subroutine TwoEqTurbConvectionInterior(FacInx,NXX,NYY,NZZ,U,V,W,FirstEqConv,ScondEqConv)
 use MeshVar
 use MeanFlowVar
 use TurbulenceVar
 use KwSSTVariables
 use LocalVar
 implicit none
!**********************************************************************************************

integer::FacInx,Left,Rigt,P1,P2
real(8)::NXX,NYY,NZZ,U,V,W,DX,DY,F1,F2,Q,RoK,RoOmg,FirstEqConv,ScondEqConv
!**********************************************************************************************	

!Part 9:
Left = IDS(1,FacInx)
Rigt = IDS(2,FacInx)

!Part 11:
Q  = U*NXX + V*NYY + W*NZZ
    
!Part 12:
if( Q>=0.0 )then
RoK   = RK(Left)
RoOmg = ROmg(Left)
else
RoK   = RK(Rigt)
RoOmg = ROmg(Rigt)
end if
    
FirstEqConv = Q * RoK
ScondEqConv = Q * RoOmg

!*********************************************************************************************
    end
!###########################################################################################

    
    