!*********************************************************************************************
 subroutine KwSSTdiffBoundary(FacInx,NXX,NYY,NZZ,U,V,W,FirstEqDiff,SecndEqDiff)
 use MeshVar
 use MeanFlowVar
 use TurbulenceVar
 use KwSSTVariables
 use LocalVar
 implicit none
!*********************************************************************************************
integer::FacInx,Left,Rigt , j
real(8)::NXX,NYY,NZZ,U,V,W,Mu_k,Mu_w,SigkMean,SigwMean,FirstEqDiff,SecndEqDiff
!*********************************************************************************************

Left = IDS(1,FacInx)

SigkMean = Sigk(Left)
SigwMean = Sigw(Left)
    
!Part 4:
Mu_k = MuFac(FacInx) + MutFac(FacInx)*SigkMean
Mu_w = MuFac(FacInx) + MutFac(FacInx)*SigwMean
    
FirstEqDiff = -MR*Mu_k * ( DKX_F(FacInx)   *NXX + DKY_F(FacInx)   *NYY + DKZ_F(FacInx)   *NZZ ) 
SecndEqDiff = -MR*Mu_w * ( DOmegX_F(FacInx)*NXX + DOmegY_F(FacInx)*NYY + DOmegZ_F(FacInx)*NZZ ) 
 
!*********************************************************************************************
 end
!###########################################################################################
