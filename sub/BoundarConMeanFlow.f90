!*********************************************************************************************
subroutine BoundarConMeanFlow(FacInx,NXX,NYY,NZZ,U,V,W,ConMass,ConMom1,ConMom2,ConMom3,ConEnrg)
use MeshVar
use MeanFlowVar
implicit none
!*********************************************************************************************
integer::FacInx
real(8),intent(in)::NXX,NYY,NZZ,U,V,W
real(8),intent(out)::ConMass,ConMom1,ConMom2,ConMom3,ConEnrg
real(8)::Q,Pm,Rho
!*********************************************************************************************	

!Part 5:
Rho= RFac(FacInx)
	
Q  = U*Nxx+V*Nyy+W*Nzz
Pm = PFac(FacInx)

!Part 7:
ConMass = Q * Rho
ConMom1 = Q * Rho*UFac(FacInx)     + Pm*Nxx
ConMom2 = Q * Rho*VFac(FacInx)     + Pm*Nyy
ConMom3 = Q * Rho*WFac(FacInx)     + Pm*Nzz
ConEnrg = Q *(Rho*EFac(FacInx)+Pm)

!********************************************************************************************* 
    end
!###########################################################################################
