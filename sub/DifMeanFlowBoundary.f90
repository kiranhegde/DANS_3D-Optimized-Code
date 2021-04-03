!*********************************************************************************************
subroutine DifMeanFlowBoundary(FacInx,NXX,NYY,NZZ,U,V,W,DifMass,DifMom1,DifMom2,DifMom3,DifEnrg)
 use MeshVar
 use MeanFlowVar
 use TurbulenceVar
 use KwSSTVariables
 implicit none
!*********************************************************************************************
real(8),intent(out)::DifMass,DifMom1,DifMom2,DifMom3,DifEnrg
integer                                       ::FacInx
real(8)                                       ::U,V,W,NXX,NYY,NZZ,F2,F3,F4,F5,QX,QY,QZ,K,MumL,MumT,Mum,Uii,Sxx,Sxy,Sxz,Syx,Syy
real(8)                                       ::Syz,Szx,Szy,Szz,Txx,Txy,Txz,Tyx,Tyy,Tyz,Tzx,Tzy,Tzz,Ro1,Ro2,Fluc
!*********************************************************************************************

Mum = MuFac(FacInx)+MutFac(FacInx)

Fluc = KFac(FacInx) / 1.5
 
Uii = (dUdx_F(FacInx)+dVdy_F(FacInx)+dWdz_F(FacInx))/1.5

Sxx = 2*dUdx_F(FacInx)-Uii ; Sxy = dUdy_F(FacInx)+dVdx_F(FacInx) ; Sxz = dUdz_F(FacInx)+dWdx_F(FacInx)
                        Syy = 2*dVdy_F(FacInx)-Uii  ; Syz = dVdz_F(FacInx)+dWdy_F(FacInx)
                                            Szz = 2*dWdz_F(FacInx)-Uii
    
Txx = -Mum * Sxx + Fluc ; Txy = -Mum * Sxy        ; Txz = -Mum * Sxz
Tyx = Txy               ; Tyy = -Mum * Syy + Fluc ; Tyz = -Mum * Syz
Tzx = Txz               ; Tzy = Tyz               ; Tzz = -Mum * Szz + Fluc

!Part 7:	
K  = MuFac(FacInx) / ((GM-1)*PrL) + MutFac(FacInx) / ((GM-1)*PrT)     
Qx =-K*dTdx_F(FacInx)
Qy =-K*dTdy_F(FacInx)
Qz =-K*dTdz_F(FacInx)

!Part 8:
DifMass = 0.0
DifMom1 = MR*( Txx*NXX + Txy*NYY + Txz*Nzz )
DifMom2 = MR*( Tyx*NXX + Tyy*NYY + Tyz*Nzz )
DifMom3 = MR*( Tzx*NXX + Tzy*NYY + Tzz*Nzz )
DifEnrg = MR*( (U*Txx+V*Tyx+W*Tzx+Qx)*Nxx + (U*Txy+V*Tyy+W*Tzy+Qy)*Nyy + (U*Txz+V*Tyz+W*Tzz+Qz)*Nzz )
     
!*********************************************************************************************
 end
!###########################################################################################
