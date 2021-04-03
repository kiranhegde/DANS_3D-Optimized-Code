 
Module MeanFlowVar
use Gradient
use TransientVar
implicit none


real(8)::R0   !Density of Infinite Flow
real(8)::U0   !Component of Infinite Flow Velocity
real(8)::V0   !Component of Infinite Flow Velocity
real(8)::W0   !Component of Infinite Flow Velocity
real(8)::E0 !Internal Energy of Infinite Flow
real(8)::P0   !Pressure of Infinite Flow
real(8)::MU0  !Molecular Viscosity of Infinite Flow
 
real(8)::C0 !Sound Speed of Infinite Flow
real(8)::T0 !Temperature of Infinite Flow
real(8)::B0 !Sauterland Constant
real(8)::GM   !Gama Constant (Specific Heat Ratio),Co
real(8)::GM1

real(8)::ALF  !Infinite Flow Angle to X Axis
real(8)::Tt !Total Temprature
real(8)::Minf !infinit Flow Mach number
real(8)::Rinf !Reynolds Number of infinite Flow 
real(8)::MR !Much Number over Reynolds Number of infinite Flow
real(8)::PrL !Prantle Number for Laminar Flows  

real(8),dimension(:),pointer::Ro
real(8),dimension(:),pointer::RU
real(8),dimension(:),pointer::RV
real(8),dimension(:),pointer::RW
real(8),dimension(:),pointer::RE

real(8),dimension(:),pointer::RFac
real(8),dimension(:),pointer::UFac
real(8),dimension(:),pointer::VFac
real(8),dimension(:),pointer::WFac
real(8),dimension(:),pointer::EFac

real(8),dimension(:),allocatable::P 
real(8),dimension(:),allocatable::PFac 
 
real(8),dimension(:),allocatable::Mu
real(8),dimension(:),allocatable::MuFac 
 

real(8),dimension(:),pointer::dUdx_F
real(8),dimension(:),pointer::dUdy_F
real(8),dimension(:),pointer::dUdz_F
real(8),dimension(:),pointer::dVdx_F
real(8),dimension(:),pointer::dVdy_F
real(8),dimension(:),pointer::dVdz_F
real(8),dimension(:),pointer::dWdx_F
real(8),dimension(:),pointer::dWdy_F
real(8),dimension(:),pointer::dWdz_F
real(8),dimension(:),pointer::dTdx_F
real(8),dimension(:),pointer::dTdy_F
real(8),dimension(:),pointer::dTdz_F

real(8),dimension(:),pointer::DUX_C
real(8),dimension(:),pointer::DUY_C
real(8),dimension(:),pointer::DUZ_C
real(8),dimension(:),pointer::DVX_C
real(8),dimension(:),pointer::DVY_C
real(8),dimension(:),pointer::DVZ_C
real(8),dimension(:),pointer::DWX_C
real(8),dimension(:),pointer::DWY_C
real(8),dimension(:),pointer::DWZ_C


contains

 
subroutine AllocateMeanFlowVar(NC,NF)
implicit none

integer::NC
integer::NF

allocate( P(1:NC)         )
allocate( PFac(1:NF)      )
 
allocate( Mu(1:NC)        )
allocate( MuFac(1:NF)     )
 

Ro  => ConservativeNP1(:,1)
RU  => ConservativeNP1(:,2)
RV  => ConservativeNP1(:,3)
RW  => ConservativeNP1(:,4)
RE  => ConservativeNP1(:,5)

RFac  => ConservativeF(:,1)
UFac  => ConservativeF(:,2)
VFac  => ConservativeF(:,3)
WFac  => ConservativeF(:,4)
EFac  => ConservativeF(:,5)

dUdx_F    => FaceGrad(:,1)
dUdy_F    => FaceGrad(:,2)
dUdz_F    => FaceGrad(:,3)
dVdx_F    => FaceGrad(:,4)
dVdy_F    => FaceGrad(:,5)
dVdz_F    => FaceGrad(:,6)
dWdx_F    => FaceGrad(:,7)
dWdy_F    => FaceGrad(:,8)
dWdz_F    => FaceGrad(:,9)
dTdx_F    => FaceGrad(:,10)
dTdy_F    => FaceGrad(:,11)
dTdz_F    => FaceGrad(:,12)
 
DUx_C    => CellGrad(:,1)
DUy_C    => CellGrad(:,2)
DUz_C    => CellGrad(:,3)
DVx_C    => CellGrad(:,4)
DVy_C    => CellGrad(:,5)
DVz_C    => CellGrad(:,6)
DWx_C    => CellGrad(:,7)
DWy_C    => CellGrad(:,8)
DWz_C    => CellGrad(:,9)

end subroutine
    
end module
    
  