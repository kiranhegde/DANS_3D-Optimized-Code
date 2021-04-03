module KwSSTVariables
use gradient
use TransientVar
implicit none
    

real(8)::Sigk1,Sigk2,Sigw1,Sigw2,Beta1,Beta2,Gama1,Gama2,Bstar

real(8),dimension(:),pointer::RK
real(8),dimension(:),pointer::ROmg

real(8),dimension(:),pointer::KFac
real(8),dimension(:),pointer::OmgFac

real(8),dimension(:),allocatable::F11
real(8),dimension(:),allocatable::F22
real(8),dimension(:),allocatable::Sigk
real(8),dimension(:),allocatable::Sigw
real(8),dimension(:),allocatable::Beta
real(8),dimension(:),allocatable::Gama


real(8),dimension(:),pointer::DKX_F
real(8),dimension(:),pointer::DKY_F
real(8),dimension(:),pointer::DKZ_F
real(8),dimension(:),pointer::DOmegX_F
real(8),dimension(:),pointer::DOmegY_F
real(8),dimension(:),pointer::DOmegZ_F

real(8),dimension(:),pointer::DKX_C
real(8),dimension(:),pointer::DKY_C
real(8),dimension(:),pointer::DKZ_C
real(8),dimension(:),pointer::DOmegX_C
real(8),dimension(:),pointer::DOmegY_C
real(8),dimension(:),pointer::DOmegZ_C


contains
    
subroutine AllocateKWSSTVar(NC)
use TurbulenceVar
implicit none

integer::NC
 
allocate ( F11 (1:NC)      )
allocate ( F22 (1:NC)      )
allocate ( Sigk (1:NC)     )
allocate ( Sigw (1:NC)     ) 
allocate ( Beta (1:NC)     )
allocate ( Gama (1:NC)     )
 

 RK       => ConservativeNP1(:,6)
 ROmg     => ConservativeNP1(:,7)

 KFac    => ConservativeF(:,6)
 OmgFac  => ConservativeF(:,7)

   
 DKX_F    => FaceGrad(:,13)
 DKY_F    => FaceGrad(:,14)
 DKZ_F    => FaceGrad(:,15)
 DOmegX_F => FaceGrad(:,16)
 DOmegY_F => FaceGrad(:,17)
 DOmegZ_F => FaceGrad(:,18)

 DKx_C    => CellGrad(:,10)
 DKy_C    => CellGrad(:,11)
 DKz_C    => CellGrad(:,12)
 DOmegx_C => CellGrad(:,13)
 DOmegy_C => CellGrad(:,14)
 DOmegz_C => CellGrad(:,15)

 
 Bstar=0.09
 Sigk1=0.85   ;   Sigw1=0.5     ;   Beta1=0.075    ;   Gama1=5.0/9.0
 Sigk2=1.0    ;   Sigw2=0.856   ;   Beta2=0.0828   ;   Gama2=0.44

end subroutine

end module