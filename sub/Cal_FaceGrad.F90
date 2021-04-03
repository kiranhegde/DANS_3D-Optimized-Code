
!SSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
!*********************************************************************************************
subroutine Cal_FaceGrad(nPhiGradient,PhiCell,PhiFace,FaceGrad)
use MeshVar
use VirtualMesh
use LocalVar
implicit none
!*********************************************************************************************
integer,INTENT(in)::nPhiGradient
real(8),dimension(1:NC,1:nPhiGradient)    ,INTENT(IN)  ::PhiCell
real(8),dimension(1:NF,1:nPhiGradient)    ,INTENT(IN)  ::PhiFace
real(8),dimension(1:NF,1:3*nPhiGradient)  ,INTENT(OUT) ::FaceGrad
 
integer::j,i,Pv,FacePt,j1,j2,j3
real(8)::NXX,NYY,NZZ,Tmp  
real(8)::eps=0.00000001

integer::iFace,nFace,Pt
real(8),dimension(1:nPhiGradient)::Phi
real(8),dimension(1:nPhiGradient)::PhiFacev
real(8),dimension(1:NPv,1:nPhiGradient)::PhiPointv
!*********************************************************************************************

do i=1,NP
    J1 = nConnectedFaces(i) + 1
    J2 = nConnectedFaces(i+1)

    nFace = J2 - J1 + 1
    Phi(:) = 0.0
    do j=j1,j2
        iFace = iConnectedFaces(j)
        Phi(:) = Phi(:) + PhiFace(iFace,:)
    end do
    PhiPointv(i,:) = Phi(:)/nFace
end do

do i=NFW1+1,NFW2

    do j=1,FaceType(i)
        Pt  = IDS(j+2,i)
        PhiPointv(Pt,:) = PhiFace(i,:)
    end do

end do

do i=1,NC
    PhiPointv(NP+i,:) = PhiCell(i,:)
end do
 
do i=1,NFv
     
    FacePt = FaceTypev(i)
    PhiFacev(:) = 0.0
    do j=1,FacePt
       Pv = IDSv(j+2,i) 
       PhiFacev(:) = PhiFacev(:) + PhiPointv(Pv,:)
    end do
    PhiFacev(:) = PhiFacev(:) / FacePt
    
    NXX = NXv(i)
    NYY = NYv(i)
    NZZ = NZv(i)
    
    Tmp = 0
    do j=1,nPhiGradient
       j1 = Tmp + 1
       j2 = Tmp + 2
       j3 = Tmp + 3
       
       LocalFacValuv(i,j1)   = PhiFacev(j) * NXX
       LocalFacValuv(i,j2)   = PhiFacev(j) * NYY
       LocalFacValuv(i,j3)   = PhiFacev(j) * NZZ
       
       Tmp =Tmp + 3
    end do
    
 end do 

 call TrasFacValToCell(NCv,NFv,nFaceCellv,iFaceCellv,DirFaceCellv,3*nPhiGradient,LocalFacValuv,FaceGrad)

 
 do i=1,NCv
    FaceGrad(i,:) = FaceGrad(i,:) / Volv(i)
 end do
 
 do i=NFS1+1,NFS2
  
    NXX = dabs(NX(i)) 
    NYY = dabs(NY(i))  
    NZZ = dabs(NZ(i))
    
    Tmp = 0
    do j=1,nPhiGradient
        
       j1 = Tmp + 1
       j2 = Tmp + 2
       j3 = Tmp + 3
       
       if(NYY<eps .AND. NZZ<eps)then !dPhi/dx=0
        FaceGrad(i,j1)  = 0.0
       elseif (NXX<eps .AND. NZZ<eps)then !dPhi/dy=0
        FaceGrad(i,j2)  = 0.0
       elseif (NXX<eps .AND. NYY<eps)then !dPhi/dz=0
        FaceGrad(i,j3)  = 0.0
       endif

       Tmp =Tmp + 3
              
    end do
       
 end do

!*********************************************************************************************
end
!###########################################################################################
 