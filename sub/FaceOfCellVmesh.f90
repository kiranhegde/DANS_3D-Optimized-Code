!*********************************************************************************************
 subroutine FaceOfCellVmesh()
 use VirtualMesh
 implicit none
!*********************************************************************************************

 integer::Left,Rigt,i,j,E1,E2,E3,P1_E2,P2_E1,J1,J2,E,P,N
!*********************************************************************************************
!Part 1:
 nFaceCellv(:)=0

!Part 2:
 do i=1,NFv

   !Part 3:
    Left = IDSv(1, i)
    Rigt = IDSv(2, i)
	
   !Part 4:
    nFaceCellv(Left) = nFaceCellv(Left) + 1
    iFaceCellv(Left,nFaceCellv(Left))=i
    DirFaceCellv(Left,nFaceCellv(Left))=1
    
    if(Rigt/=0)then
	 nFaceCellv(Rigt) = nFaceCellv(Rigt) + 1
     iFaceCellv(Rigt,nFaceCellv(Rigt))=i
     DirFaceCellv(Rigt,nFaceCellv(Rigt))=-1
    endif

 end do
!*********************************************************************************************
 end
!###########################################################################################