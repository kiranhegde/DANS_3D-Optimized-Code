!*********************************************************************************************
 subroutine FaceOfCell()
 use MeshVar
 implicit none
!*********************************************************************************************
 integer::Left,Rigt,i,j,E1,E2,E3,P1_E2,P2_E1,J1,J2,E,P,N 
!*********************************************************************************************
!Part 1:
 nFaceCell(:)=0

!Part 2:
 do i=1,NF

   !Part 3:
    Left = IDS(1, i)
    Rigt = IDS(2, i)
	
   !Part 4:
     nFaceCell(Left) =  nFaceCell(Left) + 1
     iFaceCell(Left, nFaceCell(Left))=i
     DirFaceCell(Left, nFaceCell(Left))=1
    
    if(Rigt/=0)then
	  nFaceCell(Rigt) =  nFaceCell(Rigt) + 1
      iFaceCell(Rigt, nFaceCell(Rigt))=i
      DirFaceCell(Rigt, nFaceCell(Rigt))=-1
    endif

 end do

!*********************************************************************************************
 end
!###########################################################################################

