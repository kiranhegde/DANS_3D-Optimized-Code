
!SSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
!*******************************************************************************************
 subroutine TrasFacValToCell(NC,NF,nFaceCell,iFaceCell,DirFaceCell,nVar,FaceVal,CellVal)
 implicit none
!*******************************************************************************************
 integer::i,j,ifac,nFace
 integer::Dir
 integer::nVar
 integer::NC
 integer::NF
 integer,dimension(1:NC)::nFaceCell
 integer,dimension(1:NC,1:8)::iFaceCell
 integer,dimension(1:NC,1:8)::DirFaceCell
 real(8),dimension(1:NC,1:nVar),intent(inout)::CellVal
 real(8),dimension(1:NF,1:nVar),intent(in)::FaceVal
!*******************************************************************************************

 do i=1,NC
      
      CellVal(i,:) = 0.0
      
      nFace=nFaceCell(i)
      
      do j=1,nFace
          iFac = iFaceCell(i,j)
          Dir  = DirFaceCell(i,j)

          CellVal(i,:) = CellVal(i,:) + Dir * FaceVal(iFac,:)
      end do

 end do
!*******************************************************************************************
end
!###########################################################################################
