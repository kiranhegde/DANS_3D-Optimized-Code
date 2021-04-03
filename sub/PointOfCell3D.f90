!SSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
!//       /////////////       ////////////////    ////////     //////    ////////////////  //!
!//       /////////////      ////////////////    //////////   //////    ////////////////   //!
!//      /////    /////     //////    //////    //////////// //////    /////               //!
!//     /////    //////    ////////////////    ///////////////////    ////////////////     //!
!//    /////    //////    ////////////////    ////// ////////////               /////      //!
!//   ///////////////    //////    //////    //////   //////////    ////////////////       //!
!// ///////////////     //////    //////    //////     ////////    ////////////////        //!
!//    Developer            Assistant    in      Numerical             Sciences            //!
!//----------------------------------------------------------------------------------------//!
!// Chief Developer: N. msnkre, Aerospace eng. Amirkabir University of Technology          //!
!// Supervisor: Dr. h. hdhrnuidn, Aerospace eng. Amirkabir University of Technology      //!
!// Date: Feb., 10, 2018                                                                   //!
!// Developed by: N. msnkre, Aerospace Eng., Amirkabir University of Technology            //!
!//                                                                                        //!
!// The Program is Available Through the Website: www.DANS.ir                              //!
!// It May be Copied, Modified and Redistributed for Non-Commercial Use.                   //!
!//----------------------------------------------------------------------------------------//!
!// Duty:                                                                                  //!
!//                                                                                        //!
!////////////////////////////////////////////////////////////////////////////////////////////!
!*********************************************************************************************
 subroutine PointOfCell3D()
 use MeshVar
 implicit none
!*********************************************************************************************
 integer,dimension(1:6)::iFaceCellTmp 

 integer               ::i,j,k,P1,P2,NFace,Face,Face1,Face2,Temp,Cell
 integer,dimension(1:2)::IFace
 integer,dimension(1:8)::P
!*********************************************************************************************

do cell=1,NC

 iFaceCellTmp(1:6) = iFaceCell(Cell,1:6)
   
!Part 1:
 if( nFaceCell(Cell)>4 )then
  do j=1,nFaceCell(Cell)
     
      Face = iFaceCellTmp(j)
 
     !Part 2:
      if (Facetype(Face)==4) then
      Temp            = iFaceCellTmp(1)
      iFaceCellTmp(1) = iFaceCellTmp(j)
      iFaceCellTmp(j) = Temp
      exit
     end if 
     
  end do
 end if
  
!Part 3: 
 P(:) = 0

!Part 4:
 Face = iFaceCellTmp(1)
 do i=1,FaceType(Face)
    P(i) = IDS(2+i,Face)
 end do

 if( P(4)==0 ) P(4) = P(3)

!Part 5:
 do k=1,4

    NFace = 0
   
   !Part 6:
    do i=2,nFaceCell(Cell)

       Face = iFaceCellTmp(i)
 
      !Part 7:
       do j=1,FaceType(Face)
         
          if(P(k)==IDS(2+j,Face) )then
 	       NFace=NFace+1
	       IFace(NFace) = Face
	      endif

       end do

    end do

   !Part 8:
    Face1 = IFace(1)
    Face2 = IFace(2)
 
   !Part 9:
    do i=1,FaceType(Face1)
       P1 = IDS(2+i,Face1)

       do j=1,FaceType(Face2)
          P2 = IDS(2+j,Face2)
          
         !Part 10:
          if(P1==P2 .and. P1/=P(k) ) P(4+k) = P1
       end do

    end do

 end do
 
!Part 11:
 Corn(:,Cell) = P(:)

 end do

!*********************************************************************************************
 end
!###########################################################################################



 