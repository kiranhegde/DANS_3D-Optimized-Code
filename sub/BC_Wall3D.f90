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
 subroutine BC_Wall3D(FacInx)
 use MeshVar
 use MeanFlowVar
 implicit none
!********************************************************************************************** 

integer                                  ::FacInx,Left
real(8)                                  ::PB
!**********************************************************************************************	
!Part 1:

Left = IDS(1,FacInx)

!Part 4:
RFac(FacInx) = Ro(Left)
    
!Part 5:
UFac(FacInx) = 0.0
VFac(FacInx) = 0.0
WFac(FacInx) = 0.0
    
!Part 6:
PB      = P(Left)
EFac(FacInx) = PB/(RFac(FacInx)*GM1)
PFac(FacInx) = PB
MuFac(FacInx) = Mu(Left)

!**********************************************************************************************
 end
!##############################################################################################
