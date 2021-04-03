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
 subroutine BC_interior(FacInx)
 use MeshVar
 use MeanFlowVar
 implicit none
!********************************************************************************************** 

real(8)::RoME,RoNE
integer::FacInx
integer::Left,Rigt
!**********************************************************************************************	
!Part 1:

Left = IDS(1,FacInx)
Rigt = IDS(2,FacInx)
    
RoME = Ro(Left)
RoNE = Ro(Rigt)
    
RFac(FacInx) = 0.5*( RoME          + RoNE          )
UFac(FacInx) = 0.5*( RU(Left)/RoME + RU(Rigt)/RoNE )
VFac(FacInx) = 0.5*( RV(Left)/RoME + RV(Rigt)/RoNE )
WFac(FacInx) = 0.5*( RW(Left)/RoME + RW(Rigt)/RoNE )
EFac(FacInx) = 0.5*( RE(Left)/RoME + RE(Rigt)/RoNE )
    
PFac(FacInx)   = 0.5*( P(Left)+P(Rigt) )
    
MuFac(FacInx) = 0.5*( Mu(Left)+Mu(Rigt) )

!**********************************************************************************************
 end
!##############################################################################################
