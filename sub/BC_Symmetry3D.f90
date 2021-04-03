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
!// Date: Mar., 05, 2013                                                                   //!
!// Developed by: N. msnkre, Aerospace Eng., Amirkabir University of Technology            //!
!//                                                                                        //!
!// The Program is Available Through the Website: www.DANS.ir                              //!
!// It May be Copied, Modified and Redistributed for Non-Commercial Use.                   //!
!//----------------------------------------------------------------------------------------//!
!// Duty:                                                                                  //!
!//                                                                                        //!
!////////////////////////////////////////////////////////////////////////////////////////////!
!*********************************************************************************************
 subroutine	 BC_Symmetry3D(FacInx)
 use MeshVar
 use MeanFlowVar
 implicit none
!**********************************************************************************************

integer                                 ::FacInx,Left
real(8)                                 ::UB,VB,WBB,PB,RB,EB,NXX,NYY,NZZ,eps
!**********************************************************************************************	
 eps = 0.000001


Left  = IDS(1,FacInx)

RB = Ro(Left)
    
UB = RU(Left)/RB
VB = RV(Left)/RB
WBB = RW(Left)/RB
    
NXX = dabs(NX(FacInx)) 
NYY = dabs(NY(FacInx))  
NZZ = dabs(NZ(FacInx))
    
if (NXX<eps .AND. NYY<eps) then
    WBB = 0.0
elseif (NYY<eps .AND. NZZ<eps) then
    UB = 0.0
elseif (NXX<eps .AND. NZZ<eps) then
    VB = 0.0
end if
    
PB = P(Left)

EB= PB/(GM1*RB) + 0.5*(UB*UB + VB*VB + WBB*WBB)

RFac(FacInx) = RB
UFac(FacInx) = UB
VFac(FacInx) = VB
WFac(FacInx) = WBB
EFac(FacInx) = EB
PFac(FacInx) = PB
MuFac(FacInx) = Mu(Left)

!**********************************************************************************************
 end
!SSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS