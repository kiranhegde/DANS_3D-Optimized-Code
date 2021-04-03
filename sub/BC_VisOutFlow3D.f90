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
 subroutine BC_VisOutFlow3D(FacInx)
 use MeshVar
 use MeanFlowVar
 implicit none
!**********************************************************************************************

integer                                 ::FacInx,Left
real(8)                                 ::NXX,NYY,NZZ,U,V,W,CC,MLoc,PE,REE,UE,VE,WE,TE,RB,UB,VB,WBB,PB,TB,EB,Rho
!**********************************************************************************************	

!Part 1:

!Part 2:
Left  = IDS(1,FacInx)

Rho = Ro(Left)
U = RU(Left)/Rho
V = RV(Left)/Rho
W = RW(Left)/Rho

CC = GM*P(Left)/Rho
MLoc = SQRT((U*U+V*V+W*W)/CC)
	
!Part3 3:	
REE= Rho
UE = RU(Left)/REE
VE = RV(Left)/REE
WE = RW(Left)/REE
PE = P(Left)
TE = PE*GM/REE
	
!Part 4:
if(MLoc<1.)then
PB=P0
UB=UE
VB=VE
WBB=WE
TB=TE
RB=PB*GM/TB
	
!Part 5:
elseif(MLoc>1.)then
PB=PE
UB=UE
VB=VE
WBB=WE
TB=TE
RB=PB*GM/TB

end if
	
!Part 6:
EB= PB/(RB*GM1) + 0.5*(UB*UB + VB*VB + WBB*WBB)

!Part 7:    
RFac(FacInx) = RB
UFac(FacInx) = UB
VFac(FacInx) = VB
WFac(FacInx) = WBB
EFac(FacInx) = EB
PFac(FacInx) = PB
MuFac(FacInx) = Mu(Left)

!**********************************************************************************************
 end
!##############################################################################################