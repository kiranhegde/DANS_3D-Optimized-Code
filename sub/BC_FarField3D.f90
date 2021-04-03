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
 subroutine BC_FarField3D(FacInx)
 use MeshVar
 use MeanFlowVar
 implicit none
!**********************************************************************************************

integer                                  ::FacInx,Left
real(8)                                  ::DS,Nxx,Nyy,Nzz,UI,VI,WI,CI,MI,QNI,RI,SI,QN0,S0,QN,S,UB,VB,WBB,SB,ROB,PB1,R00,Rho
!**********************************************************************************************	

!Part 2:
DS  = DA(FacInx)
Nxx = Nx(FacInx)/DS    
Nyy = Ny(FacInx)/DS    
Nzz = Nz(FacInx)/DS

!Part 3:
Left  = IDS(1,FacInx)
Rho = Ro(Left)
UI  = RU(Left)/Rho
VI  = RV(Left)/Rho
WI  = RW(Left)/Rho

CI  = DSQRT(DABS(GM*P(Left)/Rho))
MI  = DSQRT(UI*UI+VI*VI+WI*WI)/CI

!Part 4:
QNI = UI*Nxx+VI*Nyy+WI*Nzz
RI  = QNI+2*CI/GM1
SI  = P(Left)/Rho**GM

!Part 5:
QN0 = U0*Nxx+V0*Nyy+W0*Nzz
R00  = QN0-2*C0/GM1
S0  = P0/R0**GM

!Part 6:
QN = 0.5 *(RI+R00)
S  = 0.25*GM1*(RI-R00)
	
!Part 7:
if(MI >= 1.0)then       !SUPERSONIC BOUNDARY CONDITION

!Part 8:
	if( QN<0.0 )then        !INPUT BOUNDARY
	UB  = U0
	VB  = V0
	WBB = W0
	ROB = R0 
	PB1 = P0
      
!Part 9:
	ELSE                  !OUTPUT BOUNDARY
	UB  = UI
	VB  = VI
	WBB = WI  
	ROB = Ro(Left) 
	PB1 = P(Left)
	end if

!Part 10:
elseif(MI < 1.0)then  !SUBSONIC BOUNDARY CONDITION
         
!Part 11:
	if( QN<0.0 )then          !INPUT BOUNDARY
	UB  = U0+(QN-QN0)*Nxx
	VB  = V0+(QN-QN0)*Nyy
	WBB = W0+(QN-QN0)*Nzz
	SB  = S0
	ROB = (S*S/GM/SB)**(1/GM1)
	PB1 = S*S*ROB/GM
      
!Part 12:
	ELSE                      !OUTPUT BOUNDARY
	UB  = UI+(QN-QNI)*Nxx
	VB  = VI+(QN-QNI)*Nyy
	WBB = WI+(QN-QNI)*Nzz
	SB  = SI
	ROB = (S*S/GM/SB)**(1/GM1)
	PB1 = S*S*ROB/GM
	end if

end if

!Part 13:
RFac(FacInx) =	ROB
UFac(FacInx) =	UB
VFac(FacInx) =	VB
WFac(FacInx) =	WBB
EFac(FacInx) =	PB1/(ROB*GM1) + 0.5*(UB*UB + VB*VB + WBB*WBB)
PFac(FacInx) =	PB1
MuFac(FacInx) = Mu(Left)

!**********************************************************************************************
 end
!##############################################################################################


