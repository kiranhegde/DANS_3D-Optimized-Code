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
!// Date: Mar., 10, 2015                                                                   //!
!// Developed by: N. msnkre, Aerospace Eng., Amirkabir University of Technology            //!
!//                                                                                        //!
!// The Program is Available Through the Website: www.DANS.ir                              //!
!// It May be Copied, Modified and Redistributed for Non-Commercial Use.                   //!
!//----------------------------------------------------------------------------------------//!
!// Duty:                                                                                  //!
!//                                                                                        //!
!////////////////////////////////////////////////////////////////////////////////////////////!
!*********************************************************************************************
 subroutine	BC_InFlow3D(FacInx)
 use MeshVar
 use MeanFlowVar
 implicit none
!**********************************************************************************************

integer                                 ::FacInx,Left
real(8)                                 ::MLoc,NXX,NYY,NZZ,PE,Rho,CE,VE,HTE,QNE,QTE,RIE,SE,UE,WE,UB,VB,WBB,RB,PB,EB
real(8)                                 ::ITT,ITP,RIB,HTB,EP1,EP2,EP3,C1,C2,MB,TB,UBB,C
!**********************************************************************************************	
!Part 1:
ITT=(1.+0.2*Minf*Minf)
ITP= (ITT**3.5)   * P0


!Part 3:
Left  = IDS(1,FacInx)

NXX = NX(FacInx)/DA(FacInx)
NYY = NY(FacInx)/DA(FacInx)
NZZ = NZ(FacInx)/DA(FacInx)

!Part 4:
Rho= Ro(Left)
UE = RU(Left)/Rho
VE = RV(Left)/Rho
WE = RW(Left)/Rho
PE = P(Left)
CE = SQRT(ABS(GM*PE/Rho))
    
!Part 5:
MLoc = SQRT((UE*UE+VE*VE+WE*WE)/CE)
    
!Part 6:
if(MLoc<1.)then
              
!Part 7: 
    QNE = UE*NXX+VE*NYY+WE*NZZ
            
!Part 8:
    RIE =-QNE-2.*CE/GM1
    HTE = (PE/Rho)*(GM/GM1)+0.5*(UE*UE+VE*VE+WE*WE)
           
!Part 9:
	RIB=RIE
	HTB=HTE
       
!Part 10:
	EP1=1+2/GM1
	EP2=2*RIB
	EP3=(GM1/2)*(RIB*RIB-2*HTB)

	C1=(-EP2+SQRT(EP2*EP2-4*EP1*EP3))/(2*EP1)
	C2=(-EP2-SQRT(EP2*EP2-4*EP1*EP3))/(2*EP1)

	C=MAX(C1,C2)
        
!Part 11:
	UBB= (2*C/GM1)+RIB

    MB=UBB/C

	PB=ITP*(1+(GM1/2)*MB**2)**(-GM/GM1)

	TB=ITT*(PB/ITP)**(GM1/GM)

	RB=GM*PB/TB

    UB=Cos(ALF)*UBB
	VB=Sin(ALF)*UBB
	WBB=0.0
        
!Part 12:
elseif(MLoc>=1.)then
        
    RB  = R0
    UB  = U0
    VB  = V0
    WBB = W0
    PB  = P0

end if
    
!Part 13:
EB= PB/(RB*GM1) + 0.5*(UB*UB + VB*VB + WBB*WBB)
    
!Part 14:
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

