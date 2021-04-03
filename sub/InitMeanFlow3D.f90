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
!// Date: April, 01, 2017                                                                  //!
!// Developed by: N. msnkre, Aerospace Eng., Amirkabir University of Technology            //!
!//                                                                                        //!
!// The Program is Available Through the Website: www.DANS.ir                              //!
!// It May be Copied, Modified and Redistributed for Non-Commercial Use.                   //!
!//----------------------------------------------------------------------------------------//!
!// Duty:                                                                                  //!
!//                                                                                        //!
!////////////////////////////////////////////////////////////////////////////////////////////!
!*********************************************************************************************
 subroutine InitMeanFlow3D(Init)
 use MeshVar
 use MeanFlowVar
 use TurbulenceVar
 use TransientVar
 implicit none
!*********************************************************************************************
integer,INTENT(IN)  ::Init

 integer::i,j
 real(8)::PI,Ts,ALFA,U,V,W,Temp,h_Ts
!*********************************************************************************************	
!Part 1:
 PI  = 4.0*Atan(1.0)
 ALFA= ALF*PI/180.

!Part 2:
 PrL = 0.72
 PrT=0.9
 GM = 1.4
 GM1= GM-1
 
!Part 3:
 R0 = 1.0

!Part 4:
 P0 = 1.0/GM  

!Part 5:
 T0 = GM*P0/R0  

!Part 6:
 Ts = Tt/(1.0 + 0.5*(GM-1)*Minf*Minf)

!Part 7:
 B0 = 110.4/Ts

!Part 8:
 C0 = SQRT(GM*P0/R0)

!Part 9:
 U0 = Minf*C0*COS(ALFA)
 V0 = Minf*C0*SIN(ALFA)
 W0 = 0.0

!Part 10:
 E0 = P0/(R0*(GM-1))+ 0.5*(U0*U0 + V0*V0 + W0*W0) 

!Part 11:
 Mu0 = 1.0 

 do i=1,NC
    Ro(i) = R0
    RU(i) = R0*U0
    RV(i) = R0*V0
    RW(i) = R0*W0
    RE(i) = R0*E0
 end do

!Part 13:
 MR = Minf/Rinf

!Part 14:
 if(Init==1)then
 Open(1,File='ConservativeVariables.txt')
 Read(1,*) 
 do i=1,NC
	Read(1,*) Ro(i),RU(i),RV(i),RE(i);RW(i)=0.0
 end do
 Close(1)
 endif
 
 
 do j=1,NC 
	U = RU(j)/Ro(j)
    V = RV(j)/Ro(j)
    W = RW(j)/Ro(j)
    P(j) = (GM-1)*(RE(j)-0.5*Ro(j)*(U*U+V*V+W*W))
    
   !Part 14:
    Temp = GM*P(j)/Ro(j) 

   !Part 15:
    Mu(j) = (Temp**1.5)*(1.0+B0)/(Temp+B0)     

 end do


!*********************************************************************************************
 end
!###########################################################################################