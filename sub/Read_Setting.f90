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
 subroutine Read_Setting(ERmx,NWrite,Init)
 use TransientVar
 use MeanFlowVar
 implicit none
!********************************************************************************************* 

real(8)               ,INTENT(OUT) ::ERmx
integer               ,INTENT(OUT) ::NWrite
integer               ,INTENT(OUT) ::Init
real(8)               ::k2,k4
!*********************************************************************************************	
 Open(1,File='Setting.Txt')

!Part 1:
 Read(1,*) Minf

!Part 2:
 Read(1,*) Rinf

!Part 3:
 Read(1,*) ALF

!Part 4:
 Read(1,*) Tt

!Part 5:
 Read(1,*) ERmx
 
!Part 6:
 Read(1,*) CFLx
 
!Part 7:
 Read(1,*) NRKS

!Part 8:
 Read(1,*) NWrite

!Part 9:
 Read(1,*) Init

!Part 10:
 Read(1,*) K2
 Read(1,*) K4
  
!Part 11: 
 Read(1,*) TotTime
 
!Part 13:
 Read(1,*) Time_Coe
 
!Part 14:
 if(NRKS==1)then
  RKJ(1)=1.0
 elseif(NRKS==2)then
  RKJ(1)=0.5 ; RKJ(2)=1.0
 elseif(NRKS==3)then
  RKJ(1)=0.15 ; RKJ(2)=0.4 ; RKJ(3)=1.0
 elseif(NRKS==4)then
  RKJ(1)=0.333 ; RKJ(2)=0.2667 ; RKJ(3)=0.555 ; RKJ(4)=1.0
 elseif(NRKS==5)then
  RKJ(1)=0.25 ; RKJ(2)=0.1667 ; RKJ(3)=0.375 ; RKJ(4)=0.5 ; RKJ(5)=1.0
 end if

 Close(1)
!*********************************************************************************************
 end
!###########################################################################################