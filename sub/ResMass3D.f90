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
 subroutine ResMass3D(Ncyc,Rm)
 use MeshVar
 use MeanFlowVar
 use TransientVar
 implicit none
!*********************************************************************************************

integer                     ,INTENT(IN) ::Ncyc
real(8)                     ,INTENT(OUT)::Rm

integer                                 ::i
real(8)                                 ::time_end
!*********************************************************************************************	
!Part 1:
 Open(111,File='ResMass_Iter.Plt')
 Open(110,File='ResMass_Time.Plt')
 	
!Part 2:
 Rm = 0.0

!Part 3:
 do i=1,NC
    Rm = Rm + Abs( ConservativeNP1(i,1)-ConservativeN(i,1) ) / DT(i)
 end do

!Part 4:
 Rm = Rm / NC

!Part 5:
 Rm = Dlog10(Rm+1.0E-16)
 write(111,*) Ncyc,Rm
 
 call CPU_Time(time_end)
 write(110,*) time_end,Rm

!*********************************************************************************************
 end
!###########################################################################################