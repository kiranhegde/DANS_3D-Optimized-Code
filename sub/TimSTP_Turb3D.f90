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
 subroutine TimSTP_Turb3D()
 use MeshVar
 use MeanFlowVar
 use TurbulenceVar
 use TransientVar
 use LocalVar
 implicit none
!*********************************************************************************************
integer::i,Left,Rigt,P1,P2
real(8)::U,V,W,Ti,Tv,Cj,Muj,Rj,Sig,Tmp

real(8),dimension(:),pointer::DTi
real(8),dimension(:),pointer::DTv

DTi => Array2D(:,1)
DTv => Array2D(:,2)
!*********************************************************************************************	

 do i=1,NF
 
   !Part 8:
    Left = IDS(1,i)
	Rigt = IDS(2,i)

   !Part 9:
    U = UFac(i)
    V = VFac(i)
    W = WFac(i)

   !Part 10:
    Rj  = RFac(i)
    Cj  = SQRT( ABS( GM*PFac(i)/Rj ) )
    Muj = MuFac(i)/PrL + MutFac(i)/PrT

   !Part 11:
    FacVal2D(i,1) = ABS(U*NX(i) + V*NY(i) + W*NZ(i)) + Cj*DA(i)
    

   !Part 13:
    FacVal2D(i,2)  = Muj*DA(i)*DA(i)/Rj

 end do

call TrasFacValToCell(NC,NF,nFaceCell,iFaceCell,abs( DirFaceCell ),2,FacVal2D,Array2D)
 
 Sig = 0.15
 Tmp = GM**1.5*MR

 do i=1,NC
    DTi(i) = VOL(i)/DTi(i)
    DTv(i) = Sig*Vol(i)*VOL(i) / (Tmp*DTv(i))
    DT(i) = CFLx*DTi(i)*DTv(i)/(DTi(i)+DTv(i))
    
    !just active for inviscid flow
    !DT(i) = CFLx*Vol(i)/DTi(i)
    
 end do
!*********************************************************************************************
 end
!###########################################################################################
