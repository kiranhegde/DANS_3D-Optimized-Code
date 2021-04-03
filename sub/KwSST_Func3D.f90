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
 subroutine KwSST_Func3D()
 use MeshVar
 use MeanFlowVar
 use TurbulenceVar
 use KwSSTVariables
 implicit none
!*********************************************************************************************

integer                                  ::Cell
real(8)                                  ::CDKw,F1,F2,Arg1,Arg2,Part1,Part2,k,Omega,Rho
!*********************************************************************************************
	
      
 

  do Cell=1,NC

   !Part 2:
    Rho   = Ro(Cell)
    k     = RK(Cell)/Rho
    Omega = ROmg(Cell)/Rho

   !Part 3:    
    CDkw = max( 2.0*Rho*Sigw2*(DKX_C(Cell)*DomegX_C(Cell)+DKY_C(Cell)*DomegY_C(Cell)+DKZ_C(Cell)*DomegZ_C(Cell))/Omega,1.0D-20)

   !Part 4:
	Part1 = Dsqrt(K) / ( Bstar*Omega*DisWall(Cell) )
	Part2 = 500.0*Mu(Cell)*MR / (DisWall(Cell)*DisWall(Cell)*Rho*Omega)     

    arg1=min ( max(Part1,Part2), 4.0*Rho*Sigw2*K / (DisWall(Cell)*DisWall(Cell)*CDkw) )
    arg2=max(2*Part1,Part2)

    F1=tanh(arg1*arg1*arg1*arg1)
    F2=tanh(arg2*arg2)

   !Part 5:
	F11(Cell) = F1
	F22(Cell) = F2

   !Part 6:
    Sigk(Cell) = F1 * Sigk1 + (1.0-F1) * Sigk2
    Sigw(Cell) = F1 * Sigw1 + (1.0-F1) * Sigw2
    Beta(Cell) = F1 * Beta1 + (1.0-F1) * Beta2
    Gama(Cell) = F1 * Gama1 + (1.0-F1) * Gama2
  
 end do
!*********************************************************************************************
 end
!###########################################################################################

