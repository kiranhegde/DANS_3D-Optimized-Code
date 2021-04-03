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
subroutine KwSST_Source3D(Cell,FirstEqSorc,SecndEqSorc)
 use MeshVar
 use MeanFlowVar
 use TurbulenceVar
 use KwSSTVariables
 implicit none
!*********************************************************************************************
integer::Cell
real(8)::K,Omega,Rho,Sxx,Syy,Szz,Sxy,Sxz,Syz,Rhok,Coeff,Txx,Txy,Txz,Tyx,Tyy,Tyz,Tzx,Tzy,Tzz,Pk,Uii,FirstEqSorc,SecndEqSorc
!********************************************************************************************* 

!Part 2: 
Rho   = Ro(Cell)
k     = Rk(Cell)/Rho
Omega = ROmg(Cell)/Rho
    
!Part 3:
Uii = (DUX_C(Cell)+DVY_C(Cell)+DWZ_C(Cell))/1.5

Sxx =2*DUX_C(Cell)-Uii  ;  Sxy =  DUY_C(Cell)+DVX_C(Cell)  ;  Sxz =  DUZ_C(Cell)+DWX_C(Cell)
                        Syy =2*DVY_C(Cell)-Uii       ;  Syz =  DVZ_C(Cell)+DWY_C(Cell)
                                                        Szz =2*DWZ_C(Cell)-Uii

Coeff = MR * Mut(Cell)
Rhok    = -(2.0/3.0)*Rho*K
    
Txx = Coeff * Sxx  + Rhok  ;  Txy = Coeff * Sxy        ;  Txz = Coeff * Sxz
Tyx = Txy                ;  Tyy = Coeff * Syy  + Rhok  ;  Tyz = Coeff * Syz
Tzx = Txz                ;  Tzy = Tyz                ;  Tzz = Coeff * Szz   + Rhok
    
!Part 4:
Pk = Txx*DUX_C(Cell) + Tyy*DVY_C(Cell) + Tzz*DWZ_C(Cell) + Txy*(DUY_C(Cell)+DVX_C(Cell)) + Txz*(DWX_C(Cell)+DUZ_C(Cell)) + Tyz*(DVZ_C(Cell)+DWY_C(Cell))

!Part 5:
Pk = min (Pk,20.0*Bstar*Rho*Omega*K)

!Part 6:
FirstEqSorc = Vol(Cell) * ( Pk - Bstar*Rho*Omega*K  ) 
SecndEqSorc = Vol(Cell) * ( (Gama(Cell)*Rho/Mut(Cell)/MR)*Pk - Beta(Cell)*Rho*Omega*Omega &
            + 2.0*(1.0-F11(Cell))*Rho*Sigw2*(DKX_C(Cell)*DomegX_C(Cell)+DKY_C(Cell)*DomegY_C(Cell)+DKZ_C(Cell)*DomegZ_C(Cell))/Omega  )    
	  	   		  

!********************************************************************************************* 
 end
!###########################################################################################

