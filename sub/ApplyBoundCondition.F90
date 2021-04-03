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
 subroutine	ApplyBoundCondition()
 use MeshVar
 use MeanFlowVar
 implicit none
!**********************************************************************************************
 integer ::FacInx
!**********************************************************************************************	 

 do FacInx=NF1+1,NF2
    call BC_interior(FacInx)
    call BC_InteriorTwoEqTurb(FacInx)
 end do
 
 do FacInx=NFW1+1,NFW2
    call BC_Wall3D(FacInx)
    call BC_WallTwoEqTurb(FacInx)
 end do
  
 do FacInx=NFF1+1,NFF2
    call BC_FarField3D(FacInx)
    call BC_FarFieldTwoEqTurb(FacInx)
 end do
  
 do FacInx=NFI1+1,NFI2
 	call BC_InFlow3D(FacInx)
    call BC_InflowTwoEqTurb(FacInx)
 end do
   
 do FacInx=NFO1+1,NFO2
    call BC_VisOutFlow3D(FacInx)
    call BC_OutFlowTwoEqTurb(FacInx)
 end do
   
 do FacInx=NFS1+1,NFS2
    call BC_Symmetry3D(FacInx)
    call BC_SymetryTwoEqTurb(FacInx)
 end do
 
!**********************************************************************************************
 end
!##############################################################################################
