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
 subroutine Cal_CellGrad(nPhiGradient,PhiFace,CellGrad)
 use MeshVar
 use LocalVar
 implicit none
!*********************************************************************************************
integer,INTENT(in)::nPhiGradient
real(8),dimension(1:NF,1:nPhiGradient),INTENT(in)::PhiFace
real(8),dimension(1:NC,1:3*nPhiGradient),INTENT(OUT)::CellGrad

integer::i,j,Tmp,j1,j2,j3
real(8)::Phi,NXX,NYY,NZZ
!********************************************************************************************* 

 do i=1,NF

    NXX = NX(i)
    NYY = NY(i)
    NZZ = NZ(i)

    Tmp = 0
    do j=1,nPhiGradient
       Phi = PhiFace(i,j)
       
       j1 = Tmp + 1
       j2 = Tmp + 2
       j3 = Tmp + 3
       
       LocalFacValu(i,j1)   = Phi * NXX
       LocalFacValu(i,j2)   = Phi * NYY
       LocalFacValu(i,j3)   = Phi * NZZ
       
       Tmp =Tmp + 3
    end do
    
 end do 
 
 call TrasFacValToCell(NC,NF,nFaceCell,iFaceCell,DirFaceCell,3*nPhiGradient,LocalFacValu,CellGrad)
 

 do i=1,NC
    CellGrad(i,:)  = CellGrad(i,:) / Vol(i)
 end do
 
!*********************************************************************************************
 end
!###########################################################################################

