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
 subroutine GeoCal3D()
 use MeshVar
 use LocalVar
 implicit none
!*********************************************************************************************
integer                         ::i,j,K,P1,P2,P3,Left,Rigt,NFacePnt,IFace 
real(8)                         ::a,b,c,X1,X2,X3,Y1,Y2,Y3,Z1,Z2,Z3,X21,X32,vol_tt,VOLUME,Xcc,Ycc,Zcc
real(8)                         ::Y21,Y32,Z21,Z32,DV,SumX,SumY,SumZ,aX,bX,cX,aY,bY,cY,aZ,bZ,cZ

real(8),allocatable,dimension(:,:),target::CellGeoProprtyTmp
real(8),dimension(:),pointer::XC_Face 
real(8),dimension(:),pointer::YC_Face 
real(8),dimension(:),pointer::ZC_Face 
real(8),dimension(:),pointer::Vol_Face 

allocate ( CellGeoProprtyTmp(1:NF,1:4) )
XC_Face  => CellGeoProprtyTmp(:,1)
YC_Face  => CellGeoProprtyTmp(:,2)
ZC_Face  => CellGeoProprtyTmp(:,3)
Vol_Face => CellGeoProprtyTmp(:,4)
!*********************************************************************************************

 call FaceProprties3D(XC_Face,YC_Face,ZC_Face,Vol_Face)

 call TrasFacValToCell(NC,NF,nFaceCell,iFaceCell,DirFaceCell,4,CellGeoProprtyTmp,CellGeoProprty)

 do i=1,NC
    XC(i) = XC(i)/(48*vol(i))
    YC(i) = YC(i)/(48*vol(i))
    ZC(i) = ZC(i)/(48*vol(i))
    if(Vol(i)<0.0) print*,i
 end do

!********************************************************************************************* 
 end
!###########################################################################################