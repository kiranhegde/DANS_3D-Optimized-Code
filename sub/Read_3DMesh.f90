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
!// Date: Dec., 05, 2016                                                                   //!
!// Developed by: N. msnkre, Aerospace Eng., Amirkabir University of Technology            //!
!//                                                                                        //!
!// The Program is Available Through the Website: www.DANS.ir                              //!
!// It May be Copied, Modified and Redistributed for Non-Commercial Use.                   //!
!//----------------------------------------------------------------------------------------//!
!// Duty:                                                                                  //!
!//                                                                                        //!
!////////////////////////////////////////////////////////////////////////////////////////////!
!*********************************************************************************************
 subroutine Read_3DMesh(FileName)
 use MeshVar
 implicit none
!*********************************************************************************************
CHARACTER(LEN=8)::FileName
integer::i,j,J1,JJ,SFace,MeshDim
!*********************************************************************************************
!Part 1:
 Open(1,File=FileName)

!Part 2:
 Read(1,*) MeshDim
 if(MeshDim/=3)Print*,'Please Check the Mesh File. It is not a 3D Mesh'
 
!Part 3:
 Read(1,*)  NP

!Part 4:
 Read(1,*)  NC 

!Part 5:
 Read(1,*)  NF

!Part 6:
 Read(1,*)  NR 

 call AllocateMeshVar()
 
!Part 7:
 Read(1,*)
 Do J=1, NR
    Read(1,*)  NFR(J) ,  BC(J)
 End Do 
 
!Part 8:
 Read(1,*)

 Do J=1, NF
    Read(1,'(I15)',Advance='No')  FaceType(J)
    Do I=1,2            
       Read(1,'(I15)',Advance='No')  IDS(I,J)            
    End Do
    Do I=3 , FaceType(J)+2        
       Read(1,'(I15)',Advance='No')  IDS(I,J)            
    End Do
    Read(1,*)
 End Do

!Part 9:
 Read(1,*)        
 Do J=1, NP
    Read(1,*)  X(J), Y(J), Z(J)
 End Do

 Close(1)
!*********************************************************************************************
 End
!###########################################################################################
