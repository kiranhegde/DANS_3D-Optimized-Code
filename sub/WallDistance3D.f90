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
 subroutine WallDistance3D()
 use MeshVar
 implicit none
!*********************************************************************************************

 integer::j,i,II,JJ,Pi,FacTyp
 real(8)::Dmin,Dis,Xj,Yj,Zj,Xi,Yi,Zi,DX,DY,DZ
!*********************************************************************************************	

  do j=1,NC
  
   !Part 2:
	Xj = Xc(j)
	Yj = Yc(j)
	Zj = Zc(j)
   
   !Part 3:    
    Dmin=1000000.0

   !Part 4:
    do i=NFW1+1,NFW2
      
	  !Part 6:  
	   DX = Xj-Xmid(i)
	   DY = Yj-Ymid(i)
	   DZ = Zj-Zmid(i)
       Dis = Dsqrt(DX*DX+DY*DY+DZ*DZ) 
      
	  !Part 7:
	   if(Dis<Dmin)then
	    Dmin = Dis
	    II   = i
	   endif

	end do
      
   !Part 8: 
    DisWall(j)  = Dmin
    InxNearWall(j) = II

 end do

!*********************************************************************************************
 end
!###########################################################################################