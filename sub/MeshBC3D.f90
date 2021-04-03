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
 subroutine MeshBC3D()
 use MeshVar
 implicit none
!*********************************************************************************************
integer                                    ::j,JJ,J1,i,SF,N,M,NFN,NFW,NFF,NFI,NFO,NFS,NFIF
!integer,constant                           ::d_NFN,d_NFW,d_NFF,d_NFI,d_NFO,d_NFS,d_NFIF
integer,dimension(1:NR)                    ::TNFR,TBC
integer,dimension(:,:),allocatable         ::TIDS
integer,dimension(:)  ,allocatable         ::TFaceType


allocate( TFaceType(1:NF) )  
allocate( TIDS(1:6,1:NF)  ) 
!*********************************************************************************************
!Part 1:
 TIDS      = IDS
 TFaceType = FaceType    
 TNFR      = NFR 
 TBC       = BC

!Part 3:
 N=0
 M=0
 do JJ=1,10

   !Part 4:
    SF=0
    do j=1,NR
       if(TBC(j)==JJ)then

	    do i=SF+1,SF+TNFR(j)

           N=N+1
           IDS(:,N) = TIDS(:,i)
		  
           FaceType(N) = TFaceType(i)

        end do
		
	   !Part 5:		   
	    M=M+1
	    NFR(M) = TNFR(j)
		BC(M)  = TBC(j)  

	   endif
	   SF=SF+TNFR(j)
    end do

 end do

!Part 6:
 NFN  = 0
 NFW  = 0
 NFF  = 0
 NFI  = 0
 NFO  = 0
 NFS  = 0
 NFIF = 0
 do j=1,NR
    if( BC(j)==1 ) NFN  = NFN  + NFR(j)
    if( BC(j)==2 ) NFW  = NFW  + NFR(j)
    if( BC(j)==3 ) NFF  = NFF  + NFR(j)
    if( BC(j)==4 ) NFI  = NFI  + NFR(j)
    if( BC(j)==5 ) NFO  = NFO  + NFR(j)
    if( BC(j)==6 ) NFS  = NFS  + NFR(j)
    if( BC(j)==7 ) NFIF = NFIF + NFR(j)
 end do

!Part 7:
  NF1=0
  NF2= NF1+NFN

  NFW1= NF2
  NFW2= NFW1+NFW

  NFF1= NFW2
  NFF2= NFF1+NFF

  NFI1= NFF2
  NFI2= NFI1+NFI

  NFO1= NFI2
  NFO2= NFO1+NFO

  NFS1= NFO2
  NFS2= NFS1+NFS

  NFIF1= NFS2
  NFIF2= NFIF1+NFIF

 deallocate( TFaceType )  
 deallocate( TIDS      )
!*********************************************************************************************
 end
!###########################################################################################

 
