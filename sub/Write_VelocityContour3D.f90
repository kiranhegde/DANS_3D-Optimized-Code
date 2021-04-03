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
 Subroutine Write_VelocityContour3D()
 use MeshVar
 use MeanFlowVar
 use TransientVar
 Implicit None
!**********************************************************************************************

Integer                                 ::I,J
INTEGER                                 ::P1,P2,P3,P4,P5,P6,P7,P8
!**********************************************************************************************	
!Part 1:
 Open(104,File='VelocityContour.Plt')

!Part 2:
 Write(104,*) 'Variables="X","Y","Z","U","V","W" '
 Write(104,*) 'ZONE N=' ,   NP , ' E=' ,  NC  
 Write(104,*) ' ZONETYPE=FEBRICK DATAPACKING=BLOCK VARLOCATION=([4-6]=CELLCENTERED)'


!Part 3:
 Do J=1,NP
	Write(104,*) X(J)
 End Do
 Do J=1,NP
	Write(104,*) Y(J) 
 End Do
 Do J=1,NP
	Write(104,*) Z(J) 
 End Do


!Part 4:
 Do J=1,NC
	Write(104,*) RU(J)/Ro(J)
 End Do
 Do J=1,NC
	Write(104,*) RV(J)/Ro(J)
 End Do
 Do J=1,NC
	Write(104,*) RW(J)/Ro(J)
 End Do

   
 !Part 5:  
 DO I=1,NC
    P1 = Corn(1,I) 
    P2 = Corn(2,I) 
    P3 = Corn(3,I)
    P4 = Corn(4,I)
    P5 = Corn(5,I) 
    P6 = Corn(6,I) 
    P7 = Corn(7,I)
    P8 = Corn(8,I)

	Write(104,*) P1,P2,P3,P4,P5,P6,P7,P8
 End Do
 

 Close(104)
!**********************************************************************************************	
 END 
!##############################################################################################
 
 
 
 
