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
!// Developed by: *//*-+/                       //!
!//                                                                                        //!
!// The Program is Available Through the Website: www.DANS.ir                              //!
!// It May be Copied, Modified and Redistributed for Non-Commercial Use.                   //!
!//----------------------------------------------------------------------------------------//!
!// Duty:                                                                                  //!
!//                                                                                        //!
!////////////////////////////////////////////////////////////////////////////////////////////!
!*********************************************************************************************
 Subroutine Write_FlatPlateAnalyticData3D()
 use MeshVar
 use MeanFlowVar
 use TurbulenceVar
 Implicit None
!*********************************************************************************************


INTEGER                                 ::I,J,P1,P2,P3,P4,ME,IDC,IDF
Real(8)                                 ::Rt,Ut,Vt,Pt,VTt,Mt,CP,CCP,DX,DY,DL,Xm,Ym,Yk,TAUW,CFC,X21,X10
Real(8)                                 ::Y21,Y10,Et,Rex,CF,CFexactT ,CFexactL,Yp,UpP,Utau,Dis,XD,Dmin,U,V,CC,Mach,Teta5
!*********************************************************************************************2500149633	
!Part 1:
 Open(1,File='FlatPlateAnalytic_CF.Plt')
 Open(2,File='FlatPlateAnalytic_UpYp.Plt')

!Part 2:
 CFC = 2/(Rinf*Minf)

!Part 3:
 WRITE(1,*) 'zone'
 DO I=NFW1+1,NFW2

    ME = IDS(1,I)
	Rex= Xc(ME)* Rinf
    CFexactL =  0.664/Dsqrt(Rex) 	 		
    WRITE(1,*) Xc(ME) , CFexactL

 End do

!Part 4:
 WRITE(1,*) 'zone'
 DO I=NFW1+1,NFW2

    ME = IDS(1,I)
	Rex= Xc(ME)* Rinf
    CFexactT = 0.025*Rex**(-1/7.)
    WRITE(1,*) Xc(ME) , CFexactT

 End do

!Part 5:
 XD = 0.5
 Dmin = 1000
 
!Part 6:
 Do I=NFW1+1,NFW2
    P1 = IDS(3,I)
    P2 = IDS(4,I)
    Xm = 0.5*( X(P1)+X(P2) )
	Dis = Dabs(Xm-XD)
    IF( Dis<Dmin )Then
	 Dmin=Dis
	 IDF=I
	Endif
 end do
 IDC = IDS(1,IDF)

!Analytical Solution of 'Viscous Sub Layer'
!Part 7:
 WRITE(2,*) 'zone'
 Do Yp=0.1,20,0.2
	   Upp=Yp
	   WRITE(2,*) Yp,Upp
 End do

!Analytical Solution of 'Logaritmic Layer'
 WRITE(2,*) 'zone'
 Do Yp=1,1000
         Upp=(1/0.41)*dlog(9.0*Yp)
	   WRITE(2,*) Yp,Upp
 End do

!Numerical Solution
!Part 8:
 WRITE(2,*) 'zone'
 Do I=1,NC
    IF(abs(XC(I)-XC(IDC))<0.0000001)then
        
     Utau=Sqrt(Mu(IDC)*dUdy_F(IDF)/Ro(IDC)) 
     Yp=Yc(I)*Utau*Ro(IDC)/Mu(IDC) * Sqrt(Rinf/Minf)
	 Upp=RU(I)/Utau * Sqrt(Rinf/Minf)

	 write(2,*) Yp,Upp

    End if
 End do

100 continue

!Part 9:
 Close(1)
 Close(2)
!**********************************************************************************************
 End
!##############################################################################################
