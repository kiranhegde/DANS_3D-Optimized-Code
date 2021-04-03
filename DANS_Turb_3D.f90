!DDDDDDDDDDDDDDDDDDDDDDDDDDDAAAAAAAAAAAAAAAAAAAAAAAAAAAANNNNNNNNNNNNNNNNNNNNNNNNSSSSSSSSSSSSSSSSSSSSSSSSSS
!//             /////////////       ////////////////    ////////     //////    ////////////////        //!
!//             /////////////      ////////////////    //////////   //////    ////////////////         //!
!//            /////    /////     //////    //////    //////////// //////    /////                     //!
!//           /////    //////    ////////////////    ///////////////////    ////////////////           //!
!//          /////    //////    ////////////////    ////// ////////////               /////            //!
!//         ///////////////    //////    //////    //////   //////////    ////////////////             //!
!//       ///////////////     //////    //////    //////     ////////    ////////////////              //!
!//          Developer            Assistant    in      Numerical             Sciences                  //!
!//----------------------------------------------------------------------------------------------------//!
!// Supervisor: Dr. h. hdhrnuidn, Aerospace Department, Amirkabir University of Technology           //!
!// Chief Developer: N. msnkre, Aerospace eng., Amirkabir University of Technology                     //!
!// Date: October, 14, 2013                                                                            //!
!//                                                                                                    //!
!// The Program is Available Through the Website: www.DANS.ir                                          //!
!// It May be Copied, Modified and Redistributed for Non-Commercial Use.                               //!
!//----------------------------------------------------------------------------------------------------//!
!// Description:                                                                                       //!
!// This Code designed for analyzing steady Turbulence flow by solving three dimensional Navier-Stocks //!
!// equations. One of the main advantages of this code is that it enjoy several subroutines that       //!
!// assists user to follow code step by step. Mesh and Settings have external files that read through  //!
!// their subroutines in the first step of code. In addition Gauss-Green theorem used for calculating  //!
!// gradients. This code uses wide range wide range of turbulence models. ranging for calculating      //!
!// turbulence viscosity Main features of this code is as following:                                   //!
!// 1-dimension:	                            3D                                                     //!
!// 2-Type Of Mesh:	                            Unstructured                                           //!
!// 3-Data Structure of Mesh:                   Edge Base                                              //!
!// 4-Data Structure of Solver:	                Cell Centered                                          //!
!// 5-Flow Solver algorithm:	                Density Base                                           //!
!// 6-Flow Regime:	                            Turbulent                                              //!
!// 7-Convection Discretization Scheme:	        AUSM+                                                  //!
!//                                             Ausm+Up                                                //!
!//                                             ScalarDiss                                             //!
!// 8-Convection Term Discretization Accuracy:	First order                                            //!
!//                                             Second Order                                           //!
!// 9-Transient Term Discretization Scheme:     Explicit(Rung-Kutta)                                   //!
!// 10-Steady/Unsteady: 	                    Steady and Unsteady                                    //!
!// 11-Gradient Calculation Scheme:	            Green-Gauss                                            //!
!//                                             Least Square                                           //!
!// 12-Turbulence Model:                        KWSST                                                  //!
!//                                             KWSST                                                  //!
!//                                             KeLB                                                   //!
!//                                             KwWilcox                                               //!
!//                                             LES_WALE                                               //!
!//                                             LES_DSmag                                              //!
!//                                             KWSST_Transition                                       //!
!//                                             SA                                                     //!
!// 13-Moving Mesh Method:	                    non                                                    //!
!////////////////////////////////////////////////////////////////////////////////////////////////////////!
!*********************************************************************************************************
 Program DANS_Turb_3D
 use MeshVar
 use MeanFlowVar
 use TurbulenceVar
 use Gradient
 use LocalVar
 use VirtualMesh
 use KwSSTVariables
 implicit none


 
 integer::NS      !  Number of Steps in rung-kutta method
 integer::Ncyc !Nymber of Cycles of iterations 
 integer::Naverage   !Number of Averaging Steps
 
 integer::NWrite   !Number of Cycle to write Results
 integer::Init !Initialize from Available Data in the file(1) or Infinite Flows(0)
 real(8)::ERMX !Maximum Error in Steady Approach

 real(8)::Rm !Residual of Mass Equation
 integer::i,j,nTurb,Tmp3,Tmp4,Tmp5,Tmp6,nPhiGradient
!*********************************************************************************************************

 print*,'read mesh finished'
 call Read_3DMesh("mesh.gid")
 
 call Write3DMeshSepRgn_gid_plt("Imesh.plt")

 

!!!!------ Hint: uncomment following if using not extruded mesh -------
!!!do i=1,NF
!!!     if(FaceType(i)==3)then
!!!     Tmp3=IDS(3,i)
!!!     Tmp4=IDS(4,i)
!!!     Tmp5=IDS(5,i)
!!!     
!!!     IDS(5,i)=Tmp3
!!!     IDS(4,i)=Tmp4
!!!     IDS(3,i)=Tmp5
!!!     elseif(FaceType(i)==4)then
!!!     Tmp3=IDS(3,i)
!!!     Tmp4=IDS(4,i)
!!!     Tmp5=IDS(5,i)
!!!     Tmp6=IDS(6,i)
!!!     
!!!     IDS(6,i)=Tmp3
!!!     IDS(5,i)=Tmp4
!!!     IDS(4,i)=Tmp5
!!!     IDS(3,i)=Tmp6
!!!     endif
!!!end do
!!!!------------------------------
 
 
nTurb = 2
call AllocateTransientVar(NC,NF,nTurb)
call AllocateGradient(6,NC,NF)
call AllocateMeanFlowVar(NC,NF)
call AllocateTurbulenceVar(NC,NF)
call AllocateKWSSTVar(NC)
call AllocateLocalVar(NF,NC)

print*,'MeshBC'
call MeshBC3D()


print*,'find Face Of Cell'
call FaceOfCell()

 call CPU_TIME(T1)
print*,'find points Of Cell'
call PointOfCell3D()
  call CPU_TIME(T2) 
print*,'CPU time',T2-T1
pause
print*,'find connected faces to any points'
call FindConnectedFaces()
 
print*,'Calculate Geomertice properties of mesh'
call GeoCal3D()

print*,'Calculate wall distance'
call WallDistance3D()


print*,'generate virtual mesh'
call GenerateVirtualMesh3D() 

print*,'read setting file'
call Read_Setting(ERmx,NWrite,Init)

print*,'initial mean flow'
call InitMeanFlow3D(Init)

print*,'initial turbulence var' 
call Kw_Init()

 
 Ncyc = 0
 Rm   = 10.0

 do While(Rm > ERmx)
    Ncyc=Ncyc+1
 
    ConservativeN  = ConservativeNP1
 
    do NS=1,NRKS

	   call Cal_Residual()

	   if(NS==1) call TimSTP_Turb3D()

       call RunkKutaUpdatVariables(NS)

    end do !Ns	

 	call ResMass3D(Ncyc,Rm)
    Print*,'Ncyc:',Ncyc,'  Mass eq. Residual:',Rm
    
    if( Mod(Ncyc,NWrite)==0 )then
     Print*,'Writing Results... ',Ncyc,Rm
	   
     call Write_CP3D(Minf,P)
     call Write_CF3D()
      
     call Write_FlatPlateAnalyticData3D()
      
     call Write_VelocityContour3D()
      
     call Write_ScalarContour3D(P,"Pressr.plt")
     call Write_ScalarContour3D(Mut,"Muturb.plt")
      
     call Write_ConservativeVariables3D()
    end if
     
 end do !do While

!*********************************************************************************************
 end 
!###########################################################################################



      

