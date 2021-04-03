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
 subroutine Cal_Residual()
 use MeshVar
 use MeanFlowVar
 use TurbulenceVar
 use KwSSTVariables
 use TransientVar
 use Gradient
 use LocalVar
 implicit none
!*********************************************************************************************
 real(8)::NXX,NYY,NZZ,U,V,W ,ConMass,ConMom1,ConMom2,ConMom3,ConEnrg,DifMass,DifMom1,&
          DifMom2,DifMom3,DifEnrg,FirstEqConv,ScondEqConv,FirstEqDiff,SecndEqDiff,FirstEqSorc,SecndEqSorc
 integer::i,FacInx,Cell
 real(8)::Rho
!*********************************************************************************************	
 
call ApplyBoundCondition()


do i=1,NC
    Rho = Ro(i)
    PhiCell(i,1) = RU(i) / Rho
    PhiCell(i,2) = RV(i) / Rho
    PhiCell(i,3) = RW(i) / Rho
    
    PhiCell(i,4) = GM*P(i) / Rho
    
    PhiCell(i,5) = RK(i)   / Rho
    PhiCell(i,6) = ROmg(i) / Rho
end do


do i=1,NF
    PhiFace(i,1) = UFac(i)
    PhiFace(i,2) = VFac(i)
    PhiFace(i,3) = WFac(i)
    
    PhiFace(i,4) = GM*PFac(i)/RFac(i)
    
    PhiFace(i,5) = KFac(i)
    PhiFace(i,6) = OmgFac(i)
end do
call Cal_FaceGrad(6,PhiCell,PhiFace,FaceGrad)


do i=NFW1+1,NFW2       
    dTdx_F(i) = 0.0
    dTdy_F(i) = 0.0
    dTdz_F(i) = 0.0
end do



do i=1,NF
    PhiFace(i,1) = UFac(i)
    PhiFace(i,2) = VFac(i)
    PhiFace(i,3) = WFac(i)
    
    PhiFace(i,4) = KFac(i)
    PhiFace(i,5) = OmgFac(i)

end do
call Cal_CellGrad(6,PhiFace,CellGrad)
 
 
call KwSST_Func3D()

!call wrapper_Cal_Residual()
do FacInx=NF1+1,NF2

    NXX = NX(FacInx) 
    NYY = NY(FacInx)  
    NZZ = NZ(FacInx)

    U = UFac(FacInx)
    V = VFac(FacInx)
    W = WFac(FacInx)
    
    call AUSM_PlusUP3D(FacInx,NXX,NYY,NZZ,U,V,W,ConMass,ConMom1,ConMom2,ConMom3,ConEnrg)  
    
    call DifMeanFlowInterior(FacInx,NXX,NYY,NZZ,U,V,W,DifMass,DifMom1,DifMom2,DifMom3,DifEnrg)
    
    call TwoEqTurbConvectionInterior(FacInx,NXX,NYY,NZZ,U,V,W,FirstEqConv,ScondEqConv)
    
    call KwSSTdiffInterior(FacInx,NXX,NYY,NZZ,U,V,W,FirstEqDiff,SecndEqDiff)
   
    FacVal(FacInx,1) = ConMass
    FacVal(FacInx,2) = ConMom1 + DifMom1
    FacVal(FacInx,3) = ConMom2 + DifMom2
    FacVal(FacInx,4) = ConMom3 + DifMom3
    FacVal(FacInx,5) = ConEnrg + DifEnrg
    
    FacVal(FacInx,6) = FirstEqConv + FirstEqDiff
    FacVal(FacInx,7) = ScondEqConv + SecndEqDiff
end do
 
 
do FacInx=NF2+1,NF
 
    NXX = NX(FacInx) 
    NYY = NY(FacInx)  
    NZZ = NZ(FacInx)

    U = UFac(FacInx)
    V = VFac(FacInx)
    W = WFac(FacInx)
    
    call BoundarConMeanFlow(FacInx,NXX,NYY,NZZ,U,V,W,ConMass,ConMom1,ConMom2,ConMom3,ConEnrg)

    call DifMeanFlowBoundary(FacInx,NXX,NYY,NZZ,U,V,W,DifMass,DifMom1,DifMom2,DifMom3,DifEnrg)
    
    call TwoEqTurbConvectionBoundary(FacInx,NXX,NYY,NZZ,U,V,W,FirstEqConv,ScondEqConv)
    
    call KwSSTdiffBoundary(FacInx,NXX,NYY,NZZ,U,V,W,FirstEqDiff,SecndEqDiff)

    FacVal(FacInx,1) = ConMass
    FacVal(FacInx,2) = ConMom1 + DifMom1
    FacVal(FacInx,3) = ConMom2 + DifMom2
    FacVal(FacInx,4) = ConMom3 + DifMom3
    FacVal(FacInx,5) = ConEnrg + DifEnrg
    
    FacVal(FacInx,6) = FirstEqConv + FirstEqDiff
    FacVal(FacInx,7) = ScondEqConv + SecndEqDiff

end do
  
  
call TrasFacValToCell(NC,NF,nFaceCell,iFaceCell,DirFaceCell,7,FacVal,Residual) 
 
do Cell=1,NC
     
    call KwSST_Source3D(Cell,FirstEqSorc,SecndEqSorc)
    
    Residual(Cell,6) = Residual(Cell,6) - FirstEqSorc
    Residual(Cell,7) = Residual(Cell,7) - SecndEqSorc
 
end do

!********************************************************************************************* 
end
!###########################################################################################

    
    


    
    
    

