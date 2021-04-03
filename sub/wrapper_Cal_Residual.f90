!*********************************************************************************************
 subroutine wrapper_Cal_Residual()
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
 
!********************************************************************************************* 
end
!###########################################################################################

    
    


    
    
    
    

