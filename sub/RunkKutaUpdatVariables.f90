subroutine RunkKutaUpdatVariables(NS)
 use MeshVar
 use MeanFlowVar
 use TurbulenceVar
 use TransientVar
 use KwSSTVariables
 implicit none
    
 integer::i,j
 integer::NS
 real(8)::Rho
 real(8)::U
 real(8)::V
 real(8)::W
 real(8)::K
 real(8)::Omega
 
 real(8)::Temp
 real(8)::Vor

 real(8)::co 
 
 do j=1,NC
  
		Co = RKJ(NS)*DT(j)/Vol(j)
         
        ConservativeNP1(j,:) = ConservativeN(j,:) - Co* Residual(j,:) 

       !====================== Pressur ==========================
	    U = RU(j)/Ro(j)
        V = RV(j)/Ro(j)
        W = RW(j)/Ro(j)
        P(j) = (GM-1)*(RE(j)-0.5*Ro(j)*(U*U+V*V+W*W))
          
        
       !================ Molecular Viscousity ===================
        Temp = GM*P(j)/Ro(j) 
        Mu(j) = (Temp**1.5)*(1.0+B0)/(Temp+B0)     
       
    
      !==================== Eddy Viscousity ====================
       if(RK(j)<0.0 ) RK(j)=ConservativeN(j,6)
       if(ROmg(j)<0.0 ) ROmg(j)=ConservativeN(j,7)
 

       Rho   = Ro(j)
       K     = RK(j)/Rho
       Omega = ROmg(j)/Rho
 
       Vor= Dabs( DUY_C(j)-DVX_C(j) + DUZ_C(j)-DWX_C(j) + &
                  DVX_C(j)-DUY_C(j) + DVZ_C(j)-DWY_C(j) + &
                  DWX_C(j)-DUZ_C(j) + DWY_C(j)-DVZ_C(j)   )
     
       Mut(j) = 0.31*Rho*K / max(0.31*Omega,Vor*F22(j)) /MR

    end do


!*********************************************************************************************
 end 
!###########################################################################################

