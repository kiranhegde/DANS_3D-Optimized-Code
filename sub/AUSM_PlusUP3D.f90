!*********************************************************************************************
 subroutine AUSM_PlusUP3D(FacInx,NXX,NYY,NZZ,U,V,W,ConMass,ConMom1,ConMom2,ConMom3,ConEnrg)
 use MeshVar
 use MeanFlowVar
 implicit none
!*********************************************************************************************

 integer::FacInx,Left,Rigt,L,R,F
 real(8)::Alpha,Beta,Ku,Kp,Const1,Sigma,Temp1,Temp2,Temp3,U,V,W,Q,Pm,Rho_L,Rho_R,u_L,u_R,&
          v_L,v_R,w_L,w_R,P_L,P_R,a_L,a_R,astar_L,astar_R,Utot_L,Utot_R,Ucontra_L,Ucontra_R,&
          a_Face,Ma_L,Ma_R,Ma_Plus_L,Ma_minus_R,Ma_bar,Mo,M_Co,fa,Ma_p,Ma_face,MassFlux,&
          P_plus_L,P_minus_R,P_u,P_face,H_L,H_R,nx_norm,ny_norm,nz_norm,DAA,Kapa,NXX,NYY,NZZ

 real(8),intent(out)::ConMass,ConMom1,ConMom2,ConMom3,ConEnrg
!*********************************************************************************************	
!Part1 :
 Beta   = 0.125
 Ku     = 0.75
 Kp     = 0.25
 Sigma  = 1.0
 Kapa   = 4.0
 Const1 = 2.0 * (GM - 1.0 ) / ( GM + 1.0)


L = IDS(1,FacInx)
R = IDS(2,FacInx)

DAA = DA(FacInx)
nx_norm = NXX/DAA
ny_norm = NYY/DAA
nz_norm = NZZ/DAA

!Part 11:
Rho_L = Ro(L)
U_L   = RU(L) / Rho_L
V_L   = RV(L) / Rho_L  
W_L   = RW(L) / Rho_L  
P_L   = P(L) 
     
!Part 12:
Rho_R = Ro(R)
U_R   = RU(R) / Rho_R
V_R   = RV(R) / Rho_R 
W_R   = RW(R) / Rho_R 
P_R   = P(R)
    
!Part 13:
!Left
Utot_L  = u_L*u_L + v_L*v_L + w_L*w_L
H_L     = ( (GM * P_L / Rho_L) / (GM - 1.0) ) + (0.5 * Utot_L)
astar_L = Dsqrt(H_L  * Const1)

!Right
Utot_R  = u_R*u_R + v_R*v_R + w_R*w_R 
H_R     = ( ((GM * P_R / Rho_R) / (GM - 1.0) ) + (0.5 * Utot_R))

astar_R = Dsqrt(H_R  * Const1)    

!Part 14:
Ucontra_L = u_L*nx_norm + v_L*ny_norm  + w_L*nz_norm 
Ucontra_R = u_R*nx_norm + v_R*ny_norm  + w_R*nz_norm
    
!Part 15:
!Left
a_L = ( astar_L*astar_L ) / DMax1( astar_L , Ucontra_L )
a_R = ( astar_R*astar_R ) / DMax1( astar_R ,-Ucontra_R )

a_face = DMax1( a_L , a_R )

!Part 16:    
Ma_L = Ucontra_L / a_face
Ma_R = Ucontra_R / a_face    

!Part 17:    
if( ABS(Ma_L) >= 1.0 )then
    Ma_plus_L = 0.5 * ( Ma_L + ABS(Ma_L) )
Else 
    Ma_plus_L =  0.25 * ( ( Ma_L + 1.0 )**2.0 ) + Beta * ( ( (Ma_L**2.0) -1 ) ** 2.0 ) 
end if

if( ABS(Ma_R) >= 1.0 )then
    Ma_minus_R = 0.5 * ( Ma_R - ABS(Ma_R) )
Else 
    Ma_minus_R = -0.25 * ( ( Ma_R - 1.0 )**2.0 )  - Beta * ( ( (Ma_R**2.0) -1 ) ** 2.0 )  
end if    
       
!Part 18:
Ma_bar = 0.5 * ( Ma_L*Ma_L + Ma_R*Ma_R )  
Ma_bar =  ( Ucontra_L*Ucontra_L + Ucontra_R*Ucontra_R ) / ( 2*a_face*a_face )
  
!Part 19:
M_Co = Kapa*Minf*Minf !max(0.3,0.5*Minf)

Mo = min(1.0,max(Ma_bar,M_Co))
Mo = Dsqrt(Mo)   
fa = Mo * ( 2.0 - Mo )

!Part 20: 
Temp1 = 1.0 - (Sigma * Ma_bar )
Temp2 = 0.5 * ( Rho_L + Rho_R ) * ( a_face *a_face )
Ma_p = -(Kp/fa) * Max(Temp1,0.0) * ( P_R - P_L ) / Temp2        

!Part 21:
Ma_face = Ma_plus_L + Ma_minus_R + Ma_p     

!Part 22:    
if( Ma_face > 0.0 )then 
    MassFlux = a_face * Ma_face * Rho_L
Else
    MassFlux = a_face * Ma_face * Rho_R
end if         

!Part 23:
Alpha = ( 3.0 / 16.0 ) * ( -4.0 + 5.0 * fa * fa ) 
 
if( ABS(Ma_L) >= 1.0 )then
    P_plus_L = 0.5* ( 1 + Ma_L/ABS( Ma_L )  )
Else 
    P_plus_L = 0.25 * ( ( Ma_L+1.0 ) ** 2.0 ) * ( 2.0 - Ma_L ) + &
            Alpha * Ma_L* ( ( Ma_L** 2.0 - 1.0 ) ** 2.0 )
end if
   
if( ABS(Ma_R) >= 1.0 )then
    P_minus_R = 0.5* ( 1 - Ma_R/ABS( Ma_R )  )
Else 
    P_minus_R = 0.25 * ( ( Ma_R-1.0 ) ** 2.0 ) * ( 2.0 + Ma_R ) - &
                Alpha * Ma_R * ( ( Ma_R** 2.0 - 1.0 ) ** 2.0 )  
end if        

!Part 24:
P_u = - Ku*P_plus_L*P_minus_R*( Rho_L + Rho_R )*fa *a_Face*( Ucontra_R - Ucontra_L )    
      
!Part 25:
P_Face = P_plus_L * P_L + P_minus_R * P_R + P_u    

!Part 26:
if( Ma_face >= 0. )then 
    ConMass = ( MassFlux                      ) * DAA
    ConMom1 = ( MassFlux * u_L + P_Face * nx_norm ) * DAA
    ConMom2 = ( MassFlux * v_L + P_Face * ny_norm ) * DAA
    ConMom3 = ( MassFlux * w_L + P_Face * nz_norm ) * DAA
    ConEnrg = ( MassFlux * H_L                ) * DAA
Else
    ConMass = ( MassFlux                      ) * DAA 
    ConMom1 = ( MassFlux * u_R + P_Face * nx_norm ) * DAA
    ConMom2 = ( MassFlux * v_R + P_Face * ny_norm ) * DAA
    ConMom3 = ( MassFlux * w_R + P_Face * nz_norm ) * DAA
    ConEnrg = ( MassFlux * H_R                ) * DAA
end if          

!********************************************************************************************* 
    end
!###########################################################################################
