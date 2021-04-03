!SSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS 
 subroutine FaceProprties3D(XC_Face,YC_Face,ZC_Face,Vol_Face)
 use MeshVar
 implicit none
!*********************************************************************************************

real(8),dimension(1:NF)           ::XC_Face
real(8),dimension(1:NF)           ::YC_Face
real(8),dimension(1:NF)           ::ZC_Face
real(8),dimension(1:NF)           ::Vol_Face

integer                         ::i,j,K,P1,P2,P3,Left,Rigt,NFacePnt,IFace,FacTyp,Pt
real(8)                         ::a,b,c,X1,X2,X3,Y1,Y2,Y3,Z1,Z2,Z3,X21,X32,vol_tt,VOLUME,Xcc,Ycc,Zcc ,Xi,Yi,Zi
real(8)                         ::Y21,Y32,Z21,Z32,DV,SumX,SumY,SumZ,aX,bX,cX,aY,bY,cY,aZ,bZ,cZ,FaceArea,MagN
!*********************************************************************************************   

 do IFace=1,NF
  
	FacTyp = FaceType(IFace)

	Xi = 0.0
    Yi = 0.0
    Zi = 0.0
	do i=1,FacTyp
	   Pt = IDS(i+2,IFace)
       
	   Xi = Xi + X(Pt)
       Yi = Yi + Y(Pt)
       Zi = Zi + Z(Pt)
	end do
	Xmid(IFace) = Xi/FacTyp
    Ymid(IFace) = Yi/FacTyp
    Zmid(IFace) = Zi/FacTyp


    DV =0.0 
    Xcc=0.0
    Ycc=0.0
    Zcc=0.0
    FaceArea = 0.0
    NFacePnt = FacTyp+2
    do i=4,NFacePnt-1

       P1= IDS(3  ,IFace)
	   P2= IDS(i  ,IFace)
	   P3= IDS(i+1,IFace)

	   X1 = X(P1) ; Y1 = Y(P1) ; Z1 = Z(P1)
	   X2 = X(P2) ; Y2 = Y(P2) ; Z2 = Z(P2)
	   X3 = X(P3) ; Y3 = Y(P3) ; Z3 = Z(P3)

       x21 = x2 - x1
       x32 = x3 - x2

       y21 = y2 - y1
       y32 = y3 - y2

       z21 = z2 - z1
       z32 = z3 - z2

       a = y21*z32 - z21 * y32
       b = z21*x32 - x21 * z32
       c = x21*y32 - y21 * x32

	   DV = DV + a*( x1+ x2+ x3 ) + b*( y1+ y2+ y3 ) + c*( z1+ z2+ z3 )

       FaceArea = FaceArea + 0.5*Dsqrt( a*a + b*b + c*c )
       
       Xcc = Xcc + a*((X1+X2)**2+(X2+X3)**2+(X1+X3)**2) 
       Ycc = Ycc + b*((Y1+Y2)**2+(Y2+Y3)**2+(Y1+Y3)**2) 
       Zcc = Zcc + c*((Z1+Z2)**2+(Z2+Z3)**2+(Z1+Z3)**2) 
   
    end do 

    MagN = Dsqrt( a*a + b*b + c*c ) 
    NX(IFace) = a / MagN
	NY(IFace) = b / MagN
	NZ(IFace) = c / MagN

    NX(IFace) = NX(IFace) * FaceArea
	NY(IFace) = NY(IFace) * FaceArea
	NZ(IFace) = NZ(IFace) * FaceArea

	DA(IFace) = FaceArea 

    XC_Face(IFace)  = Xcc
    YC_Face(IFace)  = Ycc
    ZC_Face(IFace)  = Zcc

    Vol_Face(IFace) = DV / 18

 end do
 
!********************************************************************************************* 
 end
!###########################################################################################
  