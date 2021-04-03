!SSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS 
 subroutine VirtualFaceProprties3D(Xv,Yv,Zv,Vol_Face)   !NFv,FaceTypev,IDSv,NXv,NYv,NZv,
 use VirtualMesh
 implicit none
!*********************************************************************************************
!!! Intent(In   )::NFv,FaceTypev,IDSv,Xv,Yv,Zv
!!! Intent(Out  )::NXv,NYv,NZv,Vol_Face
!!!
!!!integer                        ::NFv
!!!integer,dimension(:,:)         ::IDSv
!!!integer,dimension(:)           ::FaceTypev
real(8),dimension(1:NPv)           ::Xv
real(8),dimension(1:NPv)           ::Yv
real(8),dimension(1:NPv)           ::Zv
!!!real(8),dimension(:)           ::Nxv
!!!real(8),dimension(:)           ::Nyv
!!!real(8),dimension(:)           ::Nzv
real(8),dimension(1:NFv)           ::Vol_Face

integer                         ::i,j,K,P1,P2,P3,Left,Rigt,NFacePnt,IFace,FacTyp,Pt
real(8)                         ::a,b,c,X1,X2,X3,Y1,Y2,Y3,Z1,Z2,Z3,X21,X32,vol_tt,VOLUME,Xcc,Ycc,Zcc ,Xi,Yi,Zi
real(8)                         ::Y21,Y32,Z21,Z32,DV,SumX,SumY,SumZ,aX,bX,cX,aY,bY,cY,aZ,bZ,cZ,FaceArea,MagN
!*********************************************************************************************   

  do IFace=1,NFv

    DV = 0.0 
    FaceArea = 0.0 
    NFacePnt = FaceTypev(IFace)+2
    do i=4,NFacePnt-1

       P1= IDSv(3  ,IFace)
	   P2= IDSv(i  ,IFace)
	   P3= IDSv(i+1,IFace)

	   X1 = Xv(P1) ; Y1 = Yv(P1) ; Z1 = Zv(P1)
	   X2 = Xv(P2) ; Y2 = Yv(P2) ; Z2 = Zv(P2)
	   X3 = Xv(P3) ; Y3 = Yv(P3) ; Z3 = Zv(P3)

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
       
    end do 

    MagN = Dsqrt( a*a + b*b + c*c ) 
    NXv(IFace) = a / MagN
	NYv(IFace) = b / MagN
	NZv(IFace) = c / MagN

    NXv(IFace) = NXv(IFace) * FaceArea
	NYv(IFace) = NYv(IFace) * FaceArea
	NZv(IFace) = NZv(IFace) * FaceArea

    Vol_Face(IFace) = DV / 18
 
 end do

!********************************************************************************************* 
 end
!########################################################################################### 
