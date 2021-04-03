
Module MeshVar 
implicit none

real(8)::T2,T1
integer::NC !Number of Cells of mesh
integer::NP  !Number of Existing Points
integer::NF !Number of Faces Constructing Mesh  !Number of Faces Constructing Mesh 
integer::NR   !Number Of Regions of mesh
 
integer,allocatable,dimension(:)::NFR !Number of  Face of each Regions
integer,allocatable,dimension(:)::BC  !Boundary Condition index
integer,allocatable,dimension(:,:)::IDS !Information of Data Structured (1,i):Left Cell,(2,i):Right Cell,(3:FaceType,i):Forming Point of Face
integer,allocatable,dimension(:)::FaceType !Type of Face (Triangle or Rectangle)
real(8),allocatable,dimension(:)::X,Y,Z !Coordinate of Points Constructing Mesh !Coordinate of Points Constructing Mesh
 
integer::NF1,NF2 !Index of 1st and last Non-Boundary Faces
integer::NFW1,NFW2 !Index of 1st and last Faces on Wall Boundary 
integer::NFF1,NFF2 !Index of 1st and Last Faces on Far-Field Boundary
integer::NFI1,NFI2 !Index of 1st and Last Faces on Inflow Boundary 
integer::NFS1,NFS2 !Index of 1st ans Last Faces on Symmetry Boundary
integer::NFO1,NFO2 !Index of 1st and Last Faces on Inflow Boundary
integer::NFIF1,NFIF2 !Index of 1st and last Faces on InterFsce Boundary
 
real(8),allocatable,dimension(:,:),target::CellGeoProprty
real(8),pointer,dimension(:)::Xc
real(8),pointer,dimension(:)::Yc
real(8),pointer,dimension(:)::Zc !Coordinate of Center of Element
real(8),pointer,dimension(:)::Vol !Volume of each Cell
 
real(8),allocatable,dimension(:)::NX
real(8),allocatable,dimension(:)::NY
real(8),allocatable,dimension(:)::NZ

real(8),allocatable,dimension(:)::DA   !Area of each Face
real(8),allocatable,dimension(:)::Xmid
real(8),allocatable,dimension(:)::Ymid
real(8),allocatable,dimension(:)::Zmid
 
integer,allocatable,dimension(:)::N_Corn !Number of point Forming a Cell
integer,allocatable,dimension(:,:)::Corn 
 
integer,allocatable,dimension(:)::InxNearWall  !Index of Nearest Wall
real(8),allocatable,dimension(:)::DisWall  !Distance to Nearest Wall
 
integer,allocatable,dimension(:)::nFaceCell
integer,allocatable,dimension(:,:)::iFaceCell
integer,allocatable,dimension(:,:)::DirFaceCell
 
integer,allocatable,dimension(:)::nConnectedFaces
integer,allocatable,dimension(:)::iConnectedFaces

contains 
 





   
subroutine AllocateMeshVar()
implicit none
    
allocate ( NFR(1:NR) )
allocate ( BC(1:NR) )
allocate ( FaceType(1:NF) )
allocate ( IDS(1:6,1:NF) )
allocate ( X(1:NP) )
allocate ( Y(1:NP) )
allocate ( Z(1:NP) )

allocate ( CellGeoProprty(1:NC,1:4) )
XC   => CellGeoProprty(:,1)
YC   => CellGeoProprty(:,2)
ZC   => CellGeoProprty(:,3)
Vol  => CellGeoProprty(:,4)


allocate ( NX(1:NF) )
allocate ( NY(1:NF) )
allocate ( NZ(1:NF) )
 
allocate ( Xmid(1:NF) )
allocate ( Ymid(1:NF) )
allocate ( Zmid(1:NF) )
 
allocate ( DA(1:NF) )
 
allocate ( N_Corn(1:NC) )
allocate ( Corn(1:8,1:NC) )
 
allocate ( InxNearWall(1:NC) )
allocate ( DisWall(1:NC) )

allocate( nFaceCell(1:NC)       )
allocate( iFaceCell(1:NC,1:8)   )
allocate( DirFaceCell(1:NC,1:8) )


end subroutine
 
 
 
 
 
   
end module
    
    