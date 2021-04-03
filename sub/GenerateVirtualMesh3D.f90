!SSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
!*********************************************************************************************
 subroutine GenerateVirtualMesh3D() 
 use VirtualMesh
 implicit none
!********************************************************************************************* 
integer::j,i,Left,Rigt,Left_i,Left_j,Rigt_i,Rigt_j,P1,P2,P3,P4,P11,P22,P33,FacTyp,NFt,NewP  , Cell,iFac,nFace,NF_Tmp,Tmp

    

integer::h_NFv,h_NF2
real(8),allocatable,dimension(:)::Xv
real(8),allocatable,dimension(:)::Yv
real(8),allocatable,dimension(:)::Zv
integer,dimension(1:5,1:28) ::IDSt
real(8),allocatable,dimension(:)::FaceVal
integer,dimension(1:NC)::nVFacCel
integer,allocatable,dimension(:,:)::IDS_Tmp
allocate( IDS_Tmp(1:5,1:24*NC) )
!*********************************************************************************************

 do Cell=1,NC
 
    NFt = 0
 
    nFace=nFaceCell(Cell)
    do j=1,nFace
        iFac = iFaceCell(Cell,j) 

        FacTyp = FaceType(iFac)
 
        Left = IDS(1,iFac)
        Rigt = IDS(2,iFac)
        P1   = IDS(3,iFac)
        P2   = IDS(4,iFac)
        P3   = IDS(5,iFac)
        P4   = IDS(6,iFac)

        if(DirFaceCell(Cell,j)==-1 .and. FacTyp==4)then 
         Left = IDS(2,iFac)
         Rigt = IDS(1,iFac)
         P1   = IDS(6,iFac)
         P2   = IDS(5,iFac)
         P3   = IDS(4,iFac)
         P4   = IDS(3,iFac)
        elseif(DirFaceCell(Cell,j)==-1 .and. FacTyp==3)then
         Left = IDS(2,iFac)
         Rigt = IDS(1,iFac)
         P1   = IDS(5,iFac)
         P2   = IDS(4,iFac)
         P3   = IDS(3,iFac)
        endif
        
        NewP = NP + Left

        if(FacTyp==3)then
         NFt = NFt+1 ; IDSt(1,NFt) = iFac  ; IDSt(2,NFt) = 0  ; IDSt(3,NFt) = NewP ; IDSt(4,NFt) = P2 ; IDSt(5,NFt) = P1
         NFt = NFt+1 ; IDSt(1,NFt) = iFac  ; IDSt(2,NFt) = 0  ; IDSt(3,NFt) = NewP ; IDSt(4,NFt) = P3 ; IDSt(5,NFt) = P2
         NFt = NFt+1 ; IDSt(1,NFt) = iFac  ; IDSt(2,NFt) = 0  ; IDSt(3,NFt) = NewP ; IDSt(4,NFt) = P1 ; IDSt(5,NFt) = P3
        elseif(FacTyp==4)then
         NFt = NFt+1 ; IDSt(1,NFt) = iFac  ; IDSt(2,NFt) = 0  ; IDSt(3,NFt) = NewP ; IDSt(4,NFt) = P2 ; IDSt(5,NFt) = P1
         NFt = NFt+1 ; IDSt(1,NFt) = iFac  ; IDSt(2,NFt) = 0  ; IDSt(3,NFt) = NewP ; IDSt(4,NFt) = P3 ; IDSt(5,NFt) = P2
         NFt = NFt+1 ; IDSt(1,NFt) = iFac  ; IDSt(2,NFt) = 0  ; IDSt(3,NFt) = NewP ; IDSt(4,NFt) = P4 ; IDSt(5,NFt) = P3
         NFt = NFt+1 ; IDSt(1,NFt) = iFac  ; IDSt(2,NFt) = 0  ; IDSt(3,NFt) = NewP ; IDSt(4,NFt) = P1 ; IDSt(5,NFt) = P4
        endif

    end do  


    do i=1,NFt

        Left_i = IDSt(1,i)
        Rigt_i = IDSt(2,i)
        
        if(Rigt_i==-1)cycle
        
        P1 = IDSt(3,i)
        P2 = IDSt(4,i)
        P3 = IDSt(5,i)
    
        do j=i+1,NFt
            
           Left_j = IDSt(1,j)
           Rigt_j = IDSt(2,j)
           
           if(Rigt_j==-1)cycle
       
           P11 = IDSt(3,j)
           P22 = IDSt(4,j)
           P33 = IDSt(5,j)
       
           if(  (P1==P33 .and. P2==P22 .and. P3==P11) .or. (P1==P22 .and. P2==P11 .and. P3==P33) .or. (P1==P11 .and. P2==P33 .and. P3==P22)  )then
            IDSt(2,i) = Left_j
            IDSt(2,j) = -1
            exit
           end if
    
       end do 

    end do     

    NF_Tmp = (Cell-1)*12 
    nVFacCel(Cell) = 0
    do j=1,NFt
     
       if( IDSt(2,j)/=-1 )then
        NF_Tmp = NF_Tmp + 1  
        IDS_Tmp(1,NF_Tmp) = IDSt(1,j)
        IDS_Tmp(2,NF_Tmp) = IDSt(2,j)
        IDS_Tmp(3,NF_Tmp) = IDSt(3,j)
        IDS_Tmp(4,NF_Tmp) = IDSt(4,j)
        IDS_Tmp(5,NF_Tmp) = IDSt(5,j) 
        
        nVFacCel(Cell) = nVFacCel(Cell) + 1
       end if
    
    end do  
        
    
 end do !Cell    
  
 
 
 

 
   
 NFv = 0
 do j=1,NC
    NFv = NFv + nVFacCel(j)   
 end do
 NFv = NFv + NF - NF2
 
 call AllocatVirtualMesh(NFv)
  

 NFv = 0
 do Cell=1,NC
     
    Tmp = (Cell-1)*12 
    do j=Tmp+1,Tmp + nVFacCel(Cell)
        
     NFv = NFv+1   
     
     FaceTypev(NFv) = 3
     
     IDSv(1,NFv) = IDS_Tmp(1,j)
     IDSv(2,NFv) = IDS_Tmp(2,j)
     IDSv(3,NFv) = IDS_Tmp(3,j)
     IDSv(4,NFv) = IDS_Tmp(4,j)
     IDSv(5,NFv) = IDS_Tmp(5,j)
 
    end do
    
 end do
  
 
 do j=NF2+1,NF
     
     NFv = NFv+1  
     
     FaceTypev(NFv) = FaceType(j)
     
     IDSv(1,NFv) = j
     IDSv(2,NFv) = 0
     IDSv(3,NFv) = IDS(3,j)
     IDSv(4,NFv) = IDS(4,j)
     IDSv(5,NFv) = IDS(5,j)
     IDSv(6,NFv) = IDS(6,j)
     
 end do
 
 NCv = NF 
 
 NPv = NP + NC
  
 allocate( Xv(1:NPv)   )
 allocate( Yv(1:NPv)   )
 allocate( Zv(1:NPv)   )
 
 
 do i=1,NP
     XV(i) = X(i)
     YV(i) = Y(i)
     ZV(i) = Z(i)
 end do
 
 do i=1,NC
     XV(NP+i) = XC(i)
     YV(NP+i) = YC(i)
     ZV(NP+i) = ZC(i)
 end do
 

 call FaceOfCellVmesh()
 
 
 allocate( FaceVal(1:NFv)   )
 call VirtualFaceProprties3D(Xv,Yv,Zv,FaceVal)   ! NFv,FaceTypev,IDSv,NXv,NYv,NZv,
 
 
 call TrasFacValToCell(NCv,NFv,nFaceCellv,iFaceCellv,DirFaceCellv,1,FaceVal,Volv)
  
 
 deallocate( IDS_Tmp )
 deallocate( FaceVal )
 deallocate( Xv   )
 deallocate( Yv   )
 deallocate( Zv   )
!*********************************************************************************************
 end
!###########################################################################################