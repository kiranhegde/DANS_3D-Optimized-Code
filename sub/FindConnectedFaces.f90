!SSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
!*********************************************************************************************
 subroutine FindConnectedFaces()
 use MeshVar
 implicit none
!*********************************************************************************************
 integer::i,j,Point,Face,cnt
 

integer::nConctFac(1:NP)
integer::iConctFac(1:100,1:NP)
!*********************************************************************************************
!Part 1:
 nConctFac(:) = 0
 
 do Face=1,NF

    do j=3,(2+FaceType(Face))
       Point = IDS(j,Face)
       nConctFac(Point) = nConctFac(Point)  + 1

       iConctFac(nConctFac(Point),Point) = Face
    end do
    
 end do
 
 
 
 allocate( NConnectedFaces(1:NP+1) )
 
 cnt = 0
 Do I=1,NP
    cnt = cnt + nConctFac(I)
 End do
 Allocate( IConnectedFaces(1:cnt) )


 NConnectedFaces(1) = 0
 cnt = 0
 Do I=1,NP

    Do J=1,nConctFac(I)
       cnt      = cnt+1
       IConnectedFaces(cnt) = iConctFac(J,I)
    End Do
    NConnectedFaces(I+1)=NConnectedFaces(I)+nConctFac(I)
    
 End do
 
!*********************************************************************************************
    end
!###########################################################################################
