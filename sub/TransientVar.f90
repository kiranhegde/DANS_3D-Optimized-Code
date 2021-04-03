Module TransientVar
implicit none

real(8)::DT_min                        !Minimum Time Step
real(8)::TotTime                       !Total Time for Unsteady simulation
real(8)::Time_Coe                      !Time Coefficient for dual time stepping
real(8)::time                          !Simulation Time
real(8)::CFLx                          !Currant Number for Explicit Methods
integer::NRKS                          !Number of Runge Kutta Stages
real(8),dimension(1:5)::RKJ            !Runge Kutta Jameson Coefficient

real(8),allocatable,dimension(:)::DT   !Time step
  
real(8),allocatable,dimension(:,:),Target::ConservativeN 
real(8),allocatable,dimension(:,:),Target::ConservativeNP1
real(8),allocatable,dimension(:,:),Target::ConservativeF

real(8),allocatable,dimension(:,:),Target::Residual

contains

 
subroutine AllocateTransientVar(NC,NF,nTurb)
implicit none

integer::NC,NF,nTurb
 
allocate( DT(1:NC)  )
 
allocate( ConservativeN(1:NC,1:5+nTurb)   )
  
allocate( ConservativeNP1(1:NC,1:5+nTurb) )
allocate( ConservativeF(1:NF,1:5+nTurb)   )
 
allocate( Residual(1:NC,1:5+nTurb)        )
 
end subroutine
    
end module