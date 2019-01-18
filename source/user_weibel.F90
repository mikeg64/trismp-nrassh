!
! User module
!
! This module contains functions that may be altered by a user of the code, and that are called 
! if caseinit variable is set to a number greater than 0. The functions that are going to be 
! called in such a case are: SetEMFieldsUser, ..., ...
!
! If a user wants to alter the functions that are called he/she may also alter the module m_overload
! which branches with the variable caseinit.

#ifdef twoD 

module m_user

	use m_globaldata
	use m_system
	use m_aux
	use m_communications
	use m_fields
	use m_particles
	use m_inputparser
	use m_fparser
	use m_domain
	
#else

module m_user_3d

	use m_globaldata_3d
	use m_system_3d
	use m_aux_3d
	use m_communications_3d
	use m_fields_3d
	use m_particles_3d
	use m_inputparser_3d 
	use m_fparser_3d
	use m_domain_3d

#endif




	implicit none
		
	private

!-------------------------------------------------------------------------------
!	PARAMETERS
!-------------------------------------------------------------------------------

	
!-------------------------------------------------------------------------------
!	TYPE DEFINITIONS
!-------------------------------------------------------------------------------

!-------------------------------------------------------------------------------
!	VARIABLES
!-------------------------------------------------------------------------------

	real(sprec) :: temperature_ratio, sigma_ext, bz_ext0
	character(len=256) :: density_profile

!-------------------------------------------------------------------------------
!	INTERFACE DECLARATIONS
!-------------------------------------------------------------------------------
	
!-------------------------------------------------------------------------------
!	PUBLIC MODIFIERS
!-------------------------------------------------------------------------------

	public :: init_EMfields_user, init_particle_distribution_user, &
	inject_particles_user, read_input_user, field_bc_user, get_external_fields, &
	particle_bc_user

!-------------------------------------------------------------------------------
!	MODULE PROCEDURES AND FUNCTIONS
!-------------------------------------------------------------------------------

	contains



!-------------------------------------------------------------------------------
! 						subroutine read_input_shock		
!									
! Reads any variables related to (or needed by) this module
! 							
!-------------------------------------------------------------------------------

subroutine read_input_user()

	implicit none
	integer lextflds, luserpartbcs
	integer :: lmovwin, lwall

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!CHANGE THIS NAME IF YOU ARE CREATING A NEW USER FILE
!This helps to identify which user file is being compiled through Makefile. 
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
	if(rank.eq.0)print *, "Using user file user_weibel.F90"

	call inputpar_getd_def("problem", "temperature_ratio", 1._sprec, Temperature_ratio)
	call inputpar_gets_def("problem", "density_profile", "0", density_profile)

	call inputpar_getd_def("problem","sigma_ext",0._sprec,sigma_ext)

	call inputpar_geti_def("problem","external_fields",0,lextflds)

	if(lextflds .eq. 1) then 
	   external_fields =.true.
	else
	   external_fields =.false.
	endif

	if(external_fields) bz_ext0 = sqrt((gamma0)*.5*ppc0*c**2*(mi+me)*sigma_ext)

	call inputpar_geti_def("problem","user_part_bcs",0,luserpartbcs)

	if(luserpartbcs .eq. 1) then 
	   user_part_bcs = .true.
	else
	   user_part_bcs = .false.
	endif

	call inputpar_geti_def("problem", "caseinit", 0, caseinit)

	call inputpar_geti_def("problem", "wall", 0, lwall)
	
	if (lwall==1) then
		wall=.true.
	else
		wall=.false.
	endif
	
	if(wall) user_part_bcs=.true.


!overwrite conditions for weibel:
	wall=.false.
	movwin=.false.
	leftclean=0
	periodicx=1
	user_part_bcs=.false.
	enlarge=0.

!user read is called last, so any parameter can be overwritten here
	
end subroutine read_input_user

!-------------------------------------------------------------
!     Compute external fields to be added to the mover. 
!     These fields do not evolve via Maxwell Eqs, but can depend on time
!-------------------------------------------------------------
	subroutine get_external_fields(x,y,z,ex_ext, ey_ext, ez_ext, bx_ext,by_ext,bz_ext)
	
	real,intent(inout):: bx_ext,by_ext,bz_ext, ex_ext, ey_ext, ez_ext
	real, intent(in):: x,y,z
	ex_ext=0.
	ey_ext=0.
	ez_ext=0.
	bx_ext=0.
	by_ext=0.
	bz_ext=bz_ext0 !defined in the input reading part above 
	
	end subroutine get_external_fields


!-------------------------------------------------------------------------------
! 						subroutine parse_density_profile_function()
!												
! Parses the mathematical function that defines the density profile, defined in
! the input file as density_profile
!-------------------------------------------------------------------------------

subroutine parse_density_profile_function(use_density_profile)

	implicit none
	
	! dummy variables
	
	logical, intent(out) :: use_density_profile
	
	! local variables
	
	character(len=1), dimension(3) :: vars=(/'x','y','z'/)
	logical, save :: initialized=.false.

	use_density_profile=.true.
		
	if (density_profile=="0") then
		use_density_profile=.false.
		return
	endif
	
	
	if (.not. initialized) then	
		call initf(10)
		call parsef (1, density_profile, vars)
		initialized=.true.
	endif
	
end subroutine parse_density_profile_function



!-------------------------------------------------------------------------------
! 						subroutine init_EMfields_shock		 
!												
! Sets the electromagnetic fields of any specific user purpose
!							
!-------------------------------------------------------------------------------

subroutine init_EMfields_user()
	
	! local variables
	
	integer :: i, j, k, jglob, kglob
	real(sprec) :: beta0, betacur
	
	!determine initial magnetic field based on magnetization sigma which 
        !is magnetic energy density/ kinetic energy density
	!this definition works even for nonrelativistic flows. 
	
	btheta=btheta/180.*pi
	bphi=bphi/180.*pi
	
	Binit=sqrt((gamma0 )*ppc0*.5*c**2*(mi+me)*sigma)

	!initialize B field to be set by Binit and the inclination angle -- used for shocks

	do  k=1,mz
		do  j=1,my
			do  i=1,mx

				jglob=j+modulo(rank,sizey)*(myall-5) !global j,k coords in 
				kglob=k+(rank/sizey)*(mzall-5)                   !case need global variation of fields
				
				bx(i,j,k)=Binit*cos(btheta) 
				by(i,j,k)=Binit*sin(btheta)*sin(bphi)
				bz(i,j,k)=Binit*sin(btheta)*cos(bphi)

				ex(i,j,k)=0.
				ey(i,j,k)=(-beta)*bz(i,j,k) 
				ez(i,j,k)=-(-beta)*by(i,j,k)

			enddo
		enddo
	enddo
	
end subroutine init_EMfields_user


!-------------------------------------------------------------------------------
! 						subroutine init_particle_distribution_shock()	
!											
! Sets the particle distrubtion for a user defined case
!
!-------------------------------------------------------------------------------

subroutine init_particle_distribution_user()

	implicit none

	! local variables
	
	real(sprec), dimension(pdf_sz) :: func
	real(sprec) :: maxg, delgam1
	integer :: i, n, direction
	real       b00,db0,vpl,vmn,gam, cossq, rad, gamma_drift, delgam_i, delgam_e
	real betap, real, tmp, ppc
	real(dprec) :: Lps,pps 
	real numps,kps,ups, weight
	logical :: use_density_profile

	real(dprec) :: x1,x2,y1,y2,z1,z2

	call parse_density_profile_function(use_density_profile)
	
	call init_split_parts() !set split points for particle splits, not used unless splitpart is set to true in input
	
	pcosthmult=0 !if 0 and 2D run, the Maxwellian distribution corresponding to 
	             ! temperature is initialized in 2D, 1 for 3D. 
	             !when sigma > 0, it is set to 3D automatically

	! -------- Set particle boundaries ------------
	!set initial injection points for shocks 
	
	xinject=3 
	xinject2=(mx0-2) 

	! ------------------------------------------

	totalpartnum=0 !for purpose of keeping track of the total number of particles injected on this cpu

!initialize left going beam

	gamma_drift=-gamma0 ! negative gamma_drift will send the plasma in the negative direction
	delgam_i=delgam
	delgam_e=delgam*mi/me*Temperature_ratio

	x1=xinject  
	x2=xinject2 

	y1=3. !in global coordinates
	y2=my0-2.  
	z1=3.
	z2=mz0-2. !if 2D, it will reset automatically
	ppc=ppc0/2. 
	weight=1.

	direction=1 !drift along x, + or - determined by the sign of gamma_drift
	call inject_plasma_region(x1,x2,y1,y2,z1,z2,ppc,&
             gamma_drift,delgam_i,delgam_e,weight,use_density_profile,direction)
	
        if(periodicx .eq. 1) then 
	   x1in=3		!set the location of planes where particles 
                                !are removed from simulation, perpendicular to x. 
	   x2in=mx0-2
	endif


!initialize left going beam

	gamma_drift=gamma0 ! negative gamma_drift will send the plasma in the negative direction
	delgam_i=delgam
	delgam_e=delgam*mi/me*Temperature_ratio

	x1=xinject  
	x2=xinject2 

	y1=3. !in global coordinates
	y2=my0-2.  
	z1=3.
	z2=mz0-2. !if 2D, it will reset automatically
	ppc=ppc0/2. 
	weight=1.

	direction=1 !drift along x, + or - determined by the sign of gamma_drift
	call inject_plasma_region(x1,x2,y1,y2,z1,z2,ppc,&
             gamma_drift,delgam_i,delgam_e,weight,use_density_profile,direction)
	
        if(periodicx .eq. 1) then 
	   x1in=3		!set the location of planes where particles are removed from simulation, perpendicular to x. 
	   x2in=mx0-2
	endif

	call check_overflow()
	call reorder_particles()
	
end subroutine init_particle_distribution_user


!-------------------------------------------------------------------------------
! 				subroutine inject_particles_shock()					 
!										
! Injects new particles in the simulation on every step. To skip injection, set ppc=0 below
!
!-------------------------------------------------------------------------------

subroutine inject_particles_user()

	implicit none
	real(dprec) :: x1,x2,y1,y2,z1,z2
	real delgam_i, delgam_e, injector_speed, ppc, betainj, gamma_drift, weight
	logical use_density_profile

	use_density_profile=.false. !no profile possible in injection now

	injectedions=0 !set counter to 0 here, so that all possible other injections on this step add up
	injectedlecs=0

!no injection is needed for weibel case
	
end subroutine inject_particles_user



!-------------------------------------------------------------------------------
! 				subroutine field_bc_shock()	
!										
! Applies boundary conditions specific to user problem. 
! 
!-------------------------------------------------------------------------------

	subroutine field_bc_user()
	implicit none

!reset fields on the right end of the grid, where the plasma is injected
!make it drifting fields, even though there is no plasma there

!no special field BCs needed. Periodic conditions are taken care off in fieldboundaries

	end subroutine field_bc_user

!------------------------------------------------------------------------------

	subroutine particle_bc_user()
	implicit none
	real invgam, walloc, gammawall, betawall, gamma, x0, walloc0,xcolis,t0,t1
	integer n1,i0,i1, iter

	!loop over particles to check if they crossed special boundaries, like reflecting walls
	!outflow and periodic conditions are handled automatically in deposit_particles
	!
	!This routine is called after the mover and before deposit, thus allowing to avoid
        ! charge deposition behind walls. 
	
	
	   do iter=1,2
	      if(iter.eq.1) then 
		 i0=1
		 i1=ions
	      else
		 i0=maxhlf+1
		 i1=maxhlf+lecs
	      endif
	      
	      do n1=i0,i1
		 
! no special conditions needed, this routine is not called if user_part_bcs=0 in input file
		 
	      enddo
	   enddo
	   
	end subroutine particle_bc_user
	

	
#ifdef twoD
end module m_user
#else
end module m_user_3d
#endif

