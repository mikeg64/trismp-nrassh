!
! Fields module
!
! This module contains the field solver routines and field related variables 
!
!
!

#ifdef twoD 

module m_fields

	use m_system
	use m_aux
	use m_communications
	use m_globaldata
	use m_inputparser
	
#else

module m_fields_3d

	use m_system_3d
	use m_aux_3d
	use m_communications_3d
	use m_globaldata_3d
	use m_inputparser_3d
	
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

	! variables controling the boundary conditions

	integer :: radiationx, radiationy, radiationz, periodicx, periodicy, periodicz

	! physical constants relevant for the code
	
	real(sprec) :: c, corr ! speed of light and correction for the speed of light
	
	real(sprec) :: xinject, xinject2, x1in, x2in !xinject -- where particles launched from, xin1,2 -- where particles should be removed from
	
	logical :: wall
	
	logical :: highorder ! order of finite differencing (true == 4th order, false=2nd)

	logical :: movwin ! controls the moving window
	
	real(sprec) :: movwingam, wallgam

	real(sprec) ::  Binit, Btheta, Bphi, beta, alpha !, Binit_default

	integer :: ix, iy, iz, lot ! linear grid indexes

	integer ::  ntimes, cleanint, cleanfld, mxrest


	integer ::mx,my,mz
	
	integer(8) :: mx0,my0,mz0, mzall, myall, mylast, mzlast
			
	real, allocatable, dimension(:,:,:) :: ex, ey, ez, bx, by, bz
	
	real, allocatable, dimension(:,:,:) :: bufferin1, bufferin2, &
	bufferin1y, bufferin2y, bufferin1x, bufferin2x, sendbufy, sendbufz
	
	real, allocatable, dimension(:,:,:) :: curx, cury, curz

	real, allocatable, dimension(:,:,:) :: temp 

	character (len=70) :: frestartfldlap

!-------------------------------------------------------------------------------
!	INTERFACE DECLARATIONS
!-------------------------------------------------------------------------------
	
!-------------------------------------------------------------------------------
!	PUBLIC MODIFIERS
!-------------------------------------------------------------------------------

	! public functions
	
	public :: advance_Efield, advance_Bhalfstep, add_current, allocate_fields, read_input_grid, &
	read_input_fields, reset_currents

	public :: radiationx, radiationy, radiationz, periodicx, periodicy, periodicz, &
			  wall, wallgam, bx, by, bz, ex, ey, ez, curx, cury, curz, ix, iy, iz, lot, &
			  beta, xinject, xinject2, mx, my, mz, c, binit, btheta, bphi, & ! used in particles module
			  movwin, movwingam, &  !Binit_default, & 															  ! used in particles module
			  bufferin1, bufferin2, bufferin1y, bufferin2y, bufferin1x, bufferin2x, sendbufy, &   ! used in domain module
			  sendbufz, temp, mx0, my0, mz0, mzall, myall, mylast, mzlast, &					  ! used in domain module
			  alpha, ntimes, cleanfld, cleanint, frestartfldlap, mxrest, &						  ! used in outputs module
			  highorder, corr, x1in, x2in
	

!-------------------------------------------------------------------------------
!	MODULE PROCEDURES AND FUNCTIONS
!-------------------------------------------------------------------------------

	contains



!-------------------------------------------------------------------------------
! 						subroutine read_input_grid					 
!																		
! Reads any variables related to (or needed by) this module
!							
!-------------------------------------------------------------------------------

subroutine read_input_grid()

	implicit none

	! local variables
	
	integer :: lmx0, lmy0, lmz0


	call inputpar_geti_def("grid", "mx0", 100, lmx0)
	call inputpar_geti_def("grid", "my0", 100, lmy0)
	call inputpar_geti_def("grid", "mz0", 1, lmz0)

	mx0=lmx0+5
	my0=lmy0+5
	mz0=lmz0+5

end subroutine read_input_grid



!-------------------------------------------------------------------------------
! 						subroutine read_input_fields					 
!																		
! Reads any variables related to (or needed by) this module
!							
!-------------------------------------------------------------------------------

subroutine read_input_fields()

	implicit none
	
	call inputpar_getd_def("fields", "btheta", 90._sprec, btheta)
	call inputpar_getd_def("fields", "bphi", 0._sprec, bphi)

end subroutine read_input_fields



!-------------------------------------------------------------------------------
! 						subroutine allocate_fields					 
!						
! Allocates variables needed for the grid and fields
!							
!-------------------------------------------------------------------------------

subroutine allocate_fields()

	implicit none
	

#ifdef twoD        
	mz0=1
#endif

	call read_restart_Ncell_info()
	
	if(modulo(int(my0-5),sizey).ne.0) then
		print *,rank,":","my indivisible by number of processors in the y direction",sizey, my
	endif
	if(modulo(int(mz0-5),sizez).ne.0) then
		print *,rank,":","mz indivisible by number of processors in the z direction",sizez, mz
	endif
	
	!there are 5 ghost cells in each direction: 1,2,(real domain), mx-2, mx-1, mx
	
	mx=mx0
	my=(my0-5)/sizey+5 !my0
	mz=(mz0-5)/sizez+5 !(mz0-5)/size0+5
	mzall=mz
	myall=my
	
	!give the last processor the rest of the cells -- in the unlikely event that 
	!the number of cpus does not divide domain evenly
	!PARALLEL note: we split the y and z direction among processors -- slabs.
	!x is always contained on each processor completely. 
	
	mzlast=mz0-((mz0-5)/sizez)*(sizez-1)
	mylast=my0-((my0-5)/sizey)*(sizey-1)
	
	if(rank .eq. size0-1) mz=mzlast
	
	!check if the proc is in the last row in z
	
	if(rank/sizey+1 .eq. sizez)  mz=mzlast
	
	!check if the proc is in the last column in y
	if(modulo(rank,sizey)+1 .eq. sizey)  my=mylast
		
	ix=1
	iy=mx
	iz=iy*my
	lot=iz*mz
	
#ifdef twoD
		iz=0
		lot=mx*my
#endif

	! allocate field buffers
	
	allocate(curx(mx,my,mz),cury(mx,my,mz),curz(mx,my,mz))
	allocate( ex(mx,my,mz),ey(mx,my,mz),ez(mx,my,mz), &
	bx(mx,my,mz),by(mx,my,mz),bz(mx,my,mz))
	
	
		
	allocate(temp(mx,my,mz)) !for filter

	allocate( bufferin1(mx,my,2),bufferin2(mx,my,2), &
	bufferin1y(mx,2,mz),bufferin2y(mx,2,mz), &
	bufferin1x(2,my,mz),bufferin2x(2,my,mz), &
	sendbufy(mx,2,mz),sendbufz(mx,my,2))


end subroutine allocate_fields

!-------------------------------------------------------------------------------
! 						subroutine read_restart_Ncell_info					 
!
! When simulation is restarting, this function will be called, and mxrest,my,mz0,mz 							
! are read
!-------------------------------------------------------------------------------

subroutine reset_currents()
	implicit none

	curx=0.
	cury=0.
	curz=0.

end subroutine reset_currents

!-------------------------------------------------------------------------------
! 						subroutine read_restart_Ncell_info					 
!
! When simulation is restarting, this function will be called, and mxrest,my,mz0,mz 							
! are read
!-------------------------------------------------------------------------------

subroutine read_restart_Ncell_info()

	implicit none
	
	! local variables
	
	integer :: ierr
	
	if (irestart .ne. 1) return
	
!integer ::token, request,status(MPI_STATUS_SIZE),request1,status1(MPI_STATUS_SIZE), ierr
!#ifdef MPI
!		!all of this for systems that can be swamped by too many cpus accessing the disk at once
!		goto 631 !try without the sempahore
!
!		!if rank non-divisible by 10 wait for the token from rank-1 and immediately 
!		!trigger next rank. Wait until read file only every 10th rank. 
!
!		if(rank .gt. 0) then 
!			token=rank-1
!			!wait for the message from the previous rank
!			call mpi_irecv(token,1,mpi_integer,rank-1,100,MPI_Comm_WORLD,request,ierr)
!			call mpi_wait(request,status,ierr)
!			
!			print *,"rank ",rank," received the token from ",rank-1
!			
!			if(modulo(rank,10).ne.0 .and. rank .ne. size0-1) then
!				token=rank
!				call mpi_isend(token,1,mpi_integer,rank+1,100,MPI_Comm_WORLD,request1,ierr)
!				call mpi_wait(request1,status1,ierr)
!			endif
!		endif
!		631        continue
!#endif
	
	open(unit=7,file=frestartfldlap,form='unformatted')
	
	rewind(7)
	read(7) mxrest,my,mz0,mz
	close(7)
	
	if(mxrest .gt. mx0) then
		mx0=mxrest
	endif
	
	print *,rank,": in init_restart, read field info, mx,my,mz0",mxrest,my,mz0,mz
	
	
#ifdef MPI
	call mpi_barrier(MPI_COMM_WORLD,ierr)
#endif

end subroutine read_restart_Ncell_info



!-------------------------------------------------------------------------------
! 						subroutine advance_b_halfstep					 
!																		
! Advances the B field half time step
!							
!-------------------------------------------------------------------------------

subroutine advance_b_halfstep()

	implicit none

	! local variables
		
	integer ::k1,k2,j1,j2,i1,i2, kp1, ip1, jp1, km1, jm1, im1, i, j, k
	real betx, alpx, const, const2, const3
	logical :: pukhov
	
	if(periodicx.eq.1) then
		i1=3
		i2=mx-3
	else
		i1=1
		i2=mx-1
	endif
	
	if(periodicy.eq.1) then
		j1=3
		j2=my-3
	else
		if(modulo(rank,sizey).eq.0) then
			j1=1
			j2=my-3
		endif
		if(modulo(rank,sizey).eq.sizey-1)then
			j1=3
			j2=my-1
		endif
		if(size0.eq.1)then
			j1=1
			j2=my-1
		endif
	endif
	
#ifndef twoD
		if(periodicz.eq.1) then
			k1=3
			k2=mz-3
		else
			if(rank/sizey.eq.0) then
				k1=1
				k2=mz-3
			endif
			if(rank/sizey.eq.sizez-1)then
				k1=3
				k2=mz-1
			endif
			if(size0.eq.1)then
				k1=1
				k2=mz-1
			endif
		endif
#else
		k1=1
		k2=1
#endif
	
	const=corr*(.5*c)
	
#ifndef twoD      
			do k=k1,k2           !3,mz-3 !1,mz-1 !3,mz-3
				kp1=k+1
				do j=j1,j2       !3,my-3 !1,my-1 !3,my-3
					jp1=j+1
					do i=i1,i2   !1,mx-1 !3,mx-3 !1,mx-1
						ip1=i+1
						bx(i,j,k)=bx(i,j,k)+const*(ey(i,j,kp1)-ey(i,j,k)- &
						ez(i,jp1,k)+ez(i,j,k))
						by(i,j,k)=by(i,j,k)+const*(ez(ip1,j,k)-ez(i,j,k)- &
						ex(i,j,kp1)+ex(i,j,k))
						bz(i,j,k)=bz(i,j,k)+const*(ex(i,jp1,k)-ex(i,j,k)- &
						ey(ip1,j,k)+ey(i,j,k))
					enddo
				enddo
			enddo
#else
			k=1
				do j=j1,j2			!3,my-3 !1,my-1 !3,my-3
					jp1=j+1
					do i=i1,i2		!1,mx-1 !3,mx-3 !1,mx-1
						ip1=i+1
						bx(i,j,k)=bx(i,j,k)+const*(-ez(i,jp1,k)+ez(i,j,k))
						by(i,j,k)=by(i,j,k)+const*(ez(ip1,j,k)-ez(i,j,k))
						bz(i,j,k)=bz(i,j,k)+const*(ex(i,jp1,k)-ex(i,j,k) &
						-ey(ip1,j,k)+ey(i,j,k))
					enddo
				enddo
#endif

end subroutine advance_b_halfstep



!-------------------------------------------------------------------------------
! 						subroutine advance_e_fullstep					 
!																		
! Advances the electric field a full time step
!							
!-------------------------------------------------------------------------------

subroutine advance_e_fullstep()

	implicit none
	
	! local variables

	integer ::k1,k2,j1,j2,i1,i2, kp1, jp1, ip1, km1, jm1, im1, i, j, k
	real const
	
	
	if(periodicx.eq.1) then
		i1=3
		i2=mx-3
	else
		i1=2
		i2=mx
	endif
	
	if(periodicy.eq.1) then
		j1=3
		j2=my-3
	else
		if(modulo(rank,sizey).eq.0) then
			j1=2
			j2=my-3
		endif
		if(modulo(rank,sizey).eq.sizey-1)then
			j1=3
			j2=my
		endif
		if(size0.eq.1)then
			j1=2
			j2=my
		endif
	endif
	
#ifndef twoD
		if(periodicz.eq.1) then
			k1=3
			k2=mz-3
		else
			if(rank/sizey.eq.0) then
				k1=2
				k2=mz-3
			endif
			if(rank/sizey.eq.sizez-1)then
				k1=3
				k2=mz
			endif
			if(size0.eq.1)then
				k1=2
				k2=mz
			endif
		endif
#else
		k1=1
		k2=1
#endif
	
	const=corr*c
	

#ifndef twoD
		do k=k1,k2		!3,mz-3 !2,mz !3,mz-3
			km1=k-1
			do j=j1,j2		!3,my-3 !2,mz !3,my-3
				jm1=j-1
				do i=i1,i2		!2,mx !3,mx-3 !2,mx
					im1=i-1
					ex(i,j,k)=ex(i,j,k)+const*(by(i,j,km1)-by(i,j,k)-bz(i,jm1,k)+bz(i,j,k))
					ey(i,j,k)=ey(i,j,k)+const*(bz(im1,j,k)-bz(i,j,k)-bx(i,j,km1)+bx(i,j,k))
					ez(i,j,k)=ez(i,j,k)+const*(bx(i,jm1,k)-bx(i,j,k)-by(im1,j,k)+by(i,j,k))
				enddo
			enddo
		enddo
#else
		k=1
		do j=j1,j2			!3,my-3 !2,mz !3,my-3
			jm1=j-1
			do i=i1,i2		!2,mx !3,mx-3 !2,mx
				im1=i-1
				ex(i,j,k)=ex(i,j,k)+const*(-bz(i,jm1,k)+bz(i,j,k))
				ey(i,j,k)=ey(i,j,k)+const*(bz(im1,j,k)-bz(i,j,k))
				ez(i,j,k)=ez(i,j,k)+const*(bx(i,jm1,k)-bx(i,j,k)- &
				by(im1,j,k)+by(i,j,k))
			enddo
		enddo
#endif

end subroutine advance_e_fullstep



!-------------------------------------------------------------------------------
! 						subroutine advance_b_halfstep_1D					 
!																		
! Advances the B field by half a time step, to be used in 1D
!  							
!-------------------------------------------------------------------------------

subroutine advance_b_halfstep_1D()

	implicit none
	
	! local variables
	
	integer :: k1,k2,j1,j2,i1,i2, ip1, i, j, k
	real const
	
	
	if(periodicx.eq.1) then
		i1=3
		i2=mx-3
	else
		i1=1
		i2=mx-1
	endif
	
	if(periodicy.eq.1) then
		j1=3
		j2=my-3
	else
		if(modulo(rank,sizey).eq.0) then
			j1=1
			j2=my-3
		endif
		if(modulo(rank,sizey).eq.sizey-1)then
			j1=3
			j2=my-1
		endif
		if(size0.eq.1)then
			j1=1
			j2=my-1
		endif
	endif
	
	if(periodicz.eq.1) then
		k1=3
		k2=mz-3
	else
		if(rank/sizey.eq.0) then
			k1=1
			k2=mz-3
		endif
		if(rank/sizey.eq.sizez-1)then
			k1=3
			k2=mz-1
		endif
		if(size0.eq.1)then
			k1=1
			k2=mz-1
		endif
	endif
	
	const=.5*c
	
	do k=k1,k2				!3,mz-3 !1,mz-1 !3,mz-3
		do j=j1,j2			!3,my-3 !1,my-1 !3,my-3
			do i=i1,i2		!1,mx-1 !3,mx-3 !1,mx-1
				ip1=i+1
				by(i,j,k)=by(i,j,k)+const*(ez(ip1,j,k)-ez(i,j,k)) !-0*ex(i,j,k+1)+0*ex(i,j,k))
				bz(i,j,k)=bz(i,j,k)+const*(ey(i,j,k)-ey(ip1,j,k)) !+0*ex(i,j+1,k)-0*ex(i,j,k))
			enddo
		enddo
	enddo 

end subroutine advance_b_halfstep_1D



!-------------------------------------------------------------------------------
! 						subroutine advance_e_fullstep_1D					 
!																		
! Advances the Electric field by a full time step (one dimension)
! 							
!-------------------------------------------------------------------------------

subroutine advance_e_fullstep_1D()

	implicit none
	
	! local variables
	
	integer :: k1,k2,j1,j2,i1,i2,im1, i, j, k
	
	if(periodicx.eq.1) then
		i1=3
		i2=mx-3
	else
		i1=2
		i2=mx
	endif
	
	if(periodicy.eq.1) then
		j1=3
		j2=my-3
	else
		if(modulo(rank,sizey).eq.0) then
			j1=2
			j2=my-3
		endif
		if(modulo(rank,sizey).eq.sizey-1)then
			j1=3
			j2=my
		endif
		if(size0.eq.1)then
			j1=2
			j2=my
		endif
	endif
	
	if(periodicz.eq.1) then
		k1=3
		k2=mz-3
	else
		if(rank/sizey.eq.0) then
			k1=2
			k2=mz-3
		endif
		if(rank/sizey.eq.sizez-1)then
			k1=3
			k2=mz
		endif
		if(size0.eq.1)then
			k1=2
			k2=mz
		endif
	endif
	
	do k=k1,k2			!3,mz-3 !2,mz !3,mz-3
		do j=j1,j2		!3,my-3 !2,mz !3,my-3
			do i=i1,i2	!2,mx !3,mx-3 !2,mx
				im1=i-1
				ey(i,j,k)=ey(i,j,k) + c *(bz(im1,j,k)-bz(i,j,k))  !-0*bx(i,j,k-1)+0*bx(i,j,k))
				ez(i,j,k)=ez(i,j,k) + c *(by(i,j,k)-by(im1,j,k)) !+0*bx(i,j-1,k)-0*bx(i,j,k)
			enddo
		enddo
	enddo

end subroutine advance_e_fullstep_1D



!-------------------------------------------------------------------------------
! 						subroutine advance_b_halfstep_42					 
!																		
! Advances B half time step, using higher an order method for added accuracy
!  							
!-------------------------------------------------------------------------------

subroutine advance_b_halfstep_42()

	implicit none
	
	! local variables
	
	integer :: k1,k2,j1,j2,i1,i2,istr,ifin, kp1, kp2, km1, jp1, jp2, jm1, ip1, &     
			   ip2, im1, i, j, k
	real(sprec) :: coef1, coef2, const
	
	
	const=corr*(.5*c)
	coef1=9./8.*corr*(.5*c)
	coef2=-1./24.*corr*(.5*c)
	
	if(periodicx.eq.1) then
		i1=3
		i2=mx-3
	else
		i1=2
		i2=mx-2
	endif
		
	if(wall) then
		istr=min(i2,int(xinject2)+10)
		i2=istr
	endif
	
	if(periodicy.eq.1) then
		j1=3
		j2=my-3
	else
		print *,"Fix boundary in advance_b_42"
		j1=1+1
		j2=my-1-1
	endif
	
#ifndef twoD
		if(periodicz.eq.1) then
			k1=3
			k2=mz-3
		else
			print *, "Fix boundary in advance_b_42"
			if(rank.eq.0) then
				k1=1    
				k2=mz-3
			endif
			if(rank.eq.size0-1)then
				k1=3
				k2=mz-1
			endif
			if(size0.eq.1)then
				k1=1 +1
				k2=mz-1 -1 
			endif
		endif
#else
		k1=1
		k2=1
#endif
	
#ifndef twoD
		do k=k1,k2!3,mz-3 !1,mz-1 !3,mz-3
			kp1=k+1
			kp2=k+2
			km1=k-1
			do j=j1,j2!3,my-3 !1,my-1 !3,my-3
				jp1=j+1
				jp2=j+2
				jm1=j-1
				do i=i1,i2!1,mx-1 !3,mx-3 !1,mx-1
					ip1=i+1
					ip2=i+2
					im1=i-1
					bx(i,j,k)=bx(i,j,k)+coef1*(ey(i,j,kp1)-ey(i,j,k)- &
					ez(i,jp1,k)+ez(i,j,k))+coef2*(ey(i,j,kp2)-ey(i,j,km1)- &
					ez(i,jp2,k)+ez(i,jm1,k))
					by(i,j,k)=by(i,j,k)+coef1*(ez(ip1,j,k)-ez(i,j,k)- &
					ex(i,j,kp1)+ex(i,j,k))+coef2*(ez(ip2,j,k)-ez(im1,j,k)- &
					ex(i,j,kp2)+ex(i,j,km1))
					bz(i,j,k)=bz(i,j,k)+coef1*(ex(i,jp1,k)-ex(i,j,k)- &
					ey(ip1,j,k)+ey(i,j,k))+coef2*(ex(i,jp2,k)-ex(i,jm1,k)- &
					ey(ip2,j,k)+ey(im1,j,k))
				enddo
			enddo
		enddo
#else
		k=1
		do j=j1,j2		!3,my-3 !1,my-1 !3,my-3
			jp1=j+1
			jp2=j+2
			jm1=j-1
			do i=i1,i2	!1,mx-1 !3,mx-3 !1,mx-1
				ip1=i+1
				ip2=i+2
				im1=i-1
				bx(i,j,k)=bx(i,j,k)+coef1*(-ez(i,jp1,k)+ez(i,j,k))+ &
				coef2*(-ez(i,jp2,k)+ez(i,jm1,k))
				by(i,j,k)=by(i,j,k)+coef1*(ez(ip1,j,k)-ez(i,j,k))+ &
				coef2*(ez(ip2,j,k)-ez(im1,j,k))
				bz(i,j,k)=bz(i,j,k)+coef1*(ex(i,jp1,k)-ex(i,j,k)-ey(ip1,j,k)+ &
				ey(i,j,k))+coef2*(ex(i,jp2,k)- &
				ex(i,jm1,k)-ey(ip2,j,k)+ey(im1,j,k))
			enddo
		enddo
#endif
	
	if(radiationx .eq. 1) then 
#ifndef twoD
			do k=k1,k2
				kp1=k+1
				do j=j1,j2
					jp1=j+1
					do i=1,mx-1,mx-2
						ip1=i+1
						bx(i,j,k)=bx(i,j,k)+const*(ey(i,j,kp1)-ey(i,j,k)- &
						ez(i,jp1,k)+ez(i,j,k))
						by(i,j,k)=by(i,j,k)+const*(ez(ip1,j,k)-ez(i,j,k)- &
						ex(i,j,kp1)+ex(i,j,k))
						bz(i,j,k)=bz(i,j,k)+const*(ex(i,jp1,k)-ex(i,j,k)- &
						ey(ip1,j,k)+ey(i,j,k))
					enddo
				enddo
			enddo
#else
			k=1
			do j=j1,j2
				jp1=j+1
				do i=1,mx-1,mx-2
					ip1=i+1
					bx(i,j,k)=bx(i,j,k)+const*(-ez(i,jp1,k)+ez(i,j,k))
					by(i,j,k)=by(i,j,k)+const*(ez(ip1,j,k)-ez(i,j,k))
					bz(i,j,k)=bz(i,j,k)+const*(ex(i,jp1,k)-ex(i,j,k)- &
					ey(ip1,j,k)+ey(i,j,k))
				enddo
			enddo
#endif
	endif
	
	
	goto 70 !use this to update the first cell FIX
	
	do k=k1,k2			 !3,mz-3 !1,mz-1 !3,mz-3
		kp1=k+1
		do j=j1,j2		 !3,my-3 !1,my-1 !3,my-3
			jp1=j+1
			do i=i1,i2	 !1,mx-1 !3,mx-3 !1,mx-1
				ip1=i+1
				bx(i,j,k)=bx(i,j,k)+const*(ey(i,j,kp1)-ey(i,j,k)- &
				ez(i,jp1,k)+ez(i,j,k))
				by(i,j,k)=by(i,j,k)+const*(ez(ip1,j,k)-ez(i,j,k)- &
				ex(i,j,kp1)+ex(i,j,k))
				bz(i,j,k)=bz(i,j,k)+const*(ex(i,jp1,k)-ex(i,j,k)- &
				ey(ip1,j,k)+ey(i,j,k))
			enddo
		enddo
	enddo
	
	70     continue
	
end subroutine advance_b_halfstep_42



!-------------------------------------------------------------------------------
! 						subroutine advance_e_fullstep_42					 
!																		
! Advances the Electric field a full time step, using a higher order scheme
!							
!-------------------------------------------------------------------------------

subroutine advance_e_fullstep_42()

	implicit none
	
	! local variables
	
	integer :: k1,k2,j1,j2,i1,i2, istr, ifin, km1, km2, kp1, jm1, jm2, jp1, im1,&
			   im2, ip1, i, j, k
	real coef1, coef2, const
	
	
	const=corr*c
	coef1=9./8.*corr*c
	coef2=-1./24.*corr*c
	
	if(periodicx.eq.1) then
		i1=3
		i2=mx-3
	else       
		i1=3
		i2=mx-1
	endif
		
	if(wall) then
		istr=min(i2,int(xinject2)+10)
		i2=istr
	endif
	
	if(periodicy.eq.1) then
		j1=3
		j2=my-3
	else
		j1=2 +1
		j2=my -1
	endif
	
#ifndef twoD
		if(periodicz.eq.1) then	
			k1=3
			k2=mz-3
		else
			print *, "FIX range in advance_e_42"
			if(rank.eq.0) then
				k1=2 +1
				k2=mz-3 
			endif
			if(rank.eq.size0-1)then
				k1=3
				k2=mz
			endif
			if(size0.eq.1)then
				k1=2
				k2=mz
			endif
		endif
#else
		k1=1
		k2=1
#endif
	
#ifndef twoD
		do k=k1,k2			!3,mz-3 !2,mz !3,mz-3
			km1=k-1
			km2=k-2
			kp1=k+1
			do j=j1,j2		!3,my-3 !2,mz !3,my-3
				jm1=j-1
				jm2=j-2
				jp1=j+1
				do i=i1,i2	!2,mx !3,mx-3 !2,mx
					im1=i-1
					im2=i-2
					ip1=i+1
					ex(i,j,k)=ex(i,j,k)+coef1*(by(i,j,km1)-by(i,j,k)- &
					bz(i,jm1,k)+bz(i,j,k))+coef2*(by(i,j,km2)-by(i,j,kp1)- &
					bz(i,jm2,k)+bz(i,jp1,k))
					ey(i,j,k)=ey(i,j,k)+coef1*(bz(im1,j,k)-bz(i,j,k)- &
					bx(i,j,km1)+bx(i,j,k))+coef2*(bz(im2,j,k)-bz(ip1,j,k)- &
					bx(i,j,km2)+bx(i,j,kp1))
					ez(i,j,k)=ez(i,j,k)+coef1*(bx(i,jm1,k)-bx(i,j,k)- &
					by(im1,j,k)+by(i,j,k))+coef2*(bx(i,jm2,k)-bx(i,jp1,k)- &
					by(im2,j,k)+by(ip1,j,k))
				enddo
			enddo
		enddo
#else
		k=1
		do j=j1,j2			!3,my-3 !2,mz !3,my-3
			jm1=j-1
			jm2=j-2
			jp1=j+1
			do i=i1,i2		!2,mx !3,mx-3 !2,mx
				im1=i-1
				im2=i-2
				ip1=i+1
				ex(i,j,k)=ex(i,j,k)+coef1*(-bz(i,jm1,k)+bz(i,j,k))+&      
				coef2*(-bz(i,jm2,k)+bz(i,jp1,k))
				ey(i,j,k)=ey(i,j,k)+coef1*(bz(im1,j,k)-bz(i,j,k))+&      
				coef2*(bz(im2,j,k)-bz(ip1,j,k))
				ez(i,j,k)=ez(i,j,k)+coef1*(bx(i,jm1,k)-bx(i,j,k)- &
				by(im1,j,k)+by(i,j,k))+coef2*(bx(i,jm2,k)- &
				bx(i,jp1,k)-by(im2,j,k)+by(ip1,j,k))
			enddo
		enddo
#endif
	
	if (radiationx .eq. 1) then 
#ifndef twoD
			do k=k1,k2!3,mz-3 !2,mz !3,mz-3
				km1=k-1
				do j=j1,j2!3,my-3 !2,mz !3,my-3
					jm1=j-1
					do i=2,mx,mx-2 !i1,i2!2,mx !3,mx-3 !2,mx
						im1=i-1
						ex(i,j,k)=ex(i,j,k)+const*(by(i,j,km1)-by(i,j,k)- &
						bz(i,jm1,k)+bz(i,j,k))
						ey(i,j,k)=ey(i,j,k)+const*(bz(im1,j,k)-bz(i,j,k)- &
						bx(i,j,km1)+bx(i,j,k))
						ez(i,j,k)=ez(i,j,k)+const*(bx(i,jm1,k)-bx(i,j,k)- &
						by(im1,j,k)+by(i,j,k))
					enddo
				enddo
			enddo
#else
			k=1
			do j=j1,j2!3,my-3 !2,mz !3,my-3
				jm1=j-1
				do i=2,mx,mx-2 !i1,i2!2,mx !3,mx-3 !2,mx
					im1=i-1
					ex(i,j,k)=ex(i,j,k)+const*(-bz(i,jm1,k)+bz(i,j,k))
					ey(i,j,k)=ey(i,j,k)+const*(bz(im1,j,k)-bz(i,j,k))
					ez(i,j,k)=ez(i,j,k)+const*(bx(i,jm1,k)-bx(i,j,k)- &
					by(im1,j,k)+by(i,j,k))
				enddo
			enddo
#endif
	endif
	
end subroutine advance_e_fullstep_42



!-------------------------------------------------------------------------------
! 						subroutine add_current					 
!																		
! Adds the current to the electric field
! 							
!-------------------------------------------------------------------------------

subroutine add_current()

	implicit none
		

	ex=ex+curx !minus sign is included in deposit
	ey=ey+cury
	ez=ez+curz


end subroutine add_current




!-------------------------------------------------------------------------------
! 						subroutine advance_Bhalfstep()					 
!																		
! Calls the B field advance routine
!
!-------------------------------------------------------------------------------

subroutine advance_Bhalfstep()

	implicit none

	if(.not. highorder) then 
		call advance_b_halfstep()
	else
		call advance_b_halfstep_42()
	endif

end subroutine



!-------------------------------------------------------------------------------
! 						subroutine advance_Efield()					 
!																		
! Advances the Electric field
!
!-------------------------------------------------------------------------------

subroutine advance_Efield()

	implicit none

	
	if(.not. highorder) then 
		call advance_e_fullstep()
	else
		call advance_e_fullstep_42()
	endif
	

end subroutine advance_Efield



#ifdef twoD
end module m_fields
#else
end module m_fields_3d
#endif
