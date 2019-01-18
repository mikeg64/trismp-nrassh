!
! domain module
!
! This module contains routines that alter the simulation domain, such as the moving window, and 
! the routine that enlarges the simulation domain
!
!

#ifdef twoD 

module m_domain

	use m_system
	use m_aux
	use m_communications
	use m_fields
	use m_particles
	use m_globaldata
	use m_inputparser
	
#else

module m_domain_3d

	use m_system_3d
	use m_aux_3d
	use m_communications_3d
	use m_fields_3d
	use m_particles_3d
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

	integer :: enlarge, shiftint, shiftstart
	character (len=5) :: rankchar
	character (len=34) :: fenlargeloc
	
!-------------------------------------------------------------------------------
!	INTERFACE DECLARATIONS
!-------------------------------------------------------------------------------
	
!-------------------------------------------------------------------------------
!	PUBLIC MODIFIERS
!-------------------------------------------------------------------------------

	! public functions
	
	public ::  enlarge_domain, moving_window, read_input_domain

	! public variables (used in outputs)

	public :: shiftint, shiftstart, rankchar
	
	! public variables (used in initialize)
	
	public :: fenlargeloc, enlarge

!-------------------------------------------------------------------------------
!	MODULE PROCEDURES AND FUNCTIONS
!-------------------------------------------------------------------------------

	contains



!-------------------------------------------------------------------------------
! 						subroutine read_input_domain					 
!																		
! Reads any variables related to (or needed by) this module
! 							
!-------------------------------------------------------------------------------

subroutine read_input_domain()

	implicit none
	integer lmovwin

	call inputpar_geti_def("domain", "enlarge", 0, enlarge)	
	call inputpar_geti_def("domain", "movwin", 0, lmovwin)

	if (lmovwin==1) then
		movwin=.true.
	else
		movwin=.false.
	endif

	call inputpar_geti_def("domain", "shiftint", 20, shiftint)
	call inputpar_geti_def("domain", "shiftstart", 20, shiftstart)

	call inputpar_getd_def("domain", "movwingam", 20000._sprec, movwingam)	
	
	
end subroutine read_input_domain


!-------------------------------------------------------------------------------
! 						subroutine initialize_domain_default()					 
!																		
! Sets enlarge to 0 for the default problem
!							
!-------------------------------------------------------------------------------

!subroutine initialize_domain_default()
!
!	implicit none
!	
!	enlarge=0

!end subroutine initialize_domain_default



!-------------------------------------------------------------------------------
! 						subroutine enlarge_domain()					 
!																		
! Calls the enlarge_domain_with_disk subroutine, if needed
!							
!-------------------------------------------------------------------------------

subroutine enlarge_domain()

	implicit none
	
	if(enlarge .ne. 1) return 
	
#ifndef twoD 
	if(.not.wall .or. xinject2 .le. mx0-300 ) return 
#else
	if(xinject2 .le. mx0-500 .or. periodicx .ne. 0) return 
#endif
	
	if(debug) print *, rank, ": b4 enlarge"
	call enlarge_domain_with_disk()	


end subroutine enlarge_domain



!-------------------------------------------------------------------------------
! 						subroutine enlarge_domain_with_disk()					 
!													
! Enlarges the simulation domain as needed
!							
!-------------------------------------------------------------------------------

subroutine enlarge_domain_with_disk()

	implicit none
	external m_overload_mp_init_EMfields
	
	! local variables

	integer :: mxold, idummy, i , j, k, ierr
	character*20 testout
	

	mxold=mx
	
	mx=mxold+500 !0
	mx0=mx
	
	if(rank .eq. 0) print *, "outin-s done"
	iy=mx
	iz=iy*my
	lot=iz*mz
#ifdef twoD
		iz=0
		lot=mx*my
#endif
	
#ifndef twoD
		buffsize = max(1*int(ppc0*c*max((mx-5)*(my-5),1*(mx-5)*(mz-5))) &
		,10000)
#else
		buffsize = max(3*int(ppc0*c*(1*(mx-5))),60000)
		if (splitparts) buffsize=buffsize*150 
#endif

	testout="err"//"."//trim(rankchar)
	
	if(rank .eq. 0) print *, "Enlarging domain!", mxold, mx
	
	!first save the arrays to disk as if I am doing restart.
	call mpi_barrier(MPI_COMM_WORLD,ierr)
	
	open(unit=7,file=fenlargeloc, form='unformatted')
	rewind 7
	if(rank.ge.0)print *, rank, ": writing enlarge file"
	write(20,*)rank,"; writing enlarge file"
	close(20)

	write(7)(((bx(i,j,k),i=1,mxold-3),j=1,my),k=1,mz), &
	(((by(i,j,k),i=1,mxold-3),j=1,my),k=1,mz), &
	(((bz(i,j,k),i=1,mxold-3),j=1,my),k=1,mz), &
	(((ex(i,j,k),i=1,mxold-3),j=1,my),k=1,mz), &
	(((ey(i,j,k),i=1,mxold-3),j=1,my),k=1,mz), &
	(((ez(i,j,k),i=1,mxold-3),j=1,my),k=1,mz), &
	(((curx(i,j,k),i=1,mxold-3),j=1,my),k=1,mz), &
	(((cury(i,j,k),i=1,mxold-3),j=1,my),k=1,mz), &
	(((curz(i,j,k),i=1,mxold-3),j=1,my),k=1,mz)

	close(7)
	
	call mpi_barrier(MPI_COMM_WORLD,ierr)
	
	deallocate(bx,by,bz,ex,ey,ez,curx,cury,curz)

	deallocate(bufferin1,bufferin2)

	deallocate(bufferin1y,bufferin2y,bufferin1x,bufferin2x)

	deallocate(sendbufy,sendbufz)

	deallocate( temp, poutup, poutdwn, pinblw &
	, pinabv, poutlft, poutrgt,pinlft, pinrgt, pall ) 
	
	call mpi_barrier(MPI_COMM_WORLD,ierr)
	
	allocate(bx(mx,my,mz),by(mx,my,mz),bz(mx,my,mz),ex(mx,my,mz),ey(mx &
	,my,mz),ez(mx,my,mz),curx(mx,my,mz),cury(mx,my,mz),curz(mx,my &
	,mz),bufferin1(mx,my,2),bufferin2(mx,my,2), &
	bufferin1y(mx,2,mz),bufferin2y(mx,2,mz))
	
	call mpi_barrier(MPI_COMM_WORLD,ierr)
	
	allocate(bufferin1x(2,my,mz),bufferin2x(2,my,mz))

	allocate(sendbufy(mx,2,mz &
	),sendbufz(mx,my,2))

	allocate(temp(mx,my,mz))

	allocate(poutup(buffsize))

	allocate(poutdwn(buffsize),pinblw(buffsize))

	allocate(pinabv(buffsize), poutlft(buffsize))

	allocate(poutrgt(buffsize),pinlft(buffsize))

	allocate(pinrgt(buffsize),pall(lot))
	
	call mpi_barrier(MPI_COMM_WORLD,ierr)
	
	pall=0
	bx=0.
	by=0.
	bz=0.
	ex=0.
	ey=0.
	ez=0.

!this initialization is specific to shocks
	do  k=1,mz
		do  j=1,my
			do  i=1,mx

				bx(i,j,k)=Binit*cos(btheta) 
				by(i,j,k)=Binit*sin(btheta)*sin(bphi)
				bz(i,j,k)=Binit*sin(btheta)*cos(bphi)

				ex(i,j,k)=0.
				ey(i,j,k)=(-beta)*bz(i,j,k) 
				ez(i,j,k)=-(-beta)*by(i,j,k)

			enddo
		enddo
	enddo
	
	
	open(unit=7,file=fenlargeloc, form='unformatted')
	rewind 7
	if(rank.eq.0) print *, rank,":reading back enlarge file"
	
	read(7)(((bx(i,j,k),i=1,mxold-3),j=1,my),k=1,mz), &
	(((by(i,j,k),i=1,mxold-3),j=1,my),k=1,mz), &
	(((bz(i,j,k),i=1,mxold-3),j=1,my),k=1,mz), &
	(((ex(i,j,k),i=1,mxold-3),j=1,my),k=1,mz), &
	(((ey(i,j,k),i=1,mxold-3),j=1,my),k=1,mz), &
	(((ez(i,j,k),i=1,mxold-3),j=1,my),k=1,mz), &
	(((curx(i,j,k),i=1,mxold-3),j=1,my),k=1,mz), &
	(((cury(i,j,k),i=1,mxold-3),j=1,my),k=1,mz), &
	(((curz(i,j,k),i=1,mxold-3),j=1,my),k=1,mz)     
	
	close(7,status='delete')
	bufferin1=0.
	bufferin2=0.
	bufferin1y=0.
	bufferin2y=0.
	bufferin1x=0.
	bufferin2x=0.
	sendbufy=0.
	sendbufz=0.

	temp=0.
	if(rank .eq. 0) print *, rank, "enlarge done"
	print *, rank,"enlarge done"

	
end subroutine enlarge_domain_with_disk



!-------------------------------------------------------------------------------
! 						subroutine moving_window					 
!																		
! Moves the simulation window, for eficiency purposes
!							
!-------------------------------------------------------------------------------

subroutine moving_window(direction)
!moving window flying to the right (direction > 0), left (direction < 0). Movwin flies at the speed of light
!to ensure easy boundary conditions on the outflowing wall. 

	implicit none
	
	! local variables
	real velmov
	integer :: direction, direct, shift, n, n1, i, j, k
	logical in
	if(rank.eq.0) print *, "movwin=",movwin
        if(.not.movwin) return

        if(direction .lt. 0) direct = -1 !use local var direct so as not to overwrite the global var
        if(direction .ge. 0) direct =  1

	if(direct .lt. 0 .and. ( xinject .gt. 30 .or. modulo(lap,shiftint).ne. 0) ) return
	if(direct .gt. 0 .and. (lap .lt. shiftstart .or. modulo(lap,shiftint) .ne. 0)) return

	if(rank.eq.0) print *,"shifting", xinject, xinject2 ,"step=",lap
	
	! shift fields and particles by -direct*c*shiftint. If window is 
        ! flying to the right, need to shift everything to the left
	! NOTE: c*shiftint has to be an integer
	
!	shift=int(c*shiftint)
!	if(c*shiftint-shift > 1e-5) then
!	   if(rank .eq. 0) print *, "moving window: c*shiftint is not an integer. &
!	   Shifting by noninteger value not supported. Check input file"
!	   stop
!	endif

	if(movwingam .lt. 10000.) then
	   if(movwingam .lt. 1) then
	      velmov=movwingam
	   else
	      velmov = c*sqrt(movwingam**2 - 1)/movwingam
	   endif
	else
	   velmov = c
	endif

!the shift of the grid is determined so that it is always an integer. It helps to have
! velmov * shiftint == integer, but it's not required. 

        shift=anint(leftwall+velmov*lap) - anint(leftwall+velmov*lap - velmov*shiftint)
        
	movwinoffset=movwinoffset+shift

	if((c*shiftint) .ne. real(shift,4)) print *,rank,": Moving window error: c*shiftint is not an integer"

	shift=shift*(-direct)

	xinject=xinject+shift
	xinject2=xinject2+shift

!	if(direct .gt. 0) pistonloc=pistonloc+shift

        if(direct .lt. 0) then 
	do k=1,mz
		do j=1,my
			do i=mx,shift+1,-1
				bx(i,j,k)=bx(i-shift,j,k)
				by(i,j,k)=by(i-shift,j,k)
				bz(i,j,k)=bz(i-shift,j,k)
				ex(i,j,k)=ex(i-shift,j,k)
				ey(i,j,k)=ey(i-shift,j,k)
				ez(i,j,k)=ez(i-shift,j,k)              
			enddo
		enddo
	enddo
	
	do k=1,mz
		do j=1,my
			do i=shift,1,-1
				bx(i,j,k)=0
				by(i,j,k)=0
				bz(i,j,k)=0
				ex(i,j,k)=0
				ey(i,j,k)=0
				ez(i,j,k)=0
			enddo
		enddo
	enddo
	endif


        if(direct .gt. 0) then 
	do k=1,mz
		do j=1,my
			do i=1,mx+(shift+1) 
				bx(i,j,k)=bx(i-shift,j,k)
				by(i,j,k)=by(i-shift,j,k)
				bz(i,j,k)=bz(i-shift,j,k)
				ex(i,j,k)=ex(i-shift,j,k)
				ey(i,j,k)=ey(i-shift,j,k)
				ez(i,j,k)=ez(i-shift,j,k)              


			enddo
		enddo
	enddo
	
	do k=1,mz
		do j=1,my
			do i=mx+(shift),mx 
!				bx(i,j,k)=0
!				by(i,j,k)=0
!				bz(i,j,k)=0
!				ex(i,j,k)=0
!				ey(i,j,k)=0
!				ez(i,j,k)=0

! this is specific to shock problem
				bx(i,j,k)=Binit*cos(btheta)
				by(i,j,k)=Binit*sin(btheta)*sin(bphi)
				bz(i,j,k)=Binit*sin(btheta)*cos(bphi)
				ex(i,j,k)=0.
				ey(i,j,k)=(-beta)*bz(i,j,k) 
				ez(i,j,k)=-(-beta)*by(i,j,k)               

			enddo
		enddo
	enddo
	endif


	!now move the particles

	n=1
	if(Rank.eq.0)print *, "In movshift, ions=", ions

	if(ions.gt.0) then
		152  continue
		
		n1=n !indp(n)
		in=.true.
		p(n1)%x=p(n1)%x+shift
		if(p(n1)%x .gt. mx -2. .or. p(n1)%x .lt. 3. ) in=.false.
		if(in) goto 158

		!  Replace by the ion from the top of the stack:
		
		p(n1)=p(ions)

		ions=ions-1
		n=n-1
		158	n=n+1
		if(n.le.ions)go to 152
	endif
	
	if(Rank.eq.0)print *, "In movshift, ions=", ions
	if(Rank.eq.0)print *, "In movshift, lecs=", lecs
	
	n=maxhlf+1
	
	if(lecs.gt.0) then
		153      continue
		n1=n
		in=.true.
		p(n1)%x=p(n1)%x+shift
		
		if(p(n1)%x .gt. mx -2. .or. p(n1)%x .lt. 3. ) in=.false.
		if(in) goto 159
		!  Replace by the lec from the top of the stack:
		
		
		!  Replace by the electron from the top of the stack:
		p(n1)=p(maxhlf+lecs)
		lecs=lecs-1
		n=n-1
		159	n=n+1        
		if(n.le.maxhlf+lecs)go to 153       
		
	endif 
	if(Rank.eq.0)print *, "In movshift, lecs=", lecs
	

end subroutine moving_window



#ifdef twoD
end module m_domain
#else
end module m_domain_3d
#endif
