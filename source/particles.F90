!
! Particle module
!
! Includes the particle data structures and the routines for depositing
! and moving the particles
!
!

#ifdef twoD 

module m_particles

	use m_globaldata
	use m_system
	use m_aux
	use m_communications
	use m_fields
	use m_inputparser
	use m_fparser
	
#else

module m_particles_3d

	use m_globaldata_3d
	use m_system_3d
	use m_aux_3d
	use m_communications_3d
	use m_fields_3d
	use m_inputparser_3d
	use m_fparser_3d

#endif


	
	implicit none
	
	private

!-------------------------------------------------------------------------------
!	PARAMETERS
!-------------------------------------------------------------------------------

	integer, parameter :: pdf_sz=1000
	
!-------------------------------------------------------------------------------
!	TYPE DEFINITIONS
!-------------------------------------------------------------------------------

	type :: particle
		sequence
		real(dprec) :: x !x is real(dprec) :: to accomodate long and thin shock runs
		real(sprec) :: y,z,u,v,w, ch, dummy
		integer (kind=4) :: ind, proc,splitlev !need dum to keep size even # of bytes
	end type particle
	
	type :: prtnum
		sequence
		integer (kind=4) :: ind, proc
	end type prtnum
	
!-------------------------------------------------------------------------------
!	VARIABLES
!-------------------------------------------------------------------------------

	real(sprec) :: gamma0, ppc0, delgam, npnb, betshock, dummyvar, qom, omega, sigma, mi, me, qmi, &
				   qme, leftwall, acool, qi, qe, q, c_omp, movwinoffset
				   
        real(dprec) :: pi 

	logical :: cooling, shockframe, splitparts

	
	integer :: totalpartnum, shockloc, leftclean, nionout, nlecout, splitnmax,   &
			   splitratio, nioneject, nleceject, buffsize, movingshock, pcosthmult
			   
	integer, allocatable, dimension(:) :: pall 
	
	integer :: ions, lecs, maxhlf, lapreorder, maxptl, maxptl0, LenIonOutUp,LenIonOutDwn,LenLecOutUp, &
               LenLecOutDwn,LenIonInBlw,LenIonInAbv,LenLecInBlw, LenLecInAbv, LenIonOutLft, &
               LenIonOutRgt,LenLecOutLft, LenLecOutRgt, LenIonInLft, LenIonInRgt,LenLecInLft,LenLecInRgt
      
	integer :: receivedions, receivedlecs, injectedions, injectedlecs


	logical prt_first
	real(dprec) :: time_cut     
	
	type(particle),allocatable :: p(:)
	type(particle),allocatable :: tempp(:) 
	type(particle),allocatable :: poutup(:), pinabv(:),poutdwn(:) &
	,pinblw(:),poutlft(:),poutrgt(:),pinlft(:),pinrgt(:)
	
	!integer :: particletype !, oldtypes(0:2), blockcounts(0:2),offsets(0:2),extent


	! Variables related to particle spliting
	
	real, allocatable, dimension(:) :: split_E_ions, split_E_lecs
	real splitoffs          ! maximal offset of child particles [in cells]
	integer :: split_auto      ! if =1 then split level thresholds automatically adjust  
	integer :: split_lap_start, split_lap_rate
	
	integer ::split_prev_ions ! number of unsplit ions in last cycle split
	integer ::split_prev_lecs ! number of unsplit ions in last cycle split
	real split_frac_ions    ! fraction of split ions w.r.t total
	real split_frac_lecs    ! fraction of split lecs w.r.t total
	integer ::split_gamma_res ! resolution of energy histogram
	real split_lgamma_m1_min ! min(log(gamma)-1) of histogram
	real split_lgamma_m1_max ! max(log(gamma)-1) of histogram
	real split_min_ratio    ! minimal ratio betwen consecutive split thresholds. 
	real(dprec) :: split_timer

	logical :: external_fields, user_part_bcs

!-------------------------------------------------------------------------------
!	INTERFACE DECLARATIONS
!-------------------------------------------------------------------------------
	
!-------------------------------------------------------------------------------
!	PUBLIC MODIFIERS
!-------------------------------------------------------------------------------

	! public functions
	
	public :: split_parts, reorder_particles, exchange_particles, zigzag, &
			  inject_others, allocate_particles, maxwell_dist,  powerlaw3d, &
			  read_input_particles, check_overflow, check_overflow_num, init_maxw_table, init_split_parts, &
			  inject_from_wall, inject_plasma_region

	! public variables 

	public ::  ppc0, splitparts, buffsize, poutup, pinabv, poutdwn, pinblw, poutlft, &    ! used in domain
			  poutrgt, pinlft, pinrgt, pall, ions, lecs, particle, p, maxhlf, maxptl0, &           ! used in domain
			  leftwall, split_E_ions, split_E_lecs,  &							   ! used in restart
			  sigma, gamma0, delgam, mi, me, dummyvar, cooling, acool, nionout, nlecout, & ! used in outputs
			  injectedions, injectedlecs, receivedions, receivedlecs, nioneject, nleceject, &      ! used in outputs
			  splitratio, qi, qe, q, prtnum, c_omp, maxptl, movingshock, &						   ! used in outputs
			  time_cut, prt_first, shockframe, betshock, leftclean, tempp, &					   ! used in initialize
!			  gamma_table, pdf_table, gamma_table1, pdf_table1, gamma_table_cold, pdf_table_cold, &! used in initialize
			  splitnmax, splitoffs, split_auto, split_lap_start, split_lap_rate, split_prev_ions, &! used in initialize
			  split_prev_lecs, split_frac_ions, split_frac_lecs, split_gamma_res, &				   ! used in initialize
			  split_lgamma_m1_min, split_lgamma_m1_max, split_min_ratio, split_timer, pdf_sz, npnb,&! used in initialize
			  qme, qmi, lapreorder, &											   				   ! used in initialize
			  totalpartnum, qom, omega, pcosthmult, LenIonOutUp, &		   ! used in the m_user_* module
			  LenIonOutDwn,LenLecOutUp, LenLecOutDwn,LenIonInBlw,LenIonInAbv,LenLecInBlw, &        ! used in the m_user_* module
			  LenLecInAbv, LenIonOutLft, LenIonOutRgt,LenLecOutLft, LenLecOutRgt, LenIonInLft, &   ! used in the m_user_* module
			  LenIonInRgt,LenLecInLft,LenLecInRgt, shockloc, & 										   ! used in the m_user_* module
			  movwinoffset, pi, external_fields, user_part_bcs
			  
!-------------------------------------------------------------------------------
!	MODULE PROCEDURES AND FUNCTIONS
!-------------------------------------------------------------------------------

	contains



!-------------------------------------------------------------------------------
! 						subroutine read_input_communications					 
!																		
! Reads any variables related to (or needed by) this module
! 							
!-------------------------------------------------------------------------------

subroutine read_input_particles()

	implicit none
	
	real(sprec) :: lmaxptl0
	real(sprec) :: omp

	call inputpar_getd_def("particles", "sigma", 0._sprec, sigma)
	call inputpar_getd_def("particles", "maxptl0", 1.e7_sprec, lmaxptl0)
	call inputpar_getd_def("particles", "ppc0", 1._sprec, ppc0)

	call inputpar_getd_def("particles", "delgam", 1.e-8_sprec, delgam)
	call inputpar_getd_def("particles", "me", 1._sprec, me)
	call inputpar_getd_def("particles", "mi", 20._sprec, mi)

	call inputpar_getd_def("particles", "gamma0", 1.1_sprec, gamma0)

	call inputpar_getd_def("particles", "c_omp", 0._sprec, c_omp)

        !non-rel drift velocities can be specified with gamma<1 in input file. gamma = beta in that case. 
	if(gamma0 .lt. 1) gamma0=sqrt(1./(1.-gamma0**2)) 

	maxptl0=lmaxptl0

	!plasma reaction
	omp=c/c_omp
	        
 	beta=sqrt(1.-1./gamma0**2)
	
	!note that gamma0 is used in the definition of omega_p to get charge. 
	qe=-(omp**2*gamma0)/((ppc0*.5)*(1+me/mi)) 
	!no sqrt because qe/me = 1 
	
	qi=-qe 
	
	me=me*abs(qi)
	mi=mi*abs(qi)
	
	qme=qe/me
	qmi=qi/mi 


end subroutine read_input_particles



!-------------------------------------------------------------------------------
! 						subroutine allocate_particles				 
!																		
! Allocates the particle module's variables, some initialization of default vars
! 							
!-------------------------------------------------------------------------------

subroutine allocate_particles()

	implicit none
	
	! local variables
		
	pi=3.1415927
	
!	Binit_default=sqrt((gamma0-1)*.5*ppc0*c**2*(mi+me)*sigma) ! needed in fields module	
	
	!effectively me and mi = abs(qe) and qi, for use in computing energy density  

	leftwall=15.   !location of the left wall to reflect the stream
	movwinoffset = 0.

	!particle buffer size
	
	maxptl=maxptl0/size0
	maxhlf=maxptl/2
		
	! buffer size for particle communication

#ifndef twoD
	buffsize = max(1*int(ppc0*c*max((mx-5)*(my-5),1*(mx-5)*(mz-5))),10000)
#else
	buffsize = max(3*int(ppc0*c*(1*(mx-5))),60000) !5 for splitting particles
	if (splitparts) buffsize=buffsize*150 
#endif
	
	! allocate particle buffers
	
	allocate(p(maxptl),tempp(maxhlf))
	
	p%x=0
	p%y=0
	p%z=0
	p%u=0
	p%v=0
	p%w=0
	p%ch=1
	p%splitlev=1
	
	allocate(poutup(buffsize),poutdwn(buffsize),pinblw(buffsize) &
	,pinabv(buffsize), poutlft(buffsize),poutrgt(buffsize) &
	,pinlft(buffsize),pinrgt(buffsize))
	
	allocate(pall(lot)) ! this is used in the particle module to reorder particles

	splitnmax=15            ! max number of levels when splitting is used

	allocate(split_E_ions(splitnmax))
	allocate(split_E_lecs(splitnmax))
	
end subroutine allocate_particles




!-------------------------------------------------------------------------------
! 						subroutine check_overflow()
!														
! 
!
!-------------------------------------------------------------------------------
subroutine check_overflow()

	if(ions .gt. maxhlf .or. lecs .gt. maxhlf) then
		print *,rank,": Particle OVERFLOW: ions,lecs,maxhlf",ions &
		,lecs,maxhlf, "Increase the maxptl variable in input file and restart"
		stop
	endif

end subroutine check_overflow

!-------------------------------------------------------------------------------
! 						subroutine check_overflow_num()
!										
!
!-------------------------------------------------------------------------------
subroutine check_overflow_num(num)
	real num
	if(num .gt. maxhlf) then 
	   print *, rank, ": OVERFLOW: number of injected &
	   particles will exceed the maximum. Increase maxptl in &
	   input file and restart" 
	   stop
	endif
end subroutine check_overflow_num

!-------------------------------------------------------------------------------
! 						subroutine reorder_particles()					 
!															
! 
!
!-------------------------------------------------------------------------------

subroutine reorder_particles()

	implicit none
	
	if(lap .eq. lapreorder+1) return
	
	if(modulo(lap,10) .ne. 0) return
	

	call reorder_particles_()	
	lapreorder=lap


end subroutine reorder_particles



!-------------------------------------------------------------------------------
! 						subroutine reorder_particles_()					 
!			
! 
!
!-------------------------------------------------------------------------------

subroutine reorder_particles_()

	implicit none
	
	! local variables
	
	integer ::n, celli, k,j, i



	if(Rank.eq.0)print *, "reordering particles"    
	pall=0
	!count the number of particles in each cell

	do n=1,ions
		celli=int(p(n)%x)+int(p(n)%y)*iy+int(p(n)%z)*iz !ix=1
		pall(celli)=pall(celli)+1
	enddo
	
	!convert Pall into an allocation
	!      if(rank.eq.0)print *,"done count"
	
	k=1
	do i=1,lot
		j=pall(i) 
		pall(i)=k
		k=k+j
	enddo
	
	if(debug) print *,rank,": done realloc"
	if(debug) print *, rank,":","ions=",ions

	do n=1,ions
		celli=int(p(n)%x)+int(p(n)%y)*iy+int(p(n)%z)*iz !ix=1
		j=pall(celli)
		pall(celli)=pall(celli)+1
		tempp(j)=p(n)
	enddo
	
	do n=1,ions
		p(n)=tempp(n)
	enddo
	
	!now for lecs
	!            if(Rank .eq. 0) print *, "now lecs"

	pall=0

	!count the number of particles in each cell

	if(debug) print *, rank,":","maxhlf",maxhlf,"lecs",lecs

	do n=maxhlf+1,maxhlf+lecs
		celli=int(p(n)%x)+int(p(n)%y)*iy+int(p(n)%z)*iz !ix=1
		pall(celli)=pall(celli)+1
	enddo
	
	!convert Pall into an allocation
	
	k=1
	
	do i=1,lot
		j=pall(i) 
		pall(i)=k
		k=k+j
	enddo
	
	!            if(Rank .eq. 0) print *,"done realloc"
	
	if(debug) print *,rank,": done realloc"
	
	do n=maxhlf+1,maxhlf+lecs
		celli=int(p(n)%x)+int(p(n)%y)*iy+int(p(n)%z)*iz !ix=1
		j=pall(celli)
		pall(celli)=pall(celli)+1
		tempp(j)=p(n)
	enddo
	
	do n=1,lecs
		p(maxhlf+n)=tempp(n)
	enddo
	
	if(debug) print *,rank,": done reorder"

end subroutine reorder_particles_



!-------------------------------------------------------------------------------
! 						subroutine index_particles()					 
!				
! Indexing particles, currently turned off
!
!-------------------------------------------------------------------------------

subroutine index_particles()

	implicit none
	
	! local variables

	integer ::n
	
	
!	if(rank.eq.0)print *,rank,":","indexing particles...",ions,lecs
!	
!	do n=1,ions
!		icell(n)=int(p(n)%x)+int(p(n)%y)*iy+int(p(n)%z)*iz !ix=1
!	enddo
!	
!	do n=maxhlf+1,maxhlf+lecs
!		icell(n)=int(p(n)%x)*ix +int(p(n)%y)*iy+int(p(n)%z)*iz
!	enddo
!	
!	
!	do n=1,ions
!		indprevrs(indp(n))=n
!	enddo
!	
!	do n=maxhlf+1,maxhlf+lecs
!		indprevrs(indp(n))=n
!	enddo
!	
!	if(rank.eq.0)print *,rank,":", "done indexing" 
	
end subroutine index_particles




!-------------------------------------------------------------------------------
! 						subroutine zigzag()					 
!																		
! Charge (current) deposition on the grid
!
!-------------------------------------------------------------------------------

subroutine zigzag(x2,y2,z2,x1,y1,z1,in)

	implicit none
	
	! dummy variables
	
	logical :: in
	real(dprec) :: x1,x2
	real(sprec) :: y1,z1,y2,z2
	
	! local variables
	
	real(dprec) :: xr
	real yr,zr
	real Fx1, Fx2, Fy1, Fy2, Fz1, Fz2
	real Wx1, Wx2, Wy1, Wy2, Wz1, Wz2
	integer ::i1, i2, j1, j2, k1, k2
	
	i1=aint(x1)
	i2=aint(x2)
	j1=aint(y1)
	j2=aint(y2)
	k1=aint(z1)
	k2=aint(z2)
		
		xr=min(real(min(i1,i2)+1),max(real(max(i1,i2)),.5*(x1+x2)))
		yr=min(real(min(j1,j2)+1),max(real(max(j1,j2)),.5*(y1+y2)))
		zr=min(real(min(k1,k2)+1),max(real(max(k1,k2)),.5*(z1+z2)))
	
#ifdef twoD
		k1=1
		k2=1
#endif

	!-q to include -j in the Ampere's equation, to be consistent
	
	Fx1=-q*(xr-x1)
	Fy1=-q*(yr-y1)
	Fz1=-q*(zr-z1)
	
	Wx1=.5*(x1+xr)-i1
	Wy1=.5*(y1+yr)-j1

#ifndef twoD
		Wz1=.5*(z1+zr)-k1
#endif

	Wx2=.5*(x2+xr)-i2
	Wy2=.5*(y2+yr)-j2

#ifndef twoD
		Wz2=.5*(z2+zr)-k2
#endif
	
	Fx2=-q*(x2-xr)
	Fy2=-q*(y2-yr)
	Fz2=-q*(z2-zr)
	
#ifdef twoD
		Wz1=0
		Wz2=0
#endif

	curx(i1,j1,  k1  )= curx(i1,j1,  k1  )+Fx1 * (1.-Wy1)*(1.-Wz1)
	curx(i1,j1+1,k1  )= curx(i1,j1+1,k1  )+Fx1 *  Wy1    *(1.-Wz1)

#ifndef twoD
		curx(i1,j1,  k1+1)= curx(i1,j1,  k1+1)+Fx1 * (1-Wy1) * Wz1
		curx(i1,j1+1,k1+1)= curx(i1,j1+1,k1+1)+Fx1 *  Wy1    * Wz1
#endif
	
	curx(i2,j2,  k2  )= curx(i2,j2,  k2  )+Fx2 * (1.-Wy2)*(1.-Wz2)
	curx(i2,j2+1,k2  )= curx(i2,j2+1,k2  )+Fx2 *  Wy2    *(1.-Wz2)
#ifndef twoD
		curx(i2,j2,  k2+1)= curx(i2,j2,  k2+1)+Fx2 * (1.-Wy2)* Wz2
		curx(i2,j2+1,k2+1)= curx(i2,j2+1,k2+1)+Fx2 *  Wy2    * Wz2
#endif

	!-----

	cury(i1  ,j1,k1  )= cury(i1  ,j1,k1  )+Fy1 * (1.-Wx1)*(1.-Wz1)
	cury(i1+1,j1,k1  )= cury(i1+1,j1,k1  )+Fy1 *  Wx1    *(1.-Wz1) 
#ifndef twoD
		cury(i1  ,j1,k1+1)= cury(i1  ,j1,k1+1)+Fy1 * (1.-Wx1)* Wz1
		cury(i1+1,j1,k1+1)= cury(i1+1,j1,k1+1)+Fy1 *  Wx1    * Wz1
#endif

	cury(i2  ,j2,k2  )= cury(i2  ,j2,k2  )+Fy2 * (1.-Wx2)*(1.-Wz2)
	cury(i2+1,j2,k2  )= cury(i2+1,j2,k2  )+Fy2 *  Wx2    *(1.-Wz2) 
#ifndef twoD
		cury(i2  ,j2,k2+1)= cury(i2  ,j2,k2+1)+Fy2 * (1.-Wx2)* Wz2
		cury(i2+1,j2,k2+1)= cury(i2+1,j2,k2+1)+Fy2 *  Wx2    * Wz2
#endif

	!-----

	curz(i1  ,j1  ,k1)= curz(i1  ,j1  ,k1)+Fz1 * (1.-Wx1)*(1.-Wy1)
	curz(i1+1,j1  ,k1)= curz(i1+1,j1  ,k1)+Fz1 *  Wx1    *(1.-Wy1)
	curz(i1  ,j1+1,k1)= curz(i1  ,j1+1,k1)+Fz1 * (1.-Wx1)* Wy1
	curz(i1+1,j1+1,k1)= curz(i1+1,j1+1,k1)+Fz1 *  Wx1    * Wy1
	
	curz(i2  ,j2  ,k2)= curz(i2  ,j2  ,k2)+Fz2 * (1.-Wx2)*(1.-Wy2)
	curz(i2+1,j2  ,k2)= curz(i2+1,j2  ,k2)+Fz2 *  Wx2    *(1.-Wy2)
	curz(i2  ,j2+1,k2)= curz(i2  ,j2+1,k2)+Fz2 * (1.-Wx2)* Wy2
	curz(i2+1,j2+1,k2)= curz(i2+1,j2+1,k2)+Fz2 *  Wx2    * Wy2
	

	
	in=.true.
	if(periodicx .eq. 1) then 
		in=.true.
	else
		in=(x2.gt.3.0).and.(x2.lt.mx-2.0)
	endif
	
	if(in) then
	
		if(periodicy .eq. 1) then 
			in=.true.
		else
			if(modulo(rank,sizey).eq.0) in=(y2.gt.3.0)
			if(modulo(rank,sizey).eq.sizey-1) in=(y2.lt.my-2.0)
			if(size0.eq.1) in=(y2.gt.3.0).and.(y2.lt.my-2.0)
		endif
		
		if(periodicz .eq. 1) then
			in=.true.
		else
			if(rank/sizey.eq.0) in=(z2.gt.3.0)
			if(rank/sizey.eq.sizez-1) in=(z2.lt.mz-2.0)
			if(size0.eq.1)in=(z2.gt.3.0).and.(z2.lt.mz-2.0)
		endif
	
	endif

	
end subroutine zigzag


!-------------------------------------------------------------------------------
! 						subroutine inject_others()					 
!																		
! Injects particles into the simulation
!
!-------------------------------------------------------------------------------

subroutine inject_others()

	implicit none

	real(dprec) :: time01, time02, time03, time04
	integer ::n
	
	receivedions=0
	receivedlecs=0
	
	do n=1,LenIonInAbv
		ions=ions+1
		p(ions)=pinabv(n)
	enddo
	
	do n=1,LenLecInAbv
		lecs=lecs+1
		p(maxhlf+lecs)=pinabv(LenIonInAbv+n)
	enddo

	!those that entered from below

	do n=1,LenIonInBlw
		ions=ions+1
		p(ions)=pinblw(n)
	enddo
	
	do n=1,LenLecInBlw
		lecs=lecs+1
		p(maxhlf+lecs)=pinblw(LenIonInBlw+n)
	enddo
	
	!now inject particles that entered from left and right in y
	
	do n=1,LenIonInRgt
		ions=ions+1
		p(ions)=pinrgt(n)
	enddo
	
	do n=1,LenLecInRgt
		lecs=lecs+1
		p(maxhlf+lecs)=pinrgt(LenIonInRgt+n)
	enddo

	!those that entered from left

	do n=1,LenIonInLft
		ions=ions+1
		p(ions)=pinlft(n)
	enddo
	
	do n=1,LenLecInLft
		lecs=lecs+1
		p(maxhlf+lecs)=pinlft(LenIonInLft+n)
	enddo
	
	if(rank.eq.0) time03=mpi_wtime()
	
	receivedions=lenioninabv+lenioninblw+lenioninrgt+lenioninlft
	receivedlecs=lenlecinabv+lenlecinblw+lenlecinrgt+lenlecinlft

end subroutine inject_others





!-------------------------------------------------------------------------------
! 						subroutine exchange_particles()					 
!																		
! Exchanges particles between diferent processors
!
!-------------------------------------------------------------------------------

subroutine exchange_particles()

	implicit none
	
	! local variables
	
	integer ::arrsizeup(2),arrsizedwn(2), lenup, lendwn, ierr
	integer ::arrsizeblw(2),arrsizeabv(2),status(statsize)
	integer ::uprank, dwnrank,uptag, dwntag, lenabv, lenblw
	
	integer ::arrsizeoutrgt(2),arrsizeoutlft(2), lenoutrgt, lenoutlft
	integer ::arrsizeinlft(2),arrsizeinrgt(2)
	integer ::rgtrank, lftrank,rgttag, lfttag, leninrgt, leninlft
	

	arrsizeup(1)=LenIonOutUp
	arrsizeup(2)=LenLecOutUp
	lenup=max((arrsizeup(1)+arrsizeup(2)),1)
	
	arrsizedwn(1)=LenIonOutDwn
	arrsizedwn(2)=LenLecOutDwn
	lendwn=max((arrsizedwn(1)+arrsizedwn(2)),1)
	
	uprank=modulo((rank/sizey + 1),sizez)*sizey + modulo(rank,sizey) 
	dwnrank=modulo((rank/sizey - 1),sizez)*sizey + modulo(rank,sizey)
	uptag=100
	dwntag=200
	
	!send the info about the size of the array
	!send up and recv from below

#ifdef MPI      
		call MPI_SendRecv(arrsizeup,2,mpi_integer,uprank,uptag, &
		arrsizeblw,2,mpi_integer,dwnrank,uptag, &
		MPI_Comm_World,status,ierr)
#else
		call MPI_SendRecv(arrsizeup,2,mpi_integer,uprank,uptag, &
		arrsizeblw,2,mpi_integer,dwnrank,uptag, &
		MPI_Comm_World,status,ierr)
#endif
	
	!send dwn and recv from above
#ifdef MPI
		call MPI_SendRecv(arrsizedwn,2,mpi_integer,dwnrank,dwntag, &
		arrsizeabv,2,mpi_integer,uprank,dwntag, &
		MPI_Comm_World,status,ierr)
#else
		call MPI_SendRecv(arrsizedwn,2,mpi_integer,dwnrank,dwntag, &
		arrsizeabv,2,mpi_integer,uprank,dwntag, &
		MPI_Comm_World,status,ierr)
#endif

	LenIonInBlw=arrsizeblw(1)
	LenLecInBlw=arrsizeblw(2)
	
	LenIonInAbv=arrsizeabv(1)
	LenLecInAbv=arrsizeabv(2)
	
	lenblw=max((LenIonInBlw+LenLecInBlw),1)
	lenabv=max((LenIonInAbv+LenLecInAbv),1)
	
	if(lenblw .gt. buffsize .or. lenabv .gt. buffsize) then
		print *, "ERROR: BUFFERSIZE SMALLER THAN PARTICLE LIST"
		print *,rank,":","buffsize", buffsize, "lenblw",lenblw,"lenabv" &
		,lenabv
		stop
	endif

	!send and receive particle arrays
	!send up and recv from below
#ifdef MPI
		call MPI_SendRecv(poutup,lenup,particletype,uprank,uptag, &
		pinblw,lenblw,particletype,dwnrank,uptag, &
		MPI_Comm_World,status,ierr)
#else
		pinblw=poutup	
#endif

	!send dwn and recv from above
	!      print *,"lenup", lendwn, lenabv
	
#ifdef MPI
		call MPI_SendRecv(poutdwn,lendwn,particletype,dwnrank,dwntag, &
		pinabv,lenabv,particletype,uprank,dwntag, &
		MPI_Comm_World,status,ierr)
#else
		pinabv=poutdwn
#endif
	
	!-------------------------------------
	! now send and receive particles left and right
	!-------------------------------------
	
	arrsizeoutrgt(1)=LenIonOutRgt
	arrsizeoutrgt(2)=LenLecOutRgt
	lenoutrgt=max((arrsizeoutrgt(1)+arrsizeoutrgt(2)),1)
	
	arrsizeoutlft(1)=LenIonOutLft
	arrsizeoutlft(2)=LenLecOutLft
	lenoutlft=max((arrsizeoutlft(1)+arrsizeoutlft(2)),1)
	
	rgtrank=(rank/sizey)*sizey + modulo(rank+1,sizey) 
	!modulo(rank+1,size0)
	lftrank=(rank/sizey)*sizey + modulo(rank-1,sizey) 
	!modulo(rank-1,size0)
	rgttag=100
	lfttag=200
	
	!send the info about the size of the array
	!send up and recv from below
	if(debug)print *,rank,": outrgt",arrsizeoutrgt

#ifdef MPI
		call MPI_SendRecv(arrsizeoutrgt,2,mpi_integer,rgtrank,rgttag, &
		arrsizeinlft,2,mpi_integer,lftrank,rgttag, &
		MPI_Comm_World,status,ierr)
#else
		call MPI_SendRecv(arrsizeoutrgt,2,mpi_integer,rgtrank,rgttag, &
		arrsizeinlft,2,mpi_integer,lftrank,rgttag, &
		MPI_Comm_World,status,ierr)
#endif

	!send dwn and recv from above
	if(debug)print *,rank,": outlft",arrsizeoutlft

#ifdef MPI
		call MPI_SendRecv(arrsizeoutlft,2,mpi_integer,lftrank,lfttag, &
		arrsizeinrgt,2,mpi_integer,rgtrank,lfttag, &
		MPI_Comm_World,status,ierr)
#else
		call MPI_SendRecv(arrsizeoutlft,2,mpi_integer,lftrank,lfttag, &
		arrsizeinrgt,2,mpi_integer,rgtrank,lfttag, &
		MPI_Comm_World,status,ierr)
#endif
	
	LenIonInLft=arrsizeinlft(1)
	LenLecInLft=arrsizeinlft(2)
	
	LenIonInRgt=arrsizeinrgt(1)
	LenLecInRgt=arrsizeinrgt(2)
	
	leninlft=max((LenIonInLft+LenLecInLft),1)
	leninrgt=max((LenIonInRgt+LenLecInRgt),1)
	
	if(leninlft .gt. buffsize .or. leninrgt .gt. buffsize) then
		print *, "ERROR: BUFFERSIZE SMALLER THAN PARTICLE LIST"
		print *,rank,":","buffsize", buffsize, "leninlft",leninlft &
		,"leninrgt",leninrgt
		stop
	endif

	!send and receive particle arrays
	!send up and recv from below
	
	if(debug)print *,rank,"lenoutrgt",lenoutrgt,"leninlft",leninlft
#ifdef MPI
		call MPI_SendRecv(poutrgt,lenoutrgt,particletype,rgtrank,rgttag, &
		pinlft,leninlft,particletype,lftrank,rgttag, &
		MPI_Comm_World,status,ierr)
#else
		pinlft=poutrgt
#endif
	
	!send dwn and recv from above
	!      print *,"lenup", lendwn, lenabv
	
#ifdef MPI   
		call MPI_SendRecv(poutlft,lenoutlft,particletype,lftrank,lfttag, &
		pinrgt,leninrgt,particletype,rgtrank,lfttag, &
		MPI_Comm_World,status,ierr)
#else
		pinrgt=poutlft
#endif

end subroutine exchange_particles

!-------------------------------------------------------------------------------
! 						subroutine init_split_parts()					 
!																		
! Initialize splitting points for splitting routine
!
!-------------------------------------------------------------------------------


subroutine init_split_parts()
	integer n

	splitratio=10 ! 200 the splitratio should be high for nonrel shocks.
		
	splitoffs=.02
	split_auto=1            ! 1 for auto adjustment of split thresholds
	split_lap_start=2000
	split_lap_rate=50
	
	split_prev_ions=0
	split_prev_lecs=0
	split_frac_ions = 1.0
	split_frac_lecs = 1.0
	split_gamma_res=200     ! number of logarithmic gamma bins in calculating split levels.
	split_lgamma_m1_min=-2  ! log_10(gamma_min-1)
	split_lgamma_m1_max=4   ! log_10(gamma_max-1)
	split_min_ratio=1.001   ! minimal ratio betwen consecutive split thresholds. 
	

	!     Note that split_E(splitnmax) is never used.
	!     Initialization below doesn't matter in auto-split mode.                                

	split_E_ions(1)=1.0+20.0*(gamma0-1.0)          !10 !7
	do n=2,splitnmax
		split_E_ions(n)=1.0+(split_E_ions(n-1)-1.0)*2.0
	enddo
	split_E_lecs(1)=1.0+20.0*(gamma0-1.0)          !10 !7
	do n=2,splitnmax
		split_E_lecs(n)=1.0+(split_E_lecs(n-1)-1.0)*2.0
	enddo
	
	if (rank .eq. size0-1) then
		split_timer=mpi_wtime()
	endif

end subroutine init_split_parts

!-------------------------------------------------------------------------------
! 						subroutine split_parts()					 
!																		
! Splits particles in the simulation
!
!-------------------------------------------------------------------------------

subroutine split_parts()

	implicit none

	! local variables

	real deltay, gamma
	integer ::tempions, templecs, n, nnew, ierr
	! For auto split:
	integer ::level, pos, new_ions, new_lecs, n1, n2, n_to_split, gpos
	real(dprec) :: time01, time02, time03, time04, time05
	
	integer ::cur_level_nlecs(splitnmax)
	integer ::cur_level_nlecs1(splitnmax)
	
	integer ::cur_level_nions(splitnmax)
	integer ::cur_level_nions1(splitnmax)
	
	real gamma_avg_ions(splitnmax) 
	real gamma_avg_ions1(splitnmax) 
	
	real gamma_avg_lecs(splitnmax) 
	real gamma_avg_lecs1(splitnmax) 
	
	integer ::gamma_stat_ions(splitnmax,split_gamma_res)
	integer ::gamma_stat_ions1(splitnmax,split_gamma_res)
	
	integer ::gamma_stat_lecs(splitnmax,split_gamma_res)
	integer ::gamma_stat_lecs1(splitnmax,split_gamma_res)
	
	if (.not.splitparts) return
	
	if(lap .le. split_lap_start.or. modulo(lap, split_lap_rate) .ne. 0.or..not.splitparts) return 			
	
	if (rank .eq. size0-1) then
		time01=mpi_wtime()
		time02=time01
		time03=time01
		time04=time01
	endif
	
	if (split_auto .eq. 1) then 
		
		!     Auto adjust split thresholds. Algorithm:
		!     For each split level n1, adjust split threshold to assure a constant split rate. 
		!     To do this, we need to first bin the present energy statistics. 
		
		!     Gather statistics of present split situation
		do n1=1,splitnmax
			cur_level_nions(n1)=0
			cur_level_nlecs(n1)=0
			gamma_avg_ions(n1)=0
			gamma_avg_lecs(n1)=0
			!     Note that we currently use gamma_avg only for statistics.
			do n2=1,split_gamma_res
				gamma_stat_ions(n1,n2)=0
				gamma_stat_lecs(n1,n2)=0
			enddo
		enddo
	
		! for each level measure energy histogram and average gamma
	
		do n=1,ions
			level=p(n)%splitlev
			gamma=sqrt(1+(p(n)%u**2+p(n)%v**2+p(n)%w**2)) 
			gpos = 1+floor(real(split_gamma_res-1)* &
			(log10(gamma-1.0)-split_lgamma_m1_min)/ &
			(split_lgamma_m1_max-split_lgamma_m1_min))
			gpos = max(1,min(split_gamma_res,gpos)) 
			cur_level_nions(level)=cur_level_nions(level)+1
			gamma_stat_ions(level,gpos)=gamma_stat_ions(level,gpos)+1
			gamma_avg_ions(level)=gamma_avg_ions(level)+gamma
		enddo
		do n=maxhlf+1, maxhlf+lecs
			level=p(n)%splitlev
			gamma=sqrt(1+(p(n)%u**2+p(n)%v**2+p(n)%w**2)) 
			gpos = 1+floor(real(split_gamma_res-1)* &
			(log10(gamma-1.0)-split_lgamma_m1_min)/ &
			(split_lgamma_m1_max-split_lgamma_m1_min))
			gpos = max(1,min(split_gamma_res,gpos)) 
			cur_level_nlecs(level)=cur_level_nlecs(level)+1
			gamma_stat_lecs(level,gpos)=gamma_stat_lecs(level,gpos)+1
			gamma_avg_lecs(level)=gamma_avg_lecs(level)+gamma
		enddo
		
		if (rank .eq. size0-1) then
			time02=mpi_wtime()
		endif
		
		! Gather info from all cpus
		call mpi_allreduce(cur_level_nions,cur_level_nions1, &
		splitnmax ,mpi_integer,mpi_sum,mpi_comm_world,ierr)
		call mpi_allreduce(cur_level_nlecs,cur_level_nlecs1, &
		splitnmax ,mpi_integer,mpi_sum,mpi_comm_world,ierr)
		
		call mpi_allreduce(gamma_avg_ions,gamma_avg_ions1,splitnmax, &
		mpi_read,mpi_sum,mpi_comm_world,ierr)
		call mpi_allreduce(gamma_avg_lecs,gamma_avg_lecs1,splitnmax, &
		mpi_read,mpi_sum,mpi_comm_world,ierr)
		
		call mpi_allreduce(gamma_stat_ions,gamma_stat_ions1, &
		splitnmax*split_gamma_res,mpi_integer,mpi_sum, &
		mpi_comm_world,ierr)
		call mpi_allreduce(gamma_stat_lecs,gamma_stat_lecs1, &
		splitnmax*split_gamma_res,mpi_integer,mpi_sum, &
		mpi_comm_world,ierr)
		
		if (rank .eq. size0-1) then
			time03=mpi_wtime()
		endif
		
		cur_level_nions=cur_level_nions1
		cur_level_nlecs=cur_level_nlecs1
		gamma_avg_ions=gamma_avg_ions1
		gamma_avg_lecs=gamma_avg_lecs1
		gamma_stat_ions=gamma_stat_ions1
		gamma_stat_lecs=gamma_stat_lecs1
		
		do n1=1,splitnmax
			gamma_avg_ions(n1)=gamma_avg_ions(n1)/ &
			max(1.0,real(cur_level_nions(n1)))
			gamma_avg_lecs(n1)=gamma_avg_lecs(n1)/ &
			max(1.0,real(cur_level_nlecs(n1)))
		enddo
		
		if (split_prev_ions .eq. 0 .and. split_prev_lecs .eq. 0) then
			!     We never split before, split next cycle.
			split_prev_ions = cur_level_nions(1)
			split_prev_lecs = cur_level_nlecs(1)
			
		else 
			! calculate split injection rate per level
			new_ions = floor(real(cur_level_nions(1)-split_prev_ions) &
			*split_frac_ions/real(splitratio*splitnmax))
			new_lecs = floor(real(cur_level_nlecs(1)-split_prev_lecs) &
			*split_frac_lecs/real(splitratio*splitnmax))
			
			split_prev_ions = cur_level_nions(1)
			split_prev_lecs = cur_level_nlecs(1)
			
			! get ion thresholds such that n(>threshold)<new_ions
			do n1=1,splitnmax-1
				if (cur_level_nions(n1) .gt. 0) then ! there is something to split
					n2=split_gamma_res
					n_to_split=gamma_stat_ions(n1,n2)
					do while (n_to_split .lt. new_ions .and. n2 .gt. 1)
						n2=n2-1
						n_to_split=n_to_split+gamma_stat_ions(n1,n2)
					enddo
					split_E_ions(n1) = 1.0+10.0**(split_lgamma_m1_min &
					+(split_lgamma_m1_max-split_lgamma_m1_min) &
					*real(n2)/real(split_gamma_res-1)) ! So new_split_E corresponds to gpos=1+floor(n2)
					if (n1 .gt. 1) then ! Make sure we're not too close to threshold of lower level
						split_E_ions(n1) = max(split_E_ions(n1),  &
						1.0+(split_E_ions(n1-1)-1.0)*split_min_ratio)
					endif
				endif
			enddo
		
			do n1=1,splitnmax-1
				if (cur_level_nlecs(n1) .gt. 0) then ! there is something to split
					n2=split_gamma_res
					n_to_split=gamma_stat_lecs(n1,n2)
					do while (n_to_split .lt. new_lecs .and. n2 .gt. 1)
						n2=n2-1
						n_to_split=n_to_split+gamma_stat_lecs(n1,n2)
					enddo
					split_E_lecs(n1) = 1.0+10.0**(split_lgamma_m1_min &
					+(split_lgamma_m1_max-split_lgamma_m1_min) &
					*real(n2)/real(split_gamma_res-1)) ! So new_split_E corresponds to gpos=1+floor(n2)
					if (n1 .gt. 1) then
						split_E_lecs(n1) = max(split_E_lecs(n1),  &
						1.0+(split_E_lecs(n1-1)-1.0)*split_min_ratio)
					endif
					
				endif
			enddo
			
			if (rank .eq. size0-1) then 
				do n1=1,splitnmax
					if(n1 .eq. 1) then
						print *,rank,": i-split stat ", ions,new_ions,lap
						print *,rank,": e-split stat ", lecs,new_lecs,lap
					endif
					print *,rank,": i-split ",n1,cur_level_nions(n1), &
					gamma_avg_ions(n1),split_E_ions(n1)
					print *,rank,": e-split ",n1,cur_level_nlecs(n1), &
					gamma_avg_lecs(n1),split_E_lecs(n1)
				enddo
			endif
		
		endif                  ! else { if (split_prev_lecs .eq. 0 .and. split_prev_ions .eq. 0) }
	endif                     ! if (split_auto .eq. 1) then 
	
	if (rank .eq. size0-1) then
		time04=mpi_wtime()
	endif
	
	tempions=ions
	templecs=lecs
	do n=1,ions
		
		gamma=sqrt(1+(p(n)%u**2+p(n)%v**2+p(n)%w**2)) 
		if(gamma .gt. split_E_ions(p(n)%splitlev) .and. p(n)%splitlev  &
		.lt. splitnmax) then
			!split particles
			p(n)%splitlev=p(n)%splitlev+1
			do nnew=1, splitratio-1
				tempions=tempions+1
				p(tempions)=p(n)
				deltay=splitoffs*(2*random(dseed)-1) !check if falling outside
				p(tempions)%y=p(tempions)%y+deltay
				deltay=splitoffs*(2*random(dseed)-1) !check if falling outside
				p(tempions)%x=p(tempions)%x+deltay
			enddo
			!            p(n)%splitlev=p(n)%splitlev-1 !to retain orig charge. testing
		endif
	
	enddo
	
	do n=maxhlf+1, maxhlf+lecs
		gamma=sqrt(1+(p(n)%u**2+p(n)%v**2+p(n)%w**2)) 
		if(gamma .gt. split_E_lecs(p(n)%splitlev) .and. p(n)%splitlev &
		.lt. splitnmax) then
			!split particles
			
			if(rank .eq. size0-1) print *, "splitting e 0", p(n)%x, p(n)%y
			
			p(n)%splitlev=p(n)%splitlev+1
			do nnew=1, splitratio-1
				templecs=templecs+1
				p(maxhlf+templecs)=p(n)
				deltay=splitoffs*(2*random(dseed)-1) !check if falling outside
				p(maxhlf+templecs)%y=p(maxhlf+templecs)%y+deltay
				deltay=splitoffs*(2*random(dseed)-1) !check if falling outside
				p(maxhlf+templecs)%x=p(maxhlf+templecs)%x+deltay
				
				if(rank .eq. size0-1)  print *, "splitting e", gamma, &
				deltay, p(maxhlf+templecs)%x, p(maxhlf+templecs)%y
				!add a call to zigzag here 
			enddo
			
			!            p(n)%splitlev=p(n)%splitlev-1 !to retain orig charge. testing
		endif
	
	enddo
	
	print *,rank, ": Released ",tempions-ions,templecs-lecs &
	," particles" 
	
	if (rank .eq. size0-1) then
	time05=mpi_wtime()
		print *,rank, "split ", time05-time01, " sec total"
		print *,rank, "split ", time04-time01, " sec auto split"
		print *,rank, "split ", time03-time02, " sec mpi allreduce"        
		print *,rank, "split ", (time05-time01)/(time05-split_timer), &
		" of the time"
		split_timer=time05 
	endif
	
	ions=tempions
	lecs=templecs
	print *, rank, ": last ind", p(ions)%splitlev, p(lecs)%splitlev
	if(ions .gt. maxhlf .or. lecs .gt. maxhlf) then
		print *,rank,": OVERFLOW in split_parts ions,lecs,maxhlf",ions &
		,lecs,maxhlf
		stop
	endif

end subroutine split_parts


!-------------------------------------------------------------------------------
! 						subroutine init_maxw_table() 
!																		
! Initialize probability distribution tables for quick maxwellian generation
!
!-------------------------------------------------------------------------------

subroutine init_maxw_table(delgam, gamma_table, pdf_table)
	real maxg, delgam, gamma_table(:), pdf_table(:), func(pdf_sz)
	integer i

	!initialize gamma_table and pdf_table
	!tables are integrated PDFs for initialization of maxwellians for electrons and ions
	!delgam in the input file sets the temperature in delta gamma, here it is converted 
	!between species by multiplying by mass.
	
	maxg=max(delgam,1e-8)*20+1. !20 times the temperature is usually enough, unless the temperature is tiny
	
	do i=1,pdf_sz
		gamma_table(i)=(maxg-1.)/(pdf_sz-1)*(i-1)+1.
	enddo
	
#ifndef twoD
	func=gamma_table*sqrt(gamma_table**2-1)*exp(-(gamma_table-1)/delgam)
#else
	func=gamma_table*exp(-(gamma_table-1)/delgam)
	! if magnetized shocks, initialize a full 3d distribution
	if (sigma .ne. 0) func=gamma_table*sqrt(gamma_table**2-1)*exp(-(gamma_table-1)/delgam) 	
#endif
	
	pdf_table(1)=0.

	do i=2,pdf_sz
		pdf_table(i)=sum(func(1:i))
	enddo
	
	!normalize pdf_table
	
	pdf_table=pdf_table/pdf_table(pdf_sz)
	

end subroutine init_maxw_table

!-------------------------------------------------------------------------------
! 						subroutine maxwell_dist()					 
!																		
! Maxwell distribution for the particles (similar to above)
!
!-------------------------------------------------------------------------------

subroutine maxwell_dist(gamma0, cd, dseed, u,v,w,gamma_table,pdf_table,pdf_sz)
	
	implicit none
	
	! dummy variables
	
	integer :: pdf_sz
	real(sprec), dimension(:), intent(in) :: gamma_table, pdf_table
	real(sprec) ::  cd, gamma0,  u,v,w
	real(dprec) :: dseed

	! local variables
	
	integer :: i
	logical flag
	real v0, rannum, gam
	real pcosth, pphi, psinth, v0t, ut1, vt1, wt1, ptx, pty, ptz
	real px0, px1, py0, py1, pz0, pz1, gam1, ek 
	
	
	v0=cd*sqrt(1.-1./gamma0**2)
	
	rannum=random(dseed)
	if(rannum .eq. 1.0) rannum=random(dseed)
	
	i=1
	flag=.true.
	gam=1.
	!choose gamma from the table
	do while (flag)
		
		if(i.eq.pdf_sz) then
			gam=gamma_table(pdf_sz)
			flag=.false.
		endif         
		if(rannum .ge. pdf_table(i) .and. rannum .lt. pdf_table(i+1)) then
			
			gam=gamma_table(i)+(gamma_table(i+1)-gamma_table(i)) &
			/(pdf_table(i+1)-pdf_table(i))*(rannum-pdf_table(i)) 
			
			flag=.false.
		endif
		i=i+1
		
	enddo
	
	!choose phase space angles
#ifdef twoD
		pcosth=(2*random(dseed)-1)*pcosthmult
		if (sigma .ne. 0.) pcosth=2*random(dseed)-1
#else 
		pcosth=2*random(dseed)-1
#endif
	
	pphi=random(dseed)*2*pi
	psinth=sqrt(1-pcosth**2)
	
	v0t=cd*sqrt((gam-1)*(gam+1))/gam !*sqrt(1.-1./gam**2)
	
	ut1=v0t*psinth*cos(pphi)
	vt1=v0t*psinth*sin(pphi)
	wt1=v0t*pcosth
	
	ptx=gam*ut1
	pty=gam*vt1
	ptz=gam*wt1
	
	px1=(ptx+v0*gam)*gamma0 !this will include the sign of gamma0
 	py1=pty
	pz1=ptz
	
	gam1=sqrt(1+(px1**2+py1**2+pz1**2)/cd**2)
	u=px1/cd                  
	v=py1/cd                  
	w=pz1/cd                  
	
	
end subroutine maxwell_dist



!-------------------------------------------------------------------------------
! 						subroutine powerlaw3d()					 
!																		
! Calculates a power law for the Bell instability (CR) problem 
!
!-------------------------------------------------------------------------------

subroutine powerlaw3d(gammashock, pmin, cd, dseed, u,v,w)
	
	implicit none
	
	! dummy variables

	real(sprec) :: gammashock, pmin, cd, u,v,w
	real(dprec), intent(in) :: dseed

	! local variables
	
	logical flag
	real v0, rannum, gam, potencia, p	
	real pphi, mu, v0t, ut1, vt1, wt1, ptx, pty, ptz
	real px1, py1, pz1 
	
	v0=cd*sqrt(1.-1./gammashock**2)
	
	rannum=random(dseed)
	flag=.true.
	do while (flag) !putting an arbitrary cut off to the momentum of CRs
		flag=.false.
		if(rannum .gt. .9999) then
			rannum=random(dseed)
			flag=.true.
		endif
	enddo
	
	p = pmin!/(1.-rannum)
	gam = sqrt(1.+p**2)
	
	!choose phase space angles
	!      pcosth=2*random(dseed)-1
	pphi=random(dseed)*2*pi
	mu=1.!*random(dseed)! - 1.
	
	v0t=cd*sqrt((gam-1)*(gam+1))/gam !*sqrt(1.-1./gam**2)
	
	ut1=v0t*mu
	vt1=v0t*sqrt(1.-mu**2)*sin(pphi)
	wt1=v0t*sqrt(1.-mu**2)*cos(pphi)
	
	ptx=gam*ut1
	pty=gam*vt1
	ptz=gam*wt1

	px1=(ptx+v0*gam)*gammashock
	py1=pty
	pz1=ptz
	
	u=px1/cd !/gam1
	v=py1/cd !/gam1
	w=pz1/cd !/gam1

end subroutine powerlaw3d 



!-------------------------------------------------------------------------------
! 				subroutine inject_from_wall()	
!															
! Injects plasma from a wall set by global coordinates. Wall is perpendicular to x axis.
! Plasma can be a Maxwellian
! with individual ion and electron temperatures (set as spread in gamma factor,
! the distribution in momentum space is exp(-gamma/delgame), exp(-gamma/delgami) )
! 
!-------------------------------------------------------------------------------

subroutine inject_from_wall(x1,x2,y1,y2,z1,z2,ppc,gamma_drift,delgam_i,delgam_e,&
	wall_speed,weight,use_density_profile)
	implicit none
	real(dprec):: x1,x2,xt,y1,y2,z1,z2, x1new,x2new
	real(dprec):: y1new,y2new, z1new, z2new

	real ppc,delgam_i,delgam_e,beta_inj,gamma_drift,wall_speed,beta_wall,weight
	logical use_density_profile
	integer direction

	if(abs(wall_speed) .ge. 1) then 
	   beta_wall=sign(sqrt(1-1/wall_speed**2),wall_speed) !if wall_speed > 1, it is interpreted as gamma of wall
	else
	   beta_wall=wall_speed !otherwise, it's wall speed normalized by c
	endif
	
	if(abs(gamma_drift) .ge. 1) then 
	   beta_inj=sign(sqrt(1-1./gamma_drift**2),gamma_drift)
	else
	   beta_inj=gamma_drift
	endif
	
	if(x1 .ne. x2 .and. y1 .ne. y2) then
!#ifndef twoD
	    if(z1 .ne. z2) then
!#endif
	       print *,rank,": in injection from wall: wall is not along x,y,or z"
	       stop
!#ifndef twoD
	    endif
!#endif
	endif

!determine which wall is the inection wall and set coordinates of a small 
!volume that the plasma emitted from the injector will occupy in one step, 
!also including the possibility of wall motion.
!With magnetic field, this way may be too aggressive, may have to inject really
! on a line. This works fine without external fields. 

	if(x1 .eq. x2) then 
	   x1new=x1
	   x2new=x1+(beta_inj-beta_wall)*c
	   direction=1

	   if(x2new .lt. x1new) then !plasma_region expects x2>x1
	      xt=x1new
	      x1new=x2new
	      x2new=xt
	   endif   
	else
	   x1new=x1
	   x2new=x2
	endif

	if(y1 .eq. y2) then 
	   y1new=y1
	   y2new=y1+(beta_inj-beta_wall)*c
	   direction=2

	   if(y2new .lt. y1new) then 
	      xt=y1new
	      y1new=y2new
	      y2new=xt
	   endif   
	else
	   y1new=y1
	   y2new=y2
	endif

	if(z1 .eq. z2) then 
	   z1new=z1
	   z2new=z1+(beta_inj-beta_wall)*c
	   direction=3

	   if(z2new .lt. z1new) then 
	      xt=z1new
	      z1new=z2new
	      z2new=xt
	   endif   
	else
	   z1new=z1
	   z2new=z2
	endif

	call inject_plasma_region(x1new,x2new,y1new,y2new,z1new,z2new,ppc,gamma_drift,delgam_i,delgam_e,&
           weight,use_density_profile,direction)

end subroutine inject_from_wall

!-------------------------------------------------------------------------------
! 				subroutine inject_plasma_region()	
!															
! Injects plasma in a region set by global coordinates. Plasma can be a Maxwellian
! with individual ion and electron temperatures (set as spread in gamma factor,
! the distribution in momentum space is exp(-gamma/delgame), exp(-gamma/delgami) )
!
!-------------------------------------------------------------------------------

subroutine inject_plasma_region(x1,x2,y1,y2,z1,z2,ppc,gamma_drift_in, &
          delgam_i,delgam_e,weight,use_density_profile,direction)
	implicit none

	real(dprec):: x1,x2,y1,y2,z1,z2 !to accurately calculate differences between x-s, which may be >64k, use doubles
	real ppc,delgam_i,delgam_e,gamma_drift,gamma_drift_in,weight
	logical use_density_profile
	integer direction

	real(dprec), dimension(3) :: values

!tables for pre-computed maxwellian distributions. Can add more here
	real, dimension(pdf_sz) :: pdf_table_i, gamma_table_i, pdf_table_e, gamma_table_e

	integer :: i, n, jglob_min, jglob_max
	real :: minx, maxx, miny, maxy, inject_miny, inject_maxy, delta_y, minz, maxz, delta_z, delta_x,numps, tmp
#ifndef twoD
	integer :: kglob_min, kglob_max
	real :: inject_minz, inject_maxz
#endif

	 if(abs(gamma_drift_in) .lt. 1) then
	    gamma_drift=sign(sqrt(1./(1.-gamma_drift_in**2)),gamma_drift_in) !gamma_drift_in can be used as velocity/c, to simplify non-relativistic drift assignment.
	 else
	    gamma_drift=gamma_drift_in
	 endif

! init_maxw assumes dimensionality of the Maxwellian based on the dimensionality of simulation
! and whether B field is non-zero (then you can have 3D maxwellians even in 2D). 
! choice of dimensionality should be made user assigned in a more transparent way.
! Additional tables can be defined in particles.F90

	call init_maxw_table(delgam_i, gamma_table_i, pdf_table_i)
	call init_maxw_table(delgam_e, gamma_table_e, pdf_table_e)

!	call init_maxw_table(1e-4*mi/me, gamma_table_cold, pdf_table_cold)

	
	jglob_min=3+modulo(rank,sizey)*(myall-5.) !global extent of the y boundaries on this processor
	jglob_max=(my-2)+modulo(rank,sizey)*(myall-5.)

	minx=max(x1,3.)
	maxx=min(x2,mx0-2.)

	miny=3.
	maxy=3.
	
	if(y2 < y1) then 
	   print *, rank, ": Inject_Plasma_Region. y2 < y1. y1=",y1," y2=",y2 
	   stop
	endif
	
	inject_miny=y1
	inject_maxy=y2

	if(inject_miny<jglob_min) miny=3.
	if(inject_maxy<jglob_min) maxy=3.
	if(inject_miny>=jglob_max) miny=my-2
	if(inject_maxy>=jglob_max) maxy=my-2

	if(inject_miny>=jglob_min .and. inject_miny<jglob_max) then
	   miny=inject_miny-modulo(rank,sizey)*(myall-5.)
	endif

	if(inject_maxy>=jglob_min .and. inject_maxy<jglob_max) then
	   maxy=inject_maxy-modulo(rank,sizey)*(myall-5.)
	endif

	delta_y=(maxy-miny)

	if(delta_y < 0) print *,"deltay", miny, maxy, inject_miny, inject_maxy

	if (maxy==0 .or. miny==0) then !injection region is outside this CPU's domain
		delta_y=0 
		maxy=0
		miny=0
	endif
	
	!now for x
	if(x2 < x1) then 
	   print *, rank, ": In inject_Plasma_Region. x2 < x1. x1=",x1," x2=",x2 
	   stop
	endif
	
	delta_x=x2-x1
	
#ifdef twoD
	maxz=4.
	minz=3.
	delta_z=(maxz-minz)
#endif
	
#ifndef twoD

	kglob_min=3+(rank/sizey)*(mzall-5)
	kglob_max=(mz-2)+(rank/sizey)*(mzall-5)

	minz=0
	maxz=0
	
	if(z2 < z1) then 
	   print *, rank, ": In inject_Plasma_Region. z2 < z1. z1=",z1," z2=",z2 
	   stop
	endif
	
	inject_minz=z1
	inject_maxz=z2

	if(inject_minz<kglob_min) minz=3.
	if(inject_maxz<kglob_min) maxz=3.
	if(inject_minz>=kglob_max) minz=mz-2
	if(inject_maxz>=kglob_max) maxz=mz-2

	if(inject_minz>=kglob_min .and. inject_minz<kglob_max) then
	   minz=inject_minz-(rank/sizey)*(mzall-5)
	endif

	if(inject_maxz>=kglob_min .and. inject_maxz<kglob_max) then
	   maxz=inject_maxz-(rank/sizey)*(mzall-5)
	endif

	delta_z=(maxz-minz)

	if (maxz==0 .or. minz==0) then !injection region is outside this CPU's domain
		delta_z=0 
		maxz=0
		minz=0
	endif

#endif

!now ready to inject the plasma region

	n=0
	
	!compute number to be injected from Poisson statistics
	
	numps=(.5*ppc)*delta_z*delta_y*delta_x

	if(numps < 10) then 
	   numps=poisson(numps)
	else 
	   numps=ceiling(numps)
	endif

	call check_overflow_num(numps)	

	do while(n < int(numps))
		
		n=n+1
		ions=ions+1
		lecs=lecs+1

		!  Add some random spread to these regular spacings:
		
		p(ions)%x=minx+delta_x * random(dseed) !revers(n,3) 
		p(ions)%y=miny+delta_y * random(dseed) 
		p(ions)%z=minz+delta_z * random(dseed) 
		
		!  Place electrons in the same locations as ions for zero charge
		!  density (consistent with zero or uniform inital electric fields):
		p(maxhlf+lecs)%x=p(ions)%x
		p(maxhlf+lecs)%y=p(ions)%y
		p(maxhlf+lecs)%z=p(ions)%z

		p(ions)%ch=weight 
		p(maxhlf+lecs)%ch=weight
		
		if (use_density_profile) then
			values(1)=(p(ions)%x-3.)/c_omp
			values(2)=((p(ions)%y+modulo(rank,sizey)*(myall-5)) -3.)/c_omp
			values(3)=((p(ions)%z+(rank/sizey)*(mzall-5))-3.)/c_omp
			p(ions)%ch=evalf(1,values)
			p(maxhlf+lecs)%ch=evalf(1,values)
		endif

		call maxwell_dist(gamma_drift,c,dseed,p(ions)%u,p(ions)%v,p(ions &
		)%w,gamma_table_i, pdf_table_i,pdf_sz)	
	
		call maxwell_dist(gamma_drift,c,dseed,p(lecs+maxhlf)%u,p(lecs+maxhlf)%v,p(lecs &
		+maxhlf)%w,gamma_table_e, pdf_table_e,pdf_sz)	

		if(direction.eq.2)then
		   tmp=p(ions)%u
		   p(ions)%u=p(ions)%v
		   p(ions)%v=tmp

		   tmp=p(maxhlf+lecs)%u
		   p(maxhlf+lecs)%u=p(maxhlf+lecs)%v
		   p(maxhlf+lecs)%v=tmp
		endif

		if(direction.eq.3)then
		   tmp=p(ions)%u
		   p(ions)%u=p(ions)%w
		   p(ions)%w=tmp

		   tmp=p(maxhlf+lecs)%u
		   p(maxhlf+lecs)%u=p(maxhlf+lecs)%w
		   p(maxhlf+lecs)%w=tmp
		endif

		totalpartnum =totalpartnum+1
		p(ions)%ind=totalpartnum
		p(ions)%proc=rank
		p(ions)%splitlev=1
		totalpartnum =totalpartnum+1
		p(maxhlf+lecs)%ind=totalpartnum
		p(maxhlf+lecs)%proc=rank
		p(maxhlf+lecs)%splitlev=1
	enddo

		injectedions=injectedions+n
		injectedlecs=injectedlecs+n
	
end subroutine inject_plasma_region




#ifdef twoD
end module m_particles
#else
end module m_particles_3d
#endif
