!
! Output module
!
! This module takes care of all the outputs from tristan
! (calls specific output procedures for each kind of output)
!
!

#ifdef twoD 

module m_output

	use m_system
	use m_aux
	use m_communications
	use m_fields
	use m_particles
	use m_domain
	use m_globaldata
	use m_inputparser
	use m_user
	
#else

module m_output_3d

	use m_system_3d
	use m_aux_3d
	use m_communications_3d
	use m_fields_3d
	use m_particles_3d
	use m_domain_3d
	use m_globaldata_3d
	use m_inputparser_3d
	use m_user_3d
#endif

	use hdf5
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

	real(dprec) :: time_end, time_diff, time_beg, timespan

	integer(8) :: laprestart, namedrestartint
	integer :: intrestlocal, last, interval, torqint, istep1, graphics, ips, intrestart, dlap, &
			   pltstart, restart_status, stride, sele, seli, idx, idy, idz, istep

			   
	logical :: writetestpart, selectprt 
	character (len=5) :: indchar
	character (len=7) :: lapchar
	character (len=9) :: frootprt, frootfld, froottrq
	character (len=19) :: fnametrq
	character (len=100) :: fnameprt, fnamefld
	character (len=46) :: frestartfld, frestartprt
	character (len=34) :: frestartprtloc, frestartfldloc
	character (len=70) :: frestartprtlap
	character (len=10) :: jobname
    real(sprec), allocatable, dimension(:,:,:) :: temporary1,temporary2, &
    temporary3,temporary4,temporary5,temporary6,temporary7,temporary8, &
    temporary9,temporary10,temporary11,temporary12,temporary13,temporary14, &
    temporary15,temporary16

	real(sprec), allocatable, dimension(:,:,:) :: nav, navi
	type(prtnum),allocatable :: selprti(:), selprte(:) 
	
	real, allocatable, dimension(:,:,:) :: gam, pxm, pym, pzm

!-------------------------------------------------------------------------------
!	INTERFACE DECLARATIONS
!-------------------------------------------------------------------------------
	
!-------------------------------------------------------------------------------
!	PUBLIC MODIFIERS
!-------------------------------------------------------------------------------

	! public functions
	
	public :: output_parameters_in_use, diagnostics, initialize_outputs, read_input_output

	! public variables 

	public :: frestartprtlap, frootprt, frootfld, froottrq, & 									   !used in restart module
			   last, interval, torqint, graphics, ips, c_omp, writetestpart, &     ! used in initialize module
			  selectprt, intrestlocal, rankchar, jobname, frestartfld, frestartprt, laprestart, &  ! used in initialize module
			  lapchar, frestartprtloc, frestartfldloc, idx, idy, idz, timespan, time_beg, &        ! used in initialize module
			  intrestart, namedrestartint, pltstart, istep1, stride, istep						   ! used in initialize module
	

!-------------------------------------------------------------------------------
!	MODULE PROCEDURES AND FUNCTIONS
!-------------------------------------------------------------------------------

	contains



!-------------------------------------------------------------------------------
! 						subroutine read_input_output					 
!																		
! Reads any variables related to (or needed by) this module
! 							
!-------------------------------------------------------------------------------

subroutine read_input_output()

	implicit none
	
	! local variables 
	
	integer :: lwritetestpart, lselectprt
	
	call inputpar_geti_def("output", "interval", 50, interval)
	call inputpar_geti_def("output", "torqint", 2000000, torqint)
	call inputpar_geti_def("output", "pltstart", 0, pltstart)

	call inputpar_geti_def("output", "istep", 2, istep)
	call inputpar_geti_def("output", "istep1", 2, istep1)
	call inputpar_geti_def("output", "stride", 100, stride)

	call inputpar_geti_def("output", "writetestpart", 0, lwritetestpart)
	call inputpar_geti_def("output", "selectprt", 1, lselectprt)
	
	if (lwritetestpart==1) then
		writetestpart=.true.
	else
		writetestpart=.false.
	endif
	
	if (lselectprt==1) then
		selectprt=.true.
	else
		selectprt=.false.
	endif

end subroutine read_input_output



!-------------------------------------------------------------------------------
! 						subroutine initialize_outputs					 
!																		
! Initializes some variables used in the outputs module
!  							
!-------------------------------------------------------------------------------

subroutine initialize_outputs()

	idx=2!5 !averaging
	idy=2!5
	idz=2!5
	
#ifdef twoD
		idz=0
#endif
	
end subroutine initialize_outputs



!-------------------------------------------------------------------------------
! 						subroutine output_parameters_in_use					 
!																		
! Outputs the initial parameters read from the input file
!  							
!-------------------------------------------------------------------------------

subroutine output_parameters_in_use()

	implicit none

	print *, "Read the following parameters from the input file"
	print *, "mx0, my0, mz0", mx0, my0, mz0
	print *, "maxptl0", maxptl0
	print *, "sizey", sizey, "caseinit", caseinit
	print *, "c,sigma, c_omp, gamma0,delgam,ppc0,me,mi,freevar", c, &
	sigma,c_omp,  gamma0,  delgam ,ppc0,   me, mi ,dummyvar 
	print *, "irestart", irestart,"intrestart",intrestart,"laprest", &
	laprestart,"namedrestint", namedrestartint, intrestlocal
	print *,"periodicx,periodicy,periodicz",periodicx,periodicy,periodicz
	print *," movwin,wall,shiftint,shiftstart",movwin,wall,shiftint,shiftstart
	print *," Highorder",Highorder
	print *, "Corr", corr
	print *, "Last", last
	print *, "interval, torqint", interval, torqint
	print *, "istep, istep1", istep, istep1
	print *, "ntimes", ntimes,"cleanfld",cleanfld,"cleanint",cleanint
	print *, "Graphics, ips", graphics, ips
	print *, "debug", debug
	print *, "btheta, bphi", btheta, bphi
	print *,"cooling, acool",cooling, acool
	print *,"writetestpart, selectprt",writetestpart,selectprt
	print *, "rank", rank, periodicx, radiationx, gamma0

#ifdef HDF5
	print *,"hdf5 def"
#endif

	print *, "cooling=", cooling, acool
	print *, "maxptl0",maxptl0
	print *, rank,":", "mz", mz, "mzall", mzall,"mz0",mz0
	print *, rank,":","mzlast",mzlast
	print *,rank,":","my", my, "myall", myall, "my0", my0
	print *, "mylast",mylast
	print *, "rank:", rank, frestartfldlap
	if(rank.eq.0) then
		print *, rank, ":", "qe=", qe
	endif

	print *,"Hello, world! I am ", rank, " of ", size0 
	print *,rank,":", frootprt, " ", frootfld," ",frestartfld

end subroutine output_parameters_in_use



!-------------------------------------------------------------------------------
! 						subroutine Diagnostics					 
!																		
! Manages all outputs to disk
!  							
!-------------------------------------------------------------------------------

subroutine Diagnostics()

	implicit none
	
		dlap=25

		!     if(testpartout .and. modulo(lap,testpartint) .eq. 0) then 
		
		if(writetestpart) then
			if(modulo(lap,dlap) .eq. 0) then 
				if (selectprt) then
					call write_testp1()
				else
					if (lap .gt. last-int(0.5*dlap)) then
						call write_testp0()
						prt_first=.false.
					endif
				endif
			endif
		endif
		
		! add output, diagnostics, checkpointing, etc

		
		if(debug) print *,rank,": output", "step=", lap ,"step=",lap
		
		!!!!!-->        

#ifdef HDF5
			call output_par()
			call save_spectrum()
			call save_momentumspec()
#endif
		
		!         call output()
		
		!call timer(12)

		if(debug) print *,rank,": b4 diagnost", "step=", lap

		call do_diagnostics()

end subroutine Diagnostics



!-------------------------------------------------------------------------------
! 						subroutine do_diagnostics					 
!																		
!
!  							
!-------------------------------------------------------------------------------

subroutine do_diagnostics()

	implicit none
	
	! local variables
	
	integer ::request, add,status(MPI_STATUS_SIZE), n, ierr
	real dz
	real, allocatable :: templecsloc(:), tempionsloc(:), templecs(:,:) &
	, tempions(:,:)
	!temppar(:),tempparall(:)
	integer, allocatable :: pararr(:,:), pararrloc(:)
	integer ::ni,nl,totion,totlec, ni1, nl1, maxions, maxlecs,sizein &
	,sizeout
	integer ::maxlecsloc,maxionsloc
	integer*8 redparts(10), reduc(10)
	real norm
	
	
	redparts(1)=ions
	redparts(2)=lecs
	redparts(3)=nionout
	redparts(4)=nlecout
	redparts(5)=injectedions
	redparts(6)=injectedlecs
	redparts(7)=receivedions
	redparts(8)=receivedlecs
	redparts(9)=nioneject
	redparts(10)=nleceject
	
	call MPI_reduce(redparts,reduc,10,mpi_integer8,mpi_sum,0 &
	,MPI_Comm_World,ierr)
	
	if(rank.eq.0) print *, "lap",lap, reduc(1), reduc(2)
	
	if(ions.gt.maxhlf .or. lecs.gt.maxhlf) then 
		print *,rank,": OVERFLOW ions,lecs,maxhlf",ions,lecs,maxhlf
		stop
	endif
	
	dz=.1
	
	
	if(modulo(lap,torqint).eq. 0 .and. graphics .eq.1 ) then 
		call meanq_dens_cur()
	endif
	
end subroutine do_diagnostics



!-------------------------------------------------------------------------------
! 						subroutine save_spectrum					 
!																		
!
!  							
!-------------------------------------------------------------------------------

subroutine save_spectrum()

	implicit none
	
	! local variables
	
	integer ::gambins, xbin, gbin, ishock, ierr
	real ishockall
	real gammin, gammax, gammax1, dgam,dxslice, c, gam
	real maxdens, mindens, middens, gammin1
	logical logscale
	integer ::n, i, ind
	character fname*20
	character(len=10) dsetname(20)
	
#ifdef HDF5
		integer(HID_T) :: file_id       ! File identifier 
		integer(HID_T) :: dset_id       ! Dataset identifier 
		integer(HID_T) :: dspace_id
		integer(HSIZE_T) data_dims(2), data_dims1(1)
#endif

	integer ::datarank, error
	
	real, allocatable:: spece(:,:), specp(:,:), specin(:,:)
	real, allocatable :: xgamma(:), xslice(:), dens(:)
	real, allocatable :: dens1(:), densin(:), dens2(:)!, densin1(:)
	

	logscale=.true.
	gammin=1
	gammax=1 !+(gamma0-1)*100
	gambins=100*2
	dxslice=100
	
	!      debug=.true.
	
	if(lap .ge.pltstart .and. modulo((lap-pltstart),interval) .eq.0 .or. lap .eq. -190001   )then
		
		do i=1,ions
			gam=sqrt(1+(p(i)%u**2+p(i)%v**2+p(i)%w**2)) 
			if(gam .gt. gammax) gammax=gam
			if(gam .lt. gammin) gammin=gam
		enddo
		do i=maxhlf+1,maxhlf+lecs
			gam=sqrt(1+(p(i)%u**2+p(i)%v**2+p(i)%w**2)) 
			if(gam .gt. gammax) gammax=gam
			if(gam .lt. gammin) gammin=gam
		enddo
		
		call mpi_allreduce(gammax,gammax1,1,mpi_read,mpi_max &
		,mpi_comm_world,ierr)
		gammax=gammax1
		call mpi_allreduce(gammin,gammin1,1,mpi_read,mpi_min &
		,mpi_comm_world,ierr)
		gammin=gammin1
		
		gammin=max(gammin, 1.+1e-5)
		
		dgam=(gammax-gammin)/gambins
		
		if(logscale)dgam=(log10(gammax-1)-log10(gammin-1))/gambins
		
		allocate(spece(max(int(mx0/dxslice),1),gambins), dens(max(int(mx0/10),1)))
		allocate(xgamma(gambins),xslice(max(int(mx0/dxslice),1)) &
		,dens2(max(int(mx0/10),1)))
		allocate(specp(max(int(mx0/dxslice),1),gambins))
		allocate(specin(max(int(mx0/dxslice),1),gambins))
		allocate(dens1(max(int(mx0/dxslice),1)))
		allocate(densin(max(int(mx0/dxslice),1)))!,densin1(mx0/10) )
		
		do i=1,gambins
			xgamma(i)=10.**((i-1.)*dgam+log10(gammin-1))
		enddo
		
		spece(:,:)=0.
		specp(:,:)=0.
		specin(:,:)=0.
		dens1(:)=0.
		dens(:)=0.
		
		!find where is the shock
		maxdens=0
		mindens=0
		do n=1,ions
			i=int(p(n)%x)
			dens(max(min(int(i/10.+1),mx0/10),1))=dens(max(min(int(i/10.+1) &
			,mx0/10),1))+1
		enddo
		
		call mpi_allreduce(dens,dens2,max(mx0/10,1),mpi_read,mpi_sum &
		,mpi_comm_world,ierr)
		
		dens=dens2/size0
		
		maxdens=maxval(dens)
		do i=1,mx0/10
			if(dens(i) .gt. maxdens/10.) then
				if(mindens.lt.dens(i)) mindens=dens(i)
			endif
		enddo
		
		middens=minval(abs(dens-(maxdens+mindens)/2.))
		
		do i=1,mx/10
			if(abs(dens(i)-(maxdens+mindens)/2.) .eq. middens) then
				ishock=i*10. -5
			endif
		enddo
		
		709  continue
		
		if(mx .lt. 100) ishock=1
		
		if(rank.eq.0) print *, rank,": ishock=", ishock
		
		if(rank .eq. 0) print *, rank,": Shock location=", ishock
		
		
		!define boundaries of x zones. Move them to fit the shock 
		!at boundary between 2 cells
	
		do i=1,max(int(mx0/dxslice),1)
			!just do linear filling of slices
			if(debug) print *, rank,": writing xslice,  i=",i
			xslice(i)=(i-1)*dxslice+dxslice/2
		enddo
	
		!compute the spectrum
	
		if(debug) print *, "in spec, b4 ions"
		do i=1,ions
			!the shock is always going to be in the middle of the output grid
			!        xbin=int((p(i)%x-ishock)/dxslice+1)+(mx/dxslice)/2
			!one of the grids always falls on the shock, but shock moves -- this captures
			!the whole box
			!         xbin=int((p(i)%x-ishock)/dxslice+1)+(ishock*1./dxslice)
			
			xbin=int(p(i)%x/dxslice+1)
			
			if(xbin .ge. 1 .and. xbin .le. max(int(mx0/dxslice),1)) then
				gam=sqrt(1+(p(i)%u**2+p(i)%v**2+p(i)%w**2)) 
				
				!            gbin=int(gam/dgam+1)
				
				!            if(logscale)gbin=int(alog10(gam)/dgam+1)
				
				if(logscale)gbin=int((alog10(gam-1)-alog10(gammin-1))/dgam+1)
				if(gbin .ge. 1 .and. gbin .le. gambins ) then 
					dens1(xbin)=dens1(xbin)+real(splitratio)**(1.-real(p(i &
					)%splitlev))*p(i)%ch
					
					specp(xbin,gbin)=specp(xbin,gbin)+real(splitratio)**(1. &
					-real(p(i)%splitlev))*p(i)%ch
				endif
			endif
		enddo
	
		if(debug) print *, "in spec, b4 lecs"
		do i=maxhlf,maxhlf+lecs
			!the shock is always going to be in the middle of the output grid
			!         xbin=int((p(i)%x-ishock)/dxslice+1)+(mx/dxslice)/2
			!one of the grids always falls on the shock, but shock moves -- this captures
			!the whole box
			!         xbin=int((p(i)%x-ishock)/dxslice+1)+(ishock*1./dxslice)
			
			xbin=int(p(i)%x/dxslice+1)
			
			if(xbin .ge. 1 .and. xbin .le. max(int(mx0/dxslice),1)) then
				gam=sqrt(1+(p(i)%u**2+p(i)%v**2+p(i)%w**2)) 
				
				!            gbin=int(gam/dgam+1)
				
				!            if(logscale)gbin=int(alog10(gam)/dgam+1)
				
				if(logscale)gbin=int((alog10(gam-1)-alog10(gammin-1))/dgam+1)
				
				if(gbin .ge. 1 .and. gbin .le. gambins ) then 
					dens1(xbin)=dens1(xbin)+real(splitratio)**(1.-real(p(i &
					)%splitlev))*p(i)%ch
					spece(xbin,gbin)=spece(xbin,gbin)+real(splitratio)**(1. &
					-real(p(i)%splitlev))*p(i)%ch
				endif
			endif         
		enddo
		
		call MPI_BARRIER(MPI_COMM_WORLD,ierr)
		if(debug) print *, rank, "reducing spectra"
		
		!now reduce all to rank 0, which will save everything
		
		call mpi_allreduce(spece,specin,max(int(mx0/dxslice),1)*gambins &
		,mpi_read,mpi_sum,mpi_comm_world,ierr)
		
		spece=specin
		specin(:,:)=0.
		
		call mpi_allreduce(specp,specin,max(int(mx0/dxslice),1)*gambins &
		,mpi_read,mpi_sum,mpi_comm_world,ierr)
		
		specp=specin
		
		call mpi_allreduce(dens1,densin,max(int(mx0/dxslice),1) &
		,mpi_read,mpi_sum,mpi_comm_world,ierr)
		
		dens1=densin/size0
		
		
		do i=1,max(int(mx0/dxslice),1)
			spece(i,:)=spece(i,:)/((xgamma)*alog10(10.))
			specp(i,:)=specp(i,:)/((xgamma)*alog10(10.))
		enddo
	
		call MPI_BARRIER(MPI_COMM_WORLD,ierr)
		
		if(debug) print *, rank, "done spec calculation, start writing"
		
		if(rank .eq. 0) then 
		
			!         print *, "specp", specp      
			
			ind=(lap-pltstart)/interval
			
			write(indchar,"(i3.3)")ind
			
			fname="output/spect."//trim(indchar)
			
			open(unit=11,file=fname,form='unformatted')
			close(11,status='delete')
			
			if(debug) print *,rank, ": in spec,starting save"
		
#ifdef HDF5
				!
				!  Initialize FORTRAN predefined datatypes
				!
				call h5open_f(error) 
				call h5fcreate_f(fname,H5F_ACC_TRUNC_F,file_id,error)
				
				dsetname(1)="spece"
				datarank=2
				
				data_dims(1)=max(int(mx0/dxslice),1)
				data_dims(2)=gambins
				
				call h5screate_simple_f(datarank, data_dims, dspace_id, error)
				
				call h5dcreate_f(file_id, dsetname(1), H5T_NATIVE_REAL &
				,dspace_id,dset_id,error)
				
				call h5dwrite_f(dset_id, H5T_NATIVE_REAL,spece, data_dims, error)
				
				call h5dclose_f(dset_id,error)
				call h5sclose_f(dspace_id,error)
				
				dsetname(1)="specp"
				datarank=2
				
				data_dims(1)=max(int(mx0/dxslice),1)
				data_dims(2)=gambins
				
				call h5screate_simple_f(datarank, data_dims, dspace_id, error)
				
				call h5dcreate_f(file_id, dsetname(1), H5T_NATIVE_REAL &
				,dspace_id,dset_id,error)
				
				call h5dwrite_f(dset_id, H5T_NATIVE_REAL,specp, data_dims, error)
				
				call h5dclose_f(dset_id,error)
				call h5sclose_f(dspace_id,error)
				
				
				dsetname(2)="gamma"
				datarank=1
				
				data_dims1(1)=gambins
				call h5screate_simple_f(datarank, data_dims1, dspace_id, error)
				
				call h5dcreate_f(file_id, dsetname(2), H5T_NATIVE_REAL &
				,dspace_id,dset_id,error)
				
				call h5dwrite_f(dset_id, H5T_NATIVE_REAL,xgamma,data_dims1,error)
				
				call h5dclose_f(dset_id,error)
				call h5sclose_f(dspace_id,error)      
				
				
				
				dsetname(2)="xsl"
				datarank=1
				
				data_dims1(1)=max(int(mx0/dxslice),1)
				call h5screate_simple_f(datarank, data_dims1, dspace_id, error)
				
				call h5dcreate_f(file_id, dsetname(2), H5T_NATIVE_REAL &
				,dspace_id,dset_id,error)
				
				call h5dwrite_f(dset_id, H5T_NATIVE_REAL,xslice,data_dims1,error)
				
				call h5dclose_f(dset_id,error)
				call h5sclose_f(dspace_id,error)      
				
				
				
				dsetname(2)="dens"
				datarank=1
				
				data_dims1(1)=max(int(mx0/dxslice),1)
				call h5screate_simple_f(datarank, data_dims1, dspace_id, error)
				
				call h5dcreate_f(file_id, dsetname(2), H5T_NATIVE_REAL &
				,dspace_id,dset_id,error)
				
				call h5dwrite_f(dset_id, H5T_NATIVE_REAL,dens1,data_dims1,error)
				
				call h5dclose_f(dset_id,error)
				call h5sclose_f(dspace_id,error)      
				
				
				
				dsetname(2)="dens0"
				datarank=1
				
				data_dims1(1)=max(int(mx0/10),1)
				call h5screate_simple_f(datarank, data_dims1, dspace_id, error)
				
				call h5dcreate_f(file_id, dsetname(2), H5T_NATIVE_REAL &
				,dspace_id,dset_id,error)
				
				call h5dwrite_f(dset_id, H5T_NATIVE_REAL,dens,data_dims1,error)
				
				call h5dclose_f(dset_id,error)
				call h5sclose_f(dspace_id,error)      
				
				
				
				dsetname(2)="xshock"
				datarank=1
				
				data_dims1(1)=1
				call h5screate_simple_f(datarank, data_dims1, dspace_id, error)
				
				call h5dcreate_f(file_id, dsetname(2), H5T_NATIVE_REAL &
				,dspace_id,dset_id,error)
				
				call h5dwrite_f(dset_id, H5T_NATIVE_REAL,ishock*1.,data_dims1 &
				,error)
				
				call h5dclose_f(dset_id,error)
				call h5sclose_f(dspace_id,error)      
				
				
				dsetname(2)="gmin"
				datarank=1
				
				data_dims1(1)=1
				call h5screate_simple_f(datarank, data_dims1, dspace_id, error)
				
				call h5dcreate_f(file_id, dsetname(2), H5T_NATIVE_REAL &
				,dspace_id,dset_id,error)
				
				call h5dwrite_f(dset_id, H5T_NATIVE_REAL,gammin,data_dims1 &
				,error)
				
				call h5dclose_f(dset_id,error)
				call h5sclose_f(dspace_id,error)      
				
				
				dsetname(2)="gmax"
				datarank=1
				
				data_dims1(1)=1
				call h5screate_simple_f(datarank, data_dims1, dspace_id, error)
				
				call h5dcreate_f(file_id, dsetname(2), H5T_NATIVE_REAL &
				,dspace_id,dset_id,error)
				
				call h5dwrite_f(dset_id, H5T_NATIVE_REAL,gammax,data_dims1 &
				,error)
				
				call h5dclose_f(dset_id,error)
				call h5sclose_f(dspace_id,error)      
				
				
				dsetname(2)="dgam"
				datarank=1
				
				data_dims1(1)=1
				call h5screate_simple_f(datarank, data_dims1, dspace_id, error)
				
				call h5dcreate_f(file_id, dsetname(2), H5T_NATIVE_REAL &
				,dspace_id,dset_id,error)
				
				call h5dwrite_f(dset_id, H5T_NATIVE_REAL,dgam,data_dims1 &
				,error)
				
				call h5dclose_f(dset_id,error)
				call h5sclose_f(dspace_id,error)      
				
				
				call h5fclose_f(file_id, error)
				
				call h5close_f(error)
#endif
		endif !if rank eq 0
		
		call MPI_BARRIER(MPI_COMM_WORLD,ierr)
		if(debug) print *, rank, "in spec deallocating"
		if(debug) print *, rank,"al spece?", allocated(spece)
		call MPI_BARRIER(MPI_COMM_WORLD,ierr)
		if(debug) print *, rank,"al specp?", allocated(specp)
		call MPI_BARRIER(MPI_COMM_WORLD,ierr)
		if(debug) print *, rank,"al dens?", allocated(dens)
		
		if(allocated(spece)) deallocate(spece)
		if(debug) print *, rank, "deallocated spece"
		
		!      if(allocated(spece) .and. allocated(dens)) deallocate(spece,
		!     &          dens)
		call MPI_BARRIER(MPI_COMM_WORLD,ierr)
		if(debug) print *, rank, "in spec deallocating 1"
		
		if(allocated(xgamma) .and. allocated(xslice)) deallocate(xgamma &
		,xslice)
		call MPI_BARRIER(MPI_COMM_WORLD,ierr)
		if(debug) print *, rank, "removed xgamma, xslice"
		
		if(allocated(specp)) deallocate(specp)
		call MPI_BARRIER(MPI_COMM_WORLD,ierr)
		if(debug) print *, rank, "removed specp"
		
		if(allocated(specin))deallocate(specin)
		call MPI_BARRIER(MPI_COMM_WORLD,ierr)
		if(debug) print *, rank, "removed spece"
		
		if(allocated(dens1)) deallocate(dens1, dens2)
		call MPI_BARRIER(MPI_COMM_WORLD,ierr)
		if(debug) print *, rank, "removed dens1"
		
		if(allocated(densin)) deallocate(densin)
		call MPI_BARRIER(MPI_COMM_WORLD,ierr)
		if(debug) print *, rank, "removed densin"
		
		if(debug) print *, rank, "about to deallocate dens"
		if(allocated(dens)) deallocate(dens)
		if(debug) print *, rank, "deallocated dens"
		
		if(debug) print *, rank, "done spec"
	
	endif ! if lap is right for output
	
	
	call MPI_BARRIER(MPI_COMM_WORLD,ierr)
	
	!      debug=.false.
	
end subroutine save_spectrum



!-------------------------------------------------------------------------------
! 						subroutine save_momentumspec					 
!																		
!
!  							
!-------------------------------------------------------------------------------

subroutine save_momentumspec()
  use hdf5 
  real ishockall, ishock, c, dxslice
  real maxdens, mindens, middens
  integer mombins, xbin, mbin, mbinneg, ierr
  real mommin, mommax, mommin1, mommax1, dmom, mom
  logical logscale
  integer n, i, ind, var
  character fname*80
  character(len=20) dsetname(20)
  
#ifdef HDF5
  INTEGER(HID_T) :: file_id ! File identifier 
  INTEGER(HID_T) :: dset_id ! Dataset identifier 
  integer(HID_T) :: dspace_id
  INTEGER(HSIZE_T) data_dims(2), data_dims1(1)
#endif
  integer datarank, error
  
  real, allocatable :: spece(:,:), specp(:,:), specin(:,:)
  real, allocatable :: logmom(:), xslice(:)
  real, allocatable :: dens1(:), densin(:),  dens(:), dens2(:)
  
  logscale=.true.
  mombins=100*2
  dxslice=100
  
  IF(lap.ge.pltstart.and.modulo((lap-pltstart),interval).eq.0 &
       .or.lap.eq.-190001) THEN
     
     do var=1,3             !loop over dimensions
        
        select case (var)
        case (1)
           if(rank .eq. 0) PRINT *, "Doing x momenta"
        case (2)
           if(rank .eq. 0) PRINT *, "Doing y momenta"
        case (3)
           if(rank .eq. 0) PRINT *, "Doing z momenta"
        end select
        
        !     Find momenta with maximum and minimum magnitudes            
        mommax=0.
        mommin=1e8
        
        do i=1,ions
           select case (var)
           case (1) 
              mom=p(i)%u
           case (2)
              mom=p(i)%v
           case (3)
              mom=p(i)%w
           end select
           
           if(abs(mom).gt. mommax) mommax=abs(mom)
           if(abs(mom).lt. mommin) mommin=abs(mom)
        enddo
        
        do i=maxhlf+1,maxhlf+lecs
           select case (var)
           case (1) 
              mom=p(i)%u
           case (2)
              mom=p(i)%v
           case (3)
              mom=p(i)%w
           end select
           
           if(abs(mom).gt. mommax) mommax=abs(mom)
           if(abs(mom).lt. mommin) mommin=abs(mom)
        enddo
        
        
        !     Compare vals from each processor and determine global maxima and minima.
        call mpi_allreduce(mommax,mommax1,1,mpi_read,mpi_max &
             ,mpi_comm_world,ierr)
        mommax=mommax1
        call mpi_allreduce(mommin,mommin1,1,mpi_read,mpi_min &
             ,mpi_comm_world,ierr)
        mommin=mommin1
        
        !     checks
        IF(mommin.eq.0.and.mommax.eq.0) THEN
           if(rank .eq. 0) PRINT *, "No interesting momenta"
        ENDIF
        mommin=max(mommin,10.**-5.)
        IF(mommax.eq.0.)THEN
           mommax=10.**-4.
        ENDIF
        
        !     Compute logarithmic binsize.      
        IF(logscale)dmom=(log10(mommax)-log10(mommin))/(mombins/2.)  
        IF(dmom.eq.0)THEN
           dmom=10.**(-5.)
        ENDIF
        
        if(rank.eq.0) print *, "mommin=", mommin, "mommax=", mommax
        if(rank.eq.0) print *, "dmom=", dmom
        
        !     Allocate appropriate storage for logarithmic histogram.
        allocate(spece(max(int(mx0/dxslice),1),mombins))
        allocate(specp(max(int(mx0/dxslice),1),mombins))
        allocate(specin(max(int(mx0/dxslice),1),mombins))
        allocate(logmom(mombins))
        allocate(xslice(max(int(mx0/dxslice),1)))
        allocate(dens(max(int(mx0/10),1)))
        allocate(dens2(max(int(mx0/10),1)))
        allocate(dens1(max(int(mx0/dxslice),1)))
        allocate(densin(max(int(mx0/dxslice),1)))
        
        !     Compute values of logarithmic bins
        do i=1,mombins/2
           !               logmom(i+mombins/2)=10.**((i-1)*dmom)*mommin
           !               logmom(mombins/2+1-i)=-(10.**((i-1)*dmom)*mommin)
           logmom(i+mombins/2)=10.**((i-0.5)*dmom)*mommin
           logmom(mombins/2+1-i)=-(10.**((i-0.5)*dmom)*mommin)
        enddo
        
        !     Set default spectra to 0. 
        spece(:,:)=0.
        specp(:,:)=0.
        specin(:,:)=0.
        dens1(:)=0.
        densin(:)=0.
        dens(:)=0.
        dens2(:)=0.
        
        !     find where is the shock
        maxdens=0
        mindens=1e8
        do n=1,ions
           i=int(p(n)%x)               
           dens(min(int(i/10.+1),mx0/10))= &
                dens(min(int(i/10.+1),mx0/10))+ &
                real(splitratio)**(1.-real(p(n)%splitlev))*p(n)%ch
        enddo
        call mpi_allreduce(dens,dens2,mx0/10,mpi_read,mpi_sum &
             ,mpi_comm_world,ierr)            
        dens=dens2/size0
        
        maxdens=maxval(dens)
        do i=1,mx0/10
           if(dens(i) .gt. maxdens/10.) then
              if(dens(i).lt.mindens)mindens=dens(i)
           endif
        enddo
        middens=minval(abs(dens-(maxdens+mindens)/2.))            
        do i=1,mx/10
           if(abs(dens(i)-(maxdens+mindens)/2.) .eq. middens) then
              ishock=i*10.-5
           endif
        enddo
        
        if(rank .eq. 0) print *, rank,": Shock location=", ishock
        
        !     define spatial bins
        do i=1,max(int(mx0/dxslice),1)
           xslice(i)=(i-1)*dxslice+dxslice/2.
        enddo
        
        if(debug) print *, rank, ": spec calculation"
        
        !     Compute the spectrum for ions
        do i=1,ions
           
           !     Compute the x bin
           xbin=int(p(i)%x/dxslice+1)
           
           !     Compute momentum bin
           mbin=mombins/2+1
           if(xbin .ge. 1 .and. xbin .le. int(mx/dxslice)) then
              select case (var)
              case (1) 
                 mom=p(i)%u
              case (2)
                 mom=p(i)%v
              case (3)
                 mom=p(i)%w
              end select
              
              !     Boost momentum within limits (logscale)
              if(abs(mom).lt.mommin)then
                 if(mom.ge.0)then
                    mom=mommin 
                 else
                    mom=-mommin
                 endif
              endif
              
              !     Compute the logarithmically binned spectra.
              if(mom.lt.0)then
                 if(logscale) mbinneg=int((alog10(abs(mom))- &
                      alog10(mommin))/dmom+1)
                 mbin=mombins/2-mbinneg+1
              endif
              if(mom.gt.0)then
                 if(logscale) mbin=int((alog10(abs(mom))- &
                      alog10(mommin))/dmom+1)+mombins/2
              endif
              
              if(mbin .ge. 1 .and. mbin .le. mombins ) then 
                 dens1(xbin)=dens1(xbin)+ &
                      real(splitratio)**(1.-real(p(i)%splitlev))*p(i)%ch
                 specp(xbin,mbin)=specp(xbin,mbin)+ &
                      real(splitratio)**(1.-real(p(i)%splitlev))*p(i)%ch
              endif
           endif !xbin
        enddo !over ions
        
        !     Compute the spectrum for electrons.
        do i=maxhlf,maxhlf+lecs

           !     Compute the x bin
           xbin=int(p(i)%x/dxslice+1)
           
           !     Compute momentum bin
           mbin=mombins/2+1
           if(xbin .ge. 1 .and. xbin .le. int(mx/dxslice)) then
              select case (var)
              case (1) 
                 mom=p(i)%u
              case (2)
                 mom=p(i)%v
              case (3)
                 mom=p(i)%w
              end select
              
              !     Boost momentum within limits
              if(abs(mom).lt.mommin)then
                 if(mom.ge.0)then
                    mom=mommin 
                 else
                    mom=-mommin
                 endif
              endif
              
              !     Compute the logarithmic bin.
              if(mom.lt.0) then
                 if(logscale) mbinneg=int((alog10(abs(mom))- &
                      alog10(mommin))/dmom+1)
                 mbin=mombins/2-mbinneg+1
              endif
              if(mom.gt.0)then
                 if(logscale) mbin=int((alog10(abs(mom))- &
                      alog10(mommin))/dmom+1)+mombins/2 
              endif
              
              !     Compute the logarithmic spectra.
              if(mbin .ge. 1 .and. mbin .le. mombins ) then 
                 dens1(xbin)=dens1(xbin)+ &
                      real(splitratio)**(1.-real(p(i)%splitlev))*p(i)%ch
                 spece(xbin,mbin)=spece(xbin,mbin)+ &
                      real(splitratio)**(1.-real(p(i)%splitlev))*p(i)%ch
              endif
           endif  !xbin       
           
        enddo !over lecs
        
        call MPI_BARRIER(MPI_COMM_WORLD,ierr)
        if(debug) print *, rank, "reducing spectra"
        
        !     now reduce all to rank 0, which will save everything
        !     Sum the logarithmic spectra and make the final spectra global.
        call mpi_allreduce(spece,specin,max(int(mx0/dxslice),1) &
             *mombins,mpi_read,mpi_sum,mpi_comm_world,ierr)
        
        spece=specin
        specin(:,:)=0.
        
        call mpi_allreduce(specp,specin,max(int(mx0/dxslice),1) &
             *mombins,mpi_read,mpi_sum,mpi_comm_world,ierr)
        
        specp=specin
        
        call mpi_allreduce(dens1,densin,max(int(mx0/dxslice),1) &
             ,mpi_read,mpi_sum,mpi_comm_world,ierr)
        
        dens1=densin/size0
        
        !     Convert from dN/dlogp to dN/dp.
        do i=1,max(int(mx0/dxslice),1)
           spece(i,:)=spece(i,:)/abs(logmom)
           specp(i,:)=specp(i,:)/abs(logmom)
        enddo
        
        call MPI_BARRIER(MPI_COMM_WORLD,ierr)
        
        if(debug) print *, rank, "done spec calc,now writing"
        
        if(rank .eq. 0) then 
           ind=(lap-pltstart)/interval
           write(indchar,"(i3.3)")ind               
           fname="output/momentum."//trim(indchar)               
           if(var.eq.1) then 
              if(rank.eq.0)PRINT *, "filename=", fname
              open(unit=11,file=fname,form='unformatted')
              close(11,status='delete')
           endif
#ifdef HDF5
           
           !     Initialize FORTRAN predefined datatypes.               
           !     Create storage file to save spectra to.               
           IF(var.eq.1)THEN 
              CALL h5open_f(error) 
              CALL h5fcreate_f(fname,H5F_ACC_TRUNC_F,file_id,error)
           ENDIF
           
           !     Write logarithmic spectra.
           select case (var)
           case (1)
              dsetname(1)="pxelogsp"
           case (2)
              dsetname(1)="pyelogsp"
           case (3)
              dsetname(1)="pzelogsp"
           end select
           
           datarank=2          
           data_dims(1)=max(int(mx0/dxslice),1)
           data_dims(2)=mombins
           CALL h5screate_simple_f(datarank,data_dims,dspace_id, &
                error)               
           call h5dcreate_f(file_id, dsetname(1), H5T_NATIVE_REAL &
                ,dspace_id,dset_id,error)
           CALL h5dwrite_f(dset_id, H5T_NATIVE_REAL,spece,data_dims &
                , error)
           call h5dclose_f(dset_id,error)
           call h5sclose_f(dspace_id,error)
           
           select case (var)
           case (1)
              dsetname(1)="pxplogsp"
           case (2)
              dsetname(1)="pyplogsp"
           case (3)
              dsetname(1)="pzplogsp"
           end select
           
           datarank=2               
           data_dims(1)=max(int(mx0/dxslice),1)
           data_dims(2)=mombins               
           CALL h5screate_simple_f(datarank, data_dims, dspace_id,&
                error)
           call h5dcreate_f(file_id, dsetname(1), H5T_NATIVE_REAL &
                ,dspace_id,dset_id,error)               
           CALL h5dwrite_f(dset_id, H5T_NATIVE_REAL,specp,data_dims &
                , error)               
           call h5dclose_f(dset_id,error)
           call h5sclose_f(dspace_id,error)         
           
           select case (var)
           case (1)
              dsetname(2)="pxbin"
           case (2)
              dsetname(2)="pybin"
           case (3)
              dsetname(2)="pzbin"
           end select
           
           datarank=1              
           data_dims1(1)=mombins
           CALL h5screate_simple_f(datarank, data_dims1,dspace_id, &
                error)               
           call h5dcreate_f(file_id, dsetname(2), H5T_NATIVE_REAL &
                ,dspace_id,dset_id,error)               
           CALL h5dwrite_f(dset_id, H5T_NATIVE_REAL,logmom, &
                data_dims1,error)               
           call h5dclose_f(dset_id,error)
           call h5sclose_f(dspace_id,error)      
           
           select case (var)
           case (1)
              dsetname(1)="pxlim"
           case (2)
              dsetname(1)="pylim"
           case (3)
              dsetname(1)="pzlim"
           end select
           
           datarank=1               
           data_dims1(1)=2               
           CALL h5screate_simple_f(datarank, data_dims1,dspace_id, & 
                error)               
           call h5dcreate_f(file_id, dsetname(1), H5T_NATIVE_REAL &
                ,dspace_id,dset_id,error)               
           CALL h5dwrite_f(dset_id,H5T_NATIVE_REAL,[mommin, mommax], &
                data_dims1, error)               
           call h5dclose_f(dset_id,error)
           call h5sclose_f(dspace_id,error)
           
           select case (var)
           case (1)
              dsetname(2)="dpx"
           case (2)
              dsetname(2)="dpy"
           case (3)
              dsetname(2)="dpz"
           end select
           
           datarank=1               
           data_dims1(1)=1
           CALL h5screate_simple_f(datarank, data_dims1, dspace_id, &
                error)               
           call h5dcreate_f(file_id, dsetname(2), H5T_NATIVE_REAL &
                ,dspace_id,dset_id,error)               
           CALL h5dwrite_f(dset_id, H5T_NATIVE_REAL,dmom,data_dims1 &
                ,error)               
           call h5dclose_f(dset_id,error)
           call h5sclose_f(dspace_id,error)      
           
           if(var .eq. 1) then
              dsetname(2)="xshock"
              datarank=1                  
              data_dims1(1)=1
              CALL h5screate_simple_f(datarank, data_dims1, &
                   dspace_id, error)                  
              call h5dcreate_f(file_id, dsetname(2), H5T_NATIVE_REAL &
                   ,dspace_id,dset_id,error)                  
              CALL h5dwrite_f(dset_id, H5T_NATIVE_REAL,ishock*1., &
                   data_dims1,error)                  
              call h5dclose_f(dset_id,error)
              call h5sclose_f(dspace_id,error)      
              
              dsetname(2)="xsl"               
              datarank=1               
              data_dims1(1)=max(int(mx0/dxslice),1)
              CALL h5screate_simple_f(datarank,data_dims1,dspace_id, &
                   error)               
              call h5dcreate_f(file_id, dsetname(2), H5T_NATIVE_REAL &
                   ,dspace_id,dset_id,error)               
              CALL h5dwrite_f(dset_id, H5T_NATIVE_REAL,xslice, &
                   data_dims1,error)
              
              call h5dclose_f(dset_id,error)
              call h5sclose_f(dspace_id,error) 
              
              dsetname(2)="dens"
              datarank=1
              data_dims1(1)=max(int(mx0/dxslice),1)
              CALL h5screate_simple_f(datarank, data_dims1, & 
                   dspace_id, error)                  
              call h5dcreate_f(file_id, dsetname(2), H5T_NATIVE_REAL &
                   ,dspace_id,dset_id,error)                  
              CALL h5dwrite_f(dset_id, H5T_NATIVE_REAL,dens1, &
                   data_dims1,error)                  
              call h5dclose_f(dset_id,error)
              call h5sclose_f(dspace_id,error)      
              
              dsetname(2)="dens0" 
              datarank=1                  
              data_dims1(1)=max(int(mx0/10),1)
              CALL h5screate_simple_f(datarank, data_dims1, &
                   dspace_id, error)                  
              call h5dcreate_f(file_id, dsetname(2), H5T_NATIVE_REAL &
                   ,dspace_id,dset_id,error)                  
              CALL h5dwrite_f(dset_id, H5T_NATIVE_REAL,dens, &
                   data_dims1,error)                  
              call h5dclose_f(dset_id,error)
              call h5sclose_f(dspace_id,error)            
           endif !var eq 1
           
           !     Close main file.!
           If (var.eq.3) THEN
              CALL h5fclose_f(file_id, error)
              CALL h5close_f(error)
           ENDIF
#endif
        endif               !if rank eq 0
        if(debug) print *, rank, "done writing, now cleaning"
        
        call MPI_BARRIER(MPI_COMM_WORLD,ierr)
        if(debug) print *, rank, "in spec deallocating"
        if(debug) print *, rank,"al spece?", allocated(spece)
        call MPI_BARRIER(MPI_COMM_WORLD,ierr)
        if(debug) print *, rank,"al specp?", allocated(specp)
        call MPI_BARRIER(MPI_COMM_WORLD,ierr)
        if(debug) print *, rank,"al dens?", allocated(dens)
        
        if(allocated(logmom)) deallocate(logmom)
        call MPI_BARRIER(MPI_COMM_WORLD,ierr)
        if(debug) print *, rank, "removed logmom"
        
        if(allocated(xslice)) deallocate(xslice)
        call MPI_BARRIER(MPI_COMM_WORLD,ierr)
        if(debug) print *, rank, "removed xslice"
        
        if(allocated(spece)) deallocate(spece)
        call MPI_BARRIER(MPI_COMM_WORLD,ierr)
        if(debug) print *, rank, "deallocated spece"
        
        if(allocated(specp)) deallocate(specp)
        call MPI_BARRIER(MPI_COMM_WORLD,ierr)
        if(debug) print *, rank, "removed specp"
        
        if(allocated(specin))deallocate(specin)
        call MPI_BARRIER(MPI_COMM_WORLD,ierr)
        if(debug) print *, rank, "removed specin"
        
        if(debug) print *, rank, "about to deallocate dens"
        if(allocated(dens)) deallocate(dens)
        call MPI_BARRIER(MPI_COMM_WORLD,ierr)
        if(debug) print *, rank, "deallocated dens"            
        
        if(allocated(dens1)) deallocate(dens1)
        call MPI_BARRIER(MPI_COMM_WORLD,ierr)
        if(debug) print *, rank, "removed dens1"
        
        if(allocated(dens2)) deallocate(dens2)
        call MPI_BARRIER(MPI_COMM_WORLD,ierr)
        if(debug) print *, rank, "removed dens2"
        
        if(allocated(densin)) deallocate(densin)
        call MPI_BARRIER(MPI_COMM_WORLD,ierr)
        if(debug) print *, rank, "removed densin"
        
        if(debug) print *, rank, "done momentum spec"
        
     end do                 ! var
  endif                     ! if lap is right for output
  
  call MPI_BARRIER(MPI_COMM_WORLD,ierr)
  
end subroutine save_momentumspec

			
!-------------------------------------------------------------------------------
! 						subroutine output_trq					 
!																		
! Outputs the grid quantities to an hdf5 file (never called)
!  							
!-------------------------------------------------------------------------------

subroutine output_trq()

	implicit none

	! local variables
	
	integer :: ind, is, kglob, kloc, kstart, kfinish,nz, fh,info, i, j, k, n
	integer ::jstart, jfinish,ny,iv, my1, mz1, mx1
	real bx0,by0,bz0,ex0,ey0 ,ez0,bxd,byd,bzd,exd,eyd,ezd, curx0, &
	cury0, curz0
	real*4,allocatable::temporary_vec(:)
	integer ::error, error_n  ! Error flags
	integer ::nvars, midvars,stride
	integer ::all_counts(size0) ! array to know the sizes of other proc array
	integer ::all_ions(size0), all_lecs(size0)
	character(len=9) dsetname(20)
	integer(HID_T) :: file_id ! File identifier 
	integer(HID_T) :: dset_id(20) ! Dataset identifier 
	integer(HID_T) :: filespace(20) ! Dataspace identifier in file 
	integer(HID_T) :: memspace ! Dataspace identifier in memory
	integer(HID_T) :: plist_id ! Property list identifier 
	integer(HSIZE_T) dimsf(3),dimsfions(1), dimsflecs(1)
	integer(HSIZE_T) dimsfi(7), dimsfiions(7), dimsfilecs(7)
	integer ::skipflag,datarank
	integer(HSIZE_T), DIMENSION(3) :: count  
	integer(HSSIZE_T), DIMENSION(3) :: offset
	integer(HSIZE_T), DIMENSION(1) :: countpart  
	integer(HSSIZE_T), DIMENSION(1) :: offsetpart
	!      integer ::istep1
	
	
	ind=(lap)/torqint
	write(fnametrq,"(a9,i3.3)") "ftrq.sml.",ind
	if(rank.eq.0) write(*,*) rank,": name",fnametrq
	
	!      istep1= 4
	skipflag=0
	dimsf(1)=mx0/istep1
	dimsf(2)=my0/istep1

#ifndef twoD
		dimsf(3)=mz0/istep1
#else
		dimsf(3)=1
#endif
	
	dimsfi(1:3)=dimsf(1:3)
	dimsfi(4:7)=0
	datarank=3
	
	nvars=12
	
	if(debug .and. rank .eq. 0) print *, "in output_par_trq"
	
	kstart=3
	kfinish=mz-3
	if(rank/sizey .eq. 0) kstart=1
	if(rank/sizey .eq. sizez-1) kfinish=mz
	
#ifndef twoD
		110  if(modulo(kstart+(rank/sizey)*(mzall-5)-1*0,istep1) .eq. 0) goto 120
		kstart=kstart+1
		goto 110
		120  continue
		
		130  if(modulo(kfinish+(rank/sizey)*(mzall-5)-1*0,istep1) .eq. 0) goto 140
		kfinish=kfinish-1
		goto 130
		140  continue
		mz1=(kfinish-kstart)/istep1+1
#else
		kstart=1
		kfinish=1
		mz1=1
#endif
	
	jstart=3
	jfinish=my-3
	if(modulo(rank,sizey) .eq. 0) jstart=1
	if(modulo(rank,sizey) .eq. sizey-1) jfinish=my
	
	
	210  if(modulo(jstart+modulo(rank,sizey)*(myall-5)-1*0,istep1) .eq. 0) goto 220
	jstart=jstart+1
	goto 210
	220  continue
	
	230  if(modulo(jfinish+modulo(rank,sizey)*(myall-5)-1*0,istep1).eq. 0) goto 240
	jfinish=jfinish-1
	goto 230
	240  continue
	
	my1=(jfinish-jstart)/istep1+1
	
	!      print *, rank,":","mz1=", mz1, kstart, kfinish, istep1,mzall,mz
	
	allocate( temporary1(mx0/istep1,my1,mz1))
	
	!
	!  Initialize FORTRAN predefined datatypes
	!
	call h5open_f(error) 
	
	! 
	! Setup file access property list with parallel I/O access.
	!
#ifdef MPI
		call h5pcreate_f(H5P_FILE_ACCESS_F, plist_id, error)
		call h5pset_fapl_mpio_f(plist_id, mpi_comm_world, mpi_info_null, &
		error)
#endif

	dsetname(1)="ex"
	dsetname(2)="ey"
	dsetname(3)="ez"
	dsetname(4)="bx"
	dsetname(5)="by"
	dsetname(6)="bz"
	dsetname(7)="dens"      
	dsetname(8)="jx"
	dsetname(9)="jy"
	dsetname(10)="jz"
	dsetname(11)="Temp"
	dsetname(12)="dummy"
	
	!
	! Create the file collectively.
	! 

#ifdef MPI
		call h5fcreate_f(fnametrq, H5F_ACC_TRUNC_F, file_id, error, &
		access_prp = plist_id)
		call h5pclose_f(plist_id, error)
#else
		call h5fcreate_f(fnametrq, H5F_ACC_TRUNC_F, file_id, error)
#endif

	!
	! Create the data space for the  dataset. 
	!

	do i=1,nvars
		call h5screate_simple_f(datarank, dimsf, filespace(i), error)
	enddo
	!      print *,rank, ": filespace=",filespace
	
	!
	! Create the dataset with default properties.
	!
	do i=1,nvars
		call h5dcreate_f(file_id, dsetname(i), H5T_NATIVE_REAL, &
		filespace(i),dset_id(i), error)
		call h5sclose_f(filespace(i), error)
	enddo
	
	!
	! Each process defines dataset in memory and writes it to the hyperslab
	! in the file. 
	!
	count(1) = dimsf(1)
	count(2) = my1 !dimsf(2)
	count(3) = mz1
	
	!need to communicate with others to find out the offset
#ifdef MPI
		call mpi_allgather(mz1,1,mpi_integer,all_counts,1,mpi_integer &
		,mpi_comm_world, error)
#endif

	if(rank .eq. 0) print *, "allcounts mz1=",all_counts
	offset(1) = 0
	offset(2) = 0
	offset(3) = 0
	!z offset
	if(rank/sizey .gt. 0) then 
		offset(3) = sum(all_counts(1:(rank/sizey)*sizey:sizey))
		if(debug)print *,rank,":","offset3=", offset(3), "count=",count
	endif

#ifdef twoD
		offset(3)=0
#endif
#ifdef MPI
		!now get the y offset
		call mpi_allgather(my1,1,mpi_integer,all_counts,1,mpi_integer &
		,mpi_comm_world, error)
#endif

	if(modulo(rank,sizey) .gt. 0) then 
		offset(2)=sum( &
		all_counts((rank/sizey)*sizey+1:rank))
	endif

	if(debug) print *,rank,":","offset2=",offset(2)
	
	if(debug)print *,rank,": offsets",offset 

#ifdef MPI
		call h5screate_simple_f(datarank, count, memspace, error) 
#endif

	!
	! Create property list for collective dataset write
	!
#ifdef MPI
		call h5pcreate_f(H5P_DATASET_XFER_F, plist_id, error) 
		call h5pset_dxpl_mpio_f(plist_id, H5FD_MPIO_COLLECTIVE_F, error)
#endif
	! 
	! Select hyperslab in the file.
	!
	do iv=1,nvars
#ifdef MPI
			call h5dget_space_f(dset_id(iv), filespace(iv), error)
			
			
			call h5sselect_hyperslab_f (filespace(iv),H5S_SELECT_SET_F,offset &
			,count, error)
#endif
	
		!
		! Write the dataset collectively. 
		!
		!      print *,rank,": before h5dwrite" 
		!      temporary1=temporary_arr(:,:,:,iv)
		!_arr(:,:,:,iv)
		
		do k=kstart, kfinish, istep1 !1,mz/istep1
			nz=(k-kstart)/istep1+1
			do j=jstart, jfinish,istep1 !1,my/istep1
				ny=(j-jstart)/istep1+1
				do i=1,mx/istep1
					
					call interpolfld(real(i*istep1),real(j),real(k),bx0 &
					,by0,bz0,ex0,ey0,ez0,bxd,byd,bzd,exd,eyd,ezd)
					
					if(iv.eq.1)temporary1(i,ny,nz)=real(ex0+exd,4)
					if(iv.eq.2)temporary1(i,ny,nz)=real(ey0+eyd,4)
					if(iv.eq.3)temporary1(i,ny,nz)=real(ez0+ezd,4)
					if(iv.eq.4)temporary1(i,ny,nz)=real(bx0+bxd,4)
					if(iv.eq.5)temporary1(i,ny,nz)=real(by0+byd,4)
					if(iv.eq.6)temporary1(i,ny,nz)=real(bz0+bzd,4)
					if(iv.eq.7)temporary1(i,ny,nz)=real(nav(i*istep1,j,k),4)
					
					call interpolcurrent(real(i*istep1),real(j),real(k) &
					,curx0, cury0, curz0)
					
					if(iv.eq.8)temporary1(i,ny,nz)=real(curx0,4)
					if(iv.eq.9)temporary1(i,ny,nz)=real(cury0,4)
					if(iv.eq.10)temporary1(i,ny,nz)=real(curz0,4)
					if(iv.eq.11)temporary1(i,ny,nz)=real(temp(i*istep1,j,k),4)
					if(iv.eq.12)temporary1(i,ny,nz)=0. !real(rh(i*istep1,j,k),4)
					
				enddo
			enddo
		enddo
		
#ifdef MPI       
			call h5dwrite_real_1(dset_id(iv), H5T_NATIVE_REAL, temporary1(:,: &
			,:),dimsfi,error,file_space_id = filespace(iv), mem_space_id &
			=memspace,xfer_prp = plist_id)
			
#else
			call h5dwrite_f(dset_id(iv), H5T_NATIVE_REAL, temporary1(:,: &
			,:),dimsfi,error)
			
#endif
	
	enddo !ivar

	!
	! Close dataspaces.
	!

#ifdef MPI
		do iv=1,nvars
			call h5sclose_f(filespace(iv), error)
		enddo
		call h5sclose_f(memspace, error)
#endif

	!
	! Close the dataset and property list.
	!

	do iv=1,nvars
		call h5dclose_f(dset_id(iv), error)
	enddo

#ifdef MPI
		call h5pclose_f(plist_id, error)
#endif

	!
	! Close the file.
	!
	call h5fclose_f(file_id, error)
	!
	! Close FORTRAN predefined datatypes.
	!
	call h5close_f(error)
	
	if(debug) print *, rank,": finished writing fields"
	
	
	mx1=mx/istep
	my1=my/istep
	mz1=mz/istep
	
	goto 7001
	!-------------------------------------
	!now write particles
	
	!
	!  Initialize FORTRAN predefined datatypes
	!
	call h5open_f(error) 
	
	! 
	! Setup file access property list with parallel I/O access.
	!
#ifdef MPI
		call h5pcreate_f(H5P_FILE_ACCESS_F, plist_id, error)
		call h5pset_fapl_mpio_f(plist_id, mpi_comm_world, mpi_info_null, &
		error)
#endif

	nvars=14
	midvars=7
	stride=20 !1 !100 !60 !15 !*4
	dsetname(1)="xi"
	dsetname(2)="yi"
	dsetname(3)="zi"
	dsetname(4)="ui"
	dsetname(5)="vi"
	dsetname(6)="wi"
	dsetname(7)="gammai"
	dsetname(8)="xe"
	dsetname(9)="ye"
	dsetname(10)="ze"
	dsetname(11)="ue"
	dsetname(12)="ve"
	dsetname(13)="we"
	dsetname(14)="gammae"
	
	dsetname(15)="spectrum"
	!need to add writing spectrum from root
	
	datarank=1
	!find out number of ions and lecs
#ifdef MPI
		call mpi_allgather(ions,1,mpi_integer,all_ions,1,mpi_integer &
		,mpi_comm_world, error)
		call mpi_allgather(lecs,1,mpi_integer,all_lecs,1,mpi_integer &
		,mpi_comm_world, error)
#else
		all_ions=ions
		all_lecs=lecs
#endif
	
	dimsfions(1)=sum(all_ions/stride*1.)
	dimsflecs(1)=sum(all_lecs/stride*1.)
	dimsfiions(1)=dimsfions(1)
	dimsfilecs(1)=dimsflecs(1)
	dimsfiions(2:7)=0
	dimsfilecs(2:7)=0

	if(rank .eq. 0) then 
		print *, "all_ions", all_ions
		print *, "all_lecs", all_lecs
		print *,"dimsions", sum(all_ions/stride*1.)
		print *,"dimslecs", sum(all_lecs/stride*1.)
	endif
	
	allocate(temporary_vec(max(ions/stride,lecs/stride)))
	
	!
	! Create the file collectively.
	! 
#ifdef MPI
		call h5fcreate_f(fnameprt, H5F_ACC_TRUNC_F, file_id, error, &
		access_prp = plist_id)
		call h5pclose_f(plist_id, error)
#else
		call h5fcreate_f(fnameprt, H5F_ACC_TRUNC_F, file_id, error)
#endif
	
	!
	! Create the data space for the  dataset. 
	!
	do i=1,midvars
		call h5screate_simple_f(datarank, dimsfions(1), filespace(i), &
		error)
	enddo
	do i=midvars+1,nvars
		call h5screate_simple_f(datarank, dimsflecs(1), filespace(i), &
		error)
	enddo
	
	!
	! Create the dataset with default properties.
	!
	do i=1,nvars
		call h5dcreate_f(file_id, dsetname(i), H5T_NATIVE_REAL, &
		filespace(i),dset_id(i), error)
		call h5sclose_f(filespace(i), error)
	enddo
	
	!
	! Create property list for collective dataset write
	!
#ifdef MPI
		call h5pcreate_f(H5P_DATASET_XFER_F, plist_id, error) 
		call h5pset_dxpl_mpio_f(plist_id, H5FD_MPIO_COLLECTIVE_F, error)
#endif
	! 
	! Select hyperslab in the file.
	!
	do i=1,nvars
		
		!
		! Each process defines dataset in memory and writes it to the hyperslab
		! in the file. 
		!
		if(i.le.midvars)countpart = ions/stride
		if(i.gt.midvars)countpart = lecs/stride
		
		!      if(rank .eq. 0) print *, "allcounts=",all_counts
		offsetpart = 0
		if(rank .gt. 0) then 
			if(i .le. midvars) then
				offsetpart = sum(all_ions(1:rank)/stride)
			else
				offsetpart = sum(all_lecs(1:rank)/stride)
			endif
		endif
#ifdef MPI
			call h5screate_simple_f(1, countpart, memspace, error) 
			
			call h5dget_space_f(dset_id(i), filespace(i), error)
			call h5sselect_hyperslab_f (filespace(i),H5S_SELECT_SET_F &
			,offsetpart,countpart, error)
#endif
		!
		! Write the dataset collectively. 
		!
		!      print *,rank,": before h5dwrite" 
		!      temporary1=temporary_arr(:,:,:,i)
		!_arr(:,:,:,i)
		
		if(i.eq.1)  temporary_vec(1:ions/stride)=p(1:ions:stride)%x
		if(i.eq.2) temporary_vec(1:ions/stride)=p(1:ions:stride)%y
		if(i.eq.3) temporary_vec(1:ions/stride)=p(1:ions:stride)%z+rank &
		*(mzall-5)
		if(i.eq.4) temporary_vec(1:ions/stride)=p(1:ions:stride)%u
		if(i.eq.5) temporary_vec(1:ions/stride)=p(1:ions:stride)%v
		if(i.eq.6) temporary_vec(1:ions/stride)=p(1:ions:stride)%w
		!         if(i.eq.7) temporary_vec(1:ions/stride)=sqrt(1./(1.
		!     &             -(p(1:ions:stride)%u**2+p(1:ions:stride)%v**2
		!     &             +p(1:ions:stride)%w **2)/c**2))
		
		if(i.eq.7) temporary_vec(1:ions/stride)=sqrt(1. &
		+(p(1:ions:stride)%u**2+p(1:ions:stride)%v**2 &
		+p(1:ions:stride)%w**2))
		
		
		if(i.eq.8) temporary_vec(1:lecs/stride)=p(maxhlf+1:maxhlf &
		+lecs:stride)%x
		if(i.eq.9) temporary_vec(1:lecs/stride)=p(maxhlf+1:maxhlf &
		+lecs:stride)%y
		if(i.eq.10) temporary_vec(1:lecs/stride)=p(maxhlf+1:maxhlf &
		+lecs:stride)%z+rank*(mzall-5)
		if(i.eq.11) temporary_vec(1:lecs/stride)=p(maxhlf+1:maxhlf &
		+lecs:stride)%u
		if(i.eq.12) temporary_vec(1:lecs/stride)=p(maxhlf+1:maxhlf &
		+lecs:stride)%v
		if(i.eq.13) temporary_vec(1:lecs/stride)=p(maxhlf+1:maxhlf &
		+lecs:stride)%w
		!         if(i.eq.14) temporary_vec(1:lecs/stride)=sqrt(1./(1. -(p(maxhlf
		!     &             +1:maxhlf+lecs:stride)%u**2+p(maxhlf+1:maxhlf +lecs:stride
		!     &             )%v**2+p(maxhlf +1:maxhlf+lecs:stride)%w**2)/c**2))
		
		if(i.eq.14) temporary_vec(1:lecs/stride)=sqrt(1. +(p(maxhlf &
		+1:maxhlf+lecs:stride)%u**2+p(maxhlf+1:maxhlf +lecs:stride &
		)%v**2+p(maxhlf +1:maxhlf+lecs:stride)%w**2))
		
#ifdef MPI
			if(i .le. midvars) then 
				call h5dwrite_f(dset_id(i), H5T_NATIVE_REAL, &
				temporary_vec(1:ions/stride), dimsfiions,error &
				,file_space_id = filespace(i), mem_space_id=memspace &
				,xfer_prp = plist_id)
			else 
				call h5dwrite_f(dset_id(i), H5T_NATIVE_REAL, &
				temporary_vec(1:lecs/stride), dimsfilecs,error &
				,file_space_id = filespace(i), mem_space_id=memspace &
				,xfer_prp = plist_id)
			endif
#else
			if(i .le. midvars) then 
				call h5dwrite_f(dset_id(i), H5T_NATIVE_REAL, &
				temporary_vec(1:ions/stride), dimsfiions,error)
			else 
				call h5dwrite_f(dset_id(i), H5T_NATIVE_REAL, &
				temporary_vec(1:lecs/stride), dimsfilecs,error)
			endif
			
#endif
#ifdef MPI
			call h5sclose_f(memspace,error)
#endif
	enddo
	!
	! Close dataspaces.
	!
	do i=1,nvars
		call h5sclose_f(filespace(i), error)
	enddo
	!
	! Close the dataset and property list.
	!
	do i=1,nvars
		call h5dclose_f(dset_id(i), error)
	enddo
#ifdef MPI
		call h5pclose_f(plist_id, error)
#endif
	!
	! Close the file.
	!
	call h5fclose_f(file_id, error)
	!
	! Close FORTRAN predefined datatypes.
	!
	call h5close_f(error)
	
	if(debug) print *, rank,": finished writing particles"
	
	!now write particles
	!      if(rank.eq.0)then
	
	is=1
	
	goto 1000
	
	write(12)real((ions/is),4),real((lecs/is),4), &
	real(maxptl,4),real(maxhlf,4), &
	(real(p(n)%x,4),n=1,(ions/is)*is,is), &
	(real(p(n)%x,4),n=maxhlf+1,maxhlf+(lecs/is)*is,is), &
	(real(p(n)%y,4),n=1,(ions/is)*is,is), &
	(real(p(n)%y,4),n=maxhlf+1,maxhlf+(lecs/is)*is,is), &
	(real(p(n)%z,4),n=1,(ions/is)*is,is), &
	(real(p(n)%z,4),n=maxhlf+1,maxhlf+(lecs/is)*is,is), &
	(real(p(n)%u,4),n=1,(ions/is)*is,is), &
	(real(p(n)%u,4),n=maxhlf+1,maxhlf+(lecs/is)*is,is), &
	(real(p(n)%v,4),n=1,(ions/is)*is,is), &
	(real(p(n)%v,4),n=maxhlf+1,maxhlf+(lecs/is)*is,is), &
	(real(p(n)%w,4),n=1,(ions/is)*is,is), &
	(real(p(n)%w,4),n=maxhlf+1,maxhlf+(lecs/is)*is,is),     &
	(real(1./sqrt(1.-(p(n)%u**2+p(n)%v**2+p(n)%w**2)/c**2),4),  &
	n=1,(ions/is)*is,is), &
	(real(1./sqrt(1.-(p(n)%u**2+p(n)%v**2+p(n)%w**2)/c**2),4),  &
	n=maxhlf+1,maxhlf+(lecs/is)*is,is)
	
	1000 continue
	
	7001 continue
	deallocate(temporary1) !, temporary_vec)
	
	
end subroutine output_trq



!-------------------------------------------------------------------------------
! 						subroutine output_par					 
!																		
!
!  							
!-------------------------------------------------------------------------------

subroutine output_par()

	implicit none

	! local variables
	
	integer ::sendbuf
	logical exst1
	
	integer ::ind, is, kglob, kloc, kstart, kfinish,nz, fh,info, n, i, j, k, ierr
	integer ::jstart, jfinish,ny, iv, mz1, my1, mx1
	real bx0,by0,bz0,ex0,ey0 ,ez0,bxd,byd,bzd,exd,eyd,ezd, curx0, &
	cury0, curz0
	real*4,allocatable:: temporary_arr(:,:,:,:),temporary_vec(:)
	integer ::error, error_n  ! Error flags
	integer ::nvars, midvars!,stride
	integer ::all_counts(size0) ! array to know the sizes of other proc array
	integer ::all_ions(size0), all_lecs(size0)
	
	integer*8 all_ions_lng(size0), all_lecs_lng(size0)
	
	character(len=10) dsetname(40),dsetnamei(30),dsetnamer(30)
	character(len=20) fparam
	integer,dimension(30)::intdata,dsetsav,dsetbool
	real,dimension(30)::realdata
	integer(HID_T) :: file_id ! File identifier 
	integer(HID_T) :: dset_id(40) ! Dataset identifier 
	integer(HID_T) :: filespace(40) ! Dataspace identifier in file 
	integer(HID_T) :: memspace ! Dataspace identifier in memory
	integer(HID_T) :: plist_id ! Property list identifier 
	
	!for MPI_Info
	
	integer ::FILE_INFO_TEMPLATE
	character keyval*20
	integer ::keyflag
	integer ::procn
	integer ::my1list(size0),mz1list(size0) 
	real*4, allocatable :: temporary0(:,:,:), temporary0_vec(:) 
	
	integer(HSIZE_T) dimsf(3),dimsfions(1), dimsflecs(1)
	integer(HSIZE_T)dimsfi(7), dimsfiions(7), dimsfilecs(7)
	integer ::skipflag,datarank
	integer(HSIZE_T), DIMENSION(3) :: count  
	integer(HSSIZE_T), DIMENSION(3) :: offset
	integer(HSIZE_T), DIMENSION(1) :: countpart  
	integer(HSSIZE_T), DIMENSION(1) :: offsetpart
	integer ::token, request,status(MPI_STATUS_SIZE),request1 &
	,status1(MPI_STATUS_SIZE)
	integer ::tmp1
	real tmp1r
	integer(hid_t) :: fapl
	integer(hsize_t) :: family_size=1000000

	integer ::istep0
	
	skipflag=0
	dimsf(1)=mx0/istep
	dimsf(2)=my0/istep

#ifndef twoD
		dimsf(3)=mz0/istep
#else
		dimsf(3)=1
#endif 
	
	dimsfi(1:3)=dimsf(1:3)
	dimsfi(4:7)=0
	datarank=3
	
	nvars=16-5
	
	!save restart file
	!naming convention: write to rest(flds/prtl).jobname.rank.d
	!if the interval is met, rename the file into rest(flds/prtl).jobname.lapNNNNN.rank.d
	
	sendbuf=0
	if(rank .eq. 0) then 
		inquire(file="write_restart",exist=exst1)
		if(exst1)sendbuf=1
	endif

	call mpi_bcast(sendbuf,1,mpi_integer,0,mpi_comm_world,ierr)
	exst1=.false.

	if(sendbuf .eq. 1) exst1=.true.
	
	if(modulo(lap,1) .eq.0 .or. exst1 ) then
	
#ifdef LOCALRESTART
			if(intrestlocal .gt. 0 .and. modulo(lap,intrestlocal) .eq.0) then 
				open(unit=7,file=frestartfldloc, form='unformatted')
				rewind 7
				if(rank.eq.0)print *, rank, ": writing local restart file"
				write(7)mx,my,mz0,mz,ex,ey,ez,bx,by,bz,dseed,lap,xinject &
				,xinject2,leftwall,split_E_ions,split_E_lecs 
				close(7)
				
				!            open(unit=8,file=frestartprt, form='unformatted')
				!            close(8,status='delete')
				
				open(unit=8,file=frestartprtloc, form='unformatted')
				rewind 8
				print *, rank,": wrote fields, writing local particles"
				
				write(8)ions,lecs,maxptl,maxhlf, &
				(p(n)%x,n=1,ions),(p(n)%x,n=maxhlf+1,maxhlf+lecs), &
				(p(n)%y,n=1,ions),(p(n)%y,n=maxhlf+1,maxhlf+lecs), &
				(p(n)%z,n=1,ions),(p(n)%z,n=maxhlf+1,maxhlf+lecs), &
				(p(n)%u,n=1,ions),(p(n)%u,n=maxhlf+1,maxhlf+lecs), &
				(p(n)%v,n=1,ions),(p(n)%v,n=maxhlf+1,maxhlf+lecs), &
				(p(n)%w,n=1,ions),(p(n)%w,n=maxhlf+1,maxhlf+lecs), &
				(p(n)%ch,n=1,ions),(p(n)%ch,n=maxhlf+1,maxhlf+lecs), &
				(p(n)%ind,n=1,ions),(p(n)%ind,n=maxhlf+1,maxhlf+lecs), &
				(p(n)%proc,n=1,ions),(p(n)%proc,n=maxhlf+1,maxhlf+lecs), &
				(p(n)%splitlev,n=1,ions), &
				(p(n)%splitlev,n=maxhlf+1,maxhlf+lecs)
				
				close(8)
			endif ! if(modulo(lap,intrestlocal) .eq.0)
			7645       continue
#endif
		!------------------------------
		
		call MPI_BARRIER(MPI_COMM_WORLD,ierr)
		
		!     check if I have time to save the restart file
		if(modulo(lap,intrestart) .eq. 0  .or.exst1) then
			if(rank.eq.0) then
				time_end=mpi_wtime()
				time_diff=time_end-time_beg !elapsed time
				print *,"times for restart",real(timespan,4), &
				real(time_diff,4),real(time_cut,4) 
				if(timespan-time_diff.gt.time_cut)then 
					restart_status=1
				else
					restart_status=0
				endif
			endif

			call MPI_BCAST(restart_status,1,MPI_INTEGER,0, &
			MPI_COMM_WORLD,ierr)               
			print *,"rank, restart_status",rank,restart_status
		endif
		
		call MPI_BARRIER(MPI_COMM_WORLD,ierr)                
		
		if((modulo(lap,intrestart) .eq. 0  .or.exst1) .and. restart_status .eq. 1) then 
		
			!install a semaphore
			
#ifdef MPI
				if(rank .gt. 0) then 
					token=rank-1
					!wait for the message from the previous rank
					call mpi_irecv(token,1, &
					mpi_integer,rank-1,100,MPI_Comm_WORLD,request,ierr)
					call mpi_wait(request,status,ierr)
					
					print *,"rank ",rank," received the token from ",rank-1
					
					if(modulo(rank,15).ne.0 .and. rank .ne. size0-1) then
						token=rank
						call mpi_isend(token,1, &
						mpi_integer,rank+1,100,MPI_Comm_WORLD,request1,ierr)
						call mpi_wait(request1,status1,ierr)
					endif
				endif
				3004        continue
#endif
			
			open(unit=7,file=frestartfld, form='unformatted')
			close(7,status='delete')
			
			open(unit=7,file=frestartfld, form='unformatted')
			rewind 7
			if(rank.eq.0)print *, rank, ": writing restart file"
			write(7)mx,my,mz0,mz,ex,ey,ez,bx,by,bz,dseed,lap,xinject &
			,xinject2,leftwall,split_E_ions,split_E_lecs 
			close(7)
			
			open(unit=8,file=frestartprt, form='unformatted')
			close(8,status='delete')
			
			open(unit=8,file=frestartprt, form='unformatted')
			rewind 8
			if(rank .eq. 0) print *, rank,": wrote fields, writing particles"
			
			write(8)ions,lecs,maxptl,maxhlf, &
			(p(n)%x,n=1,ions),(p(n)%x,n=maxhlf+1,maxhlf+lecs), &
			(p(n)%y,n=1,ions),(p(n)%y,n=maxhlf+1,maxhlf+lecs), &
			(p(n)%z,n=1,ions),(p(n)%z,n=maxhlf+1,maxhlf+lecs), &
			(p(n)%u,n=1,ions),(p(n)%u,n=maxhlf+1,maxhlf+lecs), &
			(p(n)%v,n=1,ions),(p(n)%v,n=maxhlf+1,maxhlf+lecs), &
			(p(n)%w,n=1,ions),(p(n)%w,n=maxhlf+1,maxhlf+lecs), &
			(p(n)%ch,n=1,ions),(p(n)%ch,n=maxhlf+1,maxhlf+lecs), &
			(p(n)%ind,n=1,ions),(p(n)%ind,n=maxhlf+1,maxhlf+lecs), &
			(p(n)%proc,n=1,ions),(p(n)%proc,n=maxhlf+1,maxhlf+lecs), &
			(p(n)%splitlev,n=1,ions), &
			(p(n)%splitlev,n=maxhlf+1,maxhlf+lecs)
			
			close(8)
			!           if(rank.eq.0) 
			if(rank.eq.0)print *, rank, ": done writing restart file"
			
			if(modulo(lap,namedrestartint) .eq. 0) then
				if(rank.eq.0)print *, rank, ": renaming restart"
				write(lapchar,"(i6.6)")lap	
				frestartfldlap="restart/restflds."//trim(jobname) &
				//"."//"lap"//trim(lapchar)//"."//trim(rankchar)//".d"
				frestartprtlap="restart/restprtl."//trim(jobname) &
				//"."//"lap"//trim(lapchar)//"."//trim(rankchar)//".d"
				
				
				open(unit=7,file=frestartfldlap, form='unformatted')
				close(7,status='delete')
				
				open(unit=7,file=frestartfldlap, form='unformatted')
				rewind 7
				if(rank.eq.0)print *, rank, ": writing laprestart file"
				write(7)mx,my,mz0,mz,ex,ey,ez,bx,by,bz,dseed,lap,xinject &
				,xinject2,leftwall,split_E_ions,split_E_lecs 
				close(7)
				
				open(unit=8,file=frestartprtlap, form='unformatted')
				close(8,status='delete')
				
				open(unit=8,file=frestartprtlap, form='unformatted')
				rewind 8
				if(rank.eq.0)print *, rank,": wrote fields, writing particles"
				
				write(8)ions,lecs,maxptl,maxhlf, &
				(p(n)%x,n=1,ions),(p(n)%x,n=maxhlf+1,maxhlf+lecs), &
				(p(n)%y,n=1,ions),(p(n)%y,n=maxhlf+1,maxhlf+lecs), &
				(p(n)%z,n=1,ions),(p(n)%z,n=maxhlf+1,maxhlf+lecs), &
				(p(n)%u,n=1,ions),(p(n)%u,n=maxhlf+1,maxhlf+lecs), &
				(p(n)%v,n=1,ions),(p(n)%v,n=maxhlf+1,maxhlf+lecs), &
				(p(n)%w,n=1,ions),(p(n)%w,n=maxhlf+1,maxhlf+lecs), &
				(p(n)%ch,n=1,ions),(p(n)%ch,n=maxhlf+1,maxhlf+lecs), &
				(p(n)%ind,n=1,ions),(p(n)%ind,n=maxhlf+1,maxhlf+lecs), &
				(p(n)%proc,n=1,ions),(p(n)%proc,n=maxhlf+1,maxhlf+lecs), &
				(p(n)%splitlev,n=1,ions), &
				(p(n)%splitlev,n=maxhlf+1,maxhlf+lecs)
				
				close(8)
				!           if(rank.eq.0) 
				if(rank.eq.0)print *, rank, ": done writing restart file"
			endif
			
			!send the message to the next rank, if not the last
		
#ifdef MPI
				if(modulo(rank,15).eq.0 .and. rank .ne. size0-1) then
					token=rank
					call mpi_isend(token,1, &
					mpi_integer,rank+1,100,MPI_Comm_WORLD,request,ierr)
				endif
				2121      continue
				
				call mpi_barrier(MPI_COMM_WORLD,ierr)
#endif
			
			if(rank .eq. 0 .and. exst1) then 
				open(unit=11,file="write_restart",form='unformatted')
				close(11,status='delete')
			endif
			
			!after a barrier can delete the temporary files in /tmp
			!            open(unit=7,file=frestartfldloc, form='unformatted')
			!            close(7,status='delete')
			!            open(unit=7,file=frestartprtloc, form='unformatted')
			!            close(7,status='delete')
			
		endif ! modulo lap 1000
	endif                  !modulo lap,200
	
	!-------------------------------------
	!------------------------------------end restarts
	
	!----now save the data

	if(lap .ge.pltstart .and. modulo((lap-pltstart),interval) .eq.0 .or. lap .eq. -150001 )then
		!     &             .or.lap.eq.1) then
		ind=(lap-pltstart)/interval
		
		!          write(fnamefld,"(a16,i3.3)") "output/fl%d.tot.",ind
		
		
		write(fnamefld,"(a16,i3.3)") "output/flds.tot.",ind
		write(fnameprt,"(a16,i3.3)") "output/prtl.tot.",ind
		
		write(indchar,"(i3.3)")ind
				
		if(rank.eq.0)write(*,*) rank,": ",fnamefld, '  ',fnameprt
		
		!just in case destroy the file
		if(rank .eq. 0) then 
			open(unit=11,file=fnamefld,form='unformatted')
			close(11,status='delete')
			!             open(unit=11,file=fnamefld,form='unformatted')
			
			open(unit=12,file=fnameprt,form='unformatted')
			close(12,status='delete')
		endif
	
		call MPI_BARRIER(MPI_COMM_WORLD,ierr)
		
		if(debug) print *,"in output_par, b4 density"
		!       rh=0
		!       call density(1,ions) !,maxptl,p.x,p.y,p.z,mx,my,mz,rh)
		!       call meanq()
		
		!!!!       call meanq_dens_cur() !compute density 
		
		!       call spectrum()
		if(rank.eq.0)print *,"out of meanq"
		
		
#ifndef serIO 
	
			!defining the template for writing
			!     to be inserted after each instance of h5pcreate_f
			!     call H5Pset_sieve_buf_size_f(plist_id, 262144,error) 
			!     call H5Pset_alignment_f(plist_id, 524288, 262144,error)
			
#ifdef MPI      
				call MPI_Info_create(FILE_INFO_TEMPLATE,error)
				!      call MPI_Info_set(FILE_INFO_TEMPLATE, "IBM_largeblock_io", 
				!     &          "true", error)
				call MPI_Info_set(FILE_INFO_TEMPLATE, "access_style",  &
				"write_once",error)
				call MPI_Info_set(FILE_INFO_TEMPLATE,  &
				"collective_buffering", "true",error)
				call MPI_Info_set(FILE_INFO_TEMPLATE, "cb_block_size", &
				"4194304",error)
				call MPI_Info_set(FILE_INFO_TEMPLATE, "cb_buffer_size", &
				"16777216",error)
				call MPI_Info_set(FILE_INFO_TEMPLATE, "cb_nodes", &
				"1",error)
#endif
			
			
			if(debug .and. rank .eq. 0) print *, "in output_par"
			
			kstart=3
			kfinish=mz-3
			if(rank/sizey .eq. 0) kstart=1
			if(rank/sizey .eq. sizez-1) kfinish=mz
			
			
			110  if(modulo(kstart+(rank/sizey)*(mzall-5)-1*0,istep) .eq. 0) goto 120
			kstart=kstart+1
			goto 110
			120  continue
			
			130  if(modulo(kfinish+(rank/sizey)*(mzall-5)-1*0,istep) .eq. 0) goto 140
			kfinish=kfinish-1
			goto 130
			140  continue
			mz1=(kfinish-kstart)/istep+1
	
#ifdef twoD
				kstart=1
				kfinish=1
				mz1=1
#endif
			
			jstart=3
			jfinish=my-3
			if(modulo(rank,sizey) .eq. 0) jstart=1
			if(modulo(rank,sizey) .eq. sizey-1) jfinish=my
			
			
			210  if(modulo(jstart+modulo(rank,sizey)*(myall-5)-1*0,istep) .eq. 0) goto 220
			jstart=jstart+1
			goto 210
			220  continue
			
			230  if(modulo(jfinish+modulo(rank,sizey)*(myall-5)-1*0,istep) .eq. 0) goto 240
			jfinish=jfinish-1
			goto 230
			240  continue
			
			my1=(jfinish-jstart)/istep+1
			
			!      print *, rank,":","mz1=", mz1, kstart, kfinish, istep,mzall,mz
			
			if(debug) print *, rank,"in output b4 allocate"
			allocate( temporary1(mx0/istep,my1,mz1))
			
			!
			!  Initialize FORTRAN predefined datatypes
			!
			call h5open_f(error) 
			
			! 
			! Setup file access property list with parallel I/O access.
			!
			
#ifdef MPI
				call h5pcreate_f(H5P_FILE_ACCESS_F, plist_id, error)
				!      call H5Pset_sieve_buf_size_f(plist_id, 262144,error) 
				!      call H5Pset_alignment_f(plist_id, 524288, 262144,error)
				call h5pset_fapl_mpio_f(plist_id, mpi_comm_world, &
				FILE_INFO_TEMPLATE,error)
#endif
	
			if(debug) print *,"flds.tot.",ind,rank,"done 0f"
			
			if(debug) print *, rank,"h5pset"
			dsetname(1)="ex"
			dsetname(2)="ey"
			dsetname(3)="ez"
			dsetname(4)="bx"
			dsetname(5)="by"
			dsetname(6)="bz"
			dsetname(7)="ux"
			dsetname(8)="uy"
			dsetname(9)="uz"
			dsetname(10)="Temp"
			dsetname(11)="dens"
			dsetname(12)="densi"
			dsetname(13)="dummy"
			dsetname(14)="jx"
			dsetname(15)="jy"
			dsetname(16)="jz"
			
			
			dsetname(1)="ex"
			dsetname(2)="ey"
			dsetname(3)="ez"
			dsetname(4)="bx"
			dsetname(5)="by"
			dsetname(6)="bz"
			!      dsetname(7)="ux"
			!      dsetname(8)="uy"
			!      dsetname(9)="uz"
			!      dsetname(10)="Temp"
			dsetname(11-4)="dens"
			dsetname(12-4)="densi"
			!      dsetname(13)="dummy"
			dsetname(14-5)="jx"
			dsetname(15-5)="jy"
			dsetname(16-5)="jz"
			
			!
			! Create the file collectively.
			! 
#ifdef MPI
				call h5fcreate_f(fnamefld, H5F_ACC_TRUNC_F, file_id, error, &
				access_prp = plist_id)
				call h5pclose_f(plist_id, error)
#else
				call h5fcreate_f(fnamefld, H5F_ACC_TRUNC_F, file_id, error)
#endif
			!
			! Create the data space for the  dataset. 
			!
			do i=1,nvars
				call h5screate_simple_f(datarank, dimsf, filespace(i), error)
			enddo
			!      print *,rank, ": filespace=",filespace
			
			if(debug) print *,"flds.tot.",ind,rank,"done 1f"
			
			!
			! Create the dataset with default properties.
			!
			do i=1,nvars
				call h5dcreate_f(file_id, dsetname(i), H5T_NATIVE_REAL, &
				filespace(i),dset_id(i), error)
				call h5sclose_f(filespace(i), error)
			enddo
			
			!
			! Each process defines dataset in memory and writes it to the hyperslab
			! in the file. 
			!
			count(1) = dimsf(1)
			count(2) = my1 !dimsf(2)
			count(3) = mz1
			
			!need to communicate with others to find out the offset
#ifdef MPI
				if(debug) print *, rank,": in output, b4 allgather", mz1
				call mpi_allgather(mz1,1,mpi_integer,all_counts,1,mpi_integer &
				,mpi_comm_world, error)
#endif
			if(debug .and. rank .eq. 0) print *, "allcounts mz1=",all_counts
			offset(1) = 0
			offset(2) = 0
			offset(3) = 0
			!z offset
			if(rank/sizey .gt. 0) then 
				offset(3) = sum(all_counts(1:(rank/sizey)*sizey:sizey))
				!      print *,rank,":","offset=", offset(3), "count=",count
			endif
#ifdef twoD 
				offset(3)=0
#endif
			!now get the y offset
#ifdef MPI
				call mpi_allgather(my1,1,mpi_integer,all_counts,1,mpi_integer &
				,mpi_comm_world, error)
#endif
			if(modulo(rank,sizey) .gt. 0) then 
				offset(2)=sum(all_counts((rank/sizey)*sizey+1:rank))
			endif
			if(debug)print *,rank,":","offset=",offset(2),"count=",count
#ifdef MPI
				call h5screate_simple_f(datarank, count, memspace, error) 
#endif
			!
			! Create property list for collective dataset write
			!
#ifdef MPI
				call h5pcreate_f(H5P_DATASET_XFER_F, plist_id, error) 
				call h5pset_dxpl_mpio_f(plist_id, H5FD_MPIO_COLLECTIVE_F, error)
#endif
			
			if(debug)print *,"flds.tot.",ind,rank,"done 2f"
			! 
			! Select hyperslab in the file.
			!
			open(unit=7,file=fenlargeloc, form='unformatted')
			rewind 7
			write(7) (((curx(i,j,k),i=1,mx),j=1,my),k=1,mz), &
			(((cury(i,j,k),i=1,mx),j=1,my),k=1,mz)
			close(7)
	
			do iv=1,nvars
				if(debug)print *,rank,"iv=",iv
				
				if(iv .eq. 11-4) call meanq_dens_cur()
				if(iv .eq. 14-5) then 
					!restore currents from file
					open(unit=7,file=fenlargeloc, form='unformatted')
					rewind 7
					if(debug .and. rank.eq.0) print *, rank,":reading back current file"
					read(7)(((curx(i,j,k),i=1,mx),j=1,my),k=1,mz), &
					(((cury(i,j,k),i=1,mx),j=1,my),k=1,mz)
					!      close(7,status='delete')
					if(debug .and. rank .eq.0)print *, rank,"resetting arrays"
					
				endif
				
#ifdef MPI      
					call h5dget_space_f(dset_id(iv), filespace(iv), error)
					
					call h5sselect_hyperslab_f(filespace(iv),H5S_SELECT_SET_F,offset &
					,count, error)
#endif
				
				if(debug)print *,rank,": selected hyperslab"
				!
				! Write the dataset collectively. 
				!
				!      print *,rank,": before h5dwrite" 
				!      temporary1=temporary_arr(:,:,:,iv)
				!_arr(:,:,:,iv)
				
				do k=kstart, kfinish, istep !1,mz/istep
					nz=(k-kstart)/istep+1
					do j=jstart, jfinish,istep !1,my/istep
						ny=(j-jstart)/istep+1
						do i=1,mx/istep
							
							call interpolfld(real(i*istep),real(j),real(k),bx0 &
							,by0,bz0,ex0,ey0,ez0,bxd,byd,bzd,exd,eyd,ezd)
							
							if(iv.eq.1)temporary1(i,ny,nz)=real(ex0+exd,4)
							if(iv.eq.2)temporary1(i,ny,nz)=real(ey0+eyd,4)
							if(iv.eq.3)temporary1(i,ny,nz)=real(ez0+ezd,4)
							if(iv.eq.4)temporary1(i,ny,nz)=real(bx0+bxd,4)
							if(iv.eq.5)temporary1(i,ny,nz)=real(by0+byd,4)
							if(iv.eq.6)temporary1(i,ny,nz)=real(bz0+bzd,4)						
							
							if(iv.eq.11-4)temporary1(i,ny,nz)=real(curx(i*istep,j,k),4) !nav
							if(iv.eq.12-4)temporary1(i,ny,nz)=real(cury(i*istep,j,k),4) !navi
							
							if(iv .ge. 14-5) then
								call interpolcurrent(real(i*istep),real(j),real(k) &
								,curx0, cury0, curz0)
								
								curx0=curx(i*istep,j,k)
								cury0=cury(i*istep,j,k)
								curz0=curz(i*istep,j,k)
								
							endif
							
							!        if(iv.eq.13-4)temporary1(i,ny,nz)=0.
							if(iv.eq.14-5)temporary1(i,ny,nz)=real(curx0,4)
							if(iv.eq.15-5)temporary1(i,ny,nz)=real(cury0,4)
							if(iv.eq.16-5)temporary1(i,ny,nz)=real(curz0,4)
							
						enddo
					enddo
				enddo
				
				if(debug)print *,rank,": before h5dwrite","filespace",filespace(iv &
				),memspace
				
				if(debug)print *,"flds.tot.",ind,rank,"done 3f"
				
#ifdef MPI
					call h5dwrite_real_1(dset_id(iv), H5T_NATIVE_REAL, temporary1(:,: &
					,:),dimsfi,error,file_space_id = filespace(iv), mem_space_id &
					=memspace,xfer_prp = h5p_default_f)
#else
					call h5dwrite_f(dset_id(iv),H5T_NATIVE_REAL,TEMPORARY1(:,: &
					,:) ,dimsfi,error)
#endif      
				
				if(debug)print *,"flds.tot.",ind,rank,"done 4f"
				
			enddo !ivar
	
			if(debug)print *,rank,": after h5dwrite"
			!
			! Close dataspaces.
			!
	
#ifdef MPI
				do iv=1,nvars
					if(debug)print *, rank, ": ", iv, "closing s"
					call h5sclose_f(filespace(iv), error)
				enddo
				call h5sclose_f(memspace, error)
#endif
	
			!
			! Close the dataset and property list.
			!
			do iv=1,nvars
				if(debug)print *, rank, ": ", iv, "closing d"
				call h5dclose_f(dset_id(iv), error)
			enddo
#ifdef MPI
				if(debug)print *, rank, ": ", "closing p"
				call h5pclose_f(plist_id, error)
#endif
			!
			! Close the file.
			!
			if(debug)print *, rank, ": ", "closing f"
			call h5fclose_f(file_id, error)
			!
			! Close FORTRAN predefined datatypes.
			!
			if(debug)print *, rank, ": ", "closing h5"
			call h5close_f(error)
			
			if(debug) print *, rank,": finished writing fields"
			
			if(debug)print *,"flds.tot.",ind,rank,"done 5f"
			
			!-------------------------------------
			!now write particles
			
			!
			!  Initialize FORTRAN predefined datatypes
			!
			call h5open_f(error) 
			
			! 
			! Setup file access property list with parallel I/O access.
			!
#ifdef MPI
				call h5pcreate_f(H5P_FILE_ACCESS_F, plist_id, error)
				!      call H5Pset_sieve_buf_size_f(plist_id, 262144,error) 
				!      call H5Pset_alignment_f(plist_id, 524288, 262144,error)
				call h5pset_fapl_mpio_f(plist_id, mpi_comm_world, &
				FILE_INFO_TEMPLATE,error)
#endif
			if(debug)print *,"flds.tot.",ind,rank,"done 0p"
			
			nvars=14+2+2+2
			midvars=7+1+1+1
			!      stride=100 !20 !1 !100 !60 !15 !*4
			dsetname(1)="xi"
			dsetname(2)="yi"
			dsetname(3)="zi"
			dsetname(4)="ui"
			dsetname(5)="vi"
			dsetname(6)="wi"
			dsetname(7)="chi"
			dsetname(8)="gammai"
			dsetname(9)="indi"
			dsetname(10)="proci"
			dsetname(11)="xe"
			dsetname(12)="ye"
			dsetname(13)="ze"
			dsetname(14)="ue"
			dsetname(15)="ve"
			dsetname(16)="we"
			dsetname(17)="che"
			dsetname(18)="gammae"
			dsetname(19)="inde"
			dsetname(20)="proce"
			!      dsetname(15)="spectrum"
			!need to add writing spectrum from root
			
			datarank=1
			!find out number of ions and lecs
#ifdef MPI
				tmp1=max(ions,1)
				call mpi_allgather(tmp1,1,mpi_integer,all_ions,1 &
				,mpi_integer,mpi_comm_world, error)
				tmp1=max(lecs,1)
				call mpi_allgather(tmp1,1,mpi_integer,all_lecs,1,mpi_integer &
				,mpi_comm_world, error)
				
				if(rank.eq.0)print *,"all_ions",all_ions, tmp1 
				all_ions_lng=all_ions
				all_lecs_lng=all_lecs
#else
				all_ions_lng=ions
				all_lecs_lng=lecs
#endif
			where(all_ions .lt. stride)all_ions=stride
			where(all_lecs .lt. stride)all_lecs=stride
			where(all_ions_lng .lt. stride)all_ions_lng=stride
			where(all_lecs_lng .lt. stride)all_lecs_lng=stride
			dimsfions(1)=sum(all_ions_lng/stride)
			dimsflecs(1)=sum(all_lecs_lng/stride)
			dimsfiions(1)=dimsfions(1)
			dimsfilecs(1)=dimsflecs(1)
			dimsfiions(2:7)=0
			dimsfilecs(2:7)=0
			if(rank .eq. 0) then 
				print *, "all_ions", all_ions_lng
				print *, "all_lecs", all_lecs_lng
				print *,"dimsions", sum(all_ions_lng/stride)
				print *,"dimslecs", sum(all_lecs_lng/stride)
			endif
			
			!      allocate(temporary_vec(max(ions/stride,lecs/stride)))
			allocate(temporary_vec(max(max(ions/stride,lecs/stride),1)))     
			!
			! Create the file collectively.
			! 
#ifdef MPI
				call h5fcreate_f(fnameprt, H5F_ACC_TRUNC_F, file_id, error, &
				access_prp = plist_id)
				call h5pclose_f(plist_id, error)
#else
				call h5fcreate_f(fnameprt, H5F_ACC_TRUNC_F, file_id, error)
#endif
			
			if(debug)print *,"flds.tot.",ind,rank,"done 1p"
			
			!
			! Create the data space for the  dataset. 
			!
			
			do i=1,midvars
				call h5screate_simple_f(datarank, dimsfions(1), filespace(i), &
				error)
			enddo
			do i=midvars+1,nvars
				call h5screate_simple_f(datarank, dimsflecs(1), filespace(i), &
				error)
			enddo
			
			if(debug)print *,"flds.tot.",ind,rank,"done 2p"
			
			!
			! Create the dataset with default properties.
			!
			do i=1,nvars
				call h5dcreate_f(file_id, dsetname(i), H5T_NATIVE_REAL, &
				filespace(i),dset_id(i), error)
				call h5sclose_f(filespace(i), error)
			enddo
			
			
			
			!
			! Create property list for collective dataset write
			!
#ifdef MPI
				call h5pcreate_f(H5P_DATASET_XFER_F, plist_id, error) 
				call h5pset_dxpl_mpio_f(plist_id, H5FD_MPIO_COLLECTIVE_F, error)
#endif
	
			! 
			! Select hyperslab in the file.
			!
			if(debug)print *,"flds.tot.",ind,rank,"done 3p"
			
			do i=1,nvars
				
				!
				! Each process defines dataset in memory and writes it to the hyperslab
				! in the file. 
				!
				if(i.le.midvars)countpart = max(ions/stride,1)
				if(i.gt.midvars)countpart = max(lecs/stride,1)
				
				offsetpart = 0
				if(rank .gt. 0) then 
					if(i .le. midvars) then
						offsetpart = sum(all_ions_lng(1:rank)/stride)
					else
						offsetpart = sum(all_lecs_lng(1:rank)/stride)
					endif
				endif
				
#ifdef MPI
					call h5screate_simple_f(1, countpart, memspace, error) 
					
					call h5dget_space_f(dset_id(i), filespace(i), error)
					
					call h5sselect_hyperslab_f(filespace(i),H5S_SELECT_SET_F &
					,offsetpart,countpart, error)
					
#endif
				
				if(debug)print *,"flds.tot.",ind,rank,"done 4p"
				!
				! Write the dataset collectively. 
				!
				!      print *,rank,": before h5dwrite" 
				!      temporary1=temporary_arr(:,:,:,i)
				!_arr(:,:,:,i)
				
				temporary_vec=0.
				if(i.eq.1) temporary_vec(1:ions/stride)=p(1:ions:stride)%x
				if(i.eq.2) temporary_vec(1:ions/stride)=p(1:ions:stride)%y &
				+modulo(rank,sizey)*(myall-5)
				if(i.eq.3) temporary_vec(1:ions/stride)=p(1:ions:stride)%z+(rank &
				/sizey)*(mzall-5)
				
				if(i.eq.4) temporary_vec(1:ions/stride)=p(1:ions:stride)%u
				if(i.eq.5) temporary_vec(1:ions/stride)=p(1:ions:stride)%v
				if(i.eq.6) temporary_vec(1:ions/stride)=p(1:ions:stride)%w
				!         if(i.eq.7) temporary_vec(1:ions/stride)=sqrt(1./(1.
				!     &             -(p(1:ions:stride)%u**2+p(1:ions:stride)%v**2
				!     &             +p(1:ions:stride)%w **2)/c**2))
				
				if(i.eq.7) temporary_vec(1:ions/stride)=p(1:ions:stride)%ch
				if(i.eq.8) temporary_vec(1:ions/stride)=sqrt(1. &
				+(p(1:ions:stride)%u**2+p(1:ions:stride)%v**2 &
				+p(1:ions:stride)%w**2))
				
				if(i.eq.9) temporary_vec(1:ions/stride)=real(p(1:ions:stride) &
				%ind,4)
				
				!         if(i.eq.9) temporary_vec(1:ions/stride)=real(p(1:ions:stride)
				!     &             %proc,4)
				
				if(i.eq.10) temporary_vec(1:ions/stride)=real(p(1:ions:stride) &
				%splitlev,4)
				
				
				
				if(i.eq.11) temporary_vec(1:lecs/stride)=p(maxhlf+1:maxhlf &
				+lecs:stride)%x
				if(i.eq.12) temporary_vec(1:lecs/stride)=p(maxhlf+1:maxhlf &
				+lecs:stride)%y+modulo(rank,sizey)*(myall-5)
				if(i.eq.13) temporary_vec(1:lecs/stride)=p(maxhlf+1:maxhlf &
				+lecs:stride)%z+(rank/sizey)*(mzall-5)
				if(i.eq.14) temporary_vec(1:lecs/stride)=p(maxhlf+1:maxhlf &
				+lecs:stride)%u
				if(i.eq.15) temporary_vec(1:lecs/stride)=p(maxhlf+1:maxhlf &
				+lecs:stride)%v
				if(i.eq.16) temporary_vec(1:lecs/stride)=p(maxhlf+1:maxhlf &
				+lecs:stride)%w
				!         if(i.eq.14) temporary_vec(1:lecs/stride)=sqrt(1./(1. -(p(maxhlf
				!     &             +1:maxhlf+lecs:stride)%u**2+p(maxhlf+1:maxhlf +lecs:stride
				!     &             )%v**2+p(maxhlf +1:maxhlf+lecs:stride)%w**2)/c**2))
				
				if(i.eq.17) temporary_vec(1:lecs/stride)=p(maxhlf+1:maxhlf+lecs:stride)%ch
				if(i.eq.18) temporary_vec(1:lecs/stride)=sqrt(1. +(p(maxhlf &
				+1:maxhlf+lecs:stride)%u**2+p(maxhlf+1:maxhlf +lecs:stride &
				)%v**2+p(maxhlf +1:maxhlf+lecs:stride)%w**2))
				
				if(i.eq.19)temporary_vec(1:lecs/stride)=real(p(maxhlf+1:maxhlf &
				+lecs:stride)%ind,4)
				
				!         if(i.eq.18)temporary_vec(1:lecs/stride)=real(p(maxhlf+1:maxhlf
				!     &             +lecs:stride)%proc,4)
				
				if(i.eq.20)temporary_vec(1:lecs/stride)=real(p(maxhlf+1:maxhlf &
				+lecs:stride)%splitlev,4)
				
				if(debug)print *,"flds.tot.",ind,rank,"done 5p"
				
#ifdef MPI
					if(i .le. midvars) then 
						call h5dwrite_real_1(dset_id(i), H5T_NATIVE_REAL, &
						temporary_vec(1:max(ions/stride,1)), dimsfiions,error &
						,file_space_id = filespace(i), mem_space_id=memspace &
						,xfer_prp = h5p_default_f)
					else 
						call h5dwrite_real_1(dset_id(i), H5T_NATIVE_REAL, &
						temporary_vec(1:max(lecs/stride,1)), dimsfilecs,error &
						,file_space_id = filespace(i), mem_space_id=memspace &
						,xfer_prp = h5p_default_f)
					endif
					
#else
					if(i .le. midvars) then 
						call h5dwrite_f(dset_id(i), H5T_NATIVE_REAL, &
						temporary_vec(1:max(ions/stride,1)), dimsfiions,error)
					else 
						call h5dwrite_f(dset_id(i), H5T_NATIVE_REAL, &
						temporary_vec(1:max(lecs/stride,1)), dimsfilecs,error)
					endif
#endif
	
#ifdef MPI
					call h5sclose_f(memspace,error)
#endif
			enddo
			
			if(debug)print *,"flds.tot.",ind,rank,"done 6p"
			
			!
			! Close dataspaces.
			!
#ifdef MPI
				do i=1,nvars
					call h5sclose_f(filespace(i), error)
				enddo
#endif
	
			!
			! Close the dataset and property list.
			!
			do i=1,nvars
				call h5dclose_f(dset_id(i), error)
			enddo
	
#ifdef MPI
				call h5pclose_f(plist_id, error)
#endif
			!
			! Close the file.
			!
			call h5fclose_f(file_id, error)
			
			!
			! Close FORTRAN predefined datatypes.
			!
			call h5close_f(error)
			
			if(debug) print *, rank,": finished writing particles"
			
			!now write particles
			!      if(rank.eq.0)then
			
			is=1
			
			goto 1000
			
			write(12)real((ions/is),4),real((lecs/is),4), &
			real(maxptl,4),real(maxhlf,4), &
			(real(p(n)%x,4),n=1,(ions/is)*is,is), &
			(real(p(n)%x,4),n=maxhlf+1,maxhlf+(lecs/is)*is,is), &
			(real(p(n)%y,4),n=1,(ions/is)*is,is), &
			(real(p(n)%y,4),n=maxhlf+1,maxhlf+(lecs/is)*is,is), &
			(real(p(n)%z,4),n=1,(ions/is)*is,is), &
			(real(p(n)%z,4),n=maxhlf+1,maxhlf+(lecs/is)*is,is), &
			(real(p(n)%u,4),n=1,(ions/is)*is,is), &
			(real(p(n)%u,4),n=maxhlf+1,maxhlf+(lecs/is)*is,is), &
			(real(p(n)%v,4),n=1,(ions/is)*is,is), &
			(real(p(n)%v,4),n=maxhlf+1,maxhlf+(lecs/is)*is,is), &
			(real(p(n)%w,4),n=1,(ions/is)*is,is), &
			(real(p(n)%w,4),n=maxhlf+1,maxhlf+(lecs/is)*is,is),     &
			(real(p(n)%ch,4),n=1,(ions/is)*is,is), &
			(real(p(n)%ch,4),n=maxhlf+1,maxhlf+(lecs/is)*is,is),     &
			(real(1./sqrt(1.-(p(n)%u**2+p(n)%v**2+p(n)%w**2)/c**2),4),  &
			n=1,(ions/is)*is,is), &
			(real(1./sqrt(1.-(p(n)%u**2+p(n)%v**2+p(n)%w**2)/c**2),4),  &
			n=maxhlf+1,maxhlf+(lecs/is)*is,is)
			
			1000 continue
	
			deallocate(temporary1, temporary_vec)
			
#ifdef MPI
				call MPI_Info_free(FILE_INFO_TEMPLATE,error)
#endif
			
#else
			!defining the template for writing
			!     to be inserted after each instance of h5pcreate_f
			!     call H5Pset_sieve_buf_size_f(plist_id, 262144,error) 
			!     call H5Pset_alignment_f(plist_id, 524288, 262144,error)
			
#ifdef MPI 
#ifndef serIO     
					call MPI_Info_create(FILE_INFO_TEMPLATE,error)
					!     call MPI_Info_set(FILE_INFO_TEMPLATE, "IBM_largeblock_io", 
					!     &          "true", error)
					call MPI_Info_set(FILE_INFO_TEMPLATE, "access_style",  &
					"write_once",error)
					call MPI_Info_set(FILE_INFO_TEMPLATE,  &
					"collective_buffering", "true",error)
					call MPI_Info_set(FILE_INFO_TEMPLATE, "cb_block_size", &
					"4194304",error)
					call MPI_Info_set(FILE_INFO_TEMPLATE, "cb_buffer_size", &
					"16777216",error)
					call MPI_Info_set(FILE_INFO_TEMPLATE, "cb_nodes", &
					"1",error)
#endif
#endif
			
			!computing my1 and mz1 for each proc
			if(debug .and. rank .eq. 0) print *, "in output_par"
			
			kstart=3
			kfinish=mz-3
			if(rank/sizey .eq. 0) kstart=1
			if(rank/sizey .eq. sizez-1) kfinish=mz
			
			110  if(modulo(kstart+(rank/sizey)*(mzall-5)-1*0,istep) .eq. 0) goto 120
			kstart=kstart+1
			goto 110
			120  continue
			
			130  if(modulo(kfinish+(rank/sizey)*(mzall-5)-1*0,istep) .eq. 0) goto 140
			kfinish=kfinish-1
			goto 130
			140  continue
		
			mz1=(kfinish-kstart)/istep+1
#ifdef twoD
				kstart=1
				kfinish=1
				mz1=1
#endif
			
			jstart=3
			jfinish=my-3
			if(modulo(rank,sizey) .eq. 0) jstart=1
			if(modulo(rank,sizey) .eq. sizey-1) jfinish=my
			
			210  if(modulo(jstart+modulo(rank,sizey)*(myall-5)-1*0,istep) .eq. 0) goto 220
			jstart=jstart+1
			goto 210
			220  continue
			
			230  if(modulo(jfinish+modulo(rank,sizey)*(myall-5)-1*0,istep) .eq. 0)  goto 240
			jfinish=jfinish-1
			goto 230
			240  continue
			
			my1=(jfinish-jstart)/istep+1
			
			!      print *, rank,":","mz1=", mz1, kstart, kfinish, istep,mzall,mz
			
			if(debug) print *, rank,"in output b4 allocate"
			!allocate temporary1 on every processor
			allocate(temporary1(mx0/istep,my1,mz1))
			
			!
			!  Initialize FORTRAN predefined datatypes
			!
#ifdef serIO
				if (rank .eq. 0) then
					call h5open_f(error)
				endif 
#else
				call h5open_f(error) 
#endif
			
			! 
			! Setup file access property list with parallel I/O access.
			!
			
#ifdef MPI
#ifndef serIO
					call h5pcreate_f(H5P_FILE_ACCESS_F, plist_id, error)
					!      call H5Pset_sieve_buf_size_f(plist_id, 262144,error) 
					!      call H5Pset_alignment_f(plist_id, 524288, 262144,error)
					call h5pset_fapl_mpio_f(plist_id, mpi_comm_world, &
					FILE_INFO_TEMPLATE,error)
#endif
#endif
	
			if(debug)print *,"flds.tot.",ind,rank,"done 0f"
			
			!define dsetname
			if(debug) print *, rank,"h5pset"
			dsetname(1)="ex"
			dsetname(2)="ey"
			dsetname(3)="ez"
			dsetname(4)="bx"
			dsetname(5)="by"
			dsetname(6)="bz"
			dsetname(7)="ux"
			dsetname(8)="uy"
			dsetname(9)="uz"
			dsetname(10)="Temp"
			dsetname(11)="dens"
			dsetname(12)="densi"
			dsetname(13)="dummy"
			dsetname(14)="jx"
			dsetname(15)="jy"
			dsetname(16)="jz"
			
			
			dsetname(1)="ex"
			dsetname(2)="ey"
			dsetname(3)="ez"
			dsetname(4)="bx"
			dsetname(5)="by"
			dsetname(6)="bz"
			!      dsetname(7)="ux"
			!      dsetname(8)="uy"
			!      dsetname(9)="uz"
			!      dsetname(10)="Temp"
			dsetname(11-4)="dens"
			dsetname(12-4)="densi"
			!      dsetname(13)="dummy"
			dsetname(14-5)="jx"
			dsetname(15-5)="jy"
			dsetname(16-5)="jz"
			
			!
			! Create the file collectively.
			! 
#ifdef MPI
#ifdef serIO
					if (rank .eq. 0) then
						call h5fcreate_f(fnamefld, H5F_ACC_TRUNC_F, file_id, error, &
						h5p_default_f,h5p_default_f)
					endif
#else
					call h5fcreate_f(fnamefld, H5F_ACC_TRUNC_F, file_id, error, &
					access_prp = plist_id)
					call h5pclose_f(plist_id, error)
#endif
#else
				call h5fcreate_f(fnamefld, H5F_ACC_TRUNC_F, file_id, error)
#endif      
			
			if(debug)print *,"flds.tot.",ind,rank,"done 1f"
			
			! Create the data space for the  dataset. 
			!
			
#ifdef serIO
				if (rank .eq. 0) then
					do i=1,nvars
						call h5screate_simple_f(datarank, dimsf, filespace(i), error)
					enddo
				endif
#else
				do i=1,nvars
					call h5screate_simple_f(datarank, dimsf, filespace(i), error)
				enddo
#endif
			
			!create property list for collective writing
			
			if(debug)print *,"flds.tot.",ind,rank,"done 2f"
			
			
			!getting information about the size of temporary1
#ifdef MPI
				if(debug) print *, rank,": in output, b4 allgather", my1
				call mpi_allgather(my1,1,mpi_integer,my1list,1,mpi_integer &
				,mpi_comm_world, error)
				if(debug) print *, rank,": in output, b4 allgather", mz1
				call mpi_allgather(mz1,1,mpi_integer,mz1list,1,mpi_integer &
				,mpi_comm_world, error) 
#ifdef serIO       
					if (rank .eq. 0) then
						allocate(temporary0(mx0/istep,maxval(my1list), &
						maxval(mz1list)))
					endif
#endif
#endif
			
			if(debug)print *,"flds.tot.",ind,rank,"done 3f"
			
			!writing currents (all procs)
			open(unit=7,file=fenlargeloc, form='unformatted')
			rewind 7
			write(7) (((curx(i,j,k),i=1,mx),j=1,my),k=1,mz), &
			(((cury(i,j,k),i=1,mx),j=1,my),k=1,mz)
			close(7)
			
			if(debug)print *,"flds.tot.",ind,rank,"done 4f"
			
			
			!looping nvars
			do iv=1,nvars
				
				! special things for special choices
				if(debug)print *,rank,"iv=",iv
				
				if(iv .eq. 11-4) call meanq_dens_cur()
				if(iv .eq. 14-5) then 
					!     restore currents from file
					open(unit=7,file=fenlargeloc, form='unformatted')
					rewind 7
					if(debug.and.rank.eq.0) print *, rank,":reading back current file"
					read(7)(((curx(i,j,k),i=1,mx),j=1,my),k=1,mz), &
					(((cury(i,j,k),i=1,mx),j=1,my),k=1,mz)
					!     close(7,status='delete')
					if(debug .and. rank.eq.0)print *, rank,"resetting arrays"           
				endif
				
				
				!     define temporary1 for every proc
				do k=kstart, kfinish, istep !1,mz/istep
					nz=(k-kstart)/istep+1
					do j=jstart, jfinish,istep !1,my/istep
						ny=(j-jstart)/istep+1
						do i=1,mx/istep
							
							call interpolfld(real(i*istep),real(j), &
							real(k),bx0,by0,bz0,ex0,ey0,ez0,bxd, &
							byd,bzd,exd,eyd,ezd)
							
							if(iv.eq.1)temporary1(i,ny,nz)=real(ex0+exd,4)
							if(iv.eq.2)temporary1(i,ny,nz)=real(ey0+eyd,4)
							if(iv.eq.3)temporary1(i,ny,nz)=real(ez0+ezd,4)
							if(iv.eq.4)temporary1(i,ny,nz)=real(bx0+bxd,4)
							if(iv.eq.5)temporary1(i,ny,nz)=real(by0+byd,4)
							if(iv.eq.6)temporary1(i,ny,nz)=real(bz0+bzd,4)
							if(iv.eq.11-4)temporary1(i,ny,nz)= &
							real(curx(i*istep,j,k),4) !nav
							if(iv.eq.12-4)temporary1(i,ny,nz)= &
							real(cury(i*istep,j,k),4) !navi
							
							if(iv .ge. 14-5) then
								call interpolcurrent(real(i*istep),real(j), &
								real(k),curx0, cury0, curz0)
								
								curx0=curx(i*istep,j,k)
								cury0=cury(i*istep,j,k)
								curz0=curz(i*istep,j,k)
								
							endif
							
							!     if(iv.eq.13-4)temporary1(i,ny,nz)=0.
							if(iv.eq.14-5)temporary1(i,ny,nz)=real(curx0,4)
							if(iv.eq.15-5)temporary1(i,ny,nz)=real(cury0,4)
							if(iv.eq.16-5)temporary1(i,ny,nz)=real(curz0,4)                         
						enddo
					enddo
				enddo         
				
				
				!serial writing
#ifdef serIO
				if(rank.eq.0)print *, "Serial writing of dataset: ",dsetname(iv)
					do procn=0,size0-1
						
						if (rank .eq. 0) then
							
							if (procn .ne. 0) then !receive from procn                                
#ifdef MPI
									call MPI_Recv(temporary0,mx0/istep*my1list(procn+1) &
									*mz1list(procn+1),mpi_real,procn,1, &
									MPI_COMM_WORLD,status,error)
#endif
							else           !procn eq 0
								temporary0=temporary1               
							endif
							
														
							!     computing the offset
							count(1) = dimsf(1)
							count(2) = my1list(procn+1)   !dimsf(2)
							count(3) = mz1list(procn+1)
							
							offset(1) = 0
							offset(2) = 0
							offset(3) = 0
							!     z offset
							if(procn/sizey .gt. 0) then 
								offset(3)=sum(mz1list(1:(procn/sizey)*sizey:sizey))
								!     print *,rank,":","offset=", offset(3), "count=",count
							endif
#ifdef twoD 
								offset(3)=0
#endif
							!     now get the y offset
							if(modulo(procn,sizey) .gt. 0) then 
								offset(2)=sum(my1list((procn/sizey)*sizey+1:procn))
							endif
							!               if(debug)print *,rank,":","offset=",offset(2),"count=",count
							
							
							!hyperslab selection
#ifdef MPI
								! this is necessay only if I close the filespace above      
								!               call h5dget_space_f(dset_id(iv), filespace(iv), error)               
								call h5sselect_hyperslab_f(filespace(iv),H5S_SELECT_SET_F &
								,offset,count,error)
#endif               
					
							if(debug)print *,rank,": selected hyperslab"
							
							
							!memory space
#ifdef MPI
								call h5screate_simple_f(datarank, count, memspace, error) 
#endif
							
							
							!dataset creation/opening
							if (procn .eq. 0) then
								call h5dcreate_f(file_id,dsetname(iv),H5T_NATIVE_REAL, &
								filespace(iv),dset_id(iv), error)
							else 
								call h5dopen_f(file_id,dsetname(iv),dset_id(iv),error)
							endif
							
							
							!writing
							if(debug)print *,rank,": before h5dwrite","filespace", &
							filespace(iv),"memspace",memspace
							
#ifdef MPI
								call h5dwrite_f(dset_id(iv), H5T_NATIVE_REAL,  &
								temporary0(:,:,:),dimsfi,error,mem_space_id= &
								memspace,file_space_id = filespace(iv))
								!               call h5dwrite_real_1(dset_id(iv), H5T_NATIVE_REAL, 
								!     &                   temporary0(:,:,:),dimsfi,error,file_space_id = 
								!     &                   filespace(iv), mem_space_id=memspace)
#else
								call h5dwrite_f(dset_id(iv),H5T_NATIVE_REAL, &
								temporary(:,:,:) ,dimsfi,error)
#endif 
							
							
							!closing dataset and memory space
							if(debug)print *, rank, ": ", iv, "closing d"
							call h5dclose_f(dset_id(iv), error)
							if(debug)print *, rank, ": ", iv, "closing m"
							call h5sclose_f(memspace, error)
							
						else !rank ne 0
					
							if (procn .eq. rank) then
#ifdef MPI
									call MPI_Send(temporary1,mx0/istep*my1 &
									*mz1,mpi_real,0,1, &
									MPI_COMM_WORLD,error) 
#endif             
							endif
						endif !rank ne 0
						
					enddo!procn
					
#endif        
				
				!closing dataspace    
				if(debug)print *, rank, ": ", iv, "closing s" 
#ifdef serIO    
					if (rank .eq. 0) then
						call h5sclose_f(filespace(iv), error)
					endif
#else
					call h5sclose_f(filespace(iv), error)
#endif
	
			enddo!nvars
			
			!closing property list for collective writing
			!#ifdef MPI
			!      if(debug)print *, rank, ": ", "closing p"
			!      call h5pclose_f(plist_id, error)
			!#endif
			
			
#ifdef serIO
				if (rank .eq. 0) then
					if(debug)print *, rank, ": ", "closing f"
					call h5fclose_f(file_id, error)
					if(debug)print *, rank, ": ", "closing h5"
					call h5close_f(error)
					deallocate(temporary0)
				endif
#else
				if(debug)print *, rank, ": ", "closing f"
				call h5fclose_f(file_id, error)
				if(debug)print *, rank, ": ", "closing h5"
				call h5close_f(error)
#endif
			
			if(debug) print *, rank,": finished writing fields"
			
			if(debug)print *,"flds.tot.",ind,rank,"done 5f"
			
			deallocate(temporary1)
			
			!      goto 127
			
			!-------------------------------------
			!now write particles
			
			!  Initialize FORTRAN predefined datatypes
			!
#ifdef serIO
				if (rank .eq. 0) then
					call h5open_f(error)
				endif 
#else
				call h5open_f(error) 
#endif
			
			! 
			! Setup file access property list with parallel I/O access.
			!
			
#ifdef MPI
#ifndef serIO
					call h5pcreate_f(H5P_FILE_ACCESS_F, plist_id, error)
					!      call H5Pset_sieve_buf_size_f(plist_id, 262144,error) 
					!      call H5Pset_alignment_f(plist_id, 524288, 262144,error)
					call h5pset_fapl_mpio_f(plist_id, mpi_comm_world, &
					FILE_INFO_TEMPLATE,error)
#endif
#endif
			if(debug)print *,"prtl.tot.",ind,rank,"done 0p"
			
			!define dsetname
			nvars=14+2+2+2
			midvars=7+1+1+1
			!      stride=100 !20 !1 !100 !60 !15 !*4
			dsetname(1)="xi"
			dsetname(2)="yi"
			dsetname(3)="zi"
			dsetname(4)="ui"
			dsetname(5)="vi"
			dsetname(6)="wi"
			dsetname(7)="chi"
			dsetname(8)="gammai"
			dsetname(9)="indi"
			dsetname(10)="proci"
			dsetname(11)="xe"
			dsetname(12)="ye"
			dsetname(13)="ze"
			dsetname(14)="ue"
			dsetname(15)="ve"
			dsetname(16)="we"
			dsetname(17)="che"
			dsetname(18)="gammae"
			dsetname(19)="inde"
			dsetname(20)="proce"
			!      dsetname(15)="spectrum"
			!need to add writing spectrum from root
			
			!find out number of ions and lecs
			datarank=1
	
#ifdef MPI
				tmp1=max(ions,1)
				call mpi_allgather(tmp1,1,mpi_integer,all_ions,1 &
				,mpi_integer,mpi_comm_world, error)
				tmp1=max(lecs,1)
				call mpi_allgather(tmp1,1,mpi_integer,all_lecs,1, &
				mpi_integer,mpi_comm_world, error)
				
				if(rank.eq.0)print *,"all_ions",all_ions, tmp1 
				all_ions_lng=all_ions
				all_lecs_lng=all_lecs
#else
				all_ions_lng=ions
				all_lecs_lng=lecs
#endif
			
			where(all_ions .lt. stride)all_ions=stride
			where(all_lecs .lt. stride)all_lecs=stride
			where(all_ions_lng .lt. stride)all_ions_lng=stride
			where(all_lecs_lng .lt. stride)all_lecs_lng=stride
			dimsfions(1)=sum(all_ions_lng/stride)
			dimsflecs(1)=sum(all_lecs_lng/stride)
			dimsfiions(1)=dimsfions(1)
			dimsfilecs(1)=dimsflecs(1)
			dimsfiions(2:7)=0
			dimsfilecs(2:7)=0
			if(rank .eq. 0) then 
				print *, "all_ions", all_ions_lng
				print *, "all_lecs", all_lecs_lng
				print *,"dimsions", sum(all_ions_lng/stride)
				print *,"dimslecs", sum(all_lecs_lng/stride)
			endif
			
			!      allocate(temporary_vec(max(ions/stride,lecs/stride)))
			allocate(temporary_vec(max(max(ions/stride,lecs/stride),1)))     
			
#ifdef serIO
				if (rank .eq. 0) then
					allocate(temporary0_vec(max(maxval(all_ions_lng/stride), &
					maxval(all_lecs_lng/stride))))
				endif
#endif
			
			! Create the file collectively.
			! 
#ifdef MPI
#ifdef serIO
					if (rank .eq. 0) then
						call h5fcreate_f(fnameprt, H5F_ACC_TRUNC_F, file_id, error, &
						h5p_default_f,h5p_default_f)
					endif
#else
					call h5fcreate_f(fnameprt, H5F_ACC_TRUNC_F, file_id, error, &
					access_prp = plist_id)
					call h5pclose_f(plist_id, error)
#endif
#else
				call h5fcreate_f(fnameprt, H5F_ACC_TRUNC_F, file_id, error)
#endif      
			
			if(debug)print *,"prtl.tot.",ind,rank,"done 1p"
			
			
			!
			! Create the data space for the  dataset. 
			!
			
#ifdef serIO
				if (rank .eq. 0) then
					do i=1,midvars
						call h5screate_simple_f(datarank, dimsfions(1), filespace(i), &
						error)
					enddo
					do i=midvars+1,nvars
						call h5screate_simple_f(datarank, dimsflecs(1), filespace(i), &
						error)
					enddo
				endif
#else
				do i=1,midvars
					call h5screate_simple_f(datarank, dimsfions(1), filespace(i), &
					error)
				enddo
				do i=midvars+1,nvars
					call h5screate_simple_f(datarank, dimsflecs(1), filespace(i), &
					error)
				enddo
#endif
			
			if(debug)print *,"prtl.tot.",ind,rank,"done 2p"
			
			!
			! Create property list for collective dataset write
			!
			!#ifdef MPI
			!      call h5pcreate_f(H5P_DATASET_XFER_F, plist_id, error) 
			!      call h5pset_dxpl_mpio_f(plist_id, H5FD_MPIO_COLLECTIVE_F, error)
			!#endif
			! 
			
			if(debug)print *,"prtl.tot.",ind,rank,"done 3p"
			
			
			!looping nvars
			do i=1,nvars
				
				!     define temporary_vec for every proc
			   temporary_vec=0.
				if(i.eq.1) temporary_vec(1:ions/stride)=p(1:ions:stride)%x
				if(i.eq.2) temporary_vec(1:ions/stride)=p(1:ions:stride)%y &
				+modulo(rank,sizey)*(myall-5)
				if(i.eq.3)temporary_vec(1:ions/stride)=p(1:ions:stride)%z+(rank &
				/sizey)*(mzall-5)
				
				if(i.eq.4) temporary_vec(1:ions/stride)=p(1:ions:stride)%u
				if(i.eq.5) temporary_vec(1:ions/stride)=p(1:ions:stride)%v
				if(i.eq.6) temporary_vec(1:ions/stride)=p(1:ions:stride)%w
				!     if(i.eq.7) temporary_vec(1:ions/stride)=sqrt(1./(1.
				!     &             -(p(1:ions:stride)%u**2+p(1:ions:stride)%v**2
				!     &             +p(1:ions:stride)%w **2)/c**2))
				
				if(i.eq.7) temporary_vec(1:ions/stride)=p(1:ions:stride)%ch
				if(i.eq.8) temporary_vec(1:ions/stride)=sqrt(1. &
				+(p(1:ions:stride)%u**2+p(1:ions:stride)%v**2 &
				+p(1:ions:stride)%w**2))
				
				if(i.eq.9) temporary_vec(1:ions/stride)=real(p(1:ions:stride) &
				%ind,4)
				
				!     if(i.eq.9) temporary_vec(1:ions/stride)=real(p(1:ions:stride)
				!     &             %proc,4)
				
				if(i.eq.10) temporary_vec(1:ions/stride)=real(p(1:ions:stride) &
				%splitlev,4)
				
				if(i.eq.11) temporary_vec(1:lecs/stride)=p(maxhlf+1:maxhlf &
				+lecs:stride)%x
				if(i.eq.12) temporary_vec(1:lecs/stride)=p(maxhlf+1:maxhlf &
				+lecs:stride)%y+modulo(rank,sizey)*(myall-5)
				if(i.eq.13) temporary_vec(1:lecs/stride)=p(maxhlf+1:maxhlf &
				+lecs:stride)%z+(rank/sizey)*(mzall-5)
				if(i.eq.14) temporary_vec(1:lecs/stride)=p(maxhlf+1:maxhlf &
				+lecs:stride)%u
				if(i.eq.15) temporary_vec(1:lecs/stride)=p(maxhlf+1:maxhlf &
				+lecs:stride)%v
				if(i.eq.16) temporary_vec(1:lecs/stride)=p(maxhlf+1:maxhlf &
				+lecs:stride)%w
				!     if(i.eq.14) temporary_vec(1:lecs/stride)=sqrt(1./(1. -(p(maxhlf
				!     &             +1:maxhlf+lecs:stride)%u**2+p(maxhlf+1:maxhlf +lecs:stride
				!     &             )%v**2+p(maxhlf +1:maxhlf+lecs:stride)%w**2)/c**2))
				
				if(i.eq.17) temporary_vec(1:lecs/stride)=p(maxhlf+1:maxhlf+lecs:stride)%ch
				if(i.eq.18) temporary_vec(1:lecs/stride)=sqrt(1. +(p(maxhlf &
				+1:maxhlf+lecs:stride)%u**2+p(maxhlf+1:maxhlf +lecs:stride &
				)%v**2+p(maxhlf +1:maxhlf+lecs:stride)%w**2))
				
				if(i.eq.19)temporary_vec(1:lecs/stride)=real(p(maxhlf+1:maxhlf &
				+lecs:stride)%ind,4)
				
				!     if(i.eq.18)temporary_vec(1:lecs/stride)=real(p(maxhlf+1:maxhlf
				!     &             +lecs:stride)%proc,4)
				
				if(i.eq.20)temporary_vec(1:lecs/stride)=real(p(maxhlf+1:maxhlf &
				+lecs:stride)%splitlev,4)
				

				!serial writing
#ifdef serIO
				if(rank.eq.0)print *, "Serial writing of dataset: ",dsetname(i)
							

					do procn=0,size0-1
						
						if (rank .eq. 0) then
							
							if (procn .ne. 0) then !receive from procn                                
#ifdef MPI
									if(i .le. midvars) then
										call MPI_Recv(temporary0_vec,all_ions_lng(procn+1)/ &
										stride,mpi_real,procn,1, &
										MPI_COMM_WORLD,status,error)
									else
										call MPI_Recv(temporary0_vec,all_lecs_lng(procn+1)/ &
										stride,mpi_real,procn,1, &
										MPI_COMM_WORLD,status,error)
									endif
#endif
							else           !procn eq 0
								temporary0_vec=temporary_vec               
							endif
							
							
							!define count and offset
							if(i.le.midvars)countpart = max(all_ions_lng(procn+1)/ &
							stride,1)
							if(i.gt.midvars)countpart = max(all_lecs_lng(procn+1)/ &
							stride,1)
							
							offsetpart = 0
							if(procn .gt. 0) then 
								if(i .le. midvars) then
									offsetpart = sum(all_ions_lng(1:procn)/stride)
								else
									offsetpart = sum(all_lecs_lng(1:procn)/stride)
								endif
							endif
							
							
							!hyperslab selection
#ifdef MPI
								! this is necessary only if I close the filespace above      
								!               call h5dget_space_f(dset_id(i), filespace(i), error)               
								call h5sselect_hyperslab_f(filespace(i),H5S_SELECT_SET_F &
								,offsetpart,countpart,error)
#endif               
					
							if(debug)print *,rank,": selected hyperslab"
							
							
							!memory space
#ifdef MPI
								call h5screate_simple_f(1, countpart, memspace, error) 
#endif
							
							
							!dataset creation/opening
							if (procn .eq. 0) then
								call h5dcreate_f(file_id,dsetname(i),H5T_NATIVE_REAL, &
								filespace(i),dset_id(i), error)
							else 
								call h5dopen_f(file_id,dsetname(i),dset_id(i),error)
							endif
							
							
							!writing
#ifdef MPI
								if(i .le. midvars) then 
									call h5dwrite_real_1(dset_id(i), H5T_NATIVE_REAL, &
									temporary0_vec(1:all_ions_lng(procn+1)/stride),  &
									dimsfiions,error,file_space_id = filespace(i),  &
									mem_space_id=memspace)
								else 
									call h5dwrite_real_1(dset_id(i), H5T_NATIVE_REAL, &
									temporary0_vec(1:all_lecs_lng(procn+1)/stride),  &
									dimsfilecs,error,file_space_id = filespace(i),  &
									mem_space_id=memspace)
								endif
								
#else
								if(i .le. midvars) then 
									call h5dwrite_f(dset_id(i), H5T_NATIVE_REAL, &
									temporary_vec(1:max(ions/stride,1)),  &
									dimsfiions,error)
								else 
									call h5dwrite_f(dset_id(i), H5T_NATIVE_REAL, &
									temporary_vec(1:max(lecs/stride,1)),  &
									dimsfilecs,error)
								endif
#endif
							
							
							!closing dataset and memory space
							if(debug)print *, rank, ": ", i, "closing d"
							call h5dclose_f(dset_id(i), error)
							if(debug)print *, rank, ": ", i, "closing m"
							call h5sclose_f(memspace, error)
						
						else !rank ne 0
							if (procn .eq. rank) then
#ifdef MPI
									if(i .le. midvars) then
										call MPI_Send(temporary_vec,all_ions_lng(procn+1)/ &
										stride,mpi_real,0,1, &
										MPI_COMM_WORLD,error)
									else
										call MPI_Send(temporary_vec,all_lecs_lng(procn+1)/ &
										stride,mpi_real,0,1, &
										MPI_COMM_WORLD,error)
									endif
#endif       
							endif
						endif               !rank ne 0
						
					enddo!procn
					
#endif
				
				if(debug)print *, rank, ": ", i, "closing s" 
#ifdef serIO    
					if (rank .eq. 0) then
						call h5sclose_f(filespace(i), error)
					endif
#else
					call h5sclose_f(filespace(i), error)
#endif
				
			enddo!nvars
			
#ifdef serIO
				if (rank .eq. 0) then
					if(debug)print *, rank, ": ", "closing f"
					call h5fclose_f(file_id, error)
					if(debug)print *, rank, ": ", "closing h5"
					call h5close_f(error)
					deallocate(temporary0_vec)
				endif
#else
				if(debug)print *, rank, ": ", "closing f"
				call h5fclose_f(file_id, error)
				if(debug)print *, rank, ": ", "closing h5"
				call h5close_f(error)
#endif
			
			if(debug) print *, rank,": finished writing particles"
			
			if(debug)print *,"prtl.tot.",ind,rank,"done 4p"
			
			
			
			!#ifdef MPI
			!      call h5pclose_f(plist_id, error)
			!#endif
			
			
			is=1
			
			goto 1000
			
			write(12)real((ions/is),4),real((lecs/is),4), &
			real(maxptl,4),real(maxhlf,4), &
			(real(p(n)%x,4),n=1,(ions/is)*is,is), &
			(real(p(n)%x,4),n=maxhlf+1,maxhlf+(lecs/is)*is,is), &
			(real(p(n)%y,4),n=1,(ions/is)*is,is), &
			(real(p(n)%y,4),n=maxhlf+1,maxhlf+(lecs/is)*is,is), &
			(real(p(n)%z,4),n=1,(ions/is)*is,is), &
			(real(p(n)%z,4),n=maxhlf+1,maxhlf+(lecs/is)*is,is), &
			(real(p(n)%u,4),n=1,(ions/is)*is,is), &
			(real(p(n)%u,4),n=maxhlf+1,maxhlf+(lecs/is)*is,is), &
			(real(p(n)%v,4),n=1,(ions/is)*is,is), &
			(real(p(n)%v,4),n=maxhlf+1,maxhlf+(lecs/is)*is,is), &
			(real(p(n)%w,4),n=1,(ions/is)*is,is), &
			(real(p(n)%w,4),n=maxhlf+1,maxhlf+(lecs/is)*is,is),     &
			(real(p(n)%ch,4),n=1,(ions/is)*is,is), &
			(real(p(n)%ch,4),n=maxhlf+1,maxhlf+(lecs/is)*is,is),     &
			(real(1./sqrt(1.-(p(n)%u**2+p(n)%v**2+p(n)%w**2)/c**2),4),  &
			n=1,(ions/is)*is,is), &
			(real(1./sqrt(1.-(p(n)%u**2+p(n)%v**2+p(n)%w**2)/c**2),4),  &
			n=maxhlf+1,maxhlf+(lecs/is)*is,is)
			
			1000 continue
			
			deallocate(temporary_vec)
			
			127  continue
			
#ifdef MPI
#ifndef serIO
					call MPI_Info_free(FILE_INFO_TEMPLATE,error)
#endif
#endif
	
#endif
	
		!       call save_spectrum()
		
		
		!don't need families -- can use h5repart
		!!!#include "output_par_lessmem.family.f"
		!saving the huge dump
		
		!      include "output_par.f"
		
		
		!save parameter file
		!          goto 608
		fparam="output/param."//trim(indchar)
		
		if(rank .eq. 0) then 
			open(unit=12,file=fparam,form='unformatted')
			close(12,status='delete')
			call h5open_f (error)
			call h5fcreate_f(fparam, H5F_ACC_TRUNC_F, file_id, error)
			datarank=1
			dimsf(1)=1
			
			!first pack them into an array
			intdata(1)=int(mx0)
			intdata(2)=my0
			intdata(3)=mz0
			intdata(4)=caseinit
			intdata(5)=interval
			intdata(6)=torqint
			intdata(7)=pltstart
			intdata(8)=istep
			intdata(9)=istep1
			intdata(10)=stride
			intdata(11)=ntimes
			intdata(12)=0
			if(cooling) intdata(12)=1
			intdata(13)=splitratio
			
			realdata(1)=c
			realdata(2)=sigma
			realdata(3)=c_omp
			realdata(4)=gamma0
			realdata(5)=delgam
			realdata(6)=ppc0
			realdata(7)=me
			realdata(8)=mi
			realdata(9)=dummyvar
			realdata(10)=acool
			realdata(11)=lap*(c/c_omp)
			realdata(12)=qi
			
			dsetnamei(1)="mx0"
			dsetnamei(2)="my0"
			dsetnamei(3)="mz0"
			dsetnamei(4)="caseinit"
			dsetnamei(5)="interval"
			dsetnamei(6)="torqint"
			dsetnamei(7)="pltstart"
			dsetnamei(8)="istep"
			dsetnamei(9)="istep1"
			dsetnamei(10)="stride"
			dsetnamei(11)="ntimes"
			dsetnamei(12)="cooling"
			dsetnamei(13)="splitratio"
			
			dsetnamer(1)="c"
			dsetnamer(2)="sigma"
			dsetnamer(3)="c_omp"
			dsetnamer(4)="gamma0"
			dsetnamer(5)="delgam"
			dsetnamer(6)="ppc0"
			dsetnamer(7)="me"
			dsetnamer(8)="mi"
			dsetnamer(9)="dummy"
			dsetnamer(10)="acool"
			dsetnamer(11)="time"
			dsetnamer(12)="qi"

			do i=1,13 
				if(debug)print *, rank,": dataseti ",i
				call h5screate_simple_f(datarank, dimsf, filespace(1), &
				error)
				call h5dcreate_f(file_id, dsetnamei(i), H5T_NATIVE_INTEGER, &
				filespace(1),dset_id(1), error)
				call h5dwrite_integer_1(dset_id(1),H5T_NATIVE_INTEGER &
				,intdata(i),dimsf,error)
				call h5dclose_f(dset_id(1), error)
				call h5sclose_f(filespace(1), error)
			enddo
			
			do i=1,12
				if(debug)print *,rank, ": datasetr ",i
				call h5screate_simple_f(datarank, dimsf, filespace(1), &
				error)
				call h5dcreate_f(file_id, dsetnamer(i), H5T_NATIVE_REAL, &
				filespace(1),dset_id(1), error)
				call h5dwrite_real_1(dset_id(1),H5T_NATIVE_REAL,realdata(i) &
				,dimsf,error)
				call h5dclose_f(dset_id(1), error)
				call h5sclose_f(filespace(1), error)
			enddo
			
			call h5fclose_f(file_id,error)
			call h5close_f(error)
			
		endif
		608      continue
		
		
		
		
	endif !if lap ge pltstart
	
	
	!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

	istep0=istep
	
	if(lap.ge.pltstart.and.modulo((lap-pltstart),torqint).eq.0) then

	istep=istep1
	
	dimsf(1)=mx0/istep
	dimsf(2)=my0/istep
#ifndef twoD
		dimsf(3)=mz0/istep
#else
		dimsf(3)=1
#endif 
	dimsfi(1:3)=dimsf(1:3)
	dimsfi(4:7)=0
	datarank=3
	
	nvars=16
	
	
	ind=(lap-pltstart)/torqint
	
	!          write(fnamefld,"(a16,i3.3)") "output/fl%d.tot.",ind
	
	write(indchar,"(i3.3)")ind
	
	print *, rank," writing dimsf", dimsf
	write(fnamefld,"(a16,i3.3)") "output/flds.hug.",ind
	
	!       fnamefld=
	!    &     "/scratch/pvfs2/anatoly/output.mime1.2d.2048.ppc64.sig0/"//
	!    &     "flds.hug."//trim(indchar)
	
	!       write(fnameprt,"(a16,i3.3)") "output/prtl.hug.",ind
	
	if(rank.eq.0)write(*,*) rank,": Writing Huge ",fnamefld, '  ',fnameprt
	
	if(rank .eq. 0) then 
		open(unit=11,file=fnamefld,form='unformatted')
		close(11,status='delete')
		!             open(unit=11,file=fnamefld,form='unformatted')
		!       open(unit=12,file=fnameprt,form='unformatted')
		!       close(12,status='delete')
	endif
	
	call MPI_BARRIER(MPI_COMM_WORLD,ierr)
	
	!!!
	!!! THIS CODE WAS ON OUTPUT_PAR_LESSMEM.HUG
	!!!
	
	
	
	
	
	
	if(debug .and. rank .eq. 0) print *, "in output_par"
	
	kstart=3
	kfinish=mz-3
	if(rank/sizey .eq. 0) kstart=1
	if(rank/sizey .eq. sizez-1) kfinish=mz
	
	
	1110  if(modulo(kstart+(rank/sizey)*(mzall-5)-1*0,istep) .eq. 0) goto &
	1120
	kstart=kstart+1
	goto 1110
	1120  continue
	
	1130  if(modulo(kfinish+(rank/sizey)*(mzall-5)-1*0,istep) .eq. 0) goto &
	1140
	kfinish=kfinish-1
	goto 1130
	1140  continue
	mz1=(kfinish-kstart)/istep+1
#ifdef twoD
		kstart=1
		kfinish=1
		mz1=1
#endif
	
	jstart=3
	jfinish=my-3
	if(modulo(rank,sizey) .eq. 0) jstart=1
	if(modulo(rank,sizey) .eq. sizey-1) jfinish=my
	
	
	1210  if(modulo(jstart+modulo(rank,sizey)*(myall-5)-1*0,istep) .eq. 0) &
	goto 1220
	jstart=jstart+1
	goto 1210
	1220  continue
	
	1230  if(modulo(jfinish+modulo(rank,sizey)*(myall-5)-1*0,istep) .eq. 0)  &
	goto 1240
	jfinish=jfinish-1
	goto 1230
	1240  continue
	
	my1=(jfinish-jstart)/istep+1
	
	!      print *, rank,":","mz1=", mz1, kstart, kfinish, istep,mzall,mz
	
	if(Rank.eq.0)print *, rank, ":", "size of temp", mx0/istep, my1, mz1
	
	allocate( temporary1(mx0/istep,my1,mz1))
	
	!
	!  Initialize FORTRAN predefined datatypes
	!
	call h5open_f(error) 
	
	! 
	! Setup file access property list with parallel I/O access.
	!
#ifdef MPI
		call h5pcreate_f(H5P_FILE_ACCESS_F, plist_id, error)
		call h5pset_fapl_mpio_f(plist_id, mpi_comm_world, mpi_info_null, &
		error)
#endif

	dsetname(1)="ex"
	dsetname(2)="ey"
	dsetname(3)="ez"
	dsetname(4)="bx"
	dsetname(5)="by"
	dsetname(6)="bz"
	dsetname(7)="ux"
	dsetname(8)="uy"
	dsetname(9)="uz"
	dsetname(10)="Temp"
	dsetname(11)="dens"
	dsetname(12)="densi"
	dsetname(13)="dummy"
	dsetname(14)="jx"
	dsetname(15)="jy"
	dsetname(16)="jz"
	
	
	dsetname(1)="ex"
	dsetname(2)="ey"
	dsetname(3)="ez"
	dsetname(4)="bx"
	dsetname(5)="by"
	dsetname(6)="bz"
	!      dsetname(7)="ux"
	!      dsetname(8)="uy"
	!      dsetname(9)="uz"
	!      dsetname(10)="Temp"
	dsetname(11-4)="dens"
	dsetname(12-4)="densi"
	!      dsetname(13)="dummy"
	dsetname(14-5)="jx"
	dsetname(15-5)="jy"
	dsetname(16-5)="jz"
	
	!
	! Create the file collectively.
	! 
#ifdef MPI
		call h5fcreate_f(fnamefld, H5F_ACC_TRUNC_F, file_id, error, &
		access_prp = plist_id)
		call h5pclose_f(plist_id, error)
#else
		call h5fcreate_f(fnamefld, H5F_ACC_TRUNC_F, file_id, error)
#endif
	!
	! Create the data space for the  dataset. 
	
	!determine the fluid variables I want to save
	dsetsav=0
#ifdef twoD
		if (sigma .eq. 0.) then 
			dsetsav(1:4)=[1,2,6,7]
		else
			dsetsav(1:8)=[1,2,3,4,5,6,7,8]
		endif
#else
		dsetsav(1:8)=[1,2,3,4,5,6,7,8]
#endif
	dsetbool=0
	dsetbool(dsetsav)=1
	!
	do i=1,nvars
		if (dsetbool(i) .eq. 1) then 
			call h5screate_simple_f(datarank, dimsf, filespace(i), error)
		endif
	enddo
	!      print *,rank, ": filespace=",filespace
	
	!
	! Create the dataset with default properties.
	!
	do i=1,nvars
		if (dsetbool(i) .eq. 1) then 
			call h5dcreate_f(file_id, dsetname(i), H5T_NATIVE_REAL, &
			filespace(i),dset_id(i), error)
			call h5sclose_f(filespace(i), error)
		endif
	enddo
	
	!
	! Each process defines dataset in memory and writes it to the hyperslab
	! in the file. 
	!
	count(1) = dimsf(1)
	count(2) = my1 !dimsf(2)
	count(3) = mz1
	
	!need to communicate with others to find out the offset
#ifdef MPI
		call mpi_allgather(mz1,1,mpi_integer,all_counts,1,mpi_integer &
		,mpi_comm_world, error)
#endif
	if(debug .and. rank .eq. 0) print *, "allcounts mz1=",all_counts
	offset(1) = 0
	offset(2) = 0
	offset(3) = 0
	!z offset
	if(rank/sizey .gt. 0) then 
			offset(3) = sum(all_counts(1:(rank/sizey)*sizey:sizey))
			!      print *,rank,":","offset=", offset(3), "count=",count
		endif
#ifdef twoD 
			offset(3)=0
#endif
		!now get the y offset
#ifdef MPI
			call mpi_allgather(my1,1,mpi_integer,all_counts,1,mpi_integer &
			,mpi_comm_world, error)
#endif
		if(modulo(rank,sizey) .gt. 0) then 
			
			offset(2)=sum(all_counts((rank/sizey)*sizey+1:rank))
		endif
		if(debug)print *,rank,":","offset=",offset(2),"count=",count
#ifdef MPI
			call h5screate_simple_f(datarank, count, memspace, error) 
#endif
		!
		! Create property list for collective dataset write
		!
#ifdef MPI
			call h5pcreate_f(H5P_DATASET_XFER_F, plist_id, error) 
			call h5pset_dxpl_mpio_f(plist_id, H5FD_MPIO_COLLECTIVE_F, error)
#endif
		! 
		! Select hyperslab in the file.
		!
		
		open(unit=7,file=fenlargeloc, form='unformatted')
		rewind 7
		write(7) (((curx(i,j,k),i=1,mx),j=1,my),k=1,mz), &
		(((cury(i,j,k),i=1,mx),j=1,my),k=1,mz)
		close(7)
	
		do iv=1,nvars
			if(debug)print *,rank,"iv=",iv
			
			if(iv .eq. 11-4) call meanq_dens_cur()
			if(iv .eq. 14-5) then 
				!restore currents from file
				open(unit=7,file=fenlargeloc, form='unformatted')
				rewind 7
				if(debug .and. rank.eq.0) print *, rank,": reading back current file"
				read(7)(((curx(i,j,k),i=1,mx),j=1,my),k=1,mz), &
				(((cury(i,j,k),i=1,mx),j=1,my),k=1,mz)
				close(7,status='delete')
				if(debug .and. rank.eq.0) print *, rank,": resetting arrays"
				
			endif
			
#ifdef MPI      
				
				if (dsetbool(iv) .eq. 1) then 
					
					call h5dget_space_f(dset_id(iv), filespace(iv), error)
					
					call h5sselect_hyperslab_f(filespace(iv),H5S_SELECT_SET_F,offset &
					,count, error)
				endif
#endif
			
			if(debug)print *,rank,": selected hyperslab"
			!
			! Write the dataset collectively. 
			!
			!      print *,rank,": before h5dwrite" 
			!      temporary1=temporary_arr(:,:,:,iv)
			!_arr(:,:,:,iv)
			
			do k=kstart, kfinish, istep !1,mz/istep
				nz=(k-kstart)/istep+1
				do j=jstart, jfinish,istep !1,my/istep
					ny=(j-jstart)/istep+1
					do i=1,mx/istep
						
						call interpolfld(real(i*istep),real(j),real(k),bx0 &
						,by0,bz0,ex0,ey0,ez0,bxd,byd,bzd,exd,eyd,ezd)
						
						if(iv.eq.1)temporary1(i,ny,nz)=real(ex0+exd,4)
						if(iv.eq.2)temporary1(i,ny,nz)=real(ey0+eyd,4)
						if(iv.eq.3)temporary1(i,ny,nz)=real(ez0+ezd,4)
						if(iv.eq.4)temporary1(i,ny,nz)=real(bx0+bxd,4)
						if(iv.eq.5)temporary1(i,ny,nz)=real(by0+byd,4)
						if(iv.eq.6)temporary1(i,ny,nz)=real(bz0+bzd,4)
						
						
						if(iv.eq.11-4)temporary1(i,ny,nz)=real(curx(i*istep,j,k),4) !nav
						if(iv.eq.12-4)temporary1(i,ny,nz)=real(cury(i*istep,j,k),4) !navi
						
						if(iv .ge. 14-5) then
							call interpolcurrent(real(i*istep),real(j),real(k) &
							,curx0, cury0, curz0)
						endif
						
						!        if(iv.eq.13-4)temporary1(i,ny,nz)=0.
						if(iv.eq.14-5)temporary1(i,ny,nz)=real(curx0,4)
						if(iv.eq.15-5)temporary1(i,ny,nz)=real(cury0,4)
						if(iv.eq.16-5)temporary1(i,ny,nz)=real(curz0,4)
						
					enddo
				enddo
			enddo
			
			if(debug)print *,rank,": before h5dwrite","filespace",filespace(iv &
			),memspace
			
#ifdef MPI
				
				if (dsetbool(iv) .eq. 1) then 
					
					call h5dwrite_real_1(dset_id(iv), H5T_NATIVE_REAL, temporary1(:,: &
					,:),dimsfi,error,file_space_id = filespace(iv), mem_space_id &
					=memspace,xfer_prp = plist_id)
				endif
				
#else
				call h5dwrite_f(dset_id(iv),H5T_NATIVE_REAL,TEMPORARY1(:,: &
				,:) ,dimsfi,error)
#endif      
			
		enddo !ivar
	
		if(debug)print *,rank,": after h5dwrite"
	
		!
		! Close dataspaces.
		!
#ifdef MPI
			do iv=1,nvars
				
				if (dsetbool(iv) .eq. 1) then 
					
					call h5sclose_f(filespace(iv), error)
				endif
			enddo
			call h5sclose_f(memspace, error)
#endif
		!
		! Close the dataset and property list.
		!
		do iv=1,nvars
			
			if (dsetbool(iv) .eq. 1) then 
				
				call h5dclose_f(dset_id(iv), error)
			endif
		enddo
#ifdef MPI
			call h5pclose_f(plist_id, error)
#endif
		!
		! Close the file.
		!
		call h5fclose_f(file_id, error)
		!
		! Close FORTRAN predefined datatypes.
		!
		call h5close_f(error)
		
		if(debug) print *, rank,": finished writing fields"
		
		!-------------------------------------
		!now write particles
		!for huge don't need to write particles
		goto 1681
		!
		!  Initialize FORTRAN predefined datatypes
		!
		call h5open_f(error) 
		
		! 
		! Setup file access property list with parallel I/O access.
		!
#ifdef MPI
			call h5pcreate_f(H5P_FILE_ACCESS_F, plist_id, error)
			call h5pset_fapl_mpio_f(plist_id, mpi_comm_world, mpi_info_null, &
			error)
#endif
		nvars=14+2+2+2
		midvars=7+1+1+1
		!      stride=100 !20 !1 !100 !60 !15 !*4
		dsetname(1)="xi"
		dsetname(2)="yi"
		dsetname(3)="zi"
		dsetname(4)="ui"
		dsetname(5)="vi"
		dsetname(6)="wi"
		dsetname(7)="chi"
		dsetname(8)="gammai"
		dsetname(9)="indi"
		dsetname(10)="proci"
		dsetname(11)="xe"
		dsetname(12)="ye"
		dsetname(13)="ze"
		dsetname(14)="ue"
		dsetname(15)="ve"
		dsetname(16)="we"
		dsetname(17)="che"
		dsetname(18)="gammae"
		dsetname(19)="inde"
		dsetname(20)="proce"
		!      dsetname(15)="spectrum"
		!need to add writing spectrum from root
		
		datarank=1
		!find out number of ions and lecs
#ifdef MPI
			call mpi_allgather(ions,1,mpi_integer,all_ions,1,mpi_integer &
			,mpi_comm_world, error)
			call mpi_allgather(lecs,1,mpi_integer,all_lecs,1,mpi_integer &
			,mpi_comm_world, error)
			all_ions_lng=all_ions
			all_lecs_lng=all_lecs
#else
			all_ions=ions
			all_lecs=lecs
			all_ions_lng=all_ions
			all_lecs_lng=all_lecs
#endif
		dimsfions(1)=sum(all_ions_lng/stride)
		dimsflecs(1)=sum(all_lecs_lng/stride)
		dimsfiions(1)=dimsfions(1)
		dimsfilecs(1)=dimsflecs(1)
		dimsfiions(2:7)=0
		dimsfilecs(2:7)=0
		if(debug .and. rank .eq. 0) then 
			print *, "all_ions", all_ions
			print *, "all_lecs", all_lecs
			print *,"dimsions", sum(all_ions_lng/stride)
			print *,"dimslecs", sum(all_lecs_lng/stride)
		endif
		
		allocate(temporary_vec(max(ions/stride,lecs/stride)))
		
		!
		! Create the file collectively.
		! 
#ifdef MPI
			call h5fcreate_f(fnameprt, H5F_ACC_TRUNC_F, file_id, error, &
			access_prp = plist_id)
			call h5pclose_f(plist_id, error)
#else
			call h5fcreate_f(fnameprt, H5F_ACC_TRUNC_F, file_id, error)
#endif
		
		!
		! Create the data space for the  dataset. 
		!
		
		do i=1,midvars
			call h5screate_simple_f(datarank, dimsfions(1), filespace(i), &
			error)
		enddo
		do i=midvars+1,nvars
			call h5screate_simple_f(datarank, dimsflecs(1), filespace(i), &
			error)
		enddo
		
		!
		! Create the dataset with default properties.
		!
		do i=1,nvars
			call h5dcreate_f(file_id, dsetname(i), H5T_NATIVE_REAL, &
			filespace(i),dset_id(i), error)
			call h5sclose_f(filespace(i), error)
		enddo
		
		!
		! Create property list for collective dataset write
		!
#ifdef MPI
			call h5pcreate_f(H5P_DATASET_XFER_F, plist_id, error) 
			call h5pset_dxpl_mpio_f(plist_id, H5FD_MPIO_COLLECTIVE_F, error)
#endif
		! 
		! Select hyperslab in the file.
		!
		do i=1,nvars
			
			!
			! Each process defines dataset in memory and writes it to the hyperslab
			! in the file. 
			!
			if(i.le.midvars)countpart = ions/stride
			if(i.gt.midvars)countpart = lecs/stride
			
			!      if(rank .eq. 0) print *, "allcounts=",all_counts
			offsetpart = 0
			if(rank .gt. 0) then 
				if(i .le. midvars) then
					offsetpart = sum(all_ions_lng(1:rank)/stride)
				else
					offsetpart = sum(all_lecs_lng(1:rank)/stride)
				endif
				!      print *,rank,":","offset=", offsetpart, "count=",countpart      
			endif
			
#ifdef MPI
				call h5screate_simple_f(1, countpart, memspace, error) 
				
				call h5dget_space_f(dset_id(i), filespace(i), error)
				call h5sselect_hyperslab_f (filespace(i),H5S_SELECT_SET_F &
				,offsetpart,countpart, error)
#endif
			!
			! Write the dataset collectively. 
			!
			!      print *,rank,": before h5dwrite" 
			!      temporary1=temporary_arr(:,:,:,i)
			!_arr(:,:,:,i)
			temporary_vec=0.
			if(i.eq.1)  temporary_vec(1:ions/stride)=p(1:ions:stride)%x
			if(i.eq.2) temporary_vec(1:ions/stride)=p(1:ions:stride)%y &
			+modulo(rank,sizey)*(myall-5)
			if(i.eq.3) temporary_vec(1:ions/stride)=p(1:ions:stride)%z+(rank &
			/sizey)*(mzall-5)
			
			if(i.eq.4) temporary_vec(1:ions/stride)=p(1:ions:stride)%u
			if(i.eq.5) temporary_vec(1:ions/stride)=p(1:ions:stride)%v
			if(i.eq.6) temporary_vec(1:ions/stride)=p(1:ions:stride)%w
			!         if(i.eq.7) temporary_vec(1:ions/stride)=sqrt(1./(1.
			!     &             -(p(1:ions:stride)%u**2+p(1:ions:stride)%v**2
			!     &             +p(1:ions:stride)%w **2)/c**2))
			
			if(i.eq.7) temporary_vec(1:ions/stride)=p(1:ions:stride)%ch
			if(i.eq.8) temporary_vec(1:ions/stride)=sqrt(1. &
			+(p(1:ions:stride)%u**2+p(1:ions:stride)%v**2 &
			+p(1:ions:stride)%w**2))
			
			if(i.eq.9) temporary_vec(1:ions/stride)=real(p(1:ions:stride) &
			%ind,4)
			
			if(i.eq.10) temporary_vec(1:ions/stride)=real(p(1:ions:stride) &
			%proc,4)
			
			if(i.eq.11) temporary_vec(1:lecs/stride)=p(maxhlf+1:maxhlf &
			+lecs:stride)%x
			if(i.eq.12) temporary_vec(1:lecs/stride)=p(maxhlf+1:maxhlf &
			+lecs:stride)%y+modulo(rank,sizey)*(myall-5)
			if(i.eq.13) temporary_vec(1:lecs/stride)=p(maxhlf+1:maxhlf &
			+lecs:stride)%z+(rank/sizey)*(mzall-5)
			if(i.eq.14) temporary_vec(1:lecs/stride)=p(maxhlf+1:maxhlf &
			+lecs:stride)%u
			if(i.eq.15) temporary_vec(1:lecs/stride)=p(maxhlf+1:maxhlf &
			+lecs:stride)%v
			if(i.eq.16) temporary_vec(1:lecs/stride)=p(maxhlf+1:maxhlf &
			+lecs:stride)%w
			!         if(i.eq.14) temporary_vec(1:lecs/stride)=sqrt(1./(1. -(p(maxhlf
			!     &             +1:maxhlf+lecs:stride)%u**2+p(maxhlf+1:maxhlf +lecs:stride
			!     &             )%v**2+p(maxhlf +1:maxhlf+lecs:stride)%w**2)/c**2))
			
			if(i.eq.17) temporary_vec(1:lecs/stride)=p(maxhlf+1:maxhlf &
			+lecs:stride)%ch
			if(i.eq.18) temporary_vec(1:lecs/stride)=sqrt(1. +(p(maxhlf &
			+1:maxhlf+lecs:stride)%u**2+p(maxhlf+1:maxhlf +lecs:stride &
			)%v**2+p(maxhlf +1:maxhlf+lecs:stride)%w**2))
			
			if(i.eq.19)temporary_vec(1:lecs/stride)=real(p(maxhlf+1:maxhlf &
			+lecs:stride)%ind,4)
			
			if(i.eq.20)temporary_vec(1:lecs/stride)=real(p(maxhlf+1:maxhlf &
			+lecs:stride)%proc,4)
			
#ifdef MPI
				if(i .le. midvars) then 
					call h5dwrite_real_1(dset_id(i), H5T_NATIVE_REAL, &
					temporary_vec(1:ions/stride), dimsfiions,error &
					,file_space_id = filespace(i), mem_space_id=memspace &
					,xfer_prp = plist_id)
				else 
					call h5dwrite_real_1(dset_id(i), H5T_NATIVE_REAL, &
					temporary_vec(1:lecs/stride), dimsfilecs,error &
					,file_space_id = filespace(i), mem_space_id=memspace &
					,xfer_prp = plist_id)
				endif
#else
				if(i .le. midvars) then 
					call h5dwrite_f(dset_id(i), H5T_NATIVE_REAL, &
					temporary_vec(1:ions/stride), dimsfiions,error)
				else 
					call h5dwrite_f(dset_id(i), H5T_NATIVE_REAL, &
					temporary_vec(1:lecs/stride), dimsfilecs,error)
				endif
#endif
		
#ifdef MPI
				call h5sclose_f(memspace,error)
#endif
		
		enddo
	
		!
		! Close dataspaces.
		!
	
#ifdef MPI
			do i=1,nvars
				call h5sclose_f(filespace(i), error)
			enddo
#endif
		!
		! Close the dataset and property list.
		!
		do i=1,nvars
			call h5dclose_f(dset_id(i), error)
		enddo
#ifdef MPI
			call h5pclose_f(plist_id, error)
#endif
		!
		! Close the file.
		!
		call h5fclose_f(file_id, error)
		!
		! Close FORTRAN predefined datatypes.
		!
		call h5close_f(error)
		
		if(debug) print *, rank,": finished writing particles"
		
		!now write particles
		!      if(rank.eq.0)then
		
		is=1
		
		deallocate(temporary_vec)
		
		1681     continue
		
		goto 11000
		
		write(12)real((ions/is),4),real((lecs/is),4), &
		real(maxptl,4),real(maxhlf,4), &
		(real(p(n)%x,4),n=1,(ions/is)*is,is), &
		(real(p(n)%x,4),n=maxhlf+1,maxhlf+(lecs/is)*is,is), &
		(real(p(n)%y,4),n=1,(ions/is)*is,is), &
		(real(p(n)%y,4),n=maxhlf+1,maxhlf+(lecs/is)*is,is), &
		(real(p(n)%z,4),n=1,(ions/is)*is,is), &
		(real(p(n)%z,4),n=maxhlf+1,maxhlf+(lecs/is)*is,is), &
		(real(p(n)%u,4),n=1,(ions/is)*is,is), &
		(real(p(n)%u,4),n=maxhlf+1,maxhlf+(lecs/is)*is,is), &
		(real(p(n)%v,4),n=1,(ions/is)*is,is), &
		(real(p(n)%v,4),n=maxhlf+1,maxhlf+(lecs/is)*is,is), &
		(real(p(n)%w,4),n=1,(ions/is)*is,is), &
		(real(p(n)%w,4),n=maxhlf+1,maxhlf+(lecs/is)*is,is),     &
		(real(p(n)%ch,4),n=1,(ions/is)*is,is), &
		(real(p(n)%ch,4),n=maxhlf+1,maxhlf+(lecs/is)*is,is),     &
		(real(1./sqrt(1.-(p(n)%u**2+p(n)%v**2+p(n)%w**2)/c**2),4),  &
		n=1,(ions/is)*is,is), &
		(real(1./sqrt(1.-(p(n)%u**2+p(n)%v**2+p(n)%w**2)/c**2),4),  &
		n=maxhlf+1,maxhlf+(lecs/is)*is,is)
		
		11000 continue
		deallocate(temporary1)
	
	endif
	
	istep=istep0
	
	
end subroutine output_par



!-------------------------------------------------------------------------------
! 						subroutine write_testp1					 
!																		
!
!  							
!-------------------------------------------------------------------------------

subroutine write_testp1()
  implicit none
  integer n, n1, statusfile
  logical ext, twod
  real bx0,by0,bz0,ex0,ey0,ez0,dummy,gam
  character fnamehen*20, fnametst*20
  integer lapsel,num1,proc1
  
  !     lapsel should match what it is written in selectprt.f
  twod=.false.
  lapsel=100000
  
  write(fnamehen,"(a9,i3.3)") "test.ene.",rank
  
  if(lap .lt. lapst+dlap) then  !first time -- delete the file and read particles to be saved
     if(irestart.eq.0) then 
        open(unit=44,file=fnamehen)
        close(44,status='delete') 
        open(unit=44,file=fnamehen)
     endif
     
     if(irestart.eq.1) then 
        inquire(file=fnamehen,exist=ext)
        
        statusfile=0
        if(ext) statusfile=1
        if(statusfile .eq. 0) then 
           open(unit=44,file=fnamehen)
        else
           open(unit=44,file=fnamehen,status='old',position='APPEND' &
                )
        endif
     endif
     
     !     read the particles to be saved
     write(fnametst,"(a9,i6.6)") "test.sel.",lapsel
     open(unit=43, file=fnametst,form='formatted')
     if (rank .eq. 0) print *,"loading particles from ",fnametst
     sele=0
     seli=0
301  read(43,*,err=398,end=399)lapsel,num1,proc1,gam
     if (modulo(abs(num1)-1,2) .eq. 0) then !ions
        seli=seli+1
     else                   !electrons
        sele=sele+1
     endif
     goto 301
398  print *,"error reading the test.sel. file"
399  continue
     
     close(43)
     
     if (seli .ge. 1) allocate(selprti(seli))
     if (sele .ge. 1) allocate(selprte(sele))
     if (rank .eq. 0) print *,"done allocation ",fnametst
     
     open(unit=43, file=fnametst,form='formatted')
     sele=0
     seli=0
401  read(43,*,err=498,end=499)lapsel,num1,proc1,gam
     if (modulo(abs(num1)-1,2) .eq. 0) then !ions
        seli=seli+1
        selprti(seli)%ind=num1
        selprti(seli)%proc=proc1
     else !electrons
        sele=sele+1
        selprte(sele)%ind=num1
        selprte(sele)%proc=proc1
     endif
     goto 401
498  print *,"error reading the test.sel. file"
499  continue
     
  endif !first time
  
  
  !     search for ions
  do n=1,ions
     if(p(n)%ind .ne. 0) then 
        do n1=1,seli
           if(p(n)%ind .eq. selprti(n1)%ind .and. p(n)%proc .eq. &
                selprti(n1)%proc) then 
              call interpolfld(real(p(n)%x),p(n)%y,p(n)%z,bx0,by0, &
                   bz0,ex0,ey0,ez0,dummy,dummy,dummy,dummy,dummy,dummy)
              if (twod) then
                 write(44,fmt="(i7, i12,i5,2(F10.3,' '), 6(E15.5,' ') &
                      )")lap,p(n)%ind,p(n)%proc,p(n)%x,p(n)%y &
                      +modulo(rank,sizey)*(myall-5), &
                      p(n)%u,p(n)%v, &
                      sqrt(1.+(p(n)%u**2+p(n)%v**2+p(n)%w**2)),&
                      bz0,ex0,ey0
              else
                 write(44,fmt="(i7, i12,i5,3(F10.3,' '), 10(E15.5,' ') &
                      )")lap,p(n)%ind,p(n)%proc,p(n)%x,p(n)%y &
                      +modulo(rank,sizey)*(myall-5),p(n)%z+(rank/sizey) &
                      *(mzall-5),p(n)%u,p(n)%v,p(n)%w, &
                      sqrt(1.+(p(n)%u**2+p(n)%v**2+p(n)%w**2)),bx0 &
                      ,by0,bz0,ex0,ey0,ez0                 
              endif
           endif
        enddo
     endif
  enddo
  
  !      goto 456
  !     search for electrons
  do n=maxhlf+1,maxhlf+lecs
     if(p(n)%ind .ne. 0) then
        do n1=1,sele
           if(p(n)%ind .eq. selprte(n1)%ind .and. p(n)%proc .eq. &
                selprte(n1)%proc) then 
              call interpolfld(real(p(n)%x),p(n)%y,p(n)%z,bx0,by0,&
                   bz0,ex0,ey0,ez0,dummy,dummy,dummy,dummy,dummy,dummy)
              if (twod) then
                 write(44,fmt="(i7, i12,i5,2(F10.3,' '), 6(E15.5,' ')&
                      )")lap,p(n)%ind,p(n)%proc,p(n)%x,p(n)%y &
                      +modulo(rank,sizey)*(myall-5), &
                      p(n)%u,p(n)%v, &
                      sqrt(1.+(p(n)%u**2+p(n)%v**2+p(n)%w**2)), &
                      bz0,ex0,ey0
              else
                 write(44,fmt="(i7, i12,i5,3(F10.3,' '), 10(E15.5,' ') &
                      )")lap,p(n)%ind,p(n)%proc,p(n)%x,p(n)%y &
                      +modulo(rank,sizey)*(myall-5),p(n)%z+(rank/sizey) &
                      *(mzall-5),p(n)%u,p(n)%v,p(n)%w, &
                      sqrt(1.+(p(n)%u**2+p(n)%v**2+p(n)%w**2)),bx0 &
                      ,by0,bz0,ex0,ey0,ez0                 
              endif
           endif
        enddo
     endif
  enddo
456 continue
  
end subroutine write_testp1


!-------------------------------------------------------------------------------
! 						subroutine write_testp0					 
!																		
! 
!  							
!-------------------------------------------------------------------------------

subroutine write_testp0()

  implicit none

	! local variables

	integer n,n1,statusfile,tstart
	integer subpart,nsavi,nsave,nchild,powcut
	logical ext, twod
	real bx0,by0,bz0,ex0,ey0,ez0,dummy,gam,shockloc,shockloc0
	character fnamehen*20
	logical cond1a,cond1b,cond1,cond2
	real d_cut,u_cut
	real, dimension (4) :: gami_cut, game_cut
	integer, dimension (4) :: whcut
	
	!     usage: nchild=1 is uniform downsampling; gami_cut(1) is lowest energy
	twod=.false.
	subpart=50 !for the highest energy bin
	nchild=4 !for splitting
	gami_cut=(/50.,100.,200.,400./)
	game_cut=gami_cut !(/40.,80.,160.,320./)      
	d_cut=400. ! less than d_cut skin depths downstream 
	u_cut=800. ! the same, in the upstream
	tstart=0 !starting lap
	
	whcut=0
	nsavi=0
	nsave=0
	write(fnameprt,"(a9,i3.3)") "test.prt.",rank
	fnamehen = "highen"//"."//trim(rankchar)
	
	if (rank .eq. 0) print *, "prt_first",prt_first
	
	!      if(lap .lt. lapst+dlap) then  !first time -- delete the file !uncomment this when saving just for one time
	if (prt_first) then
	 if(irestart.eq.0) then 
		open(unit=22,file=fnameprt)
		close(22,status='delete') 
		open(unit=22,file=fnameprt)
		
		open(unit=42,file=fnamehen)
		close(42,status='delete') 
		open(unit=42,file=fnamehen)
	 endif
     
	 if(irestart.eq.1) then 
		
		inquire(file=fnameprt,exist=ext)
		
		statusfile=0
		if(ext) statusfile=1
		if(statusfile .eq. 0) then 
		   open(unit=22,file=fnameprt)
		else
		   open(unit=22,file=fnameprt,status='old',position='APPEND' &
				)
		endif
		
		
		!commented out highen saving 8/27/08: check cond2 var below
		inquire(file=fnamehen,exist=ext)
		
		statusfile=0
		if(ext) statusfile=1
		if(statusfile .eq. 0) then 
		   !               open(unit=42,file=fnamehen)
		else
		   !               open(unit=42,file=fnamehen,status='old',position='APPEND' &
		   !                   )
		endif
		
	 endif
	endif
	
	shockloc=lap*c*betshock !*movingshock
	
	shockloc0=betshock*c*tstart
	
	do n=1,ions 
	 !minus 1 because all ions are odd, and electrons are even
	 !position
	 cond1a=(p(n)%ind .ne. 0. .and. &
		  p(n)%x .gt. shockloc0-d_cut*c_omp-100-(lap-tstart)*c .and. &
		  p(n)%x .lt. shockloc0+u_cut*c_omp+100+(lap-tstart)*c) 
	 cond1a=(p(n)%ind .ne. 0. .and. &
		  p(n)%x .gt. shockloc-d_cut*c_omp .and. &
		  p(n)%x .lt. shockloc+u_cut*c_omp)
	 !energy
	 gam=sqrt(1+(p(n)%u**2+p(n)%v**2+p(n)%w**2))
	 where(gami_cut/gam .gt. 1.) whcut=1
	 if (whcut(1) .eq. 0) then !above gami_cut(1)
		powcut=sum(whcut)
		cond1b=(modulo(abs(p(n)%ind),subpart*nchild**powcut) .eq. 1)
	 else
		cond1b=.false. !below gami_cut(1)
	 endif
	 whcut=0
	 !total
	 cond1=(cond1a .and. cond1b)
	 
	 cond2=(gam .ge. 500000 .and. p(n)%x .gt. shockloc-4000)
	 
	 if(cond1  .or. cond2) then 
		if (prt_first) nsavi=nsavi+1   
		
		call interpolfld(real(p(n)%x),p(n)%y,p(n)%z,bx0,by0,bz0,ex0,ey0 &
			 ,ez0,dummy,dummy,dummy,dummy,dummy,dummy)
		
		if(cond1) then 
		   if (twod) then
			  write(22,fmt="(i7, i12,i5,2(F10.3,' '), 6(E15.5,' '))")lap &
				   ,p(n)%ind,p(n)%proc,p(n)%x,p(n)%y +modulo(rank,sizey) &
				   *(myall -5),p(n)%u,p(n) &
				   %v,sqrt(1.+(p(n)%u**2+p(n)%v**2+p(n)%w**2)), &
				   bz0,ex0,ey0
		   else
			  write(22,fmt="(i7, i12,i5,3(F10.3,' '), 10(E15.5,' '))")lap &
				   ,p(n)%ind,p(n)%proc,p(n)%x,p(n)%y +modulo(rank,sizey) &
				   *(myall -5),p(n)%z+(rank /sizey)*(mzall-5),p(n)%u,p(n) &
				   %v,p(n)%w,sqrt(1.+(p(n)%u**2+p(n)%v**2+p(n)%w**2)),bx0 &
				   ,by0,bz0,ex0,ey0,ez0
		   endif
		endif
		   
		if(cond2) then 
		   if (twod) then
			  write(42,fmt="(i7, i12,i5,2(F10.3,' '), 6(E15.5,' '))")lap &
				   ,p(n)%ind,p(n)%proc,p(n)%x,p(n)%y +modulo(rank,sizey) &
				   *(myall -5),p(n)%u,p(n) &
				   %v,sqrt(1.+(p(n)%u**2+p(n)%v**2+p(n)%w**2)), &
				   bz0,ex0,ey0 
		   else
			  write(42,fmt="(i7, i12,i5,3(F10.3,' '), 10(E15.5,' '))")lap &
				   ,p(n)%ind,p(n)%proc,p(n)%x,p(n)%y +modulo(rank,sizey) &
				   *(myall -5),p(n)%z+(rank /sizey)*(mzall-5),p(n)%u,p(n) &
				   %v,p(n)%w,sqrt(1.+(p(n)%u**2+p(n)%v**2+p(n)%w**2)),bx0 &
				   ,by0,bz0,ex0,ey0,ez0
		   endif
		endif
		
	 endif
	 
	 
	enddo
	
	if (rank .eq. 0 .and. prt_first) &
	   print *,"Saved ions in rank=0:",nsavi
	
	!---------------------------------------------------------------------------
	!      goto 7066 !only save positrons
	
	do n=maxhlf+1,maxhlf+lecs 
	 !     no minus 1 because all ions are odd, and electrons are even
	 !position
	 cond1a=(p(n)%ind .ne. 0. .and. &
		  p(n)%x .gt. shockloc0-d_cut*c_omp-100-(lap-tstart)*c .and. &
		  p(n)%x .lt. shockloc0+u_cut*c_omp+100+(lap-tstart)*c)
	 cond1a=(p(n)%ind .ne. 0. .and. &
		  p(n)%x .gt. shockloc-d_cut*c_omp .and. &
		  p(n)%x .lt. shockloc+u_cut*c_omp)
	 !energy
	 gam=sqrt(1+(p(n)%u**2+p(n)%v**2+p(n)%w**2))
	 where(game_cut/gam .gt. 1.) whcut=1
	 if (whcut(1) .eq. 0) then !above game_cut(1)
		powcut=sum(whcut)
		cond1b=(modulo(abs(p(n)%ind),subpart*nchild**powcut) .eq. 0)
	 else
		cond1b=.false. !below game_cut(1)
	 endif
	 whcut=0
	 !total
	 cond1=(cond1a .and. cond1b)
	 
	 cond2=(gam .ge. 500000 .and. p(n)%x .gt. shockloc-4000)
	 
	 if(cond1  .or. cond2)  then
		if (prt_first) nsave=nsave+1    
		
		call interpolfld(real(p(n)%x),p(n)%y,p(n)%z,bx0,by0,bz0,ex0,ey0 &
			 ,ez0,dummy,dummy,dummy,dummy,dummy,dummy)
		
		if(cond1) then
		   if (twod) then
			  write(22,fmt="(i7, i12,i5,2(F10.3,' '), 6(E15.5,' '))")lap &
				   ,p(n)%ind,p(n)%proc,p(n)%x,p(n)%y +modulo(rank,sizey) &
				   *(myall -5),p(n)%u,p(n) &
				   %v,sqrt(1.+(p(n)%u**2+p(n)%v**2+p(n)%w**2)), &
				   bz0,ex0,ey0
		   else
			  write(22,fmt="(i7, i12,i5,3(F10.3,' '), 10(E15.5,' '))")lap &
				   ,p(n)%ind,p(n)%proc,p(n)%x,p(n)%y +modulo(rank,sizey) &
				   *(myall -5),p(n)%z+(rank /sizey)*(mzall-5),p(n)%u,p(n) &
				   %v,p(n)%w,sqrt(1.+(p(n)%u**2+p(n)%v**2+p(n)%w**2)),bx0 &
				   ,by0,bz0,ex0,ey0,ez0
		   endif
		endif
		
		if(cond2) then 
		   if (twod) then
			  write(42,fmt="(i7, i12,i5,2(F10.3,' '), 6(E15.5,' '))")lap &
				   ,p(n)%ind,p(n)%proc,p(n)%x,p(n)%y +modulo(rank,sizey) &
				   *(myall -5),p(n)%u,p(n) &
				   %v,sqrt(1.+(p(n)%u**2+p(n)%v**2+p(n)%w**2)), &
				   bz0,ex0,ey0
		   else
			  write(42,fmt="(i7, i12,i5,3(F10.3,' '), 10(E15.5,' '))")lap &
				   ,p(n)%ind,p(n)%proc,p(n)%x,p(n)%y +modulo(rank,sizey) &
				   *(myall -5),p(n)%z+(rank /sizey)*(mzall-5),p(n)%u,p(n) &
				   %v,p(n)%w,sqrt(1.+(p(n)%u**2+p(n)%v**2+p(n)%w**2)),bx0 &
				   ,by0,bz0,ex0,ey0,ez0
		   endif
		endif
	
	 endif
	 
	 
	enddo
	
	if (rank .eq. 0 .and. prt_first) & 
	   print *,"Saved electrons in rank=0:",nsave
	
	7066 continue
	
	!--------------------------------------------------------------------
	
	
	if(rank .eq. 0) print *, "Test particles -- 1 in ",subpart," !" 
	
	!check which particles are the same
	goto 7067
	do n=1,ions
	 if(p(n)%ind .ne. 0) then 
		do n1=1,ions
		   if(p(n)%ind .eq. p(n1)%ind) then 
			  if(n .ne. n1) then 
				 print *, "Same ion",rank,":",p(n)%ind, n, n1 
			  endif
		   endif
		   
		enddo
	 endif
	enddo
	
	do n=maxhlf+1,maxhlf+lecs
	 if(p(n)%ind .ne. 0) then
		do n1=maxhlf+1, maxhlf+lecs
		   if(p(n)%ind .eq. p(n1)%ind) then 
			  if(n .ne. n1) then 
				 print *, "Same lec",rank,":",p(n)%ind, n, n1 
			  endif
		   endif
		   
		enddo
	 endif
	enddo
	7067 continue
  
end subroutine write_testp0


!-------------------------------------------------------------------------------
! 						subroutine interpolfld					 
!																		
!
!  							
!-------------------------------------------------------------------------------

subroutine interpolfld(x,y,z,bx0,by0,bz0,ex0,ey0,ez0,bx_ext,by_ext,bz_ext,ex_ext,ey_ext,ez_ext)
	
	implicit none
	
	! dummy variables
	
	real(sprec) :: x,y,z,bx0,by0,bz0,ex0,ey0,ez0,bx_ext,by_ext,bz_ext,ex_ext,ey_ext,ez_ext
	
	! local variables
	
	real dx, dy, dz, f, g
	real u0,v0,w0,u1,v1,w1
	integer ::l,i,j,k

	i=x
	dx=x-i
	j=y
	dy=y-j
	k=z
	dz=z-k
	if(i.eq.1)i=2
	if(i.eq.mx)i=mx-1
	if(j.eq.1)j=2
	if(j.eq.my)j=my-1
	if(k.eq.1)k=2
	if(k.eq.mz)k=mz-1
	
#ifdef twoD
		k=1
		dz=0
		iz=0
#endif
	
	l=i+iy*(j-1)+iz*(k-1)
	
	! Field interpolations are tri-linear (linear in x times linear in y
	! times linear in z). This amounts to the 3-D generalisation of "area
	! weighting". A modification of the simple linear interpolation formula
	!           f(i+dx) = f(i) + dx * (f(i+1)-f(i))
	! is needed since fields are recorded at half-integer ::locations in certain
	! dimensions: see comments and illustration with the Maxwell part of this
	! code. One then has first to interpolate from "midpoints" to "gridpoints"
	! by averaging neighbors. Then one proceeds with normal interpolation.
	! Combining these two steps leads to:
	!   f at location i+dx  = half of f(i)+f(i-1) + dx*(f(i+1)-f(i-1))
	! where now f(i) means f at location i+1/2. The halving is absorbed
	! in the final scaling.
	!   E-component interpolations:

	f=ex(l,1,1)+ex(l-ix,1,1)+dx*(ex(l+ix,1,1)-ex(l-ix,1,1))
	f=f+dy*(ex(l+iy,1,1)+ex(l-ix+iy,1,1)+dx*(ex(l+ix+iy,1,1)-ex(l-ix &
	+iy,1,1))-f)
	g=ex(l+iz,1,1)+ex(l-ix+iz,1,1)+dx*(ex(l+ix+iz,1,1)-ex(l-ix+iz,1 &
	,1))
	g=g+dy* &
	(ex(l+iy+iz,1,1)+ex(l-ix+iy+iz,1,1)+dx*(ex(l+ix+iy+iz,1,1) &
	-ex(l-ix+iy+iz,1,1))-g)
	ex0=(f+dz*(g-f))*(.5)
	
	!   ------------
	f=ey(l,1,1)+ey(l-iy,1,1)+dy*(ey(l+iy,1,1)-ey(l-iy,1,1))
	f=f+dz*(ey(l+iz,1,1)+ey(l-iy+iz,1,1)+dy*(ey(l+iy+iz,1,1)-ey(l-iy &
	+iz,1,1))-f)
	g=ey(l+ix,1,1)+ey(l-iy+ix,1,1)+dy*(ey(l+iy+ix,1,1)-ey(l-iy+ix,1 &
	,1))
	g=g+dz* &
	(ey(l+iz+ix,1,1)+ey(l-iy+iz+ix,1,1)+dy*(ey(l+iy+iz+ix,1,1) &
	-ey(l-iy+iz+ix,1,1))-g)
	ey0=(f+dx*(g-f))*(.5)
	!   ------------
	f=ez(l,1,1)+ez(l-iz,1,1)+dz*(ez(l+iz,1,1)-ez(l-iz,1,1))
	f=f+dx*(ez(l+ix,1,1)+ez(l-iz+ix,1,1)+dz*(ez(l+iz+ix,1,1)-ez(l-iz &
	+ix,1,1))-f)
	g=ez(l+iy,1,1)+ez(l-iz+iy,1,1)+dz*(ez(l+iz+iy,1,1)-ez(l-iz+iy,1 &
	,1))
	g=g+dx* &
	(ez(l+ix+iy,1,1)+ez(l-iz+ix+iy,1,1)+dz*(ez(l+iz+ix+iy,1,1) &
	-ez(l-iz+ix+iy,1,1))-g)
	ez0=(f+dy*(g-f))*(.5)
	!   ---------
	!   B-component interpolations:
	f=bx(l-iy,1,1)+bx(l-iy-iz,1,1)+dz*(bx(l-iy+iz,1,1)-bx(l-iy-iz,1 &
	,1))
	f=bx(l,1,1)+bx(l-iz,1,1)+dz*(bx(l+iz,1,1)-bx(l-iz,1,1))+f+dy &
	* (bx(l+iy,1,1)+bx(l+iy-iz,1,1)+dz*(bx(l+iy+iz,1,1)-bx(l+iy &
	-iz,1,1))-f)
	g=bx(l+ix-iy,1,1)+bx(l+ix-iy-iz,1,1)+dz*(bx(l+ix-iy+iz,1,1) &
	-bx(l+ix-iy-iz,1,1))
	g=bx(l+ix,1,1)+bx(l+ix-iz,1,1)+dz*(bx(l+ix+iz,1,1)-bx(l+ix-iz,1 &
	,1))+g+dy*(bx(l+ix+iy,1,1)+bx(l+ix+iy-iz,1,1)+dz*(bx(l+ix &
	+iy+iz,1,1)-bx(l+ix+iy-iz,1,1))-g)
	bx0=(f+dx*(g-f))*(.25)
	!   ------------
	f=by(l-iz,1,1)+by(l-iz-ix,1,1)+dx*(by(l-iz+ix,1,1)-by(l-iz-ix,1 &
	,1))
	f=by(l,1,1)+by(l-ix,1,1)+dx*(by(l+ix,1,1)-by(l-ix,1,1))+f+dz &
	* (by(l+iz,1,1)+by(l+iz-ix,1,1)+dx*(by(l+iz+ix,1,1)-by(l+iz &
	-ix,1,1))-f)
	g=by(l+iy-iz,1,1)+by(l+iy-iz-ix,1,1)+dx*(by(l+iy-iz+ix,1,1)-by(l &
	+iy-iz-ix,1,1))
	g=by(l+iy,1,1)+by(l+iy-ix,1,1)+dx*(by(l+iy+ix,1,1)-by(l+iy-ix,1 &
	,1))+g+dz*(by(l+iy+iz,1,1)+by(l+iy+iz-ix,1,1)+dx*(by(l+iy &
	+iz+ix,1,1)-by(l+iy+iz-ix,1,1))-g)
	by0=(f+dy*(g-f))*(.25)
	!   ------------
	f=bz(l-ix,1,1)+bz(l-ix-iy,1,1)+dy*(bz(l-ix+iy,1,1)-bz(l-ix-iy,1 &
	,1))
	f=bz(l,1,1)+bz(l-iy,1,1)+dy*(bz(l+iy,1,1)-bz(l-iy,1,1))+f+dx &
	* (bz(l+ix,1,1)+bz(l+ix-iy,1,1)+dy*(bz(l+ix+iy,1,1)-bz(l+ix &
	-iy,1,1))-f)
	g=bz(l+iz-ix,1,1)+bz(l+iz-ix-iy,1,1)+dy*(bz(l+iz-ix+iy,1,1)-bz(l &
	+iz-ix-iy,1,1))
	g=bz(l+iz,1,1)+bz(l+iz-iy,1,1)+dy*(bz(l+iz+iy,1,1)-bz(l+iz-iy,1 &
	,1))+g+dx*(bz(l+iz+ix,1,1)+bz(l+iz+ix-iy,1,1)+dy*(bz(l+iz &
	+ix+iy,1,1)-bz(l+iz+ix-iy,1,1))-g)
	bz0=(f+dz*(g-f))*(.25)
	
	!        call deutschsub((x),(y),(z),
	!     &   bxd,byd,bzd,exd,eyd,ezd)
	
	bx_ext=0
	by_ext=0
	bz_ext=0
	ex_ext=0
	ey_ext=0
	ez_ext=0

	if(external_fields) then
	   call get_external_fields(x,y,z,ex_ext,ey_ext,ez_ext,bx_ext,by_ext,bz_ext)
	endif


end subroutine interpolfld



!-------------------------------------------------------------------------------
! 						subroutine interpolcurrent					 
!																		
!
!  							
!-------------------------------------------------------------------------------

subroutine interpolcurrent(x,y,z,curx0, cury0, curz0)

	implicit none
	
	! dummy variables
	
	real(sprec) :: x, y, z, curx0, cury0, curz0
	
	! local variables
	
	real dx, dy, dz, f, g
	integer ::l,i,j,k
	
	
	i=x
	dx=x-i
	j=y
	dy=y-j
	k=z
	dz=z-k
	
	if(i.eq.1)i=2
	if(i.eq.mx)i=mx-1
	if(j.eq.1)j=2
	if(j.eq.my)j=my-1
	if(k.eq.1)k=2
	if(k.eq.mz)k=mz-1
	
#ifdef twoD
		k=1
		dz=0
		iz=0
#endif
	
	l=i+iy*(j-1)+iz*(k-1)
	
	! Field interpolations are tri-linear (linear in x times linear in y
	! times linear in z). This amounts to the 3-D generalisation of "area
	! weighting". A modification of the simple linear interpolation formula
	!           f(i+dx) = f(i) + dx * (f(i+1)-f(i))
	! is needed since fields are recorded at half-integer ::locations in certain
	! dimensions: see comments and illustration with the Maxwell part of this
	! code. One then has first to interpolate from "midpoints" to "gridpoints"
	! by averaging neighbors. Then one proceeds with normal interpolation.
	! Combining these two steps leads to:
	!   f at location i+dx  = half of f(i)+f(i-1) + dx*(f(i+1)-f(i-1))
	! where now f(i) means f at location i+1/2. The halving is absorbed
	! in the final scaling.
	!   E-component interpolations:
	
	f=curx(l,1,1)+curx(l-ix,1,1)+dx*(curx(l+ix,1,1)-curx(l-ix,1,1))
	f=f+dy*(curx(l+iy,1,1)+curx(l-ix+iy,1,1)+dx*(curx(l+ix+iy,1,1) &
	-curx(l-ix+iy,1,1))-f)
	g=curx(l+iz,1,1)+curx(l-ix+iz,1,1)+dx*(curx(l+ix+iz,1,1)-curx(l &
	-ix+iz,1,1))
	g=g+dy* (curx(l+iy+iz,1,1)+curx(l-ix+iy+iz,1,1)+dx*(curx(l+ix+iy &
	+iz,1,1) -curx(l-ix+iy+iz,1,1))-g)
	curx0=(f+dz*(g-f))*(.5)
	
	!   ------------
	f=cury(l,1,1)+cury(l-iy,1,1)+dy*(cury(l+iy,1,1)-cury(l-iy,1,1))
	f=f+dz*(cury(l+iz,1,1)+cury(l-iy+iz,1,1)+dy*(cury(l+iy+iz,1,1) &
	-cury(l-iy+iz,1,1))-f)
	g=cury(l+ix,1,1)+cury(l-iy+ix,1,1)+dy*(cury(l+iy+ix,1,1)-cury(l &
	-iy+ix,1,1))
	g=g+dz* (cury(l+iz+ix,1,1)+cury(l-iy+iz+ix,1,1)+dy*(cury(l+iy+iz &
	+ix,1,1) -cury(l-iy+iz+ix,1,1))-g)
	cury0=(f+dx*(g-f))*(.5)
	!   ------------
	f=curz(l,1,1)+curz(l-iz,1,1)+dz*(curz(l+iz,1,1)-curz(l-iz,1,1))
	f=f+dx*(curz(l+ix,1,1)+curz(l-iz+ix,1,1)+dz*(curz(l+iz+ix,1,1) &
	-curz(l-iz+ix,1,1))-f)
	g=curz(l+iy,1,1)+curz(l-iz+iy,1,1)+dz*(curz(l+iz+iy,1,1)-curz(l &
	-iz+iy,1,1))
	g=g+dx* (curz(l+ix+iy,1,1)+curz(l-iz+ix+iy,1,1)+dz*(curz(l+iz+ix &
	+iy,1,1) -curz(l-iz+ix+iy,1,1))-g)
	curz0=(f+dy*(g-f))*(.5)
	!   ---------

end subroutine interpolcurrent


!-------------------------------------------------------------------------------
! 						subroutine meanq_dens_cur					 
!																		
!
!  							
!-------------------------------------------------------------------------------

subroutine meanq_dens_cur()

	implicit none
	
	! local variables
	
	!save only density array
	real uloc, vloc, wloc, gamma
	integer ::count, uprank, dwnrank,nn0, i, j, k, lx1, lx2, ly1, ly2, &
	lz1, lz2, uptag, dwntag, comm,ierr,status(statsize)
	integer ::request(2), status1(statsize,2)
	real, allocatable :: buffer(:,:,:), bufferout(:,:,:)
	integer ::lfttag, rgttag, lftrank, rgtrank
	

	allocate(buffer(mx,my,2), bufferout(mx,my,2))
	
	if(debug .and. rank.eq.0) print *, rank,": ","in meanq", mx,my,mz

	uptag=100
	dwntag=200
	rgttag=uptag
	lfttag=dwntag
	
	comm=MPI_Comm_world
	
	maxhlf=maxptl/2
	curx=0.
	cury=0.
	
	do nn0=1,maxptl
		if(nn0.le.ions .or. (nn0.gt.maxhlf .and. nn0.le.maxhlf+lecs)) then
		if(debug .and. ions .eq. 0) print *,rank,": ions=0",ions, lecs, nn0              
			
			i=p(nn0)%x
			j=p(nn0)%y
			k=p(nn0)%z
			
			lz1=max(k-idz,1)
			lz2=min(k+idz,mz)
			
#ifdef twoD
				lz1=1
				lz2=1
#endif
			
			ly1=max(j-idy,1)
			ly2=min(j+idy,my)
			
			lx1=max(i-idx,1)
			lx2=min(i+idx,mx)
			
			do k=lz1,lz2
				do j=ly1,ly2
					do i=lx1,lx2
						!density
						curx(i,j,k)=curx(i,j,k)+real(splitratio)**(1-p(nn0) &
						%splitlev)*p(nn0)%ch
						if(nn0 .le. ions) cury(i,j,k)=cury(i,j,k) &
						+real(splitratio)**(1-p(nn0)%splitlev)*p(nn0)%ch
						
					enddo
				enddo
			enddo
			
		endif
	enddo                     !particle loop

	
#ifndef twoD
		if(debug .and. rank.eq.0)print *,rank,":","1"
		!      uprank=modulo(rank+1,size0)
		!      dwnrank=modulo(rank-1,size0)
		
		uprank=modulo((rank/sizey + 1),sizez)*sizey + modulo(rank,sizey)
		dwnrank=modulo((rank/sizey - 1),sizez)*sizey + modulo(rank,sizey) 
		
		count=2*mx*my
		
		if(debug .and. rank.eq.0)print *, rank,":", uprank,dwnrank,count,mz,uptag, &
		dwntag 
		!send up
		bufferout=curx(:,:,mz-2:mz-1)
		
		call MPI_SendRecv(bufferout,count,mpi_read,uprank,uptag, &
		buffer,count,mpi_read,dwnrank,uptag,comm &
		,status,ierr) 
		curx(:,:,3:4)=curx(:,:,3:4)+buffer
		buffer=0.
		
		!send down
		bufferout=curx(:,:,1:2)
		call MPI_SendRecv(bufferout,count,mpi_read,dwnrank,dwntag, &
		buffer,count,mpi_read,uprank,dwntag,comm &
		,status,ierr) 
		curx(:,:,mz-4:mz-3)=curx(:,:,mz-4:mz-3)+buffer
		
		if(debug .and. rank.eq.0)print *,rank,":", "sent & received nav" 
		!::::::::::::::::::::::::::::::::::::::::::::::::::::::
		!send up
		bufferout=cury(:,:,mz-2:mz-1)
		
		call MPI_SendRecv(bufferout,count,mpi_read,uprank,uptag, &
		buffer,count,mpi_read,dwnrank,uptag,comm &
		,status,ierr) 
		cury(:,:,3:4)=cury(:,:,3:4)+buffer
		buffer=0.
		
		!send down
		bufferout=cury(:,:,1:2)
		call MPI_SendRecv(bufferout,count,mpi_read,dwnrank,dwntag, &
		buffer,count,mpi_read,uprank,dwntag,comm &
		,status,ierr) 
		cury(:,:,mz-4:mz-3)=cury(:,:,mz-4:mz-3)+buffer
		
		if(debug .and. rank.eq.0)print *,rank,":", "sent & received navi" 
		
#endif
	
	!--------------------------------------------------------------
	! now send and receive left and right
	!--------------------------------------------------------------

	deallocate(buffer,bufferout)
	allocate(buffer(mx,2,mz), bufferout(mx,2,mz))
	
	rgtrank=(rank/sizey)*sizey + modulo(rank+1,sizey) 
	lftrank=(rank/sizey)*sizey + modulo(rank-1,sizey) 
	
	count=2*mx*mz
	
	!send rgt
	bufferout=curx(:,my-2:my-1,:)
	
	call MPI_SendRecv(bufferout,count,mpi_read,rgtrank,rgttag, &
	buffer,count,mpi_read,lftrank,rgttag,comm &
	,status,ierr) 
	curx(:,3:4,:)=curx(:,3:4,:)+buffer
	buffer=0.
	
	!send lft
	bufferout=curx(:,1:2,:)
	call MPI_SendRecv(bufferout,count,mpi_read,lftrank,lfttag, &
	buffer,count,mpi_read,rgtrank,lfttag,comm &
	,status,ierr) 
	curx(:,my-4:my-3,:)=curx(:,my-4:my-3,:)+buffer
	
	!::::::::::::::::::::::::::::::::::::::::::::::::::::::
	!send rgt
	bufferout=cury(:,my-2:my-1,:)
	
	call MPI_SendRecv(bufferout,count,mpi_read,rgtrank,rgttag, &
	buffer,count,mpi_read,lftrank,rgttag,comm &
	,status,ierr) 
	cury(:,3:4,:)=cury(:,3:4,:)+buffer
	buffer=0.
	
	!send lft
	bufferout=cury(:,1:2,:)
	call MPI_SendRecv(bufferout,count,mpi_read,lftrank,lfttag, &
	buffer,count,mpi_read,rgtrank,lfttag,comm &
	,status,ierr) 
	cury(:,my-4:my-3,:)=cury(:,my-4:my-3,:)+buffer
	
	!:::::::::::::::::::::::::::::::::::::::::::::::::::::::::
	!normalize density
	do k=1,mz
		do j=1,my
			do i=1,mx
				
				lz1=max(k-idz,1)
				lz2=min(k+idz,mz)
				
				ly1=max(j-idy,1)
				ly2=min(j+idy,my)
				
				lx1=max(i-idx,1)
				lx2=min(i+idx,mx)
				
#ifdef twoD
					lz1=1
					lz2=1
#endif
				curx(i,j,k)=curx(i,j,k)/((lx2-lx1+1)*(ly2-ly1+1)*(lz2-lz1+1))
				cury(i,j,k)=cury(i,j,k)/((lx2-lx1+1)*(ly2-ly1+1)*(lz2-lz1+1))
				
			enddo
		enddo
	enddo
	deallocate(buffer,bufferout)
	
end subroutine meanq_dens_cur

#ifdef twoD
end module m_output
#else
end module m_output_3d
#endif
