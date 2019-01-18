!
! Communications module
!
! Holds the communications (MPI) related data structures and procedures 
! 
!
!

#ifdef twoD 

module m_communications

	use m_system
	use m_aux
	use m_globaldata
	use m_inputparser
	
#else

module m_communications_3d

	use m_system_3d
	use m_aux_3d
	use m_globaldata_3d
	use m_inputparser_3d

#endif


	implicit none
	
	include "mpif.h"
	
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

	integer :: rank, size0, sizey,sizez,ierr,statsize
	integer :: id1, id2, id3, id4, mpi_read, dst, mpi_double
	integer :: particletype
	
	real(dprec), dimension(20) :: tm

!-------------------------------------------------------------------------------
!	INTERFACE DECLARATIONS
!-------------------------------------------------------------------------------

	interface init_communications
		module procedure init_communications
	end interface init_communications
	
	interface timer
		module procedure timer
	end interface timer


!-------------------------------------------------------------------------------
!	PUBLIC MODIFIERS
!-------------------------------------------------------------------------------

	! public functions (used in m_tris)
	
	public :: print_timers, init_communications, initialize_random_seed, timer, read_input_communications

	! public variables

	public :: rank, size0, sizey, sizez, statsize, mpi_read, mpi_double,MPI_COMM_WORLD, mpi_wtime, mpi_integer,&
			  mpi_sum, MPI_STATUS_SIZE, mpi_integer8, MPI_MAX, MPI_MIN, MPI_INFO_NULL, mpi_real,  &
			  particletype

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

subroutine read_input_communications()

	implicit none
	
	call inputpar_geti_def("node_configuration", "sizey", 1, sizey)

end subroutine read_input_communications



!-------------------------------------------------------------------------------
! 						subroutine init_communications					 
!																		
! Calls MPI init, and creates the MPI data structures for communication
! 							
!-------------------------------------------------------------------------------

subroutine init_communications()

	implicit none
	
	integer, dimension(0:2) :: oldtypes, blockcounts, offsets
	integer :: extent
	
	
	mpi_read=MPI_REAL 
	mpi_double=MPI_DOUBLE_PRECISION
	size0=1

	call MPI_Init(ierr)
	call MPI_Comm_rank(MPI_Comm_world, rank, ierr)
	call MPI_Comm_size(MPI_Comm_world, size0, ierr)

	statsize=MPI_STATUS_SIZE

	!set up description of the particle type fields
	!for double field

	offsets(0)=0
	oldtypes(0)=MPI_REAL8
	blockcounts(0)=1

	!for real fields

	call MPI_TYPE_EXTENT(MPI_REAL8,extent,ierr)
	offsets(1)=extent
	oldtypes(1)= MPI_REAL
	blockcounts(1)=7


	!for integer ::fields

	call MPI_TYPE_EXTENT(MPI_REAL,extent,ierr)
	offsets(2)=7*extent+offsets(1)
	oldtypes(2)=MPI_INTEGER
	blockcounts(2)=2+1

	call MPI_TYPE_STRUCT(3,blockcounts,offsets,oldtypes,particletype,ierr)

	call MPI_TYPE_COMMIT(particletype, ierr)

	sizez=size0/sizey
	
#ifdef twoD
	sizez=1
#endif
	
	if(sizez*sizey .ne. size0) then
		print *, rank,":", "Error: sizex*sizey ne size0","size0=",size0,"sizez=",sizez,"sizey=",sizey
	endif


end subroutine init_communications



!-------------------------------------------------------------------------------
! 						subroutine timer					 
!																		
! Calls the mpi timer to time the execution of the code
! 							
!-------------------------------------------------------------------------------

subroutine timer(i, tmstop)

	implicit none
	
	! dummy variables
	
	integer, intent(in) :: i
	logical, intent(in), optional :: tmstop
	

	if (rank == 0) then
		if (present(tmstop)) then	! stop the timer
			tm(i)=mpi_wtime()-tm(i)
		else						! start the timer
			tm(i)=mpi_wtime()
		endif
	endif
	
end subroutine timer



!-------------------------------------------------------------------------------
! 						subroutine initialize_random_seed					 
!																		
! Initializes the random seed for the random number generator 
!							
!-------------------------------------------------------------------------------

subroutine initialize_random_seed()

	implicit none

	dseed=123457.D0        
	dseed=dseed+rank

end subroutine initialize_random_seed



!-------------------------------------------------------------------------------
! 						subroutine print_timers					 
!																		
! Prints the timers
! 							
!-------------------------------------------------------------------------------

subroutine print_timers()

	implicit none
	
	if(rank.eq.0) then
		write(*,*) "Timers for this step ----------------------------------"
		write(*,*) "Field solve -->",real(tm(2)+tm(4)+tm(9),4),"   Mover -->",real(tm(3),4)
		write(*,*) "Deposit -->",real(tm(5),4),"   Particle exchange -->",real(tm(6),4)
		write(*,*) "Current Exchange -->",real(tm(7),4),"   Filter -->",real(tm(8),4)
		write(*,*) "Inject -->",real(tm(10),4),"   Moving window -->",real(tm(11),4)
		write(*,*) "Reorder -->",real(tm(12),4),"   Total time -->",real(tm(1),4)
		write(*,*) "------------------------------------------------------"
	endif

end subroutine print_timers


#ifdef twoD
end module m_communications
#else
end module m_communications_3d
#endif
