!
! Tristan main file
!
! Calls initialization routines and the main loop only.
!
!
!

program tristan


#ifdef twoD 

	use m_globaldata
	use m_initialize	
	use m_tris
	
#else

	use m_globaldata_3d
	use m_initialize_3d	
	use m_tris_3d

#endif


	implicit none 

	! Initialization: read input file, initialize data structures, allocate memory

	call initialize()
	
	call mainloop() 	! Tristan main loop

	call finalize()		! house-keeping (finalizing MPI, etc) 

	stop
	
end program tristan
