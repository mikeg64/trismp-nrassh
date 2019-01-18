

!
! Particle module
!
! Includes the particle data structures and the routines for depositing
! and moving the particles
!
!

#ifdef twoD 

module m_particles_movedeposit

	use m_globaldata
	use m_system
	use m_aux
	use m_communications
	use m_fields
	use m_inputparser
	use m_fparser
	use m_particles
        use m_user
#else

module m_particles_movedeposit_3d

	use m_globaldata_3d
	use m_system_3d
	use m_aux_3d
	use m_communications_3d
	use m_fields_3d
	use m_inputparser_3d
	use m_fparser_3d
        use m_particles_3d
        use m_user_3d
#endif


	
	implicit none
        
!-------------------------------------------------------------------------------
!	INTERFACE DECLARATIONS
!-------------------------------------------------------------------------------
	
!-------------------------------------------------------------------------------
!	PUBLIC MODIFIERS
!-------------------------------------------------------------------------------

	! public functions
	
	public :: deposit_particles, move_particles

        contains
!-------------------------------------------------------------------------------
! 						subroutine move_particles()					 
!																		
! Pushes the particle's velocity and positions in time
!
!-------------------------------------------------------------------------------

subroutine move_particles()

	implicit none

		call mover(1,ions,qmi)					! ions
		call mover(1+maxhlf,lecs+maxhlf,qme)	! electrons
	
	if(debug) print *,rank,": done mover", "step=", lap

end subroutine move_particles



!-------------------------------------------------------------------------------
! 						subroutine mover()					 
!																		
! 
!
!-------------------------------------------------------------------------------

subroutine mover(n1,n2,qm)

	implicit none

	! dummy variables
	
	integer :: n2, n1
	real(sprec) :: qm

	! local variables

	integer :: npr, l, n, i, j, k
	real dx, dy, dz, f, g, ex0, ey0, ez0, bx0, by0, bz0
	real u0,v0,w0,u1,v1,w1, cinv, g1, corrqm
	real qm0
!, xglob, yglob, gammawall, betawall, gamma, walloc
!	real vdriftx, vdrifty, gdrift, decrem

!	integer ::yes_gammacut
!	real gammacut
#ifdef vay
	real ustar,sig,tx,ty,tz,vx0,vy0,vz0
#endif
	real bx_ext, by_ext, bz_ext, ex_ext, ey_ext, ez_ext
!	real pcosth, pphi, psinth, v0t, ut1, vt1, wt1, gam
!	real ptx, pty, ptz

	cinv=1./c	
	qm0=qm

	do n=n1,n2
			
		npr=n
		i=aint(p(npr)%x)
		dx=p(npr)%x-i
		j=p(npr)%y
		dy=p(npr)%y-j
		k=p(npr)%z
		dz=p(npr)%z-k
		
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
		
		ex0=(f+dz*(g-f))*(.25*qm)
		
		!   ------------
		f=ey(l,1,1)+ey(l-iy,1,1)+dy*(ey(l+iy,1,1)-ey(l-iy,1,1))
		f=f+dz*(ey(l+iz,1,1)+ey(l-iy+iz,1,1)+dy*(ey(l+iy+iz,1,1)-ey(l-iy &
		+iz,1,1))-f)
		g=ey(l+ix,1,1)+ey(l-iy+ix,1,1)+dy*(ey(l+iy+ix,1,1)-ey(l-iy+ix,1 &
		,1))
		g=g+dz* &
		(ey(l+iz+ix,1,1)+ey(l-iy+iz+ix,1,1)+dy*(ey(l+iy+iz+ix,1,1) &
		-ey(l-iy+iz+ix,1,1))-g)
		
		ey0=(f+dx*(g-f))*(.25*qm)
		
		!   ------------
		f=ez(l,1,1)+ez(l-iz,1,1)+dz*(ez(l+iz,1,1)-ez(l-iz,1,1))
		f=f+dx*(ez(l+ix,1,1)+ez(l-iz+ix,1,1)+dz*(ez(l+iz+ix,1,1)-ez(l-iz &
		+ix,1,1))-f)
		g=ez(l+iy,1,1)+ez(l-iz+iy,1,1)+dz*(ez(l+iz+iy,1,1)-ez(l-iz+iy,1 &
		,1))
		g=g+dx* &
		(ez(l+ix+iy,1,1)+ez(l-iz+ix+iy,1,1)+dz*(ez(l+iz+ix+iy,1,1) &
		-ez(l-iz+ix+iy,1,1))-g)
		
		ez0=(f+dy*(g-f))*(.25*qm)
		
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
		
		bx0=(f+dx*(g-f))*(.125*qm*cinv)
		
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
		
		by0=(f+dy*(g-f))*(.125*qm*cinv)
		
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
		
		bz0=(f+dz*(g-f))*(.125*qm*cinv)

		if(external_fields) then
		
                   call get_external_fields(real(p(npr)%x,sprec),p(npr)%y,&
		   p(npr)%z,ex_ext,ey_ext,ez_ext,bx_ext,by_ext,bz_ext)
                   
		   bx0=bx0+bx_ext*0.5*qm*cinv
                   by0=by0+by_ext*0.5*qm*cinv
                   bz0=bz0+bz_ext*0.5*qm*cinv
                   ex0=ex0+ex_ext*0.5*qm
                   ey0=ey0+ey_ext*0.5*qm
                   ez0=ez0+ez_ext*0.5*qm

		endif


		!   First half electric acceleration, with relativity's gamma:		
#ifdef vay
			!Use Vay 2008 particle mover
			g=1./sqrt(1.+p(npr)%u**2+p(npr)%v**2+p(npr)%w**2) !reciprocal of the Lorentz factor
			vx0=c*p(npr)%u*g !3-velocity of the particle
			vy0=c*p(npr)%v*g
			vz0=c*p(npr)%w*g
			
			u1=c*p(npr)%u+2.*ex0+vy0*bz0-vz0*by0 !uprime, taking into account 
                                                             !that cinv is already incorporated within B
			v1=c*p(npr)%v+2.*ey0+vz0*bx0-vx0*bz0
			w1=c*p(npr)%w+2.*ez0+vx0*by0-vy0*bx0

				!Lorentz factor for uprime			

			ustar=cinv*(u1*bx0+v1*by0+w1*bz0)
			sig=cinv*cinv*(c**2+u1**2+v1**2+w1**2)-(bx0**2+by0**2+bz0**2)
			g=1./sqrt(0.5*(sig+sqrt(sig**2+4.*(bx0**2+by0**2+bz0**2+ustar**2))))
			tx=bx0*g
			ty=by0*g
			tz=bz0*g
			f=1./(1.+tx**2+ty**2+tz**2)
			
			u0=f*(u1+(u1*tx+v1*ty+w1*tz)*tx+v1*tz-w1*ty)
			v0=f*(v1+(u1*tx+v1*ty+w1*tz)*ty+w1*tx-u1*tz)
			w0=f*(w1+(u1*tx+v1*ty+w1*tz)*tz+u1*ty-v1*tx)				
#else
			!Use Boris algorithm
			!   First half electric acceleration, with Lorentz gamma:
			u0=c*p(npr)%u+ex0
			v0=c*p(npr)%v+ey0
			w0=c*p(npr)%w+ez0
			!   First half magnetic rotation, with Lorentz gamma:
			g=c/sqrt(c**2+u0**2+v0**2+w0**2)
			bx0=g*bx0
			by0=g*by0
			bz0=g*bz0
			
			f=2./(1.+bx0*bx0+by0*by0+bz0*bz0)
			u1=(u0+v0*bz0-w0*by0)*f
			v1=(v0+w0*bx0-u0*bz0)*f
			w1=(w0+u0*by0-v0*bx0)*f
			!   Second half mag. rot'n &   el. acc'n:
			u0=u0+v1*bz0-w1*by0+ex0 
			v0=v0+w1*bx0-u1*bz0+ey0 
			w0=w0+u1*by0-v1*bx0+ez0 
#endif
			!   Get normalized 4-velocity:
			p(npr)%u=u0*cinv
			p(npr)%v=v0*cinv
			p(npr)%w=w0*cinv
		
		!   Position advance:
			g=c/sqrt(c**2+u0**2+v0**2+w0**2)
			p(npr)%x=p(npr)%x + p(npr)%u*g*c
			p(npr)%y=p(npr)%y + p(npr)%v*g*c 
			p(npr)%z=p(npr)%z + p(npr)%w*g*c
		


	enddo

end subroutine mover

!-------------------------------------------------------------------------------
! 						subroutine deposit_particles()					 
!																		
! Deposits the particles on the grid
!
!-------------------------------------------------------------------------------

subroutine deposit_particles()

	implicit none
	
	! local variables
	
	integer ::ind1,n1,n
	logical in
	real(dprec) :: x0, perx
	real y0,z0,perz,pery, invgam
	
	LenIonOutUp=0
	LenIonOutDwn=0
	LenLecOutUp=0
	LenLecOutDwn=0
	LenIonInBlw=0
	LenIonInAbv=0
	LenLecInBlw=0
	LenLecInAbv=0
	
	LenIonOutLft=0
	LenIonOutRgt=0
	LenLecOutLft=0
	LenLecOutRgt=0
	LenIonInLft=0
	LenIonInRgt=0
	LenLecInLft=0
	LenLecInRgt=0
	
	nionout=0
	nlecout=0	
	
	!particles are in local coordinates
	n=1

	if (ions.gt.0) then
		52     continue
		n1=n !indp(n)

		invgam=1./sqrt(1+p(n1)%u**2+p(n1)%v**2+p(n1)%w**2)
		
		x0=p(n1)%x-p(n1)%u*invgam*c
		y0=p(n1)%y-p(n1)%v*invgam*c
		z0=p(n1)%z-p(n1)%w*invgam*c
		
		q=p(n1)%ch*real(splitratio)**(1.-real(p(n1)%splitlev))*qi
		
				!deposition of current, based on papers by Umeda
		if(x0<0 .or. y0<0 ) then !.or. lap.gt.13244) then
		write(*,'(A,I7,A,3(I5,A),3(F8.4,A))') "prb ",p(n1)%ind," ",p(n1)%proc," ",&
		lap," ",rank," ",p(n1)%x," ",x0," ",y0," ",z0," "
		endif
		
                call zigzag(p(n1)%x,p(n1)%y,p(n1)%z,x0,y0,z0,in)

		!
		if(periodicx.eq.1) then
			perx=sign(real(.5*(mx-5.),8),p(n1)%x-3.)+sign(real(.5*(mx-5.) &
			,8),(p(n1)%x)-mx+2.) 
			p(n1)%x=p(n1)%x-perx
		endif
		if(periodicx .eq. 0 ) then
               
			in=(p(n1)%x.gt.x1in).and.(p(n1)%x.lt.x2in)

!			if(wall)then
!				in=((p(n1)%x.gt.leftwall-10).and.(p(n1)%x.le.xinject2))
!			else
!				in=((p(n1)%x.gt.3.0).and.(p(n1)%x.le.xinject2))
!			endif
		endif
		
		
		!        x0=x0-per   
		
		pery=sign(.5*(my-5.),p(n1)%y-3.)+sign(.5*(my-5.),p(n1)%y-my+2.)
		if(pery .ne. 0 .and. .not. in) pery=0
		if(pery .lt. 0 .and. modulo(rank,sizey).eq.0  .and. in) then
			pery=sign(.5*(mylast-5.),p(n1)%y-3.)+sign(.5*(mylast-5.),p(n1) &
			%y-mylast +2.) !put it at the top of last rank
		endif
		if(pery .lt. 0 .and. modulo(rank,sizey).eq.sizey-1 .and. in) then
			pery=sign(.5*(myall-5.),p(n1)%y-3.)+sign(.5*(myall-5.),p(n1)%y &
			-myall +2.) !put it at the top of the size-2
		endif
		if(pery .ne. 0 .and. in ) in=.false.
		
		p(n1)%y=p(n1)%y-pery
		
		!
#ifndef twoD
			perz=sign(.5*(mz-5.),p(n1)%z-3.)+sign(.5*(mz-5.),p(n1)%z-mz +2.)
			
			if(perz .ne. 0 .and. .not. in  ) perz=0
			if(perz .lt. 0 .and. rank/sizey.eq.0  .and. in) then
				perz=sign(.5*(mzlast-5.),p(n1)%z-3.)+sign(.5*(mzlast-5.),p(n1) &
				%z-mzlast +2.) !put it at the top of last rank
			endif
			if(perz .lt. 0 .and. rank/sizey.eq.sizez-1  .and. in) then
				perz=sign(.5*(mzall-5.),p(n1)%z-3.)+sign(.5*(mzall-5.),p(n1)%z &
				-mzall +2.) !put it at the top of the size-2
			endif
			if(perz .ne. 0 .and. in ) in=.false.
#else
			perz=0
			perz=sign(.5*(6-5),p(n1)%z-3.)+sign(.5*(6-5.),p(n1)%z-6 +2.)
			p(n1)%z=p(n1)%z-perz
			perz=0
#endif
		
		p(n1)%z=p(n1)%z-perz
		
		!        z0=z0-per   
		
		if(in) go to 58
	
		nionout=nionout+1
			
		!check if the paricle is going to another proc
		
		if(perz .lt. 0) then
			!this particle is going to the processor below
			LenIonOutDwn=LenIonOutDwn+1
			poutdwn(LenIonOutDwn)=p(n1)           
		endif
	
		if(perz .gt. 0) then
			!this particle is going to the processor above
			LenIonOutUp=LenIonOutUp+1
			poutup(LenIonOutUp)=p(n1)
		endif
		
		if(pery .lt. 0) then
			!this particle is going to the processor on the left
			LenIonOutLft=LenIonOutLft+1
			poutlft(LenIonOutLft)=p(n1)
		endif
		if(pery .gt. 0) then
			!this particle is going to the processor on the right
			LenIonOutRgt=LenIonOutRgt+1
			poutrgt(LenIonOutRgt)=p(n1)
		endif
		
		!  Replace by the ion from the top of the stack:
		
		p(n1)=p(ions)
		
		ions=ions-1
		n=n-1
		58	n=n+1
	
		if(n.le.ions)go to 52
	
	endif !if ions .gt. 0
	
	if(debug) print *, rank,":","deposited ions","step=",lap 
	
	n=maxhlf+1
	
	if(lecs.gt.0) then
		53      continue
		n1=n !indp(n)

		invgam=1./sqrt(1+p(n1)%u**2+p(n1)%v**2+p(n1)%w**2)
		
		x0=p(n1)%x-p(n1)%u*invgam*c
		y0=p(n1)%y-p(n1)%v*invgam*c
		z0=p(n1)%z-p(n1)%w*invgam*c
		
		q=p(n1)%ch*real(splitratio)**(1.-real(p(n1)%splitlev))*qe
		
		if(x0<0 .or. y0<0) then ! .or. lap.gt.13244) then
		write(*,'(A,I7,A,3(I5,A),4(F8.4,A))') "prb ",p(n1)%ind," ",p(n1)%proc," ",&
		lap," ",rank," ",p(n1)%x," ",x0," ",y0," ",z0," "
		endif

		call zigzag(p(n1)%x,p(n1)%y,p(n1)%z,x0,y0,z0,in)
	
		
		if(periodicx.eq.1) then
			perx=sign(real(.5*(mx-5.),8),real(p(n1)%x-3.,8))+sign(real(.5 &
			*(mx-5.),8),(p(n1)%x)-mx+2.)
			p(n1)%x=p(n1)%x-perx
		endif
		if(periodicx .eq. 0 ) then

			in=(p(n1)%x.gt.x1in).and.(p(n1)%x.lt.x2in)

!			in=(p(n1)%x.gt.leftwall-10).and.(p(n1)%x.lt.mx-2.0)
!		
!			if(wall)then
!				in=((p(n1)%x.gt.leftwall-10).and.(p(n1)%x.le.xinject2))
!			else
!				in=((p(n1)%x.gt.3.0).and.(p(n1)%x.le.xinject2))
!			endif
		endif
		
			
		pery=sign(.5*(my-5.),p(n1)%y-3.)+sign(.5*(my-5.),p(n1)%y-my+2.)
		if(pery .ne. 0 .and. .not. in) pery=0

		if(pery .lt. 0 .and. modulo(rank,sizey).eq.0  .and. in) then
			pery=sign(.5*(mylast-5.),p(n1)%y-3.)+sign(.5*(mylast-5.),p(n1) &
			%y-mylast +2.) !put it at the top of last rank
		endif
		if(pery .lt. 0 .and. modulo(rank,sizey).eq.sizey-1 .and. in) then
			pery=sign(.5*(myall-5.),p(n1)%y-3.)+sign(.5*(myall-5.),p(n1)%y &
			-myall +2.) !put it at the top of the size-2
		endif

		if(pery .ne. 0 .and. in ) in=.false.
		
		p(n1)%y=p(n1)%y-pery
		
#ifndef twoD
			perz=sign(.5*(mz-5.),p(n1)%z-3.)+sign(.5*(mz-5.),p(n1)%z-mz +2.)
			
			if(perz .ne. 0 .and. .not. in  ) perz=0
			if(perz .lt. 0 .and. rank/sizey.eq.0  .and. in) then
				perz=sign(.5*(mzlast-5.),p(n1)%z-3.)+sign(.5*(mzlast-5.),p(n1) &
				%z-mzlast +2.) !put it at the top of last rank
			endif
			if(perz .lt. 0 .and. rank/sizey.eq.sizez-1  .and. in) then
				perz=sign(.5*(mzall-5.),p(n1)%z-3.)+sign(.5*(mzall-5.),p(n1)%z &
				-mzall +2.) !put it at the top of the size-2
			endif
			if(perz .ne. 0 .and. in ) in=.false.
#else
			perz=0			
			perz=sign(.5*(6-5),p(n1)%z-3.)+sign(.5*(6-5.),p(n1)%z-6 +2.)
			p(n1)%z=p(n1)%z-perz
			perz=0
#endif
		
		p(n1)%z=p(n1)%z-perz
		
		if(in) go to 59
		nlecout=nlecout+1
		
		if(perz .lt. 0) then
			!this particle is going to the processor below
			LenLecOutDwn=LenLecOutDwn+1
			poutdwn(LenIonOutDwn+LenLecOutDwn)=p(n1)
		endif

		if(perz .gt. 0) then
			!this particle is going to the processor above
			LenLecOutUp=LenLecOutUp+1
			poutup(LenIonOutUp+LenLecOutUp)=p(n1)
		endif
		
		if(pery .lt. 0) then
			!this particle is going to the processor on the left
			LenLecOutLft=LenLecOutLft+1
			poutlft(LenIonOutLft+LenLecOutLft)=p(n1)
		endif

		if(pery .gt. 0) then
			!this particle is going to the processor on the right
			LenLecOutRgt=LenLecOutRgt+1
			poutrgt(LenIonOutRgt+LenLecOutRgt)=p(n1)
		endif
		
		!  Replace by the electron from the top of the stack:
		
		p(n1)=p(maxhlf+lecs)
		
		lecs=lecs-1
		n=n-1
		59	n=n+1        
		if(n.le.maxhlf+lecs)go to 53       
		
		if(debug) print *, rank,":","deposited lecs","step=",lap 
		
	endif !if lecs .gt. 0
	
	nioneject=LenIonOutUp+LenIonOutDwn+LenIonOutLft+LenIonOutRgt
	
	nleceject=LenLecOutUp+LenLecOutDwn+LenLecOutLft+LenLecOutRgt


end subroutine deposit_particles


#ifdef twoD
end module m_particles_movedeposit
#else
end module m_particles_movedeposit_3d
#endif
