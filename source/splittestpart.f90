! initial and final time and timestep can be modified
program splittest
  !goes through the test particle files and creates files sorted by the time step
  
  logical exst, twod
  character fnametst*22, fnamestep*15,fnamesrc*3
  integer rank,numfiles
  integer (kind=4) lap, num, proc, prevlap, lapstep, lapbeg, lapend
  real*8 x
  real y,z,u,v,w,gamma,bx0,by0,bz0,ex0,ey0,ez0
  
  twod=.false.
  lapstep=50 !the same as in trist.par.F
  lapbeg=100050
  lapend=200000
  !      fnamesrc="prt"
  
  !first find out how many files there are to work with
  
  rank=0
  exst=.true.
  do while (exst) 
     write(fnametst,"(a9,i3.3)") "test.ene.",rank
     inquire(file=fnametst,exist=exst)
     rank=rank+1
  enddo
  
  print *, "Detected ",rank, " files"
  
  numfiles=rank
  
  !now start going through the timesteps
  prevlap=0
  
  do num=0,numfiles-1
     exst=.false.
     write(fnametst,"(a9,i3.3)") "test.ene.",num
     open(unit=3, file=fnametst,form='formatted')
     print *, "opening ",fnametst
     
301  continue
     if (twod) then
        read(3,*,err=398,end=399)lap,num1,proc,x,y,u,v,gamma,&
             bz0,ex0,ey0
     else
        read(3,*,err=398,end=399)lap,num1,proc,x,y,z,u,v,w,gamma,&
             bx0,by0,bz0,ex0,ey0,ez0
     endif
     
     !            if(modulo(lap,50) .eq. 0 .and. lap .ge. 8000 .and. lap .lt. &
     !               55000) then
     
     if(modulo(lap,lapstep).eq.0.and.lap.ge.lapbeg .and. lap .le. &
                   lapend) then
        !<50000 for 2D case 
        write(fnamestep,"(a9,i6.6)") "test.spl.",lap
        
        inquire(file=fnamestep,exist=exst)
        
        if(lap .ne. prevlap) then 
           print *, fnametst, lap
           if(exst) close(22)
           
           if(num .eq. 0) then
              open(unit=22,file=fnamestep,form='formatted')
              exst=.true.
           else
              inquire(file=fnamestep,exist=exst)
              if(exst) then 
                 open(unit=22,file=fnamestep,status='old' &
                                        ,position='APPEND')
              else
                 open(unit=22,file=fnamestep,form='formatted')
                 exst=.true.
              endif
           endif
        endif
        
        if(exst) then 
           if (twod) then
              write(22,fmt="(i7, i12,i5,2(F10.3,' '), &
                   6(E15.5,' '))")lap,num1,proc, &
                   x,y,u,v,gamma,bz0,ex0,ey0
           else
              write(22,fmt="(i7, i12,i5,3(F10.3,' '), &
                   10(E15.5,' '))")lap,num1,proc, &
                   x,y,z,u,v,w,gamma,bx0,by0,bz0,ex0,ey0,ez0
           endif
        endif
        
        prevlap=lap
     endif
     
     goto 301
398  print *,"error reading the test.ene. file"
399  continue
     
  enddo !do num=0,numfiles-1
  
  
end program splittest
