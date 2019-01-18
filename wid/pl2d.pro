PRO pl2d, $
	data2d, $  ; data to plot
	x, $
	y, $
	vx=vx, $
	vy=vy, $
	minmax=minmaxv, $ ;val=minval, maxval=maxval, 
	glob=glob, $ ;min=globmin, globmax=globmax
	zrange=zrange, $
	xout=xout, yout=yout, $
	subxsampl=subxsampl, $
	subysampl=subysampl, $
	scale=scale, backgr=backgroundcolor, _extra=extra, np=np, reset=reset,$
        outxr=outxr, outyr=outyr,$
        xoffset1=xoffset1 

	common globals, gl
	if(n_elements(glob) ne 0 and n_elements(gl) lt 2) then gl=fltarr(2,20)
	if(n_elements(glob) ne 0 and n_elements(np) eq 0) then np=0
	if(n_elements(glob) ne 0 and n_elements(reset) ne 0) then gl(*,np)=0
	
	;value of position specified on command line overwrites position, calculated here
	if(n_elements(subxsampl) eq 0) then subxsampl=4.
	if(n_elements(subysampl) eq 0) then subysampl=4.
	if(n_elements(scale) eq 0) then scale=10. ; c/omp
	!p.charsize=1.5
	if(n_elements(backgr) eq 0) then backgroundcolor=0
	
	if(n_elements(vx) eq 0 or n_elements(vy) eq 0) then begin 
		vx=data2d*0
		vy=vx
	endif	

	if(n_elements(xoffset1) eq 0) then xoffset1=0
	
	sz=size(data2d)
	if(n_elements(x) eq 0) then begin 
		x=findgen(sz(1)) *subxsampl / scale 
		y=findgen(sz(2)) *subysampl / scale
	if(n_elements(xout) ne 0) then xout=x
	;help, "help xout", xout, n_elements(xout)
	if(n_elements(yout) ne 0) then yout=y
	endif	
		
	if(n_elements(xtitle) eq 0) then begin
		xtitle="x, c/omega_pe";
                xtitle=textoidl('x, [c/\omega_{pe}]')
	endif	
	if(n_elements(ytitle) eq 0) then begin
		ytitle="x, c/omega_pe";
                ytitle=textoidl('y, [c/\omega_{pe}]')
	endif	

;determine minmax bounds on data
	maxval=max(data2d)
	minval=min(data2d)
	minmaxv=[minval,maxval]
;	print, "minmax", minval, maxval
	if(n_elements(glob) ne 0) then begin 
		if(maxval gt gl(1,np)) then gl(1,np)=maxval
		if(minval lt gl(0,np)) then gl(0,np)=minval	
;		print , "glob=", gl(*,np)
	endif	

	if(n_elements(zrange) ne 0) then begin 
		minrange=zrange(0)
		maxrange=zrange(1)
	endif
	if(n_elements(zrange) eq 0) then begin 
		minrange=minval
		maxrange=maxval
		if(n_elements(glob) ne 0) then begin 
			zrange=[gl(0,np), gl(1,np)]
	;		print, "zran=",zrange
			minrange=gl(0,np)
			maxrange=gl(1,np)
		endif	
	endif	
;determine global minmax bounds on x,y?
	
        x=x-xoffset1

	xw0=0 ;!x.window(0)
	xw1=!d.x_vsize ;!x.window(1)
	yw0=0 ;!y.window(0)
	yw1=!d.y_vsize ;!y.window(1)

        if(!d.name eq "X") then begin
            pm=!p.multi
            plot, x, y, col=backgroundcolor,/nodata
            !p.multi=pm
            xw0=!x.window(0)
            xw1=!x.window(1)
            yw0=!y.window(0)
            yw1=!y.window(1)
        endif

;
;
;fixed width for the colorbar 100 cells
        
        sz=!d.x_vsize
        widthplotpix=((xw1-xw0)*sz)
        wid=min([.07,30./widthplotpix])
;        print, sz, wid, widthplotpix,xw0,xw1

;	pos=[xw0,yw0, xw0+(xw1-xw0)*.8, yw1]
	pos=[xw0,yw0, xw0+(xw1-xw0)*max([(1-3.5*wid),.8]), yw1]

        if(!d.name eq "PS") then begin
;            pos=[.1,.1,.8,.9]
        endif
        
        if(!d.name eq "X") then begin 
            plot_3d_vect, vx, vy, data2d, x, y, /ke,pos=pos,_extra=extra,$
              xtit=xtitle,ytit=ytitle,zran=zrange
        endif
        if(!d.name eq "PS" or !d.name eq "Z") then begin 
;            tvlct,r,g,b,/get
;            print,"color in pl2d", r

            if(!d.name eq "PS") then begin 
                backcol=254 & col=0
            endif
            if(!d.name eq "Z") then begin 
                backcol=0 & col=254
            endif
            print , "in pl2d, col=",col, "dname", !d.name
            plot_3d_vect, vx, vy, data2d, x, y, /ke,_extra=extra,$
              xtit=xtitle,ytit=ytitle,zran=zrange,/ps_ful , col=col, back=backcol
        endif
        
	outxr=[min(x),max(x)]
        outyr=[min(y),max(y)]

;	colorbar,/vert,minrange=minrange, maxrange=maxrange,pos=[xw0+(xw1-xw0)*.93,yw0,xw1,yw1],format='(F6.3)'
        
        pos=[xw0+(xw1-xw0)*(1-wid),yw0,xw1,yw1]
        if(!d.name eq "PS" or !d.name eq "Z") then begin
            pos=[.8,.1,.9,.9]
        endif
        
	if(!d.name eq "X") then begin 
colorbar,/vert,minrange=minrange, maxrange=maxrange,pos=pos,format='(F6.3)'
endif
	;print , "minrange=", minrange, maxrange
	;,charsize=chars
	;title=textoidl('Density^{1/2}'), 

end	
