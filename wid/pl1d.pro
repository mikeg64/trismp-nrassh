PRO pl1d, $
	data1d, $  ; data to plot
	x, $
	minmax=minmaxv, $ ;val=minval, maxval=maxval, 
	glob=glob, $ ;min=globmin, globmax=globmax
	xout=xout, yout=yout, $
	subxsampl=subxsampl, $
	scale=scale, _extra=extra, backgr=backgroundcolor, np=np, reset=reset,$
        outxr=outxr, outyr=outyr, label=label,$
        xoffset1=xoffset1 
	

	common globals, gl
	if(n_elements(glob) ne 0 and n_elements(gl) lt 2) then gl=fltarr(2,20)
	if(n_elements(glob) ne 0 and n_elements(np) eq 0) then np=0
	if(n_elements(glob) ne 0 and n_elements(reset) ne 0) then gl(*,np)=0
	
	;value of position specified on command line overwrites position, calculated here
	if(n_elements(subxsampl) eq 0) then subxsampl=4.
	if(n_elements(scale) eq 0) then scale=10. ; c/omp
	!p.charsize=1.5
	if(n_elements(backgr) eq 0) then backgroundcolor=0
		
	sz=size(data1d)
	if(n_elements(x) eq 0) then begin
        x=findgen(sz(1)) *subxsampl / scale 
	if(n_elements(xout) ne 0) then xout=x
;	help, "help xout", xout, n_elements(xout)
	endif	
	
	if(n_elements(xtitle) eq 0) then begin
		xtitle="x, c/omega_pe";
                xtitle=textoidl('x, [c/\omega_{pe}]')
	endif	

	if(n_elements(xoffset1) eq 0) then xoffset1=0

;determine minmax bounds on data
	maxval=max(data1d)
	minval=min(data1d)
	minmaxv=[minval,maxval]
	;print, "minmax", minval, maxval
	if(n_elements(glob) ne 0) then begin 
;		if(maxval gt glob(1)) then glob(1)=maxval
;		if(minval lt glob(0)) then glob(0)=minval	

		if(maxval gt gl(1,np)) then gl(1,np)=maxval
		if(minval lt gl(0,np)) then gl(0,np)=minval	
;		print , "glob=", glob
	endif	

	if(n_elements(yrange) ne 0) then begin 
		minrange=yrange(0)
		maxrange=yrange(1)
	endif
	if(n_elements(yrange) eq 0) then begin 
		minrange=minval
		maxrange=maxval
		yrange=[minrange,maxrange]
		if(n_elements(glob) ne 0) then begin 
			minrange=gl(0,np)
			maxrange=gl(1,np)
			yrange=[minrange,maxrange]
		endif	
	endif	
;determine global minmax bounds on x,y?

        x=x-xoffset1
	
;	pm=!p.multi
;	plot, x, col=backgroundcolor,/nodata
;	!p.multi=pm
;	xw0=!x.window(0)
;	xw1=!x.window(1)
;	yw0=!y.window(0)
;	yw1=!y.window(1)


	xw0=0 ;!x.window(0)
	xw1=!d.x_vsize ;!x.window(1)
	yw0=0 ;!y.window(0)
	yw1=!d.y_vsize ;!y.window(1)

        if(!d.name eq "X") then begin
            pm=!p.multi
            plot, x, col=backgroundcolor,/nodata
            !p.multi=pm
            xw0=!x.window(0)
            xw1=!x.window(1)
            yw0=!y.window(0)
            yw1=!y.window(1)
        endif


;	pos=[xw0,yw0, xw0+(xw1-xw0)*.8, yw1]

        sz=!d.x_vsize
        widthplotpix=((xw1-xw0)*sz)
        wid=min([.07,30./widthplotpix])
        ;print, sz, wid, widthplotpix,xw0,xw1

;	pos=[xw0,yw0, xw0+(xw1-xw0)*.8, yw1]
	pos=[xw0,yw0, xw0+(xw1-xw0)*max([(1-3.5*wid),.8]), yw1]

        if(!d.name eq "X") then begin
            plot, x, data1d,  pos=pos,_extra=extra,$
              xtit=xtitle,ytit=ytitle,yran=yrange
        endif

        if(!d.name eq "PS" or !d.name eq "Z") then begin 
            plot, x, data1d, _extra=extra,xtit=xtitle,ytit=ytitle,yran=yrange
        endif

	outxr=[min(x),max(x)]

        if(n_elements(label) ne 0) then begin 
            xyouts,.9*xw1,.9*yw1,label,/normal
        endif

;	colorbar,/vert,minrange=minrange, maxrange=maxrange,pos=[xw0+(xw1-xw0)*.93,yw0,xw1,yw1]
;	print , "minrange=", minrange, maxrange
	;,charsize=chars
	;title=textoidl('Density^{1/2}'), 

end	
