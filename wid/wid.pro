; wid.pro - widget for tristen produced data
; Written by Anatoly Spitkovsky. 
; 290409 (Uri Keshet) - indented, fixed radial FFT: units, bins, include negative kx, 
; 300409 (Lorenzo) - fixed normalization of fftpwx and fftpwy; changed
;                    choice of plots (added pwx and pwy to radial)
; 300409 (Lorenzo) - changed reset for kx=0 or ky=0
; 300409 (Lorenzo) - slider_x and slider_y now used to select the
;                    particles for phase space; if plotting electrons
;                    and oneD=1, then momentum is divided by mime
; 300409 (Lorenzo) - fixed (useless) plotting twice for pl1d and threed=0 (my
;                    fault from the past); added partial range in y
;                    for pl1d (according to isoval); added choice
;                    (according to isoval) for plotting T_e/T_p
;                    .or. (T_e .and. T_p); added electric
;                    pseudo-potential
; 010509 (Lorenzo) - slab where pxp-pyp,.. are computed is defined via
;                    fft sliders (and ymin-ymax); ymin and ymax also act on the region
;                    where the FFT is computed

function fnumber, n
  allzeroes='000000000000000000'
  strvalue=strtrim(string(n),1) ;converts i to string and cuts leading zeroes
  fullstr=allzeroes+strvalue    ;concatenates i with bunch of zeroes
  lenfull=strlen(fullstr)       ;finds out total length 
  number=strmid(fullstr,lenfull-3) ;cuts out the last 3 digits
  return, number
end

pro wid_resize, event
  common basecom, baseid,baseplots,wLCol1,wLSliders,plotarr1idvert
  common widcom, winid, widid, nplots
  
  if n_elements(event) ne 0 then begin 
      xsize=event.x
      ysize=event.y
;    print , "xsize=",xsize, ysize
  endif
  
;determine the size of the buttons
  geom1=widget_info(wLCol1,/geometry)
  geom2=widget_info(wLSliders,/geometry)
  geom4=widget_info(plotarr1idvert,/geometry)
;current size of the graphics column
  geom3=widget_info(baseplots,/geometry)
  widget_control,baseplots,tlb_get_size=tlbsize
  
  newx=xsize-geom1.xsize-geom2.xsize-geom4.xsize
  
  for i=0,nplots-1 do begin 
      geomplot=widget_info(widid(i),/geometry)
      newy=geomplot.ysize*(ysize/geom3.ysize)
      widget_control, widid(i),draw_xsize=newx, draw_ysize=newy
  endfor
  
end

pro donothing
end

pro animarray, data, mpeg=mpeg

if not  keyword_set(mpeg) then mpeg=0
set_plot, 'x'

sz=size(data)
max=sz(3)
resx=sz(1)
resy=sz(2)

if mpeg then begin
mpegid=mpeg_open([resx,resy])
endif 

base = WIDGET_BASE(TITLE = 'Animation Widget')
animate = CW_ANIMATE(base, resx, resy, max)
WIDGET_CONTROL, /REALIZE, base
loadct, 4;5;4;39 ;39

FOR I=0,max-1 DO begin
CW_ANIMATE_LOAD, animate, FRAME=I, IMAGE=data[*,*,I]
if mpeg then begin
	mpeg_put,mpegid, image=data[*,*,I],frame=i,/color
endif
endfor

CW_ANIMATE_GETP, animate, pixmap_vect

CW_ANIMATE_RUN, animate

XMANAGER, 'CW_ANIMATE Demo', base, EVENT_HANDLER = 'EHANDLER', /no_block

if mpeg then begin
	mpeg_save,mpegid,filename='mov.mpg'
	print, "don't forget to rename mov.mpg"
	mpeg_close, mpegid
endif

end


pro wid_events, event
  common widcom, winid, widid, nplots
  common basecom, baseid, baseplots,wLCol1,wLSliders,plotarr1idvert
  common counter, count

;print, "event="
;help, event,/str
  if tag_names(event,/structure_name) eq 'WIDGET_BASE' then begin 
;    print, "in if loop in wid_events"
      count=count+1 & if count gt 20000 then count=0
      widget_control, widget_info(baseid,find_by_uname='slowX'),get_uvalue=uval
      if count mod (1+5*uval) eq 0 then begin 
          wid_resize,event
          replot_all
      endif
      return
  endif
  
  widget_control, event.id, get_uvalue=Uvalue
  uname=widget_info(event.id,/uname)
  
  CASE 1 of 
      (Uname eq "Replot"): BEGIN
          replot_all
          widget_control, widget_info(baseid,find_by_uname='SliderYMax'), get_value=ymx
          if(ymx eq 1) then begin 
              replot_all
          endif
      END
      (Uname eq "Forward"): BEGIN
          widget_control, widget_info(baseid,find_by_uname="TimeStepSlider"), get_value=tstep
          widget_control, widget_info(baseid,find_by_uname="TimeSlider"), get_value=tind
          tminmax=widget_info(widget_info(baseid,find_by_uname="TimeSlider"),/slider_min_max)
          widget_control, widget_info(baseid,find_by_uname="TimeSlider"), set_value=((tind+tstep) mod (tminmax(1)+1))
                                ; print, "tminmax=",tminmax, tind, tstep,((tind+tstep) mod (tminmax(1)+1))
          replot_all
      end
      (Uname eq "Backward"): BEGIN
          widget_control, widget_info(baseid,find_by_uname="TimeStepSlider"), get_value=tstep
          widget_control, widget_info(baseid,find_by_uname="TimeSlider"), get_value=tind
          tminmax=widget_info(widget_info(baseid,find_by_uname="TimeSlider"),/slider_min_max)
          newstep=tind-tstep
          if(newstep lt 1) then newstep=newstep+tminmax(1)
          widget_control, widget_info(baseid,find_by_uname="TimeSlider"), set_value=newstep
          replot_all
      end
        ;;;;;;;;;;;;;;;;;;;;;;;
      (Uname eq "SliderXMin") or (Uname eq "SliderXMax"): BEGIN
          refresh_FFTxsliderRange
      end
      (Uname eq "FFTX0") or (Uname eq "FFTDX"): begin
          widget_control, widget_info(baseid,find_by_uname='FFTX0'), get_value=fftx0str
          widget_control, widget_info(baseid,find_by_uname='FFTDX'), get_value=fftdxstr
          fftx0=convert_to_type(fftx0str,'FLOAT') & fftx0=fftx0(0)
          fftdx=convert_to_type(fftdxstr,'FLOAT') & fftdx=fftdx(0)
          xloc=-1.234           ; = unset
          for ii=0,nplots-1 do begin 
              boxtext=widget_info(widget_info(baseid,find_by_uname='DropMenu'+fnumber(ii)), /combobox_gettext)
              if(boxtext eq "spect") then widget_control, widget_info(baseid,find_by_uname='Contrast'+fnumber(ii)), get_value=xloc
          endfor
          for ii=0,nplots-1 do begin 
              boxtext=widget_info(widget_info(baseid,find_by_uname='DropMenu'+fnumber(ii)), /combobox_gettext)
              fntext=widget_info(widget_info(baseid,find_by_uname='fnMenu'+fnumber(ii)), /combobox_gettext)
              if(boxtext ne "spect" and fntext ne "fft") then begin
;                 and boxtext ne "pxp-pyp" and boxtext ne "pxe-pye" and boxtext ne "pxp-pzp" and $
;                 boxtext ne "pxe-pze" and boxtext ne "pyp-pzp" and boxtext ne "pye-pze") then begin 
                  if (xloc eq -1.234) then plot_window,ii,regionloc1=fftx0-0.5*fftdx,regionloc2=fftx0+0.5*fftdx else plot_window,ii,lineloc=xloc,regionloc1=fftx0-0.5*fftdx,regionloc2=fftx0+0.5*fftdx
              endif
          endfor
      end
      (Uname eq "Contrast000"): begin
          plot_window, 0
          i=0 & boxtext=widget_info(widget_info(baseid,find_by_uname='DropMenu'+fnumber(i)), /combobox_gettext)
          if(boxtext eq "spect") then begin
              widget_control, widget_info(baseid,find_by_uname='Contrast'+fnumber(i)), get_value=xloc
              for ii=0,nplots-1 do begin 
                  if(ii ne i) then begin 
                      boxtext=widget_info(widget_info(baseid,find_by_uname='DropMenu'+fnumber(ii)), /combobox_gettext)
                      fntext=widget_info(widget_info(baseid,find_by_uname='DropMenu'+fnumber(ii)), /combobox_gettext)
                      if(boxtext ne "spect") then begin 
                          if(fntext ne "fft" and $
                             boxtext ne "pxp-pyp" and boxtext ne "pxe-pye" and boxtext ne "pxp-pzp" and $
                             boxtext ne "pxe-pze" and boxtext ne "pyp-pzp" and boxtext ne "pye-pze") then $
                            plot_window,ii,lineloc=xloc else plot_window,ii
                          if(ii eq 0) then add_timestamp
                      endif
                  endif
              endfor
          endif
      end
      (Uname eq "Contrast001"): begin
          plot_window, 1
          i=1 & boxtext=widget_info(widget_info(baseid,find_by_uname='DropMenu'+fnumber(i)), /combobox_gettext)
          if(boxtext eq "spect") then begin
              widget_control, widget_info(baseid,find_by_uname='Contrast'+fnumber(i)), get_value=xloc 
              for ii=0,nplots-1 do begin 
                  if(ii ne i) then begin 
                      boxtext=widget_info(widget_info(baseid,find_by_uname='DropMenu'+fnumber(ii)), /combobox_gettext)
                      fntext=widget_info(widget_info(baseid,find_by_uname='DropMenu'+fnumber(ii)), /combobox_gettext)
                      if(boxtext ne "spect") then begin
                          if(fntext ne "fft" and $
                             boxtext ne "pxp-pyp" and boxtext ne "pxe-pye" and boxtext ne "pxp-pzp" and $
                             boxtext ne "pxe-pze" and boxtext ne "pyp-pzp" and boxtext ne "pye-pze") then $
                            plot_window,ii,lineloc=xloc else plot_window,ii 
                          if(ii eq 0) then add_timestamp
                      endif
                  endif
              endfor
          endif
      end
      (Uname eq "Contrast002"): begin
          plot_window, 2
          i=2 & boxtext=widget_info(widget_info(baseid,find_by_uname='DropMenu'+fnumber(i)), /combobox_gettext)
          if(boxtext eq "spect") then begin
              widget_control, widget_info(baseid,find_by_uname='Contrast'+fnumber(i)), get_value=xloc 
              for ii=0,nplots-1 do begin 
                  if(ii ne i) then begin 
                      boxtext=widget_info(widget_info(baseid,find_by_uname='DropMenu'+fnumber(ii)), /combobox_gettext)
                      fntext=widget_info(widget_info(baseid,find_by_uname='DropMenu'+fnumber(ii)), /combobox_gettext)
                      if(boxtext ne "spect") then begin 
                          if(fntext ne "fft" and $
                             boxtext ne "pxp-pyp" and boxtext ne "pxe-pye" and boxtext ne "pxp-pzp" and $
                             boxtext ne "pxe-pze" and boxtext ne "pyp-pzp" and boxtext ne "pye-pze") then $
                            plot_window,ii,lineloc=xloc else plot_window,ii 
                          if(ii eq 0) then add_timestamp
                      endif
                  endif
              endfor
          endif
          
      end
        ;;;;;;;;;;;;;;;;;;;;;;;
      (Uname eq "DropMenu000"): begin
          i=0
          boxtext=widget_info(widget_info(baseid,find_by_uname='DropMenu'+fnumber(i)), /combobox_gettext)
          widget_control, widget_info(baseid,find_by_uname='Contrast'+fnumber(i)), set_value=[1.,0,1]
          if(boxtext eq "eb") then widget_control, widget_info(baseid,find_by_uname='Contrast'+fnumber(i)), set_value=[.25,0,1]
          widget_control, widget_info(baseid,find_by_uname='Contrast'+fnumber(i)), get_value=contr            
          if(contr gt 1 or contr lt 1e-3) then widget_control, widget_info(baseid,find_by_uname='Contrast'+fnumber(i)), set_value=[1,0,1]
          if(boxtext eq "spect") then begin
              widget_control, widget_info(baseid,find_by_uname='T'),set_value=[0,-4,2]
              widget_control, widget_info(baseid,find_by_uname='Norm'),set_value=[0,-1,6]
          endif
          plot_window, 0
      end
      (Uname eq "DropMenu001"): begin
          i=1

          widget_control, widget_info(baseid,find_by_uname='Contrast'+fnumber(i)), set_value=[1.,0,1]
          boxtext=widget_info(widget_info(baseid,find_by_uname='DropMenu'+fnumber(i)), /combobox_gettext)
          if(boxtext eq "eb") then widget_control, widget_info(baseid,find_by_uname='Contrast'+fnumber(i)), set_value=[.25,0,1]
          widget_control, widget_info(baseid,find_by_uname='Contrast'+fnumber(i)), get_value=contr            
          if(contr gt 1 or contr lt 1e-3) then widget_control, widget_info(baseid,find_by_uname='Contrast'+fnumber(i)), set_value=[1,0,1]
          if(boxtext eq "spect") then begin
              widget_control, widget_info(baseid,find_by_uname='T'),set_value=[0,-4,2]
              widget_control, widget_info(baseid,find_by_uname='Norm'),set_value=[0,-1,6]
          endif
          plot_window, 1
      end
      (Uname eq "DropMenu002"): begin
          i=2
          boxtext=widget_info(widget_info(baseid,find_by_uname='DropMenu'+fnumber(i)), /combobox_gettext)
          widget_control, widget_info(baseid,find_by_uname='Contrast'+fnumber(i)), set_value=[1.,0,1]
          if(boxtext eq "eb") then widget_control, widget_info(baseid,find_by_uname='Contrast'+fnumber(i)), set_value=[.25,0,1]
          widget_control, widget_info(baseid,find_by_uname='Contrast'+fnumber(i)), get_value=contr            
          if(contr gt 1 or contr lt 1e-3) then widget_control, widget_info(baseid,find_by_uname='Contrast'+fnumber(i)), set_value=[1,0,1]
          if(boxtext eq "spect") then begin
              widget_control, widget_info(baseid,find_by_uname='T'),set_value=[0,-4,2]
              widget_control, widget_info(baseid,find_by_uname='Norm'),set_value=[0,-1,10]
              
                                ;
              goto, kkk
                                ;rename  iso button:
              chkid=widget_info(baseid, find_by_uname='basecheckbx'+fnumber(i))
              id=widget_info(baseid, find_by_uname='iso'+fnumber(i))
              
;                button=widget_button(chkid,Value='Log',uvalue=1,uname='logbut'+fnumber(i))
;                Widget_Control, button, Set_Button=1
              
;                id0=widget_info(chkid,/parent)
              id0=widget_info(baseid,find_by_uname='basevertsubpl'+fnumber(i))
              drawid=widget_info(baseid,find_by_uname='widdraw'+fnumber(i))
               ;;; widget_control,drawid,/destroy
              
;	widid(nplots) = WIDGET_DRAW(plotarr1id, RETAIN=2, XSIZE=950, YSIZE=200,uname='widdraw'+fnumber(i) );,scr_ysize=200,/scroll)
              widget_control,id0,map=0
              tempsl=cw_fslider(id0, Value=.1, Min=0, Max=1,uvalue='Tslider'+fnumber(i),uname='Tslider'+fnumber(i),title="Temp",xsize=70)
              normsl=cw_fslider(id0, Value=1, Min=1, Max=7,uvalue='Nslider'+fnumber(i),uname='Nslider'+fnumber(i),title="Norm",xsize=70)
;	widid(nplots) = WIDGET_DRAW(, RETAIN=2, XSIZE=950, YSIZE=200,uname='widdraw'+fnumber(nplots) );,scr_ysize=200,/scroll)
              widget_control,id0,/realize
              widget_control,id0,map=1
                                ;create fit button
kkk:
          endif
          plot_window, 2
      end
      
      (Uname eq "fnMenu000") or (Uname eq "fnMenu001") or (Uname eq "fnMenu002"): begin
          fft_on=0
          for i=0,nplots-1 do begin
              fntext=widget_info(widget_info(baseid,find_by_uname='DropMenu'+fnumber(i)), /combobox_gettext)
              if(fntext eq "fft") then fft_on=1
          endfor
          if (fft_on eq 1) then begin
              refresh_FFTxsliderRange
              widget_control, widget_info(baseid,find_by_uname='FFTX0'), get_value=fftx0str
              widget_control, widget_info(baseid,find_by_uname='FFTDX'), get_value=fftdxstr
              fftx0=convert_to_type(fftx0str,'FLOAT') & fftx0=fftx0(0)
              fftdx=convert_to_type(fftdxstr,'FLOAT') & fftdx=fftdx(0)
              xloc=-1.234       ;meaningless
              for ii=0,nplots-1 do begin 
                  boxtext=widget_info(widget_info(baseid,find_by_uname='DropMenu'+fnumber(ii)), /combobox_gettext)
                  if(boxtext eq "spect") then widget_control, widget_info(baseid,find_by_uname='Contrast'+fnumber(ii)), get_value=xloc
              endfor
              for ii=0,nplots-1 do begin 
                  boxtext=widget_info(widget_info(baseid,find_by_uname='DropMenu'+fnumber(ii)), /combobox_gettext)
                  fntext=widget_info(widget_info(baseid,find_by_uname='fnMenu'+fnumber(ii)), /combobox_gettext)
                  if(boxtext ne "spect" and fntext ne "fft") then begin
; and $
;                     boxtext ne "pxp-pyp" and boxtext ne "pxe-pye" and boxtext ne "pxp-pzp" and $
;                     boxtext ne "pxe-pze" and boxtext ne "pyp-pzp" and boxtext ne "pye-pze") then begin
                      if (xloc eq -1.234) then begin 
                          plot_window,ii,regionloc1=fftx0-0.5*fftdx,regionloc2=fftx0+0.5*fftdx
                      endif else begin 
                          plot_window,ii,lineloc=xloc,regionloc1=fftx0-0.5*fftdx,regionloc2=fftx0+0.5*fftdx
                      endelse
                  endif
              endfor
          endif
      end
      
      
        ;;;;;;;;;;;;;;;;;;;;;;;
      (Uname eq "iso000"): begin
          widget_control, widget_info(baseid,find_by_uname='iso'+fnumber(0)),get_uvalue=uval
          if(uval eq 1) then uval=0 else uval=1 
          widget_control, widget_info(baseid,find_by_uname='iso'+fnumber(0)),set_uvalue=uval
          plot_window,0
      end
      (Uname eq "iso001"): begin
          widget_control, widget_info(baseid,find_by_uname='iso'+fnumber(1)),get_uvalue=uval
          if(uval eq 1) then uval=0 else uval=1 
          widget_control, widget_info(baseid,find_by_uname='iso'+fnumber(1)),set_uvalue=uval
          plot_window,1
      end
      (Uname eq "iso002"): begin
          widget_control, widget_info(baseid,find_by_uname='iso'+fnumber(2)),get_uvalue=uval
          if(uval eq 1) then uval=0 else uval=1 
          widget_control, widget_info(baseid,find_by_uname='iso'+fnumber(2)),set_uvalue=uval
          plot_window,2
      end
        ;;;;;;;;;;;;;;;;;;;;;
      (Uname eq "oneD000"): begin
          widget_control, widget_info(baseid,find_by_uname='oneD'+fnumber(0)),get_uvalue=uval
          if(uval eq 1) then uval=0 else uval=1 
          widget_control, widget_info(baseid,find_by_uname='oneD'+fnumber(0)),set_uvalue=uval
          plot_window,0
      end
      (Uname eq "oneD001"): begin
          widget_control, widget_info(baseid,find_by_uname='oneD'+fnumber(1)),get_uvalue=uval
          if(uval eq 1) then uval=0 else uval=1 
          widget_control, widget_info(baseid,find_by_uname='oneD'+fnumber(1)),set_uvalue=uval
          plot_window,1
      end
      (Uname eq "oneD002"): begin
          widget_control, widget_info(baseid,find_by_uname='oneD'+fnumber(2)),get_uvalue=uval
          if(uval eq 1) then uval=0 else uval=1 
          widget_control, widget_info(baseid,find_by_uname='oneD'+fnumber(2)),set_uvalue=uval
          plot_window,2
      end
        ;;;;;;;;;;;;;;;;;;;;;;;
      (Uname eq "slowX"): begin
          widget_control, widget_info(baseid,find_by_uname='slowX'),get_uvalue=uval
          if(uval eq 1) then uval=0 else uval=1
          widget_control, widget_info(baseid,find_by_uname='slowX'),set_uvalue=uval
      end
      (Uname eq "fit2D"): begin
          widget_control, widget_info(baseid,find_by_uname='fit2D'),get_uvalue=uval
          if(uval eq 1) then uval=0 else uval=1
          widget_control, widget_info(baseid,find_by_uname='fit2D'),set_uvalue=uval
      end
      (Uname eq "shoffset"): begin
          widget_control, widget_info(baseid,find_by_uname='shoffset'),get_uvalue=uval
          if(uval eq 1) then uval=0 else uval=1
          widget_control, widget_info(baseid,find_by_uname='shoffset'),set_uvalue=uval
          if(uval eq 0) then clear_offsets
          refresh_xslider
          refresh_FFTxslider
      end
            ;;;;;;;;;
      (Uname eq "T"): begin
          for i=0,2 do begin 
              boxtext=widget_info(widget_info(baseid,find_by_uname='DropMenu'+fnumber(i)), /combobox_gettext)
              if(boxtext eq "spect") then plot_window,i
          endfor
      end

      
      (Uname eq "Norm"): begin
          for i=0,2 do begin 
              boxtext=widget_info(widget_info(baseid,find_by_uname='DropMenu'+fnumber(i)), /combobox_gettext)
              if(boxtext eq "spect") then plot_window,i
          endfor
      end

      (Uname eq "Tlast"): begin
          widget_control, widget_info(baseid,find_by_uname='Tlast'),get_uvalue=uval
          if(uval eq 1) then uval=0 else uval=1
          widget_control, widget_info(baseid,find_by_uname='Tlast'),set_uvalue=1 
          if(uval eq 1) then begin
              load_last, finish
              widget_control, widget_info(baseid,find_by_uname="TimeSlider"), set_slider_min=1, set_slider_max=finish,set_value=finish
              widget_control, widget_info(baseid,find_by_uname="TimeStepSlider"), get_value=tstep
              widget_control, widget_info(baseid,find_by_uname="TimeStepSlider"), set_slider_min=1, set_slider_max=finish,set_value=tstep
              replot_all
          endif
          Widget_Control, widget_info(baseid,find_by_uname='Tlast'), Set_Button=0
          widget_control, widget_info(baseid,find_by_uname='Tlast'),set_uvalue=0
      end
      

      (Uname eq "prnt"): begin
    widget_control, widget_info(baseid,find_by_uname='prnt'),get_uvalue=uval
    if(uval eq 1) then uval=0 else uval=1
    widget_control, widget_info(baseid,find_by_uname='prnt'),set_uvalue=1 
          if(uval eq 1) then begin 
              pm=!p.multi
              pth=!p.thick
              xth=!x.thick
              yth=!y.thick
              chth=!p.charthick

              !x.thick=2
              !y.thick=2
              !p.charthick=2
              !p.thick=2

;first save the current window
              !p.multi=[0,1,3]
              set_plot, "PS"
              device,file="wid.ps", xsize=12,ysize=13,/encaps; 17.2+4, ysize=20

              for i=0,2 do begin
                  plot_window,i
              endfor
              cd,current=currentdir
              xyouts, .1*0+.02,.98*0+.1,currentdir,charsize=.7,/normal,orientation=90
              add_timestamp,chs=.7,xl=.75, yl=.66
              device,/close

;now save the standard collection of fields

              !p.multi=[0,2,5]
              set_plot, "PS"
              device,file="widall.ps", xsize=12*2,ysize=20,/encaps; 17.2+4, ysize=20
              !p.multi=[10-0,2,5]
              plot_window,2,boxtext="dens",iso=0,oneD=0,contr=1.
              !p.multi=[10-2,2,5]
              plot_window,2,boxtext="eb",iso=0,contr=0.25, oneD=0
              !p.multi=[10-4,2,5]
              plot_window,2,boxtext="bz",iso=0,oneD=0,contr=1.
              !p.multi=[10-6,2,5]
              plot_window,2,boxtext="T",iso=0
              !p.multi=[10-8,2,5]
              plot_window,2,boxtext="T",iso=1
              !p.multi=[10-1,2,5]
              plot_window,2,boxtext="dens",iso=0,oneD=1
              loadct,5
              !p.multi=[10-3,2,5]
              plot_window,2,boxtext="x-pxp",contr=1,oneD=0
              !p.multi=[10-5,2,5]
              plot_window,2,boxtext="x-pxe",contr=1,oneD=0
              
              loadct,4
              !p.multi=[10-7,2,5]
              plot_window,2,boxtext="x-game",contr=1,oneD=0
              !p.multi=[10-9,2,5]
              plot_window,2,boxtext="x-gamp",contr=1,oneD=0

              xyouts, .35,.98,currentdir,/normal,charsize=.8
              add_timestamp,chs=.85, xl=.8,yl=.8

              device,/close
              loadct,4
              !p.thick=pth
              set_plot,"x"
              !p.multi=pm
              !x.thick=xth
              !y.thick=yth
              !p.charthick=chth

          endif 
          Widget_Control, widget_info(baseid,find_by_uname='prnt'), Set_Button=0
          widget_control, widget_info(baseid,find_by_uname='prnt'),set_uvalue=0

          widget_control, widget_info(baseid,find_by_uname="TimeSlider"), get_value=tind
          spawn,"cp widall.ps widall."+fnumber(tind)+ ".ps"
          spawn,"cp wid.ps wid."+fnumber(tind)+ ".ps"
      end

      (Uname eq "Movie"): begin
          
          widget_control, widget_info(baseid,find_by_uname='Movie'),get_uvalue=uval
          if(uval eq 1) then uval=0 else uval=1
          widget_control, widget_info(baseid,find_by_uname='Movie'),set_uvalue=1 
          

          if(uval eq 1) then begin
              
              pm=!p.multi
              pth=!p.thick
              xth=!x.thick
              yth=!y.thick
              chth=!p.charthick
              
;              !x.thick=2
;              !y.thick=2
;              !p.charthick=2
;              !p.thick=2
              print, "in movie"
;first save the movie of the current window
              set_plot, "Z"
;              device,file="wid.ps", xsize=12,ysize=13,/encaps; 17.2+4, ysize=20
              movie1=1
              movie2=0
              
              if(movie1 eq 1) then begin 

              !p.multi=[0,1,3]
              resx=550
              resy=600 ;should determine it from the current widget size. How?
              device,set_resolution=[resx,resy]
              
              sminmax=widget_info(widget_info(baseid,find_by_uname="TimeSlider"), /slider_min_max)
              widget_control, widget_info(baseid,find_by_uname="TimeStepSlider"), get_value=dt
              
              bitmap=bytarr(resx,resy,(sminmax(1)-sminmax(0))/dt+1)
              
              for tstep=sminmax(0),sminmax(1),dt do begin 
                  widget_control, widget_info(baseid,find_by_uname="TimeSlider"), set_value=tstep

                  for i=0,2 do begin
                      boxtext=widget_info(widget_info(baseid,find_by_uname='DropMenu'+fnumber(i)), /combobox_gettext)
                      if(n_elements(yrng) ne 0) then y1=temporary(yrng)
                     ; if(boxtext eq "x-pxp") then yrng=[-.2,.2]
                      if(boxtext eq "dens") then yrng=[0,40]
                      if(boxtext eq "T") then yrng=[0,1.5]
                      plot_window,i,yrng=yrng  ;, tstep=tstep
                  endfor   
                  
                  cd,current=currentdir
                  xyouts, .1*0+.02,.98*0+.1,currentdir,charsize=.7,/normal,orientation=90
                  add_timestamp,chs=.7,xl=.75, yl=.66, time=tstep
                  
                  bitmap(*,*,(tstep-sminmax(0))/dt)=tvrd()

;save png
                     image24 = BytArr(3, resx, resy) 
                     TVLCT, red, gr, bl, /Get
                                ; Load the frames.
                     image24[0,*,*] = red(bitmap(*,*,(tstep-sminmax(0))/dt))
                     image24[1,*,*] = gr(bitmap(*,*,(tstep-sminmax(0))/dt))
                     image24[2,*,*] = bl(bitmap(*,*,(tstep-sminmax(0))/dt))
                     filename="pic."+fnumber(tstep)+".png"
                     write_png,filename,image24

              endfor  
              device,/close

          endif ;movie1


              if(movie2 eq 1) then begin 

              !p.multi=[0,2,4]
              resx=1020; 550
              resy=600 ;should determine it from the current widget size. How?
              device,set_resolution=[resx,resy]
              
              sminmax=widget_info(widget_info(baseid,find_by_uname="TimeSlider"), /slider_min_max)
              widget_control, widget_info(baseid,find_by_uname="TimeStepSlider"), get_value=dt
              
              bitmap=bytarr(resx,resy,(sminmax(1)-sminmax(0))/dt+1)
              
              for tstep=sminmax(0),sminmax(1),dt do begin 
                  widget_control, widget_info(baseid,find_by_uname="TimeSlider"), set_value=tstep

;                  for i=0,2 do begin
;                      boxtext=widget_info(widget_info(baseid,find_by_uname='DropMenu'+fnumber(i)), /combobox_gettext)
;                      if(n_elements(yrng) ne 0) then y1=temporary(yrng)
;                      if(boxtext eq "x-pxp") then yrng=[-.2,.2]
;                      if(boxtext eq "dens") then yrng=[0,40]
;                      if(boxtext eq "T") then yrng=[0,1.5]
;                      plot_window,i,yrng=yrng  ;, tstep=tstep
;                  endfor   
              !p.multi=[0,2,4]
 
;              !p.multi=[8-0,2,4]
              plot_window,2,boxtext="dens",iso=0,oneD=1,contr=1,yrng=[0,40]/2
              !p.multi=[8-2,2,4]
              plot_window,2,boxtext="x-pxp",contr=1,oneD=0, yrng=[-.15,.15];/2;*2/2/2
              !p.multi=[8-4,2,4]
              plot_window,2,boxtext="x-pxe",contr=1,oneD=0, yrng=[-.7,.7]*2;/2 /2
    !p.multi=[8-6,2,4]
widget_control, widget_info(baseid,find_by_uname='shoffset'),get_uvalue=uval
if(uval eq 0) then begin 

    plot_window,2,boxtext="T",iso=0, yrng=[0,1.7]
endif else begin
              widget_control, widget_info(baseid,find_by_uname='T'),set_value=[0,-4,2]
              widget_control, widget_info(baseid,find_by_uname='Norm'),set_value=[0,-1,6]
    plot_window,1,boxtext="spect",iso=1,contr=-50
endelse

;
              !p.multi=[8-1,2,4]
              plot_window,2,boxtext="dens",iso=0,oneD=0,contr=.75
;              loadct,5
              !p.multi=[8-3,2,4]
              plot_window,2,boxtext="eb",iso=0,contr=0.25,oneD=0

              !p.multi=[8-5,2,4]
              plot_window,2,boxtext="bz",iso=0,oneD=0,contr=1. 

    !p.multi=[8-7,2,4]

widget_control, widget_info(baseid,find_by_uname='shoffset'),get_uvalue=uval
if(uval eq 0) then begin 
              plot_window,2,boxtext="T",iso=1, yrng=[0,1]

endif else begin
              widget_control, widget_info(baseid,find_by_uname='T'),set_value=[0,-4,2]
              widget_control, widget_info(baseid,find_by_uname='Norm'),set_value=[0,-1,6]
    plot_window,1,boxtext="spect",iso=1,contr=50
endelse
                

                  cd,current=currentdir
                  xyouts, .1*0+.01,.98*0+.1,currentdir,charsize=.7,/normal,orientation=90
                  add_timestamp,chs=.7,xl=.85, yl=.76, time=tstep
                  
                  bitmap(*,*,(tstep-sminmax(0))/dt)=tvrd()

     
;save png
                     image24 = BytArr(3, resx, resy) 
                     TVLCT, red, gr, bl, /Get
                                ; Load the frames.
                     image24[0,*,*] = red(bitmap(*,*,(tstep-sminmax(0))/dt))
                     image24[1,*,*] = gr(bitmap(*,*,(tstep-sminmax(0))/dt))
                     image24[2,*,*] = bl(bitmap(*,*,(tstep-sminmax(0))/dt))
                     filename="pic."+fnumber(tstep)+".png"
widget_control, widget_info(baseid,find_by_uname='shoffset'),get_uvalue=uval
if(uval eq 1) then begin 
    filename="shpic."+fnumber(tstep)+".png"
endif
                     write_png,filename,image24



                 endfor
  
              device,/close

          endif ;movie2

          


          if(movie1 eq 1) then animarray, bitmap

              set_plot,"x"
              loadct,4
              !p.thick=pth
              set_plot,"x"
              !p.multi=pm
              !x.thick=xth
              !y.thick=yth
              !p.charthick=chth


          endif
          Widget_Control, widget_info(baseid,find_by_uname='Movie'), Set_Button=0
          widget_control, widget_info(baseid,find_by_uname='Movie'),set_uvalue=0
      end
      


      
      ELSE: donothing
  endcase
  
end

pro add_timestamp,chs=chs,top=top,xloc=xloc, yloc=yloc, time=tind
  common widcom, winid, widid, nplots
  common basecom, baseid,baseplots,wLCol1,wLSliders,plotarr1idvert
  
 if(n_elements(tind) eq 0) then widget_control, widget_info(baseid,find_by_uname="TimeSlider"), get_value=tind
  widget_control, widget_info(baseid,find_by_uname="interval"), get_value=intervstr
  widget_control, widget_info(baseid,find_by_uname='compfld'),get_value=compstr        
  comp=convert_to_type(compstr,'FLOAT')& comp=comp(0)
  interv=convert_to_type(intervstr,'FLOAT') & interv=interv(0)
  omp=.45/comp
  timestamp=omp*tind*interv
  
  label=textoidl('\omega_{pe}t=')+strtrim(convert_to_type(long(timestamp),'STRING'),1)+" out="+strtrim(convert_to_type(fix(tind),'STRING'),1)
  if(n_elements(chs) eq 0) then chs=1.2
  if(n_elements(xloc) eq 0) then begin 
      if(n_elements(top) eq 0) then begin 
          xloc=.7 & yloc=.05
      endif else begin
                                ;top
          xloc=.7 & yloc=.92
      endelse
  endif
  xyouts,xloc,yloc,label,/normal,chars=chs
end 

pro replot_all
  common widcom, winid, widid, nplots
  common basecom, baseid,baseplots,wLCol1,wLSliders,plotarr1idvert
  
  calc_offset
  for ni=0,nplots-1 do begin 
      plot_window, ni
;add timestamp to the first plot
      if(ni eq 0) then begin 
          add_timestamp
      endif
  endfor
end


; offset wrt shock will have to be calculated later
pro clear_offsets
  common offset, xoffset, xoffsets
  xoffsets = xoffsets*0 - 1
end


; calculate the offset in x to bring the shock (=median density) to x=0.
pro calc_offset, i
  common widcom, winid, widid, nplots
  common basecom, baseid,baseplots,wLCol1,wLSliders,plotarr1idvert
  common datacom, ss, allname,fldname,prtname,spname,fnname
  common datasize, dims, threed
  common offset, xoffset, xoffsets
  widget_control, widget_info(baseid,find_by_uname='shoffset'),get_uvalue=uval
  if(uval eq 1) then begin
      widget_control, widget_info(baseid,find_by_uname="TimeSlider"), get_value=tind
      if(xoffsets(tind) ne -1) then begin
          xoffset=xoffsets(tind)
                                ;print,'computed already, offset=',xoffset
      endif else begin 
          dataind=where(allname eq "dens")
          dataind=dataind(0)
          if(dataind(0) eq -1) then print, "no such data in the list"
          if(ss(tind,dataind) eq ptr_new()) then load_data, tind,dataind
          temp1=ss(tind,dataind)
          tempdens=total(*ss(tind,dataind),2)
          ind1=where(tempdens gt 0.5*(min(tempdens(where(tempdens gt 0)))+max(tempdens)))
          ind1=ind1((size(ind1))(1)-1) ;
;print,ind1
;widget_control, widget_info(baseid,find_by_uname="TimeSlider"), get_value=tind
;widget_control, widget_info(baseid,find_by_uname="interval"), get_value=intervstr
          widget_control, widget_info(baseid,find_by_uname='compfld'),get_value=compstr        
          widget_control, widget_info(baseid,find_by_uname='subsmpfld'),get_value=substr
          comp=convert_to_type(compstr,'FLOAT') & comp=comp(0)
          sub=convert_to_type(substr,'FLOAT') & sub=sub(0)
          sz=size((*ss(tind,dataind))) & mx=sz(1) & xoffset=round(float(mx*sub/comp)*float(ind1)/float(mx))
          xoffsets(tind)=xoffset
                                ;print,'computed now, offset=',xoffset
      endelse
                                ;Widget_Control, widget_info(baseid,find_by_uname='shoffset'), Set_Button=0
  endif else begin
      xoffset=0
                                ;print,'calc offset called but shoffset=',uval
  endelse
end


pro refresh_xslider
  
  common widcom, winid, widid, nplots
  common basecom, baseid,baseplots,wLCol1,wLSliders,plotarr1idvert
  common datacom, ss, allname,fldname,prtname,spname,fnname
  common datasize, dims, threed
  common offset, xoffset, xoffsets
  
  widget_control, widget_info(baseid,find_by_uname='compfld'),get_value=compstr
  widget_control, widget_info(baseid,find_by_uname='subsmpfld'),get_value=substr
  comp=convert_to_type(compstr,'FLOAT')& comp=comp(0)
  sub=convert_to_type(substr,'FLOAT') & sub=sub(0)
                                ;data extent: 
;print,'refreshed'
  mx=dims(0)
  my=dims(1)
  if(threed eq 1) then mz=dims(2)
  datxmax=mx*sub/comp
  datymax=my*sub/comp
  if(threed eq 1) then datzmax=mz*sub/comp
  
  yr0=[0,datymax]
  calc_offset
  xr0=[0-xoffset,datxmax-xoffset]
  
  xminmax=[xr0(0), xr0(1)]
  widget_control,widget_info(baseid,find_by_uname='SliderXMin'),$
    set_slider_min=min([xminmax(0),xr0(0)]),set_slider_max=max([xminmax(1),xr0(1)]),set_value=max([-150,xminmax(0)]) ;max([xmin,-150])
  widget_control,widget_info(baseid,find_by_uname='SliderXMax'),$
    set_slider_min=min([xminmax(0),xr0(0)]),set_slider_max=max([xminmax(1),xr0(1)]),set_value=min([150,xminmax(1)])  ;xmax ;min([xmax,150])
  
end


pro plot_window, i, lineloc=lineloc, regionloc1=regionloc1, regionloc2=regionloc2, boxtext=boxtext, iso=iso, oned=oned, contrin=contrin, tstep=tstep, yrng=yrng
  common widcom, winid, widid, nplots
  common basecom, baseid,baseplots,wLCol1,wLSliders,plotarr1idvert
  common datacom, ss, allname,fldname,prtname,spname,fnname
  common datasize, dims, threed
  common offset, xoffset, xoffsets
  common params, comp, istep, mime, qi, ppc0, gamma0, c, keions

;  yrng=[0,30]
;  y1=temporary(yrng) ;this undefines yrng variable

if(!d.name eq "X") then   !p.multi=[0,1,1]
;first check that the data is loaded
;get timestep
 if(n_elements(tstep) eq 0) then  begin 
     widget_control, widget_info(baseid,find_by_uname="TimeSlider"), get_value=tind
 endif else begin
     tind=tstep
 endelse

  if(n_elements(boxtext) eq 0) then begin  
  boxtext=widget_info(widget_info(baseid,find_by_uname='DropMenu'+fnumber(i)), /combobox_gettext)
endif

  dataind=where(allname eq boxtext)
  dataind=dataind(0)
  special=0 & if(boxtext eq "x-pxe" or boxtext eq "x-pxp" or boxtext eq "x-pye" $
                 or boxtext eq "x-pyp" or boxtext eq "x-pze" or boxtext eq "x-pzp" $
                 or boxtext eq "spect" $
                 or boxtext eq "T" or boxtext eq "Te" or boxtext eq "x-p_avg" or boxtext eq "x-p_stdev" $
                 or boxtext eq "x-game" or boxtext eq "x-gamp" $
                 or boxtext eq "pxp-pyp" or boxtext eq "pxe-pye" or boxtext eq "pxp-pzp" $
                 or boxtext eq "pxe-pze" or boxtext eq "pyp-pzp" or boxtext eq "pye-pze") then special=1
  
  if(dataind(0) eq -1) then print, "no such data in the list"
  if(ss(tind,dataind) eq ptr_new()) then load_data, tind,dataind

;  widget_control, widget_info(baseid,find_by_uname='prnt'),get_uvalue=uval
;  if(uval eq 0) then wset,winid(i)    ;checking for ps output

  if(!d.name eq "X") then wset,winid(i)

;	boxtext=widget_info(widget_info(baseid,find_by_uname='DropMenu'+fnumber(i)), /combobox_gettext)
;	dataind=where(allname eq boxtext)
;	dataind=dataind(0)
;	if(dataind(0) eq -1) then print, "no such data in the list"
  
  flag1=0
  
  widget_control, widget_info(baseid,find_by_uname='SliderXMin'), get_value=xmin
  widget_control, widget_info(baseid,find_by_uname='SliderXMax'), get_value=xmax
  
  widget_control, widget_info(baseid,find_by_uname='SliderYMin'), get_value=ymin
  widget_control, widget_info(baseid,find_by_uname='SliderYMax'), get_value=ymax
  refresh_FFTxsliderRange
  
  if(threed eq 1) then widget_control, widget_info(baseid,find_by_uname='Zloc'), get_value=zloc
  
  
;	print, "xmin, xmax", xmin,xmax
  
  widget_control, widget_info(baseid,find_by_uname='Contrast'+fnumber(i)), get_value=contr
  widget_control, widget_info(baseid,find_by_uname='iso'+fnumber(i)),get_uvalue=isoval
  widget_control, widget_info(baseid,find_by_uname='oneD'+fnumber(i)),get_uvalue=oneDval
  if(n_elements(oned) ne 0) then oneDval=oned 
  if(n_elements(contrin) ne 0) then contr=contrin
  if(n_elements(iso) ne 0) then isoval = iso
  
  widget_control, widget_info(baseid,find_by_uname='compfld'),get_value=compstr
  widget_control, widget_info(baseid,find_by_uname='subsmpfld'),get_value=substr
  comp=convert_to_type(compstr,'FLOAT')& comp=comp(0)
  sub=convert_to_type(substr,'FLOAT') & sub=sub(0)
                                ;data extent: 
  
  mx=dims(0)
  my=dims(1)
  if(threed eq 1) then mz=dims(2)
  
  datxmax=mx*sub/comp
  datymax=my*sub/comp
  if(threed eq 1) then datzmax=mz*sub/comp
  
; mins are zeros 
; now change the bounds
;enlarge the bounds if needed
;
; if yvalue=1 then this is the first time, and can reset the sliders
; to maximum coverage
  
  xr0=[0,datxmax]
  yr0=[0,datymax]
  
  widget_control, widget_info(baseid,find_by_uname='shoffset'), get_uvalue=uval
  calc_offset
  xr0=[0-xoffset,datxmax-xoffset]
  
  if(threed eq 1) then zr0=[0,datzmax]
  
  xminmax=widget_info(widget_info(baseid,find_by_uname="SliderXMin"),/slider_min_max)
  widget_control,widget_info(baseid,find_by_uname='SliderXMin'),$
    set_slider_min=min([xminmax(0),xr0(0)]),set_slider_max=max([xminmax(1),xr0(1)]),set_value=xmin
  widget_control,widget_info(baseid,find_by_uname='SliderXMax'),$
    set_slider_min=min([xminmax(0),xr0(0)]),set_slider_max=max([xminmax(1),xr0(1)]),set_value=xmax
;		if(oneDval eq 0) then begin ;set y bounds only if it's not 1D plot
  yminmax=widget_info(widget_info(baseid,find_by_uname="SliderYMin"),/slider_min_max)
  widget_control,widget_info(baseid,find_by_uname='SliderYMin'),set_slider_min=min([yminmax(0),yr0(0)]),$
    set_slider_max=max([yminmax(1),yr0(1)]),set_value=ymin
  widget_control,widget_info(baseid,find_by_uname='SliderYMax'),set_slider_min=min([yminmax(0),yr0(0)]),$
    set_slider_max=max([yminmax(1),yr0(1)]),set_value=ymax
  
  if(threed eq 1) then begin 
      zminmax=widget_info(widget_info(baseid,find_by_uname="Zloc"),/slider_min_max)
      widget_control,widget_info(baseid,find_by_uname='Zloc'),set_slider_min=min([zminmax(0),zr0(0)]),$
        set_slider_max=max([zminmax(1),zr0(1)]),set_value=zloc
  endif
  
;		endif
;	endif		
  widget_control, widget_info(baseid,find_by_uname='SliderYMax'), get_value=ymx
  if(ymx eq 1 ) then begin
      xminmax=widget_info(widget_info(baseid,find_by_uname="SliderXMax"),/slider_min_max)
      widget_control, widget_info(baseid,find_by_uname='SliderXMax'), set_value=xminmax(1)
      yminmax=widget_info(widget_info(baseid,find_by_uname="SliderYMax"),/slider_min_max)
      widget_control, widget_info(baseid,find_by_uname='SliderYMax'), set_value=yminmax(1)
      flag1=1
  endif
  
  widget_control, widget_info(baseid,find_by_uname='SliderXMin'), get_value=xmin
  widget_control, widget_info(baseid,find_by_uname='SliderXMax'), get_value=xmax
  widget_control, widget_info(baseid,find_by_uname='SliderYMin'), get_value=ymin
  widget_control, widget_info(baseid,find_by_uname='SliderYMax'), get_value=ymax
  
  if(special eq 0) then begin 
      sz=size((*ss(tind,dataind))) & mx=sz(1) & datxmax=mx*sub/comp & my=sz(2) 
      if(datxmax-xoffset lt xmin) then xmin=datxmax-xoffset-10
      
  endif else begin
      if(boxtext eq "x-pxe" or boxtext eq "x-pxp" or boxtext eq "x-pye" $
         or boxtext eq "x-pyp" or boxtext eq "x-pze" or boxtext eq "x-pzp" $
         or boxtext eq "x-game" or boxtext eq "x-gamp" $
         or boxtext eq "x-p_avg" or boxtext eq "x-p_stdev") then begin
          datxmax=max((*ss(tind,dataind)).varx/comp)
          if(datxmax-xoffset lt xmin) then xmin=datxmax-xoffset-10
      endif
      if(boxtext eq "spect") then begin
                                ;
          
      endif
  endelse	
  
  if(special eq 0) then begin 
      
      if(oneDval eq 0 ) then begin 
          if(threed eq 0) then zloc=0
          if(threed eq 1) then  widget_control, widget_info(baseid,find_by_uname="Zloc"), get_value=zloc
          
;            widget_control, widget_info(baseid,find_by_uname=''),get_uvalue=uval
          
          fntext=widget_info(widget_info(baseid,find_by_uname='fnMenu'+fnumber(i)), /combobox_gettext)
          fnind=where(fnname eq boxtext)
          fnind=fnind(0)
          
;symmetric zrange for vector fields (and rho and Phi)
          indvec=where(allname eq "bx" or allname eq "by" or allname eq "bz" or allname eq "ex" or allname eq "ey" or allname eq "ez" or allname eq "jx" or allname eq "jy" or allname eq "jz")
          whvec=where(indvec eq dataind)
;in cell units, boundaries where to compute zrange
          xminc=max([1,ceil((1.*xmin+xoffset)*comp/sub)])
          xmaxc=min([mx-1,floor((1.*xmax+xoffset)*comp/sub)])
          yminc=ceil(1.*ymin*comp/sub)
          ymaxc=min([my-1,floor(1.*ymax*comp/sub)])
;min and max within that region
          zmin=min((*ss(tind,dataind))(xminc:xmaxc,yminc:ymaxc,zloc))
          zmax=max((*ss(tind,dataind))(xminc:xmaxc,yminc:ymaxc,zloc))
          zminave=min(total((*ss(tind,dataind))(xminc:xmaxc,yminc:ymaxc,zloc),2))
          zmaxave=max(total((*ss(tind,dataind))(xminc:xmaxc,yminc:ymaxc,zloc),2))
          if (whvec(0) ne -1 or boxtext eq 'rho' or boxtext eq 'Phi') then begin
              if (zminave*zmaxave lt 0) then begin
                  zabsmax=max([zmax,abs(zmin)])
                  zrange=[-zabsmax^contr,zabsmax^contr]
              endif else begin
                  zrange=[abs(zmin)^contr*sign(zmin),abs(zmax)^contr*sign(zmax)]
              endelse
          endif else begin
              zrange=[abs(zmin)^contr*sign(zmin),abs(zmax)^contr*sign(zmax)]
          endelse

          if (fntext eq 'data') then begin
              pl2d, abs((*ss(tind,dataind))(*,*,zloc))^contr*sign((*ss(tind,dataind))(*,*,zloc)),outxr=xr0,xoffset1=xoffset, $
                outyr=yr0,xr=[xmin,xmax],yr=[ymin,ymax],iso=isoval,scale=comp,subxsampl=sub,subysampl=sub,$
                /xs,/ys,title=boxtext,/quiet,zr=zrange
;print, minrange,maxrange,zrange
          endif else begin
              case fntext of
                  'vect':begin
                      indbx=where(allname eq "bx")
                      indby=where(allname eq "by")
                      
                      if(ss(tind,indbx(0)) eq ptr_new()) then load_data, tind,indbx(0)
                      if(ss(tind,indby(0)) eq ptr_new()) then load_data, tind,indby(0)
                      
                      vx=(*ss(tind,indbx(0)))(*,*,zloc)
                      vy=(*ss(tind,indby(0)))(*,*,zloc)
                      
                      pl2d, abs((*ss(tind,dataind))(*,*,zloc))^contr*sign((*ss(tind,dataind))(*,*,zloc)), vx=vx,vy=vy, $
                        outxr=xr0,xoffset1=xoffset, $
                        outyr=yr0,xr=[xmin,xmax],yr=[ymin,ymax],iso=isoval,scale=comp,subxsampl=sub,subysampl=sub,$
                        /xs,/ys,title=boxtext,/quiet,len=4 , maxvec=[100,30]
                     
                  end
                  'P(k)':begin
;two-dimensional Fourier transform
                      widget_control, widget_info(baseid,find_by_uname='FFTX0'), get_value=fftx0str
                      widget_control, widget_info(baseid,find_by_uname='FFTDX'), get_value=fftdxstr
                      fftx0=convert_to_type(fftx0str,'FLOAT') & fftx0=fftx0(0)
                      fftdx=convert_to_type(fftdxstr,'FLOAT') & fftdx=fftdx(0)
                      sz=size((*ss(tind,dataind))) & mx=sz(1) & my=sz(2)
                      fftxi1=max([0,round(float(fftx0-0.5*fftdx+xoffset)*float(comp/sub))])
                      fftxi2=min([mx-1,round(float(fftx0+0.5*fftdx+xoffset)*float(comp/sub))])
                      fftyi1=max([0,round(ymin*float(comp/sub))])
                      fftyi2=min([my-1,round(ymax*float(comp/sub))])
                                ; make the data lengths odd
                      if ((fftxi2-fftxi1)/2.0 ne (fftxi2-fftxi1)/2) then fftxi2=fftxi2-1
                      if ((fftyi2-fftyi1)/2.0 ne (fftyi2-fftyi1)/2) then fftyi2=fftyi2-1
                      fftmx=fftxi2-fftxi1+1 & fftmy=fftyi2-fftyi1+1
                      fftdt = (*ss(tind,dataind))(fftxi1:fftxi2,fftyi1:fftyi2,zloc)
                      fftfn = MAKE_ARRAY(fftmx+1, fftmy+1, /COMPLEX, VALUE = 0)
                      fftfn(0:fftmx-1,0:fftmy-1) = fft(fftdt)
                      fftfn(fftmx,*)=fftfn(0,*)
                      fftfn(*,fftmy)=fftfn(*,0)
                      fftnx = [-reverse(findgen((fftmx-1)/2))-1,findgen((fftmx+1)/2)]*comp/sub/fftmx
                      fftny = findgen((fftmy+1)/2)*comp/sub/fftmy

                      fftpw = (fftmx*fftmy)*MAKE_ARRAY(fftmx,fftmy, /FLOAT, VALUE = 1.0)
                      for ix=0, fftmx-1 do begin 
                          for iy=0, fftmy-1 do begin 
                              fftpw(ix,iy)=fftpw(ix,iy)*fftfn(ix,iy)*fftfn(fftmx-ix,fftmy-iy)
                          endfor
                      endfor
                      fftpw=shift(fftpw,(fftmx-1)/2,0)
                      
;to adjust the plot zrange by plotting only the interesting part
                      kcut=1
                      kxmin=min(where(fftnx gt -kcut))-1
                      kxmax=max(where(fftnx lt kcut))+1
                      kymax=max(where(fftny lt kcut))+1
                    
                      if (not isoval) then begin                  
;getting rid of the power for k=0 by smoothing
                          fftpw((fftmx-1)/2,0)=0.25*(fftpw((fftmx-1)/2,1)+fftpw((fftmx+1)/2,0)+fftpw((fftmx-1)/2,fftmy-1)+fftpw((fftmx-3)/2,0))
                          fftpwplot=fftpw(kxmin:kxmax,0:kymax)
                          fftnxplot=fftnx(kxmin:kxmax)
                          fftnyplot=fftny(0:kymax)
                          
                          pl2d, abs(alog10(fftpwplot))^contr*sign(alog10(fftpwplot)),fftnxplot,fftnyplot,title=fntext+' '+boxtext,/quiet, xtit=textoidl('l_x^{-1} [\omega_{pe}/c]'),ytit=textoidl('l_y^{-1} [\omega_{pe}/c]'),/iso
                      endif else begin
;build the radial spectrum
;                          fftr = MAKE_ARRAY(fftmx,(fftmy+1)/2, /FLOAT, VALUE = 0.0)
;                          for ix=0, fftmx-1 do begin ; both positive and negative kx, only positive ky 
;                              for iy=0, (fftmy-1)/2 do begin
;                                  fftr(ix,iy) = sqrt(fftnx(ix)^2+fftny(iy)^2)
;                              endfor
;                          endfor
                          fftrN = 30
                          fftrK1 = min([fftnx(2)-fftnx(1),fftny(2)-fftny(1)])
                          fftrK2 = kcut
                          fftrK = fltarr(fftrN+1)
                          fftrP = fltarr(fftrN)
                          for ib=0,fftrN do begin
                              fftrK(ib) = fftrK1*(fftrK2/fftrK1)^(convert_to_type(ib,'FLOAT')/convert_to_type(fftrN,'FLOAT'))
                          endfor  
                          fftrqV = (fftnx(2)-fftnx(1))*(fftny(2)-fftny(1))/(alog(fftrK(2))-alog(fftrK(1)))
                          for ix=0, fftmx-1 do begin ; both positive and negative kx, only positive ky 
                              for iy=0, (fftmy-1)/2 do begin
                                  fftkr = sqrt(fftnx(ix)^2+fftny(iy)^2)
                                  fftrki = floor(((alog(fftkr)-alog(fftrK1))/(alog(fftrK2)-alog(fftrK1)))*convert_to_type(fftrN,'FLOAT'))
                                  if ((fftrki ge 0) and (fftrki le fftrN-1)) then begin 
                                      fftrP(fftrki) = fftrP(fftrki) + fftrqV * fftpw(ix,iy)
                                  endif
                              endfor
                          endfor

                          fftpwx=total(fftpw,2)*comp/sub/fftmy
                          fftpwy=total(fftpw,1)*comp/sub/fftmx
                          plot,fftrK(1:fftrN),fftrP,/xl,/yl,tit=textoidl('dP/dln(k_i): i=r(yel),x(red),y(green) ')+boxtext,xtit=textoidl('l_i^{-1} [\omega_{pe}/c]'),xr=[fftrK1,fftrK2],/xs,yr=minmax([fftrP(where(fftrP gt 0)),fftnx[(fftmx+1)/2:kxmax]*fftpwx[(fftmx+1)/2:kxmax],fftny[1:kymax]*fftpwy[1:kymax]]),/ys                      
                          oplot,fftny[1:kymax],fftny[1:kymax]*fftpwy[1:kymax],col=100
                          oplot,fftnx[(fftmx+1)/2:kxmax],fftnx[(fftmx+1)/2:kxmax]*fftpwx[(fftmx+1)/2:kxmax], col=150

;   Lorenzo's version:
;                          nrad=(fftmx+1)/2*(fftmy+1)/2
;                          fftpwrad=fltarr(nrad)
;                          fftnrad=fltarr(nrad)
;                          irad=0l
;                          for ix=(fftmx-1)/2, fftmx-1 do begin ;only positive (or zero) kx and ky
;                              for iy=0, (fftmy-1)/2 do begin
;                                  fftpwrad(irad)=fftpw(ix,iy) ; already including the additional k_r for the power
;                                  fftnrad(irad)=sqrt(fftnx(ix)^2+fftny(iy)^2)
;                                  irad=irad+1 
;                              endfor
;                          endfor
;                          sortrad=sort(fftnrad)
;                          fftnrad=fftnrad(sortrad)
;                          fftpwrad=fftpwrad(sortrad)
;                          plot,fftnrad[1:nrad-1],fftpwrad[1:nrad-1],/xl,/yl,tit=textoidl('k_r P(k_r) ')+boxtext,xtit=textoidl('l_r^{-1} [\omega_{pe}/c]'),ytit=textoidl('k_r P(k_r)'),xr=[fftnrad(1),kcut],/xs                      

                      endelse
                      
                                ;,xr=[-0.1,0.1],yr=[0.,0.1],/xs,/ys;,xoffset1=xoffset
                      
; this just plots the field data for the fft region, so we know we're
; in the right place:
;		pl2d, abs(fftdt)^contr*sign(fftdt),outxr=xr0,xoffset1=xoffset, outyr=yr0,xr=[xmin,xmax],yr=[ymin,ymax],iso=isoval,scale=comp,subxsampl=sub,subysampl=sub, /xs,/ys,title=fntext+' '+boxtext,/quiet, xtit=textoidl('k_x'),ytit=textoidl('k_y')
                  end 
                  else:		pl2d, abs((*ss(tind,dataind))(*,*,zloc))^contr*sign((*ss(tind,dataind))(*,*,zloc)),outxr=xr0,xoffset1=xoffset, $
                    outyr=yr0,xr=[xmin,xmax],yr=[ymin,ymax],iso=isoval,scale=comp,subxsampl=sub,subysampl=sub,$
                    /xs,/ys,title=boxtext,/quiet,zr=zrange
              endcase
          endelse               ;no data
      endif else begin
          ; 1D from here
    
          if(threed eq 0) then zloc=0
          if(threed eq 1) then  widget_control, widget_info(baseid,find_by_uname="Zloc"), get_value=zloc
          
          fntext=widget_info(widget_info(baseid,find_by_uname='fnMenu'+fnumber(i)), /combobox_gettext)
          fnind=where(fnname eq boxtext)
          fnind=fnind(0)
          if (fntext eq 'data') then begin
              widget_control, widget_info(baseid,find_by_uname='Contrast'+fnumber(i)), get_value=contr

              if(threed eq 0) then begin
                  if (not isoval) then begin
                      dny=max([0,round(float(ymin)*comp/sub)])
                      upy=min([my-1,round(float(ymax)*comp/sub)])
                      pl1d, total(abs((*ss(tind,dataind))(*,dny:upy))^contr*sign((*ss(tind,dataind))(*,dny:upy)),2)/(upy-dny),$
                    xr=[xmin,xmax],yr=yrng,outxr=xr0,xoffset1=xoffset,scale=comp,subxsampl=sub,/xs,ytitle=boxtext+textoidl(' PARTIAL Y') ,/ys;,subysampl=sub        
                      print , "contr=", contr, minmax(total(abs((*ss(tind,dataind)))^contr*sign(*ss(tind,dataind)),2))
                  endif else begin ;isoval=1
                      pl1d, total(abs((*ss(tind,dataind)))^contr*sign(*ss(tind,dataind)),2)/my,$
                    xr=[xmin,xmax],yr=yrng,outxr=xr0,xoffset1=xoffset,scale=comp,subxsampl=sub,/xs,ytitle=boxtext+textoidl(' TOTAL Y'),/ys ;,subysampl=sub        
                      print , "contr=", contr, minmax(total(abs((*ss(tind,dataind)))^contr*sign(*ss(tind,dataind)),2))
                  endelse
              endif

              if(threed eq 1) then begin
                  if (not isoval) then begin
                      dny=max([0,round(float(ymin)*comp/sub)])
                      upy=min([my-1,round(float(ymax)*comp/sub)])
                      pl1d, total(total(abs((*ss(tind,dataind))(*,dny:upy,*))^contr*sign((*ss(tind,dataind))(*,dny:upy,*)),2),2)/(upy-dny),$
                    xr=[xmin,xmax],yr=yrng,outxr=xr0,xoffset1=xoffset,scale=comp,subxsampl=sub,/xs,ytitle=boxtext+textoidl(' PARTIAL Y') ,/ys
                  endif else begin
                      pl1d, total(total(abs((*ss(tind,dataind)))^contr*sign(*ss(tind,dataind)),2),2)/my,$
                    xr=[xmin,xmax],yr=yrng,outxr=xr0,xoffset1=xoffset,scale=comp,subxsampl=sub,/xs,ytitle=boxtext+textoidl(' TOTAL Y')   ,/ys   
                  endelse
              endif
              
          endif else begin
              case fntext of
                  'P(k)':begin
                      widget_control, widget_info(baseid,find_by_uname='FFTX0'), get_value=fftx0str
                      widget_control, widget_info(baseid,find_by_uname='FFTDX'), get_value=fftdxstr
                      fftx0=convert_to_type(fftx0str,'FLOAT') & fftx0=fftx0(0)
                      fftdx=convert_to_type(fftdxstr,'FLOAT') & fftdx=fftdx(0)
                      sz=size((*ss(tind,dataind))) & mx=sz(1) & my=sz(2)
                      fftxi1=max([0,round(float(fftx0-0.5*fftdx+xoffset)*float(comp/sub))])
                      fftxi2=min([mx-1,round(float(fftx0+0.5*fftdx+xoffset)*float(comp/sub))])
                      fftyi1=max([0,round(ymin*float(comp/sub))])
                      fftyi2=min([my-1,round(ymax*float(comp/sub))])
                                ; make the data lengths odd
                      if ((fftxi2-fftxi1)/2.0 ne (fftxi2-fftxi1)/2) then fftxi2=fftxi2-1
                      if ((fftyi2-fftyi1)/2.0 ne (fftyi2-fftyi1)/2) then fftyi2=fftyi2-1
                      fftmx=fftxi2-fftxi1+1 & fftmy=fftyi2-fftyi1+1
                      fftdt = (*ss(tind,dataind))(fftxi1:fftxi2,fftyi1:fftyi2,zloc)
                      fftfn = MAKE_ARRAY(fftmx+1, fftmy+1, /COMPLEX, VALUE = 0)
                      fftfn(0:fftmx-1,0:fftmy-1) = fft(fftdt)
                      fftfn(fftmx,*)=fftfn(0,*)
                      fftfn(*,fftmy)=fftfn(*,0)
                      fftnx = [-reverse(findgen((fftmx-1)/2))-1,findgen((fftmx+1)/2)]*comp/sub/fftmx
                      fftny = findgen((fftmy+1)/2)*comp/sub/fftmy

                      fftpw = (fftmx*fftmy)*MAKE_ARRAY(fftmx,fftmy, /FLOAT, VALUE = 1.0)
                      for ix=0, fftmx-1 do begin 
                          for iy=0, fftmy-1 do begin 
                              fftpw(ix,iy)=fftpw(ix,iy)*fftfn(ix,iy)*fftfn(fftmx-ix,fftmy-iy)
                          endfor
                      endfor
                      fftpw=shift(fftpw,(fftmx-1)/2,0)
                      
                      kcut=1
                      fftpwx=total(fftpw,2)*comp/sub/fftmy
                      fftpwy=total(fftpw,1)*comp/sub/fftmx
                      kxmin=min(where(fftnx gt -kcut))-1
                      kxmax=max(where(fftnx lt kcut))+1
                      kymax=max(where(fftny lt kcut))+1
                          plot,fftnx[(fftmx+1)/2:kxmax],fftnx[(fftmx+1)/2:kxmax]*fftpwx[(fftmx+1)/2:kxmax],/xl,/yl,tit=textoidl('dP/dlog(k_i): i=x(red),y(green) ')+boxtext,xtit=textoidl('l_i^{-1} [\omega_{pe}/c]'),xr=[min([fftnx((fftmx+1)/2),fftny(1)]),kcut],/xs,yr=minmax([fftnx[(fftmx+1)/2:kxmax]*fftpwx[(fftmx+1)/2:kxmax],fftny[1:kymax]*fftpwy[1:kymax]]),/ys,/nodata
                          oplot,fftny[1:kymax],fftny[1:kymax]*fftpwy[1:kymax],col=100
                          oplot,fftnx[(fftmx+1)/2:kxmax],fftnx[(fftmx+1)/2:kxmax]*fftpwx[(fftmx+1)/2:kxmax], col=150                        

                  end 
                  else:begin
                      widget_control, widget_info(baseid,find_by_uname='Contrast'+fnumber(i)), get_value=contr
                      
                      if(threed eq 0) then begin
                          if (not isoval) then begin
                              dny=max([0,round(float(ymin)*comp/sub)])
                              upy=min([my-1,round(float(ymax)*comp/sub)])
                              pl1d, total(abs((*ss(tind,dataind))(*,dny:upy))^contr*sign((*ss(tind,dataind))(*,dny:upy)),2),$
                                xr=[xmin,xmax],outxr=xr0,xoffset1=xoffset,scale=comp,subxsampl=sub,/xs,ytitle=boxtext+textoidl(' PARTIAL Y') ;,subysampl=sub        
                              print , "contr=", contr, minmax(total(abs((*ss(tind,dataind)))^contr*sign(*ss(tind,dataind)),2))
                          endif else begin
                              pl1d, total(abs((*ss(tind,dataind)))^contr*sign(*ss(tind,dataind)),2),$
                                xr=[xmin,xmax],outxr=xr0,xoffset1=xoffset,scale=comp,subxsampl=sub,/xs,ytitle=boxtext+textoidl(' TOTAL Y') ;,subysampl=sub        
                              print , "contr=", contr, minmax(total(abs((*ss(tind,dataind)))^contr*sign(*ss(tind,dataind)),2))
                          endelse
                      endif

                      if(threed eq 1) then begin
                          if (not isoval) then begin
                              dny=max([0,round(float(ymin)*comp/sub)])
                              upy=min([my-1,round(float(ymax)*comp/sub)])
                              pl1d, total(total(abs((*ss(tind,dataind))(*,dny:upy,*))^contr*sign((*ss(tind,dataind))(*,dny:upy,*)),2),2),$
                                xr=[xmin,xmax],outxr=xr0,xoffset1=xoffset,scale=comp,subxsampl=sub,/xs,ytitle=boxtext+textoidl(' PARTIAL Y') 
                          endif else begin
                              pl1d, total(total(abs((*ss(tind,dataind)))^contr*sign(*ss(tind,dataind)),2),2),$
                                xr=[xmin,xmax],outxr=xr0,xoffset1=xoffset,scale=comp,subxsampl=sub,/xs,ytitle=boxtext+textoidl(' TOTAL Y')      
                          endelse
                      endif

                  end
              endcase
          endelse
                                ;no data
      endelse
      
                                ;no one-dimensional
      if(n_elements(lineloc) ne 0) then oplot, [lineloc,lineloc],[-1e6,1e6],li=2
      if(n_elements(regionloc1) ne 0 and boxtext ne "pxp-pyp" and boxtext ne "pxe-pye" $
         and boxtext ne "pxp-pzp" and boxtext ne "pxe-pze" and boxtext ne "pyp-pzp" $
         and boxtext ne "pye-pze") then oplot, [regionloc1,regionloc1],[-1e6,1e6],li=3,col=150
      if(n_elements(regionloc2) ne 0 and boxtext ne "pxp-pyp" and boxtext ne "pxe-pye" $
         and boxtext ne "pxp-pzp" and boxtext ne "pxe-pze" and boxtext ne "pyp-pzp" $
         and boxtext ne "pye-pze") then oplot, [regionloc2,regionloc2],[-1e6,1e6],li=3,col=150
      
  endif else begin              ;special plot 
       ;;;;;;;;;;;Momentum Space plot ;;;;;;;;;;;;;;;;;;;;;
      if(boxtext eq "x-pxe" or boxtext eq "x-pxp" or boxtext eq "x-pye" or boxtext eq "x-pyp" $
         or boxtext eq "x-pze" or boxtext eq "x-pzp" or boxtext eq "x-game" or boxtext eq "x-gamp" $
         or boxtext eq "pxp-pyp" or boxtext eq "pxe-pye" or boxtext eq "pxp-pzp" $
         or boxtext eq "pxe-pze" or boxtext eq "pyp-pzp" or boxtext eq "pye-pze") then begin
                                ;print , "minmax=",xmin,xmax


          print, "contr set to", contr
          loadct,5,/silent
          norm=comp
          case boxtext of
              'x-pxe': ytitle=textoidl('u_{xe}')
              'x-pye': ytitle=textoidl('u_{ye}')
              'x-pze': ytitle=textoidl('u_{ze}')
              'x-pxp': ytitle=textoidl('u_{xp}')
              'x-pyp': ytitle=textoidl('u_{yp}')
              'x-pzp': ytitle=textoidl('u_{zp}')
              'x-gamp': ytitle=textoidl('\gamma_p')
              'x-game': ytitle=textoidl('\gamma_e')
              'pxe-pye': begin 
                  ytitle=textoidl('u_{ye}') & specxtitle=textoidl('u_{xe}') & norm=1
              end
              'pxp-pyp': begin
                  ytitle=textoidl('u_{yp}') & specxtitle=textoidl('u_{xp}') & norm=1
              end
              'pxe-pze': begin 
                  ytitle=textoidl('u_{ze}') & specxtitle=textoidl('u_{xe}') & norm=1
              end
              'pxp-pzp': begin
                  ytitle=textoidl('u_{zp}') & specxtitle=textoidl('u_{xp}') & norm=1
              end
              'pye-pze': begin 
                  ytitle=textoidl('u_{ze}') & specxtitle=textoidl('u_{ye}') & norm=1
              end
              'pyp-pzp': begin
                  ytitle=textoidl('u_{zp}') & specxtitle=textoidl('u_{yp}') & norm=1
              end
          end
          
          if(boxtext ne 'pxp-pyp' and boxtext ne 'pxe-pye' and boxtext ne 'pxp-pzp' $
             and boxtext ne 'pxe-pze' and boxtext ne 'pyp-pzp' and boxtext ne 'pye-pze') then begin 
              varx=(*ss(tind,dataind)).varx
              vary=(*ss(tind,dataind)).vary
              varz=(*ss(tind,dataind)).varz
              varp=(*ss(tind,dataind)).varp
;select slab
              sel=where(varx gt (xmin+xoffset)*comp and varx lt (xmax+xoffset)*comp and vary gt ymin*comp and vary lt ymax*comp) 
              if(threed eq 1) then begin
                  widget_control, widget_info(baseid,find_by_uname='Zloc'), get_value=zloc
                  sel=where(varx gt (xmin+xoffset)*comp and varx lt (xmax+xoffset)*comp and vary gt ymin*comp and vary lt ymax*comp and abs(varz-zloc*comp) lt comp) ;within one skin depth from zloc, if I have enough particles
              endif                     
              varx=varx(sel)
              varp=varp(sel)

              minx=min(varx)    
              maxx=max(varx)
              minp=min(varp)
              maxp=max(varp)
              
              nbx=400
              nbp=400
              
              rangex=(maxx-minx)
              rangep=(maxp-minp)
              
              binx=rangex/nbx
              binp=rangep/nbp
              
              h=hist_2d(varx,varp, bin1=binx,bin2=binp, min1=minx, max1=maxx, min2=minp, max2=maxp)*1.
              
              xcoord=findgen(nbx)/nbx * rangex+minx
;              xcoord=findgen(nbx)/nbx*mx*sub
              pcoord=findgen(nbp)/nbp * rangep+minp
              loadct,5,/silent
;              tvlct,r,g,b,/get
;              print,"color in wid", r
              if (oneDval and (boxtext eq 'x-game' or boxtext eq 'x-pxe' or boxtext eq 'x-pye' or boxtext eq 'x-pze')) then begin
;for electrons, I am rescaling with mime
                  widget_control, widget_info(baseid,find_by_uname='mimefld'),get_value=mimestr
                  mime=convert_to_type(mimestr,'FLOAT') & mime=mime(0)
                  pl2d, h^(.12*contr), xcoord/comp,pcoord/mime,scale=1,subxsampl=1,outxr=xr0,outyr=yr0,/xs,/ys,xr=[xmin,xmax],xoffset1=xoffset,ytitle=ytitle+textoidl('/m_i'),title=boxtext,yr=yrng
              endif else begin               
                  pl2d, h^(.12*contr), xcoord/comp,pcoord,scale=1,subxsampl=1,outxr=xr0,outyr=yr0,/xs,/ys,xr=[xmin,xmax],xoffset1=xoffset,ytitle=ytitle,title=boxtext,yr=yrng
              endelse
              loadct,4,/silent

          endif
          if(boxtext eq 'pxp-pyp' or boxtext eq 'pxe-pye' or boxtext eq 'pxp-pzp' $
             or boxtext eq 'pxe-pze' or boxtext eq 'pyp-pzp' or boxtext eq 'pye-pze') then  begin
              varx=(*ss(tind,dataind)).varx
              vary=(*ss(tind,dataind)).vary
              varz=(*ss(tind,dataind)).varz
              varp1=(*ss(tind,dataind)).varp1
              varp2=(*ss(tind,dataind)).varp2

              widget_control, widget_info(baseid,find_by_uname='FFTX0'), get_value=fftx0str
              widget_control, widget_info(baseid,find_by_uname='FFTDX'), get_value=fftdxstr
              fftx0=convert_to_type(fftx0str,'FLOAT') & fftx0=fftx0(0)
              fftdx=convert_to_type(fftdxstr,'FLOAT') & fftdx=fftdx(0)
;select slab

;print, fftx0, fftdx

;              dnx=xmin+xoffset ; in skin depths
;              upx=xmax+xoffset ; in skin depths
              dnx=fftx0+xoffset-0.5*fftdx ; in skin depths, based on the fft ranges
              upx=fftx0+xoffset+0.5*fftdx ; in skin depths, based on the fft ranges

              sel=where(varx gt dnx*comp and varx lt upx*comp and vary gt ymin*comp and vary lt ymax*comp) 
              if(threed eq 1) then begin
                  widget_control, widget_info(baseid,find_by_uname='Zloc'), get_value=zloc
                  sel=where(varx gt dnx*comp and varx lt upx*comp and vary gt ymin*comp and vary lt ymax*comp $
                            and abs(varz-zloc*comp) lt comp) ;within one skin depth from zloc, if I have enough particles
              endif                     
              varx=varp1(sel)
              varp=varp2(sel)

              minx=min(varx)    
              maxx=max(varx)
              minp=min(varp)
              maxp=max(varp)
              
              nbx=200
              nbp=200
              
              rangex=(maxx-minx)
              rangep=(maxp-minp)
              
              binx=rangex/nbx
              binp=rangep/nbp
              
              h=hist_2d(varx,varp, bin1=binx,bin2=binp, min1=minx, max1=maxx, min2=minp, max2=maxp)*1.
              
              xcoord=findgen(nbx)/nbx * rangex+minx
              pcoord=findgen(nbp)/nbp * rangep+minp
              
              if (oneDval and (boxtext eq 'pxe-pye' or boxtext eq 'pxe-pze' or boxtext eq 'pye-pze')) then begin
;for electrons, I am rescaling with mime
                  widget_control, widget_info(baseid,find_by_uname='mimefld'),get_value=mimestr
                  mime=convert_to_type(mimestr,'FLOAT') & mime=mime(0)
                  pl2d, h^(.12*contr), xcoord/mime,pcoord/mime, scale=1,subxsampl=1,outxr=xr0,outyr=yr0,/xs,/ys,ytitle=ytitle+textoidl('/m_i'),title=boxtext,iso=isoval,xtitle=specxtitle+textoidl('/m_i')	
              endif else begin               
                  pl2d, h^(.12*contr), xcoord,pcoord, scale=1,subxsampl=1,outxr=xr0,outyr=yr0,/xs,/ys,ytitle=ytitle,title=boxtext,iso=isoval,xtitle=specxtitle, yr=yrng
              endelse
	
              loadct,4,/silent 
          endif
          
          
          if(n_elements(lineloc) ne 0) then oplot, [lineloc,lineloc],[-1e6,1e6],li=2
          if(n_elements(regionloc1) ne 0 and boxtext ne "pxp-pyp" and boxtext ne "pxe-pye" $
             and boxtext ne "pxp-pzp" and boxtext ne "pxe-pze" and boxtext ne "pyp-pzp" $
             and boxtext ne "pye-pze") then oplot, [regionloc1,regionloc1],[-1e6,1e6],li=3,col=150
          if(n_elements(regionloc2) ne 0 and boxtext ne "pxp-pyp" and boxtext ne "pxe-pye" $
             and boxtext ne "pxp-pzp" and boxtext ne "pxe-pze" and boxtext ne "pyp-pzp" $
             and boxtext ne "pye-pze") then oplot, [regionloc2,regionloc2],[-1e6,1e6],li=3,col=150

      endif
      
         ;;;;;;;;;;;;;;;;; SPECTRUM plot ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      if(boxtext eq "spect") then begin
          widget_control, widget_info(baseid,find_by_uname='mimefld'),get_value=mimestr
          mime=convert_to_type(mimestr,'FLOAT') & mime=mime(0)

                                ;find the x-slice of the spectrum at the location indicated by the slider "contrast"
          sz=size((*ss(tind,dataind)).xsl) & bins=sz(1)
          thisbin=bins-1
          
          print, "thisbin=",thisbin, bins, "contr=",contr
          
          if(contr ne 1) then begin
              xloc=contr
              widget_control, widget_info(baseid,find_by_uname='Contrast'+fnumber(i)), set_value=[xloc,xmin,xmax]
          endif else begin
              xloc=(xmax-xmin)*contr+xmin
              widget_control, widget_info(baseid,find_by_uname='Contrast'+fnumber(i)), set_value=[xloc,xmin,xmax]
          endelse
          
          calc_offset
                                ;print,'xloc=',xloc,', will take data for actual x=',xloc+xoffset
                                ;print,'xmin=',xmin
          for i0=0,bins-2 do begin 
              if( (*ss(tind,dataind)).xsl(i0)/comp lt xloc+xoffset and (*ss(tind,dataind)).xsl(i0+1)/comp ge xloc+xoffset) then thisbin=i0 
          endfor
          
          logval=isoval     ; use the iso knob -- can change its label
          
          binmin=max([thisbin-2*1,0])
          binmax=min([thisbin+2*1,bins-1])
          
          sumplot=total((*ss(tind,dataind)).specp(binmin:binmax,*),1)/max([binmax-binmin,1])
;sumplot=(*ss(tind,dataind)).specp(thisbin,*)  ;no summing of bins
          
          sumplote=total((*ss(tind,dataind)).spece(binmin:binmax,*),1)/max([binmax-binmin,1])
          
          pl1d, sumplot*(*ss(tind,dataind)).gamma, $
            (*ss(tind,dataind)).gamma, xlog=logval,/ylog,$
            yr=[max([1e-9*1e3,.9*min(sumplot*(*ss(tind,dataind)).gamma)]),$
                1.1*max(sumplot*(*ss(tind,dataind)).gamma)],$ ;xr=[1e-6,30],$
            xtit=textoidl('\gamma-1'),ytit=textoidl('\gamma d N(\gamma)/d \gamma') , $
            label=convert_to_type(fix(xloc),'STRING')+textoidl('c/\omega_{pe}'),scale=1,subxsampl=1 ,xtickformat='(E10.0)' 
          
          oplot, (*ss(tind,dataind)).gamma*mime/mime, sumplot*(*ss(tind,dataind)).gamma, col=150
          oplot, (*ss(tind,dataind)).gamma, sumplote*(*ss(tind,dataind)).gamma, col=250
          
          if(oneDval eq 1 ) then oplot, (*ss(tind,dataind)).gamma*mime, $
            (*ss(tind,dataind)).specp(thisbin,*)*(*ss(tind,dataind)).gamma, col=200,li=2

            ;;;;;;;;;;;; fitting ;;;;;;;;;;;;;;;;;;;;;
          widget_control, widget_info(baseid,find_by_uname='T'),get_value=tempval
          widget_control, widget_info(baseid,find_by_uname='Norm'),get_value=normval
          temper=(10.d0)^tempval
          norm=10.^normval
          if(normval gt 0.) then begin 
              gam=(*ss(tind,dataind)).gamma+1
;3D
              func=(gam-1)*sqrt(gam^2-1)*gam*exp(-(gam-1)/temper)

;2D
              widget_control, widget_info(baseid,find_by_uname='fit2D'),get_uvalue=uval
              if(uval eq 1) then func=(gam-1)*sqrt(gam^2)*exp(-(gam-1)/temper)
              maxf=max([func])
              
              oplot,gam-1 , func/maxf*norm ,col=100, li=2
              print, "Norm=",norm, " Temper=",temper, " maxf=", maxf
          endif
                                ; Auto-fit:
          if(normval eq -1.) then begin
              gam=(*ss(tind,dataind)).gamma+1
              max_ind=where(sumplot*(*ss(tind,dataind)).gamma eq max(sumplot*(*ss(tind,dataind)).gamma))
              ind1=max([0,round(1.0*max_ind)])
              ind2=min([(size(gam))(1)-1,round(1.2*max_ind)])
              print,"auto fit betwen gamma(",ind1,")=",gam(ind1)," and gamma(",ind2,")=",gam(ind2)
              weights=replicate(1.0,ind2-ind1+1) ; can add weights to fit later
              model=[(sumplot*(*ss(tind,dataind)).gamma)(max_ind), gam(max_ind)-1.0] ; guess
              print,"trial Norm,T=",model
              widget_control, widget_info(baseid,find_by_uname='fit2D'),get_uvalue=uval
              if(uval eq 1) then begin ; 2D
                  yfit=curvefit(gam(ind1:ind2), (sumplot*(*ss(tind,dataind)).gamma)(ind1:ind2), weights, model, SIGMA, FUNCTION_NAME='T2Dfunc', itmax=100)
                  T2Dfunc,gam,model,f
              endif else begin  ; 3D
                  yfit=curvefit(gam(ind1:ind2), (sumplot*(*ss(tind,dataind)).gamma)(ind1:ind2), weights, model, SIGMA, FUNCTION_NAME='T3Dfunc', itmax=100)
                  T3Dfunc,gam,model,f
              endelse
              oplot,gam-1, f ,col=100, li=2
              widget_control, widget_info(baseid,find_by_uname='T'),set_value=alog10(model(1))
              widget_control,widget_info(baseid,find_by_uname='Norm'),set_value=alog10(max(f))
          endif

      endif
        ;;;;;;;;;;;;;Temperature Plot;;;;;;;;;;;;;;;;;
      if(boxtext eq "T" or boxtext eq "Te") then begin
          widget_control, widget_info(baseid,find_by_uname='mimefld'),get_value=mimestr
          mime=convert_to_type(mimestr,'FLOAT') & mime=mime(0)
          widget_control, widget_info(baseid,find_by_uname='SliderXMin'), get_value=xmin
          widget_control, widget_info(baseid,find_by_uname='SliderXMax'), get_value=xmax
          widget_control, widget_info(baseid,find_by_uname='compfld'),get_value=compstr
          comp=convert_to_type(compstr,'FLOAT')& comp=comp(0)
          print, "xmin=", xmin, "xmax=",xmax

          data1=smooth((*ss(tind,dataind)).Te,3)
          data2=smooth((*ss(tind,dataind)).Tp*mime,3)
          sz=max(where(data2 gt 0))
;temperatures are normalized to the injected ion energy
          data1=data1/data2(sz)
          data2=data2/data2(sz)
;if I want the ratio of temperatures
          data3=smooth((*ss(tind,dataind)).Te/((*ss(tind,dataind)).Tp*mime),3)

;          data=smooth((*ss(tind,dataind)).Tp*mime,3)

;          data = (*ss(tind,dataind)).Te/((*ss(tind,dataind)).Tp*mime)
          if(boxtext eq "T") then begin 
              if (isoval) then begin
                  if(n_elements(yrng) eq 0) then yrange1=[0,max(data3)]
                  if(n_elements(yrng) gt 0) then yrange1=yrng

                  pl1d, data3, (*ss(tind,dataind)).xsl/comp,$
                    xtit=textoidl('x, [c/\omega_{pe}]'),ytit=textoidl('T_e/T_p') , $
                    scale=1,subxsampl=1,/xs,xr=[xmin,xmax],xoffset1=xoffset,yr=yrange1,/ys
              endif else begin
                  if(n_elements(yrng) eq 0) then yrange1=[0,max([data1,data2])]
                  if(n_elements(yrng) gt 0) then yrange1=yrng

                  pl1d, data1, (*ss(tind,dataind)).xsl/comp,$
                    xtit=textoidl('x, [c/\omega_{pe}]'),ytit=textoidl('T_e,T_p') , $ 
                    scale=1,subxsampl=1,/xs,xr=[xmin,xmax],xoffset1=xoffset,yr=yrange1 ,tit=textoidl('T_e (yellow), T_p (red)')
                  oplot, (*ss(tind,dataind)).xsl/comp-xoffset,data2,col=150
              endelse
              
          endif 
          if(boxtext eq "Te") then begin 

              pl1d, data1, (*ss(tind,dataind)).xsl/comp,$
                    xtit=textoidl('x, [c/\omega_{pe}]'),ytit=textoidl('T_e') , $
                    scale=1,subxsampl=1,/xs,xr=[xmin,xmax],xoffset1=xoffset,yr=yrng ;,yr=[0,max(data3)]

          endif
                  
              if(n_elements(lineloc) ne 0) then oplot, [lineloc,lineloc],[-1e6,1e6],li=2
              if(n_elements(regionloc1) ne 0)  then oplot, [regionloc1,regionloc1],[-1e6,1e6],li=3,col=150
              if(n_elements(regionloc2) ne 0)  then oplot, [regionloc2,regionloc2],[-1e6,1e6],li=3,col=150

      endif

      if(boxtext eq "x-p_avg") then begin
          widget_control, widget_info(baseid,find_by_uname='mimefld'),get_value=mimestr
          mime=convert_to_type(mimestr,'FLOAT') & mime=mime(0)
          widget_control, widget_info(baseid,find_by_uname='SliderXMin'), get_value=xmin
          widget_control, widget_info(baseid,find_by_uname='SliderXMax'), get_value=xmax
          widget_control, widget_info(baseid,find_by_uname='compfld'),get_value=compstr
          comp=convert_to_type(compstr,'FLOAT')& comp=comp(0)
          print, "xmin=", xmin, "xmax=",xmax
          binsm=0

          if (isoval) then begin
              if (oneDval) then begin
                  pl1d, smooth((*ss(tind,dataind)).uiavg,binsm), (*ss(tind,dataind)).varx/comp,$
                    xtit=textoidl('x, [c/\omega_{pe}]'),ytit=textoidl('<u_{p}>') , $
                    scale=1,subxsampl=1,/xs,xr=[xmin,xmax],xoffset1=xoffset, yr=minmax([smooth((*ss(tind,dataind)).uiavg,binsm), smooth((*ss(tind,dataind)).viavg,binsm), smooth((*ss(tind,dataind)).wiavg,binsm),smooth((*ss(tind,dataind)).uiavg_l,binsm)]),$
                    tit=textoidl('u_{xp} (yel), u_{yp} (green), u_{zp} (red)') 
                  oplot, (*ss(tind,dataind)).varx/comp-xoffset, smooth((*ss(tind,dataind)).viavg,binsm), col=100
                  oplot, (*ss(tind,dataind)).varx/comp-xoffset, smooth((*ss(tind,dataind)).wiavg,binsm), col=150
                  oplot, (*ss(tind,dataind)).varx/comp-xoffset, smooth((*ss(tind,dataind)).uiavg_l,binsm), linesty=2
              endif else begin
                  pl1d, smooth((*ss(tind,dataind)).uiavg_l,binsm), (*ss(tind,dataind)).varx/comp,$
                    xtit=textoidl('x, [c/\omega_{pe}]'),ytit=textoidl('<u_{p}>') , $
                    scale=1,subxsampl=1,/xs,xr=[xmin,xmax],xoffset1=xoffset, yr=minmax([smooth((*ss(tind,dataind)).uiavg_l,binsm), smooth((*ss(tind,dataind)).viavg_l,binsm), smooth((*ss(tind,dataind)).wiavg_l,binsm)]),$
                    tit=textoidl('u_{xp}<0 (yel), u_{yp} (green), u_{zp} (red)') 
                  oplot, (*ss(tind,dataind)).varx/comp-xoffset, smooth((*ss(tind,dataind)).viavg_l,binsm), col=100
                  oplot, (*ss(tind,dataind)).varx/comp-xoffset, smooth((*ss(tind,dataind)).wiavg_l,binsm), col=150
              endelse
          endif else begin
              if (oneDval) then begin
                  pl1d, smooth((*ss(tind,dataind)).ueavg,binsm), (*ss(tind,dataind)).varx/comp,$
                    xtit=textoidl('x, [c/\omega_{pe}]'),ytit=textoidl('<u_{e}>') , $
                    scale=1,subxsampl=1,/xs,xr=[xmin,xmax],xoffset1=xoffset, yr=minmax([smooth((*ss(tind,dataind)).ueavg,binsm), smooth((*ss(tind,dataind)).veavg,binsm), smooth((*ss(tind,dataind)).weavg,binsm),smooth((*ss(tind,dataind)).ueavg_l,binsm)]),$
                    tit=textoidl('u_{xe} (yel), u_{ye} (green), u_{ze} (red)') 
                  oplot, (*ss(tind,dataind)).varx/comp-xoffset, smooth((*ss(tind,dataind)).veavg,binsm), col=100
                  oplot, (*ss(tind,dataind)).varx/comp-xoffset, smooth((*ss(tind,dataind)).weavg,binsm), col=150
                  oplot, (*ss(tind,dataind)).varx/comp-xoffset, smooth((*ss(tind,dataind)).ueavg_l,binsm), linesty=2
              endif else begin
                  pl1d, smooth((*ss(tind,dataind)).ueavg_l,binsm), (*ss(tind,dataind)).varx/comp,$
                    xtit=textoidl('x, [c/\omega_{pe}]'),ytit=textoidl('<u_{e}>') , $
                    scale=1,subxsampl=1,/xs,xr=[xmin,xmax],xoffset1=xoffset, yr=minmax([smooth((*ss(tind,dataind)).ueavg_l,binsm), smooth((*ss(tind,dataind)).veavg_l,binsm), smooth((*ss(tind,dataind)).weavg_l,binsm)]),$
                    tit=textoidl('u_{xe}<0 (yel), u_{ye} (green), u_{ze} (red)') 
                  oplot, (*ss(tind,dataind)).varx/comp-xoffset, smooth((*ss(tind,dataind)).veavg_l,binsm), col=100
                  oplot, (*ss(tind,dataind)).varx/comp-xoffset, smooth((*ss(tind,dataind)).weavg_l,binsm), col=150
              endelse
          endelse

          if(n_elements(lineloc) ne 0) then oplot, [lineloc,lineloc],[-1e6,1e6],li=2
          if(n_elements(regionloc1) ne 0) then oplot, [regionloc1,regionloc1],[-1e6,1e6],li=3,col=150
          if(n_elements(regionloc2) ne 0) then oplot, [regionloc2,regionloc2],[-1e6,1e6],li=3,col=150
      endif

      if(boxtext eq "x-p_stdev") then begin
          widget_control, widget_info(baseid,find_by_uname='mimefld'),get_value=mimestr
          mime=convert_to_type(mimestr,'FLOAT') & mime=mime(0)
          widget_control, widget_info(baseid,find_by_uname='SliderXMin'), get_value=xmin
          widget_control, widget_info(baseid,find_by_uname='SliderXMax'), get_value=xmax
          widget_control, widget_info(baseid,find_by_uname='compfld'),get_value=compstr
          comp=convert_to_type(compstr,'FLOAT')& comp=comp(0)
          print, "xmin=", xmin, "xmax=",xmax
          binsm=0

          if (isoval) then begin
              pl1d, smooth((*ss(tind,dataind)).uivar,binsm), (*ss(tind,dataind)).varx/comp,$
                xtit=textoidl('x, [c/\omega_{pe}]'),ytit=textoidl('<(u_{p}-<u_{p}>)^2>^{0.5}') , $
                scale=1,subxsampl=1,/xs,xr=[xmin,xmax],xoffset1=xoffset, yr=minmax([smooth((*ss(tind,dataind)).uivar,binsm), smooth((*ss(tind,dataind)).vivar,binsm), smooth((*ss(tind,dataind)).wivar,binsm)]),$
                tit=textoidl('u_{xp} (yel), u_{yp} (green), u_{zp} (red)') 
              oplot, (*ss(tind,dataind)).varx/comp-xoffset, smooth((*ss(tind,dataind)).vivar,binsm), col=100
              oplot, (*ss(tind,dataind)).varx/comp-xoffset, smooth((*ss(tind,dataind)).wivar,binsm), col=150
          endif else begin
              if (oneDval) then begin
                  pl1d, smooth((*ss(tind,dataind)).uevar,binsm)/mime, (*ss(tind,dataind)).varx/comp,$
                    xtit=textoidl('x, [c/\omega_{pe}]'),ytit=textoidl('<(u_{e}-<u_{e}>)^2>^{0.5}/m_i') , $
                    scale=1,subxsampl=1,/xs,xr=[xmin,xmax],xoffset1=xoffset, yr=minmax([smooth((*ss(tind,dataind)).uevar,binsm), smooth((*ss(tind,dataind)).vevar,binsm), smooth((*ss(tind,dataind)).wevar,binsm)])/mime,$
                    tit=textoidl('u_{xe} (yel), u_{ye} (green), u_{ze} (red)') 
                  oplot, (*ss(tind,dataind)).varx/comp-xoffset, smooth((*ss(tind,dataind)).vevar,binsm)/mime, col=100
                  oplot, (*ss(tind,dataind)).varx/comp-xoffset, smooth((*ss(tind,dataind)).wevar,binsm)/mime, col=150
              endif else begin
                  pl1d, smooth((*ss(tind,dataind)).uevar,binsm), (*ss(tind,dataind)).varx/comp,$
                    xtit=textoidl('x, [c/\omega_{pe}]'),ytit=textoidl('<(u_{e}-<u_{e}>)^2>^{0.5}') , $
                    scale=1,subxsampl=1,/xs,xr=[xmin,xmax],xoffset1=xoffset, yr=minmax([smooth((*ss(tind,dataind)).uevar,binsm), smooth((*ss(tind,dataind)).vevar,binsm), smooth((*ss(tind,dataind)).wevar,binsm)]),$
                    tit=textoidl('u_{xe} (yel), u_{ye} (green), u_{ze} (red)') 
                  oplot, (*ss(tind,dataind)).varx/comp-xoffset, smooth((*ss(tind,dataind)).vevar,binsm), col=100
                  oplot, (*ss(tind,dataind)).varx/comp-xoffset, smooth((*ss(tind,dataind)).wevar,binsm), col=150        
              endelse
          endelse

          if(n_elements(lineloc) ne 0) then oplot, [lineloc,lineloc],[-1e6,1e6],li=2
          if(n_elements(regionloc1) ne 0) then oplot, [regionloc1,regionloc1],[-1e6,1e6],li=3,col=150
          if(n_elements(regionloc2) ne 0) then oplot, [regionloc2,regionloc2],[-1e6,1e6],li=3,col=150
      endif

  endelse



;	if(i eq 0) then begin  ;set the range for x and y based on the output from the pl2d
  if(i eq 0 and !d.name eq "X" ) then add_timestamp
end

pro refresh_FFTxslider

  common widcom, winid, widid, nplots
  common basecom, baseid,baseplots,wLCol1,wLSliders,plotarr1idvert
  common datacom, ss, allname,fldname,prtname,spname,fnname
  common datasize, dims, threed
  common offset, xoffset, xoffsets

  widget_control, widget_info(baseid,find_by_uname='SliderXMin'), get_value=xmin
  widget_control, widget_info(baseid,find_by_uname='SliderXMax'), get_value=xmax
  widget_control, widget_info(baseid,find_by_uname='FFTX0'), set_slider_min=xmin(0),set_slider_max=xmax(0),set_value=0.5*(xmin(0)+xmax(0))
  widget_control, widget_info(baseid,find_by_uname='FFTDX'), set_slider_min=1,set_slider_max=(xmax-xmin),set_value=max([1,0.1*(xmax-xmin)])
end


pro refresh_FFTxsliderRange

  common widcom, winid, widid, nplots
  common basecom, baseid,baseplots,wLCol1,wLSliders,plotarr1idvert
  common datacom, ss, allname,fldname,prtname,spname,fnname
  common datasize, dims, threed
  common offset, xoffset, xoffsets

  widget_control, widget_info(baseid,find_by_uname='SliderXMin'), get_value=xmin
  widget_control, widget_info(baseid,find_by_uname='SliderXMax'), get_value=xmax
  widget_control, widget_info(baseid,find_by_uname='FFTX0'), get_value=fftx0
  widget_control, widget_info(baseid,find_by_uname='FFTDX'), get_value=fftdx
  fftx0new=min([xmax,max([xmin,fftx0])])
  fftdxnew=max([1, min([fftdx,xmax-xmin])])
  widget_control, widget_info(baseid,find_by_uname='FFTX0'), set_slider_min=xmin(0),set_slider_max=xmax(0),set_value=fftx0new
  widget_control, widget_info(baseid,find_by_uname='FFTDX'), set_slider_min=1,set_slider_max=(xmax-xmin),set_value=fftdxnew
;if (fftx0new ne fftx0 or fftdxnew ne fftdx) then replot_all
;print,xmin,xmax
end


pro T2Dfunc, x,a,f,pder
; for auto fit in 2D
  f=a(0)*(x-1.0)*sqrt(x^2.0)*exp(-(x-1.0)/a(1))
; partial derivatives
  if(n_params() ge 4) then pder=[[(x-1.0)*sqrt(x^2.0)*exp(-(x-1.0)/a(1))], [a(0)*(x-1.0)*sqrt(x^2.0)*exp(-(x-1.0)/a(1))*(x-1.0)/a(1)^2]]
end


pro T3Dfunc, x,a,f,pder
; for auto fit in 3D
  f=a(0)*(x-1.0)*sqrt(x^2.0-1.0)*x*exp(-(x-1.0)/a(1))
; partial derivatives
  if(n_params() ge 4) then pder=[[(x-1.0)*sqrt(x^2.0-1.0)*x*exp(-(x-1.0)/a(1))], [a(0)*(x-1.0)*sqrt(x^2.0-1.0)*x*exp(-(x-1.0)/a(1))*(x-1.0)/a(1)^2]]
end


pro find_num_files, finish
  goto,jump
  notthere=1
  n=1
;print, "looking for last"
  while (notthere eq 1)  do begin 
;    print , n
      rootfld="flds.tot."
      fnamefld=rootfld+fnumber(n)
      res=file_info(fnamefld)
      if(res.exists) then begin 
          n=n+1
      endif else begin 
          notthere=0
      endelse
  endwhile
jump:
;goto,kk1
  
  nfiles=1
  for n=1,999 do begin 
      rootfld="flds.tot."
      fnamefld=rootfld+fnumber(n)
      res=file_info(fnamefld)
      if(res.exists) then begin 
          nfiles=n
      endif else begin 
          
      endelse
  endfor
  n=nfiles+1
kk1:
;print, "last=",n-1
  finish=n-1
end

pro load_data, tind, dataind
  common datacom, ss, allname,fldname,prtname,spname,fnname
  common basecom, baseid,baseplots,wLCol1,wLSliders,plotarr1idvert
  common params, comp, istep, mime, qi, ppc0, gamma0, c, keions

  varname=allname(dataind)
  varname=varname(0)
;which file is it in?
  indf=where(fldname eq varname)
  indp=where(prtname eq varname)
  indsp=where(spname eq varname)
  compound=0
  if(varname eq "eb" or varname eq "rho" or varname eq "Phi" or varname eq "x-pxe" $
     or varname eq "x-pxp" or varname eq "x-pye" or $
     varname eq "x-pyp" or varname eq "x-pze" or varname eq "x-pzp" $
     or varname eq "spect" or varname eq "T" or varname eq "x-p_avg" or varname eq "x-p_stdev" $
     or varname eq "x-game" or varname eq "x-gamp" or varname eq "Te" $
     or varname eq "pxp-pyp" or varname eq "pxe-pye" or varname eq "pxp-pzp" $
     or varname eq "pxe-pze" or varname eq "pyp-pzp" or varname eq "pye-pze") $
    then compound=1             ; not a simple dataset read
   
  if(compound eq 0) then begin
      if(indf(0) ne -1) then filename="flds.tot."+fnumber(tind)
      if(indp(0) ne -1) then filename="prtl.tot."+fnumber(tind)
      if(indsp(0) ne -1) then filename="spect."+fnumber(tind)
;print, filename
      filename='../run/output/'+filename
print, '**********', filename
      fid=h5f_open('../run/output/'+filename)

      id=h5d_open(fid,varname)
      data_array=h5d_read(id)
      ss(tind,dataind)=ptr_new(data_array)	
      h5d_close,id
      h5f_close,fid
  endif else begin ;for compound data objects that may need more than one variable read
      if(varname eq 'eb') then begin 
          filename="flds.tot."+fnumber(tind)
          fid=h5f_open(filename)
          id=h5d_open(fid,"bz")
          data_array=h5d_read(id)
          h5d_close,id
          id=h5d_open(fid,"bx")
          data_array1=h5d_read(id)
          h5d_close,id
          id=h5d_open(fid,"by")
          data_array2=h5d_read(id)
          h5d_close,id
;		ss(tind,dataind)=ptr_new(data_array^2)	

          ss(tind,dataind)=ptr_new((data_array^2+data_array1^2+data_array2^2)/keions)

      endif
      if(varname eq 'rho') then begin 
          filename="flds.tot."+fnumber(tind)
          fid=h5f_open(filename)
          id=h5d_open(fid,"dens")
          data_array=h5d_read(id)
          h5d_close,id
          id=h5d_open(fid,"densi")
          data_array1=h5d_read(id)
          h5d_close,id
;		ss(tind,dataind)=ptr_new(data_array^2)	
          ss(tind,dataind)=ptr_new(2.*data_array1-data_array) ;charge density			
      endif
      if(varname eq 'Phi') then begin 
          filename="flds.tot."+fnumber(tind)
          fid=h5f_open(filename)
          id=h5d_open(fid,"ex")
          data_array=h5d_read(id)
          h5d_close,id
          data_array1=reverse(total(reverse(data_array,1),1,/cumulative),1)
          ss(tind,dataind)=ptr_new(data_array1) ;pseudo-electrostatic potential			
      endif
      if(varname eq 'x-pxe' or varname eq 'x-pxp' or varname eq "x-pye" or varname eq "x-pyp" or $
         varname eq "x-pze" or varname eq "x-pzp" or varname eq "x-game" or varname eq "x-gamp" $
         or varname eq "pxp-pyp" or varname eq "pxe-pye" or varname eq "pxp-pzp" $
         or varname eq "pxe-pze" or varname eq "pyp-pzp" or varname eq "pye-pze") then begin 
          filename="prtl.tot."+fnumber(tind)
          fid=h5f_open(filename)

;reading particle positions
          if(varname eq 'x-pxe' or varname eq "x-pye" or varname eq "x-pze" or varname eq "x-game" $
             or varname eq 'pxe-pye' or varname eq 'pxe-pze' or varname eq 'pye-pze') then $
            idx=h5d_open(fid,"xe") & idy=h5d_open(fid,"ye") & idz=h5d_open(fid,"ze")
          if(varname eq 'x-pxp' or varname eq "x-pyp" or varname eq "x-pzp" or varname eq "x-gamp" $
             or varname eq 'pxp-pyp' or varname eq 'pxp-pzp' or varname eq 'pyp-pzp') then $
            idx=h5d_open(fid,"xi") & idy=h5d_open(fid,"yi") & idz=h5d_open(fid,"zi")

          varx=h5d_read(idx)
          vary=h5d_read(idy)
          varz=h5d_read(idz)
          h5d_close,idx
          h5d_close,idy
          h5d_close,idz 

;reading momenta         
          ppspace=0
          case varname of
              'x-pxe':  id=h5d_open(fid,"ue") 
              'x-pxp':  id=h5d_open(fid,"ui")
              'x-pye':  id=h5d_open(fid,"ve")
              'x-pyp':  id=h5d_open(fid,"vi")
              'x-pze':  id=h5d_open(fid,"we")
              'x-pzp':  id=h5d_open(fid,"wi")
              'x-game':  id=h5d_open(fid,"gammae") 
              'x-gamp':  id=h5d_open(fid,"gammai")
              'pxp-pyp': begin 
                  id1=h5d_open(fid,"ui") & id2=h5d_open(fid,"vi") & ppspace=1
              end
              'pxe-pye': begin 
                  id1=h5d_open(fid,"ue") & id2=h5d_open(fid,"ve") & ppspace=1
              end
              'pxp-pzp': begin
                  id1=h5d_open(fid,"ui") & id2=h5d_open(fid,"wi") & ppspace=1
              end
              'pxe-pze': begin
                  id1=h5d_open(fid,"ue") & id2=h5d_open(fid,"we") & ppspace=1
              end
              'pyp-pzp': begin
                  id1=h5d_open(fid,"vi") & id2=h5d_open(fid,"wi") & ppspace=1
              end
              'pye-pze': begin
                  id1=h5d_open(fid,"ve") & id2=h5d_open(fid,"we") & ppspace=1
              end
          end
 
          if (not ppspace) then begin
              varp=h5d_read(id)
              ss(tind,dataind)=ptr_new(create_struct('varx',varx,'vary',vary,'varz',varz,'varp',varp))
          endif else begin
              varp1=h5d_read(id1)
              varp2=h5d_read(id2)
              ss(tind,dataind)=ptr_new(create_struct('varx',varx,'vary',vary,'varz',varz,'varp1',varp1,'varp2',varp2))
          endelse
         	
          h5f_close,fid

;in case I also need compi
;          widget_control, widget_info(baseid,find_by_uname='mimefld'),get_value=mimestr
;          mime=convert_to_type(mimestr,'FLOAT') & mime=mime(0)
;          widget_control, widget_info(baseid,find_by_uname='subsmpfld'),get_value=compstr
;          comp=convert_to_type(compstr,'FLOAT') & comp=comp(0)	
;          compi=comp*sqrt(mime)


      endif

      if (varname eq  'x-p_avg' or varname eq 'x-p_stdev') then begin
          filename="prtl.tot."+fnumber(tind)
          fid=h5f_open(filename)
          idxe=h5d_open(fid,"xe") & idye=h5d_open(fid,"ye") & idze=h5d_open(fid,"ze")
          idxi=h5d_open(fid,"xi") & idyi=h5d_open(fid,"yi") & idzi=h5d_open(fid,"zi")
          varxe=h5d_read(idxe) & varye=h5d_read(idye) & varze=h5d_read(idze)
          varxi=h5d_read(idxi) & varyi=h5d_read(idyi) & varzi=h5d_read(idzi)
          idue=h5d_open(fid,"ue") & idve=h5d_open(fid,"ve") & idwe=h5d_open(fid,"we")
          idui=h5d_open(fid,"ui") & idvi=h5d_open(fid,"vi") & idwi=h5d_open(fid,"wi")
          varue=h5d_read(idue) & varve=h5d_read(idve) & varwe=h5d_read(idwe)
          varui=h5d_read(idui) & varvi=h5d_read(idvi) & varwi=h5d_read(idwi)

;computing average momentum and dispersion, in bins of 100 cells
          cellwide=100.        ; longitudinal width (cells) of each bin
          binmax=floor(max([varxi,varxe])/cellwide)

          varx=(findgen(binmax)+0.5)*cellwide ; in cell units
          uiavg=fltarr(binmax)  ;average
          uivar=fltarr(binmax)  ;standard deviation
          viavg=fltarr(binmax)
          vivar=fltarr(binmax)
          wiavg=fltarr(binmax)
          wivar=fltarr(binmax)

          ueavg=fltarr(binmax)
          uevar=fltarr(binmax)
          veavg=fltarr(binmax)
          vevar=fltarr(binmax)
          weavg=fltarr(binmax)
          wevar=fltarr(binmax)

;only for the left-going particles (to study the upstream flow)
          uiavg_l=fltarr(binmax) 
          viavg_l=fltarr(binmax) 
          wiavg_l=fltarr(binmax) 
          ueavg_l=fltarr(binmax) 
          veavg_l=fltarr(binmax) 
          weavg_l=fltarr(binmax) 

          for i=0,binmax-1 do begin
              seli=where(varxi ge i*cellwide and varxi lt (i+1)*cellwide)
              ntoti=n_elements(seli)
              if (seli(0) ne -1) then begin 
                  uiavg(i)=total(varui(seli))/n_elements(seli)
                  uivar(i)=sqrt(total(varui(seli)^2)/ntoti-uiavg(i)^2)
                  viavg(i)=total(varvi(seli))/n_elements(seli)
                  vivar(i)=sqrt(total(varvi(seli)^2)/ntoti-viavg(i)^2)
                  wiavg(i)=total(varwi(seli))/n_elements(seli)
                  wivar(i)=sqrt(total(varwi(seli)^2)/ntoti-wiavg(i)^2)
              endif
              seli_l=where(varxi ge i*cellwide and varxi lt (i+1)*cellwide and varui lt 0.)          
              if (seli_l(0) ne -1) then begin 
                  uiavg_l(i)=total(varui(seli_l))/n_elements(seli_l)
                  viavg_l(i)=total(varvi(seli_l))/n_elements(seli_l)
                  wiavg_l(i)=total(varwi(seli_l))/n_elements(seli_l)
              endif

              sele=where(varxe ge i*cellwide and varxe lt (i+1)*cellwide)
              ntote=n_elements(sele)
              if (sele(0) ne -1) then begin 
                  ueavg(i)=total(varue(sele))/n_elements(sele)
                  uevar(i)=sqrt(total(varue(sele)^2)/ntote-ueavg(i)^2)
                  veavg(i)=total(varve(sele))/n_elements(sele)
                  vevar(i)=sqrt(total(varve(sele)^2)/ntote-veavg(i)^2)
                  weavg(i)=total(varwe(sele))/n_elements(sele)
                  wevar(i)=sqrt(total(varwe(sele)^2)/ntote-weavg(i)^2)
              endif
              sele_l=where(varxe ge i*cellwide and varxe lt (i+1)*cellwide and varue lt 0.)          
              if (sele_l(0) ne -1) then begin 
                  ueavg_l(i)=total(varue(sele_l))/n_elements(sele_l)
                  veavg_l(i)=total(varve(sele_l))/n_elements(sele_l)
                  weavg_l(i)=total(varwe(sele_l))/n_elements(sele_l)
              endif
          endfor
          
          ss(tind,dataind)=ptr_new(create_struct('varx',varx,'uiavg',uiavg,'uivar',uivar,'ueavg',ueavg,'uevar',uevar,$
                                                 'viavg',viavg,'vivar',vivar,'veavg',veavg,'vevar',vevar, $
                                                 'wiavg',wiavg,'wivar',wivar,'weavg',weavg,'wevar',wevar, $
                                                 'uiavg_l',uiavg_l,'viavg_l',viavg_l,'wiavg_l',wiavg_l, $
                                                 'ueavg_l',ueavg_l,'veavg_l',veavg_l,'weavg_l',weavg_l))
          h5f_close,fid
      endif


      if(varname eq 'spect') then begin
          filename="spect."+fnumber(tind)
          fid=h5f_open(filename)
          id=h5d_open(fid,"spece")
          spece=h5d_read(id)
          h5d_close,id
          id=h5d_open(fid,"specp")
          specp=h5d_read(id)
          h5d_close,id
          id=h5d_open(fid,"gamma")
          gamma=h5d_read(id)
          h5d_close,id
          id=h5d_open(fid,"xsl")
          xsl=h5d_read(id)
          h5d_close,id
          h5f_close,fid
          ss(tind,dataind)=ptr_new(create_struct('spece',spece,'specp',specp,'gamma',gamma,'xsl',xsl))
      endif

      if(varname eq 'T' or varname eq 'Te') then begin
         filename="spect."+fnumber(tind)
         res=file_info(filename)
         if(res.exists) then begin ; use spect file rather than particle files to get T

          fid=h5f_open(filename)
          id=h5d_open(fid,"spece")
          spece=h5d_read(id)
          h5d_close,id
          id=h5d_open(fid,"specp")
          specp=h5d_read(id)
          h5d_close,id
          id=h5d_open(fid,"gamma")
          gamma=h5d_read(id)
          h5d_close,id
          id=h5d_open(fid,"xsl")
          xsl=h5d_read(id)
          h5d_close,id
          h5f_close,fid
                                ;now find the mean of the temperature for both species
                                ;number of slices in x
          nbins=n_elements(xsl)
          Te=fltarr(nbins)
          Tp=fltarr(nbins)
                                ; integration in log gamma is much better for int_tabulated
                                ;T=<gamma>=\Int{gamma*dN/dgamma dgamma}/\Int{dN/dgamma dgamma}=
                                ;         =\Int{gamma^2*dN/dgamma dlog(gamma)}/\Int{gamma dN/dgamma dlog(gamma)}
          for nn=0,nbins-1 do begin 
              kee=(gamma)^2*spece(nn,*) ;gamma is really gamma-1
                                ;max=max(kee,ind) & Te(nn)=gamma(ind)
              kep=(gamma)^2*specp(nn,*) 
                                ;max=max(kep,ind) & Tp(nn)=gamma(ind)

              Tp(nn)=int_tabulated(alog10(gamma),kep)/int_tabulated(alog10(gamma),gamma*specp(nn,*))
              Te(nn)=int_tabulated(alog10(gamma),kee)/int_tabulated(alog10(gamma),gamma*spece(nn,*))

          endfor	
          ss(tind,dataind)=ptr_new(create_struct('Te',Te,'Tp',Tp,'xsl',xsl))
      endif else begin 
;;;;;;;;;;;;;;;;;;;;;;;;;;;alternative using prtl files;;;;;;;;;;;;;;;;;;;
;          goto,jumpalt
          filename="prtl.tot."+fnumber(tind)
          fid=h5f_open(filename)
          id=h5d_open(fid,"xi")
          xi=h5d_read(id)
          h5d_close,id
          
          id=h5d_open(fid,"xe")
          xe=h5d_read(id)
          h5d_close,id
          
          id=h5d_open(fid,"gammai")                	
          gammai=h5d_read(id)
          h5d_close,id

          id=h5d_open(fid,"gammae")                	
          gammae=h5d_read(id)
          h5d_close,id
          h5f_close, fid
          
          nbins=fix((max(xi)-min(xi))/100.)
          Te=fltarr(nbins)
          Tp=fltarr(nbins)
          xsl=fltarr(nbins)
          
          for nn=0,nbins-1 do begin
              xsl(nn)=nn*100.
              ind=where(abs(xi-xsl(nn)) lt 100.)
              if(ind(0) ne -1) then Tp(nn)=mean(gammai(ind))-1
              ind=where(abs(xe-xsl(nn)) lt 100.)
              if(ind(0) ne -1) then Te(nn)=mean(gammae(ind))-1
          endfor
          
          ss(tind,dataind)=ptr_new(create_struct('Te',Te,'Tp',Tp,'xsl',xsl))
jumpalt:
      endelse

  endif
  
endelse	

;endif
end
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
pro load_last, finish
  common datasize, dims, threed
  common basecom, baseid,baseplots,wLCol1,wLSliders,plotarr1idvert
  common datacom, ss, allname,fldname,prtname,spname,fnname
  common offset, xoffset, xoffsets
  
  ;determine last file and fluid dimensions
  find_num_files, finish  
  fid=h5f_open("flds.tot."+fnumber(finish))
  id=h5d_open(fid,"dens") 
  dspace_id=h5d_get_space(id)
  dims=h5s_get_simple_extent_dims(dspace_id)
  threed=0
  if(dims(2) ne 1) then threed=1
  print , "data has dimensions", dims  
  h5f_close,fid

  nallname=n_elements(allname)
  ss_temp=ss
  tsteps=size(ss_temp)
  tsteps=tsteps(1)
  ss=ptrarr(finish+1,nallname) ;data storage; as long as the timesteps, and with upto nallname variables
  ss(0:tsteps-1,*)=ss_temp
  xoffsets_temp=xoffsets
  xoffsets=lonarr(finish+1)
  for i=0,finish do xoffsets(i)=-1
  xoffsets(0:tsteps-1)=xoffsets_temp

  ;refresh x range, taking the xoffset into account
  widget_control, widget_info(baseid,find_by_uname='compfld'),get_value=compstr
  widget_control, widget_info(baseid,find_by_uname='subsmpfld'),get_value=substr
  comp=convert_to_type(compstr,'FLOAT')& comp=comp(0)
  sub=convert_to_type(substr,'FLOAT') & sub=sub(0)
  mx=dims(0)
  datxmax=mx*sub/comp
  calc_offset
  xr0=[0-xoffset,datxmax-xoffset] 
  xminmax=[xr0(0), xr0(1)]

  widget_control, widget_info(baseid,find_by_uname='shoffset'),get_uvalue=uval
  if (uval) then begin
      widget_control,widget_info(baseid,find_by_uname='SliderXMin'),$
        set_slider_min=min([xminmax(0),xr0(0)]),set_slider_max=max([xminmax(1),xr0(1)]),set_value=max([-150,xminmax(0)])
      widget_control,widget_info(baseid,find_by_uname='SliderXMax'),$
        set_slider_min=min([xminmax(0),xr0(0)]),set_slider_max=max([xminmax(1),xr0(1)]),set_value=min([150,xminmax(1)]) 
  
  endif else begin
      widget_control,widget_info(baseid,find_by_uname='SliderXMin'),$
        set_slider_min=xr0(0),set_slider_max=xr0(1),set_value=xr0(0)
      widget_control,widget_info(baseid,find_by_uname='SliderXMax'),$
        set_slider_min=xr0(0),set_slider_max=xr0(1),set_value=xr0(1)
  endelse

  refresh_FFTxsliderRange

end


pro wid, nmax=nmax, set=nset

  common basecom, baseid,baseplots,wLCol1,wLSliders,plotarr1idvert
  common widcom, winid, widid, nplots
  common counter, count
  common datacom, ss, allname, fldname, prtname, spname,fnname
  common datasize, dims, threed
  common offset, xoffset, xoffsets
  common params, comp, istep, mime, qi, ppc0, gamma0, c, keions

  print, '*****'
nmax=100
set=2
  loadct,4,/silent

  winid=intarr(10)              ; to reference subwindows
  widid=lonarr(10)              ; draw widget ids
  nplots=0
  count=0
  xoffset=0
;;;;;
set_plot,"x"

  if(n_elements(nmax) eq 0) then begin 
      find_num_files, finish
  endif else begin 
      finish=nmax
  endelse
;find out how many fields are in the files

  fid=h5f_open("../run/output/flds.tot."+fnumber(finish))
  
  nmem=h5g_get_nmembers(fid,"/")
  fldname=strarr(nmem)
  for nn=0,nmem-1 do begin 
      fldname(nn) = H5G_GET_MEMBER_NAME(fid, "/", nn)        
  endfor
  id=h5d_open(fid,"dens") 
  dspace_id=h5d_get_space(id)
  dims=h5s_get_simple_extent_dims(dspace_id)
  threed=0
  if(dims(2) ne 1) then threed=1
  print , "data has dimensions", dims
  
  h5f_close,fid

  fid=h5f_open("../run/output/prtl.tot."+fnumber(finish))
  nmemp=h5g_get_nmembers(fid,"/")
  prtname=strarr(nmemp)
  for nn=0,nmemp-1 do begin 
      prtname(nn) = H5G_GET_MEMBER_NAME(fid, "/", nn)        
  endfor
  h5f_close,fid
;    prtname=["dummy","dummy"]
  
  fnamespect="spect."+fnumber(finish)
  res=file_info(fnamespect)
  if(res.exists) then begin 
      fid=h5f_open(fnamespect)
      nmemsp=h5g_get_nmembers(fid,"/")
      spname=strarr(nmemsp)
      for nn=0,nmemsp-1 do begin 
          spname(nn) = H5G_GET_MEMBER_NAME(fid, "/", nn)        
      endfor
      h5f_close,fid
      ind=where(spname eq 'dens')
      spname(ind)='spdens'

  endif else begin
      spname=strarr(10)
      spname(*)="dummy"
  endelse

  fnameparam="param."+fnumber(finish)
  res=file_info(fnameparam)
 
  if(res.exists) then begin 
      ;read parameters from the file
        fid=h5f_open(fnameparam)
        id=h5d_open(fid,"c_omp") 
        comp=h5d_read(id)
        comp=comp(0)

        id=h5d_open(fid,"mi") 
        mi=h5d_read(id)
        mi=mi(0)

        id=h5d_open(fid,"me") 
        me=h5d_read(id)
        me=me(0)

        mime=mi*1./me

        id=h5d_open(fid,"istep")
        istep=h5d_read(id)
        istep=istep(0)

        id=h5d_open(fid,"interval")
        interval=h5d_read(id)
        interval=interval(0)

        id=h5d_open(fid,"qi")
        qi=h5d_read(id)
        qi=qi(0)

        id=h5d_open(fid,"ppc0")
        ppc0=h5d_read(id)
        ppc0=ppc0(0)

        id=h5d_open(fid,"gamma0")
        gamma0=h5d_read(id)
        gamma0=gamma0(0)

        if(gamma0 lt 1) then gamma0=gamma0+1

        id=h5d_open(fid,"my0")
        my=h5d_read(id)-5
        my=my(0)

        id=h5d_open(fid,"c")
        c=h5d_read(id)
        c=c(0)
        
        keions=mime*qi*ppc0/2*(1+1/mime)*c^2*(gamma0-1) ;initial kin energy of ions
        
  endif else begin 
      ;use defaults
        comp=15
        mime=1
        istep=2.
        interval=1000
        
        gamma0=15.
        qi=1 ;fix
        ppc0=4
        my=1024 ;fix
        c=0.45
        
        keions=1 ;fix

  endelse
  
  allname=['dens','densi','bx','by','bz','ex','ey','ez','eb','rho','Phi','spect','T','Te',$
           'x-game','x-gamp','x-pxe','x-pxp','x-pye','x-pyp','x-pze','x-pzp',$
           'x-p_avg','x-p_stdev','pxp-pyp','pxe-pye','pxp-pzp','pxe-pze','pyp-pzp','pye-pze','jx','jy','jz']

;,fldname,prtname,spname]

  nallname=n_elements(allname)
  fnname=['data','vect','P(k)','smooth','traj_part']
  nfnname=n_elements(fnname)

  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;---------------------------------------------------------------
; Create a base
  cd,current=currentdir
  baseID =    WIDGET_BASE( TITLE='Shock '+currentdir, /row, MBAR=wBarBase, /tlb_size_events)

  wFileMenu = Widget_Button( wBarBase, VALUE='File', /MENU)

  wQuitButton = Widget_Button( wFileMenu, VALUE='Quit', Uname='exit')
  wHelpMenu = Widget_Button( wBarBase, VALUE='Help', /HELP)
  wHelpButton = Widget_Button( wHelpMenu, VALUE='Help', Uname='Help_Program')


;Vertical set of buttons in column 1

  wLCol1= WIDGET_BASE(baseID,/COL)

  checkboxes = Widget_Base(wLCol1,Title='Checkbox Buttons', /col, /NonExclusive,ypad=0)
  button1 = Widget_Button(checkboxes, Value='slwX?',uvalue=0,uname='slowX')
  Widget_Control, button1, Set_Button=0

  buttonID3 = WIDGET_BUTTON(wLCol1, VALUE='', uvalue='Plot1',UNAME='Plot1')

  buttonID4 = WIDGET_BUTTON(wLCol1, VALUE='', Uvalue='Plot2',UNAME='Plot2')

;form=widget_base(wLCol1,/row)
  label=widget_label(wlCol1,/align_left,value="mi/me=",uname="Labelmime")
  textfield = Widget_text(wlCol1,value=strtrim(string(mime,'(I4)'),1),uname="mimefld",/editable,scr_xsize=40)

  label=widget_label(wlCol1,/align_left,value="c/omp=",uname="Labelomp")
  textfield = Widget_text(wlCol1,value=strtrim(string(comp,'(I4)'),1),uname="compfld",/editable,scr_xsize=40)

  label=widget_label(wlCol1,/align_left,value="Subsmp=",uname="Labelomp")
  textfield = Widget_text(wlCol1,value=strtrim(string(istep,'(I4)'),1),uname="subsmpfld",/editable,scr_xsize=40)

  label=widget_label(wlCol1,/align_left,value="Interv=",uname="LabelInterv")
  textfield = Widget_text(wlCol1,value=strtrim(string(interval,'(I5)'),1),uname="interval",/editable,scr_xsize=40)


  checkboxes1 = Widget_Base(wLCol1,Title='Checkbox Buttons1', /col, /NonExclusive,ypad=0)
  buttonID4 = Widget_Button(checkboxes1, Value='fit2D',uvalue=0,uname='fit2D')
  Widget_Control, buttonID4, Set_Button=0

  checkboxes2 = Widget_Base(wLCol1,Title='Checkbox Buttons2', /col, /NonExclusive,ypad=0)
  buttonID5 = Widget_Button(checkboxes2, Value='offst',uvalue=0,uname='shoffset')
  buttonIDtemp = Widget_Button(checkboxes2, Value='print',uvalue=0,uname='prnt')
  buttonIDlast = Widget_Button(checkboxes2, Value='Tlast',uvalue=0,uname='Tlast')
  buttonIDmovie = Widget_Button(checkboxes2, Value='Movie',uvalue=0,uname='Movie')
  Widget_Control, buttonID5, Set_Button=0
  Widget_Control, buttonIDtemp, Set_Button=0
  Widget_Control, buttonIDlast, Set_Button=0
  Widget_Control, buttonIDmovie, Set_Button=0

;convert_to_type(a,'FLOAT')

;sliders in column 2
  halfwidth1=85
  halfwidth2=75

  wLSliders =  WIDGET_BASE(baseID, /col)

  timeSliderPanel = WIDGET_BASE(wLSliders, /row,xpad=0,/ALIGN_CENTER)
  slidertimeID = Widget_Slider(timeSliderPanel, Value=1, Min=0, Max=1,uvalue='TimeSlider',uname='TimeSlider',title="Time",xsize=halfwidth1)
  sliderskipID = Widget_Slider(timeSliderPanel, Value=1, Min=0, Max=1,uvalue='TimeStepSlider',uname='TimeStepSlider', title="Interval",xsize=halfwidth1)


  timeButtons = WIDGET_BASE(wLSliders, /row,xpad=0,/ALIGN_CENTER)
  buttonID = WIDGET_BUTTON(timeButtons, VALUE='Replot!', uvalue='Replot',UNAME='Replot')
  buttonBackw = WIDGET_BUTTON(timeButtons, VALUE='<', uvalue='Backward',UNAME='Backward',xsize=30)
  buttonForw = WIDGET_BUTTON(timeButtons, VALUE='>', uvalue='Forward',UNAME='Forward',xsize=30)


  xSliderPanel = WIDGET_BASE(wLSliders, /row,xpad=0,/ALIGN_CENTER)
  sliderxminID = Widget_Slider(xSliderPanel, Value=0, Min=0, Max=1,uvalue='SliderXMin',uname='SliderXMin',title="Xmin, c/omp",xsize=halfwidth1)
  sliderxmaxID = Widget_Slider(xSliderPanel, Value=1, Min=0, Max=1,uvalue='SliderXMax',uname='SliderXMax',title="Xmax, c/omp",xsize=halfwidth1)

  ySliderPanel = WIDGET_BASE(wLSliders, /row,xpad=0,/ALIGN_CENTER)
  slideryminID = Widget_Slider(ySliderPanel, Value=0, Min=0, Max=1,uvalue='SliderYMin',uname='SliderYMin',title="Ymin,c/omp",xsize=halfwidth1)
  sliderymaxID = Widget_Slider(ySliderPanel, Value=1, Min=0, Max=1,uvalue='SliderYMax',uname='SliderYMax',title="Ymax,c/omp",xsize=halfwidth1)

  if(threed eq 1) then begin 
      sliderz = Widget_Slider(wLSliders, Value=0, Min=0, Max=1,uvalue='Zloc',uname='Zloc',title="Z,c/omp")
  endif

  SpectSliderPanel = WIDGET_BASE(wLSliders, /row,xpad=0,/ALIGN_CENTER)
  sliderassign1ID = my_cw_fSlider(SpectSliderPanel, Value=0, Min=0, Max=1,uvalue='T',uname='T',title="T",scroll=.1,xsize=halfwidth2)
  sliderassign2ID = my_cw_fSlider(SpectSliderPanel, Value=1, Min=0, Max=1,uvalue='Norm',uname='Norm',title="Norm",scroll=.1,xsize=halfwidth2)

  FFTSliderPanel = WIDGET_BASE(wLSliders, /row,xpad=0,/ALIGN_CENTER)
  sliderFFTX0 = Widget_Slider(FFTSliderPanel, Value=0, Min=0, Max=1,uvalue='FFTX0',uname='FFTX0',title="P(k) x0",xsize=halfwidth1)
  sliderFFTDX = Widget_Slider(FFTSliderPanel, Value=1, Min=1, Max=2,uvalue='FFTDX',uname='FFTDX',title="P(k) Dx",xsize=halfwidth1)


  basePlots =       WIDGET_BASE( baseID, /Col ,ypad=0) 

;ind2=Widget_Slider(plotarr1idvert, Value=0, Min=0, Max=1,uvalue='Slidertst2',uname='Slidertst2',title="Contrast2")

  maxplots=3
  dropmenuID=lonarr(maxplots)
  fnmenuID=lonarr(maxplots)

  nplots=0

  while nplots lt maxplots do begin ;create interface for each subplot
      plotarr1id= widget_base(basePlots,/row,ypad=0)
      plotarr1idvert= widget_base(plotarr1id,/col,ypad=0,scr_xsize=100, uname='basevertsubpl'+fnumber(nplots))
      dropmenuID(nplots) = WIDGET_COMBOBOX(plotarr1idvert,/editable, VALUE=allname, uvalue='DropMenu'+fnumber(nplots),$
                                           UNAME='DropMenu'+fnumber(nplots))
      ind1=cw_fslider(plotarr1idvert, Value=1, Min=0, Max=1,uvalue='Contrast'+fnumber(nplots), $
                      uname='Contrast'+fnumber(nplots),title="Contrast",scroll=0.05)
      checkboxes = Widget_Base(plotarr1idvert,Title='Checkbox Buttons', /row, /NonExclusive,ypad=0, $
                               uname='basecheckbx'+fnumber(nplots))
      button1 = Widget_Button(checkboxes, Value='iso',uvalue=1,uname='iso'+fnumber(nplots))
      Widget_Control, button1, Set_Button=1
      button2 = Widget_Button(checkboxes, Value='1D',uvalue=0,uname='oneD'+fnumber(nplots))
      Widget_Control, button2, Set_Button=0
      fnmenuID(nplots) = WIDGET_COMBOBOX(plotarr1idvert,/editable, VALUE=fnname, uvalue='fnMenu'+fnumber(nplots),$
                                         UNAME='fnMenu'+fnumber(nplots))


      widid(nplots) = WIDGET_DRAW(plotarr1id, RETAIN=2, XSIZE=550, YSIZE=200,uname='widdraw'+fnumber(nplots) ) ;,scr_ysize=200,/scroll)
      nplots=nplots+1
  endwhile

  widget_control, slidertimeID, set_slider_min=1, set_slider_max=finish,set_value=finish
  widget_control, sliderskipID, set_slider_min=1, set_slider_max=finish,set_value=5

  widget_control, dropmenuID(0), SET_COMBOBOX_SELECT=where(allname eq 'dens')
  widget_control, dropmenuID(1), SET_COMBOBOX_SELECT=where(allname eq 'eb')
  widget_control, widget_info(baseid,find_by_uname='Contrast'+fnumber(1)), set_value=[.25,0,1]
  widget_control, dropmenuID(2), SET_COMBOBOX_SELECT=where(allname eq 'bz')

;for shock comparison
  if(n_elements(nset) ne 0) then begin 
      if(nset eq 1) then begin 
          widget_control, dropmenuID(0), SET_COMBOBOX_SELECT=where(allname eq 'dens') ;make 1D density
          widget_control, widget_info(baseid,find_by_uname='oneD'+fnumber(0)),get_uvalue=uval
          if(uval eq 0) then uval=1 
          widget_control, widget_info(baseid,find_by_uname='oneD'+fnumber(0)),set_uvalue=uval
          Widget_Control, widget_info(baseid,find_by_uname='oneD'+fnumber(0)), Set_Button=1

          widget_control, dropmenuID(1), SET_COMBOBOX_SELECT=where(allname eq 'dens')
          widget_control, widget_info(baseid,find_by_uname='Contrast'+fnumber(1)), set_value=[1,0,1]
          widget_control, widget_info(baseid,find_by_uname='iso'+fnumber(1)),get_uvalue=uval
          if(uval eq 1) then uval=0 
          widget_control, widget_info(baseid,find_by_uname='iso'+fnumber(1)),set_uvalue=uval
          Widget_Control, widget_info(baseid,find_by_uname='iso'+fnumber(1)), Set_Button=0

          widget_control, dropmenuID(2), SET_COMBOBOX_SELECT=where(allname eq 'eb')
          widget_control, widget_info(baseid,find_by_uname='Contrast'+fnumber(2)), set_value=[.25,0,1]
          widget_control, widget_info(baseid,find_by_uname='iso'+fnumber(2)),get_uvalue=uval
          if(uval eq 1) then uval=0 
          widget_control, widget_info(baseid,find_by_uname='iso'+fnumber(2)),set_uvalue=uval
          Widget_Control, widget_info(baseid,find_by_uname='iso'+fnumber(2)), Set_Button=0

      endif

      if(nset eq 2) then begin 
          widget_control, dropmenuID(0), SET_COMBOBOX_SELECT=where(allname eq 'T') ;make 1D T
          widget_control, widget_info(baseid,find_by_uname='oneD'+fnumber(0)),get_uvalue=uval
          if(uval eq 0) then uval=1 
          widget_control, widget_info(baseid,find_by_uname='oneD'+fnumber(0)),set_uvalue=uval
          Widget_Control, widget_info(baseid,find_by_uname='oneD'+fnumber(1)), Set_Button=1


          widget_control, dropmenuID(1), SET_COMBOBOX_SELECT=where(allname eq 'x-game')
          widget_control, widget_info(baseid,find_by_uname='Contrast'+fnumber(1)), set_value=[1,0,1]
          widget_control, widget_info(baseid,find_by_uname='iso'+fnumber(1)),get_uvalue=uval
          if(uval eq 1) then uval=0 
          widget_control, widget_info(baseid,find_by_uname='iso'+fnumber(1)),set_uvalue=uval
          Widget_Control, widget_info(baseid,find_by_uname='iso'+fnumber(1)), Set_Button=0


          widget_control, dropmenuID(2), SET_COMBOBOX_SELECT=where(allname eq 'x-gamp')
          widget_control, widget_info(baseid,find_by_uname='Contrast'+fnumber(2)), set_value=[1,0,1]
          widget_control, widget_info(baseid,find_by_uname='iso'+fnumber(2)),get_uvalue=uval
          if(uval eq 1) then uval=0 
          widget_control, widget_info(baseid,find_by_uname='iso'+fnumber(2)),set_uvalue=uval
          Widget_Control, widget_info(baseid,find_by_uname='iso'+fnumber(2)), Set_Button=0

      endif

      if(nset eq 3) then begin 
          widget_control, dropmenuID(0), SET_COMBOBOX_SELECT=where(allname eq 'eb') ;make 1D eb
          widget_control, widget_info(baseid,find_by_uname='oneD'+fnumber(0)),get_uvalue=uval
          if(uval eq 0) then uval=1 
          widget_control, widget_info(baseid,find_by_uname='oneD'+fnumber(0)),set_uvalue=uval
          Widget_Control, widget_info(baseid,find_by_uname='oneD'+fnumber(0)), Set_Button=1


          widget_control, dropmenuID(1), SET_COMBOBOX_SELECT=where(allname eq 'spect')
          widget_control, widget_info(baseid,find_by_uname='Contrast'+fnumber(1)), set_value=[.5,0,1]
          widget_control, widget_info(baseid,find_by_uname='iso'+fnumber(1)),get_uvalue=uval
          print , "uval=", uval
          uval=1
          widget_control, widget_info(baseid,find_by_uname='iso'+fnumber(1)),set_uvalue=uval
          Widget_Control, widget_info(baseid,find_by_uname='iso'+fnumber(1)), Set_Button=1

          widget_control, widget_info(baseid,find_by_uname='oneD'+fnumber(1)),get_uvalue=uval
          uval=1 
          widget_control, widget_info(baseid,find_by_uname='oneD'+fnumber(1)),set_uvalue=uval
          Widget_Control, widget_info(baseid,find_by_uname='oneD'+fnumber(1)), Set_Button=1


          widget_control, dropmenuID(2), SET_COMBOBOX_SELECT=where(allname eq 'spect')
          widget_control, widget_info(baseid,find_by_uname='Contrast'+fnumber(2)), set_value=[.25,0,1]
          widget_control, widget_info(baseid,find_by_uname='iso'+fnumber(2)),get_uvalue=uval
          uval=1 
          widget_control, widget_info(baseid,find_by_uname='iso'+fnumber(2)),set_uvalue=uval
          Widget_Control, widget_info(baseid,find_by_uname='iso'+fnumber(2)), Set_Button=1

          widget_control, widget_info(baseid,find_by_uname='oneD'+fnumber(2)),get_uvalue=uval
          uval=1 
          widget_control, widget_info(baseid,find_by_uname='oneD'+fnumber(2)),set_uvalue=uval
          Widget_Control, widget_info(baseid,find_by_uname='oneD'+fnumber(2)), Set_Button=1

          widget_control, widget_info(baseid,find_by_uname='T'),set_value=[0,-4,2]
          widget_control, widget_info(baseid,find_by_uname='Norm'),set_value=[0,0,10]

      endif
      if(nset eq 4) then begin 

          widget_control, dropmenuID(0), SET_COMBOBOX_SELECT=where(allname eq 'x-pxe')
          widget_control, widget_info(baseid,find_by_uname='Contrast'+fnumber(0)), set_value=[1,0,1]
          widget_control, widget_info(baseid,find_by_uname='iso'+fnumber(0)),get_uvalue=uval
          if(uval eq 1) then uval=0 
          widget_control, widget_info(baseid,find_by_uname='iso'+fnumber(0)),set_uvalue=uval
          Widget_Control, widget_info(baseid,find_by_uname='iso'+fnumber(0)), Set_Button=0


          widget_control, dropmenuID(1), SET_COMBOBOX_SELECT=where(allname eq 'x-pye')
          widget_control, widget_info(baseid,find_by_uname='Contrast'+fnumber(1)), set_value=[1,0,1]
          widget_control, widget_info(baseid,find_by_uname='iso'+fnumber(1)),get_uvalue=uval
          if(uval eq 1) then uval=0 
          widget_control, widget_info(baseid,find_by_uname='iso'+fnumber(1)),set_uvalue=uval
          Widget_Control, widget_info(baseid,find_by_uname='iso'+fnumber(1)), Set_Button=0


          widget_control, dropmenuID(2), SET_COMBOBOX_SELECT=where(allname eq 'x-pze')
          widget_control, widget_info(baseid,find_by_uname='Contrast'+fnumber(2)), set_value=[1,0,1]
          widget_control, widget_info(baseid,find_by_uname='iso'+fnumber(2)),get_uvalue=uval
          if(uval eq 1) then uval=0 
          widget_control, widget_info(baseid,find_by_uname='iso'+fnumber(2)),set_uvalue=uval
          Widget_Control, widget_info(baseid,find_by_uname='iso'+fnumber(2)), Set_Button=0

      endif

      if(nset eq 5) then begin 

          widget_control, dropmenuID(0), SET_COMBOBOX_SELECT=where(allname eq 'x-pxp')
          widget_control, widget_info(baseid,find_by_uname='Contrast'+fnumber(0)), set_value=[1,0,1]
          widget_control, widget_info(baseid,find_by_uname='iso'+fnumber(0)),get_uvalue=uval
          if(uval eq 1) then uval=0 
          widget_control, widget_info(baseid,find_by_uname='iso'+fnumber(0)),set_uvalue=uval
          Widget_Control, widget_info(baseid,find_by_uname='iso'+fnumber(0)), Set_Button=0


          widget_control, dropmenuID(1), SET_COMBOBOX_SELECT=where(allname eq 'x-pyp')
          widget_control, widget_info(baseid,find_by_uname='Contrast'+fnumber(1)), set_value=[1,0,1]
          widget_control, widget_info(baseid,find_by_uname='iso'+fnumber(1)),get_uvalue=uval
          if(uval eq 1) then uval=0 
          widget_control, widget_info(baseid,find_by_uname='iso'+fnumber(1)),set_uvalue=uval
          Widget_Control, widget_info(baseid,find_by_uname='iso'+fnumber(1)), Set_Button=0


          widget_control, dropmenuID(2), SET_COMBOBOX_SELECT=where(allname eq 'x-pzp')
          widget_control, widget_info(baseid,find_by_uname='Contrast'+fnumber(2)), set_value=[1,0,1]
          widget_control, widget_info(baseid,find_by_uname='iso'+fnumber(2)),get_uvalue=uval
          if(uval eq 1) then uval=0 
          widget_control, widget_info(baseid,find_by_uname='iso'+fnumber(2)),set_uvalue=uval
          Widget_Control, widget_info(baseid,find_by_uname='iso'+fnumber(2)), Set_Button=0

      endif

      if(nset eq 6) then begin 
          
          widget_control, dropmenuID(0), SET_COMBOBOX_SELECT=where(allname eq 'T')
          widget_control, widget_info(baseid,find_by_uname='Contrast'+fnumber(0)), set_value=[1,0,1]
          widget_control, widget_info(baseid,find_by_uname='iso'+fnumber(0)),get_uvalue=uval
          if(uval eq 0) then uval=1
          widget_control, widget_info(baseid,find_by_uname='iso'+fnumber(0)),set_uvalue=uval
          Widget_Control, widget_info(baseid,find_by_uname='iso'+fnumber(0)), Set_Button=1


          widget_control, dropmenuID(1), SET_COMBOBOX_SELECT=where(allname eq 'Te')
          widget_control, widget_info(baseid,find_by_uname='Contrast'+fnumber(1)), set_value=[1,0,1]
          widget_control, widget_info(baseid,find_by_uname='iso'+fnumber(1)),get_uvalue=uval
          if(uval eq 0) then uval=1 
          widget_control, widget_info(baseid,find_by_uname='iso'+fnumber(1)),set_uvalue=uval
          Widget_Control, widget_info(baseid,find_by_uname='iso'+fnumber(1)), Set_Button=1


          widget_control, dropmenuID(2), SET_COMBOBOX_SELECT=where(allname eq 'T')
          widget_control, widget_info(baseid,find_by_uname='Contrast'+fnumber(2)), set_value=[1,0,1]
          widget_control, widget_info(baseid,find_by_uname='iso'+fnumber(2)),get_uvalue=uval
          if(uval eq 1) then uval=0 
          widget_control, widget_info(baseid,find_by_uname='iso'+fnumber(2)),set_uvalue=uval
          Widget_Control, widget_info(baseid,find_by_uname='iso'+fnumber(2)), Set_Button=0

      endif


  endif

  ss=ptrarr(finish+1,nallname) ;data storage; as long as the timesteps, and with upto nallname variables
  xoffsets=lonarr(finish+1)
  for i=0,finish do xoffsets(i)=-1


  WIDGET_CONTROL,baseID,/REALIZE

  WIDGET_CONTROL, widid(0), GET_VALUE=indx
  winid(0)=indx
  WIDGET_CONTROL, widid(1), GET_VALUE=indx ;
  winid(1)=indx
  WIDGET_CONTROL, widid(2), GET_VALUE=indx ;
  winid(2)=indx

  WSET, winid(0)

;determine minmax for the first one
;sz=size(data2d)
;	if(n_elements(x) eq 0) then begin 
;		x=findgen(sz(1)) *subxsampl / scale 
;		y=findgen(sz(2)) *subysampl / scale


  xmanager,'wid',baseID,event_handler='wid_events',/no_block
;replot_all
end
