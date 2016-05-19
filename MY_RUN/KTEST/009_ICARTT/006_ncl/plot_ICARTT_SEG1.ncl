;================================================;
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl"   
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_csm.ncl"   
load "MESONHtools_ex.ncl"   
; ================================================;
begin
;=================================================;
; open file and read in data
;=================================================;
  dir_input1="$SRC_MESONH/MY_RUN/KTEST/009_ICARTT/005_cdf"
  a = addfile(dir_input1+"/ICART.1.SEG01.012cvZCL.nc", "r")
;-------------------------------------------------;
; Time values and units.
;-------------------------------------------------;
  time = a ->time
  time@units = "seconds since 2002-01-0100:00:00"
; Convert to UTC time.
  utc_date = ut_calendar(time, 0)
; Store return information into more meaningful variables.
  year   = floattointeger(utc_date(:,0))    ; Convert to integer for
  month  = floattointeger(utc_date(:,1))    ; use sprinti
  day    = floattointeger(utc_date(:,2))
  hour   = floattointeger(utc_date(:,3))
  minute = floattointeger(utc_date(:,4))
  second = utc_date(:,5)
; Array to hold month abbreviations. Don't store anything in index
; '0' (i.e. let index 1=Jan, 2=Feb, ..., index 12=Dec).
  month_abbr = (/"","Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep", \
                   "Oct","Nov","Dec"/)
  date_str = sprinti("%0.2iZ ", hour) + sprinti("%0.2i ", day) + \
             month_abbr(month) + " "  + sprinti("%0.4i", year)
  hh = sprinti("%0.2i", hour)
;-------------------------------------------------;
; Read data.
;-------------------------------------------------;
  rc = a->RCT(:,:,:,:)*1.e3
  rc@long_name="Cloud mixing ratio"
  rc@units="g/kg"
  rc!0 = "time"
  rc&time=hh
  rc!1 = "alt"
  rc&alt = a->vertical_levels(:)
;
  o3 = a->O3T(:,:,:,:)*1.e9
  o3@long_name="Ozone"
  o3@units="ppbv"
  o3!0="time"
  o3&time=hh
  o3!1 = "alt"
  o3&alt = a->vertical_levels(:)
;
  co = a->COT(:,:,:,:)*1.e9
  co@long_name="carbon monoxide"
  co@units="ppbv"
  co!0="time"
  co&time=hh
  co!1 = "alt"
  co&alt = a->vertical_levels(:)
;
;  nlevel = 7 ; 1500 m
  nlevel = 5  ; 1000 m
  
;=================================================;
; PLOT
;=================================================;

  figname ="zsection_1000"
  wks  = gsn_open_wks("ps",figname)   ; open a ncgm file
  gsn_define_colormap(wks,"WhBlGrYeRe") ; Choose colormap

  res                 = True          
  res@gsnDraw   		= False         ; don't draw yet
  res@gsnFrame  		= False         ; don't advance frame yet

; X-axis title (tiY)                              
  res@tiXAxisFontHeightF = 0.018  ; font height
  res@tiXAxisFont        = 21     ; font index
  res@tiXAxisString      = "longitude"  ; string to use as the X-Axis title

; Y-axis title (tiY)
  res@tiYAxisFontHeightF = 0.018  ; font height
  res@tiYAxisFont        = 21     ; font index
  res@tiYAxisString      = "latitude"  ; string to use as the Y-Axis title

; BW
  res@cnLinesOn            = False           
  res@cnFillOn             = True        
  res@gsnSpreadColors      = True 
;
; label bar (lb)
;  res@lbAutoManage       = False  
;  res@lbBottomMarginF    = 0.4        ; offset
;  res@lbOrientation      = "Vertical"

; Map ressources 
;  res@mpDataBaseVersion    	= "HighRes"	; choose highres map data version (must be donwloaded)
;  res@mpDataBaseVersion    	= "MediumRes"	; choose highres map data version (must be donwloaded)
  res@mpGridAndLimbOn   	= True             ; turn on lat/lon lines
  res@mpGridLatSpacingF 	= 10.              ; spacing for lat lines
  res@mpGridLonSpacingF 	= 10.              ; spacing for lon lines

  res@mpGeophysicalLineColor = "Black"  ; default value in lowres
  res@mpNationalLineColor    = "Black"  ; idem
  res@mpUSStateLineColor     = "Black"  ; idem
  res@mpGridLineColor        = "Black"
  res@mpLimbLineColor        = "Black"
  res@mpPerimLineColor       = "Black"

; Use MESONHtools procedure to set map resources
  dimrc=dimsizes(rc)
  IMAX=dimrc(3)
  JMAX=dimrc(2)  
  latmod1 = new((/JMAX,IMAX/),"double")
  latmod1(:,:)=0.
  lonmod1 = new((/JMAX,IMAX/),"double")
  cormod1 = new((/2,2/),"integer")
  cormod1(:,:)=0
  MESONH_map_c (a, res, latmod1, lonmod1, cormod1)  
  
  res@gsnCenterString="heure="+hh(0)

; plot cloud mixing ratio
  res@cnLevelSelectionMode = "AutomaticLevels" 
;  res@cnLevelSelectionMode = "ExplicitLevels"
;  res@cnLevels =     (/1.e-5/)
  plot_rc = gsn_csm_contour_map(wks,rc(0,nlevel,:,:),res)
  draw(plot_rc)
  frame(wks)

; plot ozone
  res@cnLevelSelectionMode = "ExplicitLevels" ; method for selecting the contour
  res@cnLevels     = (/15.,20., 25., 35., 40., 45., 50., 55., 60., 65./)
  res@cnFillColors = (/2,6,12,40,45,51,62,72,80,89,102,2/) ; color of a contour
;  res@cnLevelSelectionMode = "AutomaticLevels" 
  plot_o3 = gsn_csm_contour_map(wks,o3(0,nlevel,:,:),res)
  draw(plot_o3)
  frame(wks)

; plot co
;  res@cnLevelSelectionMode = "ExplicitLevels" ; method for selecting the contour
;  res@cnLevels     = (/15.,20., 25., 35., 40., 45., 50., 55., 60., 65./)
;  res@cnFillColors = (/2,6,12,40,45,51,62,72,80,89,102,2/) ; color of a contour
  res@cnLevelSelectionMode = "AutomaticLevels" 
  plot_co = gsn_csm_contour_map(wks,co(0,nlevel,:,:),res)
  draw(plot_co)
  frame(wks)

  res@gsnCenterString="heure="+hh(1)

; plot cloud mixing ratio
  res@cnLevelSelectionMode = "AutomaticLevels" 
;  res@cnLevelSelectionMode = "ExplicitLevels"
;  res@cnLevels =     (/1.e-5/)
  plot_rc1 = gsn_csm_contour_map(wks,rc(1,nlevel,:,:),res)
  draw(plot_rc1)
  frame(wks)

; plot ozone
  res@cnLevelSelectionMode = "ExplicitLevels" ; method for selecting the contour
  res@cnLevels     = (/15.,20., 25., 35., 40., 45., 50., 55., 60., 65./)
  res@cnFillColors = (/2,6,12,40,45,51,62,72,80,89,102,2/) ; color of a contour
;  res@cnLevelSelectionMode = "AutomaticLevels" 
  plot_o31 = gsn_csm_contour_map(wks,o3(1,nlevel,:,:),res)
  draw(plot_o31)
  frame(wks)

; plot co
;  res@cnLevelSelectionMode = "ExplicitLevels" ; method for selecting the contour
;  res@cnLevels     = (/15.,20., 25., 35., 40., 45., 50., 55., 60., 65./)
;  res@cnFillColors = (/2,6,12,40,45,51,62,72,80,89,102,2/) ; color of a contour
  res@cnLevelSelectionMode = "AutomaticLevels" 
  plot_co1 = gsn_csm_contour_map(wks,co(1,nlevel,:,:),res)
  draw(plot_co1)
  frame(wks)

;;;;;;;;;;;;;;;;;;;;;;;;

end



