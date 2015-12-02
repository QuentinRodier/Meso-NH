;================================================;
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl"   
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_csm.ncl"   
; ================================================;
begin
;=================================================;
; open file and read in data
;=================================================;
  a = addfile("ICART.1.SEG01.001.nc4", "r")
  a2 = addfile("ICART.1.SEG01.002.nc4", "r")

;=================================================;
; Get informations on variable sizes
; dims are dims-2 to remove non-physical values
;=================================================;
  mdims = getfilevardimsizes(a,"THT") ; get dimension sizes
  nd = dimsizes(mdims)
  imax=mdims(nd-1)-2
  jmax=mdims(nd-2)-2
  kmax=mdims(nd-3)-2

;-------------------------------------------------;
; Read data.
;-------------------------------------------------;
  lat2d = a->LAT(1:jmax,1:imax)
  lat2d@units="degrees_north"
  lon2d = a->LON(1:jmax,1:imax)
  lon2d@units="degrees_east"

zs  = a->ZS(1:jmax,1:imax) ; ZS
zs@long_name="Orography"
zs@units="m"
zs@lat2d = lat2d
zs@lon2d = lon2d

printMinMax(zs,0)

  rc_t1 = a->RCT(1:kmax,1:jmax,1:imax)*1.e3
  rc_t1@long_name="Cloud mixing ratio"
  rc_t1@units="g/kg"
  rc_t1@lat2d=lat2d
  rc_t1@lon2d=lon2d
printMinMax(rc_t1,0)

;
  o3_t1 = a->O3T(1:kmax,1:jmax,1:imax)*1.e9
  o3_t1@long_name="Ozone"
  o3_t1@units="ppbv"
  o3_t1@lat2d=lat2d
  o3_t1@lon2d=lon2d

;
  co_t1 = a->COT(1:kmax,1:jmax,1:imax)*1.e9
  co_t1@long_name="carbon monoxide"
  co_t1@units="ppbv"
  co_t1@lat2d=lat2d
  co_t1@lon2d=lon2d
;
;
  rc_t2 = a2->RCT(1:kmax,1:jmax,1:imax)*1.e3
  rc_t2@long_name="Cloud mixing ratio"
  rc_t2@units="g/kg"
  rc_t2@lat2d=lat2d
  rc_t2@lon2d=lon2d

;
  o3_t2 = a2->O3T(1:kmax,1:jmax,1:imax)*1.e9
  o3_t2@long_name="Ozone"
  o3_t2@units="ppbv"
  o3_t2@lat2d=lat2d
  o3_t2@lon2d=lon2d

;
  co_t2 = a2->COT(1:kmax,1:jmax,1:imax)*1.e9
  co_t2@long_name="carbon monoxide"
  co_t2@units="ppbv"
  co_t2@lat2d=lat2d
  co_t2@lon2d=lon2d

;-----------------------------------------------;
;=================================================;
; On calcule l'altitude des champs modèle
;=================================================;

zhat= a->ZHAT(1:kmax+1)

; Unstagger zhat (from grid 4 to 1)
    nzhat=new(kmax,double)
    do k=0,kmax-1
     nzhat(k)=(zhat(k)+zhat(k+1))/2.
    end do

; Create Z3D == ALT
    alt=new(dimsizes(rc_t1),double)
    zcoef=1.-zs/nzhat(kmax-1)

    do i=0,imax-1
    do j=0,jmax-1
       alt(:,j,i) = nzhat*zcoef(j,i)+zs(j,i)
    end do
    end do

alt@lat2d = lat2d
alt@lon2d = lon2d




;-----------------------------------------------;
; Set map projection ressources using projection parameters
;-----------------------------------------------;
; Read projection parameters
; --------------------
    RPK  = a->RPK
    BETA = a->BETA
    LON0 = a->LON0

  resmap=True
  if (RPK.gt.0)
; ---------------------------
  ;   Lambert  projection from north pole
; ---------------------------
   resmap@mpProjection          = "LambertConformal"     ; projection
   pole                         = 1    ; projection for north hemisphere
   resmap@mpLambertParallel1F   = pole*asin(RPK)*180/3.14 ; with pole=1 for north hemisphere and -1 for south hemisphere
   resmap@mpLambertParallel2F   = resmap@mpLambertParallel1F  ; ncl adds from grib file
   resmap@mpLambertMeridianF    = LON0      ; ncl adds from grib file
  end if

  if (RPK.lt.0)
; ---------------------------
  ;   Lambert projection from south pole
; ---------------------------
   resmap@mpProjection          = "LambertConformal"     ; projection
   pole                         = -1                     ; projection for south hemisphere
   resmap@mpLambertParallel1F   = pole*asin(RPK)*180/3.14 ; with pole=1 for north hemisphere and -1 for south hemisphere
   resmap@mpLambertParallel2F   = resmap@mpLambertParallel1F  ; ncl adds from grib file
   resmap@mpLambertMeridianF    =  LON0      ; ncl adds from grib file
  end if

  if (RPK.eq.1)
; ---------------------------
  ;   Stereographic projection north
; ---------------------------
    resmap@mpProjection = "Stereographic"
    resmap@mpCenterLonF           = LON0
    resmap@mpCenterRotF           = BETA
    resmap@mpCenterLatF           = 90
  end if

  if (RPK.eq.-1)
; ---------------------------
  ;   Stereographic projection south
; ---------------------------
    resmap@mpProjection = "Stereographic"
    resmap@mpCenterLonF           = LON0
    resmap@mpCenterRotF           = BETA
    resmap@mpCenterLatF           = -90
  end if

  if (RPK.eq.0) then
; ---------------------------
  ;   Mercator projection
; ---------------------------
    resmap@mpProjection = "Mercator"
  end if

 print("Map projection="+resmap@mpProjection)

; Defining the corners for projection
; --------------------------------
  resmap@mpLimitMode            = "Corners"
  resmap@mpLeftCornerLatF       = lat2d(0,0)
  resmap@mpLeftCornerLonF       = lon2d(0,0)
  resmap@mpRightCornerLatF     = lat2d(jmax-1,imax-1)
  resmap@mpRightCornerLonF     = lon2d(jmax-1,imax-1)
  
;=================================================;
; PLOT
;=================================================;
; interpolation des champs a 1250 m
rc_t1_plane = wrf_user_intrp3d(rc_t1,alt,"h",1250,0.,False)
printMinMax(rc_t1_plane,0)
printMinMax(alt,0)

rc_t2_plane = wrf_user_intrp3d(rc_t2,alt,"h",1250,0.,False)
co_t1_plane = wrf_user_intrp3d(co_t1,alt,"h",1250,0.,False)
co_t2_plane = wrf_user_intrp3d(co_t2,alt,"h",1250,0.,False)
o3_t1_plane = wrf_user_intrp3d(o3_t1,alt,"h",1250,0.,False)
o3_t2_plane = wrf_user_intrp3d(o3_t2,alt,"h",1250,0.,False)


  figname ="zsection_1250"
  wks  = gsn_open_wks("ps",figname)   ; open a ncgm file
  gsn_define_colormap(wks,"WhBlGrYeRe") ; Choose colormap

  res                 = resmap          
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

  
  res@gsnCenterString="heure=19"

; plot cloud mixing ratio
  res@cnLevelSelectionMode = "ExplicitLevels"
  res@cnLevels =     (/0.01,0.015,0.02,0.025,0.03,0.035,0.04,0.045,0.05,0.055,0.06/)
  res@cnFillColors = (/2,6,12,40,45,51,62,72,80,89,96,101/) ; color of a contour
  plot_rc = gsn_csm_contour_map(wks,rc_t1_plane(:,:),res)
  draw(plot_rc)
  frame(wks)
  delete(res@cnLevels)
  delete(res@cnFillColors)

; plot ozone
  res@cnLevelSelectionMode = "ExplicitLevels" ; method for selecting the contour
  res@cnLevels     = (/15.,20., 25., 35., 40., 45., 50., 55., 60., 65./)
  res@cnFillColors = (/2,6,12,40,45,51,62,72,80,89,96/) ; color of a contour
;  res@cnLevelSelectionMode = "AutomaticLevels" 
  plot_o3 = gsn_csm_contour_map(wks,o3_t1_plane(:,:),res)
  draw(plot_o3)
  frame(wks)
  delete(res@cnLevels)
  delete(res@cnFillColors)

; plot co
  res@cnLevelSelectionMode = "ExplicitLevels" ; method for selecting the contour
  res@cnLevels     = (/110.,112.5,115.,117.5,120.,122.5,125.,127.5,130.,132.5,135./)
  res@cnFillColors = (/2,6,12,40,45,51,62,72,80,89,96,101/) ; color of a contour
;  res@cnLevelSelectionMode = "AutomaticLevels" 
  plot_co = gsn_csm_contour_map(wks,co_t1_plane(:,:),res)
  draw(plot_co)
  frame(wks)
  delete(res@cnLevels)
  delete(res@cnFillColors)

  res@gsnCenterString="heure=20"

; plot cloud mixing ratio
  res@cnLevelSelectionMode = "ExplicitLevels"
  res@cnLevels =     (/0.01,0.015,0.02,0.025,0.03,0.035,0.04,0.045,0.05,0.055,0.06/)
  res@cnFillColors = (/2,6,12,40,45,51,62,72,80,89,96,101/) ; color of a contour
  plot_rc1 = gsn_csm_contour_map(wks,rc_t2_plane(:,:),res)
  draw(plot_rc1)
  frame(wks)
  delete(res@cnLevels)
  delete(res@cnFillColors)

; plot ozone
  res@cnLevelSelectionMode = "ExplicitLevels" ; method for selecting the contour
  res@cnLevels     = (/15.,20., 25., 35., 40., 45., 50., 55., 60., 65./)
  res@cnFillColors = (/2,6,12,40,45,51,62,72,80,89,96/) ; color of a contour
;  res@cnLevelSelectionMode = "AutomaticLevels" 
  plot_o31 = gsn_csm_contour_map(wks,o3_t2_plane(:,:),res)
  draw(plot_o31)
  frame(wks)
  delete(res@cnLevels)
  delete(res@cnFillColors)

; plot co
  res@cnLevelSelectionMode = "ExplicitLevels" ; method for selecting the contour
  res@cnLevels     = (/110.,112.5,115.,117.5,120.,122.5,125.,127.5,130.,132.5,135./)
  res@cnFillColors = (/2,6,12,40,45,51,62,72,80,89,96,101/) ; color of a contour
;  res@cnLevelSelectionMode = "AutomaticLevels" 
  plot_co1 = gsn_csm_contour_map(wks,co_t2_plane(:,:),res)
  draw(plot_co1)
  frame(wks)

;;;;;;;;;;;;;;;;;;;;;;;;

end



