;================================================;
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl"   
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_csm.ncl"   
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/contributed.ncl"  
load "$NCARG_ROOT/lib/ncarg/nclscripts/wrf/WRFUserARW.ncl"
; ================================================;
begin
;=================================================;
; open file and read in data
;=================================================;
;  a11 = addfile("ICART.1.SEG02.001dg.nc", "r")
;  a12 = addfile("ICART.1.SEG02.002dg.nc", "r")
  a13 = addfile("ICART.1.SEG02.003dg.nc", "r")
;  a21 = addfile("ICART.2.SEG02.001dg.nc", "r")
;  a22 = addfile("ICART.2.SEG02.002dg.nc", "r")
  a23 = addfile("ICART.2.SEG02.003dg.nc", "r")

;=================================================;
; Get informations on variable sizes for domain 1
; dims are dims-2 to remove non-physical values
;=================================================;
  jphext1  = a13->JPHEXT
  mdims1 = getfilevardimsizes(a13,"UT") 
  nd1 = dimsizes(mdims1)
  imax1=mdims1(nd1-1)-2*jphext1
  jmax1=mdims1(nd1-2)-2*jphext1
  kmax1=mdims1(nd1-3)-2

;=================================================;
; Get informations on variable sizes for domain 2
; dims are dims-2 to remove non-physical values
;=================================================;
  jphext2  = a23->JPHEXT
  mdims2 = getfilevardimsizes(a23,"UT") 
  nd2 = dimsizes(mdims2)
  imax2=mdims2(nd2-1)-2*jphext2
  jmax2=mdims2(nd2-2)-2*jphext2
  kmax2=mdims2(nd2-3)-2

;-------------------------------------------------;
; Read data domain 1
;-------------------------------------------------;
  lat2d1 = a13->LAT(jphext1:jmax1+jphext1-1,jphext1:imax1+jphext1-1)
  lat2d1@units="degrees_north"
  lon2d1 = a13->LON(jphext1:jmax1+jphext1-1,jphext1:imax1+jphext1-1)
  lon2d1@units="degrees_east"
;
  zs1  = a13->ZS(jphext1:jmax1+jphext1-1,jphext1:imax1+jphext1-1) ; ZS
  zs1@long_name="Orography"
  zs1@units="m"
  zs1@lat2d = lat2d1
  zs1@lon2d = lon2d1
printMinMax(zs1,0)
;
;
  rc1_t3 = a13->MRC(:,1:kmax1,jphext1:jmax1+jphext1-1,jphext1:imax1+jphext1-1)
  rc1_t3@long_name="Cloud mixing ratio"
  rc1_t3@units="g/kg"
  rc1_t3@lat2d = lat2d1
  rc1_t3@lon2d = lon2d1
printMinMax(rc1_t3,0)
;
  HCHO1_t3 = a13->HCHO(:,1:kmax1,jphext1:jmax1+jphext1-1,jphext1:imax1+jphext1-1)
  HCHO1_t3@long_name="HCHO"
  HCHO1_t3@units="ppbv"
  HCHO1_t3@lat2d=lat2d1
  HCHO1_t3@lon2d=lon2d1
;
  OH1_t3 = a13->OH(:,1:kmax1,jphext1:jmax1+jphext1-1,jphext1:imax1+jphext1-1)
  OH1_t3 = OH1_t3*1e+3
  OH1_t3@long_name="OH"
  OH1_t3@units="pptv"
  OH1_t3@lat2d=lat2d1
  OH1_t3@lon2d=lon2d1
;
  O31_t3 = a13->O3(:,1:kmax1,jphext1:jmax1+jphext1-1,jphext1:imax1+jphext1-1)
  O31_t3@long_name="O3"
  O31_t3@units="ppbv"
  O31_t3@lat2d=lat2d1
  O31_t3@lon2d=lon2d1
;  
  SO21_t3 = a13->SO2(:,1:kmax1,jphext1:jmax1+jphext1-1,jphext1:imax1+jphext1-1)
  SO21_t3@long_name="SO2"
  SO21_t3@units="ppbv"
  SO21_t3@lat2d=lat2d1
  SO21_t3@lon2d=lon2d1
;
  H2O21_t3 = a13->H2O2(:,1:kmax1,jphext1:jmax1+jphext1-1,jphext1:imax1+jphext1-1)
  H2O21_t3@long_name="H2O2"
  H2O21_t3@units="ppbv"
  H2O21_t3@lat2d=lat2d1
  H2O21_t3@lon2d=lon2d1
;
;-------------------------------------------------;
; Read data domain 2 : keep only net budget
;-------------------------------------------------;
  lat2d2 = a23->LAT(jphext2:jmax2+jphext2-1,jphext2:imax2+jphext2-1)
  lat2d2@units="degrees_north"
  lon2d2 = a23->LON(jphext2:jmax2+jphext2-1,jphext2:imax2+jphext2-1)
  lon2d2@units="degrees_east"
;
  zs2  = a23->ZS(jphext2:jmax2+jphext2-1,jphext2:imax2+jphext2-1) ; ZS
  zs2@long_name="Orography"
  zs2@units="m"
  zs2@lat2d = lat2d2
  zs2@lon2d = lon2d2
printMinMax(zs1,0)
;
  rc2_t3 = a23->MRC(:,1:kmax2,jphext2:jmax2+jphext2-1,jphext2:imax2+jphext2-1)
  rc2_t3@long_name="Cloud mixing ratio"
  rc2_t3@units="g/kg"
  rc2_t3@lat2d = lat2d2
  rc2_t3@lon2d = lon2d2
printMinMax(rc2_t3,0)
;
  HCHO2_t3 = a23->HCHO(:,1:kmax2,jphext2:jmax2+jphext2-1,jphext2:imax2+jphext2-1)
  HCHO2_t3@long_name="HCHO"
  HCHO2_t3@units="ppbv"
  HCHO2_t3@lat2d=lat2d2
  HCHO2_t3@lon2d=lon2d2
;
  OH2_t3 = a23->OH(:,1:kmax2,jphext2:jmax2+jphext2-1,jphext2:imax2+jphext2-1)
  OH2_t3 = OH2_t3*1e+3
  OH2_t3@long_name="OH"
  OH2_t3@units="pptv"
  OH2_t3@lat2d=lat2d2
  OH2_t3@lon2d=lon2d2
;
  SO22_t3 = a23->SO2(:,1:kmax2,jphext2:jmax2+jphext2-1,jphext2:imax2+jphext2-1)
  SO22_t3@long_name="SO2"
  SO22_t3@units="ppbv"
  SO22_t3@lat2d=lat2d2
  SO22_t3@lon2d=lon2d2
;
  H2O22_t3 = a23->H2O2(:,1:kmax2,jphext2:jmax2+jphext2-1,jphext2:imax2+jphext2-1)
  H2O22_t3@long_name="H2O2"
  H2O22_t3@units="ppbv"
  H2O22_t3@lat2d=lat2d2
  H2O22_t3@lon2d=lon2d2
;
  wHCHO2_t3 = a23->WC_HCHO(:,1:kmax2,jphext2:jmax2+jphext2-1,jphext2:imax2+jphext2-1)
  wHCHO2_t3@long_name="WC_HCHO"
  wHCHO2_t3@units="M"
  wHCHO2_t3@lat2d=lat2d2
  wHCHO2_t3@lon2d=lon2d2
printMinMax(wHCHO2_t3,0)
;
  wOH2_t3 = a23->WC_OH(:,1:kmax2,jphext2:jmax2+jphext2-1,jphext2:imax2+jphext2-1)
  wOH2_t3@long_name="WC_OH"
  wOH2_t3@units="M"
  wOH2_t3@lat2d=lat2d2
  wOH2_t3@lon2d=lon2d2
printMinMax(wOH2_t3,0)
;
  wSO22_t3 = a23->WC_SO2(:,1:kmax2,jphext2:jmax2+jphext2-1,jphext2:imax2+jphext2-1)
  wSO22_t3@long_name="WC_SO2"
  wSO22_t3@units="M"
  wSO22_t3@lat2d=lat2d2
  wSO22_t3@lon2d=lon2d2
printMinMax(wSO22_t3,0)
;
  wH2O22_t3 = a23->WC_H2O2(:,1:kmax2,jphext2:jmax2+jphext2-1,jphext2:imax2+jphext2-1)
  wH2O22_t3@long_name="WC_H2O2"
  wH2O22_t3@units="M"
  wH2O22_t3@lat2d=lat2d2
  wH2O22_t3@lon2d=lon2d2
printMinMax(wH2O22_t3,0)
;
  wSULF2_t3 = a23->WC_SULF(:,1:kmax2,jphext2:jmax2+jphext2-1,jphext2:imax2+jphext2-1)
  wSULF2_t3@long_name="WC_SULF"
  wSULF2_t3@units="M"
  wSULF2_t3@lat2d=lat2d2
  wSULF2_t3@lon2d=lon2d2
printMinMax(wSULF2_t3,0)
;
  wORA12_t3 = a23->WC_ORA1(:,1:kmax2,jphext2:jmax2+jphext2-1,jphext2:imax2+jphext2-1)
  wORA12_t3@long_name="WC_ORA1"
  wORA12_t3@units="M"
  wORA12_t3@lat2d=lat2d2
  wORA12_t3@lon2d=lon2d2
printMinMax(wORA12_t3,0)
;
;=================================================;
; On calcule l'altitude des champs modèle 
;=================================================;

zhat= a13->ZHAT(1:kmax1+1)

; Unstagger zhat (from grid 4 to 1)
  nzhat=new(kmax1,double)
  do k=0,kmax1-1
    nzhat(k)=(zhat(k)+zhat(k+1))/2.
  end do

; Create Z3D == ALT for domain 1
  alt1=new(dimsizes(HCHO1_t3),double)
  zcoef1=1.-zs1/nzhat(kmax1-1)
  do i=0,imax1-1
  do j=0,jmax1-1
     alt1(0,:,j,i) = nzhat*zcoef1(j,i)+zs1(j,i)
  end do
  end do
  alt1@lat2d = lat2d1
  alt1@lon2d = lon2d1

; Create Z3D == ALT for domain 2
  alt2=new(dimsizes(HCHO2_t3),double)
  zcoef2=1.-zs2/nzhat(kmax2-1)
  do i=0,imax2-1
  do j=0,jmax2-1
     alt2(0,:,j,i) = nzhat*zcoef2(j,i)+zs2(j,i)
  end do
  end do
  alt2@lat2d = lat2d2
  alt2@lon2d = lon2d2

;=================================================;
; Set map projection ressources using projection parameters
; Domaine 1
;=================================================;
; Read projection parameters
; --------------------
    RPK1  = a13->RPK
    BETA1 = a13->BETA
    LON01 = a13->LON0

  resmap1=True
  if (RPK1.gt.0)
; ---------------------------
  ;   Lambert  projection from north pole
; ---------------------------
   resmap1@mpProjection          = "LambertConformal"     ; projection
   pole                         = 1    ; projection for north hemisphere
   resmap1@mpLambertParallel1F   = pole*asin(RPK1)*180/3.14 ; with pole=1 for north hemisphere and -1 for south hemisphere
   resmap1@mpLambertParallel2F   = resmap1@mpLambertParallel1F  ; ncl adds from grib file
   resmap1@mpLambertMeridianF    = LON01      ; ncl adds from grib file
  end if

  if (RPK1.lt.0)
; ---------------------------
  ;   Lambert projection from south pole
; ---------------------------
   resmap1@mpProjection          = "LambertConformal"     ; projection
   pole                         = -1                     ; projection for south hemisphere
   resmap1@mpLambertParallel1F   = pole*asin(RPK1)*180/3.14 ; with pole=1 for north hemisphere and -1 for south hemisphere
   resmap1@mpLambertParallel2F   = resmap1@mpLambertParallel1F  ; ncl adds from grib file
   resmap1@mpLambertMeridianF    =  LON01      ; ncl adds from grib file
  end if

  if (RPK1.eq.1)
; ---------------------------
  ;   Stereographic projection north
; ---------------------------
    resmap1@mpProjection = "Stereographic"
    resmap1@mpCenterLonF           = LON01
    resmap1@mpCenterRotF           = BETA1
    resmap1@mpCenterLatF           = 90
  end if

  if (RPK1.eq.-1)
; ---------------------------
  ;   Stereographic projection south
; ---------------------------
    resmap1@mpProjection = "Stereographic"
    resmap1@mpCenterLonF           = LON01
    resmap1@mpCenterRotF           = BETA1
    resmap1@mpCenterLatF           = -90
  end if

  if (RPK1.eq.0) then
; ---------------------------
  ;   Mercator projection
; ---------------------------
    resmap1@mpProjection = "Mercator"
  end if

 print("Map projection="+resmap1@mpProjection)

; ---------------------------
; Defining the corners for projection
; --------------------------------
  resmap1@mpLimitMode            = "Corners"
  resmap1@mpLeftCornerLatF       = lat2d1(0,0)
  resmap1@mpLeftCornerLonF       = lon2d1(0,0)
  resmap1@mpRightCornerLatF     = lat2d1(jmax1-1,imax1-1)
  resmap1@mpRightCornerLonF     = lon2d1(jmax1-1,imax1-1)
  
;-----------------------------------------------;
; Domaine 2
;-----------------------------------------------;
; Read projection parameters
; --------------------
    RPK2  = a23->RPK
    BETA2 = a23->BETA
    LON02 = a23->LON0

  resmap2=True
  if (RPK2.gt.0)
; ---------------------------
  ;   Lambert  projection from north pole
; ---------------------------
   resmap2@mpProjection          = "LambertConformal"     ; projection
   pole                         = 1    ; projection for north hemisphere
   resmap2@mpLambertParallel1F   = pole*asin(RPK2)*180/3.14 ; with pole=1 for north hemisphere and -1 for south hemisphere
   resmap2@mpLambertParallel2F   = resmap2@mpLambertParallel1F  ; ncl adds from grib file
   resmap2@mpLambertMeridianF    = LON02      ; ncl adds from grib file
  end if

  if (RPK2.lt.0)
; ---------------------------
  ;   Lambert projection from south pole
; ---------------------------
   resmap2@mpProjection          = "LambertConformal"     ; projection
   pole                         = -1                     ; projection for south hemisphere
   resmap2@mpLambertParallel1F   = pole*asin(RPK2)*180/3.14 ; with pole=1 for north hemisphere and -1 for south hemisphere
   resmap2@mpLambertParallel2F   = resmap2@mpLambertParallel1F  ; ncl adds from grib file
   resmap2@mpLambertMeridianF    =  LON02      ; ncl adds from grib file
  end if

  if (RPK2.eq.1)
; ---------------------------
  ;   Stereographic projection north
; ---------------------------
    resmap2@mpProjection = "Stereographic"
    resmap2@mpCenterLonF           = LON02
    resmap2@mpCenterRotF           = BETA2
    resmap2@mpCenterLatF           = 90
  end if

  if (RPK2.eq.-1)
; ---------------------------
  ;   Stereographic projection south
; ---------------------------
    resmap2@mpProjection = "Stereographic"
    resmap2@mpCenterLonF           = LON02
    resmap2@mpCenterRotF           = BETA2
    resmap2@mpCenterLatF           = -90
  end if

  if (RPK2.eq.0) then
; ---------------------------
  ;   Mercator projection
; ---------------------------
    resmap2@mpProjection = "Mercator"
  end if

 print("Map projection="+resmap2@mpProjection)

; ---------------------------
; Defining the corners for projection
; --------------------------------
  resmap2@mpLimitMode            = "Corners"
  resmap2@mpLeftCornerLatF       = lat2d2(0,0)
  resmap2@mpLeftCornerLonF       = lon2d2(0,0)
  resmap2@mpRightCornerLatF     = lat2d2(jmax2-1,imax2-1)
  resmap2@mpRightCornerLonF     = lon2d2(jmax2-1,imax2-1)

;=================================================;
; interpolation des champs a 1250 m
; domain 1
;=================================================;

rc1_t3_plane = wrf_user_intrp3d(rc1_t3(0,:,:,:),alt1(0,:,:,:),"h",1300,0.,False)
printMinMax(rc1_t3_plane,0)
printMinMax(alt1,0)
HCHO1_t3_plane = wrf_user_intrp3d(HCHO1_t3(0,:,:,:),alt1(0,:,:,:),"h",1300,0.,False)
OH1_t3_plane = wrf_user_intrp3d(OH1_t3(0,:,:,:),alt1(0,:,:,:),"h",1300,0.,False)
O31_t3_plane = wrf_user_intrp3d(O31_t3(0,:,:,:),alt1(0,:,:,:),"h",1300,0.,False)
H2O21_t3_plane = wrf_user_intrp3d(H2O21_t3(0,:,:,:),alt1(0,:,:,:),"h",1300,0.,False)
SO21_t3_plane = wrf_user_intrp3d(SO21_t3(0,:,:,:),alt1(0,:,:,:),"h",1300,0.,False)
;
; --------------------------------
; domain 2
; --------------------------------
rc2_t3_plane = wrf_user_intrp3d(rc2_t3(0,:,:,:),alt2(0,:,:,:),"h",1300,0.,False)
printMinMax(rc2_t3_plane,0)
printMinMax(alt2,0)
HCHO2_t3_plane = wrf_user_intrp3d(HCHO2_t3(0,:,:,:),alt2(0,:,:,:),"h",1300,0.,False)
OH2_t3_plane = wrf_user_intrp3d(OH2_t3(0,:,:,:),alt2(0,:,:,:),"h",1300,0.,False)
H2O22_t3_plane = wrf_user_intrp3d(H2O22_t3(0,:,:,:),alt2(0,:,:,:),"h",1300,0.,False)
SO22_t3_plane = wrf_user_intrp3d(SO22_t3(0,:,:,:),alt2(0,:,:,:),"h",1300,0.,False)
wHCHO2_t3_plane = wrf_user_intrp3d(wHCHO2_t3(0,:,:,:),alt2(0,:,:,:),"h",1300,0.,False)
wOH2_t3_plane = wrf_user_intrp3d(wOH2_t3(0,:,:,:),alt2(0,:,:,:),"h",1300,0.,False)
wH2O22_t3_plane = wrf_user_intrp3d(wH2O22_t3(0,:,:,:),alt2(0,:,:,:),"h",1300,0.,False)
wSO22_t3_plane = wrf_user_intrp3d(wSO22_t3(0,:,:,:),alt2(0,:,:,:),"h",1300,0.,False)
wSULF2_t3_plane = wrf_user_intrp3d(wSULF2_t3(0,:,:,:),alt2(0,:,:,:),"h",1300,0.,False)
wORA12_t3_plane = wrf_user_intrp3d(wORA12_t3(0,:,:,:),alt2(0,:,:,:),"h",1300,0.,False)
;
  figname ="zsection_1300"
;  wks  = gsn_open_wks("png",figname)   ; open a ncgm file
  wks  = gsn_open_wks("eps",figname)   ; open a ncgm file
  gsn_define_colormap(wks,"WhBlGrYeRe") ; Choose colormap
;
;=================================================;
; PLOT domain 1
;=================================================;
  res                 = resmap1          
  res@gsnDraw   		= False         ; don't draw yet
  res@gsnFrame  		= False         ; don't advance frame yet
; General ressources
  res@gsnMaximize        = True          ; uncomment to maximize size
  res@gsnSpreadColors    = True         ; use full range of colormap
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
;
  res@gsnCenterString="heure=18 domain 1"
;
; plot cloud mixing ratio domain 1 time 3
  res@cnLevelSelectionMode = "ExplicitLevels"
  res@cnLevels =     (/0.01,0.015,0.02,0.025,0.03,0.035,0.04,0.045,0.05,0.055,0.06/)
  res@cnFillColors = (/2,6,12,40,45,51,62,72,80,89,96,101/) ; color of a contour
  plot_rc13 = gsn_csm_contour_map(wks,rc1_t3_plane(:,:),res)
  draw(plot_rc13)
  frame(wks)
  delete(res@cnLevels)
  delete(res@cnFillColors)
; plot ozone
  res@cnLevelSelectionMode = "ExplicitLevels" ; method for selecting the contour
  res@cnLevels     = (/10.,15.,20.,25.,30.,35.,40.,45.,50.,55.,60./)
  res@cnFillColors = (/6,12,24,40,45,51,62,72,80,89,96,101/) ; color of a contour
  plot_o313 = gsn_csm_contour_map(wks,O31_t3_plane(:,:),res)
  draw(plot_o313)
  frame(wks)
  delete(res@cnLevels)
  delete(res@cnFillColors)
; plot hcho
  res@cnLevelSelectionMode = "ExplicitLevels" ; method for selecting the contour
  res@cnLevels     = (/0.8,0.9,1.,1.1,1.2,1.3,1.4,1.5,1.6,1.7,1.8/)
  res@cnFillColors = (/6,12,24,40,45,51,62,72,80,89,96,101/) ; color of a contour
  plot_hcho13 = gsn_csm_contour_map(wks,HCHO1_t3_plane(:,:),res)
  draw(plot_hcho13)
  frame(wks)
  delete(res@cnLevels)
  delete(res@cnFillColors)
; plot OH
  res@cnLevelSelectionMode = "AutomaticLevels" ; method for selecting the contour
;  res@cnLevelSelectionMode = "ExplicitLevels" ; method for selecting the contour
  plot_OH13 = gsn_csm_contour_map(wks,OH1_t3_plane(:,:),res)
  draw(plot_OH13)
  frame(wks)
; plot SO2
  res@cnLevelSelectionMode = "ExplicitLevels" ; method for selecting the contour
  res@cnLevels     = (/0.5,0.6,0.7,0.8,0.9,1.,1.1,1.2,1.3,1.4,1.5/)
  res@cnFillColors = (/6,12,24,40,45,51,62,72,80,89,96,101/) ; color of a contour
  plot_SO213 = gsn_csm_contour_map(wks,SO21_t3_plane(:,:),res)
  draw(plot_SO213)
  frame(wks)
  delete(res@cnLevels)
  delete(res@cnFillColors)
; plot H2O2
  res@cnLevelSelectionMode = "ExplicitLevels" ; method for selecting the contour
  res@cnLevels     = (/0.8,0.9,1.,1.1,1.2,1.3,1.4,1.5,1.6,1.7,1.8/)
  res@cnFillColors = (/6,12,24,40,45,51,62,72,80,89,96,101/) ; color of a contour
  plot_H2O213 = gsn_csm_contour_map(wks,H2O21_t3_plane(:,:),res)
  draw(plot_H2O213)
  frame(wks)
  delete(res@cnLevels)
  delete(res@cnFillColors)
;
;=================================================;
; PLOT domain 2
;=================================================;
  delete(res)
  res                 = resmap2          
  res@gsnDraw   		= False         ; don't draw yet
  res@gsnFrame  		= False         ; don't advance frame yet
; General ressources
  res@gsnMaximize        = True          ; uncomment to maximize size
  res@gsnSpreadColors    = True         ; use full range of colormap
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
;
  res@gsnCenterString="heure=18 domain 2"
;
; plot cloud mixing ratio domain 2 time 3
  res@cnLevelSelectionMode = "ExplicitLevels"
  res@cnLevels =     (/0.01,0.02,0.03,0.04,0.05,0.06,0.07,0.08,0.09,0.1/)
  res@cnFillColors = (/2,6,12,40,45,51,62,72,80,89,96/) ; color of a contour
  plot_rc23 = gsn_csm_contour_map(wks,rc2_t3_plane(:,:),res)
  draw(plot_rc23)
  frame(wks)
  delete(res@cnLevels)
  delete(res@cnFillColors)
; plot hcho
  res@cnLevelSelectionMode = "ExplicitLevels" ; method for selecting the contour
  res@cnLevels     = (/0.8,0.9,1.,1.1,1.2,1.3,1.4,1.5,1.6,1.7,1.8/)
  res@cnFillColors = (/6,12,24,40,45,51,62,72,80,89,96,101/) ; color of a contour
  plot_hcho23 = gsn_csm_contour_map(wks,HCHO2_t3_plane(:,:),res)
  draw(plot_hcho23)
  frame(wks)
  delete(res@cnLevels)
  delete(res@cnFillColors)
; plot OH
  res@cnLevelSelectionMode = "AutomaticLevels" ; method for selecting the contour
  plot_OH23 = gsn_csm_contour_map(wks,OH2_t3_plane(:,:),res)
  draw(plot_OH23)
  frame(wks)
; plot SO2
  res@cnLevelSelectionMode = "ExplicitLevels" ; method for selecting the contour
  res@cnLevels     = (/0.5,0.6,0.7,0.8,0.9,1.,1.1,1.2,1.3,1.4,1.5/)
  res@cnFillColors = (/6,12,24,40,45,51,62,72,80,89,96,101/) ; color of a contour
  plot_SO223 = gsn_csm_contour_map(wks,SO22_t3_plane(:,:),res)
  draw(plot_SO223)
  frame(wks)
  delete(res@cnLevels)
  delete(res@cnFillColors)
; plot H2O2
  res@cnLevelSelectionMode = "ExplicitLevels" ; method for selecting the contour
  res@cnLevels     = (/0.8,0.9,1.,1.1,1.2,1.3,1.4,1.5,1.6,1.7,1.8/)
  res@cnFillColors = (/6,12,24,40,45,51,62,72,80,89,96,101/) ; color of a contour
  plot_H2O223 = gsn_csm_contour_map(wks,H2O22_t3_plane(:,:),res)
  draw(plot_H2O223)
  frame(wks)
  delete(res@cnLevels)
  delete(res@cnFillColors)
; plot hcho in cloud
  res@cnLevelSelectionMode = "AutomaticLevels" 
  plot_whcho23 = gsn_csm_contour_map(wks,wHCHO2_t3_plane(:,:),res)
  draw(plot_whcho23)
  frame(wks)
; plot OH in cloud
  res@cnLevelSelectionMode = "AutomaticLevels" 
  plot_wOH23 = gsn_csm_contour_map(wks,wOH2_t3_plane(:,:),res)
  draw(plot_wOH23)
  frame(wks)
; plot ORA1 in cloud
  res@cnLevelSelectionMode = "AutomaticLevels" 
  plot_wORA123 = gsn_csm_contour_map(wks,wORA12_t3_plane(:,:),res)
  draw(plot_wORA123)
  frame(wks)
; plot SO2 in cloud
  res@cnLevelSelectionMode = "AutomaticLevels" 
  plot_wSO223 = gsn_csm_contour_map(wks,wSO22_t3_plane(:,:),res)
  draw(plot_wSO223)
  frame(wks)
; plot H2O2 in cloud
  res@cnLevelSelectionMode = "AutomaticLevels" 
  plot_wH2O223 = gsn_csm_contour_map(wks,wH2O22_t3_plane(:,:),res)
  draw(plot_wH2O223)
  frame(wks)
; plot SULF in cloud
  res@cnLevelSelectionMode = "AutomaticLevels" 
  plot_wSULF23 = gsn_csm_contour_map(wks,wSULF2_t3_plane(:,:),res)
  draw(plot_wSULF23)
  frame(wks)

;;;;;;;;;;;;;;;;;;;;;;;;

end
