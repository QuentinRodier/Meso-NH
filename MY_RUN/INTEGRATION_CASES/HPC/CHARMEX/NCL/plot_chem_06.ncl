load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl"   
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_csm.ncl"   
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/contributed.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/wrf/WRFUserARW.ncl"
; ================================================;
begin
;=================================================;
; open file and read in data
;=================================================;

  a = addfile("../RUN/OUTFILES/CHARM.1.SEG02.004.nc", "r")
;=================================================;
; Get informations on variable sizes
; dims are dims-2 to remove non-physical values
;=================================================;
  jphext  = a->JPHEXT
  mdims = getfilevardimsizes(a,"ISOP") 
  nd = dimsizes(mdims)
  imax=mdims(nd-1)-2*jphext
  jmax=mdims(nd-2)-2*jphext
  kmax=mdims(nd-3)-2
  
;======================================================;
; define parameters to be drawn
;======================================================;
  nlevel = 0


    lat=a->LAT(jphext:jmax+jphext-1,jphext:imax+jphext-1)
    lon=a->LON(jphext:jmax+jphext-1,jphext:imax+jphext-1)
printVarSummary(lat) 
printVarSummary(lon) 

 
  ISOP = a->ISOP(0,1,jphext:jmax+jphext-1,jphext:imax+jphext-1)
  ISOP@long_name=""
  ISOP@units=" (ppb)"
  ISOP=ISOP*1.E9
printVarSummary(ISOP) 
               
  O3 = a->O3(0,1,jphext:jmax+jphext-1,jphext:imax+jphext-1)
  O3@long_name="Ozone"
  O3@units=" PPB "
  O3=O3*1.E9

  MVK= a->MVK(0,1,jphext:jmax+jphext-1,jphext:imax+jphext-1)
  MVK@long_name="MVK"
  MVK@units=" PPB "
  MVK=MVK*1.E9
  
  MCR= a->MCR(0,1,jphext:jmax+jphext-1,jphext:imax+jphext-1)
  MCR@long_name="MCR"
  MCR@units=" PPB "
  MCR=MCR*1.E9
  
    NO= a->NO(0,1,jphext:jmax+jphext-1,jphext:imax+jphext-1)
  NO@long_name="NO"
  NO@units=" PPB "
  NO=NO*1.E9

  NO2= a->NO2(0,1,jphext:jmax+jphext-1,jphext:imax+jphext-1)
  NO2@long_name="NO2"
  NO2@units=" PPB "
  NO2=NO2*1.E9

  NOX = NO2 + NO
  NOX@long_name="NOx"
  NOX@units=" PPB "
 
; ==========;
; PLOT
;=================================================;
  wks  = gsn_open_wks("ps","charmex_06")   ; open a ncgm file
 gsn_define_colormap(wks,"WhBlGrYeRe") ; Choose colormap


res			= True         
  res@gsnDraw   	= False         ; don't draw yet
  res@gsnFrame  	= False         ; don't advance frame yet
res@cnFillOn               =  True            ; turn on color fill
res@cnLinesOn              =  False            ; turn off contour lines
; X-axis title (tiY)                              
res@tiXAxisFontHeightF = 0.018  ; font height
res@tiXAxisFont        = 21     ; font index
res@tiXAxisString      = "longitude"  ; string to use as the X-Axis title

; Y-axis title (tiY)
res@tiYAxisFontHeightF = 0.018  ; font height
res@tiYAxisFont        = 21     ; font index
res@tiYAxisString      = "latitude"  ; string to use as the Y-Axis title
  
res@mpGridAndLimbOn          = True             ; turn on lat/lon lines
res@mpGridLatSpacingF        = 10.              ; spacing for lat lines
res@mpGridLonSpacingF        = 10.              ; spacing for lon lines
res@mpDataBaseVersion        = "MediumRes"         ; choose highres map data version (must be donwloaded)
res@mpGeophysicalLineColor   = "Black"  ; default value in lowres
res@mpNationalLineColor      = "Black"  ; idem
res@mpGridLineColor          = "Black"
res@mpLimbLineColor          = "Black"
res@mpPerimLineColor         = "Black"
res@mpGridLineDashPattern    = 14
res@mpGridLineThicknessF     = 0.75
res@pmTickMarkDisplayMode    = "Always"
;time0 = 0
;time1 = 1
;time2 = 3
;time3 = 5
;*****************************************************
; Create THT plot  (contour)
;*****************************************************
; General ressources
res@gsnMaximize        = True          ; uncomment to maximize size
res@gsnSpreadColors    = True       	; use full range of colormap
res@lbAutoManage = False
res@lbBottomMarginF = .1
;res@lbOrientation = "Vertical"
res@lbLabelFontHeightF = 0.02
res@lbLabelFont = 21

;;;; Comme ci-dessous
  res@tfDoNDCOverlay       = True
;;; Utiliser projection Lambert Conforme plutot que Stereographique
  res@mpProjection          = "LambertConformal"
  res@mpLambertParallel1F   = 30
  res@mpLambertParallel2F   = res@mpLambertParallel1F
  res@mpLambertMeridianF    = a->LON0
  res@mpLimitMode           = "Corners"
  res@mpLeftCornerLatF      = lat(0,0)
  res@mpLeftCornerLonF      = lon(0,0)
  res@mpRightCornerLatF     = lat(jmax-1,imax-1)
  res@mpRightCornerLonF     = lon(jmax-1,imax-1)

  
res@cnLevelSelectionMode = "ExplicitLevels" ; method for selecting the contour
res@cnLevels     = (/10,8,6,3,2,1,0.8,0.6,0.4,0.2,0.1,0.05,0.01/)
res@cnFillColors = (/0,7,14,21,28,35,42,49,56,63,70,77,84,91/) ; color of a contour

res@tiMainFontThicknessF = 2
res@tiMainFontHeightF   = 0.035
res@tiMainOn            = True
  res@tiMainString      = "Isoprene_12h"
;  res@tiMainString      = "Isoprene_0h"
 res@tiXAxisString      = ""
 res@tiYAxisString      = "latitude"
;   plot_ISOP_0h = gsn_csm_contour_map(wks,ISOP(time0,:,:),res)
   plot_ISOP = gsn_csm_contour_map(wks,ISOP(:,:),res)
   draw(plot_ISOP)
   frame(wks)
   delete(res@cnLevels)
   delete(res@cnFillColors)
;  res@tiMainString      = "Isoprene_6h"
; res@tiYAxisString      = ""
 
;   plot_ISOP_6h = gsn_csm_contour_map(wks,ISOP(time1,:,:),res)
;  res@tiMainString      = "Isoprene_12h"
;res@tiYAxisString      = ""
; res@tiXAxisString      = "Longitude"
; res@tiYAxisString      = "latitude"
;   plot_ISOP_12h = gsn_csm_contour_map(wks,ISOP(time2,:,:),res)
;   res@tiMainString      = "Isoprene_18h"
;   res@tiYAxisString      = ""
;   res@tiXAxisString      = "Longitude"
;   plot_ISOP_18h = gsn_csm_contour_map(wks,ISOP(time3,:,:),res)



   res@tiYAxisString      = "latitude"
;res@cnLevelSelectionMode = "ExplicitLevels" ; method for selecting the contour
  res@cnLevels     = (/150,140,130,120,110,100,90,80, 70, 60, 50, 40, 30, 20, 10/)
  res@cnFillColors = (/0,7,14,21,28,35,42,49,56,63,70,77,84,91,98,101/) ; color of a contour
  res@tiMainOn            = True
  res@tiMainString      = "ozone_12h"
   plot_O3_12h = gsn_csm_contour_map(wks,O3(:,:),res)
   draw(plot_O3_12h)
   frame(wks)
   delete(res@cnLevels)
   delete(res@cnFillColors)
   ;res@cnLevelSelectionMode = "ExplicitLevels" ; method for selecting the contour
  res@cnLevels     = (/40,20,15,10,8,6,3,1, 0.8, 0.5, 0.3, 0.1, 0.01, 0.001, 0.00010/)
  res@cnFillColors = (/0,7,14,21,28,35,42,49,56,63,70,77,84,91,98,101/) ; color of a contour
   res@tiMainString      = "NOx_12h"
   plot_NOX_12h = gsn_csm_contour_map(wks,NOX(:,:),res)
   draw(plot_NOX_12h)
   frame(wks)
;;;;;;;;;;;;;;;;;;;;;;;;

end



