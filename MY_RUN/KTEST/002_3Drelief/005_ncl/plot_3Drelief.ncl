;================================================;
  load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl"   
  load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_csm.ncl"   
; ================================================;
;================================================;
  begin
;=================================================;
; open file and read in data
;=================================================;
  fichier1 = addfile("REL3D.1.EXP01.002.nc4", "r")
;==================================================;
; Open the workstation
;==================================================;
  type = "ps"
  wks = gsn_open_wks(type,"visu_3Drelief")
;=================================================;
; lecture des différents champs
;=================================================;
  zs  = fichier1->ZS(0,:) ; ZS
  zhat  = fichier1->ZHAT(:) ; ZHAT
  xhat  = fichier1->XHAT(:) ; XHAT

  ut= fichier1->UT(:,:,:) ; UT
  ut@long_name="Zonal wind"
  ut@units="m/s"


;=================================================;
; Récupération des dimensions des tableaux 
;=================================================;
  mdims = dimsizes(ut)
  nd=dimsizes(mdims)
  imax=mdims(nd-1)
  jmax=mdims(nd-2)
  kmax=mdims(nd-3)

;=================================================;
; Unstagger U
;=================================================;
  ut!0="Z"
  ut!1="Y"
  ut!2="X"
  u = wrf_user_unstagger(ut,"X")

;=================================================;
; Set some other basic resources
;=================================================;
  resmap = True
  resmap@gsnFrame = False
  resmap@gsnDraw = False
  resmap@gsnMaximize = True
  resmap@gsnPaperOrientation = "portrait" 
  resmap@gsnSpreadColors= True       	
  resmap@tiMainString ="UT level 2 "
  resmap@cnFillOn= True
  resmap@cnLinesOn= False 

;=================================================;
; TRACE
;=================================================;
  gsn_define_colormap(wks,"rainbow")

  resmap@cnLevelSelectionMode = "ManualLevels"
  resmap@cnLevelSpacingF    = 0.25
  resmap@cnMinLevelValF    = 8
  resmap@cnMaxLevelValF    = 14
  plot_ut = gsn_csm_contour(wks,u(1,:,1:),resmap)
  draw(plot_ut)
  frame(wks)

end
