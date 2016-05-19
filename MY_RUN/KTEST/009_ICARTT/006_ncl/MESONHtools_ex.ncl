load "$NCARG_ROOT/lib/ncarg/nclscripts/wrf/WRFUserARW.ncl"

;*************************************************************************
; J.-P. CHABOUREAU
; This is a driver that selects the appropriate 
; mapping function based upon the file attribute: MAP_PROJ
; MAP_PROJ=1 [Lambert Conformal]; =2 [Stereographic]; =3 [Mercator]
;
; opt: currently not used [potentail use: time counter for XLAT/XLONG]
;
; Sample usage:
;             ncdf = addfile("...", r")
;             res  = True
;             WRF_map_c (ncdf, res, 0)
;             res  = ...
;
undef("MESONH_map_c")
procedure MESONH_map_c (f:file, res:logical, plat, plon, icorner)
local rank, dimll, nlat, mlon, lat2d, lon2d
begin

  if (plat(0,0).eq.plat(1,1)) then
    ZRPK = f->RPK
;  ZLATOR = f->LATORI
;  ZLONOR = f->LONORI
  ZLATOR = f->LATOR
  ZLONOR = f->LONOR
;  ZLATOR = 46.59
;  ZLONOR = -3.09
  ZBETA = f->BETA
  ZLAT0 = f->LAT0
  ZLON0 = f->LON0
  XHAT = f->W_E_direction
  YHAT = f->S_N_direction
  IMAX = dimsizes(XHAT)
  JMAX = dimsizes(YHAT)
  zdx=XHAT(2)-XHAT(1)
  zdy=YHAT(2)-YHAT(1)
 print ("LATOR="+ZLATOR+" - LONOR="+ZLONOR)
 print ("ZLAT0="+ZLAT0+" - ZLON0="+ZLON0)
 print ("ZDX="+zdx+" - RPK="+ZRPK+" - BETA="+ZBETA)
 print ("IMAX="+IMAX+" - JMAX="+JMAX)

;  if (isatt(f,"RPK")) then
;print ("tutu "+f@RPK)
;  if (isatt(f,"RPK")) then
;print ("tutu "+f@RPK)
;      if (f@MAP_PROJ.eq.1) then
;          res@mpProjection = "LambertConformal"
;      end if
      if (abs(ZRPK).eq.1) then
        res@mpProjection = "Stereographic"
        res@mpCenterLonF           = ZLON0
        res@mpCenterRotF = ZBETA
        if (ZRPK.eq.1) then
;;;; XRPK=1: polar stereographic projection from south pole
          res@mpCenterLatF           = 90. 
        else
          res@mpCenterLatF           = -90. 
        end if
       end if
      if (ZRPK.eq.0) then
;;;; XRPK=0: Mercator projection from earth center
          res@mpProjection = "Mercator"
      end if
      if (abs(ZRPK).gt.0.and.abs(ZRPK).lt.1) then
;;;; 1>XRPK>0: Lambert projection from south pole
;;;; -1<XRPK<0: Lambert projection from north pole
;        res@mpProjection = "LambertConformal"
        res@mpProjection = "Stereographic"
        res@mpCenterLonF           = ZLON0
         if (ZRPK.gt.0) then
        res@mpCenterLatF           = 90. 
         else
        res@mpCenterLatF           = -90. 
         end if
      end if
;  else
;      print ("MESONH_mapProj: no MAP_PROJ attribute")
;  end if

 print("Map projection="+res@mpProjection)

;=================================================;
; src/mesonh_MOD/mode_gridproj.f90
;=================================================;
  XRADIUS=6371229.0d ; Earth radius (meters)
  XPI=2.0d*asin(1.)    ; Pi
  ZRDSDG= XPI/180.0d          ; Radian to Degree conversion factor
  ZXBM0 = 0.0d
  ZYBM0 = 0.0d

  lat2d = new((/JMAX,IMAX/),"double")
  lon2d = new((/JMAX,IMAX/),"double")

;=================================================;
;  if (abs(ZRPK).eq.1) then
      if (ZRPK.gt.0.and.ZRPK.le.1) then
; STEREOGRAPHIC PROJECTION
    ZCLAT0  = cos(ZRDSDG*ZLAT0)
    ZSLAT0  = sin(ZRDSDG*ZLAT0)
    ZCLATOR = cos(ZRDSDG*ZLATOR)
    ZSLATOR = sin(ZRDSDG*ZLATOR)
    ZRO0    = (XRADIUS/ZRPK)*(abs(ZCLAT0))^(1.-ZRPK) * \
              ((1.+ZSLAT0)*abs(ZCLATOR)/(1.+ZSLATOR))^ZRPK
    ZGA0    = (ZRPK*(ZLONOR-ZLON0)-ZBETA)*ZRDSDG
    ZXP     = ZXBM0-ZRO0*sin(ZGA0)
    ZYP     = ZYBM0+ZRO0*cos(ZGA0)
    do ji=0,IMAX-1
      do jj=0,JMAX-1
        ZATA = atan2( -(ZXP-XHAT(ji)) , (ZYP-YHAT(jj)) )/ZRDSDG
        zlon  = (ZBETA+ZATA)/ZRPK+ZLON0
        lon2d(jj,ji)=zlon
        ZRO2 = (XHAT(ji)-ZXP)^2+(YHAT(jj)-ZYP)^2
        ZJD1 = XRADIUS*(abs(ZCLAT0))^(1.-ZRPK)
        ZT1  = (ZJD1)^(2./ZRPK)* (1+ZSLAT0)^2
        ZJD3 = (ZRPK^2*ZRO2)
        ZT2  = ZJD3
        ZT2 = ZT2^(1./ZRPK)
        ZJD1 = (ZT1-ZT2)/(ZT1+ZT2)
        ZJD1 = acos(ZJD1)
        ZJD3 = ZJD1
        zlat = (XPI/2.-ZJD3)/ZRDSDG
        lat2d(jj,ji)=zlat
      end do
    end do
    
;   print ("minlat="+min(lat2d)+" maxlat="+max(lat2d))
;   print ("minlon="+min(lon2d)+" maxlon="+max(lon2d))
  end if
;=================================================;
  if (ZRPK.eq.0) then
    XBETA=0.
    XLAT0=0.   ; map reference latitude  (degrees)
    ZXBM0 = 0.
    ZYBM0 = 0.
    ZCGAM    = cos(-ZRDSDG*XBETA)
    ZSGAM    = sin(-ZRDSDG*XBETA)
    ZRACLAT0 = XRADIUS*cos(ZRDSDG*ZLAT0)
    do ji=0,IMAX-1
      jj=0
      ZXMI0 = XHAT(ji)-ZXBM0
      ZYMI0 = YHAT(jj)-ZYBM0
      zlon  = (ZXMI0*ZCGAM+ZYMI0*ZSGAM)/(ZRACLAT0*ZRDSDG)+ZLONOR
      do jj=0,JMAX-1
        lon2d(jj,ji)=zlon
      end do
    end do
    do jj=0,JMAX-1
      ji=0
      ZXMI0 = XHAT(ji)-ZXBM0
      ZYMI0 = YHAT(jj)-ZYBM0
      ZT1  = log(tan(XPI/4.+ZLATOR*ZRDSDG/2.))
      ZT2  = (-ZXMI0*ZSGAM+ZYMI0*ZCGAM)/ZRACLAT0
      zlat  = (-XPI/2.+2.*atan(exp(ZT1+ZT2)))/ZRDSDG
      do ji=0,IMAX-1
        lat2d(jj,ji)=zlat
      end do
    end do
    
  end if

  do ji=0,IMAX-1
    do jj=0,JMAX-1
      plat(jj,ji)=lat2d(jj,ji)
      plon(jj,ji)=lon2d(jj,ji)
    end do
  end do
  end if
;=================================================;
  if (icorner(0,0).eq.icorner(1,1)) then
    icorner(0,0)=0
    icorner(1,0)=JMAX-1
    icorner(0,1)=0
    icorner(1,1)=IMAX-1
  end if
; print ("icorner"+icorner)
;=================================================;
;  lat2d@units = "degrees_north"    ; not needed
;  lon2d@units = "degrees_east"

  res@gsnAddCyclic           = False              ; regional data

  res@mpLimitMode            = "Corners"
  res@mpLeftCornerLatF       = plat(icorner(0,0),icorner(0,1))
  res@mpLeftCornerLonF       = plon(icorner(0,0),icorner(0,1))
  res@mpRightCornerLatF     = plat(icorner(1,0),icorner(1,1))
  res@mpRightCornerLonF     = plon(icorner(1,0),icorner(1,1))

; print ("Corner (0,0); Lat="+res@mpLeftCornerLatF+ \
;                    ", Lon="+res@mpLeftCornerLonF)
; print ("Oppos corner; Lat="+res@mpRightCornerLatF+ \
;                     ", Lon= "+res@mpRightCornerLonF)

;;; print ("0,0 "+XHAT(0)+" lon "+YHAT(0))
;;; print ("0,0 "+XHAT(IMAX-1)+"  "+YHAT(JMAX-1))

;************************************************
; Turn on lat / lon labeling
;************************************************
  res@pmTickMarkDisplayMode = "Always"         ; turn on tickmarks

  res@mpOutlineBoundarySets = "AllBoundaries" ; state boundaries

  res@mpPerimDrawOrder      = "PostDraw"       ; force map perim
;************************************************
; set True for native projection (faster)
;************************************************
  res@tfDoNDCOverlay       = True      
  
end

;*************************************************************************
; S. BIELLI
; This is a routine that interpolate  fields on pressure level for plotting
; based on pinter.f90 
; The field to be interpolated must be given at the mass point (grid 1)
; usage : var_inter=MESONHfunction(var_to_interpol, 850., AbsPressure)
; Abs pressure must be in Pa
;
undef("MESONH_pinter")
function MESONH_pinter( pfield:numeric, loc_param:numeric, ppabs:numeric )

begin

 dimL= dimsizes(loc_param)

; First test for grid = 0

  dimp=dimsizes(ppabs)

  pout=pfield(0:dimL-1,:,:)
  pfield@_FillValue=999
  pout@_FillValue=999
  pout=pout@_FillValue

  do jkp = 0, dimL-1
 	zref=log10(loc_param(jkp)*100.)
	do jloop = 0, dimp(1)-1
  	  do iloop = 0, dimp(2)-1
            kloop=0
            flag=True
	    do while (flag .and. (kloop.lt.(dimp(2)-2)))
	      if (.not.ismissing(ppabs(kloop,jloop,iloop))) then
		zxm=log10(ppabs(kloop,jloop,iloop))
		zxp=log10(ppabs(kloop+1,jloop,iloop))
		if ((zxp-zref)*(zref-zxm) .ge. 0) then
                  pout(jkp,jloop,iloop)= (pfield(kloop,jloop,iloop)*(zxp-zref)+ \
			pfield(kloop+1,jloop,iloop)*(zref-zxm))/ (zxp-zxm)
                  flag=False
                end if
              end if
              kloop=kloop+1
            end do
          end do
        end do
  end do

  return(pout)
	
end

