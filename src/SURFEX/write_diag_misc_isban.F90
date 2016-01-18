!SURFEX_LIC Copyright 1994-2014 Meteo-France 
!SURFEX_LIC This is part of the SURFEX software governed by the CeCILL-C  licence
!SURFEX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!SURFEX_LIC for details. version 1.
!     #########
      SUBROUTINE WRITE_DIAG_MISC_ISBA_n(HPROGRAM)
!     #################################
!
!!****  *WRITE_DIAG_MISC_ISBA* - writes the ISBA diagnostic fields
!!
!!    PURPOSE
!!    -------
!!
!!
!!**  METHOD
!!    ------
!!
!!    REFERENCE
!!    ---------
!!
!!
!!    AUTHOR
!!    ------
!!	P. Le Moigne   *Meteo France*	
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    10/2004
!!      B. Decharme    2008  Total Albedo, Total SWI and Floodplains
!!      B. Decharme 06/2009  key to write (or not) patch result
!!      A.L. Gibelin 04/09 : Add respiration diagnostics
!!      A.L. Gibelin 05/09 : Add carbon spinup
!!      A.L. Gibelin 07/09 : Suppress RDK and transform GPP as a diagnostic
!!      D. Carrer    04/11 : Add FAPAR and effective LAI
!!      B. Decharme  09/2012 : suppress NWG_LAYER (parallelization problems)
!!      B. Decharme  09/12 : Carbon fluxes in diag_evap
!!      B. Decharme  09/12   New diag for DIF:
!!                           F2 stress
!!                           Root zone swi, wg and wgi
!!                           swi, wg and wgi comparable to ISBA-FR-DG2 and DG3 layers
!!                           active layer thickness over permafrost
!!                           frozen layer thickness over non-permafrost
!!      M.Moge    01/2016  using WRITE_SURF_FIELD2D/3D for 2D/3D surfex fields writes
!!
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
USE MODD_SURFEX_MPI, ONLY : NWG_SIZE
!
USE MODD_SURF_PAR,        ONLY :   NUNDEF, XUNDEF
USE MODD_ISBA_n,          ONLY :   NGROUND_LAYER,       &
                                   CRUNOFF, CRAIN, CISBA, LTR_ML,  &
                                   XMUF, NWG_LAYER,                &
                                   CPHOTO, CRESPSL, LFLOOD,        &
                                   XFFLOOD, XPIFLOOD, TSNOW  
!                                 
USE MODD_DIAG_ISBA_n,     ONLY :   LPATCH_BUDGET, XTS, XAVG_TS,    &
                                   XTSRAD, XAVG_TSRAD  
!                                 
USE MODD_AGRI,            ONLY :   LAGRIP
USE MODD_DIAG_MISC_ISBA_n,ONLY :   LSURF_MISC_BUDGET, LSURF_MISC_DIF,   &
                                   XHV, XAVG_HV, XSWI, XAVG_SWI,        &
                                   XTSWI, XAVG_TSWI, XDPSNG, XAVG_PSNG, &
                                   XDPSNV, XAVG_PSNV, XDPSN, XAVG_PSN,  &
                                   XSEUIL, XSOIL_TSWI, XALBT, XAVG_ALBT,&                                   
                                   XTWSNOW, XAVG_TWSNOW, XTDSNOW,       &
                                   XAVG_TDSNOW,XTTSNOW, XAVG_TTSNOW,    &
                                   XDFFG, XAVG_FFG, XDFFV, XAVG_FFV,    &
                                   XDFF, XAVG_FF, XSOIL_TWG, XSOIL_TWGI,&
                                   XDFSAT , XAVG_FSAT,                  &
                                   XSURF_TSWI, XSURF_TWG, XSURF_TWGI,   &
                                   XROOT_TSWI, XROOT_TWG, XROOT_TWGI,   &
                                   XFRD2_TSWI, XFRD2_TWG, XFRD2_TWGI,   &
                                   XFRD3_TSWI, XFRD3_TWG, XFRD3_TWGI,   &                                   
                                   XSNOWLIQ, XSNOWTEMP, XDLAI_EFFC,     &
                                   XFAPAR, XFAPIR, XDFAPARC, XDFAPIRC,  &
                                   XFAPAR_BS, XFAPIR_BS, XALT, XAVG_ALT,&
                                   XFLT, XAVG_FLT, XAVG_LAI
!
USE MODI_INIT_IO_SURF_n
USE MODI_WRITE_SURF
USE MODI_WRITE_SURF_FIELD2D
USE MODI_END_IO_SURF_n
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*       0.1   Declarations of arguments
!              -------------------------
!
 CHARACTER(LEN=6),  INTENT(IN)  :: HPROGRAM ! program calling
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
INTEGER           :: IRESP          ! IRESP  : return-code if a problem appears
 CHARACTER(LEN=12) :: YRECFM         ! Name of the article to be read
 CHARACTER(LEN=100):: YCOMMENT       ! Comment string
 CHARACTER(LEN=100):: YCOMMENTUNIT   ! Comment string : unit of the datas in the field to write
 CHARACTER(LEN=2)  :: YLVL
 CHARACTER(LEN=20) :: YFORM
!
INTEGER           :: JLAYER, IWORK, JJ, IDEPTH
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
!
!         Initialisation for IO
!
IF (LHOOK) CALL DR_HOOK('WRITE_DIAG_MISC_ISBA_N',0,ZHOOK_HANDLE)
 CALL INIT_IO_SURF_n(HPROGRAM,'NATURE','ISBA  ','WRITE')
!
!-------------------------------------------------------------------------------
!
IF (LSURF_MISC_BUDGET) THEN
  !
  !*       2.     Miscellaneous fields :
  !
  !-------------------------------------------------------------------------------
  !
  !        2.1    Halstead coefficient
  !               --------------------
  !
  YRECFM='HV_ISBA'
  YCOMMENT='Halstead coefficient averaged over tile nature (-)'
  CALL WRITE_SURF(HPROGRAM,YRECFM,XAVG_HV(:),IRESP,HCOMMENT=YCOMMENT)
  !
  !        2.2    Snow fractions
  !               --------------
  !
  YRECFM='PSNG_ISBA'
  YCOMMENT='snow fraction over ground averaged over tile nature (-)'
  CALL WRITE_SURF(HPROGRAM,YRECFM,XAVG_PSNG(:),IRESP,HCOMMENT=YCOMMENT)
  !
  YRECFM='PSNV_ISBA'
  YCOMMENT='snow fraction over vegetation averaged over tile nature (-)'
  CALL WRITE_SURF(HPROGRAM,YRECFM,XAVG_PSNV(:),IRESP,HCOMMENT=YCOMMENT)
  !
  YRECFM='PSN_ISBA'
  YCOMMENT='total snow fraction averaged over tile nature (-)'
  CALL WRITE_SURF(HPROGRAM,YRECFM,XAVG_PSN(:),IRESP,HCOMMENT=YCOMMENT)
  !
  !        2.3    Total Albedo and surface temperature
  !               ------------------------------------
  !
  YRECFM='TALB_ISBA'
  YCOMMENT='total albedo over tile nature (-)'
  CALL WRITE_SURF(HPROGRAM,YRECFM,XAVG_ALBT(:),IRESP,HCOMMENT=YCOMMENT)
  !
  IF (TSNOW%SCHEME=='3-L' .OR. TSNOW%SCHEME=='CRO') THEN
    !        
    YRECFM='TS_ISBA'
    YCOMMENT='total surface temperature (isba+snow) over tile nature'
    CALL WRITE_SURF(HPROGRAM,YRECFM,XAVG_TS(:),IRESP,HCOMMENT=YCOMMENT)
    !
    YRECFM='TSRAD_ISBA'
    YCOMMENT='total radiative surface temperature (isba+snow) over tile nature'
    CALL WRITE_SURF(HPROGRAM,YRECFM,XAVG_TSRAD(:),IRESP,HCOMMENT=YCOMMENT)
    !
  END IF
  !
  !        2.4    Soil Wetness Index, Water content and active layer depth
  !               --------------------------------------------------------
  !  
  IF(CISBA=='DIF')THEN
    !
    IWORK = NWG_SIZE
    !          
    DO JLAYER = 1,NGROUND_LAYER
     DO JJ=1,SIZE(NWG_LAYER,1)
        IDEPTH=MAXVAL(NWG_LAYER(JJ,:),NWG_LAYER(JJ,:)/=NUNDEF)
        IF(JLAYER>IDEPTH)THEN  
          XAVG_SWI (JJ,JLAYER) = XUNDEF
          XAVG_TSWI(JJ,JLAYER) = XUNDEF
        ENDIF
      ENDDO 
    ENDDO
  ELSE
    IWORK = NGROUND_LAYER    
  ENDIF         
  !
  DO JLAYER=1,IWORK
    !
    WRITE(YLVL,'(I2)') JLAYER
    !
    YRECFM='SWI'//ADJUSTL(YLVL(:LEN_TRIM(YLVL)))
    YRECFM=YRECFM(:LEN_TRIM(YRECFM))//'_ISBA'
    YFORM='(A29,I1.1,A4)'
    IF (JLAYER >= 10)  YFORM='(A29,I2.2,A4)'
    WRITE(YCOMMENT,FMT=YFORM) 'soil wetness index for layer ',JLAYER,' (-)'
    CALL WRITE_SURF(HPROGRAM,YRECFM,XAVG_SWI(:,JLAYER),IRESP,HCOMMENT=YCOMMENT)
    !
    YRECFM='TSWI'//ADJUSTL(YLVL(:LEN_TRIM(YLVL)))
    YRECFM=YRECFM(:LEN_TRIM(YRECFM))//'_ISBA'
    YFORM='(A29,I1.1,A4)'
    IF (JLAYER >= 10)  YFORM='(A29,I2.2,A4)'
    WRITE(YCOMMENT,FMT=YFORM) 'total swi (liquid+solid) for layer ',JLAYER,' (-)'
    CALL WRITE_SURF(HPROGRAM,YRECFM,XAVG_TSWI(:,JLAYER),IRESP,HCOMMENT=YCOMMENT)
    !
  END DO
  !
  YRECFM='TSWI_T_ISBA'
  YCOMMENT='total soil wetness index over the soil column (-)'
  CALL WRITE_SURF(HPROGRAM,YRECFM,XSOIL_TSWI(:),IRESP,HCOMMENT=YCOMMENT)
  !
  YRECFM='WGTOT_T_ISBA'
  YCOMMENT='total water content (liquid+solid) over the soil column (kg/m2)'
  CALL WRITE_SURF(HPROGRAM,YRECFM,XSOIL_TWG(:),IRESP,HCOMMENT=YCOMMENT)
  !
  YRECFM='WGI_T_ISBA'
  YCOMMENT='total ice content (solid) over the soil column (kg/m2)'
  CALL WRITE_SURF(HPROGRAM,YRECFM,XSOIL_TWGI(:),IRESP,HCOMMENT=YCOMMENT)
  !
  IF(CISBA=='DIF') THEN
    !
    IF (LSURF_MISC_DIF)THEN
      !
      YRECFM='TSWI_R_ISBA'
      YCOMMENT='total soil wetness index over the root zone (-)'
      CALL WRITE_SURF(HPROGRAM,YRECFM,XROOT_TSWI(:),IRESP,HCOMMENT=YCOMMENT)
      !
      YRECFM='WGTOT_R_ISBA'
      YCOMMENT='total water content (liquid+solid) over the root zone (kg/m2)'
      CALL WRITE_SURF(HPROGRAM,YRECFM,XROOT_TWG(:),IRESP,HCOMMENT=YCOMMENT)
      !
      YRECFM='WGI_R_ISBA'
      YCOMMENT='total ice content (solid) over the root zone (kg/m2)'
      CALL WRITE_SURF(HPROGRAM,YRECFM,XROOT_TWGI(:),IRESP,HCOMMENT=YCOMMENT)  
      !    
      YRECFM='TSWI_S_ISBA'
      YCOMMENT='total soil wetness index over the surface (-)'
      CALL WRITE_SURF(HPROGRAM,YRECFM,XSURF_TSWI(:),IRESP,HCOMMENT=YCOMMENT)
      !
      YRECFM='WG_S_ISBA'
      YCOMMENT='liquid water content over the surface (m3/m3)'
      CALL WRITE_SURF(HPROGRAM,YRECFM,XSURF_TWG(:),IRESP,HCOMMENT=YCOMMENT)
      !
      YRECFM='WGI_S_ISBA'
      YCOMMENT='ice content over the surface (m3/m3)'
      CALL WRITE_SURF(HPROGRAM,YRECFM,XSURF_TWGI(:),IRESP,HCOMMENT=YCOMMENT)  
      !
      YRECFM='TSWI_D2_ISBA'
      YCOMMENT='total soil wetness index over comparable FR-DG2 reservoir (-)'
      CALL WRITE_SURF(HPROGRAM,YRECFM,XFRD2_TSWI(:),IRESP,HCOMMENT=YCOMMENT)
      !
      YRECFM='WG_D2_ISBA'
      YCOMMENT='liquid water content over comparable FR-DG2 reservoir (m3/m3)'
      CALL WRITE_SURF(HPROGRAM,YRECFM,XFRD2_TWG(:),IRESP,HCOMMENT=YCOMMENT)
      !
      YRECFM='WGI_D2_ISBA'
      YCOMMENT='ice content over comparable FR-DG2 reservoir (m3/m3)'
      CALL WRITE_SURF(HPROGRAM,YRECFM,XFRD2_TWGI(:),IRESP,HCOMMENT=YCOMMENT)  
      !
      YRECFM='TSWI_D3_ISBA'
      YCOMMENT='total soil wetness index over comparable FR-DG3 reservoir (-)'
      CALL WRITE_SURF(HPROGRAM,YRECFM,XFRD3_TSWI(:),IRESP,HCOMMENT=YCOMMENT)
      !
      YRECFM='WG_D3_ISBA'
      YCOMMENT='liquid water content over comparable FR-DG3 reservoir (m3/m3)'
      CALL WRITE_SURF(HPROGRAM,YRECFM,XFRD3_TWG(:),IRESP,HCOMMENT=YCOMMENT)
      !
      YRECFM='WGI_D3_ISBA'
      YCOMMENT='ice content over comparable FR-DG3 reservoir (m3/m3)'
      CALL WRITE_SURF(HPROGRAM,YRECFM,XFRD3_TWGI(:),IRESP,HCOMMENT=YCOMMENT)  
      !
    ENDIF
    !
    YRECFM='ALT_ISBA'
    YCOMMENT='active layer thickness over permafrost (m)'
    CALL WRITE_SURF(HPROGRAM,YRECFM,XAVG_ALT(:),IRESP,HCOMMENT=YCOMMENT)
    !
    YRECFM='FLT_ISBA'
    YCOMMENT='frozen layer thickness over non-permafrost (m)'
    CALL WRITE_SURF(HPROGRAM,YRECFM,XAVG_FLT(:),IRESP,HCOMMENT=YCOMMENT)
    !
  ENDIF
  !
  !        2.5    Snow outputs
  !               -------------
  !
  YRECFM='WSNOW_T_ISBA'
  YCOMMENT='Total_snow_reservoir (kg/m2)'
  CALL WRITE_SURF(HPROGRAM,YRECFM,XAVG_TWSNOW(:),IRESP,HCOMMENT=YCOMMENT)
  !
  YRECFM='DSNOW_T_ISBA'
  YCOMMENT='Total_snow_depth (m)'
  CALL WRITE_SURF(HPROGRAM,YRECFM,XAVG_TDSNOW(:),IRESP,HCOMMENT=YCOMMENT)
  !
  YRECFM='TSNOW_T_ISBA'
  YCOMMENT='Total_snow_temperature (K)'
  CALL WRITE_SURF(HPROGRAM,YRECFM,XAVG_TTSNOW(:),IRESP,HCOMMENT=YCOMMENT)
  !
  !        2.6    SGH scheme
  !               ----------
  !
  IF(CRUNOFF=='SGH '.OR.CRUNOFF=='DT92')THEN     
    YRECFM='FSAT_ISBA'
    YCOMMENT='Soil saturated fraction (-)'
    CALL WRITE_SURF(HPROGRAM,YRECFM,XAVG_FSAT(:),IRESP,HCOMMENT=YCOMMENT)
  ENDIF
  !
  IF(CRAIN=='SGH ')THEN
    YRECFM='MUF_ISBA'
    YCOMMENT='fraction of the grid cell reached by the rainfall (-)'
    CALL WRITE_SURF(HPROGRAM,YRECFM,XMUF(:),IRESP,HCOMMENT=YCOMMENT)
  ENDIF
  !
  !        2.7    Flooding scheme
  !               ---------------
  !
  IF(LFLOOD)THEN
    !
    YRECFM='FFG_ISBA'
    YCOMMENT='flood fraction over ground averaged over tile nature (-)'
    CALL WRITE_SURF(HPROGRAM,YRECFM,XAVG_FFG(:),IRESP,HCOMMENT=YCOMMENT)
    !
    YRECFM='FFV_ISBA'
    YCOMMENT='flood fraction over vegetation averaged over tile nature (-)'
    CALL WRITE_SURF(HPROGRAM,YRECFM,XAVG_FFV(:),IRESP,HCOMMENT=YCOMMENT)
    !
    YRECFM='FF_ISBA'
    YCOMMENT='total flood fraction averaged over tile nature (-)'
    CALL WRITE_SURF(HPROGRAM,YRECFM,XAVG_FF(:),IRESP,HCOMMENT=YCOMMENT)
    !
    YRECFM='FFLOOD_ISBA'
    YCOMMENT='Grdi-cell potential flood fraction (-)'
    CALL WRITE_SURF(HPROGRAM,YRECFM,XFFLOOD(:),IRESP,HCOMMENT=YCOMMENT)
    !
    YRECFM='PIFLOOD_ISBA'
    YCOMMENT='Grdi-cell Potential_floodplain_infiltration (kg/m2s)'
    CALL WRITE_SURF(HPROGRAM,YRECFM,XPIFLOOD(:),IRESP,HCOMMENT=YCOMMENT)
    !
  ENDIF
  !
  !        2.8    Total LAI
  !               ---------
  !
  IF(CPHOTO/='NON')THEN        
    YRECFM='LAI_ISBA'
    YCOMMENT='leaf area index (m2/m2)'
    CALL WRITE_SURF(HPROGRAM,YRECFM,XAVG_LAI(:),IRESP,HCOMMENT=YCOMMENT)
  ENDIF
  !  
  !*       3.     Miscellaneous fields for each patch :
  !               -------------------------------------
  !
  !----------------------------------------------------------------------------
  !User wants (or not) patch output
  IF(LPATCH_BUDGET)THEN
    !----------------------------------------------------------------------------
    !
    !        3.1    Soil Wetness Index and active layer depth
    !               -----------------------------------------   
    !
    DO JLAYER=1,IWORK
      !
      WRITE(YLVL,'(I2)') JLAYER
      !
      YRECFM='SWI'//ADJUSTL(YLVL(:LEN_TRIM(YLVL)))
      YFORM='(A39,I1.1)'
      IF (JLAYER >= 10)  YFORM='(A39,I2.2)'
      WRITE(YCOMMENT,FMT=YFORM) 'soil wetness index per patch for layer ',JLAYER
      YCOMMENTUNIT='-'
      CALL WRITE_SURF_FIELD2D(HPROGRAM,XSWI(:,JLAYER,:),YRECFM,YCOMMENT,YCOMMENTUNIT)
      !
      YRECFM='TSWI'//ADJUSTL(YLVL(:LEN_TRIM(YLVL)))
      YFORM='(A39,I1.1)'
      IF (JLAYER >= 10)  YFORM='(A39,I2.2)'
      WRITE(YCOMMENT,FMT=YFORM) 'total swi (liquid+solid) per patch for layer ',JLAYER
      YCOMMENTUNIT='-'
      CALL WRITE_SURF_FIELD2D(HPROGRAM,XTSWI(:,JLAYER,:),YRECFM,YCOMMENT,YCOMMENTUNIT)
      !
    END DO
    !
    IF(CISBA=='DIF')THEN
      !
      YRECFM='ALT_P'
      YCOMMENT='active layer thickness over permafrost per patch'
      YCOMMENTUNIT='m'
      CALL WRITE_SURF_FIELD2D(HPROGRAM,XALT(:,:),YRECFM,YCOMMENT,YCOMMENTUNIT)
      !
      YRECFM='FLT_P'
      YCOMMENT='frozen layer thickness over non-permafrost per patch'
      YCOMMENTUNIT='m'
      CALL WRITE_SURF_FIELD2D(HPROGRAM,XFLT(:,:),YRECFM,YCOMMENT,YCOMMENTUNIT)
      !
    ENDIF
    !    
    !        3.2    Snow fractions
    !               --------------
    !
    YRECFM='PSNG'
    YCOMMENT='snow fraction per patch over ground'
    YCOMMENTUNIT='-'
    CALL WRITE_SURF_FIELD2D(HPROGRAM,XDPSNG(:,:),YRECFM,YCOMMENT,YCOMMENTUNIT)
    !
    YRECFM='PSNV'
    YCOMMENT='snow fraction per patch over vegetation'
    YCOMMENTUNIT='-'
    CALL WRITE_SURF_FIELD2D(HPROGRAM,XDPSNV(:,:),YRECFM,YCOMMENT,YCOMMENTUNIT)
    !
    YRECFM='PSN'
    YCOMMENT='total snow fraction per patch'
    YCOMMENTUNIT='-'
    CALL WRITE_SURF_FIELD2D(HPROGRAM,XDPSN(:,:),YRECFM,YCOMMENT,YCOMMENTUNIT)
    !
    !        3.3    SGH scheme
    !               ----------
    !
    IF(CRUNOFF=='DT92')THEN     
      YRECFM='FSAT_P'
      YCOMMENT='Soil saturated fraction per patch'
      YCOMMENTUNIT='-'
      CALL WRITE_SURF_FIELD2D(HPROGRAM,XDFSAT(:,:),YRECFM,YCOMMENT,YCOMMENTUNIT)
    ENDIF
    !
    !        3.3    Flood fractions
    !               --------------
    !
    IF(LFLOOD)THEN
      !        
      YRECFM='FFG_P'
      YCOMMENT='flood fraction per patch over ground'
      YCOMMENTUNIT='-'
      CALL WRITE_SURF_FIELD2D(HPROGRAM,XDFFG(:,:),YRECFM,YCOMMENT,YCOMMENTUNIT)
      !
      YRECFM='FFV_P'
      YCOMMENT='flood fraction per patch over vegetation'
      YCOMMENTUNIT='-'
      CALL WRITE_SURF_FIELD2D(HPROGRAM,XDFFV(:,:),YRECFM,YCOMMENT,YCOMMENTUNIT)
      !
      YRECFM='FF_P'
      YCOMMENT='total flood fraction per patch'
      YCOMMENTUNIT='-'
      CALL WRITE_SURF_FIELD2D(HPROGRAM,XDFF(:,:),YRECFM,YCOMMENT,YCOMMENTUNIT)
      !
    ENDIF
    !
    !        3.4    Total Albedo
    !               ------------
    !
    YRECFM='TALB'
    YCOMMENT='total albedo per patch'
    !
    CALL WRITE_SURF(HPROGRAM,YRECFM,XALBT(:,:),IRESP,HCOMMENT=YCOMMENT)
    !
    IF (TSNOW%SCHEME=='3-L' .OR. TSNOW%SCHEME=='CRO') THEN
      YRECFM='TS_P'
      YCOMMENT='total surface temperature (isba+snow) per patch'
      YCOMMENTUNIT='-'
      CALL WRITE_SURF_FIELD2D(HPROGRAM,XTS(:,:),YRECFM,YCOMMENT,YCOMMENTUNIT)
      YRECFM='TSRAD_P'
      YCOMMENT='total radiative surface temperature (isba+snow) per patch'
      YCOMMENTUNIT='-'
      CALL WRITE_SURF_FIELD2D(HPROGRAM,XTSRAD(:,:),YRECFM,YCOMMENT,YCOMMENTUNIT)
    ENDIF
    !
    !        3.5    Halstead coefficient
    !               --------------------
    !
    YRECFM='HV'
    YCOMMENT='Halstead coefficient per patch'
    YCOMMENTUNIT='-'
    CALL WRITE_SURF_FIELD2D(HPROGRAM,XHV(:,:),YRECFM,YCOMMENT,YCOMMENTUNIT)
    !
    !        3.6  Snow outputs 
    !        -----------------
    !
    YRECFM='WSNOW_VEGT'
    YCOMMENT='X_Y_WSNOW_VEG_TOT per patch'
    YCOMMENTUNIT='kg/m2'
    CALL WRITE_SURF_FIELD2D(HPROGRAM,XTWSNOW(:,:),YRECFM,YCOMMENT,YCOMMENTUNIT)
    !
    YRECFM='DSNOW_VEGT'
    YCOMMENT='X_Y_DSNOW_VEG_TOT per patch'
    YCOMMENTUNIT='m'
    CALL WRITE_SURF_FIELD2D(HPROGRAM,XTDSNOW(:,:),YRECFM,YCOMMENT,YCOMMENTUNIT)
    !
    YRECFM='TSNOW_VEGT'
    YCOMMENT='X_Y_TSNOW_VEG_TOT per patch'
    YCOMMENTUNIT='k'
    CALL WRITE_SURF_FIELD2D(HPROGRAM,XTTSNOW(:,:),YRECFM,YCOMMENT,YCOMMENTUNIT)
    !
    IF (TSNOW%SCHEME=='3-L' .OR. TSNOW%SCHEME=='CRO') THEN
      !
      DO JLAYER=1,TSNOW%NLAYER
        !
        WRITE(YLVL,'(I2)') JLAYER
        !
        YRECFM='SNOWLIQ'//ADJUSTL(YLVL(:LEN_TRIM(YLVL)))
        YFORM='(A17,I1.1)'
        IF (JLAYER >= 10)  YFORM='(A17,I2.2)'
        WRITE(YCOMMENT,FMT=YFORM) 'snow liquid water',JLAYER
	YCOMMENTUNIT='m'
	CALL WRITE_SURF_FIELD2D(HPROGRAM,XSNOWLIQ(:,JLAYER,:),YRECFM,YCOMMENT,YCOMMENTUNIT)
        !
        YRECFM='SNOWTEMP'//ADJUSTL(YLVL(:LEN_TRIM(YLVL)))
        YFORM='(A16,I1.1)'
        IF (JLAYER >= 10)  YFORM='(A16,I2.2)'
        WRITE(YCOMMENT,FMT=YFORM) 'snow temperature',JLAYER
	YCOMMENTUNIT='K'
	CALL WRITE_SURF_FIELD2D(HPROGRAM,XSNOWTEMP(:,JLAYER,:),YRECFM,YCOMMENT,YCOMMENTUNIT)
        !
      END DO
      !        
    ENDIF
    !
  END IF
  !
  IF (LAGRIP) THEN
    !
    !        2.8    Irrigation threshold
    !               --------------------
    !
    YRECFM='IRRISEUIL'
    YCOMMENT='irrigation threshold per patch'
    YCOMMENTUNIT='-'
    CALL WRITE_SURF_FIELD2D(HPROGRAM,XSEUIL(:,:),YRECFM,YCOMMENT,YCOMMENTUNIT)
    !
  ENDIF
  !
  IF (LTR_ML) THEN
    !
    YRECFM='FAPAR'
    YCOMMENT='FAPAR'
    YCOMMENTUNIT='-'
    CALL WRITE_SURF_FIELD2D(HPROGRAM,XFAPAR(:,:),YRECFM,YCOMMENT,YCOMMENTUNIT)
    !
    YRECFM='FAPIR'
    YCOMMENT='FAPIR'
    YCOMMENTUNIT='-'
    CALL WRITE_SURF_FIELD2D(HPROGRAM,XFAPIR(:,:),YRECFM,YCOMMENT,YCOMMENTUNIT)
    !
    YRECFM='FAPAR_BS'
    YCOMMENT='FAPAR_BS'
    YCOMMENTUNIT='-'
    CALL WRITE_SURF_FIELD2D(HPROGRAM,XFAPAR_BS(:,:),YRECFM,YCOMMENT,YCOMMENTUNIT)
    !
    YRECFM='FAPIR_BS'
    YCOMMENT='FAPIR_BS'
    YCOMMENTUNIT='-'
    CALL WRITE_SURF_FIELD2D(HPROGRAM,XFAPIR_BS(:,:),YRECFM,YCOMMENT,YCOMMENTUNIT)
    !
    YRECFM='DFAPARC'
    YCOMMENT='DFAPARC'
    YCOMMENTUNIT='-'
    CALL WRITE_SURF_FIELD2D(HPROGRAM,XDFAPARC(:,:),YRECFM,YCOMMENT,YCOMMENTUNIT)
    !
    YRECFM='DFAPIRC'
    YCOMMENT='DFAPIRC'
    YCOMMENTUNIT='-'
    CALL WRITE_SURF_FIELD2D(HPROGRAM,XDFAPIRC(:,:),YRECFM,YCOMMENT,YCOMMENTUNIT)
    !
    YRECFM='DLAI_EFFC'
    YCOMMENT='DLAI_EFFC'
    YCOMMENTUNIT='m2/m2'
    CALL WRITE_SURF_FIELD2D(HPROGRAM,XDLAI_EFFC(:,:),YRECFM,YCOMMENT,YCOMMENTUNIT)
    !
  ENDIF
  !  
ENDIF
!         End of IO
!
 CALL END_IO_SURF_n(HPROGRAM)
IF (LHOOK) CALL DR_HOOK('WRITE_DIAG_MISC_ISBA_N',1,ZHOOK_HANDLE)
!
END SUBROUTINE WRITE_DIAG_MISC_ISBA_n
