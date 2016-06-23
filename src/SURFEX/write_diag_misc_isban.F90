!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE WRITE_DIAG_MISC_ISBA_n (DTCO, DGU, U, DGI, DGMI, I, &
                                         HPROGRAM)
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
!!      P. Le Moigne   *Meteo France*
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
!!      B. Decharme  06/13   All snow outputs noted SN
!!                           XTSRAD_NAT instead of XAVG_TSRAD
!!                           delete NWG_SIZE
!!                           water table depth
!!      M.Moge    01/2016  using WRITE_SURF_FIELD2D/3D for 2D/3D surfex fields writes
!!
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
!
USE MODD_DATA_COVER_n, ONLY : DATA_COVER_t
USE MODD_DIAG_SURF_ATM_n, ONLY : DIAG_SURF_ATM_t
USE MODD_SURF_ATM_n, ONLY : SURF_ATM_t
USE MODD_DIAG_ISBA_n, ONLY : DIAG_ISBA_t
USE MODD_DIAG_MISC_ISBA_n, ONLY : DIAG_MISC_ISBA_t
USE MODD_ISBA_n, ONLY : ISBA_t
!
USE MODD_SURF_PAR,        ONLY :   NUNDEF, XUNDEF
!
USE MODD_ASSIM, ONLY : LASSIM, CASSIM_ISBA, NVAR
!                                 
USE MODD_AGRI,            ONLY :   LAGRIP
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
!
TYPE(DATA_COVER_t), INTENT(INOUT) :: DTCO
TYPE(DIAG_SURF_ATM_t), INTENT(INOUT) :: DGU
TYPE(SURF_ATM_t), INTENT(INOUT) :: U
TYPE(DIAG_ISBA_t), INTENT(INOUT) :: DGI
TYPE(DIAG_MISC_ISBA_t), INTENT(INOUT) :: DGMI
TYPE(ISBA_t), INTENT(INOUT) :: I
!
 CHARACTER(LEN=6),  INTENT(IN)  :: HPROGRAM ! program calling
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
INTEGER           :: IRESP          ! IRESP  : return-code if a problem appears
 CHARACTER(LEN=1) :: YVAR
 CHARACTER(LEN=12) :: YRECFM         ! Name of the article to be read
 CHARACTER(LEN=100):: YCOMMENT       ! Comment string
 CHARACTER(LEN=100):: YCOMMENTUNIT   ! Comment string : unit of the datas in the field to write
 CHARACTER(LEN=2)  :: YLVL
 CHARACTER(LEN=20) :: YFORM
!
INTEGER           :: JLAYER, JJ, IDEPTH, JVAR
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
!
!         Initialisation for IO
!
IF (LHOOK) CALL DR_HOOK('WRITE_DIAG_MISC_ISBA_N',0,ZHOOK_HANDLE)
 CALL INIT_IO_SURF_n(DTCO, DGU, U, &
                     HPROGRAM,'NATURE','ISBA  ','WRITE')
!
!-------------------------------------------------------------------------------
!
IF (DGMI%LSURF_MISC_BUDGET) THEN
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
  CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,DGMI%XAVG_HV(:),IRESP,HCOMMENT=YCOMMENT)
  !
  !        2.2    Snow fractions
  !               --------------
  !
  YRECFM='PSNG_ISBA'
  YCOMMENT='snow fraction over ground averaged over tile nature (-)'
  CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,DGMI%XAVG_PSNG(:),IRESP,HCOMMENT=YCOMMENT)
  !
  YRECFM='PSNV_ISBA'
  YCOMMENT='snow fraction over vegetation averaged over tile nature (-)'
  CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,DGMI%XAVG_PSNV(:),IRESP,HCOMMENT=YCOMMENT)
  !
  YRECFM='PSN_ISBA'
  YCOMMENT='total snow fraction averaged over tile nature (-)'
  CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,DGMI%XAVG_PSN(:),IRESP,HCOMMENT=YCOMMENT)
  !
  !        2.3    Total Albedo and surface temperature
  !               ------------------------------------
  !
  YRECFM='TALB_ISBA'
  YCOMMENT='total albedo over tile nature (-)'
  CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,DGMI%XAVG_ALBT(:),IRESP,HCOMMENT=YCOMMENT)
  !
  IF (I%TSNOW%SCHEME=='3-L' .OR. I%TSNOW%SCHEME=='CRO') THEN
    !        
    YRECFM='TS_ISBA'
    YCOMMENT='total surface temperature (isba+snow) over tile nature'
    CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,DGI%XAVG_TS(:),IRESP,HCOMMENT=YCOMMENT)
    !
    YRECFM='TSRAD_ISBA'
    YCOMMENT='total radiative surface temperature (isba+snow) over tile nature'
    CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,I%XTSRAD_NAT(:),IRESP,HCOMMENT=YCOMMENT)
    !
  END IF
  !
  !        2.4    Soil Wetness Index, Water content and active layer depth
  !               --------------------------------------------------------
  !  
  IF(I%CISBA=='DIF')THEN
    DO JLAYER = 1,I%NGROUND_LAYER
     DO JJ=1,SIZE(I%NWG_LAYER,1)
        IDEPTH=MAXVAL(I%NWG_LAYER(JJ,:),I%NWG_LAYER(JJ,:)/=NUNDEF)
        IF(JLAYER>IDEPTH)THEN  
          DGMI%XAVG_SWI (JJ,JLAYER) = XUNDEF
          DGMI%XAVG_TSWI(JJ,JLAYER) = XUNDEF
        ENDIF
      ENDDO 
    ENDDO
  ENDIF         
  !
  DO JLAYER=1,I%NGROUND_LAYER
    !
    WRITE(YLVL,'(I2)') JLAYER
    !
    YRECFM='SWI'//ADJUSTL(YLVL(:LEN_TRIM(YLVL)))
    YRECFM=YRECFM(:LEN_TRIM(YRECFM))//'_ISBA'
    YFORM='(A29,I1.1,A4)'
    IF (JLAYER >= 10)  YFORM='(A29,I2.2,A4)'
    WRITE(YCOMMENT,FMT=YFORM) 'soil wetness index for layer ',JLAYER,' (-)'
    CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,DGMI%XAVG_SWI(:,JLAYER),IRESP,HCOMMENT=YCOMMENT)
    !
    YRECFM='TSWI'//ADJUSTL(YLVL(:LEN_TRIM(YLVL)))
    YRECFM=YRECFM(:LEN_TRIM(YRECFM))//'_ISBA'
    YFORM='(A29,I1.1,A4)'
    IF (JLAYER >= 10)  YFORM='(A29,I2.2,A4)'
    WRITE(YCOMMENT,FMT=YFORM) 'total swi (liquid+solid) for layer ',JLAYER,' (-)'
    CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,DGMI%XAVG_TSWI(:,JLAYER),IRESP,HCOMMENT=YCOMMENT)
    !
  END DO
  !
  YRECFM='SWI_T_ISBA'
  YCOMMENT='soil wetness index over the soil column (-)'
  CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,DGMI%XSOIL_SWI(:),IRESP,HCOMMENT=YCOMMENT)
  !
  YRECFM='TSWI_T_ISBA'
  YCOMMENT='total soil wetness index over the soil column (-)'
  CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,DGMI%XSOIL_TSWI(:),IRESP,HCOMMENT=YCOMMENT)
  !
  YRECFM='WGTOT_T_ISBA'
  YCOMMENT='total water content (liquid+solid) over the soil column (kg/m2)'
  CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,DGMI%XSOIL_TWG(:),IRESP,HCOMMENT=YCOMMENT)
  !
  YRECFM='WGI_T_ISBA'
  YCOMMENT='total ice content (solid) over the soil column (kg/m2)'
  CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,DGMI%XSOIL_TWGI(:),IRESP,HCOMMENT=YCOMMENT)
  !
  YRECFM='WGTOT_ISBA'
  YCOMMENT='total volumetric water content (liquid+solid) over the soil column (m3/m3)'
  CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,DGMI%XSOIL_WG(:),IRESP,HCOMMENT=YCOMMENT)
  !
  YRECFM='WGI_ISBA'
  YCOMMENT='total volumetric ice content (solid) over the soil column (m3/m3)'
  CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,DGMI%XSOIL_WGI(:),IRESP,HCOMMENT=YCOMMENT)
  !
  IF(I%CISBA=='DIF') THEN
    !
    IF (DGMI%LSURF_MISC_DIF)THEN
      !
      YRECFM='TSWI_D2_ISBA'
      YCOMMENT='total soil wetness index over comparable FR-DG2 reservoir (-)'
      CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,DGMI%XFRD2_TSWI(:),IRESP,HCOMMENT=YCOMMENT)
      !
      YRECFM='WG_D2_ISBA'
      YCOMMENT='liquid water content over comparable FR-DG2 reservoir (m3/m3)'
      CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,DGMI%XFRD2_TWG(:),IRESP,HCOMMENT=YCOMMENT)
      !
      YRECFM='WGI_D2_ISBA'
      YCOMMENT='ice content over comparable FR-DG2 reservoir (m3/m3)'
      CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,DGMI%XFRD2_TWGI(:),IRESP,HCOMMENT=YCOMMENT)  
      !
      YRECFM='TSWI_D3_ISBA'
      YCOMMENT='total soil wetness index over comparable FR-DG3 reservoir (-)'
      CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,DGMI%XFRD3_TSWI(:),IRESP,HCOMMENT=YCOMMENT)
      !
      YRECFM='WG_D3_ISBA'
      YCOMMENT='liquid water content over comparable FR-DG3 reservoir (m3/m3)'
      CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,DGMI%XFRD3_TWG(:),IRESP,HCOMMENT=YCOMMENT)
      !
      YRECFM='WGI_D3_ISBA'
      YCOMMENT='ice content over comparable FR-DG3 reservoir (m3/m3)'
      CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,DGMI%XFRD3_TWGI(:),IRESP,HCOMMENT=YCOMMENT)  
      !
    ENDIF
    !
    YRECFM='ALT_ISBA'
    YCOMMENT='active layer thickness over permafrost (m)'
    CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,DGMI%XAVG_ALT(:),IRESP,HCOMMENT=YCOMMENT)
    !
    YRECFM='FLT_ISBA'
    YCOMMENT='frozen layer thickness over non-permafrost (m)'
    CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,DGMI%XAVG_FLT(:),IRESP,HCOMMENT=YCOMMENT)
    !
  ENDIF
  !
  !        2.5    Snow outputs
  !               -------------
  !
  YRECFM='WSN_T_ISBA'
  YCOMMENT='Total_snow_reservoir (kg/m2)'
  CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,DGMI%XAVG_TWSNOW(:),IRESP,HCOMMENT=YCOMMENT)
  !
  YRECFM='DSN_T_ISBA'
  YCOMMENT='Total_snow_depth (m)'
  CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,DGMI%XAVG_TDSNOW(:),IRESP,HCOMMENT=YCOMMENT)
  !
  YRECFM='TSN_T_ISBA'
  YCOMMENT='Total_snow_temperature (K)'
  CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,DGMI%XAVG_TTSNOW(:),IRESP,HCOMMENT=YCOMMENT)
  !
  !        2.6    SGH scheme
  !               ----------
  !
  IF(I%CRUNOFF=='SGH '.OR.I%CRUNOFF=='DT92')THEN     
    YRECFM='FSAT_ISBA'
    YCOMMENT='Soil saturated fraction (-)'
    CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,DGMI%XAVG_FSAT(:),IRESP,HCOMMENT=YCOMMENT)
  ENDIF
  !
  IF(I%CRAIN=='SGH ')THEN
    YRECFM='MUF_ISBA'
    YCOMMENT='fraction of the grid cell reached by the rainfall (-)'
    CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,I%XMUF(:),IRESP,HCOMMENT=YCOMMENT)
  ENDIF
  !
  !        2.7    Flooding scheme
  !               ---------------
  !
  IF(I%LFLOOD)THEN
    !
    YRECFM='FFG_ISBA'
    YCOMMENT='flood fraction over ground averaged over tile nature (-)'
    CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,DGMI%XAVG_FFG(:),IRESP,HCOMMENT=YCOMMENT)
    !
    YRECFM='FFV_ISBA'
    YCOMMENT='flood fraction over vegetation averaged over tile nature (-)'
    CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,DGMI%XAVG_FFV(:),IRESP,HCOMMENT=YCOMMENT)
    !
    YRECFM='FF_ISBA'
    YCOMMENT='total flood fraction averaged over tile nature (-)'
    CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,DGMI%XAVG_FF(:),IRESP,HCOMMENT=YCOMMENT)
    !
    YRECFM='FFLOOD_ISBA'
    YCOMMENT='Grdi-cell potential flood fraction (-)'
    CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,I%XFFLOOD(:),IRESP,HCOMMENT=YCOMMENT)
    !
    YRECFM='PIFLOOD_ISBA'
    YCOMMENT='Grdi-cell Potential_floodplain_infiltration (kg/m2/s)'
    CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,I%XPIFLOOD(:),IRESP,HCOMMENT=YCOMMENT)
    !
  ENDIF
  !
  !        2.8    Total LAI
  !               ---------
  !
  IF(I%CPHOTO/='NON'.OR.I%NPATCH>1)THEN        
    YRECFM='LAI_ISBA'
    YCOMMENT='leaf area index (m2/m2)'
    CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,DGMI%XAVG_LAI(:),IRESP,HCOMMENT=YCOMMENT)
  ENDIF
  !
  !        2.9    Water table depth
  !               -----------------
  !
  IF(I%LWTD)THEN
    !
    YRECFM='FWTD_ISBA'
    YCOMMENT='grid-cell fraction of water table to rise'
    CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,I%XFWTD(:),IRESP,HCOMMENT=YCOMMENT)
    !
    YRECFM='WTD_ISBA'
    YCOMMENT='water table depth from RRM model or observation (m)'
    CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,I%XWTD(:),IRESP,HCOMMENT=YCOMMENT)
    !
  ENDIF
  !*       3.     Miscellaneous fields for each patch :
  !               -------------------------------------
  !
  !----------------------------------------------------------------------------
  !User wants (or not) patch output
  IF(DGI%LPATCH_BUDGET)THEN
    !----------------------------------------------------------------------------
    !
    !        3.1    Soil Wetness Index and active layer depth
    !               -----------------------------------------   
    !
    DO JLAYER=1,I%NGROUND_LAYER
      !
      WRITE(YLVL,'(I2)') JLAYER
      !
      YRECFM='SWI'//ADJUSTL(YLVL(:LEN_TRIM(YLVL)))
      YFORM='(A39,I1.1)'
      IF (JLAYER >= 10)  YFORM='(A39,I2.2)'
      WRITE(YCOMMENT,FMT=YFORM) 'soil wetness index per patch for layer ',JLAYER
      YCOMMENTUNIT='-'
      CALL WRITE_SURF_FIELD2D(DGU, U, &
                  HPROGRAM,DGMI%XSWI(:,JLAYER,:),YRECFM,YCOMMENT,YCOMMENTUNIT)
      !
      YRECFM='TSWI'//ADJUSTL(YLVL(:LEN_TRIM(YLVL)))
      YFORM='(A39,I1.1)'
      IF (JLAYER >= 10)  YFORM='(A39,I2.2)'
      WRITE(YCOMMENT,FMT=YFORM) 'total swi (liquid+solid) per patch for layer ',JLAYER
      YCOMMENTUNIT='-'
      CALL WRITE_SURF_FIELD2D(DGU, U, &
                  HPROGRAM,DGMI%XTSWI(:,JLAYER,:),YRECFM,YCOMMENT,YCOMMENTUNIT)
      !
    END DO
    !
    IF(I%CISBA=='DIF')THEN
      !
      YRECFM='ALT_P'
      YCOMMENT='active layer thickness over permafrost per patch '
      YCOMMENTUNIT='m'
      CALL WRITE_SURF_FIELD2D(DGU, U, &
                  HPROGRAM,DGMI%XALT(:,:),YRECFM,YCOMMENT,YCOMMENTUNIT)
      !
      YRECFM='FLT_P'
      YCOMMENT='frozen layer thickness over non-permafrost per patch'
      YCOMMENTUNIT='m'
      CALL WRITE_SURF_FIELD2D(DGU, U, &
                  HPROGRAM,DGMI%XFLT(:,:),YRECFM,YCOMMENT,YCOMMENTUNIT) 
      !
    ENDIF
    !    
    !        3.2    Snow fractions
    !               --------------
    !
    YRECFM='PSNG_P'
    YCOMMENT='snow fraction per patch over ground '
    YCOMMENTUNIT='-'
    CALL WRITE_SURF_FIELD2D(DGU, U, &
                  HPROGRAM,DGMI%XDPSNG(:,:),YRECFM,YCOMMENT,YCOMMENTUNIT)
    !
    YRECFM='PSNV_P'
    YCOMMENT='snow fraction per patch over vegetation'
    YCOMMENTUNIT='-'
    CALL WRITE_SURF_FIELD2D(DGU, U, &
                  HPROGRAM,DGMI%XDPSNV(:,:),YRECFM,YCOMMENT,YCOMMENTUNIT)
    !
    YRECFM='PSN_P'
    YCOMMENT='total snow fraction per patch'
    YCOMMENTUNIT='-'
    CALL WRITE_SURF_FIELD2D(DGU, U, &
                  HPROGRAM,DGMI%XDPSN(:,:),YRECFM,YCOMMENT,YCOMMENTUNIT)
    !
    !        3.3    SGH scheme
    !               ----------
    !
    IF(I%CRUNOFF=='DT92')THEN     
      YRECFM='FSAT_P'
      YCOMMENT='Soil saturated fraction per patch '
      YCOMMENTUNIT='-'
      CALL WRITE_SURF_FIELD2D(DGU, U, &
                  HPROGRAM,DGMI%XDFSAT(:,:),YRECFM,YCOMMENT,YCOMMENTUNIT)
    ENDIF
    !
    !        3.3    Flood fractions
    !               --------------
    !
    IF(I%LFLOOD)THEN
      !        
      YRECFM='FFG_P'
      YCOMMENT='flood fraction per patch over ground '
      YCOMMENTUNIT='-'      
      CALL WRITE_SURF_FIELD2D(DGU, U, &
                  HPROGRAM,DGMI%XDFFG(:,:),YRECFM,YCOMMENT,YCOMMENTUNIT)
      !
      YRECFM='FFV_P'
      YCOMMENT='flood fraction per patch over vegetation'
      YCOMMENTUNIT='-'      
      CALL WRITE_SURF_FIELD2D(DGU, U, &
                  HPROGRAM,DGMI%XDFFV(:,:),YRECFM,YCOMMENT,YCOMMENTUNIT)
      !
      YRECFM='FF_P'
      YCOMMENT='total flood fraction per patch'
      YCOMMENTUNIT='-'      
      CALL WRITE_SURF_FIELD2D(DGU, U, &
                  HPROGRAM,DGMI%XDFF(:,:),YRECFM,YCOMMENT,YCOMMENTUNIT)
      !
    ENDIF
    !
    !        3.4    Total Albedo
    !               ------------
    !
    YRECFM='TALB'
    YCOMMENT='total albedo per patch'
    !
    CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,DGMI%XALBT(:,:),IRESP,HCOMMENT=YCOMMENT)
    !
    IF (I%TSNOW%SCHEME=='3-L' .OR. I%TSNOW%SCHEME=='CRO') THEN
      YRECFM='TS_P'
      YCOMMENT='total surface temperature (isba+snow) per patch'
      YCOMMENTUNIT='-'      
      CALL WRITE_SURF_FIELD2D(DGU, U, &
                  HPROGRAM,DGI%XTS(:,:),YRECFM,YCOMMENT,YCOMMENTUNIT)
      YRECFM='TSRAD_P'
      YCOMMENT='total radiative surface temperature (isba+snow) per patch'
      YCOMMENTUNIT='-'      
      CALL WRITE_SURF_FIELD2D(DGU, U, &
                  HPROGRAM,DGI%XTSRAD(:,:),YRECFM,YCOMMENT,YCOMMENTUNIT)
    ENDIF
    !
    !        3.5    Halstead coefficient
    !               --------------------
    !
    YRECFM='HV'
    YCOMMENT='Halstead coefficient per patch'
    YCOMMENTUNIT='-'    
    CALL WRITE_SURF_FIELD2D(DGU, U, &
                  HPROGRAM,DGMI%XHV(:,:),YRECFM,YCOMMENT,YCOMMENTUNIT)
    !
    !        3.6  Snow outputs 
    !        -----------------
    !
    YRECFM='WSN_T_P'
    YCOMMENT='X_Y_WSNOW_TOT per patch'
    YCOMMENTUNIT='kg/m2'    
    CALL WRITE_SURF_FIELD2D(DGU, U, &
                  HPROGRAM,DGMI%XTWSNOW(:,:),YRECFM,YCOMMENT,YCOMMENTUNIT)
    !
    YRECFM='DSN_T_P'
    YCOMMENT='X_Y_DSNOW_TOT per patch'
    YCOMMENTUNIT='m'    
    CALL WRITE_SURF_FIELD2D(DGU, U, &
                  HPROGRAM,DGMI%XTDSNOW(:,:),YRECFM,YCOMMENT,YCOMMENTUNIT)
    !
    YRECFM='TSN_T_P'
    YCOMMENT='X_Y_TSNOW_TOT per patch'
    YCOMMENTUNIT='k'    
    CALL WRITE_SURF_FIELD2D(DGU, U, &
                  HPROGRAM,DGMI%XTTSNOW(:,:),YRECFM,YCOMMENT,YCOMMENTUNIT)
    !
    IF (I%TSNOW%SCHEME=='3-L' .OR. I%TSNOW%SCHEME=='CRO') THEN
      !
      DO JLAYER=1,I%TSNOW%NLAYER
        !
        WRITE(YLVL,'(I2)') JLAYER
        !
        YRECFM='SNOWLIQ'//ADJUSTL(YLVL(:LEN_TRIM(YLVL)))
        YFORM='(A17,I1.1)'
        IF (JLAYER >= 10)  YFORM='(A17,I2.2)'
        WRITE(YCOMMENT,FMT=YFORM) 'snow liquid water',JLAYER
        YCOMMENTUNIT='m'
        CALL WRITE_SURF_FIELD2D(DGU, U, &
                  HPROGRAM,DGMI%XSNOWLIQ(:,JLAYER,:),YRECFM,YCOMMENT,YCOMMENTUNIT)
        !
        YRECFM='SNOWTEMP'//ADJUSTL(YLVL(:LEN_TRIM(YLVL)))
        YFORM='(A16,I1.1)'
        IF (JLAYER >= 10)  YFORM='(A16,I2.2)'
        WRITE(YCOMMENT,FMT=YFORM) 'snow temperature',JLAYER
        YCOMMENTUNIT='K'        
        CALL WRITE_SURF_FIELD2D(DGU, U, &
                  HPROGRAM,DGMI%XSNOWTEMP(:,JLAYER,:),YRECFM,YCOMMENT,YCOMMENTUNIT)
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
    CALL WRITE_SURF_FIELD2D(DGU, U, &
                  HPROGRAM,DGMI%XSEUIL(:,:),YRECFM,YCOMMENT,YCOMMENTUNIT)
    !
  ENDIF
  !
  IF (I%LTR_ML) THEN
    !
    YRECFM='FAPAR'
    YCOMMENT='FAPAR'
    YCOMMENTUNIT='-'    
    CALL WRITE_SURF_FIELD2D(DGU, U, &
                  HPROGRAM,DGMI%XFAPAR(:,:),YRECFM,YCOMMENT,YCOMMENTUNIT)
    !
    YRECFM='FAPIR'
    YCOMMENT='FAPIR'
    YCOMMENTUNIT='-'    
    CALL WRITE_SURF_FIELD2D(DGU, U, &
                  HPROGRAM,DGMI%XFAPIR(:,:),YRECFM,YCOMMENT,YCOMMENTUNIT)
    !
    YRECFM='FAPAR_BS'
    YCOMMENT='FAPAR_BS'
    YCOMMENTUNIT='-'    
    CALL WRITE_SURF_FIELD2D(DGU, U, &
                  HPROGRAM,DGMI%XFAPAR_BS(:,:),YRECFM,YCOMMENT,YCOMMENTUNIT)
    !
    YRECFM='FAPIR_BS'
    YCOMMENT='FAPIR_BS'
    YCOMMENTUNIT='-'    
    CALL WRITE_SURF_FIELD2D(DGU, U, &
                  HPROGRAM,DGMI%XFAPIR_BS(:,:),YRECFM,YCOMMENT,YCOMMENTUNIT)
    !
    YRECFM='DFAPARC'
    YCOMMENT='DFAPARC '
    YCOMMENTUNIT='-'    
    CALL WRITE_SURF_FIELD2D(DGU, U, &
                  HPROGRAM,DGMI%XDFAPARC(:,:),YRECFM,YCOMMENT,YCOMMENTUNIT)
    !
    YRECFM='DFAPIRC'
    YCOMMENT='DFAPIRC'
    YCOMMENTUNIT='-'    
    CALL WRITE_SURF_FIELD2D(DGU, U, &
                  HPROGRAM,DGMI%XDFAPIRC(:,:),YRECFM,YCOMMENT,YCOMMENTUNIT)
    !
    YRECFM='DLAI_EFFC'
    YCOMMENT='DLAI_EFFC'
    YCOMMENTUNIT='m2/m2'    
    CALL WRITE_SURF_FIELD2D(DGU, U, &
                  HPROGRAM,DGMI%XDLAI_EFFC(:,:),YRECFM,YCOMMENT,YCOMMENTUNIT)
    !
  ENDIF
  ! 
  IF (LASSIM .AND. CASSIM_ISBA=="EKF  ") THEN
    !
    DO JVAR = 1,NVAR
      WRITE(YVAR,FMT='(I1.1)') JVAR
      YRECFM="ANAL_INCR"//YVAR
      YCOMMENT="by patch"
      CALL WRITE_SURF(DGU, U, &
                      HPROGRAM,YRECFM,I%XINCR(:,I%NPATCH*(JVAR-1)+1:I%NPATCH*JVAR),IRESP,HCOMMENT=YCOMMENT)
    ENDDO
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
