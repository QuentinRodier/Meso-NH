!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE WRITE_DIAG_SEB_ISBA_n ( DTCO, DGU, U, CHI, DGEI, DGI, DST, GB, I, &
                                        HPROGRAM)
!     #################################
!
!!****  *WRITE_DIAG_SEB_ISBA* - writes the ISBA diagnostic fields
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
!!      V. Masson   *Meteo France*
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    01/2004
!!      B. Decharme 06/2009  key to write (or not) patch result
!!      B. Decharme 08/2009  cumulative radiative budget
!!      B. Decharme  09/2012 : Bug in local variables declaration in PROVAR_TO_DIAG
!!      B. Decharme 09/2012  New diag :
!!                           carbon fluxes and reservoirs
!!                           soil liquid and ice water content in kg/m2 and m3/m3
!!      B. Decharme  06/13   Add diags (sublimation, lateral drainage)
!!                           All snow outputs noted SN
!!                           delete NWG_SIZE
!!      S. Belamari 06/2014 : Introduce GRESET to avoid errors due to NBLOCK=0
!!                            when coupled with ARPEGE/ALADIN/AROME
!!      P. Samuelsson 10/2014 MEB
!!      B. Decharme 02/2016 : NBLOCK instead of LCOUNTW for compilation in AAA
!!      M.Moge    01/2016  using WRITE_SURF_FIELD2D/3D for 2D/3D surfex fields writes
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
!
USE MODD_DATA_COVER_n, ONLY : DATA_COVER_t
USE MODD_DIAG_SURF_ATM_n, ONLY : DIAG_SURF_ATM_t
USE MODD_SURF_ATM_n, ONLY : SURF_ATM_t
USE MODD_CH_ISBA_n, ONLY : CH_ISBA_t
USE MODD_DIAG_EVAP_ISBA_n, ONLY : DIAG_EVAP_ISBA_t
USE MODD_DIAG_ISBA_n, ONLY : DIAG_ISBA_t
USE MODD_DST_n, ONLY : DST_t
USE MODD_GR_BIOG_n, ONLY : GR_BIOG_t
USE MODD_ISBA_n, ONLY : ISBA_t
!
!
#ifdef SFX_ARO
USE MODD_IO_SURF_ARO,   ONLY : NBLOCK
#endif
!
USE MODD_SURF_PAR,   ONLY : XUNDEF, NUNDEF
!
USE MODD_CSTS,       ONLY : XRHOLW, XTT, XLMTT
!
!
!
USE MODD_AGRI  ,     ONLY : LAGRIP
!
!
USE MODI_INIT_IO_SURF_n
USE MODI_WRITE_SURF
USE MODI_WRITE_SURF_FIELD2D
USE MODI_END_IO_SURF_n
!
USE MODD_DST_SURF
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
TYPE(CH_ISBA_t), INTENT(INOUT) :: CHI
TYPE(DIAG_EVAP_ISBA_t), INTENT(INOUT) :: DGEI
TYPE(DIAG_ISBA_t), INTENT(INOUT) :: DGI
TYPE(DST_t), INTENT(INOUT) :: DST
TYPE(GR_BIOG_t), INTENT(INOUT) :: GB
TYPE(ISBA_t), INTENT(INOUT) :: I
!
 CHARACTER(LEN=6),  INTENT(IN)  :: HPROGRAM ! program calling
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
INTEGER           :: IRESP          ! IRESP  : return-code if a problem appears
 CHARACTER(LEN=12) :: YRECFM         ! Name of the article to be write
 CHARACTER(LEN=100):: YCOMMENT       ! Comment string
 CHARACTER(LEN=100):: YCOMMENTUNIT   ! Comment string : unit of the datas in the field to write
 CHARACTER(LEN=2)  :: YNUM
!
LOGICAL           :: GRESET
INTEGER           :: JSV, JSW
INTEGER           :: ISIZE_LMEB_PATCH   ! Number of patches where multi-energy balance should be applied
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
!
!         Initialisation for IO
!
IF (LHOOK) CALL DR_HOOK('WRITE_DIAG_SEB_ISBA_N',0,ZHOOK_HANDLE)
!
ISIZE_LMEB_PATCH=COUNT(I%LMEB_PATCH(:))
!
GRESET=.TRUE.
#ifdef SFX_ARO
GRESET=(NBLOCK>0)
#endif
!
 CALL INIT_IO_SURF_n(DTCO, DGU, U, &
                    HPROGRAM,'NATURE','ISBA  ','WRITE')
!
!-------------------------------------------------------------------------------
!
!*       2.     Richardson number :
!               -----------------
!
IF (DGI%N2M>=1) THEN
  !
  YRECFM='RI_ISBA'
  YCOMMENT='Richardson number over tile nature'
  CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,DGI%XAVG_RI(:),IRESP,HCOMMENT=YCOMMENT)
  !
END IF
!
!*       3.     Energy fluxes :
!               -------------
!
IF (DGI%LSURF_BUDGET) THEN
  !
  YRECFM='RN_ISBA'
  YCOMMENT='Net radiation over tile nature'//' (W/m2)'
  CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,DGI%XAVG_RN(:),IRESP,HCOMMENT=YCOMMENT)
  !
  YRECFM='H_ISBA'
  YCOMMENT='Sensible heat flux over tile nature'//' (W/m2)'
  CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,DGI%XAVG_H(:),IRESP,HCOMMENT=YCOMMENT)
  !
  YRECFM='LE_ISBA'
  YCOMMENT='total latent heat flux over tile nature'//' (W/m2)'
  CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,DGI%XAVG_LE(:),IRESP,HCOMMENT=YCOMMENT)
  !
  YRECFM='LEI_ISBA'
  YCOMMENT='sublimation latent heat flux over tile nature'//' (W/m2)'
  CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,DGI%XAVG_LEI(:),IRESP,HCOMMENT=YCOMMENT)
  !
  YRECFM='GFLUX_ISBA'
  YCOMMENT='Ground flux over tile nature'//' (W/m2)'
  CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,DGI%XAVG_GFLUX(:),IRESP,HCOMMENT=YCOMMENT)
  !
  IF (DGI%LRAD_BUDGET  .OR. (DGEI%LSURF_BUDGETC .AND. .NOT.DGU%LRESET_BUDGETC)) THEN
    !
    YRECFM='SWD_ISBA'
    YCOMMENT='short wave downward radiation over tile nature'//' (W/m2)'
    CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,DGI%XAVG_SWD(:),IRESP,HCOMMENT=YCOMMENT)
    !
    YRECFM='SWU_ISBA'
    YCOMMENT='short wave upward radiation over tile nature'//' (W/m2)'
    CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,DGI%XAVG_SWU(:),IRESP,HCOMMENT=YCOMMENT)
    !
    YRECFM='LWD_ISBA'
    YCOMMENT='long wave downward radiation over tile nature'//' (W/m2)'
    CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,DGI%XAVG_LWD(:),IRESP,HCOMMENT=YCOMMENT)
    !
    YRECFM='LWU_ISBA'
    YCOMMENT='long wave upward radiation over tile nature'//' (W/m2)'
    CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,DGI%XAVG_LWU(:),IRESP,HCOMMENT=YCOMMENT)
    !
    DO JSW=1, SIZE(DGI%XSWBD,2)
      YNUM=ACHAR(48+JSW)
      !
      YRECFM='SWD_ISBA_'//YNUM
      YCOMMENT='short wave downward radiation over tile nature for spectral band'//YNUM//' (W/m2)'
      CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,DGI%XAVG_SWBD(:,JSW),IRESP,HCOMMENT=YCOMMENT)
      !
      YRECFM='SWU_ISBA_'//YNUM
      YCOMMENT='short wave upward radiation over tile nature for spectral band'//YNUM//' (W/m2)'
      CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,DGI%XAVG_SWBU(:,JSW),IRESP,HCOMMENT=YCOMMENT)
      !
    ENDDO
    !
  ENDIF
  !
  YRECFM='FMU_ISBA'
  YCOMMENT='u component of wind stress'//' (Pa)'  
  CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,DGI%XAVG_FMU(:),IRESP,HCOMMENT=YCOMMENT)
  !
  YRECFM='FMV_ISBA'
  YCOMMENT='v component of wind stress'//' (Pa)'  
  CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,DGI%XAVG_FMV(:),IRESP,HCOMMENT=YCOMMENT)
  !
END IF
!
!*       4.    Specific Energy fluxes :(for each patch)
!              ----------------------------------------
!
IF (DGEI%LSURF_EVAP_BUDGET) THEN
  !
  YRECFM='LEG_ISBA'
  YCOMMENT='bare ground evaporation for tile nature'//' (W/m2)'
  CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,DGEI%XAVG_LEG(:),IRESP,HCOMMENT=YCOMMENT)
  !
  YRECFM='LEGI_ISBA'
  YCOMMENT='bare ground sublimation for tile nature'//' (W/m2)'
  CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,DGEI%XAVG_LEGI(:),IRESP,HCOMMENT=YCOMMENT)
  !
  YRECFM='LEV_ISBA'
  YCOMMENT='total vegetation evaporation for tile nature'//' (W/m2)'
  CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,DGEI%XAVG_LEV(:),IRESP,HCOMMENT=YCOMMENT)
  !
  YRECFM='LES_ISBA'
  YCOMMENT='snow sublimation for tile nature'//' (W/m2)'  
  CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,DGEI%XAVG_LES(:),IRESP,HCOMMENT=YCOMMENT)
  !
  IF(I%TSNOW%SCHEME=='3-L' .OR. I%TSNOW%SCHEME=='CRO')THEN  
    YRECFM='LESL_ISBA'
    YCOMMENT='liquid water evaporation over snow for tile nature'//' (W/m2)'
    CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,DGEI%XAVG_LESL(:),IRESP,HCOMMENT=YCOMMENT)
    YRECFM='SNDRIF_ISBA'
    YCOMMENT='blowing snow sublimation for tile nature'//' (Kg/m2/s)'
    CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,DGEI%XAVG_SNDRIFT(:),IRESP,HCOMMENT=YCOMMENT)    
  ENDIF
  !  
  YRECFM='LER_ISBA'
  YCOMMENT='canopy direct evaporation for tile nature'//' (W/m2)'
  CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,DGEI%XAVG_LER(:),IRESP,HCOMMENT=YCOMMENT)
  !
  YRECFM='LETR_ISBA'
  YCOMMENT='vegetation transpiration for tile nature'//' (W/m2)'
  CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,DGEI%XAVG_LETR(:),IRESP,HCOMMENT=YCOMMENT)
  !
  YRECFM='EVAP_ISBA'
  YCOMMENT='total evaporative flux for tile nature'//' (Kg/m2/s)'
  CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,DGEI%XAVG_EVAP(:),IRESP,HCOMMENT=YCOMMENT)
  !
  YRECFM='SUBL_ISBA'
  YCOMMENT='sublimation flux for tile nature'//' (Kg/m2/s)'
  CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,DGEI%XAVG_SUBL(:),IRESP,HCOMMENT=YCOMMENT)
  !
  YRECFM='DRAIN_ISBA'
  YCOMMENT='drainage for tile nature'//' (Kg/m2/s)'
  CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,DGEI%XAVG_DRAIN(:),IRESP,HCOMMENT=YCOMMENT)
  !
  IF(I%CRUNOFF=='SGH'.AND.I%CISBA=='DIF')THEN
    YRECFM='QSB_ISBA'
    YCOMMENT='lateral subsurface flow for tile nature'//' (Kg/m2/s)'
    CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,DGEI%XAVG_QSB(:),IRESP,HCOMMENT=YCOMMENT)
  ENDIF
  !
  YRECFM='RUNOFF_ISBA'
  YCOMMENT='runoff for tile nature'//' (Kg/m2/s)'
  CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,DGEI%XAVG_RUNOFF(:),IRESP,HCOMMENT=YCOMMENT)
  !
  IF(I%CHORT=='SGH'.OR.I%CISBA=='DIF')THEN
    YRECFM='HORTON_ISBA'
    YCOMMENT='horton runoff for tile nature'//' (Kg/m2/s)'
    CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,DGEI%XAVG_HORT(:),IRESP,HCOMMENT=YCOMMENT)
  ENDIF
  !
  YRECFM='DRIVEG_ISBA'
  YCOMMENT='X_Y_'//YRECFM//' (Kg/m2/s)'
  CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,DGEI%XAVG_DRIP(:),IRESP,HCOMMENT=YCOMMENT)
  !
  YRECFM='RRVEG_ISBA'
  YCOMMENT='X_Y_'//YRECFM//' (Kg/m2/s)'
  CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,DGEI%XAVG_RRVEG(:),IRESP,HCOMMENT=YCOMMENT)
  !
  YRECFM='SNOMLT_ISBA'
  YCOMMENT='snow melting rate'//' (Kg/m2/s)'
  CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,DGEI%XAVG_MELT(:),IRESP,HCOMMENT=YCOMMENT)
  !
  IF(LAGRIP)THEN
    YRECFM='IRRIG_ISBA'
    YCOMMENT='irrigation rate'//' (Kg/m2/s)'
    CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,DGEI%XAVG_IRRIG_FLUX(:),IRESP,HCOMMENT=YCOMMENT)
  ENDIF  
! MEB STUFF
  IF (ISIZE_LMEB_PATCH>0) THEN
    YRECFM='LEVCV_ISBA'
    YCOMMENT='X_Y_'//YRECFM//' (W/m2)'
    CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,DGEI%XAVG_LEVCV(:),IRESP,HCOMMENT=YCOMMENT)
    !
    YRECFM='LESC_ISBA'
    YCOMMENT='X_Y_'//YRECFM//' (W/m2)'
    CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,DGEI%XAVG_LESC(:),IRESP,HCOMMENT=YCOMMENT)
    !
    YRECFM='LETRGV_ISBA'
    YCOMMENT='X_Y_'//YRECFM//' (W/m2)'
    CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,DGEI%XAVG_LETRGV(:),IRESP,HCOMMENT=YCOMMENT)
    !
    YRECFM='LETRCV_ISBA'
    YCOMMENT='X_Y_'//YRECFM//' (W/m2)'
    CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,DGEI%XAVG_LETRCV(:),IRESP,HCOMMENT=YCOMMENT)
    !
    YRECFM='LERGV_ISBA'
    YCOMMENT='X_Y_'//YRECFM//' (W/m2)'
    CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,DGEI%XAVG_LERGV(:),IRESP,HCOMMENT=YCOMMENT)
    !
    YRECFM='LELIT_ISBA'
    YCOMMENT='X_Y_'//YRECFM//' (W/m2)'
    CALL WRITE_SURF(DGU, U, &
                 HPROGRAM,YRECFM,DGEI%XAVG_LELITTER(:),IRESP,HCOMMENT=YCOMMENT)
    !
    YRECFM='LELITI_ISBA'
    YCOMMENT='X_Y_'//YRECFM//' (W/m2)'
    CALL WRITE_SURF(DGU, U, &
                   HPROGRAM,YRECFM,DGEI%XAVG_LELITTERI(:),IRESP,HCOMMENT=YCOMMENT)
    !
    YRECFM='DRIPLIT_ISBA'
    YCOMMENT='X_Y_'//YRECFM//' (W/m2)'
    CALL WRITE_SURF(DGU, U, &
                HPROGRAM,YRECFM,DGEI%XAVG_DRIPLIT(:),IRESP,HCOMMENT=YCOMMENT)
    !
    YRECFM='RRLIT_ISBA'
    YCOMMENT='X_Y_'//YRECFM//' (W/m2)'
    CALL WRITE_SURF(DGU, U, &
                 HPROGRAM,YRECFM,DGEI%XAVG_RRLIT(:),IRESP,HCOMMENT=YCOMMENT)
    !
    YRECFM='LERCV_ISBA'
    YCOMMENT='X_Y_'//YRECFM//' (W/m2)'
    CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,DGEI%XAVG_LERCV(:),IRESP,HCOMMENT=YCOMMENT)
    !
    YRECFM='LE_C_A_ISBA'
    YCOMMENT='X_Y_'//YRECFM//' (W/m2)'
    CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,DGEI%XAVG_LE_C_A(:),IRESP,HCOMMENT=YCOMMENT)
    !
    YRECFM='LE_V_C_ISBA'
    YCOMMENT='X_Y_'//YRECFM//' (W/m2)'
    CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,DGEI%XAVG_LE_V_C(:),IRESP,HCOMMENT=YCOMMENT)
    !
    YRECFM='LE_G_C_ISBA'
    YCOMMENT='X_Y_'//YRECFM//' (W/m2)'
    CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,DGEI%XAVG_LE_G_C(:),IRESP,HCOMMENT=YCOMMENT)
    !
    YRECFM='LE_N_C_ISBA'
    YCOMMENT='X_Y_'//YRECFM//' (W/m2)'
    CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,DGEI%XAVG_LE_N_C(:),IRESP,HCOMMENT=YCOMMENT)
    !
    YRECFM='SWNT_V_ISBA'
    YCOMMENT='X_Y_'//YRECFM//' (W/m2)'
    CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,DGEI%XAVG_SWNET_V(:),IRESP,HCOMMENT=YCOMMENT)
    !
    YRECFM='SWNT_G_ISBA'
    YCOMMENT='X_Y_'//YRECFM//' (W/m2)'
    CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,DGEI%XAVG_SWNET_G(:),IRESP,HCOMMENT=YCOMMENT)
    !
    YRECFM='SWNT_N_ISBA'
    YCOMMENT='X_Y_'//YRECFM//' (W/m2)'
    CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,DGEI%XAVG_SWNET_N(:),IRESP,HCOMMENT=YCOMMENT)
    !
    YRECFM='SWNT_NS_ISBA'
    YCOMMENT='X_Y_'//YRECFM//' (W/m2)'
    CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,DGEI%XAVG_SWNET_NS(:),IRESP,HCOMMENT=YCOMMENT)
    !
    YRECFM='LWNT_V_ISBA'
    YCOMMENT='X_Y_'//YRECFM//' (W/m2)'
    CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,DGEI%XAVG_LWNET_V(:),IRESP,HCOMMENT=YCOMMENT)
    !
    YRECFM='LWNT_G_ISBA'
    YCOMMENT='X_Y_'//YRECFM//' (W/m2)'
    CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,DGEI%XAVG_LWNET_G(:),IRESP,HCOMMENT=YCOMMENT)
    !
    YRECFM='LWNT_N_ISBA'
    YCOMMENT='X_Y_'//YRECFM//' (W/m2)'
    CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,DGEI%XAVG_LWNET_N(:),IRESP,HCOMMENT=YCOMMENT)
    !
    YRECFM='SWDN_GN_ISBA'
    YCOMMENT='X_Y_'//YRECFM//' (W/m2)'
    CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,DGEI%XAVG_SWDOWN_GN(:),IRESP,HCOMMENT=YCOMMENT)
    !
    YRECFM='LWDN_GN_ISBA'
    YCOMMENT='X_Y_'//YRECFM//' (W/m2)'
    CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,DGEI%XAVG_LWDOWN_GN(:),IRESP,HCOMMENT=YCOMMENT)
    !
    YRECFM='H_V_C_ISBA'
    YCOMMENT='X_Y_'//YRECFM//' (W/m2)'
    CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,DGEI%XAVG_H_V_C(:),IRESP,HCOMMENT=YCOMMENT)
    !
    YRECFM='H_G_C_ISBA'
    YCOMMENT='X_Y_'//YRECFM//' (W/m2)'
    CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,DGEI%XAVG_H_G_C(:),IRESP,HCOMMENT=YCOMMENT)
    !
    YRECFM='H_C_A_ISBA'
    YCOMMENT='X_Y_'//YRECFM//' (W/m2)'
    CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,DGEI%XAVG_H_C_A(:),IRESP,HCOMMENT=YCOMMENT)
    !
    YRECFM='H_N_C_ISBA'
    YCOMMENT='X_Y_'//YRECFM//' (W/m2)'
    CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,DGEI%XAVG_H_N_C(:),IRESP,HCOMMENT=YCOMMENT)
    !
    YRECFM='SR_GN_ISBA'
    YCOMMENT='X_Y_'//YRECFM//' (kg/m2/s)'
    CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,DGEI%XAVG_SR_GN(:),IRESP,HCOMMENT=YCOMMENT)
    !
    YRECFM='MELTCV_ISBA'
    YCOMMENT='X_Y_'//YRECFM//' (kg/m2/s)'
    CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,DGEI%XAVG_MELTCV(:),IRESP,HCOMMENT=YCOMMENT)
    !
    YRECFM='FRZCV_ISBA'
    YCOMMENT='X_Y_'//YRECFM//' (kg/m2/s)'
    CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,DGEI%XAVG_FRZCV(:),IRESP,HCOMMENT=YCOMMENT)
  ENDIF
  ! END MEB STUFF
  !
  IF(I%LFLOOD)THEN
    !        
    YRECFM='IFLOOD_ISBA'
    YCOMMENT='flood soil infiltration (Kg/m2/s)'    
    CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,DGEI%XAVG_IFLOOD(:),IRESP,HCOMMENT=YCOMMENT)
    !
    YRECFM='PFLOOD_ISBA'
    YCOMMENT='intercepted precipitation by floodplains (Kg/m2/s)'    
    CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,DGEI%XAVG_PFLOOD(:),IRESP,HCOMMENT=YCOMMENT)
    !
    YRECFM='LEF_ISBA'
    YCOMMENT='total floodplains evaporation (W/m2)'   
    CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,DGEI%XAVG_LE_FLOOD(:),IRESP,HCOMMENT=YCOMMENT)
    !
    YRECFM='LEIF_ISBA'
    YCOMMENT='solid floodplains evaporation (W/m2)'    
    CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,DGEI%XAVG_LEI_FLOOD(:),IRESP,HCOMMENT=YCOMMENT)
    !
  ENDIF
  !
  IF(I%CPHOTO/='NON')THEN
    !
    YRECFM='GPP_ISBA'
    YCOMMENT='gross primary production over tile nature (kgCO2/m2/s)'
    CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,DGEI%XAVG_GPP(:),IRESP,HCOMMENT=YCOMMENT)
    !
    YRECFM='R_AUTO_ISBA'
    YCOMMENT='autotrophic respiration over tile nature (kgCO2/m2/s)'
    CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,DGEI%XAVG_RESP_AUTO(:),IRESP,HCOMMENT=YCOMMENT)
    !
    YRECFM='R_ECO_ISBA'
    YCOMMENT='ecosystem respiration over tile nature (kgCO2/m2/s)'
    CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,DGEI%XAVG_RESP_ECO(:),IRESP,HCOMMENT=YCOMMENT)
    !
  ENDIF
  !  
  IF(DGEI%LWATER_BUDGET)THEN 
    !
    YRECFM='RAINF_ISBA'
    YCOMMENT='input rainfall rate (Kg/m2/s)'
    CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,DGEI%XRAINFALL(:),IRESP,HCOMMENT=YCOMMENT)
    !
    YRECFM='SNOWF_ISBA'
    YCOMMENT='input snowfall rate (Kg/m2/s)'
    CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,DGEI%XSNOWFALL(:),IRESP,HCOMMENT=YCOMMENT)
    !
    YRECFM='DWG_ISBA'
    YCOMMENT='change in liquid soil moisture (Kg/m2/s)'
    CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,DGEI%XAVG_DWG(:),IRESP,HCOMMENT=YCOMMENT)
    !
    YRECFM='DWGI_ISBA'
    YCOMMENT='change in solid soil moisture (Kg/m2/s)'
    CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,DGEI%XAVG_DWGI(:),IRESP,HCOMMENT=YCOMMENT)
    !
    YRECFM='DWR_ISBA'
    YCOMMENT='change in water on canopy (Kg/m2/s)'
    CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,DGEI%XAVG_DWR(:),IRESP,HCOMMENT=YCOMMENT)
    !
    YRECFM='DSWE_ISBA'
    YCOMMENT='change in snow water equivalent (Kg/m2/s)'
    CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,DGEI%XAVG_DSWE(:),IRESP,HCOMMENT=YCOMMENT)
    !
    YRECFM='WATBUD_ISBA'
    YCOMMENT='isba water budget as residue (Kg/m2/s)'
    CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,DGEI%XAVG_WATBUD(:),IRESP,HCOMMENT=YCOMMENT)
    !
  ENDIF
  !  
ENDIF
!
!*       5.    Cumulated Energy fluxes
!              -----------------------
!
IF (DGEI%LSURF_BUDGETC) THEN
  !
  YRECFM='LEGC_ISBA'
  YCOMMENT='X_Y_'//YRECFM//' (J/m2)'
  CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,DGEI%XAVG_LEGC(:),IRESP,HCOMMENT=YCOMMENT)
  !
  YRECFM='LEGIC_ISBA'
  YCOMMENT='X_Y_'//YRECFM//' (J/m2)'
  CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,DGEI%XAVG_LEGIC(:),IRESP,HCOMMENT=YCOMMENT)
  !
  YRECFM='LEVC_ISBA'
  YCOMMENT='X_Y_'//YRECFM//' (J/m2)'
  CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,DGEI%XAVG_LEVC(:),IRESP,HCOMMENT=YCOMMENT)
  !
  YRECFM='LESC_ISBA'
  YCOMMENT='X_Y_'//YRECFM//' (J/m2)'
  CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,DGEI%XAVG_LESAC(:),IRESP,HCOMMENT=YCOMMENT)
  !
  IF(I%TSNOW%SCHEME=='3-L' .OR. I%TSNOW%SCHEME=='CRO')THEN  
    YRECFM='LESLC_ISBA'
    YCOMMENT='X_Y_'//YRECFM//' (J/m2)'
    CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,DGEI%XAVG_LESLC(:),IRESP,HCOMMENT=YCOMMENT)
    YRECFM='SNDRIFC_ISBA'
    YCOMMENT='X_Y_'//YRECFM//' (kg/m2)'
    CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,DGEI%XAVG_SNDRIFTC(:),IRESP,HCOMMENT=YCOMMENT)
  ENDIF
  !  
  YRECFM='LERC_ISBA'
  YCOMMENT='X_Y_'//YRECFM//' (J/m2)'
  CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,DGEI%XAVG_LERC(:),IRESP,HCOMMENT=YCOMMENT)
  !
  YRECFM='LETRC_ISBA'
  YCOMMENT='X_Y_'//YRECFM//' (J/m2)'
  CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,DGEI%XAVG_LETRC(:),IRESP,HCOMMENT=YCOMMENT)
  !
  YRECFM='EVAPC_ISBA'
  YCOMMENT='X_Y_'//YRECFM//' (Kg/m2)'
  CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,DGEI%XAVG_EVAPC(:),IRESP,HCOMMENT=YCOMMENT)
  !
  YRECFM='SUBLC_ISBA'
  YCOMMENT='X_Y_'//YRECFM//' (Kg/m2)'
  CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,DGEI%XAVG_SUBLC(:),IRESP,HCOMMENT=YCOMMENT)
  !
  YRECFM='DRAINC_ISBA'
  YCOMMENT='X_Y_'//YRECFM//' (Kg/m2)'
  CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,DGEI%XAVG_DRAINC(:),IRESP,HCOMMENT=YCOMMENT)
  !
  IF(I%CRUNOFF=='SGH'.AND.I%CISBA=='DIF')THEN
    YRECFM='QSBC_ISBA'
    YCOMMENT='X_Y_'//YRECFM//' (Kg/m2)'
    CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,DGEI%XAVG_QSBC(:),IRESP,HCOMMENT=YCOMMENT)
  ENDIF
  !
  YRECFM='RUNOFFC_ISBA'
  YCOMMENT='X_Y_'//YRECFM//' (Kg/m2)'
  CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,DGEI%XAVG_RUNOFFC(:),IRESP,HCOMMENT=YCOMMENT)
  !
  IF(I%CHORT=='SGH'.OR.I%CISBA=='DIF')THEN
    YRECFM='HORTONC_ISBA'
    YCOMMENT='X_Y_'//YRECFM//' (Kg/m2)'
    CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,DGEI%XAVG_HORTC(:),IRESP,HCOMMENT=YCOMMENT)
  ENDIF
  !
  YRECFM='DRIVEGC_ISBA'
  YCOMMENT='X_Y_'//YRECFM//' (Kg/m2)'
  CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,DGEI%XAVG_DRIPC(:),IRESP,HCOMMENT=YCOMMENT)
  !
  YRECFM='RRVEGC_ISBA'
  YCOMMENT='X_Y_'//YRECFM//' (Kg/m2)'
  CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,DGEI%XAVG_RRVEGC(:),IRESP,HCOMMENT=YCOMMENT)
  !
  YRECFM='SNOMLTC_ISBA'
  YCOMMENT='X_Y_'//YRECFM//' (Kg/m2)'
  CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,DGEI%XAVG_MELTC(:),IRESP,HCOMMENT=YCOMMENT)
  !
  ! MEB STUFF
  IF (ISIZE_LMEB_PATCH>0) THEN
    YRECFM='LEVCVC_ISBA'
    YCOMMENT='X_Y_'//YRECFM//' (J/m2)'
    CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,DGEI%XAVG_LEVCVC(:),IRESP,HCOMMENT=YCOMMENT)
    !
    YRECFM='LESCC_ISBA'
    YCOMMENT='X_Y_'//YRECFM//' (J/m2)'
    CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,DGEI%XAVG_LESCC(:),IRESP,HCOMMENT=YCOMMENT)
    !
    YRECFM='LETRGVC_ISBA'
    YCOMMENT='X_Y_'//YRECFM//' (J/m2)'
    CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,DGEI%XAVG_LETRGVC(:),IRESP,HCOMMENT=YCOMMENT)
    !
    YRECFM='LETRCVC_ISBA'
    YCOMMENT='X_Y_'//YRECFM//' (J/m2)'
    CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,DGEI%XAVG_LETRCVC(:),IRESP,HCOMMENT=YCOMMENT)
    !
    YRECFM='LERGVC_ISBA'
    YCOMMENT='X_Y_'//YRECFM//' (J/m2)'
    CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,DGEI%XAVG_LERGVC(:),IRESP,HCOMMENT=YCOMMENT)
    !
    YRECFM='LERCVC_ISBA'
    YCOMMENT='X_Y_'//YRECFM//' (J/m2)'
    CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,DGEI%XAVG_LERCVC(:),IRESP,HCOMMENT=YCOMMENT)
    !
    YRECFM='LE_C_AC_ISBA'
    YCOMMENT='X_Y_'//YRECFM//' (J/m2)'
    CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,DGEI%XAVG_LE_C_AC(:),IRESP,HCOMMENT=YCOMMENT)
    !
    YRECFM='LE_V_CC_ISBA'
    YCOMMENT='X_Y_'//YRECFM//' (J/m2)'
    CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,DGEI%XAVG_LE_V_CC(:),IRESP,HCOMMENT=YCOMMENT)
    !
    YRECFM='LE_G_CC_ISBA'
    YCOMMENT='X_Y_'//YRECFM//' (J/m2)'
    CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,DGEI%XAVG_LE_G_CC(:),IRESP,HCOMMENT=YCOMMENT)
    !
    YRECFM='LE_N_CC_ISBA'
    YCOMMENT='X_Y_'//YRECFM//' (J/m2)'
    CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,DGEI%XAVG_LE_N_CC(:),IRESP,HCOMMENT=YCOMMENT)
    !
    YRECFM='SWNT_VC_ISBA'
    YCOMMENT='X_Y_'//YRECFM//' (J/m2)'
    CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,DGEI%XAVG_SWNET_VC(:),IRESP,HCOMMENT=YCOMMENT)
    !
    YRECFM='SWNT_GC_ISBA'
    YCOMMENT='X_Y_'//YRECFM//' (J/m2)'
    CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,DGEI%XAVG_SWNET_GC(:),IRESP,HCOMMENT=YCOMMENT)
    !
    YRECFM='SWNT_NC_ISBA'
    YCOMMENT='X_Y_'//YRECFM//' (J/m2)'
    CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,DGEI%XAVG_SWNET_NC(:),IRESP,HCOMMENT=YCOMMENT)
    !
    YRECFM='SWNT_NSC_ISBA'
    YCOMMENT='X_Y_'//YRECFM//' (J/m2)'
    CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,DGEI%XAVG_SWNET_NSC(:),IRESP,HCOMMENT=YCOMMENT)
    !
    YRECFM='LWNT_VC_ISBA'
    YCOMMENT='X_Y_'//YRECFM//' (J/m2)'
    CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,DGEI%XAVG_LWNET_VC(:),IRESP,HCOMMENT=YCOMMENT)
    !
    YRECFM='LWNT_GC_ISBA'
    YCOMMENT='X_Y_'//YRECFM//' (J/m2)'
    CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,DGEI%XAVG_LWNET_GC(:),IRESP,HCOMMENT=YCOMMENT)
    !
    YRECFM='LWNT_NC_ISBA'
    YCOMMENT='X_Y_'//YRECFM//' (J/m2)'
    CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,DGEI%XAVG_LWNET_NC(:),IRESP,HCOMMENT=YCOMMENT)
    !
    YRECFM='SWDN_GNC_ISBA'
    YCOMMENT='X_Y_'//YRECFM//' (J/m2)'
    CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,DGEI%XAVG_SWDOWN_GNC(:),IRESP,HCOMMENT=YCOMMENT)
    !
    YRECFM='LWDN_GNC_ISBA'
    YCOMMENT='X_Y_'//YRECFM//' (J/m2)'
    CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,DGEI%XAVG_LWDOWN_GNC(:),IRESP,HCOMMENT=YCOMMENT)
    !
    YRECFM='H_V_CC_ISBA'
    YCOMMENT='X_Y_'//YRECFM//' (J/m2)'
    CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,DGEI%XAVG_H_V_CC(:),IRESP,HCOMMENT=YCOMMENT)
    !
    YRECFM='H_G_CC_ISBA'
    YCOMMENT='X_Y_'//YRECFM//' (J/m2)'
    CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,DGEI%XAVG_H_G_CC(:),IRESP,HCOMMENT=YCOMMENT)
    !
    YRECFM='H_C_AC_ISBA'
    YCOMMENT='X_Y_'//YRECFM//' (J/m2)'
    CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,DGEI%XAVG_H_C_AC(:),IRESP,HCOMMENT=YCOMMENT)
    !
    YRECFM='H_N_CC_ISBA'
    YCOMMENT='X_Y_'//YRECFM//' (J/m2)'
    CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,DGEI%XAVG_H_N_CC(:),IRESP,HCOMMENT=YCOMMENT)
    !
    YRECFM='SR_GNC_ISBA'
    YCOMMENT='X_Y_'//YRECFM//' (kg/m2)'
    CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,DGEI%XAVG_SR_GNC(:),IRESP,HCOMMENT=YCOMMENT)
    !
    YRECFM='MELTCVC_ISBA'
    YCOMMENT='X_Y_'//YRECFM//' (kg/m2)'
    CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,DGEI%XAVG_MELTCVC(:),IRESP,HCOMMENT=YCOMMENT)
    !
    YRECFM='FRZCVC_ISBA'
    YCOMMENT='X_Y_'//YRECFM//' (kg/m2)'
    CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,DGEI%XAVG_FRZCVC(:),IRESP,HCOMMENT=YCOMMENT)
  ENDIF
  ! END MEB STUFF
  !
  IF(LAGRIP)THEN
    YRECFM='IRRIGC_ISBA'
    YCOMMENT='X_Y_'//YRECFM//' (Kg/m2)'
    CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,DGEI%XAVG_IRRIG_FLUXC(:),IRESP,HCOMMENT=YCOMMENT)
  ENDIF
  !
  IF(I%LGLACIER)THEN
    YRECFM='ICE_FC_ISBA'
    YCOMMENT='X_Y_'//YRECFM//' (Kg/m2)'
    CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,DGEI%XAVG_ICEFLUXC(:),IRESP,HCOMMENT=YCOMMENT)
  ENDIF
  !
  IF(I%LFLOOD)THEN
    !
    YRECFM='IFLOODC_ISBA'
    YCOMMENT='X_Y_'//YRECFM//' (Kg/m2)'
    CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,DGEI%XAVG_IFLOODC(:),IRESP,HCOMMENT=YCOMMENT)
    !
    YRECFM='PFLOODC_ISBA'
    YCOMMENT='X_Y_'//YRECFM//' (Kg/m2)'
    CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,DGEI%XAVG_PFLOODC(:),IRESP,HCOMMENT=YCOMMENT)
    !
    YRECFM='LEFC_ISBA'
    YCOMMENT='X_Y_'//YRECFM//' (J/m2)'
    CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,DGEI%XAVG_LE_FLOODC(:),IRESP,HCOMMENT=YCOMMENT)
    !
    YRECFM='LEIFC_ISBA'
    YCOMMENT='X_Y_'//YRECFM//' (J/m2)'
    CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,DGEI%XAVG_LEI_FLOODC(:),IRESP,HCOMMENT=YCOMMENT)
    !
  ENDIF
  !
  YRECFM='RNC_ISBA'
  YCOMMENT='X_Y_'//YRECFM//' (J/m2)'
  CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,DGEI%XAVG_RNC(:),IRESP,HCOMMENT=YCOMMENT)
  !
  YRECFM='HC_ISBA'
  YCOMMENT='X_Y_'//YRECFM//' (J/m2)'
  CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,DGEI%XAVG_HC(:),IRESP,HCOMMENT=YCOMMENT)
  !
  YRECFM='LEC_ISBA'
  YCOMMENT='X_Y_'//YRECFM//' (J/m2)'
  CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,DGEI%XAVG_LEC(:),IRESP,HCOMMENT=YCOMMENT)
  !
  YRECFM='LEIC_ISBA'
  YCOMMENT='X_Y_'//YRECFM//' (J/m2)'
  CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,DGEI%XAVG_LEIC(:),IRESP,HCOMMENT=YCOMMENT)
  !
  YRECFM='GFLUXC_ISBA'
  YCOMMENT='X_Y_'//YRECFM//' (J/m2)'
  CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,DGEI%XAVG_GFLUXC(:),IRESP,HCOMMENT=YCOMMENT)
  !
  IF (DGI%LRAD_BUDGET .OR. (DGEI%LSURF_BUDGETC .AND. .NOT.DGU%LRESET_BUDGETC)) THEN
    !
    YRECFM='SWDC_ISBA'
    YCOMMENT='X_Y_'//YRECFM//' (J/m2)'
    CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,DGI%XAVG_SWDC(:),IRESP,HCOMMENT=YCOMMENT)
    !
    YRECFM='SWUC_ISBA'
    YCOMMENT='X_Y_'//YRECFM//' (J/m2)'
    CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,DGI%XAVG_SWUC(:),IRESP,HCOMMENT=YCOMMENT)
    !
    YRECFM='LWDC_ISBA'
    YCOMMENT='X_Y_'//YRECFM//' (J/m2)'
    CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,DGI%XAVG_LWDC(:),IRESP,HCOMMENT=YCOMMENT)
    !
    YRECFM='LWUC_ISBA'
    YCOMMENT='X_Y_'//YRECFM//' (J/m2)'
    CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,DGI%XAVG_LWUC(:),IRESP,HCOMMENT=YCOMMENT)
    !
  ENDIF
  !
  YRECFM='FMUC_ISBA'
  YCOMMENT='X_Y_'//YRECFM//' (Pa.s)'  
  CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,DGI%XAVG_FMUC(:),IRESP,HCOMMENT=YCOMMENT)
  !
  YRECFM='FMVC_ISBA'
  YCOMMENT='X_Y_'//YRECFM//' (Pa.s)'  
  CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,DGI%XAVG_FMVC(:),IRESP,HCOMMENT=YCOMMENT)
  !
  IF(I%CPHOTO/='NON')THEN
    !
    YRECFM='GPPC_ISBA'
    YCOMMENT='X_Y_'//YRECFM//' (kgCO2/m2)'
    CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,DGEI%XAVG_GPPC(:),IRESP,HCOMMENT=YCOMMENT)
    !
    YRECFM='RC_AUTO_ISBA'
    YCOMMENT='X_Y_'//YRECFM//' (kgCO2/m2)'
    CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,DGEI%XAVG_RESPC_AUTO(:),IRESP,HCOMMENT=YCOMMENT)
    !
    YRECFM='RC_ECO_ISBA'
    YCOMMENT='X_Y_'//YRECFM//' (kgCO2/m2)'
    CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,DGEI%XAVG_RESPC_ECO(:),IRESP,HCOMMENT=YCOMMENT)
    !
  ENDIF
  !  
  IF(DGEI%LWATER_BUDGET .OR. (DGEI%LSURF_BUDGETC .AND. .NOT.DGU%LRESET_BUDGETC))THEN 
    !
    YRECFM='RAINFC_ISBA'
    YCOMMENT='cumulated input rainfall rate (Kg/m2)'
    CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,DGEI%XRAINFALLC(:),IRESP,HCOMMENT=YCOMMENT)
    !
    YRECFM='SNOWFC_ISBA'
    YCOMMENT='cumulated input snowfall rate (Kg/m2)'
    CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,DGEI%XSNOWFALLC(:),IRESP,HCOMMENT=YCOMMENT)
    !
    YRECFM='DWGC_ISBA'
    YCOMMENT='cumulated change in liquid soil moisture (Kg/m2)'
    CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,DGEI%XAVG_DWGC(:),IRESP,HCOMMENT=YCOMMENT)
    !
    YRECFM='DWGIC_ISBA'
    YCOMMENT='cumulated change in solid soil moisture (Kg/m2)'
    CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,DGEI%XAVG_DWGIC(:),IRESP,HCOMMENT=YCOMMENT)
    !
    YRECFM='DWRC_ISBA'
    YCOMMENT='cumulated change in water on canopy (Kg/m2)'
    CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,DGEI%XAVG_DWRC(:),IRESP,HCOMMENT=YCOMMENT)
    !
    YRECFM='DSWEC_ISBA'
    YCOMMENT='cumulated change in snow water equivalent (Kg/m2)'
    CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,DGEI%XAVG_DSWEC(:),IRESP,HCOMMENT=YCOMMENT)
    !
    YRECFM='WATBUDC_ISBA'
    YCOMMENT='cumulated isba water budget as residue (Kg/m2)'
    CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,DGEI%XAVG_WATBUDC(:),IRESP,HCOMMENT=YCOMMENT)
    !
  ENDIF 
  !  
ENDIF
!
!*       6.     parameters at 2 and 10 meters :
!               -------------------------------
!
IF (DGI%N2M>=1) THEN
  !
  YRECFM='T2M_ISBA'
  YCOMMENT='X_Y_'//YRECFM//' (K)'
  CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,DGI%XAVG_T2M(:),IRESP,HCOMMENT=YCOMMENT)
  !
  YRECFM='T2MMIN_ISBA'
  YCOMMENT='X_Y_'//YRECFM//' (K)'
  CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,DGI%XAVG_T2M_MIN(:),IRESP,HCOMMENT=YCOMMENT)
  IF(GRESET)DGI%XAVG_T2M_MIN(:)=XUNDEF
  !
  YRECFM='T2MMAX_ISBA'
  YCOMMENT='X_Y_'//YRECFM//' (K)'
  CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,DGI%XAVG_T2M_MAX(:),IRESP,HCOMMENT=YCOMMENT)
  IF(GRESET)DGI%XAVG_T2M_MAX(:)=-XUNDEF
  !
  YRECFM='Q2M_ISBA'
  YCOMMENT='X_Y_'//YRECFM//' (KG/KG)'
  CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,DGI%XAVG_Q2M(:),IRESP,HCOMMENT=YCOMMENT)
  !
  YRECFM='HU2M_ISBA'
  YCOMMENT='X_Y_'//YRECFM//' (-)'
  CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,DGI%XAVG_HU2M(:),IRESP,HCOMMENT=YCOMMENT)
  !
  YRECFM='HU2MMIN_ISBA'
  YCOMMENT='X_Y_'//YRECFM//' (-)'
  CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,DGI%XAVG_HU2M_MIN(:),IRESP,HCOMMENT=YCOMMENT)
  IF(GRESET)DGI%XAVG_HU2M_MIN(:)=XUNDEF
  !
  YRECFM='HU2MMAX_ISBA'
  YCOMMENT='X_Y_'//YRECFM//' (-)'
  CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,DGI%XAVG_HU2M_MAX(:),IRESP,HCOMMENT=YCOMMENT)
  IF(GRESET)DGI%XAVG_HU2M_MAX(:)=-XUNDEF
  !
  YRECFM='ZON10M_ISBA'
  YCOMMENT='X_Y_'//YRECFM//' (M/S)'
  CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,DGI%XAVG_ZON10M(:),IRESP,HCOMMENT=YCOMMENT)
  !
  YRECFM='MER10M_ISBA'
  YCOMMENT='X_Y_'//YRECFM//' (M/S)'
  CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,DGI%XAVG_MER10M(:),IRESP,HCOMMENT=YCOMMENT)
  !
  YRECFM='W10M_ISBA'
  YCOMMENT='X_Y_'//YRECFM//' (M/S)'
  CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,DGI%XAVG_WIND10M(:),IRESP,HCOMMENT=YCOMMENT)
  !
  YRECFM='W10MMAX_ISBA'
  YCOMMENT='X_Y_'//YRECFM//' (M/S)'
  CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,DGI%XAVG_WIND10M_MAX(:),IRESP,HCOMMENT=YCOMMENT)
  IF(GRESET)DGI%XAVG_WIND10M_MAX(:)=0.0
  !
  YRECFM='SFCO2_ISBA'
  YCOMMENT='X_Y_'//YRECFM//' (M.kgCO2.S-1.kgAIR-1)'
  CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,DGI%XAVG_SFCO2(:),IRESP,HCOMMENT=YCOMMENT)
  !  
END IF
!----------------------------------------------------------------------------
!
!*       7.     Transfer coefficients
!               ---------------------
!
IF (DGI%LCOEF) THEN
  !
  YRECFM='CD_ISBA'
  YCOMMENT='X_Y_'//YRECFM
  CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,DGI%XAVG_CD(:),IRESP,HCOMMENT=YCOMMENT)
  !
  YRECFM='CH_ISBA'
  YCOMMENT='X_Y_'//YRECFM
  CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,DGI%XAVG_CH(:),IRESP,HCOMMENT=YCOMMENT)
  !
  YRECFM='CE_ISBA'
  YCOMMENT='X_Y_'//YRECFM
  CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,DGI%XAVG_CE(:),IRESP,HCOMMENT=YCOMMENT)
  !
  YRECFM='Z0_ISBA'
  YCOMMENT='X_Y_'//YRECFM//' (M)'
  CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,DGI%XAVG_Z0(:),IRESP,HCOMMENT=YCOMMENT)
  !
  YRECFM='Z0H_ISBA'
  YCOMMENT='X_Y_'//YRECFM//' (M)'
  CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,DGI%XAVG_Z0H(:),IRESP,HCOMMENT=YCOMMENT)
  !
ENDIF
!
!----------------------------------------------------------------------------
!
!*       8.     Surface humidity
!               ----------------
IF (DGI%LSURF_VARS) THEN
  !
  YRECFM='QS_ISBA'
  YCOMMENT='X_Y_'//YRECFM//' (KG/KG)'
  CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,DGI%XAVG_QS(:),IRESP,HCOMMENT=YCOMMENT)
  !
ENDIF
!
!----------------------------------------------------------------------------
!
!*       9.     Diag of prognostic fields
!               -------------------------
!
IF (DGU%LPROVAR_TO_DIAG) CALL PROVAR_TO_DIAG
!
!----------------------------------------------------------------------------
!
!User want (or not) patch output
IF(DGI%LPATCH_BUDGET.AND.(I%NPATCH >1))THEN
    !----------------------------------------------------------------------------
    !
    !*      10.     Richardson number (for each patch)
    !               -----------------
    !
    IF (DGI%N2M>=1) THEN
      !
      YRECFM='RI_P'
      YCOMMENT='X_Y_'//YRECFM
      YCOMMENTUNIT='-'      
      CALL WRITE_SURF_FIELD2D(DGU, U, &
                  HPROGRAM,DGI%XRI(:,:),YRECFM,YCOMMENT,YCOMMENTUNIT)
      !
    END IF
    !
    !*       11.     Energy fluxes :(for each patch)
    !                -------------
    !
    IF (DGI%LSURF_BUDGET) THEN
      !
      YRECFM='RN_P'
      YCOMMENT='X_Y_'//YRECFM
      YCOMMENTUNIT='W/m2'
      CALL WRITE_SURF_FIELD2D(DGU, U, &
                  HPROGRAM,DGI%XRN(:,:),YRECFM,YCOMMENT,YCOMMENTUNIT)
      !
      YRECFM='H_P'
      YCOMMENT='X_Y_'//YRECFM
      YCOMMENTUNIT='W/m2'
      CALL WRITE_SURF_FIELD2D(DGU, U, &
                  HPROGRAM,DGI%XH(:,:),YRECFM,YCOMMENT,YCOMMENTUNIT)
      !
      YRECFM='LE_P'
      YCOMMENT='X_Y_'//YRECFM
      YCOMMENTUNIT='W/m2'
      CALL WRITE_SURF_FIELD2D(DGU, U, &
                  HPROGRAM,I%XLE(:,:),YRECFM,YCOMMENT,YCOMMENTUNIT)
      !
      YRECFM='LEI_P'
      YCOMMENT='X_Y_'//YRECFM
      YCOMMENTUNIT='W/m2'
      CALL WRITE_SURF_FIELD2D(DGU, U, &
                  HPROGRAM,DGI%XLEI(:,:),YRECFM,YCOMMENT,YCOMMENTUNIT)
      !
      YRECFM='GFLUX_P'
      YCOMMENT='X_Y_'//YRECFM
      YCOMMENTUNIT='W/m2'
      CALL WRITE_SURF_FIELD2D(DGU, U, &
                  HPROGRAM,DGI%XGFLUX(:,:),YRECFM,YCOMMENT,YCOMMENTUNIT)
      !
      IF (DGI%LRAD_BUDGET .OR. (DGEI%LSURF_BUDGETC .AND. .NOT.DGU%LRESET_BUDGETC)) THEN
        !
        YRECFM='SWD_P'
        YCOMMENT='X_Y_'//YRECFM
        YCOMMENTUNIT='W/m2'
        CALL WRITE_SURF_FIELD2D(DGU, U, &
                  HPROGRAM,DGI%XSWD(:,:),YRECFM,YCOMMENT,YCOMMENTUNIT)
        !
        YRECFM='SWU_P'
        YCOMMENT='X_Y_'//YRECFM
        YCOMMENTUNIT='W/m2'
        CALL WRITE_SURF_FIELD2D(DGU, U, &
                  HPROGRAM,DGI%XSWU(:,:),YRECFM,YCOMMENT,YCOMMENTUNIT)
        !
        YRECFM='LWD_P'
        YCOMMENT='X_Y_'//YRECFM
        YCOMMENTUNIT='W/m2'
        CALL WRITE_SURF_FIELD2D(DGU, U, &
                  HPROGRAM,DGI%XLWD(:,:),YRECFM,YCOMMENT,YCOMMENTUNIT)
        !
        YRECFM='LWU_P'
        YCOMMENT='X_Y_'//YRECFM
        YCOMMENTUNIT='W/m2'
        CALL WRITE_SURF_FIELD2D(DGU, U, &
                  HPROGRAM,DGI%XLWU(:,:),YRECFM,YCOMMENT,YCOMMENTUNIT)
        !
        DO JSW=1, SIZE(DGI%XSWBD,2)
          YNUM=ACHAR(48+JSW)
          !
          YRECFM='SWD_P'//YNUM
          YCOMMENT='X_Y_'//YRECFM
          YCOMMENTUNIT='W/m2'
          CALL WRITE_SURF_FIELD2D(DGU, U, &
                  HPROGRAM,DGI%XSWBD(:,JSW,:),YRECFM,YCOMMENT,YCOMMENTUNIT)
          !
          YRECFM='SWU_P'//YNUM
          YCOMMENT='X_Y_'//YRECFM
          YCOMMENTUNIT='W/m2'
          CALL WRITE_SURF_FIELD2D(DGU, U, &
                  HPROGRAM,DGI%XSWBU(:,JSW,:),YRECFM,YCOMMENT,YCOMMENTUNIT)
          !
        ENDDO
        !
      ENDIF
      !
      YRECFM='FMU_P'
      YCOMMENT='X_Y_'//YRECFM
      YCOMMENTUNIT='Pa'      
      CALL WRITE_SURF_FIELD2D(DGU, U, &
                  HPROGRAM,DGI%XFMU(:,:),YRECFM,YCOMMENT,YCOMMENTUNIT)
      !
      YRECFM='FMV_P'
      YCOMMENT='X_Y_'//YRECFM
      YCOMMENTUNIT='Pa'      
      CALL WRITE_SURF_FIELD2D(DGU, U, &
                  HPROGRAM,DGI%XFMV(:,:),YRECFM,YCOMMENT,YCOMMENTUNIT)
      !
    END IF
    !
    !*       12.    Specific Energy fluxes :(for each patch)
    !               ----------------------------------------
    !
    IF (DGEI%LSURF_EVAP_BUDGET) THEN
      !
      YRECFM='LEG_P'
      YCOMMENT='X_Y_'//YRECFM
      YCOMMENTUNIT='W/m2'
      CALL WRITE_SURF_FIELD2D(DGU, U, &
                  HPROGRAM,DGEI%XLEG(:,:),YRECFM,YCOMMENT,YCOMMENTUNIT)
      !
      YRECFM='LEGI_P'
      YCOMMENT='X_Y_'//YRECFM
      YCOMMENTUNIT='W/m2'
      CALL WRITE_SURF_FIELD2D(DGU, U, &
                  HPROGRAM,DGEI%XLEGI(:,:),YRECFM,YCOMMENT,YCOMMENTUNIT)
      !
      YRECFM='LEV_P'
      YCOMMENT='X_Y_'//YRECFM
      YCOMMENTUNIT='W/m2'
      CALL WRITE_SURF_FIELD2D(DGU, U, &
                  HPROGRAM,DGEI%XLEV(:,:),YRECFM,YCOMMENT,YCOMMENTUNIT)
      !
      YRECFM='LES_P'
      YCOMMENT='X_Y_'//YRECFM
      YCOMMENTUNIT='W/m2'
      CALL WRITE_SURF_FIELD2D(DGU, U, &
                  HPROGRAM,DGEI%XLES(:,:),YRECFM,YCOMMENT,YCOMMENTUNIT)
      !
      IF(I%TSNOW%SCHEME=='3-L' .OR. I%TSNOW%SCHEME=='CRO')THEN  
        YRECFM='LESL_P'
        YCOMMENT='X_Y_'//YRECFM
        YCOMMENTUNIT='W/m2'
        CALL WRITE_SURF_FIELD2D(DGU, U, &
                  HPROGRAM,DGEI%XLESL(:,:),YRECFM,YCOMMENT,YCOMMENTUNIT)
        YRECFM='SNDRIF_P'
        YCOMMENT='X_Y_'//YRECFM
        YCOMMENTUNIT='Kg/m2/s'
        CALL WRITE_SURF_FIELD2D(DGU, U, &
                  HPROGRAM,DGEI%XSNDRIFT(:,:),YRECFM,YCOMMENT,YCOMMENTUNIT)
      ENDIF
      !      
      YRECFM='LER_P'
      YCOMMENT='X_Y_'//YRECFM
      YCOMMENTUNIT='W/m2'
      CALL WRITE_SURF_FIELD2D(DGU, U, &
                  HPROGRAM,DGEI%XLER(:,:),YRECFM,YCOMMENT,YCOMMENTUNIT)
      !
      YRECFM='LETR_P'
      YCOMMENT='X_Y_'//YRECFM
      YCOMMENTUNIT='W/m2'
      CALL WRITE_SURF_FIELD2D(DGU, U, &
                  HPROGRAM,DGEI%XLETR(:,:),YRECFM,YCOMMENT,YCOMMENTUNIT)
      !
      YRECFM='EVAP_P'
      YCOMMENT='X_Y_'//YRECFM
      YCOMMENTUNIT='Kg/m2/s'
      CALL WRITE_SURF_FIELD2D(DGU, U, &
                  HPROGRAM,DGEI%XEVAP(:,:),YRECFM,YCOMMENT,YCOMMENTUNIT)
      !
      YRECFM='SUBL_P'
      YCOMMENT='X_Y_'//YRECFM
      YCOMMENTUNIT='Kg/m2/s'
      CALL WRITE_SURF_FIELD2D(DGU, U, &
                  HPROGRAM,DGEI%XSUBL(:,:),YRECFM,YCOMMENT,YCOMMENTUNIT)
      !
      YRECFM='DRAIN_P'
      YCOMMENT='X_Y_'//YRECFM
      YCOMMENTUNIT='Kg/m2/s'
      CALL WRITE_SURF_FIELD2D(DGU, U, &
                  HPROGRAM,DGEI%XDRAIN(:,:),YRECFM,YCOMMENT,YCOMMENTUNIT)
      !
      IF(I%CRUNOFF=='SGH'.AND.I%CISBA=='DIF')THEN
        YRECFM='QSB_P'
        YCOMMENT='X_Y_'//YRECFM
        YCOMMENTUNIT='Kg/m2/s'
        CALL WRITE_SURF_FIELD2D(DGU, U, &
                  HPROGRAM,DGEI%XQSB(:,:),YRECFM,YCOMMENT,YCOMMENTUNIT)
      ENDIF
      !
      YRECFM='RUNOFF_P'
      YCOMMENT='X_Y_'//YRECFM
      YCOMMENTUNIT='Kg/m2/s'
      CALL WRITE_SURF_FIELD2D(DGU, U, &
                  HPROGRAM,DGEI%XRUNOFF(:,:),YRECFM,YCOMMENT,YCOMMENTUNIT)
      !
      IF(I%CHORT=='SGH'.OR.I%CISBA=='DIF')THEN
        YRECFM='HORTON_P'
        YCOMMENT='X_Y_'//YRECFM
        YCOMMENTUNIT='Kg/m2/s'
        CALL WRITE_SURF_FIELD2D(DGU, U, &
                  HPROGRAM,DGEI%XHORT(:,:),YRECFM,YCOMMENT,YCOMMENTUNIT)
      ENDIF
      !
      YRECFM='DRIVEG_P'
      YCOMMENT='X_Y_'//YRECFM
      YCOMMENTUNIT='Kg/m2/s'
      CALL WRITE_SURF_FIELD2D(DGU, U, &
                  HPROGRAM,DGEI%XDRIP(:,:),YRECFM,YCOMMENT,YCOMMENTUNIT)
      !
      YRECFM='RRVEG_P'
      YCOMMENT='X_Y_'//YRECFM
      YCOMMENTUNIT='Kg/m2/s'
      CALL WRITE_SURF_FIELD2D(DGU, U, &
                  HPROGRAM,DGEI%XRRVEG(:,:),YRECFM,YCOMMENT,YCOMMENTUNIT)
      !
      YRECFM='SNOMLT_P'
      YCOMMENT='X_Y_'//YRECFM
      YCOMMENTUNIT='Kg/m2/s'
      CALL WRITE_SURF_FIELD2D(DGU, U, &
                  HPROGRAM,DGEI%XMELT(:,:),YRECFM,YCOMMENT,YCOMMENTUNIT)
      !
      ! MEB STUFF
      IF (ISIZE_LMEB_PATCH>0) THEN
        YRECFM='LEVCV_P'
        YCOMMENT='X_Y_'//YRECFM
        YCOMMENTUNIT='W/m2'
        CALL WRITE_SURF_FIELD2D(DGU, U, &
                  HPROGRAM,DGEI%XLEVCV(:,:),YRECFM,YCOMMENT,YCOMMENTUNIT)
        !
        YRECFM='LESC_P'
        YCOMMENT='X_Y_'//YRECFM
        YCOMMENTUNIT='W/m2'
        CALL WRITE_SURF_FIELD2D(DGU, U, &
                  HPROGRAM,DGEI%XLESC(:,:),YRECFM,YCOMMENT,YCOMMENTUNIT)
        !
!        YRECFM='LETRGV_P'
!        YCOMMENT='X_Y_'//YRECFM
!        YCOMMENTUNIT='W/m2'
!        CALL WRITE_SURF_FIELD2D(HPROGRAM,YRECFM,XLETRGV(:,:),YRECFM,YCOMMENT,YCOMMENTUNIT)
        !
        YRECFM='LETRCV_P'
        YCOMMENT='X_Y_'//YRECFM
        YCOMMENTUNIT='W/m2'
        CALL WRITE_SURF_FIELD2D(DGU, U, &
                  HPROGRAM,DGEI%XLETRCV(:,:),YRECFM,YCOMMENT,YCOMMENTUNIT)
        !
!        YRECFM='LERGV_P'
!        YCOMMENT='X_Y_'//YRECFM
!        YCOMMENTUNIT='W/m2'
!        CALL WRITE_SURF_FIELD2D(DGU, U, &
!                HPROGRAM,DGEI%XLERGV(:,:),YRECFM,YCOMMENT,YCOMMENTUNIT)
        !
        YRECFM='LELITTER_P'
        YCOMMENT='X_Y_'//YRECFM
        YCOMMENTUNIT='W/m2'
        CALL WRITE_SURF_FIELD2D(DGU, U, &
                        HPROGRAM,DGEI%XLELITTER(:,:),YRECFM,YCOMMENT,YCOMMENTUNIT)
        !
        YRECFM='LELITTERI_P'
        YCOMMENT='X_Y_'//YRECFM
        YCOMMENTUNIT='W/m2'
        CALL WRITE_SURF_FIELD2D(DGU, U, &
                        HPROGRAM,DGEI%XLELITTERI(:,:),YRECFM,YCOMMENT,YCOMMENTUNIT)
        !
        YRECFM='DRIPLIT_P'
        YCOMMENT='X_Y_'//YRECFM
        YCOMMENTUNIT='W/m2'
        CALL WRITE_SURF_FIELD2D(DGU, U, &
                        HPROGRAM,DGEI%XDRIPLIT(:,:),YRECFM,YCOMMENT,YCOMMENTUNIT)
        !
        YRECFM='RRLIT_P'
        YCOMMENT='X_Y_'//YRECFM
        YCOMMENTUNIT='W/m2'
        CALL WRITE_SURF_FIELD2D(DGU, U, &
                        HPROGRAM,DGEI%XRRLIT(:,:),YRECFM,YCOMMENT,YCOMMENTUNIT)
        !
        YRECFM='LERCV_P'
        YCOMMENT='X_Y_'//YRECFM
        YCOMMENTUNIT='W/m2'
        CALL WRITE_SURF_FIELD2D(DGU, U, &
                  HPROGRAM,DGEI%XLERCV(:,:),YRECFM,YCOMMENT,YCOMMENTUNIT)
        !
        YRECFM='LE_C_A_P'
        YCOMMENT='X_Y_'//YRECFM
        YCOMMENTUNIT='W/m2'
        CALL WRITE_SURF_FIELD2D(DGU, U, &
                  HPROGRAM,DGEI%XLE_C_A(:,:),YRECFM,YCOMMENT,YCOMMENTUNIT)
        !
        YRECFM='LE_V_C_P'
        YCOMMENT='X_Y_'//YRECFM
        YCOMMENTUNIT='W/m2'
        CALL WRITE_SURF_FIELD2D(DGU, U, &
                  HPROGRAM,DGEI%XLE_V_C(:,:),YRECFM,YCOMMENT,YCOMMENTUNIT)
        !
        YRECFM='LE_G_C_P'
        YCOMMENT='X_Y_'//YRECFM
        YCOMMENTUNIT='W/m2'
        CALL WRITE_SURF_FIELD2D(DGU, U, &
                  HPROGRAM,DGEI%XLE_G_C(:,:),YRECFM,YCOMMENT,YCOMMENTUNIT)
        !
        YRECFM='LE_N_C_P'
        YCOMMENT='X_Y_'//YRECFM
        YCOMMENTUNIT='W/m2'
        CALL WRITE_SURF_FIELD2D(DGU, U, &
                  HPROGRAM,DGEI%XLE_N_C(:,:),YRECFM,YCOMMENT,YCOMMENTUNIT)
        !
        YRECFM='SWNT_V_P'
        YCOMMENT='X_Y_'//YRECFM
        YCOMMENTUNIT='W/m2'
        CALL WRITE_SURF_FIELD2D(DGU, U, &
                  HPROGRAM,DGEI%XSWNET_V(:,:),YRECFM,YCOMMENT,YCOMMENTUNIT)
        !
        YRECFM='SWNT_G_P'
        YCOMMENT='X_Y_'//YRECFM
        YCOMMENTUNIT='W/m2'
        CALL WRITE_SURF_FIELD2D(DGU, U, &
                  HPROGRAM,DGEI%XSWNET_G(:,:),YRECFM,YCOMMENT,YCOMMENTUNIT)
        !
        YRECFM='SWNT_N_P'
        YCOMMENT='X_Y_'//YRECFM
        YCOMMENTUNIT='W/m2'
        CALL WRITE_SURF_FIELD2D(DGU, U, &
                  HPROGRAM,DGEI%XSWNET_N(:,:),YRECFM,YCOMMENT,YCOMMENTUNIT)
        !
        YRECFM='SWNT_NS_P'
        YCOMMENT='X_Y_'//YRECFM
        YCOMMENTUNIT='W/m2'
        CALL WRITE_SURF_FIELD2D(DGU, U, &
                  HPROGRAM,DGEI%XSWNET_NS(:,:),YRECFM,YCOMMENT,YCOMMENTUNIT)
        !
        YRECFM='LWNT_V_P'
        YCOMMENT='X_Y_'//YRECFM
        YCOMMENTUNIT='W/m2'
        CALL WRITE_SURF_FIELD2D(DGU, U, &
                  HPROGRAM,DGEI%XLWNET_V(:,:),YRECFM,YCOMMENT,YCOMMENTUNIT)
        !
        YRECFM='LWNT_G_P'
        YCOMMENT='X_Y_'//YRECFM
        YCOMMENTUNIT='W/m2'
        CALL WRITE_SURF_FIELD2D(DGU, U, &
                  HPROGRAM,DGEI%XLWNET_G(:,:),YRECFM,YCOMMENT,YCOMMENTUNIT)
        !
        YRECFM='LWNT_N_P'
        YCOMMENT='X_Y_'//YRECFM
        YCOMMENTUNIT='W/m2'
        CALL WRITE_SURF_FIELD2D(DGU, U, &
                  HPROGRAM,DGEI%XLWNET_N(:,:),YRECFM,YCOMMENT,YCOMMENTUNIT)
        !
        YRECFM='SWDN_GN_P'
        YCOMMENT='X_Y_'//YRECFM
        YCOMMENTUNIT='W/m2'
        CALL WRITE_SURF_FIELD2D(DGU, U, &
                  HPROGRAM,DGEI%XSWDOWN_GN(:,:),YRECFM,YCOMMENT,YCOMMENTUNIT)
        !
        YRECFM='LWDN_GN_P'
        YCOMMENT='X_Y_'//YRECFM
        YCOMMENTUNIT='W/m2'
        CALL WRITE_SURF_FIELD2D(DGU, U, &
                  HPROGRAM,DGEI%XLWDOWN_GN(:,:),YRECFM,YCOMMENT,YCOMMENTUNIT)
        !
        YRECFM='H_V_C_P'
        YCOMMENT='X_Y_'//YRECFM
        YCOMMENTUNIT='W/m2'
        CALL WRITE_SURF_FIELD2D(DGU, U, &
                  HPROGRAM,DGEI%XH_V_C(:,:),YRECFM,YCOMMENT,YCOMMENTUNIT)
        !
        YRECFM='H_G_C_P'
        YCOMMENT='X_Y_'//YRECFM
        YCOMMENTUNIT='W/m2'
        CALL WRITE_SURF_FIELD2D(DGU, U, &
                  HPROGRAM,DGEI%XH_G_C(:,:),YRECFM,YCOMMENT,YCOMMENTUNIT)
        !
        YRECFM='H_C_A_P'
        YCOMMENT='X_Y_'//YRECFM
        YCOMMENTUNIT='W/m2'
        CALL WRITE_SURF_FIELD2D(DGU, U, &
                  HPROGRAM,DGEI%XH_C_A(:,:),YRECFM,YCOMMENT,YCOMMENTUNIT)
        !
        YRECFM='H_N_C_P'
        YCOMMENT='X_Y_'//YRECFM
        YCOMMENTUNIT='W/m2'
        CALL WRITE_SURF_FIELD2D(DGU, U, &
                  HPROGRAM,DGEI%XH_N_C(:,:),YRECFM,YCOMMENT,YCOMMENTUNIT)
        !
        YRECFM='SR_GN_P'
        YCOMMENT='X_Y_'//YRECFM
        YCOMMENTUNIT='W/m2'
        CALL WRITE_SURF_FIELD2D(DGU, U, &
                  HPROGRAM,DGEI%XSR_GN(:,:),YRECFM,YCOMMENT,YCOMMENTUNIT)
        !
        YRECFM='MELTCV_P'
        YCOMMENT='X_Y_'//YRECFM
        YCOMMENTUNIT='Kg/m2/s'
        CALL WRITE_SURF_FIELD2D(DGU, U, &
                  HPROGRAM,DGEI%XMELTCV(:,:),YRECFM,YCOMMENT,YCOMMENTUNIT)
        !
        YRECFM='FRZCV_P'
        YCOMMENT='X_Y_'//YRECFM
        YCOMMENTUNIT='Kg/m2/s'
        CALL WRITE_SURF_FIELD2D(DGU, U, &
                  HPROGRAM,DGEI%XFRZCV(:,:),YRECFM,YCOMMENT,YCOMMENTUNIT)
      ENDIF
      ! END MEB STUFF
      !
      IF(LAGRIP)THEN
        YRECFM='IRRIG_P'
        YCOMMENT='X_Y_'//YRECFM
        YCOMMENTUNIT='Kg/m2/s'
        CALL WRITE_SURF_FIELD2D(DGU, U, &
                  HPROGRAM,DGEI%XIRRIG_FLUX(:,:),YRECFM,YCOMMENT,YCOMMENTUNIT)
      ENDIF
      !      
      IF(I%LFLOOD)THEN
        !
        YRECFM='IFLOOD_P'
        YCOMMENT='X_Y_'//YRECFM
        YCOMMENTUNIT='Kg/m2/s'
        CALL WRITE_SURF_FIELD2D(DGU, U, &
                  HPROGRAM,DGEI%XIFLOOD(:,:),YRECFM,YCOMMENT,YCOMMENTUNIT)
        !
        YRECFM='PFLOOD_P'
        YCOMMENT='X_Y_'//YRECFM
        YCOMMENTUNIT='Kg/m2/s'
        CALL WRITE_SURF_FIELD2D(DGU, U, &
                  HPROGRAM,DGEI%XPFLOOD(:,:),YRECFM,YCOMMENT,YCOMMENTUNIT)
        !
        YRECFM='LEF_P'
        YCOMMENT='X_Y_'//YRECFM
        YCOMMENTUNIT='W/m2'
        CALL WRITE_SURF_FIELD2D(DGU, U, &
                  HPROGRAM,DGEI%XLE_FLOOD(:,:),YRECFM,YCOMMENT,YCOMMENTUNIT)
        !
        YRECFM='LEIF_P'
        YCOMMENT='X_Y_'//YRECFM
        YCOMMENTUNIT='W/m2'
        CALL WRITE_SURF_FIELD2D(DGU, U, &
                  HPROGRAM,DGEI%XLEI_FLOOD(:,:),YRECFM,YCOMMENT,YCOMMENTUNIT)
        !
      ENDIF
      !
      IF(I%CPHOTO/='NON')THEN
        !
        YRECFM='GPP_P'
        YCOMMENT='gross primary production per patch '
        YCOMMENTUNIT='kgCO2/m2/s'
        CALL WRITE_SURF_FIELD2D(DGU, U, &
                  HPROGRAM,DGEI%XGPP(:,:),YRECFM,YCOMMENT,YCOMMENTUNIT)
        !
        YRECFM='R_AUTO_P'
        YCOMMENT='autotrophic respiration per patch'
        YCOMMENTUNIT='kgCO2/m2/s'
        CALL WRITE_SURF_FIELD2D(DGU, U, &
                  HPROGRAM,DGEI%XRESP_AUTO(:,:),YRECFM,YCOMMENT,YCOMMENTUNIT)
        !
        YRECFM='R_ECO_P'
        YCOMMENT='ecosystem respiration per patch'
        YCOMMENTUNIT='kgCO2/m2/s'
        CALL WRITE_SURF_FIELD2D(DGU, U, &
                  HPROGRAM,DGEI%XRESP_ECO(:,:),YRECFM,YCOMMENT,YCOMMENTUNIT)
        !
      ENDIF
      !
      IF(DGEI%LWATER_BUDGET)THEN 
        !
        YRECFM='DWG_P'
        YCOMMENT='change in liquid soil moisture per patch'
        YCOMMENTUNIT='Kg/m2/s'
        CALL WRITE_SURF_FIELD2D(DGU, U, &
                  HPROGRAM,DGEI%XDWG(:,:),YRECFM,YCOMMENT,YCOMMENTUNIT)
        !
        YRECFM='DWGI_P'
        YCOMMENT='change in solid soil moisture per patch'
        YCOMMENTUNIT='Kg/m2/s'
        CALL WRITE_SURF_FIELD2D(DGU, U, &
                  HPROGRAM,DGEI%XDWGI(:,:),YRECFM,YCOMMENT,YCOMMENTUNIT)
        !
        YRECFM='DWR_P'
        YCOMMENT='change in water on canopy per patch '
        YCOMMENTUNIT='Kg/m2/s'
        CALL WRITE_SURF_FIELD2D(DGU, U, &
                  HPROGRAM,DGEI%XDWR(:,:),YRECFM,YCOMMENT,YCOMMENTUNIT)
        !
        YRECFM='DSWE_P'
        YCOMMENT='change in snow water equivalent per patch '
        YCOMMENTUNIT='Kg/m2/s'
        CALL WRITE_SURF_FIELD2D(DGU, U, &
                  HPROGRAM,DGEI%XDSWE(:,:),YRECFM,YCOMMENT,YCOMMENTUNIT)
        !
        YRECFM='WATBUD_P'
        YCOMMENT='isba water budget as residue per patch'
        YCOMMENTUNIT='Kg/m2/s'
        CALL WRITE_SURF_FIELD2D(DGU, U, &
                  HPROGRAM,DGEI%XWATBUD(:,:),YRECFM,YCOMMENT,YCOMMENTUNIT)
        !
      ENDIF
      !      
    ENDIF
    !
    !*       13.    surface temperature parameters at 2 and 10 meters (for each patch):
    !               -------------------------------------------------------------------
    !
    IF (DGI%N2M>=1) THEN
      !
      YRECFM='T2M_P'
      YCOMMENT='X_Y_'//YRECFM
      YCOMMENTUNIT='K'
      CALL WRITE_SURF_FIELD2D(DGU, U, &
                  HPROGRAM,DGI%XT2M(:,:),YRECFM,YCOMMENT,YCOMMENTUNIT)
      !
      YRECFM='T2MMIN_P'
      YCOMMENT='X_Y_'//YRECFM
      YCOMMENTUNIT='K'
      CALL WRITE_SURF_FIELD2D(DGU, U, &
                  HPROGRAM,DGI%XT2M_MIN(:,:),YRECFM,YCOMMENT,YCOMMENTUNIT)
      DGI%XT2M_MIN(:,:)=XUNDEF
      !
      YRECFM='T2MMAX_P'
      YCOMMENT='X_Y_'//YRECFM
      YCOMMENTUNIT='K'
      CALL WRITE_SURF_FIELD2D(DGU, U, &
                  HPROGRAM,DGI%XT2M_MAX(:,:),YRECFM,YCOMMENT,YCOMMENTUNIT)
      DGI%XT2M_MAX(:,:)=0.0
      !
      YRECFM='Q2M_P'
      YCOMMENT='X_Y_'//YRECFM
      YCOMMENTUNIT='KG/KG'
      CALL WRITE_SURF_FIELD2D(DGU, U, &
                  HPROGRAM,DGI%XQ2M(:,:),YRECFM,YCOMMENT,YCOMMENTUNIT)
      !
      YRECFM='HU2M_P'
      YCOMMENT='X_Y_'//YRECFM
      YCOMMENTUNIT='PERCENT'
      CALL WRITE_SURF_FIELD2D(DGU, U, &
                  HPROGRAM,DGI%XHU2M(:,:),YRECFM,YCOMMENT,YCOMMENTUNIT)
      !
      YRECFM='ZON10M_P'
      YCOMMENT='X_Y_'//YRECFM
      YCOMMENTUNIT='M/S'
      CALL WRITE_SURF_FIELD2D(DGU, U, &
                  HPROGRAM,DGI%XZON10M(:,:),YRECFM,YCOMMENT,YCOMMENTUNIT)
      !
      YRECFM='MER10M_P'
      YCOMMENT='X_Y_'//YRECFM
      YCOMMENTUNIT='M/S'
      CALL WRITE_SURF_FIELD2D(DGU, U, &
                  HPROGRAM,DGI%XMER10M(:,:),YRECFM,YCOMMENT,YCOMMENTUNIT)
      !
      YRECFM='W10M_P'
      YCOMMENT='X_Y_'//YRECFM
      YCOMMENTUNIT='M/S'
      CALL WRITE_SURF_FIELD2D(DGU, U, &
                  HPROGRAM,DGI%XWIND10M(:,:),YRECFM,YCOMMENT,YCOMMENTUNIT)
      !
    END IF
    !
    !*       14.    Cumulated Energy fluxes :(for each patch)
    !               -----------------------------------------
    !
    IF (DGEI%LSURF_BUDGETC) THEN
      !
      YRECFM='LEGC_P'
      YCOMMENT='X_Y_'//YRECFM
      YCOMMENTUNIT='J/m2'
      CALL WRITE_SURF_FIELD2D(DGU, U, &
                  HPROGRAM,DGEI%XLEGC(:,:),YRECFM,YCOMMENT,YCOMMENTUNIT)
      !
      YRECFM='LEGIC_P'
      YCOMMENT='X_Y_'//YRECFM
      YCOMMENTUNIT='J/m2'
      CALL WRITE_SURF_FIELD2D(DGU, U, &
                  HPROGRAM,DGEI%XLEGIC(:,:),YRECFM,YCOMMENT,YCOMMENTUNIT)
      !
      YRECFM='LEVC_P'
      YCOMMENT='X_Y_'//YRECFM
      YCOMMENTUNIT='J/m2'
      CALL WRITE_SURF_FIELD2D(DGU, U, &
                  HPROGRAM,DGEI%XLEVC(:,:),YRECFM,YCOMMENT,YCOMMENTUNIT)
      !
      YRECFM='LESC_P'
      YCOMMENT='X_Y_'//YRECFM
      YCOMMENTUNIT='J/m2'
      CALL WRITE_SURF_FIELD2D(DGU, U, &
                  HPROGRAM,DGEI%XLESAC(:,:),YRECFM,YCOMMENT,YCOMMENTUNIT)
      !
      IF(I%TSNOW%SCHEME=='3-L' .OR. I%TSNOW%SCHEME=='CRO')THEN  
        YRECFM='LESLC_P'
        YCOMMENT='X_Y_'//YRECFM
        YCOMMENTUNIT='J/m2'
        CALL WRITE_SURF_FIELD2D(DGU, U, &
                  HPROGRAM,DGEI%XLESLC(:,:),YRECFM,YCOMMENT,YCOMMENTUNIT)
        YRECFM='SNDRIFC_P'
        YCOMMENT='X_Y_'//YRECFM
        YCOMMENTUNIT='Kg/m2'
        CALL WRITE_SURF_FIELD2D(DGU, U, &
                  HPROGRAM,DGEI%XSNDRIFTC(:,:),YRECFM,YCOMMENT,YCOMMENTUNIT)
      ENDIF      
      !
      YRECFM='LERC_P'
      YCOMMENT='X_Y_'//YRECFM
      YCOMMENTUNIT='J/m2'
      CALL WRITE_SURF_FIELD2D(DGU, U, &
                  HPROGRAM,DGEI%XLERC(:,:),YRECFM,YCOMMENT,YCOMMENTUNIT)
      !
      YRECFM='LETRC_P'
      YCOMMENT='X_Y_'//YRECFM
      YCOMMENTUNIT='J/m2'
      CALL WRITE_SURF_FIELD2D(DGU, U, &
                  HPROGRAM,DGEI%XLETRC(:,:),YRECFM,YCOMMENT,YCOMMENTUNIT)
      !
      YRECFM='EVAPC_P'
      YCOMMENT='X_Y_'//YRECFM
      YCOMMENTUNIT='Kg/m2'
      CALL WRITE_SURF_FIELD2D(DGU, U, &
                  HPROGRAM,DGEI%XEVAPC(:,:),YRECFM,YCOMMENT,YCOMMENTUNIT)
      !
      YRECFM='SUBLC_P'
      YCOMMENT='X_Y_'//YRECFM
      YCOMMENTUNIT='Kg/m2'
      CALL WRITE_SURF_FIELD2D(DGU, U, &
                  HPROGRAM,DGEI%XSUBLC(:,:),YRECFM,YCOMMENT,YCOMMENTUNIT)
      !
      YRECFM='DRAINC_P'
      YCOMMENT='X_Y_'//YRECFM
      YCOMMENTUNIT='Kg/m2'
      CALL WRITE_SURF_FIELD2D(DGU, U, &
                  HPROGRAM,DGEI%XDRAINC(:,:),YRECFM,YCOMMENT,YCOMMENTUNIT)
      !
      IF(I%CRUNOFF=='SGH'.AND.I%CISBA=='DIF')THEN
        YRECFM='QSBC_P'
        YCOMMENT='X_Y_'//YRECFM
        YCOMMENTUNIT='Kg/m2'
        CALL WRITE_SURF_FIELD2D(DGU, U, &
                  HPROGRAM,DGEI%XQSBC(:,:),YRECFM,YCOMMENT,YCOMMENTUNIT)
      ENDIF
      !
      YRECFM='RUNOFFC_P'
      YCOMMENT='X_Y_'//YRECFM
      YCOMMENTUNIT='Kg/m2'
      CALL WRITE_SURF_FIELD2D(DGU, U, &
                  HPROGRAM,DGEI%XRUNOFFC(:,:),YRECFM,YCOMMENT,YCOMMENTUNIT)
      !
      IF(I%CHORT=='SGH'.OR.I%CISBA=='DIF')THEN
        YRECFM='HORTONC_P'
        YCOMMENT='X_Y_'//YRECFM
        YCOMMENTUNIT='Kg/m2'
        CALL WRITE_SURF_FIELD2D(DGU, U, &
                  HPROGRAM,DGEI%XHORTC(:,:),YRECFM,YCOMMENT,YCOMMENTUNIT)
      ENDIF
      !
      YRECFM='DRIVEGC_P'
      YCOMMENT='X_Y_'//YRECFM
      YCOMMENTUNIT='Kg/m2'
      CALL WRITE_SURF_FIELD2D(DGU, U, &
                  HPROGRAM,DGEI%XDRIPC(:,:),YRECFM,YCOMMENT,YCOMMENTUNIT)
      !
      YRECFM='RRVEGC_P'
      YCOMMENT='X_Y_'//YRECFM
      YCOMMENTUNIT='Kg/m2'
      CALL WRITE_SURF_FIELD2D(DGU, U, &
                  HPROGRAM,DGEI%XRRVEGC(:,:),YRECFM,YCOMMENT,YCOMMENTUNIT)
      !
      YRECFM='SNOMLTC_P'
      YCOMMENT='X_Y_'//YRECFM
      YCOMMENTUNIT='Kg/m2'
      CALL WRITE_SURF_FIELD2D(DGU, U, &
                  HPROGRAM,DGEI%XMELTC(:,:),YRECFM,YCOMMENT,YCOMMENTUNIT)
      !
      ! MEB STUFF
      IF (ISIZE_LMEB_PATCH>0) THEN
        YRECFM='LEVCVC_P'
        YCOMMENT='X_Y_'//YRECFM
        YCOMMENTUNIT='J/m2'
        CALL WRITE_SURF_FIELD2D(DGU, U, &
                  HPROGRAM,DGEI%XLEVCVC(:,:),YRECFM,YCOMMENT,YCOMMENTUNIT)
        !
        YRECFM='LESCC_P'
        YCOMMENT='X_Y_'//YRECFM
        YCOMMENTUNIT='J/m2'
        CALL WRITE_SURF_FIELD2D(DGU, U, &
                  HPROGRAM,DGEI%XLESCC(:,:),YRECFM,YCOMMENT,YCOMMENTUNIT)
        !
!        YRECFM='LETRGVC_P'
!        YCOMMENT='X_Y_'//YRECFM
!        YCOMMENTUNIT='J/m2'
!        CALL WRITE_SURF_FIELD2D(DGU, U, &
!                 HPROGRAM,XLETRGVC(:,:),YRECFM,YCOMMENT,YCOMMENTUNIT)
        !
        YRECFM='LETRCVC_P'
        YCOMMENT='X_Y_'//YRECFM
        YCOMMENTUNIT='J/m2'
        CALL WRITE_SURF_FIELD2D(DGU, U, &
                  HPROGRAM,DGEI%XLETRCVC(:,:),YRECFM,YCOMMENT,YCOMMENTUNIT)
        !
!        YRECFM='LERGVC_P'
!        YCOMMENT='X_Y_'//YRECFM
!        YCOMMENTUNIT='J/m2'
!        CALL WRITE_SURF_FIELD2D(DGU, U, &
!                  HPROGRAM,XLERGVC(:,:),YRECFM,YCOMMENT,YCOMMENTUNIT)
        !
        YRECFM='LERCVC_P'
        YCOMMENT='X_Y_'//YRECFM
        YCOMMENTUNIT='J/m2'
        CALL WRITE_SURF_FIELD2D(DGU, U, &
                  HPROGRAM,DGEI%XLERCVC(:,:),YRECFM,YCOMMENT,YCOMMENTUNIT)
        !
        YRECFM='LE_C_AC_P'
        YCOMMENT='X_Y_'//YRECFM
        YCOMMENTUNIT='J/m2'
        CALL WRITE_SURF_FIELD2D(DGU, U, &
                  HPROGRAM,DGEI%XLE_C_AC(:,:),YRECFM,YCOMMENT,YCOMMENTUNIT)
        !
        YRECFM='LE_V_CC_P'
        YCOMMENT='X_Y_'//YRECFM
        YCOMMENTUNIT='J/m2'
        CALL WRITE_SURF_FIELD2D(DGU, U, &
                  HPROGRAM,DGEI%XLE_V_CC(:,:),YRECFM,YCOMMENT,YCOMMENTUNIT)
        !
        YRECFM='LE_G_CC_P'
        YCOMMENT='X_Y_'//YRECFM
        YCOMMENTUNIT='J/m2'
        CALL WRITE_SURF_FIELD2D(DGU, U, &
                  HPROGRAM,DGEI%XLE_G_CC(:,:),YRECFM,YCOMMENT,YCOMMENTUNIT)
        !
        YRECFM='LE_N_CC_P'
        YCOMMENT='X_Y_'//YRECFM
        YCOMMENTUNIT='J/m2'
        CALL WRITE_SURF_FIELD2D(DGU, U, &
                  HPROGRAM,DGEI%XLE_N_CC(:,:),YRECFM,YCOMMENT,YCOMMENTUNIT)
        !
        YRECFM='SWNT_VC_P'
        YCOMMENT='X_Y_'//YRECFM
        YCOMMENTUNIT='J/m2'
        CALL WRITE_SURF_FIELD2D(DGU, U, &
                  HPROGRAM,DGEI%XSWNET_VC(:,:),YRECFM,YCOMMENT,YCOMMENTUNIT)
        !
        YRECFM='SWNT_GC_P'
        YCOMMENT='X_Y_'//YRECFM
        YCOMMENTUNIT='J/m2'
        CALL WRITE_SURF_FIELD2D(DGU, U, &
                  HPROGRAM,DGEI%XSWNET_GC(:,:),YRECFM,YCOMMENT,YCOMMENTUNIT)
        !
        YRECFM='SWNT_NC_P'
        YCOMMENT='X_Y_'//YRECFM
        YCOMMENTUNIT='J/m2'
        CALL WRITE_SURF_FIELD2D(DGU, U, &
                  HPROGRAM,DGEI%XSWNET_NC(:,:),YRECFM,YCOMMENT,YCOMMENTUNIT)
        !
        YRECFM='SWNT_NSC_P'
        YCOMMENT='X_Y_'//YRECFM
        YCOMMENTUNIT='J/m2'
        CALL WRITE_SURF_FIELD2D(DGU, U, &
                  HPROGRAM,DGEI%XSWNET_NSC(:,:),YRECFM,YCOMMENT,YCOMMENTUNIT)
        !
        YRECFM='LWNT_VC_P'
        YCOMMENT='X_Y_'//YRECFM
        YCOMMENTUNIT='J/m2'
        CALL WRITE_SURF_FIELD2D(DGU, U, &
                  HPROGRAM,DGEI%XLWNET_VC(:,:),YRECFM,YCOMMENT,YCOMMENTUNIT)
        !
        YRECFM='LWNT_GC_P'
        YCOMMENT='X_Y_'//YRECFM
        YCOMMENTUNIT='J/m2'
        CALL WRITE_SURF_FIELD2D(DGU, U, &
                  HPROGRAM,DGEI%XLWNET_GC(:,:),YRECFM,YCOMMENT,YCOMMENTUNIT)
        !
        YRECFM='LWNT_NC_P'
        YCOMMENT='X_Y_'//YRECFM
        YCOMMENTUNIT='J/m2'
        CALL WRITE_SURF_FIELD2D(DGU, U, &
                  HPROGRAM,DGEI%XLWNET_NC(:,:),YRECFM,YCOMMENT,YCOMMENTUNIT)
        !
        YRECFM='SWDN_GNC_P'
        YCOMMENT='X_Y_'//YRECFM
        YCOMMENTUNIT='J/m2'
        CALL WRITE_SURF_FIELD2D(DGU, U, &
                  HPROGRAM,DGEI%XSWDOWN_GNC(:,:),YRECFM,YCOMMENT,YCOMMENTUNIT)
        !
        YRECFM='LWDN_GNC_P'
        YCOMMENT='X_Y_'//YRECFM
        YCOMMENTUNIT='J/m2'
        CALL WRITE_SURF_FIELD2D(DGU, U, &
                  HPROGRAM,DGEI%XLWDOWN_GNC(:,:),YRECFM,YCOMMENT,YCOMMENTUNIT)
        !
        YRECFM='H_V_CC_P'
        YCOMMENT='X_Y_'//YRECFM
        YCOMMENTUNIT='J/m2'
        CALL WRITE_SURF_FIELD2D(DGU, U, &
                  HPROGRAM,DGEI%XH_V_CC(:,:),YRECFM,YCOMMENT,YCOMMENTUNIT)
        !
        YRECFM='H_G_CC_P'
        YCOMMENT='X_Y_'//YRECFM
        YCOMMENTUNIT='J/m2'
        CALL WRITE_SURF_FIELD2D(DGU, U, &
                  HPROGRAM,DGEI%XH_G_CC(:,:),YRECFM,YCOMMENT,YCOMMENTUNIT)
        !
        YRECFM='H_C_AC_P'
        YCOMMENT='X_Y_'//YRECFM
        YCOMMENTUNIT='J/m2'
        CALL WRITE_SURF_FIELD2D(DGU, U, &
                  HPROGRAM,DGEI%XH_C_AC(:,:),YRECFM,YCOMMENT,YCOMMENTUNIT)
        !
        YRECFM='H_N_CC_P'
        YCOMMENT='X_Y_'//YRECFM
        YCOMMENTUNIT='J/m2'
        CALL WRITE_SURF_FIELD2D(DGU, U, &
                  HPROGRAM,DGEI%XH_N_CC(:,:),YRECFM,YCOMMENT,YCOMMENTUNIT)
        !
        YRECFM='SR_GNC_P'
        YCOMMENT='X_Y_'//YRECFM
        YCOMMENTUNIT='Kg/m2'
        CALL WRITE_SURF_FIELD2D(DGU, U, &
                  HPROGRAM,DGEI%XSR_GNC(:,:),YRECFM,YCOMMENT,YCOMMENTUNIT)
        !
        YRECFM='MELTCVC_P'
        YCOMMENT='X_Y_'//YRECFM
        YCOMMENTUNIT='Kg/m2'
        CALL WRITE_SURF_FIELD2D(DGU, U, &
                  HPROGRAM,DGEI%XMELTCVC(:,:),YRECFM,YCOMMENT,YCOMMENTUNIT)
        !
        YRECFM='FRZCVC_P'
        YCOMMENT='X_Y_'//YRECFM
        YCOMMENTUNIT='Kg/m2'
        CALL WRITE_SURF_FIELD2D(DGU, U, &
                  HPROGRAM,DGEI%XFRZCVC(:,:),YRECFM,YCOMMENT,YCOMMENTUNIT)
      ENDIF
      ! END MEB STUFF
      !
      IF(LAGRIP)THEN
        YRECFM='IRRIGC_P'
        YCOMMENT='X_Y_'//YRECFM
        YCOMMENTUNIT='Kg/m2'
        CALL WRITE_SURF_FIELD2D(DGU, U, &
                  HPROGRAM,DGEI%XIRRIG_FLUXC(:,:),YRECFM,YCOMMENT,YCOMMENTUNIT)
      ENDIF      
      !
      IF(I%LGLACIER)THEN
        YRECFM='ICE_FC_P'
        YCOMMENT='X_Y_'//YRECFM
        YCOMMENTUNIT='Kg/m2'
        CALL WRITE_SURF_FIELD2D(DGU, U, &
                  HPROGRAM,DGEI%XICEFLUXC(:,:),YRECFM,YCOMMENT,YCOMMENTUNIT)
      ENDIF
      !
      IF(I%LFLOOD)THEN
        !        
        YRECFM='IFLOODC_P'
        YCOMMENT='X_Y_'//YRECFM
        YCOMMENTUNIT='Kg/m2'
        CALL WRITE_SURF_FIELD2D(DGU, U, &
                  HPROGRAM,DGEI%XIFLOODC(:,:),YRECFM,YCOMMENT,YCOMMENTUNIT)
        !
        YRECFM='PFLOODC_P'
        YCOMMENT='X_Y_'//YRECFM
        YCOMMENTUNIT='Kg/m2'
        CALL WRITE_SURF_FIELD2D(DGU, U, &
                  HPROGRAM,DGEI%XPFLOODC(:,:),YRECFM,YCOMMENT,YCOMMENTUNIT)
        !
        YRECFM='LEFC_P'
        YCOMMENT='X_Y_'//YRECFM
        YCOMMENTUNIT='W/m2'
        CALL WRITE_SURF_FIELD2D(DGU, U, &
                  HPROGRAM,DGEI%XLE_FLOODC(:,:),YRECFM,YCOMMENT,YCOMMENTUNIT)
        !
        YRECFM='LEIFC_P'
        YCOMMENT='X_Y_'//YRECFM
        YCOMMENTUNIT='W/m2'
        CALL WRITE_SURF_FIELD2D(DGU, U, &
                  HPROGRAM,DGEI%XLEI_FLOODC(:,:),YRECFM,YCOMMENT,YCOMMENTUNIT)
        !
      ENDIF
      !
      YRECFM='RNC_P'
      YCOMMENT='X_Y_'//YRECFM
      YCOMMENTUNIT='J/m2'
      CALL WRITE_SURF_FIELD2D(DGU, U, &
                  HPROGRAM,DGEI%XRNC(:,:),YRECFM,YCOMMENT,YCOMMENTUNIT)
      !
      YRECFM='HC_P'
      YCOMMENT='X_Y_'//YRECFM
      YCOMMENTUNIT='J/m2'
      CALL WRITE_SURF_FIELD2D(DGU, U, &
                  HPROGRAM,DGEI%XHC(:,:),YRECFM,YCOMMENT,YCOMMENTUNIT)
      !
      YRECFM='LEC_P'
      YCOMMENT='X_Y_'//YRECFM
      YCOMMENTUNIT='J/m2'
      CALL WRITE_SURF_FIELD2D(DGU, U, &
                  HPROGRAM,DGEI%XLEC(:,:),YRECFM,YCOMMENT,YCOMMENTUNIT)
      !
      YRECFM='LEIC_P'
      YCOMMENT='X_Y_'//YRECFM
      YCOMMENTUNIT='J/m2'
      CALL WRITE_SURF_FIELD2D(DGU, U, &
                  HPROGRAM,DGEI%XLEIC(:,:),YRECFM,YCOMMENT,YCOMMENTUNIT)
      !
      YRECFM='GFLUXC_P'
      YCOMMENT='X_Y_'//YRECFM
      YCOMMENTUNIT='J/m2'
      CALL WRITE_SURF_FIELD2D(DGU, U, &
                  HPROGRAM,DGEI%XGFLUXC(:,:),YRECFM,YCOMMENT,YCOMMENTUNIT)
      !
      IF (DGI%LRAD_BUDGET .OR. (DGEI%LSURF_BUDGETC .AND. .NOT.DGU%LRESET_BUDGETC)) THEN
        !
        YRECFM='SWDC_P'
        YCOMMENT='X_Y_'//YRECFM
        YCOMMENTUNIT='J/m2'
        CALL WRITE_SURF_FIELD2D(DGU, U, &
                  HPROGRAM,DGI%XSWDC(:,:),YRECFM,YCOMMENT,YCOMMENTUNIT)
        !
        YRECFM='SWUC_P'
        YCOMMENT='X_Y_'//YRECFM
        YCOMMENTUNIT='J/m2'
        CALL WRITE_SURF_FIELD2D(DGU, U, &
                  HPROGRAM,DGI%XSWUC(:,:),YRECFM,YCOMMENT,YCOMMENTUNIT)
        !
        YRECFM='LWDC_P'
        YCOMMENT='X_Y_'//YRECFM
        YCOMMENTUNIT='J/m2'
        CALL WRITE_SURF_FIELD2D(DGU, U, &
                  HPROGRAM,DGI%XLWDC(:,:),YRECFM,YCOMMENT,YCOMMENTUNIT)
        !
        YRECFM='LWUC_P'
        YCOMMENT='X_Y_'//YRECFM
        YCOMMENTUNIT='J/m2'
        CALL WRITE_SURF_FIELD2D(DGU, U, &
                  HPROGRAM,DGI%XLWUC(:,:),YRECFM,YCOMMENT,YCOMMENTUNIT)
        !
      ENDIF
      !
      YRECFM='FMUC_P'
      YCOMMENT='X_Y_'//YRECFM
      YCOMMENTUNIT='Pa.s'     
      CALL WRITE_SURF_FIELD2D(DGU, U, &
                  HPROGRAM,DGI%XFMUC(:,:),YRECFM,YCOMMENT,YCOMMENTUNIT)
      !
      YRECFM='FMVC_P'
      YCOMMENT='X_Y_'//YRECFM
      YCOMMENTUNIT='Pa.s'      
      CALL WRITE_SURF_FIELD2D(DGU, U, &
                  HPROGRAM,DGI%XFMVC(:,:),YRECFM,YCOMMENT,YCOMMENTUNIT)
      !
      IF(I%CPHOTO/='NON')THEN
        !
        YRECFM='GPPC_P'
        YCOMMENT='cumulated gross primary production per patch'
        YCOMMENTUNIT='KgCO2/m2'
        CALL WRITE_SURF_FIELD2D(DGU, U, &
                  HPROGRAM,DGEI%XGPPC(:,:),YRECFM,YCOMMENT,YCOMMENTUNIT)
        !
        YRECFM='RC_AUTO_P'
        YCOMMENT='cumulated autotrophic respiration per patch '
        YCOMMENTUNIT='KgCO2/m2'
        CALL WRITE_SURF_FIELD2D(DGU, U, &
                  HPROGRAM,DGEI%XRESPC_AUTO(:,:),YRECFM,YCOMMENT,YCOMMENTUNIT)
        !
        YRECFM='RC_ECO_P'
        YCOMMENT='cumulated ecosystem respiration per patch'
        YCOMMENTUNIT='KgCO2/m2'
        CALL WRITE_SURF_FIELD2D(DGU, U, &
                  HPROGRAM,DGEI%XRESPC_ECO(:,:),YRECFM,YCOMMENT,YCOMMENTUNIT)
        !
      ENDIF
      !  
      IF(DGEI%LWATER_BUDGET .OR. (DGEI%LSURF_BUDGETC .AND. .NOT.DGU%LRESET_BUDGETC))THEN 
        !
        YRECFM='DWGC_P'
        YCOMMENT='cumulated change in liquid soil moisture per patch'
        YCOMMENTUNIT='Kg/m2'
        CALL WRITE_SURF_FIELD2D(DGU, U, &
                  HPROGRAM,DGEI%XDWGC(:,:),YRECFM,YCOMMENT,YCOMMENTUNIT)
        !
        YRECFM='DWGIC_P'
        YCOMMENT='cumulated change in solid soil moisture per patch '
        YCOMMENTUNIT='Kg/m2'
        CALL WRITE_SURF_FIELD2D(DGU, U, &
                  HPROGRAM,DGEI%XDWGIC(:,:),YRECFM,YCOMMENT,YCOMMENTUNIT)
        !
        YRECFM='DWRC_P'
        YCOMMENT='cumulated change in water on canopy per patch '
        YCOMMENTUNIT='Kg/m2'
        CALL WRITE_SURF_FIELD2D(DGU, U, &
                  HPROGRAM,DGEI%XDWRC(:,:),YRECFM,YCOMMENT,YCOMMENTUNIT)
        !
        YRECFM='DSWEC_P'
        YCOMMENT='cumulated change in snow water equivalent per patch '
        YCOMMENTUNIT='Kg/m2'
        CALL WRITE_SURF_FIELD2D(DGU, U, &
                  HPROGRAM,DGEI%XDSWEC(:,:),YRECFM,YCOMMENT,YCOMMENTUNIT)
        !
        YRECFM='WATBUDC_P'
        YCOMMENT='cumulated isba water budget as residue per patch'
        YCOMMENTUNIT='Kg/m2'
        CALL WRITE_SURF_FIELD2D(DGU, U, &
                  HPROGRAM,DGEI%XWATBUDC(:,:),YRECFM,YCOMMENT,YCOMMENTUNIT)
        !
      ENDIF
      !      
    ENDIF
    !-------------------------------------------------------------------------------
ENDIF
!User want (or not) patch output
!-------------------------------------------------------------------------------
!
!*       15.     chemical diagnostics:
!               --------------------
!
IF (CHI%SVI%NBEQ>0 .AND. CHI%CCH_DRY_DEP=="WES89 ") THEN
  !
  DO JSV = 1,SIZE(CHI%CCH_NAMES,1)
    YRECFM='DV_NAT_'//TRIM(CHI%CCH_NAMES(JSV))
    WRITE(YCOMMENT,'(A13,I3.3)')'DV_NAT_',JSV
    YCOMMENTUNIT='m/s'
    CALL WRITE_SURF_FIELD2D(DGU, U, &
                  HPROGRAM,CHI%XDEP(:,JSV,:),YRECFM,YCOMMENT,YCOMMENTUNIT)
  END DO
  !
ENDIF
!
IF (CHI%SVI%NBEQ>0 .AND. CHI%LCH_BIO_FLUX) THEN
  !
  IF (ASSOCIATED(GB%XFISO)) THEN
    YRECFM='FISO'
    WRITE(YCOMMENT,'(A21)')'FISO (molecules/m2/s)'
    CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,GB%XFISO(:),IRESP,HCOMMENT=YCOMMENT)
  END IF
  !
  IF (ASSOCIATED(GB%XFISO)) THEN
    YRECFM='FMONO'
    WRITE(YCOMMENT,'(A22)')'FMONO (molecules/m2/s)'
    CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,GB%XFMONO(:),IRESP,HCOMMENT=YCOMMENT)
  END IF
  !
ENDIF
!
IF (CHI%LCH_NO_FLUX) THEN
  IF (ASSOCIATED(GB%XNOFLUX)) THEN
    YRECFM='NOFLUX'
    WRITE(YCOMMENT,'(A21)')'NOFLUX (molecules/m2/s)'
    CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,GB%XNOFLUX(:),IRESP,HCOMMENT=YCOMMENT)
  END IF
END IF
!
IF (CHI%SVI%NDSTEQ > 0)THEN
  !
  DO JSV = 1,NDSTMDE ! for all dust modes
    WRITE(YRECFM,'(A7,I3.3)')'FLX_DST',JSV
    YCOMMENT='X_Y_'//YRECFM
    YCOMMENTUNIT='kg/m2/s'    
    CALL WRITE_SURF_FIELD2D(DGU, U, &
                  HPROGRAM,DST%XSFDST(:,JSV,:),YRECFM,YCOMMENT,YCOMMENTUNIT)
  END DO
  !
ENDIF
!
!-------------------------------------------------------------------------------
!
!         End of IO
!
 CALL END_IO_SURF_n(HPROGRAM)
IF (LHOOK) CALL DR_HOOK('WRITE_DIAG_SEB_ISBA_N',1,ZHOOK_HANDLE)
!
 CONTAINS
!
!-------------------------------------------------------------------------------
!
SUBROUTINE PROVAR_TO_DIAG
!
REAL, DIMENSION(SIZE(I%XTG,1))             :: ZPATCH, ZWORK
REAL, DIMENSION(SIZE(I%XWG,1),SIZE(I%XWG,2)) :: ZWG
REAL, DIMENSION(SIZE(I%XWG,1),SIZE(I%XWG,2)) :: ZWGI
REAL, DIMENSION(SIZE(I%XTG,1),SIZE(I%XTG,2)) :: ZTG
REAL, DIMENSION(SIZE(I%XDG,1),SIZE(I%XDG,2)) :: ZDG_TOT
REAL, DIMENSION(SIZE(I%XDG,1),SIZE(I%XDG,2),SIZE(I%XDG,3)) :: ZDG
!
REAL, DIMENSION(SIZE(I%XDG,1),I%NNBIOMASS)   :: ZBIOMASS
REAL, DIMENSION(SIZE(I%XDG,1),I%NNSOILCARB)  :: ZSOILCARB
REAL, DIMENSION(SIZE(I%XDG,1),I%NNLITTLEVS)  :: ZLIGNIN_STRUC
REAL, DIMENSION(SIZE(I%XDG,1),I%NNLITTER,I%NNLITTLEVS)  :: ZLITTER
!
 CHARACTER(LEN=4 ) :: YLVL
REAL              :: ZMISS
INTEGER           :: JLAYER, JPATCH, JJ, INI, IWORK, IDEPTH
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
IF (LHOOK) CALL DR_HOOK('WRITE_DIAG_SEB_ISBA_N:PROVAR_TO_DIAG',0,ZHOOK_HANDLE)
!
INI=SIZE(I%XDG,1)
!
! * soil temperatures (K)
!
IF(I%LTEMP_ARP)THEN
  IWORK=I%NTEMPLAYER_ARP
ELSEIF(I%CISBA/='DIF')THEN
  IWORK=2
ELSE
  IWORK=I%NGROUND_LAYER
ENDIF
!
ZTG(:,:)=0.0
DO JPATCH=1,I%NPATCH
   DO JLAYER=1,IWORK
      DO JJ=1,INI 
         ZTG(JJ,JLAYER) = ZTG(JJ,JLAYER) + I%XPATCH(JJ,JPATCH) * I%XTG(JJ,JLAYER,JPATCH)
      ENDDO
   ENDDO
ENDDO
!
DO JLAYER=1,IWORK
  WRITE(YLVL,'(I4)') JLAYER
  YRECFM='TG'//ADJUSTL(YLVL(:LEN_TRIM(YLVL)))
  YRECFM=YRECFM(:LEN_TRIM(YRECFM))//'_ISBA'
  YCOMMENT='X_Y_'//YRECFM//' (K)'
  CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,ZTG(:,JLAYER),IRESP,HCOMMENT=YCOMMENT)
END DO
!
! * Compute soil liquid and ice water content (kg/m2 and m3/m3) 
!
ZWG (:,:)=0.0
ZWGI(:,:)=0.0
ZDG_TOT(:,:)=0.0
!  
IF(I%CISBA=='DIF')THEN
  !
  DO JPATCH=1,I%NPATCH
     DO JLAYER=1,I%NGROUND_LAYER
        DO JJ=1,INI 
!
!          liquid and ice water content
           IDEPTH=I%NWG_LAYER(JJ,JPATCH)
           IF(JLAYER<=IDEPTH)THEN    
             ZWG    (JJ,JLAYER)=ZWG    (JJ,JLAYER)+I%XPATCH(JJ,JPATCH)*I%XWG (JJ,JLAYER,JPATCH)*I%XDZG(JJ,JLAYER,JPATCH)
             ZWGI   (JJ,JLAYER)=ZWGI   (JJ,JLAYER)+I%XPATCH(JJ,JPATCH)*I%XWGI(JJ,JLAYER,JPATCH)*I%XDZG(JJ,JLAYER,JPATCH)
             ZDG_TOT(JJ,JLAYER)=ZDG_TOT(JJ,JLAYER)+I%XPATCH(JJ,JPATCH)*I%XDZG(JJ,JLAYER,JPATCH)
           ENDIF
!                      
        ENDDO
     ENDDO
  ENDDO
!  
ELSE
  !
  ZDG(:,1,:) = I%XDG(:,1,:)
  ZDG(:,2,:) = I%XDG(:,2,:)
  IF(I%CISBA=='3-L')THEN
    ZDG(:,3,:) = I%XDG(:,3,:)-I%XDG(:,2,:)
  ENDIF
!
  DO JPATCH=1,I%NPATCH
     DO JLAYER=1,I%NGROUND_LAYER
        DO JJ=1,INI 
           ZWG    (JJ,JLAYER)=ZWG    (JJ,JLAYER)+I%XPATCH(JJ,JPATCH)*I%XWG (JJ,JLAYER,JPATCH)*ZDG(JJ,JLAYER,JPATCH)
           ZWGI   (JJ,JLAYER)=ZWGI   (JJ,JLAYER)+I%XPATCH(JJ,JPATCH)*I%XWGI(JJ,JLAYER,JPATCH)*ZDG(JJ,JLAYER,JPATCH)
           ZDG_TOT(JJ,JLAYER)=ZDG_TOT(JJ,JLAYER)+I%XPATCH(JJ,JPATCH)*ZDG(JJ,JLAYER,JPATCH)
        ENDDO
     ENDDO
  ENDDO
!  
ENDIF
!
IF(HPROGRAM=='AROME '.OR.HPROGRAM=='FA    ')THEN
  ZMISS=0.0
ELSE
  ZMISS=XUNDEF
ENDIF
!
WHERE(ZDG_TOT(:,:)>0.0)
      ZWG   (:,:)=ZWG (:,:)/ZDG_TOT(:,:)
      ZWGI  (:,:)=ZWGI(:,:)/ZDG_TOT(:,:)
ELSEWHERE
      ZWG   (:,:)=ZMISS
      ZWGI  (:,:)=ZMISS    
ENDWHERE
!
! * soil liquid water content (m3/m3) and soil moisture (kg/m2)
!
DO JLAYER=1,I%NGROUND_LAYER
  WRITE(YLVL,'(I4)') JLAYER
  YRECFM='WG'//ADJUSTL(YLVL(:LEN_TRIM(YLVL)))
  YRECFM=YRECFM(:LEN_TRIM(YRECFM))//'_ISBA'
  YCOMMENT='Soil liquid water content (m3/m3)' 
  CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,ZWG(:,JLAYER),IRESP,HCOMMENT=YCOMMENT)
END DO
!
! * soil ice water content (m3/m3) and soil ice mass (kg/m2)
!
IWORK=I%NGROUND_LAYER
IF(I%CISBA/='DIF')THEN
  IWORK=2 ! No ice in the FR 3-layers
ENDIF
!
DO JLAYER=1,IWORK
  WRITE(YLVL,'(I4)') JLAYER
  YRECFM='WGI'//ADJUSTL(YLVL(:LEN_TRIM(YLVL)))
  YRECFM=YRECFM(:LEN_TRIM(YRECFM))//'_ISBA'
  YCOMMENT='Soil solid water content (m3/m3)' 
  CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,ZWGI(:,JLAYER),IRESP,HCOMMENT=YCOMMENT) 
END DO   
!
! * water intercepted on leaves (kg/m2)
!
ZWORK(:)=0.0
DO JPATCH=1,I%NPATCH
   DO JJ=1,INI 
      ZWORK(JJ) = ZWORK(JJ) + I%XPATCH(JJ,JPATCH) * I%XWR(JJ,JPATCH)
   ENDDO
ENDDO
!
YRECFM='WR_ISBA'
YCOMMENT='X_Y_'//YRECFM//' (kg/m2)'
 CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,ZWORK(:),IRESP,HCOMMENT=YCOMMENT)
!
! * Glacier ice storage (semi-prognostic) (kg/m2)
!
IF(I%LGLACIER)THEN
  !
  ZWORK(:)=0.0
  DO JPATCH=1,I%NPATCH
    DO JJ=1,INI 
       ZWORK(JJ) = ZWORK(JJ) + I%XPATCH(JJ,JPATCH) * I%XICE_STO(JJ,JPATCH)
    ENDDO    
  ENDDO    
  !
  YRECFM='ICE_STO_ISBA'
  YCOMMENT='X_Y_'//YRECFM//' (kg/m2)'
  CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,ZWORK(:),IRESP,HCOMMENT=YCOMMENT)
  !
ENDIF
!
! * Snow albedo (-) 
!
ZPATCH(:) = 0.0
ZWORK (:) = 0.0
DO JPATCH=1,I%NPATCH
   DO JJ=1,INI 
      IF(I%TSNOW%ALB(JJ,JPATCH)/=XUNDEF)THEN
        ZWORK (JJ) = ZWORK (JJ) + I%XPATCH(JJ,JPATCH) * I%TSNOW%ALB(JJ,JPATCH)
        ZPATCH(JJ) = ZPATCH(JJ) + I%XPATCH(JJ,JPATCH)
      ENDIF
   ENDDO
ENDDO
!
WHERE(ZPATCH(:)>0.0)
  ZWORK(:) = ZWORK(:) / ZPATCH(:)
ELSEWHERE
  ZWORK(:) = XUNDEF
ENDWHERE
!
YRECFM='ASN_ISBA'
YCOMMENT='X_Y_'//YRECFM//' (-)'
 CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,ZWORK(:),IRESP,HCOMMENT=YCOMMENT)
!  
IF(I%TSNOW%SCHEME=='3-L' .OR. I%TSNOW%SCHEME=='CRO')THEN
  !
  ! * Snow reservoir (kg/m2) by layer
  !
  DO JLAYER = 1,I%TSNOW%NLAYER
    !
    ZWORK(:)=0.0
    DO JPATCH=1,I%NPATCH
       DO JJ=1,INI 
          ZWORK(JJ) = ZWORK(JJ) + I%XPATCH(JJ,JPATCH) * I%TSNOW%WSNOW(JJ,JLAYER,JPATCH)
       ENDDO
    ENDDO
    !
    WRITE(YLVL,'(I4)') JLAYER
    YRECFM='WSN_'//ADJUSTL(YLVL(:LEN_TRIM(YLVL)))
    YRECFM=YRECFM(:LEN_TRIM(YRECFM))//'_ISBA'
    YCOMMENT='X_Y_'//YRECFM//' (kg/m2)'
    CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,ZWORK(:),IRESP,HCOMMENT=YCOMMENT)
    !
  ENDDO
  !
  ! * Snow depth (m)
  !
  DO JLAYER = 1,I%TSNOW%NLAYER
    !
    ZWORK(:)=0.0
    DO JPATCH=1,I%NPATCH
       DO JJ=1,INI 
         ZWORK(JJ) = ZWORK(JJ) + I%XPATCH(JJ,JPATCH) * I%TSNOW%WSNOW(JJ,JLAYER,JPATCH)/I%TSNOW%RHO(JJ,JLAYER,JPATCH)
       ENDDO
    ENDDO
    !
    WRITE(YLVL,'(I4)') JLAYER
    YRECFM='DSN_'//ADJUSTL(YLVL(:LEN_TRIM(YLVL)))
    YRECFM=YRECFM(:LEN_TRIM(YRECFM))//'_ISBA'
    YCOMMENT='X_Y_'//YRECFM//' (kg/m2)'
    CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,ZWORK(:),IRESP,HCOMMENT=YCOMMENT)
    !
  ENDDO
  !
  ! * Snow temperature (k)
  !  
  IF(HPROGRAM=='AROME '.OR.HPROGRAM=='FA    ')THEN
    ZMISS=XTT
  ELSE
    ZMISS=XUNDEF
  ENDIF
  !  
  DO JLAYER = 1,I%TSNOW%NLAYER
    !
    ZWORK (:) = 0.0
    ZPATCH(:) = 0.0
    DO JPATCH=1,I%NPATCH
       DO JJ=1,INI 
          IF(I%TSNOW%WSNOW(JJ,JLAYER,JPATCH)>0.)THEN
             ZWORK (JJ) = ZWORK (JJ) + I%XPATCH(JJ,JPATCH) * I%TSNOW%TEMP(JJ,JLAYER,JPATCH) 
             ZPATCH(JJ) = ZPATCH(JJ) + I%XPATCH(JJ,JPATCH)
          ENDIF
       ENDDO
    ENDDO
    !
    WHERE(ZPATCH(:)>0.0)
      ZWORK(:) = ZWORK(:) / ZPATCH(:)
    ELSEWHERE
      ZWORK(:) = ZMISS
    ENDWHERE
    !
    WRITE(YLVL,'(I4)') JLAYER
    YRECFM='TSN_'//ADJUSTL(YLVL(:LEN_TRIM(YLVL)))
    YRECFM=YRECFM(:LEN_TRIM(YRECFM))//'_ISBA'
    YCOMMENT='X_Y_'//YRECFM//' (K)'
    CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,ZWORK(:),IRESP,HCOMMENT=YCOMMENT)
    !
  ENDDO
  !
  ! * Snow age (day)
  !    
  DO JLAYER = 1,I%TSNOW%NLAYER
    !
    ZWORK (:) = 0.0
    ZPATCH(:) = 0.0
    DO JPATCH=1,I%NPATCH
       DO JJ=1,INI 
          IF(I%TSNOW%WSNOW(JJ,JLAYER,JPATCH)>0.)THEN    
             ZWORK (JJ) = ZWORK (JJ) + I%XPATCH(JJ,JPATCH) * I%TSNOW%AGE(JJ,JLAYER,JPATCH) 
             ZPATCH(JJ) = ZPATCH(JJ) + I%XPATCH(JJ,JPATCH)
          ENDIF
       ENDDO
    ENDDO
    !
    WHERE(ZPATCH(:)>0.0)
      ZWORK(:) = ZWORK(:) / ZPATCH(:)
    ENDWHERE
    !
    WRITE(YLVL,'(I4)') JLAYER
    YRECFM='AGSN_'//ADJUSTL(YLVL(:LEN_TRIM(YLVL)))
    YRECFM=YRECFM(:LEN_TRIM(YRECFM))//'_ISBA'
    YCOMMENT='X_Y_'//YRECFM//' (day_since_snowfall)'
    CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,ZWORK(:),IRESP,HCOMMENT=YCOMMENT)
    !
  ENDDO
  !
ENDIF
!
! * Isba-Ags biomass reservoir
!
IF(I%CPHOTO=='NIT'.OR.I%CPHOTO=='NCB')THEN
!
  ZBIOMASS(:,:)=0.0
  DO JPATCH=1,I%NPATCH
     DO JLAYER=1,I%NNBIOMASS
        DO JJ=1,INI 
         ZBIOMASS(JJ,JLAYER) = ZBIOMASS(JJ,JLAYER) + I%XPATCH(JJ,JPATCH) * I%XBIOMASS(JJ,JLAYER,JPATCH)
        ENDDO
     ENDDO
  ENDDO
!
  DO JLAYER = 1,I%NNBIOMASS
    WRITE(YLVL,'(I4)') JLAYER
    YRECFM='BIOM'//ADJUSTL(YLVL(:LEN_TRIM(YLVL)))
    YRECFM=YRECFM(:LEN_TRIM(YRECFM))//'_ISBA'
    YCOMMENT='X_Y_'//YRECFM//' (kgDM/m2)'
    CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,ZBIOMASS(:,JLAYER),IRESP,HCOMMENT=YCOMMENT)  
  ENDDO
!
ENDIF
!
! * Isba-CC carbon reservoir
!
IF(I%CRESPSL=='CNT')THEN
!
  ZLITTER(:,:,:)=0.0
  ZLIGNIN_STRUC(:,:)=0.0
  DO JPATCH=1,I%NPATCH
     DO JLAYER=1,I%NNLITTLEVS
       DO JJ=1,INI 
          ZLITTER(JJ,1,JLAYER) = ZLITTER(JJ,1,JLAYER) + I%XPATCH(JJ,JPATCH) * I%XLITTER(JJ,1,JLAYER,JPATCH)
          ZLITTER(JJ,2,JLAYER) = ZLITTER(JJ,2,JLAYER) + I%XPATCH(JJ,JPATCH) * I%XLITTER(JJ,2,JLAYER,JPATCH)
          ZLIGNIN_STRUC(JJ,JLAYER) = ZLIGNIN_STRUC(JJ,JLAYER) + I%XPATCH(JJ,JPATCH) * I%XLIGNIN_STRUC(JJ,JLAYER,JPATCH)
       ENDDO
    ENDDO
  ENDDO
!       
  DO JLAYER=1,I%NNLITTLEVS
     WRITE(YLVL,'(I4)') JLAYER
     YRECFM='LIT1_'//ADJUSTL(YLVL(:LEN_TRIM(YLVL)))
     YRECFM=YRECFM(:LEN_TRIM(YRECFM))//'_ISBA'
     YCOMMENT='X_Y_'//YRECFM//' (gC/m2)'
     CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,ZLITTER(:,1,JLAYER),IRESP,HCOMMENT=YCOMMENT)  
     WRITE(YLVL,'(I4)') JLAYER
     YRECFM='LIT2_'//ADJUSTL(YLVL(:LEN_TRIM(YLVL)))
     YRECFM=YRECFM(:LEN_TRIM(YRECFM))//'_ISBA'
     YCOMMENT='X_Y_'//YRECFM//' (gC/m2)'
     CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,ZLITTER(:,2,JLAYER),IRESP,HCOMMENT=YCOMMENT)
     WRITE(YLVL,'(I4)') JLAYER
     YRECFM='LIGSTR'//ADJUSTL(YLVL(:LEN_TRIM(YLVL)))
     YRECFM=YRECFM(:LEN_TRIM(YRECFM))//'_ISBA'
     YCOMMENT='X_Y_'//YRECFM//' (-)'
     CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,ZLIGNIN_STRUC(:,JLAYER),IRESP,HCOMMENT=YCOMMENT)      
  END DO
!
  ZSOILCARB(:,:)=0.0
  DO JPATCH=1,I%NPATCH
     DO JLAYER=1,I%NNSOILCARB
       DO JJ=1,INI 
          ZSOILCARB(JJ,JLAYER) = ZSOILCARB(JJ,JLAYER) + I%XPATCH(JJ,JPATCH) * I%XSOILCARB(JJ,JLAYER,JPATCH)
       ENDDO
    ENDDO
  ENDDO
!
  DO JLAYER = 1,I%NNSOILCARB
    WRITE(YLVL,'(I4)') JLAYER
    YRECFM='SCARB'//ADJUSTL(YLVL(:LEN_TRIM(YLVL)))
    YRECFM=YRECFM(:LEN_TRIM(YRECFM))//'_ISBA'
    YCOMMENT='X_Y_'//YRECFM//' (gC/m2)'
    CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,ZSOILCARB(:,JLAYER),IRESP,HCOMMENT=YCOMMENT)  
  ENDDO
!
ENDIF
!
IF (LHOOK) CALL DR_HOOK('WRITE_DIAG_SEB_ISBA_N:PROVAR_TO_DIAG',1,ZHOOK_HANDLE)
!
END SUBROUTINE PROVAR_TO_DIAG
!
END SUBROUTINE WRITE_DIAG_SEB_ISBA_n
