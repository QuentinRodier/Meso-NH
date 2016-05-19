!     #########################
      SUBROUTINE INI_DATA_PARAM(PTYPE,PSURF, PSURF2, PLAI, PH_TREE,                 &
                                PALBNIR_VEG, PALBVIS_VEG, PALBUV_VEG, PRSMIN,       &
                                PRGL, PCV, PGAMMA, PGMES, PGC, PBSLAI, PSEFOLD,     &
                                PLAIMIN, PDMAX, PSTRESS, PF2I, PVEG_IN, PVEG_OUT,   &
                                PGREEN, PZ0, PZ0_O_Z0H, PEMIS_ECO, PWRMAX_CF,       &
                                PROOT_LIN, PROOT_EXTINCTION, PSOILRC_SO2,           &
                                PSOILRC_O3, PRE25, PCE_NITRO,PCF_NITRO,PCNA_NITRO,  &
                                PGMES_ST, PGC_ST, PBSLAI_ST, PSEFOLD_ST, PDMAX_ST   )

!     #########################
!
!!**** *INI_DATA_PARAM* initializes secondary cover-field correspondance arrays
!!                      from VEGTYPE and LAI
!!
!!    PURPOSE
!!    -------
!!
!!    METHOD
!!    ------
!!
!!
!!    EXTERNAL
!!    --------
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!
!!    REFERENCE
!!    ---------
!!
!!    AUTHOR
!!    ------
!!
!!    V. Masson        Meteo-France
!!
!!    MODIFICATION
!!    ------------
!!
!!    Original    06/01/2000
!!    F.solmon    01/06/2000 adaptation for patch approach: calculation of parameters 
!!                for each vegtypes of basic covers
!!    V Masson    03/04/2002 set RSMIN value to 120 for NVT_TROG and NVT_C4
!!    L Jarlan    15/10/2004 modify xdata_gmes following Gibelin
!!    P Le Moigne 09/2005 AGS modifs of L. Jarlan (duplicate arrays for ast, lst or nit options)
!!    S. Lafont      03/09 : change unit of RE25
!!    S. Faroux      03/09 : irrigated crops are assumed C4 crops
!!    S. Lafont      09/11 : Reco bare soil is 0; corrected comments
!!    B. Decharme    07/12 : Ponderation coefficient for cumulative root fraction of evergreen forest
!----------------------------------------------------------------------------
!
!*    0.     DECLARATION
!            -----------
!
USE MODD_CSTS,           ONLY : XDAY
USE MODD_DATA_COVER_PAR, ONLY : NVT_TREE, NVT_CONI, NVT_EVER, NVT_C3, &
                                NVT_C4, NVT_IRR, NVT_TROG, NVT_GRAS,  &
                                NVT_PARK, NVT_ROCK, NVT_NO, NVT_SNOW, &
                                NVEGTYPE, JPCOVER
!
USE MODI_VEG_FROM_LAI
USE MODI_GREEN_FROM_LAI
USE MODI_Z0V_FROM_LAI
USE MODI_EMIS_FROM_VEG
USE MODI_ABOR1_SFX
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*    0.1    Declaration of arguments
!            ------------------------
!
REAL, DIMENSION(:,:), INTENT(IN) :: PTYPE
REAL, DIMENSION(:), OPTIONAL, INTENT(IN) :: PSURF
REAL, DIMENSION(:), OPTIONAL, INTENT(IN) :: PSURF2
REAL, DIMENSION(:,:,:), OPTIONAL, INTENT(IN) :: PLAI
REAL, DIMENSION(:,:), OPTIONAL, INTENT(IN) :: PH_TREE
REAL, DIMENSION(:,:), INTENT(OUT), OPTIONAL :: PALBNIR_VEG
REAL, DIMENSION(:,:), INTENT(OUT), OPTIONAL :: PALBVIS_VEG
REAL, DIMENSION(:,:), INTENT(OUT), OPTIONAL :: PALBUV_VEG
REAL, DIMENSION(:,:), INTENT(OUT), OPTIONAL :: PRSMIN
REAL, DIMENSION(:,:), INTENT(OUT), OPTIONAL :: PRGL
REAL, DIMENSION(:,:), INTENT(OUT), OPTIONAL :: PCV
REAL, DIMENSION(:,:), INTENT(OUT), OPTIONAL :: PGAMMA
REAL, DIMENSION(:,:), INTENT(OUT), OPTIONAL :: PGMES
REAL, DIMENSION(:,:), INTENT(OUT), OPTIONAL :: PGC
REAL, DIMENSION(:,:), INTENT(OUT), OPTIONAL :: PBSLAI
REAL, DIMENSION(:,:), INTENT(OUT), OPTIONAL :: PSEFOLD
REAL, DIMENSION(:,:), INTENT(OUT), OPTIONAL :: PLAIMIN
REAL, DIMENSION(:,:), INTENT(OUT), OPTIONAL :: PDMAX
REAL, DIMENSION(:,:), INTENT(OUT), OPTIONAL :: PSTRESS
REAL, DIMENSION(:,:), INTENT(OUT), OPTIONAL :: PF2I
REAL, DIMENSION(:,:,:), INTENT(IN), OPTIONAL :: PVEG_IN
REAL, DIMENSION(:,:,:), INTENT(OUT), OPTIONAL :: PVEG_OUT
REAL, DIMENSION(:,:,:), INTENT(OUT), OPTIONAL :: PGREEN
REAL, DIMENSION(:,:,:), INTENT(OUT), OPTIONAL :: PZ0
REAL, DIMENSION(:,:), INTENT(OUT), OPTIONAL :: PZ0_O_Z0H
REAL, DIMENSION(:,:,:), INTENT(OUT), OPTIONAL :: PEMIS_ECO
REAL, DIMENSION(:,:), INTENT(OUT), OPTIONAL :: PWRMAX_CF
REAL, DIMENSION(:,:), INTENT(OUT), OPTIONAL :: PROOT_LIN
REAL, DIMENSION(:,:), INTENT(OUT), OPTIONAL :: PROOT_EXTINCTION
REAL, DIMENSION(:,:), INTENT(OUT), OPTIONAL :: PSOILRC_SO2
REAL, DIMENSION(:,:), INTENT(OUT), OPTIONAL :: PSOILRC_O3
REAL, DIMENSION(:,:), INTENT(OUT), OPTIONAL :: PRE25
REAL, DIMENSION(:,:), INTENT(OUT), OPTIONAL :: PCE_NITRO
REAL, DIMENSION(:,:), INTENT(OUT), OPTIONAL :: PCF_NITRO
REAL, DIMENSION(:,:), INTENT(OUT), OPTIONAL :: PCNA_NITRO
REAL, DIMENSION(:,:), INTENT(OUT), OPTIONAL :: PGMES_ST
REAL, DIMENSION(:,:), INTENT(OUT), OPTIONAL :: PGC_ST
REAL, DIMENSION(:,:), INTENT(OUT), OPTIONAL :: PBSLAI_ST
REAL, DIMENSION(:,:), INTENT(OUT), OPTIONAL :: PSEFOLD_ST
REAL, DIMENSION(:,:), INTENT(OUT), OPTIONAL :: PDMAX_ST
!
!*    0.2    Declaration of local variables
!      ------------------------------
!
REAL, DIMENSION(SIZE(PTYPE,1)) :: ZGARDEN
LOGICAL            :: LSURF
INTEGER            :: JLOOP                     ! class loop counter
!
INTEGER            :: JMONTH                     ! month loop counter
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------
!
!*    7.     Secondary variables on natural covers
!            -------------------------------------
!
IF (LHOOK) CALL DR_HOOK('INI_DATA_PARAM',0,ZHOOK_HANDLE)
!
LSURF=.TRUE.
!
DO JLOOP=1,SIZE(PTYPE,1)
!  
  IF (PRESENT(PSURF2) .AND. PRESENT(PSURF)) THEN
    LSURF=(PSURF(JLOOP)>0. .OR. PSURF2(JLOOP)>0.)
  ELSEIF (PRESENT(PSURF)) THEN
    LSURF=(PSURF(JLOOP)>0.)
  ENDIF
!
!-------------------------------------------------------------------------------
!
!* nature exists for this cover type
!
  IF (LSURF) THEN
!
!-------------------------------------------------------------------------------
!*    7.5    albnir (veg only)
!            ------
    IF (PRESENT(PALBNIR_VEG)) THEN
      PALBNIR_VEG(JLOOP,:)= 0.30
      IF(PTYPE(JLOOP,NVT_TREE)>0. )  PALBNIR_VEG(JLOOP,NVT_TREE)= 0.25
      IF(PTYPE(JLOOP,NVT_CONI)>0. )  PALBNIR_VEG(JLOOP,NVT_CONI)= 0.15
      IF(PTYPE(JLOOP,NVT_EVER)>0. )  PALBNIR_VEG(JLOOP,NVT_EVER)= 0.21
    ENDIF
!-------------------------------------------------------------------------------
!*    7.6    albvis (veg only)
!            ------
    IF (PRESENT(PALBVIS_VEG)) THEN
      PALBVIS_VEG(JLOOP,:)= 0.10        
      IF(PTYPE(JLOOP,NVT_TREE)>0. )  PALBVIS_VEG(JLOOP,NVT_TREE)= 0.05
      IF(PTYPE(JLOOP,NVT_CONI)>0. )  PALBVIS_VEG(JLOOP,NVT_CONI)= 0.05
      IF(PTYPE(JLOOP,NVT_EVER)>0. )  PALBVIS_VEG(JLOOP,NVT_EVER)= 0.05   
    ENDIF        
!-------------------------------------------------------------------------------
!*    7.6    albUV (veg only)
!            -----
    IF (PRESENT(PALBUV_VEG)) THEN
      PALBUV_VEG(JLOOP,:)= 0.06        
      IF(PTYPE(JLOOP,NVT_TREE)>0. )  PALBUV_VEG(JLOOP,NVT_TREE)= 0.0525
      IF(PTYPE(JLOOP,NVT_CONI)>0. )  PALBUV_VEG(JLOOP,NVT_CONI)= 0.0425
      IF(PTYPE(JLOOP,NVT_EVER)>0. )  PALBUV_VEG(JLOOP,NVT_EVER)= 0.038
      IF(PTYPE(JLOOP,NVT_GRAS)>0. )  PALBUV_VEG(JLOOP,NVT_GRAS)= 0.08
      IF(PTYPE(JLOOP,NVT_TROG)>0. )  PALBUV_VEG(JLOOP,NVT_TROG)= 0.125
      IF(PTYPE(JLOOP,NVT_IRR )>0. )  PALBUV_VEG(JLOOP,NVT_IRR )= 0.045
      IF(PTYPE(JLOOP,NVT_PARK)>0. )  PALBUV_VEG(JLOOP,NVT_PARK)= 0.08
    ENDIF        
!------------------------------------------------------------------------------
!*    7.7    Rsmin
!            -----
    IF (PRESENT(PRSMIN)) THEN
      PRSMIN(JLOOP,:)= 40.
      IF(PTYPE(JLOOP,NVT_TREE)>0. )  PRSMIN(JLOOP,NVT_TREE)= 150.
      IF(PTYPE(JLOOP,NVT_CONI)>0. )  PRSMIN(JLOOP,NVT_CONI)= 150.
      IF(PTYPE(JLOOP,NVT_EVER)>0. )  PRSMIN(JLOOP,NVT_EVER)= 250.
      IF(PTYPE(JLOOP,NVT_TROG)>0. )  PRSMIN(JLOOP,NVT_TROG)= 120.
      IF(PTYPE(JLOOP,NVT_C4  )>0. )  PRSMIN(JLOOP,NVT_C4  )= 120.
      IF(PTYPE(JLOOP,NVT_IRR )>0. )  PRSMIN(JLOOP,NVT_IRR )= 120.
    ENDIF
!-------------------------------------------------------------------------------
!*    7.8    Gamma
!            -----
    IF (PRESENT(PGAMMA)) THEN
      PGAMMA(JLOOP,:)= 0.  
      IF(PTYPE(JLOOP,NVT_TREE)>0. )  PGAMMA(JLOOP,NVT_TREE)= 0.04
      IF(PTYPE(JLOOP,NVT_CONI)>0. )  PGAMMA(JLOOP,NVT_CONI)= 0.04
      IF(PTYPE(JLOOP,NVT_EVER)>0. )  PGAMMA(JLOOP,NVT_EVER)= 0.04
    ENDIF        
!-------------------------------------------------------------------------------
!*    7.8    Wrmax_cf
!            --------
    IF (PRESENT(PWRMAX_CF)) THEN
      PWRMAX_CF(JLOOP,:)= 0.2
      IF(PTYPE(JLOOP,NVT_TREE)>0. )  PWRMAX_CF(JLOOP,NVT_TREE)=0.1
      IF(PTYPE(JLOOP,NVT_CONI)>0. )  PWRMAX_CF(JLOOP,NVT_CONI)=0.1
      IF(PTYPE(JLOOP,NVT_EVER)>0. )  PWRMAX_CF(JLOOP,NVT_EVER)=0.1
    ENDIF 
!-------------------------------------------------------------------------------
!*    7.9    Rgl
!            ---
    IF (PRESENT(PRGL)) THEN
      PRGL(JLOOP,:)= 100.
      IF(PTYPE(JLOOP,NVT_TREE)>0. )  PRGL(JLOOP,NVT_TREE)= 30.
      IF(PTYPE(JLOOP,NVT_CONI)>0. )  PRGL(JLOOP,NVT_CONI)= 30.
      IF(PTYPE(JLOOP,NVT_EVER)>0. )  PRGL(JLOOP,NVT_EVER)= 30.  
    ENDIF    
!-------------------------------------------------------------------------------
!*    7.10   Cv
!            --
    IF (PRESENT(PCV)) THEN
      PCV(JLOOP,:)=2.E-5
      IF(PTYPE(JLOOP,NVT_TREE)>0. )  PCV(JLOOP,NVT_TREE)= 1.E-5
      IF(PTYPE(JLOOP,NVT_CONI)>0. )  PCV(JLOOP,NVT_CONI)= 1.E-5
      IF(PTYPE(JLOOP,NVT_EVER)>0. )  PCV(JLOOP,NVT_EVER)= 1.E-5
    ENDIF    
!-------------------------------------------------------------------------------
!*    7.11   mesophyll conductance (m s-1)
!            -----------------------------
    IF (PRESENT(PGMES)) THEN
      PGMES(JLOOP,:)=0.020
      IF(PTYPE(JLOOP,NVT_TREE)>0. )  PGMES(JLOOP,NVT_TREE)= 0.001
      IF(PTYPE(JLOOP,NVT_CONI)>0. )  PGMES(JLOOP,NVT_CONI)= 0.001
      IF(PTYPE(JLOOP,NVT_EVER)>0. )  PGMES(JLOOP,NVT_EVER)= 0.001
      IF(PTYPE(JLOOP,NVT_C3  )>0. )  PGMES(JLOOP,NVT_C3  )= 0.003
      IF(PTYPE(JLOOP,NVT_C4  )>0. )  PGMES(JLOOP,NVT_C4  )= 0.003
      IF(PTYPE(JLOOP,NVT_IRR )>0. )  PGMES(JLOOP,NVT_IRR )= 0.003
    ENDIF    
!
    IF (PRESENT(PGMES_ST)) THEN
      PGMES_ST(JLOOP,:)=0.003
      IF(PTYPE(JLOOP,NVT_TREE)>0. )  PGMES_ST(JLOOP,NVT_TREE)= 0.003
      IF(PTYPE(JLOOP,NVT_CONI)>0. )  PGMES_ST(JLOOP,NVT_CONI)= 0.002
      IF(PTYPE(JLOOP,NVT_EVER)>0. )  PGMES_ST(JLOOP,NVT_EVER)= 0.002
      IF(PTYPE(JLOOP,NVT_C3  )>0. )  PGMES_ST(JLOOP,NVT_C3  )= 0.001
      IF(PTYPE(JLOOP,NVT_C4  )>0. )  PGMES_ST(JLOOP,NVT_C4  )= 0.009
      IF(PTYPE(JLOOP,NVT_IRR )>0. )  PGMES_ST(JLOOP,NVT_IRR )= 0.009
      IF(PTYPE(JLOOP,NVT_GRAS)>0. ) PGMES_ST(JLOOP,NVT_GRAS )= 0.001
      IF(PTYPE(JLOOP,NVT_TROG)>0. ) PGMES_ST(JLOOP,NVT_TROG )= 0.006
      IF(PTYPE(JLOOP,NVT_PARK)>0. ) PGMES_ST(JLOOP,NVT_PARK )= 0.001
    ENDIF    
!-------------------------------------------------------------------------------
!*    7.11   Ecosystem Respiration (kg m-2 s-1)
!            -----------------------------------
    IF (PRESENT(PRE25)) THEN
      PRE25(JLOOP,:)= 3.6E-7  
      IF(PTYPE(JLOOP,NVT_CONI)>0. )  PRE25(JLOOP,NVT_CONI)= 1.8E-7
      IF(PTYPE(JLOOP,NVT_C4  )>0. )  PRE25(JLOOP,NVT_C4  )= 3.0E-7
      IF(PTYPE(JLOOP,NVT_IRR )>0. )  PRE25(JLOOP,NVT_IRR )= 3.0E-7
      !ecosystem respiration only if vegetetation is present
      IF(PTYPE(JLOOP,NVT_NO  )>0. )  PRE25(JLOOP,NVT_NO  )= 0.
      IF(PTYPE(JLOOP,NVT_ROCK)>0. )  PRE25(JLOOP,NVT_ROCK)= 0.
      IF(PTYPE(JLOOP,NVT_SNOW)>0. )  PRE25(JLOOP,NVT_SNOW)= 0.
    ENDIF
!-------------------------------------------------------------------------------
!*    7.11   cuticular conductance (m s-1)
!            -----------------------------
    IF (PRESENT(PGC)) THEN
      PGC(JLOOP,:)=0.00025
      IF(PTYPE(JLOOP,NVT_TREE)>0. )  PGC(JLOOP,NVT_TREE)= 0.00015
      IF(PTYPE(JLOOP,NVT_CONI)>0. )  PGC(JLOOP,NVT_CONI)= 0.
      IF(PTYPE(JLOOP,NVT_EVER)>0. )  PGC(JLOOP,NVT_EVER)= 0.00015  
    ENDIF
!
    IF (PRESENT(PGC_ST)) THEN
      PGC_ST(JLOOP,:)=0.00015
      IF(PTYPE(JLOOP,NVT_CONI)>0. )  PGC_ST(JLOOP,NVT_CONI)= 0.
      IF(PTYPE(JLOOP,NVT_C3  )>0. )  PGC_ST(JLOOP,NVT_C3  )= 0.00025
      IF(PTYPE(JLOOP,NVT_GRAS)>0. )  PGC_ST(JLOOP,NVT_GRAS)= 0.00025
      IF(PTYPE(JLOOP,NVT_PARK)>0. )  PGC_ST(JLOOP,NVT_PARK)= 0.001
    ENDIF    
!-------------------------------------------------------------------------------
!*    7.11   critical normilized soil water content for stress parameterisation
!            ------------------------------------------------------------------
    IF (PRESENT(PF2I)) PF2I(JLOOP,:)=0.3
!-------------------------------------------------------------------------------
!*    7.12   ratio d(biomass)/d(lai) (kg/m2)
!            -----------------------
    IF (PRESENT(PBSLAI)) THEN
      PBSLAI(JLOOP,:)=0.36 
      IF(PTYPE(JLOOP,NVT_TREE)>0. )  PBSLAI(JLOOP,NVT_TREE)= 0.25
      IF(PTYPE(JLOOP,NVT_CONI)>0. )  PBSLAI(JLOOP,NVT_CONI)= 0.25
      IF(PTYPE(JLOOP,NVT_EVER)>0. )  PBSLAI(JLOOP,NVT_EVER)= 0.25
      IF(PTYPE(JLOOP,NVT_C3  )>0. )  PBSLAI(JLOOP,NVT_C3  )= 0.06
      IF(PTYPE(JLOOP,NVT_C4  )>0. )  PBSLAI(JLOOP,NVT_C4  )= 0.06
      IF(PTYPE(JLOOP,NVT_IRR )>0. )  PBSLAI(JLOOP,NVT_IRR )= 0.06
    ENDIF
!    
    IF (PRESENT(PBSLAI_ST)) THEN
      PBSLAI_ST(JLOOP,:)=0.08 
      IF(PTYPE(JLOOP,NVT_TREE)>0. )  PBSLAI_ST(JLOOP,NVT_TREE)= 0.125
      IF(PTYPE(JLOOP,NVT_CONI)>0. )  PBSLAI_ST(JLOOP,NVT_CONI)= 0.50
      IF(PTYPE(JLOOP,NVT_EVER)>0. )  PBSLAI_ST(JLOOP,NVT_EVER)= 0.25
      IF(PTYPE(JLOOP,NVT_C3  )>0. )  PBSLAI_ST(JLOOP,NVT_C3  )= 0.06
      IF(PTYPE(JLOOP,NVT_C4  )>0. )  PBSLAI_ST(JLOOP,NVT_C4  )= 0.06
      IF(PTYPE(JLOOP,NVT_IRR )>0. )  PBSLAI_ST(JLOOP,NVT_IRR )= 0.06
    ENDIF    
!-------------------------------------------------------------------------------
!*    7.12   maximum air saturation deficit tolerate by vegetation (kg/kg)
!            -----------------------------------------------------
    IF (PRESENT(PDMAX)) THEN
      PDMAX(JLOOP,:) = 0.1
      IF(PTYPE(JLOOP,NVT_TREE)>0. )  PDMAX(JLOOP,NVT_TREE)= 0.1
      IF(PTYPE(JLOOP,NVT_CONI)>0. )  PDMAX(JLOOP,NVT_CONI)= 0.1
      IF(PTYPE(JLOOP,NVT_EVER)>0. )  PDMAX(JLOOP,NVT_EVER)= 0.1
    ENDIF    
!
    IF (PRESENT(PDMAX_ST)) THEN
      PDMAX_ST(JLOOP,:) = 0.05
      IF(PTYPE(JLOOP,NVT_C4  )>0. )  PDMAX_ST(JLOOP,NVT_C4  )= 0.033
      IF(PTYPE(JLOOP,NVT_IRR )>0. )  PDMAX_ST(JLOOP,NVT_IRR )= 0.033
      IF(PTYPE(JLOOP,NVT_TROG)>0. )  PDMAX_ST(JLOOP,NVT_TROG)= 0.052
      IF(PTYPE(JLOOP,NVT_CONI)>0. )  PDMAX_ST(JLOOP,NVT_CONI)= 0.124
      IF(PTYPE(JLOOP,NVT_EVER)>0. )  PDMAX_ST(JLOOP,NVT_EVER)= 0.124
      IF(PTYPE(JLOOP,NVT_TREE)>0. )  PDMAX_ST(JLOOP,NVT_TREE)= 0.109
    ENDIF    
!-------------------------------------------------------------------------------
!*    7.12   Defensive/offensive strategy (1/0)
!            ----------------------------
    IF (PRESENT(PSTRESS)) THEN
      PSTRESS(JLOOP,:) = 1. 
      IF(PTYPE(JLOOP,NVT_TREE)>0. )  PSTRESS(JLOOP,NVT_TREE)= 0.
      IF(PTYPE(JLOOP,NVT_EVER)>0. )  PSTRESS(JLOOP,NVT_EVER)= 0.
      IF(PTYPE(JLOOP,NVT_C4  )>0. )  PSTRESS(JLOOP,NVT_C4  )= 0.
      IF(PTYPE(JLOOP,NVT_GRAS)>0. )  PSTRESS(JLOOP,NVT_GRAS)= 0.
      IF(PTYPE(JLOOP,NVT_TROG)>0. )  PSTRESS(JLOOP,NVT_TROG)= 0.
      IF(PTYPE(JLOOP,NVT_PARK)>0. )  PSTRESS(JLOOP,NVT_PARK)= 0.
    ENDIF    
!-------------------------------------------------------------------------------
!*    7.13   e-folding time for senescence (days)
!            ------------------------------------
! parameters use in case HPHOTO == 'NONE' 'AGS' 'LAI'
    IF (PRESENT(PSEFOLD)) THEN
      PSEFOLD(JLOOP,:)=90. * XDAY
      IF(PTYPE(JLOOP,NVT_TREE)>0. )  PSEFOLD(JLOOP,NVT_TREE)= 365.* XDAY
      IF(PTYPE(JLOOP,NVT_CONI)>0. )  PSEFOLD(JLOOP,NVT_CONI)= 365.* XDAY
      IF(PTYPE(JLOOP,NVT_EVER)>0. )  PSEFOLD(JLOOP,NVT_EVER)= 365.* XDAY
      IF(PTYPE(JLOOP,NVT_C3  )>0. )  PSEFOLD(JLOOP,NVT_C3  )= 60.* XDAY
      IF(PTYPE(JLOOP,NVT_C4  )>0. )  PSEFOLD(JLOOP,NVT_C4  )= 60.* XDAY
      IF(PTYPE(JLOOP,NVT_IRR )>0. )  PSEFOLD(JLOOP,NVT_IRR )= 60.* XDAY
    ENDIF    
!
! parameters use in case HPHOTO == 'AST','LST' 'NIT', 'NCB'

    IF (PRESENT(PSEFOLD_ST)) THEN
      PSEFOLD_ST(JLOOP,:)=150. * XDAY
      IF(PTYPE(JLOOP,NVT_TREE)>0. )  PSEFOLD_ST(JLOOP,NVT_TREE)= 230.* XDAY
      IF(PTYPE(JLOOP,NVT_CONI)>0. )  PSEFOLD_ST(JLOOP,NVT_CONI)= 365.* XDAY
      IF(PTYPE(JLOOP,NVT_EVER)>0. )  PSEFOLD_ST(JLOOP,NVT_EVER)= 365.* XDAY
    ENDIF    
!-------------------------------------------------------------------------------
!*    7.14   Minimum LAI (m2/m2)
!            -------------------
! Modi lai/patch defined
    IF (PRESENT(PLAIMIN)) THEN
      PLAIMIN (JLOOP,:) = 0.3
      IF(PTYPE(JLOOP,NVT_CONI)>0. )  PLAIMIN (JLOOP,NVT_CONI) = 1.0
      IF(PTYPE(JLOOP,NVT_EVER)>0. )  PLAIMIN (JLOOP,NVT_EVER) = 1.0
    ENDIF   
!------------------------------------------------------------------------
!*    2.20   leaf aera ratio sensitivity to nitrogen concentration
!            ----------
    IF (PRESENT(PCE_NITRO)) THEN
      PCE_NITRO(JLOOP,:)=7.68
      IF(PTYPE(JLOOP,NVT_TREE)>0. )  PCE_NITRO(JLOOP,NVT_TREE)= 4.83
      IF(PTYPE(JLOOP,NVT_CONI)>0. )  PCE_NITRO(JLOOP,NVT_CONI)= 4.85
      IF(PTYPE(JLOOP,NVT_EVER)>0. )  PCE_NITRO(JLOOP,NVT_EVER)= 4.83
      IF(PTYPE(JLOOP,NVT_C3  )>0. )  PCE_NITRO(JLOOP,NVT_C3  )= 3.79
      IF(PTYPE(JLOOP,NVT_GRAS)>0. )  PCE_NITRO(JLOOP,NVT_GRAS)= 5.56
      IF(PTYPE(JLOOP,NVT_PARK)>0. )  PCE_NITRO(JLOOP,NVT_PARK)= 5.56
    ENDIF    
!-------------------------------------------------------------------------------
!*    2.21   lethal minimum value of leaf area ratio
!            ----------
    IF (PRESENT(PCF_NITRO)) THEN
      PCF_NITRO(JLOOP,:)=-4.33
      IF(PTYPE(JLOOP,NVT_TREE)>0. )  PCF_NITRO(JLOOP,NVT_TREE)= 2.53
      IF(PTYPE(JLOOP,NVT_CONI)>0. )  PCF_NITRO(JLOOP,NVT_CONI)= -0.24
      IF(PTYPE(JLOOP,NVT_EVER)>0. )  PCF_NITRO(JLOOP,NVT_EVER)= 2.53
      IF(PTYPE(JLOOP,NVT_C3  )>0. )  PCF_NITRO(JLOOP,NVT_C3  )= 9.84
      IF(PTYPE(JLOOP,NVT_GRAS)>0. )  PCF_NITRO(JLOOP,NVT_GRAS)= 6.73
      IF(PTYPE(JLOOP,NVT_PARK)>0. )  PCF_NITRO(JLOOP,NVT_PARK)= 6.73
    ENDIF    
!-------------------------------------------------------------------------------
!*    2.22   nitrogen concentration of active biomass (assimilated to N
!            concentration of leaf biomass following Gibelin)
!            ----------
    IF (PRESENT(PCNA_NITRO)) THEN
      PCNA_NITRO(JLOOP,:)=1.3
      IF(PTYPE(JLOOP,NVT_C4  )>0. )  PCNA_NITRO(JLOOP,NVT_C4  )= 1.9
      IF(PTYPE(JLOOP,NVT_IRR )>0. )  PCNA_NITRO(JLOOP,NVT_IRR) = 1.9
      IF(PTYPE(JLOOP,NVT_CONI)>0. )  PCNA_NITRO(JLOOP,NVT_CONI)= 2.8
      IF(PTYPE(JLOOP,NVT_TREE)>0. )  PCNA_NITRO(JLOOP,NVT_TREE)= 2.0
      IF(PTYPE(JLOOP,NVT_EVER)>0. )  PCNA_NITRO(JLOOP,NVT_EVER)= 2.5
    ENDIF    
!-------------------------------------------------------------------------------
!*    7.15   Jackson (1996) coefficient for cumulative root fraction
!            -------------------------------------------------------
    IF (PRESENT(PROOT_EXTINCTION)) THEN
      PROOT_EXTINCTION(JLOOP,:)= 0.943 ! no vegetation (default value)
      IF(PTYPE(JLOOP,NVT_TREE)>0. )  PROOT_EXTINCTION(JLOOP,NVT_TREE)= 0.966
      IF(PTYPE(JLOOP,NVT_CONI)>0. )  PROOT_EXTINCTION(JLOOP,NVT_CONI)= 0.943
      IF(PTYPE(JLOOP,NVT_EVER)>0. )  PROOT_EXTINCTION(JLOOP,NVT_EVER)= 0.962
      IF(PTYPE(JLOOP,NVT_C3  )>0. )  PROOT_EXTINCTION(JLOOP,NVT_C3  )= 0.961
      IF(PTYPE(JLOOP,NVT_C4  )>0. )  PROOT_EXTINCTION(JLOOP,NVT_C4  )= 0.972
      IF(PTYPE(JLOOP,NVT_IRR )>0. )  PROOT_EXTINCTION(JLOOP,NVT_IRR )= 0.972
      IF(PTYPE(JLOOP,NVT_GRAS)>0. )  PROOT_EXTINCTION(JLOOP,NVT_GRAS)= 0.943
      IF(PTYPE(JLOOP,NVT_PARK)>0. )  PROOT_EXTINCTION(JLOOP,NVT_PARK)= 0.943
      IF(PTYPE(JLOOP,NVT_TROG)>0. )  PROOT_EXTINCTION(JLOOP,NVT_TROG)= 0.972
    ENDIF    
!-------------------------------------------------------------------------------
!*    7.16   Ponderation coefficient between formulations for cumulative root fraction
!            -------------------------------------------------------------------------
!
    IF (PRESENT(PROOT_LIN)) THEN
      PROOT_LIN(JLOOP,:)= 0.05
      IF(PTYPE(JLOOP,NVT_EVER)>0. )  PROOT_LIN(JLOOP,NVT_EVER)= 0.25
    ENDIF
!
!-------------------------------------------------------------------------------
!*    7.17   Coefficient for chemistry deposition of SO2
!            -------------------------------------------
    IF (PRESENT(PSOILRC_SO2)) THEN
      PSOILRC_SO2(JLOOP,:)= 9999.
      IF(PTYPE(JLOOP,NVT_TREE)>0. )  PSOILRC_SO2(JLOOP,NVT_TREE)= 500.
      IF(PTYPE(JLOOP,NVT_CONI)>0. )  PSOILRC_SO2(JLOOP,NVT_CONI)= 500.
      IF(PTYPE(JLOOP,NVT_EVER)>0. )  PSOILRC_SO2(JLOOP,NVT_EVER)= 200.
      IF(PTYPE(JLOOP,NVT_C3  )>0. )  PSOILRC_SO2(JLOOP,NVT_C3  )= 150.
      IF(PTYPE(JLOOP,NVT_C4  )>0. )  PSOILRC_SO2(JLOOP,NVT_C4  )= 150.
      IF(PTYPE(JLOOP,NVT_IRR )>0. )  PSOILRC_SO2(JLOOP,NVT_IRR )= 0.001
      IF(PTYPE(JLOOP,NVT_GRAS)>0. )  PSOILRC_SO2(JLOOP,NVT_GRAS)= 350.
      IF(PTYPE(JLOOP,NVT_PARK)>0. )  PSOILRC_SO2(JLOOP,NVT_PARK)= 350.
      IF(PTYPE(JLOOP,NVT_TROG)>0. )  PSOILRC_SO2(JLOOP,NVT_TROG)= 350.
      IF(PTYPE(JLOOP,NVT_NO  )>0. )  PSOILRC_SO2(JLOOP,NVT_NO  )=1000.
      IF(PTYPE(JLOOP,NVT_ROCK)>0. )  PSOILRC_SO2(JLOOP,NVT_ROCK)= 400.
      IF(PTYPE(JLOOP,NVT_SNOW)>0. )  PSOILRC_SO2(JLOOP,NVT_SNOW)= 100.
    ENDIF
!------------------------------------------------------------------------------
!*    7.18   Coefficient for chemistry deposition of O3
!            ------------------------------------------
    IF (PRESENT(PSOILRC_O3)) THEN
      PSOILRC_O3(JLOOP,:)= 9999.
      IF(PTYPE(JLOOP,NVT_TREE)>0. )  PSOILRC_O3(JLOOP,NVT_TREE)= 200.
      IF(PTYPE(JLOOP,NVT_CONI)>0. )  PSOILRC_O3(JLOOP,NVT_CONI)= 200.
      IF(PTYPE(JLOOP,NVT_EVER)>0. )  PSOILRC_O3(JLOOP,NVT_EVER)= 500.
      IF(PTYPE(JLOOP,NVT_C3  )>0. )  PSOILRC_O3(JLOOP,NVT_C3  )= 150.
      IF(PTYPE(JLOOP,NVT_C4  )>0. )  PSOILRC_O3(JLOOP,NVT_C4  )= 150.
      IF(PTYPE(JLOOP,NVT_IRR )>0. )  PSOILRC_O3(JLOOP,NVT_IRR )=1000.
      IF(PTYPE(JLOOP,NVT_GRAS)>0. )  PSOILRC_O3(JLOOP,NVT_GRAS)= 200.
      IF(PTYPE(JLOOP,NVT_PARK)>0. )  PSOILRC_O3(JLOOP,NVT_PARK)= 200.
      IF(PTYPE(JLOOP,NVT_TROG)>0. )  PSOILRC_O3(JLOOP,NVT_TROG)= 200.
      IF(PTYPE(JLOOP,NVT_NO  )>0. )  PSOILRC_O3(JLOOP,NVT_NO  )= 400.
      IF(PTYPE(JLOOP,NVT_ROCK)>0. )  PSOILRC_O3(JLOOP,NVT_ROCK)= 200.
      IF(PTYPE(JLOOP,NVT_SNOW)>0. )  PSOILRC_O3(JLOOP,NVT_SNOW)=3500.
    ENDIF
!-------------------------------------------------------------------------------
!*    7.15   vegetation and greeness fractions
!            ---------------------------------
    IF (PRESENT(PVEG_OUT) .AND. PRESENT(PLAI)) THEN
      DO JMONTH=1,SIZE(PVEG_OUT,2)
          PVEG_OUT(JLOOP,JMONTH,:) = VEG_FROM_LAI(PLAI(JLOOP,JMONTH,:),   &
                                         PTYPE(JLOOP,:))         
      END DO
    ELSEIF (PRESENT(PVEG_OUT) .AND. .NOT. PRESENT(PLAI)) THEN
     CALL ABOR1_SFX("INI_DATA_PARAM: WHEN CALLING WITH PVEG_OUT, PLAI MUST BE IN ARGUMENTS TOO") 
    ENDIF
! 
         
    IF (PRESENT(PGREEN) .AND. PRESENT(PLAI)) THEN
      DO JMONTH=1,SIZE(PGREEN,2)
          PGREEN(JLOOP,JMONTH,:) = GREEN_FROM_LAI(PLAI(JLOOP,JMONTH,:),   &
                                           PTYPE(JLOOP,:))  
      END DO
    ELSEIF (PRESENT(PGREEN) .AND. .NOT. PRESENT(PLAI)) THEN
     CALL ABOR1_SFX("INI_DATA_PARAM: WHEN CALLING WITH PGREEN, PLAI MUST BE IN ARGUMENTS TOO")
    ENDIF     
!-------------------------------------------------------------------------------
!*    7.16   z0
!            --
   IF (PRESENT(PZ0) .AND. PRESENT(PLAI) .AND. PRESENT(PH_TREE)) THEN
     DO JMONTH=1,SIZE(PZ0,2)
       PZ0(JLOOP,JMONTH,:) = Z0V_FROM_LAI(PLAI(JLOOP,JMONTH,:),   &
                                                  PH_TREE(JLOOP,:),       &
                                                  PTYPE(JLOOP,:)       )  
     END DO
   ELSEIF (PRESENT(PZ0) .AND. (.NOT. PRESENT(PLAI) .OR. .NOT. PRESENT(PH_TREE))) THEN
     CALL ABOR1_SFX("INI_DATA_PARAM: WHEN CALLING WITH PZ0, PLAI AND PH_TREE MUST BE IN ARGUMENTS TOO")
   ENDIF   
!-------------------------------------------------------------------------------
!*    7.17   z0/z0h
!            ------
    IF (PRESENT(PZ0_O_Z0H)) PZ0_O_Z0H (JLOOP,:) = 10.
!-------------------------------------------------------------------------------
!*    7.18   emissivity
!            ----------
    IF (PRESENT(PEMIS_ECO) .AND. (PRESENT(PVEG_IN).OR.PRESENT(PVEG_OUT))) THEN
      DO JMONTH=1,SIZE(PEMIS_ECO,2)

        IF (PRESENT(PVEG_OUT)) THEN
          PEMIS_ECO(JLOOP,JMONTH,:) = EMIS_FROM_VEG(PVEG_OUT(JLOOP,JMONTH,:),  &
                PTYPE(JLOOP,:))  
        ELSEIF (PRESENT(PVEG_IN)) THEN
          PEMIS_ECO(JLOOP,JMONTH,:) = EMIS_FROM_VEG(PVEG_IN(JLOOP,JMONTH,:),  &
                PTYPE(JLOOP,:))  
        ENDIF
      END DO
    ELSEIF (PRESENT(PEMIS_ECO) .AND. .NOT.PRESENT(PVEG_IN) .AND. .NOT.PRESENT(PVEG_OUT)) THEN
      CALL ABOR1_SFX("INI_DATA_PARAM: WHEN CALLING WITH PEMIS_ECO, PVEG_IN OR PVEG_OUT MUST BE IN ARGUMENTS TOO")
    ENDIF
!-------------------------------------------------------------------------------
  END IF
!-------------------------------------------------------------------------------
END DO
!-------------------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('INI_DATA_PARAM',1,ZHOOK_HANDLE)
!-------------------------------------------------------------------------------
!
END SUBROUTINE INI_DATA_PARAM
