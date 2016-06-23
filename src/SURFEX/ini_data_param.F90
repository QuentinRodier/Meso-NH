!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########################
      SUBROUTINE INI_DATA_PARAM(PTYPE,PSURF, PSURF2, PLAI, PH_TREE,                 &
                                PALBNIR_VEG, PALBVIS_VEG, PALBUV_VEG, PRSMIN,       &
                                PRGL, PCV, PGAMMA, PGMES, PGC, PBSLAI, PSEFOLD,     &
                                PLAIMIN_IN, PLAIMIN_OUT, PDMAX, PSTRESS, PF2I,      &
                                PVEG_IN, PVEG_OUT,                                  &
                                PGREEN, PZ0, PZ0_O_Z0H, PEMIS_ECO, PWRMAX_CF,       &
                                PROOT_LIN, PROOT_EXTINCTION, PSOILRC_SO2,           &
                                PSOILRC_O3, PRE25, PCE_NITRO, PCF_NITRO, PCNA_NITRO,&
                                PGMES_ST, PGC_ST, PBSLAI_ST, PSEFOLD_ST, PDMAX_ST  ,&
                                PGNDLITTER,PZF_TALLVEG, PRGLGV,PGAMMAGV,            &
                                PRSMINGV, PROOT_EXTINCTIONGV, PWRMAX_CFGV,          &
                                PH_VEG, PLAIGV_IN, PLAIGV_OUT, PZ0LITTER,           &
                                OAGRI_TO_GRASS                                      )
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
!!    R. Alkama      05/12 : Add 7 new vegtype (19 rather than 12)
!!    B. Decharme    05/13 : new param for equatorial forest
!!    P. Samuelsson  10/14 : Multi-energy balance (MEB)
!!Seferian & Delire  06/15 : Updating Nitrogen content and coef (PCF,PCNA) and 
!                            mesophyl conductance based on TRY database (Kattge et al., GCB 2011) and Jacobs Thesis
!!
!----------------------------------------------------------------------------
!
!*    0.     DECLARATION
!            -----------
!
USE MODD_CSTS,           ONLY : XDAY
USE MODD_DATA_COVER_PAR, ONLY : NVT_NO, NVT_ROCK, NVT_SNOW, NVT_TEBD,     & 
                                  NVT_BONE, NVT_TRBE, NVT_C3, NVT_C4,     &
                                  NVT_IRR, NVT_GRAS, NVT_TROG,NVT_PARK,   &
                                  NVT_TRBD, NVT_TEBE, NVT_TENE, NVT_BOBD, &
                                  NVT_BOND, NVT_BOGR, NVT_SHRB,           &
                                  NVEGTYPE, JPCOVER
!
USE MODD_REPROD_OPER,    ONLY : XEVERG_RSMIN
!
USE MODI_VEG_FROM_LAI
USE MODI_GREEN_FROM_LAI
USE MODI_Z0V_FROM_LAI
USE MODI_EMIS_FROM_VEG
USE MODI_ABOR1_SFX
USE MODI_VEG_HEIGHT_FROM_LAI
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
REAL, DIMENSION(:,:), INTENT(IN),  OPTIONAL :: PLAIMIN_IN
REAL, DIMENSION(:,:), INTENT(OUT), OPTIONAL :: PLAIMIN_OUT
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
LOGICAL, OPTIONAL, INTENT(IN) :: OAGRI_TO_GRASS
!
!            MEB parameters
!            --------------
REAL, DIMENSION(:,:), INTENT(OUT), OPTIONAL :: PZF_TALLVEG
REAL, DIMENSION(:,:), INTENT(OUT), OPTIONAL :: PRGLGV
REAL, DIMENSION(:,:), INTENT(OUT), OPTIONAL :: PGAMMAGV
REAL, DIMENSION(:,:), INTENT(OUT), OPTIONAL :: PRSMINGV
REAL, DIMENSION(:,:), INTENT(OUT), OPTIONAL :: PROOT_EXTINCTIONGV
REAL, DIMENSION(:,:), INTENT(OUT), OPTIONAL :: PWRMAX_CFGV
REAL, DIMENSION(:,:,:), INTENT(OUT), OPTIONAL :: PH_VEG
REAL, DIMENSION(:,:,:), INTENT(OUT), OPTIONAL :: PLAIGV_OUT
REAL, DIMENSION(:,:,:), INTENT(IN), OPTIONAL  :: PLAIGV_IN
REAL, DIMENSION(:,:,:), INTENT(OUT), OPTIONAL :: PZ0LITTER
REAL, DIMENSION(:,:,:), INTENT(OUT), OPTIONAL :: PGNDLITTER
!
!*    0.2    Declaration of local variables
!      ------------------------------
!
REAL, DIMENSION(SIZE(PTYPE,1)) :: ZGARDEN
LOGICAL            :: GSURF, GAGRI_TO_GRASS
REAL, DIMENSION(SIZE(PTYPE,1),NVEGTYPE) :: ZLAIFRGV
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
GSURF          = .TRUE.
GAGRI_TO_GRASS = .FALSE.
!
!When set, C3 and C4 crop values are replaced by value for C3 grass
IF(PRESENT(OAGRI_TO_GRASS))GAGRI_TO_GRASS=OAGRI_TO_GRASS
!
DO JLOOP=1,SIZE(PTYPE,1)
!  
  IF (PRESENT(PSURF2) .AND. PRESENT(PSURF)) THEN
    GSURF=(PSURF(JLOOP)>0. .OR. PSURF2(JLOOP)>0.)
  ELSEIF (PRESENT(PSURF)) THEN
    GSURF=(PSURF(JLOOP)>0.)
  ENDIF
!
!-------------------------------------------------------------------------------
!
!* nature exists for this cover type
!
  IF (GSURF) THEN
!
!-------------------------------------------------------------------------------
!*    7.5    albnir (veg only)
!            ------
    IF (PRESENT(PALBNIR_VEG)) THEN
      PALBNIR_VEG(JLOOP,:)= 0.30
      IF(PTYPE(JLOOP,NVT_TEBD)>0. )  PALBNIR_VEG(JLOOP,NVT_TEBD)= 0.25
      IF(PTYPE(JLOOP,NVT_TRBD)>0. )  PALBNIR_VEG(JLOOP,NVT_TRBD)= 0.25
      IF(PTYPE(JLOOP,NVT_TEBE)>0. )  PALBNIR_VEG(JLOOP,NVT_TEBE)= 0.25
      IF(PTYPE(JLOOP,NVT_BOBD)>0. )  PALBNIR_VEG(JLOOP,NVT_BOBD)= 0.25
      IF(PTYPE(JLOOP,NVT_SHRB)>0. )  PALBNIR_VEG(JLOOP,NVT_SHRB)= 0.25
      IF(PTYPE(JLOOP,NVT_BONE)>0. )  PALBNIR_VEG(JLOOP,NVT_BONE)= 0.15
      IF(PTYPE(JLOOP,NVT_TENE)>0. )  PALBNIR_VEG(JLOOP,NVT_TENE)= 0.15
      IF(PTYPE(JLOOP,NVT_BOND)>0. )  PALBNIR_VEG(JLOOP,NVT_BOND)= 0.15
      IF(PTYPE(JLOOP,NVT_TRBE)>0. )  PALBNIR_VEG(JLOOP,NVT_TRBE)= 0.21
    ENDIF
!-------------------------------------------------------------------------------
!*    7.6    albvis (veg only)
!            ------
    IF (PRESENT(PALBVIS_VEG)) THEN
      PALBVIS_VEG(JLOOP,:)= 0.10
      IF(PTYPE(JLOOP,NVT_TEBD)>0. )  PALBVIS_VEG(JLOOP,NVT_TEBD)= 0.05
      IF(PTYPE(JLOOP,NVT_TRBD)>0. )  PALBVIS_VEG(JLOOP,NVT_TRBD)= 0.05
      IF(PTYPE(JLOOP,NVT_TEBE)>0. )  PALBVIS_VEG(JLOOP,NVT_TEBE)= 0.05
      IF(PTYPE(JLOOP,NVT_BOBD)>0. )  PALBVIS_VEG(JLOOP,NVT_BOBD)= 0.05
      IF(PTYPE(JLOOP,NVT_SHRB)>0. )  PALBVIS_VEG(JLOOP,NVT_SHRB)= 0.05
      IF(PTYPE(JLOOP,NVT_BONE)>0. )  PALBVIS_VEG(JLOOP,NVT_BONE)= 0.05
      IF(PTYPE(JLOOP,NVT_TENE)>0. )  PALBVIS_VEG(JLOOP,NVT_TENE)= 0.05
      IF(PTYPE(JLOOP,NVT_BOND)>0. )  PALBVIS_VEG(JLOOP,NVT_BOND)= 0.05
      IF(PTYPE(JLOOP,NVT_TRBE)>0. )  PALBVIS_VEG(JLOOP,NVT_TRBE)= 0.05
    ENDIF        
!-------------------------------------------------------------------------------
!*    7.6    albUV (veg only)
!            -----
    IF (PRESENT(PALBUV_VEG)) THEN
      PALBUV_VEG(JLOOP,:)= 0.06  
      IF(PTYPE(JLOOP,NVT_TEBD)>0. )  PALBUV_VEG(JLOOP,NVT_TEBD)= 0.0525
      IF(PTYPE(JLOOP,NVT_TRBD)>0. )  PALBUV_VEG(JLOOP,NVT_TRBD)= 0.0525
      IF(PTYPE(JLOOP,NVT_TEBE)>0. )  PALBUV_VEG(JLOOP,NVT_TEBE)= 0.0525
      IF(PTYPE(JLOOP,NVT_BOBD)>0. )  PALBUV_VEG(JLOOP,NVT_BOBD)= 0.0525
      IF(PTYPE(JLOOP,NVT_SHRB)>0. )  PALBUV_VEG(JLOOP,NVT_SHRB)= 0.0525
      IF(PTYPE(JLOOP,NVT_BONE)>0. )  PALBUV_VEG(JLOOP,NVT_BONE)= 0.0425
      IF(PTYPE(JLOOP,NVT_TENE)>0. )  PALBUV_VEG(JLOOP,NVT_TENE)= 0.0425
      IF(PTYPE(JLOOP,NVT_BOND)>0. )  PALBUV_VEG(JLOOP,NVT_BOND)= 0.0425
      IF(PTYPE(JLOOP,NVT_TRBE)>0. )  PALBUV_VEG(JLOOP,NVT_TRBE)= 0.038 
      IF(PTYPE(JLOOP,NVT_GRAS)>0. )  PALBUV_VEG(JLOOP,NVT_GRAS)= 0.08
      IF(PTYPE(JLOOP,NVT_BOGR)>0. )  PALBUV_VEG(JLOOP,NVT_BOGR)= 0.08
      IF(PTYPE(JLOOP,NVT_TROG)>0. )  PALBUV_VEG(JLOOP,NVT_TROG)= 0.125
      IF(GAGRI_TO_GRASS)THEN
        IF(PTYPE(JLOOP,NVT_C3  )>0. )  PALBUV_VEG(JLOOP,NVT_C3  )= 0.08
        IF(PTYPE(JLOOP,NVT_C4  )>0. )  PALBUV_VEG(JLOOP,NVT_C4  )= 0.08
        IF(PTYPE(JLOOP,NVT_IRR )>0. )  PALBUV_VEG(JLOOP,NVT_IRR )= 0.08
      ELSE
        IF(PTYPE(JLOOP,NVT_IRR )>0. )  PALBUV_VEG(JLOOP,NVT_IRR )= 0.045
      ENDIF
      IF(PTYPE(JLOOP,NVT_PARK)>0. )  PALBUV_VEG(JLOOP,NVT_PARK)= 0.08
    ENDIF        
!------------------------------------------------------------------------------
!*    7.7    Rsmin
!            -----
    IF (PRESENT(PRSMIN)) THEN
      PRSMIN(JLOOP,:)= 40.
      IF(PTYPE(JLOOP,NVT_TEBD)>0. )  PRSMIN(JLOOP,NVT_TEBD)= 150.
      IF(PTYPE(JLOOP,NVT_TRBD)>0. )  PRSMIN(JLOOP,NVT_TRBD)= 150.
      IF(PTYPE(JLOOP,NVT_TEBE)>0. )  PRSMIN(JLOOP,NVT_TEBE)= 150.
      IF(PTYPE(JLOOP,NVT_BOBD)>0. )  PRSMIN(JLOOP,NVT_BOBD)= 150.
      IF(PTYPE(JLOOP,NVT_SHRB)>0. )  PRSMIN(JLOOP,NVT_SHRB)= 150.
      IF(PTYPE(JLOOP,NVT_BONE)>0. )  PRSMIN(JLOOP,NVT_BONE)= 150.
      IF(PTYPE(JLOOP,NVT_TENE)>0. )  PRSMIN(JLOOP,NVT_TENE)= 150.
      IF(PTYPE(JLOOP,NVT_BOND)>0. )  PRSMIN(JLOOP,NVT_BOND)= 150.
      IF(PTYPE(JLOOP,NVT_TRBE)>0. )  PRSMIN(JLOOP,NVT_TRBE)= XEVERG_RSMIN
      IF(PTYPE(JLOOP,NVT_TROG)>0. )  PRSMIN(JLOOP,NVT_TROG)= 120.
      IF(GAGRI_TO_GRASS)THEN
        IF(PTYPE(JLOOP,NVT_C4  )>0. )  PRSMIN(JLOOP,NVT_C4  )= 40.
        IF(PTYPE(JLOOP,NVT_IRR )>0. )  PRSMIN(JLOOP,NVT_IRR )= 40.
      ELSE
        IF(PTYPE(JLOOP,NVT_C4  )>0. )  PRSMIN(JLOOP,NVT_C4  )= 120.
        IF(PTYPE(JLOOP,NVT_IRR )>0. )  PRSMIN(JLOOP,NVT_IRR )= 120.
      ENDIF
    ENDIF
!-------------------------------------------------------------------------------
!*    7.8    Gamma
!            -----
    IF (PRESENT(PGAMMA)) THEN
      PGAMMA(JLOOP,:)= 0. 
      IF(PTYPE(JLOOP,NVT_TEBD)>0. )  PGAMMA(JLOOP,NVT_TEBD)= 0.04
      IF(PTYPE(JLOOP,NVT_TRBD)>0. )  PGAMMA(JLOOP,NVT_TRBD)= 0.04
      IF(PTYPE(JLOOP,NVT_TEBE)>0. )  PGAMMA(JLOOP,NVT_TEBE)= 0.04
      IF(PTYPE(JLOOP,NVT_BOBD)>0. )  PGAMMA(JLOOP,NVT_BOBD)= 0.04
      IF(PTYPE(JLOOP,NVT_SHRB)>0. )  PGAMMA(JLOOP,NVT_SHRB)= 0.04
      IF(PTYPE(JLOOP,NVT_BONE)>0. )  PGAMMA(JLOOP,NVT_BONE)= 0.04
      IF(PTYPE(JLOOP,NVT_TENE)>0. )  PGAMMA(JLOOP,NVT_TENE)= 0.04
      IF(PTYPE(JLOOP,NVT_BOND)>0. )  PGAMMA(JLOOP,NVT_BOND)= 0.04
      IF(PTYPE(JLOOP,NVT_TRBE)>0. )  PGAMMA(JLOOP,NVT_TRBE)= 0.04
    ENDIF
!-------------------------------------------------------------------------------
!*    7.8    Wrmax_cf
!            --------
    IF (PRESENT(PWRMAX_CF)) THEN
      PWRMAX_CF(JLOOP,:)= 0.2
      IF(PTYPE(JLOOP,NVT_TEBD)>0. )  PWRMAX_CF(JLOOP,NVT_TEBD)= 0.1
      IF(PTYPE(JLOOP,NVT_TRBD)>0. )  PWRMAX_CF(JLOOP,NVT_TRBD)= 0.1
      IF(PTYPE(JLOOP,NVT_TEBE)>0. )  PWRMAX_CF(JLOOP,NVT_TEBE)= 0.1
      IF(PTYPE(JLOOP,NVT_BOBD)>0. )  PWRMAX_CF(JLOOP,NVT_BOBD)= 0.1
      IF(PTYPE(JLOOP,NVT_SHRB)>0. )  PWRMAX_CF(JLOOP,NVT_SHRB)= 0.1
      IF(PTYPE(JLOOP,NVT_BONE)>0. )  PWRMAX_CF(JLOOP,NVT_BONE)= 0.1
      IF(PTYPE(JLOOP,NVT_TENE)>0. )  PWRMAX_CF(JLOOP,NVT_TENE)= 0.1
      IF(PTYPE(JLOOP,NVT_BOND)>0. )  PWRMAX_CF(JLOOP,NVT_BOND)= 0.1
      IF(PTYPE(JLOOP,NVT_TRBE)>0. )  PWRMAX_CF(JLOOP,NVT_TRBE)= 0.1
    ENDIF
!-------------------------------------------------------------------------------
!*    7.9    Rgl
!            ---
    IF (PRESENT(PRGL)) THEN
      PRGL(JLOOP,:)= 100.
      IF(PTYPE(JLOOP,NVT_TEBD)>0. )  PRGL(JLOOP,NVT_TEBD)= 30.
      IF(PTYPE(JLOOP,NVT_TRBD)>0. )  PRGL(JLOOP,NVT_TRBD)= 30.
      IF(PTYPE(JLOOP,NVT_TEBE)>0. )  PRGL(JLOOP,NVT_TEBE)= 30.
      IF(PTYPE(JLOOP,NVT_BOBD)>0. )  PRGL(JLOOP,NVT_BOBD)= 30.
      IF(PTYPE(JLOOP,NVT_SHRB)>0. )  PRGL(JLOOP,NVT_SHRB)= 30.
      IF(PTYPE(JLOOP,NVT_BONE)>0. )  PRGL(JLOOP,NVT_BONE)= 30.
      IF(PTYPE(JLOOP,NVT_TENE)>0. )  PRGL(JLOOP,NVT_TENE)= 30.
      IF(PTYPE(JLOOP,NVT_BOND)>0. )  PRGL(JLOOP,NVT_BOND)= 30.
      IF(PTYPE(JLOOP,NVT_TRBE)>0. )  PRGL(JLOOP,NVT_TRBE)= 30.
    ENDIF    
!-------------------------------------------------------------------------------
!*    7.10   Cv
!            --
    IF (PRESENT(PCV)) THEN
      PCV(JLOOP,:)=2.E-5
      IF(PTYPE(JLOOP,NVT_TEBD)>0. )  PCV(JLOOP,NVT_TEBD)= 1.E-5
      IF(PTYPE(JLOOP,NVT_TRBD)>0. )  PCV(JLOOP,NVT_TRBD)= 1.E-5
      IF(PTYPE(JLOOP,NVT_TEBE)>0. )  PCV(JLOOP,NVT_TEBE)= 1.E-5
      IF(PTYPE(JLOOP,NVT_BOBD)>0. )  PCV(JLOOP,NVT_BOBD)= 1.E-5
      IF(PTYPE(JLOOP,NVT_SHRB)>0. )  PCV(JLOOP,NVT_SHRB)= 1.E-5
      IF(PTYPE(JLOOP,NVT_BONE)>0. )  PCV(JLOOP,NVT_BONE)= 1.E-5
      IF(PTYPE(JLOOP,NVT_TENE)>0. )  PCV(JLOOP,NVT_TENE)= 1.E-5
      IF(PTYPE(JLOOP,NVT_BOND)>0. )  PCV(JLOOP,NVT_BOND)= 1.E-5
      IF(PTYPE(JLOOP,NVT_TRBE)>0. )  PCV(JLOOP,NVT_TRBE)= 1.E-5 
    ENDIF    
!-------------------------------------------------------------------------------
!*    7.11   mesophyll conductance (m s-1)
!            -----------------------------
!            Uptdated values using Kattge et al. 2009 median values of Vcmax at 25C
!            (For TRBE, used median + 1 standard deviation)
!            For C3 tree PFTs : 
!              gmes = Vcmax / (gamma + Kc*(1 + O2/Ko))    
!              from Jacobs eq [A8.5] and Farquhar, 1980 eq 42 : gm = dA/dC estimated at Ci=Gamma 
!            For grasses (C3 and C4): used V7 value
!                crops :  used N. Canal's PhD thesis   
!            --------------------------------------------------------------------
    IF (PRESENT(PGMES)) THEN
      PGMES(JLOOP,:)=0.020
      IF(PTYPE(JLOOP,NVT_TEBD)>0. )  PGMES(JLOOP,NVT_TEBD)= 0.001
      IF(PTYPE(JLOOP,NVT_TRBD)>0. )  PGMES(JLOOP,NVT_TRBD)= 0.001
      IF(PTYPE(JLOOP,NVT_TEBE)>0. )  PGMES(JLOOP,NVT_TEBE)= 0.001
      IF(PTYPE(JLOOP,NVT_BOBD)>0. )  PGMES(JLOOP,NVT_BOBD)= 0.001
      IF(PTYPE(JLOOP,NVT_SHRB)>0. )  PGMES(JLOOP,NVT_SHRB)= 0.001
      IF(PTYPE(JLOOP,NVT_BONE)>0. )  PGMES(JLOOP,NVT_BONE)= 0.001
      IF(PTYPE(JLOOP,NVT_TENE)>0. )  PGMES(JLOOP,NVT_TENE)= 0.001
      IF(PTYPE(JLOOP,NVT_BOND)>0. )  PGMES(JLOOP,NVT_BOND)= 0.001
      IF(PTYPE(JLOOP,NVT_TRBE)>0. )  PGMES(JLOOP,NVT_TRBE)= 0.001 
      IF(GAGRI_TO_GRASS)THEN
        IF(PTYPE(JLOOP,NVT_C3  )>0. )  PGMES(JLOOP,NVT_C3  )= 0.020
        IF(PTYPE(JLOOP,NVT_C4  )>0. )  PGMES(JLOOP,NVT_C4  )= 0.020
        IF(PTYPE(JLOOP,NVT_IRR )>0. )  PGMES(JLOOP,NVT_IRR )= 0.020
      ELSE
        IF(PTYPE(JLOOP,NVT_C3  )>0. )  PGMES(JLOOP,NVT_C3  )= 0.003
        IF(PTYPE(JLOOP,NVT_C4  )>0. )  PGMES(JLOOP,NVT_C4  )= 0.003
        IF(PTYPE(JLOOP,NVT_IRR )>0. )  PGMES(JLOOP,NVT_IRR )= 0.003
      ENDIF
    ENDIF    
!
    IF (PRESENT(PGMES_ST)) THEN
      PGMES_ST(JLOOP,:)=0.003
      IF(PTYPE(JLOOP,NVT_TEBD)>0. )  PGMES_ST(JLOOP,NVT_TEBD)= 0.0018
      IF(PTYPE(JLOOP,NVT_TRBD)>0. )  PGMES_ST(JLOOP,NVT_TRBD)= 0.0012
      IF(PTYPE(JLOOP,NVT_TEBE)>0. )  PGMES_ST(JLOOP,NVT_TEBE)= 0.0019
      IF(PTYPE(JLOOP,NVT_BOBD)>0. )  PGMES_ST(JLOOP,NVT_BOBD)= 0.0018
      IF(PTYPE(JLOOP,NVT_SHRB)>0. )  PGMES_ST(JLOOP,NVT_SHRB)= 0.0016
      IF(PTYPE(JLOOP,NVT_BONE)>0. )  PGMES_ST(JLOOP,NVT_BONE)= 0.0019
      IF(PTYPE(JLOOP,NVT_TENE)>0. )  PGMES_ST(JLOOP,NVT_TENE)= 0.0019
      IF(PTYPE(JLOOP,NVT_BOND)>0. )  PGMES_ST(JLOOP,NVT_BOND)= 0.0012
      IF(PTYPE(JLOOP,NVT_TRBE)>0. )  PGMES_ST(JLOOP,NVT_TRBE)= 0.0012
      IF(GAGRI_TO_GRASS)THEN
        IF(PTYPE(JLOOP,NVT_C3  )>0. )  PGMES_ST(JLOOP,NVT_C3  )= 0.001
        IF(PTYPE(JLOOP,NVT_C4  )>0. )  PGMES_ST(JLOOP,NVT_C4  )= 0.006
        IF(PTYPE(JLOOP,NVT_IRR )>0. )  PGMES_ST(JLOOP,NVT_IRR )= 0.006
      ELSE
        IF(PTYPE(JLOOP,NVT_C3  )>0. )  PGMES_ST(JLOOP,NVT_C3  )= 0.00175
        IF(PTYPE(JLOOP,NVT_C4  )>0. )  PGMES_ST(JLOOP,NVT_C4  )= 0.0098
        IF(PTYPE(JLOOP,NVT_IRR )>0. )  PGMES_ST(JLOOP,NVT_IRR )= 0.0098
      ENDIF
      IF(PTYPE(JLOOP,NVT_GRAS)>0. ) PGMES_ST(JLOOP,NVT_GRAS )= 0.001
      IF(PTYPE(JLOOP,NVT_BOGR)>0. ) PGMES_ST(JLOOP,NVT_BOGR )= 0.001
      IF(PTYPE(JLOOP,NVT_TROG)>0. ) PGMES_ST(JLOOP,NVT_TROG )= 0.006
      IF(PTYPE(JLOOP,NVT_PARK)>0. ) PGMES_ST(JLOOP,NVT_PARK )= 0.001
    ENDIF    
!-------------------------------------------------------------------------------
!*    7.11   Ecosystem Respiration (kg m-2 s-1)
!            -----------------------------------
    IF (PRESENT(PRE25)) THEN
      PRE25(JLOOP,:)= 3.6E-7
      IF(PTYPE(JLOOP,NVT_BONE)>0. )  PRE25(JLOOP,NVT_BONE)= 1.8E-7
      IF(PTYPE(JLOOP,NVT_TENE)>0. )  PRE25(JLOOP,NVT_TENE)= 1.8E-7
      IF(PTYPE(JLOOP,NVT_BOND)>0. )  PRE25(JLOOP,NVT_BOND)= 1.8E-7
      IF(GAGRI_TO_GRASS)THEN
        IF(PTYPE(JLOOP,NVT_C3  )>0. )  PRE25(JLOOP,NVT_C3  )= 3.6E-7
        IF(PTYPE(JLOOP,NVT_C4  )>0. )  PRE25(JLOOP,NVT_C4  )= 3.6E-7
        IF(PTYPE(JLOOP,NVT_IRR )>0. )  PRE25(JLOOP,NVT_IRR )= 3.6E-7              
      ELSE
        IF(PTYPE(JLOOP,NVT_C3  )>0. )  PRE25(JLOOP,NVT_C3  )= 3.6E-7
        IF(PTYPE(JLOOP,NVT_C4  )>0. )  PRE25(JLOOP,NVT_C4  )= 3.0E-7
        IF(PTYPE(JLOOP,NVT_IRR )>0. )  PRE25(JLOOP,NVT_IRR )= 3.0E-7
      ENDIF
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
      IF(PTYPE(JLOOP,NVT_TEBD)>0. )  PGC(JLOOP,NVT_TEBD)= 0.00015
      IF(PTYPE(JLOOP,NVT_TRBD)>0. )  PGC(JLOOP,NVT_TRBD)= 0.00015
      IF(PTYPE(JLOOP,NVT_TEBE)>0. )  PGC(JLOOP,NVT_TEBE)= 0.00015
      IF(PTYPE(JLOOP,NVT_BOBD)>0. )  PGC(JLOOP,NVT_BOBD)= 0.00015
      IF(PTYPE(JLOOP,NVT_SHRB)>0. )  PGC(JLOOP,NVT_SHRB)= 0.00015
      IF(PTYPE(JLOOP,NVT_BONE)>0. )  PGC(JLOOP,NVT_BONE)= 0.
      IF(PTYPE(JLOOP,NVT_TENE)>0. )  PGC(JLOOP,NVT_TENE)= 0.
      IF(PTYPE(JLOOP,NVT_BOND)>0. )  PGC(JLOOP,NVT_BOND)= 0.
      IF(PTYPE(JLOOP,NVT_TRBE)>0. )  PGC(JLOOP,NVT_TRBE)= 0.00015      
    ENDIF
!
    IF (PRESENT(PGC_ST)) THEN
      PGC_ST(JLOOP,:)=0.00015
      IF(PTYPE(JLOOP,NVT_BONE)>0. )  PGC_ST(JLOOP,NVT_BONE)= 0.
      IF(PTYPE(JLOOP,NVT_TENE)>0. )  PGC_ST(JLOOP,NVT_TENE)= 0.
      IF(PTYPE(JLOOP,NVT_BOND)>0. )  PGC_ST(JLOOP,NVT_BOND)= 0.
      IF(PTYPE(JLOOP,NVT_C3  )>0. )  PGC_ST(JLOOP,NVT_C3  )= 0.00025
      IF(PTYPE(JLOOP,NVT_GRAS)>0. )  PGC_ST(JLOOP,NVT_GRAS)= 0.00025
      IF(PTYPE(JLOOP,NVT_BOGR)>0. )  PGC_ST(JLOOP,NVT_BOGR)= 0.00025      
      IF(PTYPE(JLOOP,NVT_PARK)>0. )  PGC_ST(JLOOP,NVT_PARK)= 0.001
      IF(GAGRI_TO_GRASS)THEN
        IF(PTYPE(JLOOP,NVT_C4  )>0. )  PGC_ST(JLOOP,NVT_C4  )= 0.00025
        IF(PTYPE(JLOOP,NVT_IRR )>0. )  PGC_ST(JLOOP,NVT_IRR )= 0.00025
      ENDIF
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
      IF(PTYPE(JLOOP,NVT_TEBD)>0. )  PBSLAI(JLOOP,NVT_TEBD)= 0.25
      IF(PTYPE(JLOOP,NVT_TRBD)>0. )  PBSLAI(JLOOP,NVT_TRBD)= 0.25
      IF(PTYPE(JLOOP,NVT_TEBE)>0. )  PBSLAI(JLOOP,NVT_TEBE)= 0.25
      IF(PTYPE(JLOOP,NVT_BOBD)>0. )  PBSLAI(JLOOP,NVT_BOBD)= 0.25
      IF(PTYPE(JLOOP,NVT_SHRB)>0. )  PBSLAI(JLOOP,NVT_SHRB)= 0.25
      IF(PTYPE(JLOOP,NVT_BONE)>0. )  PBSLAI(JLOOP,NVT_BONE)= 0.25
      IF(PTYPE(JLOOP,NVT_TENE)>0. )  PBSLAI(JLOOP,NVT_TENE)= 0.25
      IF(PTYPE(JLOOP,NVT_BOND)>0. )  PBSLAI(JLOOP,NVT_BOND)= 0.25
      IF(PTYPE(JLOOP,NVT_TRBE)>0. )  PBSLAI(JLOOP,NVT_TRBE)= 0.25 
      IF(GAGRI_TO_GRASS)THEN
        IF(PTYPE(JLOOP,NVT_C3  )>0. )  PBSLAI(JLOOP,NVT_C3  )= 0.36
        IF(PTYPE(JLOOP,NVT_C4  )>0. )  PBSLAI(JLOOP,NVT_C4  )= 0.36
        IF(PTYPE(JLOOP,NVT_IRR )>0. )  PBSLAI(JLOOP,NVT_IRR )= 0.36              
      ELSE
        IF(PTYPE(JLOOP,NVT_C3  )>0. )  PBSLAI(JLOOP,NVT_C3  )= 0.06
        IF(PTYPE(JLOOP,NVT_C4  )>0. )  PBSLAI(JLOOP,NVT_C4  )= 0.06
        IF(PTYPE(JLOOP,NVT_IRR )>0. )  PBSLAI(JLOOP,NVT_IRR )= 0.06 
      ENDIF
    ENDIF
!    
    IF (PRESENT(PBSLAI_ST)) THEN
      PBSLAI_ST(JLOOP,:)=0.08 
      IF(PTYPE(JLOOP,NVT_TEBD)>0. )  PBSLAI_ST(JLOOP,NVT_TEBD)= 0.125
      IF(PTYPE(JLOOP,NVT_TRBD)>0. )  PBSLAI_ST(JLOOP,NVT_TRBD)= 0.125
      IF(PTYPE(JLOOP,NVT_TEBE)>0. )  PBSLAI_ST(JLOOP,NVT_TEBE)= 0.125
      IF(PTYPE(JLOOP,NVT_BOBD)>0. )  PBSLAI_ST(JLOOP,NVT_BOBD)= 0.125
      IF(PTYPE(JLOOP,NVT_SHRB)>0. )  PBSLAI_ST(JLOOP,NVT_SHRB)= 0.125
      IF(PTYPE(JLOOP,NVT_BONE)>0. )  PBSLAI_ST(JLOOP,NVT_BONE)= 0.50
      IF(PTYPE(JLOOP,NVT_TENE)>0. )  PBSLAI_ST(JLOOP,NVT_TENE)= 0.50
      IF(PTYPE(JLOOP,NVT_BOND)>0. )  PBSLAI_ST(JLOOP,NVT_BOND)= 0.50
      IF(PTYPE(JLOOP,NVT_TRBE)>0. )  PBSLAI_ST(JLOOP,NVT_TRBE)= 0.25 
      IF(GAGRI_TO_GRASS)THEN
        IF(PTYPE(JLOOP,NVT_C3  )>0. )  PBSLAI_ST(JLOOP,NVT_C3  )= 0.08
        IF(PTYPE(JLOOP,NVT_C4  )>0. )  PBSLAI_ST(JLOOP,NVT_C4  )= 0.08
        IF(PTYPE(JLOOP,NVT_IRR )>0. )  PBSLAI_ST(JLOOP,NVT_IRR )= 0.08
      ELSE
        IF(PTYPE(JLOOP,NVT_C3  )>0. )  PBSLAI_ST(JLOOP,NVT_C3  )= 0.06
        IF(PTYPE(JLOOP,NVT_C4  )>0. )  PBSLAI_ST(JLOOP,NVT_C4  )= 0.06
        IF(PTYPE(JLOOP,NVT_IRR )>0. )  PBSLAI_ST(JLOOP,NVT_IRR )= 0.06
      ENDIF
    ENDIF
!
!-------------------------------------------------------------------------------
!*    7.12   maximum air saturation deficit tolerate by vegetation (kg/kg)
!            -----------------------------------------------------
    IF (PRESENT(PDMAX)) THEN
      PDMAX(JLOOP,:) = 0.1
      IF(PTYPE(JLOOP,NVT_TEBD)>0. )  PDMAX(JLOOP,NVT_TEBD)= 0.01
      IF(PTYPE(JLOOP,NVT_TRBD)>0. )  PDMAX(JLOOP,NVT_TRBD)= 0.01
      IF(PTYPE(JLOOP,NVT_TEBE)>0. )  PDMAX(JLOOP,NVT_TEBE)= 0.01
      IF(PTYPE(JLOOP,NVT_BOBD)>0. )  PDMAX(JLOOP,NVT_BOBD)= 0.01
      IF(PTYPE(JLOOP,NVT_SHRB)>0. )  PDMAX(JLOOP,NVT_SHRB)= 0.01
      IF(PTYPE(JLOOP,NVT_BONE)>0. )  PDMAX(JLOOP,NVT_BONE)= 0.01
      IF(PTYPE(JLOOP,NVT_TENE)>0. )  PDMAX(JLOOP,NVT_TENE)= 0.01
      IF(PTYPE(JLOOP,NVT_BOND)>0. )  PDMAX(JLOOP,NVT_BOND)= 0.01
      IF(PTYPE(JLOOP,NVT_TRBE)>0. )  PDMAX(JLOOP,NVT_TRBE)= 0.01 
    ENDIF    
!
    IF (PRESENT(PDMAX_ST)) THEN
      PDMAX_ST(JLOOP,:) = 0.05
      IF(PTYPE(JLOOP,NVT_TEBD)>0. )  PDMAX_ST(JLOOP,NVT_TEBD)= 0.109
      IF(PTYPE(JLOOP,NVT_TRBD)>0. )  PDMAX_ST(JLOOP,NVT_TRBD)= 0.109
      IF(PTYPE(JLOOP,NVT_TEBE)>0. )  PDMAX_ST(JLOOP,NVT_TEBE)= 0.109
      IF(PTYPE(JLOOP,NVT_BOBD)>0. )  PDMAX_ST(JLOOP,NVT_BOBD)= 0.109
      IF(PTYPE(JLOOP,NVT_SHRB)>0. )  PDMAX_ST(JLOOP,NVT_SHRB)= 0.109
      IF(PTYPE(JLOOP,NVT_BONE)>0. )  PDMAX_ST(JLOOP,NVT_BONE)= 0.124
      IF(PTYPE(JLOOP,NVT_TENE)>0. )  PDMAX_ST(JLOOP,NVT_TENE)= 0.124
      IF(PTYPE(JLOOP,NVT_BOND)>0. )  PDMAX_ST(JLOOP,NVT_BOND)= 0.124
      IF(PTYPE(JLOOP,NVT_TRBE)>0. )  PDMAX_ST(JLOOP,NVT_TRBE)= 0.124 
      IF(GAGRI_TO_GRASS)THEN
        IF(PTYPE(JLOOP,NVT_C4  )>0. )  PDMAX_ST(JLOOP,NVT_C4  )= 0.05
        IF(PTYPE(JLOOP,NVT_IRR )>0. )  PDMAX_ST(JLOOP,NVT_IRR )= 0.05
      ELSE
        IF(PTYPE(JLOOP,NVT_C4  )>0. )  PDMAX_ST(JLOOP,NVT_C4  )= 0.033
        IF(PTYPE(JLOOP,NVT_IRR )>0. )  PDMAX_ST(JLOOP,NVT_IRR )= 0.033
      ENDIF    
      IF(PTYPE(JLOOP,NVT_TROG)>0. )  PDMAX_ST(JLOOP,NVT_TROG)= 0.052
    ENDIF    
!-------------------------------------------------------------------------------
!*    7.12   Defensive/offensive strategy (1/0)
!            ----------------------------
    IF (PRESENT(PSTRESS)) THEN
      PSTRESS(JLOOP,:) = 1. 
      IF(PTYPE(JLOOP,NVT_TEBD)>0. )  PSTRESS(JLOOP,NVT_TEBD)= 0.
      IF(PTYPE(JLOOP,NVT_TRBD)>0. )  PSTRESS(JLOOP,NVT_TRBD)= 0.
      IF(PTYPE(JLOOP,NVT_TEBE)>0. )  PSTRESS(JLOOP,NVT_TEBE)= 0.
      IF(PTYPE(JLOOP,NVT_BOBD)>0. )  PSTRESS(JLOOP,NVT_BOBD)= 0.
      IF(PTYPE(JLOOP,NVT_SHRB)>0. )  PSTRESS(JLOOP,NVT_SHRB)= 0.
      IF(PTYPE(JLOOP,NVT_TRBE)>0. )  PSTRESS(JLOOP,NVT_TRBE)= 0.
      IF(GAGRI_TO_GRASS)THEN
        IF(PTYPE(JLOOP,NVT_C3  )>0. )  PSTRESS(JLOOP,NVT_C3  )= 0.
        IF(PTYPE(JLOOP,NVT_IRR )>0. )  PSTRESS(JLOOP,NVT_IRR )= 0.
      ENDIF    
      IF(PTYPE(JLOOP,NVT_C4  )>0. )  PSTRESS(JLOOP,NVT_C4  )= 0.
      IF(PTYPE(JLOOP,NVT_GRAS)>0. )  PSTRESS(JLOOP,NVT_GRAS)= 0.
      IF(PTYPE(JLOOP,NVT_BOGR)>0. )  PSTRESS(JLOOP,NVT_BOGR)= 0.      
      IF(PTYPE(JLOOP,NVT_TROG)>0. )  PSTRESS(JLOOP,NVT_TROG)= 0.
      IF(PTYPE(JLOOP,NVT_PARK)>0. )  PSTRESS(JLOOP,NVT_PARK)= 0.
    ENDIF    
!-------------------------------------------------------------------------------
!*    7.13   e-folding time for senescence (days)
!            ------------------------------------
! parameters use in case HPHOTO == 'NONE' 'AGS' 'LAI'
    IF (PRESENT(PSEFOLD)) THEN
      PSEFOLD(JLOOP,:)=90. * XDAY
      IF(PTYPE(JLOOP,NVT_TEBD)>0. )  PSEFOLD(JLOOP,NVT_TEBD)= 365.* XDAY
      IF(PTYPE(JLOOP,NVT_TRBD)>0. )  PSEFOLD(JLOOP,NVT_TRBD)= 365.* XDAY
      IF(PTYPE(JLOOP,NVT_TEBE)>0. )  PSEFOLD(JLOOP,NVT_TEBE)= 365.* XDAY
      IF(PTYPE(JLOOP,NVT_BOBD)>0. )  PSEFOLD(JLOOP,NVT_BOBD)= 365.* XDAY
      IF(PTYPE(JLOOP,NVT_SHRB)>0. )  PSEFOLD(JLOOP,NVT_SHRB)= 365.* XDAY
      IF(PTYPE(JLOOP,NVT_BONE)>0. )  PSEFOLD(JLOOP,NVT_BONE)= 365.* XDAY
      IF(PTYPE(JLOOP,NVT_TENE)>0. )  PSEFOLD(JLOOP,NVT_TENE)= 365.* XDAY
      IF(PTYPE(JLOOP,NVT_BOND)>0. )  PSEFOLD(JLOOP,NVT_BOND)= 365.* XDAY
      IF(PTYPE(JLOOP,NVT_TRBE)>0. )  PSEFOLD(JLOOP,NVT_TRBE)= 365.* XDAY
      IF(GAGRI_TO_GRASS)THEN
        IF(PTYPE(JLOOP,NVT_C3  )>0. )  PSEFOLD(JLOOP,NVT_C3  )= 90.* XDAY
        IF(PTYPE(JLOOP,NVT_C4  )>0. )  PSEFOLD(JLOOP,NVT_C4  )= 90.* XDAY
        IF(PTYPE(JLOOP,NVT_IRR )>0. )  PSEFOLD(JLOOP,NVT_IRR )= 90.* XDAY
      ELSE
        IF(PTYPE(JLOOP,NVT_C3  )>0. )  PSEFOLD(JLOOP,NVT_C3  )= 60.* XDAY
        IF(PTYPE(JLOOP,NVT_C4  )>0. )  PSEFOLD(JLOOP,NVT_C4  )= 60.* XDAY
        IF(PTYPE(JLOOP,NVT_IRR )>0. )  PSEFOLD(JLOOP,NVT_IRR )= 60.* XDAY
      ENDIF    
    ENDIF    
!
! parameters use in case HPHOTO == 'AST','LST' 'NIT', 'NCB'

    IF (PRESENT(PSEFOLD_ST)) THEN
      PSEFOLD_ST(JLOOP,:)=150. * XDAY
      IF(PTYPE(JLOOP,NVT_TEBD)>0. )  PSEFOLD_ST(JLOOP,NVT_TEBD)= 230.* XDAY
      IF(PTYPE(JLOOP,NVT_TRBD)>0. )  PSEFOLD_ST(JLOOP,NVT_TRBD)= 230.* XDAY
      IF(PTYPE(JLOOP,NVT_TEBE)>0. )  PSEFOLD_ST(JLOOP,NVT_TEBE)= 230.* XDAY
      IF(PTYPE(JLOOP,NVT_BOBD)>0. )  PSEFOLD_ST(JLOOP,NVT_BOBD)= 230.* XDAY
      IF(PTYPE(JLOOP,NVT_SHRB)>0. )  PSEFOLD_ST(JLOOP,NVT_SHRB)= 230.* XDAY
      IF(PTYPE(JLOOP,NVT_BONE)>0. )  PSEFOLD_ST(JLOOP,NVT_BONE)= 365.* XDAY
      IF(PTYPE(JLOOP,NVT_TENE)>0. )  PSEFOLD_ST(JLOOP,NVT_TENE)= 365.* XDAY
      IF(PTYPE(JLOOP,NVT_BOND)>0. )  PSEFOLD_ST(JLOOP,NVT_BOND)= 365.* XDAY
      IF(PTYPE(JLOOP,NVT_TRBE)>0. )  PSEFOLD_ST(JLOOP,NVT_TRBE)= 365.* XDAY
    ENDIF    
!-------------------------------------------------------------------------------
!*    7.14   Minimum LAI (m2/m2)
!            -------------------
! Modi lai/patch defined
    IF (PRESENT(PLAIMIN_OUT)) THEN
      PLAIMIN_OUT (JLOOP,:) = 0.3
      IF(PTYPE(JLOOP,NVT_BONE)>0. )  PLAIMIN_OUT(JLOOP,NVT_BONE)= 1.0
      IF(PTYPE(JLOOP,NVT_TENE)>0. )  PLAIMIN_OUT(JLOOP,NVT_TENE)= 1.0
      IF(PTYPE(JLOOP,NVT_BOND)>0. )  PLAIMIN_OUT(JLOOP,NVT_BOND)= 1.0
      IF(PTYPE(JLOOP,NVT_TRBE)>0. )  PLAIMIN_OUT(JLOOP,NVT_TRBE)= 1.0
    ENDIF 
!--------------------------------------------------------------------
!
!*    7.16   Thickness of ground litter
!            --------------------------
! 	 
    IF (PRESENT(PGNDLITTER)) THEN
      PGNDLITTER (JLOOP,:,:) = 0.
      IF(PTYPE(JLOOP,NVT_TEBD)>0. )  PGNDLITTER (JLOOP,:,NVT_TEBD) = 0.03
      IF(PTYPE(JLOOP,NVT_BONE)>0. )  PGNDLITTER (JLOOP,:,NVT_BONE) = 0.03
      IF(PTYPE(JLOOP,NVT_TRBE)>0. )  PGNDLITTER (JLOOP,:,NVT_TRBE) = 0.03
      IF(PTYPE(JLOOP,NVT_TRBD)>0. )  PGNDLITTER (JLOOP,:,NVT_TRBD) = 0.03
      IF(PTYPE(JLOOP,NVT_TEBE)>0. )  PGNDLITTER (JLOOP,:,NVT_TEBE) = 0.03
      IF(PTYPE(JLOOP,NVT_TENE)>0. )  PGNDLITTER (JLOOP,:,NVT_TENE) = 0.03
      IF(PTYPE(JLOOP,NVT_BOBD)>0. )  PGNDLITTER (JLOOP,:,NVT_BOBD) = 0.03
      IF(PTYPE(JLOOP,NVT_BOND)>0. )  PGNDLITTER (JLOOP,:,NVT_BOND) = 0.03
      IF(PTYPE(JLOOP,NVT_SHRB)>0. )  PGNDLITTER (JLOOP,:,NVT_SHRB) = 0.03
    ENDIF
!-------------------------------------------------------------------------------
!*    7.16   Binary for tall vegetation
!            --------------------------------------------------------------------------
! parameters use in case LMEB == .TRUE.
!
    IF (PRESENT(PZF_TALLVEG)) THEN
      PZF_TALLVEG (JLOOP,:) = 0.
      IF(PTYPE(JLOOP,NVT_TEBD)>0. )  PZF_TALLVEG (JLOOP,NVT_TEBD) = 1.0
      IF(PTYPE(JLOOP,NVT_BONE)>0. )  PZF_TALLVEG (JLOOP,NVT_BONE) = 1.0
      IF(PTYPE(JLOOP,NVT_TRBE)>0. )  PZF_TALLVEG (JLOOP,NVT_TRBE) = 1.0
      IF(PTYPE(JLOOP,NVT_TRBD)>0. )  PZF_TALLVEG (JLOOP,NVT_TRBD) = 1.0
      IF(PTYPE(JLOOP,NVT_TEBE)>0. )  PZF_TALLVEG (JLOOP,NVT_TEBE) = 1.0
      IF(PTYPE(JLOOP,NVT_TENE)>0. )  PZF_TALLVEG (JLOOP,NVT_TENE) = 1.0
      IF(PTYPE(JLOOP,NVT_BOBD)>0. )  PZF_TALLVEG (JLOOP,NVT_BOBD) = 1.0
      IF(PTYPE(JLOOP,NVT_BOND)>0. )  PZF_TALLVEG (JLOOP,NVT_BOND) = 1.0
      IF(PTYPE(JLOOP,NVT_SHRB)>0. )  PZF_TALLVEG (JLOOP,NVT_SHRB) = 1.0
    ENDIF   
!
!-------------------------------------------------------------------------------
!
!*    7.17    Rgl for understory vegetation
!             ------------------------------
!
    IF (PRESENT(PRGLGV)) THEN
      PRGLGV (JLOOP,:) = 100.
    ENDIF   
!
!-------------------------------------------------------------------------------
!
!*    7.18    Gamma for understory vegetation
!             -------------------------------
!
    IF (PRESENT(PGAMMAGV)) THEN
      PGAMMAGV (JLOOP,:) = 0.
    ENDIF   
!
!-------------------------------------------------------------------------------
!
!*    7.19    Rsmin for understory vegetation
!             -------------------------------
!   
    IF (PRESENT(PRSMINGV)) THEN
      PRSMINGV (JLOOP,:) = 40.
    ENDIF   
!
!-------------------------------------------------------------------------------
!
!*    7.20   Jackson (1996) coefficient for cumulative root fraction
!            for understory vegetataion. Assumed equal to grass value
!            -------------------------------------------------------
!
    IF (PRESENT(PROOT_EXTINCTIONGV)) THEN
      PROOT_EXTINCTIONGV (JLOOP,:) = 0.943
    ENDIF   
!
!-------------------------------------------------------------------------------
!
!*    7.21    Wrmax_cf for understory vegetation
!             ----------------------------------
!
    IF (PRESENT(PWRMAX_CFGV)) THEN
      PWRMAX_CFGV (JLOOP,:) = 0.2
    ENDIF   
!------------------------------------------------------------------------
!*    2.20   specific leaf area sensitivity to nitrogen concentration
!            -----------------------------
!            corresponds to "e" in (eq 1) from Gibelin et al, 2006 
!            SLA = f + e * Nm   with SLA = specific leaf area
!            kept values from Gibelin et al 2006 
!            -----------------------------------------------------
!
    IF (PRESENT(PCE_NITRO)) THEN
      PCE_NITRO(JLOOP,:)=7.68
      IF(PTYPE(JLOOP,NVT_TEBD)>0. )  PCE_NITRO(JLOOP,NVT_TEBD)= 4.83
      IF(PTYPE(JLOOP,NVT_TRBD)>0. )  PCE_NITRO(JLOOP,NVT_TRBD)= 4.83
      IF(PTYPE(JLOOP,NVT_TEBE)>0. )  PCE_NITRO(JLOOP,NVT_TEBE)= 4.83
      IF(PTYPE(JLOOP,NVT_BOBD)>0. )  PCE_NITRO(JLOOP,NVT_BOBD)= 4.83
      IF(PTYPE(JLOOP,NVT_SHRB)>0. )  PCE_NITRO(JLOOP,NVT_SHRB)= 4.83
      IF(PTYPE(JLOOP,NVT_BONE)>0. )  PCE_NITRO(JLOOP,NVT_BONE)= 4.85
      IF(PTYPE(JLOOP,NVT_TENE)>0. )  PCE_NITRO(JLOOP,NVT_TENE)= 4.85
      IF(PTYPE(JLOOP,NVT_BOND)>0. )  PCE_NITRO(JLOOP,NVT_BOND)= 4.85
      IF(PTYPE(JLOOP,NVT_TRBE)>0. )  PCE_NITRO(JLOOP,NVT_TRBE)= 4.83
      IF(GAGRI_TO_GRASS)THEN
        IF(PTYPE(JLOOP,NVT_C3  )>0. )  PCE_NITRO(JLOOP,NVT_C3  )= 5.56
        IF(PTYPE(JLOOP,NVT_C4  )>0. )  PCE_NITRO(JLOOP,NVT_C4  )= 5.56
        IF(PTYPE(JLOOP,NVT_IRR )>0. )  PCE_NITRO(JLOOP,NVT_IRR )= 5.56
      ELSE
        IF(PTYPE(JLOOP,NVT_C3  )>0. )  PCE_NITRO(JLOOP,NVT_C3  )= 3.79
        IF(PTYPE(JLOOP,NVT_C4  )>0. )  PCE_NITRO(JLOOP,NVT_C4  )= 7.68
        IF(PTYPE(JLOOP,NVT_IRR )>0. )  PCE_NITRO(JLOOP,NVT_IRR )= 7.68
      ENDIF
      IF(PTYPE(JLOOP,NVT_GRAS)>0. )  PCE_NITRO(JLOOP,NVT_GRAS)= 5.56
      IF(PTYPE(JLOOP,NVT_BOGR)>0. )  PCE_NITRO(JLOOP,NVT_BOGR)= 5.56      
      IF(PTYPE(JLOOP,NVT_PARK)>0. )  PCE_NITRO(JLOOP,NVT_PARK)= 5.56
    ENDIF
!
!-------------------------------------------------------------------------------
!*    2.21   lethal minimum value of leaf area ratio
!            ----------
!            intercept of SLA = f + e * Nm  from Gibelin et al, 2006 (eq 1)
!            kept Gibelin et al values for grasses and crops
!            used TRY database (Kattge et al., 2011) median values for trees                    
!            with SLA and Nm from TRY and "e" (PCE_NITRO) from Gibelin et al 2006 
!            used Domingues 2011 for TRBE SLA.
!            ------------------------------------------------------
    IF (PRESENT(PCF_NITRO)) THEN
      PCF_NITRO(JLOOP,:)=-4.33
      IF(PTYPE(JLOOP,NVT_TEBD)>0. )  PCF_NITRO(JLOOP,NVT_TEBD)= 5.11
      IF(PTYPE(JLOOP,NVT_TRBD)>0. )  PCF_NITRO(JLOOP,NVT_TRBD)= 5.11
      IF(PTYPE(JLOOP,NVT_TEBE)>0. )  PCF_NITRO(JLOOP,NVT_TEBE)= 0.17
      IF(PTYPE(JLOOP,NVT_BOBD)>0. )  PCF_NITRO(JLOOP,NVT_BOBD)= 5.11
      IF(PTYPE(JLOOP,NVT_SHRB)>0. )  PCF_NITRO(JLOOP,NVT_SHRB)= 4.98
      IF(PTYPE(JLOOP,NVT_BONE)>0. )  PCF_NITRO(JLOOP,NVT_BONE)= -0.87
      IF(PTYPE(JLOOP,NVT_TENE)>0. )  PCF_NITRO(JLOOP,NVT_TENE)= -0.87
      IF(PTYPE(JLOOP,NVT_BOND)>0. )  PCF_NITRO(JLOOP,NVT_BOND)= 0.68
      IF(PTYPE(JLOOP,NVT_TRBE)>0. )  PCF_NITRO(JLOOP,NVT_TRBE)= 0.12 ! obtained using f = SLA - e*Nm 
                                                                     ! with SLA = 8.33 m2/kg_DM (Domingues 2011), Nm=1.7% (TRY)
      IF(GAGRI_TO_GRASS)THEN
        IF(PTYPE(JLOOP,NVT_C3  )>0. )  PCF_NITRO(JLOOP,NVT_C3  )= 6.73
        IF(PTYPE(JLOOP,NVT_C4  )>0. )  PCF_NITRO(JLOOP,NVT_C4  )= 6.73
        IF(PTYPE(JLOOP,NVT_IRR )>0. )  PCF_NITRO(JLOOP,NVT_IRR )= 6.73
      ELSE
        IF(PTYPE(JLOOP,NVT_C3  )>0. )  PCF_NITRO(JLOOP,NVT_C3  )= 9.84
        IF(PTYPE(JLOOP,NVT_C4  )>0. )  PCF_NITRO(JLOOP,NVT_C4  )= -4.33
        IF(PTYPE(JLOOP,NVT_IRR )>0. )  PCF_NITRO(JLOOP,NVT_IRR )= -4.33
      ENDIF
      IF(PTYPE(JLOOP,NVT_GRAS)>0. )  PCF_NITRO(JLOOP,NVT_GRAS)= 6.73
      IF(PTYPE(JLOOP,NVT_BOGR)>0. )  PCF_NITRO(JLOOP,NVT_BOGR)= 6.73    
      IF(PTYPE(JLOOP,NVT_PARK)>0. )  PCF_NITRO(JLOOP,NVT_PARK)= 6.73
    ENDIF    
!-------------------------------------------------------------------------------
!*    2.22   nitrogen concentration of leaf biomass
!            ----------
!            kept Gibelin et al 2006 values for grasses and crops
!            Nm from TRY database (Kattge et al. GCB 2011) median values for tree PFTs
!            Nm in mg_N/g_DM and PCNA_NITRO in % --> PCNA_NITRO = Nm * 0.1 
!            --------------------------------------------------
    IF (PRESENT(PCNA_NITRO)) THEN
      PCNA_NITRO(JLOOP,:)=1.3
      IF(PTYPE(JLOOP,NVT_TEBD)>0. )  PCNA_NITRO(JLOOP,NVT_TEBD)= 2.13
      IF(PTYPE(JLOOP,NVT_TRBD)>0. )  PCNA_NITRO(JLOOP,NVT_TRBD)= 2.13
      IF(PTYPE(JLOOP,NVT_TEBE)>0. )  PCNA_NITRO(JLOOP,NVT_TEBE)= 1.69
      IF(PTYPE(JLOOP,NVT_BOBD)>0. )  PCNA_NITRO(JLOOP,NVT_BOBD)= 2.13
      IF(PTYPE(JLOOP,NVT_SHRB)>0. )  PCNA_NITRO(JLOOP,NVT_SHRB)= 2.15
      IF(PTYPE(JLOOP,NVT_BONE)>0. )  PCNA_NITRO(JLOOP,NVT_BONE)= 1.21
      IF(PTYPE(JLOOP,NVT_TENE)>0. )  PCNA_NITRO(JLOOP,NVT_TENE)= 1.21
      IF(PTYPE(JLOOP,NVT_BOND)>0. )  PCNA_NITRO(JLOOP,NVT_BOND)= 1.94
      IF(PTYPE(JLOOP,NVT_TRBE)>0. )  PCNA_NITRO(JLOOP,NVT_TRBE)= 1.7 
      IF(GAGRI_TO_GRASS)THEN
        IF(PTYPE(JLOOP,NVT_C4  )>0. )  PCNA_NITRO(JLOOP,NVT_C4  )= 1.3
        IF(PTYPE(JLOOP,NVT_IRR )>0. )  PCNA_NITRO(JLOOP,NVT_IRR) = 1.3
      ELSE
        IF(PTYPE(JLOOP,NVT_C4  )>0. )  PCNA_NITRO(JLOOP,NVT_C4  )= 1.9
        IF(PTYPE(JLOOP,NVT_IRR )>0. )  PCNA_NITRO(JLOOP,NVT_IRR) = 1.9
      ENDIF
      IF(PTYPE(JLOOP,NVT_C3  )>0. )  PCNA_NITRO(JLOOP,NVT_C3  )= 1.3
      IF(PTYPE(JLOOP,NVT_GRAS)>0. )  PCNA_NITRO(JLOOP,NVT_GRAS)= 1.3 
      IF(PTYPE(JLOOP,NVT_BOGR)>0. )  PCNA_NITRO(JLOOP,NVT_BOGR)= 1.3
      IF(PTYPE(JLOOP,NVT_PARK)>0. )  PCNA_NITRO(JLOOP,NVT_PARK)= 1.3
    ENDIF    
!-------------------------------------------------------------------------------
!*    7.15   Jackson (1996) coefficient for cumulative root fraction
!            -------------------------------------------------------
    IF (PRESENT(PROOT_EXTINCTION)) THEN
      PROOT_EXTINCTION(JLOOP,:)= 0.943 ! (default value)
      IF(PTYPE(JLOOP,NVT_TEBD)>0. )  PROOT_EXTINCTION(JLOOP,NVT_TEBD)= 0.966
      IF(PTYPE(JLOOP,NVT_TRBD)>0. )  PROOT_EXTINCTION(JLOOP,NVT_TRBD)= 0.961
      IF(PTYPE(JLOOP,NVT_TEBE)>0. )  PROOT_EXTINCTION(JLOOP,NVT_TEBE)= 0.966
      IF(PTYPE(JLOOP,NVT_SHRB)>0. )  PROOT_EXTINCTION(JLOOP,NVT_SHRB)= 0.964
      IF(PTYPE(JLOOP,NVT_TENE)>0. )  PROOT_EXTINCTION(JLOOP,NVT_TENE)= 0.976
      IF(PTYPE(JLOOP,NVT_TRBE)>0. )  PROOT_EXTINCTION(JLOOP,NVT_TRBE)= 0.962
      IF(GAGRI_TO_GRASS)THEN
        IF(PTYPE(JLOOP,NVT_C3  )>0. )  PROOT_EXTINCTION(JLOOP,NVT_C3  )= 0.943
        IF(PTYPE(JLOOP,NVT_C4  )>0. )  PROOT_EXTINCTION(JLOOP,NVT_C4  )= 0.943
        IF(PTYPE(JLOOP,NVT_IRR )>0. )  PROOT_EXTINCTION(JLOOP,NVT_IRR )= 0.943             
      ELSE
        IF(PTYPE(JLOOP,NVT_C3  )>0. )  PROOT_EXTINCTION(JLOOP,NVT_C3  )= 0.961
        IF(PTYPE(JLOOP,NVT_C4  )>0. )  PROOT_EXTINCTION(JLOOP,NVT_C4  )= 0.972
        IF(PTYPE(JLOOP,NVT_IRR )>0. )  PROOT_EXTINCTION(JLOOP,NVT_IRR )= 0.972
      ENDIF
      IF(PTYPE(JLOOP,NVT_BOGR)>0. )  PROOT_EXTINCTION(JLOOP,NVT_BOGR)= 0.914      
      IF(PTYPE(JLOOP,NVT_TROG)>0. )  PROOT_EXTINCTION(JLOOP,NVT_TROG)= 0.972
    ENDIF
!
!-------------------------------------------------------------------------------
!*    7.16   Ponderation coefficient between formulations for cumulative root fraction
!            -------------------------------------------------------------------------
!
    IF (PRESENT(PROOT_LIN)) THEN
      PROOT_LIN(JLOOP,:)= 0.05
      IF(PTYPE(JLOOP,NVT_TRBE)>0. )  PROOT_LIN(JLOOP,NVT_TRBE)= 0.25
    ENDIF
!
!-------------------------------------------------------------------------------
!*    7.17   Coefficient for chemistry deposition of SO2
!            -------------------------------------------
    IF (PRESENT(PSOILRC_SO2)) THEN
      PSOILRC_SO2(JLOOP,:)= 9999.
      IF(PTYPE(JLOOP,NVT_TEBD)>0. )  PSOILRC_SO2(JLOOP,NVT_TEBD)= 500.
      IF(PTYPE(JLOOP,NVT_TRBD)>0. )  PSOILRC_SO2(JLOOP,NVT_TRBD)= 500.
      IF(PTYPE(JLOOP,NVT_TEBE)>0. )  PSOILRC_SO2(JLOOP,NVT_TEBE)= 500.
      IF(PTYPE(JLOOP,NVT_BOBD)>0. )  PSOILRC_SO2(JLOOP,NVT_BOBD)= 500.
      IF(PTYPE(JLOOP,NVT_SHRB)>0. )  PSOILRC_SO2(JLOOP,NVT_SHRB)= 500.
      IF(PTYPE(JLOOP,NVT_BONE)>0. )  PSOILRC_SO2(JLOOP,NVT_BONE)= 500.
      IF(PTYPE(JLOOP,NVT_TENE)>0. )  PSOILRC_SO2(JLOOP,NVT_TENE)= 500.
      IF(PTYPE(JLOOP,NVT_BOND)>0. )  PSOILRC_SO2(JLOOP,NVT_BOND)= 500.
      IF(PTYPE(JLOOP,NVT_TRBE)>0. )  PSOILRC_SO2(JLOOP,NVT_TRBE)= 200.    
      IF(GAGRI_TO_GRASS)THEN
        IF(PTYPE(JLOOP,NVT_C3  )>0. )  PSOILRC_SO2(JLOOP,NVT_C3  )= 350.
        IF(PTYPE(JLOOP,NVT_C4  )>0. )  PSOILRC_SO2(JLOOP,NVT_C4  )= 350.
        IF(PTYPE(JLOOP,NVT_IRR )>0. )  PSOILRC_SO2(JLOOP,NVT_IRR )= 350.
      ELSE
        IF(PTYPE(JLOOP,NVT_C3  )>0. )  PSOILRC_SO2(JLOOP,NVT_C3  )= 150.
        IF(PTYPE(JLOOP,NVT_C4  )>0. )  PSOILRC_SO2(JLOOP,NVT_C4  )= 150.
        IF(PTYPE(JLOOP,NVT_IRR )>0. )  PSOILRC_SO2(JLOOP,NVT_IRR )= 0.001
      ENDIF
      IF(PTYPE(JLOOP,NVT_GRAS)>0. )  PSOILRC_SO2(JLOOP,NVT_GRAS)= 350.
      IF(PTYPE(JLOOP,NVT_BOGR)>0. )  PSOILRC_SO2(JLOOP,NVT_BOGR)= 350.     
      IF(PTYPE(JLOOP,NVT_PARK)>0. )  PSOILRC_SO2(JLOOP,NVT_PARK)= 350.
      IF(PTYPE(JLOOP,NVT_TROG)>0. )  PSOILRC_SO2(JLOOP,NVT_TROG)= 350.
      IF(PTYPE(JLOOP,NVT_NO  )>0. )  PSOILRC_SO2(JLOOP,NVT_NO  )=1000.
      IF(PTYPE(JLOOP,NVT_ROCK)>0. )  PSOILRC_SO2(JLOOP,NVT_ROCK)= 400.
      IF(PTYPE(JLOOP,NVT_SNOW)>0. )  PSOILRC_SO2(JLOOP,NVT_SNOW)= 100.
    ENDIF
!
!------------------------------------------------------------------------------
!*    7.18   Coefficient for chemistry deposition of O3
!            ------------------------------------------
    IF (PRESENT(PSOILRC_O3)) THEN
      PSOILRC_O3(JLOOP,:)= 9999.
      IF(PTYPE(JLOOP,NVT_TEBD)>0. )  PSOILRC_O3(JLOOP,NVT_TEBD)= 200.
      IF(PTYPE(JLOOP,NVT_TRBD)>0. )  PSOILRC_O3(JLOOP,NVT_TRBD)= 200.
      IF(PTYPE(JLOOP,NVT_TEBE)>0. )  PSOILRC_O3(JLOOP,NVT_TEBE)= 200.
      IF(PTYPE(JLOOP,NVT_BOBD)>0. )  PSOILRC_O3(JLOOP,NVT_BOBD)= 200.
      IF(PTYPE(JLOOP,NVT_SHRB)>0. )  PSOILRC_O3(JLOOP,NVT_SHRB)= 200.
      IF(PTYPE(JLOOP,NVT_BONE)>0. )  PSOILRC_O3(JLOOP,NVT_BONE)= 200.
      IF(PTYPE(JLOOP,NVT_TENE)>0. )  PSOILRC_O3(JLOOP,NVT_TENE)= 200.
      IF(PTYPE(JLOOP,NVT_BOND)>0. )  PSOILRC_O3(JLOOP,NVT_BOND)= 200.
      IF(PTYPE(JLOOP,NVT_TRBE)>0. )  PSOILRC_O3(JLOOP,NVT_TRBE)= 500.
      IF(GAGRI_TO_GRASS)THEN
        IF(PTYPE(JLOOP,NVT_C3  )>0. )  PSOILRC_O3(JLOOP,NVT_C3  )= 200.
        IF(PTYPE(JLOOP,NVT_C4  )>0. )  PSOILRC_O3(JLOOP,NVT_C4  )= 200.
        IF(PTYPE(JLOOP,NVT_IRR )>0. )  PSOILRC_O3(JLOOP,NVT_IRR )= 200.              
      ELSE
        IF(PTYPE(JLOOP,NVT_C3  )>0. )  PSOILRC_O3(JLOOP,NVT_C3  )= 150.
        IF(PTYPE(JLOOP,NVT_C4  )>0. )  PSOILRC_O3(JLOOP,NVT_C4  )= 150.
        IF(PTYPE(JLOOP,NVT_IRR )>0. )  PSOILRC_O3(JLOOP,NVT_IRR )=1000.
      ENDIF
      IF(PTYPE(JLOOP,NVT_GRAS)>0. )  PSOILRC_O3(JLOOP,NVT_GRAS)= 200.
      IF(PTYPE(JLOOP,NVT_BOGR)>0. )  PSOILRC_O3(JLOOP,NVT_BOGR)= 200.      
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
                                         PTYPE(JLOOP,:),GAGRI_TO_GRASS)         
      END DO
    ELSEIF (PRESENT(PVEG_OUT) .AND. .NOT. PRESENT(PLAI)) THEN
     CALL ABOR1_SFX("INI_DATA_PARAM: WHEN CALLING WITH PVEG_OUT, PLAI MUST BE IN ARGUMENTS TOO") 
    ENDIF
! 
         
    IF (PRESENT(PGREEN) .AND. PRESENT(PLAI)) THEN
      DO JMONTH=1,SIZE(PGREEN,2)
          PGREEN(JLOOP,JMONTH,:) = GREEN_FROM_LAI(PLAI(JLOOP,JMONTH,:),   &
                                           PTYPE(JLOOP,:),GAGRI_TO_GRASS)  
      END DO
    ELSEIF (PRESENT(PGREEN) .AND. .NOT. PRESENT(PLAI)) THEN
     CALL ABOR1_SFX("INI_DATA_PARAM: WHEN CALLING WITH PGREEN, PLAI MUST BE IN ARGUMENTS TOO")
    ENDIF     
!-------------------------------------------------------------------------------
!*    7.16   z0
!            --
   IF (PRESENT(PZ0) .AND. PRESENT(PLAI) .AND. PRESENT(PH_TREE)) THEN
     DO JMONTH=1,SIZE(PZ0,2)
       PZ0(JLOOP,JMONTH,:) = Z0V_FROM_LAI(PLAI(JLOOP,JMONTH,:),                &
                                                  PH_TREE(JLOOP,:),            &
                                                  PTYPE(JLOOP,:),GAGRI_TO_GRASS)  
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
!
!-------------------------------------------------------------------------------
!
!*    7.19   vegetation height
!            -----------------
!
    IF (PRESENT(PH_VEG) .AND. PRESENT(PLAI) .AND. PRESENT(PH_TREE)) THEN
      DO JMONTH=1,SIZE(PH_VEG,2)
        PH_VEG(JLOOP,JMONTH,:) = VEG_HEIGHT_FROM_LAI(PLAI(JLOOP,JMONTH,:),        &
                                                   PH_TREE(JLOOP,:),              &
                                                   PTYPE(JLOOP,:), GAGRI_TO_GRASS )  
      END DO
    ELSEIF (PRESENT(PH_VEG) .AND. (.NOT. PRESENT(PLAI) .OR. .NOT. PRESENT(PH_TREE))) THEN
      CALL ABOR1_SFX("INI_DATA_PARAM: WHEN CALLING WITH PH_VEG, PLAI AND PH_TREE MUST BE IN ARGUMENTS TOO")
    ENDIF   
!
!-------------------------------------------------------------------------------
!
!*    7.20   LAI understory vegetation
!            ----------
!-------------------------------------------------------------------------------
    IF (PRESENT(PLAIGV_OUT) .AND. PRESENT(PLAI) .AND. ( PRESENT(PLAIMIN_IN) .OR. PRESENT(PLAIMIN_OUT) ) ) THEN
!
!            LAI understoty vegetation defined as fraction of LAI upperstory vegetation 
!            -------------------
      ZLAIFRGV (JLOOP,:) = 0.
      IF(PTYPE(JLOOP,NVT_TEBD)>0. )  ZLAIFRGV (JLOOP,NVT_TEBD) = 0.1
      IF(PTYPE(JLOOP,NVT_BONE)>0. )  ZLAIFRGV (JLOOP,NVT_BONE) = 0.1
      IF(PTYPE(JLOOP,NVT_TRBE)>0. )  ZLAIFRGV (JLOOP,NVT_TRBE) = 0.1
      IF(PTYPE(JLOOP,NVT_GRAS)>0. )  ZLAIFRGV (JLOOP,NVT_GRAS) = 0.1
      IF(PTYPE(JLOOP,NVT_TROG)>0. )  ZLAIFRGV (JLOOP,NVT_TROG) = 0.1
      IF(PTYPE(JLOOP,NVT_TRBD)>0. )  ZLAIFRGV (JLOOP,NVT_TRBD) = 0.1
      IF(PTYPE(JLOOP,NVT_TEBE)>0. )  ZLAIFRGV (JLOOP,NVT_TEBE) = 0.1
      IF(PTYPE(JLOOP,NVT_TENE)>0. )  ZLAIFRGV (JLOOP,NVT_TENE) = 0.1
      IF(PTYPE(JLOOP,NVT_BOBD)>0. )  ZLAIFRGV (JLOOP,NVT_BOBD) = 0.1
      IF(PTYPE(JLOOP,NVT_BOND)>0. )  ZLAIFRGV (JLOOP,NVT_BOND) = 0.1
      IF(PTYPE(JLOOP,NVT_BOGR)>0. )  ZLAIFRGV (JLOOP,NVT_BOGR) = 0.1
      IF(PTYPE(JLOOP,NVT_SHRB)>0. )  ZLAIFRGV (JLOOP,NVT_SHRB) = 0.1
!
      DO JMONTH=1,SIZE(PLAIGV_OUT,2)
        IF(PRESENT(PLAIMIN_OUT))THEN
          PLAIGV_OUT(JLOOP,JMONTH,:) = MAX( PLAI(JLOOP,JMONTH,:)* &
                                       ZLAIFRGV(JLOOP,:), PLAIMIN_OUT(JLOOP,:) )
        ELSEIF(PRESENT(PLAIMIN_IN))THEN
          PLAIGV_OUT(JLOOP,JMONTH,:) = MAX( PLAI(JLOOP,JMONTH,:)* &
                                       ZLAIFRGV(JLOOP,:), PLAIMIN_IN(JLOOP,:) )
        ENDIF
      END DO
    ELSEIF (PRESENT(PLAIGV_OUT) .AND. ( .NOT.PRESENT(PLAI) .OR. &
            (.NOT.PRESENT(PLAIMIN_IN) .AND. .NOT.PRESENT(PLAIMIN_OUT) ) ) ) THEN
      CALL ABOR1_SFX("INI_DATA_PARAM: WHEN CALLING WITH PLAIGV_OUT, PLAI AND PLAIMIN_IN OR PLAIMIN_OUT MUST BE IN ARGUMENTS TOO")
    ENDIF   
!
!-------------------------------------------------------------------------------
!
!*    7.21   z0 understory litter
!            --------------------
! 
    IF (PRESENT(PZ0LITTER)) THEN
      PZ0LITTER(JLOOP,:,:)  = 0.013 ! Roughness for bare soil
    ENDIF   
!
!-------------------------------------------------------------------------------
!
  END IF
!-------------------------------------------------------------------------------
END DO
!-------------------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('INI_DATA_PARAM',1,ZHOOK_HANDLE)
!-------------------------------------------------------------------------------
!
END SUBROUTINE INI_DATA_PARAM
