!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE INIT_FROM_DATA_GRDN_n (DTGD, &
                                        KDECADE, HPHOTO,                               &
                                               PVEG,                                  &
                                               PLAI,PRSMIN,PGAMMA,PWRMAX_CF,          &
                                               PRGL,PCV,PDG,PD_ICE,PZ0,PZ0_O_Z0H,     &
                                               PALBNIR_VEG,PALBVIS_VEG,PALBUV_VEG,    &
                                               PEMIS,                                 &
                                               PVEGTYPE,PROOTFRAC,                    &
                                               PGMES,PBSLAI,PLAIMIN,PSEFOLD,PGC,      &
                                               PDMAX, PF2I, OSTRESS,                  &
                                               PH_TREE, PRE25,                        &
                                               PCE_NITRO, PCF_NITRO, PCNA_NITRO,      &
                                               PALBNIR_SOIL,PALBVIS_SOIL,PALBUV_SOIL  )  
!     ##############################################################
!
!!**** *CONVERT_COVER* convert surface cover classes into secondary 
!!                     physiographic variables for ISBA
!!
!!    PURPOSE
!!    -------
!!
!!    METHOD
!!    ------
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
!!    Original   01/2004
!     
!!      P Le Moigne 09/2005 AGS modifs of L. Jarlan
!----------------------------------------------------------------------------
!
!*    0.     DECLARATION
!            -----------
!

!
!
!
USE MODD_DATA_TEB_GARDEN_n, ONLY : DATA_TEB_GARDEN_t
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*    0.1    Declaration of arguments
!            ------------------------
!
!
TYPE(DATA_TEB_GARDEN_t), INTENT(INOUT) :: DTGD
!
INTEGER,                INTENT(IN)    :: KDECADE
 CHARACTER(LEN=*),       INTENT(IN)    :: HPHOTO  ! type of photosynthesis
!
REAL, DIMENSION(:),   OPTIONAL, INTENT(OUT)   :: PVEG
REAL, DIMENSION(:),   OPTIONAL, INTENT(OUT)   :: PLAI
REAL, DIMENSION(:),   OPTIONAL, INTENT(OUT)   :: PRSMIN
REAL, DIMENSION(:),   OPTIONAL, INTENT(OUT)   :: PGAMMA
REAL, DIMENSION(:),   OPTIONAL, INTENT(OUT)   :: PWRMAX_CF
REAL, DIMENSION(:),   OPTIONAL, INTENT(OUT)   :: PRGL
REAL, DIMENSION(:),   OPTIONAL, INTENT(OUT)   :: PCV
REAL, DIMENSION(:,:), OPTIONAL, INTENT(OUT)   :: PDG
REAL, DIMENSION(:),   OPTIONAL, INTENT(OUT)   :: PD_ICE
REAL, DIMENSION(:,:), OPTIONAL, INTENT(OUT)   :: PROOTFRAC
REAL, DIMENSION(:),   OPTIONAL, INTENT(OUT)   :: PZ0
REAL, DIMENSION(:),   OPTIONAL, INTENT(OUT)   :: PZ0_O_Z0H
REAL, DIMENSION(:),   OPTIONAL, INTENT(OUT)   :: PALBNIR_VEG
REAL, DIMENSION(:),   OPTIONAL, INTENT(OUT)   :: PALBVIS_VEG
REAL, DIMENSION(:),   OPTIONAL, INTENT(OUT)   :: PALBUV_VEG
REAL, DIMENSION(:),   OPTIONAL, INTENT(OUT)   :: PEMIS
!
REAL, DIMENSION(:,:),   OPTIONAL, INTENT(OUT)   :: PVEGTYPE
!
REAL, DIMENSION(:),   OPTIONAL, INTENT(OUT)   :: PGMES
REAL, DIMENSION(:),   OPTIONAL, INTENT(OUT)   :: PRE25
REAL, DIMENSION(:),   OPTIONAL, INTENT(OUT)   :: PBSLAI
REAL, DIMENSION(:),   OPTIONAL, INTENT(OUT)   :: PLAIMIN
REAL, DIMENSION(:),   OPTIONAL, INTENT(OUT)   :: PSEFOLD
REAL, DIMENSION(:),   OPTIONAL, INTENT(OUT)   :: PGC
REAL, DIMENSION(:),   OPTIONAL, INTENT(OUT)   :: PDMAX
REAL, DIMENSION(:),   OPTIONAL, INTENT(OUT)   :: PF2I
LOGICAL, DIMENSION(:),OPTIONAL, INTENT(OUT)   :: OSTRESS
!
REAL, DIMENSION(:),   OPTIONAL, INTENT(OUT)   :: PH_TREE
!
REAL, DIMENSION(:),   OPTIONAL, INTENT(OUT)   :: PCE_NITRO
REAL, DIMENSION(:),   OPTIONAL, INTENT(OUT)   :: PCF_NITRO
REAL, DIMENSION(:),   OPTIONAL, INTENT(OUT)   :: PCNA_NITRO
REAL, DIMENSION(:),   OPTIONAL, INTENT(OUT)   :: PALBNIR_SOIL
REAL, DIMENSION(:),   OPTIONAL, INTENT(OUT)   :: PALBVIS_SOIL
REAL, DIMENSION(:),   OPTIONAL, INTENT(OUT)   :: PALBUV_SOIL
!
!*    0.2    Declaration of local variables
!            ------------------------------
!
INTEGER :: ITIME
INTEGER :: ILUOUT
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------
!
!*    1.      TIME INITIALIZATION
!             -------------------
!
! data every month
IF (LHOOK) CALL DR_HOOK('INIT_FROM_DATA_GRDN_N',0,ZHOOK_HANDLE)
IF (DTGD%NTIME==12) THEN
  ITIME = (KDECADE+2)/3    
ELSEIF (DTGD%NTIME==1) THEN
  ITIME = 1
ENDIF
!
!*    2.      SECONDARY VARIABLES
!             -------------------
!
!*    2.1     fields on natural surfaces only, taking into account patches/ 
!             -------------------------------
!
!
IF (PRESENT(PH_TREE)) THEN
  IF (SIZE(PH_TREE)>0) PH_TREE = DTGD%XDATA_H_TREE
ENDIF
!
IF (PRESENT(PVEGTYPE)) PVEGTYPE = DTGD%XDATA_VEGTYPE
!
! vegetation fraction
! -------------------
!
IF (PRESENT(PVEG)) PVEG(:) =  DTGD%XDATA_VEG (:,ITIME)
!
! Leaf Aera Index
! ---------------
!
IF (PRESENT(PLAI)) PLAI(:) = DTGD%XDATA_LAI (:,ITIME)
!
! roughness length
! ----------------
!
IF (PRESENT(PZ0)) PZ0(:) =  DTGD%XDATA_Z0 (:,ITIME)
!
IF (PRESENT(PZ0_O_Z0H)) PZ0_O_Z0H = DTGD%XDATA_Z0_O_Z0H
!
!
!emis-eco
!--------
!
IF (PRESENT(PEMIS)) PEMIS(:) =  DTGD%XDATA_EMIS (:,ITIME)
! 
!---------------------------------------------------------------------------------
! 
!* 1/Rsmin
!
IF (PRESENT(PRSMIN)) THEN
  IF (SIZE(PRSMIN)>0) PRSMIN = DTGD%XDATA_RSMIN
END IF
!
!* other vegetation parameters
!
IF (PRESENT(PGAMMA)) PGAMMA = DTGD%XDATA_GAMMA
IF (PRESENT(PWRMAX_CF)) PWRMAX_CF = DTGD%XDATA_WRMAX_CF
!
!
IF (PRESENT(PRGL)) PRGL = DTGD%XDATA_RGL
IF (PRESENT(PCV)) PCV = DTGD%XDATA_CV
!
!---------------------------------------------------------------------------------
!
!* soil layers
!  -----------
!
IF (PRESENT(PDG)) PDG = DTGD%XDATA_DG
!
!* cumulative root fraction
!
IF (PRESENT(PROOTFRAC)) THEN
  IF (SIZE(PROOTFRAC)>0) PROOTFRAC = DTGD%XDATA_ROOTFRAC
ENDIF
!
!* soil ice for runoff
!
IF (PRESENT(PD_ICE)) PD_ICE = DTGD%XDATA_DICE
!
!---------------------------------------------------------------------------------
IF (PRESENT(PALBNIR_VEG)) PALBNIR_VEG = DTGD%XDATA_ALBNIR_VEG
IF (PRESENT(PALBVIS_VEG)) PALBVIS_VEG = DTGD%XDATA_ALBVIS_VEG
IF (PRESENT(PALBUV_VEG)) PALBUV_VEG = DTGD%XDATA_ALBUV_VEG

IF (PRESENT(PALBNIR_SOIL)) PALBNIR_SOIL(:) = DTGD%XDATA_ALBNIR_SOIL
IF (PRESENT(PALBVIS_SOIL)) PALBVIS_SOIL(:) = DTGD%XDATA_ALBVIS_SOIL
IF (PRESENT(PALBUV_SOIL)) PALBUV_SOIL(:) = DTGD%XDATA_ALBUV_SOIL

IF (PRESENT(PGMES)) THEN
  IF (SIZE(PGMES)>0) PGMES = DTGD%XDATA_GMES
END IF

IF (PRESENT(PBSLAI)) THEN
  IF (SIZE(PBSLAI)>0) PBSLAI = DTGD%XDATA_BSLAI
END IF

IF (PRESENT(PSEFOLD)) THEN
  IF (SIZE(PSEFOLD)>0) PSEFOLD = DTGD%XDATA_SEFOLD
END IF

IF (PRESENT(PGC)) THEN
  IF (SIZE(PGC)>0) PGC = DTGD%XDATA_GC
END IF

IF (PRESENT(PDMAX)) THEN
  IF (SIZE(PDMAX)>0) PDMAX = DTGD%XDATA_DMAX
END IF

IF (PRESENT(PRE25)) THEN
  IF (SIZE(PRE25)>0) PRE25 = DTGD%XDATA_RE25
END IF

IF (PRESENT(PLAIMIN)) THEN
  IF (SIZE(PLAIMIN)>0) PLAIMIN = DTGD%XDATA_LAIMIN
END IF

IF (PRESENT(PCE_NITRO)) THEN
  IF (SIZE(PCE_NITRO)>0) PCE_NITRO = DTGD%XDATA_CE_NITRO
END IF

IF (PRESENT(PCF_NITRO)) THEN
  IF (SIZE(PCF_NITRO)>0) PCF_NITRO = DTGD%XDATA_CF_NITRO
END IF

IF (PRESENT(PCNA_NITRO)) THEN
  IF (SIZE(PCNA_NITRO)>0) PCNA_NITRO = DTGD%XDATA_CNA_NITRO
END IF

IF (PRESENT(PF2I)) THEN
  IF (SIZE(PF2I)>0) PF2I = DTGD%XDATA_F2I
END IF
!
IF (PRESENT(OSTRESS)) THEN
  IF (SIZE(OSTRESS)>0) OSTRESS = DTGD%LDATA_STRESS
END IF
IF (LHOOK) CALL DR_HOOK('INIT_FROM_DATA_GRDN_N',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE INIT_FROM_DATA_GRDN_n
