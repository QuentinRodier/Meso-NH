!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE CONVERT_COVER_TEB_VEG (DTCO, TVG, &
                                        HISBA,KDECADE,PCOVER,OCOVER,HPHOTO,   &
                                         HSFTYPE,PVEG,                       &
                                         PLAI,PRSMIN,PGAMMA,PWRMAX_CF,       &
                                         PRGL,PCV,PSOILGRID,PPERM,           &
                                         PDG,KWG_LAYER,PDROOT,PDG2,          &
                                         PD_ICE,PZ0,PZ0_O_Z0H,               &
                                         PALBNIR_VEG,PALBVIS_VEG,PALBUV_VEG, &
                                         PEMIS_ECO,                          &
                                         PVEGTYPE,PROOTFRAC,                 &
                                         PGMES,PBSLAI,PLAIMIN,PSEFOLD,PGC,   &
                                         PDMAX, PF2I, OSTRESS, PH_TREE,PRE25,&
                                         PCE_NITRO, PCF_NITRO, PCNA_NITRO,   &
                                         TPSEED, TPREAP, PWATSUP, PIRRIG,    &
                                         PGNDLITTER, PLAIGV, PRSMINGV,       &
                                         PGAMMAGV,                           &
                                         PWRMAX_CFGV, PRGLGV, PROOTFRACGV,   &
                                         PZ0LITTER, PH_VEG       )  
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
!!    
!!      P Le Moigne 09/2005 AGS modifs of L. Jarlan
!!      B. Decharme 04/2013 Add CDGAVG (average depth)
!!                          Soil depth = Root depth with ISBA-DF
!!                          except for bare soil pft (but limited to 1m)
!!      P Samuelsson 10/2014 MEB
!----------------------------------------------------------------------------
!
!*    0.     DECLARATION
!            -----------
!
!
USE MODD_DATA_COVER_n, ONLY : DATA_COVER_t
USE MODD_TEB_VEG_n, ONLY : TEB_VEG_OPTIONS_t
!
USE MODD_DATA_COVER,     ONLY : XDATA_LAI, XDATA_H_TREE,                  &
                                  XDATA_VEG, XDATA_Z0, XDATA_Z0_O_Z0H,    &
                                  XDATA_EMIS_ECO, XDATA_GAMMA, XDATA_CV,  &
                                  XDATA_RGL, XDATA_RSMIN,                 &
                                  XDATA_ALBNIR_VEG, XDATA_ALBVIS_VEG,     &
                                  XDATA_ALBUV_VEG, XDATA_DICE,            &
                                  XDATA_ALB_VEG_NIR, XDATA_ALB_VEG_VIS,   &
                                  XDATA_ALB_SOIL_NIR, XDATA_ALB_SOIL_VIS, &                                  
                                  XDATA_GMES, XDATA_BSLAI, XDATA_LAIMIN,  &
                                  XDATA_SEFOLD, XDATA_GC, XDATA_WRMAX_CF, &
                                  XDATA_STRESS,                           &
                                  XDATA_DMAX, XDATA_F2I, XDATA_RE25,      &
                                  XDATA_CE_NITRO, XDATA_CF_NITRO,         &
                                  XDATA_CNA_NITRO,                        &
                                  XDATA_GMES_ST, XDATA_BSLAI_ST,          &
                                  XDATA_SEFOLD_ST, XDATA_GC_ST,           &
                                  XDATA_DMAX_ST, XDATA_WATSUP,            &
                                  TDATA_SEED, TDATA_REAP,XDATA_IRRIG,     &
                                  XDATA_ROOT_DEPTH, XDATA_GROUND_DEPTH,   &
                                  XDATA_ROOT_EXTINCTION, XDATA_ROOT_LIN,  &
                                  XDATA_GNDLITTER,                        &
                                  XDATA_RGLGV, XDATA_GAMMAGV,             &
                                  XDATA_RSMINGV, XDATA_WRMAX_CFGV,        &
                                  XDATA_LAIGV, XDATA_Z0LITTER,            &
                                  XDATA_ROOT_DEPTHGV, XDATA_H_VEG,        &
                                  XDATA_ROOT_EXTINCTIONGV

USE MODD_DATA_COVER_PAR, ONLY : NVEGTYPE, JPCOVER, NVT_NO, NVT_ROCK, NVT_SNOW
USE MODD_TYPE_DATE_SURF
!
!
USE MODI_AV_PGD
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
!
TYPE(DATA_COVER_t), INTENT(INOUT) :: DTCO
TYPE(TEB_VEG_OPTIONS_t), INTENT(INOUT) :: TVG
!
 CHARACTER(LEN=*),       INTENT(IN)    :: HISBA   ! type of soil (Force-Restore OR Diffusion)
INTEGER,                INTENT(IN)    :: KDECADE
REAL, DIMENSION(:,:),   INTENT(IN)    :: PCOVER
LOGICAL, DIMENSION(:), INTENT(IN)     :: OCOVER
 CHARACTER(LEN=*),       INTENT(IN)    :: HPHOTO  ! type of photosynthesis
 CHARACTER(LEN=*),       INTENT(IN)    :: HSFTYPE ! nature / garden
!
REAL, DIMENSION(:)  ,   OPTIONAL, INTENT(IN)    :: PSOILGRID
REAL, DIMENSION(:)  ,   OPTIONAL, INTENT(IN)    :: PPERM
REAL, DIMENSION(:,:),   OPTIONAL, INTENT(OUT)   :: PVEG
REAL, DIMENSION(:,:),   OPTIONAL, INTENT(OUT)   :: PLAI
REAL, DIMENSION(:,:),   OPTIONAL, INTENT(OUT)   :: PRSMIN
REAL, DIMENSION(:,:),   OPTIONAL, INTENT(OUT)   :: PGAMMA
REAL, DIMENSION(:,:),   OPTIONAL, INTENT(OUT)   :: PWRMAX_CF
REAL, DIMENSION(:,:),   OPTIONAL, INTENT(OUT)   :: PRGL
REAL, DIMENSION(:,:),   OPTIONAL, INTENT(OUT)   :: PCV
REAL, DIMENSION(:,:,:), OPTIONAL, INTENT(OUT)   :: PDG
INTEGER, DIMENSION(:,:),OPTIONAL, INTENT(OUT)   :: KWG_LAYER
REAL, DIMENSION(:,:),   OPTIONAL, INTENT(OUT)   :: PDROOT
REAL, DIMENSION(:,:),   OPTIONAL, INTENT(OUT)   :: PDG2
REAL, DIMENSION(:,:),   OPTIONAL, INTENT(OUT)   :: PD_ICE
REAL, DIMENSION(:,:,:), OPTIONAL, INTENT(OUT)   :: PROOTFRAC
REAL, DIMENSION(:,:),   OPTIONAL, INTENT(OUT)   :: PZ0
REAL, DIMENSION(:,:),   OPTIONAL, INTENT(OUT)   :: PZ0_O_Z0H
REAL, DIMENSION(:,:),   OPTIONAL, INTENT(OUT)   :: PALBNIR_VEG
REAL, DIMENSION(:,:),   OPTIONAL, INTENT(OUT)   :: PALBVIS_VEG
REAL, DIMENSION(:,:),   OPTIONAL, INTENT(OUT)   :: PALBUV_VEG
REAL, DIMENSION(:,:),   OPTIONAL, INTENT(OUT)   :: PEMIS_ECO
!
REAL, DIMENSION(:,:),   OPTIONAL, INTENT(OUT)   :: PGNDLITTER
REAL, DIMENSION(:,:),   OPTIONAL, INTENT(OUT)   :: PLAIGV
REAL, DIMENSION(:,:),   OPTIONAL, INTENT(OUT)   :: PRSMINGV
REAL, DIMENSION(:,:),   OPTIONAL, INTENT(OUT)   :: PGAMMAGV
REAL, DIMENSION(:,:),   OPTIONAL, INTENT(OUT)   :: PWRMAX_CFGV
REAL, DIMENSION(:,:),   OPTIONAL, INTENT(OUT)   :: PRGLGV
REAL, DIMENSION(:,:,:), OPTIONAL, INTENT(OUT)   :: PROOTFRACGV
REAL, DIMENSION(:,:),   OPTIONAL, INTENT(OUT)   :: PZ0LITTER
REAL, DIMENSION(:,:),   OPTIONAL, INTENT(OUT)   :: PH_VEG
!
REAL, DIMENSION(:,:), OPTIONAL, INTENT(OUT)   :: PVEGTYPE
!
REAL, DIMENSION(:,:),   OPTIONAL, INTENT(OUT)   :: PGMES
REAL, DIMENSION(:,:),   OPTIONAL, INTENT(OUT)   :: PRE25
REAL, DIMENSION(:,:),   OPTIONAL, INTENT(OUT)   :: PBSLAI
REAL, DIMENSION(:,:),   OPTIONAL, INTENT(OUT)   :: PLAIMIN
REAL, DIMENSION(:,:),   OPTIONAL, INTENT(OUT)   :: PSEFOLD
REAL, DIMENSION(:,:),   OPTIONAL, INTENT(OUT)   :: PGC
REAL, DIMENSION(:,:),   OPTIONAL, INTENT(OUT)   :: PDMAX
REAL, DIMENSION(:,:),   OPTIONAL, INTENT(OUT)   :: PF2I
LOGICAL, DIMENSION(:,:),OPTIONAL, INTENT(OUT)   :: OSTRESS
!
REAL, DIMENSION(:,:),   OPTIONAL, INTENT(OUT)   :: PH_TREE
!
REAL, DIMENSION(:,:),   OPTIONAL, INTENT(OUT)   :: PCE_NITRO
REAL, DIMENSION(:,:),   OPTIONAL, INTENT(OUT)   :: PCF_NITRO
REAL, DIMENSION(:,:),   OPTIONAL, INTENT(OUT)   :: PCNA_NITRO
!
TYPE(DATE_TIME), DIMENSION(:,:),   OPTIONAL, INTENT(OUT)   :: TPSEED
TYPE(DATE_TIME), DIMENSION(:,:),   OPTIONAL, INTENT(OUT)   :: TPREAP
!
REAL, DIMENSION(:,:),   OPTIONAL, INTENT(OUT)   :: PWATSUP
REAL, DIMENSION(:,:),   OPTIONAL, INTENT(OUT)   :: PIRRIG
!
!*    0.2    Declaration of local variables
!            ------------------------------
! calculation of veg from lai in the pixel
!
REAL, DIMENSION (:,:), ALLOCATABLE           :: ZWORK      ! work array
!
 CHARACTER(LEN=3)  :: YTREE, YNAT, YLAI, YVEG, YDIF
!
INTEGER :: JLAYER ! loop counter on surface layers
INTEGER :: JVEG   ! loop counter on vegetation types
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------
!
!*    2.      SECONDARY VARIABLES
!             -------------------
!
IF (LHOOK) CALL DR_HOOK('CONVERT_COVER_TEB_VEG',0,ZHOOK_HANDLE)
!
IF (ASSOCIATED(DTCO%XDATA_WEIGHT)) DEALLOCATE(DTCO%XDATA_WEIGHT)
!
IF (HSFTYPE=='NAT') THEN
  YNAT='NAT'
  YTREE='TRE'
  YLAI='LAI'
  YVEG='VEG'
  YDIF='DVG'
ELSEIF (HSFTYPE=='GRD') THEN
  YNAT='GRD'
  YTREE='GRT'
  YLAI='GRL'
  YVEG='GRV'
  YDIF='GDV'
ENDIF
!
!*    2.1     fields on natural surfaces only, taking into account patches/ 
!             -------------------------------
!
!
! Leaf Aera Index
! ---------------
!
IF (PRESENT(PLAI)) THEN
  CALL AV_PGD(DTCO, &
               PLAI ,PCOVER ,XDATA_LAI (:,KDECADE,:),YVEG,'ARI',OCOVER,KDECADE=KDECADE)
ENDIF
!
IF (PRESENT(PLAIGV)) THEN
 CALL AV_PGD(DTCO, &
               PLAIGV ,PCOVER ,XDATA_LAIGV (:,KDECADE,:),YVEG,'ARI',OCOVER,KDECADE=KDECADE) 
ENDIF
!
! 
!* 1/Rsmin
!
IF (PRESENT(PRSMIN)) THEN
  IF (SIZE(PRSMIN)>0) &
    CALL AV_PGD(DTCO, &
               PRSMIN,PCOVER ,XDATA_RSMIN,YLAI,'INV',OCOVER,KDECADE=KDECADE)  
END IF
!
IF (PRESENT(PRSMINGV)) THEN
  IF (SIZE(PRSMINGV)>0) &
   CALL AV_PGD(DTCO, &
               PRSMINGV,PCOVER ,XDATA_RSMINGV,YLAI,'INV',OCOVER,KDECADE=KDECADE) 
END IF
!
IF (PRESENT(PH_TREE)) &
  CALL AV_PGD(DTCO, &
               PH_TREE ,PCOVER ,XDATA_H_TREE (:,:) ,YTREE,'ARI',OCOVER)  
!
DO JVEG=1,NVEGTYPE
  IF (PRESENT(PVEGTYPE)) &
    CALL AV_PGD(DTCO, &
               PVEGTYPE(:,JVEG),PCOVER ,DTCO%XDATA_VEGTYPE(:,JVEG),YNAT,'ARI',OCOVER)  
END DO
!
!
! vegetation fraction
! -------------------
!
IF (PRESENT(PVEG)) &
  CALL AV_PGD(DTCO, &
               PVEG ,PCOVER ,XDATA_VEG (:,KDECADE,:),YNAT,'ARI',OCOVER)  
!
!
IF (PRESENT(PGNDLITTER)) &
 CALL AV_PGD(DTCO, &
               PGNDLITTER ,PCOVER ,XDATA_GNDLITTER (:,KDECADE,:),YNAT,'ARI',OCOVER)
!
! roughness length
! ----------------
!
IF (PRESENT(PZ0)) &
  CALL AV_PGD(DTCO, &
               PZ0 ,PCOVER ,XDATA_Z0 (:,KDECADE,:),YNAT,'CDN',OCOVER)  
!
IF (PRESENT(PZ0_O_Z0H)) &
  CALL AV_PGD(DTCO, &
               PZ0_O_Z0H ,PCOVER ,XDATA_Z0_O_Z0H (:,:),YNAT,'ARI',OCOVER)  
!
IF (PRESENT(PZ0LITTER)) &
 CALL AV_PGD(DTCO, &
               PZ0LITTER ,PCOVER ,XDATA_Z0LITTER (:,KDECADE,:),YNAT,'CDN',OCOVER) 
!
!emis-eco
!--------
!
IF (PRESENT(PEMIS_ECO)) &
  CALL AV_PGD(DTCO, &
               PEMIS_ECO ,PCOVER ,XDATA_EMIS_ECO (:,KDECADE,:),YNAT,'ARI',OCOVER)  
! 
!---------------------------------------------------------------------------------
!
!* other vegetation parameters
!
IF (PRESENT(PGAMMA)) &
  CALL AV_PGD(DTCO, &
               PGAMMA     ,PCOVER ,XDATA_GAMMA   (:,:),YVEG,'ARI',OCOVER,KDECADE=KDECADE)  
IF (PRESENT(PGAMMAGV)) &
 CALL AV_PGD(DTCO, &
               PGAMMAGV     ,PCOVER ,XDATA_GAMMAGV   (:,:),YVEG,'ARI',OCOVER,KDECADE=KDECADE)
IF (PRESENT(PWRMAX_CF)) &
  CALL AV_PGD(DTCO, &
               PWRMAX_CF  ,PCOVER ,XDATA_WRMAX_CF(:,:),YVEG,'ARI',OCOVER,KDECADE=KDECADE)  
IF (PRESENT(PWRMAX_CFGV)) &
 CALL AV_PGD(DTCO, &
               PWRMAX_CFGV  ,PCOVER ,XDATA_WRMAX_CFGV(:,:),YVEG,'ARI',OCOVER,KDECADE=KDECADE)
!
!
IF (PRESENT(PRGL)) &
  CALL AV_PGD(DTCO, &
               PRGL       ,PCOVER ,XDATA_RGL   (:,:),YVEG,'ARI',OCOVER,KDECADE=KDECADE)  
IF (PRESENT(PRGLGV)) &
 CALL AV_PGD(DTCO, &
               PRGLGV       ,PCOVER ,XDATA_RGLGV   (:,:),YVEG,'ARI',OCOVER,KDECADE=KDECADE)
IF (PRESENT(PCV)) &
  CALL AV_PGD(DTCO, &
               PCV        ,PCOVER ,XDATA_CV    (:,:),YVEG,'INV',OCOVER,KDECADE=KDECADE)  
!
IF (PRESENT(PH_VEG)) THEN
 CALL AV_PGD(DTCO, &
               PH_VEG,PCOVER,XDATA_H_VEG(:,KDECADE,:),YVEG,'ARI',OCOVER,KDECADE=KDECADE) 
ENDIF
!

!
!---------------------------------------------------------------------------------
!
!* soil layers
!  -----------
!
IF (PRESENT(PDG)) THEN
!
!* soil layers (and cumulative root fraction for DIF only)
!
  CALL SET_COVER_DG(SIZE(PDG,1),SIZE(PDG,2),SIZE(PDG,3),PRESENT(PPERM),&
                   PRESENT(PDG2),PRESENT(PDROOT),PRESENT(KWG_LAYER),   &
                   PRESENT(PROOTFRAC),PRESENT(PROOTFRACGV)             )
!
END IF
!
!---------------------------------------------------------------------------------
!
!* soil ice for runoff
!  -------------------
!
IF (PRESENT(PD_ICE)) &
 CALL AV_PGD(DTCO, &
               PD_ICE,PCOVER ,XDATA_DICE(:,:),YNAT,'ARI',OCOVER)
!
!---------------------------------------------------------------------------------
!
IF (PRESENT(PALBNIR_VEG)) THEN
  IF (TVG%CALBEDO=='CM13') THEN
    CALL AV_PGD(DTCO, &
               PALBVIS_VEG,PCOVER,XDATA_ALB_VEG_NIR(:,KDECADE,:),YVEG,'ARI',OCOVER,KDECADE=KDECADE)      
  ELSE   
    CALL AV_PGD(DTCO, &
               PALBNIR_VEG,PCOVER ,XDATA_ALBNIR_VEG(:,:),YVEG,'ARI',OCOVER,KDECADE=KDECADE)  
  ENDIF
ENDIF
!
IF (PRESENT(PALBVIS_VEG)) THEN
  IF (TVG%CALBEDO=='CM13') THEN
    CALL AV_PGD(DTCO, &
               PALBVIS_VEG,PCOVER,XDATA_ALB_VEG_VIS(:,KDECADE,:),YVEG,'ARI',OCOVER,KDECADE=KDECADE)      
  ELSE     
    CALL AV_PGD(DTCO, &
               PALBVIS_VEG,PCOVER ,XDATA_ALBVIS_VEG(:,:),YVEG,'ARI',OCOVER,KDECADE=KDECADE)  
  ENDIF
ENDIF
!
IF (PRESENT(PALBUV_VEG)) THEN
  IF ((TVG%CALBEDO=='CM13'.OR.TVG%LTR_ML).AND.PRESENT(PALBVIS_VEG)) THEN
    PALBUV_VEG(:,:)=PALBVIS_VEG(:,:)
  ELSE
    CALL AV_PGD(DTCO, &
               PALBUV_VEG, PCOVER ,XDATA_ALBUV_VEG (:,:),YVEG,'ARI',OCOVER,KDECADE=KDECADE)
  ENDIF
ENDIF
!  
! parameters for "stress option"
IF (HPHOTO == 'AST' .OR. HPHOTO == 'LST' .OR. HPHOTO == 'NIT' .OR.  HPHOTO == 'NCB')  THEN

   IF (PRESENT(PGMES)) THEN
     IF (SIZE(PGMES)>0) &
       CALL AV_PGD(DTCO, &
               PGMES  ,PCOVER ,XDATA_GMES_ST  (:,:),YVEG,'ARI',OCOVER,KDECADE=KDECADE)  
   END IF

   IF (PRESENT(PBSLAI)) THEN
     IF (SIZE(PBSLAI)>0) &
       CALL AV_PGD(DTCO, &
               PBSLAI ,PCOVER ,XDATA_BSLAI_ST (:,:),YVEG,'ARI',OCOVER,KDECADE=KDECADE)  
   END IF

   IF (PRESENT(PSEFOLD)) THEN
     IF (SIZE(PSEFOLD)>0) &
       CALL AV_PGD(DTCO, &
               PSEFOLD,PCOVER ,XDATA_SEFOLD_ST(:,:),YVEG,'ARI',OCOVER,KDECADE=KDECADE)  
   END IF

   IF (PRESENT(PGC)) THEN
     IF (SIZE(PGC)>0) &
       CALL AV_PGD(DTCO, &
               PGC    ,PCOVER ,XDATA_GC_ST    (:,:),YVEG,'ARI',OCOVER,KDECADE=KDECADE)  
   END IF

   IF (PRESENT(PDMAX)) THEN
     IF (SIZE(PDMAX)>0) &
       CALL AV_PGD(DTCO, &
               PDMAX  ,PCOVER ,XDATA_DMAX_ST  (:,:),YTREE,'ARI',OCOVER,KDECADE=KDECADE)  
   END IF

ELSE
   IF (PRESENT(PGMES)) THEN
     IF (SIZE(PGMES)>0) &
       CALL AV_PGD(DTCO, &
               PGMES  ,PCOVER ,XDATA_GMES  (:,:),YVEG,'ARI',OCOVER,KDECADE=KDECADE)  
   END IF
   IF (PRESENT(PBSLAI)) THEN
     IF (SIZE(PBSLAI)>0) &
       CALL AV_PGD(DTCO, &
               PBSLAI ,PCOVER ,XDATA_BSLAI (:,:),YVEG,'ARI',OCOVER,KDECADE=KDECADE)  
   END IF
   IF (PRESENT(PSEFOLD)) THEN
     IF (SIZE(PSEFOLD)>0) &
       CALL AV_PGD(DTCO, &
               PSEFOLD,PCOVER ,XDATA_SEFOLD(:,:),YVEG,'ARI',OCOVER,KDECADE=KDECADE)  
   END IF
   IF (PRESENT(PGC)) THEN
     IF (SIZE(PGC)>0) &
       CALL AV_PGD(DTCO, &
               PGC    ,PCOVER ,XDATA_GC    (:,:),YVEG,'ARI',OCOVER,KDECADE=KDECADE)  
   END IF
   IF (PRESENT(PDMAX)) THEN
     IF (SIZE(PDMAX)>0) &
       CALL AV_PGD(DTCO, &
               PDMAX  ,PCOVER ,XDATA_DMAX  (:,:),YTREE,'ARI',OCOVER,KDECADE=KDECADE)  
   END IF

ENDIF

IF (PRESENT(PRE25)) THEN
  IF (SIZE(PRE25)>0) &
    CALL AV_PGD(DTCO, &
               PRE25  ,PCOVER ,XDATA_RE25  (:,:),YNAT,'ARI',OCOVER,KDECADE=KDECADE)  
END IF

IF (PRESENT(PLAIMIN)) THEN
  IF (SIZE(PLAIMIN)>0) &
    CALL AV_PGD(DTCO, &
               PLAIMIN,PCOVER ,XDATA_LAIMIN(:,:),YVEG,'ARI',OCOVER,KDECADE=KDECADE)  
END IF
IF (PRESENT(PCE_NITRO)) THEN
  IF (SIZE(PCE_NITRO)>0) &
    CALL AV_PGD(DTCO, &
               PCE_NITRO  ,PCOVER ,XDATA_CE_NITRO  (:,:),YVEG,'ARI',OCOVER,KDECADE=KDECADE)  
END IF
IF (PRESENT(PCF_NITRO)) THEN
  IF (SIZE(PCF_NITRO)>0) &
    CALL AV_PGD(DTCO, &
               PCF_NITRO  ,PCOVER ,XDATA_CF_NITRO  (:,:),YVEG,'ARI',OCOVER,KDECADE=KDECADE)  
END IF
IF (PRESENT(PCNA_NITRO)) THEN
  IF (SIZE(PCNA_NITRO)>0) &
    CALL AV_PGD(DTCO, &
               PCNA_NITRO  ,PCOVER ,XDATA_CNA_NITRO(:,:),YVEG,'ARI',OCOVER,KDECADE=KDECADE)  
END IF
IF (PRESENT(PF2I)) THEN
  IF (SIZE(PF2I)>0) &
    CALL AV_PGD(DTCO, &
               PF2I   ,PCOVER ,XDATA_F2I   (:,:),YVEG,'ARI',OCOVER,KDECADE=KDECADE)  
END IF
!
IF (PRESENT(OSTRESS)) THEN
  IF (SIZE(OSTRESS)>0) THEN
    ALLOCATE(ZWORK(SIZE(OSTRESS,1),SIZE(OSTRESS,2)))
    CALL AV_PGD(DTCO, &
               ZWORK,PCOVER ,XDATA_STRESS(:,:),YVEG,'ARI',OCOVER,KDECADE=KDECADE)
    WHERE (ZWORK<0.5) 
      OSTRESS = .FALSE.
    ELSEWHERE
      OSTRESS = .TRUE.
    END WHERE
    DEALLOCATE(ZWORK)
  END IF
END IF
!
IF (HPHOTO == 'LAI' .OR. HPHOTO == 'LST' .OR. HPHOTO == 'NIT')  THEN
   !
   ! date of seeding
   ! ---------------
   !
   IF (PRESENT(TPSEED)) THEN
     IF (SIZE(TPSEED)>0) &
       CALL AV_PGD (TPSEED ,PCOVER ,TDATA_SEED(:,:),YVEG,'MAJ',OCOVER,KDECADE=KDECADE)  
   END IF
   !
   ! date of reaping
   ! ---------------
   !
   IF (PRESENT(TPREAP)) THEN
     IF (SIZE(TPREAP)>0) &
       CALL AV_PGD (TPREAP ,PCOVER ,TDATA_REAP(:,:),YVEG,'MAJ',OCOVER,KDECADE=KDECADE)  
   END IF
   !
   ! fraction of irrigated surface
   ! ---------------------------
   !
   IF (PRESENT(PIRRIG)) THEN
     IF (SIZE(PIRRIG)>0) &
       CALL AV_PGD(DTCO, &
               PIRRIG ,PCOVER ,XDATA_IRRIG(:,:),YVEG,'ARI',OCOVER,KDECADE=KDECADE)  
   END IF
   !
   ! water supply for irrigation
   ! ---------------------------
   !
   IF (PRESENT(PWATSUP)) THEN
     IF (SIZE(PWATSUP)>0) &
       CALL AV_PGD(DTCO, &
               PWATSUP ,PCOVER ,XDATA_WATSUP(:,:),YVEG,'ARI',OCOVER,KDECADE=KDECADE)  
   END IF
!
END IF
!
IF (ASSOCIATED(DTCO%XDATA_WEIGHT)) DEALLOCATE(DTCO%XDATA_WEIGHT)
!
IF (LHOOK) CALL DR_HOOK('CONVERT_COVER_TEB_VEG',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
CONTAINS
!-------------------------------------------------------------------------------
!
SUBROUTINE SET_COVER_DG(KNI,KGROUND,KPATCH,LPERM,LDG2,LDROOT,LWG_LAYER,LROOTFRAC, &
                        LROOTFRACGV                                               )
!
USE MODD_SURF_PAR,       ONLY : XUNDEF, NUNDEF
!
USE MODD_REPROD_OPER,    ONLY : CDGAVG, CDGDIF
!
USE MODI_INI_DATA_ROOTFRAC
USE MODI_INI_DATA_SOIL
USE MODI_PERMAFROST_DEPTH
!
IMPLICIT NONE
!
INTEGER, INTENT(IN) :: KNI
INTEGER, INTENT(IN) :: KGROUND
INTEGER, INTENT(IN) :: KPATCH
LOGICAL, INTENT(IN) :: LPERM
LOGICAL, INTENT(IN) :: LDG2
LOGICAL, INTENT(IN) :: LDROOT
LOGICAL, INTENT(IN) :: LWG_LAYER
LOGICAL, INTENT(IN) :: LROOTFRAC
LOGICAL, INTENT(IN) :: LROOTFRACGV
!
REAL, DIMENSION (SIZE(XDATA_GROUND_DEPTH,1),SIZE(XDATA_GROUND_DEPTH,2)) :: ZDATA_GROUND_DEPTH
REAL, DIMENSION (SIZE(XDATA_ROOT_DEPTH  ,1),3,SIZE(XDATA_ROOT_DEPTH,2)) :: ZDATA_DG
!
INTEGER, DIMENSION (KNI,KPATCH) :: IWG_LAYER
REAL, DIMENSION (KNI,KPATCH) :: ZDTOT, ZDROOT      !  work array
REAL, DIMENSION (KNI,KPATCH) :: ZROOT_EXT  !        "
REAL, DIMENSION (KNI,KPATCH) :: ZROOT_LIN  !        "
!
INTEGER :: JPATCH, JJ, JVEGTYPE
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
IF (LHOOK) CALL DR_HOOK('CONVERT_COVER_ISBA:SET_COVER_DG',0,ZHOOK_HANDLE)
!
ZDTOT    (:,:) = XUNDEF
ZDROOT   (:,:) = XUNDEF
ZROOT_EXT(:,:) = XUNDEF
ZROOT_LIN(:,:) = XUNDEF
IWG_LAYER(:,:) = NUNDEF
!
ZDATA_GROUND_DEPTH(:,:) = XDATA_GROUND_DEPTH(:,:)
!
!####################################################################################
!
!CDGAVG : old for reprod = 'ARI' Arithmetic average for all depth 
!         recommended    = 'INV' Harmonic average for all depth (default)
!
!CDGDIF : old for reprod = 'SOIL' d3 soil depth from ecoclimap for isba-df
!         recommended    = 'ROOT' d2 soil depth from ecoclimap for isba-df (default)
!
!####################################################################################
!
IF(HISBA/='DIF')THEN
  !  
  CALL INI_DATA_SOIL(HISBA, ZDATA_DG,                             &
                     PSURF      = DTCO%XDATA_NATURE,                   &
                     PSURF2     = DTCO%XDATA_GARDEN,                   &
                     PROOTDEPTH = XDATA_ROOT_DEPTH,               &
                     PSOILDEPTH = XDATA_GROUND_DEPTH              )
  !
  DO JLAYER=1,KGROUND
     CALL AV_PGD(DTCO, &
               PDG(:,JLAYER,:),PCOVER,ZDATA_DG(:,JLAYER,:),YNAT,'ARI',OCOVER,KDECADE=KDECADE)
  ENDDO
  !
ELSE
!
  IF(CDGDIF=='ROOT')THEN
    DO JVEGTYPE=1,NVEGTYPE
       IF(JVEGTYPE==NVT_NO)THEN
          WHERE(XDATA_GROUND_DEPTH(:,JVEGTYPE)/=XUNDEF)
               ZDATA_GROUND_DEPTH(:,JVEGTYPE) = MIN(1.0,XDATA_GROUND_DEPTH(:,JVEGTYPE))
          ENDWHERE
       ELSEIF(JVEGTYPE/=NVT_ROCK.AND.JVEGTYPE/=NVT_SNOW)THEN
         ZDATA_GROUND_DEPTH(:,JVEGTYPE) = MAX(1.0,XDATA_ROOT_DEPTH(:,JVEGTYPE))          
       ELSE
         ZDATA_GROUND_DEPTH(:,JVEGTYPE) = XDATA_ROOT_DEPTH(:,JVEGTYPE)
       ENDIF
    ENDDO
  ENDIF
!
  CALL AV_PGD(DTCO, &
               ZDTOT (:,:),PCOVER,ZDATA_GROUND_DEPTH,YNAT,CDGAVG,OCOVER,KDECADE=KDECADE)
!  
! CALCULATION OF GROUND_DEPTH over Permafrost area
  IF(LPERM)THEN
    CALL PERMAFROST_DEPTH(KNI,KPATCH,PPERM,ZDTOT)
  ENDIF
!  
  IF (LDG2) THEN
     IF(CDGDIF=='ROOT')THEN
       CALL AV_PGD(DTCO, &
               PDG2(:,:),PCOVER,ZDATA_GROUND_DEPTH,YNAT,CDGAVG,OCOVER)
     ELSE
       CALL AV_PGD(DTCO, &
               PDG2(:,:),PCOVER,XDATA_ROOT_DEPTH,YNAT,CDGAVG,OCOVER)
     ENDIF
  ENDIF
  IF (LDROOT .OR. LROOTFRAC .OR. LROOTFRACGV .OR. (CDGDIF=='ROOT')) THEN
    CALL AV_PGD(DTCO, &
               ZDROOT(:,:),PCOVER,XDATA_ROOT_DEPTH,YDIF,CDGAVG,OCOVER,KDECADE=KDECADE)
    IF (LDROOT) PDROOT(:,:) = ZDROOT(:,:)
    IF (CDGDIF=='ROOT') WHERE(ZDROOT(:,:).NE.XUNDEF) ZDTOT(:,:) = MAX(ZDROOT(:,:),ZDTOT(:,:))
  ENDIF
!
  CALL INI_DATA_SOIL(HISBA, PDG, PSOILDEPTH=ZDTOT, PSOILGRID=PSOILGRID, &
                     KWG_LAYER=IWG_LAYER  )
  IF (LWG_LAYER) KWG_LAYER(:,:) = IWG_LAYER(:,:)
!
  IF (LROOTFRAC .OR. LROOTFRACGV) THEN
!      
    CALL AV_PGD(DTCO, &
               ZROOT_LIN(:,:),PCOVER,XDATA_ROOT_LIN(:,:),YDIF,'ARI',OCOVER,KDECADE=KDECADE)
    IF (LROOTFRAC) THEN
      CALL AV_PGD(DTCO, &
               ZROOT_EXT(:,:),PCOVER,XDATA_ROOT_EXTINCTION(:,:),YDIF,'ARI',OCOVER,KDECADE=KDECADE)
      CALL INI_DATA_ROOTFRAC(PDG,PDROOT,ZROOT_EXT,ZROOT_LIN,PROOTFRAC)
    ENDIF
    IF (LROOTFRACGV) THEN
      CALL AV_PGD(DTCO, &
               ZROOT_EXT(:,:),PCOVER,XDATA_ROOT_EXTINCTIONGV(:,:),YDIF,'ARI',OCOVER,KDECADE=KDECADE)
      CALL INI_DATA_ROOTFRAC(PDG,PDROOT,ZROOT_EXT,ZROOT_LIN, &
                             PROOTFRACGV,OGV=LROOTFRACGV)
    ENDIF
!
  ENDIF
!
ENDIF
!
IF (LHOOK) CALL DR_HOOK('CONVERT_COVER_TEB_VEG:SET_COVER_DG',1,ZHOOK_HANDLE)
END SUBROUTINE SET_COVER_DG
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE CONVERT_COVER_TEB_VEG
