!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
    SUBROUTINE LANDUSE_BIOMASS(IG, IO, S, NP, NPE, TLU, KLUOUT,  &
                               KI, PBIO_GRID_OLD, PBIO_GRID_NEW, &
                               PLULCC_HARVEST_GRID               )
!   ###############################################################
!!****  *LAND USE BIOMASS*
!!
!!    PURPOSE
!!    -------
!
!     Update and conserv biomass after land-use change
!               
!!**  METHOD
!!    ------
!!
!!    EXTERNAL
!!    --------
!!    none
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!      
!!    none
!!
!!    REFERENCE
!!    ---------
!!
!!      
!!    AUTHOR
!!    ------
!!      B. Decharme   *Meteo France*
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    11/2020    
!!
!-------------------------------------------------------------------------------
!
!*       0.     DECLARATIONS
!               ------------
!
USE MODD_SFX_GRID_n,     ONLY : GRID_t
USE MODD_ISBA_OPTIONS_n, ONLY : ISBA_OPTIONS_t
USE MODD_ISBA_n,         ONLY : ISBA_S_t, ISBA_P_t, ISBA_PE_t, ISBA_K_t, ISBA_NP_t, ISBA_NPE_t
USE MODD_INIT_LANDUSE,   ONLY : LULCC_P_t, LULCC_NP_t
!
USE MODD_CO2V_PAR, ONLY : XPCCO2, XKGTOG, XGTOKG
!
USE MODD_SURF_PAR,ONLY : XUNDEF                 
!
USE MODI_ABOR1_SFX
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
TYPE(GRID_t),          INTENT(INOUT) :: IG
TYPE(ISBA_OPTIONS_t),  INTENT(INOUT) :: IO
TYPE(ISBA_S_t),        INTENT(INOUT) :: S
TYPE(ISBA_NP_t),       INTENT(INOUT) :: NP
TYPE(ISBA_NPE_t),      INTENT(INOUT) :: NPE
TYPE(LULCC_NP_t),      INTENT(INOUT) :: TLU
!
INTEGER,               INTENT(IN)    :: KLUOUT
INTEGER,               INTENT(IN)    :: KI
!
REAL, DIMENSION(:),    INTENT(OUT)   :: PBIO_GRID_OLD       ! previous year grid_cell Biomass  (kg/m2)
REAL, DIMENSION(:),    INTENT(OUT)   :: PBIO_GRID_NEW       ! current year grid_cell Biomass  (kg/m2)
REAL, DIMENSION(:),    INTENT(OUT)   :: PLULCC_HARVEST_GRID ! Grid_cell carbon due to biomass harvested after land use change (kgC/m2)
!
!*      0.2    declarations of local parameter
!
TYPE(ISBA_P_t),  POINTER :: PK
TYPE(ISBA_PE_t), POINTER :: PEK
TYPE(LULCC_P_t), POINTER :: OLD
!
REAL, DIMENSION(KI,IO%NNBIOMASS,IO%NPATCH) :: ZTURNOVER    ! gC/m2
!
REAL, DIMENSION(KI,IO%NNBIOMASS)   :: ZBIOMASS_RESERV_OLD  ! Kg/m2
REAL, DIMENSION(KI,IO%NNBIOMASS)   :: ZBIOMASS_RESERV_NEW  ! Kg/m2
REAL, DIMENSION(KI,IO%NNBIOMASS)   :: ZTURNOVER_GRID       ! gC/m2

REAL, DIMENSION(KI)                :: ZBUDGET, ZCONSERV
REAL, DIMENSION(KI)                :: ZTURNOVER_TOT
!
REAL, DIMENSION(KI)                :: ZOLDVEG_GRID
REAL, DIMENSION(KI)                :: ZNEWVEG_GRID
!
LOGICAL :: LSTOP
!
INTEGER :: INP, INB, IMASK  ! loop counter on levels
INTEGER :: JI, JP, JNB      ! loop counter on levels
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-----------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('LANDUSE_BIOMASS',0,ZHOOK_HANDLE)
!-----------------------------------------------------------------
!
!*      1.     Preliminaries
!              -------------
!
INB=IO%NNBIOMASS
INP=IO%NPATCH
!
PBIO_GRID_OLD       (:) = 0.0
PBIO_GRID_NEW       (:) = 0.0
PLULCC_HARVEST_GRID(:) = 0.0
!
ZCONSERV     (:) = 0.0
ZBUDGET      (:) = 0.0
ZTURNOVER_TOT(:) = 0.0
ZNEWVEG_GRID (:) = 0.0
ZOLDVEG_GRID (:) = 0.0
!
ZBIOMASS_RESERV_OLD(:,:) = 0.0
ZBIOMASS_RESERV_NEW(:,:) = 0.0
ZTURNOVER_GRID     (:,:) = 0.0
!
ZTURNOVER(:,:,:) = 0.0
!
!Biomass in kg/m2
!
DO JP=1,INP
   PK => NP%AL(JP)
   PEK => NPE%AL(JP)
   DO JNB=1,INB
      DO JI=1,PK%NSIZE_P
         PEK%XFLURES(JI)=0.0
         IF(PK%XPATCH(JI)==0)THEN
            PEK%XBIOMASS(JI,JNB)=0.0
         ENDIF
      ENDDO
   ENDDO
ENDDO
!
DO JP=1,INP
   PK => NP%AL(JP)
   PEK => NPE%AL(JP)
   DO JNB=1,INB
      DO JI=1,PK%NSIZE_P
         IMASK = PK%NR_P(JI)
         PBIO_GRID_OLD(IMASK) = PBIO_GRID_OLD(IMASK) + OLD%BIOMASS(JI,JNB)*OLD%PATCH(JI)
      ENDDO
   ENDDO
ENDDO
!
!-----------------------------------------------------------------
!
!*      2.     NIT case : Simple global grid-cell conservation for biomass
!              -----------------------------------------------------------
!
IF(IO%CPHOTO=='NIT')THEN
!
  DO JP=1,INP
     PK => NP%AL(JP)
     PEK => NPE%AL(JP)
     OLD => TLU%AL(JP)
     DO JNB=1,INB
        DO JI=1,PK%NSIZE_P
           IMASK = PK%NR_P(JI)
           ZBIOMASS_RESERV_OLD(IMASK,JNB) = ZBIOMASS_RESERV_OLD(IMASK,JNB) + OLD%BIOMASS (JI,JNB)*OLD%PATCH(JI)
           ZBIOMASS_RESERV_NEW(IMASK,JNB) = ZBIOMASS_RESERV_NEW(IMASK,JNB) + PEK%XBIOMASS(JI,JNB)*PK%XPATCH(JI)
        ENDDO
     ENDDO
  ENDDO
!
  DO JP=1,INP 
     PK => NP%AL(JP)
     PEK => NPE%AL(JP)
     DO JNB=1,INB
        DO JI=1,PK%NSIZE_P
           IMASK = PK%NR_P(JI)
           IF(ZBIOMASS_RESERV_NEW(IMASK,JNB)/=ZBIOMASS_RESERV_OLD(IMASK,JNB).AND.ZBIOMASS_RESERV_NEW(IMASK,JNB)/= 0.0)THEN
             PEK%XBIOMASS(JI,JNB) = PEK%XBIOMASS(JI,JNB) * ZBIOMASS_RESERV_OLD(IMASK,JNB)/ZBIOMASS_RESERV_NEW(IMASK,JNB)
          ENDIF
        ENDDO
     ENDDO
  ENDDO
!
ENDIF
!
!-----------------------------------------------------------------
!
!*      3.     ISBA-CC case : Update current year biomass
!              ------------------------------------------
!   
IF(IO%CPHOTO=='NCB')THEN  
!
!The patch has grown = we "sow" a seed of zero biomass
!Biomass=[ Biomass*Patch_old + 0.0 * (Patch_new-Patch_old) ] / (Patch_old+Patch_new-Patch_old) = Biomass*Patch_old/Patch_new                     
!
  DO JP=1,INP  
     PK => NP%AL(JP)
     PEK => NPE%AL(JP)
     OLD => TLU%AL(JP)
     DO JNB=1,INB
        DO JI=1,PK%NSIZE_P
           IF(PK%XPATCH(JI)>OLD%PATCH(JI).AND.OLD%PATCH(JI)>0.0)THEN
             PEK%XBIOMASS(JI,JNB) = OLD%BIOMASS(JI,JNB) * OLD%PATCH(JI) / PK%XPATCH(JI)
           ENDIF
        ENDDO
     ENDDO
  ENDDO
!
!The patch has diminished: we conserve biomass density                      
!Bio_new =[ Bio_old*Patch_old - Bio_old * (Patch_old-Patch_new) ] / Patch_new = Bio_old 
!
  DO JP=1,INP 
!
     PK => NP%AL(JP)
     PEK => NPE%AL(JP)
     OLD => TLU%AL(JP)
!
     DO JI=1,PK%NSIZE_P
!
        IMASK = PK%NR_P(JI)
        IF(PK%XPATCH(JI)<OLD%PATCH(JI).AND.OLD%PATCH(JI)>0.0)THEN
!          
!         we harvest the leaves and stems based on the old grid (kgC/m2 including patch)
!         Carbon due to biomass harvested after land use change including patch fraction (kgC/m2)
          OLD%LULCC_HARVEST(JI) = (OLD%BIOMASS(JI,1)+OLD%BIOMASS(JI,2)+OLD%BIOMASS(JI,3)+OLD%BIOMASS(JI,5)) &
                                * XPCCO2 * (OLD%PATCH(JI)-PK%XPATCH(JI))
!     
!         we leave roots to die (gC/m2 including patch)
          ZTURNOVER(IMASK,4,JP) = OLD%BIOMASS(JI,4) * (OLD%PATCH(JI)-PK%XPATCH(JI)) * (XPCCO2*XKGTOG)
          ZTURNOVER(IMASK,6,JP) = OLD%BIOMASS(JI,6) * (OLD%PATCH(JI)-PK%XPATCH(JI)) * (XPCCO2*XKGTOG)
!
        ENDIF
!  
     ENDDO
!
  ENDDO
!
! Biomass carbon transferred to litter pools due to lulucf processes (kgC m-2)
!
  DO JP=1,INP  
     PK => NP%AL(JP)
     PEK => NPE%AL(JP)
     DO JI=1,PK%NSIZE_P
        IMASK = PK%NR_P(JI)
        PEK%XFLURES(JI) = (ZTURNOVER(IMASK,4,JP)+ZTURNOVER(IMASK,6,JP)) * XGTOKG
     ENDDO
  ENDDO
!
! We add them to all vegetated turnover in the new grid (gC/m2)
!
  ZOLDVEG_GRID  (:  ) = 0.0
  ZNEWVEG_GRID  (:  ) = 0.0
  ZTURNOVER_GRID(:,:) = 0.0
  DO JP=1,INP  
     PK => NP%AL(JP)
     PEK => NPE%AL(JP)
     OLD => TLU%AL(JP)
     DO JI=1,PK%NSIZE_P
        IMASK = PK%NR_P(JI)
        IF(OLD%LAI(JI)/=XUNDEF)THEN
          ZOLDVEG_GRID(IMASK)=ZOLDVEG_GRID(IMASK)+OLD%PATCH(JI)
        ENDIF       
        IF(PEK%XLAI(JI)/=XUNDEF)THEN
          ZNEWVEG_GRID(IMASK)=ZNEWVEG_GRID(IMASK)+PK%XPATCH(JI)
        ENDIF       
        ZTURNOVER_GRID(IMASK,4)=ZTURNOVER_GRID(IMASK,4)+ZTURNOVER(IMASK,4,JP)
        ZTURNOVER_GRID(IMASK,6)=ZTURNOVER_GRID(IMASK,6)+ZTURNOVER(IMASK,6,JP)
     ENDDO
  ENDDO
!
  DO JP=1,INP  
     PK => NP%AL(JP)
     PEK => NPE%AL(JP)
     OLD => TLU%AL(JP)
     DO JI=1,PK%NSIZE_P
        IMASK = PK%NR_P(JI)
        IF(ZNEWVEG_GRID(IMASK)>0.0.AND.PEK%XLAI(JI)/=XUNDEF)THEN
          OLD%TURNOVER(JI,4)=ZTURNOVER_GRID(IMASK,4)/ZNEWVEG_GRID(IMASK)
          OLD%TURNOVER(JI,6)=ZTURNOVER_GRID(IMASK,6)/ZNEWVEG_GRID(IMASK)
        ELSEIF(ZOLDVEG_GRID(IMASK)>0.0.AND.ZNEWVEG_GRID(IMASK)==0.0)THEN ! all vegetated patches disappeared
          ZCONSERV(IMASK) = ZCONSERV(IMASK) + (ZTURNOVER(IMASK,4,JP)+ZTURNOVER(IMASK,6,JP))* XGTOKG
        ENDIF
     ENDDO
  ENDDO 
!
! The patch is new : 
!
  DO JP=1,INP  
     PK => NP%AL(JP)
     PEK => NPE%AL(JP)
     OLD => TLU%AL(JP)
     DO JI=1,PK%NSIZE_P
        IMASK = PK%NR_P(JI)
!       We use a negative turnover to conserv global carbon stock
        IF(PK%XPATCH(JI)>0.0.AND.OLD%PATCH(JI)==0.0)THEN
              OLD%TURNOVER(JI,1) = - PEK%XBIOMASS(JI,1) * (XPCCO2*XKGTOG)
              OLD%TURNOVER(JI,2) = - PEK%XBIOMASS(JI,2) * (XPCCO2*XKGTOG)
        ENDIF
!       Biomass carbon transferred to litter pools due to lulucf processes (kgC m-2)
        PEK%XFLURES(JI) = PEK%XFLURES(JI)+(OLD%TURNOVER(JI,1)+OLD%TURNOVER(JI,2))*PK%XPATCH(JI)*XGTOKG
     ENDDO
  ENDDO 
!
  ZTURNOVER_TOT(:)=0.0
  DO JP=1,INP  
     PK => NP%AL(JP)
     PEK => NPE%AL(JP)
     OLD => TLU%AL(JP)
     DO JI=1,PK%NSIZE_P
        IMASK = PK%NR_P(JI)
        PLULCC_HARVEST_GRID(IMASK)=PLULCC_HARVEST_GRID(IMASK)+OLD%LULCC_HARVEST(JI)
        ZTURNOVER_TOT      (IMASK)=ZTURNOVER_TOT      (IMASK)+(OLD%TURNOVER(JI,1)+OLD%TURNOVER(JI,2) &
                                                             + OLD%TURNOVER(JI,4)+OLD%TURNOVER(JI,6))&
                                                             * PK%XPATCH(JI)*(XGTOKG/XPCCO2)
     ENDDO
  ENDDO
!
ENDIF
!
!-----------------------------------------------------------------
!
!*      4.     Compute current year total grid-cell biomass
!              --------------------------------------------
!
! * Biomass in kg/m2
!
DO JP=1,INP
   PK => NP%AL(JP)
   PEK => NPE%AL(JP)
   DO JNB=1,INB
      DO JI=1,PK%NSIZE_P
         IMASK = PK%NR_P(JI)
         PBIO_GRID_NEW(IMASK) = PBIO_GRID_NEW(IMASK) + PEK%XBIOMASS(JI,JNB)*PK%XPATCH(JI)
      ENDDO
   ENDDO
ENDDO
!
!-----------------------------------------------------------------
!
!*      7.     Test total biomass conservation
!              -------------------------------
!
!* Biomass conservation (kg/m2)
!
S%XCCONSRV(:) = S%XCCONSRV(:) + ZCONSERV(:)
!
ZBUDGET(:) = PBIO_GRID_NEW(:)-PBIO_GRID_OLD(:)
!
IF(IO%CPHOTO=='NCB')THEN  
  ZBUDGET(:) = ZBUDGET(:) + ZTURNOVER_TOT(:) + (ZCONSERV(:)+PLULCC_HARVEST_GRID(:))/XPCCO2
ENDIF
!
LSTOP=.FALSE.
DO JI=1,KI
   IF(ABS(ZBUDGET(JI))>1.E-12)THEN
     WRITE(KLUOUT,*)'!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
     WRITE(KLUOUT,*)'LANDUSE_BIOMASS: NO CONSERVATION IN AT LEAST ONE GRID CELL'
     WRITE(KLUOUT,*)'LON = ',IG%XLON(JI),' LAT =',IG%XLAT(JI),'BUDGET =',ZBUDGET(JI),'kg/m2'
     WRITE(KLUOUT,*)'!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
     LSTOP=.TRUE.
   ENDIF           
ENDDO
IF(LSTOP) CALL ABOR1_SFX('LANDUSE_BIOMASS: INCONSISTENCY IN BIOMASS BUDGET')
!
IF (LHOOK) CALL DR_HOOK('LANDUSE_BIOMASS',1,ZHOOK_HANDLE)
!-----------------------------------------------------------------
!
END SUBROUTINE LANDUSE_BIOMASS
