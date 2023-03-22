!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
    SUBROUTINE LANDUSE_CARBON_DIF(IO, S, NP, NPE, TLU, KI,          &
                                  PLITTER_GRID_OLD,PCSOIL_GRID_OLD, &
                                  PLITTER_GRID_NEW,PCSOIL_GRID_NEW, &
                                  PLULCC_HARVEST_GRID               )
!   #############################################################
!!****  *LAND USE CARBON*
!!
!!    PURPOSE
!!    -------
!
!     Update and conserv carbon stocks after land-use change for ISBA-CC
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
USE MODD_ISBA_OPTIONS_n, ONLY : ISBA_OPTIONS_t
USE MODD_ISBA_n,         ONLY : ISBA_S_t, ISBA_P_t, ISBA_PE_t, ISBA_K_t, ISBA_NP_t, ISBA_NPE_t
USE MODD_INIT_LANDUSE,   ONLY : LULCC_P_t, LULCC_NP_t
!
USE MODD_SURF_PAR,       ONLY : XUNDEF,NUNDEF
!
USE MODD_CO2V_PAR,       ONLY : XGTOKG
!
USE MODI_BIOMASS_TO_SURFACE_LITTER
USE MODI_BIOMASS_TO_SOIL_LITTER
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
TYPE(ISBA_OPTIONS_t),  INTENT(INOUT) :: IO
TYPE(ISBA_S_t),        INTENT(INOUT) :: S
TYPE(ISBA_NP_t),       INTENT(INOUT) :: NP
TYPE(ISBA_NPE_t),      INTENT(INOUT) :: NPE
TYPE(LULCC_NP_t),      INTENT(INOUT) :: TLU
!
INTEGER,               INTENT(IN)    :: KI
!
REAL, DIMENSION(:),    INTENT(OUT)   :: PLITTER_GRID_OLD
REAL, DIMENSION(:),    INTENT(OUT)   :: PCSOIL_GRID_OLD
REAL, DIMENSION(:),    INTENT(OUT)   :: PLITTER_GRID_NEW
REAL, DIMENSION(:),    INTENT(OUT)   :: PCSOIL_GRID_NEW
REAL, DIMENSION(:),    INTENT(INOUT) :: PLULCC_HARVEST_GRID ! Grid_cell carbon due to biomass harvested after land use change (kgC/m2)
!
!
!*      0.2    declarations of local parameter
!
TYPE(ISBA_P_t),  POINTER :: PK
TYPE(ISBA_PE_t), POINTER :: PEK
TYPE(LULCC_P_t), POINTER :: OLD
!
REAL, DIMENSION(KI)                       :: ZOLDVEG_GRID
REAL, DIMENSION(KI)                       :: ZNEWVEG_GRID
!
REAL, DIMENSION(KI)                       :: ZCSURF_LIGNIN_NEW    ! current year surface lignin C stock
REAL, DIMENSION(KI)                       :: ZCSURF_LIGNIN_OLD    ! previous year surface lignin C stock
!
REAL, DIMENSION(KI)                       :: ZCSOIL_LIGNIN_NEW    ! current year lignin C stock
REAL, DIMENSION(KI)                       :: ZCSOIL_LIGNIN_OLD    ! current year lignin C stock
!
REAL, DIMENSION(KI,IO%NNLITTER)           :: ZCSURF_LITTER_NEW    ! current year surface litter C stock
REAL, DIMENSION(KI,IO%NNLITTER)           :: ZCSURF_LITTER_OLD    ! previous year surface litter C stock
!
REAL, DIMENSION(KI,IO%NNLITTER)           :: ZCSOIL_LITTER_NEW    ! current year litter C stock
REAL, DIMENSION(KI,IO%NNLITTER)           :: ZCSOIL_LITTER_OLD    ! previous year litter C stock
!
REAL, DIMENSION(KI,IO%NNSOILCARB)         :: ZCSOIL_RESERV_NEW  ! current  year soil C stock
REAL, DIMENSION(KI,IO%NNSOILCARB)         :: ZCSOIL_RESERV_OLD  ! previous year soil C stock
!
REAL, DIMENSION(KI,IO%NNLITTER,IO%NPATCH) :: ZHARVEST
!
INTEGER :: INL, INP, INLIT, INC, IDEPTH, IDEPTH_OLD ! loop counter on levels
INTEGER :: JI, JL, JP, JNL, JNC, IMASK              ! loop counter on levels
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-----------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('LANDUSE_CARBON_DIF',0,ZHOOK_HANDLE)
!-----------------------------------------------------------------
!
!*      1.     Preliminaries
!              -------------
!
INL  =IO%NGROUND_LAYER
INP  =IO%NPATCH
INLIT=IO%NNLITTER
INC  =IO%NNSOILCARB
!
PLITTER_GRID_OLD (:) = 0.0
PCSOIL_GRID_OLD  (:) = 0.0
!
PLITTER_GRID_NEW (:) = 0.0
PCSOIL_GRID_NEW  (:) = 0.0
!
ZCSURF_LIGNIN_OLD(:)   = 0.0
ZCSOIL_LIGNIN_OLD(:)   = 0.0
!
ZCSURF_LITTER_OLD(:,:) = 0.0
ZCSOIL_LITTER_OLD(:,:) = 0.0
ZCSOIL_RESERV_OLD(:,:) = 0.0
!
ZCSURF_LITTER_NEW(:,:) = 0.0
ZCSOIL_LITTER_NEW(:,:) = 0.0
ZCSOIL_RESERV_NEW(:,:) = 0.0
!
ZHARVEST   (:,:,:)=0.0
!
!No carbon where new patch = 0.0
!
DO JP=1,INP
   PK => NP%AL(JP)
   PEK => NPE%AL(JP)
   DO JI=1,PK%NSIZE_P 
      IF(PK%XPATCH(JI)==0.0)THEN
        PEK%XSURFACE_LIGNIN_STRUC(JI)=XUNDEF
      ENDIF
   ENDDO
ENDDO
!
DO JP=1,INP
   PK => NP%AL(JP)
   PEK => NPE%AL(JP)
   DO JL=1,INL
      DO JI=1,PK%NSIZE_P
         IF(PK%XPATCH(JI)==0.0)THEN
           PEK%XSOILDIF_LIGNIN_STRUC(JI,JL)=XUNDEF
         ENDIF
      ENDDO
   ENDDO
ENDDO  
!  
DO JP=1,INP
   PK => NP%AL(JP)
   PEK => NPE%AL(JP)
   DO JNL=1,INLIT
      DO JI=1,PK%NSIZE_P 
         IF(PK%XPATCH(JI)==0.0)THEN
            PEK%XSURFACE_LITTER(JI,JNL)=XUNDEF
         ENDIF
      ENDDO
   ENDDO
ENDDO  
DO JP=1,INP
   PK => NP%AL(JP)
   PEK => NPE%AL(JP)
   OLD => TLU%AL(JP)
   DO JNL=1,INLIT
      DO JI=1,PK%NSIZE_P      
         IMASK=PK%NR_P(JI)
         IF(OLD%PATCH(JI)>0.0.AND.PK%XPATCH(JI)==0.0)THEN ! Patch disappear = surface litter is harvested
            ZHARVEST(IMASK,JNL,JP)=ZHARVEST(IMASK,JNL,JP)+OLD%SURF_LITTER(JI,JNL)*OLD%PATCH(JI)
         ENDIF         
      ENDDO
   ENDDO
ENDDO  
!
DO JP=1,INP
   PK => NP%AL(JP)
   PEK => NPE%AL(JP)
   DO JNL=1,INLIT
      DO JL=1,INL
         DO JI=1,PK%NSIZE_P
            IF(PK%XPATCH(JI)==0.0)THEN
               PEK%XSOILDIF_LITTER(JI,JL,JNL)=XUNDEF
            ENDIF
         ENDDO
      ENDDO
   ENDDO
ENDDO
!
DO JP=1,INP
   PK => NP%AL(JP)
   PEK => NPE%AL(JP)
   DO JNC=1,INC
      DO JL=1,INL
         DO JI=1,PK%NSIZE_P
            IF(PK%XPATCH(JI)==0.0)THEN
              PEK%XSOILDIF_CARB(JI,JL,JNC)=XUNDEF     
            ENDIF
         ENDDO
      ENDDO
   ENDDO
ENDDO
!
!Old and New carbon grid
!
ZOLDVEG_GRID(:) = 0.0
ZNEWVEG_GRID(:) = 0.0
DO JP=1,INP
   PK => NP%AL(JP)
   PEK => NPE%AL(JP)
   OLD => TLU%AL(JP)
   DO JI=1,PK%NSIZE_P
      IMASK=PK%NR_P(JI)
      IF(OLD%SOIL_CARBON(JI,1,1)>0.0.AND.OLD%SOIL_CARBON(JI,1,1)<XUNDEF)THEN
        ZOLDVEG_GRID(IMASK)=ZOLDVEG_GRID(IMASK)+OLD%PATCH(JI)
      ENDIF       
      IF(PEK%XSOILDIF_CARB(JI,1,1)>0.0.AND.PEK%XSOILDIF_CARB(JI,1,1)<XUNDEF)THEN
        ZNEWVEG_GRID(IMASK)=ZNEWVEG_GRID(IMASK)+PK%XPATCH(JI)
      ENDIF       
   ENDDO
ENDDO
!
DO JP=1,INP
   PK => NP%AL(JP)
   PEK => NPE%AL(JP)
   OLD => TLU%AL(JP)
   DO JNL=1,INLIT
      DO JI=1,PK%NSIZE_P 
         IMASK=PK%NR_P(JI)
         PLITTER_GRID_OLD(IMASK) = PLITTER_GRID_OLD(IMASK) + OLD%SURF_LITTER(JI,JNL)*OLD%PATCH(JI)
      ENDDO
   ENDDO
ENDDO
!
DO JP=1,INP
   PK => NP%AL(JP)
   PEK => NPE%AL(JP)
   OLD => TLU%AL(JP)
   DO JNL=1,INLIT
      DO JL=1,INL
         DO JI=1,PK%NSIZE_P     
            IMASK=PK%NR_P(JI)
            IDEPTH_OLD=OLD%WG_LAYER(JI)
            IF(OLD%PATCH(JI)>0.0.AND.JL<=IDEPTH_OLD)THEN      
              PLITTER_GRID_OLD(IMASK) = PLITTER_GRID_OLD(IMASK) + OLD%SOIL_LITTER(JI,JL,JNL)*OLD%PATCH(JI)
            ENDIF
         ENDDO
      ENDDO
   ENDDO
ENDDO
!
DO JP=1,INP
   PK => NP%AL(JP)
   PEK => NPE%AL(JP)
   OLD => TLU%AL(JP)
   DO JNC=1,INC
      DO JL=1,INL
         DO JI=1,PK%NSIZE_P     
            IMASK=PK%NR_P(JI)
            IDEPTH_OLD=OLD%WG_LAYER(JI)
            IF(OLD%PATCH(JI)>0.0.AND.JL<=IDEPTH_OLD)THEN      
              PCSOIL_GRID_OLD(IMASK) = PCSOIL_GRID_OLD(IMASK) + OLD%SOIL_CARBON(JI,JL,JNC)*OLD%PATCH(JI)
            ENDIF
         ENDDO
      ENDDO
   ENDDO
ENDDO
!
!-----------------------------------------------------------------
!
!*      2.     Compute previous and current year total grid-cell biomass and carbon stocks
!              ---------------------------------------------------------------------------
!
!Carbon stock in gC/m2
!
DO JP=1,INP
   PK => NP%AL(JP)
   PEK => NPE%AL(JP)
   OLD => TLU%AL(JP)
   DO JI=1,PK%NSIZE_P 
      IMASK=PK%NR_P(JI)
      ZCSURF_LIGNIN_OLD(IMASK) = ZCSURF_LIGNIN_OLD(IMASK) + OLD%SURF_LIGNIN          (JI)*OLD%PATCH(JI)
      ZCSURF_LIGNIN_NEW(IMASK) = ZCSURF_LIGNIN_NEW(IMASK) + PEK%XSURFACE_LIGNIN_STRUC(JI)*PK%XPATCH(JI)
   ENDDO
ENDDO 
!
DO JP=1,INP
   PK => NP%AL(JP)
   PEK => NPE%AL(JP)
   OLD => TLU%AL(JP)
   DO JL=1,INL
      DO JI=1,PK%NSIZE_P   
         IMASK=PK%NR_P(JI)
         IDEPTH_OLD=OLD%WG_LAYER(JI)
         IF(OLD%PATCH(JI)>0.0.AND.JL<=IDEPTH_OLD)THEN      
           ZCSOIL_LIGNIN_OLD(IMASK) = ZCSOIL_LIGNIN_OLD(IMASK) + OLD%SOIL_LIGNIN(JI,JL)*OLD%PATCH(JI)  
         ENDIF       
         IDEPTH=PK%NWG_LAYER(JI)
         IF(PK%XPATCH(JI)>0.0.AND.JL<=IDEPTH)THEN      
           ZCSOIL_LIGNIN_NEW(IMASK) = ZCSOIL_LIGNIN_NEW(IMASK) + PEK%XSOILDIF_LIGNIN_STRUC(JI,JL)*PK%XPATCH(JI)
         ENDIF
!
      ENDDO
   ENDDO
ENDDO  
!  
DO JP=1,INP
   PK => NP%AL(JP)
   PEK => NPE%AL(JP)
   OLD => TLU%AL(JP)
   DO JNL=1,INLIT
      DO JI=1,PK%NSIZE_P 
         IMASK=PK%NR_P(JI)
         IF(OLD%PATCH(JI)>0.0)THEN
            ZCSURF_LITTER_OLD(IMASK,JNL) = ZCSURF_LITTER_OLD(IMASK,JNL) + OLD%SURF_LITTER(JI,JNL)*OLD%PATCH(JI) &
                                                                        - ZHARVEST(IMASK,JNL,JP)
         ENDIF
         IF(PK%XPATCH(JI)>0.0)THEN
            ZCSURF_LITTER_NEW(IMASK,JNL) = ZCSURF_LITTER_NEW(IMASK,JNL) + PEK%XSURFACE_LITTER(JI,JNL)*PK%XPATCH(JI)
         ENDIF
      ENDDO
   ENDDO
ENDDO
!
DO JP=1,INP
   PK => NP%AL(JP)
   PEK => NPE%AL(JP)
   OLD => TLU%AL(JP)
   DO JNL=1,INLIT
      DO JL=1,INL
         DO JI=1,PK%NSIZE_P        
            IMASK=PK%NR_P(JI)
            IDEPTH_OLD=OLD%WG_LAYER(JI)
            IF(OLD%PATCH(JI)>0.0.AND.JL<=IDEPTH_OLD)THEN      
              ZCSOIL_LITTER_OLD(IMASK,JNL) = ZCSOIL_LITTER_OLD(IMASK,JNL) + OLD%SOIL_LITTER(JI,JL,JNL)*OLD%PATCH(JI)
            ENDIF              
            IDEPTH=PK%NWG_LAYER(JI)
            IF(PK%XPATCH(JI)>0.0.AND.JL<=IDEPTH)THEN      
              ZCSOIL_LITTER_NEW(IMASK,JNL) = ZCSOIL_LITTER_NEW(IMASK,JNL) + PEK%XSOILDIF_LITTER(JI,JL,JNL)*PK%XPATCH(JI)     
            ENDIF             
         ENDDO
      ENDDO
   ENDDO
ENDDO
!
DO JP=1,INP
   PK => NP%AL(JP)
   PEK => NPE%AL(JP)
   OLD => TLU%AL(JP)
   DO JNC=1,INC
      DO JL=1,INL
         DO JI=1,PK%NSIZE_P
            IMASK=PK%NR_P(JI)
            IDEPTH_OLD=OLD%WG_LAYER(JI)
            IF(OLD%PATCH(JI)>0.0.AND.JL<=IDEPTH_OLD)THEN      
              ZCSOIL_RESERV_OLD(IMASK,JNC) = ZCSOIL_RESERV_OLD(IMASK,JNC) + OLD%SOIL_CARBON(JI,JL,JNC)*OLD%PATCH(JI)
            ENDIF        
            IDEPTH=PK%NWG_LAYER(JI)
            IF(PK%XPATCH(JI)>0.0.AND.JL<=IDEPTH)THEN      
              ZCSOIL_RESERV_NEW(IMASK,JNC) = ZCSOIL_RESERV_NEW(IMASK,JNC) + PEK%XSOILDIF_CARB(JI,JL,JNC)*PK%XPATCH(JI)        
            ENDIF
         ENDDO
      ENDDO
   ENDDO
ENDDO
!
!-----------------------------------------------------------------
!
!*      4.      Ensures grid-cell conservation for carbon and litter stock
!              -----------------------------------------------------------
!
!
DO JP=1,INP
   PK => NP%AL(JP)
   PEK => NPE%AL(JP)
   DO JI=1,PK%NSIZE_P
      IMASK=PK%NR_P(JI)
      IF(ZCSURF_LIGNIN_NEW(IMASK)/=ZCSURF_LIGNIN_OLD(IMASK).AND.ZCSURF_LIGNIN_NEW(IMASK)/=0.0)THEN
        PEK%XSURFACE_LIGNIN_STRUC(JI) = PEK%XSURFACE_LIGNIN_STRUC(JI) * ZCSURF_LIGNIN_OLD(IMASK)/ZCSURF_LIGNIN_NEW(IMASK)
      ENDIF
   ENDDO
ENDDO
!
DO JP=1,INP
   PK => NP%AL(JP)
   PEK => NPE%AL(JP)
   DO JL=1,INL
      DO JI=1,PK%NSIZE_P
         IMASK=PK%NR_P(JI)
         IDEPTH=PK%NWG_LAYER(JI)
         IF(JL<=IDEPTH.AND.ZCSOIL_LIGNIN_NEW(IMASK)/=ZCSOIL_LIGNIN_OLD(IMASK).AND.ZCSOIL_LIGNIN_NEW(IMASK)/=0.0)THEN
           PEK%XSOILDIF_LIGNIN_STRUC(JI,JL) = PEK%XSOILDIF_LIGNIN_STRUC(JI,JL) * ZCSOIL_LIGNIN_OLD(IMASK)/ZCSOIL_LIGNIN_NEW(IMASK)
         ENDIF
      ENDDO
   ENDDO
ENDDO
!
DO JP=1,INP  
   PK => NP%AL(JP)
   PEK => NPE%AL(JP)
   DO JNL=1,INLIT
      DO JI=1,PK%NSIZE_P
        IMASK=PK%NR_P(JI)
        IF(PK%XPATCH(JI)>0.0.AND.ZCSURF_LITTER_NEW(IMASK,JNL)/=ZCSURF_LITTER_OLD(IMASK,JNL) &
                            .AND.ZCSURF_LITTER_NEW(IMASK,JNL)/=0.0)THEN
          PEK%XSURFACE_LITTER(JI,JNL) = PEK%XSURFACE_LITTER(JI,JNL) * ZCSURF_LITTER_OLD(IMASK,JNL)/ZCSURF_LITTER_NEW(IMASK,JNL)
        ENDIF
      ENDDO
   ENDDO
ENDDO
!
DO JP=1,INP
   PK => NP%AL(JP)
   PEK => NPE%AL(JP)
   OLD => TLU%AL(JP)
   DO JNL=1,INLIT
      DO JL=1,INL
         DO JI=1,PK%NSIZE_P
            IMASK=PK%NR_P(JI)
            IDEPTH=PK%NWG_LAYER(JI)
            IDEPTH_OLD=OLD%WG_LAYER(JI)
            IF(PK%XPATCH(JI)>0.0.AND.JL<=IDEPTH.AND.ZCSOIL_LITTER_NEW(IMASK,JNL)/=ZCSOIL_LITTER_OLD(IMASK,JNL) &
                                               .AND.ZCSOIL_LITTER_NEW(IMASK,JNL)/=0.0)THEN
              PEK%XSOILDIF_LITTER(JI,JL,JNL) = PEK%XSOILDIF_LITTER(JI,JL,JNL) * ZCSOIL_LITTER_OLD(IMASK,JNL) &
                                                                              / ZCSOIL_LITTER_NEW(IMASK,JNL)
            ELSEIF(OLD%PATCH(JI)>0.0.AND.JL<=IDEPTH_OLD.AND.ZOLDVEG_GRID(IMASK)>0.0.AND.ZNEWVEG_GRID(IMASK)==0.0)THEN ! all vegetated patches disappeared
              S%XCCONSRV(IMASK)=S%XCCONSRV(IMASK)+OLD%SOIL_LITTER(JI,JL,JNL)*OLD%PATCH(JI)*XGTOKG
            ENDIF
         ENDDO
      ENDDO
   ENDDO
ENDDO
!
DO JP=1,INP
   PK => NP%AL(JP)
   PEK => NPE%AL(JP)
   OLD => TLU%AL(JP)
   DO JNC=1,INC
      DO JL=1,INL
         DO JI=1,PK%NSIZE_P
            IMASK=PK%NR_P(JI)
            IDEPTH=PK%NWG_LAYER(JI)
            IDEPTH_OLD=OLD%WG_LAYER(JI)
            IF(PK%XPATCH(JI)>0.0.AND.JL<=IDEPTH.AND.ZCSOIL_RESERV_NEW(IMASK,JNC)/=ZCSOIL_RESERV_OLD(IMASK,JNC) &
                                               .AND.ZCSOIL_RESERV_NEW(IMASK,JNC)/=0.0)THEN
              PEK%XSOILDIF_CARB(JI,JL,JNC) = PEK%XSOILDIF_CARB(JI,JL,JNC) * ZCSOIL_RESERV_OLD(IMASK,JNC) &
                                                                          / ZCSOIL_RESERV_NEW(IMASK,JNC)
            ELSEIF(OLD%PATCH(JI)>0.0.AND.JL<=IDEPTH_OLD.AND.ZOLDVEG_GRID(IMASK)>0.0.AND.ZNEWVEG_GRID(IMASK)==0.0)THEN ! all vegetated patches disappeared
              S%XCCONSRV(IMASK)=S%XCCONSRV(IMASK)+OLD%SOIL_CARBON(JI,JL,JNC)*OLD%PATCH(JI)*XGTOKG
            ENDIF
         ENDDO
      ENDDO
   ENDDO
ENDDO
!
!
!-----------------------------------------------------------------
!
!*      5.     Dead roots added to soil litter (gC/m2)
!              ---------------------------------------
!
DO JP=1,INP
   PK => NP%AL(JP)
   PEK => NPE%AL(JP)
   OLD => TLU%AL(JP)
   CALL BIOMASS_TO_SURFACE_LITTER(OLD%TURNOVER,PEK%XSURFACE_LITTER,PEK%XSURFACE_LIGNIN_STRUC)
   CALL BIOMASS_TO_SOIL_LITTER   (OLD%TURNOVER,PEK%XSOILDIF_LITTER,PEK%XSOILDIF_LIGNIN_STRUC,&
                                  KWG_LAYER=PK%NWG_LAYER,PROOTFRAC=PK%XROOTFRAC)     
ENDDO
!
!-----------------------------------------------------------------
!
!*      6.     Compute current year total grid-cell carbon stocks in gC/m2
!              ---------------------------------------------------------------
!
!
DO JP=1,INP  
   PK => NP%AL(JP)
   PEK => NPE%AL(JP)
   OLD => TLU%AL(JP)
   DO JNL=1,INLIT
      DO JI=1,PK%NSIZE_P
         IMASK=PK%NR_P(JI)
         OLD%LULCC_HARVEST(JI)=OLD%LULCC_HARVEST(JI)+ZHARVEST(IMASK,JNL,JP)*XGTOKG
         PLULCC_HARVEST_GRID(IMASK)=PLULCC_HARVEST_GRID(IMASK)+ZHARVEST(IMASK,JNL,JP)*XGTOKG
      ENDDO
   ENDDO
ENDDO
!  
DO JP=1,INP
   PK => NP%AL(JP)
   PEK => NPE%AL(JP)
   DO JNL=1,INLIT
      DO JI=1,PK%NSIZE_P 
         IMASK=PK%NR_P(JI)
         IF(PK%XPATCH(JI)>0.0)THEN
           PLITTER_GRID_NEW(IMASK) = PLITTER_GRID_NEW(IMASK) + PEK%XSURFACE_LITTER(JI,JNL)*PK%XPATCH(JI)
         ENDIF
      ENDDO
   ENDDO
ENDDO
!
DO JP=1,INP
   PK => NP%AL(JP)
   PEK => NPE%AL(JP)
   DO JNL=1,INLIT
      DO JL=1,INL
         DO JI=1,PK%NSIZE_P
            IMASK=PK%NR_P(JI)
            IDEPTH=PK%NWG_LAYER(JI)
            IF(PK%XPATCH(JI)>0.0.AND.JL<=IDEPTH)THEN      
              PLITTER_GRID_NEW(IMASK) = PLITTER_GRID_NEW(IMASK) + PEK%XSOILDIF_LITTER(JI,JL,JNL)*PK%XPATCH(JI)
            ENDIF
         ENDDO
      ENDDO
   ENDDO
ENDDO
!
DO JP=1,INP
   PK => NP%AL(JP)
   PEK => NPE%AL(JP)
   DO JNC=1,INC
      DO JL=1,INL
         DO JI=1,PK%NSIZE_P
            IMASK=PK%NR_P(JI)
            IDEPTH=PK%NWG_LAYER(JI)
            IF(PK%XPATCH(JI)>0.0.AND.JL<=IDEPTH)THEN      
              PCSOIL_GRID_NEW(IMASK) = PCSOIL_GRID_NEW(IMASK) + PEK%XSOILDIF_CARB(JI,JL,JNC)*PK%XPATCH(JI)
            ENDIF
         ENDDO
      ENDDO
   ENDDO
ENDDO
!
IF (LHOOK) CALL DR_HOOK('LANDUSE_CARBON_DIF',1,ZHOOK_HANDLE)
!-----------------------------------------------------------------
!
END SUBROUTINE LANDUSE_CARBON_DIF
