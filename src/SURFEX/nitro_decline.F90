!SURFEX_LIC Copyright 1994-2014 Meteo-France 
!SURFEX_LIC This is part of the SURFEX software governed by the CeCILL-C  licence
!SURFEX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!SURFEX_LIC for details. version 1.
!     #########
SUBROUTINE NITRO_DECLINE(HPHOTO, HRESPSL, OTR_ML, KSPINW ,            &
                PBSLAI_NITRO, PSEFOLD, PGMES, PANMAX, PANDAY,         &
                PLAT, PLAIMIN, PVEGTYPE, PTAU_WOOD,                   &
                PANFM, PLAI, PBIOMASS, PRESP_BIOMASS, PBIOMASS_LEAF,  &
                PINCREASE ,PTURNOVER                                  )  
!
!   ###############################################################
!!**  NITRO_DECLINE 
!!
!!    PURPOSE
!!    -------
!!
!!**  METHOD
!!    ------
!!     Calvet and Soussana (2001) and Gibelin et al. (2006) for nitrogen dilution.
!!     Gibelin et al. (2008) : New biomass reservoirs, and new method for allocation, 
!!     mortality and respiration.
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
!! Calvet and Soussana (2001), "Modelling CO2-enrichment effects using an
!! interactive vegetation SVAT scheme", Agricultural and Forest Meteorology, Vol. 108
!! pp. 129-152
!! Gibelin et al. (2008), "Modelling energy and CO2 fluxes with an interactive vegetation 
!! land surface model - Evaluation at high and middle latitudes", 
!! Agricultural and Forest Meteorology, Vol. 148 , pp. 1611-1628
!!      
!!    AUTHOR
!!    ------
!!
!!	A.-L. Gibelin           * Meteo-France *
!!      (following Belair)
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    27/01/03 
!!
!!      P Le Moigne  09/2005 : AGS modifs of L. Jarlan
!!      A.L. Gibelin 04/2009 : BIOMASS and RESP_BIOMASS arrays
!!      A.L. Gibelin 04/2009 : Suppress unused arguments
!!      A.L. Gibelin 04/2009 : Suppress unused modules and add ONLY
!!      A.L. Gibelin 04/2009 : adaptation to SURFEX environment
!!      A.   Barbu   01/2011 : modification of active biomass,leaf reservoir (see nitro_decline.f90)
!!      C.   Delire  04/2012 : spinup wood carbon
!!      B.   Decharme 05/2012 : Optimization
!!                              ZCC_NITRO and ZBIOMASST_LIM in modd_co2v_par.F90
!
!-------------------------------------------------------------------------------
!
!*       0.     DECLARATIONS
!               ------------
!
USE MODD_CSTS,           ONLY : XPI, XDAY
USE MODD_CO2V_PAR,       ONLY : XPCCO2, XCC_NIT, XCA_NIT, XMC, &
                                XMCO2, XCC_NITRO, XBIOMASST_LIM 
USE MODD_DATA_COVER_PAR, ONLY : NVT_TREE, NVT_EVER, NVT_CONI
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
 CHARACTER(LEN=*),     INTENT(IN) :: HPHOTO           ! Kind of photosynthesis
!                                                    ! 'NON'
!                                                    ! 'AGS'
!                                                    ! 'LAI'
!                                                    ! 'AST'
!                                                    ! 'LST'
 CHARACTER(LEN=3),     INTENT(IN) :: HRESPSL          ! Soil Respiration
!                                                    ! 'DEF' = Norman 1992
!                                                    ! 'PRM' = Rivalland PhD Thesis (2003)
!                                                    ! 'CNT' = CENTURY model (Gibelin 2008)
LOGICAL,              INTENT(IN) :: OTR_ML           ! new TR
INTEGER, INTENT(IN)              :: KSPINW           ! wood spinup
!
REAL,   DIMENSION(:), INTENT(IN) :: PBSLAI_NITRO     ! ratio of biomass to LAI
REAL,   DIMENSION(:), INTENT(IN) :: PSEFOLD          ! e-folding time for senescence (s)
REAL,   DIMENSION(:), INTENT(IN) :: PGMES            ! mesophyll conductance (m s-1)
REAL,   DIMENSION(:), INTENT(IN) :: PANMAX           ! maximum photosynthesis rate
REAL,   DIMENSION(:), INTENT(IN) :: PANDAY           ! daily net CO2 accumulation
REAL,   DIMENSION(:), INTENT(IN) :: PLAT             ! latitude of each grid point
REAL,   DIMENSION(:), INTENT(IN) :: PLAIMIN          ! minimum LAI
REAL, DIMENSION(:,:), INTENT(IN) :: PVEGTYPE         ! fraction of each vegetation
REAL,   DIMENSION(:), INTENT(IN) :: PTAU_WOOD        ! residence time in wood (s)
REAL,   DIMENSION(:), INTENT(IN) :: PLAI             ! leaf area index (LAI) 
!
REAL,   DIMENSION(:), INTENT(INOUT) :: PANFM         ! maximum leaf assimilation
REAL, DIMENSION(:,:), INTENT(INOUT) :: PBIOMASS      ! biomass reservoirs
REAL, DIMENSION(:,:), INTENT(INOUT) :: PRESP_BIOMASS ! cumulated daily biomass respiration (kgDM m-2 day-1)
!
REAL,   DIMENSION(:), INTENT(OUT)   :: PBIOMASS_LEAF ! temporary leaf biomass
REAL, DIMENSION(:,:), INTENT(OUT)   :: PINCREASE     ! increment of biomass
REAL, DIMENSION(:,:), INTENT(OUT)   :: PTURNOVER     ! biomass turnover going into litter (gC m-2 s-1)
!
!*      0.2    declarations of local variables
!
REAL                            :: ZBMCOEF
REAL,    DIMENSION(SIZE(PLAI))  :: ZXSEFOLD        ! e-folding time for senescence corrected (days)
REAL,    DIMENSION(SIZE(PLAI))  :: ZLAIB_NITRO     ! LAI correction parameter used in sefold calculation
REAL,    DIMENSION(SIZE(PLAI))  :: ZASSIM          ! assimilation
REAL,    DIMENSION(SIZE(PLAI))  :: ZBIOMASST       ! leaf + active structural biomass
!
REAL, DIMENSION(SIZE(PLAI),SIZE(PBIOMASS,2))  :: ZINCREASE
REAL, DIMENSION(SIZE(PLAI),SIZE(PBIOMASS,2))  :: ZBIOMASS      ! temporary biomass reservoirs
REAL, DIMENSION(SIZE(PLAI),SIZE(PBIOMASS,2))  :: ZDECLINE      ! biomass decline (storage+mortality) (kgDM m-2 day-1)
REAL, DIMENSION(SIZE(PLAI),SIZE(PBIOMASS,2))  :: ZSTORAGE      ! storage (part of decline kgDM m-2 day-1)
REAL, DIMENSION(SIZE(PLAI))                   :: ZMORT_LEAF    ! leaf mortality
!
REAL, DIMENSION(SIZE(PLAI))                   :: ZWORK
LOGICAL, DIMENSION(SIZE(PLAI))                :: LMASK_ASSIM, LMASK_VEGTYP
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
INTEGER :: JSPIN, JI, INI
!
! correspondence between array indices and biomass compartments
! LEAF = 1
! STRUCT_ACT = 2
! STRUCT_PAS = 3
! STRUCT_BELOW = 4
! WOOD_ABOVE = 5
! WOOD_BELOW = 6
!
!-------------------------------------------------------------------------------
!
! 1 - Initialisations
! -------------------
!
IF (LHOOK) CALL DR_HOOK('NITRO_DECLINE',0,ZHOOK_HANDLE)
!
INI = SIZE(PLAI)
!
ZXSEFOLD(:)         = 0.0
ZLAIB_NITRO(:)      = 0.0
ZBIOMASST(:)        = 0.0
ZASSIM(:)           = 0.0
ZBIOMASS(:,:)       = 0.0
ZDECLINE(:,:)       = 0.0
ZINCREASE(:,:)      = 0.0
ZSTORAGE(:,:)       = 0.0
ZMORT_LEAF(:)       = 0.0
!---------------------------------------------------
!
ZBMCOEF     = XMC/(XMCO2*XPCCO2)
!
!-----------------------------------------------------------------
!avoid possible but unlikely negative values for biomass:        
!
PBIOMASS(:,1) = MAX(PBIOMASS(:,1),0.0)
!
! current leaf biomass value:
!
PBIOMASS_LEAF(:) = PBIOMASS(:,1)
!
!-------------------------------------------------------------------------------
!
! Once a day (at midnight),repartition of net assimilation and mortality 
! into different biomass compartments.
!
! 2 - Evolution of leaf biomass and senescence calculations
! ---------------------------------------------------------
!
! coef c for biomass in kg/m2 now in modd_co2v_par.F90 (XCC_NITRO)
!
! LAI correction for shadow effect
IF (OTR_ML) THEN
  ZLAIB_NITRO(:) = 5.30
ELSE
  ZLAIB_NITRO(:) = MAX( 5.76-0.64*ATAN(ABS(PLAT(:))*XPI/180.),3.8 )
ENDIF
!
!
! leaf life expectancy
!
ZWORK(:) = 0.0
WHERE(PGMES(:)>0.0)
      ZWORK(:) = 0.321*LOG(PGMES(:)*1000.)
      ZWORK(:) = EXP(ZWORK(:))*PLAI(:)/ZLAIB_NITRO(:)
ENDWHERE
! before optimization
!ZXSEFOLD(:)= PSEFOLD(:) * MAX(((PGMES(:)*1000.)**0.321)*PLAI(:)/ZLAIB_NITRO(:), 1.) * ...
ZXSEFOLD(:) = PSEFOLD(:) * MAX(1.0,ZWORK(:)) * MIN(1.0,PANFM(:)/PANMAX(:)) / XDAY
!
! avoid possible but unlikely division by zero
!
ZXSEFOLD(:) = MAX(1.0E-8,ZXSEFOLD(:))
!
! limitation of leaf life expectancy
!
! OLD   ZXSEFOLD(:) = MAX(5.,ZXSEFOLD(:))
! Following Marita's work limitation of the senesence
ZXSEFOLD(:) = MAX(PSEFOLD(:)/XDAY/10.0,ZXSEFOLD(:))
!
! senesence of active biomass
!
ZDECLINE(:,1) = MIN(PBIOMASS_LEAF(:)-PLAIMIN(:)*PBSLAI_NITRO(:), &
                    PBIOMASS_LEAF(:)*(1.0-EXP(-1.0/ZXSEFOLD(:))))
!
! avoid negative values due to computation precision
!
ZDECLINE(:,1) = MAX(ZDECLINE(:,1),0.0)
!
! current leaf biomass with assimilation and senescence
!
PBIOMASS_LEAF(:) = PBIOMASS_LEAF(:) - ZDECLINE(:,1)
!
! daily active biomass assimilation
!
ZASSIM(:) = PANDAY(:)*ZBMCOEF
!
!-------------------------------------------------------------------------------
!
! 3 - Evolution of active structural biomass
! ------------------------------------------
!
ZWORK(:) = 0.0
WHERE(PBIOMASS_LEAF(:)>0.0)
      ZWORK(:) = (1.0/(1.0-XCA_NIT))*LOG(PBIOMASS_LEAF(:)/XCC_NITRO)
      ZWORK(:) = EXP(ZWORK(:))
ENDWHERE
!
WHERE (ZASSIM(:) >= ZDECLINE(:,1))
  !
  ! 3.1 - Growing phase : plant nitrogen decline theory
  !
  ! the growth allometric law is applied
  ! repartition of total biomass    
  !
  !before optimization
  !ZBIOMASST(:)= MAX(PBIOMASS_LEAF(:), (PBIOMASS_LEAF(:)/XCC_NITRO)**(1.0/(1.0-XCA_NIT)))  
  ZBIOMASST(:) = MAX(PBIOMASS_LEAF(:), ZWORK(:))
  !
  ! active structural biomass increment and storage
  !
  ZBIOMASS(:,2)  = ZBIOMASST(:)  - PBIOMASS_LEAF(:)
  ZDECLINE(:,2)  = ZBIOMASS(:,2) * (1.0-EXP(-1.0*XDAY/PSEFOLD(:)))
  ZSTORAGE(:,1)  = ZBIOMASS(:,2) - PBIOMASS(:,2) + ZDECLINE(:,2) + PRESP_BIOMASS(:,2)
  !
ELSE WHERE
  !
  ! 3.2 - Senescence phase
  !
  ! the active structural biomass dies exponentially at the lowest rate
  !
  ZSTORAGE(:,1) = 0.0
  ZDECLINE(:,2) = PBIOMASS(:,2) * (1.0-EXP(-1.0*XDAY/PSEFOLD(:)))
  ZBIOMASS(:,2) = PBIOMASS(:,2) - ZDECLINE(:,2) - PRESP_BIOMASS(:,2)
  !
  !  Avoid negative values of biomass
  !  No test on ZDECLINE(:,2) as it is not used after, or recalculated
  !  No test on PRESP_BIOMASS(:,2) as it should be smaller than PBIOMASS(:,2)
  !  otherwise there are irrealistic values of temperature     
  !
  ZBIOMASS(:,2) = MAX(ZBIOMASS(:,2),0.0)
  !
  ZBIOMASST(:) = PBIOMASS_LEAF(:) + ZBIOMASS(:,2)
  !
END WHERE
!
! 3.3 - Flow to the passive structural biomass: cut or growth after senescence
! Biomass is taken from active structural biomass, not from senescence of leaves
! 
ZINCREASE(:,1) = ZASSIM(:)
ZINCREASE(:,2) = ZSTORAGE(:,1)
ZINCREASE(:,3) = -MIN(ZSTORAGE(:,1),0.0)
!
ZSTORAGE (:,1) = MAX(0.0,ZSTORAGE(:,1))
!
! 3.4 - Mass conservation : leaf biomass sensecence must be >= structural storage
!
WHERE( ZSTORAGE(:,1) > ZDECLINE(:,1))
  ZDECLINE(:,2)    = PBIOMASS(:,2) * (1.0 - EXP(-1.0*XDAY/PSEFOLD(:)))
  ZBIOMASST(:)     = PBIOMASS(:,1) + PBIOMASS(:,2) - ZDECLINE(:,2) - PRESP_BIOMASS(:,2)  
END WHERE
!
ZWORK(:) = 0.0
WHERE( ZBIOMASST(:) > 0.0)
      ZWORK(:) = (1.0-XCA_NIT)*LOG(ZBIOMASST(:))
      ZWORK(:) = EXP(ZWORK(:))
ENDWHERE
!
WHERE( ZSTORAGE(:,1) > ZDECLINE(:,1))
  !   
  !before optimization
  !PBIOMASS_LEAF(:)= ZCC_NITRO * (ZBIOMASST(:)**(1.0-XCA_NIT))
  PBIOMASS_LEAF(:) = XCC_NITRO * ZWORK(:)
  ZBIOMASS(:,2)    = ZBIOMASST(:)  - PBIOMASS_LEAF(:)
  ZDECLINE(:,1)    = PBIOMASS(:,1) - PBIOMASS_LEAF(:)
  ZSTORAGE(:,1)    = ZBIOMASS(:,2) - PBIOMASS(:,2) + ZDECLINE(:,2) + PRESP_BIOMASS(:,2)  
  !
  ZINCREASE(:,2) = ZSTORAGE(:,1)
  !
END WHERE
!
!-------------------------------------------------------------------------------
!
! 4 - Evolution of other biomass pools and final calculations
! -----------------------------------------------------------
!
! 4.1 - Mortality of leaf biomass
!
ZMORT_LEAF(:) = MAX(0.0, ZDECLINE(:,1) - ZSTORAGE(:,1))
!
ZBIOMASS(:,3) = PBIOMASS(:,3)
!
IF (HPHOTO=='NIT') THEN
  !
  ! senesence of deep-structural biomass
  !
  ZDECLINE(:,3) = ZBIOMASS(:,3)*(1.0-EXP(-1.0*XDAY/PSEFOLD(:)))          
  !
  ! threshold value for leaf biomass and total above ground biomass in nitrogen
  ! dilution theory now in modd_co2v_par.F90 (XBIOMASST_LIM)
  !
  ! emergency deep structural biomass
  WHERE((ZBIOMASST(:) <= XBIOMASST_LIM) .AND. (ZXSEFOLD(:) > 1.0))
    ZBIOMASS(:,3) = ZBIOMASS(:,3) + ZMORT_LEAF(:)
  END WHERE
  !
ELSEIF (HPHOTO=='NCB') THEN
  !
  ! 4.2 - Evolution of the other reservoirs
  ! 4.2.1 - senesence, avoiding negative values of biomass
  !
  ZDECLINE(:,3) = MIN(PBIOMASS(:,3)*(1.0-EXP(-1.0*XDAY/(PSEFOLD(:)/4.))), &
                      PBIOMASS(:,3)-PRESP_BIOMASS(:,3))            
  ZDECLINE(:,4) = MIN(PBIOMASS(:,4)*(1.0-EXP(-1.0*XDAY/PSEFOLD(:))), &
                      PBIOMASS(:,4)-PRESP_BIOMASS(:,4))
  !
  WHERE (PVEGTYPE(:,NVT_TREE)+PVEGTYPE(:,NVT_CONI)+PVEGTYPE(:,NVT_EVER) >= 0.5)
    ! Woody
    ZDECLINE(:,5) = PBIOMASS(:,5)*(1.0-EXP(-1.0*XDAY/PTAU_WOOD(:)))
    ZDECLINE(:,6) = PBIOMASS(:,6)*(1.0-EXP(-1.0*XDAY/PTAU_WOOD(:)))
  ELSEWHERE
    ! Herbaceous
    ZDECLINE(:,5) = 0.
    ZDECLINE(:,6) = 0.
  END WHERE
  !
  ! 4.2.2 - storage (part of decline used as input for other reservoirs)
  !
  LMASK_ASSIM (:)=(ZASSIM(:) >= ZDECLINE(:,1))
  LMASK_VEGTYP(:)=(PVEGTYPE(:,NVT_TREE)+PVEGTYPE(:,NVT_CONI)+PVEGTYPE(:,NVT_EVER)>=0.5)
  !
  WHERE (LMASK_ASSIM(:))
    !
    ! Remaining mortality is stored in roots.
    ZINCREASE(:,4)   = ZMORT_LEAF(:)
    !      
    ! Growing phase, all leaf decline is used as storage.
    ZSTORAGE(:,1)    = ZSTORAGE(:,1) + ZINCREASE(:,4)
    ZMORT_LEAF(:)    = ZMORT_LEAF(:) - ZINCREASE(:,4)
    !      
    ZSTORAGE(:,2)    = ZDECLINE(:,2)
    ZSTORAGE(:,3)    = ZDECLINE(:,3)
    !   
  ELSEWHERE
    !
    ! Senescence, a part of mortality is stored in roots, limited by assimilation rate.
    ZINCREASE(:,4)   = MIN(MAX(0.5*ZASSIM(:),0.) , 0.5*ZMORT_LEAF(:))
    !
    ZSTORAGE(:,1)    = ZSTORAGE(:,1) + ZINCREASE(:,4)
    ZMORT_LEAF(:)    = ZMORT_LEAF(:) - ZINCREASE(:,4)
    !   
  END WHERE
  !
  WHERE(LMASK_ASSIM(:).AND.LMASK_VEGTYP(:))
      ! Woody
      ZSTORAGE(:,4)  = ZDECLINE(:,4)
      !
      ZINCREASE(:,4) = ZINCREASE(:,4) + 0.3* (ZSTORAGE(:,2) + ZSTORAGE(:,3))
      ZINCREASE(:,5) =                  0.7* (ZSTORAGE(:,2) + ZSTORAGE(:,3))
      ZINCREASE(:,6) = ZSTORAGE(:,4)
      !
  ELSEWHERE(LMASK_ASSIM(:).AND..NOT.LMASK_VEGTYP(:))
      ! Herbaceous
      ZSTORAGE(:,4)  = 0.
      !
      ZINCREASE(:,4) = ZINCREASE(:,4) + ZSTORAGE(:,2) + ZSTORAGE(:,3)
      !
  END WHERE
  !
  WHERE (.NOT.LMASK_ASSIM(:).AND.LMASK_VEGTYP(:))
      ! Woody
      ! Senescence, only a part of decline is used as storage
      ZSTORAGE(:,2)  = 0.5*ZDECLINE(:,2)
      ZSTORAGE(:,3)  = 0.5*ZDECLINE(:,3)
      ZSTORAGE(:,4)  = 0.5*ZDECLINE(:,4)
      !
      ZINCREASE(:,5) = ZSTORAGE(:,2) + ZSTORAGE(:,3)
      ZINCREASE(:,6) = ZSTORAGE(:,4)
      !
  ELSEWHERE(.NOT.LMASK_ASSIM(:).AND..NOT.LMASK_VEGTYP(:))
      !  Herbaceous
      ! Senescence, no storage
      ZSTORAGE(:,2)  = 0.
      ZSTORAGE(:,3)  = 0.
      ZSTORAGE(:,4)  = 0.
      !
  END WHERE
  !
  ZSTORAGE(:,5) = 0.
  ZSTORAGE(:,6) = 0.
  !
  ! 4.2.3 - mortality (senescence - storage) and turnover
  !
  IF (HRESPSL=='CNT') THEN
    PTURNOVER(:,1) = ZMORT_LEAF(:)*1000.*XPCCO2/XDAY
    PTURNOVER(:,2) = (ZDECLINE(:,2) - ZSTORAGE(:,2))*1000.*XPCCO2/XDAY
    PTURNOVER(:,3) = (ZDECLINE(:,3) - ZSTORAGE(:,3))*1000.*XPCCO2/XDAY
    PTURNOVER(:,4) = (ZDECLINE(:,4) - ZSTORAGE(:,4))*1000.*XPCCO2/XDAY
    PTURNOVER(:,5) = (ZDECLINE(:,5) - ZSTORAGE(:,5))*1000.*XPCCO2/XDAY
    PTURNOVER(:,6) = (ZDECLINE(:,6) - ZSTORAGE(:,6))*1000.*XPCCO2/XDAY
  ENDIF
  !
ENDIF
!
!
! 4.3 - Re-initialisations for next time step
!
ZBIOMASS(:,3) = ZBIOMASS(:,3) + ZINCREASE(:,3) - ZDECLINE(:,3) - PRESP_BIOMASS(:,3)
!
! Add net accumulated CO2 assimilation 
PBIOMASS_LEAF(:) = PBIOMASS_LEAF(:) + ZASSIM(:)
!
! re-initialisation of biomass compartments values: X(day) <-- X(day-1)
PBIOMASS(:,1) = PBIOMASS_LEAF(:)
PBIOMASS(:,2) = ZBIOMASS(:,2)
PBIOMASS(:,3) = ZBIOMASS(:,3)
!
! re-initialisation of respiration and assimilation terms
PRESP_BIOMASS(:,2) = 0.0
PRESP_BIOMASS(:,3) = 0.0
PANFM(:) = 0.0
!
!
! 4.2.4 - evolution of reservoirs
!
IF (HPHOTO=='NIT') THEN
  !
  PBIOMASS(:,3) = MAX(PBIOMASS(:,3),0.0)
  !
ELSEIF (HPHOTO=='NCB') THEN
  !
  ZBIOMASS(:,4) = PBIOMASS(:,4) + ZINCREASE(:,4) - ZDECLINE(:,4) - PRESP_BIOMASS(:,4)
  !
!
  ZBIOMASS(:,5) = PBIOMASS(:,5)
  ZBIOMASS(:,6) = PBIOMASS(:,6)
!
  DO JSPIN = 1, KSPINW
    DO JI = 1,INI
       IF(LMASK_VEGTYP(JI))THEN
         !Woody
         ZBIOMASS(JI,5) = ZBIOMASS(JI,5) + ZINCREASE(JI,5) - ZDECLINE(JI,5)
         ZBIOMASS(JI,6) = ZBIOMASS(JI,6) + ZINCREASE(JI,6) - ZDECLINE(JI,6)
         ZDECLINE(JI,5) = ZBIOMASS(JI,5)*(1.0-EXP((-1.0*XDAY)/PTAU_WOOD(JI)))
         ZDECLINE(JI,6) = ZBIOMASS(JI,6)*(1.0-EXP((-1.0*XDAY)/PTAU_WOOD(JI)))
       ELSE   
         !Herbaceous
         ZBIOMASS(JI,5) = 0.
         ZBIOMASS(JI,6) = 0.
       ENDIF
    ENDDO
  ENDDO
!
  PBIOMASS(:,4) = ZBIOMASS(:,4)
  PBIOMASS(:,5) = ZBIOMASS(:,5)
  PBIOMASS(:,6) = ZBIOMASS(:,6)
  !
  PRESP_BIOMASS(:,4) = 0.0
  !
  PINCREASE(:,:) = ZINCREASE(:,:)
!  
ENDIF
!
IF (LHOOK) CALL DR_HOOK('NITRO_DECLINE',1,ZHOOK_HANDLE)
!
END SUBROUTINE NITRO_DECLINE
