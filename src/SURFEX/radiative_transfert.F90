!SURFEX_LIC Copyright 1994-2014 Meteo-France 
!SURFEX_LIC This is part of the SURFEX software governed by the CeCILL-C  licence
!SURFEX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!SURFEX_LIC for details. version 1.
!     #########
SUBROUTINE RADIATIVE_TRANSFERT(PVEGTYPE,                          &
            PALBVIS_VEG, PALBVIS_SOIL, PALBNIR_VEG, PALBNIR_SOIL, &
            PSW_RAD, PLAI, PZENITH, PABC,                         &
            PFAPARC, PFAPIRC, PMUS, PLAI_EFFC, OSHADE, PIACAN,    &             
            PIACAN_SUNLIT, PIACAN_SHADE, PFRAC_SUN,               &          
            PFAPAR, PFAPIR, PFAPAR_BS, PFAPIR_BS                  ) 
!   #########################################################################
!
!!****  *RADIATIVE_TRANSFERT*  
!!
!!    PURPOSE
!!    -------
!!
!!    Calculates net assimilation of CO2 and leaf conductance.
!!              
!!**  METHOD
!!    ------
!!    Calvet et al. 1998 Forr. Agri. Met. [from model of Jacobs(1994)]
!!
!!    EXTERNAL
!!    --------
!!    none
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!      
!!    USE MODD_CST
!!    USE MODD_CO2V_PAR
!!    USE MODI_COTWO
!!    USE MODI_CCETR
!!    USE MODE_THERMOS
!!
!!    REFERENCE
!!    ---------
!!
!!    Calvet et al. 1998 Forr. Agri. Met. 
!!      
!!    AUTHOR
!!    ------
!!
!!	A. Boone           * Meteo-France *
!!      (following Belair)
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    27/10/97 
!!      V. Masson and V. Rivailland 12/2003 modificatino of ISBA routines order
!!      L. Jarlan   27/10/04 : add of T2 to calculate soil respiration and use
!!                              of CRESPSL key to manage the calculation of soil
!!                              respiration
!!                             PAN et PPST in kgCO2 m-2 s-1 to be fully
!!                              compatible with vegetation growth module (lailoss.f90)
!!      P Le Moigne 09/2005 AGS modifs of L. Jarlan
!!      S. Lafont      03/09 : change units of EPSO GAMM ANDAY
!!      A.L. Gibelin   06/09 : suppress evolution of [CO2] in canopy
!!      A.L. Gibelin   06/09 : move calculations of some CO2 fluxes
!!      A.L. Gibelin   06/09 : add RESP_LEAF
!!      A.L. Gibelin   07/09 : ensure coherence between cotwores and cotworestress
!!      A.L. Gibelin   07/09 : Suppress PPST and PPSTF as outputs, and diagnose GPP
!!        S. Lafont    03/11 : Correct a bug fopr grassland below wilting point
!!      D. Carrer      04/11 : new radiative transfert 
!!
!-------------------------------------------------------------------------------
!
USE MODD_CO2V_PAR,       ONLY : XPARCF, XLAI_SHADE,                   &
                                XXB_SUP, XXB_INF, XSSA_SUP, XSSA_INF, &
                                XSSA_SUP_PIR, XSSA_INF_PIR                            
USE MODD_SURF_PAR,       ONLY : XUNDEF
!
USE MODI_FAPAIR
!
!*       0.     DECLARATIONS
!               ------------
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
REAL, DIMENSION(:,:),INTENT(IN)  :: PVEGTYPE     ! PVEGTYPE  = type de vegetation (1 a 9)
!
REAL, DIMENSION(:), INTENT(IN)   :: PALBVIS_VEG  ! visible snow free albedo of vegetation
REAL, DIMENSION(:), INTENT(IN)   :: PALBVIS_SOIL ! visible snow free albedo of soil
REAL, DIMENSION(:), INTENT(IN)   :: PALBNIR_VEG  ! NIR snow free albedo of vegetation
REAL, DIMENSION(:), INTENT(IN)   :: PALBNIR_SOIL ! NIR snow free albedo of soil
!
REAL,DIMENSION(:),   INTENT(IN)  :: PSW_RAD
REAL,DIMENSION(:),   INTENT(IN)  :: PLAI         ! PLAI  = leaf area index
!
REAL,DIMENSION(:),    INTENT(IN)  :: PZENITH
!	                             PZENITH = solar zenith angle needed 
!                                    for computation of diffusuion of solar
!                                    radiation: for CO2 model.
!
REAL,DIMENSION(:),  INTENT(INOUT) :: PABC
!                                    PABC  = abscissa needed for integration
!                                            of net assimilation and stomatal
!                                            conductance over canopy depth
!
REAL, DIMENSION(:),   INTENT(INOUT) :: PFAPARC   ! Fapar of vegetation (cumul)
REAL, DIMENSION(:),   INTENT(INOUT) :: PFAPIRC   ! Fapir of vegetation (cumul)
REAL, DIMENSION(:),   INTENT(INOUT) :: PMUS
REAL, DIMENSION(:),   INTENT(INOUT) :: PLAI_EFFC ! Effective LAI (cumul)
!
LOGICAL, DIMENSION(:),INTENT(OUT) :: OSHADE      ! OSHADE = if 1 shading activated
REAL, DIMENSION(:,:), INTENT(OUT) :: PIACAN      ! PAR in the canopy at different gauss level
REAL, DIMENSION(:,:), INTENT(OUT) :: PIACAN_SUNLIT, PIACAN_SHADE
!                                                ! absorbed PAR of each level within the
!                                                ! canopy - Split into shaded and SUNLIT
REAL, DIMENSION(:,:), INTENT(OUT) :: PFRAC_SUN   ! fraction of sunlit leaves
!
REAL, DIMENSION(:),   INTENT(OUT) :: PFAPAR, PFAPIR, PFAPAR_BS, PFAPIR_BS
!
!*      0.2    declarations of local variables
!
!
REAL, DIMENSION(SIZE(PLAI)) :: ZIA, ZLAI, ZLAI_EFF, ZXMUS 
!                                           ZXMUS = cosine of solar zenith angle
!
REAL,    DIMENSION(SIZE(PLAI)) :: ZB_INF, ZB_SUP
INTEGER, DIMENSION(1)          :: IDMAX
!
INTEGER                        :: JJ ! index for loops
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('RADIATIVE_TRANSFERT',0,ZHOOK_HANDLE)
!
ZLAI(:)   = PLAI(:)
!
! Net assimilation of the canopy (Ac) is limitted to 7 points of LAI
WHERE (PLAI(:)==XUNDEF) ZLAI(:) = 0.0
!
! Geometrical configuration and density of leaves induce different 
! min value of LAI to start the shading.
OSHADE(:)= .TRUE.
DO JJ = 1, SIZE(PLAI)
  IDMAX = MAXLOC(PVEGTYPE(JJ,:))
  IF (PLAI(JJ).LT.XLAI_SHADE(IDMAX(1))) OSHADE(JJ) = .FALSE.
  ZB_INF(JJ) = XXB_INF(IDMAX(1))
  ZB_SUP(JJ) = XXB_SUP(IDMAX(1))
ENDDO
!
!to consider all the tickness of the canopy
PABC(1) = 0.
!
! cosine of solar zenith angle 
!
ZXMUS(:) = MAX(COS(PZENITH(:)),0.01)
!
!
ZIA(:)     = PSW_RAD(:)*(1.-XPARCF)
 CALL FAPAIR(PABC, ZIA, ZLAI, ZXMUS, XSSA_SUP_PIR, XSSA_INF_PIR,  &
         ZB_SUP, ZB_INF, PALBNIR_VEG, PALBNIR_SOIL, OSHADE,      &
         PFAPIR, PFAPIR_BS                                       )
!
ZIA(:)     = PSW_RAD(:)*XPARCF
 CALL FAPAIR(PABC, ZIA, ZLAI, ZXMUS, XSSA_SUP, XSSA_INF,          &
         ZB_SUP, ZB_INF, PALBVIS_VEG, PALBVIS_SOIL, OSHADE,      &
         PFAPAR, PFAPAR_BS, PLAI_EFF=ZLAI_EFF, PIACAN=PIACAN,    &
         PIACAN_SHADE=PIACAN_SHADE, PIACAN_SUNLIT=PIACAN_SUNLIT, &
         PFRAC_SUN=PFRAC_SUN                                     )
!
!
DO JJ = 1,SIZE(PLAI)
  IF (ZIA(JJ).NE.0.) THEN
    PFAPIRC(JJ)   = PFAPIRC(JJ)   + PFAPIR(JJ)   * ZXMUS(JJ)
    PFAPARC(JJ)   = PFAPARC(JJ)   + PFAPAR(JJ)   * ZXMUS(JJ)
    PLAI_EFFC(JJ) = PLAI_EFFC(JJ) + ZLAI_EFF(JJ) * ZXMUS(JJ)
    PMUS(JJ)      = PMUS(JJ)      + ZXMUS(JJ)
  ENDIF
ENDDO
!
IF (LHOOK) CALL DR_HOOK('RADIATIVE_TRANSFERT',1,ZHOOK_HANDLE)
!
END SUBROUTINE RADIATIVE_TRANSFERT
