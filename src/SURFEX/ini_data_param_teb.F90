!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########################
      SUBROUTINE INI_DATA_PARAM_TEB(HPROGRAM, BDD, DTT, KTYPE, PFLOOR_HEIGHT,                     &
                              PCAP_SYS_HEAT, PCAP_SYS_RAT, PT_ADP, PM_SYS_RAT,              &
                              PT_SIZE_MAX, PT_SIZE_MIN, PROUGH_ROOF, PROUGH_WALL,           &
                              PALB_ROOF, PEMIS_ROOF, PHC_ROOF, PTC_ROOF, PD_ROOF,           & 
                              PALB_WALL, PEMIS_WALL, PHC_WALL, PTC_WALL, PD_WALL,           &
                              NISOROOFPOS, NISOWALLPOS, PALB_ROAD, PEMIS_ROAD, PHC_ROAD, PTC_ROAD, PD_ROAD, &
                              PHC_FLOOR, PTC_FLOOR, PD_FLOOR, PISMASS, PHC_MASS, PTC_MASS,  &
                              PD_MASS, PN50,PGR,PU_WIN,PSHGC,PSHGC_SH,PSHADEARCHI,          &
                              PISMECH,PMECHRATE,PGREENROOF,                                 &
                              PEMIS_PANEL,PALB_PANEL,PEFF_PANEL,                            &
                              PFRAC_PANEL, PTHEAT_OCCD,PTHEAT_OCCN,PTHEAT_VCDD,PTHEAT_VCDN, &
                              PTHEAT_VCLD,PFRACOMP,PTCOOL_OCCD,PTCOOL_OCCN,PTCOOL_VCDD,PTCOOL_VCDN,&
                              PTCOOL_VCLD,PF_WATER_COND,PCOP_RAT,PHR_TARGET,PQIN,PQIN_FRAD, &
                              PQIN_FLAT,PMODQIN_VCD,PMODQIN_VLD,PMODQIN_NIG,PHOTWAT,PNATVENT,PFVSUM,PFVNIG, &
                              PTDESV,PFVVAC,PFOPEN,PFSSUM,PFSNIG,PFSVAC,PWIN_SW_MAX,PDAYWBEG_SCHED,         &
                              PHOURBEG_SCHED,PBEG_HOLIDAY,PEND_HOLIDAY,PMOD_HOLIDAY,PPROBOCC,PF_WASTE_CAN)

!     #########################
!
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
!!    S. Faroux & V. Masson        Meteo-France
!!
!!    MODIFICATION
!!    ------------
!!
!!    Original    05/2012 from INI_DATA_PARAM_TEB, separates urban parameters
!!    modified    08/2012 add PROUGH_ROOF, PROUGH_WALL
!!     V. Masson  08/2013 add solar panels
!!     R. Schoetter 11/2015 Major changes in order to read MApUCE data
!----------------------------------------------------------------------------
!
!*    0.     DECLARATION
!            -----------
!
USE MODD_BLD_DESCRIPTION_n, ONLY : BLD_DESC_t
USE MODD_DATA_TEB_n, ONLY : DATA_TEB_t
USE MODD_CSTS, ONLY : XSURF_EPSILON
!
USE MODI_ABOR1_SFX
USE MODI_GET_LUOUT
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
USE MODI_BLDCODE
!
IMPLICIT NONE
!
!*    0.1    Declaration of arguments
!            ------------------------
!
TYPE(BLD_DESC_t), INTENT(INOUT) :: BDD
TYPE(DATA_TEB_t), INTENT(INOUT) :: DTT
INTEGER, DIMENSION(:), INTENT(INOUT) :: KTYPE
 CHARACTER(LEN=6),    INTENT(IN)  :: HPROGRAM  ! program calling surf. schemes
!
! Parameters not yet read from tables
!
REAL, DIMENSION(:), INTENT(OUT), OPTIONAL :: PFLOOR_HEIGHT
REAL, DIMENSION(:), INTENT(OUT), OPTIONAL :: PCAP_SYS_HEAT
REAL, DIMENSION(:), INTENT(OUT), OPTIONAL :: PCAP_SYS_RAT
REAL, DIMENSION(:), INTENT(OUT), OPTIONAL :: PT_ADP
REAL, DIMENSION(:), INTENT(OUT), OPTIONAL :: PM_SYS_RAT
REAL, DIMENSION(:), INTENT(OUT), OPTIONAL :: PT_SIZE_MAX
REAL, DIMENSION(:), INTENT(OUT), OPTIONAL :: PT_SIZE_MIN
REAL, DIMENSION(:), INTENT(OUT), OPTIONAL :: PROUGH_ROOF
REAL, DIMENSION(:), INTENT(OUT), OPTIONAL :: PROUGH_WALL
!
! Building architectural characteristics
!
REAL, DIMENSION(:),   INTENT(OUT), OPTIONAL :: PALB_ROOF
REAL, DIMENSION(:),   INTENT(OUT), OPTIONAL :: PEMIS_ROOF
REAL, DIMENSION(:,:), INTENT(OUT), OPTIONAL :: PHC_ROOF
REAL, DIMENSION(:,:), INTENT(OUT), OPTIONAL :: PTC_ROOF
REAL, DIMENSION(:,:), INTENT(OUT), OPTIONAL :: PD_ROOF
REAL, DIMENSION(:),   INTENT(OUT), OPTIONAL :: PALB_WALL
REAL, DIMENSION(:),   INTENT(OUT), OPTIONAL :: PEMIS_WALL
REAL, DIMENSION(:,:), INTENT(OUT), OPTIONAL :: PHC_WALL
REAL, DIMENSION(:,:), INTENT(OUT), OPTIONAL :: PTC_WALL
REAL, DIMENSION(:,:), INTENT(OUT), OPTIONAL :: PD_WALL
INTEGER, DIMENSION(:),   INTENT(OUT), OPTIONAL :: NISOROOFPOS
INTEGER, DIMENSION(:),   INTENT(OUT), OPTIONAL :: NISOWALLPOS
REAL, DIMENSION(:),   INTENT(OUT), OPTIONAL :: PALB_ROAD
REAL, DIMENSION(:),   INTENT(OUT), OPTIONAL :: PEMIS_ROAD
REAL, DIMENSION(:,:), INTENT(OUT), OPTIONAL :: PHC_ROAD
REAL, DIMENSION(:,:), INTENT(OUT), OPTIONAL :: PTC_ROAD
REAL, DIMENSION(:,:), INTENT(OUT), OPTIONAL :: PD_ROAD
REAL, DIMENSION(:,:), INTENT(OUT), OPTIONAL :: PHC_FLOOR
REAL, DIMENSION(:,:), INTENT(OUT), OPTIONAL :: PTC_FLOOR
REAL, DIMENSION(:,:), INTENT(OUT), OPTIONAL :: PD_FLOOR
REAL, DIMENSION(:)  , INTENT(OUT), OPTIONAL :: PISMASS
REAL, DIMENSION(:,:), INTENT(OUT), OPTIONAL :: PHC_MASS
REAL, DIMENSION(:,:), INTENT(OUT), OPTIONAL :: PTC_MASS
REAL, DIMENSION(:,:), INTENT(OUT), OPTIONAL :: PD_MASS
REAL, DIMENSION(:),   INTENT(OUT), OPTIONAL :: PN50
REAL, DIMENSION(:),   INTENT(OUT), OPTIONAL :: PGR
REAL, DIMENSION(:),   INTENT(OUT), OPTIONAL :: PU_WIN
REAL, DIMENSION(:),   INTENT(OUT), OPTIONAL :: PSHGC
REAL, DIMENSION(:),   INTENT(OUT), OPTIONAL :: PSHGC_SH
REAL, DIMENSION(:),   INTENT(OUT), OPTIONAL :: PSHADEARCHI
REAL, DIMENSION(:),   INTENT(OUT), OPTIONAL :: PISMECH
REAL, DIMENSION(:),   INTENT(OUT), OPTIONAL :: PMECHRATE
REAL, DIMENSION(:),   INTENT(OUT), OPTIONAL :: PGREENROOF
REAL, DIMENSION(:),   INTENT(OUT), OPTIONAL :: PEMIS_PANEL
REAL, DIMENSION(:),   INTENT(OUT), OPTIONAL :: PALB_PANEL
REAL, DIMENSION(:),   INTENT(OUT), OPTIONAL :: PEFF_PANEL
REAL, DIMENSION(:),   INTENT(OUT), OPTIONAL :: PFRAC_PANEL
REAL, DIMENSION(:),   INTENT(OUT), OPTIONAL :: PF_WASTE_CAN
!
! Occupant behaviour characteristics
!
REAL, DIMENSION(:,:), INTENT(OUT), OPTIONAL :: PTHEAT_OCCD
REAL, DIMENSION(:,:), INTENT(OUT), OPTIONAL :: PTHEAT_OCCN
REAL, DIMENSION(:,:), INTENT(OUT), OPTIONAL :: PTHEAT_VCDD
REAL, DIMENSION(:,:), INTENT(OUT), OPTIONAL :: PTHEAT_VCDN
REAL, DIMENSION(:,:), INTENT(OUT), OPTIONAL :: PTHEAT_VCLD
REAL, DIMENSION(:,:), INTENT(OUT), OPTIONAL :: PFRACOMP
REAL, DIMENSION(:,:), INTENT(OUT), OPTIONAL :: PTCOOL_OCCD
REAL, DIMENSION(:,:), INTENT(OUT), OPTIONAL :: PTCOOL_OCCN
REAL, DIMENSION(:,:), INTENT(OUT), OPTIONAL :: PTCOOL_VCDD
REAL, DIMENSION(:,:), INTENT(OUT), OPTIONAL :: PTCOOL_VCDN
REAL, DIMENSION(:,:), INTENT(OUT), OPTIONAL :: PTCOOL_VCLD
REAL, DIMENSION(:),   INTENT(OUT), OPTIONAL :: PF_WATER_COND
REAL, DIMENSION(:),   INTENT(OUT), OPTIONAL :: PCOP_RAT
REAL, DIMENSION(:),   INTENT(OUT), OPTIONAL :: PHR_TARGET
REAL, DIMENSION(:,:), INTENT(OUT), OPTIONAL :: PQIN
REAL, DIMENSION(:),   INTENT(OUT), OPTIONAL :: PQIN_FRAD
REAL, DIMENSION(:),   INTENT(OUT), OPTIONAL :: PQIN_FLAT
REAL, DIMENSION(:,:), INTENT(OUT), OPTIONAL :: PMODQIN_VCD
REAL, DIMENSION(:,:), INTENT(OUT), OPTIONAL :: PMODQIN_VLD
REAL, DIMENSION(:,:), INTENT(OUT), OPTIONAL :: PMODQIN_NIG
REAL, DIMENSION(:,:), INTENT(OUT), OPTIONAL :: PHOTWAT
REAL, DIMENSION(:,:), INTENT(OUT), OPTIONAL :: PNATVENT
REAL, DIMENSION(:,:), INTENT(OUT), OPTIONAL :: PFVSUM
REAL, DIMENSION(:,:), INTENT(OUT), OPTIONAL :: PFVNIG
REAL, DIMENSION(:),   INTENT(OUT), OPTIONAL :: PTDESV
REAL, DIMENSION(:,:), INTENT(OUT), OPTIONAL :: PFVVAC
REAL, DIMENSION(:),   INTENT(OUT), OPTIONAL :: PFOPEN
REAL, DIMENSION(:,:), INTENT(OUT), OPTIONAL :: PFSSUM
REAL, DIMENSION(:,:), INTENT(OUT), OPTIONAL :: PFSNIG
REAL, DIMENSION(:,:), INTENT(OUT), OPTIONAL :: PFSVAC
REAL, DIMENSION(:),   INTENT(OUT), OPTIONAL :: PWIN_SW_MAX
REAL, DIMENSION(:,:,:), INTENT(OUT), OPTIONAL :: PBEG_HOLIDAY
REAL, DIMENSION(:,:,:), INTENT(OUT), OPTIONAL :: PEND_HOLIDAY
REAL, DIMENSION(:,:), INTENT(OUT), OPTIONAL   :: PMOD_HOLIDAY
REAL, DIMENSION(:,:,:), INTENT(OUT), OPTIONAL :: PDAYWBEG_SCHED
REAL, DIMENSION(:,:,:), INTENT(OUT), OPTIONAL :: PHOURBEG_SCHED
REAL, DIMENSION(:,:,:), INTENT(OUT), OPTIONAL :: PPROBOCC
!
!*    0.2    Declaration of local variables
!      ------------------------------
!
INTEGER, DIMENSION(:), ALLOCATABLE :: ILIST ! link between urban types and indices
INTEGER            :: II           ! Loop counter
INTEGER            :: JLOOP        ! spatial loop counter
INTEGER            :: JLIST        ! loop counter on urban types
INTEGER            :: KLIST        ! loop counter on urban types
INTEGER            :: IINDEX       ! index of type for each point
INTEGER            :: NCTL         ! Control variable
INTEGER            :: ILUOUT       ! Unit number
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('INI_DATA_PARAM_TEB',0,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
!*    1.     Builds the list of indices corresponding to the "type" field
!      ------------------------------------------------------------------
!
ALLOCATE(ILIST(SIZE(KTYPE)))
ILIST(:) = -9999
!
! ##################################################################
! Elimination of unrealistic combinations of input data
! This is only valid for MApUCE data, and an alternative
! is required for other input datasets.
! ##################################################################
!
DO JLIST=1,SIZE(KTYPE)
  !
  ! A) In the case the building type is "high-rise building" and the
  !    construction period is P1, the construction period is set
  !    to P2, since there are no high-rise buildings in P1.
  !
  IF ((DTT%NPAR_BLDTYPE(JLIST).EQ.BDD%NDESC_POS_TYP_BGH).AND.(DTT%NPAR_COL_BLD_AGE(JLIST).EQ.BDD%NDESC_POS_AGE_P1)) THEN
     !
     DTT%NPAR_COL_BLD_AGE(JLIST)=BDD%NDESC_POS_AGE_P2
     !
     CALL BLDCODE(DTT%NPAR_BLDTYPE(JLIST),DTT%NPAR_COL_BLD_AGE(JLIST),DTT%NPAR_USETYPE(JLIST), &
          DTT%NPAR_PXTERRITORY(JLIST),DTT%NPAR_BLDCODE(JLIST))
     !
  ENDIF
  !
  ! B) In the case the building type is "local", the use is set to "LOCAL NON CHAUFFE"
  !
  IF (DTT%NPAR_BLDTYPE(JLIST).EQ.BDD%NDESC_POS_TYP_LOCA) THEN
     !
     DTT%NPAR_USETYPE(JLIST)=BDD%NDESC_POS_USE_LNC
     !
     IF (DTT%NPAR_COL_BLD_AGE(JLIST).EQ.BDD%NDESC_POS_AGE_P1) THEN
        CALL BLDCODE(DTT%NPAR_BLDTYPE(JLIST),DTT%NPAR_COL_BLD_AGE(JLIST),DTT%NPAR_USETYPE(JLIST), &
             DTT%NPAR_P1TERRITORY(JLIST),DTT%NPAR_BLDCODE(JLIST))
     ELSE
        CALL BLDCODE(DTT%NPAR_BLDTYPE(JLIST),DTT%NPAR_COL_BLD_AGE(JLIST),DTT%NPAR_USETYPE(JLIST), &
             DTT%NPAR_PXTERRITORY(JLIST),DTT%NPAR_BLDCODE(JLIST))
     ENDIF
     !
  ENDIF
  !
  ! C) In the case the building type coincides with individual housing, but the
  !    use with collective housing, the use is set to individual housing
  !    The building type is given priority
  !
  IF ( ((DTT%NPAR_BLDTYPE(JLIST).EQ.BDD%NDESC_POS_TYP_PD)  .OR.(DTT%NPAR_BLDTYPE(JLIST).EQ.BDD%NDESC_POS_TYP_PSC).OR. &
        (DTT%NPAR_BLDTYPE(JLIST).EQ.BDD%NDESC_POS_TYP_PCIO).OR.(DTT%NPAR_BLDTYPE(JLIST).EQ.BDD%NDESC_POS_TYP_PCIF)) .AND. &
        (DTT%NPAR_USETYPE(JLIST).EQ.BDD%NDESC_POS_USE_HAC) )  THEN
     !
     DTT%NPAR_USETYPE(JLIST)=BDD%NDESC_POS_USE_HAI
     !
     IF (DTT%NPAR_IND_BLD_AGE(JLIST).EQ.BDD%NDESC_POS_AGE_P1) THEN
        CALL BLDCODE(DTT%NPAR_BLDTYPE(JLIST),DTT%NPAR_IND_BLD_AGE(JLIST),DTT%NPAR_USETYPE(JLIST), &
             DTT%NPAR_P1TERRITORY(JLIST),DTT%NPAR_BLDCODE(JLIST))
     ELSE
        CALL BLDCODE(DTT%NPAR_BLDTYPE(JLIST),DTT%NPAR_IND_BLD_AGE(JLIST),DTT%NPAR_USETYPE(JLIST), &
             DTT%NPAR_PXTERRITORY(JLIST),DTT%NPAR_BLDCODE(JLIST))
     ENDIF
     !
  ENDIF
  !
  ! D) In the case the building type coincides with collective housing, but the
  !    use is INDIVIDUEL, the use is set to COLLECTIF
  !    The building type is given priority
  !
  IF ( ((DTT%NPAR_BLDTYPE(JLIST).EQ.BDD%NDESC_POS_TYP_ID).OR.(DTT%NPAR_BLDTYPE(JLIST).EQ.BDD%NDESC_POS_TYP_ICIO).OR. &
        (DTT%NPAR_BLDTYPE(JLIST).EQ.BDD%NDESC_POS_TYP_ICIF)).AND.(DTT%NPAR_USETYPE(JLIST).EQ.BDD%NDESC_POS_USE_HAI)) THEN
     !
     DTT%NPAR_USETYPE(JLIST)=BDD%NDESC_POS_USE_HAC
     !
     IF (DTT%NPAR_COL_BLD_AGE(JLIST).EQ.BDD%NDESC_POS_AGE_P1) THEN
        CALL BLDCODE(DTT%NPAR_BLDTYPE(JLIST),DTT%NPAR_COL_BLD_AGE(JLIST),DTT%NPAR_USETYPE(JLIST), &
             DTT%NPAR_P1TERRITORY(JLIST),DTT%NPAR_BLDCODE(JLIST))
     ELSE
        CALL BLDCODE(DTT%NPAR_BLDTYPE(JLIST),DTT%NPAR_COL_BLD_AGE(JLIST),DTT%NPAR_USETYPE(JLIST), &
             DTT%NPAR_PXTERRITORY(JLIST),DTT%NPAR_BLDCODE(JLIST))
     ENDIF
     !
  ENDIF
  !
  ! E) In the case the building type is "BATIMENT D'ACTIVITE",
  !    but the use is INDIVIDUEL or COLLECTIF it is set to INDUSTRIEL
  !    The building type is given priority
  !
  IF ( ((DTT%NPAR_USETYPE(JLIST).EQ.BDD%NDESC_POS_USE_HAC).OR.(DTT%NPAR_USETYPE(JLIST).EQ.BDD%NDESC_POS_USE_HAI)) .AND. &
        (DTT%NPAR_BLDTYPE(JLIST).EQ.BDD%NDESC_POS_TYP_BA) )  THEN
     !
     DTT%NPAR_USETYPE(JLIST)=BDD%NDESC_POS_USE_IND
     !
     IF (DTT%NPAR_IND_BLD_AGE(JLIST).EQ.BDD%NDESC_POS_AGE_P1) THEN
        CALL BLDCODE(DTT%NPAR_BLDTYPE(JLIST),DTT%NPAR_IND_BLD_AGE(JLIST),DTT%NPAR_USETYPE(JLIST), &
             DTT%NPAR_P1TERRITORY(JLIST),DTT%NPAR_BLDCODE(JLIST))
     ELSE
        CALL BLDCODE(DTT%NPAR_BLDTYPE(JLIST),DTT%NPAR_IND_BLD_AGE(JLIST),DTT%NPAR_USETYPE(JLIST), &
             DTT%NPAR_PXTERRITORY(JLIST),DTT%NPAR_BLDCODE(JLIST))
     ENDIF
     !
  ENDIF
  !
  ! F) In the case the use is LOCAL NON CHAUFFE, the building type is set to "LOCAL"
  !
  IF (DTT%NPAR_USETYPE(JLIST).EQ.BDD%NDESC_POS_USE_LNC) THEN
     !
     DTT%NPAR_BLDTYPE(JLIST)=BDD%NDESC_POS_TYP_LOCA
     !
     IF (DTT%NPAR_IND_BLD_AGE(JLIST).EQ.BDD%NDESC_POS_AGE_P1) THEN
        CALL BLDCODE(DTT%NPAR_BLDTYPE(JLIST),DTT%NPAR_IND_BLD_AGE(JLIST),DTT%NPAR_USETYPE(JLIST), &
             DTT%NPAR_P1TERRITORY(JLIST),DTT%NPAR_BLDCODE(JLIST))
     ELSE
        CALL BLDCODE(DTT%NPAR_BLDTYPE(JLIST),DTT%NPAR_IND_BLD_AGE(JLIST),DTT%NPAR_USETYPE(JLIST), &
             DTT%NPAR_PXTERRITORY(JLIST),DTT%NPAR_BLDCODE(JLIST))
     ENDIF
     !
  ENDIF
  !
  ! G) For Office use, the building type must be ID/ICIO/ICIF/ for P1 and ID/ICIO/BGH for PX
  !
  IF (DTT%NPAR_USETYPE(JLIST).EQ.BDD%NDESC_POS_USE_TER) THEN
     !
     IF (DTT%NPAR_COL_BLD_AGE(JLIST).EQ.BDD%NDESC_POS_AGE_P1) THEN
        !
        IF ((DTT%NPAR_BLDTYPE(JLIST).NE.BDD%NDESC_POS_TYP_ID).AND.(DTT%NPAR_BLDTYPE(JLIST).NE.BDD%NDESC_POS_TYP_ICIO).AND. &
            (DTT%NPAR_BLDTYPE(JLIST).NE.BDD%NDESC_POS_TYP_ICIF)) THEN
           !
           DTT%NPAR_BLDTYPE(JLIST)=BDD%NDESC_POS_TYP_ICIO
           !
           CALL BLDCODE(DTT%NPAR_BLDTYPE(JLIST),DTT%NPAR_COL_BLD_AGE(JLIST), &
                DTT%NPAR_USETYPE(JLIST),DTT%NPAR_P1TERRITORY(JLIST), &
                DTT%NPAR_BLDCODE(JLIST))
           !
        ENDIF
        !
     ELSE
        !
        IF ((DTT%NPAR_BLDTYPE(JLIST).NE.BDD%NDESC_POS_TYP_ID).AND.(DTT%NPAR_BLDTYPE(JLIST).NE.BDD%NDESC_POS_TYP_ICIO).AND. &
            (DTT%NPAR_BLDTYPE(JLIST).NE.BDD%NDESC_POS_TYP_BGH)) THEN
           !
           DTT%NPAR_BLDTYPE(JLIST)=BDD%NDESC_POS_TYP_ICIO
           !
           CALL BLDCODE(DTT%NPAR_BLDTYPE(JLIST),DTT%NPAR_COL_BLD_AGE(JLIST), &
                DTT%NPAR_USETYPE(JLIST),DTT%NPAR_PXTERRITORY(JLIST), &
                DTT%NPAR_BLDCODE(JLIST))
           !
        ENDIF
        !
     ENDIF
     !
  ENDIF
  !
  ! H) In the case the use is SANTE and the building type is not ICIF or ICIO, the building type is set to ICIO.
  !
  IF ( (DTT%NPAR_USETYPE(JLIST).EQ.BDD%NDESC_POS_USE_SAN).AND. &
      ((DTT%NPAR_BLDTYPE(JLIST).NE.BDD%NDESC_POS_TYP_ICIF).AND.(DTT%NPAR_BLDTYPE(JLIST).NE.BDD%NDESC_POS_TYP_ICIO)) )  THEN
      !
      DTT%NPAR_BLDTYPE(JLIST)=BDD%NDESC_POS_TYP_ICIO
      !
      IF (DTT%NPAR_COL_BLD_AGE(JLIST).EQ.BDD%NDESC_POS_AGE_P1) THEN
         CALL BLDCODE(DTT%NPAR_BLDTYPE(JLIST),DTT%NPAR_COL_BLD_AGE(JLIST),DTT%NPAR_USETYPE(JLIST), &
              DTT%NPAR_P1TERRITORY(JLIST),DTT%NPAR_BLDCODE(JLIST))
      ELSE
         CALL BLDCODE(DTT%NPAR_BLDTYPE(JLIST),DTT%NPAR_COL_BLD_AGE(JLIST),DTT%NPAR_USETYPE(JLIST), &
              DTT%NPAR_PXTERRITORY(JLIST),DTT%NPAR_BLDCODE(JLIST))
      ENDIF
      !
  ENDIF
  !
  ! I) In the case the use is ENSEIGNEMENT and the building type 
  !    is not ID/ICIO/ICIF the building type is set to ICIO.
  !
  IF ( (DTT%NPAR_USETYPE(JLIST).EQ.BDD%NDESC_POS_USE_ENS).AND. &
       ((DTT%NPAR_BLDTYPE(JLIST).NE.BDD%NDESC_POS_TYP_ID).AND.(DTT%NPAR_BLDTYPE(JLIST).NE.BDD%NDESC_POS_TYP_ICIO).AND. &
        (DTT%NPAR_BLDTYPE(JLIST).NE.BDD%NDESC_POS_TYP_ICIF))) THEN
      !
      DTT%NPAR_BLDTYPE(JLIST)=BDD%NDESC_POS_TYP_ICIO
      !
      IF (DTT%NPAR_COL_BLD_AGE(JLIST).EQ.BDD%NDESC_POS_AGE_P1) THEN
         CALL BLDCODE(DTT%NPAR_BLDTYPE(JLIST),DTT%NPAR_COL_BLD_AGE(JLIST),DTT%NPAR_USETYPE(JLIST), &
              DTT%NPAR_P1TERRITORY(JLIST),DTT%NPAR_BLDCODE(JLIST))
      ELSE
         CALL BLDCODE(DTT%NPAR_BLDTYPE(JLIST),DTT%NPAR_COL_BLD_AGE(JLIST),DTT%NPAR_USETYPE(JLIST), &
              DTT%NPAR_PXTERRITORY(JLIST),DTT%NPAR_BLDCODE(JLIST))
      ENDIF
      !
   ENDIF
   !
   ! K) In the case the use is CHATEAU the building type must be ID and the period must be P1
   !
   IF (DTT%NPAR_USETYPE(JLIST).EQ.BDD%NDESC_POS_USE_CHA) THEN
     !
     DTT%NPAR_BLDTYPE(JLIST)     = BDD%NDESC_POS_TYP_ID
     DTT%NPAR_COL_BLD_AGE(JLIST) = BDD%NDESC_POS_AGE_P1
     DTT%NPAR_IND_BLD_AGE(JLIST) = BDD%NDESC_POS_AGE_P1
     !
     CALL BLDCODE(DTT%NPAR_BLDTYPE(JLIST),DTT%NPAR_COL_BLD_AGE(JLIST),DTT%NPAR_USETYPE(JLIST), &
                  DTT%NPAR_P1TERRITORY(JLIST),DTT%NPAR_BLDCODE(JLIST))
     !
  ENDIF
  !
  ! L) In the case the use is SPORTIF the building type must be BA
  !
  IF (DTT%NPAR_USETYPE(JLIST).EQ.BDD%NDESC_POS_USE_SPO) THEN
     !
     DTT%NPAR_BLDTYPE(JLIST) = BDD%NDESC_POS_TYP_BA
     !
     IF (DTT%NPAR_IND_BLD_AGE(JLIST).EQ.BDD%NDESC_POS_AGE_P1) THEN
        CALL BLDCODE(DTT%NPAR_BLDTYPE(JLIST),DTT%NPAR_IND_BLD_AGE(JLIST),DTT%NPAR_USETYPE(JLIST), &
             DTT%NPAR_P1TERRITORY(JLIST),DTT%NPAR_BLDCODE(JLIST))
     ELSE
        CALL BLDCODE(DTT%NPAR_BLDTYPE(JLIST),DTT%NPAR_IND_BLD_AGE(JLIST),DTT%NPAR_USETYPE(JLIST), &
             DTT%NPAR_PXTERRITORY(JLIST),DTT%NPAR_BLDCODE(JLIST))
     ENDIF
     !
  ENDIF
  !
  ! M) In the case the use is RELIGIEUX the building type must be ID/ICIF
  !
  IF ( (DTT%NPAR_USETYPE(JLIST).EQ.BDD%NDESC_POS_USE_REL) .AND. &
      ((DTT%NPAR_BLDTYPE(JLIST).NE.BDD%NDESC_POS_TYP_ID).AND.(DTT%NPAR_BLDTYPE(JLIST).NE.BDD%NDESC_POS_TYP_ICIF)) ) THEN
      !
      DTT%NPAR_BLDTYPE(JLIST)=BDD%NDESC_POS_TYP_ICIF
      !
      IF (DTT%NPAR_COL_BLD_AGE(JLIST).EQ.BDD%NDESC_POS_AGE_P1) THEN
         CALL BLDCODE(DTT%NPAR_BLDTYPE(JLIST),DTT%NPAR_COL_BLD_AGE(JLIST),DTT%NPAR_USETYPE(JLIST), &
             DTT%NPAR_P1TERRITORY(JLIST),DTT%NPAR_BLDCODE(JLIST))
      ELSE
         CALL BLDCODE(DTT%NPAR_BLDTYPE(JLIST),DTT%NPAR_COL_BLD_AGE(JLIST),DTT%NPAR_USETYPE(JLIST), &
             DTT%NPAR_PXTERRITORY(JLIST),DTT%NPAR_BLDCODE(JLIST))
      ENDIF
      !
  ENDIF
  !
  ! N) In the case the use is AGRICOLE the building type must be BA
  !
  IF (DTT%NPAR_USETYPE(JLIST).EQ.BDD%NDESC_POS_USE_AGR)  THEN
     !
     DTT%NPAR_BLDTYPE(JLIST)=BDD%NDESC_POS_TYP_BA
     !
     IF (DTT%NPAR_IND_BLD_AGE(JLIST).EQ.BDD%NDESC_POS_AGE_P1) THEN
        CALL BLDCODE(DTT%NPAR_BLDTYPE(JLIST),DTT%NPAR_IND_BLD_AGE(JLIST),DTT%NPAR_USETYPE(JLIST), &
             DTT%NPAR_P1TERRITORY(JLIST),DTT%NPAR_BLDCODE(JLIST))
     ELSE
        CALL BLDCODE(DTT%NPAR_BLDTYPE(JLIST),DTT%NPAR_IND_BLD_AGE(JLIST),DTT%NPAR_USETYPE(JLIST), &
             DTT%NPAR_PXTERRITORY(JLIST),DTT%NPAR_BLDCODE(JLIST))
     ENDIF
     !
  ENDIF
  !
  ! O) In the case the use is SERRE the building type must be BA
  !
  IF (DTT%NPAR_USETYPE(JLIST).EQ.BDD%NDESC_POS_USE_SER)  THEN
     !
     DTT%NPAR_BLDTYPE(JLIST)=BDD%NDESC_POS_TYP_BA
     !
     IF (DTT%NPAR_IND_BLD_AGE(JLIST).EQ.BDD%NDESC_POS_AGE_P1) THEN
        CALL BLDCODE(DTT%NPAR_BLDTYPE(JLIST),DTT%NPAR_IND_BLD_AGE(JLIST),DTT%NPAR_USETYPE(JLIST), &
             DTT%NPAR_P1TERRITORY(JLIST),DTT%NPAR_BLDCODE(JLIST))
     ELSE
        CALL BLDCODE(DTT%NPAR_BLDTYPE(JLIST),DTT%NPAR_IND_BLD_AGE(JLIST),DTT%NPAR_USETYPE(JLIST), &
             DTT%NPAR_PXTERRITORY(JLIST),DTT%NPAR_BLDCODE(JLIST))
     ENDIF
     !
  ENDIF
  !
  ! P) In the case the use is INDUSTRIEL the building type must be BA
  !
  IF (DTT%NPAR_USETYPE(JLIST).EQ.BDD%NDESC_POS_USE_IND)  THEN
      !
      DTT%NPAR_BLDTYPE(JLIST)=BDD%NDESC_POS_TYP_BA
      !
      IF (DTT%NPAR_IND_BLD_AGE(JLIST).EQ.BDD%NDESC_POS_AGE_P1) THEN
         CALL BLDCODE(DTT%NPAR_BLDTYPE(JLIST),DTT%NPAR_IND_BLD_AGE(JLIST),DTT%NPAR_USETYPE(JLIST), &
              DTT%NPAR_P1TERRITORY(JLIST),DTT%NPAR_BLDCODE(JLIST))
      ELSE
         CALL BLDCODE(DTT%NPAR_BLDTYPE(JLIST),DTT%NPAR_IND_BLD_AGE(JLIST),DTT%NPAR_USETYPE(JLIST), &
              DTT%NPAR_PXTERRITORY(JLIST),DTT%NPAR_BLDCODE(JLIST))
      ENDIF
      !
   ENDIF
   !
   ! Q) In the case the use is COMMERCE the building type must be 
   !    ID/ICIO/ICIF for P1
   !    ID/ICIF for P2 to P3
   !    ID/ICIF/BA for P4 to P7 and ID/ICIF for P1 to P3
   !
   IF (DTT%NPAR_USETYPE(JLIST).EQ.BDD%NDESC_POS_USE_COM) THEN
      !
      IF (DTT%NPAR_COL_BLD_AGE(JLIST).EQ.BDD%NDESC_POS_AGE_P1) THEN
         !
         IF ((DTT%NPAR_BLDTYPE(JLIST).NE.BDD%NDESC_POS_TYP_ID).AND.(DTT%NPAR_BLDTYPE(JLIST).NE.BDD%NDESC_POS_TYP_ICIF).AND. &
            (DTT%NPAR_BLDTYPE(JLIST).NE.BDD%NDESC_POS_TYP_ICIO)) THEN
            !
            DTT%NPAR_BLDTYPE(JLIST)=BDD%NDESC_POS_TYP_ICIF
            !
            CALL BLDCODE(DTT%NPAR_BLDTYPE(JLIST),DTT%NPAR_COL_BLD_AGE(JLIST),DTT%NPAR_USETYPE(JLIST), &
                         DTT%NPAR_P1TERRITORY(JLIST),DTT%NPAR_BLDCODE(JLIST))
            !
         ENDIF
         !
      ELSEIF (DTT%NPAR_COL_BLD_AGE(JLIST).LT.BDD%NDESC_POS_AGE_P4) THEN
         !
         IF ((DTT%NPAR_BLDTYPE(JLIST).NE.BDD%NDESC_POS_TYP_ID).AND.(DTT%NPAR_BLDTYPE(JLIST).NE.BDD%NDESC_POS_TYP_ICIF)) THEN
            !
            DTT%NPAR_BLDTYPE(JLIST)=BDD%NDESC_POS_TYP_ICIF
            !
            CALL BLDCODE(DTT%NPAR_BLDTYPE(JLIST),DTT%NPAR_COL_BLD_AGE(JLIST),DTT%NPAR_USETYPE(JLIST), &
                         DTT%NPAR_PXTERRITORY(JLIST),DTT%NPAR_BLDCODE(JLIST))
            !
         ENDIF
         !
      ELSE
         !
         IF ((DTT%NPAR_BLDTYPE(JLIST).NE.BDD%NDESC_POS_TYP_ID).AND.(DTT%NPAR_BLDTYPE(JLIST).NE.BDD%NDESC_POS_TYP_ICIF).AND. &
             (DTT%NPAR_BLDTYPE(JLIST).NE.BDD%NDESC_POS_TYP_BA)) THEN
            !
            DTT%NPAR_BLDTYPE(JLIST)=BDD%NDESC_POS_TYP_BA
            !
            CALL BLDCODE(DTT%NPAR_BLDTYPE(JLIST),DTT%NPAR_COL_BLD_AGE(JLIST),DTT%NPAR_USETYPE(JLIST), &
                         DTT%NPAR_PXTERRITORY(JLIST),DTT%NPAR_BLDCODE(JLIST))
            !
         ENDIF
         !
      ENDIF
      !
   ENDIF
   !
   ! R) In the case the use is COLLECTIF the building type must be ID/ICIO/ICIF/BGH
   !
   IF ( (DTT%NPAR_USETYPE(JLIST).EQ.BDD%NDESC_POS_USE_HAC) .AND. &
       ((DTT%NPAR_BLDTYPE(JLIST).NE.BDD%NDESC_POS_TYP_ID)  .AND.(DTT%NPAR_BLDTYPE(JLIST).NE.BDD%NDESC_POS_TYP_ICIO).AND. &
        (DTT%NPAR_BLDTYPE(JLIST).NE.BDD%NDESC_POS_TYP_ICIF).AND.(DTT%NPAR_BLDTYPE(JLIST).NE.BDD%NDESC_POS_TYP_BGH) ) )  THEN
      !
      DTT%NPAR_BLDTYPE(JLIST)=BDD%NDESC_POS_TYP_ICIO
      !
      IF (DTT%NPAR_COL_BLD_AGE(JLIST).EQ.BDD%NDESC_POS_AGE_P1) THEN
         CALL BLDCODE(DTT%NPAR_BLDTYPE(JLIST),DTT%NPAR_COL_BLD_AGE(JLIST),DTT%NPAR_USETYPE(JLIST), &
              DTT%NPAR_P1TERRITORY(JLIST),DTT%NPAR_BLDCODE(JLIST))
      ELSE
         CALL BLDCODE(DTT%NPAR_BLDTYPE(JLIST),DTT%NPAR_COL_BLD_AGE(JLIST),DTT%NPAR_USETYPE(JLIST), &
              DTT%NPAR_PXTERRITORY(JLIST),DTT%NPAR_BLDCODE(JLIST))
      ENDIF
      !
   ENDIF
   !
   ! S) In the case the use is INDIVIDUEL the building type must be PD/PSC/PCIO/PCIF
   !
   IF ( (DTT%NPAR_USETYPE(JLIST).EQ.BDD%NDESC_POS_USE_HAI) .AND. &
       ((DTT%NPAR_BLDTYPE(JLIST).NE.BDD%NDESC_POS_TYP_PD)  .AND.(DTT%NPAR_BLDTYPE(JLIST).NE.BDD%NDESC_POS_TYP_PSC).AND. &
       (DTT%NPAR_BLDTYPE(JLIST).NE.BDD%NDESC_POS_TYP_PCIO).AND.(DTT%NPAR_BLDTYPE(JLIST).NE.BDD%NDESC_POS_TYP_PCIF) ))  THEN
       !
       DTT%NPAR_BLDTYPE(JLIST)=BDD%NDESC_POS_TYP_PD
       !
       IF (DTT%NPAR_IND_BLD_AGE(JLIST).EQ.BDD%NDESC_POS_AGE_P1) THEN
          CALL BLDCODE(DTT%NPAR_BLDTYPE(JLIST),DTT%NPAR_IND_BLD_AGE(JLIST),DTT%NPAR_USETYPE(JLIST), &
               DTT%NPAR_P1TERRITORY(JLIST),DTT%NPAR_BLDCODE(JLIST))
       ELSE
          CALL BLDCODE(DTT%NPAR_BLDTYPE(JLIST),DTT%NPAR_IND_BLD_AGE(JLIST),DTT%NPAR_USETYPE(JLIST), &
               DTT%NPAR_PXTERRITORY(JLIST),DTT%NPAR_BLDCODE(JLIST))
       ENDIF
       !
   ENDIF
   !
ENDDO
!
! ##################################################################
! Treatment of missing combinations of building type,
! use, construction period and territory
! ##################################################################
!
DO JLIST=1,SIZE(KTYPE)
   !
   ! Test matching of initial building code
   !
   KTYPE(JLIST)=DTT%NPAR_BLDCODE(JLIST)
   !
   NCTL=0
   DO KLIST=1,SIZE(BDD%NDESC_CODE_LIST)
      IF (KTYPE(JLIST).EQ.BDD%NDESC_CODE_LIST(KLIST)) THEN
         ILIST(JLIST) = KLIST
         NCTL=NCTL+1
      ENDIF
   END DO
   !
   ! There must only be one line in the .csv-table corresponding
   ! to the given building code
   !
   IF (NCTL.GT.1) CALL ABOR1_SFX("More than one line in csv-file corresponds to input")
   !
   ! Treatment of missing data
   !
   IF (NCTL.EQ.0) THEN
      !
      ! If the construction period is larger than 1, the territory is set to default
      !
      IF ((DTT%NPAR_BLDTYPE(JLIST).NE.BDD%NDESC_POS_TYP_PD)  .AND.(DTT%NPAR_BLDTYPE(JLIST).NE.BDD%NDESC_POS_TYP_PSC).AND. &
          (DTT%NPAR_BLDTYPE(JLIST).NE.BDD%NDESC_POS_TYP_PCIO).AND.(DTT%NPAR_BLDTYPE(JLIST).NE.BDD%NDESC_POS_TYP_PCIF)) THEN
         !
         IF (DTT%NPAR_COL_BLD_AGE(JLIST).NE.BDD%NDESC_POS_AGE_P1) THEN
            !
            DTT%NPAR_PXTERRITORY(JLIST)=BDD%NDESC_POS_PX_DEFAULT
            !
            CALL BLDCODE(DTT%NPAR_BLDTYPE(JLIST),DTT%NPAR_COL_BLD_AGE(JLIST),DTT%NPAR_USETYPE(JLIST), &
                 DTT%NPAR_PXTERRITORY(JLIST),DTT%NPAR_BLDCODE(JLIST))
            !
            KTYPE(JLIST)=DTT%NPAR_BLDCODE(JLIST)
            !
         ENDIF
         !
      ELSE
         !
         IF (DTT%NPAR_IND_BLD_AGE(JLIST).NE.BDD%NDESC_POS_AGE_P1) THEN
            !
            DTT%NPAR_PXTERRITORY(JLIST)=BDD%NDESC_POS_PX_DEFAULT
            !
            CALL BLDCODE(DTT%NPAR_BLDTYPE(JLIST),DTT%NPAR_IND_BLD_AGE(JLIST),DTT%NPAR_USETYPE(JLIST), &
                 DTT%NPAR_PXTERRITORY(JLIST),DTT%NPAR_BLDCODE(JLIST))
            !
            KTYPE(JLIST)=DTT%NPAR_BLDCODE(JLIST)
            !
         ENDIF
         !
      ENDIF
      !
      ! Test matching of modified building code
      !
      NCTL=0
      DO KLIST=1,SIZE(BDD%NDESC_CODE_LIST)
         IF (KTYPE(JLIST).EQ.BDD%NDESC_CODE_LIST(KLIST)) THEN
            ILIST(JLIST) = KLIST
            NCTL=NCTL+1
         ENDIF
      ENDDO
      IF (NCTL.GT.1) CALL ABOR1_SFX("More than one line in csv-file corresponds to input")
      !
      IF (NCTL.EQ.0) THEN
         !
         ! In case of still missing arcitectural description,
         ! the constuction period is increased until a match is found.
         ! It is assmed, that the construction period numbering
         ! increases towards more recent periods
         !
         DO 
            !
            IF ((DTT%NPAR_BLDTYPE(JLIST).NE.BDD%NDESC_POS_TYP_PD)  .AND.(DTT%NPAR_BLDTYPE(JLIST).NE.BDD%NDESC_POS_TYP_PSC).AND. &
                (DTT%NPAR_BLDTYPE(JLIST).NE.BDD%NDESC_POS_TYP_PCIO).AND.(DTT%NPAR_BLDTYPE(JLIST).NE.BDD%NDESC_POS_TYP_PCIF)) THEN
               !
               IF (DTT%NPAR_COL_BLD_AGE(JLIST).NE.BDD%NDESC_POS_AGE_P7) THEN
                  !
                  DTT%NPAR_COL_BLD_AGE(JLIST)=DTT%NPAR_COL_BLD_AGE(JLIST)+1
                  !
                  CALL BLDCODE(DTT%NPAR_BLDTYPE(JLIST),DTT%NPAR_COL_BLD_AGE(JLIST),DTT%NPAR_USETYPE(JLIST), &
                       DTT%NPAR_PXTERRITORY(JLIST),DTT%NPAR_BLDCODE(JLIST))
                  !
                  KTYPE(JLIST)=DTT%NPAR_BLDCODE(JLIST)
                  !
               ELSE
                  CALL GET_LUOUT(HPROGRAM,ILUOUT)
                  WRITE(ILUOUT,*) "                                                          "
                  WRITE(ILUOUT,*) "In ini_data_param_teb                                     "
                  WRITE(ILUOUT,*) "No building archetype found for the following combination "
                  WRITE(ILUOUT,*) "of building type, use, construction period and territory  "
                  WRITE(ILUOUT,*) "                            "
                  WRITE(ILUOUT,*) "DTT%NPAR_BLDTYPE(JLIST)     ",DTT%NPAR_BLDTYPE(JLIST)
                  WRITE(ILUOUT,*) "DTT%NPAR_COL_BLD_AGE(JLIST) ",DTT%NPAR_COL_BLD_AGE(JLIST)
                  WRITE(ILUOUT,*) "DTT%NPAR_USETYPE(JLIST)     ",DTT%NPAR_USETYPE(JLIST)
                  WRITE(ILUOUT,*) "DTT%NPAR_PXTERRITORY(JLIST) ",DTT%NPAR_PXTERRITORY(JLIST)
                  CALL FLUSH(ILUOUT)
                  CALL ABOR1_SFX ("Missing building archetype, check report")
               ENDIF
               !
            ELSE
               !
               IF (DTT%NPAR_IND_BLD_AGE(JLIST).NE.BDD%NDESC_POS_AGE_P7) THEN
                  !
                  DTT%NPAR_IND_BLD_AGE(JLIST)=DTT%NPAR_IND_BLD_AGE(JLIST)+1
                  !
                  CALL BLDCODE(DTT%NPAR_BLDTYPE(JLIST),DTT%NPAR_IND_BLD_AGE(JLIST),DTT%NPAR_USETYPE(JLIST), &
                       DTT%NPAR_PXTERRITORY(JLIST),DTT%NPAR_BLDCODE(JLIST))
                  !
                  KTYPE(JLIST)=DTT%NPAR_BLDCODE(JLIST)
                  !
               ELSE
                  CALL GET_LUOUT(HPROGRAM,ILUOUT)
                  WRITE(ILUOUT,*) "                                                          "
                  WRITE(ILUOUT,*) "In ini_data_param_teb                                     "
                  WRITE(ILUOUT,*) "No building archetype found for the following combination "
                  WRITE(ILUOUT,*) "of building type, use, construction period and territory  "
                  WRITE(ILUOUT,*) "                            "
                  WRITE(ILUOUT,*) "DTT%NPAR_BLDTYPE(JLIST)     ",DTT%NPAR_BLDTYPE(JLIST)
                  WRITE(ILUOUT,*) "DTT%NPAR_IND_BLD_AGE(JLIST) ",DTT%NPAR_IND_BLD_AGE(JLIST)
                  WRITE(ILUOUT,*) "DTT%NPAR_USETYPE(JLIST)     ",DTT%NPAR_USETYPE(JLIST)
                  WRITE(ILUOUT,*) "DTT%NPAR_PXTERRITORY(JLIST) ",DTT%NPAR_PXTERRITORY(JLIST)
                  CALL FLUSH(ILUOUT)
                  CALL ABOR1_SFX ("Missing building archetype, check report")
               ENDIF
               !
            ENDIF
            !
            ! Test matching of modified building code
            !
            NCTL=0
            DO KLIST=1,SIZE(BDD%NDESC_CODE_LIST)
               IF (KTYPE(JLIST).EQ.BDD%NDESC_CODE_LIST(KLIST)) THEN
                  ILIST(JLIST) = KLIST
                  NCTL=NCTL+1
               ENDIF
            ENDDO
            !
            IF (NCTL.GT.0) THEN
               EXIT
            ENDIF
            !
            ! Set territory to default
            !
            DTT%NPAR_PXTERRITORY(JLIST)=BDD%NDESC_POS_PX_DEFAULT
            !
            IF ((DTT%NPAR_BLDTYPE(JLIST).NE.BDD%NDESC_POS_TYP_PD)  .AND.(DTT%NPAR_BLDTYPE(JLIST).NE.BDD%NDESC_POS_TYP_PSC).AND.&
               (DTT%NPAR_BLDTYPE(JLIST).NE.BDD%NDESC_POS_TYP_PCIO).AND.(DTT%NPAR_BLDTYPE(JLIST).NE.BDD%NDESC_POS_TYP_PCIF)) THEN
               CALL BLDCODE(DTT%NPAR_BLDTYPE(JLIST),DTT%NPAR_COL_BLD_AGE(JLIST),DTT%NPAR_USETYPE(JLIST), &
                    DTT%NPAR_PXTERRITORY(JLIST),DTT%NPAR_BLDCODE(JLIST))
            ELSE
               CALL BLDCODE(DTT%NPAR_BLDTYPE(JLIST),DTT%NPAR_IND_BLD_AGE(JLIST),DTT%NPAR_USETYPE(JLIST), &
                    DTT%NPAR_PXTERRITORY(JLIST),DTT%NPAR_BLDCODE(JLIST))
            ENDIF
            !
            KTYPE(JLIST)=DTT%NPAR_BLDCODE(JLIST)
            !
            ! Test matching of modified building code
            !
            NCTL=0
            DO KLIST=1,SIZE(BDD%NDESC_CODE_LIST)
               IF (KTYPE(JLIST).EQ.BDD%NDESC_CODE_LIST(KLIST)) THEN
                  ILIST(JLIST) = KLIST
                  NCTL=NCTL+1
               ENDIF
            ENDDO
            !
            IF (NCTL.GT.0) THEN
               EXIT
            ENDIF
            !
         ENDDO
         !
      ENDIF
      !
   ENDIF
   !
ENDDO
!
! ###########################################################
! Initialisation of architectural characteristics 
! ###########################################################
!
IINDEX = 0
!
!-------------------------------------------------------------------------------
DO JLOOP=1,SIZE(KTYPE)
!-------------------------------------------------------------------------------
!
IINDEX=ILIST(JLOOP)
! 
!*    1.     town parameters depending on urban characteristics
!      --------------------------------------------------------
!
! Road
!
    IF (PRESENT(PALB_ROAD)) THEN
      PALB_ROAD(JLOOP)=BDD%XDESC_ALB_ROAD(IINDEX)
    ENDIF
    IF (PRESENT(PEMIS_ROAD)) THEN
      PEMIS_ROAD(JLOOP)=BDD%XDESC_EMIS_ROAD(IINDEX)
    ENDIF
    IF (PRESENT(PHC_ROAD)) THEN
      PHC_ROAD(JLOOP,:)= BDD%XDESC_HC_ROAD(IINDEX,:)
    ENDIF
    IF (PRESENT(PTC_ROAD)) THEN
      PTC_ROAD(JLOOP,:)= BDD%XDESC_TC_ROAD(IINDEX,:)
    ENDIF
    IF (PRESENT(PD_ROAD)) THEN
      PD_ROAD(JLOOP,:)= BDD%XDESC_D_ROAD(IINDEX,:)
    ENDIF
!
! Roof
!
    IF (PRESENT(PALB_ROOF)) THEN
      PALB_ROOF(JLOOP)=BDD%XDESC_ALB_ROOF(IINDEX)
    ENDIF
    IF (PRESENT(PEMIS_ROOF)) THEN
      PEMIS_ROOF(JLOOP)=BDD%XDESC_EMIS_ROOF(IINDEX)
    ENDIF
    IF (PRESENT(PHC_ROOF)) THEN
      PHC_ROOF(JLOOP,:)= BDD%XDESC_HC_ROOF(IINDEX,:)
    ENDIF
    IF (PRESENT(PTC_ROOF)) THEN
      PTC_ROOF(JLOOP,:)= BDD%XDESC_TC_ROOF(IINDEX,:)
    ENDIF
    IF (PRESENT(PD_ROOF)) THEN
      PD_ROOF(JLOOP,:)= BDD%XDESC_D_ROOF(IINDEX,:)
    ENDIF
!
! Wall
!
    IF (PRESENT(PALB_WALL)) THEN
      PALB_WALL(JLOOP)=BDD%XDESC_ALB_WALL(IINDEX)
    ENDIF
    IF (PRESENT(PEMIS_WALL)) THEN
      PEMIS_WALL(JLOOP)=BDD%XDESC_EMIS_WALL(IINDEX)
    ENDIF
    IF (PRESENT(PHC_WALL)) THEN
      PHC_WALL(JLOOP,:)= BDD%XDESC_HC_WALL(IINDEX,:)
    ENDIF
    IF (PRESENT(PTC_WALL)) THEN
      PTC_WALL(JLOOP,:)= BDD%XDESC_TC_WALL(IINDEX,:)
    ENDIF
    IF (PRESENT(PD_WALL)) THEN
      PD_WALL(JLOOP,:)= BDD%XDESC_D_WALL(IINDEX,:)
    ENDIF
!
! Position of isolation layers
!
    IF (PRESENT(NISOROOFPOS)) THEN
       NISOROOFPOS(JLOOP) = BDD%NDESC_ISOROOFPOS(IINDEX)
    ENDIF
     IF (PRESENT(NISOWALLPOS)) THEN
        NISOWALLPOS(JLOOP) = BDD%NDESC_ISOWALLPOS(IINDEX)
     ENDIF
!
! Floor
!
    IF (PRESENT(PHC_FLOOR)) THEN
      PHC_FLOOR(JLOOP,:)= BDD%XDESC_HC_FLOOR(IINDEX,:)
    ENDIF
    IF (PRESENT(PTC_FLOOR)) THEN
      PTC_FLOOR(JLOOP,:)= BDD%XDESC_TC_FLOOR(IINDEX,:)
    ENDIF
    IF (PRESENT(PD_FLOOR)) THEN
      PD_FLOOR(JLOOP,:)= BDD%XDESC_D_FLOOR(IINDEX,:)
    ENDIF
!
! Mass
!
    IF (PRESENT(PHC_MASS)) THEN
       PHC_MASS(JLOOP,:)= BDD%XDESC_HC_MASS(IINDEX,:)
    ENDIF
    IF (PRESENT(PTC_MASS)) THEN
       PTC_MASS(JLOOP,:)= BDD%XDESC_TC_MASS(IINDEX,:)
    ENDIF
    IF (PRESENT(PD_MASS)) THEN
       PD_MASS(JLOOP,:)= BDD%XDESC_D_MASS(IINDEX,:)
    ENDIF
    IF (PRESENT(PISMASS)) THEN
       PISMASS(JLOOP) = BDD%XDESC_ISMASS(IINDEX)
    ENDIF
!
    IF (PRESENT(PN50)) THEN
       PN50(JLOOP) = BDD%XDESC_N50(IINDEX)
    ENDIF
!
    IF (PRESENT(PGR)) THEN
      PGR(JLOOP) = BDD%XDESC_GR(IINDEX)
    ENDIF
!
    IF (PRESENT(PU_WIN)) THEN
       PU_WIN(JLOOP) = BDD%XDESC_U_WIN(IINDEX)
    ENDIF
!
    IF (PRESENT(PSHGC)) THEN
       PSHGC(JLOOP) = BDD%XDESC_SHGC(IINDEX)
    ENDIF
!
    IF (PRESENT(PSHGC_SH)) THEN
       PSHGC_SH(JLOOP) = BDD%XDESC_SHGC_SH(IINDEX)
    ENDIF
    !
    IF (PRESENT(PSHADEARCHI)) THEN
       PSHADEARCHI(JLOOP) = BDD%XDESC_SHADEARCHI(IINDEX)
    ENDIF
!
    IF (PRESENT(PISMECH)) THEN
       PISMECH(JLOOP) = BDD%XDESC_ISMECH(IINDEX)
    ENDIF
!
    IF (PRESENT(PMECHRATE)) THEN
       PMECHRATE(JLOOP) = BDD%XDESC_MECHRATE(IINDEX)
    ENDIF
    !
    IF (PRESENT(PGREENROOF)) THEN
      PGREENROOF(JLOOP) = BDD%XDESC_GREENROOF(IINDEX)
    ENDIF
    !
    IF (PRESENT(PEMIS_PANEL)) THEN
      PEMIS_PANEL(JLOOP) = BDD%XDESC_EMIS_PANEL(IINDEX)
    ENDIF
    !
    IF (PRESENT(PALB_PANEL)) THEN
      PALB_PANEL(JLOOP) = BDD%XDESC_ALB_PANEL(IINDEX)
    ENDIF
    !
    IF (PRESENT(PEFF_PANEL)) THEN
      PEFF_PANEL(JLOOP) = BDD%XDESC_EFF_PANEL(IINDEX)
    ENDIF
    !
    IF (PRESENT(PFRAC_PANEL)) THEN
      PFRAC_PANEL(JLOOP) = BDD%XDESC_FRAC_PANEL(IINDEX)
    ENDIF
    !
!      ----------------------------------------------------
! Architectural parameters not read from table
!      ---------------------------------------------------
!
    IF (PRESENT(PCAP_SYS_HEAT)) THEN
      PCAP_SYS_HEAT(JLOOP) = 100.
    ENDIF
!
    IF (PRESENT(PCAP_SYS_RAT)) THEN
      PCAP_SYS_RAT(JLOOP) = 90.
    ENDIF
!
    IF (PRESENT(PT_ADP)) THEN
      PT_ADP(JLOOP) = 285.66
    ENDIF
!
    IF (PRESENT(PM_SYS_RAT)) THEN
      PM_SYS_RAT(JLOOP) = 0.0067
    ENDIF
!
    IF (PRESENT(PT_SIZE_MAX)) THEN
      PT_SIZE_MAX(JLOOP) = 301.95
    ENDIF
!
    IF (PRESENT(PT_SIZE_MIN)) THEN
      PT_SIZE_MIN(JLOOP) = 268.96
    ENDIF
!
    IF (PRESENT(PFLOOR_HEIGHT)) THEN
      PFLOOR_HEIGHT(JLOOP) = 2.9
    ENDIF
!
    IF (PRESENT(PROUGH_ROOF)) THEN
      PROUGH_ROOF(JLOOP) = 1.52
    ENDIF
!
    IF (PRESENT(PROUGH_WALL)) THEN
      PROUGH_WALL(JLOOP) = 1.52
    ENDIF

!
END DO
!
DEALLOCATE(ILIST)
!
! ###########################################################
! Initialisation of behavioural characteristics
! When using the building description files, it is possible
! to simulate with 1 or 6 compartments
! For 6-compartments-case, the compartments are defined as follows
! - (1): FORT CRE   (low design temperature, if applicable)
! - (2): FAIBLE CRE (high design temperature, if applicable)
! - (3): Mean between FORT CRE and FAIBLE CRE (if applicable)
! - (4): non-heated fraction of the building
! - (5): COMMERCIAL fraction (if applicable)
! - (6): TERTIARE   fraction (if applicable)
!
! For the 1-compartment-case, the input values are averaged
! to represent the variety of behaviours as good as possible
!
! More details can be found in Schoetter et al. (2017), GMD.
!
! ###########################################################
!
! Initialisation of the fraction of compartments
!
DO JLIST=1,SIZE(DTT%NPAR_USETYPE)
   !
   IF (PRESENT(PFRACOMP)) THEN
      !
      IF (SIZE(PFRACOMP,2).EQ.1) THEN
         !
         PFRACOMP(JLIST,1) = 1.0
         !
      ELSEIF (SIZE(PFRACOMP,2).EQ.6) THEN
         !
         IF (DTT%NPAR_USETYPE(JLIST).EQ.BDD%NDESC_POS_USE_HAC) THEN
            !
            ! For main use = COLLECTIVE housing, a non-heated fraction,
            ! a COMMERCIAL and TERTIARY fraction and 3 residential
            ! compartments with 3 different heating design temperatures are considered.
            ! The non-heated fraction related to this specific building archetype
            ! It will be used to normalise the commercial and office fraction
            !
            PFRACOMP(JLIST,4) = BDD%XDESC_FNOHEAT_AVG(DTT%NPAR_USETYPE(JLIST)) + &
               BDD%XDESC_FNOHEAT_MOD(DTT%NPAR_USETYPE(JLIST))*(2*DTT%XPAR_CRE_APPT(JLIST)-1)
            !
            PFRACOMP(JLIST,5) = DTT%XPAR_FRACCOM(JLIST)*(1.0-PFRACOMP(JLIST,4))
            PFRACOMP(JLIST,6) = DTT%XPAR_FRACTER(JLIST)*(1.0-PFRACOMP(JLIST,4))
            !
            PFRACOMP(JLIST,4) = ( 1.0 - DTT%XPAR_FRACNHE(JLIST) - DTT%XPAR_FRACIND(JLIST) ) * ( &
                 BDD%XDESC_FNOHEAT_AVG(DTT%NPAR_USETYPE(JLIST)) + &
                 BDD%XDESC_FNOHEAT_MOD(DTT%NPAR_USETYPE(JLIST)) * (2*DTT%XPAR_CRE_APPT(JLIST)-1) ) + &
                 DTT%XPAR_FRACNHE(JLIST) + DTT%XPAR_FRACIND(JLIST)
            !
            PFRACOMP(JLIST,1) = (1.0 - PFRACOMP(JLIST,4) - PFRACOMP(JLIST,5) - PFRACOMP(JLIST,6)) * &
                 (DTT%XPAR_CRE_APPT(JLIST)  * BDD%XDESC_FLDT(BDD%NDESC_POS_HAC_FORTCRE) + &
             (1.0-DTT%XPAR_CRE_APPT(JLIST)) * BDD%XDESC_FLDT(BDD%NDESC_POS_HAC_FAIBCRE))
            !
            PFRACOMP(JLIST,2) = (1.0 - PFRACOMP(JLIST,4) - PFRACOMP(JLIST,5) - PFRACOMP(JLIST,6)) * &
                 (DTT%XPAR_CRE_APPT(JLIST)  * BDD%XDESC_FHDT(BDD%NDESC_POS_HAC_FORTCRE) + &
             (1.0-DTT%XPAR_CRE_APPT(JLIST)) * BDD%XDESC_FHDT(BDD%NDESC_POS_HAC_FAIBCRE))
            !
            PFRACOMP(JLIST,3) = (1.0 - PFRACOMP(JLIST,4) - PFRACOMP(JLIST,5) - PFRACOMP(JLIST,6)) * &
                 (DTT%XPAR_CRE_APPT(JLIST)  * BDD%XDESC_FIDT(BDD%NDESC_POS_HAC_FORTCRE) + &
             (1.0-DTT%XPAR_CRE_APPT(JLIST)) * BDD%XDESC_FIDT(BDD%NDESC_POS_HAC_FAIBCRE))
            !
         ELSEIF (DTT%NPAR_USETYPE(JLIST).EQ.BDD%NDESC_POS_USE_HAI) THEN
            !
            ! For main use = INDIVIDUAL housing, a non-heated fraction,
            ! a COMMERCIAL and TERTIARY fraction and 3 residential
            ! compartments with 3 different heating design temperatures are considered.
            ! The non-heated fraction related to this specific building archetype
            ! It will be used to normalise the commercial and office fraction
            !
            PFRACOMP(JLIST,4) = BDD%XDESC_FNOHEAT_AVG(DTT%NPAR_USETYPE(JLIST)) + &
               BDD%XDESC_FNOHEAT_MOD(DTT%NPAR_USETYPE(JLIST)) * (2*DTT%XPAR_CRE_MAIS(JLIST)-1)
            !
            PFRACOMP(JLIST,5) = DTT%XPAR_FRACCOM(JLIST)*(1.0-PFRACOMP(JLIST,4))
            PFRACOMP(JLIST,6) = DTT%XPAR_FRACTER(JLIST)*(1.0-PFRACOMP(JLIST,4))
            !
            PFRACOMP(JLIST,4) = ( 1.0 - DTT%XPAR_FRACNHE(JLIST) - DTT%XPAR_FRACIND(JLIST) ) * ( &
               BDD%XDESC_FNOHEAT_AVG(DTT%NPAR_USETYPE(JLIST)) + &
               BDD%XDESC_FNOHEAT_MOD(DTT%NPAR_USETYPE(JLIST)) * (2*DTT%XPAR_CRE_MAIS(JLIST)-1) ) + &
               DTT%XPAR_FRACNHE(JLIST) + DTT%XPAR_FRACIND(JLIST)              
            !
            PFRACOMP(JLIST,1) = (1.0 - PFRACOMP(JLIST,4) - PFRACOMP(JLIST,5) - PFRACOMP(JLIST,6)) * &
                 (DTT%XPAR_CRE_MAIS(JLIST)  * BDD%XDESC_FLDT(BDD%NDESC_POS_HAI_FORTCRE) + &
             (1.0-DTT%XPAR_CRE_MAIS(JLIST)) * BDD%XDESC_FLDT(BDD%NDESC_POS_HAI_FAIBCRE))
            !
            PFRACOMP(JLIST,2) = (1.0 - PFRACOMP(JLIST,4) - PFRACOMP(JLIST,5) - PFRACOMP(JLIST,6)) * &
                 (DTT%XPAR_CRE_MAIS(JLIST)  * BDD%XDESC_FHDT(BDD%NDESC_POS_HAI_FORTCRE) + &
             (1.0-DTT%XPAR_CRE_MAIS(JLIST)) * BDD%XDESC_FHDT(BDD%NDESC_POS_HAI_FAIBCRE))
            !
            PFRACOMP(JLIST,3) = (1.0 - PFRACOMP(JLIST,4) - PFRACOMP(JLIST,5) - PFRACOMP(JLIST,6)) * &
                 (DTT%XPAR_CRE_MAIS(JLIST)  * BDD%XDESC_FIDT(BDD%NDESC_POS_HAI_FORTCRE) + &
             (1.0-DTT%XPAR_CRE_MAIS(JLIST)) * BDD%XDESC_FIDT(BDD%NDESC_POS_HAI_FAIBCRE))
            !
         ELSEIF ( (DTT%NPAR_USETYPE(JLIST).EQ.BDD%NDESC_POS_USE_COM).OR.(DTT%NPAR_USETYPE(JLIST).EQ.BDD%NDESC_POS_USE_SAN) .OR. &
                  (DTT%NPAR_USETYPE(JLIST).EQ.BDD%NDESC_POS_USE_ENS).OR.(DTT%NPAR_USETYPE(JLIST).EQ.BDD%NDESC_POS_USE_TER) ) THEN
            !
            ! For the main use = TERTIARY, COMMERCIAL, HEALTH and EDUCATION
            ! a non-heated fraction and 3 compartments with different design temperatures
            ! of heating and climatisation are considered
            !
            PFRACOMP(JLIST,4) = (1.0 - DTT%XPAR_FRACNHE(JLIST) - DTT%XPAR_FRACIND(JLIST) ) * &
                                BDD%XDESC_FNOHEAT_AVG(DTT%NPAR_USETYPE(JLIST)) + &
                                DTT%XPAR_FRACNHE(JLIST) + DTT%XPAR_FRACIND(JLIST) 
            !
            PFRACOMP(JLIST,5) = 0.0
            PFRACOMP(JLIST,6) = 0.0
            PFRACOMP(JLIST,1) = (1.0 - PFRACOMP(JLIST,4) - PFRACOMP(JLIST,5) - PFRACOMP(JLIST,6)) * 0.33
            PFRACOMP(JLIST,2) = (1.0 - PFRACOMP(JLIST,4) - PFRACOMP(JLIST,5) - PFRACOMP(JLIST,6)) * 0.34
            PFRACOMP(JLIST,3) = (1.0 - PFRACOMP(JLIST,4) - PFRACOMP(JLIST,5) - PFRACOMP(JLIST,6)) * 0.33
            !
         ELSE
            !
            ! Only a non-heated fraction is considered
            !
            PFRACOMP(JLIST,4) = (1.0-DTT%XPAR_FRACNHE(JLIST) - DTT%XPAR_FRACIND(JLIST))* &
                                BDD%XDESC_FNOHEAT_AVG(DTT%NPAR_USETYPE(JLIST)) + &
                                DTT%XPAR_FRACNHE(JLIST) + DTT%XPAR_FRACIND(JLIST)
            PFRACOMP(JLIST,5) = 0.0
            PFRACOMP(JLIST,6) = 0.0
            PFRACOMP(JLIST,1) = 1.0 - PFRACOMP(JLIST,4) - PFRACOMP(JLIST,5) - PFRACOMP(JLIST,6)
            PFRACOMP(JLIST,2) = 0.0
            PFRACOMP(JLIST,3) = 0.0
            !
         ENDIF
         !
      ELSE
         CALL ABOR1_SFX("No rule implemented for this number of compartments")
      ENDIF
      !
      ! Check whether the individual compartment fractions by more than 1.0E-6 wrong
      !
      DO II=1,SIZE(PFRACOMP,2)
         IF ((PFRACOMP(JLIST,II).LT.-1.0E-6).OR.(PFRACOMP(JLIST,II).GT.(1.0+1.0E-6))) THEN
            CALL ABOR1_SFX("INI_DATA_PARAM_TEB: Wrong fraction of compartments")
         ENDIF
      ENDDO
      !
      ! Very small deviations from 0.0 or 1.0 are corrected
      !
      WHERE (PFRACOMP(JLIST,:).LT.0.0)
         PFRACOMP(JLIST,:) = 0.0
      ELSEWHERE (PFRACOMP(JLIST,:).GT.1.0)
         PFRACOMP(JLIST,:) = 1.0
      ENDWHERE
      !
      ! Check whether the sum is far from 1.0
      !
      IF (ABS(SUM(PFRACOMP(JLIST,:))-1.0).GT.1.0E-6) THEN
         CALL ABOR1_SFX("INI_DATA_PARAM_TEB : Wrong initial sum of compartment fractions")
      ENDIF
      !
      ! Renormalise the sum
      !
      PFRACOMP(JLIST,:) = PFRACOMP(JLIST,:)/SUM(PFRACOMP(JLIST,:))
      !
      ! Check whether the renormalised sum is exactly 1.0
      !
      IF (ABS(SUM(PFRACOMP(JLIST,:))-1.0).GT.XSURF_EPSILON) THEN
         CALL ABOR1_SFX("INI_DATA_PARAM_TEB : Wrong final sum of compartment fractions")
      ENDIF
      !
   ENDIF
   !
ENDDO
!
! ###########################################################
!
! Initialisation of heating-related variables
!
DO JLIST=1,SIZE(DTT%NPAR_USETYPE)
   !
   IF (PRESENT(PTHEAT_OCCD)) THEN
      !
      IF (SIZE(PTHEAT_OCCD,2).EQ.1) THEN
         PTHEAT_OCCD(JLIST,1) = BDD%XDESC_THEAT_OCCD_AVG(DTT%NPAR_USETYPE(JLIST))
      ELSEIF (SIZE(PTHEAT_OCCD,2).EQ.6) THEN
         PTHEAT_OCCD(JLIST,1) = BDD%XDESC_THEAT_OCCD_AVG(DTT%NPAR_USETYPE(JLIST))- &
             BDD%XDESC_THEAT_OCCD_MOD(DTT%NPAR_USETYPE(JLIST))
         PTHEAT_OCCD(JLIST,2) = BDD%XDESC_THEAT_OCCD_AVG(DTT%NPAR_USETYPE(JLIST))+ &
              BDD%XDESC_THEAT_OCCD_MOD(DTT%NPAR_USETYPE(JLIST))
         PTHEAT_OCCD(JLIST,3) = BDD%XDESC_THEAT_OCCD_AVG(DTT%NPAR_USETYPE(JLIST))
         PTHEAT_OCCD(JLIST,4) = 200.0
         PTHEAT_OCCD(JLIST,5) = BDD%XDESC_THEAT_OCCD_AVG(BDD%NDESC_POS_USE_COM)
         PTHEAT_OCCD(JLIST,6) = BDD%XDESC_THEAT_OCCD_AVG(BDD%NDESC_POS_USE_TER)
      ELSE
         CALL ABOR1_SFX("No rule for this number of compartments")
      ENDIF
      !
   ENDIF
   !
   IF (PRESENT(PTHEAT_OCCN)) THEN
      !
      IF (SIZE(PTHEAT_OCCN,2).EQ.1) THEN
         PTHEAT_OCCN(JLIST,1) = BDD%XDESC_THEAT_OCCN_AVG(DTT%NPAR_USETYPE(JLIST))
      ELSEIF (SIZE(PTHEAT_OCCN,2).EQ.6) THEN
         PTHEAT_OCCN(JLIST,1) = BDD%XDESC_THEAT_OCCN_AVG(DTT%NPAR_USETYPE(JLIST))- &
              BDD%XDESC_THEAT_OCCN_MOD(DTT%NPAR_USETYPE(JLIST))
         PTHEAT_OCCN(JLIST,2) = BDD%XDESC_THEAT_OCCN_AVG(DTT%NPAR_USETYPE(JLIST))+ &
              BDD%XDESC_THEAT_OCCN_MOD(DTT%NPAR_USETYPE(JLIST))
         PTHEAT_OCCN(JLIST,3) = BDD%XDESC_THEAT_OCCN_AVG(DTT%NPAR_USETYPE(JLIST))
         PTHEAT_OCCN(JLIST,4) = 200.0
         PTHEAT_OCCN(JLIST,5) = BDD%XDESC_THEAT_OCCN_AVG(BDD%NDESC_POS_USE_COM)
         PTHEAT_OCCN(JLIST,6) = BDD%XDESC_THEAT_OCCN_AVG(BDD%NDESC_POS_USE_TER)
      ELSE
         CALL ABOR1_SFX("No rule for this number of compartments")
      ENDIF
      !
   ENDIF
   !
   IF (PRESENT(PTHEAT_VCDD)) THEN
      !
      IF (SIZE(PTHEAT_VCDD,2).EQ.1) THEN
         PTHEAT_VCDD(JLIST,1) = BDD%XDESC_THEAT_VCDD_AVG(DTT%NPAR_USETYPE(JLIST))
      ELSEIF (SIZE(PTHEAT_VCDD,2).EQ.6) THEN
         PTHEAT_VCDD(JLIST,1) = BDD%XDESC_THEAT_VCDD_AVG(DTT%NPAR_USETYPE(JLIST))- &
              BDD%XDESC_THEAT_VCDD_MOD(DTT%NPAR_USETYPE(JLIST))
         PTHEAT_VCDD(JLIST,2) = BDD%XDESC_THEAT_VCDD_AVG(DTT%NPAR_USETYPE(JLIST))+ &
              BDD%XDESC_THEAT_VCDD_MOD(DTT%NPAR_USETYPE(JLIST))
         PTHEAT_VCDD(JLIST,3) = BDD%XDESC_THEAT_VCDD_AVG(DTT%NPAR_USETYPE(JLIST))
         PTHEAT_VCDD(JLIST,4) = 200.0
         PTHEAT_VCDD(JLIST,5) = BDD%XDESC_THEAT_VCDD_AVG(BDD%NDESC_POS_USE_COM)
         PTHEAT_VCDD(JLIST,6) = BDD%XDESC_THEAT_VCDD_AVG(BDD%NDESC_POS_USE_TER)
      ELSE
         CALL ABOR1_SFX("No rule for this number of compartments")
      ENDIF
      !
   ENDIF
   !
   IF (PRESENT(PTHEAT_VCDN)) THEN
      !
      IF (SIZE(PTHEAT_VCDN,2).EQ.1) THEN
         PTHEAT_VCDN(JLIST,1) = BDD%XDESC_THEAT_VCDN_AVG(DTT%NPAR_USETYPE(JLIST))
      ELSEIF (SIZE(PTHEAT_VCDN,2).EQ.6) THEN
         PTHEAT_VCDN(JLIST,1) = BDD%XDESC_THEAT_VCDN_AVG(DTT%NPAR_USETYPE(JLIST))- &
              BDD%XDESC_THEAT_VCDN_MOD(DTT%NPAR_USETYPE(JLIST))
         PTHEAT_VCDN(JLIST,2) = BDD%XDESC_THEAT_VCDN_AVG(DTT%NPAR_USETYPE(JLIST))+ &
              BDD%XDESC_THEAT_VCDN_MOD(DTT%NPAR_USETYPE(JLIST))
         PTHEAT_VCDN(JLIST,3) = BDD%XDESC_THEAT_VCDN_AVG(DTT%NPAR_USETYPE(JLIST))
         PTHEAT_VCDN(JLIST,4) = 200.0
         PTHEAT_VCDN(JLIST,5) = BDD%XDESC_THEAT_VCDN_AVG(BDD%NDESC_POS_USE_COM)
         PTHEAT_VCDN(JLIST,6) = BDD%XDESC_THEAT_VCDN_AVG(BDD%NDESC_POS_USE_TER)
      ELSE
         CALL ABOR1_SFX("No rule for this number of compartments")
      ENDIF
      !
   ENDIF
   !
   IF (PRESENT(PTHEAT_VCLD)) THEN
      !
      IF (SIZE(PTHEAT_VCLD,2).EQ.1) THEN
         PTHEAT_VCLD(JLIST,1) = BDD%XDESC_THEAT_VCLD_AVG(DTT%NPAR_USETYPE(JLIST))
      ELSEIF (SIZE(PTHEAT_VCLD,2).EQ.6) THEN
         PTHEAT_VCLD(JLIST,1) = BDD%XDESC_THEAT_VCLD_AVG(DTT%NPAR_USETYPE(JLIST))- &
              BDD%XDESC_THEAT_VCLD_MOD(DTT%NPAR_USETYPE(JLIST))
         PTHEAT_VCLD(JLIST,2) = BDD%XDESC_THEAT_VCLD_AVG(DTT%NPAR_USETYPE(JLIST))+ &
              BDD%XDESC_THEAT_VCLD_MOD(DTT%NPAR_USETYPE(JLIST))
         PTHEAT_VCLD(JLIST,3) = BDD%XDESC_THEAT_VCLD_AVG(DTT%NPAR_USETYPE(JLIST))
         PTHEAT_VCLD(JLIST,4) = 200.0
         PTHEAT_VCLD(JLIST,5) = BDD%XDESC_THEAT_VCLD_AVG(BDD%NDESC_POS_USE_COM)
         PTHEAT_VCLD(JLIST,6) = BDD%XDESC_THEAT_VCLD_AVG(BDD%NDESC_POS_USE_TER)    
      ELSE
         CALL ABOR1_SFX("No rule for this number of compartments")
      ENDIF
      !
   ENDIF
   !
ENDDO
!
! Initialisation of cooling-related variables
!
DO JLIST=1,SIZE(DTT%NPAR_USETYPE)
   !
   IF (PRESENT(PTCOOL_OCCD)) THEN
      IF (SIZE(PTCOOL_OCCD,2).EQ.1) THEN
         PTCOOL_OCCD(JLIST,1) = BDD%XDESC_TCOOL_OCCD(DTT%NPAR_USETYPE(JLIST))
      ELSEIF (SIZE(PTCOOL_OCCD,2).EQ.6) THEN
         PTCOOL_OCCD(JLIST,1) = BDD%XDESC_TCOOL_OCCD(DTT%NPAR_USETYPE(JLIST))
         PTCOOL_OCCD(JLIST,2) = BDD%XDESC_TCOOL_OCCD(DTT%NPAR_USETYPE(JLIST))
         PTCOOL_OCCD(JLIST,3) = BDD%XDESC_TCOOL_OCCD(DTT%NPAR_USETYPE(JLIST))
         PTCOOL_OCCD(JLIST,4) = 400.0
         PTCOOL_OCCD(JLIST,5) = BDD%XDESC_TCOOL_OCCD(BDD%NDESC_POS_USE_COM)
         PTCOOL_OCCD(JLIST,6) = BDD%XDESC_TCOOL_OCCD(BDD%NDESC_POS_USE_TER)
      ELSE
         CALL ABOR1_SFX ("No rule for this number of compartments")
      ENDIF
   ENDIF
   !
   IF (PRESENT(PTCOOL_OCCN)) THEN
      IF (SIZE(PTCOOL_OCCN,2).EQ.1) THEN
         PTCOOL_OCCN(JLIST,1) = BDD%XDESC_TCOOL_OCCN(DTT%NPAR_USETYPE(JLIST))
      ELSEIF (SIZE(PTCOOL_OCCN,2).EQ.6) THEN
         PTCOOL_OCCN(JLIST,1) = BDD%XDESC_TCOOL_OCCN(DTT%NPAR_USETYPE(JLIST))
         PTCOOL_OCCN(JLIST,2) = BDD%XDESC_TCOOL_OCCN(DTT%NPAR_USETYPE(JLIST))
         PTCOOL_OCCN(JLIST,3) = BDD%XDESC_TCOOL_OCCN(DTT%NPAR_USETYPE(JLIST))
         PTCOOL_OCCN(JLIST,4) = 400.0
         PTCOOL_OCCN(JLIST,5) = BDD%XDESC_TCOOL_OCCN(BDD%NDESC_POS_USE_COM)
         PTCOOL_OCCN(JLIST,6) = BDD%XDESC_TCOOL_OCCN(BDD%NDESC_POS_USE_TER)
      ELSE
         CALL ABOR1_SFX ("No rule for this number of compartments")
      ENDIF
   ENDIF
   !
   IF (PRESENT(PTCOOL_VCDD)) THEN
      IF (SIZE(PTCOOL_VCDD,2).EQ.1) THEN
         PTCOOL_VCDD(JLIST,1) = BDD%XDESC_TCOOL_VCDD(DTT%NPAR_USETYPE(JLIST))
      ELSEIF (SIZE(PTCOOL_VCDD,2).EQ.6) THEN
         PTCOOL_VCDD(JLIST,1) = BDD%XDESC_TCOOL_VCDD(DTT%NPAR_USETYPE(JLIST))
         PTCOOL_VCDD(JLIST,2) = BDD%XDESC_TCOOL_VCDD(DTT%NPAR_USETYPE(JLIST))
         PTCOOL_VCDD(JLIST,3) = BDD%XDESC_TCOOL_VCDD(DTT%NPAR_USETYPE(JLIST))
         PTCOOL_VCDD(JLIST,4) = 400.0
         PTCOOL_VCDD(JLIST,5) = BDD%XDESC_TCOOL_VCDD(BDD%NDESC_POS_USE_COM)
         PTCOOL_VCDD(JLIST,6) = BDD%XDESC_TCOOL_VCDD(BDD%NDESC_POS_USE_TER)
      ELSE
         CALL ABOR1_SFX ("No rule for this number of compartments")
      ENDIF
   ENDIF
   !
   IF (PRESENT(PTCOOL_VCDN)) THEN
      IF (SIZE(PTCOOL_VCDN,2).EQ.1) THEN
         PTCOOL_VCDN(JLIST,1) = BDD%XDESC_TCOOL_VCDN(DTT%NPAR_USETYPE(JLIST))
      ELSEIF (SIZE(PTCOOL_VCDN,2).EQ.6) THEN
         PTCOOL_VCDN(JLIST,1) = BDD%XDESC_TCOOL_VCDN(DTT%NPAR_USETYPE(JLIST))
         PTCOOL_VCDN(JLIST,2) = BDD%XDESC_TCOOL_VCDN(DTT%NPAR_USETYPE(JLIST))
         PTCOOL_VCDN(JLIST,3) = BDD%XDESC_TCOOL_VCDN(DTT%NPAR_USETYPE(JLIST))
         PTCOOL_VCDN(JLIST,4) = 400.0
         PTCOOL_VCDN(JLIST,5) = BDD%XDESC_TCOOL_VCDN(BDD%NDESC_POS_USE_COM)
         PTCOOL_VCDN(JLIST,6) = BDD%XDESC_TCOOL_VCDN(BDD%NDESC_POS_USE_TER)
      ELSE
         CALL ABOR1_SFX ("No rule for this number of compartments")
      ENDIF
   ENDIF
   !
   IF (PRESENT(PTCOOL_VCLD)) THEN
      IF (SIZE(PTCOOL_VCLD,2).EQ.1) THEN
         PTCOOL_VCLD(JLIST,1) = BDD%XDESC_TCOOL_VCLD(DTT%NPAR_USETYPE(JLIST))
      ELSEIF (SIZE(PTCOOL_VCLD,2).EQ.6) THEN
         PTCOOL_VCLD(JLIST,1) = BDD%XDESC_TCOOL_VCLD(DTT%NPAR_USETYPE(JLIST))
         PTCOOL_VCLD(JLIST,2) = BDD%XDESC_TCOOL_VCLD(DTT%NPAR_USETYPE(JLIST))
         PTCOOL_VCLD(JLIST,3) = BDD%XDESC_TCOOL_VCLD(DTT%NPAR_USETYPE(JLIST))
         PTCOOL_VCLD(JLIST,4) = 400.0
         PTCOOL_VCLD(JLIST,5) = BDD%XDESC_TCOOL_VCLD(BDD%NDESC_POS_USE_COM)
         PTCOOL_VCLD(JLIST,6) = BDD%XDESC_TCOOL_VCLD(BDD%NDESC_POS_USE_TER)
      ELSE
         CALL ABOR1_SFX ("No rule for this number of compartments")
      ENDIF
   ENDIF
   !
   IF (PRESENT(PF_WATER_COND)) THEN
      PF_WATER_COND(JLIST)=BDD%XDESC_F_WATER_COND(DTT%NPAR_USETYPE(JLIST))
   ENDIF
   !
   IF (PRESENT(PF_WASTE_CAN)) THEN
      PF_WASTE_CAN(JLIST) = BDD%XDESC_F_WASTE_CAN(DTT%NPAR_USETYPE(JLIST))
   ENDIF
   !
   IF (PRESENT(PCOP_RAT)) THEN
      PCOP_RAT(JLIST)=BDD%XDESC_COP_RAT(DTT%NPAR_USETYPE(JLIST))
   ENDIF
   !
   IF (PRESENT(PHR_TARGET)) THEN
      PHR_TARGET(JLIST)=BDD%XDESC_HR_TARGET(DTT%NPAR_USETYPE(JLIST))
   ENDIF
   !
ENDDO
!
! Initialisation of internal heat release-related variables
! At the moment the same internal heat release is distributed
! over all the heating design temperature differenciated compartments
!
DO JLIST=1,SIZE(DTT%NPAR_USETYPE)
   !
   IF (PRESENT(PQIN)) THEN
      !
      IF (SIZE(PQIN,2).EQ.1) THEN
         PQIN(JLIST,1) = BDD%XDESC_QIN(DTT%NPAR_USETYPE(JLIST))
      ELSEIF (SIZE(PQIN,2).EQ.6) THEN
         !
         IF (DTT%NPAR_USETYPE(JLIST).EQ.BDD%NDESC_POS_USE_HAI) THEN
            PQIN(JLIST,1) = BDD%XDESC_QIN(DTT%NPAR_USETYPE(JLIST)) + &
              MAX(0.0,(2*DTT%XPAR_FOEQI_MAIS(JLIST)-1)*BDD%XDESC_QIN_ADDBEHAV(DTT%NPAR_USETYPE(JLIST))) - &
              MAX(0.0,(2*DTT%XPAR_FAEQI_MAIS(JLIST)-1)*BDD%XDESC_QIN_ADDBEHAV(DTT%NPAR_USETYPE(JLIST)))
         ELSE
            PQIN(JLIST,1) = BDD%XDESC_QIN(DTT%NPAR_USETYPE(JLIST)) + &
              MAX(0.0,(2*DTT%XPAR_FOEQI_APPT(JLIST)-1)*BDD%XDESC_QIN_ADDBEHAV(DTT%NPAR_USETYPE(JLIST))) - &
              MAX(0.0,(2*DTT%XPAR_FAEQI_APPT(JLIST)-1)*BDD%XDESC_QIN_ADDBEHAV(DTT%NPAR_USETYPE(JLIST)))             
         ENDIF
         !
         PQIN(JLIST,2) = PQIN(JLIST,1)
         PQIN(JLIST,3) = PQIN(JLIST,1)
         PQIN(JLIST,4) = PQIN(JLIST,1)
         PQIN(JLIST,5) = BDD%XDESC_QIN(BDD%NDESC_POS_USE_COM)
         PQIN(JLIST,6) = BDD%XDESC_QIN(BDD%NDESC_POS_USE_TER)
         !
      ELSE
         CALL ABOR1_SFX ("No rule for this number of compartments")
      ENDIF
      !
      DO II=1,SIZE(PQIN,2)
         IF (PQIN(JLIST,II).LT.-XSURF_EPSILON) THEN
            CALL ABOR1_SFX("INI_DATA_PARAM_TEB:Negative value for nominative internal heat release")
         ENDIF
      ENDDO
      !
   ENDIF
   !
   IF (PRESENT(PQIN_FRAD)) THEN
      PQIN_FRAD(JLIST)=BDD%XDESC_QIN_FRAD(DTT%NPAR_USETYPE(JLIST))
   ENDIF
   !
   IF (PRESENT(PQIN_FLAT)) THEN
      PQIN_FLAT(JLIST)=BDD%XDESC_QIN_FLAT(DTT%NPAR_USETYPE(JLIST))
   ENDIF
   !
   IF (PRESENT(PMODQIN_VCD)) THEN
      IF (SIZE(PMODQIN_VCD,2).EQ.1) THEN
        PMODQIN_VCD(JLIST,1) = BDD%XDESC_MODQIN_VCD(DTT%NPAR_USETYPE(JLIST))
      ELSEIF (SIZE(PMODQIN_VCD,2).EQ.6) THEN
         PMODQIN_VCD(JLIST,1) = BDD%XDESC_MODQIN_VCD(DTT%NPAR_USETYPE(JLIST))
         PMODQIN_VCD(JLIST,2) = BDD%XDESC_MODQIN_VCD(DTT%NPAR_USETYPE(JLIST))
         PMODQIN_VCD(JLIST,3) = BDD%XDESC_MODQIN_VCD(DTT%NPAR_USETYPE(JLIST))
         PMODQIN_VCD(JLIST,4) = BDD%XDESC_MODQIN_VCD(DTT%NPAR_USETYPE(JLIST))
         PMODQIN_VCD(JLIST,5) = BDD%XDESC_MODQIN_VCD(BDD%NDESC_POS_USE_COM)
         PMODQIN_VCD(JLIST,6) = BDD%XDESC_MODQIN_VCD(BDD%NDESC_POS_USE_TER)
      ELSE
         CALL ABOR1_SFX ("No rule for this number of compartments")
      ENDIF
   ENDIF
   !
   IF (PRESENT(PMODQIN_VLD)) THEN
      IF (SIZE(PMODQIN_VLD,2).EQ.1) THEN
         PMODQIN_VLD(JLIST,1) = BDD%XDESC_MODQIN_VLD(DTT%NPAR_USETYPE(JLIST))
      ELSEIF (SIZE(PMODQIN_VLD,2).EQ.6) THEN
         PMODQIN_VLD(JLIST,1) = BDD%XDESC_MODQIN_VLD(DTT%NPAR_USETYPE(JLIST))
         PMODQIN_VLD(JLIST,2) = BDD%XDESC_MODQIN_VLD(DTT%NPAR_USETYPE(JLIST))
         PMODQIN_VLD(JLIST,3) = BDD%XDESC_MODQIN_VLD(DTT%NPAR_USETYPE(JLIST))
         PMODQIN_VLD(JLIST,4) = BDD%XDESC_MODQIN_VLD(DTT%NPAR_USETYPE(JLIST))
         PMODQIN_VLD(JLIST,5) = BDD%XDESC_MODQIN_VLD(BDD%NDESC_POS_USE_COM)
         PMODQIN_VLD(JLIST,6) = BDD%XDESC_MODQIN_VLD(BDD%NDESC_POS_USE_TER)
      ELSE
         CALL ABOR1_SFX ("No rule for this number of compartments")
      ENDIF
   ENDIF
   !
   IF (PRESENT(PMODQIN_NIG)) THEN
      IF (SIZE(PMODQIN_NIG,2).EQ.1) THEN
        PMODQIN_NIG(JLIST,1) = BDD%XDESC_MODQIN_NIG(DTT%NPAR_USETYPE(JLIST))
      ELSEIF (SIZE(PMODQIN_NIG,2).EQ.6) THEN
         PMODQIN_NIG(JLIST,1) = BDD%XDESC_MODQIN_NIG(DTT%NPAR_USETYPE(JLIST))
         PMODQIN_NIG(JLIST,2) = BDD%XDESC_MODQIN_NIG(DTT%NPAR_USETYPE(JLIST))
         PMODQIN_NIG(JLIST,3) = BDD%XDESC_MODQIN_NIG(DTT%NPAR_USETYPE(JLIST))
         PMODQIN_NIG(JLIST,4) = BDD%XDESC_MODQIN_NIG(DTT%NPAR_USETYPE(JLIST))
         PMODQIN_NIG(JLIST,5) = BDD%XDESC_MODQIN_NIG(BDD%NDESC_POS_USE_COM)
         PMODQIN_NIG(JLIST,6) = BDD%XDESC_MODQIN_NIG(BDD%NDESC_POS_USE_TER)
      ELSE
         CALL ABOR1_SFX ("No rule for this number of compartments")
      ENDIF
   ENDIF
   !
   IF (PRESENT(PHOTWAT)) THEN
      IF (SIZE(PHOTWAT,2).EQ.1) THEN
         PHOTWAT(JLIST,1) = BDD%XDESC_HOTWAT(DTT%NPAR_USETYPE(JLIST))
      ELSEIF (SIZE(PHOTWAT,2).EQ.6) THEN       
         PHOTWAT(JLIST,1) = BDD%XDESC_HOTWAT(DTT%NPAR_USETYPE(JLIST))
         PHOTWAT(JLIST,2) = BDD%XDESC_HOTWAT(DTT%NPAR_USETYPE(JLIST))
         PHOTWAT(JLIST,3) = BDD%XDESC_HOTWAT(DTT%NPAR_USETYPE(JLIST))
         PHOTWAT(JLIST,4) = 0.0
         PHOTWAT(JLIST,5) = 0.0
         PHOTWAT(JLIST,6) = 0.0           
      ELSE
         CALL ABOR1_SFX ("No rule for this number of compartments")
      ENDIF
   ENDIF
   !
ENDDO
!
! Initialisation of shading and ventilation-related variables
!
DO JLIST=1,SIZE(DTT%NPAR_USETYPE)
   !
   IF (PRESENT(PNATVENT)) THEN
      IF (SIZE(PNATVENT,2).EQ.1) THEN
         PNATVENT(JLIST,1) = BDD%XDESC_NATVENT(DTT%NPAR_USETYPE(JLIST))
      ELSEIF (SIZE(PNATVENT,2).EQ.6) THEN
         PNATVENT(JLIST,1) = BDD%XDESC_NATVENT(DTT%NPAR_USETYPE(JLIST))
         PNATVENT(JLIST,2) = BDD%XDESC_NATVENT(DTT%NPAR_USETYPE(JLIST))
         PNATVENT(JLIST,3) = BDD%XDESC_NATVENT(DTT%NPAR_USETYPE(JLIST))
         PNATVENT(JLIST,4) = 0.0
         PNATVENT(JLIST,5) = BDD%XDESC_NATVENT(BDD%NDESC_POS_USE_COM)
         PNATVENT(JLIST,6) = BDD%XDESC_NATVENT(BDD%NDESC_POS_USE_TER)
      ELSE
         CALL ABOR1_SFX ("No rule for this number of compartments")           
      ENDIF
   ENDIF
   !
   IF (PRESENT(PFVSUM)) THEN
      IF (SIZE(PFVSUM,2).EQ.1) THEN        
         PFVSUM(JLIST,1) = BDD%XDESC_FVSUM(DTT%NPAR_USETYPE(JLIST))
      ELSEIF (SIZE(PFVSUM,2).EQ.6) THEN
         PFVSUM(JLIST,1) = BDD%XDESC_FVSUM(DTT%NPAR_USETYPE(JLIST))
         PFVSUM(JLIST,2) = BDD%XDESC_FVSUM(DTT%NPAR_USETYPE(JLIST))
         PFVSUM(JLIST,3) = BDD%XDESC_FVSUM(DTT%NPAR_USETYPE(JLIST))
         PFVSUM(JLIST,4) = 0.0
         PFVSUM(JLIST,5) = BDD%XDESC_FVSUM(BDD%NDESC_POS_USE_COM)
         PFVSUM(JLIST,6) = BDD%XDESC_FVSUM(BDD%NDESC_POS_USE_TER)
      ELSE
         CALL ABOR1_SFX ("No rule for this number of compartments") 
      ENDIF
   ENDIF
   !
   IF (PRESENT(PFVNIG)) THEN
      IF (SIZE(PFVNIG,2).EQ.1) THEN        
         PFVNIG(JLIST,1) = BDD%XDESC_FVNIG(DTT%NPAR_USETYPE(JLIST))
      ELSEIF (SIZE(PFVNIG,2).EQ.6) THEN
         PFVNIG(JLIST,1) = BDD%XDESC_FVNIG(DTT%NPAR_USETYPE(JLIST))
         PFVNIG(JLIST,2) = BDD%XDESC_FVNIG(DTT%NPAR_USETYPE(JLIST))
         PFVNIG(JLIST,3) = BDD%XDESC_FVNIG(DTT%NPAR_USETYPE(JLIST))
         PFVNIG(JLIST,4) = 0.0
         PFVNIG(JLIST,5) = BDD%XDESC_FVNIG(BDD%NDESC_POS_USE_COM)
         PFVNIG(JLIST,6) = BDD%XDESC_FVNIG(BDD%NDESC_POS_USE_TER)
      ELSE
         CALL ABOR1_SFX ("No rule for this number of compartments") 
      ENDIF
   ENDIF
   !
   IF (PRESENT(PTDESV)) THEN
      PTDESV(JLIST)=BDD%XDESC_TDESV(DTT%NPAR_USETYPE(JLIST))
   ENDIF
   !
   IF (PRESENT(PFVVAC)) THEN
      IF (SIZE(PFVVAC,2).EQ.1) THEN    
         PFVVAC(JLIST,1) = BDD%XDESC_FVVAC(DTT%NPAR_USETYPE(JLIST))
      ELSEIF (SIZE(PFVVAC,2).EQ.6) THEN
         PFVVAC(JLIST,1) = BDD%XDESC_FVVAC(DTT%NPAR_USETYPE(JLIST))
         PFVVAC(JLIST,2) = BDD%XDESC_FVVAC(DTT%NPAR_USETYPE(JLIST))
         PFVVAC(JLIST,3) = BDD%XDESC_FVVAC(DTT%NPAR_USETYPE(JLIST))
         PFVVAC(JLIST,4) = 0.0
         PFVVAC(JLIST,5) = BDD%XDESC_FVVAC(BDD%NDESC_POS_USE_COM)
         PFVVAC(JLIST,6) = BDD%XDESC_FVVAC(BDD%NDESC_POS_USE_TER)
      ELSE
         CALL ABOR1_SFX ("No rule for this number of compartments")            
      ENDIF
   ENDIF
   !
   IF (PRESENT(PFOPEN)) THEN
      PFOPEN(JLIST)=BDD%XDESC_FOPEN(DTT%NPAR_USETYPE(JLIST))
   ENDIF
   !
   IF (PRESENT(PFSSUM)) THEN
      IF (SIZE(PFSSUM,2).EQ.1) THEN   
         PFSSUM(JLIST,1) = BDD%XDESC_FSSUM(DTT%NPAR_USETYPE(JLIST))
      ELSEIF (SIZE(PFSSUM,2).EQ.6) THEN
         PFSSUM(JLIST,1) = BDD%XDESC_FSSUM(DTT%NPAR_USETYPE(JLIST))
         PFSSUM(JLIST,2) = BDD%XDESC_FSSUM(DTT%NPAR_USETYPE(JLIST))
         PFSSUM(JLIST,3) = BDD%XDESC_FSSUM(DTT%NPAR_USETYPE(JLIST))
         PFSSUM(JLIST,4) = 0.0
         PFSSUM(JLIST,5) = BDD%XDESC_FSSUM(BDD%NDESC_POS_USE_COM)      
         PFSSUM(JLIST,6) = BDD%XDESC_FSSUM(BDD%NDESC_POS_USE_TER)
      ELSE
         CALL ABOR1_SFX ("No rule for this number of compartments")               
      ENDIF
   ENDIF
   !
   IF (PRESENT(PFSNIG)) THEN
      IF (SIZE(PFSNIG,2).EQ.1) THEN    
         PFSNIG(JLIST,1) = BDD%XDESC_FSNIG(DTT%NPAR_USETYPE(JLIST))
      ELSEIF (SIZE(PFSNIG,2).EQ.6) THEN
         PFSNIG(JLIST,1) = BDD%XDESC_FSNIG(DTT%NPAR_USETYPE(JLIST))
         PFSNIG(JLIST,2) = BDD%XDESC_FSNIG(DTT%NPAR_USETYPE(JLIST))
         PFSNIG(JLIST,3) = BDD%XDESC_FSNIG(DTT%NPAR_USETYPE(JLIST))
         PFSNIG(JLIST,4) = 0.0
         PFSNIG(JLIST,5) = BDD%XDESC_FSNIG(BDD%NDESC_POS_USE_COM)
         PFSNIG(JLIST,6) = BDD%XDESC_FSNIG(BDD%NDESC_POS_USE_TER)
      ELSE
         CALL ABOR1_SFX ("No rule for this number of compartments")              
      ENDIF
   ENDIF
   !
   IF (PRESENT(PFSVAC)) THEN
      IF (SIZE(PFSVAC,2).EQ.1) THEN
         PFSVAC(JLIST,1) = BDD%XDESC_FSVAC(DTT%NPAR_USETYPE(JLIST))
      ELSEIF (SIZE(PFSVAC,2).EQ.6) THEN
         PFSVAC(JLIST,1) = BDD%XDESC_FSVAC(DTT%NPAR_USETYPE(JLIST))
         PFSVAC(JLIST,2) = BDD%XDESC_FSVAC(DTT%NPAR_USETYPE(JLIST))
         PFSVAC(JLIST,3) = BDD%XDESC_FSVAC(DTT%NPAR_USETYPE(JLIST))
         PFSVAC(JLIST,4) = 0.0
         PFSVAC(JLIST,5) = BDD%XDESC_FSVAC(BDD%NDESC_POS_USE_COM)       
         PFSVAC(JLIST,6) = BDD%XDESC_FSVAC(BDD%NDESC_POS_USE_TER)
      ELSE
         CALL ABOR1_SFX ("No rule for this number of compartments")              
      ENDIF
   ENDIF
   !
   IF (PRESENT(PWIN_SW_MAX)) THEN
      PWIN_SW_MAX(JLIST)=BDD%XDESC_WIN_SW_MAX(DTT%NPAR_USETYPE(JLIST))
   ENDIF
   !
ENDDO
!
! Initialisation of building occupation-related variables
!
DO JLIST=1,SIZE(DTT%NPAR_USETYPE)
   !
   IF (PRESENT(PPROBOCC)) THEN
      !
      IF (SIZE(PPROBOCC,3).EQ.1) THEN
         PPROBOCC(JLIST,:,1) = BDD%XDESC_PROBOCC(DTT%NPAR_USETYPE(JLIST),:)
      ELSEIF (SIZE(PPROBOCC,3).EQ.6) THEN
         PPROBOCC(JLIST,:,1) = BDD%XDESC_PROBOCC(DTT%NPAR_USETYPE(JLIST),:)
         PPROBOCC(JLIST,:,2) = BDD%XDESC_PROBOCC(DTT%NPAR_USETYPE(JLIST),:)
         PPROBOCC(JLIST,:,3) = BDD%XDESC_PROBOCC(DTT%NPAR_USETYPE(JLIST),:)
         PPROBOCC(JLIST,:,4) = 0.0
         PPROBOCC(JLIST,:,5) = BDD%XDESC_PROBOCC(BDD%NDESC_POS_USE_COM,:)
         PPROBOCC(JLIST,:,6) = BDD%XDESC_PROBOCC(BDD%NDESC_POS_USE_TER,:)
      ELSE
         CALL ABOR1_SFX ("No rule for this number of compartments")    
      ENDIF
      !
      IF (MAXVAL(PPROBOCC(JLIST,:,:)).GT.1.0) CALL ABOR1_SFX ("Wrong building occuppation probability")
      IF (MINVAL(PPROBOCC(JLIST,:,:)).LT.0.0) CALL ABOR1_SFX ("Wrong building occuppation probability")
      !
   ENDIF 
   !
   ! Initialisation of schedules
   !
   IF (PRESENT(PDAYWBEG_SCHED)) THEN
      !
      IF (SIZE(PDAYWBEG_SCHED,3).EQ.1) THEN
         PDAYWBEG_SCHED(JLIST,:,1) = BDD%XDESC_DAYWBEG_SCHED(DTT%NPAR_USETYPE(JLIST),:)
      ELSEIF (SIZE(PDAYWBEG_SCHED,3).EQ.6) THEN
         PDAYWBEG_SCHED(JLIST,:,1) = BDD%XDESC_DAYWBEG_SCHED(DTT%NPAR_USETYPE(JLIST),:)
         PDAYWBEG_SCHED(JLIST,:,2) = BDD%XDESC_DAYWBEG_SCHED(DTT%NPAR_USETYPE(JLIST),:)
         PDAYWBEG_SCHED(JLIST,:,3) = BDD%XDESC_DAYWBEG_SCHED(DTT%NPAR_USETYPE(JLIST),:)
         PDAYWBEG_SCHED(JLIST,:,4) = BDD%XDESC_DAYWBEG_SCHED(DTT%NPAR_USETYPE(JLIST),:)
         PDAYWBEG_SCHED(JLIST,:,5) = BDD%XDESC_DAYWBEG_SCHED(BDD%NDESC_POS_USE_COM,:)
         PDAYWBEG_SCHED(JLIST,:,6) = BDD%XDESC_DAYWBEG_SCHED(BDD%NDESC_POS_USE_TER,:)
      ELSE
         CALL ABOR1_SFX ("No rule for this number of compartments")              
      ENDIF
      !
   ENDIF
   !
   IF (PRESENT(PHOURBEG_SCHED)) THEN
      !
      IF (SIZE(PHOURBEG_SCHED,3).EQ.1) THEN   
         PHOURBEG_SCHED(JLIST,:,1) = BDD%XDESC_HOURBEG_SCHED(DTT%NPAR_USETYPE(JLIST),:)
      ELSEIF (SIZE(PHOURBEG_SCHED,3).EQ.6) THEN
         PHOURBEG_SCHED(JLIST,:,1) = BDD%XDESC_HOURBEG_SCHED(DTT%NPAR_USETYPE(JLIST),:)
         PHOURBEG_SCHED(JLIST,:,2) = BDD%XDESC_HOURBEG_SCHED(DTT%NPAR_USETYPE(JLIST),:)
         PHOURBEG_SCHED(JLIST,:,3) = BDD%XDESC_HOURBEG_SCHED(DTT%NPAR_USETYPE(JLIST),:)
         PHOURBEG_SCHED(JLIST,:,4) = BDD%XDESC_HOURBEG_SCHED(DTT%NPAR_USETYPE(JLIST),:)
         PHOURBEG_SCHED(JLIST,:,5) = BDD%XDESC_HOURBEG_SCHED(BDD%NDESC_POS_USE_COM,:)
         PHOURBEG_SCHED(JLIST,:,6) = BDD%XDESC_HOURBEG_SCHED(BDD%NDESC_POS_USE_TER,:)
      ELSE
         CALL ABOR1_SFX ("No rule for this number of compartments")
      ENDIF
      !
   ENDIF
   !
   ! Initialisation of holidays
   !
   IF (PRESENT(PBEG_HOLIDAY)) THEN
      !
      IF (SIZE(PBEG_HOLIDAY,3).EQ.1) THEN
         PBEG_HOLIDAY(JLIST,:,1) = BDD%XDESC_BEG_HOLIDAY(DTT%NPAR_USETYPE(JLIST),:)       
      ELSEIF (SIZE(PBEG_HOLIDAY,3).EQ.6) THEN
         PBEG_HOLIDAY(JLIST,:,1) = BDD%XDESC_BEG_HOLIDAY(DTT%NPAR_USETYPE(JLIST),:)
         PBEG_HOLIDAY(JLIST,:,2) = BDD%XDESC_BEG_HOLIDAY(DTT%NPAR_USETYPE(JLIST),:)
         PBEG_HOLIDAY(JLIST,:,3) = BDD%XDESC_BEG_HOLIDAY(DTT%NPAR_USETYPE(JLIST),:)
         PBEG_HOLIDAY(JLIST,:,4) = BDD%XDESC_BEG_HOLIDAY(DTT%NPAR_USETYPE(JLIST),:)
         PBEG_HOLIDAY(JLIST,:,5) = BDD%XDESC_BEG_HOLIDAY(BDD%NDESC_POS_USE_COM,:)
         PBEG_HOLIDAY(JLIST,:,6) = BDD%XDESC_BEG_HOLIDAY(BDD%NDESC_POS_USE_TER,:)
      ELSE
         CALL ABOR1_SFX ("No rule for this number of compartments")
      ENDIF
      !
   ENDIF
   !
   IF (PRESENT(PEND_HOLIDAY)) THEN
      !
      IF (SIZE(PEND_HOLIDAY,3).EQ.1) THEN
         PEND_HOLIDAY(JLIST,:,1) = BDD%XDESC_END_HOLIDAY(DTT%NPAR_USETYPE(JLIST),:)
      ELSEIF (SIZE(PEND_HOLIDAY,3).EQ.6) THEN
         PEND_HOLIDAY(JLIST,:,1) = BDD%XDESC_END_HOLIDAY(DTT%NPAR_USETYPE(JLIST),:)
         PEND_HOLIDAY(JLIST,:,2) = BDD%XDESC_END_HOLIDAY(DTT%NPAR_USETYPE(JLIST),:)
         PEND_HOLIDAY(JLIST,:,3) = BDD%XDESC_END_HOLIDAY(DTT%NPAR_USETYPE(JLIST),:)
         PEND_HOLIDAY(JLIST,:,4) = BDD%XDESC_END_HOLIDAY(DTT%NPAR_USETYPE(JLIST),:)
         PEND_HOLIDAY(JLIST,:,5) = BDD%XDESC_END_HOLIDAY(BDD%NDESC_POS_USE_COM,:)
         PEND_HOLIDAY(JLIST,:,6) = BDD%XDESC_END_HOLIDAY(BDD%NDESC_POS_USE_TER,:)
      ELSE
         CALL ABOR1_SFX ("No rule for this number of compartments")           
      ENDIF
      !
   ENDIF
   !
   IF (PRESENT(PMOD_HOLIDAY)) THEN
      !
      IF (SIZE(PMOD_HOLIDAY,2).EQ.1) THEN      
         PMOD_HOLIDAY(JLIST,1) = BDD%XDESC_MOD_HOLIDAY(DTT%NPAR_USETYPE(JLIST))
      ELSEIF (SIZE(PMOD_HOLIDAY,2).EQ.6) THEN
         PMOD_HOLIDAY(JLIST,1) = BDD%XDESC_MOD_HOLIDAY(DTT%NPAR_USETYPE(JLIST))
         PMOD_HOLIDAY(JLIST,2) = BDD%XDESC_MOD_HOLIDAY(DTT%NPAR_USETYPE(JLIST))
         PMOD_HOLIDAY(JLIST,3) = BDD%XDESC_MOD_HOLIDAY(DTT%NPAR_USETYPE(JLIST))
         PMOD_HOLIDAY(JLIST,4) = BDD%XDESC_MOD_HOLIDAY(DTT%NPAR_USETYPE(JLIST))
         PMOD_HOLIDAY(JLIST,5) = BDD%XDESC_MOD_HOLIDAY(BDD%NDESC_POS_USE_COM)
         PMOD_HOLIDAY(JLIST,6) = BDD%XDESC_MOD_HOLIDAY(BDD%NDESC_POS_USE_TER)
      ELSE
         CALL ABOR1_SFX ("No rule for this number of compartments")                  
      ENDIF
      !
   ENDIF
   !
ENDDO
!
IF (LHOOK) CALL DR_HOOK('INI_DATA_PARAM_TEB',1,ZHOOK_HANDLE)
!-------------------------------------------------------------------------------
!
END SUBROUTINE INI_DATA_PARAM_TEB
