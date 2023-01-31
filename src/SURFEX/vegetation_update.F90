!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
    SUBROUTINE VEGETATION_UPDATE (DTCO, DTV, KDIM, IO, KK, PK, PEK,   &
                                  KMONTH, KDAY, KPATCH, PTSTEP, TTIME,&
                                  PCOVER, OCOVER, OAGRIP, OECOSG,     &
                                  OIRRIGMODE, HSFTYPE,                &
                                  ISSK, ODUPDATED, OABSENT, OHG, OHV  )
!   ###############################################################
!!****  *VEGETATION EVOL*
!!
!!    PURPOSE
!!    -------
!
!     performs the time evolution of vegetation parameters
!       at UTC midnight for prescribed parameters, with effective change each ten days
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
!!
!!      V. Masson          * Meteo-France *
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    01/03/03 
!!
!!      P Le Moigne 09/2005 AGS modifs of L. Jarlan
!!      P Samuelsson 10/2014 MEB
!!      A. Druel     02/2019 Transmit ECOSG & NPAR_VEG_IRR_USE for irrigation
!!
!-------------------------------------------------------------------------------
!
!*       0.     DECLARATIONS
!               ------------
!
USE MODD_DATA_COVER_n,   ONLY : DATA_COVER_t
USE MODD_DATA_ISBA_n,    ONLY : DATA_ISBA_t
USE MODD_ISBA_n,         ONLY : ISBA_K_t, ISBA_P_t, ISBA_PE_t
USE MODD_ISBA_OPTIONS_n, ONLY : ISBA_OPTIONS_t
USE MODD_SSO_n,          ONLY : SSO_t
USE MODD_TEB_OPTION_n,   ONLY : TEB_OPTIONS_t
!
USE MODD_TYPE_DATE_SURF
!
USE MODI_INIT_ISBA_MIXPAR
USE MODI_CONVERT_PATCH_ISBA
USE MODI_CONVERT_PATCH_ALB_ISBA
USE MODI_INIT_FROM_DATA_TEB_VEG_n
USE MODI_SUBSCALE_Z0EFF
USE MODI_ALBEDO
USE MODI_UPDATE_DATA_COVER
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
!
TYPE(DATA_COVER_t),   INTENT(INOUT) :: DTCO
TYPE(DATA_ISBA_t) ,   INTENT(INOUT) :: DTV
INTEGER,              INTENT(IN)    :: KDIM
TYPE(ISBA_OPTIONS_t), INTENT(INOUT) :: IO
TYPE(ISBA_K_t),       INTENT(INOUT) :: KK
TYPE(ISBA_P_t),       INTENT(INOUT) :: PK
TYPE(ISBA_PE_t),      INTENT(INOUT) :: PEK
TYPE(SSO_t),          INTENT(INOUT) :: ISSK
!
TYPE(DATE_TIME),      INTENT(IN)    :: TTIME   ! UTC time
!
!
INTEGER,                INTENT(IN) :: KMONTH
INTEGER,                INTENT(IN) :: KDAY
INTEGER,                INTENT(IN) :: KPATCH
REAL,                   INTENT(IN) :: PTSTEP  ! time step
REAL,   DIMENSION(:,:), INTENT(IN) :: PCOVER  ! cover types
LOGICAL, DIMENSION(:),  INTENT(IN) :: OCOVER
LOGICAL,                INTENT(IN) :: OAGRIP
LOGICAL,                INTENT(IN) :: OECOSG
LOGICAL,                INTENT(IN) :: OIRRIGMODE
CHARACTER(LEN=*),       INTENT(IN) :: HSFTYPE    ! nature / garden
LOGICAL,                INTENT(OUT):: ODUPDATED  ! T if parameters are being reset
!
LOGICAL,DIMENSION(:),   INTENT(IN), OPTIONAL :: OABSENT ! T where field is not defined
LOGICAL,                INTENT(IN), OPTIONAL :: OHG ! T if garden option is activated
LOGICAL,                INTENT(IN), OPTIONAL :: OHV ! T if urban tree option is activated
!          
!*      0.2    declarations of local variables
!
LOGICAL :: GFIX       ! update fixed fields
LOGICAL :: GTIME      ! update time varing fields
LOGICAL :: GMEB       ! update meb fields
LOGICAL :: GIRR       ! update irrigation fields
LOGICAL :: GALBSOIL   ! update bare soil albedo fields
LOGICAL :: GALBVEG    ! update vegetation albedo
LOGICAL :: GURBTREE   ! update urban tree fields
LOGICAL :: GINTERACTIVE_VEG
!
INTEGER :: IDECADE, IDECADE2, JI, ISNOWPATCH  ! decade of simulation
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-----------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('VEGETATION_UPDATE',0,ZHOOK_HANDLE)
!
!*      2.     Non-interactive vegetation
!              --------------------------
!
IF (IO%CPHOTO=='NIT'.OR.IO%CPHOTO=='NCB')THEN 
     GINTERACTIVE_VEG = .TRUE.
ELSE
     GINTERACTIVE_VEG = .FALSE.
ENDIF
!
!*      2.1    Decade
!              ------
!
IDECADE = 3 * ( TTIME%TDATE%MONTH - 1 ) + MIN(TTIME%TDATE%DAY-1,29) / 10 + 1
IDECADE2 = IDECADE
ODUPDATED=.FALSE.
!
!*      2.2    From ecoclimap
!              --------------
!
!* new decade?
!
IF ( MOD(MIN(TTIME%TDATE%DAY,30),10)==1 .AND. TTIME%TIME - PTSTEP < 0.) THEN
  !
  ODUPDATED=.TRUE.
  !
  !* time varying parameters
  !
  IF (IO%LECOCLIMAP .OR. HSFTYPE=='NAT') THEN
    !
    !* new year ? --> recomputes data LAI and derivated parameters (usefull in case of ecoclimap2)
    !
    IF (KPATCH==1) CALL UPDATE_DATA_COVER(DTCO, DTV, KDIM, IO%NPATCH, IO%LMEB_PATCH, TTIME%TDATE%YEAR)  
    !
    IF (HSFTYPE=='NAT') THEN
      !
      IF (KPATCH==1) THEN
        CALL INIT_ISBA_MIXPAR(DTCO, DTV, KDIM, IO, IDECADE,IDECADE2,PCOVER,OCOVER,HSFTYPE,OECOSG)
      ELSE
        IDECADE2 = IDECADE
        IF (DTV%NTIME==2) IDECADE2 = IDECADE2 + 10 
        IDECADE2 = (IDECADE2-1) * DTV%NTIME / 36 + 1
        IF (DTV%NTIME==2 .AND. IDECADE2==3) IDECADE2 = 1
      ENDIF
      !
      GALBSOIL = .FALSE.! update bare soil albedo fields
      GALBVEG  = .TRUE. ! update only vegetation albedo
      !
      CALL CONVERT_PATCH_ALB_ISBA(DTCO, DTV, IO, IDECADE, IDECADE2, PCOVER, OCOVER,     &
                                  HSFTYPE, KPATCH, KK, PK, PEK, GALBSOIL, GALBVEG       )
      !
      IF(.NOT.GINTERACTIVE_VEG)THEN
        !
        ! If vegetation is not prognostic then update
        !
        GFIX      = .FALSE.   ! update fixed fields
        GTIME     = .TRUE.    ! update time varing fields
        GMEB      = .TRUE.    ! update meb fields
        GIRR      = .TRUE.    ! update irrigation fields
        GURBTREE  = .FALSE.   ! update urban tree fields
        !
        CALL CONVERT_PATCH_ISBA(DTCO, DTV, IO, KMONTH, KDAY, IDECADE, IDECADE2, PCOVER, OCOVER, &
                                OAGRIP, OECOSG, OIRRIGMODE, HSFTYPE, KPATCH, KK, PK, PEK,       &
                                GFIX, GTIME, GMEB, GIRR, GURBTREE          )
        !
      ENDIF
      !
    ELSE
      !
      GALBSOIL = .FALSE. ! update bare soil albedo fields
      GALBVEG  = .TRUE. ! update only vegetation albedo
      !
      CALL CONVERT_PATCH_ALB_ISBA(DTCO, DTV, IO, IDECADE, IDECADE2, PCOVER, OCOVER,     &
                                  HSFTYPE, KPATCH, KK, PK, PEK, GALBSOIL, GALBVEG       )
      !
      IF(.NOT.GINTERACTIVE_VEG)THEN
        !
        ! If vegetation is not prognostic then update
        !
        GFIX      = .FALSE.    ! update fixed fields
        GTIME     = .TRUE.     ! update time varing fields
        GMEB      = .FALSE.    ! update meb fields
        GIRR      = .FALSE.    ! update irrigation fields
        GURBTREE  = .FALSE.    ! update urban tree fields
        !
        CALL CONVERT_PATCH_ISBA(DTCO, DTV, IO, KMONTH, KDAY, IDECADE, IDECADE2, PCOVER, OCOVER, &
                                OAGRIP, OECOSG, OIRRIGMODE, HSFTYPE, KPATCH, KK, PK, PEK,       &
                                GFIX, GTIME, GMEB, GIRR, GURBTREE          )
        !
      ENDIF
      !
    ENDIF
    !
    IF (IO%CALBEDO=='CM13') THEN
      !
      GALBSOIL  = .TRUE.  ! update bare soil albedo fields
      GALBVEG   = .FALSE. ! update only vegetation albedo
      !
      CALL CONVERT_PATCH_ALB_ISBA(DTCO, DTV, IO, IDECADE, IDECADE2, PCOVER, OCOVER,     &
                                  HSFTYPE, KPATCH, KK, PK, PEK, GALBSOIL, GALBVEG       )
      !
    ENDIF
    !
  ELSEIF (.NOT.GINTERACTIVE_VEG .AND. (HSFTYPE=='GRD'.OR.HSFTYPE=='GNR') ) THEN
    !
    CALL INIT_FROM_DATA_TEB_VEG_n(DTV, KK, PK, PEK, IDECADE, .FALSE., .FALSE., .TRUE., .FALSE., OHG, OHV )
    !
  ENDIF
  !
  !* default values to avoid problems in physical routines
  !  for points where there is no vegetation or soil to be simulated by ISBA.
  !
  IF (.NOT.GINTERACTIVE_VEG .AND. PRESENT(OABSENT)) THEN
    !
    WHERE (OABSENT(:))
      PEK%XVEG       (:) = 0.
      PEK%XLAI       (:) = 0.
      PEK%XRSMIN     (:) = 40.
      PEK%XGAMMA     (:) = 0.
      PEK%XWRMAX_CF  (:) = 0.2
      PEK%XRGL       (:) = 100.
      PEK%XCV        (:) = 2.E-5
      PEK%XZ0        (:) = 0.013
      PEK%XALBNIR_VEG(:) = 0.30
      PEK%XALBVIS_VEG(:) = 0.30
      PEK%XALBUV_VEG (:) = 0.06
      PEK%XEMIS      (:) = 0.94                
    END WHERE
    !
    IF (IO%CPHOTO/='NON') THEN
      !
      WHERE (OABSENT(:))
        PEK%XGMES      (:) = 0.020
        PEK%XBSLAI     (:) = 0.36
        PEK%XLAIMIN    (:) = 0.3
        PEK%XSEFOLD    (:) = 90*86400.
        PEK%XGC        (:) = 0.00025                  
        PEK%XF2I       (:) = 0.3
      END WHERE
      !
      IF (IO%CPHOTO=='NIT' .OR. IO%CPHOTO=='NCB') THEN
        WHERE (OABSENT(:))
          PEK%XCE_NITRO  (:) = 7.68
          PEK%XCF_NITRO  (:) = -4.33
          PEK%XCNA_NITRO (:) = 1.3                      
        END WHERE
      ENDIF
      !
    ENDIF
    !
  ENDIF
  !
  IF (HSFTYPE=='NAT') THEN
    !
    !* albedo
    !
    CALL ALBEDO(IO%CALBEDO, PEK )
    !
    !* effective roughness length
    !
    IF (.NOT.GINTERACTIVE_VEG) THEN
       CALL SUBSCALE_Z0EFF(ISSK,PEK%XZ0,.FALSE.)  
    ENDIF
    !
  ENDIF
  !
END IF
!
IF (LHOOK) CALL DR_HOOK('VEGETATION_UPDATE',1,ZHOOK_HANDLE)
!
!-----------------------------------------------------------------
!
END SUBROUTINE VEGETATION_UPDATE
