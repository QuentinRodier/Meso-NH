!SURFEX_LIC Copyright 1994-2014 Meteo-France 
!SURFEX_LIC This is part of the SURFEX software governed by the CeCILL-C  licence
!SURFEX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!SURFEX_LIC for details. version 1.
!     #########
      SUBROUTINE PUT_SFXCPL_n(HPROGRAM,KI,KSW,PZENITH,PSW_BANDS,PTSRAD,     &
                                PEMIS,PDIR_ALB,PSCA_ALB,PICE,PSST,PALB_SEAICE,&
                                PUMER,PVMER,PTICE)  
!     #################################################################################################
!
!!****  *PUT_SFXCPL_n* - routine to modify some variables in surfex from information coming
!                        from an ocean and/or a river routing model (but already on Surfex grid)
!                        Direct and diffuse total albedo are initialysed to GELATO sea-ice albedo
!!    PURPOSE
!!    -------
!!
!!**  METHOD
!!    ------
!!
!!    EXTERNAL
!!    --------
!!
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!
!!    REFERENCE
!!    ---------
!!
!!
!!    AUTHOR
!!    ------
!!	B. Decharme      *Meteo France*	
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    08/2009
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
USE MODD_CSTS,       ONLY : XTT, XTTS, XICEC
!
USE MODD_SEAFLUX_n,  ONLY : XUMER, XVMER
!                            
USE MODI_PACK_SAME_RANK
USE MODI_GET_LUOUT
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
USE MODI_ABOR1_SFX
!
USE MODI_GET_1D_MASK
!
USE MODI_PUT_RAD_SEA_n
!
USE MODI_PUT_RAD_WAT_n
!
USE MODI_UPDATE_ESM_SURF_ATM_n
!
USE MODI_GET_FRAC_n
IMPLICIT NONE
!
!*       0.1   Declarations of arguments
!              -------------------------
!
 CHARACTER(LEN=6),        INTENT(IN)  :: HPROGRAM
INTEGER,                 INTENT(IN)  :: KI      ! number of points
INTEGER,                 INTENT(IN)  :: KSW     ! number of bands
!
REAL, DIMENSION(KI),      INTENT(IN) :: PZENITH
REAL, DIMENSION(KSW),     INTENT(IN) :: PSW_BANDS ! mean wavelength of each shortwave band (m)
!
REAL, DIMENSION(KI),     INTENT(OUT) :: PTSRAD   ! Total radiative temperature see by the atmosphere
REAL, DIMENSION(KI),     INTENT(OUT) :: PEMIS    ! Total emissivity see by the atmosphere
REAL, DIMENSION(KI,KSW), INTENT(OUT) :: PDIR_ALB ! Total direct albedo see by the atmosphere
REAL, DIMENSION(KI,KSW), INTENT(OUT) :: PSCA_ALB ! Total diffus albedo see by the atmosphere
!
REAL, DIMENSION(KI),     INTENT(IN   ), OPTIONAL :: PICE        ! Sea/Ice fraction
REAL, DIMENSION(KI),     INTENT(INOUT), OPTIONAL :: PSST        ! SST
REAL, DIMENSION(KI),     INTENT(INOUT), OPTIONAL :: PALB_SEAICE ! Sea_ice albedo
REAL, DIMENSION(KI),     INTENT(IN   ), OPTIONAL :: PUMER       ! U sea current
REAL, DIMENSION(KI),     INTENT(IN   ), OPTIONAL :: PVMER       ! V sea current
REAL, DIMENSION(KI),     INTENT(IN   ), OPTIONAL :: PTICE       ! Sea ice temperature
!
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
REAL, DIMENSION(KI) :: ZSEA    ! fraction of sea
REAL, DIMENSION(KI) :: ZWATER  ! fraction of water
REAL, DIMENSION(KI) :: ZNATURE ! fraction of nature
REAL, DIMENSION(KI) :: ZTOWN   ! fraction of town
!
INTEGER :: ILU, ILUOUT
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('PUT_SFXCPL_N',0,ZHOOK_HANDLE)
 CALL GET_LUOUT(HPROGRAM,ILUOUT)
!-------------------------------------------------------------------------------
!
! Global argument
!
 CALL GET_FRAC_n(HPROGRAM, KI, ZSEA, ZWATER, ZNATURE, ZTOWN)
!
!-------------------------------------------------------------------------------
! Put variable over sea tile
!
IF(PRESENT(PICE).AND.PRESENT(PSST).AND.PRESENT(PTICE).AND.PRESENT(PALB_SEAICE).AND.PRESENT(PUMER).AND.PRESENT(PVMER))THEN
!
  ILU=COUNT(ZSEA(:)>0.0)
!  
  IF(ILU>0) CALL TREAT_SURF(ILU,'S')
!
ENDIF
!
!-------------------------------------------------------------------------------
! Put variable over water tile
!
IF(PRESENT(PICE).AND.PRESENT(PSST).AND.PRESENT(PTICE).AND.PRESENT(PALB_SEAICE))THEN
!
  ILU=COUNT(ZWATER(:)>0.0)
!  
  IF(ILU>0) CALL TREAT_SURF(ILU,'W')
!  
ENDIF
!
!-------------------------------------------------------------------------------
! Update radiative properties at time t+1 for radiative scheme
!-------------------------------------------------------------------------------
!
 CALL UPDATE_ESM_SURF_ATM_n(HPROGRAM,KI,KSW,PZENITH,PSW_BANDS,PTSRAD,PDIR_ALB,PSCA_ALB,PEMIS)
!
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('PUT_SFXCPL_N',1,ZHOOK_HANDLE)
!
CONTAINS
!
SUBROUTINE TREAT_SURF(KLU,YTYPE)
!
IMPLICIT NONE
!
INTEGER, INTENT(IN) :: KLU
 CHARACTER(LEN=1), INTENT(IN) :: YTYPE
!
INTEGER, DIMENSION(KLU) :: IMASK   ! Working mask
REAL,    DIMENSION(KLU) :: ZICE     ! ice fraction from GELATO
REAL,    DIMENSION(KLU) :: ZTS      ! total SST (+ice)
REAL,    DIMENSION(KLU) :: ZTICE    ! Sea-ice temperature
REAL,    DIMENSION(KLU) :: ZDIR_ALB ! Initialization of total direct albedo
REAL,    DIMENSION(KLU) :: ZSCA_ALB ! Initialization of total direct albedo
REAL,    DIMENSION(KLU) :: ZICE_ALB ! Sea-ice albedo (from GELATO)
!
REAL    :: ZMIN, ZMAX
 CHARACTER(LEN=3)     :: HT
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
IF (LHOOK) CALL DR_HOOK('PUT_SFXCPL_N:TREAT_SURF',0,ZHOOK_HANDLE)
!
IF (YTYPE=='S') THEN
  CALL GET_1D_MASK(KLU,KI,ZSEA,IMASK)
ELSEIF (YTYPE=='W') THEN
  CALL GET_1D_MASK(KLU,KI,ZWATER,IMASK)
ENDIF
!
 CALL PACK_SAME_RANK(IMASK,PICE       (:),ZICE    (:))
 CALL PACK_SAME_RANK(IMASK,PSST       (:),ZTS     (:))
 CALL PACK_SAME_RANK(IMASK,PTICE      (:),ZTICE   (:))
 CALL PACK_SAME_RANK(IMASK,PALB_SEAICE(:),ZDIR_ALB(:))
 CALL PACK_SAME_RANK(IMASK,PALB_SEAICE(:),ZSCA_ALB(:))
 CALL PACK_SAME_RANK(IMASK,PALB_SEAICE(:),ZICE_ALB(:))
!
ZMIN=MINVAL(ZTS(:))
ZMAX=MAXVAL(ZTS(:))
!
IF(ZMIN<=0.0.OR.ZMAX>500.)THEN
  IF (YTYPE=='S') THEN
    HT='SST'
  ELSEIF (YTYPE=='W') THEN
    HT='TS'
  ENDIF
  WRITE(ILUOUT,*)'!'
  WRITE(ILUOUT,*)'PUT_SFXCPL_n: '//HT//' not define over at least one point'
  WRITE(ILUOUT,*)'              MIN '//HT//' =',ZMIN,'MAX SST =',ZMAX
  WRITE(ILUOUT,*)'              There is certainly a problem between  '
  WRITE(ILUOUT,*)'              SURFEX and OASIS sea/land mask        '
  CALL ABOR1_SFX('PUT_SFXCPL_n: Abort -> '//HT//' not define               ')
  WRITE(ILUOUT,*)'!'
ENDIF
!
WHERE(ZICE(:)>=XICEC)
  ZTS (:) = MIN(ZTS(:),XTTS-0.01)
ELSEWHERE
  ZTS (:) = MAX(ZTS(:), XTTS)
ENDWHERE
!
IF (YTYPE=='S') THEN
  CALL PUT_RAD_SEA_n(HPROGRAM,KLU,ZTS,ZTICE,ZDIR_ALB,ZSCA_ALB,ZICE_ALB)
  CALL PACK_SAME_RANK(IMASK,PUMER(:),XUMER(:))
  CALL PACK_SAME_RANK(IMASK,PVMER(:),XVMER(:))
ELSEIF (YTYPE=='W') THEN
  CALL PUT_RAD_WAT_n(HPROGRAM,KLU,ZTS,ZTICE,ZDIR_ALB,ZSCA_ALB,ZICE_ALB)
ENDIF
!
IF (LHOOK) CALL DR_HOOK('PUT_SFXCPL_N:TREAT_SURF',1,ZHOOK_HANDLE)
!
END SUBROUTINE TREAT_SURF
!
!==============================================================================
!
END SUBROUTINE PUT_SFXCPL_n
