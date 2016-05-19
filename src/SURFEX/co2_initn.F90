!     #########
      SUBROUTINE CO2_INIT_n(HPHOTO, KSIZE_NATURE_P, KR_NATURE_P, PVEGTYPE_PATCH, &
                            PCO2, PGMES, PGC, PDMAX, PABC, PPOI, PANMAX, &
                            PFZERO, PEPSO, PGAMM, PQDGAMM, PQDGMES,      &
                            PT1GMES, PT2GMES, PAMAX, PQDAMAX,            &
                            PT1AMAX, PT2AMAX, PAH, PBH,                  &
                            PTAU_WOOD, PINCREASE, PTURNOVER              )
!     #####################
!
!!****  *CO2_INIT_n* - routine to initialize ISBA-AGS variables
!!
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
!!	V. Masson   *Meteo France*	
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    02/2003 
!!      J.C. Calvet 01/2004 Externalization
!!      P Le Moigne 11/2004 cotwoinit changed into cotwoinit_n
!!      P Le Moigne 09/2005 AGS modifs of L. Jarlan
!!      S Lafont    09/2008 Add initialisation of POI and ABC (needed for TORI)
!!      A.L. Gibelin 04/2009 : TAU_WOOD for NCB option 
!!      A.L. Gibelin 04/2009 : Add carbon spinup
!!      A.L. Gibelin 07/2009 : Suppress RDK and transform GPP as a diagnostic
!!      A.L. Gibelin 07/2009 : Suppress PPST and PPSTF as outputs
!!
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
USE MODD_SURF_PAR,       ONLY : XUNDEF
USE MODD_DATA_COVER_PAR, ONLY : NVEGTYPE
!
USE MODI_COTWOINIT_n
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*       0.1   Declarations of arguments
!              -------------------------
!
 CHARACTER(LEN=3), INTENT(IN) :: HPHOTO
INTEGER, DIMENSION(:), INTENT(IN) :: KSIZE_NATURE_P
INTEGER, DIMENSION(:,:), INTENT(IN) :: KR_NATURE_P
REAL, DIMENSION(:,:,:), INTENT(IN) :: PVEGTYPE_PATCH
REAL, DIMENSION(:), INTENT(IN) :: PCO2 ! air CO2 concentration (kg/kg)
REAL, DIMENSION(:,:), INTENT(IN) :: PGMES
REAL, DIMENSION(:,:), INTENT(IN) :: PGC
REAL, DIMENSION(:,:), INTENT(IN) :: PDMAX
REAL, DIMENSION(:), INTENT(OUT) :: PABC
REAL, DIMENSION(:), INTENT(OUT) :: PPOI
REAL, DIMENSION(:,:), INTENT(OUT) :: PANMAX
REAL, DIMENSION(:,:), INTENT(OUT) :: PFZERO
REAL, DIMENSION(:,:), INTENT(OUT) :: PEPSO
REAL, DIMENSION(:,:), INTENT(OUT) :: PGAMM
REAL, DIMENSION(:,:), INTENT(OUT) :: PQDGAMM
REAL, DIMENSION(:,:), INTENT(OUT) :: PQDGMES
REAL, DIMENSION(:,:), INTENT(OUT) :: PT1GMES
REAL, DIMENSION(:,:), INTENT(OUT) :: PT2GMES
REAL, DIMENSION(:,:), INTENT(OUT) :: PAMAX
REAL, DIMENSION(:,:), INTENT(OUT) :: PQDAMAX
REAL, DIMENSION(:,:), INTENT(OUT) :: PT1AMAX
REAL, DIMENSION(:,:), INTENT(OUT) :: PT2AMAX
REAL, DIMENSION(:,:), INTENT(OUT) :: PAH
REAL, DIMENSION(:,:), INTENT(OUT) :: PBH
REAL, DIMENSION(:,:), INTENT(OUT) :: PTAU_WOOD
REAL, DIMENSION(:,:,:), INTENT(OUT) :: PINCREASE
REAL, DIMENSION(:,:,:), INTENT(OUT) :: PTURNOVER
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
REAL, DIMENSION(:,:), ALLOCATABLE :: ZP_VEGTYPE_PATCH  ! vegtypes present for each tile
REAL, DIMENSION(:),   ALLOCATABLE :: ZP_GMES           ! 
REAL, DIMENSION(:),   ALLOCATABLE :: ZP_CO2            ! air CO2 concentration (kg/kg)
REAL, DIMENSION(:),   ALLOCATABLE :: ZP_GC             !
REAL, DIMENSION(:),   ALLOCATABLE :: ZP_DMAX           !
REAL, DIMENSION(:),   ALLOCATABLE :: ZP_ANMAX          !
REAL, DIMENSION(:),   ALLOCATABLE :: ZP_FZERO          !
REAL, DIMENSION(:),   ALLOCATABLE :: ZP_EPSO           !
REAL, DIMENSION(:),   ALLOCATABLE :: ZP_GAMM           !
REAL, DIMENSION(:),   ALLOCATABLE :: ZP_QDGAMM         !
REAL, DIMENSION(:),   ALLOCATABLE :: ZP_QDGMES         !
REAL, DIMENSION(:),   ALLOCATABLE :: ZP_T1GMES         !
REAL, DIMENSION(:),   ALLOCATABLE :: ZP_T2GMES         !
REAL, DIMENSION(:),   ALLOCATABLE :: ZP_AMAX           !
REAL, DIMENSION(:),   ALLOCATABLE :: ZP_QDAMAX         !
REAL, DIMENSION(:),   ALLOCATABLE :: ZP_T1AMAX         !
REAL, DIMENSION(:),   ALLOCATABLE :: ZP_T2AMAX         !
REAL, DIMENSION(:),   ALLOCATABLE :: ZP_AH             !
REAL, DIMENSION(:),   ALLOCATABLE :: ZP_BH             !
REAL, DIMENSION(:),   ALLOCATABLE :: ZP_TAU_WOOD       !
REAL, DIMENSION(:,:), ALLOCATABLE :: ZP_INCREASE       !
REAL, DIMENSION(:,:), ALLOCATABLE :: ZP_TURNOVER       !
REAL, DIMENSION(:),   ALLOCATABLE :: ZP_ABC            !
REAL, DIMENSION(:),   ALLOCATABLE :: ZP_POI            !
!
INTEGER :: ILU   ! size of arrays
INTEGER :: IPATCH
INTEGER :: INBIOMASS
INTEGER :: JP    ! loop on tiles
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('CO2_INIT_N',0,ZHOOK_HANDLE)
!
ILU    = SIZE(PVEGTYPE_PATCH,1)
IPATCH = SIZE(PVEGTYPE_PATCH,3)
INBIOMASS = SIZE(PINCREASE,2)
!
DO JP=1,IPATCH
!
  IF (KSIZE_NATURE_P(JP) == 0 ) CYCLE
!
     CALL PACK_CO2_INIT(KR_NATURE_P(:,JP),KSIZE_NATURE_P(JP),JP)
!
     CALL COTWOINIT_n(HPHOTO, ZP_VEGTYPE_PATCH,ZP_GMES,ZP_CO2,ZP_GC,   &
            ZP_DMAX,ZP_ABC,ZP_POI,ZP_ANMAX,ZP_FZERO,            &
            ZP_EPSO,ZP_GAMM,ZP_QDGAMM,ZP_QDGMES,ZP_T1GMES,      &
            ZP_T2GMES,ZP_AMAX,ZP_QDAMAX,ZP_T1AMAX,              &
            ZP_T2AMAX,ZP_AH,ZP_BH,ZP_TAU_WOOD                   )  

     ZP_INCREASE = 0.
     ZP_TURNOVER = 0.
!
     CALL UNPACK_CO2_INIT(KR_NATURE_P(:,JP),KSIZE_NATURE_P(JP),JP)
ENDDO
!
!-------------------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('CO2_INIT_N',1,ZHOOK_HANDLE)
CONTAINS
!-------------------------------------------------------------------------------
SUBROUTINE PACK_CO2_INIT(KMASK,KSIZE,KPATCH)
IMPLICIT NONE
INTEGER, INTENT(IN)               :: KSIZE, KPATCH
INTEGER, DIMENSION(:), INTENT(IN) :: KMASK
!
INTEGER JJ, JI
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
IF (LHOOK) CALL DR_HOOK('PACK_CO2_INIT',0,ZHOOK_HANDLE)
ALLOCATE(ZP_VEGTYPE_PATCH(KSIZE,NVEGTYPE))
ALLOCATE(ZP_GMES         (KSIZE))
ALLOCATE(ZP_CO2          (KSIZE))
ALLOCATE(ZP_GC           (KSIZE))
ALLOCATE(ZP_DMAX         (KSIZE))
ALLOCATE(ZP_ANMAX        (KSIZE))
ALLOCATE(ZP_FZERO        (KSIZE))
ALLOCATE(ZP_EPSO         (KSIZE))
ALLOCATE(ZP_GAMM         (KSIZE))
ALLOCATE(ZP_QDGAMM       (KSIZE))
ALLOCATE(ZP_QDGMES       (KSIZE))
ALLOCATE(ZP_T1GMES       (KSIZE))
ALLOCATE(ZP_T2GMES       (KSIZE))
ALLOCATE(ZP_AMAX         (KSIZE))
ALLOCATE(ZP_QDAMAX       (KSIZE))
ALLOCATE(ZP_T1AMAX       (KSIZE))
ALLOCATE(ZP_T2AMAX       (KSIZE))
ALLOCATE(ZP_AH           (KSIZE))
ALLOCATE(ZP_BH           (KSIZE))
ALLOCATE(ZP_TAU_WOOD     (KSIZE))
ALLOCATE(ZP_INCREASE     (KSIZE,INBIOMASS))
ALLOCATE(ZP_TURNOVER     (KSIZE,INBIOMASS))
!
! initialisation needed for TORI
ALLOCATE(ZP_ABC(SIZE(PABC)))
ALLOCATE(ZP_POI(SIZE(PPOI)))
ZP_ABC(:)=0.
ZP_POI(:)=0.
!
DO JJ=1,KSIZE
  JI                     =    KMASK(JJ)
  ZP_VEGTYPE_PATCH(JJ,:) =    PVEGTYPE_PATCH(JI,:,KPATCH)
  ZP_GMES         (JJ)   =    PGMES         (JI,KPATCH)
  ZP_CO2          (JJ)   =    PCO2          (JI)
  ZP_GC           (JJ)   =    PGC           (JI,KPATCH)
  ZP_DMAX         (JJ)   =    PDMAX         (JI,KPATCH)
END DO
IF (LHOOK) CALL DR_HOOK('PACK_CO2_INIT',1,ZHOOK_HANDLE)
!-------------------------------------------------------------------------------
END SUBROUTINE PACK_CO2_INIT
!-------------------------------------------------------------------------------
SUBROUTINE UNPACK_CO2_INIT(KMASK,KSIZE,KPATCH)
IMPLICIT NONE
INTEGER, INTENT(IN)               :: KSIZE, KPATCH
INTEGER, DIMENSION(:), INTENT(IN) :: KMASK
!
INTEGER JJ, JI
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
IF (LHOOK) CALL DR_HOOK('UNPACK_CO2_INIT',0,ZHOOK_HANDLE)
PANMAX     (:,KPATCH) = XUNDEF
PFZERO     (:,KPATCH) = XUNDEF
PEPSO      (:,KPATCH) = XUNDEF
PGAMM      (:,KPATCH) = XUNDEF
PQDGAMM    (:,KPATCH) = XUNDEF
PQDGMES    (:,KPATCH) = XUNDEF
PT1GMES    (:,KPATCH) = XUNDEF
PT2GMES    (:,KPATCH) = XUNDEF
PAMAX      (:,KPATCH) = XUNDEF
PQDAMAX    (:,KPATCH) = XUNDEF
PT1AMAX    (:,KPATCH) = XUNDEF
PT2AMAX    (:,KPATCH) = XUNDEF
PAH        (:,KPATCH) = XUNDEF
PBH        (:,KPATCH) = XUNDEF
PTAU_WOOD  (:,KPATCH) = XUNDEF
PINCREASE  (:,:,KPATCH) = XUNDEF
PTURNOVER  (:,:,KPATCH) = XUNDEF
!
DO JJ=1,KSIZE
   JI                              = KMASK         (JJ)
   PANMAX          (JI, KPATCH)    = ZP_ANMAX      (JJ)
   PFZERO          (JI, KPATCH)    = ZP_FZERO      (JJ)
   PEPSO           (JI, KPATCH)    = ZP_EPSO       (JJ)
   PGAMM           (JI, KPATCH)    = ZP_GAMM       (JJ)
   PQDGAMM         (JI, KPATCH)    = ZP_QDGAMM     (JJ)
   PQDGMES         (JI, KPATCH)    = ZP_QDGMES     (JJ)
   PT1GMES         (JI, KPATCH)    = ZP_T1GMES     (JJ)
   PT2GMES         (JI, KPATCH)    = ZP_T2GMES     (JJ)
   PAMAX           (JI, KPATCH)    = ZP_AMAX       (JJ)
   PQDAMAX         (JI, KPATCH)    = ZP_QDAMAX     (JJ)
   PT1AMAX         (JI, KPATCH)    = ZP_T1AMAX     (JJ)
   PT2AMAX         (JI, KPATCH)    = ZP_T2AMAX     (JJ)
   PAH             (JI, KPATCH)    = ZP_AH         (JJ)
   PBH             (JI, KPATCH)    = ZP_BH         (JJ)
   PTAU_WOOD       (JI, KPATCH)    = ZP_TAU_WOOD   (JJ)
   PINCREASE       (JI, :, KPATCH) = ZP_INCREASE   (JJ, :)
   PTURNOVER       (JI, :, KPATCH) = ZP_TURNOVER   (JJ, :)
END DO
! 
DO JJ=1,SIZE(PABC)
   PABC(JJ)=ZP_ABC(JJ)
   PPOI(JJ)=ZP_POI(JJ)
END DO 

DEALLOCATE(ZP_VEGTYPE_PATCH)
DEALLOCATE(ZP_GMES         )
DEALLOCATE(ZP_CO2          )
DEALLOCATE(ZP_GC           )
DEALLOCATE(ZP_DMAX         )
DEALLOCATE(ZP_ANMAX        )
DEALLOCATE(ZP_FZERO        )
DEALLOCATE(ZP_EPSO         )
DEALLOCATE(ZP_GAMM         )
DEALLOCATE(ZP_QDGAMM       )
DEALLOCATE(ZP_QDGMES       )
DEALLOCATE(ZP_T1GMES       )
DEALLOCATE(ZP_T2GMES       )
DEALLOCATE(ZP_AMAX         )
DEALLOCATE(ZP_QDAMAX       )
DEALLOCATE(ZP_T1AMAX       )
DEALLOCATE(ZP_T2AMAX       )
DEALLOCATE(ZP_AH           )
DEALLOCATE(ZP_BH           )
DEALLOCATE(ZP_TAU_WOOD     )
DEALLOCATE(ZP_INCREASE     )
DEALLOCATE(ZP_TURNOVER     )
DEALLOCATE(ZP_ABC          )
DEALLOCATE(ZP_POI          )
IF (LHOOK) CALL DR_HOOK('UNPACK_CO2_INIT',1,ZHOOK_HANDLE)
!-------------------------------------------------------------------------------
END SUBROUTINE UNPACK_CO2_INIT
!-------------------------------------------------------------------------------
!
END SUBROUTINE CO2_INIT_n
