!     #########
      SUBROUTINE FLOOD_UPDATE (PTAB_F,PTAB_H,PTAB_VF,PAREA,PFLOOD_STO, &
                                 PHFLOOD,PFFLOOD,PFLOOD_LEN,PWFLOOD      )  
!     ##########################################################################
!
!!****  *FLOOD_UPDATE*  
!!
!!    PURPOSE
!!    -------
!
!     Compute HFLOOD, FFLOOD, LFLOOD, WFLOOD.
!     
!!**  METHOD
!!    ------
!
!     Direct calculation
!
!!    EXTERNAL
!!    --------
!
!     None
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!
!!      
!!    REFERENCE
!!    ---------
!!      
!!    AUTHOR
!!    ------
!!	B. Decharme     
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    01/11/06 
!-------------------------------------------------------------------------------
!
!*       0.     DECLARATIONS
!               ------------
!
USE MODD_TRIP_PAR, ONLY : XTRIP_UNDEF, XRHOLW_T
USE MODD_TRIP_n,   ONLY : XRATMED
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
REAL,DIMENSION(:), INTENT(IN)  :: PTAB_F  ! Flood fraction array
REAL,DIMENSION(:), INTENT(IN)  :: PTAB_H  ! Topo height array
REAL,DIMENSION(:), INTENT(IN)  :: PTAB_VF ! Flood volume array
REAL,              INTENT(IN)  :: PAREA   ! grid area                 [m²]
REAL,              INTENT(IN)  :: PFLOOD_STO ! Floodplain water mass  [kg]
!
REAL,              INTENT(OUT) :: PHFLOOD ! Floodplain fraction        [-]
REAL,              INTENT(OUT) :: PFFLOOD    ! Floodplain water depth  [m]
REAL,              INTENT(OUT) :: PFLOOD_LEN ! Floodplain lenght       [m]
REAL,              INTENT(OUT) :: PWFLOOD    ! Floodplain width        [m]
!
!*      0.2    declarations of local variables
!
REAL,    DIMENSION(:), ALLOCATABLE :: ZCOMP
!
REAL    :: ZF_UP, ZF_DOWN, ZV_UP, ZV_DOWN, ZH_UP, ZH_DOWN, ZSLOPE
!
INTEGER, DIMENSION(:), ALLOCATABLE  ::IUP, IDOWN
!
INTEGER :: I, IPAS
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
! Initialize and allocate local variable
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('FLOOD_UPDATE',0,ZHOOK_HANDLE)
PHFLOOD    = 0.0
PFFLOOD    = 0.0
PWFLOOD    = 0.0
PFLOOD_LEN = 0.0
!
IF(PFLOOD_STO<=0 .AND. LHOOK) CALL DR_HOOK('FLOOD_UPDATE',1,ZHOOK_HANDLE)
IF(PFLOOD_STO<=0)RETURN
!
IPAS=SIZE(PTAB_VF(:))
!
ALLOCATE(ZCOMP(IPAS))
ALLOCATE(IUP  (1))
ALLOCATE(IDOWN(1))
!
!-------------------------------------------------------------------------------
!compare arrays
!
ZCOMP(:) = PTAB_VF(:)-PFLOOD_STO
!
!-------------------------------------------------------------------------------
!calculate array index
!
IF(ALL(ZCOMP(:)<0.0))THEN
   IUP  (1) = IPAS
   IDOWN(1) = IPAS
ELSE
   IUP  (:) = MINLOC(ZCOMP(:),ZCOMP(:)>=0.0)
   IDOWN(:) = MAXLOC(ZCOMP(:),ZCOMP(:)<=0.0)
ENDIF
!
!-------------------------------------------------------------------------------
!new born
!
ZF_UP   = PTAB_F (IUP  (1))
ZF_DOWN = PTAB_F (IDOWN(1))
ZV_UP   = PTAB_VF(IUP  (1))
ZV_DOWN = PTAB_VF(IDOWN(1))
ZH_UP   = PTAB_H (IUP  (1))
ZH_DOWN = PTAB_H (IDOWN(1))
!
!-------------------------------------------------------------------------------
!Calculate new Fflood
!          
ZSLOPE = 0.0
IF(IUP(1)/=IDOWN(1)) ZSLOPE = (ZF_UP-ZF_DOWN)/(ZV_UP-ZV_DOWN)
!          
PFFLOOD = ZF_DOWN + (PFLOOD_STO-ZV_DOWN) * ZSLOPE
!
!-------------------------------------------------------------------------------
!Calculate new Hflood
!
ZSLOPE = 0.0
IF(IUP(1)/=IDOWN(1)) ZSLOPE = (ZH_UP-ZH_DOWN)/(ZV_UP-ZV_DOWN)
!          
PHFLOOD = ZH_DOWN + (PFLOOD_STO-ZV_DOWN) * ZSLOPE
!
!-------------------------------------------------------------------------------
!Calculate special case
!
IF(PFFLOOD>=1.0)THEN
   PFFLOOD=1.0
   PHFLOOD= ZH_DOWN + (PFLOOD_STO-ZV_DOWN)/(XRHOLW_T*PAREA)
ENDIF
!
!-------------------------------------------------------------------------------
!Calculate new Wflood, Lflood
!
PFLOOD_LEN = XRATMED*SQRT(PFFLOOD*PAREA)
PWFLOOD    = PAREA*PFFLOOD/PFLOOD_LEN
!
!-------------------------------------------------------------------------------
! Deallocate local variable
!-------------------------------------------------------------------------------
!
DEALLOCATE(ZCOMP)
DEALLOCATE(IUP  )
DEALLOCATE(IDOWN)
IF (LHOOK) CALL DR_HOOK('FLOOD_UPDATE',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
END SUBROUTINE FLOOD_UPDATE
