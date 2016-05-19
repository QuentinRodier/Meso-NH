!-----------------------------------------------------------------
!     #####################
      SUBROUTINE ROUT_DATA_ISBA(HPROGRAM,KI,KSTEP)
!     #####################
!
!!****  *ROUT_DATA_ISBA*  
!!
!!    PURPOSE
!!    -------
!
!    Routes runoff and drainage of ISBA with coupling with Topmodel
!         
!     
!!**  METHOD
!!    ------
!
!!    EXTERNAL
!!    --------
!!
!!    none
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------ 
!!
!!    
!!    
!!
!!      
!!    REFERENCE
!!    ---------
!!
!!    
!!      
!!    AUTHOR
!!    ------
!!
!!      B. Vincendon	*  Meteo-France *
!!
!!    MODIFICATIONS
!!    -------------
!!
!!      Original   15/06/2007
!-------------------------------------------------------------------------------
!
!*       0.     DECLARATIONS
!               ------------
!
USE MODI_GET_LUOUT
USE MODI_UNPACK_SAME_RANK
USE MODI_DIAG_ISBA_TO_ROUT
USE MODI_ISBA_TO_TOPD
USE MODI_ROUTING
!
USE MODD_TOPODYN,        ONLY : NNCAT, NMESHT, NNMC
USE MODD_COUPLING_TOPD,  ONLY : NMASKT, XRUNOFF_TOP, XATOP, NNPIX,&
                                  XAVG_RUNOFFCM, XAVG_DRAINCM
!
USE MODD_DIAG_EVAP_ISBA_n, ONLY : XAVG_DRAINC
USE MODD_SURF_ATM_n,       ONLY : NR_NATURE
USE MODD_SURF_PAR,         ONLY : XUNDEF
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
 CHARACTER(LEN=6), INTENT(IN) :: HPROGRAM ! program calling surf. schemes
INTEGER, INTENT(IN)          :: KI     ! Grid dimensions
INTEGER, INTENT(IN)          :: KSTEP  ! current time step 
!
!*      0.2    declarations of local variables
!
INTEGER                       :: JJ,JI  ! loop control 
INTEGER                       :: IUNIT       ! unit number of results files
INTEGER                       :: ILUOUT      ! unit number of listing file
 CHARACTER(LEN=30)             :: YVAR
REAL, DIMENSION(KI)           :: ZRUNOFFC_FULL  ! Cumulated runoff from isba on the full domain (kg/m2)
REAL, DIMENSION(KI)           :: ZRUNOFFC_FULLM ! Cumulated runoff from isba on the full domain (kg/m2) at t-dt
REAL, DIMENSION(KI)           :: ZRUNOFF_ISBA   ! Runoff from Isba (kg/m2)
REAL, DIMENSION(KI)           :: ZDRAINC_FULL   ! Cumulated drainage from Isba on the full domain (kg/m2)
REAL, DIMENSION(KI)           :: ZDRAINC_FULLM  ! Cumulated drainage from Isba on the full domain (kg/m2) at t-dt
REAL, DIMENSION(KI)           :: ZDRAIN_ISBA    ! Drainage from Isba (m3/s)
REAL, DIMENSION(NNCAT,NMESHT) :: ZRUNOFF_TOPD   ! Runoff on the Topodyn grid (m3/s)
REAL, DIMENSION(NNCAT,NMESHT) :: ZDRAIN_TOPD    ! Drainage from Isba on Topodyn grid (m3/s)
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('ROUT_DATA_ISBA',0,ZHOOK_HANDLE)
!
 CALL GET_LUOUT(HPROGRAM,ILUOUT)
!
ZRUNOFFC_FULL (:) = 0.
ZRUNOFFC_FULLM(:) = 0.
ZRUNOFF_ISBA  (:) = 0.
ZRUNOFF_TOPD(:,:) = 0.
ZDRAINC_FULL  (:) = 0.
ZDRAINC_FULLM (:) = 0.
ZDRAIN_ISBA   (:) = 0.
ZDRAIN_TOPD (:,:) = 0.
!
!    Runoff on TOPODYN grid
!   ---------------------------------------
!
 CALL UNPACK_SAME_RANK(NR_NATURE,XRUNOFF_TOP,ZRUNOFFC_FULL)
 CALL UNPACK_SAME_RANK(NR_NATURE,XAVG_RUNOFFCM,ZRUNOFFC_FULLM)
!
 CALL DIAG_ISBA_TO_ROUT(ZRUNOFFC_FULL,ZRUNOFFC_FULLM,ZRUNOFF_ISBA)
!
XAVG_RUNOFFCM(:) = XRUNOFF_TOP(:)
ZRUNOFF_TOPD(:,:) = 0.0
!
 CALL ISBA_TO_TOPD(ZRUNOFF_ISBA,ZRUNOFF_TOPD)
!
DO JJ=1,NNCAT
  DO JI=1,NNMC(JJ)
    ZRUNOFF_TOPD(JJ,JI) = ZRUNOFF_TOPD(JJ,JI) / NNPIX(NMASKT(JJ,JI))
  ENDDO
ENDDO
!
!    Drainage treatment
!    ----------------------------------------
!
 CALL UNPACK_SAME_RANK(NR_NATURE,XAVG_DRAINC*XATOP,ZDRAINC_FULL)
 CALL UNPACK_SAME_RANK(NR_NATURE,XAVG_DRAINCM*XATOP,ZDRAINC_FULLM)
!
 CALL DIAG_ISBA_TO_ROUT(ZDRAINC_FULL,ZDRAINC_FULLM,ZDRAIN_ISBA)
!
XAVG_DRAINCM(:)  = XAVG_DRAINC(:)
ZDRAIN_TOPD(:,:) = 0.0
!
 CALL ISBA_TO_TOPD(ZDRAIN_ISBA,ZDRAIN_TOPD)
!
DO JJ=1,NNCAT
  DO JI=1,NNMC(JJ)
    ZDRAIN_TOPD(JJ,JI) = ZDRAIN_TOPD(JJ,JI) / NNPIX(NMASKT(JJ,JI))
  ENDDO
ENDDO
!*     Routing (runoff + drainage)
!     ----------------------------------------
!
 CALL ROUTING(ZRUNOFF_TOPD,ZDRAIN_TOPD,KSTEP)
!
IF (LHOOK) CALL DR_HOOK('ROUT_DATA_ISBA',1,ZHOOK_HANDLE)
!
END SUBROUTINE ROUT_DATA_ISBA
