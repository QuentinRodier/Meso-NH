!SURFEX_LIC Copyright 1994-2014 Meteo-France 
!SURFEX_LIC This is part of the SURFEX software governed by the CeCILL-C  licence
!SURFEX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!SURFEX_LIC for details. version 1.
!     #########
      SUBROUTINE DIAG_CPL_ESM_ISBA(PTSTEP,PCPL_DRAIN,PCPL_RUNOFF,PCPL_EFLOOD, &
                                     PCPL_PFLOOD,PCPL_IFLOOD,PCPL_ICEFLUX         )  
!     #####################################################################
!
!!****  *DIAG_CPL_ESM_ISBA*  
!!
!!    PURPOSE
!!    -------
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
!!    REFERENCE
!!    ---------
!!
!!      
!!    AUTHOR
!!    ------
!!
!!	B. Decharme           * Meteo-France *
!!
!!    MODIFICATIONS
!!    -------------
!!
!-------------------------------------------------------------------------------
!
!*       0.     DECLARATIONS
!               ------------
!
USE MODD_CSTS,        ONLY : XLVTT, XRHOLW
USE MODD_ISBA_GRID_n, ONLY : XMESH_SIZE
USE MODD_ISBA_n,      ONLY : XPATCH, XCPL_EFLOOD, XCPL_PFLOOD,     &
                               XCPL_IFLOOD, XCPL_DRAIN, XCPL_RUNOFF, &
                               XFFLOOD, XPIFLOOD, XTSTEP_COUPLING,   &
                               XCPL_ICEFLUX, LFLOOD, LGLACIER  
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
REAL, INTENT(IN)                   :: PTSTEP
REAL, DIMENSION(:,:), INTENT(IN)   :: PCPL_DRAIN
REAL, DIMENSION(:,:), INTENT(IN)   :: PCPL_RUNOFF
REAL, DIMENSION(:,:), INTENT(IN)   :: PCPL_EFLOOD
REAL, DIMENSION(:,:), INTENT(IN)   :: PCPL_PFLOOD
REAL, DIMENSION(:,:), INTENT(IN)   :: PCPL_IFLOOD
REAL, DIMENSION(:,:), INTENT(IN)   :: PCPL_ICEFLUX
!
!*      0.2    declarations of local variables
!
INTEGER :: JPATCH ! tile loop counter
REAL, DIMENSION(SIZE(XPATCH,1)) :: ZSUMPATCH
REAL, DIMENSION(SIZE(XPATCH,1)) :: ZBUDGET
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
! Initialize and allocate local variable
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('DIAG_CPL_ESM_ISBA',0,ZHOOK_HANDLE)
ZSUMPATCH(:) = 0.
DO JPATCH=1,SIZE(XPATCH,2)
  ZSUMPATCH(:) = ZSUMPATCH(:) + XPATCH(:,JPATCH)
END DO
!
!* update ISBA - TRIP or NEMO coupling variable
!  ------------------------------------
!
!kg/m²
DO JPATCH=1,SIZE(XPATCH,2)
   WHERE (ZSUMPATCH(:) > 0.)
         XCPL_DRAIN (:) = XCPL_DRAIN (:) + PTSTEP * MAX(PCPL_DRAIN (:,JPATCH),0.0) * XPATCH(:,JPATCH)/ZSUMPATCH(:) 
         XCPL_RUNOFF(:) = XCPL_RUNOFF(:) + PTSTEP * MAX(PCPL_RUNOFF(:,JPATCH),0.0) * XPATCH(:,JPATCH)/ZSUMPATCH(:) 
   END WHERE
END DO
!
!kg/m²
IF(LGLACIER)THEN
  DO JPATCH=1,SIZE(XPATCH,2)
     WHERE (ZSUMPATCH(:) > 0.)
           XCPL_ICEFLUX(:) = XCPL_ICEFLUX(:) + PTSTEP * MAX(PCPL_ICEFLUX(:,JPATCH),0.0) * XPATCH(:,JPATCH)/ZSUMPATCH(:)
     END WHERE
  END DO
ENDIF
!
IF(LFLOOD)THEN
!        
!* update the Floodplains diagnostics for the coupling with TRIP !
!  ---------------------------------------------------------------
!
! kg/m²
  DO JPATCH=1,SIZE(XPATCH,2)
     WHERE (ZSUMPATCH(:) > 0.)
           XCPL_EFLOOD  (:)  = XCPL_EFLOOD  (:) + PTSTEP * PCPL_EFLOOD  (:,JPATCH)*XPATCH(:,JPATCH)/ZSUMPATCH(:)
           XCPL_PFLOOD  (:)  = XCPL_PFLOOD  (:) + PTSTEP * PCPL_PFLOOD  (:,JPATCH)*XPATCH(:,JPATCH)/ZSUMPATCH(:)
           XCPL_IFLOOD  (:)  = XCPL_IFLOOD  (:) + PTSTEP * PCPL_IFLOOD  (:,JPATCH)*XPATCH(:,JPATCH)/ZSUMPATCH(:)
     END WHERE
  END DO
!
  ZBUDGET(:)=XPIFLOOD*XTSTEP_COUPLING+(XCPL_PFLOOD(:)-XCPL_IFLOOD(:)-XCPL_EFLOOD(:))
  WHERE(ZBUDGET(:)<=0.0)
        XPIFLOOD(:)=0.0
        XFFLOOD (:)=0.0
  ENDWHERE
!
ENDIF
!
IF (LHOOK) CALL DR_HOOK('DIAG_CPL_ESM_ISBA',1,ZHOOK_HANDLE)
!
END SUBROUTINE DIAG_CPL_ESM_ISBA
