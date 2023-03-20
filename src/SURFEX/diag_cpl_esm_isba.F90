!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE DIAG_CPL_ESM_ISBA (IG, IO, S, NK, NP, PTSTEP, PCPL_DRAIN, PCPL_RUNOFF,&
                                    PCPL_EFLOOD,PCPL_PFLOOD,PCPL_IFLOOD,PCPL_ICEFLUX,  &
                                    PCPL_DOCFLUX)  
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
!!      B. Decharme           * Meteo-France *
!!
!!    MODIFICATIONS
!!    -------------
!!
!!      B. Decharme    01/16 : Bug with flood budget and add cpl keys
!!      B. Decharme   10/2016  bug surface/groundwater coupling
!!      R. Séférian    08/16 : introduce cpl field for riverine carbon cycle coupling
!-------------------------------------------------------------------------------
!
!*       0.     DECLARATIONS
!               ------------
!
USE MODD_SFX_GRID_n,     ONLY : GRID_t
USE MODD_ISBA_OPTIONS_n, ONLY : ISBA_OPTIONS_t
USE MODD_ISBA_n,         ONLY : ISBA_S_t, ISBA_K_t, ISBA_P_t, ISBA_NK_t, ISBA_NP_t
!
USE MODN_SFX_OASIS,  ONLY : XTSTEP_CPL_LAND
!
USE MODD_SFX_OASIS,  ONLY : XRUNTIME, LCPL_FLOOD, LCPL_GW, LCPL_RIVCARB
!
USE MODD_SURF_PAR,   ONLY : XUNDEF
!
USE MODI_ABOR1_SFX
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
TYPE(GRID_t),         INTENT(INOUT) :: IG
TYPE(ISBA_OPTIONS_t), INTENT(INOUT) :: IO
TYPE(ISBA_S_t),       INTENT(INOUT) :: S
TYPE(ISBA_NK_t),      INTENT(INOUT) :: NK
TYPE(ISBA_NP_t),      INTENT(INOUT) :: NP
!
REAL, INTENT(IN)                   :: PTSTEP
REAL, DIMENSION(:,:), INTENT(IN)   :: PCPL_DRAIN
REAL, DIMENSION(:,:), INTENT(IN)   :: PCPL_RUNOFF
REAL, DIMENSION(:,:), INTENT(IN)   :: PCPL_EFLOOD
REAL, DIMENSION(:,:), INTENT(IN)   :: PCPL_PFLOOD
REAL, DIMENSION(:,:), INTENT(IN)   :: PCPL_IFLOOD
REAL, DIMENSION(:,:), INTENT(IN)   :: PCPL_ICEFLUX
!
REAL, DIMENSION(:,:), INTENT(IN)   :: PCPL_DOCFLUX
!
!*      0.2    declarations of local variables
!
TYPE(ISBA_K_t), POINTER :: KK
TYPE(ISBA_P_t), POINTER :: PK
!
REAL, DIMENSION(SIZE(PCPL_DRAIN,1),SIZE(PCPL_DRAIN,2)) :: ZCPL_DRAIN
!
REAL, DIMENSION(SIZE(S%XPATCH,1)) :: ZSUMPATCH
REAL, DIMENSION(SIZE(S%XPATCH,1)) :: ZBUDGET
!
INTEGER :: INI, IMASK
INTEGER :: JI, JP ! tile loop counter
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('DIAG_CPL_ESM_ISBA',0,ZHOOK_HANDLE)
!-------------------------------------------------------------------------------
!
!* Initialization
!  --------------
!
INI=SIZE(S%XPATCH,1)
!
ZSUMPATCH(:) = 0.0
DO JP=1,IO%NPATCH
  DO JI=1,INI
     ZSUMPATCH(JI) = ZSUMPATCH(JI) + S%XPATCH(JI,JP)
  ENDDO
ENDDO
!
! * Compute Drainage
!   ----------------
!
IF(IO%CISBA/='DIF')THEN
! prevent small negatives values with ISBA-FR
  ZCPL_DRAIN(:,:)=MAX(0.0,PCPL_DRAIN(:,:))
ELSE
  ZCPL_DRAIN(:,:)=PCPL_DRAIN(:,:)
ENDIF
!
!* update ISBA - RRM coupling variable (kg/m2)
!  -------------------------------------------
!
DO JP=1,IO%NPATCH
  DO JI=1,INI
!
     IF(ZSUMPATCH(JI)>0.0)THEN
       S%XCPL_DRAIN (JI) = S%XCPL_DRAIN (JI) + PTSTEP * ZCPL_DRAIN (JI,JP) * S%XPATCH(JI,JP)/ZSUMPATCH(JI) 
       S%XCPL_RUNOFF(JI) = S%XCPL_RUNOFF(JI) + PTSTEP * PCPL_RUNOFF(JI,JP) * S%XPATCH(JI,JP)/ZSUMPATCH(JI)
       IF (S%XCPL_DRAIN (JI)>1.0E+10 .AND. S%XPATCH(JI,JP)>0.0)  THEN
         WRITE(*,*) '!!! UNREALISTIC DRAIN !!!', ZCPL_DRAIN(JI,JP)
         WRITE(*,*) 'at  lon-lat = ', IG%XLON(JI),IG%XLAT(JI) 
         WRITE(*,*) 'at point-patch', JI, JP 
         WRITE(*,*) 'patch fraction:', S%XPATCH(JI,JP), ZSUMPATCH(JI)
         CALL ABOR1_SFX('UNREALISTIC DRAIN: probably land-use issue')
       ENDIF
     ENDIF
!
!    Cumulated Calving (kg/m2)
     IF(IO%LGLACIER.AND.ZSUMPATCH(JI)>0.0)THEN
        S%XCPL_ICEFLUX(JI) = S%XCPL_ICEFLUX(JI) + PTSTEP * PCPL_ICEFLUX(JI,JP) * S%XPATCH(JI,JP)/ZSUMPATCH(JI)
     ENDIF
!
!    Cumulated disolved organic carbon flux (kgC/m2)
     IF(LCPL_FLOOD.AND.IO%LFLOOD.AND.ZSUMPATCH(JI)>0.0)THEN
        S%XCPL_EFLOOD  (JI) = S%XCPL_EFLOOD  (JI) + PTSTEP * PCPL_EFLOOD  (JI,JP)*S%XPATCH(JI,JP)/ZSUMPATCH(JI)
        S%XCPL_PFLOOD  (JI) = S%XCPL_PFLOOD  (JI) + PTSTEP * PCPL_PFLOOD  (JI,JP)*S%XPATCH(JI,JP)/ZSUMPATCH(JI)
        S%XCPL_IFLOOD  (JI) = S%XCPL_IFLOOD  (JI) + PTSTEP * PCPL_IFLOOD  (JI,JP)*S%XPATCH(JI,JP)/ZSUMPATCH(JI)
     ENDIF
!
!    Cumulated disolved organic carbon flux (kgC/m2)
     IF(LCPL_RIVCARB.AND.IO%LCLEACH.AND.ZSUMPATCH(JI)>0.0)THEN
        S%XCPL_DOCFLUX(JI) = S%XCPL_DOCFLUX(JI) + PTSTEP * PCPL_DOCFLUX(JI,JP) * S%XPATCH(JI,JP)/ZSUMPATCH(JI)
     ENDIF

  ENDDO
ENDDO
!
! * update ISBA Floodplains variable for mass conservation (kg/m2)
!   --------------------------------------------------------------
!
IF(LCPL_FLOOD.AND.IO%LFLOOD)THEN
  ZBUDGET(:) = 0.0
  DO JP = 1,IO%NPATCH
    KK => NK%AL(JP)
    PK => NP%AL(JP)

    DO JI = 1,PK%NSIZE_P
      IMASK = PK%NR_P(JI)
      ZBUDGET(IMASK) = ZBUDGET(IMASK) + (KK%XPIFLOOD(JI)*XTSTEP_CPL_LAND) + &
                       (S%XCPL_PFLOOD(IMASK)-S%XCPL_IFLOOD(IMASK)-S%XCPL_EFLOOD(IMASK))
    ENDDO
  ENDDO

  DO JP = 1,IO%NPATCH
    KK => NK%AL(JP)
    PK => NP%AL(JP)
  
    DO JI = 1,PK%NSIZE_P
      IMASK = PK%NR_P(JI)
      IF (ZBUDGET(IMASK)<=0.) THEN
        KK%XPIFLOOD(JI) = 0.0
        KK%XFFLOOD (JI) = 0.0
      ENDIF
    ENDDO
  ENDDO
ENDIF
!
!
! * update drainage and leaching for landuse mass and carbon conservation (kg/m2)
!   -----------------------------------------------------------------------------
!
IF(IO%LLULCC)THEN
  S%XCPL_DRAIN(:) = S%XCPL_DRAIN (:) + PTSTEP * S%XWCONSRV(:) / XRUNTIME
ENDIF
!
IF(IO%LLULCC.AND.LCPL_RIVCARB.AND.IO%LCLEACH)THEN
  S%XCPL_DOCFLUX(:) = S%XCPL_DOCFLUX(:) + PTSTEP * S%XCCONSRV(:) / XRUNTIME
ENDIF
!
!-------------------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('DIAG_CPL_ESM_ISBA',1,ZHOOK_HANDLE)
!-------------------------------------------------------------------------------
!
END SUBROUTINE DIAG_CPL_ESM_ISBA
