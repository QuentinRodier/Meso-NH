!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
SUBROUTINE UNPACK_DIAG_CPL_ESM_ISBA(IO, DEK, PK, KMASK, KSIZE, KNPATCH, KPATCH, &
                                    PCPL_DRAIN, PCPL_RUNOFF, PCPL_EFLOOD,       &
                                    PCPL_PFLOOD, PCPL_IFLOOD, PCPL_ICEFLUX,     &
                                    PCPL_DOCFLUX                                )  
!##############################################
!
!!****  *UNPACK_DIAG_CPL_ESM_ISBA* - unpacks diagnostics for coupling with OASIS
!!
!!    PURPOSE
!!    -------
!
!!**  METHOD
!!    ------
!!
!!    REFERENCE
!!    ---------
!!      
!!
!!    AUTHOR
!!    ------
!!     B. Decharme
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    01/2022
!!
!!------------------------------------------------------------------
!
USE MODD_ISBA_OPTIONS_n, ONLY : ISBA_OPTIONS_t
USE MODD_DIAG_EVAP_ISBA_n, ONLY : DIAG_EVAP_ISBA_t
USE MODD_ISBA_n, ONLY : ISBA_P_t
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
TYPE(ISBA_OPTIONS_t), INTENT(INOUT) :: IO
TYPE(DIAG_EVAP_ISBA_t), INTENT(INOUT) :: DEK
TYPE(ISBA_P_t), INTENT(INOUT) :: PK
!
INTEGER, INTENT(IN)                :: KSIZE, KPATCH, KNPATCH
INTEGER, DIMENSION(:), INTENT(IN)  :: KMASK
!
!Coupling variable
REAL, DIMENSION(:,:),  INTENT(OUT) :: PCPL_DRAIN
REAL, DIMENSION(:,:),  INTENT(OUT) :: PCPL_RUNOFF
REAL, DIMENSION(:,:),  INTENT(OUT) :: PCPL_EFLOOD
REAL, DIMENSION(:,:),  INTENT(OUT) :: PCPL_PFLOOD
REAL, DIMENSION(:,:),  INTENT(OUT) :: PCPL_IFLOOD
REAL, DIMENSION(:,:),  INTENT(OUT) :: PCPL_ICEFLUX
REAL, DIMENSION(:,:),  INTENT(OUT) :: PCPL_DOCFLUX
!
INTEGER :: JI, IMASK, JSW
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!------------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('UNPACK_DIAG_CPL_ESM_ISBA',0,ZHOOK_HANDLE)
!
!
IF (KNPATCH==1) THEN
  !
  IF (IO%LCPL_RRM) THEN
    PCPL_DRAIN (:,KPATCH) = DEK%XDRAIN (:)
    PCPL_RUNOFF(:,KPATCH) = DEK%XRUNOFF(:)
  END IF
  !
  IF (IO%LFLOOD) THEN
    PCPL_EFLOOD(:,KPATCH) = DEK%XLE_FLOOD(:) / PK%XLVTT(:) + DEK%XLEI_FLOOD(:) / PK%XLSTT(:)
    PCPL_PFLOOD(:,KPATCH) = DEK%XPFLOOD(:)
    PCPL_IFLOOD(:,KPATCH) = DEK%XIFLOOD(:)
  END IF
  !
  IF(IO%LCPL_RRM.AND.IO%LGLACIER)THEN
    PCPL_ICEFLUX(:,KPATCH) = DEK%XICEFLUX(:)
  ENDIF
  !
  IF(IO%LCPL_RRM.AND.IO%LCLEACH)THEN
    PCPL_DOCFLUX(:, KPATCH)= DEK%XFDOC(:)
  ENDIF
  !
ELSE
  !
  IF (IO%LCPL_RRM) THEN
    DO JI=1,KSIZE
      IMASK = KMASK(JI)
      PCPL_DRAIN (IMASK,KPATCH) = DEK%XDRAIN (JI)
      PCPL_RUNOFF(IMASK,KPATCH) = DEK%XRUNOFF (JI)
    END DO
  END IF
  !
  IF (IO%LFLOOD) THEN
    DO JI=1,KSIZE
      IMASK = KMASK(JI)
      PCPL_EFLOOD(IMASK,KPATCH) = DEK%XLE_FLOOD(JI) / PK%XLVTT(JI) + DEK%XLEI_FLOOD(JI) / PK%XLSTT(JI)
      PCPL_PFLOOD(IMASK,KPATCH) = DEK%XPFLOOD(JI)
      PCPL_IFLOOD(IMASK,KPATCH) = DEK%XIFLOOD(JI)
    END DO
  END IF
  !
  IF(IO%LCPL_RRM.AND.IO%LGLACIER)THEN
    DO JI=1,KSIZE
      IMASK  = KMASK(JI)
      PCPL_ICEFLUX(IMASK,KPATCH) = DEK%XICEFLUX(JI)
    ENDDO
  ENDIF
  !
  IF(IO%LCPL_RRM.AND.IO%LCLEACH)THEN
    DO JI=1,KSIZE
      IMASK  = KMASK(JI)
      PCPL_DOCFLUX(IMASK,KPATCH) = DEK%XFDOC(JI)
    END DO          
  ENDIF
  !
ENDIF
!
!------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('UNPACK_DIAG_CPL_ESM_ISBA',1,ZHOOK_HANDLE)
!------------------------------------------------------------------------
!
END SUBROUTINE UNPACK_DIAG_CPL_ESM_ISBA
