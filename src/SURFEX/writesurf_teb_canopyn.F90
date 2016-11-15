!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE WRITESURF_TEB_CANOPY_n (DGU, U, &
                                          TCP, TOP, &
                                         HPROGRAM,HWRITE)
!     ####################################
!
!!****  *WRITE_TEB_n* - writes TEB fields
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
!!      V. Masson   *Meteo France*
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    01/2003 
!!      E. Martin   01/2012 avoid write of XUNDEF fields
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
!
!
!
!
!
USE MODD_DIAG_SURF_ATM_n, ONLY : DIAG_SURF_ATM_t
USE MODD_SURF_ATM_n, ONLY : SURF_ATM_t
!
USE MODD_TEB_CANOPY_n, ONLY : TEB_CANOPY_t
USE MODD_TEB_OPTION_n, ONLY : TEB_OPTIONS_t
!
USE MODD_SURF_PAR       ,ONLY : XUNDEF
!
USE MODI_WRITE_SURF
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*       0.1   Declarations of arguments
!              -------------------------
!
!
!
TYPE(DIAG_SURF_ATM_t), INTENT(INOUT) :: DGU
TYPE(SURF_ATM_t), INTENT(INOUT) :: U
!
TYPE(TEB_CANOPY_t), INTENT(INOUT) :: TCP
TYPE(TEB_OPTIONS_t), INTENT(INOUT) :: TOP
!
 CHARACTER(LEN=6),  INTENT(IN)  :: HPROGRAM ! program calling
 CHARACTER(LEN=3),  INTENT(IN)  :: HWRITE   ! 'PREP' : does not write SBL XUNDEF fields
!                                          ! 'ALL' : all fields are written
!*       0.2   Declarations of local variables
!              -------------------------------
!
INTEGER           :: IRESP          ! IRESP  : return-code if a problem appears
 CHARACTER(LEN=LEN_HREC) :: YRECFM         ! Name of the article to be read
 CHARACTER(LEN=100):: YCOMMENT       ! Comment string
!
INTEGER :: JLAYER  ! loop counter on layers
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
!
!*       1.     Prognostic fields:
!               -----------------
!
!* flag to define if canopy is computed
!
IF (LHOOK) CALL DR_HOOK('WRITESURF_TEB_CANOPY_N',0,ZHOOK_HANDLE)
YRECFM='TEB_CANOPY'
YCOMMENT='flag to use canopy levels'
 CALL WRITE_SURF(DGU, U, &
                 HPROGRAM,YRECFM,TOP%LCANOPY,IRESP,HCOMMENT=YCOMMENT)
!
IF (.NOT. TOP%LCANOPY .AND. LHOOK) CALL DR_HOOK('WRITESURF_TEB_CANOPY_N',1,ZHOOK_HANDLE)
IF (.NOT. TOP%LCANOPY) RETURN
!
!* number of levels
!
YRECFM='TEB_CAN_LVL'
YCOMMENT='number of canopy levels'
 CALL WRITE_SURF(DGU, U, &
                 HPROGRAM,YRECFM,TCP%NLVL,IRESP,HCOMMENT=YCOMMENT)
!
!* altitudes
!
DO JLAYER=1,TCP%NLVL
  WRITE(YRECFM,'(A9,I2.2,A1)') 'TEB_CAN_Z',JLAYER,' '
  YCOMMENT='altitudes of canopy levels (m)'
  CALL WRITE_SURF(DGU, U, &
                 HPROGRAM,YRECFM,TCP%XZ(:,JLAYER),IRESP,HCOMMENT=YCOMMENT)
END DO
!
IF (HWRITE/='PRE') THEN
  !
  !* wind in canopy
  !
  DO JLAYER=1,TCP%NLVL
    WRITE(YRECFM,'(A9,I2.2,A1)') 'TEB_CAN_U',JLAYER,' '
    YCOMMENT='wind at canopy levels (m/s)'
    CALL WRITE_SURF(DGU, U, &
                 HPROGRAM,YRECFM,TCP%XU(:,JLAYER),IRESP,HCOMMENT=YCOMMENT)
  END DO
  !
  !* temperature in canopy
  !
  DO JLAYER=1,TCP%NLVL
    WRITE(YRECFM,'(A9,I2.2,A1)') 'TEB_CAN_T',JLAYER,' '
    YCOMMENT='temperature at canopy levels (K)'
    CALL WRITE_SURF(DGU, U, &
                 HPROGRAM,YRECFM,TCP%XT(:,JLAYER),IRESP,HCOMMENT=YCOMMENT)
  END DO
  !
  !* humidity in canopy
  !
  DO JLAYER=1,TCP%NLVL
    WRITE(YRECFM,'(A9,I2.2,A1)') 'TEB_CAN_Q',JLAYER,' '
    YCOMMENT='humidity at canopy levels (kg/m3)'
    CALL WRITE_SURF(DGU, U, &
                 HPROGRAM,YRECFM,TCP%XQ(:,JLAYER),IRESP,HCOMMENT=YCOMMENT)
  END DO
  !
  !* Tke in canopy
  !
  DO JLAYER=1,TCP%NLVL
    WRITE(YRECFM,'(A9,I2.2,A1)') 'TEB_CAN_E',JLAYER,' '
    YCOMMENT='Tke at canopy levels (m2/s2)'
    CALL WRITE_SURF(DGU, U, &
                 HPROGRAM,YRECFM,TCP%XTKE(:,JLAYER),IRESP,HCOMMENT=YCOMMENT)
  END DO
  !
  !* Monin-Obhukov length
  !
  DO JLAYER=1,TCP%NLVL
    WRITE(YRECFM,'(A10,I2.2)') 'TEB_CAN_MO',JLAYER
    YCOMMENT='Monin-Obukhov length (m)'
    CALL WRITE_SURF(DGU, U, &
                 HPROGRAM,YRECFM,TCP%XLMO(:,JLAYER),IRESP,HCOMMENT=YCOMMENT)
  END DO
  !
  !* mixing length
  !
  IF (ASSOCIATED(TCP%XLM)) THEN
    DO JLAYER=1,TCP%NLVL
      WRITE(YRECFM,'(A10,I2.2)') 'TEB_CAN_LM',JLAYER
      YCOMMENT='mixing length (m)'
      CALL WRITE_SURF(DGU, U, &
                 HPROGRAM,YRECFM,TCP%XLM(:,JLAYER),IRESP,HCOMMENT=YCOMMENT)
   END DO
  END IF
  !
  !* dissipative length
  !
  IF (ASSOCIATED(TCP%XLEPS)) THEN
    DO JLAYER=1,TCP%NLVL
      WRITE(YRECFM,'(A10,I2.2)') 'TEB_CAN_LE',JLAYER
      YCOMMENT='mixing length (m)'
      CALL WRITE_SURF(DGU, U, &
                 HPROGRAM,YRECFM,TCP%XLEPS(:,JLAYER),IRESP,HCOMMENT=YCOMMENT)
    END DO
  END IF
  !
  !* Air pressure in canopy
  !
  DO JLAYER=1,TCP%NLVL
    WRITE(YRECFM,'(A9,I2.2,A1)') 'TEB_CAN_P',JLAYER,' '
    YCOMMENT='Pressure at canopy levels (Pa)'
    CALL WRITE_SURF(DGU, U, &
                 HPROGRAM,YRECFM,TCP%XP(:,JLAYER),IRESP,HCOMMENT=YCOMMENT)
  END DO
  !
ENDIF
!
IF (LHOOK) CALL DR_HOOK('WRITESURF_TEB_CANOPY_N',1,ZHOOK_HANDLE)
!-------------------------------------------------------------------------------
!
END SUBROUTINE WRITESURF_TEB_CANOPY_n
