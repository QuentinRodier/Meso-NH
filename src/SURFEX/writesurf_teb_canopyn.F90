!SURFEX_LIC Copyright 1994-2014 Meteo-France 
!SURFEX_LIC This is part of the SURFEX software governed by the CeCILL-C  licence
!SURFEX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!SURFEX_LIC for details. version 1.
!     #########
      SUBROUTINE WRITESURF_TEB_CANOPY_n(HPROGRAM,HWRITE)
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
!!	V. Masson   *Meteo France*	
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
USE MODD_TEB_n,          ONLY : LCANOPY
USE MODD_TEB_CANOPY_n,   ONLY : NLVL, XZ, XU, XT, XQ, XTKE, XLMO, XLM, XLEPS, XP
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
 CHARACTER(LEN=6),  INTENT(IN)  :: HPROGRAM ! program calling
 CHARACTER(LEN=3),  INTENT(IN)  :: HWRITE   ! 'PREP' : does not write SBL XUNDEF fields
!                                          ! 'ALL' : all fields are written
!*       0.2   Declarations of local variables
!              -------------------------------
!
INTEGER           :: IRESP          ! IRESP  : return-code if a problem appears
 CHARACTER(LEN=12) :: YRECFM         ! Name of the article to be read
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
 CALL WRITE_SURF(HPROGRAM,YRECFM,LCANOPY,IRESP,HCOMMENT=YCOMMENT)
!
IF (.NOT. LCANOPY .AND. LHOOK) CALL DR_HOOK('WRITESURF_TEB_CANOPY_N',1,ZHOOK_HANDLE)
IF (.NOT. LCANOPY) RETURN
!
!* number of levels
!
YRECFM='TEB_CAN_LVL'
YCOMMENT='number of canopy levels'
 CALL WRITE_SURF(HPROGRAM,YRECFM,NLVL,IRESP,HCOMMENT=YCOMMENT)
!
!* altitudes
!
DO JLAYER=1,NLVL
  WRITE(YRECFM,'(A9,I2.2,A1)') 'TEB_CAN_Z',JLAYER,' '
  YCOMMENT='altitudes of canopy levels (m)'
  CALL WRITE_SURF(HPROGRAM,YRECFM,XZ(:,JLAYER),IRESP,HCOMMENT=YCOMMENT)
END DO
!
IF (HWRITE/='PRE') THEN
  !
  !* wind in canopy
  !
  DO JLAYER=1,NLVL
    WRITE(YRECFM,'(A9,I2.2,A1)') 'TEB_CAN_U',JLAYER,' '
    YCOMMENT='wind at canopy levels (m/s)'
    CALL WRITE_SURF(HPROGRAM,YRECFM,XU(:,JLAYER),IRESP,HCOMMENT=YCOMMENT)
  END DO
  !
  !* temperature in canopy
  !
  DO JLAYER=1,NLVL
    WRITE(YRECFM,'(A9,I2.2,A1)') 'TEB_CAN_T',JLAYER,' '
    YCOMMENT='temperature at canopy levels (K)'
    CALL WRITE_SURF(HPROGRAM,YRECFM,XT(:,JLAYER),IRESP,HCOMMENT=YCOMMENT)
  END DO
  !
  !* humidity in canopy
  !
  DO JLAYER=1,NLVL
    WRITE(YRECFM,'(A9,I2.2,A1)') 'TEB_CAN_Q',JLAYER,' '
    YCOMMENT='humidity at canopy levels (kg/m3)'
    CALL WRITE_SURF(HPROGRAM,YRECFM,XQ(:,JLAYER),IRESP,HCOMMENT=YCOMMENT)
  END DO
  !
  !* Tke in canopy
  !
  DO JLAYER=1,NLVL
    WRITE(YRECFM,'(A9,I2.2,A1)') 'TEB_CAN_E',JLAYER,' '
    YCOMMENT='Tke at canopy levels (m2/s2)'
    CALL WRITE_SURF(HPROGRAM,YRECFM,XTKE(:,JLAYER),IRESP,HCOMMENT=YCOMMENT)
  END DO
  !
  !* Monin-Obhukov length
  !
  DO JLAYER=1,NLVL
    WRITE(YRECFM,'(A10,I2.2)') 'TEB_CAN_MO',JLAYER
    YCOMMENT='Monin-Obukhov length (m)'
    CALL WRITE_SURF(HPROGRAM,YRECFM,XLMO(:,JLAYER),IRESP,HCOMMENT=YCOMMENT)
  END DO
  !
  !* mixing length
  !
  IF (ASSOCIATED(XLM)) THEN
    DO JLAYER=1,NLVL
      WRITE(YRECFM,'(A10,I2.2)') 'TEB_CAN_LM',JLAYER
      YCOMMENT='mixing length (m)'
      CALL WRITE_SURF(HPROGRAM,YRECFM,XLM(:,JLAYER),IRESP,HCOMMENT=YCOMMENT)
   END DO
  END IF
  !
  !* dissipative length
  !
  IF (ASSOCIATED(XLEPS)) THEN
    DO JLAYER=1,NLVL
      WRITE(YRECFM,'(A10,I2.2)') 'TEB_CAN_LE',JLAYER
      YCOMMENT='mixing length (m)'
      CALL WRITE_SURF(HPROGRAM,YRECFM,XLEPS(:,JLAYER),IRESP,HCOMMENT=YCOMMENT)
    END DO
  END IF
  !
  !* Air pressure in canopy
  !
  DO JLAYER=1,NLVL
    WRITE(YRECFM,'(A9,I2.2,A1)') 'TEB_CAN_P',JLAYER,' '
    YCOMMENT='Pressure at canopy levels (Pa)'
    CALL WRITE_SURF(HPROGRAM,YRECFM,XP(:,JLAYER),IRESP,HCOMMENT=YCOMMENT)
  END DO
  !
ENDIF
!
IF (LHOOK) CALL DR_HOOK('WRITESURF_TEB_CANOPY_N',1,ZHOOK_HANDLE)
!-------------------------------------------------------------------------------
!
END SUBROUTINE WRITESURF_TEB_CANOPY_n
