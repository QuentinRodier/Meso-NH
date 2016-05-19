!     #########
      SUBROUTINE FLAG_GR_SNOW(KFLAG,OMASK,TPSNOW)  
!     ##########################################################
!
!!****  *FLAG_GR_SNOW* - routine to flag snow surface fields
!!
!!    PURPOSE
!!    -------
!       Initialize snow surface fields.
!
!!**  METHOD
!!    ------
!!    
!!    
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
!!
!!    AUTHOR
!!    ------
!!	V. Masson       * Meteo France *
!!
!!    MODIFICATIONS
!!    -------------
!!      Original       10/2011
!-----------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!
USE MODD_TYPE_SNOW
!
!
USE MODD_SURF_PAR, ONLY : XUNDEF
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*       0.1   declarations of arguments
!
INTEGER, INTENT(IN) :: KFLAG ! 1 : to put physical values to run ISBA afterwards
!                            ! 2 : to flag with XUNDEF value for points wihtout
LOGICAL, DIMENSION(:), INTENT(IN) :: OMASK ! T: points where snow values 
!                                          !    must be flagged
TYPE(SURF_SNOW), INTENT(INOUT) :: TPSNOW   ! snow characteristics
!
!*       0.2   declarations of local variables
!
REAL            :: ZVAL
INTEGER         :: JLAYER, JPATCH
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('FLAG_GR_SNOW',0,ZHOOK_HANDLE)
!
IF (KFLAG==1) THEN
  ZVAL = 0.
ELSEIF (KFLAG==2) THEN 
  ZVAL = XUNDEF
ENDIF
!
DO JPATCH = 1,SIZE(TPSNOW%WSNOW,3)
  !
  DO JLAYER = 1,TPSNOW%NLAYER
    !
    WHERE(OMASK(:)) TPSNOW%WSNOW(:,JLAYER,JPATCH) = ZVAL
    !
    IF (KFLAG==1) THEN 
      !
      WHERE(OMASK(:)) TPSNOW%RHO  (:,JLAYER,JPATCH) = XUNDEF
      !
      IF (SIZE(TPSNOW%TEMP ) >0) THEN
        WHERE(OMASK(:)) 
          TPSNOW%TEMP (:,JLAYER,JPATCH) = XUNDEF
          TPSNOW%HEAT (:,JLAYER,JPATCH) = XUNDEF
        END WHERE
      ENDIF
      !
      IF (SIZE(TPSNOW%T    ) >0) WHERE(OMASK(:)) TPSNOW%T(:,JLAYER,JPATCH) = XUNDEF
      !
      IF (SIZE(TPSNOW%GRAN1) >0) THEN
        WHERE(OMASK(:)) 
          TPSNOW%GRAN1(:,JLAYER,JPATCH) = XUNDEF
          TPSNOW%GRAN2(:,JLAYER,JPATCH) = XUNDEF
          TPSNOW%HIST (:,JLAYER,JPATCH) = XUNDEF
          TPSNOW%AGE  (:,JLAYER,JPATCH) = XUNDEF
        END WHERE
      END IF
      !
    ENDIF
    !
  ENDDO
  !
  IF (KFLAG==1) THEN
    !
    WHERE(OMASK(:)) TPSNOW%ALB  (:,JPATCH) = XUNDEF
    !
    IF (SIZE(TPSNOW%EMIS ) >0) THEN
      WHERE(OMASK(:))
        TPSNOW%EMIS (:,JPATCH) = XUNDEF
        TPSNOW%TS   (:,JPATCH) = XUNDEF
      END WHERE
    END IF
    !
  ENDIF
  !
END DO
!
IF (LHOOK) CALL DR_HOOK('FLAG_GR_SNOW',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE FLAG_GR_SNOW
