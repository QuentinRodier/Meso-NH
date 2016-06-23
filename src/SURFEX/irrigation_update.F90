!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE IRRIGATION_UPDATE (AG, &
                                    PIRRIG, PTSTEP, KMONTH, KDAY,   &
       PTIME,TSEEDMONTH,TSEEDDAY,TREAPMONTH,TREAPDAY) 
!     ####################################################################
!
!!****  *IRRIGATION_UPDATE* - routine to update irrigation fields
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
!!      P. Le Moigne  *Meteo France*
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    06/2006
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
!
USE MODD_AGRI_n, ONLY : AGRI_t
!
USE MODD_AGRI,   ONLY   : JPSTAGE, XTHRESHOLD
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
TYPE(AGRI_t), INTENT(INOUT) :: AG
!
INTEGER, DIMENSION(:,:), INTENT(IN) :: TSEEDMONTH
INTEGER, DIMENSION(:,:), INTENT(IN) :: TSEEDDAY
INTEGER, DIMENSION(:,:), INTENT(IN) :: TREAPMONTH
INTEGER, DIMENSION(:,:), INTENT(IN) :: TREAPDAY
REAL   , DIMENSION(:,:), INTENT(IN) :: PIRRIG
REAL,    INTENT(IN)  :: PTSTEP, PTIME
INTEGER, INTENT(IN)  :: KMONTH, KDAY
INTEGER              :: IL, JL                        
LOGICAL              :: GMASK
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!*       0.1   Declarations of arguments
!-------------------------------------------------------------------------------
!
! Mask to realize update only once a day
!
IF (LHOOK) CALL DR_HOOK('MODI_IRRIGATION_UPDATE:IRRIGATION_UPDATE',0,ZHOOK_HANDLE)
GMASK = ( PTIME - PTSTEP < 0. ) .AND. ( PTIME >= 0. )
!
IF (GMASK) THEN

   WHERE( (PIRRIG(:,:).GT.0.).AND.(AG%LIRRIDAY(:,:)) .AND.(AG%NIRRINUM(:,:).LT.JPSTAGE))
      AG%NIRRINUM (:,:) = AG%NIRRINUM(:,:) + 1
      AG%LIRRIDAY (:,:) = .FALSE.
   ENDWHERE
!   
   DO IL=1,SIZE(PIRRIG,1)
       DO JL=1,SIZE(PIRRIG,2)
           AG%XTHRESHOLDSPT(IL,JL)=XTHRESHOLD(AG%NIRRINUM(IL,JL))
       ENDDO
   ENDDO
!
END IF
!
! Reinitialization of irrigation stage (necessary for runs from August to August)
!
IF((KMONTH==1).AND.(KDAY==1)) THEN
   AG%NIRRINUM(:,:) = 1
ENDIF
!
AG%LIRRIGATE(:,:) = .FALSE.
DO IL=1,SIZE(PIRRIG,1)
   DO JL=1,SIZE(PIRRIG,2)
      !
      ! Activate irrigation after seeding date
      !
      IF (KMONTH == TSEEDMONTH(IL,JL) .AND. KDAY .GE. TSEEDDAY(IL,JL)) THEN
         AG%LIRRIGATE(IL,JL) = .TRUE.
      END IF
      IF (KMONTH > TSEEDMONTH(IL,JL)) THEN
         AG%LIRRIGATE(IL,JL) = .TRUE.
      END IF
      !
      ! Stop irrigation after reaping date
      !
      IF (KMONTH == TREAPMONTH(IL,JL) .AND. KDAY .GT. TREAPDAY(IL,JL)) THEN
         AG%LIRRIGATE(IL,JL) = .FALSE.
      END IF
      IF (KMONTH > TREAPMONTH(IL,JL)) THEN
         AG%LIRRIGATE(IL,JL) = .FALSE.
      END IF
   ENDDO
ENDDO
IF (LHOOK) CALL DR_HOOK('MODI_IRRIGATION_UPDATE:IRRIGATION_UPDATE',1,ZHOOK_HANDLE)
!
END SUBROUTINE IRRIGATION_UPDATE
