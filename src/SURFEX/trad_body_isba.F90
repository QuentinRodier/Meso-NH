!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
MODULE MODI_TRAD_BODY_ISBA
INTERFACE
FUNCTION TRAD_BODY_ISBA(HPROGRAM, PSCA_SW, PREF_SW, PEMIT_LW, PLW_RAD, &
     PDIR_SW, PZENITH) RESULT(PTRAD_BODY_ISBA)
!
CHARACTER(LEN=6),    INTENT(IN)  :: HPROGRAM  ! program calling surf. schemes
!
REAL, DIMENSION(:), INTENT(IN) :: PSCA_SW  ! Diffuse solar radiation (W/m2)
REAL, DIMENSION(:), INTENT(IN) :: PREF_SW  ! Solar radiation reflected by ground (W/m2)x
REAL, DIMENSION(:), INTENT(IN) :: PEMIT_LW ! Longwave radiation emitted by the ground (W/m2)
REAL, DIMENSION(:), INTENT(IN) :: PLW_RAD  ! Atmospheric longwave radiation (W/m2)
REAL, DIMENSION(:), INTENT(IN) :: PDIR_SW !Direct solar radiation (W/m2)
REAL, DIMENSION(:), INTENT(IN) :: PZENITH !solar zenithal angle (rad from vert.)
REAL, DIMENSION(SIZE(PSCA_SW)) :: PTRAD_BODY_ISBA
END FUNCTION TRAD_BODY_ISBA
END INTERFACE
END MODULE MODI_TRAD_BODY_ISBA
!   ##########################################################################
FUNCTION TRAD_BODY_ISBA(HPROGRAM, PSCA_SW, PREF_SW, PEMIT_LW, PLW_RAD,&
                        PDIR_SW, PZENITH) RESULT(PTRAD_BODY_ISBA)
!   ##########################################################################
!
!!****  *TRAD_BODY_ISBA
!!
!!    PURPOSE
!!    -------
!
!     Computes the radiant temperature equivalent to the total radiation
!     received by the human body
!     
!!**  METHOD
!     ------
!
!!    EXTERNAL
!!    --------
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!! a supplement
!!    MODD_CST
!!
!!    REFERENCE
!!    ---------
!!   www.utci.org
!!      
!!    AUTHOR
!!    ------
!!
!!      R. Schoetter           * Meteo-France *
!!
!!    MODIFICATIONS
!!    -------------
!!      Original  03/2017
!-------------------------------------------------------------------------------
!
!*       0.     DECLARATIONS
!               ------------
!
USE MODD_CSTS, ONLY : XSTEFAN, XPI, XSURF_EPSILON, XI0
USE MODI_ABOR1_SFX  
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
CHARACTER(LEN=6)  , INTENT(IN) :: HPROGRAM  ! program calling surf. schemes
!
REAL, DIMENSION(:), INTENT(IN) :: PSCA_SW  ! Diffuse solar radiation (W/m2)
REAL, DIMENSION(:), INTENT(IN) :: PREF_SW  ! Solar radiation reflected by ground (W/m2)
REAL, DIMENSION(:), INTENT(IN) :: PEMIT_LW ! Longwave radiation emitted by the ground (W/m2)
REAL, DIMENSION(:), INTENT(IN) :: PLW_RAD  ! Atmospheric longwave radiation (W/m2)
REAL, DIMENSION(:), INTENT(IN) :: PDIR_SW !Direct solar radiation (W/m2)
REAL, DIMENSION(:), INTENT(IN) :: PZENITH !solar zenithal angle (rad from vert.)
REAL, DIMENSION(SIZE(PSCA_SW)) :: PTRAD_BODY_ISBA

!*      0.2    declarations of local variables
REAL :: ZAB = 0.7 !absorption coef of solar radiation by human body
REAL :: ZEB = 0.97 !emissivity of human body
REAL, DIMENSION(SIZE(PSCA_SW)) :: ZFGRND !ground view factor of human body
REAL, DIMENSION(SIZE(PSCA_SW)) :: ZFSKY !sky view factor of human body
REAL, DIMENSION(SIZE(PSCA_SW)) :: ZDIRSWBODY !solar radiation received by human body
REAL, DIMENSION(SIZE(PSCA_SW)) :: ZELEV !solar elevation angle
REAL, DIMENSION(SIZE(PSCA_SW)) :: ZRADBODY !total radiation received by human body
REAL, DIMENSION(SIZE(PSCA_SW)) :: ZDIR_PERP
INTEGER :: ILUOUT ! Unit number
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
IF (LHOOK) CALL DR_HOOK('TRAD_BODY_ISBA',0,ZHOOK_HANDLE)
!
!*  1 - calculation of view factors
!
ZFGRND(:) = 0.5
ZFSKY (:) = 0.5
!
!*  2 - base calculation for both sun and shade
!
ZRADBODY(:) = ZAB/ZEB * &
            ( PSCA_SW(:)*ZFSKY(:) + PREF_SW (:)*ZFGRND(:) ) &
            + PLW_RAD(:)*ZFSKY(:) + PEMIT_LW(:)*ZFGRND(:)
!
!*  3 - add direct contribution in case of sunny conditions 
!
ZELEV(:) = XPI/2. - PZENITH(:)
!
WHERE (ZELEV(:).LT.1E-6)
  ZELEV(:)=0.0
ENDWHERE
!
! Calculation of solar flux density with respect to a plane
! perpendicular to the direction of solar radiation
!
ZDIR_PERP(:) = PDIR_SW(:) / MAX(SIN(ZELEV(:)),0.05)
!
! Limit to 0.85 time solar constant
!
ZDIR_PERP = MAX(ZDIR_PERP, XSURF_EPSILON)
ZDIR_PERP = MIN(ZDIR_PERP, 0.85 * XI0)
!
ZDIRSWBODY(:) = ZDIR_PERP * 0.308 * COS( ZELEV(:)*(1-(ZELEV(:)*180./XPI)**2/48402.) )
!
! Check ZDIRSWBODY
!
IF ((MINVAL(ZDIRSWBODY).LT.-XSURF_EPSILON).OR.(MAXVAL(ZDIRSWBODY).GT.3000.0)) THEN
   CALL GET_LUOUT(HPROGRAM,ILUOUT)
   WRITE(ILUOUT,*) "                                                                 "
   WRITE(ILUOUT,*) "Unplausible value of direct solar radiation received by the body "
   WRITE(ILUOUT,*) "                                                                 "
   WRITE(ILUOUT,*) "MINVAL(ZDIRSWBODY) ",MINVAL(ZDIRSWBODY)
   WRITE(ILUOUT,*) "MAXVAL(ZDIRSWBODY) ",MAXVAL(ZDIRSWBODY)
   CALL FLUSH(ILUOUT)
   CALL ABOR1_SFX("TRAD_BODY_ISBA:Error in ZDIRSWBODY, check report")
ENDIF
!
! the direct solar radiation is weighted by a projected area factor which can be expressed by this equation
! for a rotationally symmetric human being (Fanger, 1970)
!
ZRADBODY  (:) = ZRADBODY(:) + ZAB/ZEB*ZDIRSWBODY(:)
!
IF ((MINVAL(ZRADBODY).LT.-XSURF_EPSILON).OR.(MAXVAL(ZRADBODY).GT.3000.0)) THEN
   CALL GET_LUOUT(HPROGRAM,ILUOUT)
   WRITE(ILUOUT,*) "                                                    "
   WRITE(ILUOUT,*) "Unplausible value of radiation received by the body "
   WRITE(ILUOUT,*) "                                                    "
   WRITE(ILUOUT,*) "MINVAL(ZRADBODY) ",MINVAL(ZRADBODY)
   WRITE(ILUOUT,*) "MAXVAL(ZRADBODY) ",MAXVAL(ZRADBODY)
   CALL FLUSH(ILUOUT)
   CALL ABOR1_SFX("TRAD_BODY_ISBA:Error in radiation received by body, check report")
ENDIF
!
PTRAD_BODY_ISBA(:) = (ZRADBODY(:)/XSTEFAN)**0.25
!
IF (LHOOK) CALL DR_HOOK('TRAD_BODY_ISBA',1,ZHOOK_HANDLE)
!
END FUNCTION TRAD_BODY_ISBA
