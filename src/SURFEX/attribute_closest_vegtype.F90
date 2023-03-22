!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #######################
      SUBROUTINE ATTRIBUTE_CLOSEST_VEGTYPE(KNPATCH,KNVEGTYPE,PPATCH_OLD,  &
                                           KPATCH_MISSING,KPATCH_ATTRIBUTE)
!     #######################
!
!!****  *ATTRIBUTE_CLOSEST_VEGTYPE_NUDGING*  
!!
!!    PURPOSE
!!    -------
!       computation for land-use case
!
!     
!!**  METHOD
!!    ------
!
!!    EXTERNAL
!!    --------
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
!!	R. Seferian           * Meteo-France *
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    03/2018
!                  
!       
!-------------------------------------------------------------------------------
!
!*           DECLARATIONS
!            ------------
!
USE MODD_LANDUSE_PAR, ONLY : NECO_NO_VEG, NECO_TALL_VEG, NECO_LOW_VEG
!
USE MODI_VEGTYPE_TO_PATCH
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!
!* dummy arguments
!  ---------------
!
INTEGER,                     INTENT(IN)  :: KNPATCH           ! number of patch
INTEGER,                     INTENT(IN)  :: KNVEGTYPE         ! number of vegtypes
REAL   , DIMENSION(:),       INTENT(IN)  :: PPATCH_OLD        ! patch fraction
INTEGER,                     INTENT(IN)  :: KPATCH_MISSING    ! missing patch
!
INTEGER,                     INTENT(OUT) :: KPATCH_ATTRIBUTE  ! replacement patch
!
!* local variables
!  ---------------
!
REAL, DIMENSION(KNVEGTYPE)               :: ZPATCH_VEGTYPE    ! field for each vegtype
!
INTEGER, DIMENSION(KNVEGTYPE)            :: IPATCHES
!
INTEGER                                  :: IPATCH   ! patch   counter
INTEGER                                  :: JVEG     ! vegtype counter
INTEGER                                  :: IDMAX    ! counter of the maximal patch proportion
INTEGER                                  :: JI 
!
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('ATTRIBUTE_CLOSEST_VEGTYPE',0,ZHOOK_HANDLE)
!
! 1. Distribute patches field on all corresponding vegtypes
!    ------------------------------------------------------
!
DO JVEG=1,KNVEGTYPE
!
  CALL VEGTYPE_TO_PATCH(JVEG,KNPATCH,IPATCH)
!
  ZPATCH_VEGTYPE(JVEG) = PPATCH_OLD(IPATCH)
  IPATCHES      (JVEG) = IPATCH
!
END DO
!
!* initialization
!
KPATCH_ATTRIBUTE=KPATCH_MISSING
!
! 2. Loop on vegetation charateristics
!    ---------------------------------
!
! 2.1 Treat no vegetation case first
!     ------------------------------
!
IF (ANY(KPATCH_MISSING == IPATCHES(NECO_NO_VEG))) THEN
!
  IDMAX=0
  DO JI=1,SIZE(NECO_NO_VEG) ! Loop on vegtypes of the no vegation types
    JVEG=NECO_NO_VEG(JI) ! Vegtype index
    ! search in no vegetation pfts
    IF (ZPATCH_VEGTYPE(JVEG) /= 0.) THEN
      IDMAX = MAXVAL(MAXLOC(ZPATCH_VEGTYPE(NECO_NO_VEG ),ZPATCH_VEGTYPE(NECO_NO_VEG)/=0.))
      IF (IDMAX/=0) KPATCH_ATTRIBUTE = NECO_NO_VEG(IDMAX)
    ENDIF
    ! then, search in low vegetation pfts
    IF (IDMAX<1 .OR. PPATCH_OLD(KPATCH_ATTRIBUTE)==0.) THEN
      IDMAX =MAXVAL(MAXLOC(ZPATCH_VEGTYPE(NECO_LOW_VEG),ZPATCH_VEGTYPE(NECO_LOW_VEG)/=0.))
      IF (IDMAX/=0) KPATCH_ATTRIBUTE = NECO_LOW_VEG(IDMAX)
    ENDIF
    ! Finally, search in tall vegetation pfts
    IF (IDMAX<1 .OR. PPATCH_OLD(KPATCH_ATTRIBUTE)==0.) THEN
      IDMAX =MAXVAL(MAXLOC(ZPATCH_VEGTYPE(NECO_TALL_VEG),ZPATCH_VEGTYPE(NECO_TALL_VEG)/=0.))
      IF (IDMAX/=0) KPATCH_ATTRIBUTE = NECO_TALL_VEG(IDMAX)
    ENDIF
  ENDDO
!
! 2.2 Treat low vegetation case second
!     --------------------------------
!
ELSEIF (ANY(KPATCH_MISSING == IPATCHES(NECO_LOW_VEG) )) THEN
!
  IDMAX=0
  DO JI=1,SIZE(NECO_LOW_VEG)
    JVEG=NECO_LOW_VEG(JI)
    ! search in low vegetation pfts
    IF (ZPATCH_VEGTYPE(JVEG) /= 0.) THEN
      IDMAX = MAXVAL(MAXLOC(ZPATCH_VEGTYPE(NECO_LOW_VEG ),ZPATCH_VEGTYPE(NECO_LOW_VEG)/=0.))
      IF (IDMAX/=0) KPATCH_ATTRIBUTE = NECO_LOW_VEG(IDMAX)
    ENDIF
    ! Finally, search in tall vegetation pfts
    IF (IDMAX<1 .OR. PPATCH_OLD(KPATCH_ATTRIBUTE)==0.) THEN
      IDMAX=MAXVAL(MAXLOC(ZPATCH_VEGTYPE(NECO_TALL_VEG),ZPATCH_VEGTYPE(NECO_TALL_VEG)/=0.))
      IF (IDMAX/=0) KPATCH_ATTRIBUTE = NECO_TALL_VEG(IDMAX)
    ENDIF
    ! then, search in no vegetation pfts
    IF (IDMAX<1 .OR. PPATCH_OLD(KPATCH_ATTRIBUTE)==0.) THEN
      IDMAX =MAXVAL(MAXLOC(ZPATCH_VEGTYPE(NECO_NO_VEG),ZPATCH_VEGTYPE(NECO_NO_VEG)/=0.))
      IF (IDMAX/=0) KPATCH_ATTRIBUTE = NECO_NO_VEG(IDMAX)
    ENDIF
  ENDDO
!
! 2.3 Treat high vegetation case finally
!     --------------------------------
!
ELSEIF (ANY(KPATCH_MISSING == IPATCHES(NECO_TALL_VEG) )) THEN
!
  IDMAX=0
  DO JI=1,SIZE(NECO_TALL_VEG)
    JVEG=NECO_TALL_VEG(JI)
    ! search in tall vegetation pfts
    IF (ZPATCH_VEGTYPE(JVEG) /= 0.) THEN
      IDMAX = MAXVAL(MAXLOC(ZPATCH_VEGTYPE(NECO_TALL_VEG ),ZPATCH_VEGTYPE(NECO_TALL_VEG)/=0.))
      IF (IDMAX/=0) KPATCH_ATTRIBUTE = NECO_TALL_VEG(IDMAX)
    ENDIF
    ! then, search in low vegetation pfts
    IF (IDMAX<1 .OR. PPATCH_OLD(KPATCH_ATTRIBUTE)==0.) THEN
      IDMAX =MAXVAL(MAXLOC(ZPATCH_VEGTYPE(NECO_LOW_VEG),ZPATCH_VEGTYPE(NECO_LOW_VEG)/=0.))
      IF (IDMAX/=0) KPATCH_ATTRIBUTE = NECO_LOW_VEG(IDMAX)
    ENDIF
    ! Finally, search in no vegetation pfts
    IF (IDMAX<1 .OR. PPATCH_OLD(KPATCH_ATTRIBUTE)==0.) THEN
      IDMAX =MAXVAL(MAXLOC(ZPATCH_VEGTYPE(NECO_NO_VEG),ZPATCH_VEGTYPE(NECO_NO_VEG)/=0.))
      IF (IDMAX/=0) KPATCH_ATTRIBUTE = NECO_NO_VEG(IDMAX)
    ENDIF
  ENDDO
!
ENDIF
!
IF (LHOOK) CALL DR_HOOK('ATTRIBUTE_CLOSEST_VEGTYPE',1,ZHOOK_HANDLE)
!-------------------------------------------------------------------
!
END SUBROUTINE ATTRIBUTE_CLOSEST_VEGTYPE
