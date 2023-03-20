!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
SUBROUTINE SFX_XIOS_DECLARE_FIELD(HREC, HDOMAIN, HAXIS, KLEV, HAXIS2, KLEV2, HCOMMENT, KFREQOP) 
!!
!!
!!     PURPOSE
!!     --------
!!
!!     Declare field HREC and some attributes to XIOS if needed 

!!      If 'units' or 'long_name' attribute is not defined using XIOS
!!     config files , use HCOMMENT to declare it. Same for domain and
!!     other axis, either using relevant args or with default values
!!
!!     If haxis si provided and is the name of dimension 'patch' and
!!     haxis2 is not provided, rather proceed by a loop of 2D
!!     fields declarations
!!  
!!     IMPLICIT ARGUMENTS :
!!     -------------------- 
!!
!!     EXTERNAL
!!     --------
!!
!!     XIOS LIBRARY
!!
!!     REFERENCE
!!     ---------
!!
!!     XIOS Reference guide - Yann Meurdesoif - 10/10/2014 - 
!!     svn co -r 515 http://forge.ipsl.jussieu.fr/ioserver/svn/XIOS/branchs/xios-1.0 <dir> 
!!       cd <dir>/doc ; ....
!!
!!     AUTHOR
!!     ------
!!
!!     S.Sénési, CNRM
!!
!!     MODIFICATION
!!     --------------
!!
!!     Original    03/2016
!!     S.Sénési    03/2017 Split SFX_XIOS_DECLARE_FIELD_INTERNAL ; SFX_XIOS_DECLARE_AXIS_INTERNAL
!!
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
USE MODD_XIOS,       ONLY     : LXIOS_DEF_CLOSED, YPATCH_DIM_NAME, NBASE_XIOS_FREQ
USE MODD_SURF_PAR,   ONLY     : XUNDEF

#ifdef WXIOS
USE XIOS
USE MODI_SFX_XIOS_DECLARE_AXIS_INTERNAL
#endif
!
USE MODI_SET_AXIS
USE MODI_ABOR1_SFX
!
USE YOMHOOK, ONLY  : LHOOK,   DR_HOOK
USE PARKIND1, ONLY : JPRB, JPIM
!
IMPLICIT NONE

!
!   Arguments
!
CHARACTER(LEN=*)   ,INTENT(IN)            :: HREC     ! field id
CHARACTER(LEN=*)   ,INTENT(IN), OPTIONAL  :: HDOMAIN  ! name of the horiz domain
CHARACTER(LEN=*)   ,INTENT(IN), OPTIONAL  :: HAXIS    ! name of first additional axis
INTEGER            ,INTENT(IN), OPTIONAL  :: KLEV     ! First axis size 
CHARACTER(LEN=*)   ,INTENT(IN), OPTIONAL  :: HAXIS2   ! name of second additional axis
INTEGER            ,INTENT(IN), OPTIONAL  :: KLEV2    ! Second axis size
CHARACTER(LEN=*)   ,INTENT(IN), OPTIONAL  :: HCOMMENT ! Comment string a la Surfex
INTEGER(KIND=JPIM) ,INTENT(IN), OPTIONAL  :: KFREQOP  ! Sampling frequency, in minutes
!
CHARACTER(1000)    :: YLDOMAIN
CHARACTER(1000)    :: YLCOMMENT
CHARACTER(1000)    :: YAXIS,YAXIS2
CHARACTER(3)       :: YIDIM
!
INTEGER(KIND=JPIM) :: IFREQOP  ! Sampling frequency, in minutes
INTEGER(KIND=JPIM) :: IIDIM, ILEV, ILEV2
LOGICAL            :: GGRIDDEF
!
REAL(KIND=JPRB)    :: ZHOOK_HANDLE
!
IF (LHOOK) CALL DR_HOOK('SFX_XIOS_DECLARE_FIELD',0,ZHOOK_HANDLE)
!
#ifdef WXIOS
!
!----------------------------------------------------------------------
!   If XIOS init phase is over, just returns
!----------------------------------------------------------------------
!
IF (LXIOS_DEF_CLOSED) THEN
   IF (LHOOK) CALL DR_HOOK('SFX_XIOS_DECLARE_FIELD_INTERNAL',1,ZHOOK_HANDLE)
   RETURN
ENDIF
!
YLDOMAIN='FULL'
IF (PRESENT(HDOMAIN).AND.(TRIM(HDOMAIN) /= '')) YLDOMAIN=TRIM(HDOMAIN)
!
YLCOMMENT=''
IF (PRESENT(HCOMMENT)) YLCOMMENT=TRIM(HCOMMENT)
!
IFREQOP=0
IF (PRESENT(KFREQOP))  IFREQOP=KFREQOP
!
ILEV=0          
IF (PRESENT(KLEV))     ILEV=KLEV
!
ILEV2=0         
IF (PRESENT(KLEV2))    ILEV2=KLEV2
!
YAXIS=''        
IF (PRESENT(HAXIS))    YAXIS=TRIM(HAXIS)
!
YAXIS2=''       
IF (PRESENT(HAXIS2))   YAXIS2=TRIM(HAXIS2)
!
IF (PRESENT(HAXIS) .AND. (YAXIS==TRIM(YPATCH_DIM_NAME)) .AND. .NOT. PRESENT(HAXIS2)) THEN
  ! For historical reason, in that case, a special treatment for
  ! avoiding that 'patch' dimension (provided as 1st dimension) is
  ! actually used : proceed by declaring a set of individual arrays
  IF ( ILEV == 0 ) CALL XIOS_GET_AXIS_ATTR(HAXIS, n_glo=ILEV)
  DO IIDIM=1,ILEV
    IF ( IIDIM < 10 ) THEN 
       WRITE(YIDIM,'(I1)') IIDIM
    ELSEIF ( IIDIM < 100 ) THEN
       WRITE(YIDIM,'(I2)') IIDIM
    ELSE
       WRITE(YIDIM,'(I3)') IIDIM
    ENDIF
    !write(0,*) '<field id="'//trim(HREC)//'_'//TRIM(YIDIM)//'", domain_ref="'//trim(CLDOMAIN)//'" />'
    CALL SFX_XIOS_DECLARE_FIELD_INTERNAL(TRIM(HREC)//'_'//TRIM(YIDIM), YLDOMAIN, YLCOMMENT, IFREQOP)
  END DO
  !
ELSE
  !
  ! Standard case
  ! 
  CALL SFX_XIOS_DECLARE_FIELD_INTERNAL(HREC, YLDOMAIN, YLCOMMENT, IFREQOP)
  IF (PRESENT(HAXIS))  CALL SFX_XIOS_DECLARE_AXIS_INTERNAL(HREC,YAXIS,ILEV)
  IF (PRESENT(HAXIS2)) CALL SFX_XIOS_DECLARE_AXIS_INTERNAL(HREC,YAXIS2,ILEV2,OSECOND=.TRUE.)
ENDIF
!
#endif
!
IF (LHOOK) CALL DR_HOOK('SFX_XIOS_DECLARE_FIELD',1,ZHOOK_HANDLE)
!
!----------------------------------------------------------------------
!
END SUBROUTINE SFX_XIOS_DECLARE_FIELD
