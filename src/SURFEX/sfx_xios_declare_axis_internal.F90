!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
SUBROUTINE SFX_XIOS_DECLARE_AXIS_INTERNAL(HREC, HAXIS, KLEV, OSECOND) 
!
USE YOMHOOK    , ONLY         : LHOOK,   DR_HOOK
USE PARKIND1   , ONLY         : JPRB
!
#ifdef WXIOS
USE XIOS
#endif
!
USE MODI_SET_AXIS
USE MODI_ABOR1_SFX
!
!
IMPLICIT NONE
!
!   Arguments
!
CHARACTER(LEN=*)   ,INTENT(IN) :: HREC     ! field id
CHARACTER(LEN=*)   ,INTENT(IN) :: HAXIS    ! axis name 
INTEGER            ,INTENT(IN) :: KLEV     ! axis size
LOGICAL            ,INTENT(IN),OPTIONAL :: OSECOND  ! Is it a second axis
!
!  Local variables
!
LOGICAL            :: GISDEF, GGRIDDEF, GVALID_AXIS
CHARACTER(1000)    :: YAXIS
INTEGER            :: INGLO
REAL(KIND=JPRB)    :: ZHOOK_HANDLE
!
IF (LHOOK) CALL DR_HOOK('SFX_XIOS_DECLARE_AXIS_INTERNAL',0,ZHOOK_HANDLE)
!
#ifdef WXIOS
CALL XIOS_IS_DEFINED_FIELD_ATTR(HREC,grid_ref=GGRIDDEF) 
IF (.NOT. GGRIDDEF ) THEN 
   ! If an axis is already declared, just do nothing, except
   ! if it is second call
   CALL XIOS_IS_DEFINED_FIELD_ATTR(HREC,axis_ref=GISDEF) 
   IF ( .NOT. GISDEF .OR. PRESENT(OSECOND)) THEN
      IF ( TRIM(HAXIS) == '')  THEN
         GVALID_AXIS=.FALSE.
         IF (PRESENT(OSECOND)) THEN
            YAXIS='dim2_for_'//TRIM(HREC)
         ELSE
            YAXIS='dim_for_'//TRIM(HREC)
         ENDIF
      ELSE
         GVALID_AXIS=XIOS_IS_VALID_AXIS(trim(HAXIS))
         YAXIS=TRIM(HAXIS)
      ENDIF
      IF (.NOT. GVALID_AXIS) THEN 
         IF ( KLEV /= 0) THEN 
            CALL SET_AXIS(TRIM(YAXIS),KSIZE=KLEV)
            !write(0,*) 'calling set_axis for '//trim(yaxis)//" "//HREC ; call flush(0)
         ELSE
            CALL ABOR1_SFX('SFX_XIOS_DECLARE_FIELD:SFX_XIOS_DECLARE_AXIS_INTERNAL'//&
                 ': MUST PROVIDE KLEV OR AN ALREADY DECLARED HAXIS for '//HREC)
         ENDIF
      ENDIF
      CALL XIOS_SET_FIELD_ATTR(HREC, axis_ref=TRIM(YAXIS))
   ELSE
      !write(0,*) 'An axis is already defined for '//HREC ; call flush(0)
   ENDIF
ELSE
   !write(0,*) 'A grid is already defined for '//HREC ; call flush(0)
ENDIF
#endif
!
IF (LHOOK) CALL DR_HOOK('SFX_XIOS_DECLARE_AXIS_INTERNAL',1,ZHOOK_HANDLE)
!
END SUBROUTINE SFX_XIOS_DECLARE_AXIS_INTERNAL
