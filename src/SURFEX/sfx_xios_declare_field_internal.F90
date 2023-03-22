!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
SUBROUTINE SFX_XIOS_DECLARE_FIELD_INTERNAL(HREC, HDOMAIN, HCOMMENT, KFREQOP)
!!
!!
!!     PURPOSE
!!     --------
!!
!!     See SFX_XIOS_DECLARE_FIELD for more details
!!
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
!!     AUTHOR
!!     ------
!!
!!     S.Sénési, CNRM
!!
!!     MODIFICATION
!!     --------------
!!
!!     S.Sénési    03/2017
!!
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
USE MODD_XIOS,       ONLY     : COUTPUT_DEFAULT, LGRID_MODE
USE MODD_SURF_PAR,   ONLY     : XUNDEF
USE YOMHOOK    , ONLY         : LHOOK,   DR_HOOK
USE PARKIND1   , ONLY         : JPRB, JPIM
!
#ifdef WXIOS
USE XIOS
#endif
!
USE MODI_ABOR1_SFX
!
IMPLICIT NONE
!
!   Arguments
!
CHARACTER(LEN=*)   ,INTENT(IN)  :: HREC     ! field id
CHARACTER(LEN=*)   ,INTENT(IN)  :: HDOMAIN  ! name of the horiz domain
CHARACTER(LEN=*)   ,INTENT(IN)  :: HCOMMENT ! Comment string a la Surfex
INTEGER(KIND=JPIM) ,INTENT(IN)  :: KFREQOP  ! Sampling frequency, in minutes
!
!  Local variables
!
CHARACTER(1000)    :: YDUMMY
LOGICAL            :: LISDEF, LGRIDDEF
INTEGER            :: IPO,IPF
REAL(KIND=JPRB)    :: ZHOOK_HANDLE
!
#ifdef WXIOS
TYPE(xios_field)      :: field_hdl, other_field_hdl
TYPE(xios_fieldgroup) :: fieldgroup_hdl
TYPE(xios_file)       :: file_hdl
#endif
!
IF (LHOOK) CALL DR_HOOK('SFX_XIOS_DECLARE_FIELD',0,ZHOOK_HANDLE)
!
#ifdef WXIOS
!
!$OMP SINGLE
!
! ----------------------------------------------------------------------
!  We are still in the XIOS init phase =>  Define field if necessary 
! ----------------------------------------------------------------------
!
IF (.NOT. XIOS_IS_VALID_FIELD(HREC))  THEN
   CALL XIOS_GET_HANDLE("field_definition",fieldgroup_hdl)
   CALL XIOS_ADD_CHILD(fieldgroup_hdl,field_hdl,HREC)
   !IF (.NOT. XIOS_IS_VALID_FIELD("default_field")) &
   !    CALL ABOR1_SFX('sfx_xios_check_field:cannot output field '//HREC//' : no default_field is defined')
   CALL XIOS_SET_ATTR(field_hdl,name=HREC)
   !
   ! ----------------------------------------------------------------------
   ! If default_ouput file is defined, add this field to it
   ! ----------------------------------------------------------------------
   !
   IF ( XIOS_IS_VALID_FILE(COUTPUT_DEFAULT)) THEN 
      CALL XIOS_GET_HANDLE(COUTPUT_DEFAULT,file_hdl)
      CALL XIOS_ADD_CHILD(file_hdl,field_hdl)
      CALL XIOS_SET_ATTR(field_hdl,field_ref=HREC)
   ENDIF
ENDIF
!
! ----------------------------------------------------------------------
!  If field attribute 'domain' is not defined, set it
! ----------------------------------------------------------------------
!
!
CALL XIOS_IS_DEFINED_FIELD_ATTR(HREC,grid_ref=LGRIDDEF) 
CALL XIOS_IS_DEFINED_FIELD_ATTR(HREC,domain_ref=LISDEF)
!
IF (  .NOT. LISDEF .AND. .NOT. LGRIDDEF ) THEN 
   IF (TRIM(HDOMAIN)=='') &
        CALL ABOR1_SFX('SFX_XIOS_DECLARE_FIELD_INTERNAL : MUST PROVIDE DOMAIN FOR '//HREC)
   !if (trim(hrec)=='PFRSO1') write(0,*) 'Setting domain for PFRSO1 !!!'
   IF ( LGRID_MODE ) THEN
      CALL XIOS_SET_FIELD_ATTR(HREC, grid_ref=TRIM(HDOMAIN)//'_grid')
   ELSE
      CALL XIOS_SET_FIELD_ATTR(HREC, domain_ref=TRIM(HDOMAIN))
   ENDIF
ELSE
   !write(0,*) 'Field '//trim(hrec)//' already has a grid or domain:',LGRIDDEF,LISDEF
ENDIF
!
! ----------------------------------------------------------------------
! If NetCDF variable name is not defined , set it
! ----------------------------------------------------------------------
!
!CALL XIOS_IS_DEFINED_FIELD_ATTR(HREC,name=LISDEF) 
!IF ( .NOT. LISDEF ) THEN 
!   CALL XIOS_SET_FIELD_ATTR(HREC, name=trim(HREC))
!ENDIF
!
! ------------------------------------------------------------------------
! If field attribute 'unit' is not defined or empty, try to guess a value 
! from HCOMMENT (using rightmost string between parenthesis)
! ------------------------------------------------------------------------
!
CALL XIOS_IS_DEFINED_FIELD_ATTR(HREC,unit=LISDEF)  
IF ( .NOT. LISDEF ) THEN 
   IPO=INDEX(HCOMMENT,"(",.TRUE.)
   IPF=INDEX(HCOMMENT,")",.TRUE.)
   IF ( (IPO > 0) .AND. (IPF>IPO+1) ) THEN
      CALL XIOS_SET_FIELD_ATTR(HREC,unit=HCOMMENT(IPO+1:IPF-1))
   ENDIF
ENDIF
!
! ----------------------------------------------------------------------
! If field attribute 'long_name' is not defined or empty, set it 
! ----------------------------------------------------------------------
!
CALL XIOS_IS_DEFINED_FIELD_ATTR(HREC,long_name=LISDEF) 
IF ( .NOT. LISDEF .AND. (TRIM(HCOMMENT) /= '') ) THEN 
   IF (IPO > 1) THEN 
      CALL XIOS_SET_FIELD_ATTR(HREC,long_name=TRIM(HCOMMENT(1:IPO-1)))
   ELSE
      CALL XIOS_SET_FIELD_ATTR(HREC,long_name=TRIM(HCOMMENT(:)))
   ENDIF
ENDIF
!
! ----------------------------------------------------------------------
! Set default value to Surfex's one
! ----------------------------------------------------------------------
!
CALL XIOS_SET_FIELD_ATTR(HREC,default_value=XUNDEF)
!
!$OMP END SINGLE
#endif
!
IF (LHOOK) CALL DR_HOOK('SFX_XIOS_DECLARE_FIELD_INTERNAL',1,ZHOOK_HANDLE)
! ----------------------------------------------------------------------
!
END SUBROUTINE SFX_XIOS_DECLARE_FIELD_INTERNAL
