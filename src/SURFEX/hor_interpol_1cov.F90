!SURFEX_LIC Copyright 1994-2014 Meteo-France 
!SURFEX_LIC This is part of the SURFEX software governed by the CeCILL-C  licence
!SURFEX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!SURFEX_LIC for details. version 1.
!     #########
SUBROUTINE HOR_INTERPOL_1COV(KLUOUT,PFIELDIN,PFIELDOUT)
!     #################################################################################
!
!!****  *HOR_INTERPOL * - Call the interpolation of a surface field
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
!!     M.Moge (adapted from  HOR_INTERPOL)
!!
!!    MODIFICATIONS
!!    -------------
!!      Original     06/2015
!!      J.Escobar 18/12/2015 : missing interface
!!------------------------------------------------------------------
!
!
USE MODD_PREP,       ONLY : CINGRID_TYPE, CINTERP_TYPE
!
USE MODI_HOR_INTERPOL_GAUSS_1COV
USE MODI_HOR_INTERPOL_ROTLATLON_1COV
USE MODI_HOR_INTERPOL_AROME_1COV
USE MODI_HOR_INTERPOL_CONF_PROJ_1COV
USE MODI_HOR_INTERPOL_CARTESIAN_1COV
USE MODI_HOR_INTERPOL_LATLON_1COV
USE MODI_HOR_INTERPOL_BUFFER_1COV
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
USE MODI_ABOR1_SFX
!
USE MODI_HOR_INTERPOL_BUFFER
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
INTEGER,            INTENT(IN)  :: KLUOUT    ! logical unit of output listing
REAL, DIMENSION(:), INTENT(IN)   :: PFIELDIN  ! field to interpolate horizontally
REAL, DIMENSION(:), INTENT(OUT)  :: PFIELDOUT ! interpolated field
!
!*      0.2    declarations of local variables
!
INTEGER :: JL ! loop counter
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('HOR_INTERPOL_1COV',0,ZHOOK_HANDLE)
SELECT CASE (CINTERP_TYPE)
!
!*      1.     Interpolation with horibl (from gaussian, Legendre or regular grid)
!              -------------------------------------------------------------------
!
  CASE('HORIBL')
    SELECT CASE(CINGRID_TYPE)
!
!*      1.1    Interpolation from gaussian or Legendre
!
      CASE ('GAUSS     ')
        CALL HOR_INTERPOL_GAUSS_1COV(KLUOUT,PFIELDIN,PFIELDOUT)
!
!*      1.2    Interpolation from regular grid
!
      CASE ('AROME     ')
        CALL HOR_INTERPOL_AROME_1COV(KLUOUT,PFIELDIN,PFIELDOUT)
!
!*      1.3    Interpolation from regular lat/lon coord
!
      CASE ('LATLON    ')
        CALL HOR_INTERPOL_LATLON_1COV(KLUOUT,PFIELDIN,PFIELDOUT)
!
!*      1.4    Interpolation from rotated lat/lon coord
!
      CASE ('ROTLATLON ')
        CALL HOR_INTERPOL_ROTLATLON_1COV(KLUOUT,PFIELDIN,PFIELDOUT)        
!
      CASE DEFAULT
        CALL ABOR1_SFX('HOR_INTERPOL_1COV: WRONG GRID TYPE'//CINGRID_TYPE)

    END SELECT
!
!*      2.     Prescribed uniform field
!              ------------------------
!
  CASE('UNIF  ')
    PFIELDOUT(:) = PFIELDIN(1)
!
!*      3.     Bilinear interpolation
!              ----------------------
!
  CASE('BILIN ')
    SELECT CASE(CINGRID_TYPE)
      CASE ('CONF PROJ ')
        CALL HOR_INTERPOL_CONF_PROJ_1COV(KLUOUT,PFIELDIN,PFIELDOUT)
      CASE ('CARTESIAN ')
        CALL HOR_INTERPOL_CARTESIAN_1COV(KLUOUT,PFIELDIN,PFIELDOUT)
    END SELECT
!
!*      4.     no interpolation, only packing
!              ------------------------------
!
  CASE('BUFFER')
    CALL HOR_INTERPOL_BUFFER_1COV(KLUOUT,PFIELDIN,PFIELDOUT)

!
!*      4.     no interpolation
!              ----------------
!
  CASE('NONE  ')
    PFIELDOUT(:) = PFIELDIN(:)

  CASE DEFAULT 
    CALL ABOR1_SFX('HOR_INTERPOL_1COV: WRONG INTERPOLATION TYPE'//CINTERP_TYPE)

END SELECT
IF (LHOOK) CALL DR_HOOK('HOR_INTERPOL_1COV',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------------
END SUBROUTINE HOR_INTERPOL_1COV
