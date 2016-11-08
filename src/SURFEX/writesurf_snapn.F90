!SURFEX_LIC Copyright 1994-2014 Meteo-France 
!SURFEX_LIC This is part of the SURFEX software governed by the CeCILL-C  licence
!SURFEX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!SURFEX_LIC for details. version 1.
!
!
!
!!    MODIFICATIONS
!!      M.Moge    01/2016  using WRITE_SURF_FIELD2D/3D for 2D/3D surfex fields writes
!     #########
      SUBROUTINE WRITESURF_SNAP_n(HPROGRAM)
!     #######################################################################
!
!-----------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!
USE MODI_GET_LUOUT
USE MODI_WRITE_SURF
USE MODI_WRITE_SURF_FIELD2D
!
USE MODD_CH_SNAP_n
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
USE MODI_ABOR1_SFX
!
IMPLICIT NONE
!
 CHARACTER(LEN=6) :: HPROGRAM
!
!*       0.2   declarations of local variables
!
INTEGER             :: IRESP    ! I/O error code
 CHARACTER (LEN=16)  :: YRECFM   ! article name
 CHARACTER (LEN=100) :: YCOMMENT ! comment
 CHARACTER(LEN=100):: YCOMMENTUNIT   ! Comment string : unit of the datas in the field to write
INTEGER             :: ILUOUT   ! Unit number for prints
INTEGER             :: JSPEC    ! Loop index for emission species
INTEGER             :: JSNAP    ! Loop index for SNAP categories
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('WRITESURF_SNAP_n',0,ZHOOK_HANDLE)
 CALL GET_LUOUT(HPROGRAM,ILUOUT)
!
!-------------------------------------------------------------------------------
!
YRECFM='EMISPEC_NBR'
 CALL WRITE_SURF(HPROGRAM,YRECFM,NEMIS_NBR,IRESP,YCOMMENT)
YRECFM='SNAP_NBR'
 CALL WRITE_SURF(HPROGRAM,YRECFM,NEMIS_SNAP,IRESP,YCOMMENT)
YRECFM='SNAP_TIME'
 CALL WRITE_SURF(HPROGRAM,YRECFM,CSNAP_TIME_REF,IRESP,YCOMMENT)
!
IF (CSNAP_TIME_REF=='LEGAL') THEN
  YRECFM='LEGALTIME'
  CALL WRITE_SURF(HPROGRAM,YRECFM,XDELTA_LEGAL_TIME(:),IRESP,YCOMMENT)
END IF
!-------------------------------------------------------------------------------
!
DO JSPEC=1,NEMIS_NBR
! Writes the name of species
  WRITE(YRECFM,'("EMISNAME",I3.3)') JSPEC
  YCOMMENT = CEMIS_COMMENT(JSPEC)
  CALL WRITE_SURF(HPROGRAM,YRECFM,CEMIS_NAME(JSPEC),IRESP,YCOMMENT)
!
! Writes the temporal profiles of all snaps
  YRECFM = "E_"//TRIM(CEMIS_NAME(JSPEC))//"_M"
  YCOMMENTUNIT='-'
  CALL WRITE_SURF_FIELD2D(HPROGRAM,XSNAP_MONTHLY(:,:,JSPEC),YRECFM,YCOMMENT,YCOMMENTUNIT,HDIR='-')
  YRECFM = "E_"//TRIM(CEMIS_NAME(JSPEC))//"_D"
  YCOMMENTUNIT='-'
  CALL WRITE_SURF_FIELD2D(HPROGRAM,XSNAP_DAILY(:,:,JSPEC),YRECFM,YCOMMENT,YCOMMENTUNIT,HDIR='-')
  YRECFM = "E_"//TRIM(CEMIS_NAME(JSPEC))//"_H"
  YCOMMENTUNIT='-'
  CALL WRITE_SURF_FIELD2D(HPROGRAM,XSNAP_HOURLY(:,:,JSPEC),YRECFM,YCOMMENT,YCOMMENTUNIT,HDIR='-')
! Writes the potential emission of species for each snap
  DO JSNAP=1,NEMIS_SNAP
    WRITE(YRECFM,'("SN",I2.2,"_",A7)') JSNAP,CEMIS_NAME(JSPEC)
    CALL WRITE_SURF(HPROGRAM,YRECFM,XEMIS_FIELDS_SNAP(:,JSNAP,JSPEC),IRESP,YCOMMENT)
  END DO
!
END DO
!
!-------------------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('WRITESURF_SNAP_n',1,ZHOOK_HANDLE)
!-------------------------------------------------------------------------------
!
END SUBROUTINE WRITESURF_SNAP_n
