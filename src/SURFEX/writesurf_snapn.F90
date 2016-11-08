!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE WRITESURF_SNAP_n (DGU, U, &
                                    CHN, &
                                   HPROGRAM)
!     #######################################################################
!
!-----------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!
!
!
!
!
USE MODD_DIAG_SURF_ATM_n, ONLY : DIAG_SURF_ATM_t
USE MODD_SURF_ATM_n, ONLY : SURF_ATM_t
!
USE MODD_CH_SNAP_n, ONLY : CH_EMIS_SNAP_t
!
USE MODI_GET_LUOUT
USE MODI_WRITE_SURF
USE MODI_WRITE_SURF_FIELD2D
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
USE MODI_ABOR1_SFX
!
IMPLICIT NONE
!
!
!
TYPE(DIAG_SURF_ATM_t), INTENT(INOUT) :: DGU
TYPE(SURF_ATM_t), INTENT(INOUT) :: U
!
TYPE(CH_EMIS_SNAP_t), INTENT(INOUT) :: CHN
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
YCOMMENT = ""
!
YRECFM='EMISPEC_NBR'
 CALL WRITE_SURF(DGU, U, &
                 HPROGRAM,YRECFM,CHN%NEMIS_NBR,IRESP,YCOMMENT)
YRECFM='SNAP_NBR'
 CALL WRITE_SURF(DGU, U, &
                 HPROGRAM,YRECFM,CHN%NEMIS_SNAP,IRESP,YCOMMENT)
YRECFM='SNAP_TIME'
 CALL WRITE_SURF(DGU, U, &
                 HPROGRAM,YRECFM,CHN%CSNAP_TIME_REF,IRESP,YCOMMENT)
!
IF (CHN%CSNAP_TIME_REF=='LEGAL') THEN
  YRECFM='LEGALTIME'
  CALL WRITE_SURF(DGU, U, &
                 HPROGRAM,YRECFM,CHN%XDELTA_LEGAL_TIME(:),IRESP,YCOMMENT)
END IF
!-------------------------------------------------------------------------------
!
DO JSPEC=1,CHN%NEMIS_NBR
! Writes the name of species
  WRITE(YRECFM,'("EMISNAME",I3.3)') JSPEC
  YCOMMENT = CHN%CEMIS_COMMENT(JSPEC)
  CALL WRITE_SURF(DGU, U, &
                 HPROGRAM,YRECFM,CHN%CEMIS_NAME(JSPEC),IRESP,YCOMMENT)
!
! Writes the temporal profiles of all snaps
  YRECFM = "E_"//TRIM(CHN%CEMIS_NAME(JSPEC))//"_M"
  YCOMMENTUNIT='-'
  CALL WRITE_SURF_FIELD2D(DGU, U, &
          HPROGRAM,CHN%XSNAP_MONTHLY(:,:,JSPEC),YRECFM,YCOMMENT,YCOMMENTUNIT,HDIR='-',HNAM_DIM="Nemis_snap      ")
  YRECFM = "E_"//TRIM(CHN%CEMIS_NAME(JSPEC))//"_D"
  YCOMMENTUNIT='-'
  CALL WRITE_SURF_FIELD2D(DGU, U, &
          HPROGRAM,CHN%XSNAP_DAILY(:,:,JSPEC),YRECFM,YCOMMENT,YCOMMENTUNIT,HDIR='-',HNAM_DIM="Nemis_snap      ")
  YRECFM = "E_"//TRIM(CHN%CEMIS_NAME(JSPEC))//"_H"
  YCOMMENTUNIT='-'
  CALL WRITE_SURF_FIELD2D(DGU, U, &
          HPROGRAM,CHN%XSNAP_HOURLY(:,:,JSPEC),YRECFM,YCOMMENT,YCOMMENTUNIT,HDIR='-',HNAM_DIM="Nemis_snap      ")

! Writes the potential emission of species for each snap
  DO JSNAP=1,CHN%NEMIS_SNAP
    WRITE(YRECFM,'("SN",I2.2,"_",A7)') JSNAP,CHN%CEMIS_NAME(JSPEC)
    CALL WRITE_SURF(DGU, U, &
                 HPROGRAM,YRECFM,CHN%XEMIS_FIELDS_SNAP(:,JSNAP,JSPEC),IRESP,YCOMMENT)
  END DO
!
END DO
!
!-------------------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('WRITESURF_SNAP_n',1,ZHOOK_HANDLE)
!-------------------------------------------------------------------------------
!
END SUBROUTINE WRITESURF_SNAP_n
