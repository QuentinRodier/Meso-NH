!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE READWRITE_EMIS_FIELD_n ( DTCO, DGU, U, &
                                         HPROGRAM)
!     #######################################################################
!
!-----------------------------------------------------------------------------
!!    MODIFICATIONS
!!  -------------
!!    J.Escobar : 20/04/2016 : Pb IOZ/NETCDF , replace READ/WRITE_SURF by READ/WRITE_SURF_FIELD2D
!
!*       0.    DECLARATIONS
!
!
!
USE MODD_DATA_COVER_n, ONLY : DATA_COVER_t
USE MODD_DIAG_SURF_ATM_n, ONLY : DIAG_SURF_ATM_t
USE MODD_SURF_ATM_n, ONLY : SURF_ATM_t
!
!
USE MODI_GET_LUOUT
USE MODI_INIT_IO_SURF_n
USE MODI_END_IO_SURF_n
USE MODI_READ_SURF
USE MODI_WRITE_SURF
!
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
USE MODI_ABOR1_SFX
USE MODI_READ_SURF_FIELD2D
USE MODI_WRITE_SURF_FIELD2D
!
IMPLICIT NONE
!
!
TYPE(DATA_COVER_t), INTENT(INOUT) :: DTCO
TYPE(DIAG_SURF_ATM_t), INTENT(INOUT) :: DGU
TYPE(SURF_ATM_t), INTENT(INOUT) :: U
!
 CHARACTER(LEN=6) :: HPROGRAM
!
!*       0.2   declarations of local variables
!
INTEGER             :: IRESP  ! I/O error code
 CHARACTER (LEN=16)  :: YRECFM ! article name
 CHARACTER (LEN=100) :: YCOMMENT ! comment
 CHARACTER(LEN=100)  :: YCOMMENTUNIT   ! Comment string : unit of the datas in the field to write
INTEGER             :: ILUOUT   ! Unit number for prints
INTEGER             :: JSPEC    ! Loop index for emission species
INTEGER             :: IEMISPEC_NBR    ! number of emitted chemical species
 CHARACTER(LEN=40)   :: YEMISPEC_NAME   ! species name
INTEGER             :: IEMISPEC_NTIMES ! number of emission times
 CHARACTER(LEN=3)    :: YSURF ! surface type
INTEGER,DIMENSION(:),ALLOCATABLE :: ITIMES ! emission times for a species
REAL, DIMENSION(:,:),ALLOCATABLE :: ZWORK  ! work array read in the file
!
INTEGER           :: IVERSION       ! version of surfex file being read
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('READWRITE_EMIS_FIELD_N',0,ZHOOK_HANDLE)
 CALL GET_LUOUT(HPROGRAM,ILUOUT)
!
!-------------------------------------------------------------------------------
!
 CALL INIT_IO_SURF_n(DTCO, DGU, U, &
                     HPROGRAM,'FULL  ','SURF  ','READ ')
!* ascendant compatibility
YRECFM='VERSION'
 CALL READ_SURF(&
                HPROGRAM,YRECFM,IVERSION,IRESP)
!
YRECFM='EMISFILE_NBR'
IF (IVERSION<4) YRECFM='EMISFILE_GR_NBR'
 CALL READ_SURF(&
                HPROGRAM,YRECFM,IEMISPEC_NBR,IRESP,YCOMMENT)
 CALL END_IO_SURF_n(HPROGRAM)
!
IF (IRESP/=0) THEN
  CALL ABOR1_SFX('READWRITE_EMIS_FIELDN: PROBLEM READING NUMBER OF 2D CHEMICAL EMISSION FIELDS')
END IF
!
 CALL INIT_IO_SURF_n(DTCO, DGU, U, &
                     HPROGRAM,'FULL  ','SURF  ','WRITE')
 CALL WRITE_SURF(DGU, U, &
                 HPROGRAM,YRECFM,IEMISPEC_NBR,IRESP,YCOMMENT)
 CALL END_IO_SURF_n(HPROGRAM)
!
!-------------------------------------------------------------------------------
!
 CALL INIT_IO_SURF_n(DTCO, DGU, U, &
                     HPROGRAM,'FULL  ','SURF  ','READ ')
YRECFM='EMISPEC_NBR'
IF (IVERSION<4) YRECFM='EMISPEC_GR_NBR'
 CALL READ_SURF(&
                HPROGRAM,YRECFM,IEMISPEC_NBR,IRESP,YCOMMENT)
 CALL END_IO_SURF_n(HPROGRAM)
!
IF (IRESP/=0) THEN
  CALL ABOR1_SFX('READWRITE_EMIS_FIELDN: PROBLEM READING NUMBER OF EMITTED CHEMICAL SPECIES')
END IF
!
 CALL INIT_IO_SURF_n(DTCO, DGU, U, &
                     HPROGRAM,'FULL  ','SURF  ','WRITE')
 CALL WRITE_SURF(DGU, U, &
                 HPROGRAM,YRECFM,IEMISPEC_NBR,IRESP,YCOMMENT)
 CALL END_IO_SURF_n(HPROGRAM)
!
!-------------------------------------------------------------------------------
!
DO JSPEC=1,IEMISPEC_NBR
 CALL INIT_IO_SURF_n(DTCO, DGU, U, &
                     HPROGRAM,'FULL  ','SURF  ','READ ')
  WRITE(YRECFM,'("EMISNAME",I3.3)') JSPEC
  CALL READ_SURF(&
                HPROGRAM,YRECFM,YEMISPEC_NAME,IRESP,YCOMMENT)
  CALL END_IO_SURF_n(HPROGRAM)
!
  IF (IRESP/=0) THEN
    CALL ABOR1_SFX('READWRITE_EMIS_FIELDN: PROBLEM WHEN READING THE NAME OF EMITTED CHEMICAL SPECIES '//YRECFM)
  END IF
  READ(YCOMMENT,'(A3,24x,I5)') YSURF, IEMISPEC_NTIMES
  !
 CALL INIT_IO_SURF_n(DTCO, DGU, U, &
                     HPROGRAM,'FULL  ','SURF  ','WRITE')
  CALL WRITE_SURF(DGU, U, &
                 HPROGRAM,YRECFM,YEMISPEC_NAME,IRESP,YCOMMENT)
  CALL END_IO_SURF_n(HPROGRAM)
!  
!-------------------------------------------------------------------------------
!
  ALLOCATE(ITIMES(IEMISPEC_NTIMES))
  ALLOCATE(ZWORK(U%NSIZE_FULL,IEMISPEC_NTIMES))
!
!-------------------------------------------------------------------------------
!
 CALL INIT_IO_SURF_n(DTCO, DGU, U, &
                     HPROGRAM,'FULL  ','SURF  ','READ ')
  YRECFM='E_'//TRIM(YEMISPEC_NAME)
  CALL READ_SURF_FIELD2D(&
                HPROGRAM,ZWORK,YRECFM,HCOMMENT=YCOMMENT,KRESP=IRESP)
  CALL END_IO_SURF_n(HPROGRAM)
  !
  IF (IRESP/=0) THEN
    CALL ABOR1_SFX('READWRITE_EMIS_FIELDN: PROBLEM WHEN READING THE EMISSION DATA '//YRECFM)
  END IF
  !
 CALL INIT_IO_SURF_n(DTCO, DGU, U, &
                     HPROGRAM,'FULL  ','SURF  ','WRITE')
  YCOMMENTUNIT=''
  CALL WRITE_SURF_FIELD2D(DGU, U, &
                 HPROGRAM,ZWORK,YRECFM,YCOMMENT,YCOMMENTUNIT)
  CALL END_IO_SURF_n(HPROGRAM)
!
!-------------------------------------------------------------------------------
!
 CALL INIT_IO_SURF_n(DTCO, DGU, U, &
                     HPROGRAM,'FULL  ','SURF  ','READ ')
  WRITE(YRECFM,'("EMISTIMES",I3.3)') JSPEC
  CALL READ_SURF(&
                HPROGRAM,YRECFM,ITIMES,IRESP,YCOMMENT,'-')
  CALL END_IO_SURF_n(HPROGRAM)

  IF (IRESP/=0) THEN
    CALL ABOR1_SFX('READWRITE_EMIS_FIELDN: PROBLEM WHEN READING THE EMISSION TIMES '//YRECFM)
  END IF

 CALL INIT_IO_SURF_n(DTCO, DGU, U, &
                     HPROGRAM,'FULL  ','SURF  ','WRITE')
  CALL WRITE_SURF(DGU, U, &
                 HPROGRAM,YRECFM,ITIMES,IRESP,YCOMMENT,'-')
  CALL END_IO_SURF_n(HPROGRAM)
!
!-------------------------------------------------------------------------------
!
  DEALLOCATE(ITIMES)
  DEALLOCATE(ZWORK)
!
!-------------------------------------------------------------------------------
END DO
IF (LHOOK) CALL DR_HOOK('READWRITE_EMIS_FIELD_N',1,ZHOOK_HANDLE)
!-------------------------------------------------------------------------------
!
END SUBROUTINE READWRITE_EMIS_FIELD_n
