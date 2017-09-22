!MNH_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
!MNH_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!MNH_LIC for details. version 1.
!-----------------------------------------------------------------
!--------------- special set of characters for RCS information
!-----------------------------------------------------------------
! $Source$ $Revision$
! masdev4_7 BUG1 2007/06/15 17:47:17
!-----------------------------------------------------------------
!######################## 
MODULE MODI_COMPARE_DAD
!########################
!
INTERFACE
!
      SUBROUTINE COMPARE_DAD (HDADINIFILE,HDADSPAFILE,KRESP)
!
CHARACTER (LEN=*),      INTENT(IN)  :: HDADINIFILE   ! Name of true DAD FM file
CHARACTER (LEN=*),      INTENT(IN)  :: HDADSPAFILE   ! Name of Replaced DAD FM file
INTEGER,               INTENT(OUT)  :: KRESP !  logical switch 
!
END SUBROUTINE COMPARE_DAD 
!
END INTERFACE
!
END MODULE MODI_COMPARE_DAD
!
!
!     #######################################################################
      SUBROUTINE COMPARE_DAD  (HDADINIFILE,HDADSPAFILE,KRESP)
!     #######################################################################
!
!!****  *COMPARE_DAD * - subroutine to check if the DAD model of model 1 can be 
!!                       replaced by an other FM-file for spawning file if
!!                       spawning 1
!!
!!    PURPOSE
!!    -------
!!
!!      Compare grid and time of two FM-files 
!!
!!
!!**  METHOD
!!    ------
!!
!!
!!    EXTERNAL
!!    --------
!!
!!      FMATTR        : to associate a logical unit number to a file
!!      FMOPEN        : to open a FM-file (DESFM + LFIFM)
!!      FMCLOS        : to close a FM-file (DESFM + LFIFM)
!!
!!
!!
!!    AUTHOR
!!    ------
!!
!!       G.Jaubert     * METEO-FRANCE *
!!
!!    MODIFICATIONS
!!    -------------
!!
!!      Original     O8/04/04 
!-------------------------------------------------------------------------------
!
!*       0.     DECLARATIONS
!               ------------
!
!
USE MODD_CONF
USE MODD_IO_ll,      ONLY: TFILEDATA
USE MODD_LUNIT_n
USE MODD_PARAMETERS, ONLY: JPHEXT,JPVEXT,NMNHNAMELGTMAX
!
USE MODE_FM
USE MODE_FMREAD
USE MODE_GRIDPROJ
USE MODE_IO_MANAGE_STRUCT, ONLY : IO_FILE_ADD2LIST
!
!
IMPLICIT NONE
!
!*       0.1  Declarations of dummy arguments :
!
CHARACTER (LEN=*), INTENT(IN)  :: HDADINIFILE   ! Name of true DAD FM file
CHARACTER (LEN=*), INTENT(IN)  :: HDADSPAFILE   ! Name of Replaced DAD FM file
INTEGER,           INTENT(OUT) :: KRESP
!
!
!*       0.2  Declarations of local variables :
!
!
INTEGER :: IRESP    ! Return codes in FM routines
INTEGER :: ILUOUT   ! Logical unit number for the output listing 
INTEGER :: IMASDEV
!
INTEGER             :: IIU_1,IIU_2    ! Upper dimension in x direction
INTEGER             :: IJU_1,IJU_2    ! Upper dimension in y direction
INTEGER             :: IKU_1,IKU_2    ! Upper dimension in z direction
!
REAL                :: ZLON0_1,ZLAT0_1,ZBETA_1,ZRPK_1,ZLONORI_1,ZLATORI_1
REAL                :: ZLON0_2,ZLAT0_2,ZBETA_2,ZRPK_2,ZLONORI_2,ZLATORI_2
REAL, DIMENSION(:),ALLOCATABLE  :: ZXHAT_1,ZYHAT_1,ZZHAT_1 ! Position x,y,height
REAL, DIMENSION(:),ALLOCATABLE  :: ZXHAT_2,ZYHAT_2,ZZHAT_2 ! Position x,y,height
REAL, DIMENSION(:,:),ALLOCATABLE  :: ZZS_1  ! orography
REAL, DIMENSION(:,:),ALLOCATABLE  :: ZZS_2  ! orography
!
INTEGER             :: IIMAX_1,IJMAX_1,IKMAX_1
INTEGER             :: IIMAX_2,IJMAX_2,IKMAX_2
!
REAL :: ZLATORI, ZLONORI, ZXHATM, ZYHATM
TYPE(TFILEDATA),POINTER :: TZDADINIFILE => NULL()
TYPE(TFILEDATA),POINTER :: TZDADSPAFILE => NULL()
!-------------------------------------------------------------------------------
!
!*   1.    INITIALIZATIONS
!          ---------------
!
CALL FMLOOK_ll(CLUOUT,CLUOUT,ILUOUT,IRESP)
!
ZLON0_1=0.
ZLAT0_1=0.
ZBETA_1=0.
ZRPK_1=0.
ZLONORI_1=0.
ZLATORI_1=0.
!
ZLON0_2=0.
ZLAT0_2=0.
ZBETA_2=0.
ZRPK_2=0.
ZLONORI_2=0.
ZLATORI_2=0.
!
!-------------------------------------------------------------------------------
!
!*   2.    Read DAD of initial file
!          ------------------------
!
CALL IO_FILE_ADD2LIST(TZDADINIFILE,TRIM(HDADINIFILE),'UNKNOWN','READ',KLFINPRAR=0,KLFITYPE=2,KLFIVERB=NVERB)
CALL IO_FILE_OPEN_ll(TZDADINIFILE,CLUOUT,IRESP)
!
CALL IO_READ_FIELD(TZDADINIFILE,'IMAX',IIMAX_1)
CALL IO_READ_FIELD(TZDADINIFILE,'JMAX',IJMAX_1)
CALL IO_READ_FIELD(TZDADINIFILE,'KMAX',IKMAX_1)
!
IIU_1=IIMAX_1 + 2 * JPHEXT
IJU_1=IJMAX_1 + 2 * JPHEXT
IKU_1=IKMAX_1 + 2 * JPVEXT
!
ALLOCATE(ZXHAT_1(IIU_1))
ALLOCATE(ZYHAT_1(IJU_1))
ALLOCATE(ZZHAT_1(IKU_1))
CALL IO_READ_FIELD(TZDADINIFILE,'XHAT',ZXHAT_1)
CALL IO_READ_FIELD(TZDADINIFILE,'YHAT',ZYHAT_1)
CALL IO_READ_FIELD(TZDADINIFILE,'ZHAT',ZZHAT_1)
!
ALLOCATE(ZZS_1(IIU_1,IJU_1))
CALL IO_READ_FIELD(TZDADINIFILE,'ZS',ZZS_1)
!
CALL IO_READ_FIELD(TZDADINIFILE,'LON0',ZLON0_1)
CALL IO_READ_FIELD(TZDADINIFILE,'LAT0',ZLAT0_1)
CALL IO_READ_FIELD(TZDADINIFILE,'BETA',ZBETA_1)
!
IF (.NOT.LCARTESIAN) THEN
  CALL IO_READ_FIELD(TZDADINIFILE,'RPK',ZRPK_1)
  CALL IO_READ_FIELD(TZDADINIFILE,'LATORI',ZLATORI_1)
  CALL IO_READ_FIELD(TZDADINIFILE,'LONORI',ZLONORI_1)
  CALL IO_READ_FIELD(TZDADINIFILE,'MASDEV',IMASDEV)
  !
  IF (IMASDEV<=45) THEN
    CALL IO_READ_FIELD(TZDADINIFILE,'LATOR',ZLATORI_1)
    CALL IO_READ_FIELD(TZDADINIFILE,'LONOR',ZLONORI_1)
    ZXHATM = - 0.5 * (ZXHAT_1(1)+ZXHAT_1(2))
    ZYHATM = - 0.5 * (ZYHAT_1(1)+ZYHAT_1(2))
    CALL SM_LATLON(ZLATORI_1,ZLONORI_1,ZXHATM,ZYHATM,ZLATORI,ZLONORI)
    ZLATORI_1 = ZLATORI
    ZLONORI_1 = ZLONORI
  END IF
ENDIF
!
CALL IO_FILE_CLOSE_ll(TZDADINIFILE,CLUOUT,IRESP)
!
!-------------------------------------------------------------------------------
!
!*   3.    Read DAD of spawning file 
!          ------------------------
!
CALL IO_FILE_ADD2LIST(TZDADSPAFILE,TRIM(HDADSPAFILE),'UNKNOWN','READ',KLFINPRAR=0,KLFITYPE=2,KLFIVERB=NVERB)
CALL IO_FILE_OPEN_ll(TZDADSPAFILE,CLUOUT,IRESP)
!
CALL IO_READ_FIELD(TZDADSPAFILE,'IMAX',IIMAX_2)
CALL IO_READ_FIELD(TZDADSPAFILE,'JMAX',IJMAX_2)
CALL IO_READ_FIELD(TZDADSPAFILE,'KMAX',IKMAX_2)
!
IIU_2=IIMAX_2 + 2 * JPHEXT
IJU_2=IJMAX_2 + 2 * JPHEXT
IKU_2=IKMAX_2 + 2 * JPVEXT
!
ALLOCATE(ZXHAT_2(IIU_2))
ALLOCATE(ZYHAT_2(IJU_2))
ALLOCATE(ZZHAT_2(IKU_2))
CALL IO_READ_FIELD(TZDADSPAFILE,'XHAT',ZXHAT_2)
CALL IO_READ_FIELD(TZDADSPAFILE,'YHAT',ZYHAT_2)
CALL IO_READ_FIELD(TZDADSPAFILE,'ZHAT',ZZHAT_2)
!
ALLOCATE(ZZS_2(IIU_2,IJU_2))
CALL IO_READ_FIELD(TZDADSPAFILE,'ZS',ZZS_2)
!
CALL IO_READ_FIELD(TZDADSPAFILE,'LON0',ZLON0_2)
CALL IO_READ_FIELD(TZDADSPAFILE,'LAT0',ZLAT0_2)
CALL IO_READ_FIELD(TZDADSPAFILE,'BETA',ZBETA_2)
!
IF (.NOT.LCARTESIAN) THEN
  CALL IO_READ_FIELD(TZDADSPAFILE,'RPK',ZRPK_2)
  CALL IO_READ_FIELD(TZDADSPAFILE,'LATORI',ZLATORI_2)
  CALL IO_READ_FIELD(TZDADSPAFILE,'LONORI',ZLONORI_2)
  CALL IO_READ_FIELD(TZDADSPAFILE,'MASDEV',IMASDEV)
  !
  IF (IMASDEV<=45) THEN
    CALL IO_READ_FIELD(TZDADSPAFILE,'LATOR',ZLATORI_2)
    CALL IO_READ_FIELD(TZDADSPAFILE,'LONOR',ZLONORI_2)
    ZXHATM = - 0.5 * (ZXHAT_2(1)+ZXHAT_2(2))
    ZYHATM = - 0.5 * (ZYHAT_2(1)+ZYHAT_2(2))
    CALL SM_LATLON(ZLATORI_2,ZLONORI_2,ZXHATM,ZYHATM,ZLATORI,ZLONORI)
    ZLATORI_2 = ZLATORI
    ZLONORI_2 = ZLONORI
  END IF
ENDIF  
!
CALL IO_FILE_CLOSE_ll(TZDADSPAFILE,CLUOUT,IRESP)
!
!-------------------------------------------------------------------------------
!
!*   4. Compare DAD of initial file and DAD of spawning file 
!       ----------------------------------------------------
!
KRESP=0
!
!    4.1 compare domain
!
IF (ZLON0_1 /= ZLON0_2 ) KRESP=-1 
IF (ZLAT0_1 /= ZLAT0_2 ) KRESP=-1 
IF (ZBETA_1 /= ZBETA_2 ) KRESP=-1 

IF (KRESP == -1 ) THEN
  WRITE(ILUOUT,FMT=1) 'domains',TRIM(HDADINIFILE),TRIM(HDADSPAFILE)
  RETURN
ENDIF
!
!    4.2 compare projection and dimension 
!
IF (ZRPK_1 /= ZRPK_2 ) KRESP=-1 
IF (ZLONORI_1 /= ZLONORI_2 ) KRESP=-1 
IF (ZLATORI_1 /= ZLATORI_2 ) KRESP=-1 
IF (KRESP == -1 ) THEN
  WRITE(ILUOUT,FMT=1) 'projections',TRIM(HDADINIFILE),TRIM(HDADSPAFILE)
  WRITE(ILUOUT,*)ZRPK_1,ZRPK_2,ZLONORI_1,ZLONORI_2,ZLATORI_1,ZLATORI_2 
  RETURN
ENDIF
!
IF (IIMAX_1 /= IIMAX_2 ) KRESP=-1 
IF (IJMAX_1 /= IJMAX_2 ) KRESP=-1 
IF (IKMAX_1 /= IKMAX_2 ) KRESP=-1 
IF (KRESP == -1 ) THEN
  WRITE(ILUOUT,FMT=1) 'dimensions',TRIM(HDADINIFILE),TRIM(HDADSPAFILE)
  RETURN
ENDIF
!
!    4.3 compare Position in the conformal plan
!
IF ( MINVAL(ABS(ZXHAT_1(:)-ZXHAT_2(:))) /= 0. ) KRESP=-1
IF ( MINVAL(ABS(ZYHAT_1(:)-ZYHAT_2(:))) /= 0. ) KRESP=-1

IF (KRESP == -1 ) THEN
  WRITE(ILUOUT,FMT=1) 'Locations',TRIM(HDADINIFILE),TRIM(HDADSPAFILE)
  RETURN
ENDIF
!
!    4.4 compare height
!
IF ( MINVAL(ABS(ZZHAT_1(:)-ZZHAT_2(:))) /= 0. ) KRESP=-1

IF (KRESP == -1 ) THEN
  WRITE(ILUOUT,FMT=1) 'vertical grids',TRIM(HDADINIFILE),TRIM(HDADSPAFILE)
  RETURN
ENDIF
!
!    4.5 compare orography
!
IF ( MINVAL(ABS(ZZS_1(:,:)-ZZS_2(:,:))) /= 0. ) KRESP=-1

IF (KRESP == -1 ) THEN
  WRITE(ILUOUT,FMT=1) 'Orographies',TRIM(HDADINIFILE),TRIM(HDADSPAFILE)
  RETURN
ENDIF
!
1  FORMAT('ERROR in COMPARE_DAD: ',A15,' in ',A28,' and ',A28,' are not the same')
!
END SUBROUTINE COMPARE_DAD
