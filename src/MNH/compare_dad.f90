!MNH_LIC Copyright 2004-2021 CNRS, Meteo-France and Universite Paul Sabatier
!MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
!MNH_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!MNH_LIC for details. version 1.
!-----------------------------------------------------------------
!######################## 
MODULE MODI_COMPARE_DAD
!########################
!
INTERFACE
!
      SUBROUTINE COMPARE_DAD (HDADINIFILE,HDADSPAFILE,KRESP)
!
USE MODD_PARAMETERS, ONLY: NFILENAMELGTMAX

CHARACTER (LEN=NFILENAMELGTMAX), INTENT(IN)  :: HDADINIFILE   ! Name of true DAD FM file
CHARACTER (LEN=NFILENAMELGTMAX), INTENT(IN)  :: HDADSPAFILE   ! Name of Replaced DAD FM file
INTEGER,                         INTENT(OUT) :: KRESP !  logical switch
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
!!      IO_File_open  : to open a FM-file (DESFM + LFIFM)
!!      IO_File_close : to close a FM-file (DESFM + LFIFM)
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
!!  Philippe Wautelet: 05/2016-04/2018: new data structures and calls for I/O
!  P. Wautelet 07/02/2019: force TYPE to a known value for IO_File_add2list
!-------------------------------------------------------------------------------
!
!*       0.     DECLARATIONS
!               ------------
!
!
USE MODD_CONF
use modd_field,            only: tfieldmetadata, tfieldlist
USE MODD_IO,               ONLY: TFILEDATA
USE MODD_LUNIT_n,          ONLY: TLUOUT
USE MODD_PARAMETERS,       ONLY: JPHEXT, JPVEXT, NFILENAMELGTMAX, NMNHNAMELGTMAX
!
use mode_field,            only: Find_field_id_from_mnhname
USE MODE_IO_FILE,          only: IO_File_close, IO_File_open
USE MODE_IO_FIELD_READ,    only: IO_Field_read
USE MODE_GRIDPROJ
USE MODE_IO_MANAGE_STRUCT, ONLY : IO_File_add2list
!
!
IMPLICIT NONE
!
!*       0.1  Declarations of dummy arguments :
!
CHARACTER (LEN=NFILENAMELGTMAX), INTENT(IN)  :: HDADINIFILE   ! Name of true DAD FM file
CHARACTER (LEN=NFILENAMELGTMAX), INTENT(IN)  :: HDADSPAFILE   ! Name of Replaced DAD FM file
INTEGER,                         INTENT(OUT) :: KRESP !  logical switch
!
!
!*       0.2  Declarations of local variables :
!
!
INTEGER :: IID
INTEGER :: IRESP    ! Return codes in FM routines
INTEGER :: ILUOUT   ! Logical unit number for the output listing 
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
TYPE(TFIELDMETADATA)    :: TZFIELD
TYPE(TFILEDATA),POINTER :: TZDADINIFILE => NULL()
TYPE(TFILEDATA),POINTER :: TZDADSPAFILE => NULL()
!-------------------------------------------------------------------------------
!
!*   1.    INITIALIZATIONS
!          ---------------
!
ILUOUT = TLUOUT%NLU
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
CALL IO_File_add2list(TZDADINIFILE,TRIM(HDADINIFILE),'MNH','READ',KLFITYPE=2,KLFIVERB=NVERB)
CALL IO_File_open(TZDADINIFILE)
!
CALL IO_Field_read(TZDADINIFILE,'IMAX',IIMAX_1)
CALL IO_Field_read(TZDADINIFILE,'JMAX',IJMAX_1)
CALL IO_Field_read(TZDADINIFILE,'KMAX',IKMAX_1)
!
IIU_1=IIMAX_1 + 2 * JPHEXT
IJU_1=IJMAX_1 + 2 * JPHEXT
IKU_1=IKMAX_1 + 2 * JPVEXT
!
ALLOCATE(ZXHAT_1(IIU_1))
ALLOCATE(ZYHAT_1(IJU_1))
ALLOCATE(ZZHAT_1(IKU_1))
CALL IO_Field_read(TZDADINIFILE,'XHAT',ZXHAT_1)
CALL IO_Field_read(TZDADINIFILE,'YHAT',ZYHAT_1)
CALL IO_Field_read(TZDADINIFILE,'ZHAT',ZZHAT_1)
!
ALLOCATE(ZZS_1(IIU_1,IJU_1))
CALL IO_Field_read(TZDADINIFILE,'ZS',ZZS_1)
!
CALL IO_Field_read(TZDADINIFILE,'LON0',ZLON0_1)
CALL IO_Field_read(TZDADINIFILE,'LAT0',ZLAT0_1)
CALL IO_Field_read(TZDADINIFILE,'BETA',ZBETA_1)
!
IF (.NOT.LCARTESIAN) THEN
  CALL IO_Field_read(TZDADINIFILE,'RPK',ZRPK_1)
  CALL IO_Field_read(TZDADINIFILE,'LATORI',ZLATORI_1)
  CALL IO_Field_read(TZDADINIFILE,'LONORI',ZLONORI_1)
  !
  IF (TZDADINIFILE%NMNHVERSION(1)<4 .OR. (TZDADINIFILE%NMNHVERSION(1)==4 .AND. TZDADINIFILE%NMNHVERSION(2)<=5) ) THEN
    CALL FIND_FIELD_ID_FROM_MNHNAME('LONORI',IID,IRESP)
    TZFIELD = TFIELDMETADATA( TFIELDLIST(IID) )
    TZFIELD%CMNHNAME = 'LONOR'
    CALL IO_Field_read(TZDADINIFILE,TZFIELD,ZLONORI_1)
    !
    CALL FIND_FIELD_ID_FROM_MNHNAME('LATORI',IID,IRESP)
    TZFIELD = TFIELDMETADATA( TFIELDLIST(IID) )
    TZFIELD%CMNHNAME = 'LATOR'
    CALL IO_Field_read(TZDADINIFILE,TZFIELD,ZLATORI_1)
    !
    ZXHATM = - 0.5 * (ZXHAT_1(1)+ZXHAT_1(2))
    ZYHATM = - 0.5 * (ZYHAT_1(1)+ZYHAT_1(2))
    CALL SM_LATLON(ZLATORI_1,ZLONORI_1,ZXHATM,ZYHATM,ZLATORI,ZLONORI)
    ZLATORI_1 = ZLATORI
    ZLONORI_1 = ZLONORI
  END IF
ENDIF
!
CALL IO_File_close(TZDADINIFILE)
!
!-------------------------------------------------------------------------------
!
!*   3.    Read DAD of spawning file 
!          ------------------------
!
CALL IO_File_add2list(TZDADSPAFILE,TRIM(HDADSPAFILE),'MNH','READ',KLFITYPE=2,KLFIVERB=NVERB)
CALL IO_File_open(TZDADSPAFILE)
!
CALL IO_Field_read(TZDADSPAFILE,'IMAX',IIMAX_2)
CALL IO_Field_read(TZDADSPAFILE,'JMAX',IJMAX_2)
CALL IO_Field_read(TZDADSPAFILE,'KMAX',IKMAX_2)
!
IIU_2=IIMAX_2 + 2 * JPHEXT
IJU_2=IJMAX_2 + 2 * JPHEXT
IKU_2=IKMAX_2 + 2 * JPVEXT
!
ALLOCATE(ZXHAT_2(IIU_2))
ALLOCATE(ZYHAT_2(IJU_2))
ALLOCATE(ZZHAT_2(IKU_2))
CALL IO_Field_read(TZDADSPAFILE,'XHAT',ZXHAT_2)
CALL IO_Field_read(TZDADSPAFILE,'YHAT',ZYHAT_2)
CALL IO_Field_read(TZDADSPAFILE,'ZHAT',ZZHAT_2)
!
ALLOCATE(ZZS_2(IIU_2,IJU_2))
CALL IO_Field_read(TZDADSPAFILE,'ZS',ZZS_2)
!
CALL IO_Field_read(TZDADSPAFILE,'LON0',ZLON0_2)
CALL IO_Field_read(TZDADSPAFILE,'LAT0',ZLAT0_2)
CALL IO_Field_read(TZDADSPAFILE,'BETA',ZBETA_2)
!
IF (.NOT.LCARTESIAN) THEN
  CALL IO_Field_read(TZDADSPAFILE,'RPK',ZRPK_2)
  CALL IO_Field_read(TZDADSPAFILE,'LATORI',ZLATORI_2)
  CALL IO_Field_read(TZDADSPAFILE,'LONORI',ZLONORI_2)
  !
  IF (TZDADSPAFILE%NMNHVERSION(1)<4 .OR. (TZDADSPAFILE%NMNHVERSION(1)==4 .AND. TZDADSPAFILE%NMNHVERSION(2)<=5)) THEN
    CALL FIND_FIELD_ID_FROM_MNHNAME('LONORI',IID,IRESP)
    TZFIELD = TFIELDMETADATA( TFIELDLIST(IID) )
    TZFIELD%CMNHNAME = 'LONOR'
    CALL IO_Field_read(TZDADSPAFILE,TZFIELD,ZLONORI_2)
    !
    CALL FIND_FIELD_ID_FROM_MNHNAME('LATORI',IID,IRESP)
    TZFIELD = TFIELDMETADATA( TFIELDLIST(IID) )
    TZFIELD%CMNHNAME = 'LATOR'
    CALL IO_Field_read(TZDADSPAFILE,TZFIELD,ZLATORI_2)
    !
    ZXHATM = - 0.5 * (ZXHAT_2(1)+ZXHAT_2(2))
    ZYHATM = - 0.5 * (ZYHAT_2(1)+ZYHAT_2(2))
    CALL SM_LATLON(ZLATORI_2,ZLONORI_2,ZXHATM,ZYHATM,ZLATORI,ZLONORI)
    ZLATORI_2 = ZLATORI
    ZLONORI_2 = ZLONORI
  END IF
ENDIF  
!
CALL IO_File_close(TZDADSPAFILE)
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
