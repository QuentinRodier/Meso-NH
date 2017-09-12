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
USE MODE_FM
USE MODE_FMREAD
USE MODE_GRIDPROJ
USE MODD_LUNIT_n
USE MODD_CONF
USE MODD_PARAMETERS, ONLY: JPHEXT,JPVEXT,NMNHNAMELGTMAX
!
!
IMPLICIT NONE
!
!*       0.1  Declarations of dummy arguments :
!
CHARACTER (LEN=*),      INTENT(IN)  :: HDADINIFILE   ! Name of true DAD FM file
CHARACTER (LEN=*),      INTENT(IN)  :: HDADSPAFILE   ! Name of Replaced DAD FM file
INTEGER,               INTENT(OUT)  :: KRESP !  logical switch 
!
!
!*       0.2  Declarations of local variables :
!
!
INTEGER :: IRESP    ! Return codes in FM routines
INTEGER :: ILUOUT   ! Logical unit number for the output listing 
INTEGER :: ININAR   ! Number of articles present in the LFIFM file
INTEGER :: IMASDEV
!
INTEGER             :: IIU_1,IIU_2    ! Upper dimension in x direction
INTEGER             :: IJU_1,IJU_2    ! Upper dimension in y direction
INTEGER             :: IKU_1,IKU_2    ! Upper dimension in z direction
!
CHARACTER (LEN=NMNHNAMELGTMAX) :: YRECFM
INTEGER            :: ILENCH, IGRID
CHARACTER (LEN=100):: YCOMMENT
CHARACTER(LEN=2)    :: YDIR   ! Type  of the data field in LFIFM file
!
REAL                :: ZLON0_1,ZLAT0_1,ZBETA_1,ZRPK_1,ZLONORI_1,ZLATORI_1
REAL                :: ZLON0_2,ZLAT0_2,ZBETA_2,ZRPK_2,ZLONORI_2,ZLATORI_2
REAL, DIMENSION(:),ALLOCATABLE  :: ZXHAT_1,ZYHAT_1,XZHAT_1 ! Position x,y,height
REAL, DIMENSION(:),ALLOCATABLE  :: ZXHAT_2,ZYHAT_2,XZHAT_2 ! Position x,y,height
REAL, DIMENSION(:,:),ALLOCATABLE  :: ZZS_1  ! orography
REAL, DIMENSION(:,:),ALLOCATABLE  :: ZZS_2  ! orography
!
INTEGER             :: IIMAX_1,IJMAX_1,IKMAX_1
INTEGER             :: IIMAX_2,IJMAX_2,IKMAX_2
!
!-------------------------------------------------------------------------------
REAL :: ZLATORI, ZLONORI, ZXHATM, ZYHATM
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
CALL FMOPEN_ll(HDADINIFILE,'READ',CLUOUT,0,2,NVERB,ININAR,IRESP)
!
YRECFM='IMAX'
CALL FMREAD(HDADINIFILE,YRECFM,CLUOUT,'--',IIMAX_1,IGRID,ILENCH,YCOMMENT,IRESP)
!
YRECFM='JMAX'
CALL FMREAD(HDADINIFILE,YRECFM,CLUOUT,'--',IJMAX_1,IGRID,ILENCH,YCOMMENT,IRESP)
!
YRECFM='KMAX'
CALL FMREAD(HDADINIFILE,YRECFM,CLUOUT,'--',IKMAX_1,IGRID,ILENCH,YCOMMENT,IRESP)
!
IIU_1=IIMAX_1 + 2 * JPHEXT
IJU_1=IJMAX_1 + 2 * JPHEXT
IKU_1=IKMAX_1 + 2 * JPVEXT
!
ALLOCATE(ZXHAT_1(IIU_1))
YRECFM='XHAT'
YDIR='XX'
CALL FMREAD(HDADINIFILE,YRECFM,CLUOUT,'--',ZXHAT_1,IGRID,ILENCH,YCOMMENT,IRESP)
!
ALLOCATE(ZYHAT_1(IJU_1))
YRECFM='YHAT'
YDIR='YY'
CALL FMREAD(HDADINIFILE,YRECFM,CLUOUT,'--',ZYHAT_1,IGRID,ILENCH,YCOMMENT,IRESP)
!
ALLOCATE(XZHAT_1(IKU_1))
YRECFM='ZHAT'
CALL FMREAD(HDADINIFILE,YRECFM,CLUOUT,'--',XZHAT_1,IGRID,ILENCH,YCOMMENT,IRESP)
!
ALLOCATE(ZZS_1(IIU_1,IJU_1))
YRECFM='ZS'
YDIR='XY'
CALL FMREAD(HDADINIFILE,YRECFM,CLUOUT,YDIR,ZZS_1,IGRID,ILENCH,YCOMMENT,IRESP)
!
YRECFM='LON0'
CALL FMREAD(HDADINIFILE,YRECFM,CLUOUT,'--',ZLON0_1,IGRID,ILENCH,YCOMMENT,IRESP)
!
YRECFM='LAT0'
CALL FMREAD(HDADINIFILE,YRECFM,CLUOUT,'--',ZLAT0_1,IGRID,ILENCH,YCOMMENT,IRESP)
!
YRECFM='BETA'
CALL FMREAD(HDADINIFILE,YRECFM,CLUOUT,'--',ZBETA_1,IGRID,ILENCH,YCOMMENT,IRESP)
!
IF (.NOT.LCARTESIAN) THEN
  YRECFM='RPK'
  CALL FMREAD(HDADINIFILE,YRECFM,CLUOUT,'--',ZRPK_1,IGRID,ILENCH,YCOMMENT,IRESP)
  !
  YRECFM='LATORI'
  CALL FMREAD(HDADINIFILE,YRECFM,CLUOUT,'--',ZLATORI_1,IGRID,ILENCH,YCOMMENT,IRESP)
  YRECFM='LONORI'
  CALL FMREAD(HDADINIFILE,YRECFM,CLUOUT,'--',ZLONORI_1,IGRID,ILENCH,YCOMMENT,IRESP)
  !
  YRECFM='MASDEV'
  CALL FMREAD(HDADINIFILE,YRECFM,CLUOUT,'--',IMASDEV,IGRID,ILENCH,YCOMMENT,IRESP)
  !
  IF (IMASDEV<=45) THEN
    CALL FMREAD(HDADINIFILE,'LATOR',CLUOUT,'--',ZLATORI_1,IGRID,ILENCH,YCOMMENT,IRESP)
    CALL FMREAD(HDADINIFILE,'LONOR',CLUOUT,'--',ZLONORI_1,IGRID,ILENCH,YCOMMENT,IRESP)
    ZXHATM = - 0.5 * (ZXHAT_1(1)+ZXHAT_1(2))
    ZYHATM = - 0.5 * (ZYHAT_1(1)+ZYHAT_1(2))
    CALL SM_LATLON(ZLATORI_1,ZLONORI_1,ZXHATM,ZYHATM,ZLATORI,ZLONORI)
    ZLATORI_1 = ZLATORI
    ZLONORI_1 = ZLONORI
  END IF
ENDIF
!
CALL FMCLOS_ll(HDADINIFILE,'KEEP',CLUOUT,IRESP)
!
!-------------------------------------------------------------------------------
!
!*   3.    Read DAD of spawning file 
!          ------------------------
!
CALL FMOPEN_ll(HDADSPAFILE,'READ',CLUOUT,0,2,NVERB,ININAR,IRESP)
!
YRECFM='IMAX'
CALL FMREAD(HDADSPAFILE,YRECFM,CLUOUT,'--',IIMAX_2,IGRID,ILENCH,YCOMMENT,IRESP)
!
YRECFM='JMAX'
CALL FMREAD(HDADSPAFILE,YRECFM,CLUOUT,'--',IJMAX_2,IGRID,ILENCH,YCOMMENT,IRESP)
!
YRECFM='KMAX'
CALL FMREAD(HDADSPAFILE,YRECFM,CLUOUT,'--',IKMAX_2,IGRID,ILENCH,YCOMMENT,IRESP)
!
IIU_2=IIMAX_2 + 2 * JPHEXT
IJU_2=IJMAX_2 + 2 * JPHEXT
IKU_2=IKMAX_2 + 2 * JPVEXT
!
ALLOCATE(ZXHAT_2(IIU_2))
YRECFM='XHAT'
YDIR='XX'
CALL FMREAD(HDADSPAFILE,YRECFM,CLUOUT,'--',ZXHAT_2,IGRID,ILENCH,YCOMMENT,IRESP)
!
ALLOCATE(ZYHAT_2(IJU_2))
YRECFM='YHAT'
YDIR='YY'
CALL FMREAD(HDADSPAFILE,YRECFM,CLUOUT,'--',ZYHAT_2,IGRID,ILENCH,YCOMMENT,IRESP)
!
ALLOCATE(XZHAT_2(IKU_2))
YRECFM='ZHAT'
YDIR='--'
CALL FMREAD(HDADSPAFILE,YRECFM,CLUOUT,'--',XZHAT_2,IGRID,ILENCH,YCOMMENT,IRESP)
!
ALLOCATE(ZZS_2(IIU_2,IJU_2))
YRECFM='ZS'
YDIR='XY'
CALL FMREAD(HDADSPAFILE,YRECFM,CLUOUT,YDIR,ZZS_2,IGRID,ILENCH,YCOMMENT,IRESP)
!
YRECFM='LON0'
YDIR='--'
CALL FMREAD(HDADSPAFILE,YRECFM,CLUOUT,'--',ZLON0_2,IGRID,ILENCH,YCOMMENT,IRESP)
!
YRECFM='LAT0'
CALL FMREAD(HDADSPAFILE,YRECFM,CLUOUT,'--',ZLAT0_2,IGRID,ILENCH,YCOMMENT,IRESP)
!
YRECFM='BETA'
CALL FMREAD(HDADSPAFILE,YRECFM,CLUOUT,'--',ZBETA_2,IGRID,ILENCH,YCOMMENT,IRESP)
!
IF (.NOT.LCARTESIAN) THEN
  YRECFM='RPK'
  CALL FMREAD(HDADSPAFILE,YRECFM,CLUOUT,'--',ZRPK_2,IGRID,ILENCH,YCOMMENT,IRESP)
  !
  YRECFM='LATORI'
  CALL FMREAD(HDADSPAFILE,YRECFM,CLUOUT,'--',ZLATORI_2,IGRID,ILENCH,YCOMMENT,IRESP)
  ! 
  YRECFM='LONORI'
  CALL FMREAD(HDADSPAFILE,YRECFM,CLUOUT,'--',ZLONORI_2,IGRID,ILENCH,YCOMMENT,IRESP)
  !
  YRECFM='MASDEV' 
  CALL FMREAD(HDADSPAFILE,YRECFM,CLUOUT,'--',IMASDEV,IGRID,ILENCH,YCOMMENT,IRESP)
  !
  IF (IMASDEV<=45) THEN
    CALL FMREAD(HDADSPAFILE,'LATOR',CLUOUT,'--',ZLATORI_2,IGRID,ILENCH,YCOMMENT,IRESP)
    CALL FMREAD(HDADSPAFILE,'LONOR',CLUOUT,'--',ZLONORI_2,IGRID,ILENCH,YCOMMENT,IRESP)
    ZXHATM = - 0.5 * (ZXHAT_2(1)+ZXHAT_2(2))
    ZYHATM = - 0.5 * (ZYHAT_2(1)+ZYHAT_2(2))
    CALL SM_LATLON(ZLATORI_2,ZLONORI_2,ZXHATM,ZYHATM,ZLATORI,ZLONORI)
    ZLATORI_2 = ZLATORI
    ZLONORI_2 = ZLONORI
  END IF
ENDIF  
!
CALL FMCLOS_ll(HDADSPAFILE,'KEEP',CLUOUT,IRESP)
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
IF ( MINVAL(ABS(XZHAT_1(:)-XZHAT_2(:))) /= 0. ) KRESP=-1

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
