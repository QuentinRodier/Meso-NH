!MNH_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
!MNH_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!MNH_LIC for details. version 1.
!     #############################################################
      SUBROUTINE READ_SURFX0_MNH(HREC,PFIELD,KRESP,HCOMMENT)
!     #############################################################
!
!!****  *READX0* - routine to read a real scalar
!!
!!    PURPOSE
!!    -------
!
!       The purpose of READX0 is
!
!!**  METHOD
!!    ------
!!
!!    EXTERNAL
!!    --------
!!
!!
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!
!!
!!    REFERENCE
!!    ---------
!!
!!
!!    AUTHOR
!!    ------
!!
!!      S.Malardel      *METEO-FRANCE*
!!
!!    MODIFICATIONS
!!    -------------
!!
!!      original                                                     01/08/03
!!      10/10/2011 J.Escobar & G.Tanguy change BUGFIX/MNH to BUG/SURFEX version control
!----------------------------------------------------------------------------
!
!*      0.    DECLARATIONS
!             ------------
!
USE MODD_CONF, ONLY : CPROGRAM
USE MODD_GRID, ONLY: XRPK,XBETA,XLAT0,XLON0
USE MODD_PARAMETERS, ONLY: JPHEXT, XUNDEF
!
USE MODE_FM
USE MODE_FMREAD
USE MODE_GRIDPROJ
!
USE MODD_IO_SURF_MNH,        ONLY : COUT, CFILE, NLUOUT
!
USE MODI_GET_SURF_UNDEF
!
IMPLICIT NONE
!
!*      0.1   Declarations of arguments
!
CHARACTER(LEN=16),  INTENT(IN)  :: HREC     ! name of the article to be read
REAL,               INTENT(OUT) :: PFIELD   ! the real scalar to be read
INTEGER,            INTENT(OUT) :: KRESP    ! KRESP  : return-code if a problem appears
CHARACTER(LEN=100), INTENT(OUT) :: HCOMMENT ! comment
!
!*      0.2   Declarations of local variables
!
INTEGER           :: IGRID          ! IGRID : grid indicator
INTEGER           :: ILENCH         ! ILENCH : length of comment string

INTEGER           :: IMASDEV
INTEGER           :: IRESP
INTEGER           :: IIMAX,IJMAX
REAL,DIMENSION(:), ALLOCATABLE :: ZXHAT,ZYHAT
REAL              :: ZLATOR,ZLONOR,ZXHATM,ZYHATM,ZLATORI,ZLONORI
REAL              :: ZRPK, ZBETA, ZLAT0, ZLON0
CHARACTER(LEN=100):: YCOMMENT ! comment
!-------------------------------------------------------------------------------
!
IF (HREC=='LONORI' .OR. HREC=='LATORI') THEN
  CALL FMREAD(CFILE,'MASDEV',COUT,'--',IMASDEV,IGRID,ILENCH,HCOMMENT,KRESP)
  IF (IMASDEV<=45) THEN
      ZLATORI = XUNDEF
      ZLONORI = XUNDEF
      !* saves projection parameters of MODD_GRID
      ZLAT0 = XLAT0
      ZLON0 = XLON0
      ZRPK  = XRPK
      ZBETA = XBETA
      !* reads projection and grid data in the file
      CALL FMREAD(CFILE,'LAT0',COUT,'--',XLAT0,IGRID,ILENCH,HCOMMENT,KRESP)
      CALL FMREAD(CFILE,'LON0',COUT,'--',XLON0,IGRID,ILENCH,HCOMMENT,KRESP)
      CALL FMREAD(CFILE,'RPK',COUT,'--',XRPK,IGRID,ILENCH,HCOMMENT,KRESP)
      CALL FMREAD(CFILE,'BETA',COUT,'--',XBETA,IGRID,ILENCH,HCOMMENT,KRESP)
      !
      CALL FMREAD(CFILE,'IMAX',COUT,'--',IIMAX,IGRID,ILENCH,YCOMMENT,IRESP)
      CALL FMREAD(CFILE,'JMAX',COUT,'--',IJMAX,IGRID,ILENCH,YCOMMENT,IRESP)
      ALLOCATE(ZXHAT(IIMAX+2*JPHEXT),ZYHAT(IJMAX+2*JPHEXT))
      CALL FMREAD(CFILE,'XHAT',COUT,'--',ZXHAT,IGRID,ILENCH,YCOMMENT,IRESP)
      CALL FMREAD(CFILE,'YHAT',COUT,'--',ZYHAT,IGRID,ILENCH,YCOMMENT,IRESP)
      CALL FMREAD(CFILE,'LATOR',COUT,'--',ZLATOR,IGRID,ILENCH,YCOMMENT,IRESP)
      CALL FMREAD(CFILE,'LONOR',COUT,'--',ZLONOR,IGRID,ILENCH,HCOMMENT,KRESP)
      ZXHATM = - 0.5 * (ZXHAT(1)+ZXHAT(2))
      ZYHATM = - 0.5 * (ZYHAT(1)+ZYHAT(2))
      DEALLOCATE(ZXHAT,ZYHAT)
      !* computes origin
      CALL SM_LATLON(ZLATOR,ZLONOR,ZXHATM,ZYHATM,ZLATORI,ZLONORI)
      IF (HREC=='LONORI') PFIELD = ZLONORI
      IF (HREC=='LATORI') PFIELD = ZLATORI
      !* restores projection parameters in module MODD_GRID
      XLAT0 = ZLAT0
      XLON0 = ZLON0
      XRPK  = ZRPK
      XBETA = ZBETA
      RETURN
  END IF
END IF

!-------------------------------------------------------------------------------

CALL FMREAD(CFILE,HREC,COUT,'--',PFIELD,IGRID,ILENCH,HCOMMENT,KRESP)

IF (KRESP /=0) THEN
  WRITE(NLUOUT,*) 'WARNING'
  WRITE(NLUOUT,*) '-------'
  WRITE(NLUOUT,*) 'error when reading article ', HREC,'KRESP=',KRESP
  WRITE(NLUOUT,*) 'default value may be used, who knows???'
  WRITE(NLUOUT,*) ' '
ENDIF
!-------------------------------------------------------------------------------
END SUBROUTINE READ_SURFX0_MNH
!
!     #############################################################
      SUBROUTINE READ_SURFX1_MNH(HREC,KL,PFIELD,KRESP,HCOMMENT,HDIR)
!     #############################################################
!
!!****  *READX1* - routine to fill a real 1D array for the externalised surface
!!
!!    PURPOSE
!!    -------
!
!       The purpose of READ_SURFX1 is
!
!!**  METHOD
!!    ------
!!
!!    EXTERNAL
!!    --------
!!
!!
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!
!!
!!    REFERENCE
!!    ---------
!!
!!
!!    AUTHOR
!!    ------
!!
!!      S.Malardel      *METEO-FRANCE*
!!
!!    MODIFICATIONS
!!    -------------
!!
!!      original                                                     01/08/03
!----------------------------------------------------------------------------
!
!*      0.    DECLARATIONS
!             ------------
!
USE MODE_FM
USE MODE_FMREAD
USE MODE_ll
USE MODE_IO_ll
!
USE MODD_CST,         ONLY : XPI
!
USE MODD_IO_SURF_MNH, ONLY : COUT, CFILE , NLUOUT,  NMASK, &
                             NIU, NJU, NIB, NJB, NIE, NJE, &
                             NIU_ALL, NJU_ALL, NIB_ALL,    &
                             NJB_ALL, NIE_ALL, NJE_ALL,    &
                             NMASK_ALL
USE MODD_PARAMETERS, ONLY: XUNDEF
!
USE MODI_PACK_2D_1D
!
USE MODI_GET_SURF_UNDEF
!
IMPLICIT NONE
!
!*      0.1   Declarations of arguments
!
CHARACTER(LEN=16),   INTENT(IN) :: HREC     ! name of the article to be read
INTEGER,             INTENT(IN) :: KL       !  number of points
REAL, DIMENSION(KL), INTENT(OUT):: PFIELD   ! array containing the data field
INTEGER,             INTENT(OUT):: KRESP    ! KRESP  : return-code if a problem appears
CHARACTER(LEN=100),  INTENT(OUT):: HCOMMENT ! comment
CHARACTER(LEN=1),    INTENT(IN) :: HDIR     ! type of field :
!                                           ! 'H' for HOR : with hor. dim.; and  distributed.
!                                           ! 'A' for ALL : with hor. dim.; and not distributed.
!                                           ! '-' : no horizontal dim.

!
!*      0.2   Declarations of local variables
!
INTEGER           :: IGRID          ! IGRID : grid indicator
INTEGER           :: ILENCH         ! ILENCH : length of comment string
INTEGER           :: JI, JJ         ! loop counters

REAL, DIMENSION(:,:), ALLOCATABLE :: ZWORK  ! work array read in the file
REAL, DIMENSION(:),   ALLOCATABLE :: ZWORK1D! work array read in the file
REAL                              :: ZW     ! work value

INTEGER           :: IMASDEV
CHARACTER(LEN=20) :: YREC
CHARACTER(LEN=2)  :: YSTORAGE_TYPE
!
INTEGER           :: IIU, IJU, IIB, IJB, IIE, IJE ! dimensions of horizontal fields
INTEGER, DIMENSION(:), ALLOCATABLE :: IMASK       ! mask for packing
REAL              :: ZUNDEF         ! undefined value in SURFEX
!-------------------------------------------------------------------------------
!
KRESP = 0
!
IF (HDIR=='A') THEN
  IIU = NIU_ALL
  IJU = NJU_ALL
  IIB = NIB_ALL
  IJB = NJB_ALL
  IIE = NIE_ALL
  IJE = NJE_ALL
  ALLOCATE(IMASK(SIZE(NMASK_ALL)))
  IMASK = NMASK_ALL
ELSE
  IIU = NIU
  IJU = NJU
  IIB = NIB
  IJB = NJB
  IIE = NIE
  IJE = NJE
  ALLOCATE(IMASK(SIZE(NMASK)))
  IMASK = NMASK
END IF
!
!*       2.    On traite d'abord des cas particuliers
!
IF (HREC=='LAT') THEN

  CALL FMREAD(CFILE,'LAT0',COUT,'--',ZW,IGRID,ILENCH,HCOMMENT,KRESP)
  PFIELD(:) = ZW

ELSE IF (HREC=='LON') THEN

  CALL FMREAD(CFILE,'LON0',COUT,'--',ZW,IGRID,ILENCH,HCOMMENT,KRESP)
  PFIELD(:) = ZW

ELSE IF (HREC=='MESH_SIZE') THEN

  PFIELD(:) = 0.
  HCOMMENT = ' '

ELSE IF (HREC=='XX') THEN
!! reading of a 1D field along X in the file
  ALLOCATE(ZWORK1D(IIU))
  ALLOCATE(ZWORK  (IIU,IJU))
  ZWORK(:,:) = 0.
  IF (HDIR/='A') THEN
    CALL FMREAD(CFILE,'XHAT',COUT,'XX',ZWORK1D,IGRID,ILENCH,HCOMMENT,KRESP)
  ELSE
    CALL FMREAD(CFILE,'XHAT',COUT,'--',ZWORK1D,IGRID,ILENCH,HCOMMENT,KRESP)
  END IF
  DO JJ = 1,IJU
    ZWORK(IIB:IIE,JJ) = 0.5 * ZWORK1D(IIB:IIE) + 0.5 * ZWORK1D(IIB+1:IIE+1)
  END DO
  CALL PACK_2D_1D(IMASK,ZWORK(IIB:IIE,IJB:IJE),PFIELD)
  DEALLOCATE(ZWORK1D)
  DEALLOCATE(ZWORK  )
ELSE IF (HREC=='DX') THEN
!! reading of a 1D field along X in the file
  ALLOCATE(ZWORK1D(IIU))
  ALLOCATE(ZWORK  (IIU,IJU))
  ZWORK(:,:) = 0.
  IF (HDIR/='A') THEN
    CALL FMREAD(CFILE,'XHAT',COUT,'XX',ZWORK1D,IGRID,ILENCH,HCOMMENT,KRESP)
  ELSE
    CALL FMREAD(CFILE,'XHAT',COUT,'--',ZWORK1D,IGRID,ILENCH,HCOMMENT,KRESP)
  END IF
  DO JJ = 1,IJU
    ZWORK(IIB:IIE,JJ) = - ZWORK1D(IIB:IIE) + ZWORK1D(IIB+1:IIE+1)
  END DO
  CALL PACK_2D_1D(IMASK,ZWORK(IIB:IIE,IJB:IJE),PFIELD)
  DEALLOCATE(ZWORK1D)
  DEALLOCATE(ZWORK  )
ELSE IF (HREC=='YY') THEN
!! reading of a 1D field along Y in the file
  ALLOCATE(ZWORK1D(IJU))
  ALLOCATE(ZWORK  (IIU,IJU))
  ZWORK(:,:) = 0.
  IF (HDIR/='A') THEN
    CALL FMREAD(CFILE,'YHAT',COUT,'YY',ZWORK1D,IGRID,ILENCH,HCOMMENT,KRESP)
  ELSE
    CALL FMREAD(CFILE,'YHAT',COUT,'--',ZWORK1D,IGRID,ILENCH,HCOMMENT,KRESP)
  END IF
  DO JI = 1,IIU
    ZWORK(JI,IJB:IJE) = 0.5 * ZWORK1D(IJB:IJE) + 0.5 * ZWORK1D(IJB+1:IJE+1)
  END DO
  CALL PACK_2D_1D(IMASK,ZWORK(IIB:IIE,IJB:IJE),PFIELD)
  DEALLOCATE(ZWORK1D)
  DEALLOCATE(ZWORK  )
ELSE IF (HREC=='DY') THEN
!! reading of a 1D field along Y in the file
  ALLOCATE(ZWORK1D(IJU))
  ALLOCATE(ZWORK  (IIU,IJU))
  ZWORK(:,:) = 0.
  IF (HDIR/='A') THEN
    CALL FMREAD(CFILE,'YHAT',COUT,'YY',ZWORK1D,IGRID,ILENCH,HCOMMENT,KRESP)
  ELSE
    CALL FMREAD(CFILE,'YHAT',COUT,'--',ZWORK1D,IGRID,ILENCH,HCOMMENT,KRESP)
  END IF
  DO JI = 1,IIU
    ZWORK(JI,IJB:IJE) = - ZWORK1D(IJB:IJE) + ZWORK1D(IJB+1:IJE+1)
  END DO
  CALL PACK_2D_1D(IMASK,ZWORK(IIB:IIE,IJB:IJE),PFIELD)
  DEALLOCATE(ZWORK1D)
  DEALLOCATE(ZWORK  )
!
ELSE
!
!! Reading of a 2D fields, masked and packed into 1D vector
!
  YREC = ' '
  YREC(1:LEN(HREC)) = HREC
  IF (HREC(1:8)=='Q_CANYON') THEN
    CALL FMREAD(CFILE,'MASDEV',COUT,'--',IMASDEV,IGRID,ILENCH,HCOMMENT,KRESP)
    IF (IMASDEV<=45) THEN
      CALL FMREAD(CFILE,'STORAGE_TYPE',COUT,'--',YSTORAGE_TYPE,IGRID,ILENCH,HCOMMENT,KRESP)
      IF (YSTORAGE_TYPE=='TT') THEN
        PFIELD = 0.
        DEALLOCATE(IMASK)
        RETURN
      ELSE
        YREC = 'R_CANYON            '
      END IF
    END IF
  END IF
  IF (HREC(1:8)=='T_CANYON') THEN
    CALL FMREAD(CFILE,'MASDEV',COUT,'--',IMASDEV,IGRID,ILENCH,HCOMMENT,KRESP)
    IF (IMASDEV<=45) THEN
      CALL FMREAD(CFILE,'STORAGE_TYPE',COUT,'--',YSTORAGE_TYPE,IGRID,ILENCH,HCOMMENT,KRESP)
      IF (YSTORAGE_TYPE=='TT') YREC = 'T_ROAD1             '
    END IF
  END IF
  IF (HREC(1:7)=='SSO_DIR') THEN
    CALL FMREAD(CFILE,'MASDEV',COUT,'--',IMASDEV,IGRID,ILENCH,HCOMMENT,KRESP)
    IF (IMASDEV<=45) YREC = 'SSO_DIRECTION       '
  END IF
!
  ALLOCATE(ZWORK(IIU,IJU))
!

  IF (HDIR=='H') THEN
    CALL FMREAD(CFILE,YREC,COUT,'XY',ZWORK(:,:),IGRID,ILENCH,HCOMMENT,KRESP)
  ELSEIF (HDIR=='A') THEN
    CALL FMREAD(CFILE,YREC,COUT,'--',ZWORK(:,:),IGRID,ILENCH,HCOMMENT,KRESP)
  ELSE
    CALL FMREAD(CFILE,YREC,COUT,'--',PFIELD(:),IGRID,ILENCH,HCOMMENT,KRESP)
  END IF
!
  IF (KRESP /=0) THEN
    WRITE(NLUOUT,*) 'WARNING'
    WRITE(NLUOUT,*) '-------'
    WRITE(NLUOUT,*) 'error when reading article ', HREC,'KRESP=',KRESP
    WRITE(NLUOUT,*) 'default value may be used, who knows???'
    WRITE(NLUOUT,*) ' '
  ELSE IF (HDIR=='H' .OR. HDIR=='A') THEN
    CALL PACK_2D_1D(IMASK,ZWORK(IIB:IIE,IJB:IJE),PFIELD)
    CALL GET_SURF_UNDEF(ZUNDEF)
!================================================
! 13/03/2009 : G. TANGUY
! on supprime le test sur lesvaleurs indéfinies 
! pour l'orographie pour que l'altitude 999 m 
! soit autorisée
    IF (HREC(1:2)/='ZS') THEN
      WHERE (PFIELD==XUNDEF) PFIELD=ZUNDEF
    ENDIF
!================================================

  END IF
!
  DEALLOCATE(ZWORK)

ENDIF

DEALLOCATE(IMASK)
!-------------------------------------------------------------------------------
END SUBROUTINE READ_SURFX1_MNH
!
!     #############################################################
      SUBROUTINE READ_SURFX2_MNH(HREC,KL1,KL2,PFIELD,KRESP,HCOMMENT,HDIR)
!     #############################################################
!
!!****  *READX2* - routine to fill a real 2D array for the externalised surface
!!
!!    PURPOSE
!!    -------
!
!       The purpose of READ_SURFX2 is
!
!!**  METHOD
!!    ------
!!
!!    EXTERNAL
!!    --------
!!
!!
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!
!!
!!    REFERENCE
!!    ---------
!!
!!
!!    AUTHOR
!!    ------
!!
!!      S.Malardel      *METEO-FRANCE*
!!
!!    MODIFICATIONS
!!    -------------
!!
!!      original                                                     01/08/03
!----------------------------------------------------------------------------
!
!*      0.    DECLARATIONS
!             ------------
!
USE MODE_FM
USE MODE_FMREAD
USE MODE_ll
USE MODE_IO_ll
!
USE MODD_IO_SURF_MNH, ONLY : COUT, CFILE , NLUOUT,  NMASK, NIU, NJU, NIB, NJB, NIE, NJE, &
                             NIU_ALL, NJU_ALL, NIB_ALL, NJB_ALL, NIE_ALL, NJE_ALL, NMASK_ALL
USE MODD_PARAMETERS, ONLY: XUNDEF
!
USE MODI_PACK_2D_1D
!
USE MODI_GET_SURF_UNDEF
!
IMPLICIT NONE
!
!*      0.1   Declarations of arguments
!
CHARACTER(LEN=16),       INTENT(IN)  :: HREC     ! name of the article to be read
INTEGER,                 INTENT(IN)  :: KL1      ! number of points
INTEGER,                 INTENT(IN)  :: KL2      ! second dimension
REAL, DIMENSION(KL1,KL2),INTENT(OUT) :: PFIELD   ! array containing the data field
INTEGER,                 INTENT(OUT) :: KRESP    ! KRESP  : return-code if a problem appears
CHARACTER(LEN=100),      INTENT(OUT) :: HCOMMENT ! comment
CHARACTER(LEN=1),        INTENT(IN)  :: HDIR     ! type of field :
!                                                ! 'H' for HOR : with hor. dim.; and  distributed.
!                                                ! 'A' for ALL : with hor. dim.; and not distributed.
!                                                ! '-' : no horizontal dim.
!
!*      0.2   Declarations of local variables
!
INTEGER           :: IGRID          ! IGRID : grid indicator
INTEGER           :: ILENCH         ! ILENCH : length of comment string

INTEGER           :: JP             ! loop index

REAL, DIMENSION(:,:,:), ALLOCATABLE :: ZWORK  ! work array read in the file
REAL              :: ZUNDEF         ! undefined value in SURFEX

!-------------------------------------------------------------------------------
!
!
!! Reading of a 3D field, masked (2 first dimensions) and with
!! 2 first dimensions packed into only 1 (results in a 2D array instead of 3D)
!
!*       1.     Dimension initializations:
!               -------------------------
!
!
!
IF (HDIR=='H') THEN
  ALLOCATE(ZWORK(NIU,NJU,SIZE(PFIELD,2)))
  CALL FMREAD(CFILE,HREC,COUT,'XY',ZWORK(:,:,:),IGRID,ILENCH,HCOMMENT,KRESP)
ELSEIF (HDIR=='A') THEN
  ALLOCATE(ZWORK(NIU_ALL,NJU_ALL,SIZE(PFIELD,2)))
  CALL FMREAD(CFILE,HREC,COUT,'--',ZWORK(:,:,:),IGRID,ILENCH,HCOMMENT,KRESP)
ELSE
  CALL FMREAD(CFILE,HREC,COUT,'--',PFIELD(:,:),IGRID,ILENCH,HCOMMENT,KRESP)
END IF
!
 IF (KRESP /=0) THEN
    WRITE(NLUOUT,*) 'WARNING'
    WRITE(NLUOUT,*) '-------'
    WRITE(NLUOUT,*) 'error when reading article ', HREC,'KRESP=',KRESP
    WRITE(NLUOUT,*) 'default value may be used, who knows???'
    WRITE(NLUOUT,*) ' '
    DEALLOCATE(ZWORK)
 ELSE IF (HDIR=='H') THEN
    DO JP=1,SIZE(PFIELD,2)
       CALL PACK_2D_1D(NMASK,ZWORK(NIB:NIE,NJB:NJE,JP),PFIELD(:,JP))
    END DO
    DEALLOCATE(ZWORK)
 ELSE IF (HDIR=='A') THEN
    DO JP=1,SIZE(PFIELD,2)
       CALL PACK_2D_1D(NMASK_ALL,ZWORK(NIB_ALL:NIE_ALL,NJB_ALL:NJE_ALL,JP),PFIELD(:,JP))
    END DO
    DEALLOCATE(ZWORK)
 END IF
 CALL GET_SURF_UNDEF(ZUNDEF)
 WHERE (PFIELD==XUNDEF) PFIELD=ZUNDEF
!
!-------------------------------------------------------------------------------
END SUBROUTINE READ_SURFX2_MNH
!
!     #############################################################
      SUBROUTINE READ_SURFX2COV_MNH(HREC,KL1,KL2,PFIELD,OFLAG,KRESP,HCOMMENT,HDIR)
!     #############################################################
!
!!****  *READX1* - routine to fill a real 2D array for the externalised surface
!!                 with Logical mask by level
!!
!!    PURPOSE
!!    -------
!
!       The purpose of READ_SURFX1 is
!
!!**  METHOD
!!    ------
!!
!!    EXTERNAL
!!    --------
!!
!!
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!
!!
!!    REFERENCE
!!    ---------
!!
!!
!!    AUTHOR
!!    ------
!!
!!      S.Malardel      *METEO-FRANCE*
!!
!!    MODIFICATIONS
!!    -------------
!!
!!      original                                                     01/08/03
!----------------------------------------------------------------------------
!
!*      0.    DECLARATIONS
!             ------------
!
USE MODE_FM
USE MODE_FMREAD
USE MODE_ll
USE MODE_IO_ll
!
USE MODD_CST,         ONLY : XPI
!
USE MODD_IO_SURF_MNH, ONLY : COUT, CFILE , NLUOUT,  NMASK, &
                             NIU, NJU, NIB, NJB, NIE, NJE, &
                             NIU_ALL, NJU_ALL, NIB_ALL,    &
                             NJB_ALL, NIE_ALL, NJE_ALL,    &
                             NMASK_ALL
!
USE MODI_PACK_2D_1D
!
IMPLICIT NONE
!
!*      0.1   Declarations of arguments
!
CHARACTER(LEN=16),   INTENT(IN) :: HREC     ! name of the article to be read
INTEGER,             INTENT(IN) :: KL1,KL2  !  number of points
REAL, DIMENSION(KL1,KL2), INTENT(OUT):: PFIELD   ! array containing the data field
LOGICAL,DIMENSION(KL2),   INTENT(IN) ::OFLAG  ! mask for array filling
INTEGER,             INTENT(OUT):: KRESP    ! KRESP  : return-code if a problem appears
CHARACTER(LEN=100),  INTENT(OUT):: HCOMMENT ! comment
CHARACTER(LEN=1),    INTENT(IN) :: HDIR     ! type of field :
!                                           ! 'H' for HOR : with hor. dim.; and  distributed.
!                                           ! 'A' for ALL : with hor. dim.; and not distributed.
!                                           ! '-' : no horizontal dim.

!
!*      0.2   Declarations of local variables
!
INTEGER           :: IGRID          ! IGRID : grid indicator
INTEGER           :: ILENCH         ! ILENCH : length of comment string

INTEGER           :: IMASDEV
CHARACTER(LEN=20) :: YREC
CHARACTER(LEN=2)  :: YDIR
CHARACTER(LEN=2)  :: YSTORAGE_TYPE
!
INTEGER           :: IIU, IJU, IIB, IJB, IIE, IJE ! dimensions of horizontal fields
INTEGER, DIMENSION(:), ALLOCATABLE :: IMASK       ! mask for packing
!JUANZ
INTEGER           :: NCOVER,ICOVER,JL2
REAL,DIMENSION(:,:,:), ALLOCATABLE :: ZWORK3D
!JUANZ
INTEGER  :: IVERSION, IBUGFIX
LOGICAL  :: GCOVER_PACKED ! .T. if COVER are all packed into one field
!-------------------------------------------------------------------------------
!
KRESP = 0
!
IF (HDIR=='A') THEN
  YDIR="--"
  IIU = NIU_ALL
  IJU = NJU_ALL
  IIB = NIB_ALL
  IJB = NJB_ALL
  IIE = NIE_ALL
  IJE = NJE_ALL
  ALLOCATE(IMASK(SIZE(NMASK_ALL)))
  IMASK = NMASK_ALL
ELSE
  YDIR="XY"
  IIU = NIU
  IJU = NJU
  IIB = NIB
  IJB = NJB
  IIE = NIE
  IJE = NJE
  ALLOCATE(IMASK(SIZE(NMASK)))
  IMASK = NMASK
END IF
!
!! Reading of a 2D fields, masked and packed into 1D vector
!
!
NCOVER=COUNT(OFLAG)
ALLOCATE (ZWORK3D(IIU,IJU,NCOVER))
ZWORK3D(:,:,:) =  0.0
!
 
CALL FMREAD(CFILE,'VERSION',COUT,'--',IVERSION,IGRID,ILENCH,HCOMMENT,KRESP)
!GAELLE CALL FMREAD(CFILE,'BUGFIX',COUT,'--',IBUGFIX,IGRID,ILENCH,HCOMMENT,KRESP)
CALL FMREAD(CFILE,'BUG   ',COUT,'--',IBUGFIX,IGRID,ILENCH,HCOMMENT,KRESP)

IF (IVERSION<7 .OR. (IVERSION==7 .AND. IBUGFIX==0)) THEN
  GCOVER_PACKED = .FALSE.
ELSE
  CALL FMREAD(CFILE,'COVER_PACKED',COUT,'--',GCOVER_PACKED,IGRID,ILENCH,HCOMMENT,KRESP)
END IF
!
IF (.NOT. GCOVER_PACKED) THEN
   ICOVER=0
   DO JL2=1,KL2
      WRITE(YREC,'(A5,I3.3)') 'COVER',JL2
      IF (OFLAG(JL2)) THEN
        ICOVER=ICOVER+1
        CALL FMREAD(CFILE,YREC,COUT,YDIR,ZWORK3D(:,:,ICOVER),IGRID,ILENCH,HCOMMENT,KRESP)
      END IF
   END DO

ELSE
  CALL FMREAD(CFILE,HREC,COUT,YDIR,ZWORK3D(:,:,:),IGRID,ILENCH,HCOMMENT,KRESP)
END IF
!
IF (KRESP /=0) THEN
  WRITE(NLUOUT,*) 'WARNING'
  WRITE(NLUOUT,*) '-------'
  WRITE(NLUOUT,*) 'error when reading article ', HREC,'KRESP=',KRESP
  WRITE(NLUOUT,*) ' '
ELSE IF (HDIR=='H' .OR. HDIR=='A') THEN
   ICOVER=0
   DO JL2=1,KL2
      IF (OFLAG(JL2)) THEN
         ICOVER=ICOVER+1
         CALL PACK_2D_1D(IMASK,ZWORK3D(IIB:IIE,IJB:IJE,ICOVER),PFIELD(:,JL2))
      ELSE
         PFIELD(:,JL2) = 0.0
      END IF
   END DO
END IF
!
DEALLOCATE(ZWORK3D)


DEALLOCATE(IMASK)
!-------------------------------------------------------------------------------
END SUBROUTINE READ_SURFX2COV_MNH
!
!     #############################################################
      SUBROUTINE READ_SURFX2COV_1COV_MNH(HREC,KL1,KCOVER,PFIELD,KRESP,HCOMMENT,HDIR)
!     #############################################################
!
!!****  *READX1* - routine to fill a real 2D array for the externalised surface
!!                 with Logical mask on one specified vertical level
!!
!!    PURPOSE
!!    -------
!
!       The purpose of READ_SURFX1 is
!
!!**  METHOD
!!    ------
!!
!!    EXTERNAL
!!    --------
!!
!!
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!
!!
!!    REFERENCE
!!    ---------
!!
!!
!!    AUTHOR
!!    ------
!!
!!      S.Malardel      *METEO-FRANCE*
!!
!!    MODIFICATIONS
!!    -------------
!!
!!      original                                                     01/08/03
!----------------------------------------------------------------------------
!
!*      0.    DECLARATIONS
!             ------------
!
USE MODE_FM
USE MODE_FMREAD
USE MODE_ll
USE MODE_IO_ll
!
USE MODD_CST,         ONLY : XPI
!
USE MODD_IO_SURF_MNH, ONLY : COUT, CFILE , NLUOUT,  NMASK, &
                             NIU, NJU, NIB, NJB, NIE, NJE, &
                             NIU_ALL, NJU_ALL, NIB_ALL,    &
                             NJB_ALL, NIE_ALL, NJE_ALL,    &
                             NMASK_ALL
!
USE MODI_PACK_2D_1D
!
IMPLICIT NONE
!
!*      0.1   Declarations of arguments
!
CHARACTER(LEN=16),   INTENT(IN) :: HREC     ! name of the article to be read
INTEGER,             INTENT(IN) :: KL1  !  number of points
INTEGER,             INTENT(IN) :: KCOVER ! index of the vertical level, it should be a index such that LCOVER(KCOVER)=.TRUE.
REAL, DIMENSION(KL1), INTENT(OUT):: PFIELD   ! array containing the data field
INTEGER,             INTENT(OUT):: KRESP    ! KRESP  : return-code if a problem appears
CHARACTER(LEN=100),  INTENT(OUT):: HCOMMENT ! comment
CHARACTER(LEN=1),    INTENT(IN) :: HDIR     ! type of field :
!                                           ! 'H' for HOR : with hor. dim.; and  distributed.
!                                           ! 'A' for ALL : with hor. dim.; and not distributed.
!                                           ! '-' : no horizontal dim.

!
!*      0.2   Declarations of local variables
!
INTEGER           :: IGRID          ! IGRID : grid indicator
INTEGER           :: ILENCH         ! ILENCH : length of comment string

INTEGER           :: IMASDEV
CHARACTER(LEN=20) :: YREC
CHARACTER(LEN=2)  :: YDIR
CHARACTER(LEN=2)  :: YSTORAGE_TYPE
!
INTEGER           :: IIU, IJU, IIB, IJB, IIE, IJE ! dimensions of horizontal fields
INTEGER, DIMENSION(:), ALLOCATABLE :: IMASK       ! mask for packing
!JUANZ
INTEGER           :: NCOVER,ICOVER,JL2
REAL,DIMENSION(:,:), ALLOCATABLE :: ZWORK2D
!JUANZ
INTEGER  :: IVERSION, IBUGFIX
LOGICAL  :: GCOVER_PACKED ! .T. if COVER are all packed into one field
 CHARACTER(LEN=1)   :: YDIR1
!-------------------------------------------------------------------------------
!
KRESP = 0
!YDIR1 = 'H'
!IF (PRESENT(HDIR)) YDIR1 = HDIR
YDIR1 = HDIR
!
IF (YDIR1=='A') THEN
  YDIR="--"
  IIU = NIU_ALL
  IJU = NJU_ALL
  IIB = NIB_ALL
  IJB = NJB_ALL
  IIE = NIE_ALL
  IJE = NJE_ALL
  ALLOCATE(IMASK(SIZE(NMASK_ALL)))
  IMASK = NMASK_ALL
ELSE
  YDIR="XY"
  IIU = NIU
  IJU = NJU
  IIB = NIB
  IJB = NJB
  IIE = NIE
  IJE = NJE
  ALLOCATE(IMASK(SIZE(NMASK)))
  IMASK = NMASK
END IF
!
!! Reading of a 2D fields, masked and packed into 1D vector
!
!
ALLOCATE (ZWORK2D(IIU,IJU))
ZWORK2D(:,:) =  0.0
!
 
CALL FMREAD(CFILE,'VERSION',COUT,'--',IVERSION,IGRID,ILENCH,HCOMMENT,KRESP)
!GAELLE CALL FMREAD(CFILE,'BUGFIX',COUT,'--',IBUGFIX,IGRID,ILENCH,HCOMMENT,KRESP)
CALL FMREAD(CFILE,'BUG   ',COUT,'--',IBUGFIX,IGRID,ILENCH,HCOMMENT,KRESP)

IF (IVERSION<7 .OR. (IVERSION==7 .AND. IBUGFIX==0)) THEN
  GCOVER_PACKED = .FALSE.
ELSE
  CALL FMREAD(CFILE,'COVER_PACKED',COUT,'--',GCOVER_PACKED,IGRID,ILENCH,HCOMMENT,KRESP)
END IF
!
IF (.NOT. GCOVER_PACKED) THEN
  WRITE(YREC,'(A5,I3.3)') 'COVER',KCOVER
  CALL FMREAD(CFILE,YREC,COUT,YDIR1,ZWORK2D(:,:),IGRID,ILENCH,HCOMMENT,KRESP)
ELSE
  WRITE(NLUOUT,*) 'WARNING'
  WRITE(NLUOUT,*) '-------'
  WRITE(NLUOUT,*) 'error : GCOVER_PACKED = ', GCOVER_PACKED, ' and we try to read the covers one by one '
  WRITE(NLUOUT,*) ' '
  CALL ABORT
!  CALL FMREAD(CFILE,HREC,COUT,YDIR,ZWORK2D(:,:,:),IGRID,ILENCH,HCOMMENT,KRESP)
END IF
!
IF (KRESP /=0) THEN
  WRITE(NLUOUT,*) 'WARNING'
  WRITE(NLUOUT,*) '-------'
  WRITE(NLUOUT,*) 'error when reading article ', HREC,'KRESP=',KRESP
  WRITE(NLUOUT,*) ' '
ELSE IF (YDIR1=='H' .OR. YDIR1=='A') THEN
   CALL PACK_2D_1D(IMASK,ZWORK2D(IIB:IIE,IJB:IJE),PFIELD(:))
END IF
!
DEALLOCATE(ZWORK2D)


DEALLOCATE(IMASK)
!-------------------------------------------------------------------------------
END SUBROUTINE READ_SURFX2COV_1COV_MNH
!
!     #############################################################
      SUBROUTINE READ_SURFN0_MNH(HREC,KFIELD,KRESP,HCOMMENT)
!     #############################################################
!
!!****  *READN0* - routine to read an integer
!!
!!    PURPOSE
!!    -------
!
!       The purpose of READN0 is
!
!!**  METHOD
!!    ------
!!
!!    EXTERNAL
!!    --------
!!
!!
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!
!!
!!    REFERENCE
!!    ---------
!!
!!
!!    AUTHOR
!!    ------
!!
!!      S.Malardel      *METEO-FRANCE*
!!
!!    MODIFICATIONS
!!    -------------
!!
!!      original                                                     01/08/03
!----------------------------------------------------------------------------
!
!*      0.    DECLARATIONS
!             ------------
!
USE MODE_ll
USE MODE_FM
USE MODE_FMREAD
!
USE MODD_IO_SURF_MNH,     ONLY : COUT, CFILE , NLUOUT, NMASK, &
                                 NIU, NJU, NIB, NJB, NIE, NJE
USE MODD_CONF,            ONLY : CPROGRAM
!
!
!
IMPLICIT NONE
!
!*      0.1   Declarations of arguments
!
CHARACTER(LEN=16),  INTENT(IN)  :: HREC     ! name of the article to be read
INTEGER,            INTENT(OUT) :: KFIELD   ! the integer to be read
INTEGER,            INTENT(OUT) :: KRESP    ! KRESP  : return-code if a problem appears
CHARACTER(LEN=100), INTENT(OUT) :: HCOMMENT ! comment
!
!*      0.2   Declarations of local variables
!
INTEGER           :: IGRID          ! IGRID : grid indicator
INTEGER           :: ILENCH         ! ILENCH : length of comment string
INTEGER           :: IMASDEV        ! mesonh version of the input file
INTEGER           :: IBUGFIX        ! mesonh bugfix version of the input file
INTEGER           :: IIMAX, IJMAX
!
REAL              :: ZDIM_SUM
INTEGER           :: INFO_ll
!
!* variables for reading of old (masdev4_5 and before) files
LOGICAL, DIMENSION(255) :: GCOVER
REAL, DIMENSION(:,:,:), ALLOCATABLE :: ZCOVER
INTEGER :: JCOVER
CHARACTER(LEN=16) :: YRECFM
!JUANZ
INTEGER           :: NCOVER,ICOVER,IKL2
REAL,DIMENSION(:,:,:), ALLOCATABLE :: ZWORK3D
!JUANZ
!
!-------------------------------------------------------------------------------
!
CALL FMREAD(CFILE,'MASDEV',COUT,'--',IMASDEV,IGRID,ILENCH,HCOMMENT,KRESP)
CALL FMREAD(CFILE,'BUGFIX',COUT,'--',IBUGFIX,IGRID,ILENCH,HCOMMENT,KRESP)
!
IF ((HREC=='DIM_FULL' .OR. HREC=='DIM_NATURE' .OR. HREC=='DIM_SEA'  &
                      .OR. HREC=='DIM_WATER'  .OR. HREC=='DIM_TOWN')&
    .AND. (IMASDEV<46 .OR. (IMASDEV==46 .AND. IBUGFIX<=2))          ) THEN

  IF (HREC=='DIM_FULL') THEN
    KFIELD=SIZE(NMASK)
  ELSE
    YRECFM='LCOVER'
    CALL FMREAD(CFILE,YRECFM,COUT,'XY',GCOVER(:),IGRID,ILENCH,HCOMMENT,KRESP)
    IF (KRESP/=0) THEN
      !* ground_ocean case
      KFIELD=SIZE(NMASK)
      KRESP=0
    ELSE
      ALLOCATE(ZCOVER(NIU,NJU,255))
      ZCOVER(:,:,:) = 0.
      DO JCOVER=1,255
        IF (.NOT. GCOVER(JCOVER)) CYCLE
        WRITE(YRECFM,'(A5,I3.3)') 'COVER',JCOVER
        CALL FMREAD(CFILE,YRECFM,COUT,'XY',ZCOVER(:,:,JCOVER),IGRID,ILENCH,HCOMMENT,KRESP)
      END DO
      SELECT CASE (HREC)
         CASE('DIM_SEA')
           KFIELD=COUNT(ZCOVER(NIB:NIE,NJB:NJE,1)   &
                       +ZCOVER(NIB:NIE,NJB:NJE,242) &
                       +ZCOVER(NIB:NIE,NJB:NJE,243)>0.)
         CASE('DIM_TOWN')
           KFIELD=COUNT(ZCOVER(NIB:NIE,NJB:NJE,7)   &
                       +ZCOVER(NIB:NIE,NJB:NJE,151) &
                       +ZCOVER(NIB:NIE,NJB:NJE,152) &
                       +ZCOVER(NIB:NIE,NJB:NJE,153) &
                       +ZCOVER(NIB:NIE,NJB:NJE,154) &
                       +ZCOVER(NIB:NIE,NJB:NJE,155) &
                       +ZCOVER(NIB:NIE,NJB:NJE,156) &
                       +ZCOVER(NIB:NIE,NJB:NJE,157) &
                       +ZCOVER(NIB:NIE,NJB:NJE,158) &
                       +ZCOVER(NIB:NIE,NJB:NJE,159) &
                       +ZCOVER(NIB:NIE,NJB:NJE,160) &
                       +ZCOVER(NIB:NIE,NJB:NJE,161)>0.)
         CASE('DIM_WATER')
           KFIELD=COUNT(ZCOVER(NIB:NIE,NJB:NJE,2)   &
                       +ZCOVER(NIB:NIE,NJB:NJE,3)   &
                       +ZCOVER(NIB:NIE,NJB:NJE,124) &
                       +ZCOVER(NIB:NIE,NJB:NJE,125) &
                       +ZCOVER(NIB:NIE,NJB:NJE,176) &
                       +ZCOVER(NIB:NIE,NJB:NJE,238) &
                       +ZCOVER(NIB:NIE,NJB:NJE,239) &
                       +ZCOVER(NIB:NIE,NJB:NJE,240) &
                       +ZCOVER(NIB:NIE,NJB:NJE,241)>0.)
         CASE('DIM_NATURE')
           KFIELD=COUNT(ZCOVER(NIB:NIE,NJB:NJE,1)   &
                       +ZCOVER(NIB:NIE,NJB:NJE,2)   &
                       +ZCOVER(NIB:NIE,NJB:NJE,3)   &
                       +ZCOVER(NIB:NIE,NJB:NJE,243)<1.-1.E-8)
      END SELECT
      DEALLOCATE(ZCOVER)
    END IF
  END IF

  ZDIM_SUM = FLOAT(KFIELD)
  IF (CPROGRAM/='PGD   ' .AND. CPROGRAM/='NESPGD') THEN
     CALL REDUCESUM_ll(ZDIM_SUM,INFO_ll)
     KFIELD=NINT(ZDIM_SUM)
  ENDIF
  HCOMMENT = ' '
  KRESP = 0
  WRITE(NLUOUT,*) 'HREC=', HREC, 'KFIELD=', KFIELD

ELSE IF (HREC=='DIM_FULL' .AND. ( CPROGRAM=='IDEAL ' .OR.  &
                                  CPROGRAM=='SPAWN ' .OR. CPROGRAM=='ZOOMPG' ))THEN
   CALL FMREAD(CFILE,'IMAX',COUT,'--',IIMAX,IGRID,ILENCH,HCOMMENT,KRESP)
   CALL FMREAD(CFILE,'JMAX',COUT,'--',IJMAX,IGRID,ILENCH,HCOMMENT,KRESP)
   KFIELD = IIMAX * IJMAX
ELSE
   CALL FMREAD(CFILE,HREC,COUT,'--',KFIELD,IGRID,ILENCH,HCOMMENT,KRESP)

   IF (KRESP /=0) THEN
      WRITE(NLUOUT,*) 'WARNING'
      WRITE(NLUOUT,*) '-------'
      WRITE(NLUOUT,*) 'error when reading article ', HREC,'KRESP=',KRESP
      WRITE(NLUOUT,*) 'default value may be used, who knows???'
      WRITE(NLUOUT,*) ' '
   ENDIF

ENDIF
!-------------------------------------------------------------------------------
END SUBROUTINE READ_SURFN0_MNH
!
!     #############################################################
      SUBROUTINE READ_SURFN1_MNH(HREC,KL,KFIELD,KRESP,HCOMMENT,HDIR)
!     #############################################################
!
!!****  *READN0* - routine to read an integer
!!
!!    PURPOSE
!!    -------
!
!       The purpose of READN0 is
!
!!**  METHOD
!!    ------
!!
!!    EXTERNAL
!!    --------
!!
!!
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!
!!
!!    REFERENCE
!!    ---------
!!
!!
!!    AUTHOR
!!    ------
!!
!!      S.Malardel      *METEO-FRANCE*
!!
!!    MODIFICATIONS
!!    -------------
!!
!!      original                                                     01/08/03
!----------------------------------------------------------------------------
!
!*      0.    DECLARATIONS
!             ------------
!
USE MODE_FM
USE MODE_FMREAD
!
USE MODD_IO_SURF_MNH,     ONLY : COUT, CFILE , NLUOUT, NMASK, &
                                 NIU, NJU, NIB, NJB, NIE, NJE
!
USE MODI_PACK_2D_1D
!
IMPLICIT NONE
!
!*      0.1   Declarations of arguments
!
CHARACTER(LEN=16),      INTENT(IN)  :: HREC     ! name of the article to be read
INTEGER,                INTENT(IN)  :: KL       ! number of points
INTEGER, DIMENSION(KL), INTENT(OUT) :: KFIELD   ! the integer to be read
INTEGER,                INTENT(OUT) :: KRESP    ! KRESP  : return-code if a problem appears
CHARACTER(LEN=100),     INTENT(OUT) :: HCOMMENT ! comment
CHARACTER(LEN=1),       INTENT(IN)  :: HDIR     ! type of field :
!                                               ! 'H' : field with
!                                               !       horizontal spatial dim.
!                                               ! '-' : no horizontal dim.
!
!*      0.2   Declarations of local variables
!
INTEGER           :: IGRID          ! IGRID : grid indicator
INTEGER           :: ILENCH         ! ILENCH : length of comment string
!
INTEGER, DIMENSION(:,:), ALLOCATABLE :: IWORK  ! work array read in the file
!---------------------------------------------------------------------
!
IF (HDIR=='-') THEN
!
 CALL FMREAD(CFILE,HREC,COUT,'--',KFIELD,IGRID,ILENCH,HCOMMENT,KRESP)
!
ELSE IF (HDIR=='H') THEN
 ALLOCATE(IWORK(NIU,NJU))

!
 CALL FMREAD(CFILE,HREC,COUT,'XY',IWORK(:,:),IGRID,ILENCH,HCOMMENT,KRESP)
!

 IF (KRESP /=0) THEN
    WRITE(NLUOUT,*) 'WARNING'
    WRITE(NLUOUT,*) '-------'
    WRITE(NLUOUT,*) 'error when reading article ', HREC,'KRESP=',KRESP
    WRITE(NLUOUT,*) 'default value may be used, who knows???'
    WRITE(NLUOUT,*) ' '
 ELSE
    CALL PACK_2D_1D(NMASK,IWORK(NIB:NIE,NJB:NJE),KFIELD)
 END IF
!

DEALLOCATE(IWORK)

ENDIF
!-------------------------------------------------------------------------------
END SUBROUTINE READ_SURFN1_MNH
!
!     #############################################################
      SUBROUTINE READ_SURFC0_MNH(HREC,HFIELD,KRESP,HCOMMENT)
!     #############################################################
!
!!****  *READC0* - routine to read an integer
!!
!!    PURPOSE
!!    -------
!
!       The purpose of READC0 is
!
!!**  METHOD
!!    ------
!!
!!    EXTERNAL
!!    --------
!!
!!
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!
!!
!!    REFERENCE
!!    ---------
!!
!!
!!    AUTHOR
!!    ------
!!
!!      S.Malardel      *METEO-FRANCE*
!!
!!    MODIFICATIONS
!!    -------------
!!
!!      original                                                     01/08/03
!----------------------------------------------------------------------------
!
!*      0.    DECLARATIONS
!             ------------
!
USE MODE_ll
USE MODE_FM
USE MODE_FMREAD
!
USE MODE_POS
!
USE MODD_IO_SURF_MNH,        ONLY : COUT, CFILE, NLUOUT
USE MODD_CONF,               ONLY : LCARTESIAN, CPROGRAM
USE MODD_LUNIT,              ONLY : CPGDFILE
!
!
IMPLICIT NONE
!
!*      0.1   Declarations of arguments
!
CHARACTER(LEN=16),  INTENT(IN)  :: HREC      ! name of the article to be read
CHARACTER(LEN=40),  INTENT(OUT) :: HFIELD    ! the integer to be read
INTEGER,            INTENT(OUT) :: KRESP     ! KRESP  : return-code if a problem appears
CHARACTER(LEN=100), INTENT(OUT) :: HCOMMENT  ! comment
!
!*      0.2   Declarations of local variables
!
INTEGER           :: IRESP          ! return code
INTEGER           :: IGRID          ! IGRID : grid indicator
INTEGER           :: ILENCH         ! ILENCH : length of comment string
!
INTEGER           :: IMASDEV      ! mesonh version of the input file
INTEGER           :: ILUDES       ! .des file logical unit
CHARACTER(LEN=32) :: YDESFM       ! .des file
!
LOGICAL           :: GFOUND
CHARACTER(LEN=4)  :: CTURB,CRAD,CGROUND,CCLOUD,CDCONV,CELEC
CHARACTER(LEN=6)  :: CSEA_FLUX
NAMELIST/NAM_PARAMn/CTURB,CRAD,CGROUND,CCLOUD,CDCONV,CSEA_FLUX, CELEC
!----------------------------------------------------------------------------
! On lit la version de Mesonh usilisée pour fabriquer le fichier
!
CALL FMREAD(CFILE,'MASDEV',COUT,'--',IMASDEV,IGRID,ILENCH,HCOMMENT,KRESP)

IF (HREC=='SNOW_VEG_TYPE'.AND.IMASDEV<46) THEN
  HFIELD='D95'
ELSE IF (HREC=='SNOW_ROAD_TYPE'.AND.IMASDEV<46) THEN
  HFIELD='1-L'
ELSE IF (HREC=='SNOW_ROOF_TYPE'.AND.IMASDEV<46) THEN
  HFIELD='1-L'
ELSE IF (HREC=='PHOTO'.AND.IMASDEV<46) THEN
  HFIELD='NON'
ELSE IF ( HREC=='GRID_TYPE'.AND. (IMASDEV<46 .OR. &
                           (CPROGRAM=='IDEAL ' .AND. CPGDFILE/=COUT) .OR. &
                           (CPROGRAM=='SPAWN ' .AND. CPGDFILE/=COUT) .OR. &
                           CPROGRAM=='ZOOMPG'                         )) THEN
  IF (LCARTESIAN) THEN
    HFIELD="CARTESIAN "
  ELSE
    HFIELD='CONF PROJ '
  END IF
ELSE IF ( HREC=='ISBA  ' .AND.IMASDEV<46) THEN
  HFIELD = '3-L'
ELSE IF ( (HREC=='NATURE'.OR.HREC=='SEA   '.OR.HREC=='WATER ' &
            .OR.HREC=='TOWN  ') .AND.IMASDEV<46) THEN
     IF (CPROGRAM=='REAL  ' .OR. CPROGRAM=='IDEAL ') THEN
       CGROUND='ISBA'
     ELSE
       CGROUND='NONE'
       YDESFM=ADJUSTL(ADJUSTR(CFILE)//'.des')
       CALL FMLOOK_ll(YDESFM,COUT,ILUDES,IRESP)
       CALL POSNAM(ILUDES,'NAM_PARAMN',GFOUND,NLUOUT)
       IF (GFOUND) READ(UNIT=ILUDES,NML=NAM_PARAMn)
     END IF
     IF (CGROUND=='NONE') THEN
       HFIELD ='NONE  '
     ELSE IF (CGROUND=='FLUX') THEN
       HFIELD ='FLUX  '
     ELSE IF (CGROUND=='ISBA') THEN
       IF(HREC=='SEA   ') HFIELD ='SEAFLX'
       IF(HREC=='WATER ') HFIELD ='WATFLX'
       IF(HREC=='NATURE') HFIELD ='ISBA  '
       IF(HREC=='TOWN  ') HFIELD ='TEB   '
     ELSE
       WRITE(NLUOUT,*) ' '
       WRITE(NLUOUT,*) 'error when reading article', HREC,'KRESP=',KRESP
       WRITE(NLUOUT,*) 'avec CGROUND = "',CGROUND,'"'
 !callabortstop
CALL ABORT
       STOP
     END IF

ELSE
   CALL FMREAD(CFILE,HREC,COUT,'--',HFIELD,IGRID,ILENCH,HCOMMENT,KRESP)
   !
   IF (KRESP /=0) THEN
      WRITE(NLUOUT,*) 'WARNING'
      WRITE(NLUOUT,*) '-------'
      WRITE(NLUOUT,*) 'error when reading article ', HREC,'KRESP=',KRESP
      WRITE(NLUOUT,*) 'default value may be used, who knows???'
      WRITE(NLUOUT,*) ' '
 !callabortstop
CALL ABORT
      STOP
   ENDIF

ENDIF
!
!-------------------------------------------------------------------------------
END SUBROUTINE READ_SURFC0_MNH
!
!     #############################################################
      SUBROUTINE READ_SURFL1_MNH(HREC,KL,OFIELD,KRESP,HCOMMENT,HDIR)
!     #############################################################
!
!!****  *READL1* - routine to read a logical array
!!
!!    PURPOSE
!!    -------
!
!       The purpose of READL1 is
!
!!**  METHOD
!!    ------
!!
!!    EXTERNAL
!!    --------
!!
!!
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!
!!
!!    REFERENCE
!!    ---------
!!
!!
!!    AUTHOR
!!    ------
!!
!!      S.Malardel      *METEO-FRANCE*
!!
!!    MODIFICATIONS
!!    -------------
!!
!!      original                                                     01/08/03
!----------------------------------------------------------------------------
!
!*      0.    DECLARATIONS
!             ------------
!
USE MODE_FM
USE MODE_FMREAD
USE MODI_PACK_2D_1D
!
USE MODD_IO_SURF_MNH,     ONLY : COUT, CFILE , NLUOUT, NMASK, &
                                 NIU, NJU, NIB, NJB, NIE, NJE
!
!
IMPLICIT NONE
!
!*      0.1   Declarations of arguments
!
CHARACTER(LEN=16),      INTENT(IN)  :: HREC     ! name of the article to be read
INTEGER,                INTENT(IN)  :: KL       ! number of points
LOGICAL, DIMENSION(KL), INTENT(OUT) :: OFIELD   ! array containing the data field
INTEGER,                INTENT(OUT) :: KRESP    ! KRESP  : return-code if a problem appears
CHARACTER(LEN=100),     INTENT(OUT) :: HCOMMENT ! comment
CHARACTER(LEN=1),       INTENT(IN)  :: HDIR     ! type of field :
!                                               ! 'H' : field with
!                                               !       horizontal spatial dim.
!                                               ! '-' : no horizontal dim.
!
!*      0.2   Declarations of local variables
!
INTEGER           :: IGRID          ! IGRID : grid indicator
INTEGER           :: ILENCH         ! ILENCH : length of comment string

LOGICAL, DIMENSION(:,:), ALLOCATABLE :: GWORK  ! work array read in the file
INTEGER, DIMENSION(:,:), ALLOCATABLE :: IWORK  ! work array read in the file

!-------------------------------------------------------------------------------

IF (HDIR=='-') THEN
   CALL FMREAD(CFILE,HREC,COUT,'--',OFIELD(:),IGRID,ILENCH,HCOMMENT,KRESP)

   IF (KRESP /=0) THEN
      WRITE(NLUOUT,*) 'WARNING'
      WRITE(NLUOUT,*) '-------'
      WRITE(NLUOUT,*) 'error when reading article ', HREC,'KRESP=',KRESP
      WRITE(NLUOUT,*) 'default value may be used, who knows???'
      WRITE(NLUOUT,*) ' '
   ENDIF
ELSE IF (HDIR=='H') THEN
 ALLOCATE(GWORK(NIU,NJU))
 GWORK = .FALSE.
!
 ALLOCATE(IWORK(NIU,NJU))
 CALL FMREAD(CFILE,HREC,COUT,'XY',IWORK(:,:),IGRID,ILENCH,HCOMMENT,KRESP)
 WHERE (IWORK==1) GWORK = .TRUE.
 DEALLOCATE(IWORK)
!

 IF (KRESP /=0) THEN
    WRITE(NLUOUT,*) 'WARNING'
    WRITE(NLUOUT,*) '-------'
    WRITE(NLUOUT,*) 'error when reading article ', HREC,'KRESP=',KRESP
    WRITE(NLUOUT,*) 'default value may be used, who knows???'
    WRITE(NLUOUT,*) ' '
 ELSE
    CALL PACK_2D_1D(NMASK,GWORK(NIB:NIE,NJB:NJE),OFIELD)
 END IF
!

 DEALLOCATE(GWORK)
END IF
!-------------------------------------------------------------------------------
END SUBROUTINE READ_SURFL1_MNH
!
!     #############################################################
      SUBROUTINE READ_SURFL0_MNH(HREC,OFIELD,KRESP,HCOMMENT)
!     #############################################################
!
!!****  *READL0* - routine to read a logical
!!
!!    PURPOSE
!!    -------
!
!
!!**  METHOD
!!    ------
!!
!!    EXTERNAL
!!    --------
!!
!!
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!
!!
!!    REFERENCE
!!    ---------
!!
!!
!!    AUTHOR
!!    ------
!!
!!      S.Malardel      *METEO-FRANCE*
!!
!!    MODIFICATIONS
!!    -------------
!!
!!      original                                                     01/08/03
!----------------------------------------------------------------------------
!
!*      0.    DECLARATIONS
!             ------------
!
USE MODE_FM
USE MODE_FMREAD
!
USE MODD_IO_SURF_MNH,        ONLY : COUT, CFILE, NLUOUT
!
!
IMPLICIT NONE
!
!*      0.1   Declarations of arguments
!
CHARACTER(LEN=16),  INTENT(IN)  :: HREC     ! name of the article to be read
LOGICAL,            INTENT(OUT) :: OFIELD   ! array containing the data field
INTEGER,            INTENT(OUT) :: KRESP    ! KRESP  : return-code if a problem appears
CHARACTER(LEN=100), INTENT(OUT) :: HCOMMENT ! comment
!
!*      0.2   Declarations of local variables
!
INTEGER           :: IGRID          ! IGRID : grid indicator
INTEGER           :: ILENCH         ! ILENCH : length of comment string

INTEGER           :: IMASDEV        ! MESONH version

!-------------------------------------------------------------------------------

IF (HREC(1:4)=='BUDC') THEN
  CALL FMREAD(CFILE,'MASDEV',COUT,'--',IMASDEV,IGRID,ILENCH,HCOMMENT,KRESP)
  IF (IMASDEV<=45) THEN
    OFIELD = .FALSE.
    KRESP = 0
    RETURN
  END IF
END IF
!
IF (HREC=='ECOCLIMAP') THEN
  CALL FMREAD(CFILE,'MASDEV',COUT,'--',IMASDEV,IGRID,ILENCH,HCOMMENT,KRESP)
  IF (IMASDEV<=46) THEN
    OFIELD = .TRUE.
    KRESP = 0
    RETURN
  END IF
END IF
!
CALL FMREAD(CFILE,HREC,COUT,'--',OFIELD,IGRID,ILENCH,HCOMMENT,KRESP)
!
IF (KRESP /=0) THEN
  WRITE(NLUOUT,*) 'WARNING'
  WRITE(NLUOUT,*) '-------'
  WRITE(NLUOUT,*) 'error when reading article ', HREC,'KRESP=',KRESP
  WRITE(NLUOUT,*) 'default value may be used, who knows???'
  WRITE(NLUOUT,*) ' '
ENDIF
!-------------------------------------------------------------------------------
END SUBROUTINE READ_SURFL0_MNH
!
!     #############################################################
      SUBROUTINE READ_SURFT0_MNH(HREC,KYEAR,KMONTH,KDAY,PTIME,KRESP,HCOMMENT)
!     #############################################################
!
!!****  *READT0* - routine to read a MESO-NH date_time scalar
!!
!!    PURPOSE
!!    -------
!
!       The purpose of READT0 is
!
!!**  METHOD
!!    ------
!!
!!    EXTERNAL
!!    --------
!!
!!
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!
!!
!!    REFERENCE
!!    ---------
!!
!!
!!    AUTHOR
!!    ------
!!
!!      V. MASSON      *METEO-FRANCE*
!!
!!    MODIFICATIONS
!!    -------------
!!
!!      original                                                     18/08/97
!----------------------------------------------------------------------------
!
!*      0.    DECLARATIONS
!             ------------
!
USE MODE_FM
USE MODE_FMREAD
!
USE MODD_IO_SURF_MNH,        ONLY : COUT, CFILE, NLUOUT
!
!
IMPLICIT NONE
!
!*      0.1   Declarations of arguments
!
CHARACTER(LEN=16),  INTENT(IN)    :: HREC     ! name of the article to be read
INTEGER,            INTENT(OUT)   :: KYEAR    ! year
INTEGER,            INTENT(OUT)   :: KMONTH   ! month
INTEGER,            INTENT(OUT)   :: KDAY     ! day
REAL,               INTENT(OUT)   :: PTIME    ! time
INTEGER,            INTENT(OUT)   :: KRESP    ! KRESP  : return-code if a problem appears
CHARACTER(LEN=100), INTENT(OUT)   :: HCOMMENT ! comment

!*      0.2   Declarations of local variables
!
INTEGER           :: IGRID          ! IGRID : grid indicator
INTEGER           :: ILENCH         ! ILENCH : length of comment string
!
CHARACTER(LEN=16)              :: YRECFM     ! Name of the article to be written
CHARACTER(LEN=40)              :: YFILETYPE40! MESONH file type
CHARACTER(LEN=2)               :: YFILETYPE2 ! MESONH file type
INTEGER, DIMENSION(3)  :: ITDATE
INTEGER                :: IMASDEV           ! MESONH version
!-------------------------------------------------------------------------------
!
!
CALL FMREAD(CFILE,'MASDEV',COUT,'--',IMASDEV,IGRID,ILENCH,HCOMMENT,KRESP)
IF (IMASDEV<46) THEN
  CALL FMREAD(CFILE,'STORAGE_TYPE',COUT,'--',YFILETYPE2,IGRID,ILENCH,HCOMMENT,KRESP)
ELSE
  CALL FMREAD(CFILE,'STORAGETYPE',COUT,'--',YFILETYPE40,IGRID,ILENCH,HCOMMENT,KRESP)
  YFILETYPE2 = YFILETYPE40(1:2)
END IF
IF (YFILETYPE2(1:2)=='PG') THEN
  WRITE(NLUOUT,*) 'WARNING'
  WRITE(NLUOUT,*) '-------'
  WRITE(NLUOUT,*) 'Date is not read in a PGD file'
  WRITE(NLUOUT,*) 'Atmospheric model value is kept'
  WRITE(NLUOUT,*) ' '
  KRESP = -2
  RETURN
END IF
!
YRECFM=TRIM(HREC)//'%TDATE'
CALL FMREAD(CFILE,YRECFM,COUT,'--',ITDATE,IGRID,ILENCH,HCOMMENT,KRESP)
KYEAR  = ITDATE(1)
KMONTH = ITDATE(2)
KDAY   = ITDATE(3)

   IF (KRESP /=0) THEN
    WRITE(NLUOUT,*) 'WARNING'
    WRITE(NLUOUT,*) '-------'
    WRITE(NLUOUT,*) 'error when reading article ',YRECFM,'KRESP=',KRESP
    WRITE(NLUOUT,*) 'default value may be used, who knows???'
    WRITE(NLUOUT,*) ' '
  ENDIF
!
YRECFM=TRIM(HREC)//'%TIME'
CALL FMREAD(CFILE,YRECFM,COUT,'--',PTIME,IGRID,ILENCH,HCOMMENT,KRESP)

   IF (KRESP /=0) THEN
    WRITE(NLUOUT,*) 'WARNING'
    WRITE(NLUOUT,*) '-------'
    WRITE(NLUOUT,*) 'error when reading article ',YRECFM,'KRESP=',KRESP
    WRITE(NLUOUT,*) 'default value may be used, who knows???'
    WRITE(NLUOUT,*) ' '
  ENDIF
!-------------------------------------------------------------------------------
END SUBROUTINE READ_SURFT0_MNH

!     #############################################################
      SUBROUTINE READ_SURFT1_MNH(HREC,KL1,KYEAR,KMONTH,KDAY,PTIME,KRESP,HCOMMENT)
!     #############################################################
!
!!****  *READT0* - routine to read a MESO-NH date_time vector
!!
!!    PURPOSE
!!    -------
!
!       The purpose of READT1 is
!
!!**  METHOD
!!    ------
!!
!!    EXTERNAL
!!    --------
!!
!!
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!
!!
!!    REFERENCE
!!    ---------
!!
!!
!!    AUTHOR
!!    ------
!!
!!      G. TANGUY      *METEO-FRANCE*
!!
!!    MODIFICATIONS
!!    -------------
!!
!!      original                                                     03/2009
!----------------------------------------------------------------------------
!
!*      0.    DECLARATIONS
!             ------------
!
USE MODE_FM
USE MODE_FMREAD
!
USE MODD_IO_SURF_MNH,        ONLY : COUT, CFILE, NLUOUT
!
!
IMPLICIT NONE
!
!*      0.1   Declarations of arguments
!
CHARACTER(LEN=16),  INTENT(IN)    :: HREC     ! name of the article to be read
INTEGER,            INTENT(IN) :: KL1       ! number of points

INTEGER, DIMENSION(KL1), INTENT(OUT)   :: KYEAR    ! year
INTEGER, DIMENSION(KL1), INTENT(OUT)   :: KMONTH   ! month
INTEGER, DIMENSION(KL1), INTENT(OUT)   :: KDAY     ! day
REAL,    DIMENSION(KL1), INTENT(OUT)   :: PTIME    ! time
INTEGER,            INTENT(OUT)   :: KRESP    ! KRESP  : return-code if a problem appears
CHARACTER(LEN=100), INTENT(OUT)   :: HCOMMENT ! comment

!*      0.2   Declarations of local variables
!
INTEGER           :: IGRID          ! IGRID : grid indicator
INTEGER           :: ILENCH         ! ILENCH : length of comment string
!
CHARACTER(LEN=16)              :: YRECFM     ! Name of the article to be written
CHARACTER(LEN=40)              :: YFILETYPE40! MESONH file type
CHARACTER(LEN=2)               :: YFILETYPE2 ! MESONH file type
INTEGER, DIMENSION(3,KL1)  :: ITDATE
INTEGER                :: IMASDEV           ! MESONH version
!-------------------------------------------------------------------------------
!
!
CALL FMREAD(CFILE,'MASDEV',COUT,'--',IMASDEV,IGRID,ILENCH,HCOMMENT,KRESP)
IF (IMASDEV<46) THEN
  CALL FMREAD(CFILE,'STORAGE_TYPE',COUT,'--',YFILETYPE2,IGRID,ILENCH,HCOMMENT,KRESP)
ELSE
  CALL FMREAD(CFILE,'STORAGETYPE',COUT,'--',YFILETYPE40,IGRID,ILENCH,HCOMMENT,KRESP)
  YFILETYPE2 = YFILETYPE40(1:2)
END IF
!IF (YFILETYPE2(1:2)=='PG') THEN
!  WRITE(NLUOUT,*) 'WARNING'
!  WRITE(NLUOUT,*) '-------'
!  WRITE(NLUOUT,*) 'Date is not read in a PGD file'
!  WRITE(NLUOUT,*) 'Atmospheric model value is kept'
!  WRITE(NLUOUT,*) ' '
!  KRESP = -2
!  RETURN
!END IF
!
YRECFM=TRIM(HREC)//'%TDATE'
CALL FMREAD(CFILE,YRECFM,COUT,'--',ITDATE(:,:),IGRID,ILENCH,HCOMMENT,KRESP)
KYEAR(:)  = ITDATE(1,:)
KMONTH(:) = ITDATE(2,:)
KDAY(:)   = ITDATE(3,:)

   IF (KRESP /=0) THEN
    WRITE(NLUOUT,*) 'WARNING'
    WRITE(NLUOUT,*) '-------'
    WRITE(NLUOUT,*) 'error when reading article ',YRECFM,'KRESP=',KRESP
    WRITE(NLUOUT,*) 'default value may be used, who knows???'
    WRITE(NLUOUT,*) ' '
  ENDIF
!
YRECFM=TRIM(HREC)//'%TIME'
CALL FMREAD(CFILE,YRECFM,COUT,'--',PTIME(:),IGRID,ILENCH,HCOMMENT,KRESP)

   IF (KRESP /=0) THEN
    WRITE(NLUOUT,*) 'WARNING'
    WRITE(NLUOUT,*) '-------'
    WRITE(NLUOUT,*) 'error when reading article ',YRECFM,'KRESP=',KRESP
    WRITE(NLUOUT,*) 'default value may be used, who knows???'
    WRITE(NLUOUT,*) ' '
  ENDIF
!-------------------------------------------------------------------------------
END SUBROUTINE READ_SURFT1_MNH
