!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE PGD_TEB_GARDEN_PAR (DTCO, UG, U, USS, KDIM, IO, DTV, TOP, HPROGRAM)
!     ##############################################################
!
!!**** *PGD_TEB_GARDEN_PAR* monitor for averaging and interpolations of cover fractions
!!
!!    PURPOSE
!!    -------
!!
!!    METHOD
!!    ------
!!   
!
!!    EXTERNAL
!!    --------
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!
!!    REFERENCE
!!    ---------
!!
!!    AUTHOR
!!    ------
!!
!!    A. Lemonsu       Meteo-France
!!
!!    MODIFICATION
!!    ------------
!!
!!    Original    09/2009
!!
!----------------------------------------------------------------------------
!
!*    0.     DECLARATION
!            -----------
!
!
!
USE MODD_DATA_COVER_n, ONLY : DATA_COVER_t
USE MODD_SURF_ATM_GRID_n, ONLY : SURF_ATM_GRID_t
USE MODD_SURF_ATM_n, ONLY : SURF_ATM_t
USE MODD_SSO_n, ONLY : SSO_t
USE MODD_TEB_OPTION_n, ONLY : TEB_OPTIONS_t
USE MODD_ISBA_OPTIONS_n, ONLY : ISBA_OPTIONS_t
USE MODD_DATA_ISBA_n, ONLY : DATA_ISBA_t
USE MODD_SURF_PAR, ONLY : XUNDEF
USE MODD_PGDWORK, ONLY : CATYPE
!
USE MODI_TEST_NAM_VAR_SURF
USE MODI_GET_LUOUT
USE MODI_OPEN_NAMELIST
USE MODI_CLOSE_NAMELIST
USE MODI_PGD_FIELD
USE MODI_ABOR1_SFX
!
USE MODE_POS_SURF
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*    0.1    Declaration of arguments
!            ------------------------
!
!
TYPE(DATA_COVER_t), INTENT(INOUT) :: DTCO
TYPE(SURF_ATM_GRID_t), INTENT(INOUT) :: UG
TYPE(SURF_ATM_t), INTENT(INOUT) :: U
TYPE(SSO_t), INTENT(INOUT) :: USS
INTEGER, INTENT(IN) :: KDIM
TYPE(ISBA_OPTIONS_t), INTENT(INOUT) :: IO
TYPE(DATA_ISBA_t), INTENT(INOUT) :: DTV
TYPE(TEB_OPTIONS_t), INTENT(INOUT) :: TOP
!
 CHARACTER(LEN=6),    INTENT(IN)    :: HPROGRAM     ! Type of program
!
!
!*    0.2    Declaration of local variables
!            ------------------------------
!
INTEGER               :: ILUOUT    ! output listing logical unit
INTEGER               :: ILUNAM    ! namelist file  logical unit
LOGICAL               :: GFOUND    ! true if namelist is found
INTEGER               :: JTIME     ! loop counter on time
!
!*    0.3    Declaration of namelists
!            ------------------------
!
INTEGER                                :: NTIME_GD
INTEGER, PARAMETER                     :: NGROUND_MAX  = 20
INTEGER, PARAMETER                     :: NVEGTYPE_MAX = 19
INTEGER, PARAMETER                     :: NTIME_MAX    = 12
!
! type of vegetation
!
 CHARACTER(LEN=4)                       :: CTYP_GARDEN_HVEG ! type of high vegetation
 CHARACTER(LEN=4)                       :: CTYP_GARDEN_LVEG ! type of low  vegetation
 CHARACTER(LEN=4)                       :: CTYP_GARDEN_NVEG ! type of bare soil
 CHARACTER(LEN=3)                       :: CSHAPE_GARDEN_HVEG ! shape of crown for urban trees
                                                              ! (only used for CURBTREE = 'TREE' or 'GRWL')
!
!
! uniform value
!
REAL,DIMENSION(NTIME_MAX)              :: XUNIF_LAI_HVEG   ! LAI       of high vegetation
REAL,DIMENSION(NTIME_MAX)              :: XUNIF_LAI_LVEG   ! LAI       of low  vegetation
REAL                                   :: XUNIF_H_HVEG     ! height of trees
REAL                                   :: XUNIF_HTRUNK_HVEG    ! height of TRUNK of trees
REAL                                   :: XUNIF_WCROWN_HVEG    ! width of crown of trees
REAL                                   :: XUNIF_RE25           ! Ecosystem Respiration parameter (kg.m-2.s-1)
!
! name of files containing data
!
 CHARACTER(LEN=28),DIMENSION(NTIME_MAX) :: CFNAM_LAI_HVEG   ! LAI       of high vegetation
 CHARACTER(LEN=28),DIMENSION(NTIME_MAX) :: CFNAM_LAI_LVEG   ! LAI       of low  vegetation
 CHARACTER(LEN=28)                      :: CFNAM_H_HVEG     ! height of trees
 CHARACTER(LEN=28)                      :: CFNAM_HTRUNK_HVEG   ! height of TRUNK of trees
 CHARACTER(LEN=28)                      :: CFNAM_WCROWN_HVEG   ! width of crown of trees
 CHARACTER(LEN=28)                      :: CFNAM_RE25          ! Ecosystem Respiration parameter (kg.m-2.s-1)
 
!
! type of files containing data
!
 CHARACTER(LEN=28),DIMENSION(NTIME_MAX) :: CFTYP_LAI_HVEG   ! LAI       of high vegetation
 CHARACTER(LEN=28),DIMENSION(NTIME_MAX) :: CFTYP_LAI_LVEG   ! LAI       of low  vegetation
 CHARACTER(LEN=28)                      :: CFTYP_H_HVEG     ! height of trees
 CHARACTER(LEN=28)                      :: CFTYP_HTRUNK_HVEG   ! height of TRUNK of trees
 CHARACTER(LEN=28)                      :: CFTYP_WCROWN_HVEG   ! width of crown of trees
 CHARACTER(LEN=28)                      :: CFTYP_RE25          ! Ecosystem Respiration parameter (kg.m-2.s-1)
 
!
LOGICAL :: GDATA_HVEG
LOGICAL :: GDATA_LVEG
LOGICAL :: GDATA_CROWN
LOGICAL :: GDATA_TRUNK
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
NAMELIST/NAM_DATA_TEB_GARDEN/   NTIME_GD,                                          &
                                CTYP_GARDEN_HVEG, CTYP_GARDEN_LVEG,                &
                                CTYP_GARDEN_NVEG,                                  &
                                CSHAPE_GARDEN_HVEG,                                &
                                XUNIF_LAI_HVEG , XUNIF_LAI_LVEG ,                  &
                                XUNIF_H_HVEG   ,                                   &
                                XUNIF_HTRUNK_HVEG, XUNIF_WCROWN_HVEG,              &
                                XUNIF_RE25,                                        &
                                CFNAM_LAI_HVEG , CFNAM_LAI_LVEG ,                  &
                                CFNAM_H_HVEG   ,                                   &
                                CFNAM_HTRUNK_HVEG, CFNAM_WCROWN_HVEG,              &
                                CFNAM_RE25,                                        &
                                CFTYP_LAI_HVEG , CFTYP_LAI_LVEG ,                  &
                                CFTYP_H_HVEG ,                                     & 
                                CFTYP_HTRUNK_HVEG, CFTYP_WCROWN_HVEG,              &
                                CFTYP_RE25

!-------------------------------------------------------------------------------
!
!*    1.      Initializations
!             ---------------
!
IF (LHOOK) CALL DR_HOOK('PGD_TEB_GARDEN_PAR',0,ZHOOK_HANDLE)

NTIME_GD = 12
!
CTYP_GARDEN_HVEG   = 'TEBD'           ! Temperate broadleaf trees (forest)
CTYP_GARDEN_LVEG   = 'GRAS'           ! Grassland
CTYP_GARDEN_NVEG   = 'NO  '           ! No vegetation
CSHAPE_GARDEN_HVEG = 'CYL'            ! cylindric shape of crown (urban trees)
!
XUNIF_LAI_HVEG     = XUNDEF
XUNIF_LAI_LVEG     = XUNDEF
XUNIF_H_HVEG       = XUNDEF
XUNIF_HTRUNK_HVEG  = XUNDEF
XUNIF_WCROWN_HVEG  = XUNDEF
XUNIF_RE25         = XUNDEF
!
CFNAM_LAI_HVEG     = '                            '
CFNAM_LAI_LVEG     = '                            '
CFNAM_H_HVEG       = '                            '
CFNAM_HTRUNK_HVEG  = '                            '
CFNAM_WCROWN_HVEG  = '                            '
CFNAM_RE25         = '                            '
!
CFTYP_LAI_HVEG     = '      '
CFTYP_LAI_LVEG     = '      '
CFTYP_H_HVEG       = '      '
CFTYP_HTRUNK_HVEG  = '      '
CFTYP_WCROWN_HVEG  = '      '
CFTYP_RE25         = '      '
!
!-------------------------------------------------------------------------------
DTV%NTIME = 12
!-------------------------------------------------------------------------------
IO%CTYPE_HVEG = CTYP_GARDEN_HVEG
IO%CTYPE_LVEG = CTYP_GARDEN_LVEG
IO%CTYPE_NVEG = CTYP_GARDEN_NVEG
!-------------------------------------------------------------------------------
!
!*    2.      Input file for cover types
!             --------------------------
!
 CALL GET_LUOUT(HPROGRAM,ILUOUT)
 CALL OPEN_NAMELIST(HPROGRAM,ILUNAM)
!
 CALL POSNAM(ILUNAM,'NAM_DATA_TEB_GARDEN',GFOUND,ILUOUT)
IF (GFOUND) READ(UNIT=ILUNAM,NML=NAM_DATA_TEB_GARDEN)
!
 CALL CLOSE_NAMELIST(HPROGRAM,ILUNAM)
!
IF (NTIME_GD==1) THEN
  XUNIF_LAI_HVEG(2:) = XUNIF_LAI_HVEG(1)
  XUNIF_LAI_LVEG(2:) = XUNIF_LAI_LVEG(1)
ELSE IF (NTIME_GD/=12) THEN
  CALL ABOR1_SFX( 'Namelist NAM_DATA_TEB_GARDEN: NTIME_GD must be equal to 1 or 12')
END IF
!
IF (LEN_TRIM(CTYP_GARDEN_HVEG)>0) THEN
  CALL TEST_NAM_VAR_SURF(ILUOUT,'CTYP_GARDEN_HVEG',CTYP_GARDEN_HVEG, &
        'BOBD','TEBD','TRBD','TEBE','TRBE','BONE','TENE','BOND','SHRB','FLTR')
  IO%CTYPE_HVEG = CTYP_GARDEN_HVEG
END IF
!
IF (LEN_TRIM(CTYP_GARDEN_LVEG)>0) THEN
  CALL TEST_NAM_VAR_SURF(ILUOUT,'CTYP_GARDEN_LVEG',CTYP_GARDEN_LVEG, &
        'BOGR','GRAS','TROG','C3W ','C3S ','C4  ','FLGR','C3  ','PARK')
  IO%CTYPE_LVEG = CTYP_GARDEN_LVEG
END IF
!
IF (LEN_TRIM(CTYP_GARDEN_NVEG)>0) THEN
  CALL TEST_NAM_VAR_SURF(ILUOUT,'CTYP_GARDEN_NVEG',CTYP_GARDEN_NVEG,'NO ','ROCK','SNOW')
  IO%CTYPE_NVEG = CTYP_GARDEN_NVEG
END IF
!
IF (LEN_TRIM(CSHAPE_GARDEN_HVEG)>0) THEN
  ! For the moment only cylindric shape of crown 'CYL' are available
  CALL TEST_NAM_VAR_SURF(ILUOUT,'CSHAPE_GARDEN_HVEG',CSHAPE_GARDEN_HVEG,'CYL')
END IF
!
!-------------------------------------------------------------------------------
!
DTV%NTIME = NTIME_GD
!
ALLOCATE(DTV%XPAR_LAI_HVEG    (KDIM,DTV%NTIME))
ALLOCATE(DTV%XPAR_LAI_LVEG    (KDIM,DTV%NTIME))
ALLOCATE(DTV%XPAR_H_VEG       (KDIM,1,1    ))
ALLOCATE(DTV%XPAR_HTRUNK_HVEG (KDIM     ))
ALLOCATE(DTV%XPAR_WCROWN_HVEG (KDIM     ))
ALLOCATE(DTV%XPAR_RE25        (KDIM,1   ))
!
!-------------------------------------------------------------------------------
!
!*    3.      Uniform fields are prescribed
!             -----------------------------
!
CATYPE = 'ARI'
!
 CALL PGD_FIELD(DTCO, UG, U, USS, &
                HPROGRAM,'Ecosystem Respiration parameter (kg.m-2.s-1) ' ,'TWN',CFNAM_RE25,   &
                 CFTYP_RE25,XUNIF_RE25,DTV%XPAR_RE25(:,1), DTV%LDATA_RE25(1))    
!
!-------------------------------------------------------------------------------
!
CATYPE = 'ARI'
IO%LPAR = .FALSE.
!
DO JTIME=1,DTV%NTIME
!
 CALL PGD_FIELD(DTCO, UG, U, USS, &
                HPROGRAM,'LAI_HVEG: LAI of high vegetation','TWN',CFNAM_LAI_HVEG(JTIME),  &
                  CFTYP_LAI_HVEG(JTIME),XUNIF_LAI_HVEG(JTIME),DTV%XPAR_LAI_HVEG(:,JTIME), GDATA_HVEG)  
!
 CALL PGD_FIELD(DTCO, UG, U, USS, &
                HPROGRAM,'LAI_LVEG: LAI of low  vegetation','TWN',CFNAM_LAI_LVEG(JTIME),  &
                  CFTYP_LAI_LVEG(JTIME),XUNIF_LAI_LVEG(JTIME),DTV%XPAR_LAI_LVEG(:,JTIME), GDATA_LVEG)  
!
!
ENDDO
!-------------------------------------------------------------------------------
IO%LPAR = GDATA_HVEG .OR. GDATA_LVEG
!-------------------------------------------------------------------------------
!
! Additional fields for high vegetation (CURBTREE)
!
ALLOCATE(DTV%LDATA_H_VEG(1))
CALL PGD_FIELD(DTCO, UG, U, USS, &
               HPROGRAM,'H_HVEG: height of trees','TWN',CFNAM_H_HVEG,                     &
                CFTYP_H_HVEG,XUNIF_H_HVEG,DTV%XPAR_H_VEG(:,1,1),DTV%LDATA_H_VEG(1))  
!
IF (TOP%CURBTREE=='TREE'.OR.TOP%CURBTREE=='GRWL') THEN
 CALL PGD_FIELD(DTCO, UG, U, USS, &
                HPROGRAM,'HTRUNK_HVEG: height of trunk of trees','TWN',CFNAM_HTRUNK_HVEG,  &
                 CFTYP_HTRUNK_HVEG,XUNIF_HTRUNK_HVEG,DTV%XPAR_HTRUNK_HVEG(:),GDATA_TRUNK)  
!
 CALL PGD_FIELD(DTCO, UG, U, USS, &
                HPROGRAM,'WCROWN_HVEG: width of crown of trees','TWN',CFNAM_WCROWN_HVEG,   &
                 CFTYP_WCROWN_HVEG,XUNIF_WCROWN_HVEG,DTV%XPAR_WCROWN_HVEG(:),GDATA_CROWN)
!
 IF( DTV%LDATA_H_VEG(1) .AND. .NOT. (GDATA_TRUNK .AND. GDATA_CROWN)) THEN
  WRITE(ILUOUT,*) ' Error in namelist NAM_DATA_TEB_GARDEN : '
  WRITE(ILUOUT,*) ' If data is provided for height of trees (H_TREE)'
  WRITE(ILUOUT,*) ' Then it must be the case for TRUNK height and CROWN width'
  CALL ABOR1_SFX( 'Namelist NAM_DATA_TEB_GARDEN: TRUNCK and CROWN must be provided if H_TREE is.')
 END IF
ELSE
!
  DTV%XPAR_HTRUNK_HVEG (:) = XUNDEF
  DTV%XPAR_WCROWN_HVEG (:) = XUNDEF
!
ENDIF
!
IF (LHOOK) CALL DR_HOOK('PGD_TEB_GARDEN_PAR',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE PGD_TEB_GARDEN_PAR
