!SURFEX_LIC Copyright 1994-2014 Meteo-France 
!SURFEX_LIC This is part of the SURFEX software governed by the CeCILL-C  licence
!SURFEX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!SURFEX_LIC for details. version 1.
!     #########
      SUBROUTINE READ_PREP_GARDEN_SNOW(HPROGRAM,HSNOW,KSNOW_LAYER,HFILE,HFILETYPE)
!     #######################################################
!
!!****  *READ_PREP_GARDEN_SNOW* - routine to read the configuration for snow
!!                              in ISBA fields preparation
!!
!!    PURPOSE
!!    -------
!!
!!**  METHOD
!!    ------
!!
!!    EXTERNAL
!!    --------
!!
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!
!!    REFERENCE
!!    ---------
!!
!!
!!    AUTHOR
!!    ------
!!	V. Masson   *Meteo France*	
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    01/2004 
!!     A. Bogatchev 09/2005 EBA snow option
!!     V. Vionnet   06/2008 - Flag for snow metamorphism
!                           - Preparation of uniform snow fields : density, temperture,albedo,grain types
!!                          - Flag to avtivate new maximal liquid water holding capacity : formulation used by Crocus
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
USE MODN_PREP_GARDEN_SNOW
USE MODD_READ_NAMELIST, ONLY : LNAM_READ
!
USE MODD_SURF_PAR,       ONLY : XUNDEF
USE MODD_SNOW_PAR,   ONLY : XANSMIN, XRHOSMAX
USE MODD_CSTS,       ONLY : XTT
!
USE MODE_POS_SURF
USE MODI_TEST_NAM_VAR_SURF
USE MODI_GET_LUOUT
USE MODI_OPEN_NAMELIST
USE MODI_CLOSE_NAMELIST
USE MODI_ABOR1_SFX
!
USE MODD_PREP_TEB_GARDEN, ONLY : CFILE_SNOW, CTYPE_SNOW, LSNOW_IDEAL, &
                                 XWSNOW_p=>XWSNOW, XTSNOW_p=>XTSNOW, &
                                 XRSNOW_p=>XRSNOW, XASNOW 
!
USE MODD_PREP_SNOW, ONLY : LSNOW_FRAC_TOT, NSNOW_LAYER_MAX
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*       0.1   Declarations of arguments
!              -------------------------
!
 CHARACTER(LEN=6),  INTENT(IN)  :: HPROGRAM ! program calling ISBA
 CHARACTER(LEN=3),  INTENT(OUT) :: HSNOW    ! snow scheme
INTEGER, INTENT(OUT)           :: KSNOW_LAYER  ! number of snow layers
 CHARACTER(LEN=28), OPTIONAL, INTENT(OUT) :: HFILE        ! file name
 CHARACTER(LEN=6),  OPTIONAL, INTENT(OUT) :: HFILETYPE    ! file type
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
REAL, DIMENSION(NSNOW_LAYER_MAX) :: XWSNOW, XRSNOW, XTSNOW, &
                                    XSG1SNOW, XSG2SNOW, XHISTSNOW, XAGESNOW
!
LOGICAL           :: GFOUND         ! Return code when searching namelist
INTEGER           :: ILUOUT         ! output file logical unit
INTEGER           :: ILUNAM         ! namelist file logical unit
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------
NAMELIST/NAM_PREP_ISBA_SNOW/CSNOW, NSNOW_LAYER, CFILE_SNOW, CTYPE_SNOW, &
                            LSNOW_IDEAL, LSNOW_FRAC_TOT,                &
                            XWSNOW, XTSNOW, XRSNOW, XASNOW,             &
                            XSG1SNOW, XSG2SNOW, XHISTSNOW, XAGESNOW
NAMELIST/NAM_PREP_GARDEN_SNOW/CSNOW, NSNOW_LAYER, CFILE_SNOW, CTYPE_SNOW, &
                              LSNOW_IDEAL, XWSNOW, XTSNOW, XRSNOW, XASNOW
!-------------------------------------------------------------------------------
!* default
!  -------
!

IF (LHOOK) CALL DR_HOOK('READ_PREP_GARDEN_SNOW',0,ZHOOK_HANDLE)
IF (LNAM_READ) THEN
  !
  CSNOW = 'D95'
  NSNOW_LAYER = 1
  !
  CFILE_SNOW    = '                         '
  CTYPE_SNOW    = '      '  
  !
  LSNOW_IDEAL = .FALSE.
  LSNOW_FRAC_TOT = .FALSE.
  !
  XWSNOW(:) = 0.
  XRSNOW(:) = XRHOSMAX
  XTSNOW(:) = XTT
  XASNOW = XANSMIN  
  XSG1SNOW(:) = XUNDEF
  XSG2SNOW(:) = XUNDEF
  XHISTSNOW(:) = XUNDEF
  XAGESNOW(:) = XUNDEF  
  !
  CALL GET_LUOUT(HPROGRAM,ILUOUT)
  CALL OPEN_NAMELIST(HPROGRAM,ILUNAM)
  !
  !* reading of namelist
  !  -------------------
  !
  !* default can be provided by ISBA scheme variables
  CALL POSNAM(ILUNAM,'NAM_PREP_ISBA_SNOW',GFOUND,ILUOUT)
  IF (GFOUND) READ(UNIT=ILUNAM,NML=NAM_PREP_ISBA_SNOW)
  !
  CALL TEST_NAM_VAR_SURF(ILUOUT,'CSNOW',CSNOW,'D95','3-L','EBA','CRO','NON')
  !
  !* It is erased by GARDEN namelist if specified
  CALL POSNAM(ILUNAM,'NAM_PREP_GARDEN_SNOW',GFOUND,ILUOUT)
  IF (GFOUND) THEN 
    READ(UNIT=ILUNAM,NML=NAM_PREP_GARDEN_SNOW)
  !crocus can't be used in garden if not used in isba scheme
    CALL TEST_NAM_VAR_SURF(ILUOUT,'CSNOW',CSNOW,'D95','3-L','EBA','CRO','NON')
  ENDIF
  !
  IF (CSNOW=='NON') NSNOW_LAYER = 0
  !
  IF (CSNOW=='D95' .OR. CSNOW=='EBA') NSNOW_LAYER = 1
  ! not more than 3 layers for snow in garden
  IF ((CSNOW=='3-L' .OR. CSNOW=='CRO') .AND. NSNOW_LAYER<=2) NSNOW_LAYER = 3
  !
  IF (CSNOW=='3-L' .AND. NSNOW_LAYER>3) THEN
    NSNOW_LAYER = 3
    WRITE(ILUOUT,*) '------------------------------------'
    WRITE(ILUOUT,*) 'With ISBA-ES, number of snow layers '
    WRITE(ILUOUT,*) 'cannot be more than 3.              '
    WRITE(ILUOUT,*) 'So it is forced to 3 here.          '
    WRITE(ILUOUT,*) '------------------------------------'
  ENDIF
  !  
  IF (NSNOW_LAYER > NSNOW_LAYER_MAX) THEN
    WRITE(ILUOUT,*) '------------------------------------'
    WRITE(ILUOUT,*) 'Please update modd_prep_snow.f90 routine : '
    WRITE(ILUOUT,*) 'The maximum number of snow layers  '
    WRITE(ILUOUT,*) 'in the declaration of the namelist variables '
    WRITE(ILUOUT,*) 'must be decreased to : ', NSNOW_LAYER_MAX
    WRITE(ILUOUT,*) '------------------------------------'
    CALL ABOR1_SFX('READ_PREP_GARDEN_SNOW: NUMBER OF SNOW LAYERS MUST BE INCREASED IN NAMELIST DECLARATION')
  ENDIF
  !
  ALLOCATE(XWSNOW_p(NSNOW_LAYER))
  ALLOCATE(XRSNOW_p(NSNOW_LAYER))
  ALLOCATE(XTSNOW_p(NSNOW_LAYER))
  !
  XWSNOW_p=XWSNOW(1:NSNOW_LAYER)
  XRSNOW_p=XRSNOW(1:NSNOW_LAYER)
  XTSNOW_p=XTSNOW(1:NSNOW_LAYER)
  !
  CALL CLOSE_NAMELIST(HPROGRAM,ILUNAM)
  !
ENDIF
!
HSNOW = CSNOW
!
KSNOW_LAYER = NSNOW_LAYER
!
IF (LEN_TRIM(CFILE_SNOW)>0 .AND. LEN_TRIM(CTYPE_SNOW)>0 ) THEN
  IF (PRESENT(HFILE)) HFILE = CFILE_SNOW
  IF (PRESENT(HFILETYPE)) HFILETYPE = CTYPE_SNOW
END IF
!
IF (LHOOK) CALL DR_HOOK('READ_PREP_GARDEN_SNOW',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------
!
END SUBROUTINE READ_PREP_GARDEN_SNOW
