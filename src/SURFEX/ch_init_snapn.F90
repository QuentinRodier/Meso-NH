!SURFEX_LIC Copyright 1994-2014 Meteo-France 
!SURFEX_LIC This is part of the SURFEX software governed by the CeCILL-C  licence
!SURFEX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!SURFEX_LIC for details. version 1.
!     #########
      SUBROUTINE CH_INIT_SNAP_n(HPROGRAM,KLU,HINIT,KCH,PRHOA)
!     #######################################
!
!!****  *CH_INIT_EMIISION_TEMP_n* - routine to initialize chemical emissions data structure
!!
!!    PURPOSE
!!    -------
!       Allocates and initialize emission surface fields
!       by reading their value in initial file.
!
!!**  METHOD
!!    ------
!!    
!!    
!!    AUTHOR
!!    ------
!!	S.QUEGUINER 
!!
!!    MODIFICATIONS
!!    -------------
!!      Original        11/2011
!!        M.Moge    01/2016  using READ_SURF_FIELD2D for 2D surfex fields reads
!!-----------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!
USE MODD_CSTS,       ONLY : XAVOGADRO, XMD
USE MODD_CH_SNAP_n
USE MODI_GET_LUOUT
USE MODI_READ_SURF
USE MODI_READ_SURF_FIELD2D
USE MODI_ABOR1_SFX
USE MODI_CH_CONVERSION_FACTOR
USE MODI_BUILD_PRONOSLIST_n
USE MODI_CH_OPEN_INPUTB
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
!
IMPLICIT NONE
!
!*       0.1   declarations of arguments
!
 CHARACTER(LEN=6),  INTENT(IN)  :: HPROGRAM ! Program name
INTEGER,           INTENT(IN)  :: KLU      ! number of points
 CHARACTER(LEN=3),  INTENT(IN)  :: HINIT    ! Flag to know if one initializes:
!                                          ! 'ALL' : all variables for a run
!                                          ! 'PRE' : only variables to build 
!                                          !         an initial file
INTEGER,           INTENT(IN)  :: KCH      ! logical unit of input chemistry file
REAL, DIMENSION(:),INTENT(IN)  :: PRHOA    ! air density
!
!*       0.2   declarations of local variables
!
INTEGER             :: IRESP                 !   File 
INTEGER             :: ILUOUT                ! output listing logical unit
 CHARACTER (LEN=16)  :: YRECFM                ! management
 CHARACTER (LEN=100) :: YCOMMENT              ! variables
INTEGER             :: JSPEC                 ! Loop index for chemical species
INTEGER             :: JSNAP                 ! Loop index for SNAP categories
!
 CHARACTER(LEN=40)   :: YSPEC_NAME            ! species name
!
INTEGER             :: IVERSION       ! version of surfex file being read
INTEGER             :: IBUG           ! version of SURFEX bugfix
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('CH_INIT_SNAP_N',0,ZHOOK_HANDLE)
 CALL GET_LUOUT(HPROGRAM,ILUOUT)
!
!* ascendant compatibility
YRECFM='VERSION'
 CALL READ_SURF(HPROGRAM,YRECFM,IVERSION,IRESP)
YRECFM='BUG'
 CALL READ_SURF(HPROGRAM,YRECFM,IBUG,IRESP)
!
!*      1.     Chemical Emission snap configuration
!              ------------------------------------
!
! Read the number of emission species and snaps
IF (IVERSION>7 .OR. (IVERSION==7 .AND. IBUG>=3) ) THEN
  CALL READ_SURF(HPROGRAM,'EMISPEC_NBR',NEMIS_NBR,IRESP)
  CALL READ_SURF(HPROGRAM,'SNAP_NBR',NEMIS_SNAP,IRESP)
  CALL READ_SURF(HPROGRAM,'SNAP_TIME',CSNAP_TIME_REF,IRESP)
ELSE
  CALL ABOR1_SFX('CH_INIT_SNAPN: NO SNAP EMISSIONS IN SURFEX FILE: FILE TOO OLD')
END IF
!
! Number of instants for each temporal profile.
! For the time being, they are constant (even for the diurnal cycle)
!
NSNAP_M=12 ! 12 months
NSNAP_D=7  !  7 day a week
NSNAP_H=24 ! 24 hours a day (=> temporal resolution = 1 hour)
!
!
!*      2.     Chemical Emission fields
!              ------------------------
!
ALLOCATE(CEMIS_NAME       (               NEMIS_NBR))
ALLOCATE(CEMIS_COMMENT    (               NEMIS_NBR))
ALLOCATE(XEMIS_FIELDS_SNAP(KLU,NEMIS_SNAP,NEMIS_NBR))
ALLOCATE(XEMIS_FIELDS     (KLU,           NEMIS_NBR))
LEMIS_FIELDS = .FALSE.
!
ALLOCATE(XSNAP_MONTHLY(NSNAP_M,NEMIS_SNAP,NEMIS_NBR))
ALLOCATE(XSNAP_DAILY  (NSNAP_D,NEMIS_SNAP,NEMIS_NBR))
ALLOCATE(XSNAP_HOURLY (NSNAP_H,NEMIS_SNAP,NEMIS_NBR))
!
IF (CSNAP_TIME_REF=='LEGAL') THEN
  ALLOCATE(XDELTA_LEGAL_TIME(KLU))
  YRECFM='LEGALTIME'
  CALL READ_SURF(HPROGRAM,YRECFM,XDELTA_LEGAL_TIME(:),IRESP,YCOMMENT)
END IF
!
DO JSPEC = 1,NEMIS_NBR ! Loop on the number of species
!
! Read the species name
  WRITE(YRECFM,'("EMISNAME",I3.3)') JSPEC
  CALL READ_SURF(HPROGRAM,YRECFM,YSPEC_NAME,IRESP,YCOMMENT)
  CEMIS_COMMENT(JSPEC)=YCOMMENT
  IF (IRESP/=0) THEN
    CALL ABOR1_SFX('CH_INIT_SNAPN: PROBLEM WHEN READING NAME OF EMITTED CHEMICAL SPECIES')
  END IF
  WRITE(ILUOUT,*) ' Emission ',JSPEC,' : ',TRIM(YSPEC_NAME)
  CEMIS_NAME(JSPEC) = YSPEC_NAME(1:12)
! 
! Read  the potential emission of species for each snap
  DO JSNAP=1,NEMIS_SNAP
    WRITE(YRECFM,'("SNAP",I2.2,"_",A3)') JSNAP,CEMIS_NAME(JSPEC)
    CALL READ_SURF(HPROGRAM,YRECFM,XEMIS_FIELDS_SNAP(:,JSNAP,JSPEC),IRESP,YCOMMENT)
  END DO
!
! Read the temporal profiles of all snaps
  YRECFM = "E_"//TRIM(CEMIS_NAME(JSPEC))//"_M"
  CALL READ_SURF_FIELD2D(HPROGRAM,XSNAP_MONTHLY(:,:,JSPEC),YRECFM,YCOMMENT,HDIR='-')
  YRECFM = "E_"//TRIM(CEMIS_NAME(JSPEC))//"_D"
  CALL READ_SURF_FIELD2D(HPROGRAM,XSNAP_DAILY(:,:,JSPEC),YRECFM,YCOMMENT,HDIR='-')
  YRECFM = "E_"//TRIM(CEMIS_NAME(JSPEC))//"_H"
  CALL READ_SURF_FIELD2D(HPROGRAM,XSNAP_HOURLY(:,:,JSPEC),YRECFM,YCOMMENT,HDIR='-')
END DO
!
!*      3.     Conversion factor
!              -----------------
!
IF (HINIT=='ALL') THEN
  CALL CH_OPEN_INPUTB("EMISUNIT", KCH, ILUOUT)
!
! read unit identifier
  READ(KCH,'(A3)') CCONVERSION
!
  ALLOCATE (XCONVERSION(KLU))
! determine the conversion factor
  CALL CH_CONVERSION_FACTOR(CCONVERSION,PRHOA)
!
!*      4.     List of emissions to be aggregated into atm. chemical species
!              -------------------------------------------------------------
!
  CALL BUILD_PRONOSLIST_n(NEMIS_NBR,CEMIS_NAME,TSPRONOSLIST,KCH,ILUOUT,6)
!
!-------------------------------------------------------------------------------
END IF
!
IF (LHOOK) CALL DR_HOOK('CH_INIT_SNAP_N',1,ZHOOK_HANDLE)
!-------------------------------------------------------------------------------
!
END SUBROUTINE CH_INIT_SNAP_n
