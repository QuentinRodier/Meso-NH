!MNH_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
!MNH_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!MNH_LIC for details. version 1.
!-----------------------------------------------------------------
!--------------- special set of characters for RCS information
!-----------------------------------------------------------------
! $Source$ $Revision$
! MASDEV4_7 init 2007/03/23 11:55:57
!-----------------------------------------------------------------
!      ########################
       MODULE MODI_INI_MICRO_n 
!      ########################
!
INTERFACE
      SUBROUTINE INI_MICRO_n  ( KLUOUT )
!
INTEGER,                 INTENT(IN) :: KLUOUT    ! Logical unit number for prints
!
END SUBROUTINE INI_MICRO_n 
!
END INTERFACE
!
END MODULE MODI_INI_MICRO_n 
!     ##################################
      SUBROUTINE  INI_MICRO_n ( KLUOUT )
!     ##################################
!
!
!!****  *INI_MICRO_n* allocates and fills MODD_PRECIP_n variables 
!!                    and initialize parameter for microphysical scheme
!!
!!    PURPOSE
!!    -------
!!
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
!!      P. Jabouille
!!
!!    MODIFICATIONS
!!    -------------
!!      Original         27/11/02
!!      O.Geoffroy (03/2006) : Add KHKO scheme
!!
!! --------------------------------------------------------------------------
!
!*      0. DECLARATIONS
!          ------------
!
!
USE MODD_NSV, ONLY : NSV,NSV_CHEM,NSV_C2R2BEG,NSV_C2R2END, &
                                  NSV_C1R3BEG,NSV_C1R3END
USE MODD_CONF, ONLY : CCONF,CPROGRAM       
USE MODD_LUNIT_n, ONLY : CINIFILE,CLUOUT
USE MODD_GET_n, ONLY : CGETRCT,CGETRRT, CGETRST, CGETRGT, CGETRHT, CGETCLOUD
USE MODD_DIM_n, ONLY : NIMAX_ll, NJMAX_ll
USE MODD_PARAMETERS, ONLY : JPVEXT, JPHEXT
USE MODD_PARAM_n, ONLY : CCLOUD
USE MODD_PRECIP_n, ONLY : XINPRR, XACPRR, XINPRS, XACPRS, XINPRG, XACPRG, &
                          XINPRH, XACPRH, XINPRC, XACPRC, XINPRR3D, XEVAP3D
USE MODD_FIELD_n, ONLY : XRT, XSVT, XTHT, XPABST, XTHM, XRCM
USE MODD_GRID_n, ONLY : XZZ
USE MODD_METRICS_n, ONLY : XDXX,XDYY,XDZZ,XDZX,XDZY
USE MODD_REF_n, ONLY : XRHODREF
USE MODD_DYN_n, ONLY : XTSTEP
USE MODD_CLOUDPAR_n, ONLY : NSPLITR, NSPLITG
USE MODD_PARAM_n, ONLY : CELEC
USE MODD_PARAM_ICE,  ONLY : LSEDIC
USE MODD_PARAM_C2R2, ONLY : LSEDC, LACTIT
!
USE MODI_READ_PRECIP_FIELD
USE MODI_INI_CLOUD
USE MODI_INI_RAIN_ICE
USE MODI_INI_RAIN_C2R2
USE MODI_INI_ICE_C1R3
USE MODI_CLEAN_CONC_RAIN_C2R2
USE MODI_SET_CONC_RAIN_C2R2
USE MODI_CLEAN_CONC_ICE_C1R3
USE MODI_SET_CONC_ICE_C1R3
!
USE MODE_ll
USE MODE_MODELN_HANDLER
!
IMPLICIT NONE
!
!*       0.1   Declarations of dummy arguments :
!
INTEGER,                 INTENT(IN) :: KLUOUT    ! Logical unit number for prints
!
!
!       0.2  declaration of local variables
!
!
!
INTEGER             :: IIU     ! Upper dimension in x direction (local)
INTEGER             :: IJU     ! Upper dimension in y direction (local)
INTEGER             :: IKU     ! Upper dimension in z direction
INTEGER             :: JK      ! loop vertical index
INTEGER             :: IINFO_ll! Return code of //routines
INTEGER             :: IKB,IKE
!
REAL, DIMENSION(:,:,:), ALLOCATABLE  :: ZDZ    ! mesh size
REAL :: ZDZMIN
INTEGER :: IMI
!
!-------------------------------------------------------------------------------
!
!*       1.    PROLOGUE
!
!
CALL GET_DIM_EXT_ll('B',IIU,IJU)
IKU=SIZE(XZZ,3)
!
!
!*       2.    ALLOCATE  Module MODD_PRECIP_n
!              ------------------------------
!
IF (CCLOUD /= 'NONE' .AND. CCLOUD /= 'REVE') THEN
  ALLOCATE(XINPRR(IIU,IJU))
  ALLOCATE(XINPRR3D(IIU,IJU,IKU))
  ALLOCATE(XEVAP3D(IIU,IJU,IKU))
  ALLOCATE(XACPRR(IIU,IJU))
  XINPRR(:,:)=0.0
  XACPRR(:,:)=0.0
  XINPRR3D(:,:,:)=0.0
  XEVAP3D(:,:,:)=0.0
ELSE
  ALLOCATE(XINPRR(0,0))
  ALLOCATE(XINPRR3D(0,0,0))
  ALLOCATE(XEVAP3D(0,0,0))
  ALLOCATE(XACPRR(0,0))
END IF
!
IF ((CCLOUD(1:3) == 'ICE' .AND. LSEDIC)  .OR. &
    ((CCLOUD == 'C2R2' .OR. CCLOUD == 'C3R5' .OR. &
    CCLOUD == 'KHKO') .AND. LSEDC)) THEN
  ALLOCATE(XINPRC(IIU,IJU))
  ALLOCATE(XACPRC(IIU,IJU))
  XINPRC(:,:)=0.0
  XACPRC(:,:)=0.0
ELSE
  ALLOCATE(XINPRC(0,0))
  ALLOCATE(XACPRC(0,0))
END IF
!
IF (CCLOUD(1:3) == 'ICE' .OR. CCLOUD == 'C3R5') THEN
  ALLOCATE(XINPRS(IIU,IJU))
  ALLOCATE(XACPRS(IIU,IJU))
  XINPRS(:,:)=0.0
  XACPRS(:,:)=0.0
ELSE
  ALLOCATE(XINPRS(0,0))
  ALLOCATE(XACPRS(0,0))
 END IF
!
IF (CCLOUD == 'C3R5' .OR. CCLOUD(1:3) == 'ICE' ) THEN
  ALLOCATE(XINPRG(IIU,IJU))
  ALLOCATE(XACPRG(IIU,IJU))
  XINPRG(:,:)=0.0
  XACPRG(:,:)=0.0
ELSE
  ALLOCATE(XINPRG(0,0))
  ALLOCATE(XACPRG(0,0))
END IF
!
IF (CCLOUD =='ICE4') THEN
  ALLOCATE(XINPRH(IIU,IJU))
  ALLOCATE(XACPRH(IIU,IJU))
  XINPRH(:,:)=0.0
  XACPRH(:,:)=0.0
ELSE
  ALLOCATE(XINPRH(0,0))
  ALLOCATE(XACPRH(0,0))
END IF
!
IF(SIZE(XINPRR) == 0) RETURN
!
!*       2b.    ALLOCATION for Radiative cooling 
!              ------------------------------
IF (LACTIT) THEN
  ALLOCATE( XTHM(IIU,IJU,IKU) )
  ALLOCATE( XRCM(IIU,IJU,IKU) )
  XTHM = XTHT
  XRCM(:,:,:) = XRT(:,:,:,2)
             ELSE
  ALLOCATE( XTHM(0,0,0) )
  ALLOCATE( XRCM(0,0,0) )
END IF
!
!*       3.    INITIALIZE MODD_PRECIP_n variables
!              ----------------------------------
!
CALL READ_PRECIP_FIELD(CINIFILE,CLUOUT,CPROGRAM,CCONF,                           &
                  CGETRCT,CGETRRT,CGETRST,CGETRGT,CGETRHT,                       &
                  XINPRC,XACPRC,XINPRR,XINPRR3D,XEVAP3D,                         &
                  XACPRR,XINPRS,XACPRS,XINPRG,XACPRG, XINPRH,XACPRH )           
!
!
!*       4.    INITIALIZE THE PARAMETERS FOR THE MICROPHYSICS
!              ----------------------------------------------
!
!
!*       4.1    Compute the minimun vertical mesh size
!
ALLOCATE(ZDZ(IIU,IJU,IKU))
ZDZ=0.
IKB = 1 + JPVEXT
IKE = SIZE(XZZ,3)- JPVEXT
DO JK = IKB,IKE
  ZDZ(:,:,JK) = XZZ(:,:,JK+1) - XZZ(:,:,JK)
END DO
ZDZMIN = MIN_ll (ZDZ,IINFO_ll,1,1,IKB,NIMAX_ll+2*JPHEXT,NJMAX_ll+2*JPHEXT,IKE )
DEALLOCATE(ZDZ)
IF (CCLOUD(1:3) == 'KES') THEN
  CALL INI_CLOUD(XTSTEP,ZDZMIN,NSPLITR)                  ! Warm cloud only
ELSE IF (CCLOUD(1:3) == 'ICE'  ) THEN
  CALL INI_RAIN_ICE(KLUOUT,XTSTEP,ZDZMIN,NSPLITR,CCLOUD)        ! Mixed phase cloud
                                                         ! including hail
ELSE IF (CCLOUD == 'C2R2' .OR. CCLOUD == 'C3R5' &
         .OR. CCLOUD == 'KHKO') THEN
  CALL INI_RAIN_C2R2(XTSTEP,ZDZMIN,NSPLITR,CCLOUD)        ! 1/2 spectral warm cloud
  IF (CCLOUD == 'C3R5') THEN
    CALL INI_ICE_C1R3(XTSTEP,ZDZMIN,NSPLITG)       ! 1/2 spectral cold cloud
  END IF
END IF
!
IF (CCLOUD == 'C2R2' .OR. CCLOUD == 'C3R5' .OR. CCLOUD == 'KHKO') THEN
  IF (CGETCLOUD=='READ') THEN
    CALL CLEAN_CONC_RAIN_C2R2 (CLUOUT,XRT,XSVT(:,:,:,NSV_C2R2BEG:NSV_C2R2END))
  ELSE IF (CGETCLOUD=='INI1'.OR.CGETCLOUD=='INI2') THEN
    CALL SET_CONC_RAIN_C2R2 (CLUOUT,CGETCLOUD,XRHODREF,&
         &XRT,XSVT(:,:,:,NSV_C2R2BEG:NSV_C2R2END))
  ENDIF
  IF (CCLOUD == 'C3R5' ) THEN
    IF (CGETCLOUD=='READ') THEN
      CALL CLEAN_CONC_ICE_C1R3 (CLUOUT,XRT,XSVT(:,:,:,NSV_C2R2BEG:NSV_C1R3END))
    ELSE
      CALL SET_CONC_ICE_C1R3 (CLUOUT,XRHODREF,&
           &XRT,XSVT(:,:,:,NSV_C2R2BEG:NSV_C1R3END))
    ENDIF
  ENDIF
ENDIF
!
!
!*       5.    INITIALIZE ATMOSPHERIC ELECTRICITY
!              ----------------------------------
!
!
IMI = GET_CURRENT_MODEL_INDEX()
!IF (CELEC /= 'NONE') THEN
!  CALL INI_ELEC(IMI,CINIFILE,CLUOUT,XTSTEP,ZDZMIN,NSPLITR, &
!                XDXX,XDYY,XDZZ,XDZX,XDZY                   )
!END IF
!
!
END SUBROUTINE INI_MICRO_n
