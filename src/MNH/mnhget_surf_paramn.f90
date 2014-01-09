!MNH_LIC Copyright 1994-2013 CNRS, Meteo-France and Universite Paul Sabatier
!MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
!MNH_LIC version 1. See LICENCE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!MNH_LIC for details. version 1.
!-----------------------------------------------------------------
!--------------- special set of characters for RCS information
!-----------------------------------------------------------------
! $Source$ $Revision$
! MASDEV4_7 surfex 2006/10/24 10:43:18
!-----------------------------------------------------------------
!     #######################
      MODULE MODI_MNHGET_SURF_PARAM_n
!     #######################
INTERFACE
      SUBROUTINE MNHGET_SURF_PARAM_n(PCOVER,PSEA,KCOVER,PRN,PH,PLE,PLEI,PGFLUX, &
                                     PT2M,PQ2M,PHU2M,PZON10M,PMER10M,PZS,PTOWN,&
                                     PLAI, PVH )
!
REAL, DIMENSION(:,:,:), INTENT(OUT), OPTIONAL :: PCOVER  ! cover types
REAL, DIMENSION(:,:),   INTENT(OUT), OPTIONAL :: PSEA    ! sea fraction
REAL, DIMENSION(:,:),   INTENT(OUT), OPTIONAL :: PTOWN   ! town fraction
INTEGER,                INTENT(OUT), OPTIONAL :: KCOVER  ! number of cover types
REAL, DIMENSION(:,:), INTENT(OUT), OPTIONAL :: PVH     
REAL, DIMENSION(:,:), INTENT(OUT), OPTIONAL :: PLAI   
REAL, DIMENSION(:),     INTENT(INOUT), OPTIONAL :: PRN           ! Net radiation at surface    (W/m2)
REAL, DIMENSION(:),     INTENT(INOUT), OPTIONAL :: PH            ! Sensible heat flux          (W/m2)
REAL, DIMENSION(:),     INTENT(INOUT), OPTIONAL :: PLE           ! Total Latent heat flux      (W/m2)
REAL, DIMENSION(:),     INTENT(INOUT), OPTIONAL :: PLEI          ! Solid Latent heat flux      (W/m2)
REAL, DIMENSION(:),     INTENT(INOUT), OPTIONAL :: PGFLUX        ! Net soil-vegetation flux    (W/m2)
REAL, DIMENSION(:),     INTENT(INOUT), OPTIONAL :: PT2M          ! Air temperature at 2 meters (K)
REAL, DIMENSION(:),     INTENT(INOUT), OPTIONAL :: PQ2M          ! Air humidity at 2 meters    (kg/kg)
REAL, DIMENSION(:),     INTENT(INOUT), OPTIONAL :: PHU2M         ! Air relative humidity at 2 meters (-)
REAL, DIMENSION(:),     INTENT(INOUT), OPTIONAL :: PZON10M       ! zonal Wind at 10 meters     (m/s)
REAL, DIMENSION(:),     INTENT(INOUT), OPTIONAL :: PMER10M       ! meridian Wind at 10 meters  (m/s)
REAL, DIMENSION(:,:),   INTENT(OUT), OPTIONAL :: PZS             ! orography
!
END SUBROUTINE MNHGET_SURF_PARAM_n
!
END INTERFACE
END MODULE MODI_MNHGET_SURF_PARAM_n
!
!     ########################################
      SUBROUTINE MNHGET_SURF_PARAM_n(PCOVER,PSEA,KCOVER,PRN,PH,PLE,PLEI,PGFLUX, &
                                     PT2M,PQ2M,PHU2M,PZON10M,PMER10M,PZS,PTOWN,&
                                     PLAI, PVH )
!     ########################################
!
!!****  *MNHGET_SURF_PARAM_n* - gets some surface fields on MESONH grid
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
!!      Original    01/2003
!!      10/09    (P. Aumond) Add possibility to get H_tree and Leaf area index

!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
USE MODD_PARAMETERS,     ONLY : XUNDEF
!
USE MODD_IO_SURF_MNH,    ONLY : NHALO
!
USE MODI_GET_COVER_N
USE MODI_GET_FRAC_N
USE MODI_GET_JCOVER_N
USE MODI_GET_FLUX_N
USE MODI_GET_ZS_N
!
IMPLICIT NONE
!
!*       0.1   Declarations of arguments
!              -------------------------
!
REAL, DIMENSION(:,:,:), INTENT(OUT), OPTIONAL :: PCOVER  ! cover types
REAL, DIMENSION(:,:),   INTENT(OUT), OPTIONAL :: PSEA    ! sea fraction
REAL, DIMENSION(:,:),   INTENT(OUT), OPTIONAL :: PTOWN   ! town fraction
INTEGER,                INTENT(OUT), OPTIONAL :: KCOVER  ! number of cover types
REAL, DIMENSION(:,:),   INTENT(OUT), OPTIONAL :: PVH     
REAL, DIMENSION(:,:),   INTENT(OUT), OPTIONAL :: PLAI
REAL, DIMENSION(:),     INTENT(INOUT), OPTIONAL :: PRN           ! Net radiation at surface    (W/m2)
REAL, DIMENSION(:),     INTENT(INOUT), OPTIONAL :: PH            ! Sensible heat flux          (W/m2)
REAL, DIMENSION(:),     INTENT(INOUT), OPTIONAL :: PLE           ! Total Latent heat flux      (W/m2)
REAL, DIMENSION(:),     INTENT(INOUT), OPTIONAL :: PLEI          ! Solid Latent heat flux      (W/m2)
REAL, DIMENSION(:),     INTENT(INOUT), OPTIONAL :: PGFLUX        ! Net soil-vegetation flux    (W/m2)
REAL, DIMENSION(:),     INTENT(INOUT), OPTIONAL :: PT2M      ! Air temperature at 2 meters (K)
REAL, DIMENSION(:),     INTENT(INOUT), OPTIONAL :: PQ2M      ! Air humidity at 2 meters    (kg/kg)
REAL, DIMENSION(:),     INTENT(INOUT), OPTIONAL :: PHU2M     ! Air relative humidity at 2 meters (-)
REAL, DIMENSION(:),     INTENT(INOUT), OPTIONAL :: PZON10M       ! zonal Wind at 10 meters     (m/s)
REAL, DIMENSION(:),     INTENT(INOUT), OPTIONAL :: PMER10M       ! meridian Wind at 10 meters  (m/s)
REAL, DIMENSION(:,:),   INTENT(OUT), OPTIONAL :: PZS             ! orography
!
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
INTEGER :: IIB      ! X array physical boundary
INTEGER :: IJB      ! Y array physical boundary
INTEGER :: IIE      ! X array physical boundary
INTEGER :: IJE      ! Y array physical boundary
INTEGER :: ICOVER   ! number of cover types
INTEGER :: JCOVER   ! loop on cover types
INTEGER :: ILU      ! total number of physical points (including halo)
INTEGER :: ILM      ! total number of physical points
!
REAL, DIMENSION(:), ALLOCATABLE :: ZCOVER ! cover field
REAL, DIMENSION(:),   ALLOCATABLE :: ZSEA   ! sea    fraction
REAL, DIMENSION(:),   ALLOCATABLE :: ZWATER ! lake   fraction
REAL, DIMENSION(:),   ALLOCATABLE :: ZNATURE! nature fraction
REAL, DIMENSION(:),   ALLOCATABLE :: ZTOWN  ! town   fraction
REAL, DIMENSION(:),   ALLOCATABLE :: ZVH     
REAL, DIMENSION(:),   ALLOCATABLE :: ZLAI
REAL, DIMENSION(:),   ALLOCATABLE :: ZZS    ! orography
REAL, DIMENSION(:),   ALLOCATABLE :: ZRN    ! net radiation at surface    (W/m2)
REAL, DIMENSION(:),   ALLOCATABLE :: ZH     ! Sensible heat flux          (W/m2)
REAL, DIMENSION(:),   ALLOCATABLE :: ZLE    ! Total Latent heat flux      (W/m2)
REAL, DIMENSION(:),   ALLOCATABLE :: ZLEI   ! Solid Latent heat flux      (W/m2)
REAL, DIMENSION(:),   ALLOCATABLE :: ZGFLUX ! Net soil-vegetation flux    (W/m2)
REAL, DIMENSION(:),   ALLOCATABLE :: ZT2M   ! Air temperature at 2 meters (K)
REAL, DIMENSION(:),   ALLOCATABLE :: ZQ2M   ! Air humidity at 2 meters    (kg/kg)
REAL, DIMENSION(:),   ALLOCATABLE :: ZHU2M  ! Air relative humidity at 2 meters (-)
REAL, DIMENSION(:),   ALLOCATABLE :: ZZON10M! zonal Wind at 10 meters     (m/s)
REAL, DIMENSION(:),   ALLOCATABLE :: ZMER10M! meridian Wind at 10 meters  (m/s)
REAL, DIMENSION(:),   ALLOCATABLE :: ZNETLW ! Net surface Longwave  flux  (W/m2)
REAL, DIMENSION(:),   ALLOCATABLE :: ZNETSW ! Net surface Shortwave flux  (W/m2)
!
!-------------------------------------------------------------------------------
CALL GET_INDICE_ll (IIB,IJB,IIE,IJE)
ILU = (IIE-IIB+1+2*NHALO)*(IJE-IJB+1+2*NHALO)
ILM = (IIE-IIB+1        )*(IJE-IJB+1        )
!-------------------------------------------------------------------------------
IF (PRESENT(PCOVER)) THEN
  PCOVER(:,:,:) = XUNDEF
  ICOVER = SIZE(PCOVER,3)
  ALLOCATE(ZCOVER( ILU ))
  ! A verifier car GET_COVER a une option TOWN !!'
  DO JCOVER=1,SIZE(PCOVER,3)
    CALL GET_COVER_n('MESONH',ILU,ICOVER,ZCOVER)
    CALL REMOVE_HALO(ZCOVER,PCOVER(:,:,JCOVER))    
  END DO
  DEALLOCATE(ZCOVER)
END IF
!
IF (PRESENT(PSEA) .OR. PRESENT(PTOWN)) THEN
  ALLOCATE(ZSEA   ( ILU ))
  ALLOCATE(ZWATER ( ILU ))
  ALLOCATE(ZNATURE( ILU ))
  ALLOCATE(ZTOWN  ( ILU ))
  CALL GET_FRAC_n('MESONH',ILU,ZSEA,ZWATER,ZNATURE,ZTOWN)
  IF (PRESENT(PSEA)) THEN
    CALL REMOVE_HALO(ZSEA,PSEA)
  END IF
  IF (PRESENT(PTOWN)) THEN
    CALL REMOVE_HALO(ZTOWN,PTOWN)
  END IF
  DEALLOCATE(ZSEA   )
  DEALLOCATE(ZWATER )
  DEALLOCATE(ZNATURE)
  DEALLOCATE(ZTOWN  )
END IF
!
IF (PRESENT(KCOVER)) THEN
  CALL GET_JCOVER_n('MESONH',KCOVER)
END IF
!
IF (PRESENT(PRN) .OR.PRESENT(PH)  .OR.PRESENT(PLE)  .OR.PRESENT(PGFLUX).OR. &
    PRESENT(PT2M).OR.PRESENT(PQ2M).OR.PRESENT(PHU2M).OR.                    &
    PRESENT(PZON10M) .OR. PRESENT(PMER10M)                             ) THEN
    ALLOCATE(ZRN    (ILU), ZH     (ILU), ZLE  (ILU), ZLEI  (ILU), ZGFLUX(ILU))
    ALLOCATE(ZT2M   (ILU), ZQ2M   (ILU), ZHU2M(ILU))
    ALLOCATE(ZZON10M(ILU), ZMER10M(ILU))
    ALLOCATE(ZNETLW (ILU), ZNETSW (ILU))
    CALL GET_FLUX_n('MESONH', ILU,ZRN,ZH,ZLE,ZLEI,ZGFLUX,ZT2M,ZQ2M,ZHU2M,ZZON10M,ZMER10M,&
                    ZNETLW,ZNETSW)
    IF(PRESENT(PRN))     PRN=ZRN
    IF(PRESENT(PH))      PH=ZH
    IF(PRESENT(PLE))     PLE=ZLE
    IF(PRESENT(PLEI))    PLEI=ZLEI
    IF(PRESENT(PGFLUX))  PGFLUX=ZGFLUX
    IF(PRESENT(PT2M))    PT2M=ZT2M
    IF(PRESENT(PQ2M))    PQ2M=ZQ2M
    IF(PRESENT(PHU2M))   PHU2M=ZHU2M
    IF(PRESENT(PZON10M)) PZON10M=ZZON10M
    IF(PRESENT(PMER10M)) PMER10M=ZMER10M
    DEALLOCATE(ZRN, ZH, ZLE,ZLEI, ZGFLUX, ZT2M, ZQ2M, ZHU2M, ZZON10M, ZMER10M)
    DEALLOCATE(ZNETLW,ZNETSW)
END IF
!
IF (PRESENT(PZS)) THEN
  PZS(:,:) = XUNDEF
  ALLOCATE(ZZS  ( ILU ))
  CALL GET_ZS_n('MESONH',ILU,ZZS)
  CALL REMOVE_HALO(ZZS,PZS)
  DEALLOCATE(ZZS)
END IF
!
IF (PRESENT(PVH)  .OR.PRESENT(PLAI)) THEN
  PVH(:,:) = XUNDEF
  PLAI(:,:) = XUNDEF
  ALLOCATE(ZVH  ( ILU ))
  ALLOCATE(ZLAI  ( ILU ))
  CALL GET_VEG_n('MESONH',ILU,ZLAI,ZVH)
  CALL REMOVE_HALO(ZLAI,PLAI)
  CALL REMOVE_HALO(ZVH,PVH)
  DEALLOCATE(ZVH)
  DEALLOCATE(ZLAI)
END IF
!
!==============================================================================
!
CONTAINS
!
SUBROUTINE REMOVE_HALO(PFIELD,POUT)
!
REAL, DIMENSION(:),   INTENT(IN)  :: PFIELD
REAL, DIMENSION(:,:), INTENT(OUT) :: POUT
!
INTEGER :: JI, JJ
!
POUT=XUNDEF
!
DO JJ=IJB,IJE
  DO JI=IIB,IIE
    POUT(JI,JJ) = PFIELD(JI-1+NHALO+(JJ-1+NHALO-1)*(IIE-IIB+1+2*NHALO))
  END DO
END DO
!
END SUBROUTINE REMOVE_HALO
!
END SUBROUTINE MNHGET_SURF_PARAM_n
