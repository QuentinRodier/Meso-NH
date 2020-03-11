!MNH_LIC Copyright 2003-2020 CNRS, Meteo-France and Universite Paul Sabatier
!MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
!MNH_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!MNH_LIC for details. version 1.
!-----------------------------------------------------------------
!     #######################
      MODULE MODI_MNHGET_SURF_PARAM_n
!     #######################
INTERFACE
      SUBROUTINE MNHGET_SURF_PARAM_n(PCOVER,PSEA,KCOVER,PRN,PH,PLE,PLEI,PGFLUX, &
                                     PT2M,PQ2M,PHU2M,PZON10M,PMER10M,PZS,PTOWN,&
                                     PBARE, PLAI_TREE, PH_TREE, PWALL_O_HOR,    &
                                     PBUILD_HEIGHT,PNATURE )
!
REAL, DIMENSION(:,:,:), INTENT(OUT), OPTIONAL :: PCOVER  ! cover types
REAL, DIMENSION(:,:),   INTENT(OUT), OPTIONAL :: PSEA    ! sea fraction
REAL, DIMENSION(:,:),   INTENT(OUT), OPTIONAL :: PTOWN   ! town fraction
REAL, DIMENSION(:,:),   INTENT(OUT), OPTIONAL :: PNATURE ! nature fraction
INTEGER,                INTENT(OUT), OPTIONAL :: KCOVER  ! number of cover types
REAL, DIMENSION(:,:),   INTENT(OUT), OPTIONAL :: PBARE           ! Bare soil fraction
REAL, DIMENSION(:,:),   INTENT(OUT), OPTIONAL :: PLAI_TREE       ! Tree leaf area index [m^2(leaf)/m^2(nature)]
REAL, DIMENSION(:,:),   INTENT(OUT), OPTIONAL :: PH_TREE         ! Tree height [m]
REAL, DIMENSION(:,:),   INTENT(OUT), OPTIONAL :: PWALL_O_HOR     ! Facade area density [m^2(fac.)/m^2(town)]
REAL, DIMENSION(:,:),   INTENT(OUT), OPTIONAL :: PBUILD_HEIGHT   ! Building height [m] 
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
                                     PBARE, PLAI_TREE, PH_TREE, PWALL_O_HOR,    &
                                     PBUILD_HEIGHT,PNATURE )
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
!!      Modif
!!      J.Escobar 21/03/2013: for HALOK comment all NHALO=1 test 
!!                            & correction of index linearisation for NHALO<>1 
!!       S. Donier  06/2015 : bug surface aerosols
!!  06/2016     (G.Delautier) phasage surfex 8
!!  01/2018      (G.Delautier) SURFEX 8.1
! C. Lac         11/2019: correction in the drag formula and application to building in addition to tree
! P. Wautelet 11/03/2020: bugfix: add present checks before working on optional arrays
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
USE MODD_MNH_SURFEX_n
USE MODI_GET_SURF_VAR_n
!
IMPLICIT NONE
!
!*       0.1   Declarations of arguments
!              -------------------------
!
REAL, DIMENSION(:,:,:), INTENT(OUT), OPTIONAL :: PCOVER  ! cover types
REAL, DIMENSION(:,:),   INTENT(OUT), OPTIONAL :: PSEA    ! sea fraction
REAL, DIMENSION(:,:),   INTENT(OUT), OPTIONAL :: PTOWN   ! town fraction
REAL, DIMENSION(:,:),   INTENT(OUT), OPTIONAL :: PNATURE ! nature fraction
INTEGER,                INTENT(OUT), OPTIONAL :: KCOVER  ! number of cover types
REAL, DIMENSION(:,:),   INTENT(OUT), OPTIONAL :: PBARE           ! Bare soil fraction
REAL, DIMENSION(:,:),   INTENT(OUT), OPTIONAL :: PLAI_TREE       ! 
REAL, DIMENSION(:,:),   INTENT(OUT), OPTIONAL :: PH_TREE         !
REAL, DIMENSION(:,:),   INTENT(OUT), OPTIONAL :: PWALL_O_HOR     ! Facade area density [m^2(fac.)/m^2(town)]
REAL, DIMENSION(:,:),   INTENT(OUT), OPTIONAL :: PBUILD_HEIGHT   ! Building height [m] 
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
REAL, DIMENSION(:),   ALLOCATABLE :: ZWALL_O_HOR   ! Facade surface density [m^2(fac.)/m^2(town)]
REAL, DIMENSION(:),   ALLOCATABLE :: ZBUILD_HEIGHT ! Building height [m]
REAL, DIMENSION(:),   ALLOCATABLE :: ZBARE  ! bare soil fraction
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
REAL, DIMENSION(:),   ALLOCATABLE :: ZCD, ZEVAP, ZSUBL
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
    CALL GET_COVER_n(YSURF_CUR%U,'MESONH',ICOVER,ZCOVER)
    CALL REMOVE_HALO(ZCOVER,PCOVER(:,:,JCOVER))    
  END DO
  DEALLOCATE(ZCOVER)
END IF
!
IF (PRESENT(PSEA) .OR. PRESENT(PTOWN) .OR. PRESENT(PNATURE) .OR. &
    PRESENT(PBARE) .OR. PRESENT(PLAI_TREE) .OR. PRESENT(PH_TREE) .OR. &
    PRESENT(PWALL_O_HOR) .OR. PRESENT(PBUILD_HEIGHT) ) THEN
  ALLOCATE(ZSEA   ( ILU ))
  ALLOCATE(ZWATER ( ILU ))
  ALLOCATE(ZNATURE( ILU ))
  ALLOCATE(ZTOWN  ( ILU ))
  CALL GET_FRAC_n(YSURF_CUR%U,'MESONH',ILU,ZSEA,ZWATER,ZNATURE,ZTOWN)
  IF (PRESENT(PSEA)) THEN
    CALL REMOVE_HALO(ZSEA,PSEA)
  END IF
  IF (PRESENT(PTOWN)) THEN
    CALL REMOVE_HALO(ZTOWN,PTOWN)
  END IF
  IF (PRESENT(PNATURE)) THEN
    CALL REMOVE_HALO(ZNATURE,PNATURE)
  END IF
END IF
!
IF (PRESENT(PBARE)) THEN
  ALLOCATE(ZBARE  ( ILU ))
  CALL GET_SURF_VAR_n(YSURF_CUR%FM,YSURF_CUR%IM,YSURF_CUR%SM,YSURF_CUR%TM, &
                      YSURF_CUR%WM,YSURF_CUR%DUO,YSURF_CUR%DU,YSURF_CUR%UG,&
                      YSURF_CUR%U,YSURF_CUR%USS,&
                      'MESONH', ILU, 1, PNATURE=ZNATURE, PBARE=ZBARE)
  CALL REMOVE_HALO(ZBARE,PBARE)
  DEALLOCATE(ZBARE)
END IF
!
IF (PRESENT(KCOVER)) THEN
  CALL GET_JCOVER_n(YSURF_CUR%U,'MESONH',KCOVER)
END IF
!
IF (PRESENT(PRN) .OR.PRESENT(PH)  .OR.PRESENT(PLE)  .OR.PRESENT(PGFLUX).OR. &
    PRESENT(PT2M).OR.PRESENT(PQ2M).OR.PRESENT(PHU2M).OR.                    &
    PRESENT(PZON10M) .OR. PRESENT(PMER10M)                             ) THEN
    ALLOCATE(ZRN    (ILU), ZH     (ILU), ZLE  (ILU), ZLEI  (ILU), ZGFLUX(ILU))
    ALLOCATE(ZT2M   (ILU), ZQ2M   (ILU), ZHU2M(ILU))
    ALLOCATE(ZZON10M(ILU), ZMER10M(ILU))
    ALLOCATE(ZNETLW (ILU), ZNETSW (ILU))
    ALLOCATE(ZCD(ILU), ZEVAP(ILU), ZSUBL(ILU))
    CALL GET_FLUX_n(YSURF_CUR%DUO, YSURF_CUR%DU,'MESONH', &
                    ILU,ZRN,ZH,ZLE,ZLEI,ZGFLUX,ZT2M,ZQ2M,ZHU2M,ZZON10M,ZMER10M,&
                    ZNETLW,ZNETSW,ZCD,ZEVAP,ZSUBL)
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
    DEALLOCATE(ZCD,ZEVAP,ZSUBL)       
END IF
!
IF (PRESENT(PZS)) THEN
  PZS(:,:) = XUNDEF
  ALLOCATE(ZZS  ( ILU ))
  CALL GET_ZS_n(YSURF_CUR%U,'MESONH',ILU,ZZS)
  CALL REMOVE_HALO(ZZS,PZS)
  DEALLOCATE(ZZS)
END IF
!
IF (PRESENT(PH_TREE)  .OR.PRESENT(PLAI_TREE)) THEN
  PH_TREE(:,:) = XUNDEF
  PLAI_TREE(:,:) = XUNDEF
  ALLOCATE(ZVH  ( ILU ))
  ALLOCATE(ZLAI  ( ILU ))
  CALL GET_SURF_VAR_n(YSURF_CUR%FM,YSURF_CUR%IM,YSURF_CUR%SM,YSURF_CUR%TM, &
                      YSURF_CUR%WM,YSURF_CUR%DUO,YSURF_CUR%DU,YSURF_CUR%UG,&
                      YSURF_CUR%U,YSURF_CUR%USS,&
                      'MESONH',ILU,1,PNATURE=ZNATURE,PLAI_TREE=ZLAI,PH_TREE=ZVH)
  IF ( PRESENT( PLAI_TREE ) )  CALL REMOVE_HALO(ZLAI,PLAI_TREE)
  IF ( PRESENT( PH_TREE )   ) CALL REMOVE_HALO(ZVH,PH_TREE)
  DEALLOCATE(ZVH)
  DEALLOCATE(ZLAI)
END IF
!
IF (PRESENT(PWALL_O_HOR) .OR. PRESENT(PBUILD_HEIGHT)) THEN
  IF ( PRESENT ( PBUILD_HEIGHT ) ) PBUILD_HEIGHT(:,:) = XUNDEF
  IF ( PRESENT ( PWALL_O_HOR )   ) PWALL_O_HOR(:,:) = XUNDEF
  ALLOCATE(ZBUILD_HEIGHT ( ILU ))
  ALLOCATE(ZWALL_O_HOR   ( ILU ))
  CALL GET_SURF_VAR_n(YSURF_CUR%FM,YSURF_CUR%IM,YSURF_CUR%SM,YSURF_CUR%TM, &
                      YSURF_CUR%WM,YSURF_CUR%DUO,YSURF_CUR%DU,YSURF_CUR%UG,&
                      YSURF_CUR%U,YSURF_CUR%USS,&
                       'MESONH',ILU,1,PTOWN=ZTOWN,                       &
                       PWALL_O_HOR=ZWALL_O_HOR,PBUILD_HEIGHT=ZBUILD_HEIGHT )
  IF ( PRESENT ( PBUILD_HEIGHT ) ) CALL REMOVE_HALO(ZBUILD_HEIGHT,PBUILD_HEIGHT)
  IF ( PRESENT ( PWALL_O_HOR )   ) CALL REMOVE_HALO(ZWALL_O_HOR,PWALL_O_HOR)
  DEALLOCATE(ZBUILD_HEIGHT)
  DEALLOCATE(ZWALL_O_HOR)
END IF
!
IF (ALLOCATED(ZSEA)) THEN
  DEALLOCATE(ZSEA   )
  DEALLOCATE(ZWATER )
  DEALLOCATE(ZNATURE)
  DEALLOCATE(ZTOWN  )
END IF
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
    POUT(JI,JJ) = PFIELD( JI-IIB+1 + NHALO + (JJ-IJB+NHALO)*(IIE-IIB+1+2*NHALO))
  END DO
END DO
!
END SUBROUTINE REMOVE_HALO
!
END SUBROUTINE MNHGET_SURF_PARAM_n
