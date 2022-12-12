!MNH_LIC Copyright 2000-2022 CNRS, Meteo-France and Universite Paul Sabatier
!MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
!MNH_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!MNH_LIC for details. version 1.
!-----------------------------------------------------------------
!      #####################
MODULE MODI_AIRCRAFT_BALLOON
!      #####################
!
INTERFACE
!
      SUBROUTINE AIRCRAFT_BALLOON(PTSTEP, PZ,                         &
                                  PMAP, PLONOR, PLATOR,               &
                                  PU, PV, PW, PP, PTH, PR, PSV, PTKE, &
                                  PTS, PRHODREF, PCIT, PSEA           )
!
REAL,                     INTENT(IN)     :: PTSTEP ! time step
REAL, DIMENSION(:,:,:),   INTENT(IN)     :: PZ     ! z
REAL, DIMENSION(:,:),     INTENT(IN)     :: PMAP   ! map factor
REAL,                     INTENT(IN)     :: PLONOR ! origine longitude
REAL,                     INTENT(IN)     :: PLATOR ! origine latitude
REAL, DIMENSION(:,:,:),   INTENT(IN)     :: PU     ! horizontal wind X component
REAL, DIMENSION(:,:,:),   INTENT(IN)     :: PV     ! horizontal wind Y component
REAL, DIMENSION(:,:,:),   INTENT(IN)     :: PW     ! vertical wind
REAL, DIMENSION(:,:,:),   INTENT(IN)     :: PP     ! pressure
REAL, DIMENSION(:,:,:),   INTENT(IN)     :: PTH    ! potential temperature
REAL, DIMENSION(:,:,:,:), INTENT(IN)     :: PR     ! water mixing ratios
REAL, DIMENSION(:,:,:,:), INTENT(IN)     :: PSV    ! Scalar variables
REAL, DIMENSION(:,:,:),   INTENT(IN)     :: PTKE   ! turbulent kinetic energy
REAL, DIMENSION(:,:),     INTENT(IN)     :: PTS    ! surface temperature
REAL, DIMENSION(:,:,:),   INTENT(IN)     :: PRHODREF ! dry air density of the reference state
REAL, DIMENSION(:,:,:),   INTENT(IN)     :: PCIT     ! pristine ice concentration
REAL, DIMENSION(:,:), OPTIONAL, INTENT(IN) :: PSEA
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE AIRCRAFT_BALLOON
!
SUBROUTINE AIRCRAFT_BALLOON_LONGTYPE_GET( TPFLYER, HLONGTYPE )
  USE MODD_AIRCRAFT_BALLOON, ONLY: TFLYERDATA

  CLASS(TFLYERDATA), INTENT(IN)  :: TPFLYER
  CHARACTER(LEN=*),  INTENT(OUT) :: HLONGTYPE
END SUBROUTINE AIRCRAFT_BALLOON_LONGTYPE_GET

END INTERFACE
!
END MODULE MODI_AIRCRAFT_BALLOON
!
!     #################################################################
      SUBROUTINE AIRCRAFT_BALLOON(PTSTEP, PZ,                         &
                                  PMAP, PLONOR, PLATOR,               &
                                  PU, PV, PW, PP, PTH, PR, PSV, PTKE, &
                                  PTS, PRHODREF, PCIT, PSEA           )
!     #################################################################
!
!
!!****  *AIRCRAFT_BALLOON* - monitor for balloons and aircrafts
!!
!!    PURPOSE
!!    -------
!
!
!!**  METHOD
!!    ------
!!    
!!  
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
!!      Valery Masson             * Meteo-France *
!!
!!    MODIFICATIONS
!!    -------------
!!     Original 15/05/2000
!!
!!              March, 2008 (P.Lacarrere) Add 3D fluxes
!  P. Wautelet 05/2016-04/2018: new data structures and calls for I/O
!  P. Wautelet 13/09/2019: budget: simplify and modernize date/time management
!  P. Wautelet    06/2022: reorganize flyers
! --------------------------------------------------------------------------
!       
!*      0. DECLARATIONS
!          ------------
!
USE MODD_AIRCRAFT_BALLOON
!
USE MODD_TURB_FLUX_AIRCRAFT_BALLOON
!
USE MODE_AIRCRAFT_BALLOON_EVOL,      ONLY: AIRCRAFT_BALLOON_EVOL
USE MODE_ll
!
!
IMPLICIT NONE
!
!
!*      0.1  declarations of arguments
!
!
REAL,                     INTENT(IN)     :: PTSTEP ! time step
REAL, DIMENSION(:,:,:),   INTENT(IN)     :: PZ     ! z array
REAL, DIMENSION(:,:),     INTENT(IN)     :: PMAP   ! map factor
REAL,                     INTENT(IN)     :: PLONOR ! origine longitude
REAL,                     INTENT(IN)     :: PLATOR ! origine latitude
REAL, DIMENSION(:,:,:),   INTENT(IN)     :: PU     ! horizontal wind X component
REAL, DIMENSION(:,:,:),   INTENT(IN)     :: PV     ! horizontal wind Y component
REAL, DIMENSION(:,:,:),   INTENT(IN)     :: PW     ! vertical wind
REAL, DIMENSION(:,:,:),   INTENT(IN)     :: PP     ! pressure
REAL, DIMENSION(:,:,:),   INTENT(IN)     :: PTH    ! potential temperature
REAL, DIMENSION(:,:,:,:), INTENT(IN)     :: PR     ! water mixing ratios
REAL, DIMENSION(:,:,:,:), INTENT(IN)     :: PSV    ! Scalar variables
REAL, DIMENSION(:,:,:),   INTENT(IN)     :: PTKE   ! turbulent kinetic energy
REAL, DIMENSION(:,:),     INTENT(IN)     :: PTS    ! surface temperature
REAL, DIMENSION(:,:,:),   INTENT(IN)     :: PRHODREF ! dry air density of the reference state
REAL, DIMENSION(:,:,:),   INTENT(IN)     :: PCIT     ! pristine ice concentration
REAL, DIMENSION(:,:), OPTIONAL, INTENT(IN) :: PSEA
!
!-------------------------------------------------------------------------------
!
!       0.2  declaration of local variables
!
INTEGER :: JI
!----------------------------------------------------------------------------
IF(.NOT. ALLOCATED(XTHW_FLUX)) &
ALLOCATE(XTHW_FLUX(SIZE(PTH,1),SIZE(PTH,2),SIZE(PTH,3)))
IF(.NOT. ALLOCATED(XRCW_FLUX)) &
ALLOCATE(XRCW_FLUX(SIZE(PTH,1),SIZE(PTH,2),SIZE(PTH,3)))
IF(.NOT. ALLOCATED(XSVW_FLUX)) &
ALLOCATE(XSVW_FLUX(SIZE(PSV,1),SIZE(PSV,2),SIZE(PSV,3),SIZE(PSV,4)))
!
DO JI = 1, NBALLOONS
  CALL AIRCRAFT_BALLOON_EVOL( PTSTEP, PZ, PMAP, PLONOR, PLATOR,                        &
                              PU, PV, PW, PP, PTH, PR, PSV, PTKE, PTS, PRHODREF, PCIT, &
                              TBALLOONS(JI)%TBALLOON, PSEA                             )
END DO
!
DO JI = 1, NAIRCRAFTS
  CALL AIRCRAFT_BALLOON_EVOL( PTSTEP, PZ, PMAP, PLONOR, PLATOR,                        &
                              PU, PV, PW, PP, PTH, PR, PSV, PTKE, PTS, PRHODREF, PCIT, &
                              TAIRCRAFTS(JI)%TAIRCRAFT, PSEA                           )
END DO
!
!----------------------------------------------------------------------------
!
END SUBROUTINE AIRCRAFT_BALLOON


SUBROUTINE AIRCRAFT_BALLOON_LONGTYPE_GET( TPFLYER, HLONGTYPE )
USE MODD_AIRCRAFT_BALLOON, ONLY: taircraftdata, tballoondata, TFLYERDATA

USE MODE_MSG

CLASS(TFLYERDATA), INTENT(IN)  :: TPFLYER
CHARACTER(LEN=*),  INTENT(OUT) :: HLONGTYPE

character(len=:), allocatable :: ytype

select type ( tpflyer )
  class is ( taircraftdata )
    ytype = 'Aircrafts'

  class is ( tballoondata )
    if ( Trim( TPFLYER%CTYPE ) == 'RADIOS' ) then
      ytype = 'Radiosonde_balloons'
    else if ( Trim( TPFLYER%CTYPE ) == 'ISODEN' ) then
      ytype = 'Isodensity_balloons'
    else if ( Trim( TPFLYER%CTYPE ) == 'CVBALL' ) then
      ytype = 'Constant_volume_balloons'
    else
      call Print_msg( NVERB_ERROR, 'GEN', 'AIRCRAFT_BALLOON_LONGTYPE_GET', 'unknown category for flyer ' // Trim( tpflyer%ctitle ) )
      ytype = 'Unknown'
    end if

  class default
    call Print_msg( NVERB_ERROR, 'GEN', 'AIRCRAFT_BALLOON_LONGTYPE_GET', 'unknown class for flyer ' // Trim( tpflyer%ctitle ) )
    ytype = 'Unknown'

end select

if ( Len_trim( ytype ) > Len( HLONGTYPE ) ) &
  call Print_msg( NVERB_WARNING, 'GEN', 'AIRCRAFT_BALLOON_LONGTYPE_GET', &
                  'HLONGTYPE truncated for flyer ' // Trim( tpflyer%ctitle ) )
HLONGTYPE = Trim( ytype )

END SUBROUTINE AIRCRAFT_BALLOON_LONGTYPE_GET
