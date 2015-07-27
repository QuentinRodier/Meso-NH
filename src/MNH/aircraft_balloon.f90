!MNH_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
!MNH_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!MNH_LIC for details. version 1.
!-----------------------------------------------------------------
!--------------- special set of characters for RCS information
!-----------------------------------------------------------------
! $Source$ $Revision$
! MASDEV4_7 balloon 2006/05/18 13:07:25
!-----------------------------------------------------------------
!      #####################
MODULE MODI_AIRCRAFT_BALLOON
!      #####################
!
INTERFACE
!
      SUBROUTINE AIRCRAFT_BALLOON(HLUOUT, PTSTEP,                       &
                                  TPDTEXP, TPDTMOD, TPDTSEG, TPDTCUR,   &
                                  PXHAT, PYHAT, PZ,                     &
                                  PMAP, PLONOR, PLATOR,                 &
                                  PU, PV, PW, PP, PTH, PR, PSV, PTKE,   &
                                  PTS, PRHODREF, PCIT, PSPEEDC, PSPEEDR,&
                                  PSPEEDS, PSPEEDG, PSPEEDH, PSEA)
!
USE MODD_TYPE_DATE
CHARACTER(LEN=*),         INTENT(IN)     :: HLUOUT ! output listing
REAL,                     INTENT(IN)     :: PTSTEP ! time step
TYPE(DATE_TIME),          INTENT(IN)     :: TPDTEXP! experiment date and time
TYPE(DATE_TIME),          INTENT(IN)     :: TPDTMOD! model start date and time
TYPE(DATE_TIME),          INTENT(IN)     :: TPDTSEG! segment date and time
TYPE(DATE_TIME),          INTENT(IN)     :: TPDTCUR! current date and time
REAL, DIMENSION(:),       INTENT(IN)     :: PXHAT  ! x coordinate
REAL, DIMENSION(:),       INTENT(IN)     :: PYHAT  ! y coordinate
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
! ++ OC
REAL, DIMENSION(:,:,:),   INTENT(IN)     :: PRHODREF ! dry air density of the reference state
REAL, DIMENSION(:,:,:),   INTENT(IN)     :: PCIT     ! pristine ice concentration
REAL, DIMENSION(:,:,:),   INTENT(IN)     :: PSPEEDC ! Cloud sedimentation speed
REAL, DIMENSION(:,:,:),   INTENT(IN)     :: PSPEEDR ! Rain sedimentation speed
REAL, DIMENSION(:,:,:),   INTENT(IN)     :: PSPEEDS ! Snow sedimentation speed
REAL, DIMENSION(:,:,:),   INTENT(IN)     :: PSPEEDG ! Graupel sedimentation speed
REAL, DIMENSION(:,:,:),   INTENT(IN)     :: PSPEEDH ! Hail sedimentation speed
REAL, DIMENSION(:,:),INTENT(IN) :: PSEA
! -- OC
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE AIRCRAFT_BALLOON
!
END INTERFACE
!
END MODULE MODI_AIRCRAFT_BALLOON
!
!     ###################################################################
      SUBROUTINE AIRCRAFT_BALLOON(HLUOUT, PTSTEP,                       &
                                  TPDTEXP, TPDTMOD, TPDTSEG, TPDTCUR,   &
                                  PXHAT, PYHAT, PZ,                     &
                                  PMAP, PLONOR, PLATOR,                 &
                                  PU, PV, PW, PP, PTH, PR, PSV, PTKE,   &
                                  PTS, PRHODREF, PCIT, PSPEEDC, PSPEEDR,&
                                  PSPEEDS, PSPEEDG, PSPEEDH, PSEA)
!     ###################################################################
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
!!              July, 2015 (O.Nuissier/F.Duffourg) Add microphysics diagnostic for
!!                                      aircraft, ballon and profiler
!! --------------------------------------------------------------------------
!       
!*      0. DECLARATIONS
!          ------------
!
USE MODD_AIRCRAFT_BALLOON
!
USE MODD_TURB_FLUX_AIRCRAFT_BALLOON
USE MODI_AIRCRAFT_BALLOON_EVOL
!
USE MODE_ll
!
!
IMPLICIT NONE
!
!
!*      0.1  declarations of arguments
!
!
CHARACTER(LEN=*),         INTENT(IN)     :: HLUOUT ! output listing
REAL,                     INTENT(IN)     :: PTSTEP ! time step
TYPE(DATE_TIME),          INTENT(IN)     :: TPDTEXP! experiment date and time
TYPE(DATE_TIME),          INTENT(IN)     :: TPDTMOD! model start date and time
TYPE(DATE_TIME),          INTENT(IN)     :: TPDTSEG! segment date and time
TYPE(DATE_TIME),          INTENT(IN)     :: TPDTCUR! current date and time
REAL, DIMENSION(:),       INTENT(IN)     :: PXHAT  ! x coordinate
REAL, DIMENSION(:),       INTENT(IN)     :: PYHAT  ! y coordinate
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
REAL, DIMENSION(:,:,:),   INTENT(IN)     :: PSPEEDC ! Cloud sedimentation speed
REAL, DIMENSION(:,:,:),   INTENT(IN)     :: PSPEEDR ! Rain sedimentation speed
REAL, DIMENSION(:,:,:),   INTENT(IN)     :: PSPEEDS ! Snow sedimentation speed
REAL, DIMENSION(:,:,:),   INTENT(IN)     :: PSPEEDG ! Graupel sedimentation speed
REAL, DIMENSION(:,:,:),   INTENT(IN)     :: PSPEEDH ! Hail sedimentation speed
REAL, DIMENSION(:,:),INTENT(IN) :: PSEA
!
!-------------------------------------------------------------------------------
!
!       0.2  declaration of local variables
!
!
!----------------------------------------------------------------------------
IF(.NOT. ALLOCATED(XTHW_FLUX)) &
ALLOCATE(XTHW_FLUX(SIZE(PTH,1),SIZE(PTH,2),SIZE(PTH,3)))
IF(.NOT. ALLOCATED(XRCW_FLUX)) &
ALLOCATE(XRCW_FLUX(SIZE(PTH,1),SIZE(PTH,2),SIZE(PTH,3)))
IF(.NOT. ALLOCATED(XSVW_FLUX)) &
ALLOCATE(XSVW_FLUX(SIZE(PSV,1),SIZE(PSV,2),SIZE(PSV,3),SIZE(PSV,4)))
!
IF (TBALLOON1%NMODEL /= 0) THEN
CALL AIRCRAFT_BALLOON_EVOL(HLUOUT, PTSTEP, TPDTEXP, TPDTMOD, TPDTSEG, TPDTCUR,        &
                           PXHAT, PYHAT, PZ, PMAP, PLONOR, PLATOR,                    &
                           PU, PV, PW, PP, PTH, PR, PSV, PTKE, PTS, PRHODREF, PCIT,   &
                           PSPEEDC,PSPEEDR,PSPEEDS,PSPEEDG,PSPEEDH,                    &
                           TBALLOON1, PSEA                                            )
ENDIF
IF (TBALLOON2%NMODEL /= 0) THEN
CALL AIRCRAFT_BALLOON_EVOL(HLUOUT, PTSTEP, TPDTEXP, TPDTMOD, TPDTSEG, TPDTCUR,        &
                           PXHAT, PYHAT, PZ, PMAP, PLONOR, PLATOR,                    &
                           PU, PV, PW, PP, PTH, PR, PSV, PTKE, PTS, PRHODREF, PCIT,   &
                           PSPEEDC,PSPEEDR,PSPEEDS,PSPEEDG,PSPEEDH,           &
                           TBALLOON2, PSEA                                            )
ENDIF
IF (TBALLOON3%NMODEL /= 0) THEN
CALL AIRCRAFT_BALLOON_EVOL(HLUOUT, PTSTEP, TPDTEXP, TPDTMOD, TPDTSEG, TPDTCUR,        &
                           PXHAT, PYHAT, PZ, PMAP, PLONOR, PLATOR,                    &
                           PU, PV, PW, PP, PTH, PR, PSV, PTKE, PTS, PRHODREF, PCIT,   &
                           PSPEEDC,PSPEEDR,PSPEEDS,PSPEEDG,PSPEEDH,           &
                           TBALLOON3, PSEA                                            )
ENDIF
IF (TBALLOON4%NMODEL /= 0) THEN
CALL AIRCRAFT_BALLOON_EVOL(HLUOUT, PTSTEP, TPDTEXP, TPDTMOD, TPDTSEG, TPDTCUR,        &
                           PXHAT, PYHAT, PZ, PMAP, PLONOR, PLATOR,                    &
                           PU, PV, PW, PP, PTH, PR, PSV, PTKE, PTS, PRHODREF, PCIT,   &
                           PSPEEDC,PSPEEDR,PSPEEDS,PSPEEDG,PSPEEDH,           &
                           TBALLOON4, PSEA                                            )
ENDIF
IF (TBALLOON5%NMODEL /= 0) THEN
CALL AIRCRAFT_BALLOON_EVOL(HLUOUT, PTSTEP, TPDTEXP, TPDTMOD, TPDTSEG, TPDTCUR,        &
                           PXHAT, PYHAT, PZ, PMAP, PLONOR, PLATOR,                    &
                           PU, PV, PW, PP, PTH, PR, PSV, PTKE, PTS, PRHODREF, PCIT,   &
                           PSPEEDC,PSPEEDR,PSPEEDS,PSPEEDG,PSPEEDH,           &
                           TBALLOON5, PSEA                                            )
ENDIF
IF (TBALLOON6%NMODEL /= 0) THEN
CALL AIRCRAFT_BALLOON_EVOL(HLUOUT, PTSTEP, TPDTEXP, TPDTMOD, TPDTSEG, TPDTCUR,        &
                           PXHAT, PYHAT, PZ, PMAP, PLONOR, PLATOR,                    &
                           PU, PV, PW, PP, PTH, PR, PSV, PTKE, PTS, PRHODREF, PCIT,   &
                           PSPEEDC,PSPEEDR,PSPEEDS,PSPEEDG,PSPEEDH,           &
                           TBALLOON6, PSEA                                            )
ENDIF
IF (TBALLOON7%NMODEL /= 0) THEN
CALL AIRCRAFT_BALLOON_EVOL(HLUOUT, PTSTEP, TPDTEXP, TPDTMOD, TPDTSEG, TPDTCUR,        &
                           PXHAT, PYHAT, PZ, PMAP, PLONOR, PLATOR,                    &
                           PU, PV, PW, PP, PTH, PR, PSV, PTKE, PTS, PRHODREF, PCIT,   &
                           PSPEEDC,PSPEEDR,PSPEEDS,PSPEEDG,PSPEEDH,           &
                           TBALLOON7, PSEA                                            )
ENDIF
IF (TBALLOON8%NMODEL /= 0) THEN
CALL AIRCRAFT_BALLOON_EVOL(HLUOUT, PTSTEP, TPDTEXP, TPDTMOD, TPDTSEG, TPDTCUR,        &
                           PXHAT, PYHAT, PZ, PMAP, PLONOR, PLATOR,                    &
                           PU, PV, PW, PP, PTH, PR, PSV, PTKE, PTS, PRHODREF, PCIT,   &
                           PSPEEDC,PSPEEDR,PSPEEDS,PSPEEDG,PSPEEDH,           &
                           TBALLOON8, PSEA                                            )
ENDIF
IF (TBALLOON9%NMODEL /= 0) THEN
CALL AIRCRAFT_BALLOON_EVOL(HLUOUT, PTSTEP, TPDTEXP, TPDTMOD, TPDTSEG, TPDTCUR,        &
                           PXHAT, PYHAT, PZ, PMAP, PLONOR, PLATOR,                    &
                           PU, PV, PW, PP, PTH, PR, PSV, PTKE, PTS, PRHODREF, PCIT,   &
                           PSPEEDC,PSPEEDR,PSPEEDS,PSPEEDG,PSPEEDH,           &
                           TBALLOON9, PSEA                                            )
ENDIF
!
IF (TAIRCRAFT1%NMODEL /= 0) THEN
CALL AIRCRAFT_BALLOON_EVOL(HLUOUT, PTSTEP, TPDTEXP, TPDTMOD, TPDTSEG, TPDTCUR,        &
                           PXHAT, PYHAT, PZ, PMAP, PLONOR, PLATOR,                    &
                           PU, PV, PW, PP, PTH, PR, PSV, PTKE, PTS, PRHODREF, PCIT,   &
                           PSPEEDC,PSPEEDR,PSPEEDS,PSPEEDG,PSPEEDH,           &
                           TAIRCRAFT1, PSEA                                           )
ENDIF
IF (TAIRCRAFT2%NMODEL /= 0) THEN
CALL AIRCRAFT_BALLOON_EVOL(HLUOUT, PTSTEP, TPDTEXP, TPDTMOD, TPDTSEG, TPDTCUR,        &
                           PXHAT, PYHAT, PZ, PMAP, PLONOR, PLATOR,                    &
                           PU, PV, PW, PP, PTH, PR, PSV, PTKE, PTS, PRHODREF, PCIT,   &
                           PSPEEDC,PSPEEDR,PSPEEDS,PSPEEDG,PSPEEDH,           &
                           TAIRCRAFT2, PSEA                                           )
ENDIF
IF (TAIRCRAFT3%NMODEL /= 0) THEN
CALL AIRCRAFT_BALLOON_EVOL(HLUOUT, PTSTEP, TPDTEXP, TPDTMOD, TPDTSEG, TPDTCUR,        &
                           PXHAT, PYHAT, PZ, PMAP, PLONOR, PLATOR,                    &
                           PU, PV, PW, PP, PTH, PR, PSV, PTKE, PTS, PRHODREF, PCIT,   &
                           PSPEEDC,PSPEEDR,PSPEEDS,PSPEEDG,PSPEEDH,           &
                           TAIRCRAFT3, PSEA                                           )
ENDIF
IF (TAIRCRAFT4%NMODEL /= 0) THEN
CALL AIRCRAFT_BALLOON_EVOL(HLUOUT, PTSTEP, TPDTEXP, TPDTMOD, TPDTSEG, TPDTCUR,        &
                           PXHAT, PYHAT, PZ, PMAP, PLONOR, PLATOR,                    &
                           PU, PV, PW, PP, PTH, PR, PSV, PTKE, PTS, PRHODREF, PCIT,   &
                           PSPEEDC,PSPEEDR,PSPEEDS,PSPEEDG,PSPEEDH,           &
                           TAIRCRAFT4, PSEA                                           )
ENDIF
IF (TAIRCRAFT5%NMODEL /= 0) THEN
CALL AIRCRAFT_BALLOON_EVOL(HLUOUT, PTSTEP, TPDTEXP, TPDTMOD, TPDTSEG, TPDTCUR,        &
                           PXHAT, PYHAT, PZ, PMAP, PLONOR, PLATOR,                    &
                           PU, PV, PW, PP, PTH, PR, PSV, PTKE, PTS, PRHODREF, PCIT,   &
                           PSPEEDC,PSPEEDR,PSPEEDS,PSPEEDG,PSPEEDH,           &
                           TAIRCRAFT5, PSEA                                           )
ENDIF
IF (TAIRCRAFT6%NMODEL /= 0) THEN
CALL AIRCRAFT_BALLOON_EVOL(HLUOUT, PTSTEP, TPDTEXP, TPDTMOD, TPDTSEG, TPDTCUR,        &
                           PXHAT, PYHAT, PZ, PMAP, PLONOR, PLATOR,                    &
                           PU, PV, PW, PP, PTH, PR, PSV, PTKE, PTS, PRHODREF, PCIT,   &
                           PSPEEDC,PSPEEDR,PSPEEDS,PSPEEDG,PSPEEDH,           &
                           TAIRCRAFT6, PSEA                                           )
ENDIF
IF (TAIRCRAFT7%NMODEL /= 0) THEN
CALL AIRCRAFT_BALLOON_EVOL(HLUOUT, PTSTEP, TPDTEXP, TPDTMOD, TPDTSEG, TPDTCUR,        &
                           PXHAT, PYHAT, PZ, PMAP, PLONOR, PLATOR,                    &
                           PU, PV, PW, PP, PTH, PR, PSV, PTKE, PTS, PRHODREF, PCIT,   &
                           PSPEEDC,PSPEEDR,PSPEEDS,PSPEEDG,PSPEEDH,           &
                           TAIRCRAFT7, PSEA                                           )
ENDIF
IF (TAIRCRAFT8%NMODEL /= 0) THEN
CALL AIRCRAFT_BALLOON_EVOL(HLUOUT, PTSTEP, TPDTEXP, TPDTMOD, TPDTSEG, TPDTCUR,        &
                           PXHAT, PYHAT, PZ, PMAP, PLONOR, PLATOR,                    &
                           PU, PV, PW, PP, PTH, PR, PSV, PTKE, PTS, PRHODREF, PCIT,   &
                           PSPEEDC,PSPEEDR,PSPEEDS,PSPEEDG,PSPEEDH,           &
                           TAIRCRAFT8, PSEA                                           )
ENDIF
IF (TAIRCRAFT9%NMODEL /= 0) THEN
CALL AIRCRAFT_BALLOON_EVOL(HLUOUT, PTSTEP, TPDTEXP, TPDTMOD, TPDTSEG, TPDTCUR,        &
                           PXHAT, PYHAT, PZ, PMAP, PLONOR, PLATOR,                    &
                           PU, PV, PW, PP, PTH, PR, PSV, PTKE, PTS, PRHODREF, PCIT,   &
                           PSPEEDC,PSPEEDR,PSPEEDS,PSPEEDG,PSPEEDH,           &
                           TAIRCRAFT9, PSEA                                           )
ENDIF
IF (TAIRCRAFT10%NMODEL /= 0) THEN
CALL AIRCRAFT_BALLOON_EVOL(HLUOUT, PTSTEP, TPDTEXP, TPDTMOD, TPDTSEG, TPDTCUR,        &
                           PXHAT, PYHAT, PZ, PMAP, PLONOR, PLATOR,                    &
                           PU, PV, PW, PP, PTH, PR, PSV, PTKE, PTS, PRHODREF, PCIT,   &
                           PSPEEDC,PSPEEDR,PSPEEDS,PSPEEDG,PSPEEDH,           &
                           TAIRCRAFT10, PSEA                                          )
ENDIF
IF (TAIRCRAFT11%NMODEL /= 0) THEN
CALL AIRCRAFT_BALLOON_EVOL(HLUOUT, PTSTEP, TPDTEXP, TPDTMOD, TPDTSEG, TPDTCUR,        &
                           PXHAT, PYHAT, PZ, PMAP, PLONOR, PLATOR,                    &
                           PU, PV, PW, PP, PTH, PR, PSV, PTKE, PTS, PRHODREF, PCIT,   &
                           PSPEEDC,PSPEEDR,PSPEEDS,PSPEEDG,PSPEEDH,           &
                           TAIRCRAFT11, PSEA                                          )
ENDIF
IF (TAIRCRAFT12%NMODEL /= 0) THEN
CALL AIRCRAFT_BALLOON_EVOL(HLUOUT, PTSTEP, TPDTEXP, TPDTMOD, TPDTSEG, TPDTCUR,        &
                           PXHAT, PYHAT, PZ, PMAP, PLONOR, PLATOR,                    &
                           PU, PV, PW, PP, PTH, PR, PSV, PTKE, PTS, PRHODREF, PCIT,   &
                           PSPEEDC,PSPEEDR,PSPEEDS,PSPEEDG,PSPEEDH,           &
                           TAIRCRAFT12, PSEA                                          )
ENDIF
IF (TAIRCRAFT13%NMODEL /= 0) THEN
CALL AIRCRAFT_BALLOON_EVOL(HLUOUT, PTSTEP, TPDTEXP, TPDTMOD, TPDTSEG, TPDTCUR,        &
                           PXHAT, PYHAT, PZ, PMAP, PLONOR, PLATOR,                    &
                           PU, PV, PW, PP, PTH, PR, PSV, PTKE, PTS, PRHODREF, PCIT,   &
                           PSPEEDC,PSPEEDR,PSPEEDS,PSPEEDG,PSPEEDH,           &
                           TAIRCRAFT13, PSEA                                          )
ENDIF
IF (TAIRCRAFT14%NMODEL /= 0) THEN
CALL AIRCRAFT_BALLOON_EVOL(HLUOUT, PTSTEP, TPDTEXP, TPDTMOD, TPDTSEG, TPDTCUR,        &
                           PXHAT, PYHAT, PZ, PMAP, PLONOR, PLATOR,                    &
                           PU, PV, PW, PP, PTH, PR, PSV, PTKE, PTS, PRHODREF, PCIT,   &
                           PSPEEDC,PSPEEDR,PSPEEDS,PSPEEDG,PSPEEDH,           &
                           TAIRCRAFT14, PSEA                                          )
ENDIF
IF (TAIRCRAFT15%NMODEL /= 0) THEN
CALL AIRCRAFT_BALLOON_EVOL(HLUOUT, PTSTEP, TPDTEXP, TPDTMOD, TPDTSEG, TPDTCUR,        &
                           PXHAT, PYHAT, PZ, PMAP, PLONOR, PLATOR,                    &
                           PU, PV, PW, PP, PTH, PR, PSV, PTKE, PTS, PRHODREF, PCIT,   &
                           PSPEEDC,PSPEEDR,PSPEEDS,PSPEEDG,PSPEEDH,           &
                           TAIRCRAFT15, PSEA                                          )
ENDIF
IF (TAIRCRAFT16%NMODEL /= 0) THEN
CALL AIRCRAFT_BALLOON_EVOL(HLUOUT, PTSTEP, TPDTEXP, TPDTMOD, TPDTSEG, TPDTCUR,        &
                           PXHAT, PYHAT, PZ, PMAP, PLONOR, PLATOR,                    &
                           PU, PV, PW, PP, PTH, PR, PSV, PTKE, PTS, PRHODREF, PCIT,   &
                           PSPEEDC,PSPEEDR,PSPEEDS,PSPEEDG,PSPEEDH,           &
                           TAIRCRAFT16, PSEA                                          )
ENDIF
IF (TAIRCRAFT17%NMODEL /= 0) THEN
CALL AIRCRAFT_BALLOON_EVOL(HLUOUT, PTSTEP, TPDTEXP, TPDTMOD, TPDTSEG, TPDTCUR,        &
                           PXHAT, PYHAT, PZ, PMAP, PLONOR, PLATOR,                    &
                           PU, PV, PW, PP, PTH, PR, PSV, PTKE, PTS, PRHODREF, PCIT,   &
                           PSPEEDC,PSPEEDR,PSPEEDS,PSPEEDG,PSPEEDH,           &
                           TAIRCRAFT17, PSEA                                          )
ENDIF
IF (TAIRCRAFT18%NMODEL /= 0) THEN
CALL AIRCRAFT_BALLOON_EVOL(HLUOUT, PTSTEP, TPDTEXP, TPDTMOD, TPDTSEG, TPDTCUR,        &
                           PXHAT, PYHAT, PZ, PMAP, PLONOR, PLATOR,                    &
                           PU, PV, PW, PP, PTH, PR, PSV, PTKE, PTS, PRHODREF, PCIT,   &
                           PSPEEDC,PSPEEDR,PSPEEDS,PSPEEDG,PSPEEDH,           &
                           TAIRCRAFT18, PSEA                                          )
ENDIF
IF (TAIRCRAFT19%NMODEL /= 0) THEN
CALL AIRCRAFT_BALLOON_EVOL(HLUOUT, PTSTEP, TPDTEXP, TPDTMOD, TPDTSEG, TPDTCUR,        &
                           PXHAT, PYHAT, PZ, PMAP, PLONOR, PLATOR,                    &
                           PU, PV, PW, PP, PTH, PR, PSV, PTKE, PTS, PRHODREF, PCIT,   &
                           PSPEEDC,PSPEEDR,PSPEEDS,PSPEEDG,PSPEEDH,           &
                           TAIRCRAFT19, PSEA                                          )
ENDIF
IF (TAIRCRAFT20%NMODEL /= 0) THEN
CALL AIRCRAFT_BALLOON_EVOL(HLUOUT, PTSTEP, TPDTEXP, TPDTMOD, TPDTSEG, TPDTCUR,        &
                           PXHAT, PYHAT, PZ, PMAP, PLONOR, PLATOR,                    &
                           PU, PV, PW, PP, PTH, PR, PSV, PTKE, PTS, PRHODREF, PCIT,   &
                           PSPEEDC,PSPEEDR,PSPEEDS,PSPEEDG,PSPEEDH,           &
                           TAIRCRAFT20, PSEA                                          )
ENDIF
IF (TAIRCRAFT21%NMODEL /= 0) THEN
CALL AIRCRAFT_BALLOON_EVOL(HLUOUT, PTSTEP, TPDTEXP, TPDTMOD, TPDTSEG, TPDTCUR,        &
                           PXHAT, PYHAT, PZ, PMAP, PLONOR, PLATOR,                    &
                           PU, PV, PW, PP, PTH, PR, PSV, PTKE, PTS, PRHODREF, PCIT,   &
                           PSPEEDC,PSPEEDR,PSPEEDS,PSPEEDG,PSPEEDH,           &
                           TAIRCRAFT21, PSEA                                          )
ENDIF
IF (TAIRCRAFT22%NMODEL /= 0) THEN
CALL AIRCRAFT_BALLOON_EVOL(HLUOUT, PTSTEP, TPDTEXP, TPDTMOD, TPDTSEG, TPDTCUR,        &
                           PXHAT, PYHAT, PZ, PMAP, PLONOR, PLATOR,                    &
                           PU, PV, PW, PP, PTH, PR, PSV, PTKE, PTS, PRHODREF, PCIT,   &
                           PSPEEDC,PSPEEDR,PSPEEDS,PSPEEDG,PSPEEDH,           &
                           TAIRCRAFT22, PSEA                                          )
ENDIF
IF (TAIRCRAFT23%NMODEL /= 0) THEN
CALL AIRCRAFT_BALLOON_EVOL(HLUOUT, PTSTEP, TPDTEXP, TPDTMOD, TPDTSEG, TPDTCUR,        &
                           PXHAT, PYHAT, PZ, PMAP, PLONOR, PLATOR,                    &
                           PU, PV, PW, PP, PTH, PR, PSV, PTKE, PTS, PRHODREF, PCIT,   &
                           PSPEEDC,PSPEEDR,PSPEEDS,PSPEEDG,PSPEEDH,           &
                           TAIRCRAFT23, PSEA                                          )
ENDIF
IF (TAIRCRAFT24%NMODEL /= 0) THEN
CALL AIRCRAFT_BALLOON_EVOL(HLUOUT, PTSTEP, TPDTEXP, TPDTMOD, TPDTSEG, TPDTCUR,        &
                           PXHAT, PYHAT, PZ, PMAP, PLONOR, PLATOR,                    &
                           PU, PV, PW, PP, PTH, PR, PSV, PTKE, PTS, PRHODREF, PCIT,   &
                           PSPEEDC,PSPEEDR,PSPEEDS,PSPEEDG,PSPEEDH,           &
                           TAIRCRAFT24, PSEA                                          )
ENDIF
IF (TAIRCRAFT25%NMODEL /= 0) THEN
CALL AIRCRAFT_BALLOON_EVOL(HLUOUT, PTSTEP, TPDTEXP, TPDTMOD, TPDTSEG, TPDTCUR,        &
                           PXHAT, PYHAT, PZ, PMAP, PLONOR, PLATOR,                    &
                           PU, PV, PW, PP, PTH, PR, PSV, PTKE, PTS, PRHODREF, PCIT,   &
                           PSPEEDC,PSPEEDR,PSPEEDS,PSPEEDG,PSPEEDH,           &
                           TAIRCRAFT25, PSEA                                          )
ENDIF
IF (TAIRCRAFT26%NMODEL /= 0) THEN
CALL AIRCRAFT_BALLOON_EVOL(HLUOUT, PTSTEP, TPDTEXP, TPDTMOD, TPDTSEG, TPDTCUR,        &
                           PXHAT, PYHAT, PZ, PMAP, PLONOR, PLATOR,                    &
                           PU, PV, PW, PP, PTH, PR, PSV, PTKE, PTS, PRHODREF, PCIT,   &
                           PSPEEDC,PSPEEDR,PSPEEDS,PSPEEDG,PSPEEDH,           &
                           TAIRCRAFT26, PSEA                                          )
ENDIF
IF (TAIRCRAFT27%NMODEL /= 0) THEN
CALL AIRCRAFT_BALLOON_EVOL(HLUOUT, PTSTEP, TPDTEXP, TPDTMOD, TPDTSEG, TPDTCUR,        &
                           PXHAT, PYHAT, PZ, PMAP, PLONOR, PLATOR,                    &
                           PU, PV, PW, PP, PTH, PR, PSV, PTKE, PTS, PRHODREF, PCIT,   &
                           PSPEEDC,PSPEEDR,PSPEEDS,PSPEEDG,PSPEEDH,           &
                           TAIRCRAFT27, PSEA                                          )
ENDIF
IF (TAIRCRAFT28%NMODEL /= 0) THEN
CALL AIRCRAFT_BALLOON_EVOL(HLUOUT, PTSTEP, TPDTEXP, TPDTMOD, TPDTSEG, TPDTCUR,        &
                           PXHAT, PYHAT, PZ, PMAP, PLONOR, PLATOR,                    &
                           PU, PV, PW, PP, PTH, PR, PSV, PTKE, PTS, PRHODREF, PCIT,   &
                           PSPEEDC,PSPEEDR,PSPEEDS,PSPEEDG,PSPEEDH,           &
                           TAIRCRAFT28, PSEA                                          )
ENDIF
IF (TAIRCRAFT29%NMODEL /= 0) THEN
CALL AIRCRAFT_BALLOON_EVOL(HLUOUT, PTSTEP, TPDTEXP, TPDTMOD, TPDTSEG, TPDTCUR,        &
                           PXHAT, PYHAT, PZ, PMAP, PLONOR, PLATOR,                    &
                           PU, PV, PW, PP, PTH, PR, PSV, PTKE, PTS, PRHODREF, PCIT,   &
                           PSPEEDC,PSPEEDR,PSPEEDS,PSPEEDG,PSPEEDH,           &
                           TAIRCRAFT29, PSEA                                          )
ENDIF
IF (TAIRCRAFT30%NMODEL /= 0) THEN
CALL AIRCRAFT_BALLOON_EVOL(HLUOUT, PTSTEP, TPDTEXP, TPDTMOD, TPDTSEG, TPDTCUR,        &
                           PXHAT, PYHAT, PZ, PMAP, PLONOR, PLATOR,                    &
                           PU, PV, PW, PP, PTH, PR, PSV, PTKE, PTS, PRHODREF, PCIT,   &
                           PSPEEDC,PSPEEDR,PSPEEDS,PSPEEDG,PSPEEDH,           &
                           TAIRCRAFT30, PSEA                                          )
ENDIF
!
!----------------------------------------------------------------------------
!
END SUBROUTINE AIRCRAFT_BALLOON
