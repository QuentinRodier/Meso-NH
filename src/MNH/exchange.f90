!MNH_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
!MNH_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!MNH_LIC for details. version 1.
!-----------------------------------------------------------------
!--------------- special set of characters for RCS information
!-----------------------------------------------------------------
! $Source$ $Revision$ $Date$
!-----------------------------------------------------------------
!-----------------------------------------------------------------
!-----------------------------------------------------------------
!     ####################
      MODULE MODI_EXCHANGE
!     ####################
!
INTERFACE
!
!     ##############################################################################
      SUBROUTINE EXCHANGE (PTSTEP,KRR,KSV,PRHODJ,TPFIELDS_ll,                      &
                           PRUS,PRVS,PRWS,PRTHS,PRRS,PRTKES,PRSVS                  )
!     ##############################################################################
!
USE MODD_ARGSLIST_ll, ONLY : LIST_ll
!
REAL,                     INTENT(IN) :: PTSTEP            !  Time step
INTEGER,                  INTENT(IN) :: KRR               !  Number of water var.
INTEGER,                  INTENT(IN) :: KSV               !  Number of scal. var.
                                                          ! (=1 at the segment beginning)
REAL, DIMENSION(:,:,:),   INTENT(IN) :: PRHODJ            ! (Rho) dry * Jacobian
TYPE(LIST_ll), POINTER               :: TPFIELDS_ll       ! list of fields to exchange
!
REAL, DIMENSION(:,:,:),   INTENT(INOUT) :: PRUS,PRVS,PRWS,   &!
                                           PRTHS,PRTKES       ! Source terms
REAL, DIMENSION(:,:,:,:), INTENT(INOUT) :: PRRS,PRSVS         !
!
END SUBROUTINE EXCHANGE
!
END INTERFACE
!
END MODULE MODI_EXCHANGE
!
!
!
!     #######################################################################
      SUBROUTINE EXCHANGE (PTSTEP,KRR,KSV,PRHODJ,TPFIELDS_ll,               &
                           PRUS,PRVS,PRWS,PRTHS,PRRS,PRTKES,PRSVS           )
!     #######################################################################
!
!!****  * EXCHANGE* - update the halo of each subdomains for the variables at time step t+dt
!!
!!    PURPOSE
!!    -------
!!
!!    The purpose of EXCHANGE is to transform the source terms in the variables at time step t+dt
!!    and update the halo of each subdomains. This routine also takes into account the
!!    cyclic conditions
!
!!**  METHOD
!!    ------
!!    The source term is multipied by twice the time step (except for the first time step)
!!    and divided by ( rhod J ) to obtain the value of the variables at
!!    time step t+dt. The halos of these fields are updated with the values computed by the
!!    neighbor subdomains. Cyclic conditions are treated during this exchange.
!!
!!    EXTERNAL
!!    --------
!!       UPDATE_HALO_ll  :   routine to update the halo
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!          MODD_CONF
!!
!!    REFERENCE
!!    ---------
!!    Book2 of documentation
!!
!!    AUTHOR
!!    ------
!!    P. Jabouille  Meteo France
!!
!!    MODIFICATIONS
!!    -------------
!!
!!    original     18/09/98
!!                 05/2006   Remove KEPS
!!                 10/2009 (C.Lac) FIT for variables advected by PPM
!!                 05/2014 (C.Lac) Correction of negative values of chemical
!!                   tracers moved from ch_monitor to the end of the time step 
!------------------------------------------------------------------------------
!
!*      0.   DECLARATIONS
!            ------------
!
USE MODE_ll
!
USE MODD_ARGSLIST_ll, ONLY : LIST_ll
USE MODD_GRID_n
USE MODD_NSV
USE MODD_BUDGET,      ONLY : LBUDGET_SV
USE MODD_CST,         ONLY : XMNH_TINY
USE MODD_LUNIT_n,     ONLY : CLUOUT
USE MODI_SHUMAN
USE MODI_SUM_ll
USE MODI_BUDGET
!
IMPLICIT NONE
!
!*      0.1  DECLARATIONS OF ARGUMENTS
!
REAL,                     INTENT(IN) :: PTSTEP            !  Time step
INTEGER,                  INTENT(IN) :: KRR               !  Number of water var.
INTEGER,                  INTENT(IN) :: KSV               !  Number of scal. var.
REAL, DIMENSION(:,:,:),   INTENT(IN) :: PRHODJ            ! (Rho) dry * Jacobian
TYPE(LIST_ll), POINTER               :: TPFIELDS_ll       ! list of fields to exchange
!
REAL, DIMENSION(:,:,:),   INTENT(INOUT) :: PRUS,PRVS,PRWS,   &!
                                           PRTHS,PRTKES       ! Source terms
REAL, DIMENSION(:,:,:,:), INTENT(INOUT) :: PRRS,PRSVS         !
!
!!
!*      0.2  DECLARATIONS OF LOCAL VARIABLES
!
INTEGER   :: IINFO_ll              ! return code of parallel routine
INTEGER   :: JRR,JSV              ! loop counters
!
INTEGER   :: IKU
INTEGER   :: ILUOUT         ! logical unit numbers of output-listing
INTEGER   :: IRESP          ! IRESP  : return-code if a problem appears
                                    !in LFI subroutines at the open of the file
REAL      :: ZRATIO, ZMASSTOT, ZMASSPOS
!------------------------------------------------------------------------------
!
IKU=SIZE(XZHAT)
CALL FMLOOK_ll(CLUOUT,CLUOUT,ILUOUT,IRESP)
!
!*       1.     TRANSFORMS THE SOURCE TERMS INTO PROGNOSTIC VARIABLES
!               -----------------------------------------------------
!
!        1.a Momentum variables
!
PRUS(:,:,:) = PRUS(:,:,:)*PTSTEP / MXM(PRHODJ)
PRVS(:,:,:) = PRVS(:,:,:)*PTSTEP / MYM(PRHODJ)
PRWS(:,:,:) = PRWS(:,:,:)*PTSTEP / MZM(1,IKU,1,PRHODJ)
!
!        1.b Meteorological scalar variables
!
PRTHS(:,:,:) = PRTHS(:,:,:)*PTSTEP/PRHODJ
DO JRR=1,KRR
  PRRS(:,:,:,JRR) = PRRS(:,:,:,JRR)*PTSTEP/PRHODJ
END DO
IF (SIZE(PRTKES,1) /= 0) PRTKES(:,:,:) = PRTKES(:,:,:)*PTSTEP/PRHODJ
!
!        1.c Tracer scalar variables
!
!      REMOVE NEGATIVE VALUES OF CHEM SCALAR
!
DO JSV = 1, KSV
  IF ( MIN_ll( PRSVS(:,:,:,NSV_CHEMBEG+JSV-1), IINFO_ll) < 0.0 ) THEN
!
! compute the total water mass computation
!
    ZMASSTOT = MAX( 0. , SUM3D_ll( PRSVS(:,:,:,NSV_CHEMBEG+JSV-1), IINFO_ll ) )
!
! remove the negative values
!
    PRSVS(:,:,:,NSV_CHEMBEG+JSV-1) = MAX(0., PRSVS(:,:,:,NSV_CHEMBEG+JSV-1) )
!
! compute the new total mass
!
    ZMASSPOS = MAX(XMNH_TINY,SUM3D_ll( PRSVS(:,:,:,NSV_CHEMBEG+JSV-1), IINFO_ll ) )
!
! correct again in such a way to conserve the total mass 
!
    ZRATIO = ZMASSTOT / ZMASSPOS
    PRSVS(:,:,:,NSV_CHEMBEG+JSV-1) = PRSVS(:,:,:,NSV_CHEMBEG+JSV-1) * ZRATIO
!
    WRITE(ILUOUT,*)'DUE TO CHEMISTRY',JSV,'HAS NEGATIVE VALUES'
    WRITE(ILUOUT,*)'SOURCES IS CORRECTED BY RATIO',ZRATIO
  END IF
END DO
!
IF (LBUDGET_SV) THEN
  DO JSV=NSV_CHEMBEG,NSV_CHEMEND
    CALL BUDGET(PRSVS(:,:,:,JSV),JSV+12,'NEGA_BU_RSV')
  ENDDO
ENDIF
!
DO JSV=1,KSV
  PRSVS(:,:,:,JSV) = PRSVS(:,:,:,JSV)*PTSTEP/PRHODJ
END DO
!
!------------------------------------------------------------------------------
!
!*      2      UPDATE THE FIRST LAYER OF THE HALO
!              ----------------------------------
!
CALL UPDATE_HALO_ll(TPFIELDS_ll, IINFO_ll)
!
END SUBROUTINE EXCHANGE
