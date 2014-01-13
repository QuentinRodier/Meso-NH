!SURFEX_LIC Copyright 1994-2014 Meteo-France 
!SURFEX_LIC This is part of the SURFEX software governed by the CeCILL-C  licence
!SURFEX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!SURFEX_LIC for details. version 1.
!     #########
      SUBROUTINE CARBON_SPINUP(KMONTH,KDAY,PTIME,                   &
                   OSPINUPCARBS, OSPINUPCARBW, PSPINMAXS, PSPINMAXW,   &
                   KNBYEARSPINS, KNBYEARSPINW, KNBYEARSOLD, HPHOTO,    &
                   HRESPSL, KSPINS, KSPINW                             )
  
!     #######################################################################
!
!
!!****  *CARBON_SPINUP*  
!!
!!    PURPOSE
!!    -------
!     Number of times the accelerated subroutine is called  
!     for each time step  
!!**  METHOD
!!    ------
!
!!    EXTERNAL
!!    --------
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------ 
!!
!!      
!!    REFERENCE
!!    ---------
!!      
!!    AUTHOR
!!    ------
!!	R. Alkama           * Meteo-France *
!!
!!    MODIFICATIONS
!!    -------------
!!      Original      03/26/2012
!!
!-------------------------------------------------------------------------------
!
!*       0.     DECLARATIONS
!               ------------
!
!                              
USE MODI_SPINUP_MAX
!                              
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
 CHARACTER(LEN=3), INTENT(IN) :: HRESPSL                ! Soil Respiration
!                                                      ! 'DEF' = Norman 1992
!                                                      ! 'PRM' = Rivalland PhD Thesis (2003)
!                                                      ! 'CNT' = CENTURY model (Gibelin 2008)
 CHARACTER(LEN=3), INTENT(IN) :: HPHOTO                 ! type of photosynthesis
!
INTEGER,          INTENT(IN)  :: KMONTH                ! Current month
INTEGER,          INTENT(IN)  :: KDAY                  ! Current day
REAL,             INTENT(IN)  :: PTIME                 ! Current time
LOGICAL,          INTENT(IN)  :: OSPINUPCARBS          ! T: do the soil carb spinup, F: no
LOGICAL,          INTENT(IN)  :: OSPINUPCARBW          ! T: do the wood carb spinup, F: no
REAL,             INTENT(IN)  :: PSPINMAXS             ! max number of times CARBON_SOIL subroutine
REAL,             INTENT(IN)  :: PSPINMAXW             ! max number of times the wood is accelerated
INTEGER,          INTENT(IN)  :: KNBYEARSPINS          ! nbr years needed to reaches soil equilibrium
INTEGER,          INTENT(IN)  :: KNBYEARSPINW          ! nbr years needed to reaches wood equilibrium
!
INTEGER,          INTENT(INOUT) :: KNBYEARSOLD         ! nbr years executed at curent time step
INTEGER,          INTENT(OUT)   :: KSPINS              ! number of times the soil is accelerated
INTEGER,          INTENT(OUT)   :: KSPINW              ! number of times the wood is accelerated
!
!*      0.    declarations of local variables
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
!
!       1.     Initializations
!              ---------------
!
IF (LHOOK) CALL DR_HOOK('CARBON_SPINUP',0,ZHOOK_HANDLE)
!
IF (KMONTH == 1 .AND. KDAY==1 .AND. PTIME == 0.0 )THEN
   KNBYEARSOLD = KNBYEARSOLD + 1
ENDIF
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
! number of times CARBON_SOIL subroutine is called for each time step
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
KSPINS =1
IF ( OSPINUPCARBS .AND. HPHOTO/='NON' .AND. HRESPSL=='CNT' ) THEN
   CALL SPINUP_MAX(PSPINMAXS,KNBYEARSPINS,KNBYEARSOLD,KSPINS)
ENDIF
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
! number of times  WOOD carbon subroutine is called for each time step
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
KSPINW=1
IF ( OSPINUPCARBW .AND. HPHOTO=='NCB' ) THEN
   CALL SPINUP_MAX(PSPINMAXW,KNBYEARSPINW,KNBYEARSOLD,KSPINW)
ENDIF
!
IF (LHOOK) CALL DR_HOOK('CARBON_SPINUP',1,ZHOOK_HANDLE)
!
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE 
