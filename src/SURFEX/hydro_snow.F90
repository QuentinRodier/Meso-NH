!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE HYDRO_SNOW(  OGLACIER, PTSTEP, PVEGTYPE,                   &
                              PSR, PLES, PMELT,                             &
                              PSNOWSWE, PSNOWALB, PSNOWRHO, PPG_MELT        )  
!     #####################################################################
!
!!****  *HYDRO_SNOW*  
!!
!!    PURPOSE
!!    -------
!
!     Calculates i) Snow water transfer to soil for both snow scheme options.
!     ii) the evolution of the snowpack using the Force-Restore
!     option of Douville et al. (1995): 'DEF'
!     Calculate the snow cover liquid water equivalent (Ws), the albedo and density of
!     the snow (i.e., SNOWALB and SNOWRHO).  Also determine the runoff and drainage
!     into the soil.
!         
!     
!!**  METHOD
!!    ------
!
!!    EXTERNAL
!!    --------
!!REAL, DIMENSION(:), INTENT(INOUT) :: PTG
!                                      PTG = surface temperature at 't'

!!    none
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------ 
!!
!!
!!      
!!    REFERENCE
!!    ---------
!!
!!    Noilhan and Planton (1989)
!!    Belair (1995)
!!      
!!    AUTHOR
!!    ------
!!
!!      S. Belair           * Meteo-France *
!!
!!    MODIFICATIONS
!!    -------------
!!
!!      Original    14/03/95 
!!                  31/08/98 (V. Masson and F. Habets) add Dumenil et Todini
!!                           runoff scheme
!!                  31/08/98 (V. Masson and A. Boone) add the third soil-water
!!                           reservoir (WG3,D3)
!!                  14/05/02 (A. Boone) snow only, and skip code if '3-L' option in force
!!                   03/2009 (B. Decharme) Consistency with Arpege permanent snow/ice treatment
!!                                          (LGLACIER)
!-------------------------------------------------------------------------------
!
!*       0.     DECLARATIONS
!               ------------
!
USE MODD_CSTS,        ONLY : XLSTT, XLMTT, XDAY
USE MODD_SNOW_PAR,    ONLY : XANS_T, XANS_TODRY, XANSMIN, XANSMAX, &
                               XRHOSMAX, XRHOSMIN, XWCRN, XAGLAMIN,  &
                               XAGLAMAX  
USE MODD_SURF_PAR,    ONLY : XUNDEF
USE MODD_DATA_COVER_PAR, ONLY : NVT_SNOW
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
!
LOGICAL, INTENT(IN)               :: OGLACIER   ! True = Over permanent snow and ice, 
!                                                     initialise WGI=WSAT,
!                                                     Hsnow>=10m and allow 0.8<SNOALB<0.85
                                                ! False = No specific treatment
REAL, INTENT(IN)                  :: PTSTEP
!                                    timestep of the integration
REAL, DIMENSION(:,:), INTENT(IN)  :: PVEGTYPE ! fraction of each vegetation
REAL, DIMENSION(:), INTENT(IN)    :: PSR,  PLES, PMELT
!                                    PSR = snow rate
!                                    PLES = latent heat of sublimation over the snow
!                                    PMELT = melting rate of snow
REAL, DIMENSION(:), INTENT(INOUT) :: PSNOWSWE, PSNOWALB, PSNOWRHO, PPG_MELT
!                                    PSNOWSWE = equivalent water content of the
!                                             snow reservoir at time 't+dt'
!                                    PSNOWALB = albedo of the snow at 't+dt'
!                                    PSNOWRHO = density of the snow at 't+dt'
!                                    PPG_MELT = total water reaching the ground
!
!*      0.2    declarations of local variables
!
REAL, DIMENSION(SIZE(PSR)) :: ZSNOWSWEM, ZWSX,  ZANSMIN, ZANSMAX
!                             Prognostic variables of ISBA at 't-dt'
!                             ZSNOWSWEM = equivalent water content of the
!                                         snow reservoir
!                             ZANSMIN = Minimum glacier albedo
!                             ZANSMAX = Maximum glacier albedo
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('HYDRO_SNOW',0,ZHOOK_HANDLE)                                                        
!-------------------------------------------------------------------------------
!
!*              Douville et al. (1995) 'DEF' snow option
!               ----------------------------------------
!        
!*       1.     Initialize:
!               -----------
!
ZWSX(:)       = 0.0
ZANSMIN(:)    = XANSMIN
ZANSMAX(:)    = XANSMAX
!
!
!*       2.     Fields at time t-dt
!               -------------------
!
ZSNOWSWEM (:) = PSNOWSWE(:)    
!
!*       3.     EVOLUTION OF THE SNOWPACK ('DEF' OPTION)
!               ----------------------------------------
!
!*       3.A    EVOLUTION OF THE EQUIVALENT WATER CONTENT snowSWE ('DEF' option)
!               --------------------------------------------------------------
!
!                                           evolution of Ws (without melting)
!
PSNOWSWE(:) = ZSNOWSWEM(:) + PTSTEP * ( PSR(:) - PLES(:)/XLSTT - PMELT(:))
!
!                                           melting of snow: more liquid water
!                                                            reaches the surface
!
PPG_MELT(:)     = PPG_MELT(:) + PMELT(:)  
!   
! removes very small values due to computation precision
!
WHERE(PSNOWSWE(:) < 1.0E-10) PSNOWSWE(:) = 0.
!
!-------------------------------------------------------------------------------
!
!*       3.B    EVOLUTION OF SNOW ALBEDO 
!               ------------------------
!
IF(OGLACIER)THEN
  ZANSMIN(:) = XAGLAMIN * PVEGTYPE(:,NVT_SNOW) + XANSMIN * (1.0-PVEGTYPE(:,NVT_SNOW))
  ZANSMAX(:) = XAGLAMAX * PVEGTYPE(:,NVT_SNOW) + XANSMAX * (1.0-PVEGTYPE(:,NVT_SNOW))
ELSE
  ZANSMIN(:) = XANSMIN
  ZANSMAX(:) = XANSMAX
ENDIF
!                                       the evolution of the snow albedo differs
!                                       if there is melting or not
!
WHERE (PSNOWSWE > 0.0 )
  !
  WHERE ( ZSNOWSWEM > 0.0)
    !
    ! when there is melting 
    WHERE ( PMELT > 0.0 )
      PSNOWALB(:) = (PSNOWALB(:)-ZANSMIN(:))*EXP(-XANS_T*PTSTEP/XDAY) + ZANSMIN(:) &
                    + PSR(:)*PTSTEP/XWCRN*(ZANSMAX(:)-ZANSMIN(:))  
      ! when there is no melting
    ELSEWHERE 
      PSNOWALB(:) = PSNOWALB(:) - XANS_TODRY*PTSTEP/XDAY                           &
                 + PSR(:)*PTSTEP/XWCRN*(ZANSMAX(:)-ZANSMIN(:))  
    END WHERE
    !
  ELSEWHERE (ZSNOWSWEM == 0.0)
    !
    ! new snow covered surface
    PSNOWALB(:) = ZANSMAX(:)
  END WHERE
  !
  ! limits of the albedo
  PSNOWALB(:) = MIN( ZANSMAX(:), PSNOWALB(:) )
  PSNOWALB(:) = MAX( ZANSMIN(:), PSNOWALB(:) )
END WHERE
!
!-------------------------------------------------------------------------------
!
!*       3.C    EVOLUTION OF SNOW DENSITY 
!               -------------------------
!
!                                      as for the snow albedo, the density's
!                                      evolution will depend whether or not
!                                      the snow is melting
!
WHERE ( PSNOWSWE > 0.0 ) 
  WHERE ( ZSNOWSWEM > 0.0 ) 
    ZWSX(:)     = MAX( PSNOWSWE(:),PSR(:)*PTSTEP)
    PSNOWRHO(:) = (PSNOWRHO(:)-XRHOSMAX)*EXP(-XANS_T*PTSTEP/XDAY) + XRHOSMAX
    PSNOWRHO(:) = ( (ZWSX(:)-PSR(:)*PTSTEP) * PSNOWRHO(:)                      &
                  + (PSR(:)*PTSTEP) * XRHOSMIN ) / ZWSX(:)  
  ELSEWHERE ( ZSNOWSWEM == 0.0) 
    PSNOWRHO(:) = XRHOSMIN
  END WHERE
END WHERE
!
!-------------------------------------------------------------------------------
!
!*       4.     No SNOW
!               -------
!
WHERE ( PSNOWSWE == 0.0 ) 
  PSNOWRHO(:) = XUNDEF 
  PSNOWALB(:) = XUNDEF 
END WHERE
!
IF (LHOOK) CALL DR_HOOK('HYDRO_SNOW',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE HYDRO_SNOW
