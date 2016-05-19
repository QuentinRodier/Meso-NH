      SUBROUTINE ICE_SOILFR(HSNOW_ISBA, HSOILFRZ, PTSTEP, PKSFC_IVEG, PCG, PCT, &
                            PPSNG, PFFG, PTAUICE, PDWGI1, PDWGI2, PWSATZ,       &
                            PMPOTSATZ, PBCOEFZ, PD_G, PTG, PWGI, PWG            )   
!!     ##########################################################################
!
!!****  *ICE_SOILFR*  
!!
!!    PURPOSE
!!    -------
!
!     In ISBA-FR: calculates the 
!     1.) evolution of the surface and deep-soil temperature(s)
!         (i.e., Ts and T2 if Force-Restore, TN if DIFfusion) due to soil water 
!         phase changes
!     
!!**  METHOD
!!    ------
!
!     - latent heating from soil ice phase changes
!
!!    EXTERNAL
!!    --------
!!
!!    none
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------ 
!!
!!      
!!    REFERENCE
!!    ---------
!!
!!    Noilhan and Planton (1989)
!!    Belair (1995)
!!    Boone et al. (2000)
!!      
!!    AUTHOR
!!    ------
!!
!!	A. Boone           * Meteo-France *
!!      B. Decharme        * Meteo-France *
!!
!!    MODIFICATIONS
!!    -------------
!!      Original      14/03/95 
!!      (A.Boone)     08/11/00 soil ice phase changes herein
!!      (A.Boone)     06/05/02 Updates, ordering. Addition of 'HSOILFRZ' option
!!      (B. Decharme) 03/2009  BUG : effect of insolation due to vegetation cover
!!                                  at 1 for bare soil
!!      (B. Decharme) 07/2012  Time spliting for soil ice
!!      (A.Boone)     02/2013  Split from isba_fluxes.F90
!-------------------------------------------------------------------------------
!
!*       0.     DECLARATIONS
!               ------------
!
USE MODD_CSTS,       ONLY : XCL, XTT, XPI, XDAY, XCI, XRHOLI,     &
                            XLMTT, XRHOLW, XG, XCONDI
USE MODD_SURF_PAR,   ONLY : XUNDEF
USE MODD_ISBA_PAR,   ONLY : XWGMIN, XSPHSOIL, XDRYWGHT
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
!
CHARACTER(LEN=*),     INTENT(IN)  :: HSNOW_ISBA ! 'DEF' = Default F-R snow scheme
!                                               !         (Douville et al. 1995)
!                                               ! '3-L' = 3-L snow scheme (option)
!                                               !         (Boone and Etchevers 2001)
!
CHARACTER(LEN=*),   INTENT(IN)      :: HSOILFRZ   ! soil freezing-physics option
!                                                 ! 'DEF'   Default (Boone et al. 2000; Giard and Bazile 2000)
!                                                 ! 'LWT'   phase changes as above, but relation between unfrozen 
!                                                         water and temperature considered
!
REAL, INTENT (IN)                   :: PTSTEP     ! model time step (s)
!
!
REAL, DIMENSION(:), INTENT(IN)      :: PKSFC_IVEG
!                                      PKSFC_IVEG= non-dimensional vegetation insolation coefficient
!
REAL, DIMENSION(:), INTENT(IN)      :: PTAUICE
!                                      PTAUICE = characteristic time scale for soil water phase changes (s)
!
REAL, DIMENSION(:),  INTENT(IN)     :: PPSNG
!                                      PPSNG = snow fractions over ground
!
REAL, DIMENSION(:),  INTENT(IN)     :: PFFG
!                                      PPSNG = flood fractions over ground
!
REAL, DIMENSION(:), INTENT (IN)     :: PCG, PCT
!                                      PCT    = area-averaged heat capacity (K m2 J-1)
!                                      PCG    = heat capacity of the soil (K m2 J-1)
!
REAL, DIMENSION(:,:), INTENT(IN)    :: PD_G, PWSATZ
!                                      PD_G   = Depth of bottom of Soil layers (m)
!                                      PWSATZ    = porosity (m3/m3)
!
REAL, DIMENSION(:,:), INTENT(IN)    :: PMPOTSATZ, PBCOEFZ
!                                      PMPOTSATZ = matric potential at saturation (m)
!                                      PBCOEFZ   = slope of the water retention curve (-)
!
REAL, DIMENSION(:,:), INTENT(INOUT) :: PWG, PWGI, PTG 
!                                      PWGI   = soil frozen volumetric water content (m3/m3)
!                                      PWG    = soil liquid volumetric water content (m3/m3)
!                                      PTG    = soil temperature profile (K)
!
REAL, DIMENSION(:), INTENT(OUT)     :: PDWGI1, PDWGI2
!                                      PDWGI1   = near-surface liquid water equivalent
!                                                 volumetric ice content tendency
!                                      PDWGI2   = deep ground liquid water equivalent
!                                                 volumetric ice content tendency
!
!
!*      0.2    declarations of local variables
!
REAL                        ::   ZKSOIL     ! coefficient for soil freeze/thaw
!
REAL, DIMENSION(SIZE(PCG)) ::   ZKSFC_FRZ, ZFREEZING, ZICE_MELT, ZWIM,       &
                                 ZWIT, ZWGI1, ZWGI2, ZWM, ZSOILHEATCAP,       &
                                 ZICEEFF, ZEFFIC, ZTAUICE,                    &
                                 ZWGMIN, ZTGMAX, ZMATPOT, ZDELTAT
!                                ZKSFC_FRZ    = surface insolation coefficient (kg m-2 K-1)
!                                ZFREEZING    = rate for freezing soil water (kg m-2)
!                                ZICE_MELT    = rate for melting soil ice (kg m-2)
!                                ZWIM,ZWIT    = available ice content (m3 m-3)
!                                ZWGI1,ZWGI2  = volumetric ice contents (m3 m-3)
!                                ZWM          = available liquid water content (m3 m-3)
!                                ZSOILHEATCAP = soil heat capacity (J  m-3 K-1)
!                                ZICEEFF      = effective soil ice penetration depth (m)
!                                ZEFFIC       = phase change efficiency
!                                ZMATPOT      = soil matric potential (m)
!                                ZWGMIN       = volumetric water content above which soil water can
!                                               be unfrozen (if energy and mass available)(m3 m-3)
!                                ZTGMAX       = temperature below which liquid water 
!                                               can be frozen (if energy and mass available)(K)
!                                ZDELTAT      = Freezing or melting temperature depression (K) after 
!                                               possible flux correction
!
REAL, DIMENSION(SIZE(PCG)) ::  ZWSAT_AVGZ
!                               ZWSAT_AVGZ = soil column average porosity (m3 m-3)
!
REAL, DIMENSION(SIZE(PCG)) :: ZPSNG
!                               ZPSNG = snow fractions corresponding to
!                                       dummy argument PPSNG
!                                       if HSNOW_ISBA = 'DEF' (composite
!                                       or Force-Restore snow scheme), else
!                                       they are zero for explicit snow case
!                                       as snow fluxes calculated outside of
!                                       this routine using the 
!                                       HSNOW_ISBA = '3-L' or 'CRO' option.
!
!*      0.3    declarations of local parameters
!
REAL, PARAMETER             :: ZINSOLFRZ_VEG = 0.20  ! (-)       Vegetation insolation coefficient
!
REAL, PARAMETER             :: ZINSOLFRZ_LAI = 30.0  ! (m2 m-2)  Vegetation insolation coefficient
!
REAL, PARAMETER             :: ZEFFIC_MIN    = 0.01  ! (-)   (0 <= ZEFFIC_MIN << 1)
!                                                                This parameter ensures
!                                                                a small minimum melt or freeze efficiency...
!                                                                It is numerical. When it is small, it has
!                                                                a only small impact on results, except
!                                                                that it keeps very small values of ice from persisting
!                                                                over long periods of time as they approach zero.
!                                                                If it is zero, then this effect off.
!
!
INTEGER         :: INI, JJ
!
REAL, DIMENSION(SIZE(PCG))          :: ZWORK1, ZWORK2, ZTDIURN
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------
!
!*       0.     Initialization
!               --------------
IF (LHOOK) CALL DR_HOOK('ICE_SOILFR',0,ZHOOK_HANDLE)
!
ZFREEZING(:)    = 0.0
ZKSFC_FRZ(:)    = 0.0
ZEFFIC(:)       = 0.0
ZICE_MELT(:)    = 0.0
ZWGI1(:)        = 0.0
ZWIM(:)         = 0.0
ZSOILHEATCAP(:) = 0.0
ZWIT(:)         = 0.0
ZWGI2(:)        = 0.0
ZTGMAX(:)       = 0.0
ZWGMIN(:)       = 0.0
ZMATPOT(:)      = 0.0
ZWSAT_AVGZ(:)   = XUNDEF
ZDELTAT(:)      = 0.0
ZTDIURN(:)      = 0.0
!
INI = SIZE(PTG,1)
!
!-------------------------------------------------------------------------------
!
! If ISBA-ES option in use, then snow covered surface
! fluxes calculated outside of this routine, so set
! the local snow fractions here to zero:
! 
IF(HSNOW_ISBA == '3-L' .OR. HSNOW_ISBA == 'CRO')THEN
   ZPSNG(:)     = 0.0
ELSE
   ZPSNG(:)     = PPSNG(:)+PFFG(:)
ENDIF
!
!*       1.    Melting/freezing normalized coefficient
!               ---------------------------------------
!
ZKSOIL       = (0.5 * SQRT(XCONDI*XCI*XRHOLI*XDAY/XPI))/XLMTT
!
ZTAUICE (:) = MAX(PTSTEP,PTAUICE(:))
!
DO JJ=1,INI
!-------------------------------------------------------------------------------
!*       2.     EFFECT OF THE MELTING/FREEZING 
!               ON THE SURFACE-SOIL HEAT AND ICE CONTENTS
!               ('DEF' or Force-Restore soil option)
!               -----------------------------------------
!
!        2.0    Average soil-column porosity
!               ----------------------------
!               if Force-Restore option in use, then vertical
!               profiles of soil hydrological parameters are constant,
!               so use the values in uppermost element (arbitrary)
!
    ZWSAT_AVGZ(JJ) = PWSATZ(JJ,1)
!
!               Influence of vegetation insolation on surface:
!
    ZKSFC_FRZ(JJ) = ZKSOIL * PKSFC_IVEG(JJ)
!
ENDDO
!*       2.2    Water freezing
!               --------------
!
IF(HSOILFRZ == 'LWT')THEN
!
! use option to control phase changes based on a relationship
! between the unfrozen liquid water content and temperature.
! Uses the Clapp and Hornberger model for water potential.
! The energy-limit method used by Boone et al. 2000 and
! Giard and Bazile (2000) is the default. 
!
  DO JJ=1,INI
      ZMATPOT(JJ)   = MIN(PMPOTSATZ(JJ,1), XLMTT*(PTG(JJ,1)-XTT)/(XG*PTG(JJ,1)) )
      ZWGMIN(JJ)    = ZWSAT_AVGZ(JJ)*( (ZMATPOT(JJ)/PMPOTSATZ(JJ,1))**(-1./PBCOEFZ(JJ,1)) )

      ZMATPOT(JJ)   = PMPOTSATZ(JJ,1)*( (PWG(JJ,1)/ZWSAT_AVGZ(JJ))**(-PBCOEFZ(JJ,1)) )
      ZTGMAX(JJ)    = XLMTT*XTT/(XLMTT - XG* ZMATPOT(JJ))
  ENDDO
ELSE
    ZWGMIN(:)    = XWGMIN
    ZTGMAX(:)    = XTT
ENDIF
!
DO JJ=1,INI
! 
    ZDELTAT(JJ)  = PTG(JJ,1) - ZTGMAX(JJ) ! initial temperature depression
!
    ZWORK2(JJ) = XRHOLW*PD_G(JJ,1)
    ZEFFIC(JJ)    = MAX(ZEFFIC_MIN,(PWG(JJ,1)-XWGMIN)/ZWSAT_AVGZ(JJ))
    ZFREEZING(JJ) = MIN( MAX(0.0,PWG(JJ,1)-ZWGMIN(JJ))*ZWORK2(JJ),    &  
                  ZKSFC_FRZ(JJ)*ZEFFIC(JJ)*MAX( -ZDELTAT(JJ), 0.) )
!
!*       2.3    Ground Ice melt
!               ---------------
!
    ZEFFIC(JJ)    =  MAX(ZEFFIC_MIN,PWGI(JJ,1)/(ZWSAT_AVGZ(JJ)-XWGMIN))
    ZICE_MELT(JJ) = MIN( PWGI(JJ,1)*ZWORK2(JJ),                      &
                  ZKSFC_FRZ(JJ)*ZEFFIC(JJ)*MAX( ZDELTAT(JJ), 0. ) )
!
!*       2.4    Ice reservoir evolution
!               -----------------------
!
! Melting of ice/freezing of water:
!
    ZWGI1(JJ) = PWGI(JJ,1) + (PTSTEP/ZTAUICE(JJ))*(1.0-ZPSNG(JJ))*        &
              (ZFREEZING(JJ) - ZICE_MELT(JJ))/ZWORK2(JJ) 
!
!
    ZWGI1(JJ)  = MAX( ZWGI1(JJ) , 0.             )
    ZWGI1(JJ)  = MIN( ZWGI1(JJ) , ZWSAT_AVGZ(JJ)-XWGMIN)
!
! Time tendency:
!
    PDWGI1(JJ) = ZWGI1(JJ) - PWGI(JJ,1)
!
!
!*       2.5    Effect on temperature
!               ---------------------
!
    PTG(JJ,1)   = PTG(JJ,1) + PDWGI1(JJ)*XLMTT*PCT(JJ)*ZWORK2(JJ)
!
!-------------------------------------------------------------------------------
!
!*       3.     EFFECT OF THE MELTING/FREEZING 
!               ON THE DEEP-SOIL HEAT AND ICE CONTENTS
!               ('DEF' or Force-Restore soil option)
!               --------------------------------------
!
    ZWORK1(JJ) = PD_G(JJ,1)/PD_G(JJ,2)
!*       3.1  Available Deep ice content
!             --------------------------
!
    ZWIM(JJ) = ( PWGI(JJ,2) - ZWORK1(JJ) * PWGI(JJ,1) )  / ( 1. - ZWORK1(JJ) )
!
    ZWIM(JJ) = MAX(0.,ZWIM(JJ))  ! Just in case of round-off errors
!
!*       3.2  Deep liquid water content
!             -------------------------
!
    ZWM(JJ)  = ( PWG(JJ,2) - ZWORK1(JJ) * PWG(JJ,1) )  / ( 1. - ZWORK1(JJ) )
!
!*       3.3    Water freezing
!               --------------
!
! Total soil volumetric heat capacity [J/(m3 K)]:
!
    ZSOILHEATCAP(JJ) = XCL*XRHOLW*PWG(JJ,2)  +                           &
                     XCI*XRHOLI*PWGI(JJ,2) +                           &
                     XSPHSOIL*XDRYWGHT*(1.0-ZWSAT_AVGZ(JJ))*(1.0-ZWSAT_AVGZ(JJ))
!
! Soil thickness which corresponds to T2 (m): 2 times the diurnal
! surface temperature wave penetration depth as T2 is the average
! temperature for this layer:
!
    ZTDIURN(JJ)   = MIN(PD_G(JJ,2), 4./(ZSOILHEATCAP(JJ)*PCG(JJ)))
!
! Effective soil ice penetration depth (m):
!
    ZICEEFF(JJ)   = (PWGI(JJ,2)/(PWGI(JJ,2)+PWG(JJ,2)))*PD_G(JJ,2)
!
ENDDO
!
IF(HSOILFRZ == 'LWT')THEN
!
! as for the surface layer (above)JJ 
! Note also that if the 'DIF'
! soil option is not in force, then the soil parameters are assumed
! to be homogeneous (in the verticalJJ thus we use 1st element of 2nd dimension
! of the 2D-soil parameter arrays).
!
  DO JJ=1,INI

       ZMATPOT(JJ)   = MIN(PMPOTSATZ(JJ,1), XLMTT*(PTG(JJ,2)-XTT)/(XG*PTG(JJ,2)) )
       ZWGMIN(JJ)    = ZWSAT_AVGZ(JJ)*( (ZMATPOT(JJ)/PMPOTSATZ(JJ,1))**(-1./PBCOEFZ(JJ,1)) )

       ZMATPOT(JJ)   = PMPOTSATZ(JJ,1)*( (PWG(JJ,2)/ZWSAT_AVGZ(JJ))**(-PBCOEFZ(JJ,1)) )
       ZTGMAX(JJ)    = XLMTT*XTT/(XLMTT - XG* ZMATPOT(JJ))
  ENDDO
ELSE
    ZWGMIN(:)    = XWGMIN
    ZTGMAX(:)    = XTT
ENDIF
!
! Allow freezing by T2 up to a certain depth so that
! T2 energy can not be used to freeze soil water
! at levels sufficiently deep in the soil.
!
DO JJ=1,INI
!
    ZDELTAT(JJ)  = PTG(JJ,2) - ZTGMAX(JJ) ! initial temperature depression 
!  
    ZWORK1(JJ) = PD_G(JJ,1)/PD_G(JJ,2)
    ZWORK2(JJ) = XRHOLW*(PD_G(JJ,2)-PD_G(JJ,1))

    ZFREEZING(JJ) = 0.0
    IF (ZICEEFF(JJ) <= ZTDIURN(JJ)) THEN
!
      ZEFFIC(JJ)    = MAX(ZEFFIC_MIN, MAX(0.0,ZWM(JJ) - XWGMIN)/ZWSAT_AVGZ(JJ))
      ZFREEZING(JJ) = MIN( MAX(0.0, ZWM(JJ) - ZWGMIN(JJ))* ZWORK2(JJ),            &
                     ZKSOIL*ZEFFIC(JJ)*MAX( -ZDELTAT(JJ) , 0. ) )
    ENDIF
!
!
!*       3.4    Ground Ice melt
!               ---------------
!
    ZEFFIC(JJ)    = MAX(ZEFFIC_MIN, ZWIM(JJ)/(ZWSAT_AVGZ(JJ)-XWGMIN))
    ZICE_MELT(JJ) = MIN( ZWIM(JJ)*ZWORK2(JJ),             &
                  ZKSOIL*ZEFFIC(JJ)*MAX( ZDELTAT(JJ) , 0. ) )
!
!
!*       3.5    Deep-part of deep-soil Ice reservoir evolution
!               ----------------------------------------------
!
    ZWIT(JJ)   = ZWIM(JJ) + (PTSTEP/ZTAUICE(JJ))*(1.0-ZPSNG(JJ))*       &
               ((ZFREEZING(JJ) - ZICE_MELT(JJ))/ ZWORK2(JJ))
!
    ZWIT(JJ)   = MAX( ZWIT(JJ) , 0.             )
    ZWIT(JJ)   = MIN( ZWIT(JJ) , ZWSAT_AVGZ(JJ)-XWGMIN)
!
!
!*       3.6    Add reservoir evolution from surface freezing (WI2 contains WI1)
!               ----------------------------------------------------------------
!
    ZWGI2(JJ)  = (1.-ZWORK1(JJ))*ZWIT(JJ) +  ZWORK1(JJ)*ZWGI1(JJ)
!
    PDWGI2(JJ) = ZWGI2(JJ) - PWGI(JJ,2)
!
!
!*       3.7    Effect on temperature
!               ---------------------
!
    PTG(JJ,2) = PTG(JJ,2) + PDWGI2(JJ)*XLMTT*PCG(JJ)*XRHOLW*PD_G(JJ,2)
!
ENDDO
!
IF (LHOOK) CALL DR_HOOK('ICE_SOILFR',1,ZHOOK_HANDLE)
!-------------------------------------------------------------------------------
END SUBROUTINE ICE_SOILFR











