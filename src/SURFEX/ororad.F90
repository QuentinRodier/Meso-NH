!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #################################################################################
SUBROUTINE ORORAD(USS,PZENITH,PAZIM,PSCA_ALB,PTRAD,PEMIS,PDIR_SWL,PSCA_SWL,PLWL)
!     #################################################################################
!
!
!!****  *ORORAD *  
!!
!!    PURPOSE
!!    -------
!
!!**  METHOD
!!    ------
!!
!!    REFERENCE
!!    ---------
!!      
!!
!!    AUTHOR
!!    ------
!!     A. Mary
!!
USE MODD_SSO_n, ONLY : SSO_t
!
USE MODD_SURF_PAR, ONLY : XUNDEF
USE MODD_CSTS,  ONLY : XPI, XSTEFAN
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!IN - OUT VARIABLES
TYPE(SSO_t), INTENT(INOUT)          :: USS
REAL, DIMENSION(:), INTENT(IN)      :: PZENITH   ! zenithal angle at t  (radian from the vertical)
REAL, DIMENSION(:), INTENT(IN)      :: PAZIM     ! azimuthal angle      (radian from North, clockwise)
REAL, DIMENSION(:,:),INTENT(IN)     :: PSCA_ALB  ! diffuse albedo for each spectral band (-)
REAL, DIMENSION(:), INTENT(IN)      :: PTRAD
REAL, DIMENSION(:), INTENT(IN)      :: PEMIS
REAL, DIMENSION(:,:), INTENT(INOUT) :: PDIR_SWL !direct short wave radiation
REAL, DIMENSION(:,:), INTENT(INOUT) :: PSCA_SWL !diffuse short wave radiation
REAL, DIMENSION(:), INTENT(INOUT)   :: PLWL     !downwelling long wave radiation

!LOCAL VARIABLES
REAL, DIMENSION(SIZE(PTRAD),SIZE(PDIR_SWL,2)) :: DSV, DSL, DSH   !sky view, slope and shadow factor (for output)
!
REAL                             :: PI_SEC1, PI_SEC2, RIND !for calculation of center point in each sector
REAL                             :: SCOS, SSIN             !cos and sin of zenith angle
REAL                             :: SSWDIR, SSWDIF         !direct and diffuse short wave radiation
REAL                             :: SAZIM                  !Solar azimuth
REAL                             :: DSLOPFRAC, ALFRAC      !number of sectors, sum over all secotrs
REAL                             :: DSL1, ALB1             !slope factor in section, albedo
REAL                             :: DSLOP                  !slope factor
REAL                             :: DSHAD                  !shadow factor
REAL                             :: SLWUP                  !longwave upward radiation
REAL                             :: SLWDN                  !longwave downward radiation
!
INTEGER,DIMENSION(USS%NSECTORS)  :: TAB                    !center points of the sectors
!
INTEGER                          :: SI, SK, SL             !loop variables
INTEGER                          :: SLI, IAZI              !counter for definition of section
INTEGER :: INI,ISW
!
LOGICAL                          :: LFOUND                 !logical switch for definition of section
!
REAL(KIND=JPRB)                  :: ZHOOK_HANDLE
!
IF (LHOOK) CALL DR_HOOK('ORORAD',0,ZHOOK_HANDLE)
!
INI = SIZE(PTRAD)
ISW = SIZE(PDIR_SWL,2)
!
PI_SEC1 = XPI/REAL(USS%NSECTORS)
PI_SEC2 = PI_SEC1 * 2.

DO SK=1,USS%NSECTORS          !calculating the center of the sectors
  RIND = REAL(SK - 1.)
  TAB(SK) = NINT(1000. * (RIND * PI_SEC2 + PI_SEC1))
ENDDO

DO SL=1,INI                  !loop over grid points

  DSV(SL,:) = 1.
  DSL(SL,:) = 1.
  DSH(SL,:) = 1.

  !only if all orographic shadowing parameters are available
  IF((USS%XAVG_SLO(SL)<XUNDEF).AND.(USS%XSVF(SL)<XUNDEF).AND.(USS%XFRAC_DIR(SL,1)<XUNDEF).AND. &
    (USS%XSLOPE_DIR(SL,1)<XUNDEF).AND.(USS%XSHA_DIR(SL,1)<XUNDEF).AND.(USS%XSHB_DIR(SL,1)<XUNDEF))THEN

    DO SI=1,ISW                !loop over spectral bands

      IF ((PDIR_SWL(SL,SI) >= 0.) .AND. (PSCA_SWL(SL,SI) >= 0.) .AND. &
        (PDIR_SWL(SL,SI) < XUNDEF) .AND. (PSCA_SWL(SL,SI) < XUNDEF))THEN

        !only if there is radiation (no default)
        SCOS = MIN(MAX(COS(PZENITH(SL)),1.E-12),1.0-1.E-12)
        !cos of solar zenith angle
        SAZIM = XPI-PAZIM(SL)
        IF(SAZIM < 0.)SAZIM=SAZIM+2*XPI
        SSWDIR = PDIR_SWL(SL,SI)
        SSWDIF = PSCA_SWL(SL,SI)
!--------------------------------------------------------------------------
! 1. DIFFUSE AND REFLECTED SOLAR RADIATION
!--------------------------------------------------------------------------
!
        IF (USS%LDSV )THEN
          ALB1 = PSCA_ALB(SL,SI)
          IF ((ALB1 >= 0.07) .AND. (ALB1 <= 1.0))THEN
            IF (USS%XSVF(SL) > 0.01 .AND. USS%XSVF(SL) <= 1.) DSV(SL,SI) = USS%XSVF(SL)
            SSWDIF = DSV(SL,SI) * SSWDIF + ALB1 * (1.0 - DSV(SL,SI)) *&
                      &(SSWDIR + SSWDIF)
          ENDIF
        ENDIF
!
!--------------------------------------------------------------------------
! 2. CORRECTION OF SOLAR DIRECT RADIATION FLUX
!--------------------------------------------------------------------------
!
        ! SLOPE CORRECTION
        SSIN = SQRT(1.0 - SCOS * SCOS)        ! sinus of solar zenith angle
        ALFRAC = 0.
        DSLOPFRAC = 0.
        IF (USS%LDSL)THEN
          DO SK=1,USS%NSECTORS                             ! loop over sections
            DSL1 = 1.0 + TAN(USS%XSLOPE_DIR(SL,SK)) * SSIN / SCOS &
            * COS(SAZIM - 2.*XPI / USS%NSECTORS * REAL(SK - 1.))
            ALFRAC = ALFRAC + USS%XFRAC_DIR(SL,SK)
            IF (DSL1 > 0.) DSLOPFRAC = DSLOPFRAC + DSL1 * USS%XFRAC_DIR(SL,SK)
          ENDDO
        ENDIF
        DSL(SL,SI) = DSLOPFRAC + 1 - ALFRAC

        ! CAST SHADOW, DETERMINE THE VALUE OF SHADOW MASK
        DSHAD = 1.0
        IF ( USS%LDSH )THEN
          DSHAD = USS%XSHA_DIR(SL,1) * SCOS + USS%XSHB_DIR(SL,1)
          LFOUND = .FALSE.
          SLI = 2
          IAZI = NINT(SAZIM * 1000.)
          DO WHILE(.NOT. LFOUND .AND. SLI <= USS%NSECTORS)
            IF ((IAZI >= TAB(SLI - 1)) .AND. (IAZI < TAB(SLI)))THEN
               DSHAD = USS%XSHA_DIR(SL,SLI) * SCOS + USS%XSHB_DIR(SL,SLI)
               LFOUND = .TRUE.
            ENDIF
            SLI = SLI + 1
          ENDDO
          IF (DSHAD > 0.001) DSH(SL,SI) = DSHAD
          IF (DSHAD > 1.000) DSH(SL,SI) = 1.0
          IF (DSHAD < 0.000) DSH(SL,SI) = 0.0
        ENDIF

        SSWDIR = SSWDIR * DSL(SL,SI) * DSH(SL,SI)
!--------------------------------------------------------------------------
! 3. GLOBAL AND NET SOLAR RADIATION
!--------------------------------------------------------------------------
!      
        PSCA_SWL(SL,SI) = SSWDIF
        PDIR_SWL(SL,SI) = SSWDIR
      ENDIF
!
!--------------------------------------------------------------------------
! 4. LONGWAVE RADIATION
!--------------------------------------------------------------------------
!
      IF ((PLWL(SL) >= 0.) .AND. (PLWL(SL) < 2000.)) THEN
        !only if there is radiation (no default)
        IF (USS%LDSV )THEN
           
          IF (USS%XSVF(SL) > 0.01 .AND. USS%XSVF(SL) <= 1.) DSV(SL,SI) = USS%XSVF(SL)
          SLWDN = PLWL(SL)
          SLWUP = PEMIS(SL)*XSTEFAN*PTRAD(SL)**4
          SLWDN = DSV(SL,SI) * SLWDN + (1.0 - DSV(SL,SI)) * SLWUP
          PLWL(SL) = SLWDN
        ENDIF
      ENDIF
!--------------------------------------------------------------------------
! 5. DIAGNOSTIC OUTPUT OF APPLIED FACTORS
!--------------------------------------------------------------------------
!
    ENDDO
  ENDIF
ENDDO
!
IF (LHOOK) CALL DR_HOOK('ORORAD',1,ZHOOK_HANDLE)
!
END SUBROUTINE ORORAD

