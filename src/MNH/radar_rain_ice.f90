!MNH_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
!MNH_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!MNH_LIC for details. version 1.

!      ##########################
       MODULE MODI_RADAR_RAIN_ICE 
!      ##########################
!
INTERFACE
      SUBROUTINE RADAR_RAIN_ICE(PRT,PCIT,PRHODREF,PTEMP,PRARE,PVDOP,PRZDR,PRKDP)
!
REAL,  DIMENSION(:,:,:,:), INTENT(IN)  :: PRT  ! microphysical  mix. ratios at t
REAL,  DIMENSION(:,:,:),   INTENT(IN)  :: PCIT ! pristine ice concentration at t
REAL,  DIMENSION(:,:,:),   INTENT(IN)  :: PRHODREF ! density of the ref. state
REAL,  DIMENSION(:,:,:),   INTENT(IN)  :: PTEMP    ! air temperature
!
REAL,  DIMENSION(:,:,:), INTENT(OUT) :: PRARE! radar reflectivity in dBZ
REAL,  DIMENSION(:,:,:), INTENT(OUT) :: PVDOP! radar Doppler fall speed
REAL,  DIMENSION(:,:,:), INTENT(OUT) :: PRZDR! radar differential reflectivity
                                             ! H-V in dBZ
REAL,  DIMENSION(:,:,:), INTENT(OUT) :: PRKDP! radar differential phase shift
                                             ! H-V in degree/km
!
END SUBROUTINE RADAR_RAIN_ICE
!
END INTERFACE
!
END MODULE MODI_RADAR_RAIN_ICE
!     #########################################################################
      SUBROUTINE RADAR_RAIN_ICE(PRT,PCIT,PRHODREF,PTEMP,PRARE,PVDOP,PRZDR,PRKDP)
!     #########################################################################
!
!!****  *RADAR_RAIN_ICE * - computes some pertinent radar parameters
!!
!!    PURPOSE
!!    -------
!!      The purpose of this routine is to compute the equivalent reflectivity,
!!    the Doppler reflectivity and the H and V polarized reflectivities of a 
!!    mixed phase cloud.
!!
!!**  METHOD
!!    ------
!!      The reflectivities are computed using the n(D) * D**6 formula. The 
!!    equivalent reflectiviy is the sum of the reflectivity produced by the
!!    the raindrops and the equivalent reflectivities of the ice crystals.
!!    The latter are computed using the melted diameter. The Doppler 
!!    reflectivity is the 'fall speed'-moment of individual particle
!!    reflectivity. Ice crystal are assumed to have no preferred orientation.
!!    the Z_VV formula is taken from Brandes et al. (MWR, 1995).
!!
!!    EXTERNAL
!!    --------
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!      Module MODD_CST
!!        XPI                  !
!!        XRHOLW               ! Liquid water density
!!      Module MODD_RAIN_ICE_DESCR
!!      Module MODD_RAIN_ICE_PARAM
!!
!!    REFERENCE
!!    ---------
!!      Book2 of documentation ( routine RADAR_RAIN_ICE )
!!      Smith P.L., 1984: Equivalent Radar Reflectivity Factors for Snow and
!!                        Ice Particles, JCAM, 23, 1258-1260.
!!      Andsager K., K. V. Beard, and N. F. Laird, 1999: Laboratory Measurements
!!                        of Axis Ratio for Large Raindrops, JAS, 56, 2673-2683.
!!
!!    AUTHOR
!!    ------
!!      J.-P. Pinty      * Laboratoire d'Aerologie*
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    04/05/96
!!                  03/12/96 change the arg. list
!!                  15/12/00 change the reflectivity factor
!!                  01/07/02 (E.Richard) bug in reflectivity formula
!!                          for graupeln when warmer than 0°C
!!                  19/12/00 (JP Pinty) change the hailstone reflectivity
!!                  19/05/04 (JP Pinty) add ZDR and KDP for raindops at 10.71 cm
!! J.-P. Chaboureau 17/06/10 bug correction in reflectivity calculation of icy hydrometeors
!! J.-P. Chaboureau 03/02/12 set undef values for radar reflectivities
!!       O. Caumont 09/04/14 correction of ZDR calculation
!!
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
USE MODD_CST
USE MODD_REF
USE MODD_RAIN_ICE_DESCR
USE MODD_RAIN_ICE_PARAM
USE MODD_PARAMETERS
USE MODD_LUNIT
!
USE MODE_IO_ll
USE MODE_FM
!
IMPLICIT NONE
!
!*       0.1   Declarations of dummy arguments :
!
!
REAL,  DIMENSION(:,:,:,:), INTENT(IN)  :: PRT  ! microphysical  mix. ratios at t
REAL,  DIMENSION(:,:,:),   INTENT(IN)  :: PCIT ! pristine ice concentration at t
REAL,  DIMENSION(:,:,:),   INTENT(IN)  :: PRHODREF ! density of the ref. state
REAL,  DIMENSION(:,:,:),   INTENT(IN)  :: PTEMP    ! air temperature
!
REAL,  DIMENSION(:,:,:), INTENT(OUT) :: PRARE! radar reflectivity in dBZ
REAL,  DIMENSION(:,:,:), INTENT(OUT) :: PVDOP! radar Doppler fall speed
REAL,  DIMENSION(:,:,:), INTENT(OUT) :: PRZDR! radar differential reflectivity
                                             ! H-V in dBZ
REAL,  DIMENSION(:,:,:), INTENT(OUT) :: PRKDP! radar differential phase shift
                                             ! H-V in degree/km
!
!*       0.2   Declarations of local variables :
!
INTEGER :: IKB           ! Coordinates of the first physical points along z
INTEGER :: IND           ! Number of interval to integrate the kernels
REAL :: ZALPHA, ZNU, ZP  ! Parameters to compute the value of the p_moment
       			 ! of the generalized Gamma function
REAL :: ZDINFTY          ! Factor used to define the "infinite" diameter
!
REAL :: ZCXR=-1.0                     ! for rain N ~ 1/N_0 
                                      ! (in Kessler parameterization)
REAL :: ZSLOPE, ZINTERCEPT, ZEXPONENT ! parameters defining the mean axis ratio
       				      ! functionnal
REAL :: ZDMELT_FACT                   ! factor used to compute the equivalent
	                	      ! melted diameter
REAL :: ZEQICE                        ! factor used to convert the ice crystals
               			      ! reflectivity into an equivalent  liquid
		                      ! water reflectivity (from Smith, JCAM 84)
REAL :: ZEXP                          ! anciliary parameter
REAL :: ZRHO00                        ! Surface reference air density
!
LOGICAL, DIMENSION(SIZE(PTEMP,1),SIZE(PTEMP,2),SIZE(PTEMP,3)) :: GRAIN
REAL,    DIMENSION(SIZE(PTEMP,1),SIZE(PTEMP,2),SIZE(PTEMP,3)) :: ZLBDA 
                                      ! slope distribution parameter
REAL,    DIMENSION(SIZE(PTEMP,1),SIZE(PTEMP,2),SIZE(PTEMP,3)) :: ZW
REAL,    DIMENSION(SIZE(PTEMP,1),SIZE(PTEMP,2),SIZE(PTEMP,3)) :: ZREFL_MELT_CONV
INTEGER                                                       :: JLBDA
REAL                                                          :: ZFRAC_WATER
!
INTEGER  :: ILUOUT0 ! Logical unit number for output-listing
INTEGER  :: IRESP   ! Return code of FM-routines
LOGICAL  :: GFLAG   ! Logical flag for printing the constatnts on the output
                    ! listing
!
REAL     ::   ZR0, ZR1, ZR2 ! r(D) parameters
!REAL     ::   ZREXP, ZSCALE ! parameters to compute Zhh from Zvv
REAL     ::   Z1, Z2, Z3    ! expansion coefficients
! 
INTEGER  :: II, IJ, IK 
!-------------------------------------------------------------------------------
!
!
!*       1.     FUNCTION STATEMENTS
!   	        -------------------
!
!
!*       1.1    p_moment of the Generalized GAMMA function
!
! Recall that MOMG(ZALPHA,ZNU,ZP)=GAMMA(ZNU+ZP/ZALPHA)/GAMMA(ZNU)
!
!
!-------------------------------------------------------------------------------
!
!
!        2.     INTIALIZE OUTPUT LISTING AND OTHER ARRAYS
!               -----------------------------------------
!
!
CALL FMLOOK_ll(CLUOUT0,CLUOUT0,ILUOUT0,IRESP)
!
PRARE(:,:,:) = 0.0    ! radar reflectivity
PVDOP(:,:,:) = 0.0    ! radar Doppler fall speed
PRZDR(:,:,:) = 0.0    ! radar differential reflectivity
PRKDP(:,:,:) = 0.0    ! radar differential phase shift
!
!-------------------------------------------------------------------------------
!
!
!*       3.     RAINDROPS
!               ---------
!
IF (SIZE(PRT,4) >= 3) THEN
  IND        = 50
  ZSLOPE     = 0.62     ! the mean axis ratio function writes as r**ZEXPONENT
  ZINTERCEPT = 1.03     !                  with
  ZEXPONENT  = 7.0/3.0  ! r = ZSLOPE*D+ZINTERCEPT where D is the drop diameter
  ZDINFTY    = 20.0
!
! The raindrop aspect ratio is given by Andsager et al. (1999)
!   r(D) = ZR0 + ZR1*D + ZR2*D**2
!
  ZR0 = 1.012
  ZR1 = -0.144E2
  ZR2 = -1.03E4
!
!  ZREXP  = 7.0/3.0
!  ZSCALE = ZR0**ZREXP
!  Z1     = ZREXP*(ZR1/ZR0)
!  Z2     = ZREXP*(ZR2/ZR0)+ZREXP*(ZREXP-1.0)*0.5*(ZR1/ZR0)**2
  Z1=.97
  Z2=.64
  Z3=7.8
!
  ZLBDA(:,:,:) = 0.0
  IF( SIZE(PRT,4) == 3 ) THEN
    GRAIN(:,:,:) = PRT(:,:,:,3).GT.1.0E-15
    XCCR = 1.E7; XLBEXR = -0.25          ! Marshall-Palmer law
    XALPHAR = 1.0; XNUR = 1.0            ! Marshall-Palmer law
    XCR = 842.; XDR = 0.8                ! Raindrop fall-speed
    XLBR = (XPI*XRHOLW*XCCR)**(-XLBEXR)
  ELSE
    GRAIN(:,:,:) = PRT(:,:,:,3).GT.XRTMIN(3)
  END IF
  WHERE( GRAIN(:,:,:) )
    ZLBDA(:,:,:) = XLBR*( PRHODREF(:,:,:)*PRT(:,:,:,3) )**XLBEXR
    PRARE(:,:,:) = 1.E18*XCCR*(ZLBDA(:,:,:)**(ZCXR-6.0))*MOMG(XALPHAR,XNUR,6.0)
    PVDOP(:,:,:) = 1.E18*XCCR*XCR*(ZLBDA(:,:,:)**(ZCXR-6.0-XDR))              &
    *MOMG(XALPHAR,XNUR,6.0+XDR)
!    PRZDR(:,:,:) = ZSCALE*(1.0                                                 &
!      +Z1*(MOMG(XALPHAR,XNUR,7.0)/MOMG(XALPHAR,XNUR,6.0))*(1.0/ZLBDA(:,:,:))   &
!      +Z2*(MOMG(XALPHAR,XNUR,8.0)/MOMG(XALPHAR,XNUR,6.0))*(1.0/ZLBDA(:,:,:)**2))
    PRZDR(:,:,:) = Z1+Z2*(PRHODREF(:,:,:)*PRT(:,:,:,3))**(-XLBEXR)+Z3*(PRHODREF(:,:,:)*PRT(:,:,:,3))**(-2.*XLBEXR)
!    PRZDR(:,:,:) = -10.0*LOG10( PRZDR(:,:,:) ) ! now in dBZ
    PRZDR(:,:,:) = 10.0*LOG10( PRZDR(:,:,:) ) ! now in dBZ
    PRKDP(:,:,:) = 6.7E3*( PRHODREF(:,:,:)*PRT(:,:,:,3) )*                     &
    (-ZR1*(MOMG(XALPHAR,XNUR,4.0)/MOMG(XALPHAR,XNUR,3.0))*(1.0/ZLBDA(:,:,:))   &
     -ZR2*(MOMG(XALPHAR,XNUR,5.0)/MOMG(XALPHAR,XNUR,3.0))*(1.0/ZLBDA(:,:,:)**2))
                                              ! in degree/km
  END WHERE
END IF
!
!*       4.     PRISTINE ICE
!               ------------
!
IF (SIZE(PRT,4) >= 4) THEN
  ZEQICE = 0.224
  ZDMELT_FACT = ( (6.0*XAI)/(XPI*XRHOLW) )**(2.0)
  ZEXP = 2.0*XBI
  WHERE( PRT(:,:,:,4).GT.XRTMIN(4) .AND. PCIT(:,:,:).GT.0.0 )
    ZLBDA(:,:,:) = XLBI*( PRHODREF(:,:,:)*PRT(:,:,:,4)/PCIT(:,:,:) )**XLBEXI
    ZW(:,:,:) = ZEQICE*ZDMELT_FACT                                             &
	    *1.E18*PCIT(:,:,:)*(ZLBDA(:,:,:)**(-ZEXP))*MOMG(XALPHAI,XNUI,ZEXP)
    PVDOP(:,:,:) = PVDOP(:,:,:)+ZEQICE*ZDMELT_FACT*MOMG(XALPHAI,XNUI,ZEXP+XDI) &
  	                    *1.E18*PCIT(:,:,:)*XC_I*(ZLBDA(:,:,:)**(-ZEXP-XDI))
    PRARE(:,:,:) = PRARE(:,:,:) + ZW(:,:,:)
  END WHERE
END IF
!
!*       5.     SNOW/AGGREGATES
!               ---------------
!
IF (SIZE(PRT,4) >= 5) THEN
  ZDMELT_FACT = ( (6.0*XAS)/(XPI*XRHOLW) )**(2.0)
  ZEXP = 2.0*XBS
  WHERE( PRT(:,:,:,5).GT.XRTMIN(5) )
    ZLBDA(:,:,:) = XLBS*( PRHODREF(:,:,:)*PRT(:,:,:,5) )**XLBEXS
    ZW(:,:,:) = ZEQICE*ZDMELT_FACT                                             &
           *1.E18*XCCS*(ZLBDA(:,:,:)**(XCXS-ZEXP))*MOMG(XALPHAS,XNUS,ZEXP)
    PVDOP(:,:,:) = PVDOP(:,:,:)+ZEQICE*ZDMELT_FACT*MOMG(XALPHAS,XNUS,ZEXP+XDS) &
      	   *1.E18*XCCS*XCS*(ZLBDA(:,:,:)**(XCXS-ZEXP-XDS))
    PRARE(:,:,:) = PRARE(:,:,:) + ZW(:,:,:)
  END WHERE
END IF
!
!*       6.     GRAUPELN
!               --------
!
IF (SIZE(PRT,4) >= 6) THEN
  ZFRAC_WATER = 0.14
  ZDMELT_FACT = ( (6.0*XAG)/(XPI*XRHOLW) )**(2.0)
  WHERE( PTEMP(:,:,:).GT.XTT )
    ZREFL_MELT_CONV(:,:,:) = ((1.0-ZFRAC_WATER)*ZEQICE+ZFRAC_WATER)*ZDMELT_FACT
  ELSEWHERE
    ZREFL_MELT_CONV(:,:,:) = ZEQICE*ZDMELT_FACT
  END WHERE
!
  ZEXP = 2.0*XBG
  WHERE( PRT(:,:,:,6).GT.XRTMIN(6) )
    ZLBDA(:,:,:) = XLBG*( PRHODREF(:,:,:)*PRT(:,:,:,6) )**XLBEXG
    ZW(:,:,:)    = ZREFL_MELT_CONV(:,:,:)*1.E18*XCCG*                &
                   (ZLBDA(:,:,:)**(XCXG-ZEXP))*MOMG(XALPHAG,XNUG,ZEXP)
    PVDOP(:,:,:) = PVDOP(:,:,:) +                                            &
                   ZREFL_MELT_CONV(:,:,:)*1.E18*XCCG*XCG*                    &
                   (ZLBDA(:,:,:)**(XCXG-ZEXP-XDG))*MOMG(XALPHAG,XNUG,ZEXP+XDG)
    PRARE(:,:,:) = PRARE(:,:,:) + ZW(:,:,:)
  END WHERE
END IF
!
!*       7.     HAILSTONES
!               ----------
!
IF (SIZE(PRT,4) >= 7) THEN
  ZFRAC_WATER = 1.
  ZDMELT_FACT = ( (6.0*XAH)/(XPI*XRHOLW) )**(2.0)
  ZREFL_MELT_CONV(:,:,:) = ((1.0-ZFRAC_WATER)*ZEQICE+ZFRAC_WATER)*ZDMELT_FACT
!
  ZEXP = 2.0*XBH
  WHERE( PRT(:,:,:,7).GT.XRTMIN(7) )
    ZLBDA(:,:,:) = XLBH*( PRHODREF(:,:,:)*PRT(:,:,:,7) )**XLBEXH
    ZW(:,:,:)    = ZREFL_MELT_CONV(:,:,:)*1.E18*XCCH*                &
                   (ZLBDA(:,:,:)**(XCXH-ZEXP))*MOMG(XALPHAH,XNUH,ZEXP)
    PVDOP(:,:,:) = PVDOP(:,:,:) +                                            &
                   ZREFL_MELT_CONV(:,:,:)*1.E18*XCCH*XCH*                    &
                   (ZLBDA(:,:,:)**(XCXH-ZEXP-XDH))*MOMG(XALPHAH,XNUH,ZEXP+XDH)
    PRARE(:,:,:) = PRARE(:,:,:) + ZW(:,:,:)
  END WHERE
END IF
!
!*       8.     UNIT CONVERSION
!               ---------------
!
IKB = 1 + JPVEXT
ZRHO00 = XP00/(XRD*XTHVREFZ(IKB))
WHERE( PRARE(:,:,:) >= 1.0 )
  PVDOP(:,:,:) = PVDOP(:,:,:)/PRARE(:,:,:)  ! Doppler speed normalization in m/s
  PVDOP(:,:,:) = PVDOP(:,:,:)*(ZRHO00/PRHODREF(:,:,:))**0.4
                                            ! air density correction
ELSEWHERE
  PVDOP(:,:,:) = 0.0
END WHERE
!
! MODIF FP FEB 2012
!WHERE( PRARE(:,:,:) > 0.0 )
WHERE( PRARE(:,:,:) > 1.E-3 )
! END MODIF
  PRARE(:,:,:) = 10.0*LOG10( PRARE(:,:,:) ) ! Z_equiv in dBZ
ELSEWHERE
  PRARE(:,:,:) = XUNDEF
END WHERE
!
!-------------------------------------------------------------------------------
!
CONTAINS
!
  FUNCTION MOMG (PALPHA,PNU,PP) RESULT (PMOMG)
!
! auxiliary routine used to compute the Pth moment order of the generalized
! gamma law
!
  USE MODI_GAMMA
!
  IMPLICIT NONE
!
  REAL     :: PALPHA ! first shape parameter of the dimensionnal distribution
  REAL     :: PNU    ! second shape parameter of the dimensionnal distribution
  REAL     :: PP     ! order of the moment
  REAL     :: PMOMG  ! result: moment of order ZP
!
!------------------------------------------------------------------------------
!
!
  PMOMG = GAMMA(PNU+PP/PALPHA)/GAMMA(PNU)
!
  END FUNCTION MOMG
!
!------------------------------------------------------------------------------
!
END SUBROUTINE RADAR_RAIN_ICE
