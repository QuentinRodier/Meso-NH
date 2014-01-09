!MNH_LIC Copyright 1994-2013 CNRS, Meteo-France and Universite Paul Sabatier
!MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
!MNH_LIC version 1. See LICENCE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!MNH_LIC for details. version 1.
!-----------------------------------------------------------------
!--------------- special set of characters for RCS information
!-----------------------------------------------------------------
! $Source$ $Revision$ $Date$
!-----------------------------------------------------------------
!     ######spl
       MODULE MODI_RADAR_SCATTERING 
!      #############################
!
INTERFACE
    SUBROUTINE RADAR_SCATTERING(PT_RAY,PRHODREF_RAY,PR_RAY,PI_RAY,PCIT_RAY,PS_RAY,PG_RAY,PVDOP_RAY, &
   PELEV,PX_H,PX_V,PW_H,PW_V,PZE,PBU_MASK_RAY)
REAL, DIMENSION(:,:,:,:,:,:),INTENT(IN)  :: PT_RAY ! temperature interpolated along the rays
REAL, DIMENSION(:,:,:,:,:,:),INTENT(IN)  :: PRHODREF_RAY ! 
REAL, DIMENSION(:,:,:,:,:,:),INTENT(IN)  :: PR_RAY  ! rainwater mixing ratio interpolated along the rays
REAL, DIMENSION(:,:,:,:,:,:),INTENT(IN)  :: PI_RAY  ! pristine ice mixing ratio interpolated along the rays
REAL, DIMENSION(:,:,:,:,:,:), INTENT(IN) :: PCIT_RAY  ! pristine ice concentration interpolated along the rays
REAL, DIMENSION(:,:,:,:,:,:), INTENT(IN) :: PS_RAY !aggregates mixing ratio interpolated along the rays
REAL, DIMENSION(:,:,:,:,:,:), INTENT(IN) :: PG_RAY  ! graupel         mixing ratio interpolated along the rays
REAL, DIMENSION(:,:,:,:,:,:), INTENT(IN) :: PVDOP_RAY !Doppler radial velocity interpolated along the rays
REAL, DIMENSION(:,:,:,:),     INTENT(IN) :: PELEV ! elevation
REAL, DIMENSION(:),           INTENT(IN) :: PX_H ! Gaussian horizontal nodes
REAL, DIMENSION(:),           INTENT(IN) :: PX_V ! Gaussian vertical nodes
REAL, DIMENSION(:),           INTENT(IN) :: PW_H ! Gaussian horizontal weights
REAL, DIMENSION(:),           INTENT(IN) :: PW_V ! Gaussian vertical weights
REAL,DIMENSION(:,:,:,:,:),    INTENT(INOUT) :: PZE ! gate equivalent reflectivity factor (horizontal)
! convective/stratiform
REAL, DIMENSION(:,:,:,:,:,:),INTENT(INOUT) :: PBU_MASK_RAY
! /convective/stratiform
    END SUBROUTINE RADAR_SCATTERING
END INTERFACE
END MODULE MODI_RADAR_SCATTERING
!
!     ######spl
       SUBROUTINE RADAR_SCATTERING(PT_RAY,PRHODREF_RAY,PR_RAY,PI_RAY,PCIT_RAY, &
            PS_RAY,PG_RAY,PVDOP_RAY,PELEV,PX_H,PX_V,PW_H,PW_V,PZE,PBU_MASK_RAY)
!     ##############################
!
!!****  *RADAR_SCATTERING* - computes radar reflectivities.
!!
!!    PURPOSE
!!    -------
!!      Compute equivalent reflectivities of a mixed phase cloud.
!!
!!**  METHOD
!!    ------
!!      The reflectivities are computed using the n(D) * sigma(D) formula. The 
!!    equivalent reflectiviy is the sum of the reflectivity produced by the
!!    the raindrops and the equivalent reflectivities of the ice crystals.
!!    The latter are computed using the mass-equivalent diameter.
!!    Four types of diffusion are possible : Rayleigh, Mie, T-matrix, and
!!    Rayleigh-Gans (Kerker, 1969, Chap. 10; Battan, 1973, Sec. 5.4; van de
!!    Hulst, 1981, Sec. 6.32; Doviak and Zrnic, 1993, p. 249; Bringi and 
!!    Chandrasekar, 2001, Chap. 2).
!!    The integration over diameters for Mie and T-matrix methods is done by
!!    using Gauss-Laguerre quadrature (Press et al. 1986). Attenuation is taken
!!    into account by computing the extinction efficiency and correcting 
!!    reflectivities along the beam path.
!!    Gaussian quadrature methods are used to model the beam broadening (Gauss-
!!    Hermite or Gauss-Legendre, see Press et al. 1986).
!!      
!!
!!    EXTERNAL
!!    --------
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!      Module MODD_CST
!!        XLIGHTSPEED
!!        XPI
!!      Module MODD_ARF
!!
!!    REFERENCE
!!    ---------
!!      Press, W. H., B. P. Flannery, S. A. Teukolsky et W. T. Vetterling, 1986: 
!!    Numerical Recipes: The Art of Scientific Computing. Cambridge University 
!!    Press, 818 pp.
!!      Probert-Jones, J. R., 1962 : The radar equation in meteorology. Quart. 
!!    J. Roy. Meteor. Soc., 88, 485-495.
!!
!!    AUTHOR
!!    ------
!!      O. Caumont & V. Ducrocq      * Météo-France *
!!
!!    MODIFICATIONS
!!    -------------
!!      Original   26/03/2004 
!!      O. Caumont 09/09/2009 minor changes to compute radial velocities when no
!!                              hydrometeors so as to emulate wind lidar
!!      O. Caumont 21/12/2009 correction of bugs to compute KDP.
!!      O. Caumont 11/02/2010 thresholding and conversion from linear to 
!!          log values after interpolation instead of before.
!!      G.Tanguy 25/03/2010 Introduction of MODD_TMAT and ALLOCATE/DEALLOCATE 
!!
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
USE MODD_CST
USE MODD_PARAMETERS
USE MODD_RAIN_ICE_DESCR
USE MODD_RAIN_ICE_PARAM
USE MODD_RADAR, ONLY:XLAM_RAD,XSTEP_RAD,NBELEV,NDIFF,LATT,NPTS_GAULAG,LQUAD,XVALGROUND,NDGS, &
     LFALL,LWBSCS,LWREFL,XREFLVDOPMIN
USE MODD_TMAT
! 
USE MODE_ARF
USE MODE_FSCATTER
USE MODE_FGAU , ONLY:GAULAG
USE MODI_GAMMA, ONLY:GAMMA
!
IMPLICIT NONE
!
!*       0.1   Declarations of dummy arguments :
!
!
REAL,DIMENSION(:,:,:,:,:,:), INTENT(IN) :: PT_RAY ! temperature interpolated along the rays
REAL,DIMENSION(:,:,:,:,:,:), INTENT(IN) :: PRHODREF_RAY ! 
REAL,DIMENSION(:,:,:,:,:,:), INTENT(IN) :: PR_RAY   ! rainwater mixing ratio interpolated along the rays
REAL,DIMENSION(:,:,:,:,:,:), INTENT(IN) :: PI_RAY   ! pristine ice mixing ratio interpolated along the rays
REAL,DIMENSION(:,:,:,:,:,:), INTENT(IN) :: PCIT_RAY !pristine ice concentration interpolated along the rays
REAL,DIMENSION(:,:,:,:,:,:), INTENT(IN) :: PS_RAY !aggregates mixing ratio interpolated along the rays
REAL,DIMENSION(:,:,:,:,:,:), INTENT(IN) :: PG_RAY   ! graupel mixing ratio interpolated along the rays
REAL,DIMENSION(:,:,:,:,:,:), INTENT(IN) :: PVDOP_RAY !Doppler radial velocity interpolated along the rays
REAL,DIMENSION(:,:,:,:),     INTENT(IN) :: PELEV ! elevation
REAL,DIMENSION(:),           INTENT(IN) :: PX_H ! Gaussian horizontal nodes
REAL,DIMENSION(:),           INTENT(IN) :: PX_V ! Gaussian vertical nodes
REAL,DIMENSION(:),           INTENT(IN) :: PW_H ! Gaussian horizontal weights
REAL,DIMENSION(:),           INTENT(IN) :: PW_V ! Gaussian vertical weights
REAL,DIMENSION(:,:,:,:,:),   INTENT(INOUT) :: PZE ! gate equivalent reflectivity factor (horizontal & vertical)
! convective/stratiform
REAL,DIMENSION(:,:,:,:,:,:),INTENT(INOUT) :: PBU_MASK_RAY
! /convective/stratiform
!
!*       0.2   Declarations of local variables :
!
REAL,   DIMENSION(:,:,:,:,:,:,:),ALLOCATABLE :: ZREFL! 1: radar reflectivity in dBZ, 2: ZDR, 3: KDP, 4: BU_MASK, 5-8: mixing ratios, 9-12: Z_j, 13: CIT, 14: height above ground, 15-18: specific attenuations, 19-22: total attenuations
REAL,   DIMENSION(:,:,:,:,:,:,:),ALLOCATABLE :: ZAELOC ! local attenuation
REAL,   DIMENSION(:,:,:),ALLOCATABLE :: ZAETOT ! 1: total attenuation, 2: // vertical
REAL :: ZAERINT,ZAEIINT,ZAESINT,ZAEGINT ! 1-4: total A_i
!
REAL,DIMENSION(:),ALLOCATABLE :: ZX,ZW ! Gauss-Laguerre points and weights
!
REAL,DIMENSION(4) :: ZREFLOC
REAL,DIMENSION(2) :: ZAETMP
REAL,DIMENSION(:),ALLOCATABLE :: ZVTEMP ! temp var for Gaussian quadrature 8 : r_r, 9 : r_i, 10 : r_s , 11 : r_g
REAL :: ZCXR=-1.0   ! for rain N ~ 1/N_0 (in Kessler parameterization)
REAL :: ZDMELT_FACT ! factor used to compute the equivalent melted diameter
REAL :: ZEQICE=0.224! factor used to convert the ice crystals reflectivity into an equivalent  liquid water reflectivity (from Smith, JCAM 84)
REAL :: ZEXP        ! anciliary parameter
REAL :: ZLBDA   ! slope distribution parameter
REAL :: ZFRAC_ICE,ZD,ZDE ! auxiliary variables
REAL :: ZQSCA
REAL,DIMENSION(2) :: ZQEXT
REAL,DIMENSION(3) :: ZQBACK ! Q_b(HH),Q_b(VV) (backscattering efficiencies at horizontal and vertical polarizations, resp.)
COMPLEX  :: QM,QMW,QMI,QK,QB ! dielectric parameters
!
INTEGER  :: INBRAD,IIELV,INBAZIM,INBSTEPMAX,INPTS_H,INPTS_V ! sizes of the arrays
INTEGER  :: IEL
INTEGER  :: JI,JL,JEL,JAZ,JH,JV,JJ ! Loop variables of control
REAL :: ZLB ! depolarization factor along the spheroid symmetry axis
REAL :: XCXI ! should be defined with other parameters of microphysical scheme
REAL :: ZCR=0.,ZCI=0.,ZCS=0.,ZCG=0. ! coefficients to take into account fall speeds when simulating Doppler winds
REAL, DIMENSION(:,:,:,:),ALLOCATABLE :: ZCONC_BIN
INTEGER :: IVDOP,IMAX
LOGICAL :: LPART_MASK ! indicates a partial mask along the beam
INTEGER,PARAMETER :: IZER=5,IZEI=6,IZES=7,IZEG=8, IAER=10,IAEI=11,IAES=12,IAEG=13, IATR=14,IATI=15,IATS=16,IATG=17
!-------------------------------------------------------------------------------
!
!
!*       1.     INITIALISATION 
!   	        --------------
INBRAD=SIZE(PT_RAY,1)
IIELV=SIZE(PT_RAY,2)
INBAZIM=SIZE(PT_RAY,3)
INBSTEPMAX=SIZE(PT_RAY,4)
INPTS_H=SIZE(PT_RAY,5)
INPTS_V=SIZE(PT_RAY,6)
!
! Initialisation for radial winds
IF(LFALL) THEN
   ZCR=XCR
   ZCI=XC_I
   ZCS=XCS
   ZCG=XCG
END IF

IF(NDIFF/=0) THEN
   ALLOCATE(ZX(NPTS_GAULAG),ZW(NPTS_GAULAG))
   CALL GAULAG(NPTS_GAULAG,ZX,ZW) ! for Mie and T-matrix and RG
END IF
!
IVDOP=9
IMAX=SIZE(PZE,5)
IF(.NOT.LWREFL) IMAX=IMAX+1
ALLOCATE(ZREFL(INBRAD,IIELV,INBAZIM,INBSTEPMAX,INPTS_H,INPTS_V,IMAX))
ZREFL(:,:,:,:,:,:,:)=0.
IF(LATT) THEN
   ZREFL(:,:,:,:,:,:,IATR:IATG)=1.
END IF
PZE(:,:,:,:,:)=0.
IF (LATT)THEN
   ALLOCATE(ZAELOC(INBRAD,IIELV,INBAZIM,INBSTEPMAX,INPTS_H,INPTS_V,2))
   ALLOCATE(ZAETOT(INPTS_H,INPTS_V,2))
   ZAELOC(:,:,:,:,:,:,:)=0. ! initialization of attenuation stuff (alpha_e for first gate)
   ZAETOT(:,:,:)=1. ! initialization of attenuation stuff (total attenuation)
END IF
WRITE(0,*) 'BEFORE LOOP DIFFUSION'

IF(LWBSCS) THEN
   ALLOCATE(ZCONC_BIN(INBRAD,IIELV,INBAZIM,INBSTEPMAX))
   ZCONC_BIN(:,:,:,:)=0.
END IF

! LOOP OVER EVERYTHING
DO JI=1,INBRAD  
   IEL=NBELEV(JI)
   DO JEL=1,IEL  
      DO JAZ=1,INBAZIM 
         DO JH=1,INPTS_H
            DO JV=1,INPTS_V ! we go down to check partial masks
               IF(LATT) THEN
                  ZAERINT=1.
                  ZAEIINT=1.
                  ZAESINT=1.
                  ZAEGINT=1.
               END IF
               LPART_MASK=.FALSE.
               LOOPJL: DO JL=1,INBSTEPMAX
                  ! REINDENTING FOR READIBILITY
IF(LPART_MASK) THEN ! THIS RAY IS MASKED
   ZREFL(JI,JEL,JAZ,JL:INBSTEPMAX,JH,JV,1)=0. 
   EXIT LOOPJL
ELSE
   ! if not underground or outside of the MESO-NH domain and rain 
   IF(PT_RAY(JI,JEL,JAZ,JL,JH,JV) /= -XUNDEF) THEN
!
!---------------------------------------------------------------------------------------------------
!*       2.    RAINDROPS
!              ---------
!
      IF(SIZE(PR_RAY,1) > 0) THEN
         IF(PR_RAY(JI,JEL,JAZ,JL,JH,JV) > XRTMIN(3)) THEN  
            QMW=SQRT(QEPSW(PT_RAY(JI,JEL,JAZ,JL,JH,JV),XLIGHTSPEED/XLAM_RAD(JI)))
            ZLBDA=XLBR*(PRHODREF_RAY(JI,JEL,JAZ,JL,JH,JV)*PR_RAY(JI,JEL,JAZ,JL,JH,JV))**XLBEXR
            !                        ZLBDA=XLBR*(6E-3)**XLBEXR
            QK=(QMW**2-1.)/(QMW**2+2.)
            ! DIFFUSION
            IF(NDIFF==0.OR.NDIFF==4) THEN ! Rayleigh
               ZREFLOC(1:2)=1.E18*XCCR*ZLBDA**(ZCXR-6.)*MOMG(XALPHAR,XNUR,6.)
               IF(LWREFL) THEN ! weighting by reflectivities
                  ZREFL(JI,JEL,JAZ,JL,JH,JV,IVDOP)=-ZCR*SIN(PELEV(JI,JEL,JL,JV)) &
                       *1.E18*XCCR*ZLBDA**(ZCXR-6.-XDR)*MOMG(XALPHAR,XNUR,6.+XDR)
               ELSE 
                  ZREFL(JI,JEL,JAZ,JL,JH,JV,IMAX)=XCCR*ZLBDA**ZCXR
                  ZREFL(JI,JEL,JAZ,JL,JH,JV,IVDOP)=-ZCR*SIN(PELEV(JI,JEL,JL,JV)) &
                       *XCCR*ZLBDA**(ZCXR-XDR)*MOMG(XALPHAR,XNUR,XDR)
               END IF
               IF(LATT) THEN
                  IF(NDIFF==0) THEN ! Rayleigh 3rd order
                     ZAETMP(:)=XCCR*ZLBDA**ZCXR*(                                         &
                          XPI**2     /XLAM_RAD(JI)   *AIMAG(QK)                           &
                          *                MOMG(XALPHAR,XNUR,XBR)      /ZLBDA**XBR)
                  ELSE              ! Rayleigh 6th order
                     ZAETMP(:)=XCCR*ZLBDA**ZCXR*(                                         &
                          XPI**2     /XLAM_RAD(JI)   *AIMAG(QK)                           &
                          *                MOMG(XALPHAR,XNUR,XBR)      /ZLBDA**XBR        &
                          +XPI**4/15./XLAM_RAD(JI)**3*AIMAG(QK**2*(QMW**4+27.*QMW**2+38.) &
                          /(2.*QMW**2+3.))*MOMG(XALPHAR,XNUR,5.*XBR/3.)/ZLBDA**(5.*XBR/3.)&
                          +2.*XPI**5/3. /XLAM_RAD(JI)**4*REAL(QK**2)                      &
                          *                MOMG(XALPHAR,XNUR,2.*XBR)   /ZLBDA**(2.*XBR))
                  END IF
               END IF
            ELSE ! MIE OR T-MATRIX
               ZREFLOC(:)=0.
               IF(LATT) ZAETMP(:)=0.
               DO JJ=1,NPTS_GAULAG ! Gauss-Laguerre quadrature
                  SELECT CASE(NDIFF)
                  CASE(1) ! MIE
                     CALL BHMIE(XPI/XLAM_RAD(JI)*ZX(JJ)/ZLBDA,QMW,ZQEXT(1),ZQSCA,ZQBACK(1))
                     ZQBACK(2)=ZQBACK(1)
                     ZQBACK(3)=0.
                  CASE(2) ! NDIFF==2 T-matrix
                     ! G. TANGUY Allocation des Tableaux de MODD_TMAT
                     ALLOCATE(XRT11(NPN6,NPN4,NPN4))
                     ALLOCATE(XRT12(NPN6,NPN4,NPN4))
                     ALLOCATE(XRT21(NPN6,NPN4,NPN4))
                     ALLOCATE(XRT22(NPN6,NPN4,NPN4))
                     ALLOCATE(XIT11(NPN6,NPN4,NPN4))
                     ALLOCATE(XIT12(NPN6,NPN4,NPN4))
                     ALLOCATE(XIT21(NPN6,NPN4,NPN4))
                     ALLOCATE(XIT22(NPN6,NPN4,NPN4))
                     ALLOCATE(XTR1(NPN2,NPN2))
                     ALLOCATE(XTI1(NPN2,NPN2))
                     ALLOCATE(XQR(NPN2,NPN2))
                     ALLOCATE(XQI(NPN2,NPN2))
                     ALLOCATE(XRGQR(NPN2,NPN2))
                     ALLOCATE(XRGQI(NPN2,NPN2))
                     ALLOCATE(XJ(NPNG2,NPN1))
                     ALLOCATE(XY(NPNG2,NPN1))
                     ALLOCATE(XJR(NPNG2,NPN1))
                     ALLOCATE(XJI(NPNG2,NPN1))
                     ALLOCATE(XDJ(NPNG2,NPN1))
                     ALLOCATE(XDY(NPNG2,NPN1))
                     ALLOCATE(XDJR(NPNG2,NPN1))
                     ALLOCATE(XDJI(NPNG2,NPN1))

                     CALL TMD(&!2,&       !GTTE=1 SPHERES ; =2 OBLATE
                          ZX(JJ)/ZLBDA,&!Deq (m)
                          XLAM_RAD(JI),&!LAM: radar wavelength
                          REAL(QMW),&   !MRR: real part of refractive index
                          AIMAG(QMW),&  !MRI: imaginary part of refractive index (>=0)
                          NDGS,&       !NDGS: number of division points in computing integrals over the surface particles (default=2)
                          2,& ! gouttes oscillantes ? (oui=1,non=2)
                          PELEV(JI,JEL,JL,JV)*180./XPI,&! elevation in deg
                          ZQBACK(1),ZQBACK(2),ZQBACK(3),ZQEXT(1),&
                          1./ARF(ZX(JJ)/ZLBDA))         ! axis ratio function
                     !                                 ZQBACK(3)=ZQBACK(3)/ZLBDA**2
                     ZQBACK(3)=12.*ZQBACK(3)/ZX(JJ)**2/XPI


                  ! DEALLOACTION DES TABLEAUX
                     DEALLOCATE(XRT11)
                     DEALLOCATE(XRT12)
                     DEALLOCATE(XRT21)
                     DEALLOCATE(XRT22)
                     DEALLOCATE(XIT11)
                     DEALLOCATE(XIT12)
                     DEALLOCATE(XIT21)
                     DEALLOCATE(XIT22)
                     DEALLOCATE(XTR1)
                     DEALLOCATE(XTI1)
                     DEALLOCATE(XQR)
                     DEALLOCATE(XQI)
                     DEALLOCATE(XRGQR)
                     DEALLOCATE(XRGQI)
                     DEALLOCATE(XJ)
                     DEALLOCATE(XY)
                     DEALLOCATE(XJR)
                     DEALLOCATE(XJI)
                     DEALLOCATE(XDJ)
                     DEALLOCATE(XDY)
                     DEALLOCATE(XDJR)
                     DEALLOCATE(XDJI)


                  CASE(3) ! NDIFF==3 RG
                     IF(ZX(JJ)/ZLBDA<.5E-3) THEN
                        ZLB=1./3.
                     ELSE
                        ZLB=1./(ARF(ZX(JJ)/ZLBDA))**2-1. ! f**2
                        ZLB=(1.+ZLB)/ZLB*(1.-ATAN(SQRT(ZLB))/SQRT(ZLB)) ! lambda_b
                        if(ZX(JJ)/ZLBDA>16.61E-3) print*, 'Negative axis ratio; reduce NPTS_GAULAG.'
                     END IF
                     ZQBACK(1)=4.*(XPI/XLAM_RAD(JI)*ZX(JJ)/ZLBDA)**4&
                          *ABS((QMW**2-1.)/3./(1.+.5*(1.-ZLB)*(QMW**2-1.)))**2
                     ZQBACK(2)=4.*(XPI/XLAM_RAD(JI)*ZX(JJ)/ZLBDA)**4*ABS((QMW**2-1.)/3.*&
                          (SIN(PELEV(JI,JEL,JL,JV))**2/(1.+.5*(1.-ZLB)*(QMW**2-1.))+& ! PELEV=PI+THETA_I
                          COS(PELEV(JI,JEL,JL,JV))**2/(1.+ZLB*(QMW**2-1.))) )**2 !
                     ZQBACK(3)=ZX(JJ)/ZLBDA**3*REAL((QMW**2-1.)**2*(3.*ZLB-1.)/(2.+(QMW**2-1.)*(ZLB+1.) &
                          +ZLB*(1.-ZLB)*(QMW**2-1.)**2))
                     IF(LATT) THEN
                        ZQEXT(1)=4.*(XPI/XLAM_RAD(JI)*ZX(JJ)/ZLBDA)*AIMAG((QMW**2-1.)/3./(1.+.5*(1.-ZLB)*(QMW**2-1.)))
                        ZQEXT(2)=4.*(XPI/XLAM_RAD(JI)*ZX(JJ)/ZLBDA)*AIMAG((QMW**2-1.)/3.*&
                             (SIN(PELEV(JI,JEL,JL,JV))**2/(1.+.5*(1.-ZLB)*(QMW**2-1.))+& ! PELEV=PI+THETA_I
                             COS(PELEV(JI,JEL,JL,JV))**2/(1.+ZLB*(QMW**2-1.))))
                     END IF
                  END SELECT
                  ZREFLOC(1:3)=ZREFLOC(1:3)+ZQBACK(1:3)*ZX(JJ)**2*ZW(JJ)
                  ZREFLOC(4)=ZREFLOC(4)+ZQBACK(1)*ZX(JJ)**(2+XDR)*ZW(JJ)
                  IF(LATT) ZAETMP(:)=ZAETMP(:)+ZQEXT(:)*ZX(JJ)**2*ZW(JJ)
               END DO ! end loop Gauss-Laguerre quadrature
               ZREFLOC(1:2)=1.E18*ZREFLOC(1:2)*(XLAM_RAD(JI)/XPI)**4/.93*XCCR/4./ZLBDA**3
               ZREFLOC(3)=ZREFLOC(3)*XPI**2/6./XLAM_RAD(JI)*XCCR/ZLBDA &
                    *180.E3/XPI ! (in deg/km)

               ZREFL(JI,JEL,JAZ,JL,JH,JV,IVDOP)=PVDOP_RAY(JI,JEL,JAZ,JL,JH,JV)*ZREFLOC(1) &
                    -ZCR*SIN(PELEV(JI,JEL,JL,JV))*ZREFLOC(4) &
                    *1.E18*(XLAM_RAD(JI)/XPI)**4/.93*XCCR/4./ZLBDA**(3+XDR)
               IF(LATT) ZAETMP(:)=ZAETMP(:)*XPI*XCCR*ZLBDA**(ZCXR-2.*XBR/3.)/(4.*GAMMA(XNUR))
            END IF
            ZREFL(JI,JEL,JAZ,JL,JH,JV,1:3)=ZREFL(JI,JEL,JAZ,JL,JH,JV,1:3)+ZREFLOC(1:3)
            ZREFL(JI,JEL,JAZ,JL,JH,JV,IZER)=ZREFLOC(1) ! Z_e due to raindrops
            IF(LATT) THEN
               ZAELOC(JI,JEL,JAZ,JL,JH,JV,:)=ZAETMP(:)
               ZREFL(JI,JEL,JAZ,JL,JH,JV,IAER)=ZAETMP(1)
               IF(JL>1) ZAERINT=ZAERINT*EXP(-2.*ZREFL(JI,JEL,JAZ,JL-1,JH,JV,IAER)*XSTEP_RAD)
               ZREFL(JI,JEL,JAZ,JL,JH,JV,IZER)=ZREFL(JI,JEL,JAZ,JL,JH,JV,IZER)*ZAERINT ! Z_r attenuated
            END IF
         END IF
         ! Total attenuation even if no hydrometeors
         IF(LATT.AND.JL>1) ZREFL(JI,JEL,JAZ,JL,JH,JV,IATR)=ZREFL(JI,JEL,JAZ,JL-1,JH,JV,IATR) &
              *EXP(-2.*ZREFL(JI,JEL,JAZ,JL-1,JH,JV,IAER)*XSTEP_RAD)
      END IF
      
      !
      !---------------------------------------------------------------------------------------------------
      !*       3.    PRISTINE ICE
      !              ---------
      !
      IF (SIZE(PI_RAY,1)>0)  THEN
         IF(PI_RAY(JI,JEL,JAZ,JL,JH,JV) > XRTMIN(4) .AND. PCIT_RAY(JI,JEL,JAZ,JL,JH,JV)> 527.82) THEN ! cit > 527.82 otherwise pbs due to interpolation
            QMI=SQRT(QEPSI(PT_RAY(JI,JEL,JAZ,JL,JH,JV),XLIGHTSPEED/XLAM_RAD(JI)))
            QK=(QMI**2-1.)/(QMI**2+2.)
            ZDMELT_FACT=(6.*XAI)/(XPI*.92*XRHOLW)
            ZEXP=2.*XBI
            ZLBDA=XLBI*(PRHODREF_RAY(JI,JEL,JAZ,JL,JH,JV)*PI_RAY(JI,JEL,JAZ,JL,JH,JV)/ &
                 PCIT_RAY(JI,JEL,JAZ,JL,JH,JV))**XLBEXI
            IF(NDIFF==0.OR.NDIFF==3.OR.NDIFF==4) THEN ! Rayleigh or Rayleigh-Gans (pristine ice = sphere)
               ZREFLOC(1:2)=ZEQICE*.92**2*ZDMELT_FACT**2*1.E18*PCIT_RAY(JI,JEL,JAZ,JL,JH,JV) &
                    *ZLBDA**(XCXI-ZEXP)*MOMG(XALPHAI,XNUI,ZEXP)
  						 ZREFLOC(3)=0.
               IF(LWREFL) THEN ! weighting by reflectivities
                  ZREFL(JI,JEL,JAZ,JL,JH,JV,IVDOP)=ZREFL(JI,JEL,JAZ,JL,JH,JV,IVDOP)&
                       -ZCI*SIN(PELEV(JI,JEL,JL,JV))*ZEQICE*.92**2*ZDMELT_FACT**2&
                       *1.E18*PCIT_RAY(JI,JEL,JAZ,JL,JH,JV)*ZLBDA**(XCXI-ZEXP-XDI)*MOMG(XALPHAI,XNUI,ZEXP+XDI)
               ELSE 
                  ZREFL(JI,JEL,JAZ,JL,JH,JV,IMAX)=ZREFL(JI,JEL,JAZ,JL,JH,JV,IMAX)&
                       +PCIT_RAY(JI,JEL,JAZ,JL,JH,JV)*ZLBDA**XCXI
                  ZREFL(JI,JEL,JAZ,JL,JH,JV,IVDOP)=ZREFL(JI,JEL,JAZ,JL,JH,JV,IVDOP)&
                       -ZCI*SIN(PELEV(JI,JEL,JL,JV))&
                       *PCIT_RAY(JI,JEL,JAZ,JL,JH,JV)*ZLBDA**(XCXI-XDI)*MOMG(XALPHAI,XNUI,XDI)
               END IF
               IF(LATT) THEN
                  IF(NDIFF==0.OR.NDIFF==3) THEN
                     ZAETMP(:)=PCIT_RAY(JI,JEL,JAZ,JL,JH,JV)*ZLBDA**XCXI*(                 &
                          ZDMELT_FACT          *XPI**2    /XLAM_RAD(JI)   *AIMAG(QK)       &
                          *                MOMG(XALPHAI,XNUI,XBI)      /ZLBDA**XBI)
                  ELSE
                     ZAETMP(:)=PCIT_RAY(JI,JEL,JAZ,JL,JH,JV)*ZLBDA**XCXI*(                 &
                          ZDMELT_FACT          *XPI**2    /XLAM_RAD(JI)   *AIMAG(QK)       &
                          *                MOMG(XALPHAI,XNUI,XBI)      /ZLBDA**XBI         &
                          +ZDMELT_FACT**(5./3.)*XPI**4/15./XLAM_RAD(JI)**3                 &
                          *AIMAG(QK**2*(QMI**4+27.*QMI**2+38.)                             &
                          /(2.*QMI**2+3.))*MOMG(XALPHAI,XNUI,5.*XBI/3.)/ZLBDA**(5.*XBI/3.) &
                          +ZDMELT_FACT**2   *2.*XPI**5/3. /XLAM_RAD(JI)**4*REAL(QK**2)     &
                          *                MOMG(XALPHAI,XNUI,2.*XBI)   /ZLBDA**(2.*XBI))
                  END IF
               END IF
            ELSE ! MIE OR T-MATRIX
               ZREFLOC(:)=0.
               IF(LATT) ZAETMP(:)=0.
               DO JJ=1,NPTS_GAULAG ! Gauss-Laguerre quadrature
                  ZD=ZX(JJ)**(1./XALPHAI)/ZLBDA
                  ZDE=ZDMELT_FACT**(1./3.)*ZD**(XBI/3.)
                  CALL BHMIE(XPI/XLAM_RAD(JI)*ZDE,QMI,ZQEXT(1),ZQSCA,ZQBACK(1))
                  ! zqback=4.*(XPI/XLAM_RAD(JI))**4*ABS((QMI**2-1.)/(QMI**2+2.))**2* &
                  ! ((ZX(JJ)**(1./XALPHAI)/ZLBDA/(XPI*XRHOLW/(6.*XAI))**(1./XBI))**(XBI/3.))**4 !! rayleigh
                  ZQBACK(2)=ZQBACK(1)
                  ZQBACK(3)=0.
                  ZREFLOC(1:3)=ZREFLOC(1:3)+ZQBACK(1:3)*ZX(JJ)**(XNUI-1.+2.*XBI/3./XALPHAI)*ZW(JJ)
                  ZREFLOC(4)=ZREFLOC(4)+ZQBACK(1)*ZX(JJ)**(XNUI-1.+2.*XBI/3./XALPHAI+XDI/XALPHAI)*ZW(JJ)
                  IF(LATT) ZAETMP(:)=ZAETMP(:)+ZQEXT(:)*ZX(JJ)**(XNUI-1.+2.*XBI/3./XALPHAI)*ZW(JJ)
               END DO ! END  Gauss-Laguerre quadrature
               ZREFLOC(1:2)=1.E18*(XLAM_RAD(JI)/XPI)**4*PCIT_RAY(JI,JEL,JAZ,JL,JH,JV) &
                    *ZLBDA**(XCXI-2.*XBI/3.)/(4.*GAMMA(XNUI)*.93)*ZDMELT_FACT**(2./3.)*ZREFLOC(1:2)
               ZREFL(JI,JEL,JAZ,JL,JH,JV,IVDOP)=ZREFL(JI,JEL,JAZ,JL,JH,JV,IVDOP)&
                    +PVDOP_RAY(JI,JEL,JAZ,JL,JH,JV)*ZREFLOC(1) &
                    -ZCI*SIN(PELEV(JI,JEL,JL,JV))*ZREFLOC(4) &
                    *1.E18*(XLAM_RAD(JI)/XPI)**4*PCIT_RAY(JI,JEL,JAZ,JL,JH,JV) &
                    *ZLBDA**(XCXI-2.*XBI/3.-XDI)/(4.*GAMMA(XNUI)*.93)*ZDMELT_FACT**(2./3.)
                IF(LATT) ZAETMP(:)=ZAETMP(:)*XPI*PCIT_RAY(JI,JEL,JAZ,JL,JH,JV)*ZLBDA**(XCXI-2.*XBI/3.)/(4.*GAMMA(XNUI))&
                    *ZDMELT_FACT**(2./3.)
            END IF
            ZREFL(JI,JEL,JAZ,JL,JH,JV,1:3)=ZREFL(JI,JEL,JAZ,JL,JH,JV,1:3)+ZREFLOC(1:3)
            ZREFL(JI,JEL,JAZ,JL,JH,JV,IZEI)=ZREFLOC(1) ! z_e due to pristine ice
            
            IF(LATT) THEN
               ZAELOC(JI,JEL,JAZ,JL,JH,JV,:)=ZAELOC(JI,JEL,JAZ,JL,JH,JV,:)+ZAETMP(:)
               ZREFL(JI,JEL,JAZ,JL,JH,JV,IAEI)=ZAETMP(1)
               IF(JL>1) ZAEIINT=ZAEIINT*EXP(-2.*ZREFL(JI,JEL,JAZ,JL-1,JH,JV,IAEI)*XSTEP_RAD)
               ZREFL(JI,JEL,JAZ,JL,JH,JV,IZEI)=ZREFL(JI,JEL,JAZ,JL,JH,JV,IZEI)*ZAEIINT ! Z_i attenuated
            END IF
         END IF
         ! Total attenuation even if no hydrometeors
         IF(LATT.AND.JL>1) ZREFL(JI,JEL,JAZ,JL,JH,JV,IATI)=ZREFL(JI,JEL,JAZ,JL-1,JH,JV,IATI) &
              *EXP(-2.*ZREFL(JI,JEL,JAZ,JL-1,JH,JV,IAEI)*XSTEP_RAD)
      END IF
      
      !---------------------------------------------------------------------------------------------------
      !*       4.    SNOW
      !              -----
      !
      IF (SIZE(PS_RAY,1)>0)  THEN
         IF(PS_RAY(JI,JEL,JAZ,JL,JH,JV) > 100000.*XRTMIN(5)) THEN
            QMI=SQRT(QEPSI(PT_RAY(JI,JEL,JAZ,JL,JH,JV),XLIGHTSPEED/XLAM_RAD(JI)))
            ZDMELT_FACT=6.*XAS/(XPI*.92*XRHOLW)
            ZEXP=2.*XBS
            ZLBDA= XLBS*( PRHODREF_RAY(JI,JEL,JAZ,JL,JH,JV)*PS_RAY(JI,JEL,JAZ,JL,JH,JV) )**XLBEXS
            IF(NDIFF==0.OR.NDIFF==3.OR.NDIFF==4) THEN ! Rayleigh or Rayleigh-Gans 
               ZREFLOC(1:2)=ZEQICE*.92**2*ZDMELT_FACT**2*1.E18*XCCS*ZLBDA**(XCXS-ZEXP)*MOMG(XALPHAS,XNUS,ZEXP)
  						 ZREFLOC(3)=0.
               IF(LWREFL) THEN ! weighting by reflectivities
                  ZREFL(JI,JEL,JAZ,JL,JH,JV,IVDOP)=ZREFL(JI,JEL,JAZ,JL,JH,JV,IVDOP)&
                        -ZCS*SIN(PELEV(JI,JEL,JL,JV))*ZEQICE*.92**2*ZDMELT_FACT**2&
                       *1.E18*XCCS*ZLBDA**(XCXS-ZEXP-XDS)*MOMG(XALPHAS,XNUS,ZEXP+XDS)
               ELSE
                  ZREFL(JI,JEL,JAZ,JL,JH,JV,IMAX)=ZREFL(JI,JEL,JAZ,JL,JH,JV,IMAX)+XCCS*ZLBDA**XCXS
                  ZREFL(JI,JEL,JAZ,JL,JH,JV,IVDOP)=ZREFL(JI,JEL,JAZ,JL,JH,JV,IVDOP)&
                       -ZCS*SIN(PELEV(JI,JEL,JL,JV))&
                       *XCCS*ZLBDA**(XCXS-XDS)*MOMG(XALPHAS,XNUS,XDS)
               END IF
               IF(LATT) THEN
                  IF(NDIFF==0.OR.NDIFF==3) THEN
                     ZAETMP(:)=XCCS*ZLBDA**XCXS*(                                          &
                          ZDMELT_FACT          *XPI**2    /XLAM_RAD(JI)   *AIMAG(QK)       &
                          *                MOMG(XALPHAS,XNUS,XBS)      /ZLBDA**XBS)
                  ELSE
                     ZAETMP(:)=XCCS*ZLBDA**XCXS*(                                          &
                          ZDMELT_FACT          *XPI**2    /XLAM_RAD(JI)   *AIMAG(QK)       &
                          *                MOMG(XALPHAS,XNUS,XBS)      /ZLBDA**XBS         &
                          +ZDMELT_FACT**(5./3.)*XPI**4/15./XLAM_RAD(JI)**3                 &
                          *AIMAG(QK**2*(QMI**4+27.*QMI**2+38.)                             &
                          /(2.*QMI**2+3.))*MOMG(XALPHAS,XNUS,5.*XBS/3.)/ZLBDA**(5.*XBS/3.) &
                          +ZDMELT_FACT**2   *2.*XPI**5/3. /XLAM_RAD(JI)**4*REAL(QK**2)     &
                          *                MOMG(XALPHAS,XNUS,2.*XBS)   /ZLBDA**(2.*XBS))
                  END IF
               END IF
            ELSE ! MIE OR T-MATRIX
               ZREFLOC(:)=0.
               IF(LATT) ZAETMP(:)=0.
               DO JJ=1,NPTS_GAULAG ! Gauss-Laguerre quadrature
                  ZD=ZX(JJ)**(1./XALPHAS)/ZLBDA
                  ZDE=ZDMELT_FACT**(1./3.)*ZD**(XBS/3.)
                  SELECT CASE(NDIFF)
                  CASE(1,2) ! MIE or T-matrix but we use Mie (particles are considered as isotropic=spheres)
                     CALL BHMIE(XPI/XLAM_RAD(JI)*ZDE,QMI,ZQEXT(1),ZQSCA,ZQBACK(1))
                     ZQBACK(2)=ZQBACK(1)
                     ZQBACK(3)=0.
                  END SELECT
                  ZREFLOC(1:3)=ZREFLOC(1:3)+ZQBACK(1:3)*ZX(JJ)**(XNUS-1.+2.*XBS/3./XALPHAS)*ZW(JJ)
                  ZREFLOC(4)=ZREFLOC(4)+ZQBACK(1)*ZX(JJ)**(XNUS-1.+2.*XBS/3./XALPHAS+XDS/XALPHAS)*ZW(JJ)
                  IF(LATT) ZAETMP(:)=ZAETMP(:)+ZQEXT(:)*ZX(JJ)**(XNUS-1.+2.*XBS/3./XALPHAS)*ZW(JJ)
               END DO
               ZREFLOC(1:2)=1.E18*(XLAM_RAD(JI)/XPI)**4*XCCS &
                    *ZLBDA**(XCXS-2.*XBS/3.)/(4.*GAMMA(XNUS)*.93)*ZDMELT_FACT**(2./3.)*ZREFLOC(1:2)
               ZREFL(JI,JEL,JAZ,JL,JH,JV,IVDOP)=ZREFL(JI,JEL,JAZ,JL,JH,JV,IVDOP)&
                    +PVDOP_RAY(JI,JEL,JAZ,JL,JH,JV)*ZREFLOC(1) &
                    -ZCS*SIN(PELEV(JI,JEL,JL,JV))*ZREFLOC(4) &
                    *1.E18*(XLAM_RAD(JI)/XPI)**4*XCCS &
                    *ZLBDA**(XCXS-2.*XBS/3.-XDS)/(4.*GAMMA(XNUS)*.93)*ZDMELT_FACT**(2./3.)
               IF(LATT) ZAETMP(:)=ZAETMP(:)*XPI*XCCS*ZLBDA**(XCXS-2.*XBS/3.)/(4.*GAMMA(XNUS))&
                    *ZDMELT_FACT**(2./3.)
            END IF
            ZREFL(JI,JEL,JAZ,JL,JH,JV,1:3)=ZREFL(JI,JEL,JAZ,JL,JH,JV,1:3)+ZREFLOC(1:3)
            ZREFL(JI,JEL,JAZ,JL,JH,JV,IZES)=ZREFLOC(1) ! Z_e due to snow
            IF(LATT) THEN
               ZAELOC(JI,JEL,JAZ,JL,JH,JV,:)=ZAELOC(JI,JEL,JAZ,JL,JH,JV,:)+ZAETMP(:)
               ZREFL(JI,JEL,JAZ,JL,JH,JV,IAES)=ZAETMP(1)
               IF(JL>1) ZAESINT=ZAESINT*EXP(-2.*ZREFL(JI,JEL,JAZ,JL-1,JH,JV,IAES)*XSTEP_RAD)
               ZREFL(JI,JEL,JAZ,JL,JH,JV,IZES)=ZREFL(JI,JEL,JAZ,JL,JH,JV,IZES)*ZAESINT ! Z_s attenuated
            END IF
         END IF
         ! Total attenuation even if no hydrometeors
         IF(LATT.AND.JL>1) ZREFL(JI,JEL,JAZ,JL,JH,JV,IATS)=ZREFL(JI,JEL,JAZ,JL-1,JH,JV,IATS) &
              *EXP(-2.*ZREFL(JI,JEL,JAZ,JL-1,JH,JV,IAES)*XSTEP_RAD)
      END IF
      
      !---------------------------------------------------------------------------------------------------
      !*       5.    GRAUPEL
      !              -------
      !
      !ZDG=.5 ! from Bringi & Chandrasekar 2001, p. 433
      IF (SIZE(PG_RAY,1)>0)  THEN
         IF(PG_RAY(JI,JEL,JAZ,JL,JH,JV) > XRTMIN(6)) THEN
            QMI=SQRT(QEPSI(MIN(PT_RAY(JI,JEL,JAZ,JL,JH,JV),XTT),XLIGHTSPEED/XLAM_RAD(JI)))
            QMW=SQRT(QEPSW(MAX(PT_RAY(JI,JEL,JAZ,JL,JH,JV),XTT),XLIGHTSPEED/XLAM_RAD(JI)))
            ZLBDA=XLBG*(PRHODREF_RAY(JI,JEL,JAZ,JL,JH,JV)*PG_RAY(JI,JEL,JAZ,JL,JH,JV))**XLBEXG
            IF(PT_RAY(JI,JEL,JAZ,JL,JH,JV) > XTT) THEN ! mixture of ice and water
               ZFRAC_ICE = .85
            ELSE ! only ice
               ZFRAC_ICE=1.
            END IF
            ZDMELT_FACT=6.*XAG/(XPI*XRHOLW*((1.-ZFRAC_ICE)+ZFRAC_ICE*0.92))
            ZEXP=2.*XBG
            QB=2.*QMW**2*(2.*QMI**2*LOG(QMI/QMW)/(QMI**2-QMW**2)-1.)/(QMI**2-QMW**2)
            QM=SQRT(((1.-ZFRAC_ICE)*QMW**2+ZFRAC_ICE*QB*QMI**2)/(1.-ZFRAC_ICE+ZFRAC_ICE*QB)) ! Bohren & Battan (1982)
            QK=(QM**2-1.)/(QM**2+2.)
            IF(NDIFF==0.OR.NDIFF==3.OR.NDIFF==4) THEN
               ZREFLOC(1:2)=ABS(QK)**2/.93*ZDMELT_FACT**2*1.E18*XCCG*ZLBDA**(XCXG-ZEXP)*MOMG(XALPHAG,XNUG,ZEXP)
  						 ZREFLOC(3)=0.
               IF(LWREFL) THEN ! weighting by reflectivities
                  ZREFL(JI,JEL,JAZ,JL,JH,JV,IVDOP)=ZREFL(JI,JEL,JAZ,JL,JH,JV,IVDOP)&
                       -ZCG*SIN(PELEV(JI,JEL,JL,JV))*ABS(QK)**2/.93*ZDMELT_FACT**2&
                       *1.E18*XCCG*ZLBDA**(XCXG-ZEXP-XDG)*MOMG(XALPHAG,XNUG,ZEXP+XDG)
               ELSE
                  ZREFL(JI,JEL,JAZ,JL,JH,JV,IMAX)=ZREFL(JI,JEL,JAZ,JL,JH,JV,IMAX)+XCCG*ZLBDA**XCXG
                  ZREFL(JI,JEL,JAZ,JL,JH,JV,IVDOP)=ZREFL(JI,JEL,JAZ,JL,JH,JV,IVDOP)&
                       -ZCG*SIN(PELEV(JI,JEL,JL,JV))&
                       *XCCG*ZLBDA**(XCXG-XDG)*MOMG(XALPHAG,XNUG,XDG)
               END IF
               IF(LATT) THEN
                  IF(NDIFF==0.OR.NDIFF==3) THEN
                     ZAETMP(:)=XCCG*ZLBDA**XCXG*(                                        &
                          ZDMELT_FACT          *XPI**2    /XLAM_RAD(JI)   *AIMAG(QK)     &
                          *               MOMG(XALPHAG,XNUG,XBG)      /ZLBDA**XBG)
                  ELSE
                     ZAETMP(:)=XCCG*ZLBDA**XCXG*(                                        &
                          ZDMELT_FACT          *XPI**2    /XLAM_RAD(JI)   *AIMAG(QK)     &
                          *               MOMG(XALPHAG,XNUG,XBG)      /ZLBDA**XBG        &
                          +ZDMELT_FACT**(5./3.)*XPI**4/15./XLAM_RAD(JI)**3               &
                          *AIMAG(QK**2*(QM**4+27.*QM**2+38.)                             &
                          /(2.*QM**2+3.))*MOMG(XALPHAG,XNUG,5.*XBG/3.)/ZLBDA**(5.*XBG/3.)&
                          +ZDMELT_FACT**2   *2.*XPI**5/3. /XLAM_RAD(JI)**4*REAL(QK**2)   &
                          *               MOMG(XALPHAG,XNUG,2.*XBG)   /ZLBDA**(2.*XBG))
                  END IF
               END IF
            ELSE ! Mie or T-matrix
               ZREFLOC(:)=0.
               IF(LATT) ZAETMP(:)=0.
               DO JJ=1,NPTS_GAULAG ! Gauss-Laguerre quadrature
                  ZD=ZX(JJ)**(1./XALPHAG)/ZLBDA
                  ZDE=ZDMELT_FACT**(1./3.)*ZD**(XBG/3.)
                  !                           SELECT CASE(NDIFF)
                  !                           CASE(0,3)
                  !                              ZQBACK(1)=4.*(XPI/XLAM_RAD(JI))**4*ABS(QK)**2*ZDE**4
                  !                              ZQEXT(1)=4.*(XPI*ZDE/XLAM_RAD(JI)*AIMAG(QK)&
                  !                                   +(XPI*ZDE/XLAM_RAD(JI))**3*AIMAG(QK**2*(QM**4+27.*QM**2+38.)/(2.*QM**2+3.))/15.&
                  !                                   +2.*(XPI*ZDE/XLAM_RAD(JI))**4*REAL(QK**2)/3.)
                  !                           CASE(1,2) ! MIE/T-MATRIX (we use Mie in both cases)
                  CALL BHMIE(XPI/XLAM_RAD(JI)*ZDE,QM,ZQEXT(1),ZQSCA,ZQBACK(1))
                  !                           END SELECT
                  ZQBACK(2)=ZQBACK(1)
                  ZQBACK(3)=0.
                  ZREFLOC(1:3)=ZREFLOC(1:3)+ZQBACK(1:3)*ZX(JJ)**(XNUG-1.+2.*XBG/3./XALPHAG)*ZW(JJ)
                  ZREFLOC(4)=ZREFLOC(4)+ZQBACK(1)*ZX(JJ)**(XNUG-1.+2.*XBG/3./XALPHAG+XDG/XALPHAG)*ZW(JJ)
                  IF(LATT) ZAETMP(:)=ZAETMP(:)+ZQEXT(:)*ZX(JJ)**(XNUG-1.+2.*XBG/3./XALPHAG)*ZW(JJ)
               END DO
               ZREFLOC(1:2)=ZREFLOC(1:2)*1.E18*(XLAM_RAD(JI)/XPI)**4*XCCG &
                    *ZLBDA**(XCXG-2.*XBG/3.)/(4.*GAMMA(XNUG)*.93)*ZDMELT_FACT**(2./3.)
               ZREFL(JI,JEL,JAZ,JL,JH,JV,IVDOP)=ZREFL(JI,JEL,JAZ,JL,JH,JV,IVDOP)&
                    +PVDOP_RAY(JI,JEL,JAZ,JL,JH,JV)*ZREFLOC(1) &
                    -ZCG*SIN(PELEV(JI,JEL,JL,JV))*ZREFLOC(4) &
                    *1.E18*(XLAM_RAD(JI)/XPI)**4*XCCG &
                    *ZLBDA**(XCXG-2.*XBG/3.-XDG)/(4.*GAMMA(XNUG)*.93)*ZDMELT_FACT**(2./3.)
               IF(LATT) ZAETMP(:)=ZAETMP(:)*XPI*XCCG*ZLBDA**(XCXG-2.*XBG/3.)/(4.*GAMMA(XNUG))&
                    *ZDMELT_FACT**(2./3.)
            END IF
            ZREFL(JI,JEL,JAZ,JL,JH,JV,1:3)=ZREFL(JI,JEL,JAZ,JL,JH,JV,1:3)+ZREFLOC(1:3)
            ZREFL(JI,JEL,JAZ,JL,JH,JV,IZEG)=ZREFLOC(1) ! z_e due to graupel
            IF(LATT) THEN
               ZAELOC(JI,JEL,JAZ,JL,JH,JV,:)=ZAELOC(JI,JEL,JAZ,JL,JH,JV,:)+ZAETMP(:)
               ZREFL(JI,JEL,JAZ,JL,JH,JV,IAEG)=ZAETMP(1)
               IF(JL>1) ZAEGINT=ZAEGINT*EXP(-2.*ZREFL(JI,JEL,JAZ,JL-1,JH,JV,IAEG)*XSTEP_RAD)
               ZREFL(JI,JEL,JAZ,JL,JH,JV,IZEG)=ZREFL(JI,JEL,JAZ,JL,JH,JV,IZEG)*ZAEGINT ! Z_g attenuated
            END IF
         END IF
         ! Total attenuation even if no hydrometeors
         IF(LATT.AND.JL>1) ZREFL(JI,JEL,JAZ,JL,JH,JV,IATG)=ZREFL(JI,JEL,JAZ,JL-1,JH,JV,IATG) &
              *EXP(-2.*ZREFL(JI,JEL,JAZ,JL-1,JH,JV,IAEG)*XSTEP_RAD)
      END IF
      
      IF(LWREFL) THEN ! weighting by reflectivities
         ZREFL(JI,JEL,JAZ,JL,JH,JV,IVDOP)=ZREFL(JI,JEL,JAZ,JL,JH,JV,IVDOP)&
              +PVDOP_RAY(JI,JEL,JAZ,JL,JH,JV)*ZREFL(JI,JEL,JAZ,JL,JH,JV,1)
      ELSE IF(LWBSCS) THEN ! weighting by hydrometeor concentrations
         ZREFL(JI,JEL,JAZ,JL,JH,JV,IVDOP)=ZREFL(JI,JEL,JAZ,JL,JH,JV,IVDOP)&
              +PVDOP_RAY(JI,JEL,JAZ,JL,JH,JV)*ZREFL(JI,JEL,JAZ,JL,JH,JV,IMAX)
      ELSE IF(ZREFL(JI,JEL,JAZ,JL,JH,JV,IMAX)/=0.) THEN ! no weighting
         ZREFL(JI,JEL,JAZ,JL,JH,JV,IVDOP)=ZREFL(JI,JEL,JAZ,JL,JH,JV,IVDOP)/ZREFL(JI,JEL,JAZ,JL,JH,JV,IMAX)&
              +PVDOP_RAY(JI,JEL,JAZ,JL,JH,JV)
      END IF

   ELSE
      IF(JV==1.OR.ZREFL(JI,JEL,JAZ,JL,JH,MAX(JV-1,1),1)==-XUNDEF) THEN ! ground clutter
         ZREFL(JI,JEL,JAZ,JL,JH,JV,1:2)=-XUNDEF
      ELSE ! outside model domain (top or lateral boundaries)
         ZREFL(JI,JEL,JAZ,JL,JH,JV,1:2)=0.
      END IF
      LPART_MASK=.TRUE.
   END IF
END IF

               END DO LOOPJL
            END DO !JV
         END DO !JH
      END DO !JAZ
   END DO !JEL
END DO !JI

!
! attenuation in dB/km
IF(LATT) ZREFL(:,:,:,:,:,:,IAER:IAEG)=4343.*2.*ZREFL(:,:,:,:,:,:,IAER:IAEG) ! specific attenuation
! convective/stratiform
ZREFL(:,:,:,:,:,:,4)=PBU_MASK_RAY(:,:,:,:,:,:) ! CSR
! /convective/stratiform
!---------------------------------------------------------------------------------------------------
!*       6.    FINAL STEP : TOTAL ATTENUATION AND EQUIVALENT REFLECTIVITY FACTOR
!              ---------------------------------------------------------------
!
ALLOCATE(ZVTEMP(IMAX))

DO JI=1,INBRAD  
   IEL=NBELEV(JI)
   DO JEL=1,IEL  
      DO JAZ=1,INBAZIM 
         IF (LATT) ZAETOT(:,:,1:2)=1.
         DO JL=1,INBSTEPMAX
            IF(COUNT(ZREFL(JI,JEL,JAZ,JL,:,:,1)==-XUNDEF)==0.AND.COUNT(PT_RAY(JI,JEL,JAZ,JL,:,:)/=-XUNDEF)/=0) THEN ! if no undef point in gate JL and at least one point defined
               DO JH=1,INPTS_H
                  ZVTEMP(:)=0.
                  DO JV=1,INPTS_V  ! Loop on Jv
                     IF (JL > 1) THEN       
                        IF(LATT) THEN ! we use ZALPHAE0=alpha_0 from last gate
                           ZAETOT(JH,JV,1:2)=ZAETOT(JH,JV,1:2)*EXP(-2.*ZAELOC(JI,JEL,JAZ,JL-1,JH,JV,:)*XSTEP_RAD)
                           ZREFL(JI,JEL,JAZ,JL,JH,JV,1:2)=ZREFL(JI,JEL,JAZ,JL,JH,JV,1:2)*ZAETOT(JH,JV,1:2)!attenuated reflectivity
                           IF(LWREFL) ZREFL(JI,JEL,JAZ,JL,JH,JV,IVDOP)=ZREFL(JI,JEL,JAZ,JL,JH,JV,IVDOP)*ZAETOT(JH,JV,1)
                        END IF
                        
                     END IF

                     IF(.NOT.(LWREFL.AND.LWBSCS)) THEN
                        ZREFL(JI,JEL,JAZ,JL,JH,JV,IVDOP)=PVDOP_RAY(JI,JEL,JAZ,JL,JH,JV)
                     END IF

                     ! Quadrature on vertical reflectivities +VDOP
                     IF(LQUAD) THEN
                        ZVTEMP(:)=ZVTEMP(:)+ZREFL(JI,JEL,JAZ,JL,JH,JV,:)*PW_V(ABS((2*JV-INPTS_V-1)/2)+1) &
                             *EXP(-2.*LOG(2.)*PX_V(ABS((2*JV-INPTS_V-1)/2)+1)**2)
                     ELSE
                        ZVTEMP(:)=ZVTEMP(:)+ZREFL(JI,JEL,JAZ,JL,JH,JV,:)*PW_V(ABS((2*JV-INPTS_V-1)/2)+1)
                     END IF
                  END DO ! End loop on JV
                  
                  IF(LQUAD) THEN
                     PZE(JI,JEL,JAZ,JL,:)=PZE(JI,JEL,JAZ,JL,:)+ZVTEMP(1:SIZE(PZE,5))*PW_H(ABS((2*JH-INPTS_H-1)/2)+1) &
                          *EXP(-2.*LOG(2.)*PX_H(ABS((2*JH-INPTS_H-1)/2)+1)**2)
                     IF(LWBSCS) ZCONC_BIN(JI,JEL,JAZ,JL)=ZCONC_BIN(JI,JEL,JAZ,JL)+ZVTEMP(IMAX)* &
                          PW_H(ABS((2*JH-INPTS_H-1)/2)+1)*EXP(-2.*LOG(2.)*PX_H(ABS((2*JH-INPTS_H-1)/2)+1)**2)
                  ELSE
                     PZE(JI,JEL,JAZ,JL,:)=PZE(JI,JEL,JAZ,JL,:)+ZVTEMP(1:SIZE(PZE,5))*PW_H(ABS((2*JH-INPTS_H-1)/2)+1)
                     IF(LWBSCS) ZCONC_BIN(JI,JEL,JAZ,JL)=ZCONC_BIN(JI,JEL,JAZ,JL)+ZVTEMP(IMAX)* &
                          PW_H(ABS((2*JH-INPTS_H-1)/2)+1)
                  END IF
               END DO ! End loop on JH 
               
               IF(LQUAD) THEN
                  PZE(JI,JEL,JAZ,JL,:)=PZE(JI,JEL,JAZ,JL,:)*2.*LOG(2.)/XPI
                  IF(LWBSCS) ZCONC_BIN(JI,JEL,JAZ,JL)=ZCONC_BIN(JI,JEL,JAZ,JL)*2.*LOG(2.)/XPI
               ELSE
                  PZE(JI,JEL,JAZ,JL,:)=PZE(JI,JEL,JAZ,JL,:)/XPI! ELSE REMAINS -XUNDEF
                  IF(LWBSCS) ZCONC_BIN(JI,JEL,JAZ,JL)=ZCONC_BIN(JI,JEL,JAZ,JL)/XPI
               END IF
               
               IF(PZE(JI,JEL,JAZ,JL,1)>=10**(XREFLVDOPMIN/10.)) THEN ! Doppler velocities if Z>=XREFLVDOPMIN dBZ
                  IF(LWREFL) THEN
                     PZE(JI,JEL,JAZ,JL,IVDOP)=PZE(JI,JEL,JAZ,JL,IVDOP)/PZE(JI,JEL,JAZ,JL,1)
                  ELSE IF(LWBSCS) THEN
                     IF(ZCONC_BIN(JI,JEL,JAZ,JL)>0.) THEN
                        PZE(JI,JEL,JAZ,JL,IVDOP)=PZE(JI,JEL,JAZ,JL,IVDOP)/ZCONC_BIN(JI,JEL,JAZ,JL)
                     ELSE
                        PZE(JI,JEL,JAZ,JL,IVDOP)=-XUNDEF
                     END IF
                  END IF
               ELSE
                  PZE(JI,JEL,JAZ,JL,IVDOP)=-XUNDEF
               END IF

            ELSE      ! ground clutter or outside Meso-NH domain        
               PZE(JI,JEL,JAZ,JL,1:2)=-XUNDEF
            END IF

            IF(PZE(JI,JEL,JAZ,JL,1) < 0.) THEN  ! flag bin when underground                    
               PZE(JI,JEL,JAZ,JL,1)=XVALGROUND
               PZE(JI,JEL,JAZ,JL,IZER:IZEG)=XVALGROUND
            END IF

            IF(LATT) THEN
               WHERE(PZE(JI,JEL,JAZ,JL,IATR:IATG)<=0.)
                  PZE(JI,JEL,JAZ,JL,IATR:IATG)=XVALGROUND
               END WHERE
            END IF
         END DO
      END DO
   END DO
END DO

DEALLOCATE(ZREFL,ZVTEMP)
WRITE(0,*) 'NB PZE VALGROUND :', COUNT(PZE(:,:,:,:,1) ==XVALGROUND)
WRITE(0,*) 'NB PZE > 0 :', COUNT(PZE(:,:,:,:,1)>0.)
WRITE(0,*) 'NB PZE = 0 :', COUNT(PZE(:,:,:,:,1)==0.)
WRITE(0,*) 'NB PZE < 0 :', COUNT(PZE(:,:,:,:,1) < 0.)-COUNT(PZE(:,:,:,:,1) ==XVALGROUND)
IF(NDIFF/=0) DEALLOCATE(ZX,ZW)
IF (LATT) DEALLOCATE(ZAELOC,ZAETOT)
WRITE(0,*) 'END OF RADAR SCATTERING'
END SUBROUTINE RADAR_SCATTERING

