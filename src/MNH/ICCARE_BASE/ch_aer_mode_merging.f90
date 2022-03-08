!ORILAM_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!ORILAM_LIC This is part of the ORILAM software governed by the CeCILL-C licence
!ORILAM_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!ORILAM_LIC for details.
!!   #########################
     MODULE MODI_CH_AER_MODE_MERGING
!!   #########################
!!
INTERFACE
!!
  SUBROUTINE CH_AER_MODE_MERGING(PM, PLNSIG, PRG, PDMGROW, PDMMERG)
  !!
  IMPLICIT NONE
  REAL, DIMENSION(:,:),   INTENT(IN)    :: PM, PLNSIG, PRG
  REAL, DIMENSION(:,:),   INTENT(INOUT) :: PDMGROW
  REAL, DIMENSION(:,:),   INTENT(INOUT) :: PDMMERG
  !!
  END SUBROUTINE CH_AER_MODE_MERGING
!!
END INTERFACE
!!
END MODULE MODI_CH_AER_MODE_MERGING
!!
!!   ##############################################
     SUBROUTINE CH_AER_MODE_MERGING(PM, PLNSIG, PRG, PDMGROW, PDMMERG)
!!   ##############################################
!!
!!   PURPOSE
!!   -------
!!     If the Aitken mode mass is growing faster than accumulation mode
!!   mass and the Aitken mode number concentration exceeds the
!!   accumulation mode number concentration, then moments tendency
!!   are adjusted. In the present developpement only moments 3 and 6 
!!   based on the condensated moments are modified.
!!
!!   METHOD
!!   ------
!!
!!   EXTERNAL
!!   --------
!!     none
!!
!!   IMPLICIT ARGUMENTS
!!   ------------------
!!     USE MODD_CH_AEROSOL
!!
!!   REFERENCE
!!   ---------
!!   implementation adapted from
!! 
!!     Binkowski and Roselle (2003). Models-3 Community Multiscale Air Quality (CMAQ) model 
!!   aerosol component: 1, Model description. J. Geophys. Res., 108(D6), 4183.
!!   doi:10.1029/2001JD001409
!!   
!!   for M3 and M6 tendencies.
!!
!!   AUTHOR
!!   ------
!!     Joris Pianezze (LACy)
!!
!!   MODIFICATIONS
!!   -------------
!!     Original 06/2018  
!!
!-------------------------------------------------------------------------------
!
!*       0.     DECLARATIONS
!               ------------
!    
USE MODD_CH_AEROSOL
USE MODD_CONF, ONLY : NVERB
!
IMPLICIT NONE
!
!*       0.1   Declarations of arguments
!
REAL, DIMENSION(:,:),   INTENT(IN)    :: PM, PLNSIG, PRG
REAL, DIMENSION(:,:),   INTENT(INOUT) :: PDMGROW
REAL, DIMENSION(:,:),   INTENT(INOUT) :: PDMMERG
!
!*       0.2   Declarations of local variables
!
INTEGER :: JI,JJ
REAL    :: ZA, ZB, ZC, ZDELTA
REAL    :: ZC3, ZC2, ZC1, ZQ
REAL    :: ZXNUM
REAL    :: ZXM0, ZXM6, ZXM3
REAL    :: ZFNUM, ZFM0, ZFM3, ZFM6
REAL    :: ZPHNUM, ZPHM0, ZPHM3, ZPHM6
!
!-------------------------------------------------------------------------------
!
!*       1.    MODE MERGING
!              ------------
!
DO JI=1,SIZE(PM,1) 
  !
!  IF ( PDMGROW( JI , NM3(1) ) .GT. PDMGROW( JI , NM3(2) ) ) THEN
    !
    !    
    !*          1.1 CALCULATE XNUM
    !               --------------
    !
    ! Solve equation of Ackermann et al. 1998 
    ! with xnum = ln (d/d_i) / (sqrt(2)*ln(sig_i))
    !
    ZC1 = PLNSIG(JI,1) / PLNSIG(JI,2)
    ZC2 = LOG( PRG(JI,2) / PRG(JI,1) ) / ( SQRT(2.0) * PLNSIG(JI,1) )
    ZC3 = LOG( ZC1 * PM(JI,NM0(2)) / PM(JI,NM0(1)) )
    !
    ! Calculate quadratic equation coefficients & discriminant
    ! Resolution with Press et al. algorithm : page 208
    ZA     = 1.0 - ZC1 * ZC1
    ZB     = 2.0 * ZC2 * ZC1 * ZC1
    ZC     = ZC3 - ZC2 * ZC2 * ZC1 * ZC1
    ZDELTA = ZB * ZB - 4.0 * ZA * ZC
    !
    ! If roots are imaginary, no mode merging takes place.
    !
    IF ( ZDELTA .LT. 0.0 ) THEN
      ZQ    = - 5.0
      ZXNUM = 0.0
    ELSE
      ZQ    = - 0.5 * ( ZB + SIGN( 1.0, ZB ) * SQRT( ZDELTA ) )
      ZXNUM = ZC / ZQ
    END IF
    !
    !-----------------------------------------------------------------------
    ! Ensure that Xnum is large enough so that no more than half of
    ! the Aitken mode mass is merged into the accumulation mode during
    ! any given time step.  This criterion is described in Paragraph 26
    ! of Binkowski and Roselle (2003).
    !
    ZXNUM = MAX( ZXNUM, 3.0 * PLNSIG(JI,1) / SQRT(2.0) )
    !
    !    
    !*          1.2 MODIFCATION OF MOMENTS TENDENCY
    !               -------------------------------
    !
    ZXM0  = ZXNUM
    ZXM3  = ZXNUM - 3.0 * PLNSIG(JI,1) / SQRT(2.0)
    ZXM6  = ZXNUM - 6.0 * PLNSIG(JI,1) / SQRT(2.0)
    !
    ! Calculate the fractions of the moments 0, 3 and 6
    ! distributions with diameter greater than the intersection diameter
    !
    ZFM0 = 0.5 * ERFC( ZXM0 )             ! Eq 10a of B&R 2003
    ZFM3 = 0.5 * ERFC( ZXM3 )             ! Eq 10b of B&R 2003
    ZFM6 = 0.5 * ERFC( ZXM6 )             ! Adapted to 6th moment
    !
    ! Calculate the fractions of the moments 0, 3 and 6
    ! distributions with diameters less than the intersection diameter.
    !
    ZPHM0 = 0.5 * ( 1.0 + ERF( ZXM0 ) )   ! Eq 10c of B&R 2003
    ZPHM3 = 0.5 * ( 1.0 + ERF( ZXM3 ) )   ! Eq 10d of B&R 2003
    ZPHM6 = 0.5 * ( 1.0 + ERF( ZXM6 ) )   ! Adapted to 6th moment
    !
    ! Update accumulation-mode moment tendencies using
    ! Equations 11a - 11c of Binkowski and Roselle (2003).
    !
    PDMMERG(JI,NM0(2)) = PDMGROW(JI,NM0(1)) * ZFM0
    PDMMERG(JI,NM3(2)) = PDMGROW(JI,NM3(1)) * ZFM3
    PDMMERG(JI,NM6(2)) = PDMGROW(JI,NM6(1)) * ZFM6
    !
    ! Update Aitken-mode moment tendencies using
    ! Equations 11d - 11f of Binkowski and Roselle (2003).
    !
    PDMMERG(JI,NM0(1)) = PDMGROW(JI,NM0(1)) * (ZPHM0 - 1.0)
    PDMMERG(JI,NM3(1)) = PDMGROW(JI,NM3(1)) * (ZPHM3 - 1.0)
    PDMMERG(JI,NM6(1)) = PDMGROW(JI,NM6(1)) * (ZPHM3 - 1.0)
    !
!  END IF
  !
END DO
!
END SUBROUTINE CH_AER_MODE_MERGING
