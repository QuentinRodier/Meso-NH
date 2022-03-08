!ORILAM_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!ORILAM_LIC This is part of the ORILAM software governed by the CeCILL-C licence
!ORILAM_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!ORILAM_LIC for details.
!!   #########################
     MODULE MODI_CH_AER_NUCL
!!   #########################
!!
INTERFACE
  !!
  SUBROUTINE CH_AER_NUCL(PRH,PTEMP,PSULF,PJNUC,PJ2RAT)
  IMPLICIT NONE
  !!
  REAL, DIMENSION(:), INTENT(IN)    :: PRH,PTEMP
  REAL, DIMENSION(:), INTENT(INOUT) :: PSULF
  REAL, DIMENSION(:), INTENT(INOUT) :: PJNUC
  REAL, DIMENSION(:), INTENT(INOUT) :: PJ2RAT
  !!
  END SUBROUTINE CH_AER_NUCL
  !!
END INTERFACE
!!
END MODULE MODI_CH_AER_NUCL
!!
!!   ##############################################
     SUBROUTINE CH_AER_NUCL(PRH,PTEMP,PSULF,PJNUC,PJ2RAT)
!!   ##############################################
!!
!!   PURPOSE
!!   -------
!!
!!   METHOD
!!   ------
!!
!!   EXTERNAL
!!   --------
!!     Subroutine CH_AER_KULMALA            : compute nucleation rate from Kulmala et al. 1998 parametrization
!!     Subroutine CH_AER_VEHKAMAKI          : compute nucleation rate from Vehkamaki et al. 2002 parametrization
!!     Subroutine CH_AER_MAATTANEN_NEUTRAL  : compute nucleation rate from Neural Maattanen et al. 2018 parametrization
!!     Subroutine CH_AER_MAATTANEN_IONIND   : compute nucleation rate from Ion-induced Maattanen et al. 2018 parametrization
!!
!!   IMPLICIT ARGUMENTS
!!   ------------------
!!     USE MODD_CH_AEROSOL
!!
!!   REFERENCE
!!   ---------
!!
!!   AUTHOR
!!   ------
!!   Brice Foucart & Joris Pianezze (LACy)
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
USE MODD_CST,  ONLY : XAVOGADRO
USE MODI_CH_AER_KULMALA
USE MODI_CH_AER_VEHKAMAKI
USE MODI_CH_AER_MAATTANEN_NEUTRAL
USE MODI_CH_AER_MAATTANEN_IONIND
USE MODI_CH_AER_MODE_MERGING
!
USE MODD_CH_AEROSOL
USE MODD_CONF, ONLY : NVERB
!
IMPLICIT NONE
!
!*       0.1   Declarations of arguments
!
REAL, DIMENSION(:), INTENT(IN)    :: PRH,PTEMP
REAL, DIMENSION(:), INTENT(INOUT) :: PSULF
REAL, DIMENSION(:), INTENT(INOUT) :: PJNUC
REAL, DIMENSION(:), INTENT(INOUT) :: PJ2RAT
!
!*       0.2   Declarations of local variables
!
REAL, DIMENSION(SIZE(PSULF,1)) :: ZRCN, ZRCI         ! Critical cluster in m (neutral and ion-ind)
REAL, DIMENSION(SIZE(PSULF,1)) :: ZRCN2, ZRCI2       ! Diameter of critical cluster in nm (neutral and ion-ind)
REAL, DIMENSION(SIZE(PSULF,1)) :: ZLKKN, ZLKKI       ! Final scaling factor from Lehtinen et al., 2007 (neutral and ion-ind)
REAL, DIMENSION(SIZE(PSULF,1)) :: ZJNUCN, ZJNUCI     ! Nucleation rate in part.cm-3.s-1 (neutral and ion-ind)
REAL, DIMENSION(SIZE(PSULF,1)) :: ZJ2RATN, ZJ2RATI   ! Nucleation rate for 2 nm in part.cm-3.s-1 (neutral and ion-ind) 
REAL, DIMENSION(SIZE(PSULF,1)) :: ZSULF              ! Sulfuric acid concentration in molec.cm-3
REAL, DIMENSION(SIZE(PSULF,1)) :: ZGR                ! Particle Growth Rate according to Nieminen et al., 2010 (nm.h-1)
REAL, DIMENSION(SIZE(PSULF,1)) :: ZGAMMA             ! Gamma
REAL  :: ZCS                                ! Typical CS value in atmosphere in 1/h
REAL  :: ZMAV                               ! Average m-value according to Lehtinen et al., 2007
REAL  :: ZTSIZE                             ! Target size (in geometric diameter = mobility diameter -0.3nm).
!
!-------------------------------------------------------------------------------
!
!*       1.    DEFINE VARIABLES FOR J2 (particle formation rate)
!              -----------------------------------------------
!
! [ Please, note that these calculations can fe found in the supplementary Fortran code of Maattanen et al., 2018 ]
!
!     a) H2SO4 conversion from ug.m-3 to molec.cm-3
!
!ZSULF(:) = PSULF(:)*(XAVOGADRO*1.E-12) / XH2SO4
! 
!     b) Growth rate calculation
!
!ZMAV = -1.6      ! It can also be calculated 
!
!
!ZGR(:) = ZSULF(:) / (661.1 * (PRH(:) * 100)**2 - 1.129E5 * (PRH(:)*100) + 1.549E7)
!
!
!     c) Condensation sink imposition
!
!ZCS = 22.        ! It can also be calculated
!
!     d) Target size (here 2 so 2 - 0.3 = 1.7)
!
!ZTSIZE = 1.7    ! We want a J2nm so 2nm -0.3 = 1.7 nm
!
!
!*       2.    NUCLEATION PARAMETRIZATIONS
!              ---------------------------
!
! [ Please, note that Kulmala et al., 1998 and Vehkamaki et al., 2002 are neutral parametrizations ]
!
!
!IF (NVERB .GE. 10) WRITE(*,*) '~~ CH_AER_NUCL PSULF (deb) =',PSULF
!
IF (CNUCLEATION == 'KULMALA') THEN
  !
  CALL CH_AER_KULMALA(PRH, PTEMP, PSULF, PJNUC, ZRCN)
  !
  !  J2 (J2RAT) calculation for Kulmala:
  !
  !ZRCN2(:) = 2. * ZRCN(:) * 1.E9
  !
  !ZGAMMA(:) = max( 0.0, 1.0 / (ZMAV+1) * ((ZTSIZE /(ZRCN2(:)))**(ZMAV+1) -1) )
  !
  !ZLKKN(:) = exp(-ZGAMMA(:) * ZRCN2(:) * ZCS / ZGR(:))   ! Final scaling factor 
  !
  !PJ2RAT(:) = PJNUC(:) * ZLKKN(:)
  !
ELSE IF (CNUCLEATION == 'VEHKAMAKI') THEN 
  !
  CALL CH_AER_VEHKAMAKI(PRH, PTEMP, PSULF, PJNUC, ZRCN)
  !
  !  J2 (J2RAT) calculation for Vehkamaki:
  !
  !ZRCN2(:) = 2. * ZRCN(:) * 1.E9
  !
  !ZGAMMA(:) = max( 0.0, 1.0 / (ZMAV+1) * ((ZTSIZE /(ZRCN2(:)))**(ZMAV+1) -1) )
  !
  !ZLKKN(:) = exp(-ZGAMMA(:) * ZRCN2(:) * ZCS / ZGR(:))   ! Final scaling factor 
  !
  !PJ2RAT(:) = PJNUC(:) * ZLKKN(:)
  !
ELSE IF (CNUCLEATION == 'MAATTANEN_NEUTRAL') THEN
  !
  ! Define ZJNUCN
  !
  ZJNUCN(:) = PJNUC(:)
  !
  CALL CH_AER_MAATTANEN_NEUTRAL(PRH, PTEMP, PSULF, ZJNUCN, ZRCN)
  !
  PJNUC(:) = ZJNUCN(:)
  !
  !  J2 (J2RAT) calculation for Maattanen neutral:
  !
  !ZRCN2(:) = 2. * ZRCN(:) * 1.E9
  !
  !ZGAMMA(:) = max( 0.0, 1.0 / (ZMAV+1) * ((ZTSIZE /(ZRCN2(:)))**(ZMAV+1) -1) )
  !
  !ZLKKN(:) = exp(-ZGAMMA(:) * ZRCN2(:) * ZCS / ZGR(:))   ! Final scaling factor 
  !
  !PJ2RAT(:) = PJNUC(:) * ZLKKN(:)
  !
ELSE IF (CNUCLEATION == 'MAATTANEN_IONIND') THEN
  !
  ! Define ZJNUCI
  !
  ZJNUCI(:) = PJNUC(:)
  !
  CALL CH_AER_MAATTANEN_IONIND(PRH, PTEMP, PSULF, ZJNUCI, ZRCI)
  !
  PJNUC(:) = ZJNUCI(:)
  !
  !  J2 (J2RAT) calculation for Maattanen ion-ind:
  !
  !ZRCI2(:) = 2. * ZRCI(:) * 1.E9
  !
  !ZGAMMA(:) = max( 0.0, 1.0 / (ZMAV+1) * ((ZTSIZE /(ZRCI2(:)))**(ZMAV+1) -1) )
  !
  !ZLKKI(:) = exp(-ZGAMMA(:) * ZRCI2(:) * ZCS / ZGR(:))   ! Final scaling factor 
  !
  !PJ2RAT(:) = PJNUC(:) * ZLKKI(:)
  !
ELSE IF (CNUCLEATION == 'MAATTANEN_BOTH') THEN
  !
  ! Define ZJNUCN
  !
  ZJNUCN(:) = PJNUC(:)
  !
  CALL CH_AER_MAATTANEN_NEUTRAL(PRH, PTEMP, PSULF, ZJNUCN, ZRCN)
  !
  !  J2 (J2RAT) calculation for Maattanen neutral:
  !
  !ZRCN2(:) = 2. * ZRCN(:) * 1.E9
  !
  !ZGAMMA(:) = max( 0.0, 1.0 / (ZMAV+1) * ((ZTSIZE /(ZRCN2(:)))**(ZMAV+1) -1) )
  !
  !ZLKKN(:) = exp(-ZGAMMA(:) * ZRCN2(:) * ZCS / ZGR(:))   ! Final scaling factor 
  !
  !ZJ2RATN(:) = ZJNUCN(:) * ZLKKN(:)
  !
  ! Define ZJNUCI
  !
  ZJNUCI(:) = PJNUC(:)
  !
  CALL CH_AER_MAATTANEN_IONIND(PRH, PTEMP, PSULF, ZJNUCI, ZRCI)
  !
  !  J2 (J2RAT) calculation for Maattanen ion-ind:
  !
  !ZRCI2(:) = 2. * ZRCI(:) * 1.E9
  !
  !ZGAMMA(:) = max( 0.0, 1.0 / (ZMAV+1) * ((ZTSIZE /(ZRCI2(:)))**(ZMAV+1) -1) )
  !
  !ZLKKI(:) = exp(-ZGAMMA(:) * ZRCI2(:) * ZCS / ZGR(:))   ! Final scaling factor 
  !
  !ZJ2RATI(:) = ZJNUCI(:) * ZLKKI(:)
  !
  ! New particle formation rates addition 
  !
  PJNUC(:) = ZJNUCN(:) + ZJNUCI(:)
  !
  !PJ2RAT(:) = ZJ2RATN(:) + ZJ2RATI(:)
  !
END IF
!
PJ2RAT(:) = 1E-7
!
IF (NVERB .GE. 10) WRITE(*,*) '~~ CH_AER_NUCL PJNUC =',PJNUC
IF (NVERB .GE. 10) WRITE(*,*) '~~ CH_AER_NUCL PSULF (fin) =',PSULF
IF (NVERB .GE. 10) WRITE(*,*) '~~ CH_AER_NUCL ZJNUCI =',ZJNUCI
IF (NVERB .GE. 10) WRITE(*,*) '~~ CH_AER_NUCL ZJNUCN =',ZJNUCN

!
END SUBROUTINE CH_AER_NUCL
