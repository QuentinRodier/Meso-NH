!ORILAM_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!ORILAM_LIC This is part of the ORILAM software governed by the CeCILL-C licence
!ORILAM_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!ORILAM_LIC for details.
!-----------------------------------------------------------------
!--------------- special set of characters for RCS information
!-----------------------------------------------------------------
! $Source: /home/cvsroot/MNH-VX-Y-Z/src/MNH/ch_aer_nucl.f90,v $ $Revision: 1.1.4.1.18.1 $
! MASDEV4_7 chimie 2006/05/18 13:07:25
!-----------------------------------------------------------------
!!    ################################ 
MODULE MODI_CH_AER_KULMALA
!!    ################################ 
!!
INTERFACE
  !!
  SUBROUTINE CH_AER_KULMALA(PRH,PTEMP,PSULF,PJNUC,PRC)
  IMPLICIT NONE
  !!
  REAL, DIMENSION(:), INTENT(IN)    :: PRH, PTEMP, PSULF
  REAL, DIMENSION(:), INTENT(INOUT) :: PJNUC, PRC
  !!
  END SUBROUTINE CH_AER_KULMALA
  !!
END INTERFACE
!
END MODULE MODI_CH_AER_KULMALA
!!
!! #########################################################################
SUBROUTINE CH_AER_KULMALA(PRH,PTEMP,PSULF,PJNUC,PRC)
!###########################################################
!!                   
!!    PURPOSE
!!    -------
!!      
!!    Compute nucleation rate for binary sulfate/H2O 
!!    This is the Kulmala parametrization (1998)
!!
!!    Valid for : 
!!    233.15 < T < 298.15     (K)
!!    10 < RH < 100          (%)
!!    1.10¹⁰ < [H2SO4]gas < 3.10¹⁰  (molec/cm3)
!!
!!    AUTHOR
!!    ------
!!    B. Foucart     * LACy *
!!
!!    MODIFICATIONS
!!    -------------
!!    B. Foucart (18/06/2018) * LACy *
!!
!----------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
USE MODD_CH_AEROSOL
USE MODD_CST,       ONLY : XAVOGADRO
! 
IMPLICIT NONE
!
!*       0.1   Declarations of dummy arguments :
!
REAL, DIMENSION(:), INTENT(IN)    :: PRH, PTEMP     ! Relative humidity (%), Temp (kelvin)
REAL, DIMENSION(:), INTENT(IN)    :: PSULF          ! Available acid mass (ug./m3)
REAL, DIMENSION(:), INTENT(INOUT) :: PJNUC          ! Nucleation rate (#/cm3/s)
REAL, DIMENSION(:), INTENT(INOUT) :: PRC           ! Rayon du cluster critique en nm définit pour ch_aer_nucl
INTEGER :: II
!
!*       0.2   Declarations of local variables :
!
REAL, DIMENSION(SIZE(PSULF,1)) :: ZSULF               ! Sulfuric acid concentration (molec/cm3)
REAL, DIMENSION(SIZE(PSULF,1)) :: ZAL                 ! Mole fraction of H2SO4 in the critical cluster 
REAL, DIMENSION(SIZE(PSULF,1)) :: ZRA                 ! Relative acidity
REAL, DIMENSION(SIZE(PSULF,1)) :: ZH2O                ! Water concentration (molec/cm3)
REAL, DIMENSION(SIZE(PSULF,1)) :: ZPVH2O              ! Saturation vapor pressure for water (N/m2, T in K)
REAL, DIMENSION(SIZE(PSULF,1)) :: ZPVH2SO4            ! Saturation vapor pressure for sulfuric acid (N/m2, T in K)
REAL, DIMENSION(SIZE(PSULF,1)) :: ZKHI,ZSIG,ZNSULFC,ZNSULF ! Terms for nucleation rate calculation
!
REAL, PARAMETER :: ZKB=1.381E-23                     ! Boltzman cste (m2 kg s-2 K-1)
!
PJNUC(:)=0.
ZAL(:)=1E-5
ZRA(:)=0.
ZSULF(:)=0.
ZPVH2SO4(:)=0.
ZH2O(:)=0.
ZRA(:)=0.
ZSIG(:)=0.
ZNSULFC(:)=0.
ZKHI(:)=0.
!
!    a. Sulfuric acid concentration definition: ZSULF from ug/m3 to  molec/cm3
!
ZSULF(:) = PSULF(:)
ZSULF(:) = ZSULF(:)*(XAVOGADRO*1.E-12) / XH2SO4 
!
!    b. Conditions on sulfuric acid concentration to use Kulmala
!
 ZSULF(:) = MAX(MIN(ZSULF(:), 3.E11), 0.)
!
!    c. Restrictions for parametrization
!
  WHERE(((PTEMP(:)>=223.).OR.(PTEMP(:)<=298)).AND.(PRH(:)>=0.1))
       !
       !   1. Saturation vapor pressure for H2SO4 over a flat surface (N/m-2, T in K)
       !
       !      a. Ayers et al., 1980
       !
        ZPVH2SO4(:)=EXP(27.78492066-10156.0/PTEMP(:))
       !
       !      b. Kulmala and Laaksonen., 1990
       !
       ! ZPVH2SO4(:)=EXP(-10156./ZT0+16.259+10156.*(-1./PTGAS(:)+1./ZT0+0.38/(ZTC-ZT0)*&
       !             (1.+LOG(ZT0/PTGAS(:))-ZT0/PTGAS(:))))*101325.
       !
       !      c. Noppel et al., 2002
       !
       ! ZPVH2SO4(:)=EXP(-11.94+10156*((1/360.15)-(1/PTGAS(:))+(0.38/545)*&
       !             (1+LOG((360.15/PTGAS(:))-(360.15/PTGAS(:))))))
       !
       !   2. Saturation vapor pressure for water over a flat surface (N/m2, T in K)
       !      (Preining et al, 1981)
       !
       ZPVH2O(:) = EXP(77.344913-7235.4247/PTEMP(:)-8.2*LOG(PTEMP(:))+0.0057113*PTEMP(:))
       !
       !   3. Water concentration (molec/cm3) 
       ! 
        ZH2O(:) = PRH(:)*ZPVH2O(:)/(ZKB*PTEMP(:))/1.E6
       !
       !   4. Relative Acidity
       !
        ZRA(:)=ZSULF(:)*1.E6*(ZKB*PTEMP(:))/ZPVH2SO4(:) 
       !
  END WHERE
!
!   5. H2SO4 mole fraction in the critical nucleous (no unity) 
!
  WHERE ((ZSULF(:)>0.).AND.(ZH2O(:)>0.).AND.(ZRA(:)/=0.)) 
        !
         ZAL(:)=1.2233-(0.0154*ZRA(:))/(ZRA(:)+PRH(:))+0.0102*& ! (eq 17) 
                LOG(ZSULF(:))-0.0415*LOG(ZH2O(:))+0.0016*PTEMP(:)
        !
  END WHERE
!
  WHERE (((PTEMP(:)>=223.).OR.(PTEMP(:)<=298)).AND.(PRH(:)>=0.1).AND.ZAL(:)>1E-5)
        !
        !   6. Sulfuric nucleation rate (molec/cm3/s) 
        !
        !      a. Sulfuric acid vapor needed to produce jnuc = 1 cm-3.s-1 
        !
         ZNSULFC(:)=EXP(-14.5125+0.1335*PTEMP(:)-10.5462*PRH(:)+1958.4*PRH(:)/PTEMP(:)) ! (eq 18)
        !
        !      b. Sigma term 
        !
         ZSIG(:) = 1.+(PTEMP(:)-273.15)/273.15 ! (eq 22)
        !
        !      c. Sulfuric acid vapor ratio term 
        !
         ZNSULF(:)=LOG(ZSULF(:)/ZNSULFC(:)) ! (eq 21)
        !
        !
        !      d. Exponential term
        ! 
         ZKHI(:)=25.1289*ZNSULF(:)-4890.8*ZNSULF(:)/PTEMP(:)-1743.3/PTEMP(:)-2.2479*ZSIG(:)*ZNSULF(:)*PRH(:)+&
                7643.4*ZAL(:)/PTEMP(:)-1.9712*ZAL(:)*ZSIG(:)/PRH(:)  ! (eq 20)
        !
        !      e. Nucleation rate
        !
         PJNUC(:)=EXP(ZKHI(:)) ! (eq 19)
        !
  END WHERE
!
PRC(:) = 0.5 ! The critical radius (nm) calculation is not given in Kulmala so we fix the values as 0.5
! 
RETURN
!
END SUBROUTINE CH_AER_KULMALA
