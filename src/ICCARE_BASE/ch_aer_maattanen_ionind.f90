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
MODULE MODI_CH_AER_MAATTANEN_IONIND
!!    ################################ 
!!
INTERFACE
  !!
  SUBROUTINE CH_AER_MAATTANEN_IONIND(PRH,PTEMP,PSULF,PJNUCI,PRCI)
  IMPLICIT NONE
  !!
  REAL, DIMENSION(:), INTENT(IN)    :: PRH,PTEMP,PSULF
  REAL, DIMENSION(:), INTENT(INOUT) :: PJNUCI, PRCI
  !!
  !!
  END SUBROUTINE CH_AER_MAATTANEN_IONIND
  !!
END INTERFACE
!!
END MODULE MODI_CH_AER_MAATTANEN_IONIND
!!
!! #########################################################################
SUBROUTINE CH_AER_MAATTANEN_IONIND(PRH,PTEMP,PSULF,PJNUCI,PRCI)
!###########################################################
!
!!                   
!!    PURPOSE
!!    -------
!!      
!!    Compute nucleation rate for binary H2SO4/H2O
!!    This is the Määttänen parametrization (2018) 
!!    This is the ion-induced particle formation part
!!
!!    Valid for : 
!!    195 < T < 400     (K)
!!    10⁻⁵ < RH < 100          (%)
!!    10⁴ < [H2SO4]gas < 10¹⁶  (molec/cm3)
!!
!!
!!    AUTHOR
!!    ------
!!    B. Foucart      * LACy *
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
USE MODD_CST,  ONLY : XAVOGADRO
USE MODD_CONF, ONLY : NVERB
USE MODD_CH_AEROSOL
!
IMPLICIT NONE
!
!*       0.1   Declarations of dummy arguments :
!
REAL, DIMENSION(:), INTENT(IN)    :: PRH, PTEMP, PSULF          ! Relative humidity (%), Temp (kelvin)
REAL, DIMENSION(:), INTENT(INOUT) :: PJNUCI, PRCI                ! Nucleation rate (#/cm3/s) , Critical cluster radius (nm)
!
!*       0.2   Declarations of local variables :
!
REAL, DIMENSION(SIZE(PSULF,1)) :: ZSULF               ! Sulfuric acid concentration (molec/cm3) 
REAL, DIMENSION(SIZE(PSULF,1)) :: ZAL                 ! Mole fraction of H2SO4 in the critical cluster
REAL, DIMENSION(SIZE(PSULF,1)) :: ZNTOTI              ! Total number of molec in the critical cluster 
REAL, DIMENSION(SIZE(PSULF,1)) :: ZKINTRI             ! Threshold sulfuric acid for charged kinetic nucleation
REAL, DIMENSION(SIZE(PSULF,1)) :: ZNACI               ! Sulfuric acid molecules in the charged critical cluster
REAL, DIMENSION(SIZE(PSULF,1)) :: ZIPR                ! Ion pair production rate (cm-3 .s-1)
REAL, DIMENSION(SIZE(PSULF,1)) :: ZXLOSS              ! Ion loss rate
REAL, DIMENSION(SIZE(PSULF,1)) :: ZCSI                ! Ion condensation sink (s-1)
REAL, DIMENSION(SIZE(PSULF,1)) :: ZAIRN               ! Air molecule concentration in (cm-3)
REAL, DIMENSION(SIZE(PSULF,1)) :: ZRECOMB             ! Ion-ion recombination rate
REAL, DIMENSION(SIZE(PSULF,1)) :: ZNIPAIR             ! Number of ion pairs in air (cm-3)
!
LOGICAL                        :: GKINETICI           ! True if kinetic neutral nucleation
!
INTEGER                         :: II, ITEST          ! Tests
!
IF (NVERB .GE. 10) WRITE(*,*) '~~~~ CH_AER_MAATT_ION : PSULF =',MINVAL(PSULF), MAXVAL(PSULF)
IF (NVERB .GE. 10) WRITE(*,*) '~~~~ CH_AER_MAATT_ION : (XAVOGADRO*1.E-12) =',(XAVOGADRO*1.E-12)
IF (NVERB .GE. 10) WRITE(*,*) '~~~~ CH_AER_MAATT_ION : XH2SO4=', XH2SO4
IF (NVERB .GE. 10) WRITE(*,*) '~~~~ CH_AER_MAATT_ION : PTEMP =',MINVAL(PTEMP), MAXVAL(PTEMP)
IF (NVERB .GE. 10) WRITE(*,*) '~~~~ CH_AER_MAATT_ION : PRH =',MINVAL(PRH), MAXVAL(PRH)
!
!----------------------------------------------------------------------------
!
! Parameters initialization 
!
ZAL(:)       = 0.17     ! must vary between 0 and 1 
PJNUCI(:)    = 1E-7     ! must vary between 10E-7 and 10E10 cm3.s-1
PRCI(:)      = 2.8E-10  ! (meters) must vary between 0.28 and 1.2 nm
ZNACI(:)     = 0.
ZNTOTI(:)    = 10.      ! must vary between 1 and 200 molecules
ZKINTRI(:)   = 0.
ZIPR(:)      = 20.
GKINETICI    = .FALSE.  ! Logical: if kinetic ion-induced nucleation (FALSE by default)
ZCSI(:)      = 1.0/480. ! Inverse lifetime of ions
!  
!    a. Air molecule concentration calculation
!
ZAIRN(:) = 6.023E23 * 1.013E5 / 8.31 / PTEMP(:) / 1.E6 ! Air molecule concentration in (cm-3)
!
!    b. Sulfuric acid concentration definition: ZSULF from ug/m3 to molec/cm3
!
ZSULF(:) = PSULF(:)*(XAVOGADRO*1.E-12) / XH2SO4
!
!    c. Restrictions for parametrization
!
ITEST = 0.
!
DO II = 1, SIZE(PSULF,1)
   IF ((PRH(II) > 1E-5).AND.(PTEMP(II)>195.).AND.(ZSULF(II)>1E4)) THEN
      ITEST = ITEST+1
   END IF
END DO
!
IF (NVERB .GE. 10) WRITE(*,*) '~~ CH_AER_MAATT_ION (deb): ZSULF',MINVAL(ZSULF(:)), MAXVAL(ZSULF(:))
IF (NVERB .GE. 10) WRITE(*,*) '~~ CH_AER_MAATT_ION (deb): PSULF',MINVAL(PSULF(:)), MAXVAL(PSULF(:))

!

DO II = 1, SIZE(PSULF,1)
   !
   IF ((PRH(II) > 1E-5).AND.(PTEMP(II)>195.).AND.(ZSULF(II)>1E4)) THEN
      !
      !  1. Mole fraction of H2SO4 in the critical cluster (eq 1): composition
      !
      ZAL(II) = 7.9036365428891719E-1-2.8414059650092153E-3*PTEMP(II)+&
                1.4976802556584141E-2*LOG(PRH(II))-2.4511581740839115E-4*PTEMP(II)*LOG(PRH(II))+&
                3.4319869471066424E-3*(LOG(PRH(II)))**2-2.8799393617748428E-5*PTEMP(II)*(LOG(PRH(II)))**2+&
                3.0174314126331765E-4*(LOG(PRH(II)))**3-2.2673492408841294E-6*PTEMP(II)*(LOG(PRH(II)))**3-&
                4.3948464567032377E-3*LOG(ZSULF(II))+5.3305314722492146E-5*PTEMP(II)*LOG(ZSULF(II))
      !
      IF (ZIPR(II).GT.0.0) THEN ! if the ion production rate is above zero
        !
        ! Calculate the ion induced nucleation rate wrt. concentration of 1 ion/cm3
        ! 
        ZKINTRI(II) = 5.3742280876674478e1 - &
                    & 6.6837931590012266e-3 *log(PRH(II))**(-2) &
                    & - 1.0142598385422842e-01 * log(PRH(II))**(-1) - &
                    & 6.4170597272606873e+00 * log(PRH(II)) &
                    & - 6.4315798914824518e-01 * log(PRH(II))**2 - &
                    & 2.4428391714772721e-02 * log(PRH(II))**3 &
                    & - 3.5356658734539019e-04 * log(PRH(II))**4 + &
                    & 2.5400015099140506e-05 * PTEMP(II) * log(PRH(II))**(-2) &
                    & - 2.7928900816637790e-04 * PTEMP(II) * log(PRH(II))**(-1) + &
                    & 4.4108573484923690e-02 * PTEMP(II) * log(PRH(II)) &
                    & + 6.3943789012475532e-03 * PTEMP(II) * log(PRH(II))**(2) + &
                    & 2.3164296174966580e-04 * PTEMP(II) * log(PRH(II))**(3) &
                    & + 3.0372070669934950e-06 * PTEMP(II) * log(PRH(II))**4 + &
                    & 3.8255873977423475e-06 * PTEMP(II)**2 * log(PRH(II))**(-1) &
                    & - 1.2344793083561629e-04 * PTEMP(II)**2 * log(PRH(II)) - &
                    & 1.7959048869810192e-05 * PTEMP(II)**2 * log(PRH(II))**(2) &
                    & - 3.2165622558722767e-07 * PTEMP(II)**2 * log(PRH(II))**3 - &
                    & 4.7136923780988659e-09 * PTEMP(II)**3 * log(PRH(II))**(-1) &
                    & + 1.1873317184482216e-07 * PTEMP(II)**3 * log(PRH(II)) + &
                    & 1.5685860354866621e-08 * PTEMP(II)**3 * log(PRH(II))**2 &
                    & - 1.4329645891059557e+04 * PTEMP(II)**(-1) + &
                    & 1.3842599842575321e-01 * PTEMP(II) &
                    & - 4.1376265912842938e-04 * PTEMP(II)**(2) + &
                    & 3.9147639775826004e-07 * PTEMP(II)**3
        !
        ZKINTRI(II)=exp(ZKINTRI(II)) !1/cm3
        !
        IF( ZKINTRI(II).LT.ZSULF(II)) GKINETICI=.TRUE.
        !
        IF (GKINETICI) THEN
          !
          !
          PJNUCI(II) = 1.0E6 * (0.3E-9 + 0.487E-9)**2. * sqrt(8. * 3.141593*1.38E-23 * &
                     & (1. / (1.661e-27 * 98.07)+1. / (1.661e-27*98.07))) * &
                     & sqrt(PTEMP(II))*ZSULF(II) !1/cm3s
          !
          ZNTOTI(II) = 1. !set to 1 
          !
          ZNACI(II) = 1.
          !
          ZAL(II) = ZNACI(II) / ZNTOTI(II) ! so also set this to 1
          !
          PRCI(II) = 0.487E-9
          !
        ELSE
          !
          PJNUCI(II) = 3.0108954259038608e+01 + PTEMP(II) * &
                     6.1176722090512577e+01 + PTEMP(II)**2 * &
                     8.7240333618891663e-01 + PTEMP(II)**3* &
                     (-4.6191788649375719e-03) + PTEMP(II)**(-1) * &
                     8.3537059107024481e-01 
          PJNUCI(II) = PJNUCI(II) + &
                     (1.5028549216690628e+01 + PTEMP(II) * &
                     (-1.9310989753720623e-01) + PTEMP(II)**2 * &
                     8.0155514634860480e-04 + PTEMP(II)**3 * &
                     (-1.0832730707799128e-06) + PTEMP(II)**(-1) * &
                     1.7577660457989019) * (LOG(PRH(II))**(-2)) 
          PJNUCI(II) = PJNUCI(II) + &
                     (-2.0487870170216488e-01 + PTEMP(II) * &
                     1.3263949252910405e-03 + PTEMP(II)**2 * &
                     (-8.4195688402450274e-06) + PTEMP(II)**3 * &
                     1.6154895940993287e-08 + PTEMP(II)**(-1) * &
                     3.8734212545203874e+01) * (LOG(PRH(II))**(-2) * LOG(ZSULF(II)))
          PJNUCI(II) = PJNUCI(II) + &
                     (1.4955918863858371 + PTEMP(II) * &
                     9.2290004245522454e+01 + PTEMP(II)**2 * &
                     (-8.9006965195392618e-01) + PTEMP(II)**3 * &
                     2.2319123411013099e-03 + PTEMP(II)**(-1) * &
                     4.0180079996840852e-03) * (LOG(PRH(II))**(-1) * LOG(ZSULF(II))**(-1)) 
          PJNUCI(II) = PJNUCI(II) + &
                     (7.9018031228561085 + PTEMP(II) * &
                     (-1.1649433968658949e+01) + PTEMP(II)**2 * &
                     1.1400827854910951e-01 + PTEMP(II)**3 * &
                     (-3.1941526492127755e-04) + PTEMP(II)**(-1) * &
                     (-3.7662115740271446e-01)) * (LOG(PRH(II))**(-1))
          PJNUCI(II) = PJNUCI(II) + &
                     (1.5725237111225979e+02 + PTEMP(II) * &
                     (-1.0051649979836277) + PTEMP(II)**2 * &
                     1.1866484014507624e-03 + PTEMP(II)**3 * &
                     7.3557614998540389e-06 + PTEMP(II)**(-1) * &
                     2.6270197023115189) * (LOG(PRH(II))**(-1) * LOG(ZSULF(II)))
          PJNUCI(II) = PJNUCI(II) + &
                     (-1.6973840122470968e+01 + PTEMP(II) * & 
                     1.1258423691432135e-01 + PTEMP(II)**2 * &
                     (-2.9850139351463793e-04) + PTEMP(II)**3 * &
                     1.4301286324827064e-07 + PTEMP(II)**(-1) * &
                     1.3163389235253725e+01) * (LOG(PRH(II))**(-1) * LOG(ZSULF(II))**2)
          PJNUCI(II) = PJNUCI(II) + &
                     (-1.0399591631839757 + PTEMP(II) * &
                     2.7022055588257691e-03 + PTEMP(II)**2 * &
                     (-2.1507467231330936e-06) + PTEMP(II)**3 * &
                     3.8059489037584171e-10 + PTEMP(II)**(-1) * &
                     1.5000492788553410e+02) * (LOG(PRH(II))**(-1) * LOG(ZSULF(II))**3)
          PJNUCI(II) = PJNUCI(II) + &
                     (1.2250990965305315 + PTEMP(II) * &
                     3.0495946490079444e+01 + PTEMP(II)**2 * & 
                     2.1051563135187106e+01 + PTEMP(II)**3 * &
                     (-8.2200682916580878e-02) + PTEMP(II)**(-1) * &
                     2.9965871386685029e-02) * (LOG(ZSULF(II))**(-2)) 
          PJNUCI(II) = PJNUCI(II) + &
                     (4.8281605955680433 + PTEMP(II) * &
                     1.7346551710836445e+02 + PTEMP(II)**2 * &
                     (-1.0113602140796010e+01) + PTEMP(II)**3 * &
                     3.7482518458685089e-02 + PTEMP(II)**(-1) * &
                     (-1.4449998158558205e-01)) * (LOG(ZSULF(II))**(-1))
          PJNUCI(II) = PJNUCI(II) + &
                     (2.3399230964451237e+02 + PTEMP(II) * &
                     (-2.3099267235261948e+01) + PTEMP(II)**2 * &
                     8.0122962140916354e-02 + PTEMP(II)**3 * &
                     6.1542576994557088e-05 + PTEMP(II)**(-1) * &
                     5.3718413254843007) * (LOG(ZSULF(II)))
          PJNUCI(II) = PJNUCI(II) + &
                     (1.0299715519499360e+02 + PTEMP(II) * &
                     (-6.4663357203364136e-02) + PTEMP(II)**2 * & 
                     (-2.0487150565050316e-03) + PTEMP(II)**3 * &
                     8.7935289055530897e-07 + PTEMP(II)**(-1) * &
                     3.6013204601215229e+01) * (LOG(ZSULF(II))**2)
          PJNUCI(II) = PJNUCI(II) + &
                     (-3.5452115439584042 + PTEMP(II) * &
                     1.7083445731159330e-02 + PTEMP(II)**2 * &
                     (-1.2552625290862626e-05) + PTEMP(II)**3 * &
                     1.2968447449182847e-09 + PTEMP(II)**(-1) * &
                     1.5748687512056560e+02) * (LOG(ZSULF(II))**3)
          PJNUCI(II) = PJNUCI(II) + &
                     (2.2338490119517975 + PTEMP(II) * &
                     1.0229410216045540e+02 + PTEMP(II)**2 * &
                     (-3.2103611955174052) + PTEMP(II)**3 * &
                     1.3397152304977591e-02 + PTEMP(II)**(-1) * &
                     (-2.4155187776460030e-02)) * (LOG(PRH(II))* LOG(ZSULF(II))**(-2))
          PJNUCI(II) = PJNUCI(II) + &
                     (3.7592282990713963 + PTEMP(II) * &
                     (-1.5257988769009816e+02) + PTEMP(II)**2 * &
                     2.6113805420558802 + PTEMP(II)**3 * &
                     (-9.0380721653694363e-03) + PTEMP(II)**(-1) * &
                     (-1.3974197138171082e-01)) * (LOG(PRH(II))* LOG(ZSULF(II))**(-1))
          PJNUCI(II) = PJNUCI(II) + &
                     (1.8293600730573988e+01 + PTEMP(II) * &
                     1.8344728606002992e+01 + PTEMP(II)**2 * &
                     (-4.0063363221106751e-01) + PTEMP(II)**3 * &
                     1.4842749371258522e-03 + PTEMP(II)**(-1) * & 
                     1.1848846003282287) * (LOG(PRH(II)))
          PJNUCI(II) = PJNUCI(II) + &
                     (-1.7634531623032314e+02 + PTEMP(II) * &
                     4.9011762441271278 + PTEMP(II)**2 * &
                     (-1.3195821562746339e-02) + PTEMP(II)**3 * &
                     (-2.8668619526430859e-05) + PTEMP(II)**(-1) * &
                     (-2.9823396976393551e-01)) * (LOG(PRH(II))* LOG(ZSULF(II)))
          PJNUCI(II) = PJNUCI(II) + &
                     (-3.2944043694275727e+01 + PTEMP(II) * &
                     1.2517571921051887e-01 + PTEMP(II)**2 * &
                     8.3239769771186714e-05 + PTEMP(II)**3 * &
                     2.8191859341519507e-07 + PTEMP(II)**(-1) * &
                     (-2.7352880736682319e+01)) * (LOG(PRH(II))* LOG(ZSULF(II))**2)
          PJNUCI(II) = PJNUCI(II) + &
                     (-1.1451811137553243 + PTEMP(II) * &
                     2.0625997485732494e-03 + PTEMP(II)**2 * &
                     (-3.4225389469233624e-06) + PTEMP(II)**3 * &
                     4.4437613496984567e-10 + PTEMP(II)**(-1) * &
                     1.8666644332606754e+02) * (LOG(PRH(II))* LOG(ZSULF(II))**3)
          PJNUCI(II) = PJNUCI(II) + &
                     (3.2270897099493567e+01 + PTEMP(II) * &
                     7.7898447327513687e-01 + PTEMP(II)**2 * &
                     (-6.5662738484679626e-03) + PTEMP(II)**3 * &
                     3.7899330796456790e-06 + PTEMP(II)**(-1) * &
                     7.1106427501756542e-01) * (LOG(PRH(II))**2 * LOG(ZSULF(II))**(-1))
          PJNUCI(II) = PJNUCI(II) + &
                     (-2.8901906781697811e+01 + PTEMP(II) * &
                     (-1.5356398793054860) + PTEMP(II)**2 * &
                     1.9267271774384788e-02 + PTEMP(II)**3 * &
                     (-5.3886270475516162e-05) + PTEMP(II)**(-1) * &
                     5.0490415975693426e-01) * (LOG(PRH(II))**2)
          PJNUCI(II) = PJNUCI(II) + &
                     (3.3365683645733924e+01 + PTEMP(II) * &
                     (-3.6114561564894537e-01) + PTEMP(II)**2 * &
                     9.2977354471929262e-04 + PTEMP(II)**3 * &
                     1.9549769069511355e-07 + PTEMP(II)**(-1) * & 
                     (-8.8865930095112855)) * (LOG(PRH(II))**2 * LOG(ZSULF(II)))
          PJNUCI(II) = PJNUCI(II) + &
                     (2.4592563042806375 + PTEMP(II) * &
                     (-8.3227071743101084e-03) + PTEMP(II)**2 * &
                     8.2563338043447783e-06 + PTEMP(II)**3 * &
                     (-8.4374976698593496e-09) + PTEMP(II)**(-1) * &
                     (-2.0938173949893473e+02)) * (LOG(PRH(II))**2 * LOG(ZSULF(II))**2)
          PJNUCI(II) = PJNUCI(II) + &
                     (4.4099823444352317e+01 + PTEMP(II) * &
                     2.5915665826835252 + PTEMP(II)**2 * &
                     (-1.6449091819482634e-02) + PTEMP(II)**3 * &
                     2.6797249816144721e-05 + PTEMP(II)**(-1) * &
                     5.5045672663909995e-01) * PRH(II)
          !
          PJNUCI(II) = EXP(PJNUCI(II))
          !
          ZNTOTI(II) = (-4.8324296064013375e+04 + PTEMP(II) * &
                       5.0469120697428906e+02 + PTEMP(II)**2 * &
                       (-1.1528940488496042e+00) + PTEMP(II)**(-1) * &
                       (-8.6892744676239192e+02) + (PTEMP(II)**(3)) * &
                       4.0030302028120469e-04) 
         ZNTOTI(II) = ZNTOTI(II) + &
                      (-6.7259105232039847e+03 + PTEMP(II) * &
                      1.9197488157452008e+02 + PTEMP(II)**2 * &
                      (-1.3602976930126354e+00) + PTEMP(II)**(-1) * &
                      (-1.1212637938360332e+02) + (PTEMP(II)**(3)) * &
                      2.8515597265933207e-03) * LOG(PRH(II))**(-2) * LOG(ZSULF(II))**(-2)
         ZNTOTI(II) = ZNTOTI(II) + &
                      (2.6216455217763342e+02 + PTEMP(II) * &
                      (-2.3687553252750821e+00) + PTEMP(II)**2 * &
                      7.4074554767517521e-03 + PTEMP(II)**(-1) * &
                      (-1.9213956820114927e+03) + (PTEMP(II)**(3)) * &
                      (-9.3839114856129453e-06)) * LOG(PRH(II))**(-2)
         ZNTOTI(II) = ZNTOTI(II) + &
                      (3.9652478944137344e+00 + PTEMP(II) * &
                      1.2469375098256536e-02 + PTEMP(II)**2 * &
                      (-9.9837754694045633e-05) + PTEMP(II)**(-1) * &
                      (-5.1919499210175138e+02) + (PTEMP(II)**(3)) * &
                      1.6489001324583862e-07) * LOG(PRH(II))**(-2) * LOG(ZSULF(II))
         ZNTOTI(II) = ZNTOTI(II) + &
                      (2.4975714429096206e+02 + PTEMP(II) * &
                      1.7107594562445172e+02 + PTEMP(II)**2 * &
                      (-7.8988711365135289e-01) + PTEMP(II)**(-1) * &
                      (-2.2243599782483177e+01) + (PTEMP(II)**(3)) * &
                      (-1.6291523004095427e-04)) * LOG(PRH(II))**(-1) * LOG(ZSULF(II))**(-2)
         ZNTOTI(II) = ZNTOTI(II) + &
                      (-8.9270715592533611e+02 + PTEMP(II) * &
                      1.2053538883338946e+02 + PTEMP(II)**2 * &
                      (-1.5490408828541018e+00) + PTEMP(II)**(-1) * &
                      (-1.1243275579419826e+01) + (PTEMP(II)**(3)) * &
                      4.8053105606904655e-03) * LOG(PRH(II))**(-1) * LOG(ZSULF(II))**(-1)
         ZNTOTI(II) = ZNTOTI(II) + &
                      (7.6426441642091631e+03 + PTEMP(II) * &
                      (-7.1785462414656578e+01) + PTEMP(II)**2 * &
                      2.3851864923199523e-01 + PTEMP(II)**(-1) * &
                      8.5591775688708395e+01 + (PTEMP(II)**(3)) * &
                      (-3.7000473243342858e-04)) * LOG(PRH(II))**(-1)
         ZNTOTI(II) = ZNTOTI(II) + &
                      (-5.1516826398607911e+01 + PTEMP(II) * &
                      9.1385720811460558e-01 + PTEMP(II)**2 * &
                      (-3.5477100262158974e-03) + PTEMP(II)**(-1) * &
                      2.7545544507625586e+03 + (PTEMP(II)**(3)) * &
                      5.4708262093640928e-06) * LOG(PRH(II))**(-1) * LOG(ZSULF(II))
         ZNTOTI(II) = ZNTOTI(II) + &
                      (-3.0386767129196176e+02 + PTEMP(II) * &
                      (-1.1033438883583569e+04) + PTEMP(II)**2 * &
                      8.1296859732896067e+01 + PTEMP(II)**(-1) * &
                      1.2625883141097162e+01 + (PTEMP(II)**(3)) * &
                      (-1.2728497822219101e-01)) * LOG(ZSULF(II))**(-2)
         ZNTOTI(II) = ZNTOTI(II) + &
                      (-3.3763494256461472e+03 + PTEMP(II) * &
                      3.1916579136391006e+03 + PTEMP(II)**2 * &
                      (-2.7234339474441143e+01) + PTEMP(II)**(-1) * &
                      (-2.1897653262707397e+01) + (PTEMP(II)**(3)) * & 
                      5.1788505812259071e-02) * LOG(ZSULF(II))**(-1)
         ZNTOTI(II) = ZNTOTI(II) + &
                      (-1.8817843873687068e+03 + PTEMP(II) * & 
                      4.3038072285882070e+00 + PTEMP(II)**2 * & 
                      6.6244087689671860e-03 + PTEMP(II)**(-1) * &
                      (-2.7133073605696295e+03) + (PTEMP(II)**(3)) * & 
                      (-1.7951557394285043e-05)) * LOG(ZSULF(II))
         ZNTOTI(II) = ZNTOTI(II) + &
                      (-1.7668827539244447e+02 + PTEMP(II) * & 
                      4.8160932330629913e-01 + PTEMP(II)**2 * & 
                      (-6.3133007671100293e-04) + PTEMP(II)**(-1) * &
                      2.5631774669873157e+04 + (PTEMP(II)**(3)) * & 
                      4.1534484127873519e-07) * LOG(ZSULF(II))**(2)
         ZNTOTI(II) = ZNTOTI(II) + &
                      (-1.6661835889222382e+03 + PTEMP(II) * &
                      1.3708900504682877e+03 + PTEMP(II)**2 * &
                      (-1.7919060052198969e+01) + PTEMP(II)**(-1) * &
                      (-3.5145029804436405e+01) + (PTEMP(II)**(3)) * &
                      5.1047240947371224e-02) * LOG(PRH(II))* LOG(ZSULF(II))**(-2)
         ZNTOTI(II) = ZNTOTI(II) + &
                      (1.0843549363030939e+04 + PTEMP(II) * &
                      (-7.3557073636139577e+01) + PTEMP(II)**2 * &
                      1.2054625131778862e+00 + PTEMP(II)**(-1) * &
                      1.9358737917864391e+02 + (PTEMP(II)**(3)) * &
                      (-4.2871620775911338e-03)) * LOG(PRH(II))* LOG(ZSULF(II))**(-1)
         ZNTOTI(II) = ZNTOTI(II) + &
                      (-2.4269802549752835e+03 + PTEMP(II) * &
                      1.1348265061941714e+01 + PTEMP(II)**2 * &
                      (-5.0430423939495157e-02) + PTEMP(II)**(-1) * &
                      2.3709874548950634e+03 + (PTEMP(II)**(3)) * &
                      1.4091851828620244e-04) * LOG(PRH(II))
         ZNTOTI(II) = ZNTOTI(II) + &
                      (5.2745372575251588e+02 + PTEMP(II) * &
                      (-2.6080675912627314e+00) + PTEMP(II)**2 * &
                      5.6902218056670145e-03 + PTEMP(II)**(-1) * &
                      (-3.2149319482897838e+04) + (PTEMP(II)**(3)) * &
                      (-5.4121996056745853e-06)) * LOG(PRH(II))* LOG(ZSULF(II))
         ZNTOTI(II) = ZNTOTI(II) + &
                      (-1.6401959518360403e+01 + PTEMP(II) * &
                      2.4322962162439640e-01 + PTEMP(II)**2 * & 
                      1.1744366627725344e-03 + PTEMP(II)**(-1) * &
                      (-8.2694427518413195e+03) + (PTEMP(II)**(3)) * &
                      (-5.0028379203873102e-06)) * LOG(PRH(II))**(2)
         ZNTOTI(II) = ZNTOTI(II) + &
                      (-2.7556572017167782e+03 + PTEMP(II) * &
                      4.9293344495058264e+01 + PTEMP(II)**2 * &
                      (-2.6503456520676050e-01) + PTEMP(II)**(-1) * &
                      1.2130698030982167e+03 + (PTEMP(II)**(3)) * &
                      4.3530610668042957e-04) * LOG(PRH(II))**2 * LOG(ZSULF(II))**(-1)
         ZNTOTI(II) = ZNTOTI(II) + &
                      (-6.3419182228959192e+00 + PTEMP(II) * & 
                      4.0636212834605827e-02 + PTEMP(II)**2 * & 
                      (-1.0450112687842742e-04) + PTEMP(II)**(-1) * &
                      3.1035882189759656e+02 +(PTEMP(II)**(3)) * &
                      9.4328418657873500e-08) * LOG(PRH(II))**(-3)
         ZNTOTI(II) = ZNTOTI(II) + &
                      (3.0189213304689042e+03 + PTEMP(II) * &
                      (-2.3804654203861684e+01) + PTEMP(II)**2 * &
                      6.8113013411972942e-02 + PTEMP(II)**(-1) * &
                      6.3112071081188913e+02 + (PTEMP(II)**(3)) * &
                      (-9.4460854261685723e-05)) * (PRH(II)) * LOG(ZSULF(II))
         ZNTOTI(II) = ZNTOTI(II) + &
                      (1.1924791930673702e+04 + PTEMP(II) * &
                      (-1.1973824959206000e+02) + PTEMP(II)**2 * &
                      1.6888713097971020e-01 + PTEMP(II)**(-1) * &
                      1.8735938211539585e+02 + (PTEMP(II)**(3)) * &
                      5.0974564680442852e-04) * (PRH(II))
         ZNTOTI(II) = ZNTOTI(II) + &
                      (3.6409071302482083e+01 + PTEMP(II) * &
                      1.7919859306449623e-01 + PTEMP(II)**2 * &
                      (-1.0020116255895206e-03) + PTEMP(II)**(-1) * &
                      (-8.3521083354432303e+03) + (PTEMP(II)**(3)) * &
                      1.5879900546795635e-06) * PRH(II) * LOG(ZSULF(II))**(2)
         !
         ZNTOTI(II) = abs(ZNTOTI(II))
         !
         PRCI(II) = (-3.6318550637865524e-08 + PTEMP(II) * &
                   2.1740704135789128e-09 + PTEMP(II)**2 * &
                   (-8.5521429066506161e-12) + PTEMP(II)**3 * &
                   (-9.3538647454573390e-15)) 
         PRCI(II) = PRCI(II) + &
                   (2.1366936839394922e-08 + PTEMP(II) * &
                   (-2.4087168827395623e-10) + PTEMP(II)**2 * &
                   8.7969869277074319e-13 + PTEMP(II)**3 * &
                   (-1.0294466881303291e-15)) * LOG(PRH(II))**(-2) * LOG(ZSULF(II))**(-1)
         PRCI(II) = PRCI(II) + &
                   (-7.7804007761164303e-10 + PTEMP(II) * &
                   1.0327058173517932e-11 + PTEMP(II)**2 * &
                   (-4.2557697639692428e-14) + PTEMP(II)**3 * &
                   5.4082507061618662e-17) * LOG(PRH(II))**(-2)
         PRCI(II) = PRCI(II) + &
                   (3.2628927397420860e-12 + PTEMP(II) * &
                   (-7.6475692919751066e-14) + PTEMP(II)**2 * &
                   4.1985816845259788e-16 + PTEMP(II)**3 * &
                   (-6.2281395889592719e-19)) * LOG(PRH(II))**(-2) * LOG(ZSULF(II))
         PRCI(II) = PRCI(II) + &
                   (2.0442205540818555e-09 + PTEMP(II) * &
                   4.0441858911249830e-08 + PTEMP(II)**2 * &
                   (-3.3423487629482825e-10) + PTEMP(II)**3 * & 
                   6.8000404742985678e-13) * LOG(PRH(II))**(-1) * LOG(ZSULF(II))**(-2)
         PRCI(II) = PRCI(II) + &
                   (1.8381489183824627e-08 + PTEMP(II) * &
                   (-8.9853322951518919e-09) + PTEMP(II)**2 * & 
                   7.5888799566036185e-11 + PTEMP(II)**3 * & 
                   (-1.5823457864755549e-13)) * LOG(PRH(II))**(-1) * LOG(ZSULF(II))**(-1)
         PRCI(II) = PRCI(II) + &
                   (1.1795760639695057e-07 + PTEMP(II) * & 
                   (-8.1046722896375875e-10) + PTEMP(II)**2 * & 
                   9.1868604369041857e-14 + PTEMP(II)**3 * &
                   4.7882428237444610e-15) * LOG(PRH(II))**(-1)
         PRCI(II) = PRCI(II) + &
                   (-4.4028846582545952e-09 + PTEMP(II) * &
                   4.6541269232626618e-11 + PTEMP(II)**2 * &
                   (-1.1939929984285194e-13) + PTEMP(II)**3 * &
                   2.3602037016614437e-17) * LOG(PRH(II))**(-1) * LOG(ZSULF(II))
         PRCI(II) = PRCI(II) + &
                   (2.7885056884209128e-11 + PTEMP(II) * &
                   (-4.5167129624119121e-13) + PTEMP(II)**2 * &
                   1.6558404997394422e-15 + PTEMP(II)**3 * &
                   (-1.2037336621218054e-18)) * LOG(PRH(II))**(-1) * LOG(ZSULF(II))**2
         PRCI(II) = PRCI(II) + &
                   (-2.3719627171699983e-09 + PTEMP(II) * &
                   (-1.5260127909292053e-07) + PTEMP(II)**2 * & 
                   1.7177017944754134e-09 + PTEMP(II)**3 * &
                   (-4.7031737537526395e-12)) * LOG(ZSULF(II))**(-2)
         PRCI(II) = PRCI(II) + &
                   (-5.6946433724699646e-09 + PTEMP(II) * &
                   8.4629788237081735e-09 + PTEMP(II)**2 * &
                   (-1.7674135187061521e-10) + PTEMP(II)**3 * &
                   6.6236547903091862e-13) * LOG(ZSULF(II))**(-1)
         PRCI(II) = PRCI(II) + &
                   (-2.2808617930606012e-08 + PTEMP(II) * &
                   1.4773376696847775e-10 + PTEMP(II)**2 * &
                   (-1.3076953119957355e-13) + PTEMP(II)**3 * &
                   2.3625301497914000e-16) * LOG(ZSULF(II))
         PRCI(II) = PRCI(II) + &
                   (1.4014269939947841e-10 + PTEMP(II) * &
                   (-2.3675117757377632e-12) + PTEMP(II)**2 * & 
                   5.1514033966707879e-15 + PTEMP(II)**3 * &
                   (-4.8864233454747856e-18)) * LOG(ZSULF(II))**2
         PRCI(II) = PRCI(II) + &
                   (6.5464943868885886e-11 + PTEMP(II) * &
                   1.6494354816942769e-08 + PTEMP(II)**2 * &
                   (-1.7480097393483653e-10) + PTEMP(II)**3 * & 
                   4.7460075628523984e-13) * LOG(PRH(II))* LOG(ZSULF(II))**(-2)
         PRCI(II) = PRCI(II) + &
                   (8.4737893183927871e-09 + PTEMP(II) * &
                   (-6.0243327445597118e-09) + PTEMP(II)**2 * &
                   5.8766070529814883e-11 + PTEMP(II)**3 * & 
                   (-1.4926748560042018e-13)) * LOG(PRH(II))* LOG(ZSULF(II))**(-1)
         PRCI(II) = PRCI(II) + &
                   (1.0761964135701397e-07 + PTEMP(II) * &
                   (-1.0142496009071148e-09) + PTEMP(II)**2 * &
                   2.1337312466519190e-12 + PTEMP(II)**3 * &
                   1.6376014957685404e-15) * LOG(PRH(II))
         PRCI(II) = PRCI(II) + &
                   (-3.5621571395968670e-09 + PTEMP(II) * &
                   4.1175339587760905e-11 + PTEMP(II)**2 * &
                   (-1.3535372357998504e-13) + PTEMP(II)**3 * &
                   8.9334219536920720e-17) * LOG(PRH(II))* LOG(ZSULF(II))
         PRCI(II) = PRCI(II) + &
                   (2.0700482083136289e-11 + PTEMP(II) * &
                   (-3.9238944562717421e-13) + PTEMP(II)**2 * &
                   1.5850961422040196e-15 + PTEMP(II)**3 * &
                   (-1.5336775610911665e-18)) * LOG(PRH(II))* LOG(ZSULF(II))**2
         PRCI(II) = PRCI(II) + &
                   (1.8524255464416206e-09 + PTEMP(II) * &
                   (-2.1959816152743264e-11) + PTEMP(II)**2 * &
                   (-6.4478119501677012e-14) + PTEMP(II)**3 * &
                   5.5135243833766056e-16)* LOG(PRH(II))**2 * LOG(ZSULF(II))**(-1)
         PRCI(II) = PRCI(II) + &
                   (1.9349488650922679e-09 + PTEMP(II) * &
                   (-2.2647295919976428e-11) + PTEMP(II)**2 * &
                   9.2917479748268751e-14 + PTEMP(II)**3 * &
                   (-1.2741959892173170e-16))* LOG(PRH(II))**2
         PRCI(II) = PRCI(II) + &
                   (2.1484978031650972e-11 + PTEMP(II) * &
                   (-9.3976642475838013e-14) + PTEMP(II)**2 * &
                   (-4.8892738002751923e-16) + PTEMP(II)**3 * &
                   1.4676120441783832e-18)* LOG(PRH(II))**2 * LOG(ZSULF(II))
         PRCI(II) = PRCI(II) + &
                   (6.7565715216420310e-13 + PTEMP(II) * &
                   (-3.5421162549480807e-15) + PTEMP(II)**2 * &
                   (-3.4201196868693569e-18) + PTEMP(II)**3 * &
                   2.2260187650412392e-20)* LOG(PRH(II))**3 * LOG(ZSULF(II))
         !
         ZNACI(II) = ZAL(II) * ZNTOTI(II)
         !
         IF (ZNACI(II) .LT. 1.) THEN
            !
            !
            ZNACI(II)=1.0
            !
         END IF
         !
       END IF
       !
       ! Ion loss rate (1/s)
       !
       ZXLOSS(II) = ZCSI(II) + PJNUCI(II)
       !
       ! Recombination (here following Brasseur and Chatel, 1983)
       !
       ZRECOMB(II) = 6.0e-8 * sqrt(300./PTEMP(II)) + &
                     6.0e-26 * ZAIRN(II) * (300./PTEMP(II))**4
       !
       ! Small ion concentration in air (1/cm3) (following Dunne et al., 2016)
       ! max function is to avoid n_i to go practically zero at very high J_ion
       !
       ZNIPAIR(II) = max(0.01,(sqrt(ZXLOSS(II)**2.0 + &
                     4.0 * ZRECOMB(II) * ZIPR(II)) - ZXLOSS(II)) / (2.0 * ZRECOMB(II)))
       !
       ! Ion-induced nucleation rate
       ! Min function is to ensure that max function above does not cause J_ion to overshoot
       !
       PJNUCI(II) = min(ZIPR(II),ZNIPAIR(II)*PJNUCI(II)) 
       !
       IF (PJNUCI(II).LT.1.E-7) THEN
          !
          PJNUCI(II) = 0.0
          !
       END IF
       !
    END IF
    !
  END IF
  !
END DO
!
!
IF (NVERB .GE. 10) WRITE(*,*) '~~ CH_AER_MAATT_ION (fin): PRH =',MINVAL(PRH), MAXVAL(PRH)
IF (NVERB .GE. 10) WRITE(*,*) '~~ CH_AER_MAATT_ION (fin): PTEMP =',MINVAL(PTEMP), MAXVAL(PTEMP)
IF (NVERB .GE. 10) WRITE(*,*) '~~ CH_AER_MAATT_ION (fin): ZSULF =',MINVAL(ZSULF), MAXVAL(ZSULF)
IF (NVERB .GE. 10) WRITE(*,*) '~~ CH_AER_MAATT_ION (fin): PJNUCI =',MINVAL(PJNUCI), MAXVAL(PJNUCI)
IF (NVERB .GE. 10) WRITE(*,*) '~~ CH_AER_MAATT_ION (fin): ZKINTRI =',MINVAL(ZKINTRI), MAXVAL(ZKINTRI)
IF (NVERB .GE. 10) WRITE(*,*) '~~ CH_AER_MAATT_ION (fin): ZAL =',MINVAL(ZAL), MAXVAL(ZAL)
IF (NVERB .GE. 10) WRITE(*,*) '~~ CH_AER_MAATT_ION (fin): ZNTOTI =',MINVAL(ZNTOTI), MAXVAL(ZNTOTI)
IF (NVERB .GE. 10) WRITE(*,*) '~~ CH_AER_MAATT_ION (fin): PRCI =',MINVAL(PRCI), MAXVAL(PRCI)
IF (NVERB .GE. 10) WRITE(*,*) '~~ CH_AER_MAATT_ION (fin): ZXLOSS =',MINVAL(ZXLOSS), MAXVAL(ZXLOSS)
IF (NVERB .GE. 10) WRITE(*,*) '~~ CH_AER_MAATT_ION (fin): ZRECOMB =',MINVAL(ZRECOMB), MAXVAL(ZRECOMB)
IF (NVERB .GE. 10) WRITE(*,*) '~~ CH_AER_MAATT_ION (fin): ZNIPAIR =',MINVAL(ZNIPAIR), MAXVAL(ZNIPAIR)
!
RETURN
!
END SUBROUTINE CH_AER_MAATTANEN_IONIND

