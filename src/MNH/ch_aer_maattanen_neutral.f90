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
MODULE MODI_CH_AER_MAATTANEN_NEUTRAL
!!    ################################ 
!!
INTERFACE
  !!
  SUBROUTINE CH_AER_MAATTANEN_NEUTRAL(PRH,PTEMP,PSULF,PJNUCN,PRCN)
  IMPLICIT NONE
  !!
  REAL, DIMENSION(:), INTENT(IN)    :: PRH,PTEMP,PSULF
  REAL, DIMENSION(:), INTENT(INOUT) :: PJNUCN, PRCN
  !!
  !!
  END SUBROUTINE CH_AER_MAATTANEN_NEUTRAL
  !!
END INTERFACE
!!
END MODULE MODI_CH_AER_MAATTANEN_NEUTRAL
!!
!! #########################################################################
SUBROUTINE CH_AER_MAATTANEN_NEUTRAL(PRH,PTEMP,PSULF,PJNUCN,PRCN)
!###########################################################
!
!!                   
!!    PURPOSE
!!    -------
!!      
!!    Compute nucleation rate for binary H2SO4/H2O
!!    This is the Määttänen parametrization (2018) 
!!    This is the neutral particle formation part
!!
!!    Valid for : 
!!    165 < T < 400     (K)
!!    0.001 < RH < 100          (%)
!!    10⁴ < [H2SO4]gas < 10¹³  (molec/cm3)
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
REAL, DIMENSION(:), INTENT(INOUT) :: PJNUCN, PRCN                ! Nucleation rate (#/cm3/s) , Critical cluster radius (nm)
!
!*       0.2   Declarations of local variables :
!
REAL, DIMENSION(SIZE(PSULF,1)) :: ZSULF               ! Sulfuric acid concentration (molec/cm3) 
REAL, DIMENSION(SIZE(PSULF,1)) :: ZAL                 ! Mole fraction of H2SO4 in the critical cluster
REAL, DIMENSION(SIZE(PSULF,1)) :: ZNTOTN              ! Total number of molec in the neutral critical cluster 
REAL, DIMENSION(SIZE(PSULF,1)) :: ZKINTRN             ! Threshold sulfuric acid for neutral kinetic nucleation
REAL, DIMENSION(SIZE(PSULF,1)) :: ZNACN                ! Sulfuric acid molecules in the neutral critical cluster
!
LOGICAL                        :: GKINETICN           ! True if kinetic neutral nucleation
!
INTEGER                         :: II, ITEST          ! Tests
!
IF (NVERB .GE. 10) WRITE(*,*) '~~ CH_AER_MAATT_NEUT (deb): PSULF =',MINVAL(PSULF), MAXVAL(PSULF)
IF (NVERB .GE. 10) WRITE(*,*) '~~ CH_AER_MAATT_NEUT (deb): (XAVOGADRO*1.E-12) =',(XAVOGADRO*1.E-12)
IF (NVERB .GE. 10) WRITE(*,*) '~~ CH_AER_MAATT_NEUT (deb): XH2SO4=', XH2SO4
IF (NVERB .GE. 10) WRITE(*,*) '~~ CH_AER_MAATT_NEUT (deb): PTEMP =',MINVAL(PTEMP), MAXVAL(PTEMP)
IF (NVERB .GE. 10) WRITE(*,*) '~~ CH_AER_MAATT_NEUT (deb): PRH =',MINVAL(PRH), MAXVAL(PRH)
!
!----------------------------------------------------------------------------
!
! Parameters initialization 
!
ZAL(:)       = 0.17      ! must vary between 0 and 1 
PJNUCN(:)     = 1E-7      ! must vary between 10E-7 and 10E10 cm3.s-1
PRCN(:)      = 2.8E-10   ! (meters) must vary between 0.28 and 1.2 nm
ZNACN(:)      = 0.
ZNTOTN(:)     = 10.     ! must vary between 1 and 200 molecules
ZKINTRN(:)   = 0.
GKINETICN    = .FALSE.
!
!    a. Sulfuric acid concentration definition: ZSULF from ug/m3 to molec/cm3
!
ZSULF(:) = PSULF(:)*(XAVOGADRO*1.E-12) / XH2SO4
!
!    b. Restrictions for parametrization
!
!
ITEST = 0.
!
DO II = 1, SIZE(PSULF,1)
   IF ((PRH(II) > 0.001).AND.(PTEMP(II)>165.).AND.(ZSULF(II)>1E4)) THEN
      ITEST = ITEST+1
   END IF
END DO
!
DO II = 1, SIZE(PSULF,1)
  !
  IF ( (PRH(II)>0.001) .AND. (PTEMP(II)>165.) .AND. (ZSULF(II)>1E4) ) THEN
        !
        !  1. Mole fraction of H2SO4 in the critical cluster (eq 1): composition
        !
        ZAL(II) = 7.9036365428891719E-1-2.8414059650092153E-3*PTEMP(II)+&
                 1.4976802556584141E-2*LOG(PRH(II))-2.4511581740839115E-4*PTEMP(II)*LOG(PRH(II))+&
                 3.4319869471066424E-3*(LOG(PRH(II)))**2-2.8799393617748428E-5*PTEMP(II)*(LOG(PRH(II)))**2+&
                 3.0174314126331765E-4*(LOG(PRH(II)))**3-2.2673492408841294E-6*PTEMP(II)*(LOG(PRH(II)))**3-&
                 4.3948464567032377E-3*LOG(ZSULF(II))+5.3305314722492146E-5*PTEMP(II)*LOG(ZSULF(II))
        !
        !  2. Nucleation rate calculation in part.cm-3.s-1 (eq 2)
        !
        ! a) Kinetic limit check
        !
        IF (PRH(II) .GE. 1.e-2 .AND. PRH(II) .LE. 1.) THEN
          !
          ZKINTRN(II) = exp(7.8920778706888086e+1 + 7.3665492897447082*PRH(II) - 1.2420166571163805e+4/PTEMP(II) &
                      & + (-6.1831234251470971e+2*PRH(II))/PTEMP(II) - 2.4501159970109945e-2*PTEMP(II)           &
                      & -1.3463066443605762e-2*PRH(II)*PTEMP(II) + 8.3736373989909194e-06*PTEMP(II)**2           &
                      & -1.4673887785408892*Log(PRH(II)) + (-3.2141890006517094e+1*Log(PRH(II)))/PTEMP(II)       &
                      & + 2.7137429081917556e-3*PTEMP(II)*Log(PRH(II))) !1/cm3     
          !
          IF (ZKINTRN(II).LT.ZSULF(II)) GKINETICN = .TRUE.
          !
        END IF
        !
        IF (PRH(II) .GE. 1.e-4  .AND. PRH(II) .LT. 1.e-2) THEN
          !
          ZKINTRN(II) = exp(7.9074383049843647e+1 - 2.8746005462158347e+1*PRH(II) - 1.2070272068458380e+4/PTEMP(II) &
                      & + (-5.9205040320056632e+3*PRH(II))/PTEMP(II) - 2.4800372593452726e-2*PTEMP(II) &
                      & -4.3983007681295948e-2*PRH(II)*PTEMP(II) + 2.5943854791342071e-5*PTEMP(II)**2   &
                      & -2.3141363245211317*Log(PRH(II)) + (9.9186787997857735e+1*Log(PRH(II)))/PTEMP(II) &
                      & + 5.6819382556144681e-3*PTEMP(II)*Log(PRH(II))) !1/cm3
          !
          IF (ZKINTRN(II).LT.ZSULF(II)) GKINETICN = .TRUE.
          !
        END IF
        !
        IF (PRH(II) .GE. 5.e-6  .AND. PRH(II) .LT. 1.e-4) THEN
          !
          ZKINTRN(II) = exp(8.5599712000361677e+1 + 2.7335119660796581e+3*PRH(II) - 1.1842350246291651e+4/PTEMP(II) &
                      & + (-1.2439843468881438e+6*PRH(II))/PTEMP(II) - 5.4536964974944230e-2*PTEMP(II) &
                      & + 5.0886987425326087*PRH(II)*PTEMP(II) + 7.1964722655507067e-5*PTEMP(II)**2   &
                      & -2.4472627526306372*Log(PRH(II)) + (1.7561478001423779e+2*Log(PRH(II)))/PTEMP(II) &
                      & + 6.2640132818141811e-3*PTEMP(II)*Log(PRH(II))) !1/cm3
          !
          IF(ZKINTRN(II).LT.ZSULF(II)) GKINETICN = .TRUE.
        !
        END IF
        !
        IF (GKINETICN) THEN
          !
          ! Nucleation rate calculation if dimer
          !
          PJNUCN(II) = 1.E6*(2.*0.3E-9)**2.*sqrt(8.*3.141593*1.38E-23*(1./(1.661e-27*98.07)+1./(1.661e-27*98.07))) &
                    & /2.*sqrt(PTEMP(II))*ZSULF(II)**2.
          !
          ZNTOTN(II) = 1. !set to 1 
          !
          ZNACN(II) = 1.   ! The critical cluster contains one molecule, but the produced cluster contains 2 molecules
          !
          ZAL(II) = ZNACN(II) / ZNTOTN(II)  ! so also set this to 1
          !
          PRCN(II) = 0.3E-9
          !
        ELSE
          !
          ! c) Nucleation rate calculation if not dimer
          !
          PJNUCN(II) = 2.1361182605986115e-1 + &
                    & 3.3827029855551838 * PTEMP(II) - &
                    & 3.2423555796175563e-2 * PTEMP(II)**2 +  &
                    & 7.0120069477221989e-5 * PTEMP(II)**3 + &
                    & 8.0286874752695141 / ZAL(II) &
                    & -2.6939840579762231e-1 * LOG(PRH(II)) + &
                    & 1.6079879299099518 * PTEMP(II) * LOG(PRH(II)) &
                    & -1.9667486968141933e-2 * PTEMP(II)**2 * LOG(PRH(II)) +  &
                    & 5.5244755979770844e-5 * PTEMP(II)**3 * LOG(PRH(II)) + &
                    & (7.8884704837892468 * LOG(PRH(II))) / ZAL(II) + &
                    & 4.6374659198909596 * LOG(PRH(II))**2 - &
                    & 8.2002809894792153e-2 * PTEMP(II) * LOG(PRH(II))**2 +  &
                    & 8.5077424451172196e-4 * PTEMP(II)**2 * LOG(PRH(II))**2   &
                    & -2.6518510168987462e-6 * PTEMP(II)**3 * LOG(PRH(II))**2 +  &
                    & (-1.4625482500575278 * LOG(PRH(II))**2)/ZAL(II) - &
                    & 5.2413002989192037e-1 * LOG(PRH(II))**3 +  &
                    & 5.2755117653715865e-3 * PTEMP(II) * LOG(PRH(II))**3   &
                    & -2.9491061332113830e-6 * PTEMP(II)**2 * LOG(PRH(II))**3   &
                    & -2.4815454194486752e-8 * PTEMP(II)**3 * LOG(PRH(II))**3 +  &
                    & (-5.2663760117394626e-2 * LOG(PRH(II))**3) / ZAL(II) +  &
                    & 1.6496664658266762 * LOG(ZSULF(II))   &
                    & -8.0809397859218401e-1 * PTEMP(II) * LOG(ZSULF(II)) +  &
                    & 8.9302927091946642e-3 * PTEMP(II)**2 * LOG(ZSULF(II))   &
                    & -1.9583649496497497e-5 * PTEMP(II)**3 * LOG(ZSULF(II)) +  &
                    & (-8.9505572676891685 * LOG(ZSULF(II))) / ZAL(II)   &
                    & -3.0025283601622881e+1 * LOG(PRH(II)) * LOG(ZSULF(II)) +  &
                    & 3.0783365644763633e-1 * PTEMP(II) * LOG(PRH(II)) * LOG(ZSULF(II))   &
                    & -7.4521756337984706e-4 * PTEMP(II)**2 * LOG(PRH(II)) * LOG(ZSULF(II))   &
                    & -5.7651433870681853e-7 * PTEMP(II)**3 * LOG(PRH(II)) * LOG(ZSULF(II)) +  &
                    & (1.2872868529673207 * LOG(PRH(II)) * LOG(ZSULF(II))) / ZAL(II)   &
                    & -6.1739867501526535e-1 * LOG(PRH(II))**2 * LOG(ZSULF(II)) +  &
                    & 7.2347385705333975e-3 * PTEMP(II) * LOG(PRH(II))**2 * LOG(ZSULF(II))   &
                    & -3.0640494530822439e-5 * PTEMP(II)**2 * LOG(PRH(II))**2 * LOG(ZSULF(II)) +  &
                    & 6.5944609194346214e-8 * PTEMP(II)**3 * LOG(PRH(II))**2 * LOG(ZSULF(II)) +  &
                    & (-2.8681650332461055e-2 * LOG(PRH(II))**2 * LOG(ZSULF(II))) / ZAL(II) +  &
                    & 6.5213802375160306 * LOG(ZSULF(II))**2   &
                    & -4.7907162004793016e-2 * PTEMP(II) * LOG(ZSULF(II))**2   &
                    & -1.0727890114215117e-4 * PTEMP(II)**2 * LOG(ZSULF(II))**2 +  &
                    & 5.6401818280534507e-7 * PTEMP(II)**3 * LOG(ZSULF(II))**2 +  &
                    & (5.4113070888923009e-1 * LOG(ZSULF(II))**2) / ZAL(II) +  &
                    & 5.2062808476476330e-1 * LOG(PRH(II)) * LOG(ZSULF(II))**2   &
                    & -6.0696882500824584e-3 * PTEMP(II) * LOG(PRH(II)) * LOG(ZSULF(II))**2 +  &
                    & 2.3851383302608477e-5 * PTEMP(II)**2 * LOG(PRH(II)) * LOG(ZSULF(II))**2   &
                    & -1.5243837103067096e-8 * PTEMP(II)**3 * LOG(PRH(II)) * LOG(ZSULF(II))**2 +  &
                    & (-5.6543192378015687e-2 * LOG(PRH(II)) * LOG(ZSULF(II))**2) / ZAL(II)   &
                    & -1.1630806410696815e-1 * LOG(ZSULF(II))**3 +  &
                    & 1.3806404273119610e-3 * PTEMP(II) * LOG(ZSULF(II))**3   &
                    & -2.0199865087650833e-6 * PTEMP(II)**2 * LOG(ZSULF(II))**3   &
                    & -3.0200284885763192e-9 * PTEMP(II)**3 * LOG(ZSULF(II))**3 +  &
                    & (-6.9425267104126316e-3 * LOG(ZSULF(II))**3) / ZAL(II)
          !
          PJNUCN(II)=MIN(5.0E1,PJNUCN(II)) 
          PJNUCN(II)=EXP(PJNUCN(II))
          !
          ! 3. Molecules number in the cluster calculation
          ! 
          ZNTOTN(II) = -3.5863435141979573e-3 - &
                     & 1.0098670235841110e-1*PTEMP(II) + &
                     & 8.9741268319259721e-4*PTEMP(II)**2 - &
                     & 1.4855098605195757e-6*PTEMP(II)**3 &
                     & - 1.2080330016937095e-1/ZAL(II) + &
                     & 1.1902674923928015e-3*LOG(PRH(II)) - &
                     & 1.9211358507172177e-2*PTEMP(II)*LOG(PRH(II)) + &
                     & 2.4648094311204255e-4*PTEMP(II)**2*LOG(PRH(II))- &
                     & 7.5641448594711666e-7*PTEMP(II)**3*LOG(PRH(II)) + &
                     & (-2.0668639384228818e-02*LOG(PRH(II)))/ZAL(II) - &
                     & 3.7593072011595188e-2*LOG(PRH(II))**2 + &
                     & 8.0993182774415718e-4*PTEMP(II)*LOG(PRH(II))**2  &
                     & -9.5698412164297149e-6*PTEMP(II)**2*LOG(PRH(II))**2 + &
                     & 3.7163166416110421e-8*PTEMP(II)**3*LOG(PRH(II))**2 + &
                     & (1.1026579525210847e-2*LOG(PRH(II))**2)/ZAL(II) + &
                     & 1.1530844115561925e-2*LOG(PRH(II))**3  &
                     & -1.8083253906466668e-4*PTEMP(II)*LOG(PRH(II))**3 +&
                     & 8.0213604053330654e-7*PTEMP(II)**2*LOG(PRH(II))**3  &
                     & -8.5797885383051337e-10*PTEMP(II)**3*LOG(PRH(II))**3 + &
                     & (1.0243693899717402e-3*LOG(PRH(II))**3)/ZAL(II) &
                     & -1.7248695296299649e-2*LOG(ZSULF(II)) + &
                     & 1.1294004162437157e-2*PTEMP(II)*LOG(ZSULF(II)) &
                     & -1.2283640163189278e-4*PTEMP(II)**2*LOG(ZSULF(II)) + &
                     & 2.7391732258259009e-7*PTEMP(II)**3*LOG(ZSULF(II)) +  &
                     & (6.8505583974029602e-2*LOG(ZSULF(II)))/ZAL(II) + &
                     & 2.9750968179523635e-1*LOG(PRH(II))*LOG(ZSULF(II)) &
                     & -3.6681154503992296e-3*PTEMP(II)*LOG(PRH(II))*LOG(ZSULF(II)) + &
                     & 1.0636473034653114e-5*PTEMP(II)**2*LOG(PRH(II))*LOG(ZSULF(II)) +  &
                     & 5.8687098466515866e-9*PTEMP(II)**3*LOG(PRH(II))*LOG(ZSULF(II)) + &
                     & (-5.2028866094191509e-3*LOG(PRH(II))*LOG(ZSULF(II)))/ZAL(II) +  &
                     & 7.6971988880587231e-4*LOG(PRH(II))**2*LOG(ZSULF(II)) - &
                     & 2.4605575820433763e-5*PTEMP(II)*LOG(PRH(II))**2*LOG(ZSULF(II)) +  &
                     & 2.3818484400893008e-7*PTEMP(II)**2*LOG(PRH(II))**2*LOG(ZSULF(II))   &
                     & -8.8474102392445200e-10*PTEMP(II)**3*LOG(PRH(II))**2*LOG(ZSULF(II)) +  &
                     & (-1.6640566678168968e-4*LOG(PRH(II))**2*LOG(ZSULF(II)))/ZAL(II) - &
                     & 7.7390093776705471e-2*LOG(ZSULF(II))**2 + &
                     & 5.8220163188828482e-4*PTEMP(II)*LOG(ZSULF(II))**2 + &
                     & 1.2291679321523287e-6*PTEMP(II)**2*LOG(ZSULF(II))**2  &
                     & -7.4690997508075749e-9*PTEMP(II)**3*LOG(ZSULF(II))**2 + &
                     & (-5.6357941220497648e-3*LOG(ZSULF(II))**2)/ZAL(II)  &
                     & -4.7170109625089768e-3*LOG(PRH(II))*LOG(ZSULF(II))**2 + & 
                     & 6.9828868534370193e-5*PTEMP(II)*LOG(PRH(II))*LOG(ZSULF(II))**2  &
                     & -3.1738912157036403e-7*PTEMP(II)**2*LOG(PRH(II))*LOG(ZSULF(II))**2 + &
                     & 2.3975538706787416e-10*PTEMP(II)**3*LOG(PRH(II))*LOG(ZSULF(II))**2 + &
                     & (4.2304213386288567e-4*LOG(PRH(II))*LOG(ZSULF(II))**2)/ZAL(II) + &
                     & 1.3696520973423231e-3*LOG(ZSULF(II))**3  &
                     & -1.6863387574788199e-5*PTEMP(II)*LOG(ZSULF(II))**3 + &
                     & 2.7959499278844516e-8*PTEMP(II)**2*LOG(ZSULF(II))**3 + &
                     & 3.9423927013227455e-11*PTEMP(II)**3*LOG(ZSULF(II))**3 + &
                     & (8.6136359966337272e-5*LOG(ZSULF(II))**3)/ZAL(II)
          ! 
          ZNTOTN(II)=EXP(ZNTOTN(II))
          !
          ! 4. Critical cluster size calculation (in meters)
          !
          PRCN(II) = EXP(-22.378268374023630 + 0.44462953606125100 *ZAL(II) + 0.33499495707849131 * LOG(ZNTOTN(II))) 
          !
          ! 5. Acid molecules in nucleation regime
          !
          ZNACN(II) = ZAL(II) * ZNTOTN(II) 
          !
          IF (ZNACN(II) .lt. 1.) THEN
            !
             ! print *, 'Warning: number of acid molecules < 1 in nucleation regime, setting na_n=1'
             !
             ZNACN(II)=1.0
             !
          END IF
          !
        END IF
        !
        ! 3. Restrictions for nucleation rates
        !
         IF (PJNUCN(II) .LT. 1.0E-7) PJNUCN(II) = 0.0
        !
        !
  END IF
END DO
!
!
IF (NVERB .GE. 10) WRITE(*,*) '~~ CH_AER_MAATT_NEUT (fin): PRH =',MINVAL(PRH), MAXVAL(PRH)
IF (NVERB .GE. 10) WRITE(*,*) '~~ CH_AER_MAATT_NEUT (fin): PTEMP =',MINVAL(PRH), MAXVAL(PRH)
IF (NVERB .GE. 10) WRITE(*,*) '~~ CH_AER_MAATT_NEUT (fin): ZSULF =',MINVAL(ZSULF), MAXVAL(ZSULF)
IF (NVERB .GE. 10) WRITE(*,*) '~~ CH_AER_MAATT_NEUT (fin): PJNUCN =',MINVAL(PJNUCN), MAXVAL(PJNUCN)
IF (NVERB .GE. 10) WRITE(*,*) '~~ CH_AER_MAATT_NEUT (fin): ZAL =',MINVAL(ZAL), MAXVAL(ZAL)
IF (NVERB .GE. 10) WRITE(*,*) '~~ CH_AER_MAATT_NEUT (fin): ZNTOTN =',MINVAL(ZNTOTN), MAXVAL(ZNTOTN)
IF (NVERB .GE. 10) WRITE(*,*) '~~ CH_AER_MAATT_NEUT (fin): PRCN =',MINVAL(PRCN), MAXVAL(PRCN)
!
RETURN
!
END SUBROUTINE CH_AER_MAATTANEN_NEUTRAL

