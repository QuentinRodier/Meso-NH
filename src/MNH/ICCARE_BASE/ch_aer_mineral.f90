!ORILAM_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!ORILAM_LIC This is part of the ORILAM software governed by the CeCILL-C licence
!ORILAM_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!ORILAM_LIC for details.
!!   ##########################
     MODULE MODI_CH_AER_MINERAL
!!   ##########################
!!
INTERFACE
!!
SUBROUTINE CH_AER_MINERAL(PCTOTG, PCTOTA, PRV, PDENAIR, PPRESSURE, PTEMP, PRC, POM,&
                           PCCTOT)
IMPLICIT NONE
REAL, DIMENSION(:,:),   INTENT(INOUT) :: PCTOTG, POM
REAL, DIMENSION(:,:,:), INTENT(INOUT) :: PCTOTA, PCCTOT 
REAL, DIMENSION(:),     INTENT(IN)    :: PRV, PDENAIR, PPRESSURE, PTEMP, PRC
!!
END SUBROUTINE CH_AER_MINERAL
!!
END INTERFACE
!!
END MODULE MODI_CH_AER_MINERAL
!!
!!
!!   #####################################################################################
     SUBROUTINE CH_AER_MINERAL(PCTOTG, PCTOTA, PRV, PDENAIR, PPRESSURE, PTEMP, PRC, POM, &
                               PCCTOT)
!!   #####################################################################################
!!
!!   PURPOSE
!!   -------
!!   solve the mineral thermodynamic balance
!!
!!   REFERENCE
!!   ---------
!!   None
!!
!!   AUTHOR
!!   ------
!!   P. Tulet  (GMEI)
!!
!!   MODIFICATIONS
!!   -------------
!!
!!   EXTERNAL
!!   --------
!!   None
!!
!-------------------------------------------------------------------------------
!
!*       0.     DECLARATIONS
!               ------------
!
USE MODD_CH_AEROSOL
USE MODI_CH_NNARES
USE MODI_CH_ARES
USE MODI_CH_ISOROPIA
USE MODI_CH_AER_THERMO
USE MODI_CH_AER_EQSAM
USE MODD_CST,          ONLY : XMNH_TINY
USE MODD_CONF,         ONLY : NVERB
!
IMPLICIT NONE
!
!*       0.1   Declarations of arguments
!
REAL, DIMENSION(:,:),   INTENT(INOUT) :: PCTOTG, POM
REAL, DIMENSION(:,:,:), INTENT(INOUT) :: PCTOTA, PCCTOT 
REAL, DIMENSION(:),     INTENT(IN)    :: PRV, PDENAIR, PPRESSURE, PTEMP, PRC
!
!*       0.2   Declarations of local variables
!
INTEGER :: JI,JJ
REAL, DIMENSION(SIZE(PCTOTA,1),NSP,JPMODE)     :: ZFRAC
REAL, DIMENSION(SIZE(PCTOTA,1),NSP)            :: ZTOT,ZTOTNEW, ZTOTGNEW 
REAL, DIMENSION(SIZE(PCTOTA,1),NSP+NCARB+NSOA) :: ZDEL
REAL, DIMENSION(SIZE(PCTOTA,1),6)              :: ZAER
REAL, DIMENSION(SIZE(PCTOTA,1))                :: ZPKM, ZPKH2O, ZSAT, ZRH
!
!-------------------------------------------------------------------------------
!
!*       1.    INITIALIZATION
!              --------------
!
ZPKM  (:) = 1E-3 * PDENAIR(:) * 6.0221367E+23 / 28.9644
ZPKH2O(:) = ZPKM(:) * 1.6077 * PRV(:)
!
! compute relative humidity
ZSAT(:) = 0.611*EXP(17.2694*(PTEMP(:)-273.16)/(PTEMP(:)-35.86))
ZSAT(:) = ZSAT(:)*1000.
ZRH (:) = (ZPKH2O(:)/(ZPKM(:)*1.6077))*PPRESSURE(:)/&
        & (0.622+(ZPKH2O(:)/(ZPKM(:)*1.6077)))/ZSAT(:)
ZRH(:)  = MIN(0.95, MAX(ZRH(:), .1)) ! until 0.95 thermodynamic code is not valid
!
! Mass need to be positive
PCTOTA(:,:,:) = MAX(PCTOTA(:,:,:),0.)
PCTOTG(:,:)   = MAX(PCTOTG(:,:),  0.)
ZTOTGNEW(:,:) = 0.
!
!-------------------------------------------------------------------------------
!
!*       2.    COMPUTE MINERAL CHEMICAL EQUILIBRIUM
!              ------------------------------------
!
!******************************************************************
! Calcul de la repartition des differentes especes entre les modes
! pour pouvoir conserver celle ci apres l'equilibre chimique
!******************************************************************
DO JI=1,NSP
  ZTOTNEW(:,JI)=0.
  ZTOT(:,JI)=PCTOTA(:,JI,1)+PCTOTA(:,JI,2)
  ZTOT(:,JI) = MAX(ZTOT(:,JI),1.E-40) 
  ZFRAC(:,JI,1)=PCTOTA(:,JI,1)/(ZTOT(:,JI)+1E-25)
  ZFRAC(:,JI,2)=1.-ZFRAC(:,JI,1)
  ! use SO4 fraction for all species (clean this up later)
  ZFRAC(:,JI,1)=ZFRAC(:,1,1)
  ZFRAC(:,JI,2)=1.-ZFRAC(:,JI,1)
ENDDO
!
ZTOTNEW(:,:) = ZTOT(:,:)
!
ZAER(:,1)=ZTOT(:,JP_AER_SO4)
ZAER(:,2)=PCTOTG(:,JP_AER_NH3g)
ZAER(:,3)=PCTOTG(:,JP_AER_NO3g)
ZAER(:,4)=ZTOT(:,JP_AER_H2O)
!ZAER(:,4)=0.
ZAER(:,5)=ZTOT(:,JP_AER_NO3)
ZAER(:,6)=ZTOT(:,JP_AER_NH3)
ZAER(:,:)=MAX(ZAER(:,:),0.)

! switch here for  ARES (ARES), Neuronal ARES (NARES), ISOROPIA (ISPIA)
IF (CMINERAL == 'NARES') THEN
  CALL CH_NNARES(ZAER,ZRH, PDENAIR, PPRESSURE, PTEMP, PRC)
  ZAER(:,:)=MAX(ZAER(:,:),0.)
  ZTOTNEW(:,JP_AER_SO4)=ZAER(:,1)
  ZTOTGNEW(:,JP_AER_NH3g)=ZAER(:,2)
  ZTOTGNEW(:,JP_AER_NO3g)=ZAER(:,3)
  ZTOTNEW(:,JP_AER_H2O)=ZAER(:,4)
  ZTOTNEW(:,JP_AER_NO3)=ZAER(:,5)
  ZTOTNEW(:,JP_AER_NH3)=ZAER(:,6)

! Especes phase gazeuse
!PCTOTG(:,JP_AER_SO4g)=0.         !H2SO4(g)
ELSE IF (CMINERAL == 'ARES') THEN
! test of stability
!DO III=1, 5
  CALL CH_ARES(ZAER,ZRH, PDENAIR, PPRESSURE, PTEMP, PRC)
  ZAER(:,:) = MAX(ZAER(:,:),0.)
  ZTOTNEW(:,JP_AER_SO4)=ZAER(:,1)
  ZTOTGNEW(:,JP_AER_NH3g)=ZAER(:,2)
  ZTOTGNEW(:,JP_AER_NO3g)=ZAER(:,3)
  ZTOTNEW(:,JP_AER_H2O)=ZAER(:,4)
  ZTOTNEW(:,JP_AER_NO3)=ZAER(:,5)
  ZTOTNEW(:,JP_AER_NH3)=ZAER(:,6)
!ENDDO
!
ELSE IF (CMINERAL == 'ISPIA') THEN
!
  CALL CH_ISOROPIA(ZAER,ZRH, PDENAIR, PPRESSURE, PTEMP, PRC)

  ZAER(:,:)=MAX(ZAER(:,:),0.)
  ZTOTNEW(:,JP_AER_SO4)=ZAER(:,1)
  ZTOTGNEW(:,JP_AER_NH3g)=ZAER(:,2)
  ZTOTGNEW(:,JP_AER_NO3g)=ZAER(:,3)
  ZTOTNEW(:,JP_AER_H2O)=ZAER(:,4)
  ZTOTNEW(:,JP_AER_NO3)=ZAER(:,5)
  ZTOTNEW(:,JP_AER_NH3)=ZAER(:,6)
!

ELSE IF (CMINERAL == 'TABUL') THEN

  CALL CH_AER_THERMO(ZAER,ZRH, PDENAIR, PPRESSURE, PTEMP, PRC)

  ZAER(:,:)=MAX(ZAER(:,:),0.)
  ZTOTNEW(:,JP_AER_SO4)=ZAER(:,1)
  ZTOTGNEW(:,JP_AER_NH3g)=ZAER(:,2)
  ZTOTGNEW(:,JP_AER_NO3g)=ZAER(:,3)
  ZTOTNEW(:,JP_AER_H2O)=ZAER(:,4)
  ZTOTNEW(:,JP_AER_NO3)=ZAER(:,5)
  ZTOTNEW(:,JP_AER_NH3)=ZAER(:,6)

ELSE IF (CMINERAL == 'EQSAM') THEN

  CALL CH_AER_EQSAM(ZAER,ZRH, PPRESSURE, PTEMP)

  ZAER(:,:)=MAX(ZAER(:,:),0.)
  ZTOTNEW(:,JP_AER_SO4)=ZAER(:,1)
  ZTOTGNEW(:,JP_AER_NH3g)=ZAER(:,2)
  ZTOTGNEW(:,JP_AER_NO3g)=ZAER(:,3)
  ZTOTNEW(:,JP_AER_H2O)=ZAER(:,4)
  ZTOTNEW(:,JP_AER_NO3)=ZAER(:,5)
  ZTOTNEW(:,JP_AER_NH3)=ZAER(:,6)
!
ELSE

IF (NVERB==10) PRINT *,' WARNING WARNING WARNING WARNING WARNING WARNING'
IF (NVERB==10) PRINT *,' PAS D EQUILIBRE THERMODYNAMIQUE ENTRE LES MINERAUX'
IF (NVERB==10) PRINT *,' WARNING WARNING WARNING WARNING WARNING WARNING'
  ZTOTNEW(:,:) = MAX(0.,ZTOT(:,:))

ENDIF
! Especes phase gazeuse
ZTOTGNEW(:,JP_AER_SO4g)=0.         !H2SO4(g)
ZTOTNEW(:,:) = MAX(0.,ZTOTNEW(:,:))
!
ZDEL(:,:)=0.
! Concentration des especes 'totales' presentes dans l'aerosol
ZDEL(:,1:NSP)=ZTOTNEW(:,1:NSP)-ZTOT(:,1:NSP)
!
! Calcul de la nouvelle composition chimique
! de chacun des modes apres equilibre chimique
!
DO JI=1,JPMODE
  DO JJ=1,NSP

  PCTOTA(:,JJ,JI)=MAX(XMNH_TINY,PCTOTA(:,JJ,JI)+ZFRAC(:,JJ,JI)*ZDEL(:,JJ))
  ! répartition entre les modes en fonction de la surface des aerosols (facteur
  ! omega)
  !  PCTOTA(:,JJ,JI)=MAX(XMNH_TINY,PCTOTA(:,JJ,JI)+ZDEL(:,JJ)*POM(:,JI)) 
  ! PCTOTA(:,JJ,JI)=MAX(XMNH_TINY,ZTOTNEW(:,JJ)*POM(:,JI))
 ENDDO
 !PCTOTA(:,JP_AER_SO4,JI) = ZCTOTA(:,JP_AER_SO4,JI)
ENDDO 
!
DO JJ=1,NSP
  PCTOTG(:,JJ)=MAX(XMNH_TINY,PCTOTG(:,JJ)-ZDEL(:,JJ)) 
ENDDO
!
END SUBROUTINE CH_AER_MINERAL
