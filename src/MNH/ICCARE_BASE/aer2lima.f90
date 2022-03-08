!MNH_LIC Copyright 1994-2019 CNRS, Meteo-France and Universite Paul Sabatier
!MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
!MNH_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!MNH_LIC for details. version 1.
!-----------------------------------------------------------------
!      ########################
       MODULE MODI_AER2LIMA 
!      ########################
!
INTERFACE
      SUBROUTINE AER2LIMA(PSVT, PRHODREF,PRV, PPABST, PTHT, PZZ)
!
USE MODD_CH_AEROSOL
USE MODD_DUST
USE MODD_SALT
USE MODD_NSV
USE MODD_CST
USE MODD_CONF,    ONLY : CPROGRAM
USE MODD_PARAM_n, ONLY : CACTCCN
USE MODD_PARAM_LIMA
USE MODE_AERO_PSD
USE MODE_SALT_PSD
USE MODE_DUST_PSD
USE MODI_CH_AER_EQSAM
USE MODI_DUSTLFI_n
USE MODI_SALTLFI_n
!
REAL, DIMENSION(:,:,:,:), INTENT(INOUT) :: PSVT
REAL, DIMENSION(:,:,:),   INTENT(IN)    :: PRHODREF, PRV, PPABST, PTHT, PZZ
!
END SUBROUTINE AER2LIMA 
!
END INTERFACE
!
END MODULE MODI_AER2LIMA 

!     ############################################
      SUBROUTINE AER2LIMA(PSVT, PRHODREF, PRV, PPABST, PTHT, PZZ)
!     ############################################
!
!
!!****  *AER2LIMA* lima CCN and IFN fields in case of orilam aerosols
!!
!!    PURPOSE
!!    -------
!!
!!    EXTERNAL
!!    --------
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!
!!    REFERENCE
!!    ---------
!!
!!    AUTHOR
!!    ------
!!      P. Tulet
!!
!!    MODIFICATIONS
!!    -------------
!!      Original         20/01/22
!! --------------------------------------------------------------------------
!
!*      0. DECLARATIONS
!          ------------
!
USE MODD_CH_AEROSOL
USE MODD_DUST
USE MODD_SALT
USE MODD_NSV
USE MODD_CST
USE MODD_CONF,    ONLY : CPROGRAM
USE MODD_PARAM_n, ONLY : CACTCCN
USE MODD_PARAM_LIMA
USE MODD_CH_M9_n, ONLY : CNAMES
USE MODE_AERO_PSD
USE MODE_SALT_PSD
USE MODE_DUST_PSD
USE MODI_CH_AER_EQSAM
USE MODI_DUSTLFI_n
USE MODI_SALTLFI_n
!
!
IMPLICIT NONE
!
!*      0.1   Declarations of dummy arguments :

REAL, DIMENSION(:,:,:,:), INTENT(INOUT) :: PSVT
REAL, DIMENSION(:,:,:),   INTENT(IN)    :: PRHODREF, PRV, PPABST, PTHT, PZZ

!       0.2  declaration of local variables

REAL, DIMENSION(SIZE(PSVT,1), SIZE(PSVT,2), SIZE(PSVT,3), NSP+NCARB+NSOA,JPMODE) :: ZCTOTA
REAL, DIMENSION(SIZE(PSVT,1), SIZE(PSVT,2), SIZE(PSVT,3))   :: ZSUM, ZSUM2, ZRATH2O, ZRATSO4, ZRATDST
REAL, DIMENSION(SIZE(PSVT,1), SIZE(PSVT,2), SIZE(PSVT,3),JPMODE) :: ZSIG_AER, ZRG_AER, ZN0_AER
REAL, DIMENSION(SIZE(PSVT,1), SIZE(PSVT,2), SIZE(PSVT,3),NMODE_SLT) :: ZSIG_SLT, ZRG_SLT, ZN0_SLT
REAL, DIMENSION(SIZE(PSVT,1), SIZE(PSVT,2), SIZE(PSVT,3),NMODE_DST) :: ZSIG_DST, ZRG_DST, ZN0_DST
REAL, DIMENSION(SIZE(PSVT,1), SIZE(PSVT,2), SIZE(PSVT,3),NMOD_CCN) :: ZCCN_SUM
REAL, DIMENSION(SIZE(PSVT,1), SIZE(PSVT,2), SIZE(PSVT,3),NMOD_IFN) :: ZIFN_SUM
REAL, DIMENSION(SIZE(PSVT,1), SIZE(PSVT,2), SIZE(PSVT,3))   :: ZPKM, ZPKH2O, ZTEMP, ZSAT, ZRH
REAL, DIMENSION(SIZE(PSVT,1), SIZE(PSVT,2), SIZE(PSVT,3),6) :: ZAER
REAL, DIMENSION(SIZE(PSVT,1), SIZE(PSVT,2), SIZE(PSVT,3),NSV) :: ZTOT
REAL, DIMENSION(SIZE(PSVT,1), SIZE(PSVT,2), SIZE(PSVT,3),JPMODE) :: ZOM
REAL, DIMENSION(NSV) :: ZMI
INTEGER :: JSV, JJ, JI, II, IJ, IK, JK
REAL :: ZCCNRADIUS, ZRATMASSH2O

ZCCNRADIUS = 0.04 ! to suppress the aitken mode  (µm)

IF ((CPROGRAM=="REAL  ").OR.(CPROGRAM=="IDEAL ")) CMINERAL = "EQSAM"
IF (CMINERAL /= 'NONE') THEN
     ZRATMASSH2O = 0.05 
ELSE
     ZRATMASSH2O = 0. 
END IF
ZMI(:) = 250.
ZMI(JP_AER_SO4)  = 98.
ZMI(JP_AER_NO3)  = 63.
ZMI(JP_AER_NH3)  = 17.
ZMI(JP_AER_H2O)  = 18.
ZCCN_SUM(:,:,:,:) = 0.
ZIFN_SUM(:,:,:,:) = 0.

! Anthopogenic part (orilam scheme)
!
IF (LORILAM) THEN

! moments (PSVT;ppp) --> concentration (PN3D;#/m3)
CALL PPP2AERO(PSVT(:,:,:,NSV_AERBEG:NSV_AEREND),PRHODREF,&
              PSIG3D=ZSIG_AER,PRG3D=ZRG_AER,PN3D=ZN0_AER,PCTOTA=ZCTOTA)

ZCTOTA=MAX(ZCTOTA,XMNH_TINY)

  IF ((CPROGRAM=="REAL  ").OR.(CPROGRAM=="IDEAL ")) THEN
   JP_CH_HNO3 = 0
   JP_CH_NH3  = 0
  DO JJ=1,SIZE(CNAMES)
    IF (CNAMES(JJ) == "HNO3") JP_CH_HNO3  = JJ
    IF (CNAMES(JJ) == "NH3")  JP_CH_NH3   = JJ
  END DO
  ZPKM(:,:,:) = 1E-3*PRHODREF(:,:,:) * 6.0221367E+23 / 28.9644
  ZPKH2O(:,:,:) = ZPKM(:,:,:)*1.6077*PRV(:,:,:)
!
! compute air temperature
  ZTEMP(:,:,:)   = PTHT(:,:,:)*((PPABST(:,:,:)/XP00)**(XRD/XCPD))

! compute relative humidity
  ZSAT(:,:,:)=0.611*EXP(17.2694*(ZTEMP(:,:,:)-273.16)/(ZTEMP(:,:,:)-35.86))
  ZSAT(:,:,:)=ZSAT(:,:,:)*1000.
  ZRH(:,:,:)=(ZPKH2O(:,:,:)/(ZPKM(:,:,:)*1.6077))*PPABST(:,:,:)/&
         &(0.622+(ZPKH2O(:,:,:)/(ZPKM(:,:,:)*1.6077)))/ZSAT(:,:,:)
  ZRH(:,:,:) = MIN(0.95, MAX(ZRH(:,:,:), .01)) ! until 0.95 thermodynamic code is not valid

!  Gas-particles equilibrium => H2O, SO4 aerosol mass
  DO JI=1,NSP
    ZTOT(:,:,:,JI)=ZCTOTA(:,:,:,JI,1)+ZCTOTA(:,:,:,JI,2)
    ZTOT(:,:,:,JI) = MAX(ZTOT(:,:,:,JI),XMNH_TINY)
  ENDDO
!
  ZAER(:,:,:,:) = 0.
  ZAER(:,:,:,1)=ZTOT(:,:,:,JP_AER_SO4)

! conversion ppp to µg/m3
  IF (JP_CH_NH3 .NE. 0) ZAER(:,:,:,2)=PSVT(:,:,:,NSV_CHEMBEG-1+JP_CH_NH3)*XNH3*1E-3*PRHODREF(:,:,:)/XMD
! conversion ppp to µg/m3
  IF (JP_CH_HNO3 .NE. 0) ZAER(:,:,:,3)=PSVT(:,:,:,NSV_CHEMBEG-1+JP_CH_HNO3)*XHNO3*1E-3*PRHODREF(:,:,:)/XMD
  ZAER(:,:,:,4)=ZTOT(:,:,:,JP_AER_H2O)
  ZAER(:,:,:,5)=ZTOT(:,:,:,JP_AER_NO3)
  ZAER(:,:,:,6)=ZTOT(:,:,:,JP_AER_NH3)
  ZAER(:,:,:,:)=MAX(ZAER(:,:,:,:),0.)

  DO IK=1,SIZE(PSVT,3)
    DO IJ=1,SIZE(PSVT,2)
      CALL CH_AER_EQSAM(ZAER(:,IJ,IK,:),ZRH(:,IJ,IK),PPABST(:,IJ,IK),ZTEMP(:,IJ,IK))
    END DO
  END DO
  ZTOT(:,:,:,JP_AER_SO4) = ZAER(:,:,:,1)
  ZTOT(:,:,:,JP_AER_H2O) = ZAER(:,:,:,4)
  ZTOT(:,:,:,JP_AER_NO3) = ZAER(:,:,:,5)
  ZTOT(:,:,:,JP_AER_NH3) = ZAER(:,:,:,6)

! Balance the mass according to size
  ZSUM(:,:,:) = 0.
  ZOM(:,:,:,:) = 0.
  DO JSV=1,JPMODE
    DO JJ=1,NSP
      ZSUM(:,:,:) = ZSUM(:,:,:) + ZCTOTA(:,:,:,JJ,JSV)
      ZOM(:,:,:,JSV) = ZOM(:,:,:,JSV) + ZCTOTA(:,:,:,JJ,JSV)
    ENDDO
  ENDDO

  DO JSV=1,JPMODE
      ZOM(:,:,:,JSV) = ZOM(:,:,:,JSV) / ZSUM(:,:,:)
  ENDDO

  DO JSV=1,JPMODE
    DO JJ=1,NSP
      ZCTOTA(:,:,:,JJ,JSV)=MAX(XMNH_TINY,ZTOT(:,:,:,JJ)*ZOM(:,:,:,JSV))
    END DO
  END DO

END IF !end part of init in case of IDEAL or REAL

! Compute mass ratio of sulfates, water and dusts
DO JSV=1,JPMODE
  ZRATH2O(:,:,:) = 0.
  ZRATSO4(:,:,:) = 0.
  ZRATDST(:,:,:) = 0.
  ZSUM(:,:,:)    = 0.
  ZSUM2(:,:,:)   = 0.

  DO II=1,NSP+NCARB+NSOA
    ZSUM(:,:,:) = ZSUM(:,:,:) + ZCTOTA(:,:,:,II,JSV)
  END DO

  ZSUM2(:,:,:) = ZSUM(:,:,:) - ZCTOTA(:,:,:,JP_AER_H2O,JSV)

  WHERE (ZSUM(:,:,:) .GT. 0.)
    ZRATH2O(:,:,:) = ZCTOTA(:,:,:,JP_AER_H2O,JSV) / ZSUM(:,:,:)
  END WHERE

  WHERE (ZSUM2(:,:,:) .GT. 0.)
    ZRATSO4(:,:,:) = ZCTOTA(:,:,:,JP_AER_SO4,JSV) / ZSUM2(:,:,:)
  END WHERE

  WHERE (ZSUM2(:,:,:) .GT. 0.)
    ZRATDST(:,:,:) = ZCTOTA(:,:,:,JP_AER_DST,JSV) / ZSUM2(:,:,:)
  END WHERE

! #/m3 --> #/kg
  ZN0_AER(:,:,:,JSV) = ZN0_AER(:,:,:,JSV) / PRHODREF(:,:,:)

! CCN_FREE initialization
! aerosol radius greater than ZCCNRADIUS µm to be considers as CCN 
! water mass greater than ZRATMASSH2O %

  IF (CACTCCN=="ABRK") THEN
! only one CCN_FREE mode (activation is not performed upon aerosol class but by physical paramters)
!
   WHERE (ZRG_AER(:,:,:,JSV) .GT. ZCCNRADIUS)
   !WHERE ((ZRG_AER(:,:,:,JSV) .GT. ZCCNRADIUS).AND.(ZRATH2O(:,:,:).GT.ZRATMASSH2O))
     ZCCN_SUM(:,:,:,1) =   ZCCN_SUM(:,:,:,1) + ZN0_AER(:,:,:,JSV) 
   END WHERE

  ELSE
  ! Sulfates 
  IF (NMOD_CCN .GE. 2) THEN
   WHERE ((ZRG_AER(:,:,:,JSV) .GT. ZCCNRADIUS).AND.(ZRATH2O(:,:,:).GT.ZRATMASSH2O)) 
     ZCCN_SUM(:,:,:,2) = ZCCN_SUM(:,:,:,2) + ZN0_AER(:,:,:,JSV) * ZRATSO4(:,:,:)
   END WHERE
  END IF

  ! Hyrdophylic aerosols
  IF (NMOD_CCN .GE. 3) THEN
   WHERE ((ZRG_AER(:,:,:,JSV) .GT. ZCCNRADIUS).AND.(ZRATH2O(:,:,:).GT.ZRATMASSH2O)) 
     ZCCN_SUM(:,:,:,3) = ZCCN_SUM(:,:,:,3) + ZN0_AER(:,:,:,JSV) * (1.-ZRATSO4(:,:,:))
   END WHERE
  END IF

END IF

! IFN_FREE initialization
  WHERE (ZRATH2O(:,:,:) .LE. ZRATMASSH2O) ! fraction of dust if low water
     ZIFN_SUM(:,:,:,1) = ZIFN_SUM(:,:,:,1) + ZN0_AER(:,:,:,JSV) * ZRATDST(:,:,:)
  END WHERE

! hydrophobic aerosols water mass less than 20%
  IF (NMOD_IFN .GE. 2) THEN
   WHERE (ZRATH2O(:,:,:) .LE. ZRATMASSH2O) ! hydrophobic aerosols can act as IFN
     ZIFN_SUM(:,:,:,2) = ZIFN_SUM(:,:,:,2) + ZN0_AER(:,:,:,JSV) * (1.- ZRATSO4(:,:,:))
   END WHERE
  END IF

END DO


ELSE ! keep lima class intiatialization
  IF (CACTCCN=="ABRK") THEN
! only one CCN_FREE mode (activation is not performed upon aerosol class but by physical paramters)
    IF (NMOD_CCN .GE. 2) &
    ZCCN_SUM(:,:,:,1) = ZCCN_SUM(:,:,:,1) + &
                        PSVT(:,:,:,NSV_LIMA_CCN_FREE+1) + PSVT(:,:,:,NSV_LIMA_CCN_ACTI+1)
    IF (NMOD_CCN .GE. 3) &
     ZCCN_SUM(:,:,:,1) = ZCCN_SUM(:,:,:,1) + &
                        PSVT(:,:,:,NSV_LIMA_CCN_FREE+2) + PSVT(:,:,:,NSV_LIMA_CCN_ACTI+2)
  
  ELSE
    IF (NMOD_CCN .GE. 2) &
     ZCCN_SUM(:,:,:,2) = PSVT(:,:,:,NSV_LIMA_CCN_FREE+1) + PSVT(:,:,:,NSV_LIMA_CCN_ACTI+1)

    IF (NMOD_CCN .GE. 3) &
     ZCCN_SUM(:,:,:,3) = PSVT(:,:,:,NSV_LIMA_CCN_FREE+2) + PSVT(:,:,:,NSV_LIMA_CCN_ACTI+2)
  END IF 

  IF (.NOT.(LDUST)) &
   ZIFN_SUM(:,:,:,1) = PSVT(:,:,:,NSV_LIMA_IFN_FREE)   + PSVT(:,:,:,NSV_LIMA_IFN_NUCL)

  IF (NMOD_IFN .GE. 2) &
   ZIFN_SUM(:,:,:,2) = PSVT(:,:,:,NSV_LIMA_IFN_FREE+1) + PSVT(:,:,:,NSV_LIMA_IFN_NUCL+1)

END IF ! end if sur LORILAM

! Sea Salt part
IF (LSALT) THEN
!
  IF (((CPROGRAM=="REAL  ").AND.(LSLTINIT).AND.(.NOT.LSLTCAMS)).OR.(CPROGRAM=="IDEAL ")) THEN
  CALL SALTLFI_n(PSVT(:,:,:,NSV_SLTBEG:NSV_SLTEND), PRHODREF, PZZ)
  END IF

! moments (PSVT;ppp) --> concentration (PN3D;#/m3)
  CALL PPP2SALT(PSVT(:,:,:,NSV_SLTBEG:NSV_SLTEND),PRHODREF,&
                PSIG3D=ZSIG_SLT,PRG3D=ZRG_SLT,PN3D=ZN0_SLT)
!
  DO JSV=1,NMODE_SLT
! #/m3 --> #/kg
    ZN0_SLT(:,:,:,JSV) = ZN0_SLT(:,:,:,JSV) / PRHODREF(:,:,:)

! CCN_FREE initialization
!
    WHERE (ZRG_SLT(:,:,:,JSV) .GT. ZCCNRADIUS)
     ZCCN_SUM(:,:,:,1) =  ZCCN_SUM(:,:,:,1) + ZN0_SLT(:,:,:,JSV)
    END WHERE
  END DO

ELSE ! keep lima class intiatialization for sea salt + ccn from orilam


ZCCN_SUM(:,:,:,1) = PSVT(:,:,:,NSV_LIMA_CCN_FREE) + PSVT(:,:,:,NSV_LIMA_CCN_ACTI)

END IF ! end if sur LSALT

! Dust part
IF (LDUST) THEN
  ! initatialization of dust if not macc
  IF (((CPROGRAM=="REAL  ").AND.(LDSTINIT).AND.(.NOT.LDSTCAMS)).OR.(CPROGRAM=="IDEAL ")) THEN
  CALL DUSTLFI_n(PSVT(:,:,:,NSV_DSTBEG:NSV_DSTEND), PRHODREF)
  END IF

! moments (PSVT;ppp) --> concentration (PN3D;#/m3)
  CALL PPP2DUST(PSVT(:,:,:,NSV_DSTBEG:NSV_DSTEND),PRHODREF,&
                PSIG3D=ZSIG_DST,PRG3D=ZRG_DST,PN3D=ZN0_DST)
!
  DO JSV=1,NMODE_DST

! #/m3 --> #/kg
    ZN0_DST(:,:,:,JSV) = ZN0_DST(:,:,:,JSV) / PRHODREF(:,:,:)

! IFN_FREE initialization (all dusts)
    ZIFN_SUM(:,:,:,1) = ZIFN_SUM(:,:,:,1) + ZN0_DST(:,:,:,JSV)

  END DO

ELSE ! keep lima class intiatialization

    ZIFN_SUM(:,:,:,1) = PSVT(:,:,:,NSV_LIMA_IFN_FREE) + PSVT(:,:,:,NSV_LIMA_IFN_NUCL)

END IF  ! endif sur LDUST

PSVT(:,:,:,NSV_LIMA_CCN_FREE)   = MAX(ZCCN_SUM(:,:,:,1) - PSVT(:,:,:,NSV_LIMA_CCN_ACTI), 0.)

IF (NMOD_CCN .GE. 2) &
PSVT(:,:,:,NSV_LIMA_CCN_FREE+1) = MAX(ZCCN_SUM(:,:,:,2) - PSVT(:,:,:,NSV_LIMA_CCN_ACTI+1), 0.)


IF (NMOD_CCN .GE. 3) &
PSVT(:,:,:,NSV_LIMA_CCN_FREE+2) = MAX(ZCCN_SUM(:,:,:,3) - PSVT(:,:,:,NSV_LIMA_CCN_ACTI+2), 0.)

PSVT(:,:,:,NSV_LIMA_IFN_FREE)   = MAX(ZIFN_SUM(:,:,:,1) - PSVT(:,:,:,NSV_LIMA_IFN_NUCL), 0.)
IF (NMOD_IFN .GE. 2) &
PSVT(:,:,:,NSV_LIMA_IFN_FREE+1) = MAX(ZIFN_SUM(:,:,:,2) - PSVT(:,:,:,NSV_LIMA_IFN_NUCL+1), 0.)

!
!
END SUBROUTINE AER2LIMA
