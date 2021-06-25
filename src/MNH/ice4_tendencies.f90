!MNH_LIC Copyright 1994-2021 CNRS, Meteo-France and Universite Paul Sabatier
!MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
!MNH_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!MNH_LIC for details. version 1.
!-----------------------------------------------------------------
MODULE MODI_ICE4_TENDENCIES
INTERFACE
SUBROUTINE ICE4_TENDENCIES(KSIZE, KIB, KIE, KIT, KJB, KJE, KJT, KKB, KKE, KKT, KKL, &
                          &KRR, ODSOFT, PCOMPUTE, &
                          &OWARM, HSUBG_RC_RR_ACCR, HSUBG_RR_EVAP, &
                          &HSUBG_AUCV_RC, HSUBG_AUCV_RI, HSUBG_PR_PDF, &
                          &PEXN, PRHODREF, PLVFACT, PLSFACT, K1, K2, K3, &
                          &PPRES, PCF, PSIGMA_RC, &
                          &PCIT, &
                          &PT, PTHT, &
                          &PRVT, PRCT, PRRT, PRIT, PRST, PRGT, PRHT, &
                          &PRVHENI_MR, PRRHONG_MR, PRIMLTC_MR, PRSRIMCG_MR, &
                          &PRCHONI, PRVDEPS, PRIAGGS, PRIAUTS, PRVDEPG, &
                          &PRCAUTR, PRCACCR, PRREVAV, &
                          &PRCRIMSS, PRCRIMSG, PRSRIMCG, PRRACCSS, PRRACCSG, PRSACCRG, PRSMLTG, PRCMLTSR, &
                          &PRICFRRG, PRRCFRIG, PRICFRR, PRCWETG, PRIWETG, PRRWETG, PRSWETG, &
                          &PRCDRYG, PRIDRYG, PRRDRYG, PRSDRYG, PRWETGH, PRWETGH_MR, PRGMLTR, &
                          &PRCWETH, PRIWETH, PRSWETH, PRGWETH, PRRWETH, &
                          &PRCDRYH, PRIDRYH, PRSDRYH, PRRDRYH, PRGDRYH, PRDRYHG, PRHMLTR, &
                          &PRCBERI, &
                          &PRS_TEND, PRG_TEND, PRH_TEND, PSSI, &
                          &PA_TH, PA_RV, PA_RC, PA_RR, PA_RI, PA_RS, PA_RG, PA_RH, &
                          &PB_TH, PB_RV, PB_RC, PB_RR, PB_RI, PB_RS, PB_RG, PB_RH, &
                          &PHLC_HCF, PHLC_LCF, PHLC_HRC, PHLC_LRC, &
                          &PHLI_HCF, PHLI_LCF, PHLI_HRI, PHLI_LRI, PRAINFR)
IMPLICIT NONE
INTEGER,                      INTENT(IN)    :: KSIZE, KIB, KIE, KIT, KJB, KJE, KJT, KKB, KKE, KKT, KKL
INTEGER,                      INTENT(IN)    :: KRR
LOGICAL,                      INTENT(IN)    :: ODSOFT
REAL, DIMENSION(KSIZE),    INTENT(IN)       :: PCOMPUTE
LOGICAL,                      INTENT(IN)    :: OWARM
CHARACTER(len=80),            INTENT(IN)    :: HSUBG_RC_RR_ACCR
CHARACTER(len=80),            INTENT(IN)    :: HSUBG_RR_EVAP
CHARACTER(len=4),             INTENT(IN)    :: HSUBG_AUCV_RC
CHARACTER(len=80),            INTENT(IN)    :: HSUBG_AUCV_RI
CHARACTER(len=80),            INTENT(IN)    :: HSUBG_PR_PDF ! pdf for subgrid precipitation
REAL, DIMENSION(KSIZE),       INTENT(IN)    :: PEXN
REAL, DIMENSION(KSIZE),       INTENT(IN)    :: PRHODREF
REAL, DIMENSION(KSIZE),       INTENT(IN)    :: PLVFACT
REAL, DIMENSION(KSIZE),       INTENT(IN)    :: PLSFACT
INTEGER, DIMENSION(KSIZE),    INTENT(IN)    :: K1
INTEGER, DIMENSION(KSIZE),    INTENT(IN)    :: K2
INTEGER, DIMENSION(KSIZE),    INTENT(IN)    :: K3
REAL, DIMENSION(KSIZE),       INTENT(IN)    :: PPRES
REAL, DIMENSION(KSIZE),       INTENT(IN)    :: PCF
REAL, DIMENSION(KSIZE),       INTENT(IN)    :: PSIGMA_RC
REAL, DIMENSION(KSIZE),       INTENT(INOUT) :: PCIT
REAL, DIMENSION(KSIZE),       INTENT(IN)    :: PT
REAL, DIMENSION(KSIZE),       INTENT(IN)    :: PTHT
REAL, DIMENSION(KSIZE),       INTENT(IN)    :: PRVT
REAL, DIMENSION(KSIZE),       INTENT(IN)    :: PRCT
REAL, DIMENSION(KSIZE),       INTENT(IN)    :: PRRT
REAL, DIMENSION(KSIZE),       INTENT(IN)    :: PRIT
REAL, DIMENSION(KSIZE),       INTENT(IN)    :: PRST
REAL, DIMENSION(KSIZE),       INTENT(IN)    :: PRGT
REAL, DIMENSION(KSIZE),       INTENT(IN)    :: PRHT
REAL, DIMENSION(KSIZE),       INTENT(OUT)   :: PRVHENI_MR
REAL, DIMENSION(KSIZE),       INTENT(OUT)   :: PRRHONG_MR
REAL, DIMENSION(KSIZE),       INTENT(OUT)   :: PRIMLTC_MR
REAL, DIMENSION(KSIZE),       INTENT(OUT)   :: PRSRIMCG_MR
REAL, DIMENSION(KSIZE),       INTENT(INOUT) :: PRCHONI
REAL, DIMENSION(KSIZE),       INTENT(INOUT) :: PRVDEPS
REAL, DIMENSION(KSIZE),       INTENT(INOUT) :: PRIAGGS
REAL, DIMENSION(KSIZE),       INTENT(INOUT) :: PRIAUTS
REAL, DIMENSION(KSIZE),       INTENT(INOUT) :: PRVDEPG
REAL, DIMENSION(KSIZE),       INTENT(INOUT) :: PRCAUTR
REAL, DIMENSION(KSIZE),       INTENT(INOUT) :: PRCACCR
REAL, DIMENSION(KSIZE),       INTENT(INOUT) :: PRREVAV
REAL, DIMENSION(KSIZE),       INTENT(OUT)   :: PRCRIMSS
REAL, DIMENSION(KSIZE),       INTENT(OUT)   :: PRCRIMSG
REAL, DIMENSION(KSIZE),       INTENT(OUT)   :: PRSRIMCG
REAL, DIMENSION(KSIZE),       INTENT(OUT)   :: PRRACCSS
REAL, DIMENSION(KSIZE),       INTENT(OUT)   :: PRRACCSG
REAL, DIMENSION(KSIZE),       INTENT(OUT)   :: PRSACCRG
REAL, DIMENSION(KSIZE),       INTENT(INOUT) :: PRSMLTG
REAL, DIMENSION(KSIZE),       INTENT(INOUT) :: PRCMLTSR
REAL, DIMENSION(KSIZE),       INTENT(INOUT) :: PRICFRRG
REAL, DIMENSION(KSIZE),       INTENT(INOUT) :: PRRCFRIG
REAL, DIMENSION(KSIZE),       INTENT(INOUT) :: PRICFRR
REAL, DIMENSION(KSIZE),       INTENT(OUT)   :: PRCWETG
REAL, DIMENSION(KSIZE),       INTENT(OUT)   :: PRIWETG
REAL, DIMENSION(KSIZE),       INTENT(OUT)   :: PRRWETG
REAL, DIMENSION(KSIZE),       INTENT(OUT)   :: PRSWETG
REAL, DIMENSION(KSIZE),       INTENT(OUT)   :: PRCDRYG
REAL, DIMENSION(KSIZE),       INTENT(OUT)   :: PRIDRYG
REAL, DIMENSION(KSIZE),       INTENT(OUT)   :: PRRDRYG
REAL, DIMENSION(KSIZE),       INTENT(OUT)   :: PRSDRYG
REAL, DIMENSION(KSIZE),       INTENT(OUT)   :: PRWETGH
REAL, DIMENSION(KSIZE),       INTENT(OUT)   :: PRWETGH_MR
REAL, DIMENSION(KSIZE),       INTENT(INOUT) :: PRGMLTR
REAL, DIMENSION(KSIZE),       INTENT(OUT)   :: PRCWETH
REAL, DIMENSION(KSIZE),       INTENT(OUT)   :: PRIWETH
REAL, DIMENSION(KSIZE),       INTENT(OUT)   :: PRSWETH
REAL, DIMENSION(KSIZE),       INTENT(OUT)   :: PRGWETH
REAL, DIMENSION(KSIZE),       INTENT(OUT)   :: PRRWETH
REAL, DIMENSION(KSIZE),       INTENT(OUT)   :: PRCDRYH
REAL, DIMENSION(KSIZE),       INTENT(OUT)   :: PRIDRYH
REAL, DIMENSION(KSIZE),       INTENT(OUT)   :: PRSDRYH
REAL, DIMENSION(KSIZE),       INTENT(OUT)   :: PRRDRYH
REAL, DIMENSION(KSIZE),       INTENT(OUT)   :: PRGDRYH
REAL, DIMENSION(KSIZE),       INTENT(OUT)   :: PRDRYHG
REAL, DIMENSION(KSIZE),       INTENT(INOUT) :: PRHMLTR
REAL, DIMENSION(KSIZE),       INTENT(INOUT) :: PRCBERI
REAL, DIMENSION(KSIZE, 8),    INTENT(INOUT) :: PRS_TEND
REAL, DIMENSION(KSIZE, 8),    INTENT(INOUT) :: PRG_TEND
REAL, DIMENSION(KSIZE, 10),   INTENT(INOUT) :: PRH_TEND
REAL, DIMENSION(KSIZE),       INTENT(OUT)   :: PSSI
REAL, DIMENSION(KSIZE),       INTENT(OUT)   :: PA_TH
REAL, DIMENSION(KSIZE),       INTENT(OUT)   :: PA_RV
REAL, DIMENSION(KSIZE),       INTENT(OUT)   :: PA_RC
REAL, DIMENSION(KSIZE),       INTENT(OUT)   :: PA_RR
REAL, DIMENSION(KSIZE),       INTENT(OUT)   :: PA_RI
REAL, DIMENSION(KSIZE),       INTENT(OUT)   :: PA_RS
REAL, DIMENSION(KSIZE),       INTENT(OUT)   :: PA_RG
REAL, DIMENSION(KSIZE),       INTENT(OUT)   :: PA_RH
REAL, DIMENSION(KSIZE),       INTENT(OUT)   :: PB_TH
REAL, DIMENSION(KSIZE),       INTENT(OUT)   :: PB_RV
REAL, DIMENSION(KSIZE),       INTENT(OUT)   :: PB_RC
REAL, DIMENSION(KSIZE),       INTENT(OUT)   :: PB_RR
REAL, DIMENSION(KSIZE),       INTENT(OUT)   :: PB_RI
REAL, DIMENSION(KSIZE),       INTENT(OUT)   :: PB_RS
REAL, DIMENSION(KSIZE),       INTENT(OUT)   :: PB_RG
REAL, DIMENSION(KSIZE),       INTENT(OUT)   :: PB_RH
REAL, DIMENSION(KSIZE),       INTENT(OUT)   :: PHLC_HCF
REAL, DIMENSION(KSIZE),       INTENT(OUT)   :: PHLC_LCF
REAL, DIMENSION(KSIZE),       INTENT(OUT)   :: PHLC_HRC
REAL, DIMENSION(KSIZE),       INTENT(OUT)   :: PHLC_LRC
REAL, DIMENSION(KSIZE),       INTENT(OUT)   :: PHLI_HCF
REAL, DIMENSION(KSIZE),       INTENT(OUT)   :: PHLI_LCF
REAL, DIMENSION(KSIZE),       INTENT(OUT)   :: PHLI_HRI
REAL, DIMENSION(KSIZE),       INTENT(OUT)   :: PHLI_LRI
REAL, DIMENSION(KIT,KJT,KKT), INTENT(OUT)   :: PRAINFR   ! Rain fraction
END SUBROUTINE ICE4_TENDENCIES
END INTERFACE
END MODULE MODI_ICE4_TENDENCIES
SUBROUTINE ICE4_TENDENCIES(KSIZE, KIB, KIE, KIT, KJB, KJE, KJT, KKB, KKE, KKT, KKL, &
                          &KRR, ODSOFT, PCOMPUTE, &
                          &OWARM, HSUBG_RC_RR_ACCR, HSUBG_RR_EVAP, &
                          &HSUBG_AUCV_RC, HSUBG_AUCV_RI, HSUBG_PR_PDF, &
                          &PEXN, PRHODREF, PLVFACT, PLSFACT, K1, K2, K3, &
                          &PPRES, PCF, PSIGMA_RC, &
                          &PCIT, &
                          &PT, PTHT, &
                          &PRVT, PRCT, PRRT, PRIT, PRST, PRGT, PRHT, &
                          &PRVHENI_MR, PRRHONG_MR, PRIMLTC_MR, PRSRIMCG_MR, &
                          &PRCHONI, PRVDEPS, PRIAGGS, PRIAUTS, PRVDEPG, &
                          &PRCAUTR, PRCACCR, PRREVAV, &
                          &PRCRIMSS, PRCRIMSG, PRSRIMCG, PRRACCSS, PRRACCSG, PRSACCRG, PRSMLTG, PRCMLTSR, &
                          &PRICFRRG, PRRCFRIG, PRICFRR, PRCWETG, PRIWETG, PRRWETG, PRSWETG, &
                          &PRCDRYG, PRIDRYG, PRRDRYG, PRSDRYG, PRWETGH, PRWETGH_MR, PRGMLTR, &
                          &PRCWETH, PRIWETH, PRSWETH, PRGWETH, PRRWETH, &
                          &PRCDRYH, PRIDRYH, PRSDRYH, PRRDRYH, PRGDRYH, PRDRYHG, PRHMLTR, &
                          &PRCBERI, &
                          &PRS_TEND, PRG_TEND, PRH_TEND, PSSI, &
                          &PA_TH, PA_RV, PA_RC, PA_RR, PA_RI, PA_RS, PA_RG, PA_RH, &
                          &PB_TH, PB_RV, PB_RC, PB_RR, PB_RI, PB_RS, PB_RG, PB_RH, &
                          &PHLC_HCF, PHLC_LCF, PHLC_HRC, PHLC_LRC, &
                          &PHLI_HCF, PHLI_LCF, PHLI_HRI, PHLI_LRI, PRAINFR)
!!
!!**  PURPOSE
!!    -------
!!      Computes the tendencies
!!
!!    AUTHOR
!!    ------
!!      S. Riette from the splitting of rain_ice source code (nov. 2014)
!!
!!    MODIFICATIONS
!!    -------------
!
!  P. Wautelet 29/05/2019: remove PACK/UNPACK intrinsics (to get more performance and better OpenACC support)
!
!
!*      0. DECLARATIONS
!          ------------
!
USE MODD_CST,            ONLY: XALPI,XBETAI,XCI,XCPV,XGAMI,XLSTT,XMD,XMV,XP00,XRV,XTT,XEPSILO
USE MODD_RAIN_ICE_DESCR, ONLY: XLBDAS_MAX,XLBEXG,XLBEXH,XLBEXR,XLBEXS,XLBG,XLBH,XLBR,XLBS,XRTMIN
USE MODD_RAIN_ICE_PARAM, ONLY: XSCFAC
USE MODD_PARAM_ICE, ONLY : CSNOWRIMING
!
USE MODI_ICE4_COMPUTE_PDF
USE MODI_ICE4_FAST_RG
USE MODI_ICE4_FAST_RH
USE MODI_ICE4_FAST_RI
USE MODI_ICE4_FAST_RS
USE MODI_ICE4_NUCLEATION
USE MODI_ICE4_RAINFR_VERT
USE MODI_ICE4_RIMLTC
USE MODI_ICE4_RRHONG
USE MODI_ICE4_RSRIMCG_OLD
USE MODI_ICE4_SLOW
USE MODI_ICE4_WARM
!
IMPLICIT NONE
!
!*       0.1   Declarations of dummy arguments :
!
INTEGER,                      INTENT(IN)    :: KSIZE, KIB, KIE, KIT, KJB, KJE, KJT, KKB, KKE, KKT, KKL
INTEGER,                      INTENT(IN)    :: KRR
LOGICAL,                      INTENT(IN)    :: ODSOFT
REAL, DIMENSION(KSIZE),       INTENT(IN)    :: PCOMPUTE
LOGICAL,                      INTENT(IN)    :: OWARM
CHARACTER(len=80),            INTENT(IN)    :: HSUBG_RC_RR_ACCR
CHARACTER(len=80),            INTENT(IN)    :: HSUBG_RR_EVAP
CHARACTER(len=4),             INTENT(IN)    :: HSUBG_AUCV_RC
CHARACTER(len=80),            INTENT(IN)    :: HSUBG_AUCV_RI
CHARACTER(len=80),            INTENT(IN)    :: HSUBG_PR_PDF ! pdf for subgrid precipitation
REAL, DIMENSION(KSIZE),       INTENT(IN)    :: PEXN
REAL, DIMENSION(KSIZE),       INTENT(IN)    :: PRHODREF
REAL, DIMENSION(KSIZE),       INTENT(IN)    :: PLVFACT
REAL, DIMENSION(KSIZE),       INTENT(IN)    :: PLSFACT
INTEGER, DIMENSION(KSIZE),    INTENT(IN)    :: K1
INTEGER, DIMENSION(KSIZE),    INTENT(IN)    :: K2
INTEGER, DIMENSION(KSIZE),    INTENT(IN)    :: K3
REAL, DIMENSION(KSIZE),       INTENT(IN)    :: PPRES
REAL, DIMENSION(KSIZE),       INTENT(IN)    :: PCF
REAL, DIMENSION(KSIZE),       INTENT(IN)    :: PSIGMA_RC
REAL, DIMENSION(KSIZE),       INTENT(INOUT) :: PCIT
REAL, DIMENSION(KSIZE),       INTENT(IN)    :: PT
REAL, DIMENSION(KSIZE),       INTENT(IN)    :: PTHT
REAL, DIMENSION(KSIZE),       INTENT(IN)    :: PRVT
REAL, DIMENSION(KSIZE),       INTENT(IN)    :: PRCT
REAL, DIMENSION(KSIZE),       INTENT(IN)    :: PRRT
REAL, DIMENSION(KSIZE),       INTENT(IN)    :: PRIT
REAL, DIMENSION(KSIZE),       INTENT(IN)    :: PRST
REAL, DIMENSION(KSIZE),       INTENT(IN)    :: PRGT
REAL, DIMENSION(KSIZE),       INTENT(IN)    :: PRHT
REAL, DIMENSION(KSIZE),       INTENT(OUT)   :: PRVHENI_MR
REAL, DIMENSION(KSIZE),       INTENT(OUT)   :: PRRHONG_MR
REAL, DIMENSION(KSIZE),       INTENT(OUT)   :: PRIMLTC_MR
REAL, DIMENSION(KSIZE),       INTENT(OUT)   :: PRSRIMCG_MR
REAL, DIMENSION(KSIZE),       INTENT(INOUT) :: PRCHONI
REAL, DIMENSION(KSIZE),       INTENT(INOUT) :: PRVDEPS
REAL, DIMENSION(KSIZE),       INTENT(INOUT) :: PRIAGGS
REAL, DIMENSION(KSIZE),       INTENT(INOUT) :: PRIAUTS
REAL, DIMENSION(KSIZE),       INTENT(INOUT) :: PRVDEPG
REAL, DIMENSION(KSIZE),       INTENT(INOUT) :: PRCAUTR
REAL, DIMENSION(KSIZE),       INTENT(INOUT) :: PRCACCR
REAL, DIMENSION(KSIZE),       INTENT(INOUT) :: PRREVAV
REAL, DIMENSION(KSIZE),       INTENT(OUT)   :: PRCRIMSS
REAL, DIMENSION(KSIZE),       INTENT(OUT)   :: PRCRIMSG
REAL, DIMENSION(KSIZE),       INTENT(OUT)   :: PRSRIMCG
REAL, DIMENSION(KSIZE),       INTENT(OUT)   :: PRRACCSS
REAL, DIMENSION(KSIZE),       INTENT(OUT)   :: PRRACCSG
REAL, DIMENSION(KSIZE),       INTENT(OUT)   :: PRSACCRG
REAL, DIMENSION(KSIZE),       INTENT(INOUT) :: PRSMLTG
REAL, DIMENSION(KSIZE),       INTENT(INOUT) :: PRCMLTSR
REAL, DIMENSION(KSIZE),       INTENT(INOUT) :: PRICFRRG
REAL, DIMENSION(KSIZE),       INTENT(INOUT) :: PRRCFRIG
REAL, DIMENSION(KSIZE),       INTENT(INOUT) :: PRICFRR
REAL, DIMENSION(KSIZE),       INTENT(OUT)   :: PRCWETG
REAL, DIMENSION(KSIZE),       INTENT(OUT)   :: PRIWETG
REAL, DIMENSION(KSIZE),       INTENT(OUT)   :: PRRWETG
REAL, DIMENSION(KSIZE),       INTENT(OUT)   :: PRSWETG
REAL, DIMENSION(KSIZE),       INTENT(OUT)   :: PRCDRYG
REAL, DIMENSION(KSIZE),       INTENT(OUT)   :: PRIDRYG
REAL, DIMENSION(KSIZE),       INTENT(OUT)   :: PRRDRYG
REAL, DIMENSION(KSIZE),       INTENT(OUT)   :: PRSDRYG
REAL, DIMENSION(KSIZE),       INTENT(OUT)   :: PRWETGH
REAL, DIMENSION(KSIZE),       INTENT(OUT)   :: PRWETGH_MR
REAL, DIMENSION(KSIZE),       INTENT(INOUT) :: PRGMLTR
REAL, DIMENSION(KSIZE),       INTENT(OUT)   :: PRCWETH
REAL, DIMENSION(KSIZE),       INTENT(OUT)   :: PRIWETH
REAL, DIMENSION(KSIZE),       INTENT(OUT)   :: PRSWETH
REAL, DIMENSION(KSIZE),       INTENT(OUT)   :: PRGWETH
REAL, DIMENSION(KSIZE),       INTENT(OUT)   :: PRRWETH
REAL, DIMENSION(KSIZE),       INTENT(OUT)   :: PRCDRYH
REAL, DIMENSION(KSIZE),       INTENT(OUT)   :: PRIDRYH
REAL, DIMENSION(KSIZE),       INTENT(OUT)   :: PRSDRYH
REAL, DIMENSION(KSIZE),       INTENT(OUT)   :: PRRDRYH
REAL, DIMENSION(KSIZE),       INTENT(OUT)   :: PRGDRYH
REAL, DIMENSION(KSIZE),       INTENT(OUT)   :: PRDRYHG
REAL, DIMENSION(KSIZE),       INTENT(INOUT) :: PRHMLTR
REAL, DIMENSION(KSIZE),       INTENT(INOUT) :: PRCBERI
REAL, DIMENSION(KSIZE, 8),    INTENT(INOUT) :: PRS_TEND
REAL, DIMENSION(KSIZE, 8),    INTENT(INOUT) :: PRG_TEND
REAL, DIMENSION(KSIZE, 10),   INTENT(INOUT) :: PRH_TEND
REAL, DIMENSION(KSIZE),       INTENT(OUT) :: PSSI
REAL, DIMENSION(KSIZE),       INTENT(OUT)   :: PA_TH
REAL, DIMENSION(KSIZE),       INTENT(OUT)   :: PA_RV
REAL, DIMENSION(KSIZE),       INTENT(OUT)   :: PA_RC
REAL, DIMENSION(KSIZE),       INTENT(OUT)   :: PA_RR
REAL, DIMENSION(KSIZE),       INTENT(OUT)   :: PA_RI
REAL, DIMENSION(KSIZE),       INTENT(OUT)   :: PA_RS
REAL, DIMENSION(KSIZE),       INTENT(OUT)   :: PA_RG
REAL, DIMENSION(KSIZE),       INTENT(OUT)   :: PA_RH
REAL, DIMENSION(KSIZE),       INTENT(OUT)   :: PB_TH
REAL, DIMENSION(KSIZE),       INTENT(OUT)   :: PB_RV
REAL, DIMENSION(KSIZE),       INTENT(OUT)   :: PB_RC
REAL, DIMENSION(KSIZE),       INTENT(OUT)   :: PB_RR
REAL, DIMENSION(KSIZE),       INTENT(OUT)   :: PB_RI
REAL, DIMENSION(KSIZE),       INTENT(OUT)   :: PB_RS
REAL, DIMENSION(KSIZE),       INTENT(OUT)   :: PB_RG
REAL, DIMENSION(KSIZE),       INTENT(OUT)   :: PB_RH
REAL, DIMENSION(KSIZE),       INTENT(OUT)   :: PHLC_HCF
REAL, DIMENSION(KSIZE),       INTENT(OUT)   :: PHLC_LCF
REAL, DIMENSION(KSIZE),       INTENT(OUT)   :: PHLC_HRC
REAL, DIMENSION(KSIZE),       INTENT(OUT)   :: PHLC_LRC
REAL, DIMENSION(KSIZE),       INTENT(OUT)   :: PHLI_HCF
REAL, DIMENSION(KSIZE),       INTENT(OUT)   :: PHLI_LCF
REAL, DIMENSION(KSIZE),       INTENT(OUT)   :: PHLI_HRI
REAL, DIMENSION(KSIZE),       INTENT(OUT)   :: PHLI_LRI
REAL, DIMENSION(KIT,KJT,KKT), INTENT(OUT)   :: PRAINFR   ! Rain fraction
!
!*       0.2  declaration of local variables
!
REAL, DIMENSION(KSIZE) :: ZRVT, ZRCT, ZRRT, ZRIT, ZRST, ZRGT, &
                        & ZT, ZTHT, ZRHT, &
                        & ZZW, &
                        & ZKA, ZDV, ZAI, ZCJ, &
                        & ZRF, &
                        & ZLBDAR, ZLBDAS, ZLBDAG, ZLBDAH, ZLBDAR_RF, &
                        & ZRGSI, ZRGSI_MR
REAL, DIMENSION(KIT,KJT,KKT) :: ZRRT3D, ZRST3D, ZRGT3D, ZRHT3D
INTEGER :: JL
REAL, DIMENSION(KSIZE) :: ZWETG ! 1. if graupel growths in wet mode, 0. otherwise

PA_TH(:)=0.
PA_RV(:)=0.
PA_RC(:)=0.
PA_RR(:)=0.
PA_RI(:)=0.
PA_RS(:)=0.
PA_RG(:)=0.
PA_RH(:)=0.
PB_TH(:)=0.
PB_RV(:)=0.
PB_RC(:)=0.
PB_RR(:)=0.
PB_RI(:)=0.
PB_RS(:)=0.
PB_RG(:)=0.
PB_RH(:)=0.
!
DO JL=1, KSIZE
  ZRVT(JL)=PRVT(JL)
  ZRCT(JL)=PRCT(JL)
  ZRRT(JL)=PRRT(JL)
  ZRIT(JL)=PRIT(JL)
  ZRST(JL)=PRST(JL)
  ZRGT(JL)=PRGT(JL)
  ZTHT(JL)=PTHT(JL)
  ZRHT(JL)=PRHT(JL)
  ZT(JL)=PT(JL)
ENDDO
IF(ODSOFT) THEN
  PRVHENI_MR(:)=0.
  PRRHONG_MR(:)=0.
  PRIMLTC_MR(:)=0.
  PRSRIMCG_MR(:)=0.
ELSE
  !
  !*       2.     COMPUTES THE SLOW COLD PROCESS SOURCES
  !               --------------------------------------
  CALL ICE4_NUCLEATION(KSIZE, ODSOFT, PCOMPUTE==1., &
                       ZTHT, PPRES, PRHODREF, PEXN, PLSFACT, ZT, &
                       ZRVT, &
                       PCIT, PRVHENI_MR, PB_TH, PB_RV, PB_RI)
  DO JL=1, KSIZE
    ZRIT(JL)=ZRIT(JL) + PRVHENI_MR(JL)
    ZRVT(JL)=ZRVT(JL) - PRVHENI_MR(JL)
    ZTHT(JL)=ZTHT(JL) + PRVHENI_MR(JL)*PLSFACT(JL)
    ZT(JL) = ZTHT(JL) * PEXN(JL)
  ENDDO
  !
  !*       3.3     compute the spontaneous freezing source: RRHONG
  !
  CALL ICE4_RRHONG(KSIZE, ODSOFT, PCOMPUTE, &
                  &PEXN, PLVFACT, PLSFACT, &
                  &ZT,   ZRRT, &
                  &ZTHT, &
                  &PRRHONG_MR, PB_TH, PB_RR, PB_RG)
  DO JL=1, KSIZE
    ZRGT(JL) = ZRGT(JL) + PRRHONG_MR(JL)
    ZRRT(JL) = ZRRT(JL) - PRRHONG_MR(JL)
    ZTHT(JL) = ZTHT(JL) + PRRHONG_MR(JL)*(PLSFACT(JL)-PLVFACT(JL)) ! f(L_f*(RRHONG))
    ZT(JL) = ZTHT(JL) * PEXN(JL)
  ENDDO
  !
  !*       7.1    cloud ice melting
  !
  CALL ICE4_RIMLTC(KSIZE, ODSOFT, PCOMPUTE, &
                  &PEXN, PLVFACT, PLSFACT, &
                  &ZT, &
                  &ZTHT, ZRIT, &
                  &PRIMLTC_MR, PB_TH, PB_RC, PB_RI)
  DO JL=1, KSIZE
    ZRCT(JL) = ZRCT(JL) + PRIMLTC_MR(JL)
    ZRIT(JL) = ZRIT(JL) - PRIMLTC_MR(JL)
    ZTHT(JL) = ZTHT(JL) - PRIMLTC_MR(JL)*(PLSFACT(JL)-PLVFACT(JL)) ! f(L_f*(-RIMLTC))
    ZT(JL) = ZTHT(JL) * PEXN(JL)
  ENDDO
  !
  !        5.1.6  riming-conversion of the large sized aggregates into graupel (old parametrisation)
  !
  IF(CSNOWRIMING=='OLD ') THEN
    ZLBDAS(:)=0.
    WHERE(ZRST(:)>0.)
      ZLBDAS(:)  = MIN(XLBDAS_MAX, XLBS*(PRHODREF(:)*MAX(ZRST(:), XRTMIN(5)))**XLBEXS)
    END WHERE
    CALL ICE4_RSRIMCG_OLD(KSIZE, ODSOFT, PCOMPUTE==1., &
                         &PRHODREF, &
                         &ZLBDAS, &
                         &ZT, ZRCT, ZRST, &
                         &PRSRIMCG_MR, PB_RS, PB_RG)
    DO JL=1, KSIZE
      ZRST(JL) = ZRST(JL) - PRSRIMCG_MR(JL)
      ZRGT(JL) = ZRGT(JL) + PRSRIMCG_MR(JL)
    ENDDO
  ELSE
    PRSRIMCG_MR(:) = 0.
  ENDIF
ENDIF
!
!* Derived fields
!
IF(KSIZE>0) THEN
  IF(.NOT. ODSOFT) THEN
    ZZW(:) = EXP(XALPI-XBETAI/ZT(:)-XGAMI*ALOG(ZT(:)))
    DO JL=1, KSIZE
      PSSI(JL) = ZRVT(JL)*( PPRES(JL)-ZZW(JL) ) / ( XEPSILO * ZZW(JL) ) - 1.0
                                                        ! Supersaturation over ice
      ZKA(JL) = 2.38E-2 + 0.0071E-2*(ZT(JL)-XTT) ! k_a
      ZDV(JL) = 0.211E-4*(ZT(JL)/XTT)**1.94 * (XP00/PPRES(JL)) ! D_v
      ZAI(JL) = (XLSTT+(XCPV-XCI)*(ZT(JL)-XTT))**2 / (ZKA(JL)*XRV*ZT(JL)**2) &
                                   + ( XRV*ZT(JL) ) / (ZDV(JL)*ZZW(JL))
      ZCJ(JL) = XSCFAC*PRHODREF(JL)**0.3 / SQRT(1.718E-5+0.0049E-5*(ZT(JL)-XTT))
    ENDDO
  ENDIF
  !
  !Cloud water split between high and low content part is done here
  CALL ICE4_COMPUTE_PDF(KSIZE, HSUBG_AUCV_RC, HSUBG_AUCV_RI, HSUBG_PR_PDF,&
                        PRHODREF, ZRCT, ZRIT, PCF, ZT, PSIGMA_RC,&
                        PHLC_HCF, PHLC_LCF, PHLC_HRC, PHLC_LRC,&
                        PHLI_HCF, PHLI_LCF, PHLI_HRI, PHLI_LRI, ZRF)
  IF(HSUBG_RC_RR_ACCR=='PRFR' .OR. HSUBG_RR_EVAP=='PRFR') THEN
    !Diagnostic of precipitation fraction
    PRAINFR(:,:,:) = 0.
    ZRRT3D (:,:,:) = 0.
    ZRST3D (:,:,:) = 0.
    ZRGT3D (:,:,:) = 0.
    ZRHT3D (:,:,:) = 0.
    DO JL=1,KSIZE
      PRAINFR(K1(JL), K2(JL), K3(JL)) = ZRF(JL)
      ZRRT3D (K1(JL), K2(JL), K3(JL)) = ZRRT(JL)
      ZRST3D (K1(JL), K2(JL), K3(JL)) = ZRST(JL)
      ZRGT3D (K1(JL), K2(JL), K3(JL)) = ZRGT(JL)
    END DO
    IF (KRR==7) THEN
      DO JL=1,KSIZE    
        ZRHT3D (K1(JL), K2(JL), K3(JL)) = ZRHT(JL)
      ENDDO
    ENDIF
    CALL ICE4_RAINFR_VERT(KIB, KIE, KIT, KJB, KJE, KJT, KKB, KKE, KKT, KKL, PRAINFR(:,:,:), ZRRT3D(:,:,:), &
                         &ZRST3D(:,:,:), ZRGT3D(:,:,:), ZRHT3D(:,:,:))
    DO JL=1,KSIZE
      ZRF(JL)=PRAINFR(K1(JL), K2(JL), K3(JL))
    END DO
  ELSE
    PRAINFR(:,:,:)=1.
    ZRF(:)=1.
  ENDIF
  !
  !*  compute the slope parameters
  !
  ZLBDAS(:)=0.
  WHERE(ZRST(:)>0.)
    ZLBDAS(:)  = MIN(XLBDAS_MAX, XLBS*(PRHODREF(:)*MAX(ZRST(:), XRTMIN(5)))**XLBEXS)
  END WHERE
  ZLBDAG(:)=0.
  WHERE(ZRGT(:)>0.)
    ZLBDAG(:)  = XLBG*(PRHODREF(:)*MAX(ZRGT(:), XRTMIN(6)))**XLBEXG
  END WHERE
  !ZLBDAR will be used when we consider rain diluted over the grid box
  ZLBDAR(:)=0.
  WHERE(ZRRT(:)>0.)
    ZLBDAR(:)  = XLBR*( PRHODREF(:)*MAX( ZRRT(:), XRTMIN(3)))**XLBEXR
  END WHERE
  !ZLBDAR_RF is used when we consider rain concentrated in its fraction
  IF (HSUBG_RC_RR_ACCR=='PRFR' .OR. HSUBG_RR_EVAP=='PRFR') THEN
    ZLBDAR_RF(:)=0.
    WHERE(ZRRT(:)>0. .AND. ZRF(:)>0.)
      ZLBDAR_RF(:) = XLBR*( PRHODREF(:) *MAX( ZRRT(:)/ZRF(:) , XRTMIN(3)))**XLBEXR
    END WHERE
  ELSE
    ZLBDAR_RF(:) = ZLBDAR(:)
  ENDIF
  IF(KRR==7) THEN
    ZLBDAH(:)=0.
    WHERE(PRHT(:)>0.)
      ZLBDAH(:) = XLBH*(PRHODREF(:)*MAX(PRHT(:), XRTMIN(7)))**XLBEXH
    END WHERE
  ENDIF
ENDIF
!
!
CALL ICE4_SLOW(KSIZE, ODSOFT, HSUBG_AUCV_RI, PCOMPUTE, PRHODREF, ZT, &
              &PSSI, PLVFACT, PLSFACT, &
              &ZRVT, ZRCT, ZRIT, ZRST, ZRGT, &
              &ZLBDAS, ZLBDAG, &
              &ZAI, ZCJ, PHLI_HCF, PHLI_HRI, &
              &PRCHONI, PRVDEPS, PRIAGGS, PRIAUTS, PRVDEPG, &
              &PA_TH, PA_RV, PA_RC, PA_RI, PA_RS, PA_RG)
!
!-------------------------------------------------------------------------------
!
!
!*       3.     COMPUTES THE SLOW WARM PROCESS SOURCES
!               --------------------------------------
!
!
IF(OWARM) THEN    !  Check if the formation of the raindrops by the slow
                  !  warm processes is allowed
  CALL ICE4_WARM(KSIZE, ODSOFT, PCOMPUTE, HSUBG_RC_RR_ACCR, HSUBG_RR_EVAP, &
                &PRHODREF, PLVFACT, ZT, PPRES, ZTHT,&
                &ZLBDAR, ZLBDAR_RF, ZKA, ZDV, ZCJ, &
                &PHLC_LCF, PHLC_HCF, PHLC_LRC, PHLC_HRC, &
                &PCF, ZRF, &
                &ZRVT, ZRCT, ZRRT, &
                &PRCAUTR, PRCACCR, PRREVAV, &
                &PA_TH, PA_RV, PA_RC, PA_RR)
ELSE
  PRCAUTR(:)=0.
  PRCACCR(:)=0.
  PRREVAV(:)=0.
END IF
!
!-------------------------------------------------------------------------------
!
!
!*       4.     COMPUTES THE FAST COLD PROCESS SOURCES FOR r_s
!               ----------------------------------------------
!
CALL ICE4_FAST_RS(KSIZE, ODSOFT, PCOMPUTE, &
                 &PRHODREF, PLVFACT, PLSFACT, PPRES, &
                 &ZDV, ZKA, ZCJ, &
                 &ZLBDAR, ZLBDAS, &
                 &ZT, ZRVT, ZRCT, ZRRT, ZRST, &
                 &PRIAGGS, &
                 &PRCRIMSS, PRCRIMSG, PRSRIMCG, &
                 &PRRACCSS, PRRACCSG, PRSACCRG, PRSMLTG, &
                 &PRCMLTSR, &
                 &PRS_TEND, &
                 &PA_TH, PA_RC, PA_RR, PA_RS, PA_RG)
!
!-------------------------------------------------------------------------------
!
!
!*       5.        COMPUTES THE FAST COLD PROCESS SOURCES FOR r_g
!                  ------------------------------------------------------
!
DO JL=1, KSIZE
  ZRGSI(JL) = PRVDEPG(JL) + PRSMLTG(JL) + PRRACCSG(JL) + &
           & PRSACCRG(JL) + PRCRIMSG(JL) + PRSRIMCG(JL)
  ZRGSI_MR(JL) = PRRHONG_MR(JL) + PRSRIMCG_MR(JL)
ENDDO
CALL ICE4_FAST_RG(KSIZE, ODSOFT, PCOMPUTE, KRR, &
                 &PRHODREF, PLVFACT, PLSFACT, PPRES, &
                 &ZDV, ZKA, ZCJ, PCIT, &
                 &ZLBDAR, ZLBDAS, ZLBDAG, &
                 &ZT, ZRVT, ZRCT, ZRRT, ZRIT, ZRST, ZRGT, &
                 &ZRGSI, ZRGSI_MR(:), &
                 &ZWETG, &
                 &PRICFRRG, PRRCFRIG, PRICFRR, PRCWETG, PRIWETG, PRRWETG, PRSWETG, &
                 &PRCDRYG, PRIDRYG, PRRDRYG, PRSDRYG, PRWETGH, PRWETGH_MR, PRGMLTR, &
                 &PRG_TEND, &
                 &PA_TH, PA_RC, PA_RR, PA_RI, PA_RS, PA_RG, PA_RH, PB_RG, PB_RH)
!
!-------------------------------------------------------------------------------
!
!
!*       6.     COMPUTES THE FAST COLD PROCESS SOURCES FOR r_h
!               ----------------------------------------------
!
IF (KRR==7) THEN
  CALL ICE4_FAST_RH(KSIZE, ODSOFT, PCOMPUTE, ZWETG, &
                   &PRHODREF, PLVFACT, PLSFACT, PPRES, &
                   &ZDV, ZKA, ZCJ, &
                   &ZLBDAS, ZLBDAG, ZLBDAR, ZLBDAH, &
                   &ZT,  ZRVT, ZRCT, ZRRT, ZRIT, ZRST, ZRGT, PRHT, &
                   &PRCWETH, PRIWETH, PRSWETH, PRGWETH, PRRWETH, &
                   &PRCDRYH, PRIDRYH, PRSDRYH, PRRDRYH, PRGDRYH, PRDRYHG, PRHMLTR, &
                   &PRH_TEND, &
                   &PA_TH, PA_RC, PA_RR, PA_RI, PA_RS, PA_RG, PA_RH)
ELSE
  PRCWETH(:)=0.
  PRIWETH(:)=0.
  PRSWETH(:)=0.
  PRGWETH(:)=0.
  PRRWETH(:)=0.
  PRCDRYH(:)=0.
  PRIDRYH(:)=0.
  PRSDRYH(:)=0.
  PRRDRYH(:)=0.
  PRGDRYH(:)=0.
  PRDRYHG(:)=0.
  PRHMLTR(:)=0.
END IF
!
!-------------------------------------------------------------------------------
!
!
!*       7.     COMPUTES SPECIFIC SOURCES OF THE WARM AND COLD CLOUDY SPECIES
!               -------------------------------------------------------------
!
CALL ICE4_FAST_RI(KSIZE, ODSOFT, PCOMPUTE, &
                 &PRHODREF, PLVFACT, PLSFACT, &
                 &ZAI, ZCJ, PCIT, &
                 &PSSI, &
                 &ZRCT, ZRIT, &
                 &PRCBERI, PA_TH, PA_RC, PA_RI)
!
!
END SUBROUTINE ICE4_TENDENCIES
