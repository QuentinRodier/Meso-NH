!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
    SUBROUTINE LANDUSE_HYDRO(IO, S, NK, NP, NPE, TLU, KI)
!   ###############################################################
!!****  *LAND USE HYDRO*
!!
!!    PURPOSE
!!    -------
!
!     Performs land use land cover change computation at yearly time step
!               
!!**  METHOD
!!    ------
!!
!!    EXTERNAL
!!    --------
!!    none
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!      
!!    none
!!
!!    REFERENCE
!!    ---------
!!
!!      
!!    AUTHOR
!!    ------
!!    R. Séférian & B. Decharme 08/2015
!!
!!
!-------------------------------------------------------------------------------
!
!*       0.     DECLARATIONS
!               ------------
!
USE MODD_ISBA_OPTIONS_n, ONLY : ISBA_OPTIONS_t
USE MODD_ISBA_n,         ONLY : ISBA_S_t, ISBA_P_t, ISBA_PE_t, ISBA_K_t, ISBA_NK_t, &
                                ISBA_NP_t, ISBA_NPE_t
!
USE MODD_INIT_LANDUSE,   ONLY : LULCC_P_t, LULCC_NP_t
!
USE MODD_SURF_PAR,       ONLY : XUNDEF,NUNDEF
!
USE MODD_CSTS,           ONLY : XTT, XG, XLMTT, XRHOLW
USE MODD_ISBA_PAR,       ONLY : XWGMIN, XWTD_MAXDEPTH
!
USE YOMHOOK   ,          ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,          ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
TYPE(ISBA_OPTIONS_t),  INTENT(INOUT) :: IO
TYPE(ISBA_S_t),        INTENT(INOUT) :: S
TYPE(ISBA_NK_t),       INTENT(INOUT) :: NK
TYPE(ISBA_NP_t),       INTENT(INOUT) :: NP
TYPE(ISBA_NPE_t),      INTENT(INOUT) :: NPE
TYPE(LULCC_NP_t),      INTENT(INOUT) :: TLU
!
INTEGER,               INTENT(IN)    :: KI
!
!*      0.2    declarations of local arguments
!
TYPE(ISBA_P_t),  POINTER :: PK
TYPE(ISBA_K_t),  POINTER :: KK
TYPE(ISBA_PE_t), POINTER :: PEK
TYPE(LULCC_P_t), POINTER :: OLD
!
INTEGER, DIMENSION(KI,IO%NPATCH)   :: IWG_LAYER ! Number of hydrological layers old
!
REAL, DIMENSION(KI)   :: ZWG 
REAL, DIMENSION(KI)   :: ZWGI 
REAL, DIMENSION(KI)   :: ZWG_OLD
REAL, DIMENSION(KI)   :: ZWGI_OLD
!
REAL, DIMENSION(KI)   :: ZWR
REAL, DIMENSION(KI)   :: ZWR_OLD
!
REAL, DIMENSION(KI)   :: ZSWE
REAL, DIMENSION(KI)   :: ZSWE_OLD
!
REAL, DIMENSION(KI)    :: ZTWS     ! Total land surface water storage (kg/m2)
REAL, DIMENSION(KI)    :: ZTWS_OLD ! Total land surface water storage (kg/m2)
!
! working table
!
REAL     :: ZWORK1, ZWORK2, ZWORK3, &
            ZLOG, ZWTOT, ZWL,       &
            ZMATPOT, ZMATPOTN
!
INTEGER  :: INL, INP, INS, IDEPTH, IMASK
INTEGER  :: JI, JL, JP
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-----------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('LANDUSE_HYDRO',0,ZHOOK_HANDLE)
!-----------------------------------------------------------------
!
!*      1.     Preliminaries
!              -------------
!
INL=IO%NGROUND_LAYER
INP=IO%NPATCH
INS=NPE%AL(1)%TSNOW%NLAYER
!
! 1.1 local arguments 
!
ZTWS    (:)    = 0.0
ZTWS_OLD(:)    = 0.0
!
ZWG     (:)    = 0.0
ZWGI    (:)    = 0.0
ZWG_OLD (:)    = 0.0
ZWGI_OLD(:)    = 0.0
! 
ZWR     (:)    = 0.0
ZWR_OLD (:)    = 0.0
!
ZSWE    (:)    = 0.0
ZSWE_OLD(:)    = 0.0
!
IWG_LAYER(:,:) = 0
!
!-----------------------------------------------------------------
!
!*      2.     Compute previous year total land surface water storage (kg m-2)
!              ---------------------------------------------------------------
! 
DO JP=1,INP
   PK => NP%AL(JP)
   PEK => NPE%AL(JP)
   OLD => TLU%AL(JP)
   DO JL=1,INL
      DO JI=1,PK%NSIZE_P
         IMASK = PK%NR_P(JI)
         IF(OLD%WG(JI,JL)/=XUNDEF.AND.OLD%DG(JI,JL)/=XUNDEF) THEN
           ZWG_OLD (IMASK) = ZWG_OLD (IMASK) + OLD%PATCH(JI)*OLD%DZG(JI,JL)*OLD%WG (JI,JL)*XRHOLW
           ZWGI_OLD(IMASK) = ZWGI_OLD(IMASK) + OLD%PATCH(JI)*OLD%DZG(JI,JL)*OLD%WGI(JI,JL)*XRHOLW
         ENDIF
      ENDDO
   ENDDO
ENDDO
!
DO JP=1,INP
   PK => NP%AL(JP)
   PEK => NPE%AL(JP)
   OLD => TLU%AL(JP)
   DO JL=1,INS
      DO JI=1,PK%NSIZE_P
         IMASK = PK%NR_P(JI)
         ZSWE_OLD(IMASK) = ZSWE_OLD(IMASK) + OLD%PATCH(JI)*OLD%WSN(JI,JL)
      ENDDO
   ENDDO
ENDDO
!
DO JP=1,INP
   PK => NP%AL(JP)
   PEK => NPE%AL(JP)
   OLD => TLU%AL(JP)
   DO JI=1,PK%NSIZE_P
      IMASK = PK%NR_P(JI)
      ZWR_OLD(IMASK) = ZWR_OLD(IMASK) + OLD%PATCH(JI)*OLD%WR(JI)
   ENDDO
ENDDO
!
ZTWS_OLD(:) = ZWG_OLD(:) + ZWGI_OLD(:) + ZWR_OLD(:) + ZSWE_OLD(:)
!
!-----------------------------------------------------------------
!
!*      3.     Compute current year water profile
!              -------------------------
!
DO JP=1,INP
   PK => NP%AL(JP)
   PEK => NPE%AL(JP)
   DO JL=1,INL
      DO JI=1,PK%NSIZE_P
         IF(PK%NWG_LAYER(JI)/=NUNDEF.AND.PEK%XWG(JI,JL)/=XUNDEF)THEN
           IMASK = PK%NR_P(JI)
           IWG_LAYER(IMASK,JP) = JL
         ENDIF
      ENDDO
   ENDDO
ENDDO
!
DO JP=1,INP
   PK => NP%AL(JP)
   KK => NK%AL(JP)  
   PEK => NPE%AL(JP)
   DO JL=1,INL
      DO JI=1,PK%NSIZE_P
         !
         ! 3.1 Check consitency
         !
         IF(PK%NWG_LAYER(JI)/=NUNDEF.AND.JL<=PK%NWG_LAYER(JI))THEN
           !
           ! 3.2 Ensure coherent water/ice profile
           ! Method: if layer is not defined, water profile is extrapolated using 
           ! a psi extrapolation of the last vertical level of the soil column
           !
           IF(PEK%XWG(JI,JL)==XUNDEF) THEN
             !
             IMASK = PK%NR_P(JI)
             !
             IDEPTH = IWG_LAYER(IMASK,JP)
             !
             !Total matric potential : psi=mpotsat*(w/wsat)**(-bcoef)
             !
             ZWORK1   = MIN(1.0,(PEK%XWG(JI,IDEPTH)+PEK%XWGI(JI,IDEPTH))/KK%XWSAT(JI,IDEPTH))
             ZLOG     = KK%XBCOEF(JI,IDEPTH)*LOG(ZWORK1)
             ZMATPOTN = KK%XMPOTSAT(JI,JL)*EXP(-ZLOG)
             !
             !Extrapolation of total matric potential
             !
             ZWORK1  = 0.5*(PK%XDG(JI,IDEPTH)+PK%XDG(JI,IDEPTH-1))
             ZWORK2  = 0.5*(PK%XDG(JI,JL)+PK%XDG(JI,JL-1))
             ZWORK3  = MAX(0.0,(XWTD_MAXDEPTH-ZWORK2)/(ZWORK2-ZWORK1))
             ZMATPOT = (KK%XMPOTSAT(JI,JL)+ZWORK3*ZMATPOTN)/(1.0+ZWORK3)
             !
             !Total soil water content computation (w=wsat*(psi/mpotsat)**(-1/bcoef))
             !
             ZWORK1 = MAX(1.0,ZMATPOT/KK%XMPOTSAT(JI,JL))
             ZLOG   = LOG(ZWORK1)/KK%XBCOEF(JI,JL)
             ZWTOT  = KK%XWSAT(JI,JL)*EXP(-ZLOG)
             ZWTOT  = MAX(XWGMIN,ZWTOT)
             !
             !Soil liquid water content computation
             !
             ZMATPOT        = XLMTT*(PEK%XTG(JI,JL)-XTT)/(XG*PEK%XTG(JI,JL))
             ZMATPOT        = MIN(KK%XMPOTSAT(JI,JL),ZMATPOT)
             ZWORK1         = MAX(1.0,ZMATPOT/KK%XMPOTSAT(JI,JL))
             ZLOG           = LOG(ZWORK1)/KK%XBCOEF(JI,JL)
             ZWL            = KK%XWSAT(JI,JL)*EXP(-ZLOG)
             ZWL            = MAX(ZWL,XWGMIN)
             PEK%XWG(JI,JL) = MIN(ZWL,ZWTOT )
             !       
             !Soil ice computation
             !
             PEK%XWGI(JI,JL) = MAX(0.0,ZWTOT-PEK%XWG(JI,JL))
             !
             !Supress numerical artefact
             !
             IF(PEK%XTG(JI,JL)>=XTT)THEN
               PEK%XWG (JI,JL) = MIN(PEK%XWG(JI,JL)+PEK%XWGI(JI,JL),KK%XWSAT(JI,JL))
               PEK%XWGI(JI,JL) = 0.0
             ENDIF
             !
           ENDIF
           !
         ELSE
           !
           PEK%XWG (JI,JL) = XUNDEF
           PEK%XWGI(JI,JL) = XUNDEF
           !
         ENDIF
      ENDDO
   ENDDO
ENDDO
!
!-----------------------------------------------------------------
!
!*      4.     Compute total land surface water storage (kg m-2)
!              -------------------------------------------------
! 
DO JP=1,INP
   PK => NP%AL(JP)
   PEK => NPE%AL(JP)
   DO JL=1,INL
      DO JI=1,PK%NSIZE_P
         IF(PEK%XWG(JI,JL)/=XUNDEF.AND.PK%XDG(JI,JL)/=XUNDEF) THEN
           IMASK = PK%NR_P(JI)
           ZWG (IMASK) = ZWG (IMASK) + PK%XPATCH(JI)*PK%XDZG(JI,JL)*PEK%XWG (JI,JL)*XRHOLW
           ZWGI(IMASK) = ZWGI(IMASK) + PK%XPATCH(JI)*PK%XDZG(JI,JL)*PEK%XWGI(JI,JL)*XRHOLW
         ENDIF
      ENDDO
   ENDDO
ENDDO
!
DO JP=1,INP
   PK => NP%AL(JP)
   PEK => NPE%AL(JP)
   DO JL=1,INS
      DO JI=1,PK%NSIZE_P
         IMASK = PK%NR_P(JI)
         ZSWE(IMASK) = ZSWE(IMASK) + PK%XPATCH(JI)*PEK%TSNOW%WSNOW(JI,JL)
      ENDDO
   ENDDO
ENDDO
!
DO JP=1,INP
   PK => NP%AL(JP)
   PEK => NPE%AL(JP)
   DO JI=1,PK%NSIZE_P
      IMASK = PK%NR_P(JI)
      ZWR(IMASK) = ZWR(IMASK) + PK%XPATCH(JI)*PEK%XWR(JI)
   ENDDO
ENDDO
!
ZTWS(:) = ZWG(:) + ZWGI(:) + ZWR(:) + ZSWE(:)
!
!-----------------------------------------------------------------
!
!*      5.    Land-use induced water mass (kg m-2)
!              -----------------------------------
! 
S%XWCONSRV(:) = ZTWS(:) - ZTWS_OLD(:)
!
!-----------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('LANDUSE_HYDRO',1,ZHOOK_HANDLE)
!-----------------------------------------------------------------
!
END SUBROUTINE LANDUSE_HYDRO
