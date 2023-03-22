!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
    SUBROUTINE LANDUSE_HYDRO_NUDGING(IO, NK, NP, KI)
!   ###############################################################
!!****  *LAND USE HYDRO NUDGIND*
!!
!!    PURPOSE
!!    -------
!
!     Performs land use land cover change computation for the nudging
!     climatologies of WGT and SWE!               
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
!!    J. Colin 12/2017
!!    Directly adapted from LAND_USE_HYDRO (R. Séférian & B. Decharme 08/2015)
!!
!!
!-------------------------------------------------------------------------------
!
!*       0.     DECLARATIONS
!               ------------

!
USE MODD_ISBA_OPTIONS_n, ONLY : ISBA_OPTIONS_t
USE MODD_ISBA_n,         ONLY : ISBA_P_t, ISBA_K_t, ISBA_NK_t, ISBA_NP_t
!
USE MODD_SURF_PAR,       ONLY : XUNDEF,NUNDEF
!
USE MODD_ISBA_PAR,       ONLY : XWGMIN, XWTD_MAXDEPTH
!
USE YOMHOOK   ,          ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,          ONLY : JPRB
!
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
TYPE(ISBA_OPTIONS_t),  INTENT(INOUT) :: IO
TYPE(ISBA_NK_t),       INTENT(INOUT) :: NK
TYPE(ISBA_NP_t),       INTENT(INOUT) :: NP
!
INTEGER,               INTENT(IN)    :: KI
!
!*      0.2    declarations of local parameter
!
TYPE(ISBA_P_t),  POINTER :: PK
TYPE(ISBA_K_t),  POINTER :: KK
!
INTEGER, DIMENSION(KI,IO%NPATCH):: IWG_LAYER
!
! working tables
!
REAL     :: ZWORK1, ZWORK2, ZWORK3, &
            ZLOG, ZWTOT, ZWL,       &
            ZMATPOT, ZMATPOTN
!
! Indexes and dimensions
INTEGER  :: INL, INP, IDEPTH, INTIME, IMASK
INTEGER  :: JI, JL, JP, JT
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
INTIME=SIZE(NP%AL(1)%XNUDG_WGTOT,3)
!
! 1.1 Initialize
!
IWG_LAYER(:,:) = 0
!
!-----------------------------------------------------------------
!
!*      3.     Compute current year water profile
!              -------------------------
!
DO JP=1,INP
   PK => NP%AL(JP)
   DO JL=1,INL
      DO JI=1,KI
         IF(PK%NWG_LAYER(JI)/=NUNDEF.AND.PK%XNUDG_WGTOT(JI,JL,1)/=XUNDEF)THEN
           IWG_LAYER(JI,JP) = JL
         ENDIF
      ENDDO
   ENDDO
ENDDO
!
DO JP=1,INP
   PK => NP%AL(JP)
   KK => NK%AL(JP)  
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
           IF(PK%XNUDG_WGTOT(JI,JL,1)==XUNDEF) THEN
             !
             IMASK = PK%NR_P(JI)
             !
             IDEPTH = IWG_LAYER(IMASK,JP)
             !
             DO JT=1,INTIME
               !
               !Total matric potential : psi=mpotsat*(w/wsat)**(-bcoef)
               !
               ZWORK1   = MIN(1.0,(PK%XNUDG_WGTOT(JI,IDEPTH,JT))/KK%XWSAT(JI,IDEPTH))
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
               !
               PK%XNUDG_WGTOT(JI,JL,JT)  = MAX(XWGMIN,ZWTOT)
               !
             ENDDO ! End loop on time dimension  
             !
           ENDIF
           !
         ELSE
           !
           PK%XNUDG_WGTOT(JI,JL,:) = XUNDEF
           !
         ENDIF
         !
    ENDDO
  ENDDO
ENDDO
!
!-----------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('LANDUSE_HYDRO',1,ZHOOK_HANDLE)
!-----------------------------------------------------------------
!
END SUBROUTINE LANDUSE_HYDRO_NUDGING
