!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
    SUBROUTINE FIRE_DISTURBANCE(IO, KK, PK, PEK, DEK, DMK, HPROGRAM, PTSTEP, PTIME)  
!   ###############################################################
!!****  *FIRE DISTURBANCE*
!!
!!    PURPOSE
!!    -------
!
!     Compute fire biomass processes
!              
!!**  METHOD
!!    ------
!!
!!    inspired from Thonickle et al. 2001 = GlobFirm model
!!    inspired from Li et al. 2012
!!    inspired from Mangeon et al. 2016
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
!!
!!	R. Séférian
!!      B. Decharme
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    07/2021
!!
!!
!-------------------------------------------------------------------------------
!
!*       0.     DECLARATIONS
!               ------------
!
USE MODD_ISBA_OPTIONS_n,   ONLY : ISBA_OPTIONS_t
USE MODD_ISBA_n,           ONLY : ISBA_K_t, ISBA_P_t, ISBA_PE_t
USE MODD_DIAG_EVAP_ISBA_n, ONLY : DIAG_EVAP_ISBA_t
USE MODD_DIAG_MISC_ISBA_n, ONLY : DIAG_MISC_ISBA_t
!
USE MODD_CSTS,     ONLY : XDAY
!
USE MODD_SURF_PAR, ONLY : XUNDEF
!
USE MODD_FIRE_PAR, ONLY : XTAU_FIRE
!
USE MODI_FIRE_DYN
USE MODI_FIRE_CARBON
!
USE MODI_GET_LUOUT
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
TYPE(ISBA_OPTIONS_t),   INTENT(INOUT) :: IO
TYPE(ISBA_K_t),         INTENT(INOUT) :: KK
TYPE(ISBA_P_t),         INTENT(INOUT) :: PK
TYPE(ISBA_PE_t),        INTENT(INOUT) :: PEK
TYPE(DIAG_EVAP_ISBA_t), INTENT(INOUT) :: DEK
TYPE(DIAG_MISC_ISBA_t), INTENT(INOUT) :: DMK
!
!
CHARACTER(LEN=6),      INTENT(IN)      :: HPROGRAM  ! program calling surf. schemes
!
REAL,                 INTENT(IN)       :: PTSTEP  ! time step
REAL,                 INTENT(IN)       :: PTIME   ! current time since midnight
!
!
!*      0.2    declarations of local parameter
!  
REAL, PARAMETER                                     :: ZTSTEP_DAY  = 1.0    ! Daily time step (day)
REAL, PARAMETER                                     :: ZDEPTH_FIRE = 0.04   ! Depth to compute litter characteristic (m)
!
INTEGER, PARAMETER                                  :: IABOVE = 1
INTEGER, PARAMETER                                  :: IBELOW = 2
!
!*      0.2    declarations of local variable
!
REAL, DIMENSION(SIZE(PEK%XTG,1)                            ) :: ZSURF_LIGNIN ! surface lignin/C ratio (-)
REAL, DIMENSION(SIZE(PEK%XTG,1),                IO%NNLITTER) :: ZSURF_LITTER ! surface litter (gC/m2)
REAL, DIMENSION(SIZE(PEK%XTG,1),SIZE(PEK%XTG,2)            ) :: ZSOIL_LIGNIN ! soil lignin/C ratio (-)
REAL, DIMENSION(SIZE(PEK%XTG,1),SIZE(PEK%XTG,2),IO%NNLITTER) :: ZSOIL_LITTER ! soil litter (gC/m2)
!
REAL, DIMENSION(SIZE(PEK%XTG,1))                        :: ZTEMPLIT        ! mean litter temperature    (K)
REAL, DIMENSION(SIZE(PEK%XTG,1))                        :: ZMOISTLIT       ! mean litter moisture       (m3/m3)
REAL, DIMENSION(SIZE(PEK%XTG,1))                        :: ZWWILT          ! mean litter wilting point  (m3/m3)
REAL, DIMENSION(SIZE(PEK%XTG,1))                        :: ZWFC            ! mean litter field capacity (m3/m3)
!
REAL, DIMENSION(SIZE(PEK%XTG,1))                        :: ZDG_SOIL        ! soil depth and Weight for DIF (m)
REAL, DIMENSION(SIZE(PEK%XTG,1),SIZE(PEK%XTG,2))        :: ZWGHT_SOIL 
!
LOGICAL                                             :: GMASK
!
INTEGER                                             :: INI, INL, JI, JL, IDEPTH, JTYPE, ILUOUT
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-----------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('FIRE_DISTURBANCE',0,ZHOOK_HANDLE)
!
!*      1.     Preliminaries
!              -------------
!
CALL GET_LUOUT(HPROGRAM,ILUOUT)
!
INI=SIZE(PEK%XTG,1)
INL=SIZE(PEK%XTG,2)
!
ZSURF_LIGNIN(:)     = XUNDEF
ZSURF_LITTER(:,:)   = XUNDEF
ZSOIL_LIGNIN(:,:)   = XUNDEF
ZSOIL_LITTER(:,:,:) = XUNDEF
!
!*compute long-term litter characteristic : use the average soil as a proxy
!
  ZWGHT_SOIL(:,:) = 0.  
!
  ZDG_SOIL  (:) = 0.
  ZTEMPLIT  (:) = 0.
  ZMOISTLIT (:) = 0.
!
  IF(IO%CISBA/='DIF')THEN
!          
    ZTEMPLIT (:) = PEK%XTG(:,2)
    ZMOISTLIT(:) = PEK%XWG(:,2) + PEK%XWGI(:,2)
!
  ELSE
!
    DO JI=1,SIZE(PEK%XTG,1)
      IDEPTH=PK%NWG_LAYER(JI)
      ZDG_SOIL(JI)=MIN(ZDEPTH_FIRE,PK%XDG(JI,IDEPTH))
    ENDDO
!
    DO JL=1,SIZE(PEK%XTG,2)
      DO JI=1,SIZE(PK%XDG,1)
         ZWGHT_SOIL(JI,JL) = MIN(PK%XDZG(JI,JL),MAX(0.0,ZDG_SOIL(JI)-PK%XDG(JI,JL)+PK%XDZG(JI,JL)))/ZDG_SOIL(JI)
         ZTEMPLIT  (JI   ) = ZTEMPLIT (JI) + PEK%XTG(JI,JL) * ZWGHT_SOIL(JI,JL)
         ZMOISTLIT (JI   ) = ZMOISTLIT(JI) + (PEK%XWG(JI,JL)+PEK%XWGI(JI,JL)) * ZWGHT_SOIL(JI,JL)
      ENDDO
    ENDDO
!
  ENDIF
!
PEK%XMOISTLIT_FIRE(:) = ((XTAU_FIRE*XDAY-PTSTEP) * PEK%XMOISTLIT_FIRE(:) + PTSTEP * ZMOISTLIT(:)) / (XTAU_FIRE*XDAY)
PEK%XTEMPLIT_FIRE (:) = ((XTAU_FIRE*XDAY-PTSTEP) * PEK%XTEMPLIT_FIRE (:) + PTSTEP * ZTEMPLIT (:)) / (XTAU_FIRE*XDAY)
!
!-----------------------------------------------------------------
!
!*      2.     Fire Disturbance
!              ----------------
!
! Note: this assumes that Fire disturbance occurs at daily time step
!
GMASK = ( PTIME - PTSTEP < 0. ) .AND. ( PTIME >= 0. )
!
IF (GMASK) THEN
!
  ZWWILT(:) = 0.
  ZWFC  (:) = 0.
!
  IF(IO%CISBA/='DIF')THEN       
    ZWWILT(:) = KK%XWWILT(:,2)
    ZWFC  (:) = KK%XWFC(:,2)
  ELSE
    DO JL=1,INL
      DO JI=1,INI
         ZWWILT(JI) = ZWWILT(JI) + KK%XWWILT(JI,JL) * ZWGHT_SOIL(JI,JL)
         ZWFC  (JI) = ZWFC  (JI) + KK%XWFC  (JI,JL) * ZWGHT_SOIL(JI,JL)
      ENDDO
    ENDDO
  ENDIF
!
  IF(IO%CRESPSL=='DIF')THEN
     ZSURF_LIGNIN(:    ) = PEK%XSURFACE_LIGNIN_STRUC(:    )
     ZSURF_LITTER(:,  :) = PEK%XSURFACE_LITTER      (:,  :)
     ZSOIL_LIGNIN(:,:  ) = PEK%XSOILDIF_LIGNIN_STRUC(:,:  )
     ZSOIL_LITTER(:,:,:) = PEK%XSOILDIF_LITTER      (:,:,:)
  ELSE
     ZSURF_LIGNIN(:    ) = PEK%XLIGNIN_STRUC(:,  IABOVE)
     ZSURF_LITTER(:,  :) = PEK%XLITTER      (:,:,IABOVE)
     ZSOIL_LIGNIN(:,1  ) = PEK%XLIGNIN_STRUC(:,  IBELOW)
     ZSOIL_LITTER(:,1,:) = PEK%XLITTER      (:,:,IBELOW)
  ENDIF
!
  CALL FIRE_DYN(PK, PEK, DMK, ZTSTEP_DAY, ZWFC, ZWWILT, ZSURF_LITTER)
!
  CALL FIRE_CARBON(IO, PK, PEK, DEK, DMK, ILUOUT, ZTSTEP_DAY,            &
                   ZSURF_LITTER, ZSURF_LIGNIN, ZSOIL_LIGNIN, ZSOIL_LITTER)
!
  IF(IO%CRESPSL=='DIF')THEN
     PEK%XSURFACE_LIGNIN_STRUC(:    ) = ZSURF_LIGNIN(:    )
     PEK%XSURFACE_LITTER      (:,  :) = ZSURF_LITTER(:,  :)
     PEK%XSOILDIF_LIGNIN_STRUC(:,:  ) = ZSOIL_LIGNIN(:,:  )
     PEK%XSOILDIF_LITTER      (:,:,:) = ZSOIL_LITTER(:,:,:)
  ELSE
      PEK%XLIGNIN_STRUC(:,  IABOVE) = ZSURF_LIGNIN(:    )
      PEK%XLITTER      (:,:,IABOVE) = ZSURF_LITTER(:,  :)
      PEK%XLIGNIN_STRUC(:,  IBELOW) = ZSOIL_LIGNIN(:,1  )
      PEK%XLITTER      (:,:,IBELOW) = ZSOIL_LITTER(:,1,:)
  ENDIF
!
! kgC/m2/day -> kg/m2/s
!
  DEK%XFIRETURNOVER(:) = DEK%XFIRETURNOVER(:) / XDAY
  DEK%XFIRECO2     (:) = DEK%XFIRECO2     (:) / XDAY
  DEK%XFIREBCS     (:) = DEK%XFIREBCS     (:) / XDAY
!
ENDIF
!
IF (LHOOK) CALL DR_HOOK('FIRE_DISTURBANCE',1,ZHOOK_HANDLE)
!-----------------------------------------------------------------
!
END SUBROUTINE FIRE_DISTURBANCE
