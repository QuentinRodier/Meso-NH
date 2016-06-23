!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!#############################################################
SUBROUTINE INIT_ISBA_LANDUSE (DTCO, IG, I, UG, U, &
                               HPROGRAM)  
!#############################################################
!
!!****  *INIT_ISBA_LANDUSE* - routine to initialize land use for ISBA field
!!
!!    PURPOSE
!!    -------
!     Extrapolation from existing surounding cells with same patch properties:
!!      (1) IPTS=n  interpol field with n pts
!!      (2) IPTS=0  conserve cells mass  
!!   Case 2 : simple extrapolation based on the inside cell informations.
!!             this is donne before conserving cell or global mass
!!
!!
!!**  METHOD
!!    ------
!!
!!    EXTERNAL
!!    --------
!!
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!
!!    REFERENCE
!!    ---------
!!
!!
!!    AUTHOR
!!    ------
!!      B. Decharme   *Meteo France*
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    07/2011
!!
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
!
!
!
USE MODD_DATA_COVER_n, ONLY : DATA_COVER_t
USE MODD_ISBA_GRID_n, ONLY : ISBA_GRID_t
USE MODD_ISBA_n, ONLY : ISBA_t
USE MODD_SURF_ATM_GRID_n, ONLY : SURF_ATM_GRID_t
USE MODD_SURF_ATM_n, ONLY : SURF_ATM_t
!
USE MODD_TYPE_SNOW
USE MODD_SURF_PAR,ONLY : XUNDEF                 
!
USE MODI_GET_LUOUT
USE MODI_INI_VAR_FROM_PATCH
USE MODI_CONSERV_GLOBAL_MASS
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*       0.1   Declarations of arguments
!              -------------------------
!
!
TYPE(DATA_COVER_t), INTENT(INOUT) :: DTCO
TYPE(ISBA_GRID_t), INTENT(INOUT) :: IG
TYPE(ISBA_t), INTENT(INOUT) :: I
TYPE(SURF_ATM_GRID_t), INTENT(INOUT) :: UG
TYPE(SURF_ATM_t), INTENT(INOUT) :: U
!
 CHARACTER(LEN=6),                 INTENT(IN)  :: HPROGRAM  ! program calling surf. schemes
!
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
REAL, DIMENSION(SIZE(I%XDG,1),SIZE(I%XDG,2),SIZE(I%XDG,3)) :: ZZDG     ! Actual layer thicknesses
REAL, DIMENSION(SIZE(I%XDG,1),SIZE(I%XDG,2),SIZE(I%XDG,3)) :: ZZDG_OLD ! Old layer thicknesses
REAL, DIMENSION(SIZE(I%XDG,1),SIZE(I%XDG,2),SIZE(I%XDG,3)) :: ZWG_OLD  ! Old XWG
REAL, DIMENSION(SIZE(I%XDG,1),SIZE(I%XDG,2),SIZE(I%XDG,3)) :: ZWGI_OLD ! Old XWGI
REAL, DIMENSION(SIZE(I%XDG,1),1,SIZE(I%XDG,3)) :: ZTEST
!
INTEGER :: ILUOUT
INTEGER :: JLAYER, JNBIOMASS, JNLITTER, JNLITTLEVS, JNSOILCARB
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('INIT_ISBA_LANDUSE',0,ZHOOK_HANDLE)
 CALL GET_LUOUT(HPROGRAM,ILUOUT)
!
!-------------------------------------------------------------------------------
!
IF(ALL(I%XDG(:,I%NGROUND_LAYER,:)==I%XDG_OLD(:,I%NGROUND_LAYER,:)))THEN
  IF (LHOOK) CALL DR_HOOK('INIT_ISBA_LANDUSE',1,ZHOOK_HANDLE)
  RETURN
ENDIF
!
!-------------------------------------------------------------------------------
! Conserve mass in the cell
!-------------------------------------------------------------------------------
!
 CALL INI_VAR_FROM_PATCH(DTCO, I, UG, U, &
                         HPROGRAM,ILUOUT,'WR      ', I%XWR     (:,:),0)

IF (I%LGLACIER) CALL INI_VAR_FROM_PATCH(DTCO, I, UG, U, &
                         HPROGRAM,ILUOUT,'ICE_STO ', I%XICE_STO(:,:),0)
!
DO JLAYER=1,SIZE(I%XTG,2)
   CALL INI_VAR_FROM_PATCH(DTCO, I, UG, U, &
                         HPROGRAM,ILUOUT,'TEMP GRO', I%XTG(:,JLAYER,:),0)
END DO
!
!
 CALL INI_VAR_FROM_PATCH(DTCO, I, UG, U, &
                         HPROGRAM,ILUOUT,'ALBSNOW ', I%TSNOW%ALB(:,:),0)
!
IF (I%TSNOW%SCHEME=='1-L'  .OR. I%TSNOW%SCHEME=='3-L' .OR. I%TSNOW%SCHEME=='CRO') THEN
   CALL INI_VAR_FROM_PATCH(DTCO, I, UG, U, &
                         HPROGRAM,ILUOUT,'EMISSNOW', I%TSNOW%EMIS(:,:),0)    
   CALL INI_VAR_FROM_PATCH(DTCO, I, UG, U, &
                         HPROGRAM,ILUOUT,'TSSNOW  ', I%TSNOW%TS  (:,:),0)
ENDIF
!
DO JLAYER=1,I%TSNOW%NLAYER
   !
   CALL INI_VAR_FROM_PATCH(DTCO, I, UG, U, &
                         HPROGRAM,ILUOUT,'WSNOW   ', I%TSNOW%WSNOW(:,JLAYER,:),0)
   !
   IF (I%TSNOW%SCHEME=='3-L' .OR. I%TSNOW%SCHEME=='CRO') THEN            
      CALL INI_VAR_FROM_PATCH(DTCO, I, UG, U, &
                         HPROGRAM,ILUOUT,'TEMPSNOW', I%TSNOW%TEMP(:,JLAYER,:),0)
      CALL INI_VAR_FROM_PATCH(DTCO, I, UG, U, &
                         HPROGRAM,ILUOUT,'HEATSNOW', I%TSNOW%HEAT(:,JLAYER,:),0)     
      CALL INI_VAR_FROM_PATCH(DTCO, I, UG, U, &
                         HPROGRAM,ILUOUT,'AGESNOW ', I%TSNOW%AGE (:,JLAYER,:),0)
   ENDIF
   !
   IF (I%TSNOW%SCHEME=='1-L') THEN
      CALL INI_VAR_FROM_PATCH(DTCO, I, UG, U, &
                         HPROGRAM,ILUOUT,'TSNOW   ', I%TSNOW%T(:,JLAYER,:),0)
   ENDIF
   !
   IF(I%TSNOW%SCHEME=='CRO') THEN
      CALL INI_VAR_FROM_PATCH(DTCO, I, UG, U, &
                         HPROGRAM,ILUOUT,'GRANSNOW', I%TSNOW%GRAN1(:,JLAYER,:),0)
      CALL INI_VAR_FROM_PATCH(DTCO, I, UG, U, &
                         HPROGRAM,ILUOUT,'GRANSNOW', I%TSNOW%GRAN2(:,JLAYER,:),0)
      CALL INI_VAR_FROM_PATCH(DTCO, I, UG, U, &
                         HPROGRAM,ILUOUT,'HISTSNOW', I%TSNOW%HIST (:,JLAYER,:),0)
   ENDIF
   !
ENDDO
!
!-------------------------------------------------------------------------------
! Conserve mass globaly because soil depth change
!-------------------------------------------------------------------------------
!
ZWG_OLD(:,:,:) =I%XWG (:,:,:)
ZWGI_OLD(:,:,:)=I%XWGI(:,:,:)
!
DO JLAYER=1,I%NGROUND_LAYER
   CALL INI_VAR_FROM_PATCH(DTCO, I, UG, U, &
                         HPROGRAM,ILUOUT,'WG      ', I%XWG (:,JLAYER,:),0)
   CALL INI_VAR_FROM_PATCH(DTCO, I, UG, U, &
                         HPROGRAM,ILUOUT,'WGI     ', I%XWGI(:,JLAYER,:),0)
ENDDO
!
ZZDG    (:,1,:)=I%XDG    (:,1,:)
ZZDG_OLD(:,1,:)=I%XDG_OLD(:,1,:)
IF(I%CISBA=='DIF')THEN
  DO JLAYER=2,I%NGROUND_LAYER
     ZZDG    (:,JLAYER,:)=I%XDG    (:,JLAYER,:)-I%XDG    (:,JLAYER-1,:)
     ZZDG_OLD(:,JLAYER,:)=I%XDG_OLD(:,JLAYER,:)-I%XDG_OLD(:,JLAYER-1,:)
  ENDDO
ELSE     
  ZZDG    (:,2,:)=I%XDG    (:,2,:)
  ZZDG_OLD(:,2,:)=I%XDG_OLD(:,2,:)
  IF(I%CISBA=='3-L' )THEN
    ZZDG    (:,3,:)=I%XDG    (:,3,:)-I%XDG    (:,2,:)
    ZZDG_OLD(:,3,:)=I%XDG_OLD(:,3,:)-I%XDG_OLD(:,2,:)
  ENDIF 
ENDIF
!
WHERE(ZZDG(:,:,:)    >1.E+10)ZZDG    (:,:,:)=0.
WHERE(ZZDG_OLD(:,:,:)>1.E+10)ZZDG_OLD(:,:,:)=0.
!
 CALL CONSERV_GLOBAL_MASS(DTCO, IG, I, U, &
                          ILUOUT,ZZDG,ZZDG_OLD,I%XWG, ZWG_OLD )
 CALL CONSERV_GLOBAL_MASS(DTCO, IG, I, U, &
                          ILUOUT,ZZDG,ZZDG_OLD,I%XWGI,ZWGI_OLD)
!
!-------------------------------------------------------------------------------
! Extrapolation with 3 pts 
!-------------------------------------------------------------------------------
!
 CALL INI_VAR_FROM_PATCH(DTCO, I, UG, U, &
                         HPROGRAM,ILUOUT,'RESA    ', I%XRESA(:,:),3)
!
DO JLAYER=1,I%TSNOW%NLAYER
   CALL INI_VAR_FROM_PATCH(DTCO, I, UG, U, &
                         HPROGRAM,ILUOUT,'RHOSNOW ', I%TSNOW%RHO  (:,JLAYER,:),3)
ENDDO
!
IF (I%CPHOTO/='NON') THEN
   !
   CALL INI_VAR_FROM_PATCH(DTCO, I, UG, U, &
                         HPROGRAM,ILUOUT,'AN      ', I%XAN   (:,:),3)
   CALL INI_VAR_FROM_PATCH(DTCO, I, UG, U, &
                         HPROGRAM,ILUOUT,'ANDAY   ', I%XANDAY(:,:),3)   
   CALL INI_VAR_FROM_PATCH(DTCO, I, UG, U, &
                         HPROGRAM,ILUOUT,'ANFM    ', I%XANFM (:,:),3)
   CALL INI_VAR_FROM_PATCH(DTCO, I, UG, U, &
                         HPROGRAM,ILUOUT,'LE      ', I%XLE   (:,:),3)
   !
   DO JNBIOMASS=1,I%NNBIOMASS
      CALL INI_VAR_FROM_PATCH(DTCO, I, UG, U, &
                         HPROGRAM,ILUOUT,'RESPBIOM', I%XRESP_BIOMASS(:,JNBIOMASS,:),3)
      CALL INI_VAR_FROM_PATCH(DTCO, I, UG, U, &
                         HPROGRAM,ILUOUT,'BIOMASS ', I%XBIOMASS     (:,JNBIOMASS,:),3)
   ENDDO
   !
   IF (I%CRESPSL=='CNT') THEN
      !
      DO JNLITTLEVS=1,I%NNLITTLEVS
         CALL INI_VAR_FROM_PATCH(DTCO, I, UG, U, &
                         HPROGRAM,ILUOUT,'LIGNINST',I%XLIGNIN_STRUC(:,JNLITTLEVS,:),3)
         DO JNLITTER=1,I%NNLITTER
            CALL INI_VAR_FROM_PATCH(DTCO, I, UG, U, &
                         HPROGRAM,ILUOUT,'LITTER  ',I%XLITTER(:,JNLITTER,JNLITTLEVS,:),3)
         ENDDO
      ENDDO
      !
      DO JNSOILCARB=1,I%NNSOILCARB
         CALL INI_VAR_FROM_PATCH(DTCO, I, UG, U, &
                         HPROGRAM,ILUOUT,'SOILCARB',I%XSOILCARB(:,JNSOILCARB,:),3)
      ENDDO
      !
   ENDIF
   !
ENDIF
!
!-------------------------------------------------------------------------------
!  
IF (LHOOK) CALL DR_HOOK('INIT_ISBA_LANDUSE',1,ZHOOK_HANDLE)
!
END SUBROUTINE INIT_ISBA_LANDUSE
