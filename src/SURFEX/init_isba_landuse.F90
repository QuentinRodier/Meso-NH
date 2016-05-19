!#############################################################
SUBROUTINE INIT_ISBA_LANDUSE (HPROGRAM)  
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
!!	B. Decharme   *Meteo France*	
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
USE MODD_ISBA_n, ONLY : CISBA, CPHOTO, CRESPSL, LFLOOD, NGROUND_LAYER,    &
                        NNLITTER, NNLITTLEVS, NNSOILCARB, NNBIOMASS,      &
                        XRESA, XDG, XTG, XWG, XWR, XWGI, XAN, XANDAY,     &
                        XANFM, XICE_STO, XLE, XZ0_FLOOD, XBIOMASS,        &
                        XRESP_BIOMASS,XLITTER, XSOILCARB, XLIGNIN_STRUC,  &
                        XDG_OLD,TSNOW
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
 CHARACTER(LEN=6),                 INTENT(IN)  :: HPROGRAM  ! program calling surf. schemes
!
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
REAL, DIMENSION(SIZE(XDG,1),SIZE(XDG,2),SIZE(XDG,3)) :: ZZDG     ! Actual layer thicknesses
REAL, DIMENSION(SIZE(XDG,1),SIZE(XDG,2),SIZE(XDG,3)) :: ZZDG_OLD ! Old layer thicknesses
REAL, DIMENSION(SIZE(XDG,1),SIZE(XDG,2),SIZE(XDG,3)) :: ZWG_OLD  ! Old XWG
REAL, DIMENSION(SIZE(XDG,1),SIZE(XDG,2),SIZE(XDG,3)) :: ZWGI_OLD ! Old XWGI
REAL, DIMENSION(SIZE(XDG,1),1,SIZE(XDG,3)) :: ZTEST
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
IF(ALL(XDG(:,NGROUND_LAYER,:)==XDG_OLD(:,NGROUND_LAYER,:)))THEN
  IF (LHOOK) CALL DR_HOOK('INIT_ISBA_LANDUSE',1,ZHOOK_HANDLE)
  RETURN
ENDIF
!
!-------------------------------------------------------------------------------
! Conserve mass in the cell
!-------------------------------------------------------------------------------
!
 CALL INI_VAR_FROM_PATCH(HPROGRAM,ILUOUT,'WR      ', XWR     (:,:),0)

 CALL INI_VAR_FROM_PATCH(HPROGRAM,ILUOUT,'ICE_STO ', XICE_STO(:,:),0)
!
DO JLAYER=1,SIZE(XTG,2)
   CALL INI_VAR_FROM_PATCH(HPROGRAM,ILUOUT,'TEMP GRO', XTG(:,JLAYER,:),0)
END DO
!
!
 CALL INI_VAR_FROM_PATCH(HPROGRAM,ILUOUT,'ALBSNOW ', TSNOW%ALB(:,:),0)
!
IF (TSNOW%SCHEME=='1-L'  .OR. TSNOW%SCHEME=='3-L' .OR. TSNOW%SCHEME=='CRO') THEN
   CALL INI_VAR_FROM_PATCH(HPROGRAM,ILUOUT,'EMISSNOW', TSNOW%EMIS(:,:),0)    
   CALL INI_VAR_FROM_PATCH(HPROGRAM,ILUOUT,'TSSNOW  ', TSNOW%TS  (:,:),0)
ENDIF
!
DO JLAYER=1,TSNOW%NLAYER
   !
   CALL INI_VAR_FROM_PATCH(HPROGRAM,ILUOUT,'WSNOW   ', TSNOW%WSNOW(:,JLAYER,:),0)
   !
   IF (TSNOW%SCHEME=='3-L' .OR. TSNOW%SCHEME=='CRO') THEN            
      CALL INI_VAR_FROM_PATCH(HPROGRAM,ILUOUT,'TEMPSNOW', TSNOW%TEMP(:,JLAYER,:),0)
      CALL INI_VAR_FROM_PATCH(HPROGRAM,ILUOUT,'HEATSNOW', TSNOW%HEAT(:,JLAYER,:),0)     
   ENDIF
   !
   IF (TSNOW%SCHEME=='1-L') THEN
      CALL INI_VAR_FROM_PATCH(HPROGRAM,ILUOUT,'TSNOW   ', TSNOW%T(:,JLAYER,:),0)
   ENDIF
   !
   IF(TSNOW%SCHEME=='CRO') THEN
      CALL INI_VAR_FROM_PATCH(HPROGRAM,ILUOUT,'GRANSNOW', TSNOW%GRAN1(:,JLAYER,:),0)
      CALL INI_VAR_FROM_PATCH(HPROGRAM,ILUOUT,'GRANSNOW', TSNOW%GRAN2(:,JLAYER,:),0)
      CALL INI_VAR_FROM_PATCH(HPROGRAM,ILUOUT,'HISTSNOW', TSNOW%HIST (:,JLAYER,:),0)
      CALL INI_VAR_FROM_PATCH(HPROGRAM,ILUOUT,'AGESNOW ', TSNOW%AGE  (:,JLAYER,:),0)
   ENDIF
   !
ENDDO
!
!-------------------------------------------------------------------------------
! Conserve mass globaly because soil depth change
!-------------------------------------------------------------------------------
!
ZWG_OLD(:,:,:) =XWG (:,:,:)
ZWGI_OLD(:,:,:)=XWGI(:,:,:)
!
DO JLAYER=1,NGROUND_LAYER
   CALL INI_VAR_FROM_PATCH(HPROGRAM,ILUOUT,'WG      ', XWG (:,JLAYER,:),0)
   CALL INI_VAR_FROM_PATCH(HPROGRAM,ILUOUT,'WGI     ', XWGI(:,JLAYER,:),0)
ENDDO
!
ZZDG    (:,1,:)=XDG    (:,1,:)
ZZDG_OLD(:,1,:)=XDG_OLD(:,1,:)
IF(CISBA=='DIF')THEN
  DO JLAYER=2,NGROUND_LAYER
     ZZDG    (:,JLAYER,:)=XDG    (:,JLAYER,:)-XDG    (:,JLAYER-1,:)
     ZZDG_OLD(:,JLAYER,:)=XDG_OLD(:,JLAYER,:)-XDG_OLD(:,JLAYER-1,:)
  ENDDO
ELSE     
  ZZDG    (:,2,:)=XDG    (:,2,:)
  ZZDG_OLD(:,2,:)=XDG_OLD(:,2,:)
  IF(CISBA=='3-L' )THEN
    ZZDG    (:,3,:)=XDG    (:,3,:)-XDG    (:,2,:)
    ZZDG_OLD(:,3,:)=XDG_OLD(:,3,:)-XDG_OLD(:,2,:)
  ENDIF 
ENDIF
!
WHERE(ZZDG(:,:,:)    >1.E+10)ZZDG    (:,:,:)=0.
WHERE(ZZDG_OLD(:,:,:)>1.E+10)ZZDG_OLD(:,:,:)=0.
!
 CALL CONSERV_GLOBAL_MASS(ILUOUT,ZZDG,ZZDG_OLD,XWG, ZWG_OLD )
 CALL CONSERV_GLOBAL_MASS(ILUOUT,ZZDG,ZZDG_OLD,XWGI,ZWGI_OLD)
!
!-------------------------------------------------------------------------------
! Extrapolation with 3 pts 
!-------------------------------------------------------------------------------
!
 CALL INI_VAR_FROM_PATCH(HPROGRAM,ILUOUT,'RESA    ', XRESA(:,:),3)
!
DO JLAYER=1,TSNOW%NLAYER
   CALL INI_VAR_FROM_PATCH(HPROGRAM,ILUOUT,'RHOSNOW ', TSNOW%RHO  (:,JLAYER,:),3)
ENDDO
!
IF(LFLOOD)CALL INI_VAR_FROM_PATCH(HPROGRAM,ILUOUT,'Z0_FLOOD', XZ0_FLOOD(:,:),3)
!
IF (CPHOTO/='NON') THEN
   !
   CALL INI_VAR_FROM_PATCH(HPROGRAM,ILUOUT,'AN      ', XAN   (:,:),3)
   CALL INI_VAR_FROM_PATCH(HPROGRAM,ILUOUT,'ANDAY   ', XANDAY(:,:),3)   
   CALL INI_VAR_FROM_PATCH(HPROGRAM,ILUOUT,'ANFM    ', XANFM (:,:),3)
   CALL INI_VAR_FROM_PATCH(HPROGRAM,ILUOUT,'LE      ', XLE   (:,:),3)
   !
   DO JNBIOMASS=1,NNBIOMASS
      CALL INI_VAR_FROM_PATCH(HPROGRAM,ILUOUT,'RESPBIOM', XRESP_BIOMASS(:,JNBIOMASS,:),3)
      CALL INI_VAR_FROM_PATCH(HPROGRAM,ILUOUT,'BIOMASS ', XBIOMASS     (:,JNBIOMASS,:),3)
   ENDDO
   !
   IF (CRESPSL=='CNT') THEN
      !
      DO JNLITTLEVS=1,NNLITTLEVS
         CALL INI_VAR_FROM_PATCH(HPROGRAM,ILUOUT,'LIGNINST',XLIGNIN_STRUC(:,JNLITTLEVS,:),3)
         DO JNLITTER=1,NNLITTER
            CALL INI_VAR_FROM_PATCH(HPROGRAM,ILUOUT,'LITTER  ',XLITTER(:,JNLITTER,JNLITTLEVS,:),3)
         ENDDO
      ENDDO
      !
      DO JNSOILCARB=1,NNSOILCARB
         CALL INI_VAR_FROM_PATCH(HPROGRAM,ILUOUT,'SOILCARB',XSOILCARB(:,JNSOILCARB,:),3)
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
