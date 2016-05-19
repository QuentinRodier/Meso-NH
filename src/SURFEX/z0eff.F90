!     #########
    SUBROUTINE Z0EFF(HROUGH, PALFA, PZREF, PUREF, PZ0, PZ0REL, PPSN,          &
                      PZ0EFFIP,PZ0EFFIM,PZ0EFFJP,PZ0EFFJM,PFF,PZ0_FLOOD,        &
                      PAOSIP,PAOSIM,PAOSJP,PAOSJM,PHO2IP,PHO2IM,PHO2JP,PHO2JM,  &
                      PZ0_O_Z0H, PZ0_WITH_SNOW, PZ0H_WITH_SNOW,PZ0EFF           )  
!   ############################################################################
!
!!****  *Z0EFF*  
!!
!!    PURPOSE
!!    -------
!
!     Calculates the z0eff for momentum fluxes according to wind direction.
!         
!     
!!**  METHOD
!!    ------
!
!
!!    EXTERNAL
!!    --------
!!
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!
!!      
!!    REFERENCE
!!    ---------
!!
!!    Mascart et al. (1995)
!!    Belair (1995)
!!      
!!    AUTHOR
!!    ------
!!
!!	S. Belair           * Meteo-France *
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    13/03/95 
!!      (J.Stein)   15/11/95  use the potential temperature to compute Ri
!!                            and PVMOD instead of ZVMOD
!!      (P.Lacarrere)15/03/96 replace * PEXNS by / PEXNS
!!      (V.Masson)   22/12/97 computation of z0eff after snow treatment
!!      (V.Masson)   05/10/98 clear routine
!!      (A.Boone)    11/26/98 Option for PDELTA: forested vs default surface
!!      (V Masson)   12/07/01 new formulation for aggregation with snow z0
!!      (P.LeMoigne) 09/02/06 computation of z0h in presence of snow
!!      (B; Decharme)    2008 floodplains
!-------------------------------------------------------------------------------
!
!*       0.     DECLARATIONS
!               ------------
!
USE MODD_CSTS,     ONLY : XPI, XG
USE MODD_SNOW_PAR, ONLY : XZ0SN, XWCRN, XZ0HSN
USE MODD_ISBA_n,   ONLY : TSNOW
!
USE MODI_SUBSCALE_Z0EFF
USE MODD_SURF_ATM, ONLY : LALDZ0H
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
!
 CHARACTER(LEN=*),   INTENT(IN)  :: HROUGH         ! type of roughness length
REAL, DIMENSION(:), INTENT(IN)  :: PALFA          ! wind direction from J axis (clockwise)
REAL, DIMENSION(:), INTENT(IN)  :: PZREF          ! height of atmospheric level
REAL, DIMENSION(:), INTENT(IN)  :: PUREF          ! reference height for wind
REAL, DIMENSION(:), INTENT(IN)  :: PZ0            ! vegetation roughness length
REAL, DIMENSION(:), INTENT(IN)  :: PZ0REL         ! 1d orographic roughness length
REAL, DIMENSION(:), INTENT(IN)  :: PPSN           ! fraction of snow
REAL, DIMENSION(:), INTENT(IN)  :: PZ0EFFIP       ! z0eff for increasing x
REAL, DIMENSION(:), INTENT(IN)  :: PZ0EFFIM       ! z0eff for decreasing x
REAL, DIMENSION(:), INTENT(IN)  :: PZ0EFFJP       ! z0eff for increasing y
REAL, DIMENSION(:), INTENT(IN)  :: PZ0EFFJM       ! z0eff for decreasing y
REAL, DIMENSION(:), INTENT(IN)  :: PAOSIP         ! A/S for increasing x
REAL, DIMENSION(:), INTENT(IN)  :: PAOSIM         ! A/S for decreasing x
REAL, DIMENSION(:), INTENT(IN)  :: PAOSJP         ! A/S for increasing y
REAL, DIMENSION(:), INTENT(IN)  :: PAOSJM         ! A/S for decreasing y
REAL, DIMENSION(:), INTENT(IN)  :: PHO2IP         ! h/2 for increasing x
REAL, DIMENSION(:), INTENT(IN)  :: PHO2IM         ! h/2 for decreasing x
REAL, DIMENSION(:), INTENT(IN)  :: PHO2JP         ! h/2 for increasing y
REAL, DIMENSION(:), INTENT(IN)  :: PHO2JM         ! h/2 for decreasing y
REAL, DIMENSION(:), INTENT(IN)  :: PZ0_O_Z0H      ! ratio between heat and momentum z0
!
REAL, DIMENSION(:), INTENT(IN)  :: PFF            ! fraction of flood
REAL, DIMENSION(:), INTENT(IN)  :: PZ0_FLOOD      ! floodplains roughness length
!
REAL, DIMENSION(:), INTENT(OUT) :: PZ0_WITH_SNOW  ! vegetation z0 modified by snow
REAL, DIMENSION(:), INTENT(OUT) :: PZ0H_WITH_SNOW ! vegetation z0h modified by snow
REAL, DIMENSION(:), INTENT(OUT) :: PZ0EFF         ! effective z0
!
!
!
!
!*      0.2    declarations of local variables
!
!
!
REAL, DIMENSION(SIZE(PZ0EFF)) :: ZWORK, ZALFA,       &
                                   ZZ0EFFIP, ZZ0EFFIM, &
                                   ZZ0EFFJP, ZZ0EFFJM  
!                                              effective roughness length in 4
!                                              directions
REAL                          :: Z0CR, ZUZ0CN, ZALRCN1, ZALRCN2
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('Z0EFF',0,ZHOOK_HANDLE)
ZALRCN1=1.E-03
ZALRCN2=2.5E-03
Z0CR = XG*ZALRCN1
ZUZ0CN=1./(XG*ZALRCN2)
ZALFA(:) = PALFA(:)
WHERE(ZALFA(:)<=-XPI) ZALFA = ZALFA + 2.*XPI
WHERE(ZALFA(:)>  XPI) ZALFA = ZALFA - 2.*XPI
!
!
!*       1.     GRID-AVERAGED ROUGHNESS LENGTHS
!               -------------------------------
!       (considering the effect of snow-flood-covered surfaces and orography)
!
!*       1.1    for heat
!               --------
!
PZ0_WITH_SNOW(:)  = PZ0(:)
PZ0H_WITH_SNOW(:) = PZ0(:) / PZ0_O_Z0H(:)
!
IF(TSNOW%SCHEME=='EBA') THEN
!        
   WHERE (PPSN(:)>0.)
!
!!!!!Flooding scheme not implemented with this option 
      PZ0_WITH_SNOW(:) = PZ0_WITH_SNOW(:) + ( Z0CR - PZ0(:))* &
        PPSN(:)/(PPSN(:) + XWCRN*(1.0+ZUZ0CN*PZ0(:)))  
!        
   END WHERE 


   IF (LALDZ0H) THEN  
     WHERE (PPSN(:)>0.)
         PZ0H_WITH_SNOW(:) = PZ0H_WITH_SNOW(:) + ( Z0CR - PZ0H_WITH_SNOW(:))* &
           PPSN(:)/(PPSN(:) + XWCRN*(1.0+ZUZ0CN*PZ0H_WITH_SNOW(:)))   
     END WHERE  
  END IF   
    
!        
ELSE
!        
   WHERE (PPSN(:)>0..OR.PFF(:)>0.)
!        
      ZWORK(:) =  (            PPSN(:) /(LOG(PUREF(:)/XZ0SN       ))**2 ) &
                + (            PFF (:) /(LOG(PUREF(:)/PZ0_FLOOD(:)))**2 ) &
                + ((1.-PPSN(:)-PFF (:))/(LOG(PUREF(:)/PZ0(:)      ))**2 )  
!
      PZ0_WITH_SNOW(:) = PUREF(:) /EXP( SQRT( 1./ZWORK(:) ) )
!
      ZWORK(:) =  (            PPSN(:) /(LOG(PZREF(:)/XZ0HSN                      ))**2 ) &
                + (            PFF (:) /(LOG(PZREF(:)/(PZ0_FLOOD(:)/ PZ0_O_Z0H(:))))**2 ) &
                + ((1.-PPSN(:)-PFF (:))/(LOG(PZREF(:)/(PZ0(:)/PZ0_O_Z0H(:))       ))**2 )  
!
      PZ0H_WITH_SNOW(:) = PZREF(:) /EXP( SQRT( 1./ZWORK(:) ) )
!
   END WHERE
!
ENDIF
!
!
!*       1.2    for momentum
!               ------------
!
!
!                                     In this particular case, we now use
!                                     the roughness length due to the coupled
!                                     effect of vegetation and topography
!                                     Snow and Flood effects are yet taken
!                                     into account through ZZ0EFF
!
IF (HROUGH=='Z04D') THEN
  !
  ZZ0EFFIP(:) = PZ0EFFIP(:)
  ZZ0EFFIM(:) = PZ0EFFIM(:)
  ZZ0EFFJP(:) = PZ0EFFJP(:)
  ZZ0EFFJM(:) = PZ0EFFJM(:)
  !
  CALL SUBSCALE_Z0EFF(PAOSIP,PAOSIM,PAOSJP,PAOSJM,               &
                        PHO2IP,PHO2IM,PHO2JP,PHO2JM,PZ0_WITH_SNOW, &
                        ZZ0EFFIP,ZZ0EFFIM,ZZ0EFFJP,ZZ0EFFJM,       &
                        OMASK=(PPSN>0..OR.PFF(:)>0.)               )  
  !
  WHERE(ZALFA(:)>=0. .AND. ZALFA(:)<XPI/2.)
    PZ0EFF(:)=ZZ0EFFIP(:)*SIN(ZALFA(:))**2 + ZZ0EFFJP(:)*COS(ZALFA(:))**2
  END WHERE
  WHERE(ZALFA(:)>=XPI/2. .AND. ZALFA(:)<=XPI)
    PZ0EFF(:)=ZZ0EFFIP(:)*SIN(ZALFA(:))**2 + ZZ0EFFJM(:)*COS(ZALFA(:))**2
  END WHERE
  WHERE (ZALFA(:)>=-XPI/2 .AND. ZALFA(:)<0.)
    PZ0EFF(:)=ZZ0EFFIM(:)*SIN(ZALFA(:))**2 + ZZ0EFFJP(:)*COS(ZALFA(:))**2
  END WHERE
  WHERE (ZALFA(:)>=-XPI .AND. ZALFA(:)<-XPI/2.)
    PZ0EFF(:)=ZZ0EFFIM(:)*SIN(ZALFA(:))**2 + ZZ0EFFJM(:)*COS(ZALFA(:))**2
  END WHERE
!
ELSE IF (HROUGH=='Z01D') THEN
  PZ0EFF(:) = PZ0_WITH_SNOW(:) + PZ0REL(:)
  IF (LALDZ0H) THEN
     ! Aladin dynamic z0 contains already orographic component
     PZ0EFF(:) = PZ0EFF(:) - PZ0REL(:)
!     PZ0H_WITH_SNOW(:) = PZ0EFF(:) / PZ0_O_Z0H(:)   ! it is aleardy corrected under IF statement of TSNOW%SCHEME  
  ENDIF
ELSE
  PZ0EFF(:) = PZ0_WITH_SNOW(:)
END IF
IF (LHOOK) CALL DR_HOOK('Z0EFF',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE Z0EFF
