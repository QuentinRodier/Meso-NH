!     #################################
      SUBROUTINE URBAN_HYDRO_HTRANSFERT(T, TOP, CT, PEK, TH, PTSTEP)
!     #################################
!
!     Compute horizontal transfer of water between garden, road and
!     buildings
!
!!**  METHOD
!!    ------
!!    Force-restore to balance water contents of the three compartments:
!!    Horizontal transfer depends on the ratio of hydraulic conductivity of saturation
!!    to the hydraulic conductivity and the constant XDAY of 1 day:
!!              
!!    REFERENCE
!!    ---------
!!
!!
!!    AUTHOR
!!    ------
!!    X. Laffaille	
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    08/2017  
!!      Modif       12/2020  E. Bernard, C. de Munck, V. Masson 
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
USE MODI_INIT_IO_SURF_n                                  
!
USE MODD_CSTS,             ONLY : XDAY, XRHOLW
USE MODD_TEB_n,            ONLY : TEB_t
USE MODD_TEB_OPTION_n,     ONLY : TEB_OPTIONS_t
USE MODD_ISBA_n,           ONLY : ISBA_PE_t
USE MODD_TEB_HYDRO_n,      ONLY : TEB_HYDRO_t
USE MODD_CHECK_TEB,        ONLY : CHECK_TEB_t
!
USE YOMHOOK   ,ONLY : LHOOK, DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*       0.1   Declarations of arguments
!              -------------------------
TYPE(TEB_t),            INTENT(INOUT) :: T
TYPE(TEB_OPTIONS_t),    INTENT(INOUT) :: TOP
TYPE(CHECK_TEB_t),      INTENT(INOUT) :: CT
TYPE(ISBA_PE_t),        INTENT(INOUT) :: PEK
TYPE(TEB_HYDRO_t),      INTENT(INOUT) :: TH
REAL,                   INTENT(IN)    :: PTSTEP             ! time step
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
INTEGER                                     :: JJ, JI       ! For loop on TEB grid and soil grid 
!
REAL(KIND=JPRB)                             :: ZHOOK_HANDLE
REAL, DIMENSION(SIZE(T%XBLD),TOP%NTEB_SOIL) :: ZWG_AVG  ! Water content averaged for the three soil columns
REAL, DIMENSION(SIZE(T%XBLD),TOP%NTEB_SOIL) :: ZWG_GD   ! Water content in gardens, at past time step
REAL                                        :: ZTAU     ! Time dependent constant of lateral transfer
!
!-------------------------------------------------------------------------------
!
!         Initialisation for IO
!
IF (LHOOK) CALL DR_HOOK('URBAN_HYDRO_HTRANSFERT',0,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!  Time constant of lateral transfer
  ZTAU = (1-EXP(-(PTSTEP/(XDAY*30)))) 
!
  ZWG_GD = PEK%XWG

!      Horizontal transfer of water content in the layers between gardens and buildings:
!
   DO JI=1,SIZE(T%XBLD)
       DO JJ=1,TOP%NCOAT_ROAD(JI) 
!         Averaged water content of the two compartments:
          ZWG_AVG(JI,JJ) = (PEK%XWG(JI,JJ)*T%XGARDEN(JI)+TH%XWG_BLD(JI,JJ)*T%XBLD(JI))/&
                            (T%XGARDEN(JI)+T%XBLD(JI))
!         Water content after horizontal transfer in the two compartments:      
          PEK%XWG  (JI,JJ) = PEK%XWG(JI,JJ)     + &
          (ZWG_AVG(JI,JJ) - PEK%XWG(JI,JJ))    * ZTAU
!
          TH%XWG_BLD(JI,JJ) =TH%XWG_BLD(JI,JJ)  + &
          (ZWG_AVG(JI,JJ) - TH%XWG_BLD(JI,JJ)) * ZTAU

       ENDDO
!
!      Horizontal transfer of water content in the layers between gardens, roads and buildings:
!
       DO JJ=TOP%NCOAT_ROAD(JI)+1,TOP%NTEB_SOIL 
!         Averaged water content of the three compartments:
          ZWG_AVG(JI,JJ) = (PEK%XWG(JI,JJ)    *T%XGARDEN(JI)   + &
                            TH%XWG_BLD(JI,JJ) *T%XBLD(JI)      + &
                            TH%XWG_ROAD(JI,JJ)*T%XROAD(JI) )   / &
                            ((T%XGARDEN(JI)+T%XBLD(JI)+T%XROAD(JI)))
!         Water contents after horizontal transfer
          PEK%XWG(JI,JJ)     = PEK%XWG(JI,JJ)     + (ZWG_AVG(JI,JJ) - &
          PEK%XWG(JI,JJ))     * ZTAU 
          TH%XWG_BLD (JI,JJ) = TH%XWG_BLD(JI,JJ)  + (ZWG_AVG(JI,JJ) - &
          TH%XWG_BLD(JI,JJ))  * ZTAU  
          TH%XWG_ROAD(JI,JJ) = TH%XWG_ROAD(JI,JJ) + (ZWG_AVG(JI,JJ) - &
          TH%XWG_ROAD(JI,JJ)) * ZTAU 
       ENDDO
   ENDDO
!
   IF (CT%LCHECK_TEB) THEN
     ! flux positive when going from road+ bld soil towards garden 
     CT%XLAT_SOIL_TO_GARDEN(:) = 0.
     DO JJ=1,TOP%NTEB_SOIL 
       CT%XLAT_SOIL_TO_GARDEN(:) = CT%XLAT_SOIL_TO_GARDEN(:)  &
           + (ZWG_AVG(:,JJ) - ZWG_GD(:,JJ)) * ZTAU * T%XD_ROAD(:,JJ) / PTSTEP * XRHOLW
     END DO
   END IF

!-------------------------------------------------------------------------------
!
!         End of IO
!
IF (LHOOK) CALL DR_HOOK('URBAN_HYDRO_HTRANSFERT',1,ZHOOK_HANDLE)
!
END SUBROUTINE URBAN_HYDRO_HTRANSFERT
