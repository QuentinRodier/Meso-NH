!-----------------------------------------------------------------
!--------------- special set of characters for RCS information
!-----------------------------------------------------------------
! $Source$ $Revision$
! MASDEV4_7 spawn 2006/05/23 15:36:44
!-----------------------------------------------------------------
!#######################
MODULE MODI_SPAWN_PRESSURE2
!#######################
!
INTERFACE
!
      SUBROUTINE SPAWN_PRESSURE2(KXOR,KYOR,KXEND,KYEND,KDXRATIO,KDYRATIO,   &
                                PZZ_LS,PZZ,PTHVM,PTHVT,                     &
                                PPABSM,PPABST                               )
!
INTEGER,   INTENT(IN)  :: KXOR,KXEND !  horizontal position (i,j) of the ORigin and END
INTEGER,   INTENT(IN)  :: KYOR,KYEND ! of the model 2 domain, relative to model 1
INTEGER,   INTENT(IN)  :: KDXRATIO   !  x and y-direction Resolution ratio
INTEGER,   INTENT(IN)  :: KDYRATIO   ! between model 2 and model 1
REAL, DIMENSION(:,:,:),   INTENT(IN) :: PZZ_LS     ! purely interpolated alt.
REAL, DIMENSION(:,:,:),   INTENT(IN) :: PZZ        ! model 2 altitudes
!                                                  !   model 2
REAL, DIMENSION(:,:,:),   INTENT(IN) :: PTHVM      ! virt. pot. temp. at t-dt
!
!                                                  !   model 2
REAL, DIMENSION(:,:,:),   INTENT(IN) :: PTHVT      ! virt. pot. temp. at t
!
REAL, DIMENSION(:,:,:),   INTENT(OUT) :: PPABSM    ! model 2 pressure a t-dt
REAL, DIMENSION(:,:,:),   INTENT(OUT) :: PPABST    ! model 2 pressure a t
!
END SUBROUTINE SPAWN_PRESSURE2
!
END INTERFACE
!
END MODULE MODI_SPAWN_PRESSURE2
!
!     #######################################################################
      SUBROUTINE SPAWN_PRESSURE2(KXOR,KYOR,KXEND,KYEND,KDXRATIO,KDYRATIO,   &
                                PZZ_LS,PZZ,PTHVM,PTHVT,                     &
                                PPABSM,PPABST                               )
!     #######################################################################
!
!!****  *SPAWN_PRESSURE2 * - subroutine generating the model 2 pressure
!!                      fields, consistently with the spawning model 1,
!!                      and the model 2 thermodynamic variables.
!!
!!    PURPOSE
!!    -------
!!
!!
!!**  METHOD
!!    ------
!!
!!      The model 2 variables are transmitted by argument (P or K prefixes),
!!    while the ones of model 1 are declared through calls to MODD_...
!!    (X or N prefixes)
!!
!!      For the case where the resolution ratio between models is 1,
!!    the horizontal interpolation becomes a simple equality.
!!      For the general case where resolution ratio is not egal to one,
!!    fields are interpolated using 2 types of interpolations:
!!                 1. Clark and Farley (JAS 1984) on 9 points
!!                 2. Bikhardt on 16 points
!!
!!       In case of resolution change:
!!
!!    1. Model 1 top pressure is computed
!!    2. Hydrostatic pressure is computed from top in model 1
!!    3. Difference between absolute pressure and hyd. pressure is kept
!!    4. Model top pressure is interpolated
!!    5. Difference between the pressures is interpolated
!!    6. Hydrostatic pressure is computed in model 2
!!    7. Absolute pressure is recovered as sum of hydrostatic pressure
!!         and difference between absolute pressure and hydrostatic pressure
!!
!!    EXTERNAL
!!    --------
!!
!!      Routine BIKHARDT      : to perform horizontal interpolations
!!
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!      Module MODD_PARAMETERS : contains parameters
!!      Module MODD_CONF       : contains NVERB
!!      Module MODD_CONF1      : contains CONF_MODEL(1)%NRR (total Number of moist variables)
!!      Module MODD_FIELD1     : contains pronostic variables of model 1
!!      Module MODD_GRID1      : contains grid variables
!!
!!    REFERENCE
!!    ---------
!!
!!       Book1 of the documentation
!!       SUBROUTINE SPAWN_PRESSURE2 (Book2 of the documentation)
!!
!!
!!    AUTHOR
!!    ------
!!
!!       V. Masson     * METEO-FRANCE *
!!
!!    MODIFICATIONS
!!    -------------
!!
!!      Original    10/07/97
!!                  14/09/97 (V. Masson) use of thv as dummy argument
!!      Modification 20/05/06 Remove Clark and Farley interpolation
!-------------------------------------------------------------------------------
!
!*       0.     DECLARATIONS
!               ------------
!
USE MODD_PARAMETERS       ! Declarative modules
USE MODD_CONF
USE MODD_CST
!
USE MODD_GRID_n, ONLY: GRID_MODEL 
USE MODD_CONF_n, ONLY: CONF_MODEL
USE MODD_LBC_n,  ONLY: LBC_MODEL
USE MODD_LUNIT_n,ONLY: LUNIT_MODEL
USE MODD_FIELD_n,ONLY: FIELD_MODEL
USE MODD_REF_n,  ONLY: REF_MODEL
!
USE MODD_BIKHARDT_n
USE MODD_VER_INTERP_LIN
!
USE MODI_SHUMAN
USE MODI_BIKHARDT
USE MODI_COMPUTE_EXNER_FROM_TOP
USE MODI_COEF_VER_INTERP_LIN
USE MODI_VER_INTERP_LIN
!
USE MODE_MODELN_HANDLER
!
IMPLICIT NONE
!
!*       0.1   Declarations of dummy arguments :
!
!
INTEGER,   INTENT(IN)  :: KXOR,KXEND !  horizontal position (i,j) of the ORigin and END
INTEGER,   INTENT(IN)  :: KYOR,KYEND ! of the model 2 domain, relative to model 1
INTEGER,   INTENT(IN)  :: KDXRATIO   !  x and y-direction Resolution ratio
INTEGER,   INTENT(IN)  :: KDYRATIO   ! between model 2 and model 1
REAL, DIMENSION(:,:,:),   INTENT(IN) :: PZZ_LS     ! purely interpolated alt.
REAL, DIMENSION(:,:,:),   INTENT(IN) :: PZZ        ! model 2 altitudes
!                                                  !   model 2
REAL, DIMENSION(:,:,:),   INTENT(IN) :: PTHVM      ! virt. pot. temp. at t-dt
!
!                                                  !   model 2
REAL, DIMENSION(:,:,:),   INTENT(IN) :: PTHVT      ! virt. pot. temp. at t
!
REAL, DIMENSION(:,:,:),   INTENT(OUT) :: PPABSM    ! model 2 pressure a t-dt
REAL, DIMENSION(:,:,:),   INTENT(OUT) :: PPABST    ! model 2 pressure a t
!
!*       0.2    Declarations of local variables
!
INTEGER             :: JRR       ! Loop index for moist variables
INTEGER             :: IIU       ! Upper dimension in x direction (inner model)
INTEGER             :: IJU       ! Upper dimension in y direction (inner model)
INTEGER             :: IIU1      ! Upper dimension in x direction (outer model)
INTEGER             :: IJU1      ! Upper dimension in y direction (outer model)
INTEGER             :: IKE,IKU   ! vertical indexes for models 1 and 2
INTEGER             :: JCOUNT      ! iterative loop counter
INTEGER             :: JINSTANT    ! 1: time t-dt, 2: time t
!
REAL, DIMENSION(:,:,:), ALLOCATABLE :: & ! MODEL 1 VARIABLES
 ZEXN1,       & ! Exner functions at mass points               at t or t-dt
 ZTHV1,       & ! virtual potential temperature at mass points at t or t-dt
 ZHYDEXN1,    & ! hydrostatic Exner functions at mass points   at t or t-dt
 ZSUMR          ! sum of water mixing ratios (at t-dt or t)  
REAL, DIMENSION(SIZE(FIELD_MODEL(1)%XTHT,1),SIZE(FIELD_MODEL(1)%XTHT,2)) ::              & ! MODEL 1 VARIABLES
 ZHYDEXNTOP1    ! model top Exner functions                    at t or t-dt
!
REAL, DIMENSION(:,:,:), ALLOCATABLE :: & ! MODEL 2 VARIABLES
 ZGRIDA,      & ! mass point altitudes with purely interpoled orography
 ZGRIDB,      & ! mass point altitudes with new orography
 ZTHV2,       & ! virtual potential temperature at mass points at t or t-dt
 ZHYDEXN2,    & ! hydrostatic Exner functions at mass points   at t or t-dt
 ZEXNMHEXN2     ! Exner function minus hydrostatic Ex. f.      at t or t-dt
REAL, DIMENSION(SIZE(PTHVT,1),SIZE(PTHVT,2)) ::            & ! MODEL 2 VARIABLES
 ZHYDEXNTOP2    ! model top Exner functions                    at t or t-dt
!
REAL, DIMENSION(:,:,:), ALLOCATABLE :: ZWORK
INTEGER  :: IMI
!
!-------------------------------------------------------------------------------
!
!*       1.    PROLOGUE:
!              ---------
!
IMI = GET_CURRENT_MODEL_INDEX()
CALL GOTO_MODEL(2)
!
IIU = SIZE(PTHVM,1)
IJU = SIZE(PTHVM,2)
IIU1= SIZE(FIELD_MODEL(1)%XTHT,1)
IJU1= SIZE(FIELD_MODEL(1)%XTHT,2)
IKU=SIZE(PZZ,3)
IKE=IKU-JPVEXT
!
!-------------------------------------------------------------------------------
!
!*       2.    NO CHANGE OF RESOLUTION:
!              -----------------------
!
!
IF (KDXRATIO == 1 .AND. KDYRATIO == 1 ) THEN
!
  PPABSM  (:,:,:)   =  FIELD_MODEL(1)%XPABSM  (KXOR:KXEND,KYOR:KYEND,:)
  PPABST  (:,:,:)   =  FIELD_MODEL(1)%XPABST  (KXOR:KXEND,KYOR:KYEND,:)
!
  CALL GOTO_MODEL(IMI) 
  RETURN
!
END IF
!
!-------------------------------------------------------------------------------
!
!*       2.    GENERAL CASE: CHANGE OF RESOLUTION
!              ----------------------------------
!
DO JINSTANT=1,2
!
!*       2.1      Model 1 Pi and thetav
!                 ---------------------
!
  ALLOCATE(ZEXN1(IIU1,IJU1,IKU))
  ALLOCATE(ZTHV1(IIU1,IJU1,IKU))
  IF (JINSTANT==1) THEN
    ALLOCATE(ZSUMR(IIU1,IJU1,IKU))
    ZSUMR(:,:,:) = 0.
    DO JRR=1,CONF_MODEL(1)%NRR
      ZSUMR(:,:,:) = ZSUMR(:,:,:) + FIELD_MODEL(1)%XRM(:,:,:,JRR)
    END DO
    !
    ZEXN1(:,:,:)=(FIELD_MODEL(1)%XPABSM(:,:,:)/XP00)**(XRD/XCPD)
    IF (CONF_MODEL(1)%LUSERV) THEN
      ZTHV1(:,:,:)=FIELD_MODEL(1)%XTHM(:,:,:)*(1.+XRV/XRD*FIELD_MODEL(1)%XRM(:,:,:,1))/(1.+ZSUMR)
    ELSE
      ZTHV1(:,:,:)=FIELD_MODEL(1)%XTHM(:,:,:)
    END IF
    DEALLOCATE(ZSUMR)
  ELSE IF (JINSTANT==2) THEN
    ALLOCATE(ZSUMR(IIU1,IJU1,IKU))
    ZSUMR(:,:,:) = 0.
    DO JRR=1,CONF_MODEL(1)%NRR
      ZSUMR(:,:,:) = ZSUMR(:,:,:) + FIELD_MODEL(1)%XRT(:,:,:,JRR)
    END DO
    !
    ZEXN1(:,:,:)=(FIELD_MODEL(1)%XPABST(:,:,:)/XP00)**(XRD/XCPD)
    IF (CONF_MODEL(1)%LUSERV) THEN
      ZTHV1(:,:,:)=FIELD_MODEL(1)%XTHT(:,:,:)*(1.+XRV/XRD*FIELD_MODEL(1)%XRT(:,:,:,1))/(1.+ZSUMR)
    ELSE
      ZTHV1(:,:,:)=FIELD_MODEL(1)%XTHT(:,:,:)
    END IF
    DEALLOCATE(ZSUMR)
  END IF
!
!*       2.2      Model 1 top Exner function (guess)
!                 --------------------------
!
!* hydrostatism is supposed verified at the highest level.
!  since hydrostatic computation at mass points from flux points is non linear,
!  an iterative process is necessary to retrieve the correct hydrostatic Exner
!  pressure at model top.
!
!* guess
  ZHYDEXNTOP1(:,:)=0.5*(2.*ZEXN1(:,:,IKE)-REF_MODEL(1)%XEXNREF(:,:,IKE)+REF_MODEL(1)%XEXNREF(:,:,IKE+1))
!
  ALLOCATE(ZHYDEXN1(IIU1,IJU1,IKU))
  ALLOCATE(ZWORK   (IIU1,IJU1,IKU))
!
!* iterative loop
  DO JCOUNT=1,10
    CALL COMPUTE_EXNER_FROM_TOP(ZTHV1(:,:,IKE-1:IKE+1),GRID_MODEL(1)%XZZ(:,:,IKE-1:IKE+1),   &
                                ZHYDEXNTOP1(:,:),                              &
                                ZWORK(:,:,IKE-1:IKE+1),ZHYDEXN1(:,:,IKE-1:IKE+1))
    ZHYDEXNTOP1(:,:)=ZHYDEXNTOP1(:,:)+ZEXN1(:,:,IKE)-ZHYDEXN1(:,:,IKE)
    IF (ALL(ABS(ZEXN1(:,:,IKE)-ZHYDEXN1(:,:,IKE))<1.E-10)) EXIT
  END DO
!
!*       2.3      Model 1 hydrostatic pressure
!                 ----------------------------
!
  CALL COMPUTE_EXNER_FROM_TOP(ZTHV1,GRID_MODEL(1)%XZZ,ZHYDEXNTOP1,ZWORK,ZHYDEXN1)
!
  DEALLOCATE(ZTHV1)
  DEALLOCATE(ZWORK)
!
!*       2.4      model top Exner function interpolation
!                 --------------------------------------
!
    CALL BIKHARDT (XBMX1,XBMX2,XBMX3,XBMX4,XBMY1,XBMY2,XBMY3,XBMY4, &
                   XBFX1,XBFX2,XBFX3,XBFX4,XBFY1,XBFY2,XBFY3,XBFY4, &
                   KXOR,KYOR,KXEND,KYEND,KDXRATIO,KDYRATIO,4,       &
                   LBC_MODEL(1)%CLBCX,LBC_MODEL(1)%CLBCY,ZHYDEXNTOP1,ZHYDEXNTOP2)
!
!*       2.5      interpolation of pi-hyd pi
!                 --------------------------
!
  ALLOCATE(ZEXNMHEXN2(IIU,IJU,IKU))
!
    CALL BIKHARDT (XBMX1,XBMX2,XBMX3,XBMX4,XBMY1,XBMY2,XBMY3,XBMY4, &
                   XBFX1,XBFX2,XBFX3,XBFX4,XBFY1,XBFY2,XBFY3,XBFY4, &
                   KXOR,KYOR,KXEND,KYEND,KDXRATIO,KDYRATIO,1,       &
                   LBC_MODEL(1)%CLBCX,LBC_MODEL(1)%CLBCY,ZEXN1(:,:,:)-ZHYDEXN1(:,:,:),ZEXNMHEXN2)
!
  DEALLOCATE(ZEXN1)
  DEALLOCATE(ZHYDEXN1)
!
!* vertical interpolation
!
  ALLOCATE(ZGRIDA(IIU,IJU,IKU))
  ALLOCATE(ZGRIDB(IIU,IJU,IKU))
  ZGRIDA(:,:,:)=MZF(1,IKU,1,PZZ_LS(:,:,:))
  ZGRIDA(:,:,IKU)=2.*ZGRIDA(:,:,IKU-1)-ZGRIDA(:,:,IKU-2)
  ZGRIDB(:,:,:)=MZF(1,IKU,1,PZZ(:,:,:))
  ZGRIDB(:,:,IKU)=2.*ZGRIDB(:,:,IKU-1)-ZGRIDB(:,:,IKU-2)
  CALL COEF_VER_INTERP_LIN(ZGRIDA(:,:,:),ZGRIDB(:,:,:))
!
  ZEXNMHEXN2(:,:,:)=VER_INTERP_LIN(ZEXNMHEXN2(:,:,:),NKLIN(:,:,:),XCOEFLIN(:,:,:))
!
  DEALLOCATE(ZGRIDA)
  DEALLOCATE(ZGRIDB)
  DEALLOCATE(XCOEFLIN)
  DEALLOCATE(NKLIN)
!
!*       2.6      Model 2 thetav
!                 --------------
!
  ALLOCATE(ZTHV2(IIU,IJU,IKU))
  IF (JINSTANT==1) THEN
    ZTHV2(:,:,:)=PTHVM(:,:,:)
  ELSE IF (JINSTANT==2) THEN
    ZTHV2(:,:,:)=PTHVT(:,:,:)
  END IF
!
!*       2.7      Model 2 hydrostatic pressure
!                 ----------------------------
!
  ALLOCATE(ZHYDEXN2(IIU,IJU,IKU))
  ALLOCATE(ZWORK   (IIU,IJU,IKU))
!
  CALL COMPUTE_EXNER_FROM_TOP(ZTHV2,PZZ,ZHYDEXNTOP2,ZWORK,ZHYDEXN2)
!
  DEALLOCATE(ZTHV2)
  DEALLOCATE(ZWORK)
!
!*       2.8      Model 2 pressure
!                 ----------------
!
  IF (JINSTANT==1) THEN
    PPABSM(:,:,:)=XP00*(ZEXNMHEXN2(:,:,:)+ZHYDEXN2(:,:,:))**(XCPD/XRD)
  ELSE IF (JINSTANT==2) THEN
    PPABST(:,:,:)=XP00*(ZEXNMHEXN2(:,:,:)+ZHYDEXN2(:,:,:))**(XCPD/XRD)
  END IF
!
  DEALLOCATE(ZEXNMHEXN2)
  DEALLOCATE(ZHYDEXN2)
!
END DO
!-------------------------------------------------------------------------------
!
CALL GOTO_MODEL(IMI)
!
END SUBROUTINE SPAWN_PRESSURE2
!
