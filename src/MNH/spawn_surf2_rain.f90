!-----------------------------------------------------------------
!--------------- special set of characters for RCS information
!-----------------------------------------------------------------
! $Source$ $Revision$
! MASDEV4_7 spawn 2007/03/22 18:43:45
!-----------------------------------------------------------------
!###########################
MODULE MODI_SPAWN_SURF2_RAIN
!###########################
!
INTERFACE
!
      SUBROUTINE SPAWN_SURF2_RAIN (KXOR,KYOR,KXEND,KYEND,KDXRATIO,KDYRATIO,    &
                              PINPRC,PACPRC,PINPRR,PINPRR3D,PEVAP3D,           &
                              PACPRR,PINPRS,PACPRS,                            &
                              PINPRG,PACPRG,PINPRH,PACPRH,                     &
                              HSONFILE,KIUSON,KJUSON,                          &
                              KIB2,KJB2,KIE2,KJE2,                             &
                              KIB1,KJB1,KIE1,KJE1                              )
!
!
IMPLICIT NONE
!
INTEGER,   INTENT(IN)  :: KXOR,KXEND !  horizontal position (i,j) of the ORigin and END  
INTEGER,   INTENT(IN)  :: KYOR,KYEND ! of the model 2 domain, relative to model 1
INTEGER,   INTENT(IN)  :: KDXRATIO   !  x and y-direction Resolution ratio
INTEGER,   INTENT(IN)  :: KDYRATIO   ! between model 2 and model 1
!
REAL, DIMENSION(:,:),   INTENT(OUT) :: PINPRC,PACPRC   ! Precipitations
REAL, DIMENSION(:,:),   INTENT(OUT) :: PINPRR,PACPRR   ! Precipitations
REAL, DIMENSION(:,:,:), INTENT(OUT) :: PINPRR3D,PEVAP3D  ! Rain precipitation
                                                       ! and evaporation fluxes
REAL, DIMENSION(:,:),   INTENT(OUT) :: PINPRS,PACPRS   ! Precipitations
REAL, DIMENSION(:,:),   INTENT(OUT) :: PINPRG,PACPRG   ! Precipitations
REAL, DIMENSION(:,:),   INTENT(OUT) :: PINPRH,PACPRH   ! Precipitations
!
           ! Arguments for spawning with 2 input files (father+son1)
CHARACTER (LEN=*), OPTIONAL, INTENT(IN) :: HSONFILE  ! name of the input FM-file SON
INTEGER,           OPTIONAL, INTENT(IN) :: KIUSON    ! upper dimensions of the
INTEGER,           OPTIONAL, INTENT(IN) :: KJUSON    !input FM-file SON
INTEGER,           OPTIONAL, INTENT(IN) :: KIB2,KJB2 ! indexes for common
INTEGER,           OPTIONAL, INTENT(IN) :: KIE2,KJE2 !domain in model2
INTEGER,           OPTIONAL, INTENT(IN) :: KIB1,KJB1 !and in
INTEGER,           OPTIONAL, INTENT(IN) :: KIE1,KJE1 !SON
END SUBROUTINE SPAWN_SURF2_RAIN
!
END INTERFACE
!
END MODULE MODI_SPAWN_SURF2_RAIN
!
!
!     #########################################################################
      SUBROUTINE SPAWN_SURF2_RAIN (KXOR,KYOR,KXEND,KYEND,KDXRATIO,KDYRATIO,    &
                              PINPRC,PACPRC,PINPRR,PINPRR3D,PEVAP3D,           &
                              PACPRR,PINPRS,PACPRS,                            &
                              PINPRG,PACPRG,PINPRH,PACPRH,                     &
                              HSONFILE,KIUSON,KJUSON,                          &
                              KIB2,KJB2,KIE2,KJE2,                             &
                              KIB1,KJB1,KIE1,KJE1                              )
!     #########################################################################
!
!!****  *SPAWN_SURF2_RAIN * - subroutine to interpolate surface precipitations
!
!!    PURPOSE
!!    -------
!!
!!      The surface precipitations are interpolated from the model 1, to 
!!    initialize the model 2.
!!
!!**  METHOD
!!    ------
!!
!!      The model 2 variables are transmitted by argument (P or K prefixes),
!!    while the ones of model 1 are declared through calls to MODD_... 
!!    (X or N prefixes)
!!
!!
!!    EXTERNAL
!!    --------
!!
!!      Routine BIKHARDT2     : to perform horizontal interpolations
!!
!! 
!!    IMPLICIT ARGUMENTS
!!    ------------------ 
!!
!!
!!    REFERENCE
!!    ---------
!!
!!       Book1 of the documentation
!!      
!!
!!    AUTHOR
!!    ------
!!
!!       P. Jabouille     * METEO-FRANCE *
!!
!!    MODIFICATIONS
!!    -------------
!!
!!      Original     19/07/04 after surface externalisation
!!      Modification 07/07/05 (D.Barbary) spawn with 2 input files (father+son1)
!!      Modification    20/05/06 Remove Clark and Farley interpolation
!!-------------------------------------------------------------------------------
!
!*       0.     DECLARATIONS
!               ------------
USE MODD_LBC_n,   ONLY : LBC_MODEL
USE MODD_PRECIP_n,ONLY : PRECIP_MODEL
USE MODD_BIKHARDT_n
USE MODD_LUNIT_n, ONLY : CLUOUT
USE MODD_FIELD_n, ONLY : XTHT
USE MODD_CONF,    ONLY : CCONF,CPROGRAM
!
USE MODI_BIKHARDT         ! Interface modules
!
USE MODE_MODELN_HANDLER
!
USE MODI_READ_PRECIP_FIELD
!
!
IMPLICIT NONE
!
!
!*       0.1   Declarations of dummy arguments :
!

INTEGER,   INTENT(IN)  :: KXOR,KXEND !  horizontal position (i,j) of the ORigin and END  
INTEGER,   INTENT(IN)  :: KYOR,KYEND ! of the model 2 domain, relative to model 1
INTEGER,   INTENT(IN)  :: KDXRATIO   !  x and y-direction Resolution ratio
INTEGER,   INTENT(IN)  :: KDYRATIO   ! between model 2 and model 1
!
REAL, DIMENSION(:,:),   INTENT(OUT) :: PINPRC,PACPRC   ! Precipitations
REAL, DIMENSION(:,:),   INTENT(OUT) :: PINPRR,PACPRR   ! Precipitations
REAL, DIMENSION(:,:,:), INTENT(OUT) :: PINPRR3D,PEVAP3D  ! Rain precipitation
                                                       ! and evaporation fluxes
REAL, DIMENSION(:,:),   INTENT(OUT) :: PINPRS,PACPRS   ! Precipitations
REAL, DIMENSION(:,:),   INTENT(OUT) :: PINPRG,PACPRG   ! Precipitations
REAL, DIMENSION(:,:),   INTENT(OUT) :: PINPRH,PACPRH   ! Precipitation!
! Arguments for spawning with 2 input files (father+son1)
CHARACTER (LEN=*), OPTIONAL, INTENT(IN) :: HSONFILE  ! name of the input FM-file SON
INTEGER,           OPTIONAL, INTENT(IN) :: KIUSON    ! upper dimensions of the
INTEGER,           OPTIONAL, INTENT(IN) :: KJUSON    !input FM-file SON
INTEGER,           OPTIONAL, INTENT(IN) :: KIB2,KJB2 ! indexes for common
INTEGER,           OPTIONAL, INTENT(IN) :: KIE2,KJE2 !domain in model2
INTEGER,           OPTIONAL, INTENT(IN) :: KIB1,KJB1 !and in
INTEGER,           OPTIONAL, INTENT(IN) :: KIE1,KJE1 !SON
!
!*       0.2    Declarations of local variables for print on FM file
!
CHARACTER (LEN=2)   :: YMETHOD   ! Interpolation method ('BI', 'CF')
INTEGER             :: IMI
! Variables for spawning with 2 input files (father+son1)
REAL, DIMENSION(:,:), ALLOCATABLE :: ZINPRC1, ZACPRC1,    &
                                     ZINPRR1, ZACPRR1,    &
                                     ZINPRS1, ZACPRS1,    &
                                     ZINPRG1, ZACPRG1,    &
                                     ZINPRH1, ZACPRH1
                                         ! Instant and cumul of ground
                                         ! precipitation fields of Rain,
                                         !    Snow, Graupel and Hail
                                         ! For SON1
REAL, DIMENSION(:,:,:), ALLOCATABLE :: ZINPRR3D1, ZEVAP3D1
CHARACTER (LEN=4):: YGETRCT,YGETRRT,YGETRST,YGETRGT,YGETRHT ! READ,INIT or SKIP variable
INTEGER             :: ILU                        ! vertical size of arrays
!-------------------------------------------------------------------------------
!
!*       1.    PROLOGUE:
!              ---------
! 
IMI = GET_CURRENT_MODEL_INDEX()
ILU=SIZE(XTHT,3)
IF (IMI /= 2) CALL GOTO_MODEL(2)
!*       1.1   computes dimensions of arrays and other indices
!
!
!*       1.2   Interpolation method
!
YMETHOD='BI'
!
!-------------------------------------------------------------------------------
!
!*       3.    INITIALIZATION OF THE SURFACE VARIABLES OF MODEL 2:
!              --------------------------------------------------
!
IF (KDXRATIO == 1 .AND. KDYRATIO == 1 ) THEN
!
!*       3.1   special case of spawning - no change of resolution :
!
  IF (SIZE(PRECIP_MODEL(1)%XINPRC) /= 0 ) THEN
    PINPRC(:,:) = PRECIP_MODEL(1)%XINPRC(KXOR:KXEND,KYOR:KYEND)
    PACPRC(:,:) = PRECIP_MODEL(1)%XACPRC(KXOR:KXEND,KYOR:KYEND)
  END IF
!
  IF (SIZE(PRECIP_MODEL(1)%XINPRR) /= 0 ) THEN
    PINPRR(:,:) = PRECIP_MODEL(1)%XINPRR(KXOR:KXEND,KYOR:KYEND)
    PINPRR3D(:,:,:) = PRECIP_MODEL(1)%XINPRR3D(KXOR:KXEND,KYOR:KYEND,:)
    PEVAP3D(:,:,:) = PRECIP_MODEL(1)%XEVAP3D(KXOR:KXEND,KYOR:KYEND,:)
    PACPRR(:,:) = PRECIP_MODEL(1)%XACPRR(KXOR:KXEND,KYOR:KYEND)
  END IF
!
  IF (SIZE(PRECIP_MODEL(1)%XINPRS) /= 0 ) THEN
    PINPRS(:,:) = PRECIP_MODEL(1)%XINPRS(KXOR:KXEND,KYOR:KYEND)
    PACPRS(:,:) = PRECIP_MODEL(1)%XACPRS(KXOR:KXEND,KYOR:KYEND)
  END IF
!
  IF (SIZE(PRECIP_MODEL(1)%XINPRG) /= 0 ) THEN
    PINPRG(:,:) = PRECIP_MODEL(1)%XINPRG(KXOR:KXEND,KYOR:KYEND)
    PACPRG(:,:) = PRECIP_MODEL(1)%XACPRG(KXOR:KXEND,KYOR:KYEND)
  END IF
!
  IF (SIZE(PRECIP_MODEL(1)%XINPRH) /= 0 ) THEN
    PINPRH(:,:) = PRECIP_MODEL(1)%XINPRH(KXOR:KXEND,KYOR:KYEND)
    PACPRH(:,:) = PRECIP_MODEL(1)%XACPRH(KXOR:KXEND,KYOR:KYEND)
  END IF
!
!
!-------------------------------------------------------------------------------
!
ELSE
!
!*       3.2  general case - change of resolution :
!             -----------------------------------
!
!*       3.2.1   Bikhardt interpolation
!
!
  IF ( YMETHOD == 'BI' ) THEN
!
    IF (SIZE(PRECIP_MODEL(1)%XINPRC) /= 0 ) THEN
      CALL BIKHARDT(XBMX1,XBMX2,XBMX3,XBMX4,XBMY1,XBMY2,XBMY3,XBMY4, &
                    XBFX1,XBFX2,XBFX3,XBFX4,XBFY1,XBFY2,XBFY3,XBFY4, &
                    KXOR,KYOR,KXEND,KYEND,KDXRATIO,KDYRATIO,1,       &
                    LBC_MODEL(1)%CLBCX,LBC_MODEL(1)%CLBCY,PRECIP_MODEL(1)%XINPRC,PINPRC)
      CALL BIKHARDT(XBMX1,XBMX2,XBMX3,XBMX4,XBMY1,XBMY2,XBMY3,XBMY4, &
                    XBFX1,XBFX2,XBFX3,XBFX4,XBFY1,XBFY2,XBFY3,XBFY4, &
                    KXOR,KYOR,KXEND,KYEND,KDXRATIO,KDYRATIO,1,       &
                    LBC_MODEL(1)%CLBCX,LBC_MODEL(1)%CLBCY,PRECIP_MODEL(1)%XACPRC,PACPRC)
     PINPRC(:,:)=MAX(0.,PINPRC(:,:))
     PACPRC(:,:)=MAX(0.,PACPRC(:,:))
    END IF
!
    IF (SIZE(PRECIP_MODEL(1)%XINPRR) /= 0 ) THEN
      CALL BIKHARDT(XBMX1,XBMX2,XBMX3,XBMX4,XBMY1,XBMY2,XBMY3,XBMY4, &
                    XBFX1,XBFX2,XBFX3,XBFX4,XBFY1,XBFY2,XBFY3,XBFY4, &
                    KXOR,KYOR,KXEND,KYEND,KDXRATIO,KDYRATIO,1,       &
                    LBC_MODEL(1)%CLBCX,LBC_MODEL(1)%CLBCY,PRECIP_MODEL(1)%XINPRR,PINPRR)
      CALL BIKHARDT(XBMX1,XBMX2,XBMX3,XBMX4,XBMY1,XBMY2,XBMY3,XBMY4, &
                    XBFX1,XBFX2,XBFX3,XBFX4,XBFY1,XBFY2,XBFY3,XBFY4, &
                    KXOR,KYOR,KXEND,KYEND,KDXRATIO,KDYRATIO,1,       &
                    LBC_MODEL(1)%CLBCX,LBC_MODEL(1)%CLBCY,PRECIP_MODEL(1)%XINPRR3D,PINPRR3D)
      CALL BIKHARDT(XBMX1,XBMX2,XBMX3,XBMX4,XBMY1,XBMY2,XBMY3,XBMY4, &
                    XBFX1,XBFX2,XBFX3,XBFX4,XBFY1,XBFY2,XBFY3,XBFY4, &
                    KXOR,KYOR,KXEND,KYEND,KDXRATIO,KDYRATIO,1,       &
                    LBC_MODEL(1)%CLBCX,LBC_MODEL(1)%CLBCY,PRECIP_MODEL(1)%XEVAP3D,PEVAP3D)
      CALL BIKHARDT(XBMX1,XBMX2,XBMX3,XBMX4,XBMY1,XBMY2,XBMY3,XBMY4, &
                    XBFX1,XBFX2,XBFX3,XBFX4,XBFY1,XBFY2,XBFY3,XBFY4, &
                    KXOR,KYOR,KXEND,KYEND,KDXRATIO,KDYRATIO,1,       &
                    LBC_MODEL(1)%CLBCX,LBC_MODEL(1)%CLBCY,PRECIP_MODEL(1)%XACPRR,PACPRR)
     PINPRR(:,:)=MAX(0.,PINPRR(:,:))
     PINPRR3D(:,:,:)=MAX(0.,PINPRR3D(:,:,:))
     PEVAP3D(:,:,:)=MAX(0.,PEVAP3D(:,:,:))
     PACPRR(:,:)=MAX(0.,PACPRR(:,:))
    END IF
!
    IF (SIZE(PRECIP_MODEL(1)%XINPRS) /= 0 ) THEN
      CALL BIKHARDT(XBMX1,XBMX2,XBMX3,XBMX4,XBMY1,XBMY2,XBMY3,XBMY4, &
                    XBFX1,XBFX2,XBFX3,XBFX4,XBFY1,XBFY2,XBFY3,XBFY4, &
                    KXOR,KYOR,KXEND,KYEND,KDXRATIO,KDYRATIO,1,       &
                    LBC_MODEL(1)%CLBCX,LBC_MODEL(1)%CLBCY,PRECIP_MODEL(1)%XINPRS,PINPRS)
      CALL BIKHARDT(XBMX1,XBMX2,XBMX3,XBMX4,XBMY1,XBMY2,XBMY3,XBMY4, &
                    XBFX1,XBFX2,XBFX3,XBFX4,XBFY1,XBFY2,XBFY3,XBFY4, &
                    KXOR,KYOR,KXEND,KYEND,KDXRATIO,KDYRATIO,1,       &
                    LBC_MODEL(1)%CLBCX,LBC_MODEL(1)%CLBCY,PRECIP_MODEL(1)%XACPRS,PACPRS)
     PINPRS(:,:)=MAX(0.,PINPRS(:,:))
     PACPRS(:,:)=MAX(0.,PACPRS(:,:))
    END IF
!
    IF (SIZE(PRECIP_MODEL(1)%XINPRG) /= 0 ) THEN
      CALL BIKHARDT(XBMX1,XBMX2,XBMX3,XBMX4,XBMY1,XBMY2,XBMY3,XBMY4, &
                    XBFX1,XBFX2,XBFX3,XBFX4,XBFY1,XBFY2,XBFY3,XBFY4, &
                    KXOR,KYOR,KXEND,KYEND,KDXRATIO,KDYRATIO,1,       &
                    LBC_MODEL(1)%CLBCX,LBC_MODEL(1)%CLBCY,PRECIP_MODEL(1)%XINPRG,PINPRG)
      CALL BIKHARDT(XBMX1,XBMX2,XBMX3,XBMX4,XBMY1,XBMY2,XBMY3,XBMY4, &
                    XBFX1,XBFX2,XBFX3,XBFX4,XBFY1,XBFY2,XBFY3,XBFY4, &
                    KXOR,KYOR,KXEND,KYEND,KDXRATIO,KDYRATIO,1,       &
                    LBC_MODEL(1)%CLBCX,LBC_MODEL(1)%CLBCY,PRECIP_MODEL(1)%XACPRG,PACPRG)
     PINPRG(:,:)=MAX(0.,PINPRG(:,:))
     PACPRG(:,:)=MAX(0.,PACPRG(:,:))
    END IF
!
    IF (SIZE(PRECIP_MODEL(1)%XINPRH) /= 0 ) THEN
      CALL BIKHARDT(XBMX1,XBMX2,XBMX3,XBMX4,XBMY1,XBMY2,XBMY3,XBMY4, &
                    XBFX1,XBFX2,XBFX3,XBFX4,XBFY1,XBFY2,XBFY3,XBFY4, &
                    KXOR,KYOR,KXEND,KYEND,KDXRATIO,KDYRATIO,1,       &
                    LBC_MODEL(1)%CLBCX,LBC_MODEL(1)%CLBCY,PRECIP_MODEL(1)%XINPRH,PINPRH)
      CALL BIKHARDT(XBMX1,XBMX2,XBMX3,XBMX4,XBMY1,XBMY2,XBMY3,XBMY4, &
                    XBFX1,XBFX2,XBFX3,XBFX4,XBFY1,XBFY2,XBFY3,XBFY4, &
                    KXOR,KYOR,KXEND,KYEND,KDXRATIO,KDYRATIO,1,       &
                    LBC_MODEL(1)%CLBCX,LBC_MODEL(1)%CLBCY,PRECIP_MODEL(1)%XACPRH,PACPRH)
     PINPRH(:,:)=MAX(0.,PINPRH(:,:))
     PACPRR(:,:)=MAX(0.,PACPRH(:,:))
    END IF
!
  END IF
!
!-------------------------------------------------------------------------------
!
END IF
!
!*       3.3  Informations from model SON1
!             ----------------------------
!
IF (PRESENT(HSONFILE)) THEN
  IF (SIZE(PRECIP_MODEL(1)%XINPRC) /= 0 ) THEN
    ALLOCATE(ZINPRC1(KIUSON,KJUSON))
    ALLOCATE(ZACPRC1(KIUSON,KJUSON))
    YGETRRT='READ'
  ELSE
    ALLOCATE(ZINPRC1(0,0))
    ALLOCATE(ZACPRC1(0,0))
    YGETRRT='SKIP'
  END IF
  IF (SIZE(PRECIP_MODEL(1)%XINPRR) /= 0 ) THEN
    ALLOCATE(ZINPRR1(KIUSON,KJUSON))
    ALLOCATE(ZINPRR3D1(KIUSON,KJUSON,ILU))
    ALLOCATE(ZEVAP3D1(KIUSON,KJUSON,ILU))
    ALLOCATE(ZACPRR1(KIUSON,KJUSON))
    YGETRRT='READ'
  ELSE
    ALLOCATE(ZINPRR1(0,0))
    ALLOCATE(ZINPRR3D1(0,0,0))
    ALLOCATE(ZEVAP3D1(0,0,0))
    ALLOCATE(ZACPRR1(0,0))
    YGETRRT='SKIP'
  END IF
  IF (SIZE(PRECIP_MODEL(1)%XINPRS) /= 0 ) THEN
    ALLOCATE(ZINPRS1(KIUSON,KJUSON))
    ALLOCATE(ZACPRS1(KIUSON,KJUSON))
    YGETRST='READ'
  ELSE
    ALLOCATE(ZINPRS1(0,0))
    ALLOCATE(ZACPRS1(0,0))
    YGETRST='SKIP'
  END IF
  IF (SIZE(PRECIP_MODEL(1)%XINPRG) /= 0 ) THEN
    ALLOCATE(ZINPRG1(KIUSON,KJUSON))
    ALLOCATE(ZACPRG1(KIUSON,KJUSON))
    YGETRGT='READ'
  ELSE
    ALLOCATE(ZINPRG1(0,0))
    ALLOCATE(ZACPRG1(0,0))
    YGETRGT='SKIP'
  END IF
  IF (SIZE(PRECIP_MODEL(1)%XINPRH) /= 0 ) THEN
    ALLOCATE(ZINPRH1(KIUSON,KJUSON))
    ALLOCATE(ZACPRH1(KIUSON,KJUSON))
    YGETRHT='READ'
  ELSE
    ALLOCATE(ZINPRH1(0,0))
    ALLOCATE(ZACPRH1(0,0))
    YGETRHT='SKIP'
  END IF
  CALL READ_PRECIP_FIELD(HSONFILE,CLUOUT,CPROGRAM,CCONF,                          &
                         YGETRCT,YGETRRT,YGETRST,YGETRGT,YGETRHT,                 &
                         ZINPRC1,ZACPRC1,ZINPRR1,ZINPRR3D1,ZEVAP3D1,              &
                         ZACPRR1,ZINPRS1,ZACPRS1,                                 &
                         ZINPRG1,ZACPRG1,ZINPRH1,ZACPRH1                          )
  IF (SIZE(PRECIP_MODEL(1)%XINPRC) /= 0 ) THEN
    PINPRC(KIB2:KIE2,KJB2:KJE2) = ZINPRC1(KIB1:KIE1,KJB1:KJE1)
    PACPRC(KIB2:KIE2,KJB2:KJE2) = ZACPRC1(KIB1:KIE1,KJB1:KJE1)
  END IF
  DEALLOCATE(ZINPRC1)
  DEALLOCATE(ZACPRC1)
  IF (SIZE(PRECIP_MODEL(1)%XINPRR) /= 0 ) THEN
    PINPRR(KIB2:KIE2,KJB2:KJE2) = ZINPRR1(KIB1:KIE1,KJB1:KJE1)
    PINPRR3D(KIB2:KIE2,KJB2:KJE2,:) = ZINPRR3D1(KIB1:KIE1,KJB1:KJE1,:)
    PEVAP3D(KIB2:KIE2,KJB2:KJE2,:) = ZEVAP3D1(KIB1:KIE1,KJB1:KJE1,:)
    PACPRR(KIB2:KIE2,KJB2:KJE2) = ZACPRR1(KIB1:KIE1,KJB1:KJE1)
  END IF
  DEALLOCATE(ZINPRR1)
  DEALLOCATE(ZINPRR3D1)
  DEALLOCATE(ZEVAP3D1)
  DEALLOCATE(ZACPRR1)
  IF (SIZE(PRECIP_MODEL(1)%XINPRS) /= 0 ) THEN
    PINPRS(KIB2:KIE2,KJB2:KJE2) = ZINPRS1(KIB1:KIE1,KJB1:KJE1)
    PACPRS(KIB2:KIE2,KJB2:KJE2) = ZACPRS1(KIB1:KIE1,KJB1:KJE1)
  END IF
  DEALLOCATE(ZINPRS1)
  DEALLOCATE(ZACPRS1)
  IF (SIZE(PRECIP_MODEL(1)%XINPRG) /= 0 ) THEN
    PINPRG(KIB2:KIE2,KJB2:KJE2) = ZINPRG1(KIB1:KIE1,KJB1:KJE1)
    PACPRG(KIB2:KIE2,KJB2:KJE2) = ZACPRG1(KIB1:KIE1,KJB1:KJE1)
  END IF
  DEALLOCATE(ZINPRG1)
  DEALLOCATE(ZACPRG1)
  IF (SIZE(PRECIP_MODEL(1)%XINPRH) /= 0 ) THEN
    PINPRH(KIB2:KIE2,KJB2:KJE2) = ZINPRH1(KIB1:KIE1,KJB1:KJE1)
    PACPRH(KIB2:KIE2,KJB2:KJE2) = ZACPRH1(KIB1:KIE1,KJB1:KJE1)
  END IF
  DEALLOCATE(ZINPRH1)
  DEALLOCATE(ZACPRH1)
END IF
!
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE SPAWN_SURF2_RAIN
