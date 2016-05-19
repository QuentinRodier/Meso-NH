!-----------------------------------------------------------------
!--------------- special set of characters for RCS information
!-----------------------------------------------------------------
! $Source$ $Revision$ $Date$
!-----------------------------------------------------------------
!     ###############################
      MODULE MODI_EDDY_FLUX_ONE_WAY_n
!     ###############################
!
INTERFACE
!
      SUBROUTINE EDDY_FLUX_ONE_WAY_n (KMI,KTCOUNT,KDXRATIO,KDYRATIO,HLBCX,HLBCY)
!
!
INTEGER, INTENT(IN) :: KMI     ! Model index
INTEGER, INTENT(IN) :: KTCOUNT ! iteration count
!
INTEGER, INTENT(IN) :: KDXRATIO   ! x and y-direction resolution RATIO
INTEGER, INTENT(IN) :: KDYRATIO   ! between inner model and outer model
CHARACTER (LEN=4), DIMENSION (2), INTENT(IN) :: HLBCX   ! type of lateral
CHARACTER (LEN=4), DIMENSION (2), INTENT(IN) :: HLBCY   ! boundary conditions
!
END SUBROUTINE EDDY_FLUX_ONE_WAY_n
!
END INTERFACE
!
END MODULE MODI_EDDY_FLUX_ONE_WAY_n
!
!     ##################################################################################
      SUBROUTINE EDDY_FLUX_ONE_WAY_n (KMI,KTCOUNT,KDXRATIO,KDYRATIO,HLBCX,HLBCY)
!     ##################################################################################
!
!!    PURPOSE
!!    -------
!!      In case of 2D transect (latitude, altitude) grid-nesting models
!!      Baroclinic fluxes (v'T' and w'T') from the model 1 interpolated for the son models
!!
!!**  METHOD
!!    ------
!!
!!    IMPLICIT ARGUMENT
!!    -----------------
!!
!!    REFERENCE
!!    ---------
!!
!!    AUTHOR
!!    ------
!!	  M.Tomasini          * Meteo-France *
!!
!!    MODIFICATIONS
!!    -------------
!!      Original  25/06/11
!!
!     ##################################################################################
!
USE MODD_DEF_EDDY_FLUX_n
USE MODD_FIELD_n,               ONLY:XRTHS
USE MODD_REF_n,                 ONLY:XRHODJ
USE MODD_GRID_n

USE MODD_METRICS_n
USE MODI_GRADIENT_W
USE MODI_GRADIENT_U
!
! For the horizontal interpolation
USE MODI_BIKHARDT
USE MODD_BIKHARDT_n
USE MODD_NESTING

IMPLICIT NONE
!
INTEGER, INTENT(IN) :: KMI     ! Model index
INTEGER, INTENT(IN) :: KTCOUNT ! iteration count
!
INTEGER, INTENT(IN) :: KDXRATIO   ! x and y-direction resolution RATIO
INTEGER, INTENT(IN) :: KDYRATIO   ! between inner model and outer model
CHARACTER (LEN=4), DIMENSION (2), INTENT(IN) :: HLBCX   ! type of lateral
CHARACTER (LEN=4), DIMENSION (2), INTENT(IN) :: HLBCY   ! boundary conditions

!
!*       0.2   Declarations of local variables :
!
INTEGER:: ISYNCHRO       ! model synchronic index relative to the model 1
                         ! = 1 for the first time step in phase with the model 1
                         ! = 0 for the last  time step (out of phase)
INTEGER:: JMI            ! Models loop
INTEGER:: IDTRATIO_KMI_1 ! Ratio between the time step of the son and the model 1

REAL, DIMENSION(:,:,:), ALLOCATABLE :: ZFLUX2 ! Work array=Dad interpolated flux field
                                              ! on the son grid
INTEGER :: IKU                                              

!-------------------------------------------------------------------------------
!
!
! test of temporal synchronisation between the model 1 and the son KMI
!
IKU=SIZE(XZHAT)
IDTRATIO_KMI_1=1
DO JMI=2,KMI
   IDTRATIO_KMI_1=IDTRATIO_KMI_1*NDTRATIO(JMI)
END DO
ISYNCHRO = MODULO (KTCOUNT, IDTRATIO_KMI_1)
!
IF (ISYNCHRO==1 .OR. IDTRATIO_KMI_1 == 1) THEN

   ALLOCATE(ZFLUX2(SIZE(XRTHS,1),SIZE(XRTHS,2),SIZE(XRTHS,3)))

   ! v'T' (EDDY_FLUX_MODEL(1)%XVTH_FLUX_M) of model1 interpolation on the son grid put into ZFLUX2
   ZFLUX2 = 0.
   CALL BIKHARDT (XBMX1,XBMX2,XBMX3,XBMX4,XBMY1,XBMY2,XBMY3,XBMY4, &
                  XBFX1,XBFX2,XBFX3,XBFX4,XBFY1,XBFY2,XBFY3,XBFY4, &
                  NXOR_ALL(KMI),NYOR_ALL(KMI),NXEND_ALL(KMI),NYEND_ALL(KMI),KDXRATIO,KDYRATIO,1,&
                  HLBCX,HLBCY,EDDY_FLUX_MODEL(1)%XVTH_FLUX_M,ZFLUX2)

   ! operator GX_U_M used for gradient of v'T' (flux point) placed at a mass point
   XRTHS_EDDY_FLUX(:,:,:) = - XRHODJ(:,:,:)* GX_U_M(1,IKU,1,ZFLUX2,XDXX,XDZZ,XDZX)
        
   ! w'T' (EDDY_FLUX_MODEL(1)%XWTH_FLUX_M) of model1 interpolation on the son grid put into ZFLUX2
   ZFLUX2 = 0.
   CALL BIKHARDT (XBMX1,XBMX2,XBMX3,XBMX4,XBMY1,XBMY2,XBMY3,XBMY4, &
                  XBFX1,XBFX2,XBFX3,XBFX4,XBFY1,XBFY2,XBFY3,XBFY4, &
                  NXOR_ALL(KMI),NYOR_ALL(KMI),NXEND_ALL(KMI),NYEND_ALL(KMI),KDXRATIO,KDYRATIO,1,&
                  HLBCX,HLBCY,EDDY_FLUX_MODEL(1)%XWTH_FLUX_M,ZFLUX2)

   ! DIV(W'T') put into the source term
   XRTHS_EDDY_FLUX(:,:,:) = XRTHS_EDDY_FLUX(:,:,:) &
                            - XRHODJ(:,:,:)* GZ_W_M(1,IKU,1,ZFLUX2,XDZZ)

   DEALLOCATE(ZFLUX2)

ENDIF

! COMPUTE NEW TEMPERATURE at each son time step
! ---------------------------------------------
XRTHS(:,:,:) = XRTHS(:,:,:) + XRTHS_EDDY_FLUX(:,:,:)

END SUBROUTINE EDDY_FLUX_ONE_WAY_n
