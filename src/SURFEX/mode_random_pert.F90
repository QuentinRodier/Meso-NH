!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!###################################################################
MODULE MODE_RANDOM_PERT
!###################################################################
!
USE YOMHOOK   ,ONLY : LHOOK, DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
CONTAINS

SUBROUTINE CREATE_RANDOM_MAP(UG,U,ZRANDOM_MAP)
!
!!****  CREATE_RANDOM_MAP*  
!!
!!    PURPOSE
!!    -------
!!     This routine aims at generating a map of random values to vary the initial sol moidture
!!    The needed seed is read in a ascii file.
!!
!! 
!!**  METHOD
!!    ------
!!    Random numbers order:
!!    ZRANDOM(1) affects WSAT
!!    ZRANDOM(2) affects WWILT
!!    ZRANDOM(3) affects WFC or W33
!!    ZRANDOM(4) affects BCOEF
!!    ZRANDOM(5) affects MATPOTSAT
!!    ZRANDOM(6) affects HYDCONDSAT
!!    ZRANDOM(7) affects M of TOPODYN approach (not is that routine)
!
!!    EXTERNAL
!!    --------
!!
!!    none
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------ 
!!
!!      
!!    REFERENCE
!!    ---------
!!
!!    
!!      
!!    AUTHOR
!!    ------
!!
!!      S. Edouard	* CNRM *
!!
!!    MODIFICATIONS
!!    -------------
!!
!!      Original   16/12/2015
!-------------------------------------------------------------------------------
!
!*       0.     DECLARATIONS
!               ------------
!
! Modules
USE MODI_OPEN_FILE
USE MODI_CLOSE_FILE
USE MODI_GET_GRID_DIM
!
USE MODD_SURF_ATM_GRID_n, ONLY : SURF_ATM_GRID_t
USE MODD_SURF_ATM_n, ONLY : SURF_ATM_t
!
USE MODI_GET_GRID_DIM
!
!*      0.1    declarations of arguments
!
TYPE(SURF_ATM_GRID_t), INTENT(INOUT) :: UG
TYPE(SURF_ATM_t), INTENT(INOUT) :: U
REAL, DIMENSION(:),  INTENT(INOUT)  :: ZRANDOM_MAP
!
!*      0.2    declarations of local variables
!
INTEGER               :: IDIMX, IDIMY, IUNIT
INTEGER               :: JI, JJ, JK ,JWRK
INTEGER               :: IEXT, ZDIST_FILT, ISEED_SIZE
INTEGER               :: IMEMBRE
REAL                  :: ZR, ZMOY, ZVAR, ZVAR2, ZMIN, ZMAX
REAL, DIMENSION(:,:),  ALLOCATABLE :: ZRAND_2D, ZTMP, ZRAND
INTEGER, DIMENSION(:), ALLOCATABLE :: ISEED
REAL                  :: ZMEMBRE2
LOGICAL               :: GRECT     ! T if rectangular grid
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
IF (LHOOK) CALL DR_HOOK('MODE_RANDOM_PERT:CREATE_RANDOM_MAP',0,ZHOOK_HANDLE)
!
!---------------------------------------------------------------
!
CALL OPEN_FILE('ASCII ',IUNIT,HFILE='randomMapGeneration.txt',HFORM='FORMATTED',HACTION='READ')
READ(IUNIT,*) IMEMBRE
READ(IUNIT,*) ZMEMBRE2
CALL CLOSE_FILE('ASCII ',IUNIT)
CALL RANDOM_SEED(ISEED_SIZE)
ALLOCATE(ISEED(ISEED_SIZE))
ISEED=IMEMBRE + 37 * (/ (JI - 1, JI = 1, ISEED_SIZE) /)
CALL RANDOM_SEED(PUT = ISEED)
!
! Préparation des nombres aléatoires
!
IEXT=20 
ZDIST_FILT=5
!
! Getting the grid dimensions
!
IF ( UG%G%CGRID.EQ.'CONF PROJ ' .OR. UG%G%CGRID.EQ.'CARTESIAN '&
  .OR. UG%G%CGRID.EQ.'LONLAT REG' .OR. UG%G%CGRID.EQ.'IGN' ) THEN
  !
  IF (ASSOCIATED(UG%XGRID_FULL_PAR)) THEN
    CALL GET_GRID_DIM(UG%G%CGRID,UG%G%NGRID_PAR,UG%XGRID_FULL_PAR,GRECT,IDIMX,IDIMY)
  ELSEIF (ASSOCIATED(UG%G%XGRID_PAR)) THEN
    CALL GET_GRID_DIM(UG%G%CGRID,UG%G%NGRID_PAR,UG%G%XGRID_PAR,GRECT,IDIMX,IDIMY)
  ENDIF
  !
ENDIF
!
ALLOCATE(ZRAND_2D(IDIMX+2*IEXT,IDIMY+2*IEXT))
ALLOCATE(ZTMP(IDIMX+2*IEXT,IDIMY+2*IEXT))
ALLOCATE(ZRAND(IDIMX,IDIMY))
CALL RANDOM_NUMBER(ZRAND_2D)
!
ZRAND_2D(:,:)=ZRAND_2D(:,:)-0.5
ZTMP(:,:)=ZRAND_2D(:,:)
!
ZMOY=0; ZVAR=0;
DO JJ=1+IEXT,IDIMX+IEXT
  DO JK=1+IEXT,IDIMY+IEXT
    ZMOY=ZMOY+ZRAND_2D(JJ,JK)
  END DO
END DO
!
ZMOY=ZMOY/((IDIMX)*(IDIMY))   
ZRAND_2D(:,:)=ZRAND_2D(:,:)-ZMOY
!
DO JJ=1+IEXT,IDIMX+IEXT
  DO JK=1+IEXT,IDIMY+IEXT
    ZVAR=ZVAR+(ZRAND_2D(JJ,JK))**2
  END DO
END DO
!
ZVAR=sqrt(ZVAR/((IDIMX)*(IDIMY)))
!
! Filtrage basique
!
DO JWRK=1,2
  !
  DO JJ=1+ZDIST_FILT,IDIMX+2*IEXT-ZDIST_FILT
    !
    DO JK=1+ZDIST_FILT,IDIMY+2*IEXT-ZDIST_FILT
      !
      ZR=0
      DO JI=-ZDIST_FILT,ZDIST_FILT
        ZR=ZR+ZRAND_2D(JJ+JI,JK)
      END DO
      ZR=ZR/(2*ZDIST_FILT+1)
      ZTMP(JJ,JK)=ZR
      !
    END DO
    !
  END DO
  !
  ZRAND_2D(:,:)=ZTMP(:,:)
  !
  DO JJ=1+ZDIST_FILT,IDIMX+2*IEXT-ZDIST_FILT
    !
    DO JK=1+ZDIST_FILT,IDIMY+2*IEXT-ZDIST_FILT
      !
      ZR=0
      DO JI=-ZDIST_FILT,ZDIST_FILT
        ZR=ZR+ZRAND_2D(JJ,JK+JI)
      END DO
      ZR=ZR/(2*ZDIST_FILT+1)
      ZTMP(JJ,JK)=ZR
      !
    END DO
    !
  END DO
  ZRAND_2D(:,:)=ZTMP(:,:)
  !
END DO
!
!Calcul de la variance après filtrage sur région et valeur utile
!
ZMOY=0; ZVAR2=0
!
DO JJ=1+IEXT,IDIMX+IEXT
  DO JK=1+IEXT,IDIMY+IEXT
    ZMOY=ZMOY+ZRAND_2D(JJ,JK)
  END DO
END DO
!
ZMOY=ZMOY/((IDIMX)*(IDIMY))
ZRAND_2D(:,:)=ZRAND_2D(:,:)-ZMOY
!
DO JJ=1+IEXT,IDIMX+IEXT
  DO JK=1+IEXT,IDIMY+IEXT
    ZVAR2=ZVAR2+(ZRAND_2D(JJ,JK))**2
  END DO
END DO
!
ZVAR2=sqrt(ZVAR2/((IDIMX)*(IDIMY)))    
!
! Amplification correctrice
!
ZRAND_2D(:,:)=ZRAND_2D(:,:)*ZVAR/ZVAR2
!
DO JJ=1+IEXT,IDIMX+IEXT
  DO JK=1+IEXT,IDIMY+IEXT
    ZRAND(JJ-IEXT,JK-IEXT)=ZRAND_2D(JJ,JK)
  END DO
END DO
!
ZMAX=maxval(ZRAND(:,:))
ZMIN=minval(ZRAND(:,:))
ZRAND(:,:)=ZMEMBRE2+0.2*ZRAND(:,:)
!
DO JI=1,IDIMX
  DO JJ=1,IDIMY
    ZRANDOM_MAP( (JJ-1) * IDIMX + JI) = ZRAND(JI,JJ)
  END DO
END DO
!
DEALLOCATE(ZRAND_2D)
DEALLOCATE(ZTMP)
!
IF (LHOOK) CALL DR_HOOK('MODE_RANDOM_PERT:CREATE_RANDOM_MAP',1,ZHOOK_HANDLE)
!
END SUBROUTINE CREATE_RANDOM_MAP
!
SUBROUTINE READ_RANDOM_NUMBER(ZRANDOM)
!
!!****  READ_RANDOM_NUMBER*  
!!
!!    PURPOSE
!!    -------
!!     This routine aims at reading some random values in a external file. 
!!
!! 
!!**  METHOD
!!    ------
!
!!    EXTERNAL
!!    --------
!!
!!    none
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------ 
!!
!!      
!!    REFERENCE
!!    ---------
!!
!!    
!!      
!!    AUTHOR
!!    ------
!!
!!      S. Edouard	* CNRM *
!!
!!    MODIFICATIONS
!!    -------------
!!
!!      Original   16/12/2015
!-------------------------------------------------------------------------------
!
!*       0.     DECLARATIONS
!               ------------
!
! Modules
!
USE MODI_OPEN_FILE
USE MODI_CLOSE_FILE
!
!*      0.1    declarations of arguments
!
REAL, DIMENSION(:),  INTENT(INOUT)  :: ZRANDOM
!
!*      0.2    declarations of local variables
!
INTEGER             :: IUNIT
INTEGER             :: INB_RAND
INTEGER             :: JWRK
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
IF (LHOOK) CALL DR_HOOK('MODE_RANDOM_PERT:READ_RANDOM_NUMBER',0,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
CALL OPEN_FILE('ASCII ',IUNIT,HFILE='randomNumbers.txt',HFORM='FORMATTED',HACTION='READ')
!
INB_RAND=7
DO JWRK=1,INB_RAND
  READ(IUNIT,*) ZRANDOM(JWRK)
ENDDO
CALL CLOSE_FILE('ASCII ',IUNIT)
!
IF (LHOOK) CALL DR_HOOK('MODE_RANDOM_PERT:READ_RANDOM_NUMBER',1,ZHOOK_HANDLE)
!
END SUBROUTINE READ_RANDOM_NUMBER
!
!-------------------------------------------------------------------------------
!
END MODULE MODE_RANDOM_PERT
