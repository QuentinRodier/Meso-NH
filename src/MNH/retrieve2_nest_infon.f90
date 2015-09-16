!MNH_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
!MNH_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!MNH_LIC for details. version 1.
!-----------------------------------------------------------------
!--------------- special set of characters for RCS information
!-----------------------------------------------------------------
! $Source$ $Revision$
!-----------------------------------------------------------------
!     ################################
      MODULE MODI_RETRIEVE2_NEST_INFO_n
!     ################################
!
INTERFACE 
!
      SUBROUTINE RETRIEVE2_NEST_INFO_n(KMI,KDAD,KXOR,KYOR,KXSIZE,KYSIZE,KDXRATIO,KDYRATIO)
!
INTEGER,INTENT(IN)  :: KMI      ! son model index
INTEGER,INTENT(IN)  :: KDAD     ! dad model index
INTEGER,INTENT(OUT) :: KXOR     ! position of pgd model origine points
INTEGER,INTENT(OUT) :: KYOR     ! according to father domain
INTEGER,INTENT(OUT) :: KXSIZE   ! number of grid meshes in father grid to be
INTEGER,INTENT(OUT) :: KYSIZE   ! covered by the pgd domain
INTEGER,INTENT(OUT) :: KDXRATIO ! resolution ratio between father grid
INTEGER,INTENT(OUT) :: KDYRATIO ! and son grid
!
END SUBROUTINE RETRIEVE2_NEST_INFO_n
!
END INTERFACE
!
END MODULE MODI_RETRIEVE2_NEST_INFO_n
!
!
!
!     ###############################################################
      SUBROUTINE RETRIEVE2_NEST_INFO_n(KMI,KDAD,KXOR,KYOR,KXSIZE,KYSIZE, &
                                           KDXRATIO,KDYRATIO)
!     ###############################################################
!
!!****  *RETRIEVE2_NEST_INFO_n* - routine to test coherence between grid
!!                                of model KMI and of current model IDAD.
!!                                
!!
!!    PURPOSE
!!    -------
!
!!**  METHOD
!!    ------
!!
!!    EXTERNAL
!!    --------
!!       
!!    IMPLICIT ARGUMENTS
!!    ------------------ 
!!      Module MODD_GRID : contains projection definition
!!        XLAT0
!!        XLON0
!!        XRPK
!!        XBETA
!!      Module MODD_PGDGRID : contains domain definition
!!        XPGDLATOR
!!        XPGDLONOR
!!        XPGDXHAT
!!        XPGDYHAT
!!      Module MODD_PGDDIM : contains domain size
!!        NPGDIMAX
!!        NPGDJMAX
!!        XLATORI
!!        XLONORI
!!      Module MODD_GRID_n :
!!        XXHAT
!!        XYHAT 
!!      Module MODD_DIM_n :
!!        NIMAX, NJMAX
!!      Module MODD_PARAMETERS :
!!        JPHEXT
!!      Module MODD_LUNIT :
!!        CLUOUT
!!
!!    REFERENCE
!!    ---------
!!      Book2 of the documentation
!!      
!!
!!    AUTHOR
!!    ------
!!	V. Masson       * Meteo France *
!!
!!    MODIFICATIONS
!!    -------------
!!      Original        25/09/96
!!                      22/09/99 PGD modules for dad, and _n module for son
!!      J Stein         04/07/01 add cartesian case
!!      J.Escobar : 15/09/2015 : WENO5 & JPHEXT <> 1 
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!
USE MODD_CONF
USE MODD_PARAMETERS
USE MODD_GRID
USE MODD_GRID_n
USE MODD_DIM_n
USE MODD_PGDGRID
USE MODD_PGDDIM
USE MODD_LUNIT
!
USE MODE_FM
USE MODE_GRIDPROJ
USE MODE_MODELN_HANDLER 
!
!
IMPLICIT NONE
!
!*       0.1   declarations of arguments
!
INTEGER,INTENT(IN)  :: KMI      ! son model index
INTEGER,INTENT(IN)  :: KDAD     ! dad model index
INTEGER,INTENT(OUT) :: KXOR     ! position of pgd model origine points
INTEGER,INTENT(OUT) :: KYOR     ! according to father (next refered as 1) domain
INTEGER,INTENT(OUT) :: KXSIZE   ! number of grid meshes in model 1 to be
INTEGER,INTENT(OUT) :: KYSIZE   ! covered by the pgd domain
INTEGER,INTENT(OUT) :: KDXRATIO ! resolution ratio between grid 1
INTEGER,INTENT(OUT) :: KDYRATIO ! and its son (next refered as 2) grid
!
!
!*       0.2   declarations of local variables
!
INTEGER              :: ILUOUT, IRESP
INTEGER              :: IIU           ! relatively to model 1
INTEGER              :: IJU           ! relatively to model 1
INTEGER              :: IPGDIU        ! relatively to model 2
INTEGER              :: IPGDJU        ! relatively to model 2
REAL                 :: ZLAT2         ! geographical coordinates of the first
REAL                 :: ZLON2         ! physical flux point of model 2
REAL, DIMENSION(:,:), ALLOCATABLE :: ZPGDLAT1 ! geographical coordinates of all
REAL, DIMENSION(:,:), ALLOCATABLE :: ZPGDLON1 ! the flux points of model 1
!
INTEGER,DIMENSION(2) :: IXY1          ! first point relatively to model 1
                                      ! corresponding to physical domain 2
INTEGER,DIMENSION(1) :: IX2,IY2       ! point relatively to model 2 corresponding
                                      ! to second physical point of model 1
INTEGER,DIMENSION(1) :: IXSUP1,IYSUP1 ! last point relatively to model 1
                                      ! corresponding to physical domain 2
!
REAL                 :: ZEPS = 1.E-6  ! a small number
!
INTEGER     :: JI,JJ         ! loop controls relatively to model 2
INTEGER     :: JIBOX,JJBOX   ! grid mesh relatively to model 1
REAL        :: ZCOEF         ! ponderation coefficient for linear interpolation
REAL, DIMENSION(:), ALLOCATABLE :: ZXHAT, ZYHAT ! coordinates of model 2
!                            ! recomputed from coordinates of model 1 and ratios
REAL, DIMENSION(:), ALLOCATABLE :: ZPGDXHAT, ZPGDYHAT ! as XPGDXHAT and XPGDYHAT
!                                                     ! with one more point
REAL :: ZERROR_X,ZERROR_Y
!
!-------------------------------------------------------------------------------
! Current model is DAD model
!
CALL GOTO_MODEL(KMI)
!
CALL FMLOOK_ll(CLUOUT0,CLUOUT0,ILUOUT,IRESP)
IIU=NPGDIMAX+2*JPHEXT
IJU=NPGDJMAX+2*JPHEXT
!
!*      1.    KXOR,KYOR
!             ---------
!
IF(.NOT.LCARTESIAN) THEN
!
!*      1.1   latitude and longitude of first flux point (model2)
!             ---------------------------------------------------
!
  CALL SM_LATLON(XLATORI,XLONORI,                 &
                 XXHAT(JPHEXT+1),XYHAT(JPHEXT+1), &
                 ZLAT2,ZLON2)
!
!*      1.2   latitude and longitude of all flux points (model1)
!             --------------------------------------------------
!
  ALLOCATE(ZPGDLAT1(IIU,IJU))
  ALLOCATE(ZPGDLON1(IIU,IJU))
  CALL SM_LATLON(XPGDLATOR,XPGDLONOR,                                 &
                 SPREAD(XPGDXHAT(:),2,IJU),SPREAD(XPGDYHAT(:),1,IIU), &
                 ZPGDLAT1(:,:),ZPGDLON1(:,:))
!
!*      1.3   KXOR, KYOR 
!
  IXY1(:)=MINLOC(ABS(ZPGDLAT1(:,:)-ZLAT2)+ABS(ZPGDLON1(:,:)-ZLON2))
!
ELSE
!
  IXY1(1:1)=MINLOC(ABS(XPGDXHAT(:)-XXHAT(JPHEXT+1)))
  IXY1(2:2)=MINLOC(ABS(XPGDYHAT(:)-XYHAT(JPHEXT+1)))
ENDIF  
!
KXOR=IXY1(1)-JPHEXT
KYOR=IXY1(2)-JPHEXT
!
IF (KXOR<1 .OR. KXOR>IIU .OR. KYOR<1 .OR. KYOR >IJU) THEN
  WRITE(ILUOUT,*) 'KXOR or KYOR outside of the domain'
  WRITE(ILUOUT,*) 'KXOR= ', KXOR, 'KYOR= ', KYOR
 !callabortstop
CALL ABORT
  STOP
END IF
IF (LCARTESIAN ) THEN
  ZERROR_X=MINVAL(ABS(XPGDXHAT(:)-XXHAT(JPHEXT+1)))
  ZERROR_Y=MINVAL(ABS(XPGDYHAT(:)-XYHAT(JPHEXT+1)))
  IF ( ZERROR_X+ZERROR_Y > ZEPS ) THEN
    WRITE(ILUOUT,*) 'the first physical flux point of model ',KDAD,' does not correspond'
    WRITE(ILUOUT,*) 'to any of its father.'
    WRITE(ILUOUT,*) 'error on x and y : ', ZERROR_X,ZERROR_Y
 !callabortstop
CALL ABORT
    STOP
  END IF
ELSE
  IF (MINVAL(ABS(ZPGDLAT1(:,:)-ZLAT2)+ABS(ZPGDLON1(:,:)-ZLON2))>ZEPS) THEN
    WRITE(ILUOUT,*) 'the first physical flux point of model ',KDAD,' does not correspond'
    WRITE(ILUOUT,*) 'to any of its father.'
    WRITE(ILUOUT,*) 'sum of error on latitude and longitude: ', &
                  MINVAL(ABS(ZPGDLAT1(:,:)-ZLAT2)+ABS(ZPGDLON1(:,:)-ZLON2))
 !callabortstop
CALL ABORT
    STOP
  END IF
END IF
!
!*      1.4   modify coordinates
!             ------------------
!
XXHAT(:) = XXHAT(:) + XPGDXHAT(IXY1(1))-XXHAT(JPHEXT+1)
XYHAT(:) = XYHAT(:) + XPGDYHAT(IXY1(2))-XYHAT(JPHEXT+1)
! 
!-------------------------------------------------------------------------------
!
!*      2.    KDXRATIO, KDYRATIO
!             ------------------
!
IX2(:)=MINLOC(ABS(XPGDXHAT(IXY1(1)+1)-XXHAT(:)))
IY2(:)=MINLOC(ABS(XPGDYHAT(IXY1(2)+1)-XYHAT(:)))
!
KDXRATIO=IX2(1)-JPHEXT-1
KDYRATIO=IY2(1)-JPHEXT-1
!
!-------------------------------------------------------------------------------
!
!*      3.    KXSIZE,KYSIZE
!             -------------
!
IXSUP1(:)=MINLOC(ABS(XPGDXHAT(:)-XXHAT(NIMAX+JPHEXT+1)))
IYSUP1(:)=MINLOC(ABS(XPGDYHAT(:)-XYHAT(NJMAX+JPHEXT+1)))
!
IXSUP1(:)= IXSUP1(:) -1
IYSUP1(:)= IYSUP1(:) -1
!
KXSIZE=IXSUP1(1)-IXY1(1)+1
KYSIZE=IYSUP1(1)-IXY1(2)+1
!
IF (     KXOR+KXSIZE+2*JPHEXT-1<1 .OR. KXOR+KXSIZE+2*JPHEXT-1>IIU      &
    .OR. KYOR+KYSIZE+2*JPHEXT-1<1 .OR. KYOR+KYSIZE+2*JPHEXT-1>IJU) THEN
  WRITE(ILUOUT,*) 'KXEND or KYEND (last point used in domain',KMI,') outside of the domain'
  WRITE(ILUOUT,*) 'KXEND= ', KXOR+KXSIZE+2*JPHEXT-1, 'KYEND= ', KYOR+KYSIZE+2*JPHEXT-1
 !callabortstop
CALL ABORT
  STOP
END IF
!-------------------------------------------------------------------------------
!
!*      4.    Tests on coordinate arrays
!             --------------------------
!
ALLOCATE(ZXHAT(NIMAX+2*JPHEXT))
ALLOCATE(ZYHAT(NJMAX+2*JPHEXT))
!
IPGDIU = NPGDIMAX+2*JPHEXT
IPGDJU = NPGDJMAX+2*JPHEXT
!
ALLOCATE(ZPGDXHAT(0:IPGDIU+1))
ALLOCATE(ZPGDYHAT(0:IPGDJU+1))
!
ZPGDXHAT(1:IPGDIU) = XPGDXHAT(:)
ZPGDYHAT(1:IPGDJU) = XPGDYHAT(:)
ZPGDXHAT(IPGDIU+1) = 2.* XPGDXHAT(IPGDIU) - XPGDXHAT(IPGDIU-1)
ZPGDYHAT(IPGDJU+1) = 2.* XPGDYHAT(IPGDJU) - XPGDYHAT(IPGDJU-1)
ZPGDXHAT(0)        = 2.* XPGDXHAT(1) - XPGDXHAT(2)
ZPGDYHAT(0)        = 2.* XPGDYHAT(1) - XPGDYHAT(2)
!
DO JI=1,NIMAX+2*JPHEXT
  JIBOX=(JI+KDXRATIO-1-JPHEXT)/KDXRATIO + KXOR
  ZCOEF= FLOAT(MOD(JI+KDXRATIO-1-JPHEXT,KDXRATIO))/FLOAT(KDXRATIO)
  ZXHAT(JI)=(1.-ZCOEF)*ZPGDXHAT(JIBOX+JPHEXT-1)+ZCOEF*ZPGDXHAT(JIBOX+JPHEXT) ! +1
END DO
!
DO JJ=1,NJMAX+2*JPHEXT
  JJBOX=(JJ+KDYRATIO-1-JPHEXT)/KDYRATIO + KYOR
  ZCOEF= FLOAT(MOD(JJ+KDYRATIO-1-JPHEXT,KDYRATIO))/FLOAT(KDYRATIO)
  ZYHAT(JJ)=(1.-ZCOEF)*ZPGDYHAT(JJBOX+JPHEXT-1)+ZCOEF*ZPGDYHAT(JJBOX+JPHEXT) ! +1
END DO
!
IF (     ANY(ABS(XXHAT(:)-ZXHAT(:))>ZEPS)            &
    .OR. ANY(ABS(XYHAT(:)-ZYHAT(:))>ZEPS) ) THEN
  WRITE(ILUOUT,*) 'XHAT or YHAT functions are incoherent'
  WRITE(ILUOUT,*) ' '
  DO JI=1,NIMAX+2*JPHEXT
    WRITE(ILUOUT,*) '  XXHAT(',JI,')  = ', XXHAT(JI) , &
                    '  ZXHAT(',JI,')  = ', ZXHAT(JI)
  END DO
  WRITE(ILUOUT,*) ' '
  DO JJ=1,NJMAX+2*JPHEXT
    WRITE(ILUOUT,*) '  XYHAT(',JJ,')  = ', XYHAT(JJ) , &
                    '  ZYHAT(',JJ,')  = ', ZYHAT(JJ)
  END DO
 !callabortstop
CALL ABORT
  STOP
END IF
!
DEALLOCATE(ZXHAT)
DEALLOCATE(ZYHAT)
!
DEALLOCATE(ZPGDXHAT)
DEALLOCATE(ZPGDYHAT)
!
IF (.NOT. LCARTESIAN) THEN
  DEALLOCATE(ZPGDLAT1)
  DEALLOCATE(ZPGDLON1)
ENDIF
  
!-------------------------------------------------------------------------------
!
IF (KDAD > 0) CALL GOTO_MODEL(KDAD)
!
END SUBROUTINE RETRIEVE2_NEST_INFO_n
