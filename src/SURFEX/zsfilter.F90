!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE ZSFILTER(HPROGRAM,PZS,PMASK,KOPTFILTER,KZSFILTER,PCOFILTER,PTHFILTER)
!     #############################################
!
!!**** *ZSFILTER* add a Laplacian to filter orographic signal
!!
!!    PURPOSE
!!    -------
!!
!!    METHOD
!!    ------
!!   
!!    An iterative method is used, adding each time the discretized Laplacian
!!    to the point value.
!!    Note that only points where land is present are modified, taking into
!!    account only such points in the filtering.
!!
!!    EXTERNAL
!!    --------
!!                           
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!
!!    REFERENCE
!!    ---------
!!
!!    AUTHOR
!!    ------
!!
!!    V. Masson          Meteo-France
!!
!!    MODIFICATION
!!    ------------
!!
!!    Original    15/03/96
!!    K. Yessad   2017      : new orography filtering 
!----------------------------------------------------------------------------
!
!*    0.     DECLARATION
!            -----------
!
!
USE MODI_GET_LUOUT
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*    0.1    Declaration of arguments
!            ------------------------
!
CHARACTER(LEN=6),     INTENT(IN)                :: HPROGRAM   ! program calling
REAL,             DIMENSION(:,:), INTENT(INOUT) :: PZS        ! orography
REAL,             DIMENSION(:,:), INTENT(IN)    :: PMASK      ! where filter is applied
INTEGER,                          INTENT(IN)    :: KOPTFILTER ! filtering option
INTEGER,                          INTENT(IN)    :: KZSFILTER  ! iteration number
REAL,                             INTENT(IN)    :: PCOFILTER  ! filtering coefficient
REAL,                             INTENT(IN)    :: PTHFILTER  ! filtering threshold
!
!*    0.2    Declaration of local variables
!            ------------------------------
!
REAL,DIMENSION(0:SIZE(PZS,1)+1,0:SIZE(PZS,2)+1) :: ZZS   ! modified 
                                                         ! orography
REAL,DIMENSION(0:SIZE(PZS,1)+1,0:SIZE(PZS,2)+1) :: ZMASK ! modified 
                                                         ! orography
REAL,DIMENSION(0:SIZE(PZS,1)+1,0:SIZE(PZS,2)+1) :: ZZSI
INTEGER :: JI,JJ,JITER,IIU,IJU
REAL    :: ZK                 ! filter efficiency coefficient (0.=< ZK =<1.)
REAL    :: ZINCR,ZZS0
REAL    :: ZSLOPXI, ZSLOPXF, ZINCRXI, ZINCRXF, ZOROGXI, ZOROGXF
REAL    :: ZDIFOROGN, ZDIFOROGX
INTEGER :: IMODIFIED,ILUOUT
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!----------------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('ZSFILTER',0,ZHOOK_HANDLE)
!----------------------------------------------------------------------------
!
!*       1.     Initialisations
!               ---------------
!
CALL GET_LUOUT(HPROGRAM,ILUOUT)
!
IIU=SIZE(PZS,1)
IJU=SIZE(PZS,2)
!
ZK=PCOFILTER
ZZS0=MAX(0.000001,PTHFILTER)

ZZS(:,:)=0.
ZZS(1:IIU,1:IJU)=PZS(:,:)
ZZS(0,:)    =2.*ZZS(1,:)  -ZZS(2,:)
ZZS(IIU+1,:)=2.*ZZS(IIU,:)-ZZS(IIU-1,:)
ZZS(:,0)    =2.*ZZS(:,1)  -ZZS(:,2)
ZZS(:,IJU+1)=2.*ZZS(:,IJU)-ZZS(:,IJU-1)
ZMASK(:,:)=0.
ZMASK(1:IIU,1:IJU)=PMASK(:,:)
!
! =====================================================================
! Printings before filtering
! =====================================================================

ZZSI(:,:)=ZZS(:,:)

ZSLOPXI=0.
DO JI=1,IIU
  DO JJ=1,IJU
    ZSLOPXI=MAX(ZSLOPXI,ABS(ZZS(JI,JJ)-ZZS(JI-1,JJ)))
    ZSLOPXI=MAX(ZSLOPXI,ABS(ZZS(JI,JJ)-ZZS(JI+1,JJ)))
    ZSLOPXI=MAX(ZSLOPXI,ABS(ZZS(JI,JJ)-ZZS(JI,JJ-1)))
    ZSLOPXI=MAX(ZSLOPXI,ABS(ZZS(JI,JJ)-ZZS(JI,JJ+1)))
  ENDDO
ENDDO

ZINCRXI=0.
DO JI=1,IIU
  DO JJ=1,IJU
    ZINCR=0.125* ZMASK(JI,JJ)                     &
     &  * (     ZMASK(JI-1,JJ)   * ZZS(JI-1,JJ)     &
     &      +   ZMASK(JI+1,JJ)   * ZZS(JI+1,JJ)     &
     &      +   ZMASK(JI,JJ-1)   * ZZS(JI,JJ-1)     &
     &      +   ZMASK(JI,JJ+1)   * ZZS(JI,JJ+1)     &
     &      - ( ZMASK(JI-1,JJ)                      &
     &         +ZMASK(JI+1,JJ)                      &
     &         +ZMASK(JI,JJ-1)                      &
     &         +ZMASK(JI,JJ+1) ) * ZZS(JI,JJ)       )
    ZINCRXI=MAX(ZINCRXI,ABS(ZINCR))
  ENDDO
ENDDO

ZOROGXI=MAXVAL(ZZS(1:IIU,1:IJU))

! =====================================================================
! =====================================================================
!
!*       2.     Iterative loop
!               --------------
!
IF (KOPTFILTER == 1) THEN
  DO JITER=1,KZSFILTER
    DO JI=1,IIU
      DO JJ=1,IJU
        ZINCR=0.125* ZMASK(JI,JJ)                     &
         &  * (     ZMASK(JI-1,JJ)   * ZZS(JI-1,JJ)     &
         &      +   ZMASK(JI+1,JJ)   * ZZS(JI+1,JJ)     &
         &      +   ZMASK(JI,JJ-1)   * ZZS(JI,JJ-1)     &
         &      +   ZMASK(JI,JJ+1)   * ZZS(JI,JJ+1)     &
         &      - ( ZMASK(JI-1,JJ)                      &
         &         +ZMASK(JI+1,JJ)                      &
         &         +ZMASK(JI,JJ-1)                      &
         &         +ZMASK(JI,JJ+1) ) * ZZS(JI,JJ)       )  
        ZINCR=ZINCR*MIN(1.,MAX(0.,(ZZS(JI,JJ)-ZZS0)/ZZS0))
        PZS(JI,JJ)= ZZS(JI,JJ)+ZK*ZINCR
      ENDDO
    ENDDO
    ZZS(1:IIU,1:IJU)=PZS(:,:)
  ENDDO
ELSE
  DO JITER=1,KZSFILTER
    DO JI=1,IIU
      DO JJ=1,IJU
        PZS(JI,JJ)= ZZS(JI,JJ)                          &
         &   + ZK*0.125* ZMASK(JI,JJ)                     &
         &    * (     ZMASK(JI-1,JJ)   * ZZS(JI-1,JJ)     &
         &        +   ZMASK(JI+1,JJ)   * ZZS(JI+1,JJ)     &
         &        +   ZMASK(JI,JJ-1)   * ZZS(JI,JJ-1)     &
         &        +   ZMASK(JI,JJ+1)   * ZZS(JI,JJ+1)     &
         &        - ( ZMASK(JI-1,JJ)                      &
         &           +ZMASK(JI+1,JJ)                      &
         &           +ZMASK(JI,JJ-1)                      &
         &           +ZMASK(JI,JJ+1) ) * ZZS(JI,JJ)       )
      ENDDO
    ENDDO
    ZZS(1:IIU,1:IJU)=PZS(:,:)
  ENDDO
ENDIF

! =====================================================================
! Printings after filtering
! =====================================================================

ZSLOPXF=0.
DO JI=1,IIU
  DO JJ=1,IJU
    ZSLOPXF=MAX(ZSLOPXF,ABS(ZZS(JI,JJ)-ZZS(JI-1,JJ)))
    ZSLOPXF=MAX(ZSLOPXF,ABS(ZZS(JI,JJ)-ZZS(JI+1,JJ)))
    ZSLOPXF=MAX(ZSLOPXF,ABS(ZZS(JI,JJ)-ZZS(JI,JJ-1)))
    ZSLOPXF=MAX(ZSLOPXF,ABS(ZZS(JI,JJ)-ZZS(JI,JJ+1)))
  ENDDO
ENDDO

IMODIFIED=0
DO JI=1,IIU
  DO JJ=1,IJU
    IF (ABS(ZZS(JI,JJ)-ZZSI(JI,JJ)) > 1.) IMODIFIED=IMODIFIED+1
  ENDDO
ENDDO

ZOROGXF=MAXVAL(ZZS(1:IIU,1:IJU))

ZINCRXF=0.
DO JI=1,IIU
  DO JJ=1,IJU
    ZINCR=0.125* ZMASK(JI,JJ)                     &
     &  * (     ZMASK(JI-1,JJ)   * ZZS(JI-1,JJ)     &
     &      +   ZMASK(JI+1,JJ)   * ZZS(JI+1,JJ)     &
     &      +   ZMASK(JI,JJ-1)   * ZZS(JI,JJ-1)     &
     &      +   ZMASK(JI,JJ+1)   * ZZS(JI,JJ+1)     &
     &      - ( ZMASK(JI-1,JJ)                      &
     &         +ZMASK(JI+1,JJ)                      &
     &         +ZMASK(JI,JJ-1)                      &
     &         +ZMASK(JI,JJ+1) ) * ZZS(JI,JJ)       )
    ZINCRXF=MAX(ZINCRXF,ABS(ZINCR))
  ENDDO
ENDDO

ZDIFOROGN=MINVAL(ZZS(1:IIU,1:IJU)-ZZSI(1:IIU,1:IJU))
ZDIFOROGX=MAXVAL(ZZS(1:IIU,1:IJU)-ZZSI(1:IIU,1:IJU))

WRITE(ILUOUT,*) ''
WRITE(ILUOUT,*) ' printings in ZSFILTER:'
WRITE(ILUOUT,*) ' max orography before filtering:',ZOROGXI
WRITE(ILUOUT,*) ' max orography after filtering:',ZOROGXF
WRITE(ILUOUT,*) ' max slope before filtering:',ZSLOPXI
WRITE(ILUOUT,*) ' max slope after filtering:',ZSLOPXF
WRITE(ILUOUT,*) ' max value of zincr before filtering:',ZINCRXI
WRITE(ILUOUT,*) ' max value of zincr after filtering:',ZINCRXF
WRITE(ILUOUT,*) ' bounds of orogf-orogi:  min=',ZDIFOROGN,'  max=',ZDIFOROGX
WRITE(ILUOUT,*) ' number of points where filtering has modified orography by more than 1 metre:',IMODIFIED
WRITE(ILUOUT,*) ''

! =====================================================================
! =====================================================================

IF (LHOOK) CALL DR_HOOK('ZSFILTER',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE ZSFILTER
