!MNH_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
!MNH_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!MNH_LIC for details. version 1.
!-----------------------------------------------------------------
!--------------- special set of characters for RCS information
!-----------------------------------------------------------------
! $Source$ $Revision$
!-----------------------------------------------------------------
!########################
 MODULE MODI_WRITE_SERIES_n
!########################
!
INTERFACE
!
      SUBROUTINE WRITE_SERIES_n(HFILEDIA,HLUOUT )
!
CHARACTER (LEN=*),  INTENT(IN) :: HFILEDIA     ! name of FM-file to write
CHARACTER (LEN=*),  INTENT(IN) :: HLUOUT       ! name of output listing
!
END SUBROUTINE WRITE_SERIES_n
!
END INTERFACE
!
END MODULE MODI_WRITE_SERIES_n
!     ###################
      SUBROUTINE WRITE_SERIES_n (HFILEDIA,HLUOUT)

!     #######################################################################################       
!
!!****  *WRITE_SERIES* - routine to  Write diagnostics for diachro files  (temporal series)
!!                           
!!
!!    PURPOSE
!!    -------
!
!!**  METHOD
!!    ------
!!      
!!
!!    EXTERNAL
!!    --------
!!       NONE
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!
!!    REFERENCE
!!    ---------
!!      
!!
!!
!!    AUTHOR
!!    ------
!!      V. Ducrocq from contributions of M. Tomasini, S. Donier,... * Meteo France *
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    4/03/2002
!!      Modification 7/01/2013 Add key for netcdf writing
!!      J.Escobar : 15/09/2015 : WENO5 & JPHEXT <> 1 
!!      P.Wautelet: 11/07/2016 : removed MNH_NCWRIT define
!!
!-------------------------------------------------------------------------------
!
!
!*    0. Declaration
!     --------------
! 
USE MODD_SERIES
USE MODD_SERIES_n
USE MODD_CONF, ONLY: NVERB
USE MODD_PARAMETERS
USE MODI_WRITE_DIACHRO
USE MODI_GATHER_ll
USE MODE_ll
USE MODE_IO_ll
!
IMPLICIT NONE
!
!*       0.1   declarations of arguments
!
CHARACTER (LEN=*),  INTENT(IN) :: HFILEDIA     ! name of FM-file to write
CHARACTER (LEN=*),  INTENT(IN) :: HLUOUT       ! name of output listing
!
!*       0.2     Local variables 
!
INTEGER                              :: IIB,IJB,IKB     ! Begin of physical dimensions
INTEGER                              :: IIE,IJE,IKE     ! End   of physical dimensions
INTEGER                              :: JS,JT,JJ,JI      ! Loop indices 
INTEGER                              :: ISB1,ISB2
CHARACTER (LEN=2)                    :: YS             ! String for y-slice
CHARACTER (LEN=3)                    :: YSL,YSH        ! Strings for y-slice
CHARACTER (LEN=10)                   :: YGROUP
INTEGER                              :: ISIZEHB         ! Size of the box or the slices
INTEGER                              :: IJU,IIU,IMI
INTEGER                              :: IIMAX_ll,IJMAX_ll
INTEGER                              :: IIU_ll,IJU_ll,IIB_ll,IIE_ll
INTEGER                              :: IINFO_ll
INTEGER                              :: IISL,IISH,IJL,IJH
CHARACTER(LEN=100), DIMENSION(:), ALLOCATABLE  :: YSTITLE3S
REAL, DIMENSION(:,:), ALLOCATABLE         :: ZVAR2D,ZWORK2D
REAL, DIMENSION(:,:,:), ALLOCATABLE       :: ZVAR3D,ZVAR3D_ll
REAL, DIMENSION(:,:,:,:,:,:), ALLOCATABLE :: ZSERIES3_ll
INTEGER :: IKMAX
INTEGER  :: ILUOUT ! Logical unit number for output-listing
INTEGER  :: IRESP   ! Return code of FM-routines
INTEGER  :: INFO_ll   ! Return code of FM-routines
INTEGER :: ISER,INAV
REAL :: ZSIZEHB
LOGICAL :: GICP,GJCP,GKCP ! compression flags along the 3 directions
!----------------------------------------------------------------------------
!
!*    1.     INITIALIZATION
!            --------------
!
CALL FMLOOK_ll(HLUOUT,HLUOUT,ILUOUT,IRESP)
IF (NVERB>=5) WRITE(ILUOUT,*) 'WRITE_SERIESn: beginning'
!
CALL GET_INDICE_ll (IIB,IJB,IIE,IJE) 
IKMAX=SIZE(XSSERIES2,3)
IKE = IKMAX + JPVEXT
IKB = 1     + JPVEXT
CALL GET_DIM_EXT_ll ('B', IIU,IJU)
CALL GET_GLOBALDIMS_ll (IIMAX_ll,IJMAX_ll)
IIU_ll = IIMAX_ll+2*JPHEXT
IJU_ll = IJMAX_ll+2*JPHEXT
IIB_ll = 1 + JPHEXT
IIE_ll = IIMAX_ll + JPHEXT
!
IF (LDOSERIES .AND. (NSNBSTEPT.NE. NSCOUNTD)) THEN
  WRITE(ILUOUT,*) 'WRITE_SERIESn: NSNBSTEPT .NE. NSCOUNTD ',NSNBSTEPT,NSCOUNTD
 !callabortstop
  CALL CLOSE_ll(HLUOUT,IOSTAT=IRESP)
  CALL ABORT
  STOP
END IF
!
ISER=1
IF (LMASKLANDSEA) ISER=3
!
!----------------------------------------------------------------------------
!
!*      2.   Temporal series (t)
!            -------------------
!
!*      2.1  Average processes of temporal series 
!
ALLOCATE(ZVAR2D(NSNBSTEPT,NAVER1))
IF (LSERIES1) THEN
  ZVAR2D(:,:)=XSSERIES1(1,1,1,1:NSNBSTEPT,1,1:NAVER1)
ELSE
  ZVAR2D(:,:)=0.
END IF
CALL REDUCESUM_ll(ZVAR2D,IINFO_ll)
IF (IINFO_ll /=0 ) THEN
  WRITE(ILUOUT,FMT='("WRITE_SERIES: PB in REDUCESUM FOR SERIES1")')
 !callabortstop
  CALL CLOSE_ll(HLUOUT,IOSTAT=IRESP)
  CALL ABORT
  STOP
END IF
!
ISIZEHB=(NIBOXH-NIBOXL+1)*(NJBOXH-NJBOXL+1)
INAV=NAVER1/ISER 
XSSERIES1(1,1,1,1:NSNBSTEPT,1,1:INAV)=ZVAR2D(:,1:INAV)/ISIZEHB
IF (LMASKLANDSEA) THEN
  ISIZEHB=COUNT(LINBOXL(:,:))
  ZSIZEHB=ISIZEHB
  CALL REDUCESUM_ll(ZSIZEHB,IINFO_ll)
  IF (IINFO_ll /=0 ) THEN
    WRITE(ILUOUT,*) 'WRITE_SERIES: PB in REDUCESUM for SERIES1 SIZEHB-LAND',ISIZEHB
 !callabortstop
  CALL CLOSE_ll(HLUOUT,IOSTAT=IRESP)
  CALL ABORT
    STOP
  END IF
  XSSERIES1(1,1,1,1:NSNBSTEPT,1,INAV+1:2*INAV)=ZVAR2D(:,INAV+1:2*INAV)/MAX(ZSIZEHB,1.)
  !
  ISIZEHB=COUNT(LINBOXS(:,:))
  ZSIZEHB=ISIZEHB
  CALL REDUCESUM_ll(ZSIZEHB,IINFO_ll)
  IF (IINFO_ll /=0 ) THEN
    WRITE(ILUOUT,*) 'WRITE_SERIES: PB in REDUCESUM for SERIES1 SIZEHB-SEA',ISIZEHB
 !call abortstop
    CALL CLOSE_ll(HLUOUT,IOSTAT=IRESP)
    CALL ABORT
    STOP
  END IF
  XSSERIES1(1,1,1,1:NSNBSTEPT,1,2*INAV+1:NAVER1)=ZVAR2D(:,2*INAV+1:NAVER1)/MAX(ZSIZEHB,1.)
END IF
DEALLOCATE(ZVAR2D)
!
!*      2.2  Minmax processes of temporal series 
!
! first we transfer the single max/min value in a 3D array (x,y,t) on the sub-domain
!  then we gather the arrays on the global domain and compute the Max or min 
!
IF (LWMINMAX) THEN
  ALLOCATE(ZVAR3D(IIU,IJU,NSNBSTEPT))
  ALLOCATE(ZVAR3D_ll(IIU_ll,IJU_ll,NSNBSTEPT))
  ISB1=NAVER1
  DO JI=1,ISER
    ! Max velocity 
    ISB1=ISB1+1
    IF (LSERIES1) THEN
      DO JT=1,NSNBSTEPT
        ZVAR3D(:,:,JT)=XSSERIES1(1,1,1,JT,1,ISB1)
      END DO
    ELSE
      ZVAR3D(:,:,:)=-1.E+18
    END IF
    CALL GATHERALL_FIELD_ll('XY',ZVAR3D,ZVAR3D_ll,IINFO_ll)
    IF (IINFO_ll /=0 ) THEN
      WRITE(ILUOUT,FMT='("WRITE_SERIES:PB FOR SERIES1(max)-GATHER")')
 !callabortstop
      CALL CLOSE_ll(HLUOUT,IOSTAT=IRESP)
      CALL ABORT
      STOP
    END IF
    DO JT=1,NSNBSTEPT
      XSSERIES1(1,1,1,JT,1,ISB1)=MAXVAL(ZVAR3D_ll(:,:,JT))
    END DO
    ! Min velocity 
    ISB1=ISB1+1
    IF (LSERIES1) THEN
      DO JT=1,NSNBSTEPT
        ZVAR3D(:,:,JT)=XSSERIES1(1,1,1,JT,1,ISB1)
      END DO
    ELSE
      ZVAR3D(:,:,:)=+1.E+18
    END IF
    CALL GATHERALL_FIELD_ll('XY',ZVAR3D,ZVAR3D_ll,IINFO_ll)
    IF (IINFO_ll /=0 ) THEN
      WRITE(ILUOUT,FMT='("WRITE_SERIES:PB FOR SERIES1(min)-GATHER")')
 !callabortstop
      CALL CLOSE_ll(HLUOUT,IOSTAT=IRESP)
      CALL ABORT
      STOP
    END IF
    DO JT=1,NSNBSTEPT
      XSSERIES1(1,1,1,JT,1,ISB1)=MINVAL(ZVAR3D_ll(:,:,JT))
    END DO
  END DO
  ! 
  DEALLOCATE(ZVAR3D,ZVAR3D_ll)
ENDIF
!
!*      2.3  Write in diachro file
!
GICP=.TRUE. ; GJCP=.TRUE. ; GKCP=.TRUE.
CALL WRITE_DIACHRO(HFILEDIA,HLUOUT,'TSERIES','CART',NSGRIDD1,XSDATIME(:,1:NSNBSTEPT),   &
                   XSSERIES1(1:1,1:1,1:1,1:NSNBSTEPT,:,:),               &
                   XSTRAJT(1:NSNBSTEPT,:),CSTITLE1,CSUNIT1,CSCOMMENT1,   &
                   GICP,GJCP,GKCP,                               &
                   KIL=1,KIH=1,KJL=1,KJH=1,KKL=1,KKH=1)
!
!----------------------------------------------------------------------------
!
!*      3.   Temporal series (z,t)
!            ---------------------
!
!*      3.1  Average processes of temporal series 
!
ALLOCATE(ZVAR3D(IKMAX,NSNBSTEPT,NSTEMP_SERIE2))
!
IF (LSERIES2) THEN
  ZVAR3D(:,:,:)=XSSERIES2(1,1,1:IKMAX,1:NSNBSTEPT,1,1:NSTEMP_SERIE2)
ELSE
  ZVAR3D(:,:,:)=0.
END IF
CALL REDUCESUM_ll(ZVAR3D,IINFO_ll)
IF (IINFO_ll /=0 ) THEN
  WRITE(UNIT=ILUOUT,FMT='("WRITE_SERIES:PB FOR SERIES2-SUM")')
 !callabortstop
  CALL CLOSE_ll(HLUOUT,IOSTAT=IRESP)
  CALL ABORT
  STOP
END IF
!
ISIZEHB=(NIBOXH-NIBOXL+1)*(NJBOXH-NJBOXL+1)
INAV=NSTEMP_SERIE2/ISER 
XSSERIES2(1,1,1:IKMAX,1:NSNBSTEPT,1,1:INAV)=ZVAR3D(:,:,1:INAV)/ISIZEHB
IF (LMASKLANDSEA) THEN
  ISIZEHB=COUNT(LINBOXL(:,:))
  ZSIZEHB=ISIZEHB
  CALL REDUCESUM_ll(ZSIZEHB,IINFO_ll)
  IF (IINFO_ll /=0 ) THEN
    WRITE(ILUOUT,*) 'WRITE_SERIES: PB in REDUCESUM for SERIES2 SIZEHB-LAND',ISIZEHB
 !callabortstop
    CALL CLOSE_ll(HLUOUT,IOSTAT=IRESP)
    CALL ABORT
    STOP
  END IF
  XSSERIES2(1,1,1:IKMAX,1:NSNBSTEPT,1,INAV+1:2*INAV)=ZVAR3D(:,:,INAV+1:2*INAV)/MAX(ZSIZEHB,1.)
  ISIZEHB=COUNT(LINBOXS(:,:))
  ZSIZEHB=ISIZEHB
  CALL REDUCESUM_ll(ZSIZEHB,IINFO_ll)
  IF (IINFO_ll /=0 ) THEN
    WRITE(ILUOUT,*) 'WRITE_SERIES: PB in REDUCESUM for SERIES2 SIZEHB-SEA',ISIZEHB
 !callabortstop
    CALL CLOSE_ll(HLUOUT,IOSTAT=IRESP)
    CALL ABORT
    STOP
  ENDIF
  XSSERIES2(1,1,1:IKMAX,1:NSNBSTEPT,1,2*INAV+1:NSTEMP_SERIE2)=ZVAR3D(:,:,2*INAV+1:NSTEMP_SERIE2)/MAX(ZSIZEHB,1.)
END IF
DEALLOCATE(ZVAR3D)
!
!*      3.2  Write in diachro file
!
GICP=.TRUE. ; GJCP=.TRUE. ; GKCP=.FALSE.
CALL WRITE_DIACHRO(HFILEDIA,HLUOUT,'ZTSERIES','CART',NSGRIDD2,XSDATIME(:,1:NSNBSTEPT),   &
                   XSSERIES2(1:1,1:1,1:IKMAX,1:NSNBSTEPT,:,:),            &
                   XSTRAJT(1:NSNBSTEPT,:),CSTITLE2,CSUNIT2,CSCOMMENT2,    &
                   GICP,GJCP,GKCP,                 &
                   KIL=1,KIH=1,KJL=1,KJH=1,KKL=IKB,KKH=IKE)
!
!----------------------------------------------------------------------------
!
!*      4.   Temporal series (x,t)
!            ---------------------
! 
ALLOCATE(ZVAR3D(IIU,1,NSNBSTEPT)) 
ALLOCATE(ZWORK2D(IIU_ll,NSNBSTEPT))
ALLOCATE(ZSERIES3_ll(IIU_ll,1,1,NSNBSTEPT,1,NSTEMP3))
ALLOCATE(YSTITLE3S(NSTEMP_SERIE3))
!
DO JS=1,NBJSLICE
  ISB1=1 + (JS-1) * NSTEMP_SERIE3
  ISB2=JS * NSTEMP_SERIE3 
  IJL=NJSLICEL(JS)
  IJH=NJSLICEH(JS)
  ISIZEHB=(IJH-IJL+1)
  ZSERIES3_ll(:,:,:,:,:,:)=0.
  DO JJ=ISB1,ISB2    
    ZVAR3D(:,:,:)=0.
    IF (LSERIES3(JS)) THEN
      IISL=NISL(JS)   
      IISH=NISH(JS)
      DO JT=1,NSNBSTEPT
        DO JI=IISL,IISH
          IMI=JI-IISL+1
          ZVAR3D(JI,1,JT)=XSSERIES3(IMI,1,1,JT,1,JJ)
        END DO
      END DO
    END IF
    CALL SUM_DIM1_ll(ZVAR3D,ZWORK2D,INFO_ll)
    IF (IINFO_ll /=0 ) THEN
      WRITE(ILUOUT,FMT='("WRITE_SERIES:PB FOR SERIES3-JS,JJ:",I3,I3)') JS,JJ
 !callabortstop
      CALL CLOSE_ll(HLUOUT,IOSTAT=IRESP)
      CALL ABORT
      STOP
    END IF
    DO JT=1,NSNBSTEPT
      DO JI=IIB_ll,IIE_ll
        ZSERIES3_ll(JI,1,1,JT,1,JJ)=ZWORK2D(JI,JT)/ISIZEHB
      END DO
    END DO
  END DO
!
!*      4.1  Write in diachro file
!
  WRITE(YS,'(I2.2)') JS
  YGROUP='XTSERIES'//YS
  WRITE(YSL,'(I3.3)') IJL
  WRITE(YSH,'(I3.3)') IJH
  DO JT=1, NSTEMP_SERIE3 
    YSTITLE3S(JT)=ADJUSTL(ADJUSTR(CSTITLE3(JT))//'Y'//YSL//'-'//YSH)
  END DO
  GICP=.FALSE. ; GJCP=.TRUE. ; GKCP=.TRUE.
  CALL WRITE_DIACHRO(HFILEDIA,HLUOUT,YGROUP,'CART',NSGRIDD3,XSDATIME(:,1:NSNBSTEPT),    &
                      ZSERIES3_ll(1:IIU_ll,1:1,1:1,1:NSNBSTEPT,1:1,ISB1:ISB2),&
                      XSTRAJT(1:NSNBSTEPT,:),YSTITLE3S,CSUNIT3,CSCOMMENT3,     &
                      GICP,GJCP,GKCP,                               &
                      KIL=1,KIH=IIU_ll,KJL=1,KJH=1,KKL=1,KKH=1)
END DO
DEALLOCATE(ZVAR3D,ZWORK2D,ZSERIES3_ll)
!
IF (NVERB>=5) WRITE(ILUOUT,*) 'WRITE_SERIESn: end'
!----------------------------------------------------------------------------
END SUBROUTINE WRITE_SERIES_n
