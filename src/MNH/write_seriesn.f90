!MNH_LIC Copyright 2002-2021 CNRS, Meteo-France and Universite Paul Sabatier
!MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
!MNH_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!MNH_LIC for details. version 1.
!-----------------------------------------------------------------
!########################
 MODULE MODI_WRITE_SERIES_n
!########################
!
IMPLICIT NONE
!
INTERFACE
!
      SUBROUTINE WRITE_SERIES_n(TPDIAFILE)
!
USE MODD_IO, ONLY: TFILEDATA
!
TYPE(TFILEDATA),    INTENT(IN) :: TPDIAFILE    ! file to write
!
END SUBROUTINE WRITE_SERIES_n
!
END INTERFACE
!
END MODULE MODI_WRITE_SERIES_n
!     ###################
      SUBROUTINE WRITE_SERIES_n (TPDIAFILE)

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
!              07/01/2013: add key for netCDF writing
!  J. Escobar  15/09/2015: WENO5 & JPHEXT <> 1
!  P. Wautelet 11/07/2016: removed MNH_NCWRIT define
!  P. Wautelet 05/2016-04/2018: new data structures and calls for I/O
!  P. Wautelet 13/09/2019: budget: simplify and modernize date/time management
!  P. Wautelet 09/10/2020: Write_diachro: use new datatype tpfields
!  P. Wautelet 03/03/2021: budgets: add tbudiachrometadata type (useful to pass more information to Write_diachro)
!-------------------------------------------------------------------------------
!
!
!*    0. Declaration
!     --------------
!
use modd_budget,        only: NLVL_CATEGORY, NLVL_SUBCATEGORY, NLVL_GROUP, NLVL_SHAPE, NLVL_TIMEAVG, NLVL_NORM, NLVL_MASK, &
                              tbudiachrometadata
use modd_field,         only: NMNHDIM_NI, NMNHDIM_NI_U,                                                               &
                              NMNHDIM_SERIES_LEVEL, NMNHDIM_SERIES_LEVEL_W, NMNHDIM_SERIES_TIME, NMNHDIM_SERIES_PROC, &
                              NMNHDIM_UNUSED,                                                                         &
                              tfieldmetadata_base, TYPEREAL
USE MODD_IO,            ONLY: NGEN_VERB, TFILEDATA
USE MODD_LUNIT_n,       ONLY: TLUOUT
USE MODD_PARAMETERS
USE MODD_SERIES
USE MODD_SERIES_n
!
USE MODE_GATHER_ll
USE MODE_ll
USE MODE_MSG
use mode_write_diachro, only: Write_diachro
!
IMPLICIT NONE
!
!*       0.1   declarations of arguments
!
TYPE(TFILEDATA),    INTENT(IN) :: TPDIAFILE    ! file to write
!
!*       0.2     Local variables 
!
INTEGER                              :: IIB,IJB,IKB     ! Begin of physical dimensions
INTEGER                              :: IIE,IJE,IKE     ! End   of physical dimensions
INTEGER                              :: JS, JT, JJ, JI, JP ! Loop indices
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
INTEGER  :: IRESP   ! Return code of FM-routines
INTEGER  :: INFO_ll   ! Return code of FM-routines
INTEGER :: ISER,INAV
REAL :: ZSIZEHB
CHARACTER(LEN=100) :: YMSG
type(tbudiachrometadata)                             :: tzbudiachro
type(tfieldmetadata_base), dimension(:), allocatable :: tzfields
type(tfiledata)                                      :: tzfile
!----------------------------------------------------------------------------
!
!*    1.     INITIALIZATION
!            --------------
!
IF (NGEN_VERB>=NVERB_DEBUG) CALL PRINT_MSG(NVERB_DEBUG,'GEN','WRITE_SERIES_n','beginning')
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
 !callabortstop
  WRITE(YMSG,*) 'WRITE_SERIESn: NSNBSTEPT .NE. NSCOUNTD ',NSNBSTEPT,NSCOUNTD
  CALL PRINT_MSG(NVERB_FATAL,'GEN','WRITE_SERIES_n',YMSG)
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
 !callabortstop
  CALL PRINT_MSG(NVERB_FATAL,'GEN','WRITE_SERIES_n','problem in REDUCESUM for SERIES1')
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
    WRITE(YMSG,*) 'problem in REDUCESUM for SERIES1 SIZEHB-LAND',ISIZEHB
 !callabortstop
    CALL PRINT_MSG(NVERB_FATAL,'GEN','WRITE_SERIES_n',YMSG)
  END IF
  XSSERIES1(1,1,1,1:NSNBSTEPT,1,INAV+1:2*INAV)=ZVAR2D(:,INAV+1:2*INAV)/MAX(ZSIZEHB,1.)
  !
  ISIZEHB=COUNT(LINBOXS(:,:))
  ZSIZEHB=ISIZEHB
  CALL REDUCESUM_ll(ZSIZEHB,IINFO_ll)
  IF (IINFO_ll /=0 ) THEN
    WRITE(YMSG,*) 'problem in REDUCESUM for SERIES1 SIZEHB-SEA',ISIZEHB
 !call abortstop
    CALL PRINT_MSG(NVERB_FATAL,'GEN','WRITE_SERIES_n',YMSG)
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
      WRITE(YMSG,*) 'problem for SERIES1(max)-GATHER',ISIZEHB
 !callabortstop
      CALL PRINT_MSG(NVERB_FATAL,'GEN','WRITE_SERIES_n',YMSG)
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
 !callabortstop
      CALL PRINT_MSG(NVERB_FATAL,'GEN','WRITE_SERIES_n','problem for SERIES1(min)-GATHER')
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
allocate( tzfields( nstemp_serie1 ) )

tzfields(:)%cmnhname  = cstitle1(:)
tzfields(:)%cstdname  = ''
tzfields(:)%clongname = cstitle1(:)
tzfields(:)%cunits    = csunit1(:)
tzfields(:)%ccomment  = cscomment1(:)
tzfields(:)%ngrid     = nsgridd1(:)
tzfields(:)%ntype     = TYPEREAL
! tzfields(:)%ndims     = 2
tzfields(:)%ndims     = 1 !Set to 1 because write are done in a loop (1 write per "process")
tzfields(:)%ndimlist(1) = NMNHDIM_UNUSED
tzfields(:)%ndimlist(2) = NMNHDIM_UNUSED
tzfields(:)%ndimlist(3) = NMNHDIM_UNUSED
tzfields(:)%ndimlist(4) = NMNHDIM_SERIES_TIME
tzfields(:)%ndimlist(5) = NMNHDIM_UNUSED
! tzfields(:)%ndimlist(6) = NMNHDIM_SERIES_PROC
tzfields(:)%ndimlist(6) = NMNHDIM_UNUSED !Set to unused because write are done in a loop (1 write per "process")

tzbudiachro%lleveluse(NLVL_CATEGORY)    = .true.
tzbudiachro%clevels  (NLVL_CATEGORY)    = 'Time_series'
tzbudiachro%ccomments(NLVL_CATEGORY)    = 'Level for the different time series'

tzbudiachro%lleveluse(NLVL_SUBCATEGORY) = .false.
tzbudiachro%clevels  (NLVL_SUBCATEGORY) = ''
tzbudiachro%ccomments(NLVL_SUBCATEGORY) = ''

tzbudiachro%lleveluse(NLVL_GROUP)       = .true.
tzbudiachro%clevels  (NLVL_GROUP)       = 'TSERIES'
tzbudiachro%ccomments(NLVL_GROUP)       = 'Time series of horizontally and vertically averaged fields'

tzbudiachro%lleveluse(NLVL_SHAPE)       = .false.
tzbudiachro%clevels  (NLVL_SHAPE)       = 'Cartesian' !It is based on a cartesian domain (with compression in all directions)
tzbudiachro%ccomments(NLVL_SHAPE)       = 'Cartesian domain'

tzbudiachro%lleveluse(NLVL_TIMEAVG)     = .false.
tzbudiachro%clevels  (NLVL_TIMEAVG)     = 'Not_time_averaged'
tzbudiachro%ccomments(NLVL_TIMEAVG)     = 'Values are not time averaged'

tzbudiachro%lleveluse(NLVL_NORM)        = .false.
tzbudiachro%clevels  (NLVL_NORM)        = 'Not_normalized'
tzbudiachro%ccomments(NLVL_NORM)        = 'Values are not normalized'

tzbudiachro%lleveluse(NLVL_MASK)        = .true.
! tzbudiachro%clevels  (NLVL_MASK)        = DONE LATER
! tzbudiachro%ccomments(NLVL_MASK)        = DONE LATER

tzbudiachro%lmobile    = .false.
tzbudiachro%licompress = .true.
tzbudiachro%ljcompress = .true.
tzbudiachro%lkcompress = .true.
tzbudiachro%ltcompress = .false.
tzbudiachro%lnorm      = .false.
tzbudiachro%nil        = niboxl
tzbudiachro%nih        = niboxh
tzbudiachro%njl        = njboxl
tzbudiachro%njh        = njboxh
tzbudiachro%nkl        = 1
tzbudiachro%nkh        = ikmax

tzfile = tpdiafile
if ( Trim( tpdiafile%cformat ) == 'LFI' .or. Trim( tpdiafile%cformat ) == 'LFICDF4' ) then
  !For LFI files, it is necessary to write all the 'processes' (source terms) of the different masks in one pass
  !to ensure that they are grouped together and not overwritten
  tzfile%cformat = 'LFI'

  call Write_diachro( tzfile, tzbudiachro, tzfields, tpsdates(1:nsnbstept), &
                      xsseries1(1:1,1:1,1:1,1:nsnbstept,1:1,:)              )
end if

if ( Trim( tpdiafile%cformat ) /= 'LFI' ) then! Loop on the different masks
  tzfile%cformat = 'NETCDF4'

  ! Do not provide all tzfields once because they can be stored in different HDF groups (based on masks)
  do jp = 1 , nstemp_serie1
    tzbudiachro%clevels(NLVL_MASK) = Trim( csmask1(jp) )
    tzbudiachro%ccomments(NLVL_MASK) = ''

    call Write_diachro( tzfile, tzbudiachro, [ tzfields(jp) ], tpsdates(1:nsnbstept), &
                        xsseries1(1:1,1:1,1:1,1:nsnbstept,1:1,jp:jp)                 )
  end do
end if

deallocate( tzfields )
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
 !callabortstop
  CALL PRINT_MSG(NVERB_FATAL,'GEN','WRITE_SERIES_n','problem for SERIES2-SUM')
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
    WRITE(YMSG,*) 'problem in REDUCESUM for SERIES2 SIZEHB-LAND',ISIZEHB
 !callabortstop
    CALL PRINT_MSG(NVERB_FATAL,'GEN','WRITE_SERIES_n',YMSG)
  END IF
  XSSERIES2(1,1,1:IKMAX,1:NSNBSTEPT,1,INAV+1:2*INAV)=ZVAR3D(:,:,INAV+1:2*INAV)/MAX(ZSIZEHB,1.)
  ISIZEHB=COUNT(LINBOXS(:,:))
  ZSIZEHB=ISIZEHB
  CALL REDUCESUM_ll(ZSIZEHB,IINFO_ll)
  IF (IINFO_ll /=0 ) THEN
    WRITE(YMSG,*) 'problem in REDUCESUM for SERIES2 SIZEHB-SEA',ISIZEHB
 !callabortstop
    CALL PRINT_MSG(NVERB_FATAL,'GEN','WRITE_SERIES_n',YMSG)
  ENDIF
  XSSERIES2(1,1,1:IKMAX,1:NSNBSTEPT,1,2*INAV+1:NSTEMP_SERIE2)=ZVAR3D(:,:,2*INAV+1:NSTEMP_SERIE2)/MAX(ZSIZEHB,1.)
END IF
DEALLOCATE(ZVAR3D)
!
!*      3.2  Write in diachro file
!
allocate( tzfields( nstemp_serie2 ) )

tzfields(:)%cmnhname  = cstitle2(:)
tzfields(:)%cstdname  = ''
tzfields(:)%clongname = cstitle2(:)
tzfields(:)%cunits    = csunit2(:)
tzfields(:)%ccomment  = cscomment2(:)
tzfields(:)%ngrid     = nsgridd2(:)
tzfields(:)%ntype     = TYPEREAL
! tzfields(:)%ndims     = 3
tzfields(:)%ndims     = 2 !Set to 2 because write are done in a loop (1 write per "process")
tzfields(:)%ndimlist(1) = NMNHDIM_UNUSED
tzfields(:)%ndimlist(2) = NMNHDIM_UNUSED
do ji = 1, nstemp_serie2
  if ( nsgridd2(ji) == 1 ) then
    tzfields(ji)%ndimlist(3) = NMNHDIM_SERIES_LEVEL
  else if ( nsgridd2(ji) == 4 ) then
    tzfields(ji)%ndimlist(3) = NMNHDIM_SERIES_LEVEL_W
  else
    call Print_msg( NVERB_ERROR, 'IO', 'Write_series_n', 'invalid nsgridd2' )
    tzfields(ji)%ndimlist(3) = NMNHDIM_SERIES_LEVEL
  end if
end do
tzfields(:)%ndimlist(4) = NMNHDIM_SERIES_TIME
tzfields(:)%ndimlist(5) = NMNHDIM_UNUSED
! tzfields(:)%ndimlist(6) = NMNHDIM_SERIES_PROC
tzfields(:)%ndimlist(6) = NMNHDIM_UNUSED !Set to unused because write are done in a loop (1 write per "process")

tzbudiachro%lleveluse(NLVL_CATEGORY)    = .true.
tzbudiachro%clevels  (NLVL_CATEGORY)    = 'Time_series'
tzbudiachro%ccomments(NLVL_CATEGORY)    = 'Level for the different time series'

tzbudiachro%lleveluse(NLVL_SUBCATEGORY) = .false.
tzbudiachro%clevels  (NLVL_SUBCATEGORY) = ''
tzbudiachro%ccomments(NLVL_SUBCATEGORY) = ''

tzbudiachro%lleveluse(NLVL_GROUP)       = .true.
tzbudiachro%clevels  (NLVL_GROUP)       = 'ZTSERIES'
tzbudiachro%ccomments(NLVL_GROUP)       = 'Time series of horizontally averaged vertical profile'

tzbudiachro%lleveluse(NLVL_SHAPE)       = .false.
tzbudiachro%clevels  (NLVL_SHAPE)       = 'Cartesian' !It is based on a cartesian domain (with horizontal compression)
tzbudiachro%ccomments(NLVL_SHAPE)       = 'Cartesian domain'

tzbudiachro%lleveluse(NLVL_TIMEAVG)     = .false.
tzbudiachro%clevels  (NLVL_TIMEAVG)     = 'Not_time_averaged'
tzbudiachro%ccomments(NLVL_TIMEAVG)     = 'Values are not time averaged'

tzbudiachro%lleveluse(NLVL_NORM)        = .false.
tzbudiachro%clevels  (NLVL_NORM)        = 'Not_normalized'
tzbudiachro%ccomments(NLVL_NORM)        = 'Values are not normalized'

tzbudiachro%lleveluse(NLVL_MASK)        = .true.
! tzbudiachro%clevels  (NLVL_MASK)        = DONE LATER
! tzbudiachro%ccomments(NLVL_MASK)        = DONE LATER

tzbudiachro%lmobile    = .false.
tzbudiachro%licompress = .true.
tzbudiachro%ljcompress = .true.
tzbudiachro%lkcompress = .false.
tzbudiachro%ltcompress = .false.
tzbudiachro%lnorm      = .false.
tzbudiachro%nil        = niboxl
tzbudiachro%nih        = niboxh
tzbudiachro%njl        = njboxl
tzbudiachro%njh        = njboxh
tzbudiachro%nkl        = 1
tzbudiachro%nkh        = ikmax

if ( Trim( tpdiafile%cformat ) == 'LFI' .or. Trim( tpdiafile%cformat ) == 'LFICDF4' ) then
  !For LFI files, it is necessary to write all the 'processes' (source terms) of the different masks in one pass
  !to ensure that they are grouped together and not overwritten
  tzfile%cformat = 'LFI'

  call Write_diachro( tzfile, tzbudiachro, tzfields, tpsdates(1:nsnbstept), &
                      xsseries2(1:1,1:1,1:ikmax,1:nsnbstept,1:1,:)          )
end if

if ( Trim( tpdiafile%cformat ) /= 'LFI' ) then! Loop on the different masks
  tzfile%cformat = 'NETCDF4'

  ! Do not provide all tzfields once because they can be stored in different HDF groups (based on masks)
  do jp = 1 , nstemp_serie2
    tzbudiachro%clevels(NLVL_MASK) = csmask2(jp)
    tzbudiachro%ccomments(NLVL_MASK) = ''

    call Write_diachro( tzfile, tzbudiachro, [ tzfields(jp) ], tpsdates(1:nsnbstept), &
                        xsseries2(1:1,1:1,1:ikmax,1:nsnbstept,1:1,jp:jp)              )
  end do
end if

deallocate( tzfields )
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
      WRITE(YMSG,FMT='("problem for SERIES3-JS,JJ:",I3,I3)') JS,JJ
 !callabortstop
      CALL PRINT_MSG(NVERB_FATAL,'GEN','WRITE_SERIES_n',YMSG)
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
  allocate( tzfields( isb2 - isb1 + 1 ) )

  tzfields(:)%cmnhname  = ystitle3s(:)
  tzfields(:)%cstdname  = ''
  tzfields(:)%clongname = ystitle3s(:)
  tzfields(:)%cunits    = csunit3(:)
  tzfields(:)%ccomment  = cscomment3(:)
  tzfields(:)%ngrid     = nsgridd3(:)
  tzfields(:)%ntype     = TYPEREAL
  tzfields(:)%ndims     = 3
  do ji = 1, isb2 - isb1 + 1
    if ( nsgridd3(ji) == 1 .or. nsgridd3(ji) == 4 ) then
      tzfields(ji)%ndimlist(1) = NMNHDIM_NI
    else if ( nsgridd3(ji) == 2 ) then
      tzfields(ji)%ndimlist(1) = NMNHDIM_NI_U
    else
      call Print_msg( NVERB_ERROR, 'IO', 'Write_series_n', 'invalid nsgridd3' )
      tzfields(ji)%ndimlist(1) = NMNHDIM_NI
    end if
  end do
  tzfields(:)%ndimlist(2) = NMNHDIM_UNUSED
  tzfields(:)%ndimlist(3) = NMNHDIM_UNUSED
  tzfields(:)%ndimlist(4) = NMNHDIM_SERIES_TIME
  tzfields(:)%ndimlist(5) = NMNHDIM_UNUSED
  tzfields(:)%ndimlist(6) = NMNHDIM_SERIES_PROC

  tzbudiachro%lleveluse(NLVL_CATEGORY)    = .true.
  tzbudiachro%clevels  (NLVL_CATEGORY)    = 'Time_series'
  tzbudiachro%ccomments(NLVL_CATEGORY)    = 'Level for the different time series'

  tzbudiachro%lleveluse(NLVL_SUBCATEGORY) = .false.
  tzbudiachro%clevels  (NLVL_SUBCATEGORY) = ''
  tzbudiachro%ccomments(NLVL_SUBCATEGORY) = ''

  tzbudiachro%lleveluse(NLVL_GROUP)       = .true.
  tzbudiachro%clevels  (NLVL_GROUP)       = Trim( ygroup )
  tzbudiachro%ccomments(NLVL_GROUP)       = 'Time series of y-horizontally averaged fields at one level ' // &
                                            'or vertically averaged between 2 levels'

  tzbudiachro%lleveluse(NLVL_SHAPE)       = .false.
  tzbudiachro%clevels  (NLVL_SHAPE)       = 'Cartesian' !It is based on a cartesian domain (with compression in 1 direction)
  tzbudiachro%ccomments(NLVL_SHAPE)       = 'Cartesian domain'

  tzbudiachro%lleveluse(NLVL_TIMEAVG)     = .false.
  tzbudiachro%clevels  (NLVL_TIMEAVG)     = 'Not_time_averaged'
  tzbudiachro%ccomments(NLVL_TIMEAVG)     = 'Values are not time averaged'

  tzbudiachro%lleveluse(NLVL_NORM)        = .false.
  tzbudiachro%clevels  (NLVL_NORM)        = 'Not_normalized'
  tzbudiachro%ccomments(NLVL_NORM)        = 'Values are not normalized'

  tzbudiachro%lleveluse(NLVL_MASK)        = .false.
  tzbudiachro%clevels  (NLVL_MASK)        = ''
  tzbudiachro%ccomments(NLVL_MASK)        = ''

  tzbudiachro%lmobile    = .false.
  tzbudiachro%licompress = .false.
  tzbudiachro%ljcompress = .true.
  tzbudiachro%lkcompress = .true.
  tzbudiachro%ltcompress = .false.
  tzbudiachro%lnorm      = .false.
  tzbudiachro%nil        = 1
  tzbudiachro%nih        = iiu_ll
  tzbudiachro%njl        = 1
  tzbudiachro%njh        = 1
  tzbudiachro%nkl        = 1
  tzbudiachro%nkh        = 1

  call Write_diachro( tpdiafile, tzbudiachro, tzfields, tpsdates(1:nsnbstept),     &
                      zseries3_ll(1:iiu_ll, 1:1, 1:1, 1:nsnbstept, 1:1, isb1:isb2) )

  deallocate( tzfields )
END DO
DEALLOCATE(ZVAR3D,ZWORK2D,ZSERIES3_ll)
!
IF (NGEN_VERB>=NVERB_DEBUG) CALL PRINT_MSG(NVERB_DEBUG,'GEN','WRITE_SERIES_n','end')
!----------------------------------------------------------------------------
END SUBROUTINE WRITE_SERIES_n
