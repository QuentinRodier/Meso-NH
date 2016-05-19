!     #########
      SUBROUTINE PGD_COVER(HPROGRAM)
!     ##############################################################
!
!!**** *PGD_COVER* monitor for averaging and interpolations of cover fractions
!!
!!    PURPOSE
!!    -------
!!
!!    METHOD
!!    ------
!!   
!
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
!!    V. Masson        Meteo-France
!!
!!    MODIFICATION
!!    ------------
!!
!!    Original    10/12/97
!!    B. Decharme  06/2008  limit of coast coverage under which the coast is replaced by sea or inland water
!!    B. Decharme  06/2009  remove lack and sea as the user want
!!    B. Decharme  07/2009  compatibility between Surfex and Orca (Nemo) grid (Earth Model)
!!    B. Decharme  07/2012  if sea or water imposed to 1 in a grid cell: no extrapolation
!!
!----------------------------------------------------------------------------
!
!*    0.     DECLARATION
!            -----------
!
USE MODD_SURF_PAR,       ONLY : XUNDEF
USE MODD_PGD_GRID,       ONLY : CGRID, NL, XGRID_PAR, NGRID_PAR
USE MODD_PGDWORK,        ONLY : XSUMCOVER, NSIZE
USE MODD_DATA_COVER_PAR, ONLY : JPCOVER, NROCK, NSEA, NWATER, NPERMSNOW
USE MODD_SURF_ATM_n,      ONLY : CNATURE, CSEA, CTOWN, CWATER,            &
                                  XNATURE, XSEA, XTOWN, XWATER,           &
                                  XCOVER, LCOVER,                         &
                                  NSIZE_NATURE, NSIZE_SEA,                &
                                  NSIZE_TOWN, NSIZE_WATER,NSIZE_FULL,     &
                                  NDIM_NATURE, NDIM_SEA,                  &
                                  NDIM_TOWN,NDIM_WATER  
!
USE MODI_GET_LUOUT
USE MODE_GRIDTYPE_GAUSS

USE MODI_TREAT_FIELD
USE MODI_INTERPOL_FIELD2D
USE MODI_CONVERT_COVER_FRAC
!
USE MODI_READ_LCOVER
USE MODI_READ_SURF
USE MODI_SUM_ON_ALL_PROCS
!
USE MODI_READ_NAM_PGD_COVER
!
USE MODI_INIT_IO_SURF_n
USE MODI_END_IO_SURF_n
#ifdef ASC
USE MODD_IO_SURF_ASC, ONLY : CFILEIN
#endif
#ifdef FA
USE MODD_IO_SURF_FA,  ONLY : CFILEIN_FA
#endif
#ifdef LFI
USE MODD_IO_SURF_LFI, ONLY : CFILEIN_LFI
#endif
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
USE MODI_ABOR1_SFX
!
USE MODI_PGD_ECOCLIMAP2_DATA
!
IMPLICIT NONE
!
!*    0.1    Declaration of arguments
!            ------------------------
!
 CHARACTER(LEN=6),    INTENT(IN)    :: HPROGRAM     ! Type of program
!
!
!*    0.2    Declaration of local variables
!            ------------------------------
!
INTEGER               :: ILUOUT    ! output listing logical unit
!
INTEGER               :: JCOVER    ! loop counter on covers
INTEGER               :: JL        ! loop counter on horizontal points
INTEGER, DIMENSION(1) :: IMAXCOVER ! index of maximum cover for the given point
INTEGER               :: IRESP     ! Error code after redding
!
REAL, DIMENSION(:), ALLOCATABLE :: ZLAT
!
!*    0.3    Declaration of namelists
!            ------------------------
!
REAL, DIMENSION(:), ALLOCATABLE :: XUNIF_COVER ! value of each cover (cover will be
!                                                uniform on the horizontal)
REAL, DIMENSION(:), ALLOCATABLE :: ZSEA !to check compatibility between 
REAL, DIMENSION(:), ALLOCATABLE :: ZWATER !prescribed fractions and ECOCLIMAP
REAL, DIMENSION(:), ALLOCATABLE :: ZNATURE
REAL, DIMENSION(:), ALLOCATABLE :: ZTOWN
REAL, DIMENSION(:,:), ALLOCATABLE :: ZCOVER_NATURE, ZCOVER_TOWN, ZCOVER_SEA, ZCOVER_WATER
REAL, DIMENSION(JPCOVER)         :: ZDEF
 CHARACTER(LEN=10)                :: YFIELD
!
 CHARACTER(LEN=28)        :: YCOVER      ! file name for cover types
 CHARACTER(LEN=6)         :: YFILETYPE   ! data file type
REAL                     :: XRM_COVER   ! limit of coverage under which the
                                        ! cover is removed. Default is 1.E-6
REAL                     :: XRM_COAST   ! limit of coast coverage under which
                                        ! the coast is replaced by sea or
                                        ! inland water. Default is 1.
!
REAL                     :: XRM_LAKE    ! limit of inland lake coverage under which
                                        ! the water is removed. Default is 0.0
!                                        
REAL                     :: XRM_SEA     ! limit of sea coverage under which
                                        ! the sea is removed. Default is 0.0
!
LOGICAL                  :: LORCA_GRID  ! flag to compatibility between Surfex and Orca grid 
                                        ! (Earth Model over Antarctic)
REAL                     :: XLAT_ANT    ! Lattitude limit from Orca grid (Antartic)
!
LOGICAL                  :: LIMP_COVER  ! Imposed values for Cover from another PGD file
INTEGER                  :: ICOVER      ! 0 if cover is not present, >1 if present somewhere
!                                       ! (even on another processor)
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!---------------------------------------------------------------
!
!*    1.      Initializations
!             ---------------
!
IF (LHOOK) CALL DR_HOOK('PGD_COVER',0,ZHOOK_HANDLE)
 CALL GET_LUOUT(HPROGRAM,ILUOUT)
!
ALLOCATE(XCOVER  (NL,JPCOVER))
ALLOCATE(XUNIF_COVER(JPCOVER))
!
XCOVER      = XUNDEF
XUNIF_COVER = XUNDEF
!-------------------------------------------------------------------------------
!
!*    2.      Input file for cover types
!             --------------------------
!
 CALL READ_NAM_PGD_COVER(HPROGRAM, YCOVER, YFILETYPE, XUNIF_COVER,  &
                          XRM_COVER, XRM_COAST, XRM_LAKE, XRM_SEA,   &
                          LORCA_GRID, XLAT_ANT, LIMP_COVER           )  
!
!-------------------------------------------------------------------------------
!
!*    3.      Uniform field is prescribed
!             ---------------------------
!-------------------------------------------------------------------------------
!
IF (ANY(XUNIF_COVER/=0.)) THEN
!
!*    3.1     Verification of the total input cover fractions
!             -----------------------------------------------
!
  IF (ABS(SUM(XUNIF_COVER)-1.)>1.E-6) THEN
    WRITE(ILUOUT,*) ' '
    WRITE(ILUOUT,*) '***************************************************'
    WRITE(ILUOUT,*) '* Error in COVER fractions preparation            *'
    WRITE(ILUOUT,*) '* The prescribed covers does not fit              *'
    WRITE(ILUOUT,*) '* The sum of all cover must be equal to 1 exactly *'
    WRITE(ILUOUT,*) '***************************************************'
    WRITE(ILUOUT,*) ' '
    CALL ABOR1_SFX('PGD_COVER: SUM OF ALL COVER FRACTIONS MUST BE 1.')
!
!*    3.2     Use of the presribed cover fractions
!             ------------------------------------
!
  ELSE
    XCOVER(:,:) =0.
    DO JCOVER=1,JPCOVER
       XCOVER(:,JCOVER) = XUNIF_COVER(JCOVER)
    END DO
    XCOVER(:,:)=XCOVER(:,:)/SPREAD(SUM(XCOVER(:,:),2),2,JPCOVER)
  END IF
!
!*    3.3     No data
!             -------
!
ELSEIF (LEN_TRIM(YCOVER)==0) THEN
  WRITE(ILUOUT,*) ' '
  WRITE(ILUOUT,*) '***********************************************************'
  WRITE(ILUOUT,*) '* Error in COVER fractions preparation                    *'
  WRITE(ILUOUT,*) '* There is no prescribed cover fraction and no input file *'
  WRITE(ILUOUT,*) '***********************************************************'
  WRITE(ILUOUT,*) ' '
  CALL ABOR1_SFX('PGD_COVER: NO PRESCRIBED COVER NOR INPUT FILE')
!
!-------------------------------------------------------------------------------
ELSEIF(LIMP_COVER)THEN !LIMP_COVER (impose cover from input file at the same resolution)
!
  IF(YFILETYPE=='NETCDF')THEN
    CALL ABOR1_SFX('Use another format than netcdf for cover input file with LIMP_COVER')
  ELSE
#ifdef ASC
    CFILEIN     = ADJUSTL(ADJUSTR(YCOVER)//'.txt')
#endif
#ifdef FA
    CFILEIN_FA  = ADJUSTL(ADJUSTR(YCOVER)//'.fa')
#endif
#ifdef LFI
    CFILEIN_LFI = ADJUSTL(YCOVER)
#endif
    CALL INIT_IO_SURF_n(YFILETYPE,'FULL  ','SURF  ','READ ')
  ENDIF
!
  ALLOCATE(LCOVER(JPCOVER))
  CALL READ_LCOVER(YFILETYPE,LCOVER)
!
  CALL READ_SURF(YFILETYPE,'COVER',XCOVER(:,:),LCOVER,IRESP)
!
  CALL END_IO_SURF_n(YFILETYPE)
!
ELSE 
!-------------------------------------------------------------------------------
!
!*    3.      Averages the field
!             ------------------
!
  ALLOCATE(NSIZE     (NL)        )
  ALLOCATE(XSUMCOVER (NL,JPCOVER))
!
  NSIZE    (:)   = 0.
  XSUMCOVER(:,:) = 0.
  CALL TREAT_FIELD(HPROGRAM,'SURF  ',YFILETYPE,'A_COVR',YCOVER,  &
                     'COVER               '                        )  

!
!*    4.      Interpolation if some points are not initialized (no data for these points) (same time)
!             ---------------------------------------------------------------------------------------
!
  WRITE(YFIELD,FMT='(A)') 'covers'
  CALL INTERPOL_FIELD2D(HPROGRAM,ILUOUT,NSIZE,XCOVER(:,:),YFIELD)
!
!-------------------------------------------------------------------------------
!
!*    5.      Coherence check
!             ---------------
!
  XCOVER(:,:)=XCOVER(:,:)/SPREAD(SUM(XCOVER(:,:),2),2,JPCOVER)
!
  DEALLOCATE(NSIZE    )
  DEALLOCATE(XSUMCOVER)
!
!-------------------------------------------------------------------------------
!
!*    6.      Special treatments asked by user
!             --------------------------------
!
! * removes cover with very small coverage
  DO JL=1,SIZE(XCOVER,1)
    IMAXCOVER(:) = MAXLOC(XCOVER(JL,:))
    DO JCOVER=1,JPCOVER
      IF (XCOVER(JL,JCOVER)<=XRM_COVER .AND. JCOVER /= IMAXCOVER(1)) THEN
        XCOVER(JL,JCOVER) = 0.
      END IF
    END DO
  END DO
!
! * removes cover; replace by sea or inland water if sea or inland water > XRM_COAST
  DO JCOVER=1,JPCOVER
     DO JL=1,SIZE(NSEA)
        WHERE(XCOVER(:,NSEA(JL))>=XRM_COAST)
              XCOVER(:,JCOVER) = 0.
              XCOVER(:,NSEA(JL)) = 1.
        END WHERE 
     ENDDO
     DO JL=1,SIZE(NWATER)
        WHERE(XCOVER(:,NWATER(JL))>=XRM_COAST)
              XCOVER(:,JCOVER) = 0.
              XCOVER(:,NWATER(JL)) = 1.
        END WHERE
     ENDDO
  ENDDO
!
! * removes lake as the user want
  IF(XRM_LAKE>0.0)THEN
     DO JL=1,SIZE(NWATER)
        WHERE(XCOVER(:,NWATER(JL))<=XRM_LAKE)
              XCOVER(:,NWATER(JL)) = 0.
        ENDWHERE
     ENDDO
  ENDIF
!
! * removes sea as the user want
  IF(XRM_SEA>0.0)THEN
     DO JL=1,SIZE(NSEA)
        WHERE(XCOVER(:,NSEA(JL))<=XRM_SEA)
              XCOVER(:,NSEA(JL)) = 0.
        ENDWHERE
     ENDDO
  ENDIF
!
! * Compatibility between Surfex and Orca grid 
!   (Earth Model over water bodies and Antarctic)
!
  IF(LORCA_GRID.AND.CGRID=='GAUSS     ')THEN
!
!     No river or inland water bodies
    XCOVER(:,NWATER(2)) = 0.
    XCOVER(:,NWATER(3)) = 0.
!
    ALLOCATE(ZLAT(NL))
    CALL GET_GRIDTYPE_GAUSS(XGRID_PAR,PLAT=ZLAT)
!
    DO JL=1,SIZE(NSEA)
       WHERE(ZLAT(:)<XLAT_ANT.AND.XCOVER(:,NSEA(JL))>0.0)
             XCOVER(:,NPERMSNOW) = 1.0
             XCOVER(:,NSEA(JL))  = 0.0
       ENDWHERE 
    ENDDO
    DO JL=1,SIZE(NWATER)
       WHERE(ZLAT(:)<XLAT_ANT.AND.XCOVER(:,NWATER(JL))>0.0)
             XCOVER(:,NPERMSNOW)  = 1.0
             XCOVER(:,NWATER(JL)) = 0.0
       ENDWHERE
    ENDDO
!  
    DEALLOCATE(ZLAT)
!
  ENDIF
!
!-------------------------------------------------------------------------------
!
!*    7.      Coherence check
!             ---------------
!
    XCOVER(:,:)=XCOVER(:,:)/SPREAD(SUM(XCOVER(:,:),2),2,JPCOVER)
!
!-------------------------------------------------------------------------------
END IF
!
DEALLOCATE(XUNIF_COVER)
!
!-------------------------------------------------------------------------------
!
IF(.NOT.LIMP_COVER)THEN
        
!*    8.      List of cover present
!             ---------------------
!
  IF ( SUM_ON_ALL_PROCS(HPROGRAM,CGRID,ANY(XCOVER(:,300:)>0.,DIM=2),'COV' ) >0 ) &
  CALL PGD_ECOCLIMAP2_DATA(HPROGRAM)
!
!-------------------------------------------------------------------------------
ENDIF
!-------------------------------------------------------------------------------
!
!*    9.      Land - sea fractions
!             --------------------
!
IF (.NOT.ASSOCIATED(XSEA)) THEN

  ALLOCATE(XSEA   (NL))
  ALLOCATE(XWATER (NL))
  ALLOCATE(XNATURE(NL))
  ALLOCATE(XTOWN  (NL))
  CALL CONVERT_COVER_FRAC(XCOVER,XSEA,XNATURE,XTOWN,XWATER)

ELSE
!if fractions are prescribed, it has to be verified that the locations of
!ECOCLIMAP covers are compatible with the fractions of surface types
  ALLOCATE(ZSEA   (NL))
  ALLOCATE(ZWATER (NL))
  ALLOCATE(ZNATURE(NL))
  ALLOCATE(ZTOWN  (NL))
  CALL CONVERT_COVER_FRAC(XCOVER,ZSEA,ZNATURE,ZTOWN,ZWATER)
  !
  ALLOCATE(ZCOVER_NATURE(NL,JPCOVER))
  ALLOCATE(ZCOVER_TOWN  (NL,JPCOVER))
  ALLOCATE(ZCOVER_SEA   (NL,JPCOVER))
  ALLOCATE(ZCOVER_WATER (NL,JPCOVER))
  !
  ZCOVER_NATURE(:,:) = XCOVER(:,:)
  ZCOVER_TOWN  (:,:) = XCOVER(:,:)
  ZCOVER_SEA   (:,:) = XCOVER(:,:)
  ZCOVER_WATER (:,:) = XCOVER(:,:)
  !
  ALLOCATE(NSIZE(NL))
  !
  WRITE(ILUOUT,FMT=*) &
  '*********************************************************************'
  WRITE(ILUOUT,FMT=*) &
  '*  Coherence computation between covers and imposed nature fraction *'
  WRITE(ILUOUT,FMT=*) &
  '*********************************************************************'
  NSIZE(:) = 1
  WHERE (XNATURE(:).NE.0. .AND. ZNATURE(:).EQ.0.) NSIZE(:)=0
  DO JL=1,SIZE(XCOVER,1)
    IF (XNATURE(JL).EQ.0.) NSIZE(JL)=-1
  ENDDO
  ZDEF(:)=0.
  ZDEF(4)=1.  ! if not enough covers are present, cover 4 assumed
  CALL INTERPOL_FIELD2D(HPROGRAM,ILUOUT,NSIZE,ZCOVER_NATURE(:,:),YFIELD,ZDEF)  

  WRITE(ILUOUT,FMT=*) &
  '*********************************************************************'
  WRITE(ILUOUT,FMT=*) &
  '*  Coherence computation between covers and imposed town   fraction *'
  WRITE(ILUOUT,FMT=*) &
  '*********************************************************************'
  NSIZE(:) = 1
  WHERE (XTOWN(:).NE.0. .AND. ZTOWN(:).EQ.0.) NSIZE(:)=0
  DO JL=1,SIZE(XCOVER,1)
    IF (XTOWN(JL).EQ.0.) NSIZE(JL)=-1
  ENDDO
  ZDEF(:)=0.
  ZDEF(7)=1.  ! if not enough covers are present, cover 7 assumed
  CALL INTERPOL_FIELD2D(HPROGRAM,ILUOUT,NSIZE,ZCOVER_TOWN (:,:),YFIELD,ZDEF)  

  WRITE(ILUOUT,FMT=*) &
  '*********************************************************************'
  WRITE(ILUOUT,FMT=*) &
  '*  Coherence computation between covers and imposed water  fraction *'
  WRITE(ILUOUT,FMT=*) &
  '*********************************************************************'
  NSIZE(:) = 1
  WHERE (XWATER(:).NE.0. .AND. ZWATER(:).EQ.0.) NSIZE(:)=0
! if water imposed to 1 in a grid cell: no extrapolation          
  DO JL=1,SIZE(XCOVER,1)
     IF(XWATER(JL)==1.0)THEN
        ZCOVER_WATER(JL,1)=0.0
        ZCOVER_WATER(JL,2)=1.0
        ZCOVER_WATER(JL,3:JPCOVER)=0.0
        NSIZE(JL)=1
     ELSEIF(XWATER(JL)==0.0)THEN
        NSIZE(JL)=-1
     ENDIF
  ENDDO
  ZDEF(:)=0.
  ZDEF(2)=1.  ! if not enough covers are present, cover 002 assumed
  CALL INTERPOL_FIELD2D(HPROGRAM,ILUOUT,NSIZE,ZCOVER_WATER (:,:),YFIELD,PDEF=ZDEF)

  WRITE(ILUOUT,FMT=*) &
  '*********************************************************************'
  WRITE(ILUOUT,FMT=*) &
  '*  Coherence computation between covers and imposed sea    fraction *'
  WRITE(ILUOUT,FMT=*) &
  '*********************************************************************'
  NSIZE(:) = 1
  WHERE (XSEA(:).NE.0. .AND. ZSEA(:).EQ.0.) NSIZE(:)=0
! if sea imposed to 1 in a grid cell: no extrapolation          
  DO JL=1,SIZE(XCOVER,1)
     IF(XSEA(JL)==1.0)THEN
        ZCOVER_SEA(JL,1)=1.0
        ZCOVER_SEA(JL,2:JPCOVER)=0.0
        NSIZE(JL)=1
     ELSEIF(XSEA(JL)==0.0)THEN
        NSIZE(JL)=-1
     ENDIF
  ENDDO
  ZDEF(:)=0.
  ZDEF(1)=1.  ! if not enough covers are present, cover 001 assumed
  CALL INTERPOL_FIELD2D(HPROGRAM,ILUOUT,NSIZE,ZCOVER_SEA (:,:),YFIELD,PDEF=ZDEF)
  !
  XCOVER(:,:) = XCOVER(:,:) + 0.001 * ( ZCOVER_NATURE(:,:) + ZCOVER_TOWN(:,:) + &
                                        ZCOVER_WATER (:,:) + ZCOVER_SEA (:,:) )
  !
  XCOVER(:,:)=XCOVER(:,:)/SPREAD(SUM(XCOVER(:,:),2),2,JPCOVER)
  !
  DEALLOCATE(ZCOVER_NATURE)
  DEALLOCATE(ZCOVER_TOWN  )
  DEALLOCATE(ZCOVER_WATER )
  DEALLOCATE(ZCOVER_SEA   )
  !
  DEALLOCATE(NSIZE    )
  DEALLOCATE(ZSEA     )
  DEALLOCATE(ZWATER   )
  DEALLOCATE(ZNATURE  )
  DEALLOCATE(ZTOWN    )
  !
ENDIF
!
NSIZE_NATURE    = COUNT(XNATURE(:) > 0.0)
NSIZE_WATER     = COUNT(XWATER (:) > 0.0)
NSIZE_SEA       = COUNT(XSEA   (:) > 0.0)
NSIZE_TOWN      = COUNT(XTOWN  (:) > 0.0)
NSIZE_FULL      = NL
!
NDIM_NATURE    = SUM_ON_ALL_PROCS(HPROGRAM,CGRID,XNATURE(:) > 0., 'DIM')
NDIM_WATER     = SUM_ON_ALL_PROCS(HPROGRAM,CGRID,XWATER (:) > 0., 'DIM')
NDIM_SEA       = SUM_ON_ALL_PROCS(HPROGRAM,CGRID,XSEA   (:) > 0., 'DIM')
NDIM_TOWN      = SUM_ON_ALL_PROCS(HPROGRAM,CGRID,XTOWN  (:) > 0., 'DIM')
!
!*    8.      List of cover present
!             ---------------------
!
ALLOCATE(LCOVER(JPCOVER))
LCOVER = .FALSE.
DO JCOVER=1,JPCOVER
  ICOVER = SUM_ON_ALL_PROCS(HPROGRAM,CGRID,XCOVER(:,JCOVER)/=0., 'COV')
  IF (ICOVER>0) LCOVER(JCOVER)=.TRUE. 
END DO
!
IF (LHOOK) CALL DR_HOOK('PGD_COVER',1,ZHOOK_HANDLE)
!-------------------------------------------------------------------------------
!
END SUBROUTINE PGD_COVER
