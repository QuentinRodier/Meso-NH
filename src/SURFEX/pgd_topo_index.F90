!SURFEX_LIC Copyright 1994-2014 Meteo-France 
!SURFEX_LIC This is part of the SURFEX software governed by the CeCILL-C  licence
!SURFEX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!SURFEX_LIC for details. version 1.
!     #########
      SUBROUTINE PGD_TOPO_INDEX(HPROGRAM,KLU,HCTI,HCTIFILETYPE,OIMP_CTI)
!     ##################################################################
!
!!**** *PGD_TOPO_INDEX* monitor for computing topographic index statistics used by TOIPMODEL
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
!!    B. Decharme        Meteo-France
!!
!!    MODIFICATION
!!    ------------
!!
!!    Original    06/2009
!!
!----------------------------------------------------------------------------
!
!*    0.     DECLARATION
!            -----------
!
USE MODD_PGD_GRID,       ONLY : NL
!
USE MODD_SURF_ATM_n,     ONLY : XNATURE
!
USE MODD_PGDWORK,        ONLY : XSUMVAL, XSUMVAL2, NSIZE, &
                                  XMIN_WORK, XMAX_WORK,     &
                                  XMEAN_WORK, XSTD_WORK,    &
                                  XSKEW_WORK, XSUMVAL3  
!
USE MODD_SURF_PAR,       ONLY : XUNDEF
!
USE MODD_ISBA_n,         ONLY : LCTI,XTI_MIN,XTI_MAX,XTI_MEAN,XTI_STD,XTI_SKEW
!
USE MODI_GET_LUOUT
USE MODI_GET_GRID_COORD
USE MODI_READ_SURF
USE MODI_TREAT_FIELD
USE MODI_PACK_SAME_RANK
USE MODI_INTERPOL_FIELD
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
USE MODI_GET_SURF_MASK_n
!
USE MODI_GET_TYPE_DIM_n
!
IMPLICIT NONE
!
!*    0.1    Declaration of arguments
!            ------------------------
!
 CHARACTER(LEN=6),     INTENT(IN)  :: HPROGRAM     ! program calling
INTEGER,              INTENT(IN)  :: KLU          ! number of nature points
 CHARACTER(LEN=28),    INTENT(IN)  :: HCTI         ! topographic index file name
 CHARACTER(LEN=6),     INTENT(IN)  :: HCTIFILETYPE ! topographic index file type
LOGICAL,              INTENT(IN)  :: OIMP_CTI     ! .true. if topographic index statistics is imposed
!
!
!*    0.2    Declaration of local variables
!            ------------------------------
!
REAL, DIMENSION(:), ALLOCATABLE :: ZLAT
!
INTEGER, DIMENSION(:), ALLOCATABLE :: IMASK
!
INTEGER :: IFULL     ! total number of points
INTEGER :: I_DIM
INTEGER :: IRET      ! error code
INTEGER :: ILUOUT    ! output listing logical unit
!
 CHARACTER(LEN=6  ) :: YFILETYPE, YSCHEME, YSUBROUTINE
 CHARACTER(LEN=20)  :: YFIELD        ! Name of the field.
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
!
!*    1.      Initializations
!             ---------------
!
IF (LHOOK) CALL DR_HOOK('PGD_TOPO_INDEX',0,ZHOOK_HANDLE)
IF(LEN_TRIM(HCTI)==0)THEN
!
  ALLOCATE(XTI_MIN (0))
  ALLOCATE(XTI_MAX (0))
  ALLOCATE(XTI_MEAN(0))
  ALLOCATE(XTI_STD (0))
  ALLOCATE(XTI_SKEW(0))
!        
!-------------------------------------------------------------------------------
ELSE
!-------------------------------------------------------------------------------
!
  LCTI = .TRUE.
!
!*    2.      Find LUOUT
!             ----------
!
  CALL GET_LUOUT(HPROGRAM,ILUOUT)
!
  WRITE(ILUOUT,*) '*****************************************'
  WRITE(ILUOUT,*) 'Comput Topographic indexes for TOPMODEL  '
  WRITE(ILUOUT,*) '*****************************************'
!
!*    3.      Allocations of statistics arrays
!             --------------------------------
!
  ALLOCATE(XTI_MIN (KLU))
  ALLOCATE(XTI_MAX (KLU))
  ALLOCATE(XTI_MEAN(KLU))
  ALLOCATE(XTI_STD (KLU))
  ALLOCATE(XTI_SKEW(KLU))
!
  XTI_MIN (:) = XUNDEF
  XTI_MAX (:) = XUNDEF
  XTI_MEAN (:) = XUNDEF
  XTI_STD (:) = XUNDEF
  XTI_SKEW(:) = XUNDEF
!
!-------------------------------------------------------------------------------
!
!*    4.      Allocations of work arrays
!             --------------------------
!
  CALL GET_TYPE_DIM_n('NATURE',I_DIM)
  IF (I_DIM/=KLU) THEN
     WRITE(ILUOUT,*)'PGD_TOPO_INDEX: Wrong dimension of MASK: ',I_DIM,KLU
     CALL ABOR1_SFX('PGD_TOPO_INDEX: WRONG DIMENSION OF MASK')
  ENDIF
!
  ALLOCATE(IMASK(KLU))
  IFULL=0
  CALL GET_SURF_MASK_n('NATURE',KLU,IMASK,IFULL,ILUOUT)
  IF (IFULL/=NL) THEN
     WRITE(ILUOUT,*)'PGD_TOPO_INDEX: Wrong dimension of FULL: ',IFULL,NL
     CALL ABOR1_SFX('PGD_TOPO_INDEX: WRONG DIMENSION OF FULL')
  ENDIF
!
  ALLOCATE(XMIN_WORK  (IFULL))
  ALLOCATE(XMAX_WORK  (IFULL))
  ALLOCATE(XMEAN_WORK (IFULL))
  ALLOCATE(XSTD_WORK  (IFULL))
  ALLOCATE(XSKEW_WORK (IFULL))
!
  XMIN_WORK (:)=XUNDEF
  XMAX_WORK (:)=XUNDEF
  XMEAN_WORK(:)=XUNDEF
  XSTD_WORK (:)=XUNDEF
  XSKEW_WORK(:)=XUNDEF
!
!-------------------------------------------------------------------------------
!
!*    5.      Use of imposed field
!             --------------------
!
  IF (OIMP_CTI) THEN
!
     YFILETYPE=HCTIFILETYPE
     IF(HCTIFILETYPE=='NETCDF')THEN
        CALL ABOR1_SFX('Use another format than netcdf for cti input file with LIMP_CTI')
     ELSE
#ifdef ASC
       CFILEIN     = ADJUSTL(ADJUSTR(HCTI)//'.txt')
#endif
#ifdef FA
       CFILEIN_FA  = ADJUSTL(ADJUSTR(HCTI)//'.fa')
#endif
#ifdef LFI
       CFILEIN_LFI = ADJUSTL(HCTI)
#endif
       CALL INIT_IO_SURF_n(YFILETYPE,'FULL  ','SURF  ','READ ')
     ENDIF     
!   
     CALL READ_SURF(YFILETYPE,'TI_MIN' ,XMIN_WORK ,IRET) 
     CALL READ_SURF(YFILETYPE,'TI_MAX' ,XMAX_WORK ,IRET)
     CALL READ_SURF(YFILETYPE,'TI_MEAN',XMEAN_WORK,IRET)
     CALL READ_SURF(YFILETYPE,'TI_STD' ,XSTD_WORK ,IRET) 
     CALL READ_SURF(YFILETYPE,'TI_SKEW',XSKEW_WORK,IRET) 
!
     CALL END_IO_SURF_n(YFILETYPE)
!
  ELSE
!
!-------------------------------------------------------------------------------
!
!*    6.      Use of cti file
!             ---------------
!
     ALLOCATE(NSIZE   (IFULL))
     ALLOCATE(XSUMVAL (IFULL))
     ALLOCATE(XSUMVAL2(IFULL))
     ALLOCATE(XSUMVAL3(IFULL))
!
     NSIZE    (:) = 0.
     XSUMVAL  (:) = 0.
     XSUMVAL2 (:) = 0.
     XSUMVAL3 (:) = 0.
!
     XMAX_WORK(:) =-99999.
!
     YFIELD      = 'CTI'
     YSCHEME     = 'SURF  '
     YSUBROUTINE = 'A_CTI '
     CALL TREAT_FIELD(HPROGRAM,YSCHEME,HCTIFILETYPE,YSUBROUTINE,HCTI,YFIELD)
!
!-------------------------------------------------------------------------------
!
!*    7.      Coherence
!             ---------
!
     WHERE(NSIZE(:)<36.OR.XSTD_WORK(:)==0.0)
          XMIN_WORK (:) = XUNDEF
          XMAX_WORK (:) = XUNDEF
          XMEAN_WORK(:) = XUNDEF
          XSTD_WORK (:) = XUNDEF
          XSKEW_WORK(:) = XUNDEF
          NSIZE     (:) = 0
     ENDWHERE 
!
     WHERE(XNATURE(:)>0.0.AND.XSKEW_WORK(:)<=-8.0)
          XMIN_WORK (:) = XUNDEF
          XMAX_WORK (:) = XUNDEF
          XMEAN_WORK(:) = XUNDEF
          XSTD_WORK (:) = XUNDEF
          XSKEW_WORK(:) = XUNDEF
          NSIZE     (:) = 0
     ENDWHERE             
!
     WHERE(XNATURE(:)==0.)
          XMIN_WORK (:) = XUNDEF
          XMAX_WORK (:) = XUNDEF
          XMEAN_WORK(:) = XUNDEF
          XSTD_WORK (:) = XUNDEF
          XSKEW_WORK(:) = XUNDEF
          NSIZE     (:) = 0
     ENDWHERE   
!
!-------------------------------------------------------------------------------
!
!*    5.      Interpolation if some points are not initialized (no data for these points)
!             ------------------------------------------------
!
    WRITE(ILUOUT,*) '*********************************************'
    WRITE(ILUOUT,*) 'Interpolation if some index not initialized  '
    WRITE(ILUOUT,*) '*********************************************'
!
    ALLOCATE(ZLAT(NL))
    CALL GET_GRID_COORD(ILUOUT,PY=ZLAT)
!
    WHERE (XNATURE(:)==0..AND.NSIZE(:)==0) NSIZE(:) = -1
!
!   No Antarctic
    WHERE(XNATURE(:)>0..AND.ZLAT(:)<-60.)
          XMIN_WORK (:) = XUNDEF
          XMAX_WORK (:) = XUNDEF
          XMEAN_WORK(:) = XUNDEF
          XSTD_WORK (:) = XUNDEF
          XSKEW_WORK(:) = XUNDEF
          NSIZE     (:) = -1
    ENDWHERE   
!
    CALL INTERPOL_FIELD(HPROGRAM,ILUOUT,NSIZE,XMIN_WORK (:),'TI_MIN ',PDEF=XUNDEF,KNPTS=1)
    CALL INTERPOL_FIELD(HPROGRAM,ILUOUT,NSIZE,XMAX_WORK (:),'TI_MAX ',PDEF=XUNDEF,KNPTS=1)
    CALL INTERPOL_FIELD(HPROGRAM,ILUOUT,NSIZE,XMEAN_WORK(:),'TI_MEAN',PDEF=XUNDEF,KNPTS=1)
    CALL INTERPOL_FIELD(HPROGRAM,ILUOUT,NSIZE,XSTD_WORK (:),'TI_STD ',PDEF=XUNDEF,KNPTS=1)
    CALL INTERPOL_FIELD(HPROGRAM,ILUOUT,NSIZE,XSKEW_WORK(:),'TI_SKEW ',PDEF=XUNDEF,KNPTS=1)
!
    DEALLOCATE(NSIZE     )
    DEALLOCATE(XSUMVAL   )
    DEALLOCATE(XSUMVAL2  )
    DEALLOCATE(XSUMVAL3  )
    DEALLOCATE(ZLAT      )
!
  ENDIF
!-------------------------------------------------------------------------------
!
!*    8.      Asign parameters
!             ----------------
!
  CALL PACK_SAME_RANK(IMASK,XMIN_WORK ,XTI_MIN)
  CALL PACK_SAME_RANK(IMASK,XMAX_WORK ,XTI_MAX)
  CALL PACK_SAME_RANK(IMASK,XMEAN_WORK,XTI_MEAN)
  CALL PACK_SAME_RANK(IMASK,XSTD_WORK ,XTI_STD)
  CALL PACK_SAME_RANK(IMASK,XSKEW_WORK,XTI_SKEW)
!
  WRITE(ILUOUT,*) '******************************'
  WRITE(ILUOUT,*) 'End Comput Topographic indexes'
  WRITE(ILUOUT,*) '******************************'
!
!-------------------------------------------------------------------------------
!
  DEALLOCATE(XMIN_WORK )
  DEALLOCATE(XMAX_WORK )
  DEALLOCATE(XMEAN_WORK)
  DEALLOCATE(XSTD_WORK )
  DEALLOCATE(XSKEW_WORK)
!
!-------------------------------------------------------------------------------
ENDIF
IF (LHOOK) CALL DR_HOOK('PGD_TOPO_INDEX',1,ZHOOK_HANDLE)
!-------------------------------------------------------------------------------
!
END SUBROUTINE PGD_TOPO_INDEX
