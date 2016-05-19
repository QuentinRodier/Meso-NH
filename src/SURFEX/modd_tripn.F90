!###############
MODULE MODD_TRIP_n
!###############
!
!!****  *MODD_TRIP_n - declaration of surface variable for TRIP RRM
!!
!!    PURPOSE
!!    -------
!
!!
!!**  IMPLICIT ARGUMENTS
!!    ------------------
!!      None 
!!
!!    REFERENCE
!!    ---------
!!
!!    AUTHOR
!!    ------
!!	B. Decharme   *Meteo France*
!!
!!    MODIFICATIONS
!!    -------------
!!      Original       21/05/08
!
!*       0.   DECLARATIONS
!             ------------
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE

TYPE TRIP_t
!-------------------------------------------------------------------------------
!
! TRIP Options:
!
 CHARACTER(LEN=3)               :: CGROUNDW    !Use groundwater scheme
                                              !'DEF' = No groundwater scheme
                                              !'CST' = Constant transfert time
                                              !'VAR' = Textural dependence of transfert time 
!
 CHARACTER(LEN=3)               :: CVIT        !Type of stream flow velocity
                                              !'DEF' = Constant velocit = 0.5m/s
                                              !'VAR' = variable velocity
!
LOGICAL                        :: LDIAG_CPL        !if true, dailly output diag
LOGICAL                        :: LTRIP_DIAG_MISC  !if true, more diag for model testing
LOGICAL                        :: LFLOODT          !if true, use TRIP-FLOOD
LOGICAL                        :: LNCPRINT         !Netcdf read/write messages
LOGICAL                        :: LPRINT_TRIP      !Print water budget message
!
REAL                           :: XDATA_TAUG   ! Constant transfert time value
REAL                           :: XCVEL        ! Constant velocity value
REAL                           :: XRATMED      ! Meandering ratio
REAL                           :: XTRIP_TSTEP
REAL                           :: XTSTEP_COUPLING
!
!-------------------------------------------------------------------------------
!
INTEGER, POINTER, DIMENSION(:,:) :: IGRCN       ! Flow direction (1->8)
INTEGER, POINTER, DIMENSION(:,:) :: ISEQ        ! River sequence
INTEGER                          :: ISEQMAX     ! maximum down flow
INTEGER, POINTER, DIMENSION(:,:) :: INEXTX      ! returns x and y point 
INTEGER, POINTER, DIMENSION(:,:) :: INEXTY      ! of destination grid:
!                                                        8 1 2
!                                                        7   3
!                                                        6 5 4
!-------------------------------------------------------------------------------
!
INTEGER                          :: NRUN        ! Number of coupling during a run
!
!-------------------------------------------------------------------------------
!
! Input river geometry Parameters :
!
REAL, POINTER, DIMENSION(:,:) :: XLEN           ! distance between grids       [m]
REAL, POINTER, DIMENSION(:,:) :: XTAUG          ! ground water transfer time   [s]
REAL, POINTER, DIMENSION(:,:) :: XSLOPEBED      ! river bed slopes             [m/m]
REAL, POINTER, DIMENSION(:,:) :: XWIDTH         ! river widths                 [m]
REAL, POINTER, DIMENSION(:,:) :: XN             ! Manning roughness coeficient [-] (0.03 to 0.065)
REAL, POINTER, DIMENSION(:,:) :: XN_FLOOD       ! Manning coeficient over floodplains  [-] (currently 0.1)
REAL, POINTER, DIMENSION(:,:) :: XHC_BED        ! River bed depth              [m]
!
!-------------------------------------------------------------------------------
!
! Time varing variables :
!
REAL, POINTER, DIMENSION(:,:) :: XSURF_STO        ! river channel storage        [kg]
REAL, POINTER, DIMENSION(:,:) :: XGROUND_STO      ! ground water storage         [kg]
REAL, POINTER, DIMENSION(:,:) :: XFLOOD_STO       ! Floodplain water storage     [kg]
REAL, POINTER, DIMENSION(:,:) :: XHFLOOD          ! Floodplain water depth       [m]
REAL, POINTER, DIMENSION(:,:) :: XFFLOOD          ! Floodplain grid-cell fraction [-]
REAL, POINTER, DIMENSION(:,:) :: XPIFLOOD         ! Floodplain potential infiltration for coupling with isba [kg/m2/s]
REAL, POINTER, DIMENSION(:,:) :: XWFLOOD          ! Floodplain width             [m]
REAL, POINTER, DIMENSION(:,:) :: XFLOOD_LEN       ! Floodplain lenght            [m]
!
!-------------------------------------------------------------------------------
!
! Floodplain fonctions :
!
INTEGER, POINTER, DIMENSION(:,:) :: ITABMAX       ! Maximum array
!
REAL, POINTER, DIMENSION(:,:,:) :: XTAB_F         ! Flood fraction array
REAL, POINTER, DIMENSION(:,:,:) :: XTAB_H         ! Topo height array
REAL, POINTER, DIMENSION(:,:,:) :: XTAB_VF        ! Flood volume array
!
!-------------------------------------------------------------------------------
!
END TYPE TRIP_t
!
TYPE(TRIP_t), ALLOCATABLE, TARGET, SAVE :: TRIP_MODEL(:)
!
 CHARACTER(LEN=3), POINTER        :: CGROUNDW=>NULL()
!$OMP THREADPRIVATE(CGROUNDW)
 CHARACTER(LEN=3), POINTER        :: CVIT=>NULL()
!$OMP THREADPRIVATE(CVIT)
!
LOGICAL, POINTER                 :: LFLOODT=>NULL()
!$OMP THREADPRIVATE(LFLOODT)
LOGICAL, POINTER                 :: LDIAG_CPL=>NULL()
!$OMP THREADPRIVATE(LDIAG_CPL)
LOGICAL, POINTER                 :: LNCPRINT=>NULL()
!$OMP THREADPRIVATE(LNCPRINT)
LOGICAL, POINTER                 :: LPRINT_TRIP=>NULL()
!$OMP THREADPRIVATE(LPRINT_TRIP)
LOGICAL, POINTER                 :: LTRIP_DIAG_MISC=>NULL()
!$OMP THREADPRIVATE(LTRIP_DIAG_MISC)
REAL, POINTER                    :: XTSTEP_COUPLING=>NULL()
!$OMP THREADPRIVATE(XTSTEP_COUPLING)
REAL, POINTER                    :: XTRIP_TSTEP=>NULL()
!$OMP THREADPRIVATE(XTRIP_TSTEP)
REAL, POINTER                    :: XDATA_TAUG=>NULL()
!$OMP THREADPRIVATE(XDATA_TAUG)
REAL, POINTER                    :: XCVEL=>NULL()
!$OMP THREADPRIVATE(XCVEL)
REAL, POINTER                    :: XRATMED=>NULL()
!$OMP THREADPRIVATE(XRATMED)
!
INTEGER, POINTER, DIMENSION(:,:) :: IGRCN=>NULL()       
!$OMP THREADPRIVATE(IGRCN)
INTEGER, POINTER, DIMENSION(:,:) :: ISEQ=>NULL()        
!$OMP THREADPRIVATE(ISEQ)
INTEGER, POINTER                 :: ISEQMAX=>NULL() 
!$OMP THREADPRIVATE(ISEQMAX)
INTEGER, POINTER, DIMENSION(:,:) :: INEXTX=>NULL()       
!$OMP THREADPRIVATE(INEXTX)
INTEGER, POINTER, DIMENSION(:,:) :: INEXTY=>NULL() 
!$OMP THREADPRIVATE(INEXTY)
INTEGER, POINTER                 :: NRUN=>NULL()
!$OMP THREADPRIVATE(NRUN)
!
REAL, POINTER, DIMENSION(:,:)    :: XLEN=>NULL()           
!$OMP THREADPRIVATE(XLEN)
REAL, POINTER, DIMENSION(:,:)    :: XTAUG=>NULL()          
!$OMP THREADPRIVATE(XTAUG)
REAL, POINTER, DIMENSION(:,:)    :: XSLOPEBED=>NULL()      
!$OMP THREADPRIVATE(XSLOPEBED)
REAL, POINTER, DIMENSION(:,:)    :: XWIDTH=>NULL()         
!$OMP THREADPRIVATE(XWIDTH)
REAL, POINTER, DIMENSION(:,:)    :: XN=>NULL()             
!$OMP THREADPRIVATE(XN)
REAL, POINTER, DIMENSION(:,:)    :: XN_FLOOD=>NULL()       
!$OMP THREADPRIVATE(XN_FLOOD)
REAL, POINTER, DIMENSION(:,:)    :: XHC_BED=>NULL()
!$OMP THREADPRIVATE(XHC_BED)
REAL, POINTER, DIMENSION(:,:)    :: XSURF_STO=>NULL()        
!$OMP THREADPRIVATE(XSURF_STO)
REAL, POINTER, DIMENSION(:,:)    :: XGROUND_STO=>NULL()      
!$OMP THREADPRIVATE(XGROUND_STO)
REAL, POINTER, DIMENSION(:,:)    :: XFLOOD_STO=>NULL()       
!$OMP THREADPRIVATE(XFLOOD_STO)
REAL, POINTER, DIMENSION(:,:)    :: XHFLOOD=>NULL()          
!$OMP THREADPRIVATE(XHFLOOD)
REAL, POINTER, DIMENSION(:,:)    :: XFFLOOD=>NULL() 
!$OMP THREADPRIVATE(XFFLOOD)
REAL, POINTER, DIMENSION(:,:)    :: XPIFLOOD=>NULL()
!$OMP THREADPRIVATE(XPIFLOOD)
REAL, POINTER, DIMENSION(:,:)    :: XWFLOOD=>NULL()          
!$OMP THREADPRIVATE(XWFLOOD)
REAL, POINTER, DIMENSION(:,:)    :: XFLOOD_LEN=>NULL() 
!$OMP THREADPRIVATE(XFLOOD_LEN)
!
INTEGER, POINTER, DIMENSION(:,:)   :: ITABMAX=>NULL()
!$OMP THREADPRIVATE(ITABMAX)
REAL, POINTER, DIMENSION(:,:,:)    :: XTAB_F=>NULL()         
!$OMP THREADPRIVATE(XTAB_F)
REAL, POINTER, DIMENSION(:,:,:)    :: XTAB_H=>NULL() 
!$OMP THREADPRIVATE(XTAB_H)
REAL, POINTER, DIMENSION(:,:,:)    :: XTAB_VF=>NULL() 
!$OMP THREADPRIVATE(XTAB_VF)
!
CONTAINS
!
SUBROUTINE TRIP_GOTO_MODEL(KFROM, KTO, LKFROM)
LOGICAL, INTENT(IN) :: LKFROM
INTEGER, INTENT(IN) :: KFROM, KTO
!
! Save current state for allocated arrays
IF (LKFROM) THEN
TRIP_MODEL(KFROM)%IGRCN=>IGRCN
TRIP_MODEL(KFROM)%ISEQ=>ISEQ
TRIP_MODEL(KFROM)%INEXTX=>INEXTX
TRIP_MODEL(KFROM)%INEXTY=>INEXTY
TRIP_MODEL(KFROM)%XLEN=>XLEN
TRIP_MODEL(KFROM)%XTAUG=>XTAUG
TRIP_MODEL(KFROM)%XSLOPEBED=>XSLOPEBED
TRIP_MODEL(KFROM)%XWIDTH=>XWIDTH
TRIP_MODEL(KFROM)%XN=>XN
TRIP_MODEL(KFROM)%XN_FLOOD=>XN_FLOOD
TRIP_MODEL(KFROM)%XHC_BED=>XHC_BED
TRIP_MODEL(KFROM)%XSURF_STO=>XSURF_STO
TRIP_MODEL(KFROM)%XGROUND_STO=>XGROUND_STO
TRIP_MODEL(KFROM)%XFLOOD_STO=>XFLOOD_STO
TRIP_MODEL(KFROM)%XHFLOOD=>XHFLOOD
TRIP_MODEL(KFROM)%XFFLOOD=>XFFLOOD
TRIP_MODEL(KFROM)%XPIFLOOD=>XPIFLOOD
TRIP_MODEL(KFROM)%XWFLOOD=>XWFLOOD
TRIP_MODEL(KFROM)%XFLOOD_LEN=>XFLOOD_LEN
TRIP_MODEL(KFROM)%XTAB_F=>XTAB_F
TRIP_MODEL(KFROM)%XTAB_H=>XTAB_H
TRIP_MODEL(KFROM)%XTAB_VF=>XTAB_VF
TRIP_MODEL(KFROM)%ITABMAX=>ITABMAX
ENDIF
! Current model is set to model KTO
CGROUNDW=>TRIP_MODEL(KTO)%CGROUNDW
CVIT=>TRIP_MODEL(KTO)%CVIT
LFLOODT=>TRIP_MODEL(KTO)%LFLOODT
LTRIP_DIAG_MISC=>TRIP_MODEL(KTO)%LTRIP_DIAG_MISC
LDIAG_CPL=>TRIP_MODEL(KTO)%LDIAG_CPL
LNCPRINT=>TRIP_MODEL(KTO)%LNCPRINT
LPRINT_TRIP=>TRIP_MODEL(KTO)%LPRINT_TRIP
XTSTEP_COUPLING=>TRIP_MODEL(KTO)%XTSTEP_COUPLING
XTRIP_TSTEP=>TRIP_MODEL(KTO)%XTRIP_TSTEP
XDATA_TAUG=>TRIP_MODEL(KTO)%XDATA_TAUG
XCVEL=>TRIP_MODEL(KTO)%XCVEL
XRATMED=>TRIP_MODEL(KTO)%XRATMED
NRUN=>TRIP_MODEL(KTO)%NRUN
IGRCN=>TRIP_MODEL(KTO)%IGRCN
ISEQ=>TRIP_MODEL(KTO)%ISEQ
ISEQMAX=>TRIP_MODEL(KTO)%ISEQMAX
INEXTX=>TRIP_MODEL(KTO)%INEXTX
INEXTY=>TRIP_MODEL(KTO)%INEXTY
XLEN=>TRIP_MODEL(KTO)%XLEN
XTAUG=>TRIP_MODEL(KTO)%XTAUG
XSLOPEBED=>TRIP_MODEL(KTO)%XSLOPEBED
XWIDTH=>TRIP_MODEL(KTO)%XWIDTH
XN=>TRIP_MODEL(KTO)%XN
XN_FLOOD=>TRIP_MODEL(KTO)%XN_FLOOD
XHC_BED=>TRIP_MODEL(KTO)%XHC_BED
XSURF_STO=>TRIP_MODEL(KTO)%XSURF_STO
XGROUND_STO=>TRIP_MODEL(KTO)%XGROUND_STO
XFLOOD_STO=>TRIP_MODEL(KTO)%XFLOOD_STO
XHFLOOD=>TRIP_MODEL(KTO)%XHFLOOD
XFFLOOD=>TRIP_MODEL(KTO)%XFFLOOD
XPIFLOOD=>TRIP_MODEL(KTO)%XPIFLOOD
XWFLOOD=>TRIP_MODEL(KTO)%XWFLOOD
XFLOOD_LEN=>TRIP_MODEL(KTO)%XFLOOD_LEN
XTAB_F=>TRIP_MODEL(KTO)%XTAB_F
XTAB_H=>TRIP_MODEL(KTO)%XTAB_H
XTAB_VF=>TRIP_MODEL(KTO)%XTAB_VF
ITABMAX=>TRIP_MODEL(KTO)%ITABMAX
END SUBROUTINE TRIP_GOTO_MODEL

SUBROUTINE TRIP_ALLOC(KMODEL)
INTEGER, INTENT(IN) :: KMODEL
INTEGER :: J
REAL(KIND=JPRB) :: ZHOOK_HANDLE
IF (LHOOK) CALL DR_HOOK("MODD_TRIP_N:TRIP_ALLOC",0,ZHOOK_HANDLE)
ALLOCATE(TRIP_MODEL(KMODEL))
DO J=1,KMODEL
  NULLIFY(TRIP_MODEL(J)%IGRCN)
  NULLIFY(TRIP_MODEL(J)%ISEQ)
  NULLIFY(TRIP_MODEL(J)%INEXTX)
  NULLIFY(TRIP_MODEL(J)%INEXTY)
  NULLIFY(TRIP_MODEL(J)%XLEN)
  NULLIFY(TRIP_MODEL(J)%XTAUG)
  NULLIFY(TRIP_MODEL(J)%XSLOPEBED)
  NULLIFY(TRIP_MODEL(J)%XWIDTH)
  NULLIFY(TRIP_MODEL(J)%XN)
  NULLIFY(TRIP_MODEL(J)%XN_FLOOD)
  NULLIFY(TRIP_MODEL(J)%XHC_BED)
  NULLIFY(TRIP_MODEL(J)%XSURF_STO)
  NULLIFY(TRIP_MODEL(J)%XGROUND_STO)
  NULLIFY(TRIP_MODEL(J)%XFLOOD_STO)
  NULLIFY(TRIP_MODEL(J)%XHFLOOD)
  NULLIFY(TRIP_MODEL(J)%XFFLOOD)
  NULLIFY(TRIP_MODEL(J)%XPIFLOOD)  
  NULLIFY(TRIP_MODEL(J)%XWFLOOD)
  NULLIFY(TRIP_MODEL(J)%XFLOOD_LEN)
  NULLIFY(TRIP_MODEL(J)%XTAB_F)
  NULLIFY(TRIP_MODEL(J)%XTAB_H)
  NULLIFY(TRIP_MODEL(J)%XTAB_VF)
  NULLIFY(TRIP_MODEL(J)%ITABMAX)
ENDDO
TRIP_MODEL(:)%CGROUNDW=' '
TRIP_MODEL(:)%CVIT=' '
TRIP_MODEL(:)%LDIAG_CPL=.FALSE.
TRIP_MODEL(:)%LTRIP_DIAG_MISC=.FALSE.
TRIP_MODEL(:)%LFLOODT=.FALSE.
TRIP_MODEL(:)%LNCPRINT=.FALSE.
TRIP_MODEL(:)%LPRINT_TRIP=.FALSE.
TRIP_MODEL(:)%XDATA_TAUG=0.
TRIP_MODEL(:)%XCVEL=0.
TRIP_MODEL(:)%XRATMED=0.
TRIP_MODEL(:)%XTRIP_TSTEP=0.
TRIP_MODEL(:)%XTSTEP_COUPLING=0.
TRIP_MODEL(:)%ISEQMAX=0
TRIP_MODEL(:)%NRUN=0
IF (LHOOK) CALL DR_HOOK("MODD_TRIP_N:TRIP_ALLOC",1,ZHOOK_HANDLE)
END SUBROUTINE TRIP_ALLOC

SUBROUTINE TRIP_DEALLO
REAL(KIND=JPRB) :: ZHOOK_HANDLE
IF (LHOOK) CALL DR_HOOK("MODD_TRIP_N:TRIP_DEALLO",0,ZHOOK_HANDLE)
IF (ALLOCATED(TRIP_MODEL)) DEALLOCATE(TRIP_MODEL)
IF (LHOOK) CALL DR_HOOK("MODD_TRIP_N:TRIP_DEALLO",1,ZHOOK_HANDLE)
END SUBROUTINE TRIP_DEALLO
!
END MODULE MODD_TRIP_n
