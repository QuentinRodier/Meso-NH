!     #########
      SUBROUTINE INIT_IO_SURF_ASC_n(HMASK,HACTION)
!     ######################
!
!!****  *INIT_IO_SURF_ASC* Keep in memory the output files
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
!!	V. Masson   *Meteo France*
!!
!!    MODIFICATIONS
!!    -------------
!!
!!      P. Le Moigne 04/2004: distinguish in and out file name
!!      P. Le Moigne 04/2006: special HACTION='GTMSK' to initialize
!!                            a mask different of 'FULL ' in order 
!!                            to read dimensions only.
!!      S. Faroux 06/2012 : implementations for MPI
!
!*       0.   DECLARATIONS
!             ------------
!
USE MODD_SURFEX_MPI, ONLY : NRANK, NINDEX, NSIZE, NPIO
!
USE MODD_IO_SURF_ASC,ONLY: NUNIT,CFILEIN,CFILEOUT,NMASK,NLUOUT,NFULL,CMASK, &
                           LOPEN, LCREATED
!
USE MODI_GET_LUOUT
USE MODI_READ_SURF
USE MODI_GET_DIM_FULL_n
USE MODI_GET_SIZE_FULL_n
USE MODI_GET_TYPE_DIM_n
USE MODI_INIT_IO_SURF_MASK_n
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
 CHARACTER(LEN=6),  INTENT(IN)  :: HMASK    
 CHARACTER(LEN=5),  INTENT(IN)  :: HACTION    
!
INTEGER                        :: ILU,IRET, IL
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!------------------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('INIT_IO_SURF_ASC_N',0,ZHOOK_HANDLE)
!
 CALL GET_LUOUT('ASCII ',NLUOUT)
!
!$OMP BARRIER
!
IF (NRANK==NPIO) LOPEN=.FALSE.
!
NUNIT=20
!
IF (HACTION=='GTMSK') THEN
  IF (NRANK==NPIO) THEN
!$OMP SINGLE
    OPEN(UNIT=NUNIT,FILE=CFILEIN,FORM='FORMATTED')
!$OMP END SINGLE
  ENDIF
  CMASK = HMASK
  IF (LHOOK) CALL DR_HOOK('INIT_IO_SURF_ASC_N',1,ZHOOK_HANDLE)
  RETURN
ENDIF
!
IF (HACTION == 'READ ') THEN
  IF (NRANK==NPIO) THEN 
!$OMP SINGLE          
    OPEN(UNIT=NUNIT,FILE=CFILEIN,FORM='FORMATTED')
!$OMP END SINGLE     
    LOPEN=.TRUE.
  ENDIF
  ! NFULL must be known even if HMASK/=FULL because it's no longer 
  ! updated in init_io_surf_maskn.
  CMASK = 'FULL ' 
  CALL READ_SURF('ASCII ','DIM_FULL',NFULL,IRET,HDIR='A')
  CMASK = HMASK
ELSE
  IF (NRANK==NPIO) THEN
!$OMP SINGLE
    IF (LCREATED) THEN
      OPEN(UNIT=NUNIT,FILE=CFILEOUT,FORM='FORMATTED',POSITION='APPEND')
    ELSE
      OPEN(UNIT=NUNIT,FILE=CFILEOUT,FORM='FORMATTED')
      LCREATED=.TRUE.
    ENDIF
    LOPEN=.TRUE.
!$OMP END SINGLE  
  ENDIF
  ! NFULL must be known in every case. 
  CALL GET_DIM_FULL_n(NFULL)
  CMASK = HMASK
ENDIF
!
! nindex is needed for call to get_size_full_n. In init_index_mpi, 
! it's not initialized for first readings.  
IF (.NOT.ALLOCATED(NINDEX)) THEN
  ALLOCATE(NINDEX(NFULL))
  NINDEX(:) = 0
ENDIF
!
!------------------------------------------------------------------------------
!
! MASK is sized according to the mpi task running
 CALL GET_SIZE_FULL_n('ASCII ',NFULL,ILU)
IF (ILU>NSIZE) NSIZE = ILU
!
IL = ILU
 CALL GET_TYPE_DIM_n(HMASK,IL)
 CALL INIT_IO_SURF_MASK_n(HMASK, IL, NLUOUT, ILU, NMASK)
!
!$OMP BARRIER
!
!------------------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('INIT_IO_SURF_ASC_N',1,ZHOOK_HANDLE)
!------------------------------------------------------------------------------
END SUBROUTINE INIT_IO_SURF_ASC_n
