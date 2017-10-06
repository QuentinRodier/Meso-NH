!MNH_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
!MNH_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!MNH_LIC for details. version 1.
!     #########################
      MODULE MODI_MNHOPEN_AUX_IO_SURF
!     #########################
INTERFACE
      SUBROUTINE MNHOPEN_AUX_IO_SURF(HFILE,HFILETYPE,HMASK)
!
CHARACTER(LEN=28), INTENT(IN)  :: HFILE     ! file name
CHARACTER(LEN=6),  INTENT(IN)  :: HFILETYPE ! main program
CHARACTER(LEN=6),  INTENT(IN)  :: HMASK
!
END SUBROUTINE MNHOPEN_AUX_IO_SURF
!
END INTERFACE
END MODULE MODI_MNHOPEN_AUX_IO_SURF
!
!     #######################################################
      SUBROUTINE MNHOPEN_AUX_IO_SURF(HFILE,HFILETYPE,HMASK)
!     #######################################################
!
!!****  *MNHOPEN_AUX_IO_SURF* - routine to open surface IO files (MESONH universe)
!!
!!    PURPOSE
!!    -------
!!
!!**  METHOD
!!    ------
!!
!!    EXTERNAL
!!    --------
!!
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!
!!    REFERENCE
!!    ---------
!!
!!
!!    AUTHOR
!!    ------
!!	S.Malardel   *Meteo France*	
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    09/2003 
!!         M.Moge   04/2015  parallelization og PREP_PGD on son model
!!         J.Escobar : 15/09/2015 : WENO5 & JPHEXT <> 1 
!!         J.Escobar : 19/04/2016 : Pb IOZ/NETCDF , missing OPARALLELIO=.FALSE. for PGD files
!!         J.Escobar : 02/06/2016 : abort MNHOPEN with STOP if problem with OPEN of INPUT/READ file 
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
!
USE MODD_IO_SURF_MNH, ONLY : COUT, CFILE, TPINFILE, COUTFILE, NLUOUT, &
         NMASK_ALL, CMASK, NIU_ALL, NJU_ALL, NIB_ALL, NJB_ALL, NIE_ALL, NJE_ALL, CACTION, &
         NMASK, NIU, NJU, NIB, NJB, NIE, NJE
!
USE MODD_CONF,           ONLY : CPROGRAM
USE MODD_PARAMETERS,     ONLY : JPHEXT
USE MODD_LUNIT,          ONLY : CLUOUT0, COUTFMFILE, CPGDFILE
!
USE MODE_FM
USE MODE_FMREAD
USE MODE_IO_ll
USE MODE_IO_MANAGE_STRUCT, ONLY: IO_FILE_ADD2LIST,IO_FILE_FIND_BYNAME
!
USE MODI_GET_1D_MASK
USE MODI_MNH_SURF_GRID_IO_INIT
!
IMPLICIT NONE
!
!*       0.1   Declarations of arguments
!              -------------------------
!
CHARACTER(LEN=28), INTENT(IN)  :: HFILE     ! file name
CHARACTER(LEN=6),  INTENT(IN)  :: HFILETYPE ! main program
CHARACTER(LEN=6),  INTENT(IN)  :: HMASK
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
INTEGER           :: IRESP
INTEGER           :: IMI            ! model index
INTEGER           :: IIMAX          ! number of points in X direction
INTEGER           :: IJMAX          ! number of points in Y direction
!
!
INTEGER           :: ILU            ! 1D physical dimension of XCOVER
REAL, DIMENSION(:),   ALLOCATABLE :: ZFULL  ! total cover
INTEGER           :: IJPHEXT
!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------
! WARNING : this routine works only on ONE processor jobs
!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------
!
!*       1.    initialization of output listing name
!
SELECT CASE(CPROGRAM)
  CASE('MESONH','SPAWN ')
    CALL GET_MODEL_NUMBER_ll  (IMI)
    WRITE(COUT,FMT='(A14,I1,A13)') 'OUTPUT_LISTING',IMI,'            '
  CASE DEFAULT
    COUT = CLUOUT0
END SELECT
!
CALL FMLOOK_ll(COUT,COUT,NLUOUT,IRESP)
!
!
!*       2.    initialization of surface file
!
IF (LEN_TRIM(CACTION)>0) THEN
  WRITE(NLUOUT,*) 'file ',HFILE,' cannot be opened because another MESONH file is in use'
END IF
!
IF (HFILE/=COUTFMFILE .AND. HFILE/=CPGDFILE) THEN
  CALL IO_FILE_ADD2LIST(TPINFILE,TRIM(HFILE),'UNKNOWN','READ',KLFITYPE=2,KLFIVERB=5,OOLD=.TRUE.)
  CALL IO_FILE_OPEN_ll(TPINFILE,OPARALLELIO=.FALSE.)
  !
  IF (IRESP .NE. 0) THEN
    PRINT*," /!\  MNHOPEN_AUX_IO_SURF :: FATAL PROBLEM OPENING INPUT/READ FILES =", HFILE
    STOP '/!\ MNHOPEN_AUX_IO_SURF :: FATAL PROBLEM OPENING INPUT/READ FILES , CHECK OUTPUT_LISTING* !!!'
  ENDIF
  CACTION = 'OPEN  '
ELSE
  CALL IO_FILE_FIND_BYNAME(TRIM(HFILE),TPINFILE,IRESP)
END IF
!
CFILE    = HFILE
COUTFILE = HFILE
!
!
!*       3.    initialisation of 2D arrays for entire physical field
!
CALL IO_READ_FIELD(TPINFILE,'IMAX',IIMAX)
CALL IO_READ_FIELD(TPINFILE,'JMAX',IJMAX)
CALL MNH_SURF_GRID_IO_INIT(IIMAX,IJMAX)
IJPHEXT= 1
CALL IO_READ_FIELD(TPINFILE,'JPHEXT',IJPHEXT)
IF ( IJPHEXT .NE. JPHEXT ) THEN
   WRITE(NLUOUT,FMT=*) ' MNHOPEN_AUX_IO : JPHEXT in PRE_PGD1.nam/NAM_CONF_PGD ( or default value )&
      & JPHEXT=',JPHEXT
   WRITE(NLUOUT,FMT=*) ' different from PGD files=',HFILE ,' value JPHEXT=',IJPHEXT
   WRITE(NLUOUT,FMT=*) '-> JOB ABORTED'
   CALL CLOSE_ll(COUT,IOSTAT=IRESP)
   CALL ABORT  
   STOP   
END IF
!
NIU_ALL = (IIMAX+2*JPHEXT)
NJU_ALL = (IJMAX+2*JPHEXT)
NIB_ALL = 1 + JPHEXT
NJB_ALL = 1 + JPHEXT
NIE_ALL = IIMAX + JPHEXT
NJE_ALL = IJMAX + JPHEXT
!
!*       4.    initialisation 1D physical dimension and mask for entire physical field
! 
ILU = (NIE_ALL-NIB_ALL+1)*(NJE_ALL-NJB_ALL+1)
!
CMASK=HMASK
!
!IF (HMASK=='FULL  ') THEN
  ALLOCATE(ZFULL(ILU))
  ZFULL=1.
  ALLOCATE(NMASK_ALL(ILU))
  CALL GET_1D_MASK(ILU,ILU,ZFULL,NMASK_ALL)
  DEALLOCATE(ZFULL)
!ELSE
!  WRITE(NLUOUT,*) 'mask "',HMASK,'" for reading not supported for auxilliary MESONH file'
!END IF
!
!
!*       5.    initialisation of 2D arrays for current processor
!
    CALL GET_DIM_EXT_ll('B',NIU,NJU)
    CALL GET_INDICE_ll (NIB,NJB,NIE,NJE)
!
!
!*       6.    initialisation 1D physical dimension and mask for current processor
! 
ILU = (NIE-NIB+1)*(NJE-NJB+1)
ALLOCATE(ZFULL(ILU))
ZFULL=1.
ALLOCATE(NMASK(ILU))
CALL GET_1D_MASK(ILU,ILU,ZFULL,NMASK)
DEALLOCATE(ZFULL)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE MNHOPEN_AUX_IO_SURF
