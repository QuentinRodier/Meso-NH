!MNH_LIC Copyright 2009-2021 CNRS, Meteo-France and Universite Paul Sabatier
!MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
!MNH_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!MNH_LIC for details. version 1.
!-----------------------------------------------------------------
!     ###########################
      MODULE MODI_RESOLVED_ELEC_n
!     ###########################
!
INTERFACE
      SUBROUTINE RESOLVED_ELEC_n (HCLOUD, KRR, KMI, KTCOUNT, OEXIT,       &
                                  PTSTEP, PZZ, PRHODJ, PRHODREF, PEXNREF, &
                                  PPABST, PTHT, PWT,                      & 
                                  PRT, PRS, PSVT, PSVS, PCIT,             & 
                                  PINPRR,                                 &
                                  PSEA, PTOWN,                            &
                                  PCCS, PCRS, PCSS, PCGS, PCHS,           &
                                  PSVS_LNOX                               )   
!
CHARACTER(LEN=4),         INTENT(IN)    :: HCLOUD   ! kind of cloud
INTEGER,                  INTENT(IN)    :: KRR      ! Number of moist variables
INTEGER,                  INTENT(IN)    :: KMI      ! Model index
INTEGER,                  INTENT(IN)    :: KTCOUNT  ! Temporal loop counter
LOGICAL,                  INTENT(IN)    :: OEXIT    ! switch for the end of the temporal loop
REAL,                     INTENT(IN)    :: PTSTEP   ! Double Time step (single if cold start)
!
REAL, DIMENSION(:,:,:),   INTENT(IN)    :: PZZ     ! Height (z)
REAL, DIMENSION(:,:,:),   INTENT(IN)    :: PRHODJ  !Dry density * Jacobian
REAL, DIMENSION(:,:,:),   INTENT(IN)    :: PRHODREF! Reference dry air density
REAL, DIMENSION(:,:,:),   INTENT(IN)    :: PEXNREF ! Reference Exner function
!
REAL, DIMENSION(:,:,:),   INTENT(IN)    :: PPABST  ! abs. pressure at time t
REAL, DIMENSION(:,:,:),   INTENT(IN)    :: PTHT    ! Theta at time t
REAL, DIMENSION(:,:,:),   INTENT(IN)    :: PWT     ! vertical velocity at time t
REAL, DIMENSION(:,:,:,:), INTENT(IN)    :: PRT     ! Moist variables at time t
!
REAL, DIMENSION(:,:,:,:), INTENT(INOUT) :: PRS   ! Moist  variable sources
REAL, DIMENSION(:,:,:,:), INTENT(IN)    :: PSVT  ! Scalar variable at time t
REAL, DIMENSION(:,:,:,:), INTENT(INOUT) :: PSVS  ! Scalar variable sources
!
REAL, DIMENSION(:,:,:),   INTENT(IN)    :: PCIT  ! Pristine ice nb conc.
                                                 ! - at time t (for ICE schemes)
                                                 ! - source (for LIMA)
!
REAL, DIMENSION(:,:),      INTENT(IN)   :: PINPRR   ! Rain instant precip
!
REAL, DIMENSION(:,:),   OPTIONAL, INTENT(IN)    :: PSEA   ! Land Sea mask
REAL, DIMENSION(:,:),   OPTIONAL, INTENT(IN)    :: PTOWN  ! Town fraction
!
REAL, DIMENSION(:,:,:), OPTIONAL, INTENT(IN)    :: PCCS   ! Cld droplets nb conc source
REAL, DIMENSION(:,:,:), OPTIONAL, INTENT(IN)    :: PCRS   ! Rain nb conc source
REAL, DIMENSION(:,:,:), OPTIONAL, INTENT(IN)    :: PCSS   ! Snow nb conc source
REAL, DIMENSION(:,:,:), OPTIONAL, INTENT(IN)    :: PCGS   ! Graupel nb conc source
REAL, DIMENSION(:,:,:), OPTIONAL, INTENT(IN)    :: PCHS   ! Hail nb conc source
!
REAL, DIMENSION(:,:,:), OPTIONAL, INTENT(INOUT) :: PSVS_LNOX ! Scalar variable source for LNOX
!
!
END SUBROUTINE RESOLVED_ELEC_n
END INTERFACE
END MODULE MODI_RESOLVED_ELEC_n
!
!     #####################################################################
      SUBROUTINE RESOLVED_ELEC_n (HCLOUD, KRR, KMI, KTCOUNT, OEXIT,       &
                                  PTSTEP, PZZ, PRHODJ, PRHODREF, PEXNREF, &
                                  PPABST, PTHT, PWT,                      & 
                                  PRT, PRS, PSVT, PSVS, PCIT,             & 
                                  PINPRR,                                 &
                                  PSEA, PTOWN,                            &
                                  PCCS, PCRS, PCSS, PCGS, PCHS,           &
                                  PSVS_LNOX                               )   
!     #####################################################################
!
!!    PURPOSE
!!    -------
!!      The purpose of this routine is to compute the resolved clouds and 
!!    precipitation, the associated cloud electrification, and the charge 
!!    neutralization associated to lightning flashes
!!
!!
!!    METHOD
!!    ------
!!      The main action of this routine is to call the routines computing the
!!    microphysical and electrical sources. Before that:
!!        - it computes the real absolute pressure,
!!        - negative values of the current guess of all mixing ratio are removed.
!!          This is done by a global filling algorithm based on a multiplicative
!!          method (Rood, 1987), in order to conserved the total mass in the
!!          simulation domain.
!!        - Sources are transformed in physical tendencies, by removing the
!!          multiplicative term Rhod*J.
!!        - External points values are filled owing to the use of cyclic
!!          l.b.c., in order to performe computations on the full domain.
!!      After calling to microphysical and electrical routines, the physical 
!!    tendencies are switched back to prognostic variables.
!!
!!
!!    EXTERNAL
!!    --------
!!      Subroutine SLOW_TERMS: Computes the explicit microphysical sources
!!      Subroutine FAST_TERMS: Performs the saturation adjustment for l
!!      Subroutine RAIN_ICE  : Computes the explicit microphysical sources for i
!!      Subroutine ICE_ADJUST: Performs the saturation adjustment for i+l
!!      MIN_ll,SUM3D_ll : distributed functions equivalent to MIN and SUM
!!
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!      Module MODD_PARAMETERS : contains declarations of parameter variables
!!         JPHEXT       : Horizontal external points number
!!         JPVEXT       : Vertical external points number
!!      Module MODD_CST
!!          XP00               ! Reference pressure
!!          XRD                ! Gaz  constant for dry air
!!          XCPD               ! Cpd (dry air)
!!
!!    REFERENCE
!!    ---------
!!
!!    AUTHOR
!!    ------
!!      C. Barthe       * LACy *
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    06/11/09
!!      Modifications: 
!!      M. Chong      26/01/10  Add Small ions parameters
!!      M. Chong      31/07/14  Add explicit LiNOx
!!      J.Escobar : 15/09/2015 : WENO5 & JPHEXT <> 1
!  P. Wautelet 05/2016-04/2018: new data structures and calls for I/O
!  P. Wautelet 10/01/2019: use NEWUNIT argument of OPEN
!  P. Wautelet 22/01/2019: use standard FLUSH statement instead of non standard intrinsics
!  P. Wautelet 14/03/2019: bugfix: correct management of files
!  P. Wautelet 26/04/2019: replace non-standard FLOAT function by REAL function
!  P. Wautelet 12/02/2021: bugfix: change STATUS for opening files containing flash information (NEW->UNKNOWN)
!  P. Wautelet 17/02/2021: budgets: add DRIFT and CORAY terms for electricity
!!      C. Barthe   07/02/2022: remove cloud electrification from resolved_elec
!!      C. Barthe   08/09/2022: enable using CELLS with LIMA
!!
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
USE MODE_ELEC_ll
USE MODE_IO_FILE,          ONLY: IO_File_close, IO_File_open
USE MODE_IO_MANAGE_STRUCT, ONLY: IO_File_add2list, IO_File_find_byname
USE MODE_ll
!
USE MODD_CONF,        ONLY : CEXP, CSEG
USE MODD_CST
USE MODD_IO,          ONLY : TFILEDATA, TFILE_DUMMY
USE MODD_PARAMETERS,  ONLY : JPVEXT
USE MODD_PARAM_LIMA,  ONLY : NMOM_C, NMOM_R, NMOM_I, NMOM_S, NMOM_G, NMOM_H
USE MODD_ELEC_DESCR
USE MODD_ELEC_n
USE MODD_ARGSLIST_ll, ONLY : LIST_ll
USE MODD_TIME_n
USE MODD_LMA_SIMULATOR
!
USE MODD_VAR_ll,      ONLY : NMNH_COMM_WORLD
USE MODI_FLASH_GEOM_ELEC_n
USE MODI_ION_ATTACH_ELEC
USE MODI_SERIES_CLOUD_ELEC
!
IMPLICIT NONE
!
!
!*       0.1   Declarations of dummy arguments :
!
CHARACTER(LEN=4),         INTENT(IN)    :: HCLOUD   ! kind of cloud paramerization
INTEGER,                  INTENT(IN)    :: KRR      ! Number of moist variables
INTEGER,                  INTENT(IN)    :: KMI      ! Model index
INTEGER,                  INTENT(IN)    :: KTCOUNT  ! Temporal loop counter
LOGICAL,                  INTENT(IN)    :: OEXIT    ! switch for the end of the temporal loop
REAL,                     INTENT(IN)    :: PTSTEP   ! Double Time step (single if cold start)
!
REAL, DIMENSION(:,:,:),   INTENT(IN)    :: PZZ      ! Height (z)
REAL, DIMENSION(:,:,:),   INTENT(IN)    :: PRHODJ   ! Dry density * Jacobian
REAL, DIMENSION(:,:,:),   INTENT(IN)    :: PRHODREF ! Reference dry air density
REAL, DIMENSION(:,:,:),   INTENT(IN)    :: PEXNREF  ! Reference Exner function
!
REAL, DIMENSION(:,:,:),   INTENT(IN)    :: PPABST  ! abs. pressure at time t
REAL, DIMENSION(:,:,:),   INTENT(IN)    :: PTHT    ! Theta at time t
REAL, DIMENSION(:,:,:),   INTENT(IN)    :: PWT     ! vertical velocity at time t
REAL, DIMENSION(:,:,:,:), INTENT(IN)    :: PRT     ! Moist variables at time t
!
REAL, DIMENSION(:,:,:,:), INTENT(INOUT) :: PRS   ! Moist  variable sources
REAL, DIMENSION(:,:,:,:), INTENT(IN)    :: PSVT  ! Scalar variable at time t
REAL, DIMENSION(:,:,:,:), INTENT(INOUT) :: PSVS  ! Scalar variable sources
!
REAL, DIMENSION(:,:,:),   INTENT(IN)    :: PCIT  ! Pristine ice nb conc.
                                                 ! - at time t (for ICE schemes)
                                                 ! - source (for LIMA)
!
REAL, DIMENSION(:,:),     INTENT(IN)    :: PINPRR ! Rain instant precip
!
REAL, DIMENSION(:,:),   OPTIONAL, INTENT(IN)    :: PSEA   ! Land Sea mask
REAL, DIMENSION(:,:),   OPTIONAL, INTENT(IN)    :: PTOWN  ! Town fraction
!
REAL, DIMENSION(:,:,:), OPTIONAL, INTENT(IN)    :: PCCS   ! Cld droplets nb conc source
REAL, DIMENSION(:,:,:), OPTIONAL, INTENT(IN)    :: PCRS   ! Rain nb conc source
REAL, DIMENSION(:,:,:), OPTIONAL, INTENT(IN)    :: PCSS   ! Snow nb conc source
REAL, DIMENSION(:,:,:), OPTIONAL, INTENT(IN)    :: PCGS   ! Graupel nb conc source
REAL, DIMENSION(:,:,:), OPTIONAL, INTENT(IN)    :: PCHS   ! Hail nb conc source
!
REAL, DIMENSION(:,:,:), OPTIONAL, INTENT(INOUT) :: PSVS_LNOX ! Scalar variable source for LNOX
!
!
!*       0.2   Declarations of local variables :
!
INTEGER :: JRR,JSV       ! Loop index for the moist and scalar variables
INTEGER :: IIB           !  Define the physical domain
INTEGER :: IIE           !
INTEGER :: IJB           !
INTEGER :: IJE           !
INTEGER :: IKB           !
INTEGER :: IKE           !
INTEGER :: IINFO_ll      ! return code of parallel routine
INTEGER :: IPROC         ! my proc number
INTEGER :: IERR          ! error status
INTEGER :: ILU           ! unit number for IO
!
REAL, DIMENSION(SIZE(PZZ,1),SIZE(PZZ,2),SIZE(PZZ,3)) :: ZQTOT
                                    ! total charge source term
!
INTEGER, DIMENSION(3) :: IMINLOC, IMAXLOC
!
LOGICAL, DIMENSION(SIZE(PZZ,1),SIZE(PZZ,2),SIZE(PZZ,3)):: GATTACH  ! mask for
                                     !ion recombination and attachment
!
TYPE(LIST_ll), POINTER :: TZFIELDS_ll   ! list of fields to exchange

CHARACTER (LEN=32) :: YASCFILE
!
CHARACTER (LEN=18) :: YNAME
LOGICAL            :: GLMA_FILE
LOGICAL,                 SAVE :: GFIRST_CALL = .TRUE.
TYPE(TFILEDATA),POINTER, SAVE :: TZFILE_FGEOM_COORD       => NULL()
TYPE(TFILEDATA),POINTER, SAVE :: TZFILE_FGEOM_DIAG        => NULL()
TYPE(TFILEDATA),POINTER, SAVE :: TZFILE_LMA               => NULL()
TYPE(TFILEDATA),POINTER, SAVE :: TZFILE_SERIES_CLOUD_ELEC => NULL()
!
NULLIFY(TZFIELDS_ll)
!
IF ( GFIRST_CALL ) THEN
  GFIRST_CALL = .FALSE.
  TZFILE_FGEOM_COORD       => TFILE_DUMMY
  TZFILE_FGEOM_DIAG        => TFILE_DUMMY
  TZFILE_LMA               => TFILE_DUMMY
  TZFILE_SERIES_CLOUD_ELEC => TFILE_DUMMY
END IF
!
!------------------------------------------------------------------------------
!
!*       1.     PRELIMINARY COMPUTATIONS
!               ------------------------
!
CALL GET_INDICE_ll (IIB,IJB,IIE,IJE)
IKB = 1 + JPVEXT
IKE = SIZE(PZZ,3) - JPVEXT
!
CALL MYPROC_ELEC_ll (IPROC)
!
!------------------------------------------------------------------------------
!
!
!*      2.      ION RECOMBINATION AND ATTACHMENT
!               --------------------------------
!
GATTACH(:,:,:) = .FALSE.
GATTACH(IIB:IIE, IJB:IJE, IKB:IKE) = .TRUE.
!
IF (HCLOUD(1:3) == 'ICE') THEN
  IF (PRESENT(PSEA)) THEN
    CALL ION_ATTACH_ELEC(KTCOUNT, KRR, HCLOUD, PTSTEP, PRHODREF,      &
                         PRHODJ, PSVS(:,:,:,1:KRR+1),                 &
                         PRS, PTHT, PCIT, PPABST, XEFIELDU,           &
                         XEFIELDV, XEFIELDW, GATTACH,                 &
                         PTOWN=PTOWN, PSEA=PSEA)
  ELSE
    CALL ION_ATTACH_ELEC(KTCOUNT, KRR, HCLOUD, PTSTEP, PRHODREF,      &
                         PRHODJ, PSVS(:,:,:,1:KRR+1),                 &
                         PRS, PTHT, PCIT, PPABST, XEFIELDU,           &
                         XEFIELDV, XEFIELDW, GATTACH)
  ENDIF
ELSE IF (HCLOUD == 'LIMA') THEN
  IF (KRR == 7) THEN
    IF (NMOM_S == 1 .AND. NMOM_G == 1 .AND. NMOM_H == 1) THEN
      CALL ION_ATTACH_ELEC(KTCOUNT, KRR, HCLOUD, PTSTEP, PRHODREF,      &
                           PRHODJ, PSVS(:,:,:,1:KRR+1),                 &
                           PRS, PTHT, PCIT, PPABST, XEFIELDU,           &
                           XEFIELDV, XEFIELDW, GATTACH,                 &
                           PCCS=PCCS, PCRS=PCRS)
    ELSE IF (NMOM_S == 2 .AND. NMOM_G == 2 .AND. NMOM_H == 2) THEN
      CALL ION_ATTACH_ELEC(KTCOUNT, KRR, HCLOUD, PTSTEP, PRHODREF,      &
                           PRHODJ, PSVS(:,:,:,1:KRR+1),                 &
                           PRS, PTHT, PCIT, PPABST, XEFIELDU,           &
                           XEFIELDV, XEFIELDW, GATTACH,                 &
                           PCCS=PCCS, PCRS=PCRS, PCSS=PCSS, PCGS=PCGS, PCHS=PCHS)
    END IF
  ELSE IF (KRR == 6) THEN
    IF (NMOM_S == 1 .AND. NMOM_G == 1) THEN
      CALL ION_ATTACH_ELEC(KTCOUNT, KRR, HCLOUD, PTSTEP, PRHODREF,      &
                           PRHODJ, PSVS(:,:,:,1:KRR+1),                 &
                           PRS, PTHT, PCIT, PPABST, XEFIELDU,           &
                           XEFIELDV, XEFIELDW, GATTACH,                 &
                           PCCS=PCCS, PCRS=PCRS)
    ELSE IF (NMOM_S == 2 .AND. NMOM_G == 2) THEN
      CALL ION_ATTACH_ELEC(KTCOUNT, KRR, HCLOUD, PTSTEP, PRHODREF,      &
                           PRHODJ, PSVS(:,:,:,1:KRR+1),                 &
                           PRS, PTHT, PCIT, PPABST, XEFIELDU,           &
                           XEFIELDV, XEFIELDW, GATTACH,                 &
                           PCCS=PCCS, PCRS=PCRS, PCSS=PCSS, PCGS=PCGS, PCHS=PCHS)
    END IF
  END IF
END IF
!
!-------------------------------------------------------------------------------
!
!*      3.      OPEN THE OUTPUT ASCII FILES
!               ---------------------------
!
IF (KTCOUNT == 1 .AND. IPROC == 0) THEN
  IF (LFLASH_GEOM) THEN
    YASCFILE = CEXP//"_"//CSEG//"_fgeom_diag.asc"
    TZFILE_FGEOM_DIAG => NULL()
    CALL IO_File_add2list(TZFILE_FGEOM_DIAG,YASCFILE,'TXT','WRITE')
    CALL IO_File_open(TZFILE_FGEOM_DIAG,HPOSITION='APPEND',HSTATUS='UNKNOWN')
    ILU = TZFILE_FGEOM_DIAG%NLU
    WRITE (UNIT=ILU, FMT='(A)') '--------------------------------------------------------'
    WRITE (UNIT=ILU, FMT='(A)') '*FLASH CHARACTERISTICS FROM FLASH_GEOM_ELEC*'
    WRITE (UNIT=ILU, FMT='(A)') '-- Column 1 : total flash number          --'
    WRITE (UNIT=ILU, FMT='(A)') '-- Column 2 : time (s)                    --'
    WRITE (UNIT=ILU, FMT='(A)') '-- Column 3 : cell number                 --'
    WRITE (UNIT=ILU, FMT='(A)') '-- Column 4 : flash number/cell/time step --'
    WRITE (UNIT=ILU, FMT='(A)') '-- Column 5 : flash type 1=IC, 2=CGN, 3=CGP '
    WRITE (UNIT=ILU, FMT='(A)') '-- Column 6 : number of segments          --'
    WRITE (UNIT=ILU, FMT='(A)') '-- Column 7 : trig electric field (kV/m)  --'
    WRITE (UNIT=ILU, FMT='(A)') '-- Column 8 : x coord. trig. point        --'
    WRITE (UNIT=ILU, FMT='(A)') '-- Column 9 : y coord. trig. point        --'
    WRITE (UNIT=ILU, FMT='(A)') '--         --> x,y in km if lcartesian=t, --'
    WRITE (UNIT=ILU, FMT='(A)') '--                    deg otherwise       --'
    WRITE (UNIT=ILU, FMT='(A)') '-- Column 10 : z coord. trig. point (km)  --'
    WRITE (UNIT=ILU, FMT='(A)') '-- Column 11: neutr. positive charge (C)  --'
    WRITE (UNIT=ILU, FMT='(A)') '-- Column 12: neutr. negative charge (C)  --'
    WRITE (UNIT=ILU, FMT='(A)') '--------------------------------------------'
    FLUSH(UNIT=ILU)
!
    IF (LSAVE_COORD) THEN
      YASCFILE = CEXP//"_fgeom_coord.asc"
      TZFILE_FGEOM_COORD => NULL()
      CALL IO_File_add2list(TZFILE_FGEOM_COORD,YASCFILE,'TXT','WRITE')
      CALL IO_File_open(TZFILE_FGEOM_COORD,HPOSITION='APPEND',HSTATUS='UNKNOWN')
      ILU = TZFILE_FGEOM_COORD%NLU
      WRITE (UNIT=ILU,FMT='(A)') '------------------------------------------'
      WRITE (UNIT=ILU,FMT='(A)') '*****FLASH COORD. FROM FLASH_GEOM_ELEC****'
      WRITE (UNIT=ILU,FMT='(A)') '-- Column 1 : flash number             --'
      WRITE (UNIT=ILU,FMT='(A)') '-- Column 2 : time (s)                 --'
      WRITE (UNIT=ILU,FMT='(A)') '-- Column 3 : type                     --'
      WRITE (UNIT=ILU,FMT='(A)') '-- Column 4 : coordinate along X (km)  --'
      WRITE (UNIT=ILU,FMT='(A)') '-- Column 5 : coordinate along Y (km)  --'
      WRITE (UNIT=ILU,FMT='(A)') '-- Column 6 : coordinate along Z (km)  --'
      WRITE (UNIT=ILU,FMT='(A)') '------------------------------------------'
      FLUSH(UNIT=ILU)
    END IF
  END IF
!
  IF (LSERIES_ELEC) THEN
    YASCFILE = CEXP//"_series_cloud_elec.asc"
    TZFILE_SERIES_CLOUD_ELEC => NULL()
    CALL IO_File_add2list(TZFILE_SERIES_CLOUD_ELEC,YASCFILE,'TXT','WRITE')
    CALL IO_File_open(TZFILE_SERIES_CLOUD_ELEC,HPOSITION='APPEND',HSTATUS='UNKNOWN')
    ILU = TZFILE_SERIES_CLOUD_ELEC%NLU
    WRITE (UNIT=ILU, FMT='(A)') '----------------------------------------------------'
    WRITE (UNIT=ILU, FMT='(A)') '********* RESULTS FROM of LSERIES_ELEC *************'
    WRITE (UNIT=ILU, FMT='(A)') '-- Column 1 : Time (s)                            --'
    WRITE (UNIT=ILU, FMT='(A)') '-- Column 2 : Cloud top height / Z > 20 dBZ (m)   --'
    WRITE (UNIT=ILU, FMT='(A)') '-- Column 3 : Cloud top height / m.r. > 1.e-4 (m) --'
    WRITE (UNIT=ILU, FMT='(A)') '-- Column 4 : Maximum radar reflectivity (dBZ)    --'
    WRITE (UNIT=ILU, FMT='(A)') '-- Column 5 : Maximum vertical velocity (m/s)     --'
    WRITE (UNIT=ILU, FMT='(A)') '-- Column 6 : Updraft volume for W > 5 m/s        --'
    WRITE (UNIT=ILU, FMT='(A)') '-- Column 7 : Updraft volume for W > 10 m/s       --'
    WRITE (UNIT=ILU, FMT='(A)') '-- Column 8 : Cloud water mass (kg)               --'
    WRITE (UNIT=ILU, FMT='(A)') '-- Column 9 : Rain water mass (kg)                --'
    WRITE (UNIT=ILU, FMT='(A)') '-- Column 10 : Ice crystal mass (kg)              --'
    WRITE (UNIT=ILU, FMT='(A)') '-- Column 11 : Snow mass (kg)                     --'
    WRITE (UNIT=ILU, FMT='(A)') '-- Column 12 : Graupel mass (kg)                  --'
    WRITE (UNIT=ILU, FMT='(A)') '-- Column 13 : Precipitation ice mass (kg)        --'
    WRITE (UNIT=ILU, FMT='(A)') '-- Column 14 : Ice mass flux product (kg2 m2/s2)  --'
    WRITE (UNIT=ILU, FMT='(A)') '-- Column 15 : Precip. ice mass flux (kg m/s)     --'
    WRITE (UNIT=ILU, FMT='(A)') '-- Column 16 : Non-precip. ice mass flux (kg m/s) --'
    WRITE (UNIT=ILU, FMT='(A)') '-- Column 17 : Ice water path (kg/m2)             --'
    WRITE (UNIT=ILU, FMT='(A)') '-- Column 18 : Cloud volume (m3)                  --'
    WRITE (UNIT=ILU, FMT='(A)') '-- Column 19 : Maximum rain inst. precip. (mm/H)  --'
    WRITE (UNIT=ILU, FMT='(A)') '-- Column 20 : Rain instant. precip. (mm/H)       --'
    WRITE (UNIT=ILU, FMT='(A)') '----------------------------------------------------'
    FLUSH(UNIT=ILU)
  END IF
END IF
!
IF (LFLASH_GEOM .AND. LLMA) THEN
!
! test to see if a new LMA file should be created
!
  GLMA_FILE = .FALSE.
!
  IF (TDTCUR%xtime >= TDTLMA%xtime-PTSTEP .AND. CLMA_FILE(1:5) /= "BEGIN") THEN
    LWRITE_LMA  = .TRUE.
  END IF
  IF (TDTCUR%xtime >= TDTLMA%xtime) THEN
    TDTLMA%xtime = TDTLMA%xtime + XDTLMA
    GLMA_FILE   = .TRUE.
    LWRITE_LMA  = .FALSE.
  END IF
!
  IF (GLMA_FILE) THEN
    IF(CLMA_FILE(1:5) /= "BEGIN") THEN ! close previous file if exists
      CALL IO_File_find_byname(CLMA_FILE,TZFILE_LMA,IERR)
      CALL IO_File_close(TZFILE_LMA)
      TZFILE_LMA => NULL()
    ENDIF
!
    TDTLMA%xtime = TDTLMA%xtime - XDTLMA
    WRITE (YNAME,FMT='(3I2.2,A1,3I2.2,A1,I4.4)')                                 &
          ABS(TDTCUR%nyear-2000),TDTCUR%nmonth,TDTCUR%nday,'_',                  &
            INT(TDTLMA%xtime/3600.),INT(REAL(MOD(INT(TDTLMA%xtime),3600))/60.),  &
                                      MOD(INT(TDTLMA%xtime),60), '_', INT(XDTLMA)
    TDTLMA%xtime = MOD(TDTLMA%xtime + XDTLMA,86400.)
    CLMA_FILE = CEXP//"_SIMLMA_"//YNAME//".dat"
!
    IF ( IPROC .EQ. 0 ) THEN
      TZFILE_LMA => NULL()
      CALL IO_File_add2list(TZFILE_LMA,CLMA_FILE,'TXT','WRITE')
      CALL IO_File_open(TZFILE_LMA,HPOSITION='APPEND',HSTATUS='UNKNOWN')
      ILU = TZFILE_LMA%NLU
      WRITE (UNIT=ILU,FMT='(A)') '----------------------------------------'
      WRITE (UNIT=ILU,FMT='(A)') '*** FLASH COORD. FROM LMA SIMULATOR ****'
      WRITE (UNIT=ILU,FMT='(A)') '-- Column 1  : flash number           --'
      WRITE (UNIT=ILU,FMT='(A)') '-- Column 2  : time (s)               --'
      WRITE (UNIT=ILU,FMT='(A)') '-- Column 3  : type                   --'
      WRITE (UNIT=ILU,FMT='(A)') '-- Column 4  : coordinate along X (km)--'
      WRITE (UNIT=ILU,FMT='(A)') '-- Column 5  : coordinate along Y (km)--'
      WRITE (UNIT=ILU,FMT='(A)') '-- Column 6  : coordinate along Z (km)--'
      WRITE (UNIT=ILU,FMT='(A)') '-- Column 7  : cld drop. mixing ratio --'
      WRITE (UNIT=ILU,FMT='(A)') '-- Column 8  : rain mixing ratio      --'
      WRITE (UNIT=ILU,FMT='(A)') '-- Column 9  : ice cryst mixing ratio --'
      WRITE (UNIT=ILU,FMT='(A)') '-- Column 10 : snow mixing ratio      --'
      WRITE (UNIT=ILU,FMT='(A)') '-- Column 11 : graupel mixing ratio   --'
      WRITE (UNIT=ILU,FMT='(A)') '-- Column 12 : rain charge neut       --'
      WRITE (UNIT=ILU,FMT='(A)') '-- Column 13 : ice cryst. charge neut --'
      WRITE (UNIT=ILU,FMT='(A)') '-- Column 14 : snow charge neut       --'
      WRITE (UNIT=ILU,FMT='(A)') '-- Column 15 : graupel charge neut    --'
      WRITE (UNIT=ILU,FMT='(A)') '-- Column 16 : positive ions neut     --'
      WRITE (UNIT=ILU,FMT='(A)') '-- Column 17 : negative ions neut     --'
      WRITE (UNIT=ILU,FMT='(A)') '----------------------------------------'
      FLUSH(UNIT=ILU)
    END IF
  END IF
END IF
!
!
!-------------------------------------------------------------------------------
!
!*      10.     LIGHTNING FLASHES AND NOX PRODUCTION
!               ------------------------------------
!
! the lightning scheme is now called at each time step
! but only if there's electric charge in the domain
!
ZQTOT(:,:,:) = XECHARGE * (PSVT(:,:,:,1) - PSVT(:,:,:,KRR+1))
DO JSV = 2, KRR
  ZQTOT(:,:,:) = ZQTOT(:,:,:) + PSVT(:,:,:,JSV)
END DO
!
!++cb-- reprendre les appels avec bcp de conditions : utiliser des tableaux (0,0,0)
IF ((.NOT. LOCG) .AND. LELEC_FIELD .AND.  MAX_ll(ABS(ZQTOT),IINFO_ll)>0.) THEN
  IF (LFLASH_GEOM) THEN
    IF (HCLOUD(1:3) == 'ICE') THEN 
      IF (PRESENT(PTOWN) .AND. PRESENT(PSEA)) THEN
        IF (PRESENT(PSVS_LNOX)) THEN
          CALL FLASH_GEOM_ELEC_n (KTCOUNT, KMI, KRR, HCLOUD, PTSTEP, OEXIT,          &
                                  PRHODJ, PRHODREF, PRT, PCIT,                       &
                                  PSVS(:,:,:,1:KRR+1),                               &
                                  PRS, PTHT, PPABST, XEFIELDU, XEFIELDV, XEFIELDW,   &
                                  PZZ,                                               &
                                  TZFILE_FGEOM_DIAG, TZFILE_FGEOM_COORD, TZFILE_LMA, &
                                  PTOWN=PTOWN, PSEA=PSEA, PSVS_LNOX=PSVS_LNOX)
        ELSE
          CALL FLASH_GEOM_ELEC_n (KTCOUNT, KMI, KRR, HCLOUD, PTSTEP, OEXIT,          &
                                  PRHODJ, PRHODREF, PRT, PCIT,                       &
                                  PSVS(:,:,:,1:KRR+1),                               &
                                  PRS, PTHT, PPABST, XEFIELDU, XEFIELDV, XEFIELDW,   &
                                  PZZ,                                               &
                                  TZFILE_FGEOM_DIAG, TZFILE_FGEOM_COORD, TZFILE_LMA, &
                                  PTOWN=PTOWN, PSEA=PSEA)
        END IF
      ELSE
        IF (PRESENT(PSVS_LNOX)) THEN
          CALL FLASH_GEOM_ELEC_n (KTCOUNT, KMI, KRR, HCLOUD, PTSTEP, OEXIT,          &
                                  PRHODJ, PRHODREF, PRT, PCIT,                       &
                                  PSVS(:,:,:,1:KRR+1),                               &
                                  PRS, PTHT, PPABST, XEFIELDU, XEFIELDV, XEFIELDW,   &
                                  PZZ,                                               &
                                  TZFILE_FGEOM_DIAG, TZFILE_FGEOM_COORD, TZFILE_LMA, &
                                  PSVS_LNOX=PSVS_LNOX)
        ELSE
          CALL FLASH_GEOM_ELEC_n (KTCOUNT, KMI, KRR, HCLOUD, PTSTEP, OEXIT,          &
                                  PRHODJ, PRHODREF, PRT, PCIT,                       &
                                  PSVS(:,:,:,1:KRR+1),                               &
                                  PRS, PTHT, PPABST, XEFIELDU, XEFIELDV, XEFIELDW,   &
                                  PZZ,                                               &
                                  TZFILE_FGEOM_DIAG, TZFILE_FGEOM_COORD, TZFILE_LMA)
        END IF
      END IF
    ELSE
      IF (HCLOUD == 'LIMA' .AND. ((KRR == 6 .AND. NMOM_S == 1 .AND. NMOM_G == 1) .OR. &
                                  (KRR == 7 .AND. NMOM_S == 1 .AND. NMOM_G == 1 .AND. NMOM_H == 1))) THEN  
        IF (PRESENT(PSVS_LNOX)) THEN
          CALL FLASH_GEOM_ELEC_n (KTCOUNT, KMI, KRR, HCLOUD, PTSTEP, OEXIT,          &
                                  PRHODJ, PRHODREF, PRT, PCIT,                       &
                                  PSVS(:,:,:,1:KRR+1),                               &
                                  PRS, PTHT, PPABST, XEFIELDU, XEFIELDV, XEFIELDW,   &
                                  PZZ,                                               &
                                  TZFILE_FGEOM_DIAG, TZFILE_FGEOM_COORD, TZFILE_LMA, &
                                  PCCS=PCCS, PCRS=PCRS,                              &
                                  PSVS_LNOX=PSVS_LNOX)
        ELSE
          CALL FLASH_GEOM_ELEC_n (KTCOUNT, KMI, KRR, HCLOUD, PTSTEP, OEXIT,          &
                                  PRHODJ, PRHODREF, PRT, PCIT,                       &
                                  PSVS(:,:,:,1:KRR+1),                               &
                                  PRS, PTHT, PPABST, XEFIELDU, XEFIELDV, XEFIELDW,   &
                                  PZZ,                                               &
                                  TZFILE_FGEOM_DIAG, TZFILE_FGEOM_COORD, TZFILE_LMA, &
                                  PCCS=PCCS, PCRS=PCRS)
        END IF
      ELSE IF (HCLOUD == 'LIMA' .AND. KRR == 6 .AND. NMOM_S == 2 .AND. NMOM_G == 2) THEN  
        IF (PRESENT(PSVS_LNOX)) THEN
          CALL FLASH_GEOM_ELEC_n (KTCOUNT, KMI, KRR, HCLOUD, PTSTEP, OEXIT,          &
                                  PRHODJ, PRHODREF, PRT, PCIT,                       &
                                  PSVS(:,:,:,1:KRR+1),                               &
                                  PRS, PTHT, PPABST, XEFIELDU, XEFIELDV, XEFIELDW,   &
                                  PZZ,                                               &
                                  TZFILE_FGEOM_DIAG, TZFILE_FGEOM_COORD, TZFILE_LMA, &
                                  PCCS=PCCS, PCRS=PCRS, PCSS=PCSS, PCGS=PCGS,        &
                                  PSVS_LNOX=PSVS_LNOX)
        ELSE
          CALL FLASH_GEOM_ELEC_n (KTCOUNT, KMI, KRR, HCLOUD, PTSTEP, OEXIT,          &
                                  PRHODJ, PRHODREF, PRT, PCIT,                       &
                                  PSVS(:,:,:,1:KRR+1),                               &
                                  PRS, PTHT, PPABST, XEFIELDU, XEFIELDV, XEFIELDW,   &
                                  PZZ,                                               &
                                  TZFILE_FGEOM_DIAG, TZFILE_FGEOM_COORD, TZFILE_LMA, &
                                  PCCS=PCCS, PCRS=PCRS, PCSS=PCSS, PCGS=PCGS)
        END IF
      ELSE IF (HCLOUD == 'LIMA' .AND. KRR == 7 .AND. NMOM_S == 2 .AND. NMOM_G == 2 .AND. NMOM_H == 2) THEN  
        IF (PRESENT(PSVS_LNOX)) THEN
          CALL FLASH_GEOM_ELEC_n (KTCOUNT, KMI, KRR, HCLOUD, PTSTEP, OEXIT,              &
                                  PRHODJ, PRHODREF, PRT, PCIT,                           &
                                  PSVS(:,:,:,1:KRR+1),                                   &
                                  PRS, PTHT, PPABST, XEFIELDU, XEFIELDV, XEFIELDW,       &
                                  PZZ,                                                   &
                                  TZFILE_FGEOM_DIAG, TZFILE_FGEOM_COORD, TZFILE_LMA,     &
                                  PCCS=PCCS, PCRS=PCRS, PCSS=PCSS, PCGS=PCGS, PCHS=PCHS, &
                                  PSVS_LNOX=PSVS_LNOX)
        ELSE
          CALL FLASH_GEOM_ELEC_n (KTCOUNT, KMI, KRR, HCLOUD, PTSTEP, OEXIT,          &
                                  PRHODJ, PRHODREF, PRT, PCIT,                       &
                                  PSVS(:,:,:,1:KRR+1),                               &
                                  PRS, PTHT, PPABST, XEFIELDU, XEFIELDV, XEFIELDW,   &
                                  PZZ,                                               &
                                  TZFILE_FGEOM_DIAG, TZFILE_FGEOM_COORD, TZFILE_LMA, &
                                  PCCS=PCCS, PCRS=PCRS, PCSS=PCSS, PCGS=PCGS, PCHS=PCHS)
        END IF
      END IF
    END IF
  END IF
!
  PSVS(:,:,:,1)     = MAX(0., PSVS(:,:,:,1))
  PSVS(:,:,:,KRR+1) = MAX(0., PSVS(:,:,:,KRR+1))
!
END IF
!
!
!-------------------------------------------------------------------------------
!
!*      11.     LOOK FOR FLASH RATE PROXIES
!               ---------------------------
!
IF (LSERIES_ELEC) THEN
  CALL SERIES_CLOUD_ELEC (KTCOUNT, PTSTEP,                &
                          PZZ, PRHODJ, PRHODREF, PEXNREF, &
                          PRT, PRS, PSVT,                 &
                          PTHT, PWT, PPABST, PCIT,        &
                          TZFILE_SERIES_CLOUD_ELEC,       &
                          PINPRR                          )
END IF
!
!
!-------------------------------------------------------------------------------
!
!   Close Ascii Files if KTCOUNT = NSTOP
IF (OEXIT .AND. IPROC==0) THEN
  IF (LFLASH_GEOM) THEN
    CALL IO_File_close(TZFILE_FGEOM_DIAG)
    TZFILE_FGEOM_DIAG => TFILE_DUMMY
  END IF
  IF (LFLASH_GEOM .AND. LSAVE_COORD) THEN
    CALL IO_File_close(TZFILE_FGEOM_COORD)
    TZFILE_FGEOM_COORD => TFILE_DUMMY
  END IF
  IF (LSERIES_ELEC) THEN
    CALL IO_File_close(TZFILE_SERIES_CLOUD_ELEC)
    TZFILE_SERIES_CLOUD_ELEC => TFILE_DUMMY
  END IF
  IF (LFLASH_GEOM .AND. LLMA) THEN
    CALL IO_File_close(TZFILE_LMA)
    TZFILE_LMA => TFILE_DUMMY
  END IF
ENDIF
!
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE RESOLVED_ELEC_n
