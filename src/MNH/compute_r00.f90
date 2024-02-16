!MNH_LIC Copyright 1994-2024 CNRS, Meteo-France and Universite Paul Sabatier
!MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
!MNH_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!MNH_LIC for details. version 1.
!-----------------------------------------------------------------
!###############################
MODULE MODE_COMPUTE_R00
! ###############################

  IMPLICIT NONE

  PRIVATE

  PUBLIC :: INI_COMPUTE_R00
  PUBLIC :: COMPUTE_R00

  REAL, PARAMETER :: NSPVAL = -1.E+11

  INTEGER                 :: NFILES
  INTEGER                 :: NNBR_START
  INTEGER, DIMENSION(100) :: NBRFILES

!PW:TODO: DOCTOR
!PW: si JF=1: reprendre comportement orig DIAG


CONTAINS

!#################################
SUBROUTINE INI_COMPUTE_R00()
!#################################

  USE MODD_LUNIT_n,    ONLY: CINIFILE
  USE MODD_PARAMETERS, ONLY: NUNDEF
  USE MODD_STO_FILE,   ONLY: CFILES, NSTART_SUPP, NTRAJSTLG

  USE MODE_MSG

  IMPLICIT NONE

  INTEGER :: IFILECUR, JFILECUR
  INTEGER :: JLOOP
  INTEGER :: JF

  JF=1
  DO WHILE (LEN_TRIM(CFILES(JF))/=0)
    JF=JF+1
  END DO
  !
  IF (JF/=1) THEN
    IF (CINIFILE==CFILES(JF-1)) THEN
      CALL PRINT_MSG( NVERB_FATAL, 'GEN', 'INI_COMPUTE_R00', 'initial file not treated' )
    END IF
  END IF
!
!
!*       2.0    FIND THE FILE TO BE TREATED AND THE INIT-SV FILES
!               -------------------------------------------------
!
! Search the number of the file to be treated
  IFILECUR=0
  DO JFILECUR=1,100
    IF (CINIFILE==CFILES(JFILECUR)) THEN
      IFILECUR=JFILECUR
      EXIT
    END IF
  END DO
  !
  IF ( IFILECUR == 0) CALL PRINT_MSG( NVERB_FATAL, 'GEN', 'COMPUTE_R00', 'problem with CINIFILE:' // TRIM(CINIFILE) )
  !
  ! Search the number of the files(NFILES), where the Lagrangian tracers
  !have been reinitialized
  NFILES=0
  DO JFILECUR=IFILECUR+1,100
    IF (LEN_TRIM(CFILES(JFILECUR)) /= 0) THEN
      NFILES= NFILES +1
      NBRFILES(NFILES)=JFILECUR       ! contains the number of the files where
                                      ! the Lag. tracers have been restarted
    ENDIF
  END DO
  !
  ! compute the number of supplementary cumulative starts
  NNBR_START=1
  DO JLOOP=1,NFILES-1
    IF (NSTART_SUPP(JLOOP)/=NUNDEF .AND. NSTART_SUPP(JLOOP)> IFILECUR ) THEN
      NNBR_START=NNBR_START+1
    END IF
  END DO

  NTRAJSTLG = NNBR_START
END SUBROUTINE INI_COMPUTE_R00

!#############################
SUBROUTINE COMPUTE_R00(TPFILE)
!#############################
!
!!**** 
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
!!     MODD_STO_FILE : CFILES
!!     MODD_GRID1 : XZZ, XXHAT,XYHAT
!!     MODD_LUNIT1: CINIFILE
!!     MODD_FIELD1: XSVM
!!     MODD_CONF : NVERB
!!     MODD_PARAMETERS : NUNDEF
!!     
!!    REFERENCE
!!    ---------
!!      
!!    AUTHOR
!!    ------
!!	 F. Gheusi and J. Stein  * Meteo France *
!!
!!    MODIFICATIONS
!!    -------------
!!     J. Stein  Jan. 2001  add supplementary starts and spare some memory
!!     October 2009 (G. Tanguy) add ILENCH=LEN(YCOMMENT) after
!!                              change of YCOMMENT
!!     Mai 2016 (G.Delautier) replace LG?M by LG?T
!!  Philippe Wautelet: 05/2016-04/2018: new data structures and calls for I/O
!  P. Wautelet 07/02/2019: force TYPE to a known value for IO_File_add2list
!  P. Wautelet 11/04/2019: bugfix: nullify TZTRACFILE when appropriate
!  P. Wautelet 15/02/2024: add time dimension for Lagrangian trajectories
!-------------------------------------------------------------------------------
!
!*       0.     DECLARATIONS
!               ------------
!
USE MODD_CONF
USE MODD_GRID_n
use modd_field,            only: NMNHDIM_NI, NMNHDIM_NJ, NMNHDIM_LEVEL, NMNHDIM_TRAJ_TIME, tfieldmetadata, TYPEREAL
USE MODD_FIELD_n
USE MODD_IO,               ONLY: TFILEDATA
USE MODD_LUNIT_n
USE MODD_NSV,              ONLY: NSV_LGBEG, NSV_LGEND
USE MODD_PARAMETERS
USE MODD_STO_FILE
USE MODD_TYPE_DATE
USE MODD_VAR_ll
!
USE MODE_IO_FIELD_READ,    only: IO_Field_read
USE MODE_IO_FIELD_WRITE,   only: IO_Field_create, IO_Field_write, IO_Format_write_select
USE MODE_IO_FILE,          only: IO_File_close, IO_File_open
USE MODE_IO_MANAGE_STRUCT, ONLY: IO_File_add2list
USE MODE_ll
USE MODE_MSG
!
USE MODI_SHUMAN
!
IMPLICIT NONE
!
!*       0.1   declarations of arguments
!
TYPE(TFILEDATA),   INTENT(IN) :: TPFILE ! Output file
!
!*       0.2   declarations of local variables
!
INTEGER                            :: NIU,NJU,NKU
INTEGER                            :: JFILECUR
INTEGER                            :: JLOOP
REAL                               :: ZXOR,ZYOR,ZDX,ZDY
REAL, ALLOCATABLE, DIMENSION(:,:,:):: ZX0, ZY0, ZZ0        ! origin of the 
       ! particules colocated with the mesh-grid points read in the file
REAL, ALLOCATABLE, DIMENSION(:,:,:):: ZX00, ZY00, ZZ00, ZZL ! cumulative
       ! origin for more than one restart of the tracers 
REAL, ALLOCATABLE, DIMENSION(:,:,:):: ZTH0          ! same fields 
       ! for Theta as for the coordinates of the origin
REAL, ALLOCATABLE, DIMENSION(:,:,:):: ZRV0          ! same fields 
       ! for Rv as for the coordinates of the origin
REAL, ALLOCATABLE, DIMENSION(:,:,:):: ZWORK1,ZWORK2,ZWORK3       
TYPE(DATE_TIME)                    :: TDTCUR_START
LOGICAL                            :: GLFI, GNC4
LOGICAL                            :: GSTART
REAL                               :: ZXMAX,ZYMAX,ZZMAX  ! domain extrema
TYPE(TFIELDMETADATA)               :: TZFIELD
TYPE(TFIELDMETADATA)               :: TZFIELD_X0, TZFIELD_Y0, TZFIELD_Z0
TYPE(TFIELDMETADATA)               :: TZFIELD_TH0, TZFIELD_RV0
TYPE(TFILEDATA),POINTER            :: TZTRACFILE
!
!-------------------------------------------------------------------------------

CALL IO_FORMAT_WRITE_SELECT( TPFILE, GLFI, GNC4 )
if ( GLFI ) CALL PRINT_MSG( NVERB_ERROR, 'COMPUTE_R00', 'LFI fileformat not supported for Lagrangian trajectories' )
!
!*       1.0    INITIALIZATION
!               --------------
!
TZTRACFILE => NULL()
!
!-------------------------------------------------------------------------------
!
!*       3.0    ALLOCATIONS OF THE ARRAYS AND CONVERSIONS
!               -----------------------------------------
!
!
NIU=SIZE(XZZ,1)
NJU=SIZE(XZZ,2)
NKU=SIZE(XZZ,3)
!
ALLOCATE(ZX0(NIU,NJU,NKU))
ALLOCATE(ZY0(NIU,NJU,NKU))
ALLOCATE(ZZ0(NIU,NJU,NKU))
ALLOCATE(ZWORK1(NIU,NJU,NKU))
ALLOCATE(ZWORK2(NIU,NJU,NKU))
ALLOCATE(ZWORK3(NIU,NJU,NKU))
ALLOCATE(ZX00(NIU,NJU,NKU))
ALLOCATE(ZY00(NIU,NJU,NKU))
ALLOCATE(ZZ00(NIU,NJU,NKU))
ALLOCATE(ZZL(NIU,NJU,NKU))
ALLOCATE(ZTH0(NIU,NJU,NKU))
ALLOCATE(ZRV0(NIU,NJU,NKU))

ALLOCATE( TLAGR_DATES(NTRAJSTLG) )

! initial values
ZXOR=0.5 * (XXHAT(2)+XXHAT(3)) 
ZYOR=0.5 * (XYHAT(2)+XYHAT(3))
ZDX= XXHAT(3)-XXHAT(2)
ZDY= XYHAT(3)-XYHAT(2)
ZZL=MZF(XZZ)
ZZL(:,:,NKU)=2*XZZ(:,:,NKU)-ZZL(:,:,NKU-1)
ZXMAX=ZXOR+(NIU-3)*ZDX
ZYMAX=ZYOR+(NJU-3)*ZDY
ZZMAX=ZZL(2,2,NKU-1)
!  conversion from km to meters
ZXOR=ZXOR*1.E-3
ZYOR=ZYOR*1.E-3
ZDX=ZDX*1.E-3
ZDY=ZDY*1.E-3
ZZL(:,:,:)=ZZL(:,:,:)*1.E-3
ZXMAX=ZXMAX*1.E-3
ZYMAX=ZYMAX*1.E-3
ZZMAX=ZZMAX*1.E-3
!
ZX00(:,:,:)=XSVT(:,:,:,NSV_LGBEG)*1.E-3   ! ZX0 in km
ZY00(:,:,:)=XSVT(:,:,:,NSV_LGBEG+1)*1.E-3 ! ZY0 in km
ZZ00(:,:,:)=XSVT(:,:,:,NSV_LGEND)*1.E-3   ! ZZ0 in km
!
IF (L2D) THEN
  WHERE ( ZX00<ZXOR .OR. ZX00>ZXMAX .OR. &
          ZZ00>ZZMAX)
    ZX00=NSPVAL
    ZZ00=NSPVAL
  END WHERE
ELSE
  WHERE ( ZX00<ZXOR .OR. ZX00>ZXMAX .OR. &
          ZY00<ZYOR .OR. ZY00>ZYMAX .OR. &
	      ZZ00>ZZMAX)
    ZX00=NSPVAL
    ZY00=NSPVAL
    ZZ00=NSPVAL
  END WHERE
END IF
!
! Create the metadata of the fields (has to be done only once)
TZFIELD_X0 = TFIELDMETADATA( &
  CMNHNAME   = 'X0',         &
  CSTDNAME   = '',           &
  CLONGNAME  = 'X0',         &
  CUNITS     = 'km',         &
  CDIR       = 'XY',         &
  CCOMMENT   = 'X_Y_Z_X0',   &
  NGRID      = 1,            &
  NTYPE      = TYPEREAL,     &
  NDIMS      = 4,            &
  NDIMLIST   = [ NMNHDIM_NI, NMNHDIM_NJ, NMNHDIM_LEVEL, NMNHDIM_TRAJ_TIME ] )
CALL IO_FIELD_CREATE( TPFILE, TZFIELD_X0 )

TZFIELD_Y0 = TFIELDMETADATA( &
  CMNHNAME   = 'Y0',         &
  CSTDNAME   = '',           &
  CLONGNAME  = 'Y0',         &
  CUNITS     = 'km',         &
  CDIR       = 'XY',         &
  CCOMMENT   = 'X_Y_Z_Y0',   &
  NGRID      = 1,            &
  NTYPE      = TYPEREAL,     &
  NDIMS      = 4,            &
  NDIMLIST   = [ NMNHDIM_NI, NMNHDIM_NJ, NMNHDIM_LEVEL, NMNHDIM_TRAJ_TIME ] )
CALL IO_FIELD_CREATE( TPFILE, TZFIELD_Y0 )

TZFIELD_Z0 = TFIELDMETADATA( &
  CMNHNAME   = 'Z0',         &
  CSTDNAME   = '',           &
  CLONGNAME  = 'Z0',         &
  CUNITS     = 'km',         &
  CDIR       = 'XY',         &
  CCOMMENT   = 'X_Y_Z_Z0',   &
  NGRID      = 1,            &
  NTYPE      = TYPEREAL,     &
  NDIMS      = 4,            &
  NDIMLIST   = [ NMNHDIM_NI, NMNHDIM_NJ, NMNHDIM_LEVEL, NMNHDIM_TRAJ_TIME ] )
CALL IO_FIELD_CREATE( TPFILE, TZFIELD_Z0 )

TZFIELD_TH0 = TFIELDMETADATA(   &
  CMNHNAME   = 'TH0',           &
  CSTDNAME   = '',              &
  CLONGNAME  = 'TH0',           &
  CUNITS     = 'K',             &
  CDIR       = 'XY',            &
  CCOMMENT   = 'X_Y_Z_'//'TH0', &
  NGRID      = 1,               &
  NTYPE      = TYPEREAL,        &
  NDIMS      = 4,               &
  NDIMLIST   = [ NMNHDIM_NI, NMNHDIM_NJ, NMNHDIM_LEVEL, NMNHDIM_TRAJ_TIME ] )
CALL IO_FIELD_CREATE( TPFILE, TZFIELD_TH0 )

TZFIELD_RV0 = TFIELDMETADATA(   &
  CMNHNAME   = 'RV0',           &
  CSTDNAME   = '',              &
  CLONGNAME  = 'RV0',           &
  CUNITS     = 'g kg-1',        &
  CDIR       = 'XY',            &
  CCOMMENT   = 'X_Y_Z_'//'RV0', &
  NGRID      = 1,               &
  NTYPE      = TYPEREAL,        &
  NDIMS      = 4,               &
  NDIMLIST   = [ NMNHDIM_NI, NMNHDIM_NJ, NMNHDIM_LEVEL, NMNHDIM_TRAJ_TIME ] )
CALL IO_FIELD_CREATE( TPFILE, TZFIELD_RV0 )

!-------------------------------------------------------------------------------
!
!*       4.0    COMPUTE THE ORIGIN STEP BY STEP
!               -------------------------------
!
!
! General loop for the files where a reinitialisation of the tracers 
! is performed
DO JFILECUR=1,NFILES
  !
  TZTRACFILE => NULL()
  CALL IO_File_add2list(TZTRACFILE,CFILES(NBRFILES(JFILECUR)),'MNH','READ',KLFITYPE=2,KLFIVERB=NVERB)
  CALL IO_File_open(TZTRACFILE)
!
!*       4.1  check if this file is a start instant
!
  GSTART=.FALSE.
  DO JLOOP=1,NFILES
    IF (NBRFILES(JFILECUR)==NSTART_SUPP(JLOOP) .OR. JFILECUR==NFILES) THEN
      NNBR_START=NNBR_START-1
      GSTART=.TRUE.
      EXIT
    END IF
  ENDDO

  IF (GSTART) THEN
!
!*       4.2 read the potential temp or the water vapor at the start instant      
!
    CALL IO_Field_read(TZTRACFILE,'DTCUR',TDTCUR_START)
    TLAGR_DATES(NNBR_START+1) = TDTCUR_START
    !
    CALL IO_Field_read(TZTRACFILE,'THT',ZTH0(:,:,:))
    !
    CALL IO_Field_read(TZTRACFILE,'RVT',ZRV0(:,:,:))
    ZRV0(:,:,:)=ZRV0(:,:,:)*1.E+3  ! ZRV0 in g/kg
    !
!
!*       4.3  store the X0,Y0,Z0 field for the current start before 
!             computing the new origin
!
    PRINT *,'INBR_START',NNBR_START,' NBRFILES(JFILECUR)',NBRFILES(JFILECUR)
    CALL IO_Field_write( TPFILE, TZFIELD_X0, RESHAPE( ZX00(:,:,:), [ SHAPE(ZX00), 1 ] ), KOFFSET = [0, 0, 0, NNBR_START ] )
    CALL IO_Field_write( TPFILE, TZFIELD_Y0, RESHAPE( ZY00(:,:,:), [ SHAPE(ZY00), 1 ] ), KOFFSET = [0, 0, 0, NNBR_START ] )
    CALL IO_Field_write( TPFILE, TZFIELD_Z0, RESHAPE( ZZ00(:,:,:), [ SHAPE(ZZ00), 1 ] ), KOFFSET = [0, 0, 0, NNBR_START ] )
!
!*       4.6   compute and store potential temp and water vapor at the origin
!
    CALL INTERPXYZ( ZX00, ZY00, ZZ00, ZTH0, ZWORK1 )
    CALL INTERPXYZ( ZX00, ZY00, ZZ00, ZRV0, ZWORK2 )

    CALL IO_Field_write( TPFILE, TZFIELD_TH0, RESHAPE( ZWORK1(:,:,:), [ SHAPE(ZWORK1), 1 ] ), KOFFSET = [0, 0, 0, NNBR_START ] )
    CALL IO_Field_write( TPFILE, TZFIELD_RV0, RESHAPE( ZWORK2(:,:,:), [ SHAPE(ZWORK2), 1 ] ), KOFFSET = [0, 0, 0, NNBR_START ] )
  END IF
!
!*       4.4   compute the origin of the particules using one more segment
!
  IF (JFILECUR /= NFILES) THEN
    TZFIELD = TFIELDMETADATA(&
      CMNHNAME   = 'LGX',    &
      CSTDNAME   = '',       &
      CLONGNAME  = 'LGX',    &
      CUNITS     = 'm',      &
      CDIR       = 'XY',     &
      CCOMMENT   = '',       &
      NGRID      = 1,        &
      NTYPE      = TYPEREAL, &
      NDIMS      = 3,        &
      LTIMEDEP   = .TRUE.    )
    CALL COMPAT_OLDFILE()
    CALL IO_Field_read(TZTRACFILE,TZFIELD,ZX0)
    ZX0(:,:,:)=ZX0(:,:,:)*1.E-3   ! ZX0 in km
    !
    TZFIELD%CMNHNAME   = 'LGY'
    TZFIELD%CLONGNAME  = 'LGY'
    CALL COMPAT_OLDFILE()
    CALL IO_Field_read(TZTRACFILE,TZFIELD,ZY0)
    ZY0(:,:,:)=ZY0(:,:,:)*1.E-3   ! ZY0 in km
    !
    TZFIELD%CMNHNAME   = 'LGZ'
    TZFIELD%CLONGNAME  = 'LGZ'
    CALL COMPAT_OLDFILE()
    CALL IO_Field_read(TZTRACFILE,TZFIELD,ZZ0)
    ZZ0(:,:,:)=ZZ0(:,:,:)*1.E-3   ! ZZ0 in km
    !
    ! old position of the set of particles
    ZWORK1=ZX00
    ZWORK2=ZY00
    ZWORK3=ZZ00
    !
    IF (L2D) THEN
      CALL INTERPXYZ(ZWORK1,ZWORK2,ZWORK3,         &
                     ZX0,ZX00,ZZ0,ZZ00             )
    ELSE
      CALL INTERPXYZ(ZWORK1,ZWORK2,ZWORK3,         &
                     ZX0,ZX00,ZY0,ZY00,ZZ0,ZZ00    )
    END IF
    !
    IF (L2D) THEN
      WHERE ( ZX00<ZXOR .OR. ZX00>ZXMAX .OR. &
              ZZ00>ZZMAX)
        ZX00=NSPVAL
        ZZ00=NSPVAL
      END WHERE
    ELSE
      WHERE ( ZX00<ZXOR .OR. ZX00>ZXMAX .OR. &
              ZY00<ZYOR .OR. ZY00>ZYMAX .OR. &
              ZZ00>ZZMAX)
        ZX00=NSPVAL
        ZY00=NSPVAL
        ZZ00=NSPVAL
      END WHERE
    END IF
    !
  END IF
!
!*       4.5   close the input file
!
  CALL IO_File_close(TZTRACFILE)
!
END DO
!
PRINT*, ' '
PRINT*, 'DIAG AFTER ORIGIN COMPUTATIONS AND STORAGE'
!
!-------------------------------------------------------------------------------
!
!
CONTAINS
!
!
!-------------------------------------------------------------------------------
!
!
SUBROUTINE INTERPXYZ(PX,PY,PZ,PIN1,POUT1,PIN2,POUT2,PIN3,POUT3)
!
!
!*      0. DECLARATIONS
!          ------------
!
!*       0.1  declaration of arguments
!
REAL, INTENT(IN),  DIMENSION(:,:,:)           :: PX,PY,PZ
REAL, INTENT(IN),  DIMENSION(:,:,:)           :: PIN1
REAL, INTENT(OUT), DIMENSION(:,:,:)           :: POUT1
REAL, INTENT(IN),  DIMENSION(:,:,:), OPTIONAL :: PIN2,PIN3
REAL, INTENT(OUT), DIMENSION(:,:,:), OPTIONAL :: POUT2,POUT3   
!
!*       0.2  declaration of local variables
!
INTEGER  :: JI,JJ,JK,JKK    ! loop index
INTEGER  :: II,IJ,IK        ! grid index for the interpolation
REAL     :: ZXREL,ZYREL     ! fractional grid index for the interpolation
REAL, DIMENSION(SIZE(PIN1,3)) :: ZZLXY ! vertical grid at the interpolated point
REAL     :: ZEPS1,ZEPS2,ZEPS3          ! coeff. for the interpolation
REAL     :: ZX,ZY,ZZ
LOGICAL  :: GEXT
!
!-------------------------------------------------------------------------------
!
DO JK=1,NKU
  DO JJ=1,NJU
    DO JI=1,NIU
      !
      ZX=PX(JI,JJ,JK) 
      ZY=PY(JI,JJ,JK)
      ZZ=PZ(JI,JJ,JK)
      !
      ! remove external points
      IF (L2D) THEN
        GEXT=(ZX==NSPVAL).OR.(ZZ==NSPVAL)
      ELSE
        GEXT=(ZX==NSPVAL).OR.(ZY==NSPVAL).OR.(ZZ==NSPVAL)
      END IF
      IF (GEXT) THEN
        POUT1(JI,JJ,JK) = NSPVAL
        IF (PRESENT(PIN2)) THEN
          POUT2(JI,JJ,JK) = NSPVAL
        END IF
        IF (PRESENT(PIN3)) THEN
          POUT3(JI,JJ,JK) = NSPVAL
        ENDIF
        !
        CYCLE
        !
      END IF
      !
      ZXREL=(ZX-ZXOR)/ZDX+2
      ZYREL=(ZY-ZYOR)/ZDY+2
      !
      II=FLOOR(ZXREL)
      IJ=FLOOR(ZYREL)
      !
      ZEPS1=ZXREL-REAL(II)
      ZEPS2=ZYREL-REAL(IJ)
      IF (L2D) ZEPS2=0.
      !
      DO JKK=1,NKU
        ZZLXY(JKK)=ZEPS2*(ZEPS1*(ZZL(II+1,IJ+1,JKK))+(1-ZEPS1)*(ZZL(II,IJ+1,JKK)))     &
             + (1-ZEPS2)*(ZEPS1*(ZZL(II+1,IJ,JKK))+(1-ZEPS1)*(ZZL(II,IJ,JKK)))
      ENDDO
      !
      IK=999
      DO JKK=2,NKU
        IF (ZZLXY(JKK).GE.ZZ) THEN
          IK=JKK-1
          EXIT 
        ENDIF
      ENDDO
      !
      IF (IK==999) THEN
        PRINT*,'PROBLEM AT POINT',II,IJ
        PRINT*,'XREL, YREL, Z =',ZXREL,ZYREL,ZZ
        PRINT*,'ZZLXY(NKU)',ZZLXY(NKU)
!callabortstop
        CALL PRINT_MSG(NVERB_FATAL,'GEN','COMPUTE_R00','')
      END IF 
      !
      ZEPS3=(ZZ-ZZLXY(IK))/(ZZLXY(IK+1)-ZZLXY(IK))
      !
      POUT1(JI,JJ,JK) =                                                       & 
        ZEPS3 *                                                               &
      (  ZEPS2*(ZEPS1*(PIN1(II+1,IJ+1,IK+1))+(1-ZEPS1)*(PIN1(II,IJ+1,IK+1)))  &
       + (1-ZEPS2)*(ZEPS1*(PIN1(II+1,IJ,IK+1))+(1-ZEPS1)*(PIN1(II,IJ,IK+1)))  &
      )                                                                       & 
      + (1-ZEPS3) *                                                           &
      (  ZEPS2*(ZEPS1*(PIN1(II+1,IJ+1,IK))+(1-ZEPS1)*(PIN1(II,IJ+1,IK)))      &
       + (1-ZEPS2)*(ZEPS1*(PIN1(II+1,IJ,IK))+(1-ZEPS1)*(PIN1(II,IJ,IK)))      &
      )
      IF (PRESENT(POUT2)) THEN
        POUT2(JI,JJ,JK) =                                                     & 
          ZEPS3 *                                                             &
        (  ZEPS2*(ZEPS1*(PIN2(II+1,IJ+1,IK+1))+(1-ZEPS1)*(PIN2(II,IJ+1,IK+1)))&
         + (1-ZEPS2)*(ZEPS1*(PIN2(II+1,IJ,IK+1))+(1-ZEPS1)*(PIN2(II,IJ,IK+1)))&
        )                                                                     & 
        + (1-ZEPS3) *                                                         &
        (  ZEPS2*(ZEPS1*(PIN2(II+1,IJ+1,IK))+(1-ZEPS1)*(PIN2(II,IJ+1,IK)))    &
         + (1-ZEPS2)*(ZEPS1*(PIN2(II+1,IJ,IK))+(1-ZEPS1)*(PIN2(II,IJ,IK)))    &
        )
      ENDIF
        !
      IF (PRESENT(POUT3)) THEN
        POUT3(JI,JJ,JK) =                                                     & 
          ZEPS3 *                                                             &
        (  ZEPS2*(ZEPS1*(PIN3(II+1,IJ+1,IK+1))+(1-ZEPS1)*(PIN3(II,IJ+1,IK+1)))&
         + (1-ZEPS2)*(ZEPS1*(PIN3(II+1,IJ,IK+1))+(1-ZEPS1)*(PIN3(II,IJ,IK+1)))&
        )                                                                     &
        + (1-ZEPS3) *                                                         &
        (  ZEPS2*(ZEPS1*(PIN3(II+1,IJ+1,IK))+(1-ZEPS1)*(PIN3(II,IJ+1,IK)))    &
         + (1-ZEPS2)*(ZEPS1*(PIN3(II+1,IJ,IK))+(1-ZEPS1)*(PIN3(II,IJ,IK)))    &
        )
      ENDIF
      !
    END DO
  END DO
END DO
!
!-------------------------------------------------------------------------------
!
!
END SUBROUTINE INTERPXYZ
!
SUBROUTINE COMPAT_OLDFILE()
  IF ( TZTRACFILE%NMNHVERSION(1)<5 .OR. ( TZTRACFILE%NMNHVERSION(1)==5 .AND. TZTRACFILE%NMNHVERSION(2)<6 ) ) THEN
    TZFIELD%CMNHNAME  = TRIM(TZFIELD%CMNHNAME) // 'T'
    TZFIELD%CLONGNAME = TRIM(TZFIELD%CMNHNAME) // 'T'
  END IF
END SUBROUTINE COMPAT_OLDFILE
!-------------------------------------------------------------------------------
!
END SUBROUTINE COMPUTE_R00 

END MODULE MODE_COMPUTE_R00
