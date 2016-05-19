!     #########
      SUBROUTINE READ_DIRECT(HPROGRAM,HSCHEME,HSUBROUTINE,HFILENAME,HFIELD)
!     #########################################################
!
!!**** *READ_DIRECT1* reads a latlon file and call treatment subroutine
!!
!!    PURPOSE
!!    -------
!!
!!    METHOD
!!    ------
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
!!    V. Masson        Meteo-France
!!
!!    MODIFICATION
!!    ------------
!!
!!    Original    11/09/95
!!
!! V. Masson, March 2010     Optimization of some lat/lon boundaries computations
!----------------------------------------------------------------------------
!
!*    0.     DECLARATION
!            -----------
!
USE MODD_PGD_GRID,   ONLY : LLATLONMASK, XMESHLENGTH
!
USE MODD_ARCH, ONLY : LITTLE_ENDIAN_ARCH
!
USE MODI_GET_LUOUT
USE MODI_OPEN_NAMELIST
USE MODI_CLOSE_NAMELIST
USE MODI_OPEN_FILE
USE MODI_CLOSE_FILE
USE MODI_READHEAD
USE MODI_INI_SSOWORK
USE MODI_PT_BY_PT_TREATMENT
USE MODE_CHAR2REAL
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
USE MODI_ABOR1_SFX
!
USE MODI_REFRESH_PGDWORK
USE MODD_CSTS ,ONLY : XSURF_EPSILON
!
IMPLICIT NONE
!
!*    0.1    Declaration of arguments
!            ------------------------
!
 CHARACTER(LEN=6),  INTENT(IN) :: HPROGRAM      ! Type of program
 CHARACTER(LEN=6),  INTENT(IN) :: HSCHEME       ! Scheme treated
 CHARACTER(LEN=6),  INTENT(IN) :: HSUBROUTINE   ! Name of the subroutine to call
 CHARACTER(LEN=28), INTENT(IN) :: HFILENAME     ! Name of the field file.
 CHARACTER(LEN=20), INTENT(IN) :: HFIELD        ! Name of the field.
!
!*    0.2    Declaration of local variables read in the data file head
!            ---------------------------------------------------------
!
REAL    :: ZGLBLATMIN                 ! minimum latitude of data box in the file
REAL    :: ZGLBLONMIN                 ! minimum longitude of data box in the file
REAL    :: ZGLBLATMAX                 ! maximum latitude of data box in the file
REAL    :: ZGLBLONMAX                 ! maximum longitude of data box in the file
INTEGER :: INBLINE                    ! number of latitude rows (number of lines
INTEGER :: INBCOL                     ! number of longitude rows (number of columns)
REAL    :: ZNODATA                    ! value below which data are not considered
!
!*    0.3    Declaration of local variables
!            ------------------------------
!
INTEGER :: IGLB, IGLBHDR              ! logical units
INTEGER :: ILUOUT                     ! output listing logical unit
INTEGER :: IERR                       ! return codes
!
 CHARACTER(LEN=28) :: YFILENAME        ! Name of the field file without header
 CHARACTER(LEN=28) :: YFILEHDR         ! Name of the field file header
!
 CHARACTER(LEN=7)  :: YTYPE            ! type of numerical field stored in the
!                                     ! direct access file ('INTEGER','REAL   ')
INTEGER           :: IBITS            ! number of bits of a record in the
!                                     ! direct access file (16,32,64)
INTEGER :: JLOOP                      ! loop index
 CHARACTER(LEN=100):: YSTRING          ! string
 CHARACTER(LEN=88 ):: YSTRING1         ! part of string STRING
INTEGER           :: IINDEX           ! index of a character in string STRING1
!
REAL    :: ZDLAT                      ! latitude mesh in the data file
REAL    :: ZDLON                      ! longitude mesh in the data file
INTEGER :: JLINE                      ! index of line
INTEGER :: JCOL                       ! index of column
INTEGER, DIMENSION(2) :: ICOL1, ICOL2 ! limits of index of columns
INTEGER               :: ILINE1,ILINE2! limits of index of lines
INTEGER               :: ICOL         ! number of columns in mask domain
INTEGER               :: ICOLINDEX    ! column index in record
REAL, DIMENSION(:), POINTER :: ZLAT   ! latitude of data points
REAL, DIMENSION(:), POINTER :: ZLON   ! longitude of data points
!
INTEGER :: JLON, JLAT                 ! loop counters
REAL    :: ZLONMIN                    ! minimum longitude of mask mesh
REAL    :: ZLONMAX                    ! maximum longitude of mask mesh
REAL    :: ZLATMIN                    ! minimum latitude of mask mesh
REAL    :: ZLATMAX                    ! maximum latitude of mask mesh
REAL    :: ZSHIFT                     ! shift on longitudes
REAL    :: ZFACT                      ! Factor integer to real
!
 CHARACTER,        DIMENSION(:), ALLOCATABLE :: IVALUE8 ! value of a data point
 CHARACTER(LEN=2), DIMENSION(:), ALLOCATABLE :: IVALUE16 ! value of a data point
 CHARACTER(LEN=4), DIMENSION(:), ALLOCATABLE :: IVALUE32R ! value of a data point
INTEGER (KIND=4), DIMENSION(:), ALLOCATABLE :: IVALUE32 ! value of a data point
INTEGER (KIND=8), DIMENSION(:), ALLOCATABLE :: IVALUE64 ! value of a data point
REAL    (KIND=4), DIMENSION(:), ALLOCATABLE :: ZVALUE32 ! value of a data point
 CHARACTER(LEN=8), DIMENSION(:), ALLOCATABLE :: ZVALUE64 ! value of a data point
!
REAL, DIMENSION(:), ALLOCATABLE   :: ZVALUE             ! value of a record of data points
!
REAL, DIMENSION(:), ALLOCATABLE :: ZVALUE_WORK        ! value of a valid data points 
REAL, DIMENSION(:), ALLOCATABLE   :: ZLAT_WORK          ! latitude  of a valid data points 
REAL, DIMENSION(:), ALLOCATABLE   :: ZLON_WORK          ! longitude of a valid data points 
INTEGER                           :: IWORK              ! index of these data
LOGICAL                           :: GSWAP              ! T: swap has been done
!
!
INTEGER          :: IRECLENGTH        ! record length
INTEGER          :: IREC              ! record number
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!----------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('READ_DIRECT',0,ZHOOK_HANDLE)
 CALL GET_LUOUT(HPROGRAM,ILUOUT)
!
!*    1.     Openning of global field
!            ------------------------
!
!*    1.1    Logical unit attributions
!            -------------------------
!
YFILENAME=ADJUSTL(ADJUSTR(HFILENAME)//'.dir')
YFILEHDR =ADJUSTL(ADJUSTR(HFILENAME)//'.hdr')
!
!*    1.2    Openning of header
!            ------------------
!
 CALL OPEN_NAMELIST(HPROGRAM,IGLBHDR,YFILEHDR)
!
!*    1.3    Reading in header of direct access characteristics
!            --------------------------------------------------
!
DO JLOOP=1,9
  READ(IGLBHDR,'(A100)') YSTRING
  IF (YSTRING(1:10)=='recordtype') EXIT
END DO
!
REWIND(IGLBHDR)
!
YSTRING1=YSTRING(12:100)
!
!* string analysis
!
IINDEX=INDEX(YSTRING1,'n')  ! n for integer
IF (IINDEX/=0) THEN
  YTYPE='INTEGER'
ELSE
  YTYPE='REAL   '
END IF
IINDEX=INDEX(YSTRING1,'8')
IF (IINDEX/=0) IBITS=8
IINDEX=INDEX(YSTRING1,'1')
IF (IINDEX/=0) IBITS=16
IINDEX=INDEX(YSTRING1,'3')
IF (IINDEX/=0) IBITS=32
IINDEX=INDEX(YSTRING1,'4')
IF (IINDEX/=0) IBITS=64
!
IF(YTYPE=='INTEGER')THEN
  IF(HFIELD=='CTI'.OR.HFIELD=='sand fraction'.OR. &
     HFIELD=='clay fraction'.OR.HFIELD=='organic carbon')THEN
    ZFACT=100.0
  ELSEIF (HFIELD=='water depth') THEN
    ZFACT=10.0
  ELSE
    ZFACT=1.0
  ENDIF
ELSE
  ZFACT=1.0
ENDIF
!
!----------------------------------------------------------------------------
!
!*    2.     Reading of the global field
!            ---------------------------
!
!*    2.1    Head of data file
!            -----------------
!
 CALL READHEAD(IGLBHDR,ZGLBLATMIN,ZGLBLATMAX,ZGLBLONMIN,ZGLBLONMAX, &
                INBLINE,INBCOL,ZNODATA,ZDLAT,ZDLON,ZLAT,ZLON,IERR)  
IF (IERR/=0) CALL ABOR1_SFX('READ_DIRECT: PB IN FILE HEADER')
!
!*    2.2    Closing of header
!            -----------------
!
 CALL CLOSE_NAMELIST(HPROGRAM,IGLBHDR)
!
!*    2.3    Dimension of work arrays
!            ------------------------
!
ALLOCATE(ZLAT_WORK  (INBCOL))
ALLOCATE(ZLON_WORK  (INBCOL))
ALLOCATE(ZVALUE_WORK(INBCOL))
!
!----------------------------------------------------------------------------
!
!*    3.     Adapt subgrid mesh to input file resolution
!            -------------------------------------------
!
IF (HSUBROUTINE=='A_OROG') CALL INI_SSOWORK(XMESHLENGTH,ZDLAT,ZDLON)
!
!----------------------------------------------------------------------------
!
!*    7.     Openning of direct access file
!            ------------------------------
!
!*    7.1    Record length
!            -------------
!
IRECLENGTH = IBITS/8 * INBCOL
ALLOCATE (ZVALUE(INBCOL))
ALLOCATE (IVALUE8 (INBCOL))
ALLOCATE (IVALUE16(INBCOL))
ALLOCATE (IVALUE32(INBCOL))
ALLOCATE (IVALUE32R(INBCOL))
ALLOCATE (IVALUE64(INBCOL))
ALLOCATE (ZVALUE32(INBCOL))
ALLOCATE (ZVALUE64(INBCOL))
!
!*    7.2    Openning of direct access file
!            ------------------------------
!
 CALL OPEN_FILE(HPROGRAM,IGLB,YFILENAME,'UNFORMATTED',           &
                 HACTION='READ',HACCESS='DIRECT',KRECL=IRECLENGTH )  
!
!----------------------------------------------------------------------------
!
!*    4.     loop on mask meshes (lat)
!            -------------------
!
GSWAP = .FALSE.
!
JLAT = 0
!
DO 
!
  JLAT = JLAT + 1
  IF (JLAT==361) EXIT
!
  IF ( .NOT. ANY(LLATLONMASK(:,JLAT)) ) CYCLE
!
  ZLATMIN = (JLAT-180)/2. - 0.5
  ZLATMAX = (JLAT-180)/2.
!
!----------------------------------------------------------------------------
!
!*    5.     index limits on latitude
!            ------------------------
!
  ILINE1=MAX(MIN(INT((ZGLBLATMAX-ZDLAT/2.-ZLATMAX)/ZDLAT+1.),INBLINE),0)+1
  ILINE2=MAX(MIN(INT((ZGLBLATMAX-ZDLAT/2.-ZLATMIN)/ZDLAT+1.),INBLINE),0)
  IF ( .NOT. ANY(ZLAT(:)<ZLATMAX .AND. ZLAT(:)>=ZLATMIN) ) CYCLE
!
!----------------------------------------------------------------------------
!
!*    8.     Loop on lines
!            -------------
!
  DO JLINE = ILINE1,ILINE2
!
!----------------------------------------------------------------------------
!
!*   10.     Reading in the direct access file
!            ---------------------------------
!
!*   10.1    Record number
!            -------------
!
    IREC=JLINE
! 
!*   10.2    Reading the correct data type and conversion into real
!            ------------------------------------------------------
!
    IF      (YTYPE=='INTEGER' .AND. IBITS== 8) THEN
      READ(IGLB,REC=IREC) IVALUE8(:)
      ZVALUE(:)=IVALUE8(:)
      ! negative values are shifted to positive values according to binary coding
      WHERE (ZVALUE(:)<0.) ZVALUE(:) = NINT(256.+ZVALUE(:))
      !
    ELSE IF (YTYPE=='INTEGER' .AND. IBITS==16) THEN
      READ(IGLB,REC=IREC) IVALUE16(:)
      ZVALUE(:)=IVALUE16(:)
      IF (      ANY(ABS(ZVALUE)>15000)   ) THEN
        IF (GSWAP) CALL ABOR1_SFX('READ_DIRECT: SWAP ALREADY DONE, CANNOT BE REDONE')
        LITTLE_ENDIAN_ARCH = .NOT. LITTLE_ENDIAN_ARCH
        GSWAP = .TRUE.
        WRITE(ILUOUT,*) '*******************************************************************'
        WRITE(ILUOUT,*) 'Architecture of the machine needs to swap LITTLE_ENDIAN_ARCH to ', &
                           LITTLE_ENDIAN_ARCH  
        WRITE(ILUOUT,*) '*******************************************************************'
        JLAT = 0
        CALL REFRESH_PGDWORK
        EXIT   ! rereads the file
      END IF

    ELSE IF (YTYPE=='INTEGER' .AND. IBITS==32) THEN
      READ(IGLB,REC=IREC) IVALUE32(:)
      ZVALUE(:)=IVALUE32(:)
    ELSE IF (YTYPE=='INTEGER' .AND. IBITS==64) THEN
      READ(IGLB,REC=IREC) IVALUE64(:)
      ZVALUE(:)=IVALUE64(:)
    ELSE IF (YTYPE=='REAL   ' .AND. IBITS==32) THEN
      READ(IGLB,REC=IREC) IVALUE32R(:)
      ZVALUE(:)=IVALUE32R(:)
      IF (      ANY(ABS(ZVALUE)>0. .AND. ABS(ZVALUE)<1.E-50) &
           .OR. ANY(ABS(ZVALUE)>1.E20)                       ) THEN
        IF (GSWAP) CALL ABOR1_SFX('READ_DIRECT: SWAP ALREADY DONE, CANNOT BE REDONE')
        LITTLE_ENDIAN_ARCH = .NOT. LITTLE_ENDIAN_ARCH
        GSWAP = .TRUE.
        WRITE(ILUOUT,*) '*******************************************************************'
        WRITE(ILUOUT,*) 'Architecture of the machine needs to swap LITTLE_ENDIAN_ARCH to ', &
                         LITTLE_ENDIAN_ARCH
        WRITE(ILUOUT,*) '*******************************************************************'
        JLAT = 0
        CALL REFRESH_PGDWORK
        EXIT
      END IF                
    ELSE IF (YTYPE=='REAL   ' .AND. IBITS==64) THEN
      READ(IGLB,REC=IREC) ZVALUE64(:)
      ZVALUE(:)=ZVALUE64(:)
      IF (      ANY(ABS(ZVALUE)>0. .AND. ABS(ZVALUE)<1.E-50) &
             .OR. ANY(ABS(ZVALUE)>1.E20)                       ) THEN  
        IF (GSWAP) CALL ABOR1_SFX('READ_DIRECT: SWAP ALREADY DONE, CANNOT BE REDONE')
        LITTLE_ENDIAN_ARCH = .NOT. LITTLE_ENDIAN_ARCH
        GSWAP = .TRUE.
        WRITE(ILUOUT,*) '*******************************************************************'
        WRITE(ILUOUT,*) 'Architecture of the machine needs to swap LITTLE_ENDIAN_ARCH to ', &
                           LITTLE_ENDIAN_ARCH  
        WRITE(ILUOUT,*) '*******************************************************************'
        JLAT = 0
        CALL REFRESH_PGDWORK
        EXIT
      END IF
    ELSE
      CALL ABOR1_SFX('READ_DIRECT1: DATA TYPE NOT SUPPORTED')
    END IF
!
    IF(HFIELD=='CTI')THEN
      WHERE(ZVALUE(:)<0.0)ZVALUE(:)=ZNODATA
    ENDIF
!
    WHERE(ZVALUE(:)/=ZNODATA)ZVALUE(:)=ZVALUE(:)/ZFACT
!
!----------------------------------------------------------------------------
!
!*    4.     loop on mask meshes (lon)
!            -------------------
!
    DO JLON=1,720
!
      IF (.NOT. LLATLONMASK(JLON,JLAT)) CYCLE
!
      ZLONMIN =  JLON     /2. - 0.5
      ZLONMAX =  JLON     /2.
!
!----------------------------------------------------------------------------
!
!*    5.     limits on longitude
!            -------------------
!
!*    5.1    left domain border is set just higher than the global field min. longitude
!            -----------------------------------------------------------------
!
      ZSHIFT = 360. * NINT((ZLONMIN-ZGLBLONMIN-180.*(1-XSURF_EPSILON))/360.)

!
      ZGLBLONMIN = ZGLBLONMIN + ZSHIFT
      ZGLBLONMAX = ZGLBLONMAX + ZSHIFT

!
!
!*    5.2    index limits on longitude
!            -------------------------
!
      ICOL1(1)=MAX(MIN(INT((ZLONMIN-ZGLBLONMIN-ZDLON/2.)/ZDLON+1.),INBCOL),0)+1
      ICOL2(1)=MAX(MIN(INT((ZLONMAX-ZGLBLONMIN-ZDLON/2.)/ZDLON+1.),INBCOL),0)
!
!* Does right domain border goes outside the global field longitude range?
!* Does it then goes into the global field domain by the other side?
!* Then a second part of the global field domain must be considered
!
      ICOL1(2)=1
      ICOL2(2)=MAX(MIN(INT((ZLONMAX-ZGLBLONMIN-ZDLON/2.-360.)/ZDLON+1.),INBCOL),0)
!
!----------------------------------------------------------------------------
!
!*    6.     Loop on longitude limits
!            ------------------------
!
      DO JLOOP=1,2
!
        ICOL = ICOL2(JLOOP) - ICOL1(JLOOP) + 1
!
        IF (ICOL<1) CYCLE
!
!----------------------------------------------------------------------------
!
!*   11.     Loop on columns
!            ---------------
!
        IWORK=0
!
        DO JCOL=1,ICOL
!
!*   11.1    Recovers point value
!            --------------------
!
          ICOLINDEX = JCOL+ICOL1(JLOOP)-1

!
!*   11.2    Test with respect to the 'no data' value
!            ----------------------------------------
!
          IF (ABS(ZVALUE(ICOLINDEX)-ZNODATA)<=1.E-10) CYCLE
!
!
!*   11.3    copy of the correct values in a work array
!            ------------------------------------------
!
          IWORK = IWORK + 1
          ZLAT_WORK  (IWORK) = ZLAT  (JLINE)
          ZLON_WORK  (IWORK) = ZLON  (ICOLINDEX)
          ZVALUE_WORK(IWORK) = ZVALUE(ICOLINDEX)
! 
        END DO
!
!-------------------------------------------------------------------------------
!
!*   12.     Call to the adequate subroutine (point by point treatment)
!            ----------------------------------------------------------
!
          IF (IWORK>0) &
            CALL PT_BY_PT_TREATMENT(ILUOUT, ZLAT_WORK(1:IWORK),ZLON_WORK(1:IWORK), &
                                    ZVALUE_WORK(1:IWORK),                          &
                                    HSUBROUTINE                                    )  
!
!-------------------------------------------------------------------------------
      END DO
!-------------------------------------------------------------------------------
    END DO
!-------------------------------------------------------------------------------
  END DO
!-------------------------------------------------------------------------------
END DO
!
!-------------------------------------------------------------------------------
!

DEALLOCATE(ZLAT)
DEALLOCATE(ZLON)
!
DEALLOCATE(ZLAT_WORK  )
DEALLOCATE(ZLON_WORK  )
DEALLOCATE(ZVALUE_WORK)
!
DEALLOCATE (ZVALUE)
DEALLOCATE (IVALUE8 )
DEALLOCATE (IVALUE16)
DEALLOCATE (IVALUE32)
DEALLOCATE (IVALUE32R)
DEALLOCATE (IVALUE64)
DEALLOCATE (ZVALUE32)
DEALLOCATE (ZVALUE64)
!
 CALL CLOSE_FILE(HPROGRAM,IGLB)
IF (LHOOK) CALL DR_HOOK('READ_DIRECT',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE READ_DIRECT
