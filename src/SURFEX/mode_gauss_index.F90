!##########################
MODULE MODE_GAUSS_INDEX
!##########################
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
USE MODI_ABOR1_SFX
!
IMPLICIT NONE
!
!############################################################################
!
CONTAINS
!
!############################################################################
!
!###############################################################################
SUBROUTINE READ_GAUSS_CONF(HPROGRAM,HFILE,HINDEX_1KM,HINDEX_10KM,HINDEX_100KM, &
                             HINDEX,HRES,KDIM,KNLON,KNLAT,PDLON,PDLAT,PLON,PLAT)  
!###############################################################################
!
!!****  *READ_GAUSS_CONF* - routine to read asked dimenssion
!!
!!    AUTHOR
!!    ------
!!	B. Decharme   *Meteo France*	
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    02/2010
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
USE MODI_OPEN_NAMELIST
USE MODI_CLOSE_NAMELIST
USE MODI_READHEAD
!
IMPLICIT NONE
!
!
!*       0.1   Declarations of arguments
!              -------------------------
!
 CHARACTER(LEN=6),  INTENT(IN)  :: HPROGRAM     ! Type of program
 CHARACTER(LEN=28), INTENT(IN)  :: HFILE        ! Name of the field file.
 CHARACTER(LEN=28), INTENT(IN)  :: HINDEX_1KM   ! index file at 1km
 CHARACTER(LEN=28), INTENT(IN)  :: HINDEX_10KM  ! index file at 10km
 CHARACTER(LEN=28), INTENT(IN)  :: HINDEX_100KM ! index file at 100km
 CHARACTER(LEN=28), INTENT(OUT) :: HINDEX       ! index file
 CHARACTER(LEN=5),  INTENT(OUT) :: HRES         ! resolution
INTEGER,           INTENT(OUT) :: KDIM         ! number of grid point in file
INTEGER,           INTENT(OUT) :: KNLON        ! number of longitude rows in file
INTEGER,           INTENT(OUT) :: KNLAT        ! number of latitude  rows in file
REAL, DIMENSION(:), POINTER    :: PLON         ! longitude of data points
REAL, DIMENSION(:), POINTER    :: PLAT         ! latitude  of data points
REAL,           INTENT(OUT)    :: PDLON        ! longitude mesh in the data file
REAL,           INTENT(OUT)    :: PDLAT        ! latitude mesh in the data file
!
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
 CHARACTER(LEN=28) :: YFILEHDR         ! Name of the field file header
INTEGER :: IERR, IGLBHDR              ! logical units
!
REAL    :: ZGLBLATMIN                 ! minimum latitude of data box in the file
REAL    :: ZGLBLONMIN                 ! minimum longitude of data box in the file
REAL    :: ZGLBLATMAX                 ! maximum latitude of data box in the file
REAL    :: ZGLBLONMAX                 ! maximum longitude of data box in the file
INTEGER :: INBLINE                    ! number of latitude rows (number of lines
INTEGER :: INBCOL                     ! number of longitude rows (number of columns)
REAL    :: ZNODATA                    ! value below which data are not considered
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('MODE_GAUSS_INDEX:READ_GAUSS_CONF',0,ZHOOK_HANDLE)
YFILEHDR =ADJUSTL(ADJUSTR(HFILE)//'.hdr')
 CALL OPEN_NAMELIST(HPROGRAM,IGLBHDR,YFILEHDR)
!
 CALL READHEAD(IGLBHDR,ZGLBLATMIN,ZGLBLATMAX,ZGLBLONMIN,ZGLBLONMAX, &
                INBLINE,INBCOL,ZNODATA,PDLAT,PDLON,PLAT,PLON,IERR)  
IF (IERR/=0) CALL ABOR1_SFX('READ_GAUSS_CONF: PB IN FILE HEADER')
!
 CALL CLOSE_NAMELIST(HPROGRAM,IGLBHDR)
!
!-------------------------------------------------------------------------------
!
KDIM  = INBLINE*INBCOL
KNLON = INBCOL
KNLAT = INBLINE
!
SELECT CASE (KDIM)
       CASE (21600*43200)
            HRES='1km'
            HINDEX=HINDEX_1KM
       CASE (2160*4320)
            HRES='10km'
            HINDEX=HINDEX_10KM
       CASE (216*432)
            HRES='100km'
            HINDEX=HINDEX_100KM
       CASE DEFAULT
            CALL ABOR1_SFX('READ_GAUSS_CONF: RESOLUTION NOT KNOW')
END SELECT               
IF (LHOOK) CALL DR_HOOK('MODE_GAUSS_INDEX:READ_GAUSS_CONF',1,ZHOOK_HANDLE)
!
!############################################################################
END SUBROUTINE READ_GAUSS_CONF
!############################################################################
!
!-------------------------------------------------------------------------------------------
!-------------------------------------------------------------------------------------------
!
!######################################################################################
SUBROUTINE MESH_INDEX_GAUSS(KGRID_PAR,KL,PGRID_PAR,PLAT,PLON,KINDEX,KSSO,KISSOX,KISSOY)
!######################################################################################
!
!!**** *MESH_INDEX_GAUSS* get the grid mesh where point (lat,lon) is located
!!
!!    PURPOSE
!!    -------
!!
!!    AUTHOR
!!    ------
!!
!!    V. Masson         Meteo-France
!!
!!    MODIFICATION
!!    ------------
!!
!!    Original    12/09/95
!!
!!    B. Decharme 04/2009  use XY_GAUSS only if rotated pole and/or stretching
!!                             for fast CPU time
!!
!----------------------------------------------------------------------------
!
!*    0.     DECLARATION
!            -----------
!
USE MODD_GET_MESH_INDEX_GAUSS, ONLY : XXCEN, XYCEN, XYINF, XYSUP, XXINF, XXSUP, &
                                        NNLATI, NNLOPA, XLAPO, XLOPO, XCODIL,     &
                                        LROTSTRETCH, XXMAX  

!
USE MODE_GRIDTYPE_GAUSS
!
IMPLICIT NONE
!
!*    0.1    Declaration of arguments
!            ------------------------
!
INTEGER,                       INTENT(IN)    :: KGRID_PAR ! size of PGRID_PAR
INTEGER,                       INTENT(IN)    :: KL        ! number of points
REAL,    DIMENSION(KGRID_PAR), INTENT(IN)    :: PGRID_PAR ! grid parameters
REAL,    DIMENSION(KL),        INTENT(IN)    :: PLAT      ! latitude of the point  (degrees)
REAL,    DIMENSION(KL),        INTENT(IN)    :: PLON      ! longitude of the point (degrees)
INTEGER, DIMENSION(KL),        INTENT(OUT)   :: KINDEX    ! index of the grid mesh where the point is
!
INTEGER,                OPTIONAL, INTENT(IN)  :: KSSO      ! number of subgrid mesh in each direction
INTEGER, DIMENSION(KL), OPTIONAL, INTENT(OUT) :: KISSOX    ! X index of the subgrid mesh
INTEGER, DIMENSION(KL), OPTIONAL, INTENT(OUT) :: KISSOY    ! Y index of the subgrid mesh
!
!*    0.2    Declaration of other local variables
!            ------------------------------------
!
REAL                              :: ZX, ZXX     ! pseudo longitude of input point
REAL                              :: ZY          ! pseudo latitude  of input point
INTEGER                           :: ILGRID      ! number of grid points
!
INTEGER                           :: JI          ! loop counter in x
INTEGER                           :: JJ          ! loop counter in y
INTEGER                           :: JL          ! loop counter on input points
INTEGER                           :: JGRID       ! loop counter on grid  points
!
INTEGER                           :: ISSO
LOGICAL                           :: LFOUND
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!----------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('MODE_GAUSS_INDEX:MESH_INDEX_GAUSS',0,ZHOOK_HANDLE)
KINDEX = -999
IF(PRESENT(KSSO))THEN
  ISSO   = KSSO
  KISSOX = -999
  KISSOY = -999
ELSE
  ISSO   = 0
ENDIF
!
!
IF (.NOT. ALLOCATED(NNLOPA)) THEN
!
!*    1.     Gets parameters of the projection
!            ---------------------------------
!
  CALL GET_GRIDTYPE_GAUSS(PGRID_PAR,NNLATI,KL=ILGRID)
!  
  ALLOCATE(NNLOPA(NNLATI))
  ALLOCATE(XXCEN(ILGRID))
  ALLOCATE(XYCEN(ILGRID))
  CALL GET_GRIDTYPE_GAUSS(PGRID_PAR,NNLATI,XLAPO,XLOPO,XCODIL,NNLOPA(:), &
                            ILGRID,PLAT_XY=XYCEN,PLON_XY=XXCEN             )  
!
!*    2.     Limits of grid meshes in x and y
!            --------------------------------
!
  ALLOCATE(XXINF(ILGRID))
  ALLOCATE(XYINF(ILGRID))
  ALLOCATE(XXSUP(ILGRID))
  ALLOCATE(XYSUP(ILGRID))
!
  CALL GAUSS_GRID_LIMITS(NNLATI,NNLOPA,XXINF,XXSUP,XYINF,XYSUP)
!  
  ALLOCATE(XXMAX(NNLATI))
  JGRID = 0
  DO JJ=1,NNLATI
     DO JI=1,NNLOPA(JJ)
        JGRID=JGRID+1
        XXMAX(JJ)=XXSUP(JGRID)
     ENDDO
  ENDDO
!
!
!*    3.     Find if rotated pole and/or stretching to improve CPU time
!            ----------------------------------------------------------
!
  LROTSTRETCH=.TRUE.
  IF(XCODIL==1.0.AND.XLAPO==90.0.AND.XLOPO==0.0)LROTSTRETCH=.FALSE.
!
END IF
!
!-------------------------------------------------------------------------------
!* loop on input points
DO JL=1,KL
!-------------------------------------------------------------------------------
!
!*    4.     Projection of input points into pseudo coordinates
!            --------------------------------------------------
!
  IF(LROTSTRETCH)THEN          
     CALL XY_GAUSS(XLAPO,XLOPO,XCODIL,PLAT(JL),PLON(JL),ZY,ZX)
  ELSE
     ZX=PLON(JL)
     ZY=PLAT(JL)
  ENDIF  
!  
  JJ    = 0
  JGRID = 0
  LFOUND= .FALSE.
!
!* loop on grid points: latitude
  DO WHILE(.NOT.LFOUND)
    JJ=JJ+1
    IF(JJ>NNLATI)THEN
      CALL ABOR1_SFX('MESH_INDEX_GAUSS: input data point is not on a gauss latitude')
    ENDIF
    !
    !* input data point is not on this circle of latitude : go to the next one
    IF(ZY<XYINF(JGRID+1).OR.ZY>=XYSUP(JGRID+1))THEN
      JGRID = JGRID + NNLOPA(JJ)
    ELSE
    !
    !* loop on longitudes
      DO JI=1,NNLOPA(JJ)
        JGRID = JGRID + 1
        !*Reshifts the longitudes with respect to center of grid mesh
        ZXX = ZX+NINT((XXCEN(JGRID)-ZX)/360.)*360.
        IF(ZXX>=XXMAX(JJ)) ZXX=ZXX-360.
        !* imput point is in this grid mesh
        IF(ZXX>=XXINF(JGRID).AND.ZXX<XXSUP(JGRID))THEN
          KINDEX(JL) = JGRID
!
!*    6.     Localisation of the data points on in the subgrid of this mesh
!            --------------------------------------------------------------
!
          IF (ISSO/=0) THEN
            KISSOX(JL) = 1 + INT( FLOAT(KSSO) * (ZXX-XXINF(JGRID))/(XXSUP(JGRID)-XXINF(JGRID)) )
            KISSOY(JL) = 1 + INT( FLOAT(KSSO) * (ZY -XYINF(JGRID))/(XYSUP(JGRID)-XYINF(JGRID)) )
          END IF
!
          LFOUND=.TRUE.
          EXIT
!
        END IF
!        
      END DO
!      
    END IF
!
  END DO
!
!-------------------------------------------------------------------------------
END DO
IF (LHOOK) CALL DR_HOOK('MODE_GAUSS_INDEX:MESH_INDEX_GAUSS',1,ZHOOK_HANDLE)
!-------------------------------------------------------------------------------
!
!######################################################################################
END SUBROUTINE MESH_INDEX_GAUSS
!############################################################################
!
!-------------------------------------------------------------------------------------------
!-------------------------------------------------------------------------------------------
!
!###################################################################
SUBROUTINE GET_INDEX_GAUSS(KLUOUT,PLON,PLAT,KINDEX,KSSO,KSSOX,KSSOY)
!###################################################################
!
!!****  *GET_INDEX_GAUSS* - routine to read asked dimenssion
!!
!!    AUTHOR
!!    ------
!!	B. Decharme   *Meteo France*	
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    02/2010
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
USE MODD_GET_MESH_INDEX_GAUSS, ONLY : IINDEX_1KM,IINDEX_10KM,IINDEX_100KM, &
                                        IISSOX_1KM,IISSOX_10KM,IISSOX_100KM, &
                                        IISSOY_1KM,IISSOY_10KM,IISSOY_100KM  
                                  
!
USE MODD_PGD_GRID, ONLY : CGRID, XGRID_PAR, NGRID_PAR
!
IMPLICIT NONE
!
!
!*       0.1   Declarations of arguments
!              -------------------------
!
INTEGER,                         INTENT(IN)    :: KLUOUT  ! output listing
REAL,    DIMENSION(:),           INTENT(IN)    :: PLON    ! longitude of the point
REAL,    DIMENSION(:),           INTENT(IN)    :: PLAT    ! latitude of the point
INTEGER, DIMENSION(:),           INTENT(OUT)   :: KINDEX  ! index of the grid mesh where the point is
INTEGER,               OPTIONAL, INTENT(IN)    :: KSSO    ! number of subgrid mesh in each direction
INTEGER, DIMENSION(:), OPTIONAL, INTENT(OUT)   :: KSSOX   ! X index of the subgrid mesh where the point is
INTEGER, DIMENSION(:), OPTIONAL, INTENT(OUT)   :: KSSOY   ! Y index of the subgrid mesh where the point is
!
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
REAL, DIMENSION(:), ALLOCATABLE    :: ZLON,ZLON_W
REAL, DIMENSION(:), ALLOCATABLE    :: ZLAT
!
INTEGER, DIMENSION(:), ALLOCATABLE :: ISSOX
INTEGER, DIMENSION(:), ALLOCATABLE :: ISSOY
!
REAL    :: ZGLBLONMIN  ! minimum longitude of data box in the file
REAL    :: ZGLBLONMAX  ! maximum longitude of data box in the file
!
INTEGER :: ISSO
!
INTEGER :: JLOOP                      ! loop index
INTEGER :: JLINE                      ! index of line
INTEGER :: JCOL                       ! index of column
INTEGER, DIMENSION(2) :: ICOL1, ICOL2 ! limits of index of columns
INTEGER               :: ILINE1,ILINE2! limits of index of lines
INTEGER               :: ICOL         ! number of columns in mask domain
INTEGER               :: ICOLINDEX    ! column index in record
!
INTEGER :: JLON, JLAT, IWORK          ! loop counters
REAL    :: ZLONMIN                    ! minimum longitude of mask mesh
REAL    :: ZLONMAX                    ! maximum longitude of mask mesh
REAL    :: ZLATMIN                    ! minimum latitude of mask mesh
REAL    :: ZLATMAX                    ! maximum latitude of mask mesh
REAL    :: ZSHIFT                     ! shift on longitudes
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('MODE_GAUSS_INDEX:GET_INDEX_GAUSS',0,ZHOOK_HANDLE)
IF (PRESENT(KSSO) .AND. PRESENT(KSSOX) .AND. PRESENT(KSSOY)) THEN
    ISSO = KSSO
    ALLOCATE(ISSOX(SIZE(KINDEX)))
    ALLOCATE(ISSOY(SIZE(KINDEX)))
ELSE
    ISSO = 0
ENDIF
!
!-------------------------------------------------------------------------------
!
ZGLBLONMIN=-180.0
ZGLBLONMAX=180.0
!
ALLOCATE(ZLAT(SIZE(KINDEX)))
ALLOCATE(ZLON(SIZE(KINDEX)))
ALLOCATE(ZLON_W(SIZE(PLON)))
!
ZLON_W(:) = PLON(:)
!
!----------------------------------------------------------------------------
!
IWORK = 0
!
DO JLAT=1,360
!
  ZLATMIN = (JLAT-180)/2. - 0.5
  ZLATMAX = (JLAT-180)/2.
!
  ILINE1=COUNT(PLAT(:)> ZLATMAX)+1
  ILINE2=COUNT(PLAT(:)>=ZLATMIN)
  IF ( .NOT. ANY(PLAT(:)<ZLATMAX .AND. PLAT(:)>=ZLATMIN) ) CYCLE
!
  JLINE = ILINE1

  DO
!
    DO JLON=1,720
!
      ZLONMIN =  JLON     /2. - 0.5
      ZLONMAX =  JLON     /2.
!
      ZSHIFT = 360. * NINT((ZLONMIN-ZGLBLONMIN-180.+1.E-10)/360.)
!
      ZGLBLONMIN = ZGLBLONMIN + ZSHIFT
      ZGLBLONMAX = ZGLBLONMAX + ZSHIFT

!
      ZLON_W(:)    = ZLON_W(:)    + ZSHIFT
!
      ICOL1(1)=COUNT(ZLON_W(:)< ZLONMIN )+1
      ICOL2(1)=COUNT(ZLON_W(:)<=ZLONMAX)
!
      ICOL1(2)=1
      ICOL2(2)=COUNT(ZLON_W(:)+360.<=ZLONMAX)
!
      DO JLOOP=1,2
!
        ICOL = ICOL2(JLOOP) - ICOL1(JLOOP) + 1
!
        IF (ICOL<1) CYCLE
!
        DO JCOL=1,ICOL
!
          ICOLINDEX = JCOL+ICOL1(JLOOP)-1
!
          IWORK       = IWORK + 1
          ZLAT(IWORK) = PLAT(JLINE)
          ZLON(IWORK) = ZLON_W(ICOLINDEX)
! 
        END DO
!
      END DO
!
    END DO
!
    JLINE = JLINE + 1
    IF (JLINE==ILINE2+1) EXIT
  END DO
!
END DO
!
DEALLOCATE(ZLON_W)
!
!-------------------------------------------------------------------------------
!
IF (PRESENT(KSSO) .AND. PRESENT(KSSOX) .AND. PRESENT(KSSOY)) THEN
!        
   CALL MESH_INDEX_GAUSS(NGRID_PAR,SIZE(KINDEX),XGRID_PAR,ZLAT,ZLON,KINDEX,KSSO,ISSOX,ISSOY)
!
   SELECT CASE (SIZE(KINDEX))
       CASE (21600*43200)
            ALLOCATE(IINDEX_1KM(SIZE(KINDEX)))
            ALLOCATE(IISSOX_1KM(SIZE(KINDEX)))
            ALLOCATE(IISSOY_1KM(SIZE(KINDEX)))
            IINDEX_1KM(:)=KINDEX(:)
            IISSOX_1KM(:)=ISSOX(:)
            IISSOY_1KM(:)=ISSOY(:)
       CASE (2160*4320)
            ALLOCATE(IINDEX_10KM(SIZE(KINDEX)))
            ALLOCATE(IISSOX_10KM(SIZE(KINDEX)))
            ALLOCATE(IISSOY_10KM(SIZE(KINDEX)))
            IINDEX_10KM(:)=KINDEX(:)
            IISSOX_10KM(:)=ISSOX(:)
            IISSOY_10KM(:)=ISSOY(:)               
       CASE (216*432)
            ALLOCATE(IINDEX_100KM(SIZE(KINDEX)))
            ALLOCATE(IISSOX_100KM(SIZE(KINDEX)))
            ALLOCATE(IISSOY_100KM(SIZE(KINDEX)))
            IINDEX_100KM(:)=KINDEX(:)
            IISSOX_100KM(:)=ISSOX(:)
            IISSOY_100KM(:)=ISSOY(:)               
       CASE DEFAULT
            CALL ABOR1_SFX('READ_GAUSS_CONF: RESOLUTION NOT KNOW')
   END SELECT               
!   
ELSE
!      
   CALL MESH_INDEX_GAUSS(NGRID_PAR,SIZE(KINDEX),XGRID_PAR,ZLAT,ZLON,KINDEX)
!
   SELECT CASE (SIZE(KINDEX))
       CASE (21600*43200)
            ALLOCATE(IINDEX_1KM(SIZE(KINDEX)))
            IINDEX_1KM(:)=KINDEX(:)
       CASE (2160*4320)
            ALLOCATE(IINDEX_10KM(SIZE(KINDEX)))
            IINDEX_10KM(:)=KINDEX(:)               
       CASE (216*432)
            ALLOCATE(IINDEX_100KM(SIZE(KINDEX)))
            IINDEX_100KM(:)=KINDEX(:)               
       CASE DEFAULT
            CALL ABOR1_SFX('READ_GAUSS_CONF: RESOLUTION NOT KNOW')
   END SELECT               
!   
ENDIF
!
!-------------------------------------------------------------------------------
!
DEALLOCATE(ZLON)
DEALLOCATE(ZLAT)
!
IF (PRESENT(KSSO) .AND. PRESENT(KSSOX) .AND. PRESENT(KSSOY)) THEN
    KSSOX(:) = ISSOX(:)
    KSSOY(:) = ISSOY(:)
    DEALLOCATE(ISSOX)
    DEALLOCATE(ISSOY)
ENDIF              
IF (LHOOK) CALL DR_HOOK('MODE_GAUSS_INDEX:GET_INDEX_GAUSS',1,ZHOOK_HANDLE)
!
!############################################################################
END SUBROUTINE GET_INDEX_GAUSS
!############################################################################
!
!-------------------------------------------------------------------------------------------
!-------------------------------------------------------------------------------------------
!
!##########################################################################
SUBROUTINE READ_INDEX_GAUSS(HPROGRAM,HINDEX,KNLON,KNLAT,KINDEX,KSSOX,KSSOY)
!##########################################################################
!
!!**** *READ_INDEX_GAUSS* read the grid mesh where point (lat,lon) is located
!!                         in a binary file
!!
!!    PURPOSE
!!    -------
!!
!!    AUTHOR
!!    ------
!!
!!    B. Decharme         Meteo-France
!!
!!    MODIFICATION
!!    ------------
!!
!!    Original    02/2010
!-------------------------------------------------------------------------------
!
!*    0.     DECLARATION
!            -----------
!
USE MODD_GET_MESH_INDEX_GAUSS, ONLY : IINDEX_1KM,IINDEX_10KM,IINDEX_100KM, &
                                        IISSOX_1KM,IISSOX_10KM,IISSOX_100KM, &
                                        IISSOY_1KM,IISSOY_10KM,IISSOY_100KM  
                                  
!
USE MODI_OPEN_FILE
USE MODI_CLOSE_FILE
!
IMPLICIT NONE
!
!*    0.1    Declaration of arguments
!            ------------------------
!
 CHARACTER(LEN=6),      INTENT(IN)  :: HPROGRAM
 CHARACTER(LEN=28),     INTENT(IN)  :: HINDEX       ! Index file 
INTEGER,               INTENT(IN)  :: KNLON        ! number of longitude rows in file
INTEGER,               INTENT(IN)  :: KNLAT        ! number of longitude rows in file
INTEGER, DIMENSION(:), INTENT(OUT) :: KINDEX       ! mesh index of all input points
!
INTEGER, DIMENSION(:), OPTIONAL, INTENT(OUT) :: KSSOX ! X submesh index in their mesh of all input points
INTEGER, DIMENSION(:), OPTIONAL, INTENT(OUT) :: KSSOY ! Y submesh index in their mesh of all input points
!
!*    0.2    Declaration of other local variables
!            ------------------------------------
!
 CHARACTER(LEN=28) :: YFILE
!
INTEGER (KIND=4), DIMENSION(:), ALLOCATABLE :: IVALUE ! value of a data point
!
INTEGER           :: JLON, JLAT        ! loop counters
INTEGER           :: IGLB              ! record unit
INTEGER           :: IRECLENGTH        ! record length
INTEGER           :: IREC              ! record number
INTEGER           :: IREC_SSOX         ! record number
INTEGER           :: IREC_SSOY         ! record number
INTEGER           :: IBITS             ! number of bits of a record in the
!                                      ! direct access file
!
INTEGER :: I,J,IERR
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('MODE_GAUSS_INDEX:READ_INDEX_GAUSS',0,ZHOOK_HANDLE)
IBITS      = 32
IRECLENGTH = IBITS/8 * KNLON
!
YFILE=ADJUSTL(HINDEX(:LEN_TRIM(HINDEX))//'.bin')
!
ALLOCATE (IVALUE(KNLON))
!
!-------------------------------------------------------------------------------

 CALL OPEN_FILE(HPROGRAM,IGLB,YFILE,'UNFORMATTED',           &
                 HACTION='READ',HACCESS='DIRECT',KRECL=IRECLENGTH )  
!
!-------------------------------------------------------------------------------
!
I=0
IREC=0
DO JLAT=1,KNLAT
!
   IREC=JLAT
!
   READ(IGLB,REC=IREC,IOSTAT=IERR) IVALUE(:)
   IF(IERR/=0)CALL ABOR1_SFX('READ_INDEX_GAUSS: PROBLEME READING INPUT INDEX FILE')
!
   J=0
   DO JLON=1,KNLON
      J=J+1
      I=I+1
      KINDEX(I)=IVALUE(J)
   ENDDO
!
ENDDO
!
SELECT CASE (SIZE(KINDEX))
       CASE (21600*43200)
            ALLOCATE(IINDEX_1KM(SIZE(KINDEX)))
            IINDEX_1KM(:)=KINDEX(:)
       CASE (2160*4320)
            ALLOCATE(IINDEX_10KM(SIZE(KINDEX)))
            IINDEX_10KM(:)=KINDEX(:)               
       CASE (216*432)
            ALLOCATE(IINDEX_100KM(SIZE(KINDEX)))
            IINDEX_100KM(:)=KINDEX(:)               
       CASE DEFAULT
            CALL ABOR1_SFX('READ_INDEX_GAUSS: RESOLUTION NOT KNOW')
END SELECT               
!
!-------------------------------------------------------------------------------
!
IF(PRESENT(KSSOX).AND.PRESENT(KSSOY))THEN
!
  I=0
  IREC_SSOX=0
  DO JLAT=1,KNLAT
!
     IREC_SSOX=IREC+JLAT
!
     READ(IGLB,REC=IREC_SSOX,IOSTAT=IERR) IVALUE(:)
     IF(IERR/=0)CALL ABOR1_SFX('READ_INDEX_GAUSS: NO INDEX SSOX IN INPUT INDEX FILE')

!
     J=0
     DO JLON=1,KNLON
        J=J+1
        I=I+1
        KSSOX(I)=IVALUE(J)
     ENDDO
!
  ENDDO
!
  I=0
  IREC_SSOY=0
  DO JLAT=1,KNLAT
!
     IREC_SSOY=IREC_SSOX+JLAT
!
     READ(IGLB,REC=IREC_SSOY,IOSTAT=IERR) IVALUE(:)
     IF(IERR/=0)CALL ABOR1_SFX('READ_INDEX_GAUSS: NO INDEX SSOY IN INPUT INDEX FILE')
!   
     J=0
     DO JLON=1,KNLON
        J=J+1
        I=I+1
        KSSOY(I)=IVALUE(J)
     ENDDO   
!
  ENDDO
!
  SELECT CASE (SIZE(KINDEX))
       CASE (21600*43200)
            ALLOCATE(IISSOX_1KM(SIZE(KINDEX)))
            ALLOCATE(IISSOY_1KM(SIZE(KINDEX)))
            IISSOX_1KM(:)=KSSOX(:)
            IISSOY_1KM(:)=KSSOY(:)
       CASE (2160*4320)
            ALLOCATE(IISSOX_10KM(SIZE(KINDEX)))
            ALLOCATE(IISSOY_10KM(SIZE(KINDEX)))
            IISSOX_10KM(:)=KSSOX(:)
            IISSOY_10KM(:)=KSSOY(:)               
       CASE (216*432)
            ALLOCATE(IISSOX_100KM(SIZE(KINDEX)))
            ALLOCATE(IISSOY_100KM(SIZE(KINDEX)))
            IISSOX_100KM(:)=KSSOX(:)
            IISSOY_100KM(:)=KSSOY(:)               
       CASE DEFAULT
            CALL ABOR1_SFX('READ_GAUSS_CONF: RESOLUTION NOT KNOW FOR SSO')
  END SELECT               
!
ENDIF
!
DEALLOCATE (IVALUE)
!
!-------------------------------------------------------------------------------
!
 CALL CLOSE_FILE(HPROGRAM,IGLB)
IF (LHOOK) CALL DR_HOOK('MODE_GAUSS_INDEX:READ_INDEX_GAUSS',1,ZHOOK_HANDLE)
!
!######################################################################################
END SUBROUTINE READ_INDEX_GAUSS
!############################################################################
!
!##########################################################################
SUBROUTINE STORE_INDEX_GAUSS(HPROGRAM,HRES,KNLON,KNLAT,KINDEX,KSSOX,KSSOY)
!##########################################################################
!
!!**** *STORE_INDEX_GAUSS* store the grid mesh where point (lat,lon) is located
!!                         in a binary file
!!
!!    PURPOSE
!!    -------
!!
!!    AUTHOR
!!    ------
!!
!!    B. Decharme         Meteo-France
!!
!!    MODIFICATION
!!    ------------
!!
!!    Original    02/2010
!-------------------------------------------------------------------------------
!
!*    0.     DECLARATION
!            -----------
!
USE MODD_SURF_ATM_GRID_n, ONLY : XGRID_PAR
!
USE MODI_OPEN_FILE
USE MODI_CLOSE_FILE
!
USE MODE_GRIDTYPE_GAUSS
!
IMPLICIT NONE
!
!*    0.1    Declaration of arguments
!            ------------------------
!
 CHARACTER(LEN=6),      INTENT(IN) :: HPROGRAM
 CHARACTER(LEN=5),      INTENT(IN) :: HRES         ! Resolution 
INTEGER,               INTENT(IN) :: KNLON        ! number of longitude rows in file
INTEGER,               INTENT(IN) :: KNLAT        ! number of longitude rows in file
INTEGER, DIMENSION(:), INTENT(IN) :: KINDEX       ! mesh index of all input points
!
INTEGER, DIMENSION(:), OPTIONAL, INTENT(IN) :: KSSOX ! X submesh index in their mesh of all input points
INTEGER, DIMENSION(:), OPTIONAL, INTENT(IN) :: KSSOY ! Y submesh index in their mesh of all input points
!
!*    0.2    Declaration of other local variables
!            ------------------------------------
!
INTEGER (KIND=4), DIMENSION(:), ALLOCATABLE :: IVALUE ! value of a data point
!
 CHARACTER(LEN=28) :: YFILENAME, YNLATI
!
INTEGER           :: INLATI 
!
INTEGER           :: JLON, JLAT        ! loop counters
INTEGER           :: IGLB              ! record unit
INTEGER           :: IRECLENGTH        ! record length
INTEGER           :: IREC              ! record number
INTEGER           :: IREC_SSOX         ! record number
INTEGER           :: IREC_SSOY         ! record number
INTEGER           :: IBITS             ! number of bits of a record in the
!                                      ! direct access file
!
INTEGER :: I,J
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('MODE_GAUSS_INDEX:STORE_INDEX_GAUSS',0,ZHOOK_HANDLE)
IBITS      = 32
IRECLENGTH = IBITS/8 * KNLON
!
 CALL GET_GRIDTYPE_GAUSS(XGRID_PAR,KNLATI=INLATI)
!
WRITE(YNLATI,'(I28)')(INLATI-1)
YFILENAME='index_'//ADJUSTL(HRES(:LEN_TRIM(HRES)))//'_to_t'//ADJUSTL(YNLATI(:LEN_TRIM(YNLATI))//'.bin')
!
ALLOCATE (IVALUE(KNLON))
!
!-------------------------------------------------------------------------------

 CALL OPEN_FILE(HPROGRAM,IGLB,YFILENAME,'UNFORMATTED',           &
                 HACTION='WRITE',HACCESS='DIRECT',KRECL=IRECLENGTH )  
!
!-------------------------------------------------------------------------------
!
I=0
IREC=0
DO JLAT=1,KNLAT
!
   J=0
   IREC=JLAT
   DO JLON=1,KNLON
      J=J+1
      I=I+1
      IVALUE(J)=KINDEX(I)
   ENDDO
!
   WRITE(IGLB,REC=IREC) IVALUE(:)
!
ENDDO
!
!-------------------------------------------------------------------------------
!
IF(PRESENT(KSSOX).AND.PRESENT(KSSOY))THEN
!
I=0
IREC_SSOX=0
DO JLAT=1,KNLAT
!
   J=0
   IREC_SSOX=IREC+JLAT
   DO JLON=1,KNLON
      J=J+1
      I=I+1
      IVALUE(J)=KSSOX(I)
   ENDDO
!
   WRITE(IGLB,REC=IREC_SSOX) IVALUE(:)
!
ENDDO
!
I=0
IREC_SSOY=0
DO JLAT=1,KNLAT
!
   J=0
   IREC_SSOY=IREC_SSOX+JLAT
   DO JLON=1,KNLON
      J=J+1
      I=I+1
      IVALUE(J)=KSSOY(I)
   ENDDO
!
   WRITE(IGLB,REC=IREC_SSOY) IVALUE(:)
!
ENDDO
!
ENDIF
!
DEALLOCATE (IVALUE)
!
!-------------------------------------------------------------------------------
!
 CALL CLOSE_FILE(HPROGRAM,IGLB)
IF (LHOOK) CALL DR_HOOK('MODE_GAUSS_INDEX:STORE_INDEX_GAUSS',1,ZHOOK_HANDLE)
!
!######################################################################################
END SUBROUTINE STORE_INDEX_GAUSS
!############################################################################
!
END MODULE MODE_GAUSS_INDEX

