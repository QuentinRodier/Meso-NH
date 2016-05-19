!     ###############################################################
      SUBROUTINE GET_MESH_INDEX_GAUSS(KGRID_PAR,KL,PGRID_PAR,PLAT,PLON,KINDEX,KSSO,KISSOX,KISSOY)
!     ###############################################################
!
!!**** *GET_MESH_INDEX_GAUSS* get the grid mesh where point (lat,lon) is located
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
!!
!----------------------------------------------------------------------------
!
!*    0.     DECLARATION
!            -----------
!
USE MODD_GET_MESH_INDEX_GAUSS, ONLY : IINDEX_1KM,IINDEX_10KM,IINDEX_100KM, &
                                        IISSOX_1KM,IISSOX_10KM,IISSOX_100KM, &
                                        IISSOY_1KM,IISSOY_10KM,IISSOY_100KM, &
                                        IMASK_GAUSS  
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
USE MODI_ABOR1_SFX
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
INTEGER,                       INTENT(IN)    :: KSSO      ! number of subgrid mesh in each direction
INTEGER, DIMENSION(KL),        INTENT(OUT)   :: KISSOX    ! X index of the subgrid mesh
INTEGER, DIMENSION(KL),        INTENT(OUT)   :: KISSOY    ! Y index of the subgrid mesh
!
!*    0.2    Declaration of other local variables
!            ------------------------------------
!
INTEGER                           :: JI          ! loop counter in x
INTEGER                           :: JJ          ! loop counter in y
!
INTEGER                           :: INDIM
!
INTEGER, DIMENSION(:), ALLOCATABLE :: IINDEX
INTEGER, DIMENSION(:), ALLOCATABLE :: IISSOX
INTEGER, DIMENSION(:), ALLOCATABLE :: IISSOY
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!----------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('GET_MESH_INDEX_GAUSS',0,ZHOOK_HANDLE)
INDIM=SIZE(IMASK_GAUSS)
!
ALLOCATE(IINDEX(INDIM))
ALLOCATE(IISSOX(INDIM))
ALLOCATE(IISSOY(INDIM))
!
KINDEX = -999
KISSOX = -999
KISSOY = -999
!
IF (KSSO/=0) THEN
!        
   SELECT CASE (INDIM)
       CASE (21600*43200)
            IINDEX(:) = IINDEX_1KM(:)
            IISSOX(:) = IISSOX_1KM(:)
            IISSOY(:) = IISSOY_1KM(:)              
       CASE (2160*4320)
            IINDEX(:) = IINDEX_10KM(:)
            IISSOX(:) = IISSOX_10KM(:)
            IISSOY(:) = IISSOY_10KM(:)
       CASE (216*432)
            IINDEX(:) = IINDEX_100KM(:)
            IISSOX(:) = IISSOX_100KM(:)
            IISSOY(:) = IISSOY_100KM(:)
       CASE DEFAULT
            CALL ABOR1_SFX('GET_MESH_INDEX_GAUSS: RESOLUTION NOT KNOW')
   END SELECT               
!   
ELSE
!
   SELECT CASE (INDIM)
       CASE (21600*43200)
            IINDEX(:) = IINDEX_1KM(:)
       CASE (2160*4320)
            IINDEX(:) = IINDEX_10KM(:)
       CASE (216*432)
            IINDEX(:) = IINDEX_100KM(:)
       CASE DEFAULT
            CALL ABOR1_SFX('GET_MESH_INDEX_GAUSS: RESOLUTION NOT KNOW')
   END SELECT               
!   
ENDIF
!
IF(ALL(IMASK_GAUSS(:)==1))THEN
!        
  KINDEX=IINDEX
  IF(KSSO/=0)THEN
    KISSOX=IISSOX
    KISSOY=IISSOY
  ENDIF
!  
ELSE
!
  JJ=0
  DO JI=1,INDIM
     IF(IMASK_GAUSS(JI)==1)THEN
        JJ=JJ+1
        KINDEX(JJ)=IINDEX(JI)
        IF(KSSO/=0)THEN
           KISSOX(JJ)=IISSOX(JI)
           KISSOY(JJ)=IISSOY(JI)
        ENDIF
     ENDIF
  ENDDO
! 
ENDIF
!
DEALLOCATE(IINDEX)
DEALLOCATE(IISSOX)
DEALLOCATE(IISSOY)
IF (LHOOK) CALL DR_HOOK('GET_MESH_INDEX_GAUSS',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE GET_MESH_INDEX_GAUSS
