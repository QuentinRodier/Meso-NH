!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!!
!!    MODIFICATIONS
!!    -------------
!!      M.Moge    01/2016  using READ_SURF_FIELD2D/3D for 2D/3D surfex fields reads
!     #######################
      SUBROUTINE READ_SURF_ISBA_PAR_n (DTCO, U, I,GCP, &
                                       HPROGRAM,HREC,KLUOUT,KSIZE,PFIELD,KRESP,KVERSION,HCOMMENT,HDIR)
!     #######################
!
!
!
!
!
!
USE MODD_DATA_COVER_n, ONLY : DATA_COVER_t
USE MODD_SURF_ATM_n, ONLY : SURF_ATM_t
USE MODD_GRID_CONF_PROJ, ONLY : GRID_CONF_PROJ_t
!
!
USE MODD_ISBA_n, ONLY : ISBA_t
!
USE MODD_DATA_COVER_PAR, ONLY : NVEGTYPE
!
USE MODI_READ_SURF_FIELD2D
USE MODI_HOR_INTERPOL
USE MODI_PUT_ON_ALL_VEGTYPES
USE MODI_VEGTYPE_TO_PATCH
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!
!
!
TYPE(DATA_COVER_t), INTENT(INOUT) :: DTCO
TYPE(SURF_ATM_t), INTENT(INOUT) :: U
TYPE(GRID_CONF_PROJ_t),INTENT(INOUT) :: GCP
!
!
TYPE(ISBA_t), INTENT(INOUT) :: I
!
 CHARACTER(LEN=6),        INTENT(IN) :: HPROGRAM ! calling program
 CHARACTER(LEN=*),        INTENT(IN) :: HREC   ! name of the article to be read
!
INTEGER,                 INTENT(IN) :: KLUOUT
INTEGER,                 INTENT(IN) :: KSIZE
REAL, DIMENSION(:,:),    INTENT(OUT):: PFIELD ! array containing the data field  

INTEGER                  ,INTENT(OUT) :: KRESP      ! KRESP  : return-code if a problem appears
INTEGER, INTENT(IN) :: KVERSION
 CHARACTER(LEN=*),OPTIONAL,INTENT(OUT) :: HCOMMENT   ! name of the article to be read
 CHARACTER(LEN=1),OPTIONAL,INTENT(IN)  :: HDIR       ! type of field :
!                                                   ! 'H' : field with
!                                                   !       horizontal spatial dim.
!                                                   ! '-' : no horizontal dim.
!
!* local variables
!  ---------------
!
REAL, DIMENSION(KSIZE, NVEGTYPE)  :: ZFIELD
REAL, DIMENSION(SIZE(PFIELD,1),1,I%NPATCH) :: ZFIELD_PATCH
REAL, DIMENSION(SIZE(PFIELD,1),1,NVEGTYPE) :: ZFIELD_VEGTYPE
 CHARACTER(LEN=1)   :: YDIR
INTEGER :: INI, JPATCH, IPATCH, JVEGTYPE
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('READ_SURF_ISBA_PAR_n',0,ZHOOK_HANDLE)
!
YDIR = 'H'
IF (PRESENT(HDIR)) YDIR = HDIR
!
INI = SIZE(PFIELD,1)
!
IF (KVERSION<7) THEN
  CALL READ_SURF_FIELD2D(&
                 HPROGRAM,ZFIELD(:,1:I%NPATCH),HREC,HCOMMENT=HCOMMENT,HDIR=YDIR)
  IF (INI.NE.KSIZE) THEN
    CALL HOR_INTERPOL(DTCO, U,GCP, &
                      KLUOUT,ZFIELD(:,1:I%NPATCH),PFIELD(:,1:I%NPATCH))
  ELSE
    PFIELD(:,1:I%NPATCH) = ZFIELD(:,1:I%NPATCH)
  ENDIF
  DO JPATCH = 1, I%NPATCH
    ZFIELD_PATCH(:,1,JPATCH) = PFIELD(:,JPATCH)
  ENDDO
  CALL PUT_ON_ALL_VEGTYPES(INI,1,I%NPATCH,NVEGTYPE,ZFIELD_PATCH,ZFIELD_VEGTYPE)
  PFIELD(:,:) = ZFIELD_VEGTYPE(:,1,:)
ELSE
  CALL READ_SURF_FIELD2D(&
                 HPROGRAM,ZFIELD(:,:),HREC,HCOMMENT=HCOMMENT,HDIR=YDIR)
  IF (INI.NE.KSIZE) THEN
    CALL HOR_INTERPOL(DTCO, U,GCP, &
                      KLUOUT,ZFIELD(:,:),ZFIELD_VEGTYPE(:,1,:))
  ELSE
    ZFIELD_VEGTYPE(:,1,:) = ZFIELD(:,:)
  ENDIF  
  IF (SIZE(PFIELD,2).NE.NVEGTYPE) THEN
    IPATCH = SIZE(PFIELD,2)
    PFIELD(:,:) = 0.
    DO JVEGTYPE = 1, NVEGTYPE
      JPATCH = VEGTYPE_TO_PATCH(JVEGTYPE,IPATCH)
      IF (JPATCH<=IPATCH) PFIELD(:,JPATCH) = MAX(PFIELD(:,JPATCH),ZFIELD_VEGTYPE(:,1,JVEGTYPE))
    ENDDO
  ELSE
    PFIELD(:,:) = ZFIELD_VEGTYPE(:,1,:)
  ENDIF        
ENDIF
!
IF (LHOOK) CALL DR_HOOK('READ_SURF_ISBA_PAR_n',1,ZHOOK_HANDLE)
!-------------------------------------------------------------------
!
END SUBROUTINE READ_SURF_ISBA_PAR_n
