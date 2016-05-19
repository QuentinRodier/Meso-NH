!
MODULE MODE_CONVERT
   !     MODE_CONVERT
   !     CONVERT_1D    = Convert 1D to 2D array            (ISBA -> TRIP)
   !     CONVERT_2D    = Convert 2D to 3D array            (ISBA -> TRIP)
   !     UN_CONVERT_1D = Convert 2D to 1D                  (TRIP -> ISBA)
   !     UN_CONVERT_2D = Convert 3D to 2D array            (TRIP -> ISBA)
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
  INTERFACE CONVERT
      MODULE PROCEDURE CONVERT_1D
      MODULE PROCEDURE CONVERT_2D
  END INTERFACE

  INTERFACE UN_CONVERT
      MODULE PROCEDURE UN_CONVERT_1D
      MODULE PROCEDURE UN_CONVERT_2D
  END INTERFACE 
!
CONTAINS
!
!  ##########################################################
   SUBROUTINE CONVERT_1D(P1D,GMASK,P2D)
   !-------------------------------------------------------------------------------
   !
   IMPLICIT NONE
   !
   REAL,    DIMENSION(:),         INTENT(IN)     :: P1D    ! Input 1D array without patch
   LOGICAL, DIMENSION(:,:),       INTENT(IN)     :: GMASK  ! Mask used to pack
   REAL,    DIMENSION(:,:),       INTENT(OUT)    :: P2D    ! Output 2D array
   !
   INTEGER ILON,ILAT,I,J,ICOUNT
   REAL(KIND=JPRB) :: ZHOOK_HANDLE
   !--------------------------------------------------------------------------------
   !
   ! initialize
   IF (LHOOK) CALL DR_HOOK('MODE_CONVERT:CONVERT_1D',0,ZHOOK_HANDLE)
   ILON =SIZE(GMASK,1)
   ILAT =SIZE(GMASK,2)
   !
   P2D(:,:)=0.0
   ICOUNT=0
   !
   DO J=1,ILAT
      DO I=1,ILON
         IF(GMASK(I,J))THEN
           ICOUNT=ICOUNT+1
           P2D(I,J)= P1D(ICOUNT)
         ELSE
           P2D(I,J)= 0.0
         ENDIF
      ENDDO
   ENDDO
   IF (LHOOK) CALL DR_HOOK('MODE_CONVERT:CONVERT_1D',1,ZHOOK_HANDLE)
   !
   END SUBROUTINE CONVERT_1D
!  
   SUBROUTINE CONVERT_2D(P2D,GMASK,P3D)
   !-------------------------------------------------------------------------------
   !
   IMPLICIT NONE
   !
   REAL,    DIMENSION(:,:),       INTENT(IN)     :: P2D    ! Input 2D array without patch
   LOGICAL, DIMENSION(:,:),       INTENT(IN)     :: GMASK  ! Mask used to pack
   REAL,    DIMENSION(:,:,:),     INTENT(OUT)    :: P3D    ! Output 3D array
   !
   INTEGER ILON,ILAT,I,J,ICOUNT
   REAL(KIND=JPRB) :: ZHOOK_HANDLE
   !--------------------------------------------------------------------------------
   !
   ! initialize
   IF (LHOOK) CALL DR_HOOK('MODE_CONVERT:CONVERT_2D',0,ZHOOK_HANDLE)
   ILON =SIZE(GMASK,1)
   ILAT =SIZE(GMASK,2)
   !
   P3D(:,:,:)=0.0
   ICOUNT=0
   !
   DO J=1,ILAT
      DO I=1,ILON
         IF(GMASK(I,J))THEN
           ICOUNT=ICOUNT+1
           P3D(I,J,:)= P2D(ICOUNT,:)
         ELSE
           P3D(I,J,:)= 0.0
         ENDIF
      ENDDO
   ENDDO
   IF (LHOOK) CALL DR_HOOK('MODE_CONVERT:CONVERT_2D',1,ZHOOK_HANDLE)
   !
   END SUBROUTINE CONVERT_2D
!
   SUBROUTINE UN_CONVERT_1D(P2D,GMASK,P1D)
   !-----------------------------------------------------------------------------
   !
   IMPLICIT NONE
   !
   REAL,    DIMENSION(:,:),       INTENT(IN)     :: P2D   ! Input 2D array
   LOGICAL, DIMENSION(:,:),       INTENT(IN)     :: GMASK ! Mask used to pack
   REAL,    DIMENSION(:),         INTENT(OUT)    :: P1D   ! Output 1D array
   !
   INTEGER ILON,ILAT,I,J,ICOUNT
   REAL(KIND=JPRB) :: ZHOOK_HANDLE
   !------------------------------------------------------------------------------
   !
   ! initialize
   IF (LHOOK) CALL DR_HOOK('MODE_CONVERT:UN_CONVERT_1D',0,ZHOOK_HANDLE)
   ILON =SIZE(P2D,1)
   ILAT =SIZE(P2D,2)
   P1D(:)=0.0
   ICOUNT=0
   !
   DO J=1,ILAT
      DO I=1,ILON
         IF(GMASK(I,J))THEN
           ICOUNT=ICOUNT+1
           P1D(ICOUNT)=P2D(I,J)
         ENDIF
      ENDDO
   ENDDO
   IF (LHOOK) CALL DR_HOOK('MODE_CONVERT:UN_CONVERT_1D',1,ZHOOK_HANDLE)
   !
   END SUBROUTINE UN_CONVERT_1D
!   
   SUBROUTINE UN_CONVERT_2D(P3D,GMASK,P2D)
   !-----------------------------------------------------------------------------
   !
   IMPLICIT NONE
   !
   REAL,    DIMENSION(:,:,:),     INTENT(IN)   :: P3D   ! Input 2D array
   LOGICAL, DIMENSION(:,:),       INTENT(IN)   :: GMASK ! Mask used to pack
   REAL,    DIMENSION(:,:),       INTENT(OUT)  :: P2D   ! Output 1D array
   !
   INTEGER ILON,ILAT,I,J,ICOUNT
   REAL(KIND=JPRB) :: ZHOOK_HANDLE
   !------------------------------------------------------------------------------
   !
   ! initialize
   IF (LHOOK) CALL DR_HOOK('MODE_CONVERT:UN_CONVERT_2D',0,ZHOOK_HANDLE)
   ILON =SIZE(P3D,1)
   ILAT =SIZE(P3D,2)
   P2D(:,:)=0.0
   ICOUNT=0
   !
   DO J=1,ILAT
      DO I=1,ILON
         IF(GMASK(I,J))THEN
           ICOUNT=ICOUNT+1
           P2D(ICOUNT,:)=P3D(I,J,:)
         ENDIF
      ENDDO
   ENDDO
   IF (LHOOK) CALL DR_HOOK('MODE_CONVERT:UN_CONVERT_2D',1,ZHOOK_HANDLE)
   !
   END SUBROUTINE UN_CONVERT_2D
!   
!---------------
END MODULE MODE_CONVERT
