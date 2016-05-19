!###################
MODULE MODE_TRIP_INIT
!###################
!
!!****  *MODE_TRIP_INIT*
!!
!!    PURPOSE
!!    -------
!    
!      The purpose of this routine is to store here all routines 
!      used by INIT_TRIP.
!
!!
!!**  IMPLICIT ARGUMENTS
!!    ------------------
!!       NONE          
!!
!!    REFERENCE
!!    ---------
!!
!!
!!    AUTHOR
!!    ------
!!	B. Decharme       * Meteo France *
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    15/04/08
!--------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
  INTERFACE SETNEXT
      MODULE PROCEDURE SETNEXT
  END INTERFACE
!
  INTERFACE SETAREA
      MODULE PROCEDURE SETAREA
  END INTERFACE
!  
  INTERFACE SETLEN
      MODULE PROCEDURE SETLEN
  END INTERFACE
!  
  INTERFACE SET_SUBGRID_FLOOD
      MODULE PROCEDURE SET_SUBGRID_FLOOD
  END INTERFACE
!
!-------------------------------------------------------------------------------
!
CONTAINS
!-------------------------------------------------------------------------------
!
!     #############################################################
      SUBROUTINE SETNEXT(KLON,KLAT,KGRCN,KNEXTX,KNEXTY,GMLON,GMLAT)
!     #############################################################
!
!!    PURPOSE
!!    -------
!    
!     set the destination grid point
!
!     (i, j) ===>  (inextx(i,j), inexty(i,j))
!     at river mouth : pointing itself
!     at sea         : 0
!
USE MODE_TRIP_FUNCTION
!
IMPLICIT NONE
!
!*      declarations of arguments
!
INTEGER, INTENT(IN)                    :: KLON, KLAT
!
INTEGER, DIMENSION(:,:), INTENT(INOUT) :: KGRCN
!
INTEGER, DIMENSION(:,:), INTENT(OUT)   :: KNEXTX, KNEXTY
!
LOGICAL, INTENT(IN), OPTIONAL          :: GMLON,GMLAT
!
!*      declarations of local variables
!
INTEGER :: I, J, K
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!*      procedure
!
IF (LHOOK) CALL DR_HOOK('MODE_TRIP_INIT:SETNEXT',0,ZHOOK_HANDLE)
IF(PRESENT(GMLON))THEN
IF(GMLON)THEN
  DO J=1,KLAT
     IF(KGRCN(KLON,J)==2..OR.KGRCN(KLON,J)==3.OR.KGRCN(KLON,J)==4.)KGRCN(KLON,J)=9
     IF(KGRCN(   1,J)==6..OR.KGRCN(   1,J)==7.OR.KGRCN(   1,J)==8.)KGRCN(   1,J)=9     
  ENDDO
ENDIF
ENDIF
!
IF(PRESENT(GMLAT))THEN
IF(GMLAT)THEN
  DO I=1,KLON
     IF(KGRCN(I,   1)==4..OR.KGRCN(I,   1)==5.OR.KGRCN(I,   1)==6.)KGRCN(I,   1)=9
     IF(KGRCN(I,KLAT)==1..OR.KGRCN(I,KLAT)==2.OR.KGRCN(I,KLAT)==8.)KGRCN(I,KLAT)=9     
  ENDDO
ENDIF
ENDIF
!
DO I=1,KLON
   DO J=1,KLAT
!
      K=KGRCN(I,J)
!
      IF((K>=1).AND.(K<=8))THEN              
        KNEXTX(I,J)=IRNXTX(I,KLON,K)
        KNEXTY(I,J)=IRNXTY(J,KLAT,K)        
      ELSEIF(K==9.OR.K==12)THEN     
        KNEXTX(I,J)=I
        KNEXTY(I,J)=J       
      ELSE             
        KNEXTX(I,J)=0
        KNEXTY(I,J)=0       
      ENDIF
!
   ENDDO
ENDDO
IF (LHOOK) CALL DR_HOOK('MODE_TRIP_INIT:SETNEXT',1,ZHOOK_HANDLE)
!
END SUBROUTINE SETNEXT
!
!-------------------------------------------------------------------------------
!
!     #################################################
      SUBROUTINE SETAREA(KLAT,PLATMIN,PRES,PAREA)
!     #################################################
!
!!    PURPOSE
!!    -------
!    
!     set area [m²] of each grid box
!
USE MODD_TRIP_PAR, ONLY : XPI_T, XRAD_T
!
IMPLICIT NONE
!
!*      declarations of arguments
!
INTEGER, INTENT(IN)                :: KLAT
REAL, INTENT(IN)                   :: PRES
REAL, INTENT(IN)                   :: PLATMIN
!
REAL, DIMENSION(:,:), INTENT(OUT)  :: PAREA
!
!*      declarations of local variables
!
REAL    :: ZDLAT, ZDLON, ZLAT
!
INTEGER :: I, J, K
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!*      procedure
!
IF (LHOOK) CALL DR_HOOK('MODE_TRIP_INIT:SETAREA',0,ZHOOK_HANDLE)
ZDLON=PRES
ZDLAT=PRES
!
ZLAT=PLATMIN-(PRES/2.)
!
DO J=1,KLAT
!
   ZLAT=ZLAT+PRES
!
   PAREA(:,J) = XRAD_T**2 * XPI_T/180.*(ZDLON)              &
       * (SIN((ZLAT+ZDLAT/2.)*XPI_T/180.)-SIN((ZLAT-ZDLAT/2.)*XPI_T/180.))  
!
ENDDO
IF (LHOOK) CALL DR_HOOK('MODE_TRIP_INIT:SETAREA',1,ZHOOK_HANDLE)
!
END SUBROUTINE SETAREA
!
!-------------------------------------------------------------------------------
!
!     #############################################################
      SUBROUTINE SETLEN(KLON,KLAT,KGRCN,KNEXTX,KNEXTY,PRATMED,PLEN)
!     #############################################################
!
!!    PURPOSE
!!    -------
!    
!     length from (i, j) to the destination in [m]
!     river mouth : distance to 1 grid north
!     sea         : 0.0
!
!
USE MODE_TRIP_FUNCTION
!
IMPLICIT NONE
!
!*      declarations of arguments
!
INTEGER, INTENT(IN)                 :: KLON, KLAT
REAL, INTENT(IN)                    :: PRATMED
!
INTEGER, DIMENSION(:,:), INTENT(IN) :: KGRCN
!
INTEGER, DIMENSION(:,:), INTENT(IN) :: KNEXTX, KNEXTY
!
REAL, DIMENSION(:,:), INTENT(OUT)   :: PLEN
!
!*      declarations of local variables
!
REAL    :: ZLON, ZLAT, ZLON_N, ZLAT_N
!
INTEGER :: I, J, K
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!*      procedure
!
IF (LHOOK) CALL DR_HOOK('MODE_TRIP_INIT:SETLEN',0,ZHOOK_HANDLE)
ZLON=0.0
ZLAT=0.0
!
DO J=1,KLAT
!
   ZLAT=GETLAT(KLAT-J+1,KLAT)
!
   DO I=1,KLON
!
      ZLON=GETLON(I,KLON)
!
      IF(KGRCN(I,J)>=1.AND.KGRCN(I,J)<=8)THEN
        ZLON_N = GETLON(KNEXTX(I,J),KLON)
        ZLAT_N = GETLAT(KLAT-KNEXTY(I,J)+1,KLAT)
        PLEN(I,J) = GIVELEN(ZLON,ZLAT,ZLON_N,ZLAT_N) * 1000.0
      ELSEIF(KGRCN(I,J)==9.OR.KGRCN(I,J)==12)THEN
        ZLAT_N = GETLAT(KLAT-(J-1)+1,KLAT)
        PLEN(I,J) = GIVELEN(ZLON,ZLAT,ZLON,ZLAT_N) * 1000.0
      ELSE
        PLEN(I,J) = 0.0
      ENDIF
!
   ENDDO
!   
ENDDO
!
PLEN=PLEN*PRATMED
IF (LHOOK) CALL DR_HOOK('MODE_TRIP_INIT:SETLEN',1,ZHOOK_HANDLE)
!
END SUBROUTINE SETLEN
!
!-------------------------------------------------------------------------------
!
!     ###################################################################
      SUBROUTINE SET_SUBGRID_FLOOD(KLON,KLAT,PAREA,PTAB_H,PTAB_F,PTAB_VF)
!     ###################################################################
!
!!    PURPOSE
!!    -------
!    
!     set area [m²] of each grid box
!
USE MODD_TRIP_PAR, ONLY : XTRIP_UNDEF, XRHOLW_T
!
IMPLICIT NONE
!
!*      declarations of arguments
!
INTEGER,                INTENT(IN)   :: KLON, KLAT
REAL, DIMENSION(:,:  ), INTENT(IN)   :: PAREA
REAL, DIMENSION(:,:,:), INTENT(IN)   :: PTAB_H
!
REAL, DIMENSION(:,:,:), INTENT(INOUT):: PTAB_F
REAL, DIMENSION(:,:,:), INTENT(  OUT):: PTAB_VF
!
!*      declarations of local variables
!
REAL, DIMENSION(:,:,:), ALLOCATABLE :: ZF,ZV
!
INTEGER :: I, J, K, IZDIM
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!*      procedure
!
IF (LHOOK) CALL DR_HOOK('MODE_TRIP_INIT:SET_SUBGRID_FLOOD',0,ZHOOK_HANDLE)
IZDIM=SIZE(PTAB_H,3)
!
ALLOCATE(ZF(KLON,KLAT,IZDIM))
ALLOCATE(ZV(KLON,KLAT,IZDIM))
ZF = 0.0
ZV = 0.0
!
DO J=1,KLAT
   DO I=1,KLON
!   
      IF(ALL(PTAB_H(I,J,:)>XTRIP_UNDEF-10.0))THEN
         ZV(I,J,:)=XTRIP_UNDEF
         ZF(I,J,:)=XTRIP_UNDEF
         CYCLE
      ENDIF
!      
      DO K=2,IZDIM
         IF(PTAB_H(I,J,K)<XTRIP_UNDEF-1.0)THEN
            ZV(I,J,K)=ZV(I,J,K-1)+0.5*PTAB_H(I,J,K)*PTAB_F(I,J,K)*PAREA(I,J)*XRHOLW_T
            ZF(I,J,K)=ZF(I,J,K-1)+PTAB_F(I,J,K)
         ELSE
            ZV(I,J,K:IZDIM)=XTRIP_UNDEF
            ZF(I,J,K:IZDIM)=XTRIP_UNDEF
            EXIT
         ENDIF
      ENDDO
!
   ENDDO
ENDDO
!
WHERE(PTAB_H(:,:,:)<XTRIP_UNDEF-1.0)
      PTAB_F (:,:,:)=ZF(:,:,:)
      PTAB_VF(:,:,:)=ZV(:,:,:)
ENDWHERE
!
DEALLOCATE(ZF)
DEALLOCATE(ZV)
IF (LHOOK) CALL DR_HOOK('MODE_TRIP_INIT:SET_SUBGRID_FLOOD',1,ZHOOK_HANDLE)
!
END SUBROUTINE SET_SUBGRID_FLOOD
!
!-------------------------------------------------------------------------------
!
END MODULE MODE_TRIP_INIT      
