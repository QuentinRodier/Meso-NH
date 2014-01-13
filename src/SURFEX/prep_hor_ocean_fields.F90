!SURFEX_LIC Copyright 1994-2014 Meteo-France 
!SURFEX_LIC This is part of the SURFEX software governed by the CeCILL-C  licence
!SURFEX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!SURFEX_LIC for details. version 1.
!     #########
SUBROUTINE PREP_HOR_OCEAN_FIELDS(HPROGRAM,HSURF,HFILE,HFILETYPE,KLUOUT,OUNIF)
!     #######################################################
!
!
!!****  *PREP_HOR_OCEAN_FIELDS* - prepares all oceanic fields for the 1D oceanic model
!!
!!    PURPOSE
!!    -------
!
!!**  METHOD
!!    ------
!!
!!    REFERENCE
!!    ---------
!!      
!!
!!    AUTHOR
!!    ------
!!     C. Lebeaupin Brossier 
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    01/2008
!!      Modified    07/2012, P. Le Moigne : CMO1D phasing
!!------------------------------------------------------------------
!
!
USE MODD_SURF_PAR,       ONLY : XUNDEF
USE MODD_OCEAN_CSTS,     ONLY : XRHOSWREF,NOCKMIN,NOCKMAX
USE MODD_SEAFLUX_n
USE MODD_OCEAN_GRID_n, ONLY : XDZ1,XZHOC
USE MODD_OCEAN_n
USE MODD_OCEAN_REL_n
!
USE MODI_PREP_HOR_OCEAN_FIELD
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
 CHARACTER(LEN=6),   INTENT(IN)  :: HPROGRAM  ! program calling surf. schemes
 CHARACTER(LEN=7),   INTENT(IN)  :: HSURF     ! type of field
 CHARACTER(LEN=28),  INTENT(IN)  :: HFILE     ! file name
 CHARACTER(LEN=6),   INTENT(IN)  :: HFILETYPE ! file type
INTEGER,            INTENT(IN)  :: KLUOUT    ! logical unit of output listing
LOGICAL,            INTENT(IN)  :: OUNIF     ! flag for prescribed uniform field
!
!
!*      0.2    declarations of local variables
!
 CHARACTER(LEN=8)                    :: YSURF   ! type of field
 CHARACTER(LEN=28)                   :: YNCVARNAME   ! variable to read
!
INTEGER                             :: IL        ! number of points
INTEGER                             :: IK1
INTEGER                             :: J, JLEV   ! loop counters
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!---------------------------------------------------------------------------
!
!*      1.     Patch
!
!---------------------------------------------------------------------------
!
!*      3.     Treatment of oceanic temperature
IF (LHOOK) CALL DR_HOOK('PREP_HOR_OCEAN_FIELDS',0,ZHOOK_HANDLE)
YSURF='TEMP_OC'
YNCVARNAME='temperature'
 CALL PREP_HOR_OCEAN_FIELD(HPROGRAM,HFILE,HFILETYPE,KLUOUT,OUNIF,YSURF,YNCVARNAME)
!---------------------------------------------------------------------------
!
!*      4.     Treatment of oceanic salinity
YSURF='SALT_OC'
YNCVARNAME='salinity'
 CALL PREP_HOR_OCEAN_FIELD(HPROGRAM,HFILE,HFILETYPE,KLUOUT,OUNIF,YSURF,YNCVARNAME)
!---------------------------------------------------------------------------
!
!*      5.     Treatment of oceanic current
YSURF='UCUR_OC'
YNCVARNAME='u'
 CALL PREP_HOR_OCEAN_FIELD(HPROGRAM,HFILE,HFILETYPE,KLUOUT,OUNIF,YSURF,YNCVARNAME)
YSURF='VCUR_OC'
YNCVARNAME='v'
 CALL PREP_HOR_OCEAN_FIELD(HPROGRAM,HFILE,HFILETYPE,KLUOUT,OUNIF,YSURF,YNCVARNAME)
!---------------------------------------------------------------------------
!
IK1=NOCKMIN+1
IL = SIZE(XSEAT,1)
IF (IL/=0) THEN
  ALLOCATE(XSEAE      (SIZE(XSEAT,1),NOCKMIN:NOCKMAX))
  XSEAE(:,:)   =1.E-3
  ALLOCATE(XSEABATH   (SIZE(XSEAT,1),NOCKMIN:NOCKMAX))
  XSEABATH(:,:)=1.
  ALLOCATE(XSEAHMO    (SIZE(XSEAT,1)))
  XSEAHMO(:)   =XUNDEF
  ALLOCATE(XLE        (SIZE(XSEAT,1),NOCKMIN:NOCKMAX))
  ALLOCATE(XLK        (SIZE(XSEAT,1),NOCKMIN:NOCKMAX))
  ALLOCATE(XKMEL      (SIZE(XSEAT,1),NOCKMIN:NOCKMAX))
  ALLOCATE(XKMELM     (SIZE(XSEAT,1),NOCKMIN:NOCKMAX))
  XLE(:,:)    =XUNDEF
  XLK(:,:)    =XUNDEF
  XKMEL(:,:)  =XUNDEF
  XKMELM(:,:) =XUNDEF
  ALLOCATE(XSEATEND   (SIZE(XSEAT,1)))
  XSEATEND(:) =XUNDEF
  !
  ALLOCATE(XDTFNSOL   (SIZE(XSEAT,1)))
  XDTFNSOL(:) = XUNDEF
  ALLOCATE(XDTFSOL    (SIZE(XSEAT,1),NOCKMIN:NOCKMAX))
  XDTFSOL(:,:)= XUNDEF  
!!----------------------------------------------------------------------------
!!
!!*      6.     Treatment of bathymetry indice and 
!!              apply bathy mask
  DO J=1,IL
    DO JLEV=IK1+1,NOCKMAX
      IF (XSEABATHY(J)-XZHOC(JLEV)>0.) THEN
        XSEABATH(J,JLEV)=0.
        XSEAE(J,JLEV)  = XUNDEF
        XSEAU(J,JLEV)  = XUNDEF
        XSEAV(J,JLEV)  = XUNDEF
        XSEAT(J,JLEV)  = XUNDEF
        XSEAS(J,JLEV)  = XUNDEF
        !
        XSEAT_REL(J,JLEV)  = XUNDEF
        XSEAS_REL(J,JLEV)  = XUNDEF
        !
        XSEAU_REL(J,JLEV)  = XUNDEF
        XSEAV_REL(J,JLEV)  = XUNDEF
        !        
      ENDIF 
    ENDDO
  ENDDO
!
!---------------------------------------------------------------------------
ENDIF
IF (LHOOK) CALL DR_HOOK('PREP_HOR_OCEAN_FIELDS',1,ZHOOK_HANDLE)
!----------------------------------------------------------------------------
END SUBROUTINE PREP_HOR_OCEAN_FIELDS
