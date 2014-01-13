!SURFEX_LIC Copyright 1994-2014 Meteo-France 
!SURFEX_LIC This is part of the SURFEX software governed by the CeCILL-C  licence
!SURFEX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!SURFEX_LIC for details. version 1.
!     #########
SUBROUTINE PREP_HOR_OCEAN_FIELD( HPROGRAM,                       &
                                 HFILE,HFILETYPE,KLUOUT,OUNIF,   &
                                 HSURF,HNCVARNAME                )
!     #######################################################
!
!!****  *PREP_HOR_OCEAN_FIELD* -reads, interpolates and prepares oceanic fields
!!
!!    PURPOSE
!!    -------
!!
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
USE MODD_CSTS,           ONLY : XTT
USE MODD_SURF_PAR,       ONLY : XUNDEF
USE MODD_OCEAN_CSTS  ,   ONLY : NOCKMIN,NOCKMAX
USE MODD_OCEAN_n,        ONLY : XSEAT, XSEAS, XSEAU, XSEAV, LCURRENT
USE MODD_PREP,           ONLY : CINGRID_TYPE, CINTERP_TYPE, XLAT_OUT, XLON_OUT,&
                                XX_OUT, XY_OUT
USE MODD_SEAFLUX_GRID_n, ONLY : XLAT, XLON
USE MODD_OCEAN_REL_n, ONLY : XSEAS_REL, XSEAT_REL, XSEAU_REL, XSEAV_REL
!
USE MODI_PREP_OCEAN_UNIF
USE MODI_PREP_OCEAN_NETCDF
USE MODI_PREP_OCEAN_ASCLLV
!
USE MODI_HOR_INTERPOL
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
USE MODI_ABOR1_SFX
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
 CHARACTER(LEN=6),   INTENT(IN)  :: HPROGRAM  ! program calling surf. schemes
 CHARACTER(LEN=28),  INTENT(IN)  :: HFILE     ! file name
 CHARACTER(LEN=6),   INTENT(IN)  :: HFILETYPE ! file type
INTEGER,            INTENT(IN)  :: KLUOUT    ! logical unit of output listing
LOGICAL,            INTENT(IN)  :: OUNIF     ! flag for prescribed uniform field
 CHARACTER(LEN=7)                :: HSURF   ! type of field
 CHARACTER(LEN=28),  INTENT(IN), OPTIONAL :: HNCVARNAME!var to read 
!
!
!*      0.2    declarations of local variables
!
REAL, POINTER, DIMENSION(:,:,:)    ::ZFIELDIN!field to interpolate horizontally
REAL, POINTER, DIMENSION(:,:)      ::ZFIELD  !field to interpolate horizontally
REAL, ALLOCATABLE, DIMENSION(:,:,:)::ZFIELDOUT!field interpolated horizontally
!
INTEGER                       :: JLEV    ! loop on oceanic vertical level
INTEGER                       :: IK1
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!----------------------------------------------------------------------------
!*      1.     Does the field exist?
!
!*      2.     Reading of input  configuration (Grid and interpolation type)
!
IF (LHOOK) CALL DR_HOOK('PREP_HOR_OCEAN_FIELD',0,ZHOOK_HANDLE)
!
IF (OUNIF) THEN
   WRITE(KLUOUT,*) '*****warning*****: you ask for uniform oceanic variables'
   CALL PREP_OCEAN_UNIF(KLUOUT,HSURF,ZFIELDIN)
ELSE IF (HFILETYPE=='NETCDF') THEN
   CALL PREP_OCEAN_NETCDF(HPROGRAM,HSURF,HFILE,HFILETYPE,KLUOUT,&
                         HNCVARNAME,ZFIELDIN)
ELSE IF (HFILETYPE=='ASCII') THEN
   WRITE(KLUOUT,*) 'PERSONAL LIB TEST FOR READING ',HFILETYPE,'file type'
   WRITE(KLUOUT,*) 'ASCII FILE MUST CONTAIN LAT,LON,DEPTH,T,S,U,V'
   CALL PREP_OCEAN_ASCLLV(HPROGRAM,HSURF,HFILE,KLUOUT,ZFIELDIN)                         
ELSE
  CALL ABOR1_SFX('PREP_OCEAN_HOR_FIELD: data file type not supported : '//HFILETYPE)
END IF
!
!-------------------------------------------------------------------------------
!
!*      3.     Horizontal interpolation
!
ALLOCATE(ZFIELDOUT  (SIZE(XLAT),SIZE(ZFIELDIN,2),SIZE(ZFIELDIN,3)) )
ALLOCATE(ZFIELD(SIZE(ZFIELDIN,1),SIZE(ZFIELDIN,3)))
!
DO JLEV=1,SIZE(ZFIELDIN,2)
  ZFIELD(:,:)=ZFIELDIN(:,JLEV,:)
  CALL HOR_INTERPOL(KLUOUT,ZFIELD,ZFIELDOUT(:,JLEV,:))
ENDDO
!
!*      5.     Return to historical variable
!
IK1=NOCKMIN+1
SELECT CASE (HSURF)
  CASE('TEMP_OC') 
    ALLOCATE(XSEAT(SIZE(ZFIELDOUT,1),NOCKMIN:NOCKMAX))
    ALLOCATE(XSEAT_REL(SIZE(ZFIELDOUT,1),NOCKMIN:NOCKMAX))
    DO JLEV=IK1,NOCKMAX
      XSEAT(:,JLEV) = ZFIELDOUT(:,JLEV,1)
      !prevoir interpolation sur la grille verticale si niveau différents
    ENDDO
    XSEAT(:,NOCKMIN)=XSEAT(:,IK1)
    !
    ! Relaxation Profile = initial profile for the steady regime
    ! Change it for seasonal cycle!! 
    XSEAT_REL(:,:) = XSEAT(:,:)
    !    
  CASE('SALT_OC') 
    ALLOCATE(XSEAS(SIZE(ZFIELDOUT,1),NOCKMIN:NOCKMAX))
    ALLOCATE(XSEAS_REL(SIZE(ZFIELDOUT,1),NOCKMIN:NOCKMAX))
    DO JLEV=IK1,NOCKMAX
      XSEAS(:,JLEV) = ZFIELDOUT(:,JLEV,1)
    ENDDO
    XSEAS(:,NOCKMIN)=XSEAS(:,IK1)
    !
    ! Relaxation Profile = initial profile for the steady regime
    ! Change it for seasonal cycle!! 
    XSEAS_REL(:,:) = XSEAS(:,:)
    !    
  CASE('UCUR_OC') 
    ALLOCATE(XSEAU(SIZE(ZFIELDOUT,1),NOCKMIN:NOCKMAX))
    ALLOCATE(XSEAU_REL(SIZE(ZFIELDOUT,1),NOCKMIN:NOCKMAX))
    DO JLEV=IK1,NOCKMAX
      XSEAU(:,JLEV) = ZFIELDOUT(:,JLEV,1)
    ENDDO
    XSEAU(:,NOCKMIN)=XSEAU(:,IK1)
    ! 
    IF (.NOT.LCURRENT) XSEAU(:,:)=0.
    !
    XSEAU_REL(:,:) = XSEAU(:,:)
    !
  CASE('VCUR_OC') 
    ALLOCATE(XSEAV(SIZE(ZFIELDOUT,1),NOCKMIN:NOCKMAX))
    ALLOCATE(XSEAV_REL(SIZE(ZFIELDOUT,1),NOCKMIN:NOCKMAX))
    DO JLEV=IK1,NOCKMAX
      XSEAV(:,JLEV) = ZFIELDOUT(:,JLEV,1)
    ENDDO
    XSEAV(:,NOCKMIN)=XSEAV(:,IK1)    
    !
    IF (.NOT.LCURRENT) XSEAV(:,:)=0.
    !
    XSEAV_REL(:,:) = XSEAV(:,:)
    !
END SELECT
!
!------------------------------------------------------------------------------
!
!*      6.     Deallocations
!
DEALLOCATE(ZFIELD   )
DEALLOCATE(ZFIELDOUT)
IF (LHOOK) CALL DR_HOOK('PREP_HOR_OCEAN_FIELD',1,ZHOOK_HANDLE)
!
END SUBROUTINE PREP_HOR_OCEAN_FIELD
