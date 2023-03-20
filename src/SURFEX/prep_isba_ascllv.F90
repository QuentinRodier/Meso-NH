!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
SUBROUTINE PREP_ISBA_ASCLLV (DTCO, UG, U, USS, S, IO, HPROGRAM,HSURF,KLUOUT,PFIELD)
!     #################################################################################
!
!!****  *PREP_ISBA_ASCLLV* - prepares ISBA field from prescribed values
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
!!     P. Le Moigne 
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    03/2007
!!------------------------------------------------------------------
!
!
!
!
USE MODD_SURFEX_MPI, ONLY : NPROC, NINDEX, NNUM, NCOMM, NPIO, NRANK
!
USE MODD_DATA_COVER_n, ONLY : DATA_COVER_t
USE MODD_DATA_ISBA_n, ONLY : DATA_ISBA_t
USE MODD_SURF_ATM_GRID_n, ONLY : SURF_ATM_GRID_t
USE MODD_SURF_ATM_n, ONLY : SURF_ATM_t
USE MODD_SSO_n, ONLY : SSO_t
USE MODD_ISBA_OPTIONS_n, ONLY :ISBA_OPTIONS_t 
USE MODD_ISBA_n, ONLY : ISBA_S_t
USE MODD_PREP,           ONLY : CINTERP_TYPE
USE MODD_PGD_GRID,       ONLY : NL,LLATLONMASK,CGRID,XGRID_PAR,NGRID_PAR
USE MODD_PGDWORK,        ONLY : CATYPE
USE MODD_DATA_COVER_PAR, ONLY : NVEGTYPE
USE MODD_SURF_PAR,       ONLY : XUNDEF
USE MODD_PREP_ISBA,      ONLY : CTYPE_HUG     , CTYPE_TG     , &
                                  CFILE_HUG_SURF, CFILE_TG_SURF, &
                                  CFILE_HUG_ROOT, CFILE_TG_ROOT, &
                                  CFILE_HUG_DEEP, CFILE_TG_DEEP, &
                                  CFILE_HUG_1,CFILE_TG_1     , &  
                                  CFILE_HUG_2,CFILE_TG_2     , &
                                  CFILE_HUG_3,CFILE_TG_3     , &
                                  CFILE_HUG_4,CFILE_TG_4     , &
                                  CFILE_HUG_5,CFILE_TG_5     , &
                                  CFILE_HUG_6,CFILE_TG_6     , &
                                  CFILE_HUG_7,CFILE_TG_7     , &
                                  CFILE_HUG_8,CFILE_TG_8     , &
                                  CFILE_HUG_9,CFILE_TG_9     , &
                                  CFILE_HUG_10,CFILE_TG_10     , &
                                  CFILE_HUG_11,CFILE_TG_11     , &
                                  CFILE_HUG_12,CFILE_TG_12     , &
                                  CFILE_HUG_13,CFILE_TG_13     , &
                                  CFILE_HUG_14,CFILE_TG_14
USE MODI_PGD_FIELD
USE MODI_GET_LATLONMASK_n
USE MODI_ABOR1_SFX
USE MODI_VEGTYPE_TO_PATCH
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
USE MODI_GET_TYPE_DIM_n
!
IMPLICIT NONE
!
#ifdef SFX_MPI
INCLUDE "mpif.h"
#endif
!
!*      0.1    declarations of arguments
!
!
TYPE(DATA_COVER_t), INTENT(INOUT) :: DTCO
TYPE(SURF_ATM_GRID_t), INTENT(INOUT) :: UG
TYPE(SURF_ATM_t), INTENT(INOUT) :: U
TYPE(SSO_t), INTENT(INOUT) :: USS
TYPE(ISBA_S_t), INTENT(INOUT) :: S
TYPE(ISBA_OPTIONS_t) :: IO
!
 CHARACTER(LEN=6),   INTENT(IN)  :: HPROGRAM  ! program calling surf. schemes
 CHARACTER(LEN=7),   INTENT(IN)  :: HSURF     ! type of field
INTEGER,            INTENT(IN)  :: KLUOUT    ! logical unit of output listing
REAL, POINTER, DIMENSION(:,:,:) :: PFIELD    ! field to interpolate horizontally
!
!*      0.2    declarations of local variables
!
INTEGER :: JV ! loop counter
INTEGER :: JT ! loop counter for patch
INTEGER :: JLAYER
INTEGER :: JPATCH ! patch number corresponding to a kind of vegtype
INTEGER :: IL
!
INTEGER, DIMENSION(0:NPROC-1) :: INB
INTEGER :: INFOMPI, JJ
!
REAL, ALLOCATABLE, DIMENSION(:,:)     :: ZFIELD
REAL, ALLOCATABLE, DIMENSION(:,:,:)     :: ZFIELD2
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!
!*      0.3    declarations of namelist

INTEGER, PARAMETER :: NVEGTYPE_MAX = 20

!-------------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('PREP_ISBA_ASCLLV',0,ZHOOK_HANDLE)
!
IF ((.NOT.ALLOCATED(NINDEX)).AND.(HPROGRAM=='MESONH')) THEN 
  ALLOCATE(NINDEX(U%NDIM_FULL))
  NINDEX(:) = 0
ENDIF
!
IF (.NOT.ALLOCATED(NNUM)) THEN
  ALLOCATE(NNUM(U%NDIM_FULL))
  IF (NRANK/=NPIO) THEN
    IF (ALLOCATED(NINDEX)) DEALLOCATE(NINDEX)
    ALLOCATE(NINDEX(U%NDIM_FULL))
  ENDIF
  IF (NRANK==NPIO) THEN
    INB(:) = 0
    DO JJ=1,U%NDIM_FULL
      INB(NINDEX(JJ)) = INB(NINDEX(JJ))+1
      NNUM(JJ) = INB(NINDEX(JJ))
    ENDDO
  ENDIF
  IF (NPROC>1) THEN
#ifdef SFX_MPI          
    CALL MPI_BCAST(NINDEX,SIZE(NINDEX)*KIND(NINDEX)/4,MPI_INTEGER,NPIO,NCOMM,INFOMPI)
    CALL MPI_BCAST(NNUM,SIZE(NNUM)*KIND(NNUM)/4,MPI_INTEGER,NPIO,NCOMM,INFOMPI)
    CALL MPI_BCAST(UG%NGRID_FULL_PAR,KIND(UG%NGRID_FULL_PAR)/4,MPI_INTEGER,NPIO,NCOMM,INFOMPI)
#endif
    IF (NRANK/=NPIO) ALLOCATE(UG%XGRID_FULL_PAR(UG%NGRID_FULL_PAR))
#ifdef SFX_MPI    
    CALL MPI_BCAST(UG%XGRID_FULL_PAR,&
      SIZE(UG%XGRID_FULL_PAR)*KIND(UG%XGRID_FULL_PAR)/4,MPI_REAL,NPIO,NCOMM,INFOMPI)  
#endif
  ENDIF
ENDIF
!
CATYPE = 'ARI'
!
!*      1.    get full dimension of grid
!
 CALL GET_TYPE_DIM_n(DTCO, U, 'FULL  ',NL)
!
!*      2.    get nature dimension
!
 CALL GET_TYPE_DIM_n(DTCO, U, 'NATURE',IL)
!
ALLOCATE(ZFIELD2(IL,14,19))
ALLOCATE(ZFIELD(IL,3))
!
!*      3.    get grid informations known over full grid
!
 CALL GET_LATLONMASK_n(UG, LLATLONMASK,CGRID,XGRID_PAR,NGRID_PAR)
!
!
!
!
!*      4A.    Profile of soil relative humidity
!

IF(IO%CISBA=='DIF') THEN

  SELECT CASE(HSURF)

    CASE('WG     ')

      DO JT = 1 ,19


        CALL PGD_FIELD(DTCO, UG, U, USS, HPROGRAM,'LAYER1: relative humidity','NAT',CFILE_HUG_1(JT),   &
                        CTYPE_HUG,XUNDEF,ZFIELD2(:,1,JT),PVEGTYPE=S%XVEGTYPE(:,JT))  
        CALL PGD_FIELD(DTCO, UG, U, USS, HPROGRAM,'LAYER2: relative humidity','NAT',CFILE_HUG_2(JT),   &
                        CTYPE_HUG,XUNDEF,ZFIELD2(:,2,JT),PVEGTYPE=S%XVEGTYPE(:,JT))  
        CALL PGD_FIELD(DTCO, UG, U, USS, HPROGRAM,'LAYER3: relative humidity','NAT',CFILE_HUG_3(JT),   &
                        CTYPE_HUG,XUNDEF,ZFIELD2(:,3,JT),PVEGTYPE=S%XVEGTYPE(:,JT))  
        CALL PGD_FIELD(DTCO, UG, U, USS, HPROGRAM,'LAYER4: relative humidity','NAT',CFILE_HUG_4(JT),   &
                        CTYPE_HUG,XUNDEF,ZFIELD2(:,4,JT),PVEGTYPE=S%XVEGTYPE(:,JT))  
        CALL PGD_FIELD(DTCO, UG, U, USS, HPROGRAM,'LAYER5: relative humidity','NAT',CFILE_HUG_5(JT),   &
                        CTYPE_HUG,XUNDEF,ZFIELD2(:,5,JT),PVEGTYPE=S%XVEGTYPE(:,JT))  
        CALL PGD_FIELD(DTCO, UG, U, USS, HPROGRAM,'LAYER6: relative humidity','NAT',CFILE_HUG_6(JT),   &
                        CTYPE_HUG,XUNDEF,ZFIELD2(:,6,JT),PVEGTYPE=S%XVEGTYPE(:,JT)) 
        CALL PGD_FIELD(DTCO, UG, U, USS, HPROGRAM,'LAYER7: relative humidity','NAT',CFILE_HUG_7(JT),   &
                        CTYPE_HUG,XUNDEF,ZFIELD2(:,7,JT),PVEGTYPE=S%XVEGTYPE(:,JT))  
        CALL PGD_FIELD(DTCO, UG, U, USS, HPROGRAM,'LAYER8: relative humidity','NAT',CFILE_HUG_8(JT),   &
                        CTYPE_HUG,XUNDEF,ZFIELD2(:,8,JT),PVEGTYPE=S%XVEGTYPE(:,JT))  
        CALL PGD_FIELD(DTCO, UG, U, USS, HPROGRAM,'LAYER9: relative humidity','NAT',CFILE_HUG_9(JT),   &
                        CTYPE_HUG,XUNDEF,ZFIELD2(:,9,JT),PVEGTYPE=S%XVEGTYPE(:,JT))
        CALL PGD_FIELD(DTCO, UG, U, USS, HPROGRAM,'LAYER10: relative humidity','NAT',CFILE_HUG_10(JT),   &
                        CTYPE_HUG,XUNDEF,ZFIELD2(:,10,JT),PVEGTYPE=S%XVEGTYPE(:,JT))  
        CALL PGD_FIELD(DTCO, UG, U, USS, HPROGRAM,'LAYER11: relative humidity','NAT',CFILE_HUG_11(JT),   &
                        CTYPE_HUG,XUNDEF,ZFIELD2(:,11,JT),PVEGTYPE=S%XVEGTYPE(:,JT))  
        CALL PGD_FIELD(DTCO, UG, U, USS, HPROGRAM,'LAYER12: relative humidity','NAT',CFILE_HUG_12(JT),   &
                        CTYPE_HUG,XUNDEF,ZFIELD2(:,12,JT),PVEGTYPE=S%XVEGTYPE(:,JT)) 
        CALL PGD_FIELD(DTCO, UG, U, USS, HPROGRAM,'LAYER13: relative humidity','NAT',CFILE_HUG_13(JT),   &
                        CTYPE_HUG,XUNDEF,ZFIELD2(:,13,JT),PVEGTYPE=S%XVEGTYPE(:,JT))  
        CALL PGD_FIELD(DTCO, UG, U, USS, HPROGRAM,'LAYER14: relative humidity','NAT',CFILE_HUG_14(JT),   &
                        CTYPE_HUG,XUNDEF,ZFIELD2(:,14,JT),PVEGTYPE=S%XVEGTYPE(:,JT))

      END DO


      ALLOCATE(PFIELD(IL,14,NVEGTYPE))
!      DO JV=1,NVEGTYPE
!        JPATCH=VEGTYPE_TO_PATCH(JV, IO%NPATCH)
      PFIELD(:,:,:) = ZFIELD2(:,:,:)
!      END DO



!*      5A.    Profile of temperatures

    CASE('TG     ')


      DO JT = 1, 19


        CALL PGD_FIELD(DTCO, UG, U, USS, HPROGRAM,'LAYER1: temperature','NAT',CFILE_TG_1(JT),   &
                        CTYPE_TG,XUNDEF,ZFIELD2(:,1,JT),PVEGTYPE=S%XVEGTYPE(:,JT))  
        CALL PGD_FIELD(DTCO, UG, U, USS, HPROGRAM,'LAYER2: temperature','NAT',CFILE_TG_2(JT),   &
                        CTYPE_TG,XUNDEF,ZFIELD2(:,2,JT),PVEGTYPE=S%XVEGTYPE(:,JT))  
        CALL PGD_FIELD(DTCO, UG, U, USS, HPROGRAM,'LAYER3: temperature','NAT',CFILE_TG_3(JT),   &
                        CTYPE_TG,XUNDEF,ZFIELD2(:,3,JT),PVEGTYPE=S%XVEGTYPE(:,JT))  
        CALL PGD_FIELD(DTCO, UG, U, USS, HPROGRAM,'LAYER4: temperature','NAT',CFILE_TG_4(JT),   &
                        CTYPE_TG,XUNDEF,ZFIELD2(:,4,JT),PVEGTYPE=S%XVEGTYPE(:,JT))  
        CALL PGD_FIELD(DTCO, UG, U, USS, HPROGRAM,'LAYER5: temperature','NAT',CFILE_TG_5(JT),   &
                        CTYPE_TG,XUNDEF,ZFIELD2(:,5,JT),PVEGTYPE=S%XVEGTYPE(:,JT))  
        CALL PGD_FIELD(DTCO, UG, U, USS, HPROGRAM,'LAYER6: temperature','NAT',CFILE_TG_6(JT),   &
                        CTYPE_TG,XUNDEF,ZFIELD2(:,6,JT),PVEGTYPE=S%XVEGTYPE(:,JT))
        CALL PGD_FIELD(DTCO, UG, U, USS, HPROGRAM,'LAYER7: temperature','NAT',CFILE_TG_7(JT),   &
                        CTYPE_TG,XUNDEF,ZFIELD2(:,7,JT),PVEGTYPE=S%XVEGTYPE(:,JT))  
        CALL PGD_FIELD(DTCO, UG, U, USS, HPROGRAM,'LAYER8: temperature','NAT',CFILE_TG_8(JT),   &
                        CTYPE_TG,XUNDEF,ZFIELD2(:,8,JT),PVEGTYPE=S%XVEGTYPE(:,JT))  
        CALL PGD_FIELD(DTCO, UG, U, USS, HPROGRAM,'LAYER9: temperature','NAT',CFILE_TG_9(JT),   &
                        CTYPE_TG,XUNDEF,ZFIELD2(:,9,JT),PVEGTYPE=S%XVEGTYPE(:,JT))
        CALL PGD_FIELD(DTCO, UG, U, USS, HPROGRAM,'LAYER10: temperature','NAT',CFILE_TG_10(JT),   &
                        CTYPE_TG,XUNDEF,ZFIELD2(:,10,JT),PVEGTYPE=S%XVEGTYPE(:,JT))  
        CALL PGD_FIELD(DTCO, UG, U, USS, HPROGRAM,'LAYER11: temperature','NAT',CFILE_TG_11(JT),   &
                        CTYPE_TG,XUNDEF,ZFIELD2(:,11,JT),PVEGTYPE=S%XVEGTYPE(:,JT))  
        CALL PGD_FIELD(DTCO, UG, U, USS, HPROGRAM,'LAYER12: temperature','NAT',CFILE_TG_12(JT),   &
                        CTYPE_TG,XUNDEF,ZFIELD2(:,12,JT),PVEGTYPE=S%XVEGTYPE(:,JT))
        CALL PGD_FIELD(DTCO, UG, U, USS, HPROGRAM,'LAYER13: temperature','NAT',CFILE_TG_13(JT),   &
                        CTYPE_TG,XUNDEF,ZFIELD2(:,13,JT),PVEGTYPE=S%XVEGTYPE(:,JT))  
        CALL PGD_FIELD(DTCO, UG, U, USS, HPROGRAM,'LAYER14: temperature','NAT',CFILE_TG_14(JT),   &
                        CTYPE_TG,XUNDEF,ZFIELD2(:,14,JT),PVEGTYPE=S%XVEGTYPE(:,JT))  

      END DO


      ALLOCATE(PFIELD(IL,14,NVEGTYPE))
!      DO JV=1,NVEGTYPE
!        JPATCH=VEGTYPE_TO_PATCH(JV, IO%NPATCH)
      PFIELD(:,:,:) = ZFIELD2(:,:,:)
!      END DO


  CASE DEFAULT
    CALL ABOR1_SFX('PREP_ISBA_ASCLLV: '//TRIM(HSURF)//" initialization not implemented !")
  
  END SELECT    

ELSE

!*      4B.    Profile of soil relative humidity


  SELECT CASE(HSURF)


    CASE('WG     ')

      CALL PGD_FIELD(DTCO, UG, U, USS, HPROGRAM,'HUG_SURF: relative humidity','NAT',CFILE_HUG_SURF,   &
                        CTYPE_HUG,XUNDEF,ZFIELD(:,1))  
      CALL PGD_FIELD(DTCO, UG, U, USS, HPROGRAM,'HUG_ROOT: relative humidity','NAT',CFILE_HUG_ROOT,   &
                        CTYPE_HUG,XUNDEF,ZFIELD(:,2))  
      CALL PGD_FIELD(DTCO, UG, U, USS, HPROGRAM,'HUG_DEEP: relative humidity','NAT',CFILE_HUG_DEEP,   &
                        CTYPE_HUG,XUNDEF,ZFIELD(:,3))  

      ALLOCATE(PFIELD(IL,3,NVEGTYPE))
      DO JV=1,NVEGTYPE
        PFIELD(:,1,JV) = ZFIELD(:,1)
        PFIELD(:,2,JV) = ZFIELD(:,2)
        PFIELD(:,3,JV) = ZFIELD(:,3)
      END DO

!*      5B.    Profile of temperatures

    CASE('TG     ')

      CALL PGD_FIELD(DTCO, UG, U, USS, HPROGRAM,'TG_SURF: temperature','NAT',CFILE_TG_SURF,   &
                        CTYPE_TG,XUNDEF,ZFIELD(:,1))  
      CALL PGD_FIELD(DTCO, UG, U, USS, HPROGRAM,'TG_ROOT: temperature','NAT',CFILE_TG_ROOT,   &
                        CTYPE_TG,XUNDEF,ZFIELD(:,2))  
      CALL PGD_FIELD(DTCO, UG, U, USS, HPROGRAM,'TG_DEEP: temperature','NAT',CFILE_TG_DEEP,   &
                        CTYPE_TG,XUNDEF,ZFIELD(:,3))  

      ALLOCATE(PFIELD(IL,3,NVEGTYPE))
      DO JV=1,NVEGTYPE
        PFIELD(:,1,JV) = ZFIELD(:,1)
        PFIELD(:,2,JV) = ZFIELD(:,2)
        PFIELD(:,3,JV) = ZFIELD(:,3)
      END DO

!
    CASE DEFAULT
      CALL ABOR1_SFX('PREP_ISBA_ASCLLV: '//TRIM(HSURF)//" initialization not implemented !")
!
  END SELECT

ENDIF
!
!*      6.     Interpolation method
!              --------------------
!
CINTERP_TYPE='NONE  '
DEALLOCATE(ZFIELD)
DEALLOCATE(ZFIELD2)
!
DEALLOCATE(NNUM)
IF (NRANK/=NPIO) THEN
  DEALLOCATE(NINDEX,UG%XGRID_FULL_PAR)
  ALLOCATE(NINDEX(0))
ENDIF
!
IF (LHOOK) CALL DR_HOOK('PREP_ISBA_ASCLLV',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------------
END SUBROUTINE PREP_ISBA_ASCLLV
