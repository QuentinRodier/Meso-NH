!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
SUBROUTINE PREP_HOR_ISBA_ESC_FIELD (DTCO, UG, U, USS, GCP, IG, IO, S, NK, NP, NPE, NPAR_VEG_IRR_USE, &
                                    HPROGRAM,HSURF,HATMFILE,HATMFILETYPE,HPGDFILE,HPGDFILETYPE,YDCTL)
!     #################################################################################
!
!!****  *PREP_HOR_ISBA_ESC_FIELD* - reads, interpolates and prepares an ISBA Explicit Soil Carbon field
!                                   only external case implemeted
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
!!     B. Decharme 
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    05/2020
!!------------------------------------------------------------------
!
!
USE MODD_SURFEX_MPI,       ONLY : NRANK, NPIO, NCOMM, NPROC
!
USE MODD_DATA_COVER_n,     ONLY : DATA_COVER_t
!
USE MODD_SFX_GRID_n,       ONLY : GRID_t
USE MODD_ISBA_OPTIONS_n,   ONLY : ISBA_OPTIONS_t
USE MODD_ISBA_n,           ONLY : ISBA_NK_t, ISBA_NP_t, ISBA_NPE_t, ISBA_K_t, ISBA_S_t, &
                                  ISBA_PE_t, ISBA_P_t
!
USE MODD_SURF_ATM_GRID_n,  ONLY : SURF_ATM_GRID_t
USE MODD_SURF_ATM_n,       ONLY : SURF_ATM_t
USE MODD_SSO_n,            ONLY : SSO_t
USE MODD_GRID_CONF_PROJ_n, ONLY : GRID_CONF_PROJ_t
!
USE MODD_PREP,        ONLY : LINTERP, CMASK
!
USE MODD_PREP_ISBA,   ONLY : LRESET_CSOIL, XGRID_SOIL
!
USE MODD_DATA_COVER_PAR, ONLY : NVEGTYPE
USE MODD_SURF_PAR,       ONLY : NFILENAMELGTMAX, XUNDEF, NUNDEF
USE MODD_AGRI,           ONLY : NVEG_IRR
!
USE MODE_PREP_CTL, ONLY : PREP_CTL, PREP_CTL_INT_PART2, PREP_CTL_INT_PART4
!
USE MODI_READ_PREP_ISBA_CONF
USE MODI_ABOR1_SFX
USE MODI_HOR_INTERPOL
USE MODI_VEGTYPE_GRID_TO_PATCH_GRID
USE MODI_GET_LUOUT
USE MODI_PREP_ISBA_ESC_EXTERN
USE MODI_PUT_ON_ALL_VEGTYPES
USE MODI_GET_PREP_INTERP
USE MODI_PACK_SAME_RANK
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
#ifdef SFX_MPI
INCLUDE "mpif.h"
#endif
!
!*      0.1    declarations of arguments
!
TYPE(DATA_COVER_t), INTENT(INOUT) :: DTCO
!
TYPE(GRID_t), INTENT(INOUT) :: IG
TYPE(ISBA_OPTIONS_t), INTENT(INOUT) :: IO
TYPE(ISBA_S_t), INTENT(INOUT) :: S
TYPE(ISBA_NK_t), INTENT(INOUT) :: NK
TYPE(ISBA_NP_t), INTENT(INOUT) :: NP
TYPE(ISBA_NPE_t), INTENT(INOUT) :: NPE
!
TYPE(SURF_ATM_GRID_t), INTENT(INOUT) :: UG
TYPE(SURF_ATM_t), INTENT(INOUT) :: U
TYPE(SSO_t), INTENT(INOUT) :: USS
TYPE(GRID_CONF_PROJ_t),INTENT(INOUT) :: GCP
!
TYPE (PREP_CTL),    INTENT(INOUT) :: YDCTL
!
!
INTEGER,DIMENSION(:), INTENT(IN) :: NPAR_VEG_IRR_USE ! vegtype with irrigation
CHARACTER(LEN=6),   INTENT(IN)  :: HPROGRAM  ! program calling surf. schemes
CHARACTER(LEN=8),   INTENT(IN)  :: HSURF     ! type of field
CHARACTER(LEN=NFILENAMELGTMAX), INTENT(IN)  :: HATMFILE    ! name of the Atmospheric file
CHARACTER(LEN=6),               INTENT(IN)  :: HATMFILETYPE! type of the Atmospheric file
CHARACTER(LEN=NFILENAMELGTMAX), INTENT(IN)  :: HPGDFILE    ! name of the Atmospheric file
CHARACTER(LEN=6),               INTENT(IN)  :: HPGDFILETYPE! type of the Atmospheric file
!
!*      0.2    declarations of local variables
!
CHARACTER(LEN=6)                      :: YFILETYPE ! type of input file
CHARACTER(LEN=NFILENAMELGTMAX)        :: YFILE     ! name of file
CHARACTER(LEN=6)                      :: YFILEPGDTYPE ! type of input file
CHARACTER(LEN=NFILENAMELGTMAX)        :: YFILEPGD     ! name of file
!
REAL, POINTER, DIMENSION(:,:,:,:)     :: ZFIELDIN  ! field to interpolate horizontally
REAL, POINTER, DIMENSION(:,:,:,:)     :: ZFIELDOUTP   ! field interpolated   horizontally
REAL, POINTER, DIMENSION(:,:,:,:)     :: ZFIELDOUTV ! field interpolated   horizontally
!
REAL, POINTER, DIMENSION(:,:)         :: ZFIELD, ZPATCH    ! field to interpolate horizontally
!
TYPE(ISBA_K_t),  POINTER :: KK
TYPE(ISBA_P_t),  POINTER :: PK
TYPE(ISBA_PE_t), POINTER :: PEK
!
TYPE FOUT
  REAL, DIMENSION(:,:,:), ALLOCATABLE :: ZOUT
END TYPE FOUT
TYPE NFOUT
  TYPE(FOUT), DIMENSION(:), ALLOCATABLE :: AL
END TYPE NFOUT
TYPE(NFOUT) :: ZW  ! work array by patch (x, fine   soil grid)
TYPE(NFOUT) :: ZF  ! work array by patch (x, output soil grid)
!
!
INTEGER                       :: ILUOUT! output listing logical unit
!
LOGICAL                       :: GUNIF ! flag for prescribed uniform field
LOGICAL                       :: GPREP ! flag to prepare ags field (only external case implemeted)
!
INTEGER                       :: JPATCH    ! loop on patches
INTEGER                       :: JVEGTYPE  ! loop on vegtypes
INTEGER                       :: INI, INL, INS, INP, JJ, JL, JS, JP ! Work integer
INTEGER                       :: INFOMPI
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('PREP_HOR_ISBA_ESC_FIELD',0,ZHOOK_HANDLE)
!
!*      1.     Reading of input file name and type
!
CALL GET_LUOUT(HPROGRAM,ILUOUT)
!
CALL READ_PREP_ISBA_CONF(HPROGRAM,HSURF,YFILE,YFILETYPE,YFILEPGD,YFILEPGDTYPE,   &
                         HATMFILE,HATMFILETYPE,HPGDFILE,HPGDFILETYPE,ILUOUT,GUNIF)
!
CMASK = 'NATURE'
!
INI=SIZE(IG%XLAT)
!
GPREP = .TRUE.
!
!-------------------------------------------------------------------------------------
!
!*      2.     Reading of input  configuration (Grid and interpolation type)
!
NULLIFY (ZFIELDIN, ZFIELDOUTP, ZFIELDOUTV)
!
IF (YDCTL%LPART1) THEN
!
  IF(LRESET_CSOIL)GUNIF=.TRUE.
!
  IF (GUNIF) THEN
     GPREP = .FALSE.
  ELSE IF (YFILETYPE=='ASCLLV') THEN
     GPREP = .FALSE.
  ELSE IF (YFILETYPE=='GRIB  ') THEN
     GPREP = .FALSE.
  ELSE IF (YFILETYPE=='MESONH' .OR. YFILETYPE=='ASCII ' .OR. YFILETYPE=='LFI   '&
          .OR.YFILETYPE=='FA    '.OR. YFILETYPE=='AROME '.OR.YFILETYPE=='NC    ') THEN     
     CALL PREP_ISBA_ESC_EXTERN(GCP,HPROGRAM,HSURF,YFILE,YFILETYPE,YFILEPGD,YFILEPGDTYPE,ILUOUT,ZFIELDIN,GPREP)
  ELSE IF (YFILETYPE=='BUFFER') THEN
     GPREP = .FALSE.
  ELSE IF (YFILETYPE=='NETCDF') THEN
     GPREP = .FALSE.
  ELSE
     CALL ABOR1_SFX('PREP_HOR_ISBA_ESC_FIELD: data file type not supported : '//YFILETYPE)
  END IF
!
  INL = SIZE(ZFIELDIN,2)
  INS = SIZE(ZFIELDIN,3)
  INP = SIZE(ZFIELDIN,4)
!
ENDIF
!-------------------------------------------------------------------------------------
!
!*      3.     Horizontal interpolation
!
CALL PREP_CTL_INT_PART2 (YDCTL, HSURF, CMASK, 'NATURE', ZFIELDIN)
!
IF (YDCTL%LPART3) THEN
!
  IF(GPREP)THEN
    !
    IF (NRANK==NPIO) THEN
      INL = SIZE(ZFIELDIN,2)
      INP = SIZE(ZFIELDIN,3)
    ELSEIF (.NOT.ASSOCIATED(ZFIELDIN)) THEN
      ALLOCATE(ZFIELDIN(0,0,0,0))
    ENDIF
    !
    IF (NPROC>1) THEN
#ifdef SFX_MPI
      CALL MPI_BCAST(INL,KIND(INL)/4,MPI_INTEGER,NPIO,NCOMM,INFOMPI)
      CALL MPI_BCAST(INP,KIND(INP)/4,MPI_INTEGER,NPIO,NCOMM,INFOMPI)
#endif
    ENDIF
    ALLOCATE(ZFIELDOUTP(INI,INL,INS,INP))
    !
    ALLOCATE(ZPATCH(INI,INP))
    ZPATCH(:,:) = 0.
    !
    CALL GET_PREP_INTERP(INP,IO%NPATCH,S%XVEGTYPE,S%XPATCH,ZPATCH,NPAR_VEG_IRR_USE)
    !
    ALLOCATE(ZFIELD(SIZE(ZFIELDIN,1),INL))
    DO JS = 1, INS
       DO JPATCH = 1, INP
 !we interpolate each point the output patch is present
         ZFIELD(:,:)=ZFIELDIN(:,:,JS,JPATCH)
         LINTERP(:) = (ZPATCH(:,JPATCH) > 0.)
         CALL HOR_INTERPOL(DTCO,U,GCP,ILUOUT,ZFIELD(:,:),ZFIELDOUTP(:,:,JS,JPATCH))
         LINTERP(:) = .TRUE.
      ENDDO
    ENDDO
    !
    DEALLOCATE(ZFIELD)
    DEALLOCATE(ZFIELDIN)
    !
  ENDIF
  !
ENDIF
!
CALL PREP_CTL_INT_PART4 (YDCTL, HSURF, 'NATURE', CMASK, ZFIELDIN, ZFIELDOUTP)
!
IF (YDCTL%LPART5) THEN
!
  ALLOCATE(ZW%AL(IO%NPATCH))
!
  IF (GPREP) THEN
!
    INL = SIZE(ZFIELDOUTP,2)
    INS = SIZE(ZFIELDOUTP,3)
    INP = SIZE(ZFIELDOUTP,4)
!
    IF (IO%NPATCH/=INP) THEN
!
      ALLOCATE(ZFIELDOUTV(INI,INL,INS,NVEGTYPE+NVEG_IRR))
      DO JS = 1, INS
         CALL PUT_ON_ALL_VEGTYPES(INI,INL,INP,NVEGTYPE,NPAR_VEG_IRR_USE,ZFIELDOUTP(:,:,JS,:),ZFIELDOUTV(:,:,JS,:))
      ENDDO
!
      DEALLOCATE(ZFIELDOUTP)
!
!*    Transformation from vegtype grid to patch grid
!
      DO JP = 1,IO%NPATCH
        PK => NP%AL(JP)
!
        ALLOCATE(ZW%AL(JP)%ZOUT(PK%NSIZE_P,INL,INS))
!
        DO JS = 1,INS
           CALL VEGTYPE_GRID_TO_PATCH_GRID(JP,IO%NPATCH,PK%XVEGTYPE_PATCH,PK%XPATCH,&
                                        PK%NR_P,ZFIELDOUTV(:,:,JS,:),ZW%AL(JP)%ZOUT(:,:,JS),NPAR_VEG_IRR_USE)
        ENDDO
      ENDDO
!
      DEALLOCATE(ZFIELDOUTV)
!
    ELSE
!
      DO JP = 1,IO%NPATCH
         PK => NP%AL(JP)
         ALLOCATE(ZW%AL(JP)%ZOUT(PK%NSIZE_P,INL,INS))
        DO JS = 1,INS
           CALL PACK_SAME_RANK(PK%NR_P,ZFIELDOUTP(:,:,JS,JP),ZW%AL(JP)%ZOUT(:,:,JS))
        ENDDO
      ENDDO
!
      DEALLOCATE(ZFIELDOUTP)
!
    ENDIF
!
  ELSE
!
    DO JP = 1,IO%NPATCH
      PK => NP%AL(JP)
      PEK => NPE%AL(JP)
!
      SELECT CASE (HSURF)
!
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!
       CASE('SURF_LIT','SOIL_LIT') 
        ALLOCATE(ZW%AL(JP)%ZOUT(PK%NSIZE_P,IO%NGROUND_LAYER,IO%NNLITTER))
        ZW%AL(JP)%ZOUT(:,:,:) = 0.0
!
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!
       CASE('SURF_LIG','SOIL_LIG') 
        ALLOCATE(ZW%AL(JP)%ZOUT(PK%NSIZE_P,IO%NGROUND_LAYER,1))
        ZW%AL(JP)%ZOUT(:,:,:) = 0.0
!
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!
       CASE('SOILCARB') 
        ALLOCATE(ZW%AL(JP)%ZOUT(PK%NSIZE_P,IO%NGROUND_LAYER,IO%NNSOILCARB))
        ZW%AL(JP)%ZOUT(:,:,:) = 0.0
!
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!
       CASE('SGASO2','SGASCO2','SGASCH4') 
        ALLOCATE(ZW%AL(JP)%ZOUT(PK%NSIZE_P,IO%NGROUND_LAYER,1))
        ZW%AL(JP)%ZOUT(:,:,:) = 0.0
!
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!
      END SELECT
!
    ENDDO
!
   ENDIF
!
!-------------------------------------------------------------------------------------
!
!*      7.     Return to historical variable
!
!
   SELECT CASE (HSURF)
!
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!
    CASE('SURF_LIT') 
      DO JP = 1,IO%NPATCH
         PEK => NPE%AL(JP)
         PK => NP%AL(JP)            
         ALLOCATE(PEK%XSURFACE_LITTER(PK%NSIZE_P,IO%NNLITTER))
         DO JS = 1, IO%NNLITTER
            WHERE(PK%NWG_LAYER(:)==NUNDEF)
                 PEK%XSURFACE_LITTER(:,JS) = XUNDEF
            ELSEWHERE
                 PEK%XSURFACE_LITTER(:,JS) = ZW%AL(JP)%ZOUT(:,1,JS)
            ENDWHERE
         ENDDO
      ENDDO
!
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!
    CASE('SURF_LIG') 
     DO JP = 1,IO%NPATCH
        PEK => NPE%AL(JP)
        PK => NP%AL(JP)            
        ALLOCATE(PEK%XSURFACE_LIGNIN_STRUC(PK%NSIZE_P))
        WHERE(PK%NWG_LAYER(:)==NUNDEF)
             PEK%XSURFACE_LIGNIN_STRUC(:) = XUNDEF
        ELSEWHERE
             PEK%XSURFACE_LIGNIN_STRUC(:) = ZW%AL(JP)%ZOUT(:,1,1)
        ENDWHERE
     ENDDO
!
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!
    CASE('SOILCARB') 
! 
     ALLOCATE(ZF%AL(IO%NPATCH))     
!
     DO JP = 1,IO%NPATCH
        PEK => NPE%AL(JP)
        PK => NP%AL(JP)  
!
        ALLOCATE(ZF%AL(JP)%ZOUT(PK%NSIZE_P,IO%NGROUND_LAYER,IO%NNSOILCARB))   
!
!       interpolates on output levels
        DO JS = 1, IO%NNSOILCARB
           CALL INIT_FROM_REF_GRID(XGRID_SOIL,ZW%AL(JP)%ZOUT(:,:,JS),PK%XDG,ZF%AL(JP)%ZOUT(:,:,JS))
        ENDDO
!
!       retrieves soil carbon content          
        ALLOCATE(PEK%XSOILDIF_CARB(PK%NSIZE_P,IO%NGROUND_LAYER,IO%NNSOILCARB))
        PEK%XSOILDIF_CARB(:,:,:)=XUNDEF
        DO JJ=1,PK%NSIZE_P
           IF(PK%NWG_LAYER(JJ)==NUNDEF)CYCLE
           DO JL=1,PK%NWG_LAYER(JJ)
              PEK%XSOILDIF_CARB(JJ,JL,:) = ZF%AL(JP)%ZOUT(JJ,JL,:)
          ENDDO
       ENDDO
!
       WHERE(ZF%AL(JP)%ZOUT(:,:,:)==XUNDEF) PEK%XSOILDIF_CARB(:,:,:)=XUNDEF
!
       DEALLOCATE(ZF%AL(JP)%ZOUT)
!
     ENDDO
!
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!
    CASE('SOIL_LIT') 
! 
     ALLOCATE(ZF%AL(IO%NPATCH))     
!
     DO JP = 1,IO%NPATCH
        PEK => NPE%AL(JP)
        PK => NP%AL(JP)  
!
        ALLOCATE(ZF%AL(JP)%ZOUT(PK%NSIZE_P,IO%NGROUND_LAYER,IO%NNLITTER))   
!
!       interpolates on output levels
        DO JS = 1, IO%NNLITTER
           CALL INIT_FROM_REF_GRID(XGRID_SOIL,ZW%AL(JP)%ZOUT(:,:,JS),PK%XDG,ZF%AL(JP)%ZOUT(:,:,JS))
        ENDDO
!
!       retrieves soil carbon content          
        ALLOCATE(PEK%XSOILDIF_LITTER(PK%NSIZE_P,IO%NGROUND_LAYER,IO%NNLITTER))
        PEK%XSOILDIF_LITTER(:,:,:)=XUNDEF
        DO JJ=1,PK%NSIZE_P
           IF(PK%NWG_LAYER(JJ)==NUNDEF)CYCLE
           DO JL=1,PK%NWG_LAYER(JJ)
              PEK%XSOILDIF_LITTER(JJ,JL,:) = ZF%AL(JP)%ZOUT(JJ,JL,:)
          ENDDO
       ENDDO
!
       WHERE(ZF%AL(JP)%ZOUT(:,:,:)==XUNDEF) PEK%XSOILDIF_LITTER(:,:,:)=XUNDEF
!
       DEALLOCATE(ZF%AL(JP)%ZOUT)
!
     ENDDO
!
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!
    CASE('SOIL_LIG') 
! 
     ALLOCATE(ZF%AL(IO%NPATCH))     
!
     DO JP = 1,IO%NPATCH
        PEK => NPE%AL(JP)
        PK => NP%AL(JP)  
!
        ALLOCATE(ZF%AL(JP)%ZOUT(PK%NSIZE_P,IO%NGROUND_LAYER,1))   
!
!       interpolates on output levels
        CALL INIT_FROM_REF_GRID(XGRID_SOIL,ZW%AL(JP)%ZOUT(:,:,1),PK%XDG,ZF%AL(JP)%ZOUT(:,:,1))
!
!       retrieves soil carbon content          
        ALLOCATE(PEK%XSOILDIF_LIGNIN_STRUC(PK%NSIZE_P,IO%NGROUND_LAYER))
        PEK%XSOILDIF_LIGNIN_STRUC(:,:)=XUNDEF
        DO JJ=1,PK%NSIZE_P
           IF(PK%NWG_LAYER(JJ)==NUNDEF)CYCLE
           DO JL=1,PK%NWG_LAYER(JJ)
              PEK%XSOILDIF_LIGNIN_STRUC(JJ,JL) = ZF%AL(JP)%ZOUT(JJ,JL,1)
          ENDDO
       ENDDO
!
       WHERE(ZF%AL(JP)%ZOUT(:,:,1)==XUNDEF) PEK%XSOILDIF_LIGNIN_STRUC(:,:)=XUNDEF
!
       DEALLOCATE(ZF%AL(JP)%ZOUT)
!
     ENDDO
!
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  
    CASE('SGASO2')
!
     DO JP = 1,IO%NPATCH
        PEK => NPE%AL(JP)
        PK => NP%AL(JP)  
!
        ALLOCATE(ZF%AL(JP)%ZOUT(PK%NSIZE_P,IO%NGROUND_LAYER,1))   
!
!       interpolates on output levels
        CALL INIT_FROM_REF_GRID(XGRID_SOIL,ZW%AL(JP)%ZOUT(:,:,1),PK%XDG,ZF%AL(JP)%ZOUT(:,:,1))
!
!       retrieves soil carbon content          
        ALLOCATE(PEK%XSGASO2(PK%NSIZE_P,IO%NGROUND_LAYER))
        PEK%XSGASO2(:,:)=XUNDEF
        DO JJ=1,PK%NSIZE_P
           IF(PK%NWG_LAYER(JJ)==NUNDEF)CYCLE
           DO JL=1,PK%NWG_LAYER(JJ)
              PEK%XSGASO2(JJ,JL) = ZF%AL(JP)%ZOUT(JJ,JL,1)
          ENDDO
       ENDDO
!
       WHERE(ZF%AL(JP)%ZOUT(:,:,1)==XUNDEF) PEK%XSGASO2(:,:)=XUNDEF
!
       DEALLOCATE(ZF%AL(JP)%ZOUT)
!
     ENDDO
!
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  
    CASE('SGASCO2')
!
     DO JP = 1,IO%NPATCH
        PEK => NPE%AL(JP)
        PK => NP%AL(JP)  
!
        ALLOCATE(ZF%AL(JP)%ZOUT(PK%NSIZE_P,IO%NGROUND_LAYER,1))   
!
!       interpolates on output levels
        CALL INIT_FROM_REF_GRID(XGRID_SOIL,ZW%AL(JP)%ZOUT(:,:,1),PK%XDG,ZF%AL(JP)%ZOUT(:,:,1))
!
!       retrieves soil carbon content          
        ALLOCATE(PEK%XSGASCO2(PK%NSIZE_P,IO%NGROUND_LAYER))
        PEK%XSGASCO2(:,:)=XUNDEF
        DO JJ=1,PK%NSIZE_P
           IF(PK%NWG_LAYER(JJ)==NUNDEF)CYCLE
           DO JL=1,PK%NWG_LAYER(JJ)
              PEK%XSGASCO2(JJ,JL) = ZF%AL(JP)%ZOUT(JJ,JL,1)
          ENDDO
       ENDDO
!
       WHERE(ZF%AL(JP)%ZOUT(:,:,1)==XUNDEF) PEK%XSGASCO2(:,:)=XUNDEF
!
       DEALLOCATE(ZF%AL(JP)%ZOUT)
!
     ENDDO
!
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  
    CASE('SGASCH4')
!
     DO JP = 1,IO%NPATCH
        PEK => NPE%AL(JP)
        PK => NP%AL(JP)  
!
        ALLOCATE(ZF%AL(JP)%ZOUT(PK%NSIZE_P,IO%NGROUND_LAYER,1))   
!
!       interpolates on output levels
        CALL INIT_FROM_REF_GRID(XGRID_SOIL,ZW%AL(JP)%ZOUT(:,:,1),PK%XDG,ZF%AL(JP)%ZOUT(:,:,1))
!
!       retrieves soil carbon content          
        ALLOCATE(PEK%XSGASCH4(PK%NSIZE_P,IO%NGROUND_LAYER))
        PEK%XSGASCH4(:,:)=XUNDEF
        DO JJ=1,PK%NSIZE_P
           IF(PK%NWG_LAYER(JJ)==NUNDEF)CYCLE
           DO JL=1,PK%NWG_LAYER(JJ)
              PEK%XSGASCH4(JJ,JL) = ZF%AL(JP)%ZOUT(JJ,JL,1)
          ENDDO
       ENDDO
!
       WHERE(ZF%AL(JP)%ZOUT(:,:,1)==XUNDEF) PEK%XSGASCH4(:,:)=XUNDEF
!
       DEALLOCATE(ZF%AL(JP)%ZOUT)
!
     ENDDO
!
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!
   END SELECT
!
   DO JP = 1,IO%NPATCH
     DEALLOCATE(ZW%AL(JP)%ZOUT)
   ENDDO
   DEALLOCATE(ZW%AL)
!
ENDIF
!
!-------------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('PREP_HOR_ISBA_ESC_FIELD',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------------
!-------------------------------------------------------------------------------------
!
CONTAINS
!
!-------------------------------------------------------------------------------------
!-------------------------------------------------------------------------------------
!
SUBROUTINE INIT_FROM_REF_GRID(PGRID1,PT1,PD2,PT2)
!
USE MODI_INTERP_GRID_NAT
!
REAL, DIMENSION(:,:), INTENT(IN)  :: PT1    ! variable profile
REAL, DIMENSION(:),   INTENT(IN)  :: PGRID1 ! normalized grid
REAL, DIMENSION(:,:), INTENT(IN)  :: PD2    ! output layer thickness
REAL, DIMENSION(:,:), INTENT(OUT) :: PT2    ! variable profile
!
INTEGER                                  :: JL  ! loop counter
REAL, DIMENSION(SIZE(PT1,1),SIZE(PT1,2)) :: ZD1 ! input grid
REAL, DIMENSION(SIZE(PD2,1),SIZE(PD2,2)) :: ZD2 ! output grid
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
IF (LHOOK) CALL DR_HOOK('PREP_HOR_ISBA_ESC_FIELD: INIT_FROM_REF_GRID',0,ZHOOK_HANDLE)
!
DO JL=1,SIZE(PT1,2)
   ZD1(:,JL) = PGRID1(JL)
ENDDO
!
ZD2(:,:) = PD2(:,:)
CALL INTERP_GRID_NAT(ZD1,PT1(:,:),ZD2,PT2(:,:))
!
IF (LHOOK) CALL DR_HOOK('PREP_HOR_ISBA_ESC_FIELD: INIT_FROM_REF_GRID',1,ZHOOK_HANDLE)
!
END SUBROUTINE INIT_FROM_REF_GRID
!-------------------------------------------------------------------------------------
!
END SUBROUTINE PREP_HOR_ISBA_ESC_FIELD
