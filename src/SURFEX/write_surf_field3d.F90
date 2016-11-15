!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE WRITE_SURF_FIELD3D(DGU, U, HPROGRAM,PFIELD3D,KFIRSTLAYER,KLASTLAYER,HFIELDNAME,HCOMMENT,HCOMMENTUNIT,HDIR,HNAM_DIM)
!     #####################################
!
!!****  *WRITE_SURF_FIELD3D* - writes surfex field in output file using WRITE_SURF,
!!                           layer by layer and patch by patch if needed in MESONH
!!                           with Z-parallel IO in MESO-NH, we force surfex to write 2D fields
!!                           because Z-parallel IO are not supported for 3D SURFEX fields.
!!                        
!!
!!    PURPOSE
!!    -------
!!      writes surfex field in output file using WRITE_SURF, layer by layer 
!!      and patch by patch if needed in MESONH
!!      and NB_PROCIO_W > 1
!!      examples of HFIELDNAME : 'TG', 'soil depth from ecoclimap'
!!      with Z-parallel IO in MESO-NH, we force surfex to write 2D fields
!!      because Z-parallel IO are not supported for 3D SURFEX fields.
!!
!!**  METHOD
!!    ------
!!
!!    EXTERNAL
!!    --------
!!
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!
!!    REFERENCE
!!    ---------
!!
!!
!!    AUTHOR
!!    ------
!!      M.Moge   *LA - UPS*	
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    08/01/2016
!!
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
USE MODD_SURF_PAR, ONLY : NUNDEF
USE MODD_DIAG_SURF_ATM_n, ONLY : DIAG_SURF_ATM_t
USE MODD_SURF_ATM_n, ONLY : SURF_ATM_t
!
USE MODI_WRITE_SURF
#ifdef SFX_MNH
USE MODI_GET_NB_PROCIO_WRITE_MNH
#endif
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*       0.1   Declarations of arguments
!              -------------------------
!
TYPE(DIAG_SURF_ATM_t), INTENT(INOUT) :: DGU
TYPE(SURF_ATM_t), INTENT(INOUT) :: U
CHARACTER(LEN=6),                 INTENT(IN) :: HPROGRAM     ! calling program
REAL, DIMENSION(:,:,:),           INTENT(IN) :: PFIELD3D     ! 3D field to be written
INTEGER,                          INTENT(IN) :: KFIRSTLAYER  ! first layer of PFIELD3D to be written
INTEGER,                          INTENT(IN) :: KLASTLAYER   ! last layer of PFIELD3D to be written
CHARACTER(LEN=LEN_HREC),                INTENT(IN) :: HFIELDNAME   ! name of the field PFIELD3D. Example : 'X_Y_TG'
CHARACTER(LEN=100),               INTENT(IN) :: HCOMMENT     ! Comment string
CHARACTER(LEN=100),               INTENT(IN) :: HCOMMENTUNIT ! unit of the datas in PFIELD3D
 CHARACTER(LEN=1),OPTIONAL,       INTENT(IN) :: HDIR ! type of field :
!                                             ! 'H' : field with
!                                             !       horizontal spatial dim.
!                                             ! '-' : no horizontal dim.
 CHARACTER(LEN=LEN_HREC), OPTIONAL,  INTENT(IN) :: HNAM_DIM
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
INTEGER           :: IRESP          ! IRESP  : return-code if a problem appears
INTEGER           :: ILAYER         ! number of layers in PFIELD3D
INTEGER           :: IPATCH         ! number of patches in PFIELD3D
CHARACTER(LEN=LEN_HREC) :: YRECFM         ! Name of the article to be read
CHARACTER(LEN=4 ) :: YLVL           ! current level/layer
CHARACTER(LEN=4 ) :: YPATCH         ! current patch
CHARACTER(LEN=100):: YCOMMENT       ! Comment string
INTEGER           :: INB_PROCIO     ! number of processes used for Z-parallel IO with MESO-NH
!
CHARACTER(LEN=1)   :: YDIR
INTEGER :: JJ, JLAYER ! loop counter on levels
INTEGER :: JPATCH  ! loop counter on patches
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!------------------------------------------------------------------------------
!
!
IF (LHOOK) CALL DR_HOOK('WRITE_SURF_FIELD3D',0,ZHOOK_HANDLE)
!
YDIR = 'H'
IF (PRESENT(HDIR)) YDIR = HDIR
!
ILAYER = SIZE( PFIELD3D, 2 )
IPATCH = SIZE( PFIELD3D, 3 )
!
INB_PROCIO = 1
#ifdef SFX_MNH
IF (HPROGRAM=='MESONH') THEN
  CALL GET_NB_PROCIO_WRITE_MNH( INB_PROCIO, IRESP )
ENDIF
#endif
!
IF ( INB_PROCIO > 1 ) THEN
!
  DO JLAYER=KFIRSTLAYER,KLASTLAYER
    WRITE(YLVL,'(I4)') JLAYER
    DO JPATCH=1,IPATCH
      WRITE(YPATCH,'(I4.4)') JPATCH
      YCOMMENT=ADJUSTL(HCOMMENT(:LEN_TRIM(HCOMMENT)))//ADJUSTL(YLVL(:LEN_TRIM(YLVL)))//'patch '//  &
        ADJUSTL(YPATCH(:LEN_TRIM(YPATCH)))//'  ('//ADJUSTL(HCOMMENTUNIT(:LEN_TRIM(HCOMMENTUNIT)))//')'
      YRECFM=ADJUSTL(HFIELDNAME(:LEN_TRIM(HFIELDNAME)))//ADJUSTL(YLVL(:LEN_TRIM(YLVL)))
      IF ( IPATCH > 1 ) THEN
        YRECFM=ADJUSTL(YRECFM(:LEN_TRIM(YRECFM)))//YPATCH
      ENDIF
    IF (PRESENT(HNAM_DIM)) THEN
      CALL WRITE_SURF(DGU, U,HPROGRAM,YRECFM,PFIELD3D(:,JLAYER,JPATCH),IRESP,HCOMMENT=YCOMMENT,HDIR=YDIR,HNAM_DIM=HNAM_DIM)
    ELSE
      CALL WRITE_SURF(DGU, U,HPROGRAM,YRECFM,PFIELD3D(:,JLAYER,JPATCH),IRESP,HCOMMENT=YCOMMENT,HDIR=YDIR)
    ENDIF
    ENDDO
  END DO
!
ELSE
!
  DO JLAYER=KFIRSTLAYER,KLASTLAYER
    WRITE(YLVL,'(I4)') JLAYER
    YCOMMENT=ADJUSTL(HCOMMENT(:LEN_TRIM(HCOMMENT)))//ADJUSTL(YLVL(:LEN_TRIM(YLVL)))//  &
      '  ('//ADJUSTL(HCOMMENTUNIT(:LEN_TRIM(HCOMMENTUNIT)))//')'
    YRECFM=ADJUSTL(HFIELDNAME(:LEN_TRIM(HFIELDNAME)))//ADJUSTL(YLVL(:LEN_TRIM(YLVL)))
    IF (PRESENT(HNAM_DIM)) THEN
      CALL WRITE_SURF(DGU, U,HPROGRAM,YRECFM,PFIELD3D(:,JLAYER,:),IRESP,HCOMMENT=YCOMMENT,HDIR=YDIR,HNAM_DIM=HNAM_DIM)
    ELSE
      CALL WRITE_SURF(DGU, U,HPROGRAM,YRECFM,PFIELD3D(:,JLAYER,:),IRESP,HCOMMENT=YCOMMENT,HDIR=YDIR)
    ENDIF
  END DO
!
ENDIF
!
IF (LHOOK) CALL DR_HOOK('WRITE_SURF_FIELD3D',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
      END SUBROUTINE WRITE_SURF_FIELD3D
