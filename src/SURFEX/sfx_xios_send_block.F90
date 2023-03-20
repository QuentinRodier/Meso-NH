!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
SUBROUTINE SFX_XIOS_SEND_BLOCK(HDTAG,PFIELD,PFIELD2,PFIELD3,HDOMAIN,HAXIS,HAXIS2,HDCOMMENT,KFREQOP)
!!
!!
!!     PURPOSE 
!!     --------
!!
!!     Front-end to XIOS for client models
!!
!!     It performs field declaration to XIOS if needed, provided it is
!!     not too late with respect to xios context definition closing
!!     (see sfx_xios_declare_field)  
!!
!!     It copes with client models which process fields by 'blocks'
!!     over the first dimension, and wish to send them by blocks too,
!!     a set of blocks being provided, duly ordered, between two
!!     calendar updates
!!
!!     It gathers field blocks and send them to XIOS, using
!!     xios_send_field, as soon as the field is complete (i.e. enough
!!     blocks have been received, compared to a MODD_XIOS variable)
!!
!!     METHOD :
!!     -------------------- 
!!     
!!     For each new field name received, create an entry in buffer
!!     array and records full MPI-task field size (as known by
!!     Xios). 
!!
!!     For all field names, add the block to the buffer and, if field
!!     is complete, send it to Xios and clears the buffer
!!
!!     EXTERNAL
!!     --------
!!
!!     XIOS LIBRARY
!!
!!
!!     REFERENCE
!!     ---------
!!
!!     XIOS Reference guide - Yann Meurdesoif - 10/10/2014 - 
!!     svn co --r 515 http://forge.ipsl.jussieu.fr/ioserver/svn/XIOS/branchs/xios-1.0 <dir> 
!!     cd <dir>/doc ; ....
!!
!!     AUTHOR
!!     ------
!!
!!     S.Sénési, CNRM
!!
!!     MODIFICATION
!!     --------------
!!
!!     Original    01/2016
!!     02/2017 S. Senesi    : add arg OFORRUN
!!     07/2017 D. St-Martin : bugfix for OSIACTIVE (initialization)
!!     11/2017 D. St-Martin : remove OSIACTIVE and OFORRUN
!!     01/2018 D. St-Martin : bugfix for 'once' XIOS field
!!     02/2018 D. St-Martin : check which 3D variables to be sent in inverse order
!!
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
!
USE MODD_XIOS, ONLY : LXIOS, LXIOS_DEF_CLOSED, NBLOCK, NTIMESTEP, LXIOS_INVERT_LEVELS
!
! NBLOCK dans arpege : YOMDIM:NGPBLKS
!
#ifdef WXIOS 
USE XIOS 
USE MODI_SFX_XIOS_DECLARE_FIELD
#endif
!
USE MODI_ABOR1_SFX
!
USE YOMHOOK , ONLY : LHOOK, DR_HOOK
USE PARKIND1, ONLY : JPIM, JPRB
!
IMPLICIT NONE
!
!   Arguments
!
CHARACTER(LEN=*), INTENT(IN)               :: HDTAG   ! Field name
REAL(KIND=JPRB) , INTENT(IN), OPTIONAL, DIMENSION(:)    :: PFIELD  ! Field data block
REAL(KIND=JPRB) , INTENT(IN), OPTIONAL, DIMENSION(:,:)  :: PFIELD2 ! (or) 2d field data block
REAL(KIND=JPRB) , INTENT(IN), OPTIONAL, DIMENSION(:,:,:):: PFIELD3 ! (or) 3d field data block
CHARACTER(LEN=*), INTENT(IN), OPTIONAL     :: HDOMAIN ! Field domain name, defaults to 'FULL' 
CHARACTER(LEN=*), INTENT(IN), OPTIONAL     :: HAXIS   ! Axis name, for 2d fields
CHARACTER(LEN=*), INTENT(IN), OPTIONAL     :: HAXIS2  ! 2nd axis name, for 3d fields
CHARACTER(LEN=*), INTENT(IN), OPTIONAL     :: HDCOMMENT ! Comment 'a la Surfex' (i.e. '<long name> (<units>)')
INTEGER         , INTENT(IN), OPTIONAL     :: KFREQOP ! Sampling frequency, in minutes
!
!
!  Local variables
!
!
#ifdef WXIOS
!
! A basic type for handling fields and their buffer
!
TYPE BUF_t
   CHARACTER(LEN=100) :: YLNAME   ! Field name , as for XIOS
   INTEGER(KIND=JPIM) :: ISIZEMAX ! Expected size of the complete field for the whole of the MPI task
   INTEGER(KIND=JPIM) :: ISIZE    ! Current usable size (ie. over received blocks)
   INTEGER(KIND=JPIM) :: INDIM    ! Number of dimensions 
   INTEGER(KIND=JPIM) :: ILEV     ! Size of 2nd dim  (from first call)
   INTEGER(KIND=JPIM) :: ILEV2    ! Size of 3rd dim (from first call)
   LOGICAL            :: G3DLEV   ! .TRUE. if 3rd dim is a vertical dimension
   INTEGER(KIND=JPIM) :: IBLOCK   ! Number of blocks received for current timestep
   INTEGER(KIND=JPIM) :: IFREQ    ! Field's sampling frequency (in timesteps)
   INTEGER(KIND=JPIM) :: IOFFSET  ! Field's sampling offset    (in timesteps)
   REAL(KIND=JPRB), ALLOCATABLE, DIMENSION (:,:,:) :: ZFIELD ! Accumulate received blocks
END TYPE BUF_t
!
TYPE(xios_field)      :: field_hdl, other_field_hdl
TYPE(xios_fieldgroup) :: fieldgroup_hdl
!
INTEGER(KIND=JPIM)             :: ISIZE   =1000      ! Initial number of managed field entries 
INTEGER(KIND=JPIM), PARAMETER  :: INCR    =100       ! Increment in field entries number when reallocating
INTEGER(KIND=JPIM), PARAMETER  :: IMAXSIZE=10000     ! Max number of field entries
!
TYPE(BUF_t), POINTER                   :: YLF        ! Current buffer entry
TYPE(BUF_t), ALLOCATABLE, TARGET, SAVE :: YLFIELDS(:)! Array of buffer entries
TYPE(BUF_t), ALLOCATABLE               :: YLTEMP(:)  ! id - temporary
!
INTEGER(KIND=JPIM)      :: I, IL, IEMPTY, IDIM, ITAKE, ILEV, INFIELDS
INTEGER(KIND=JPIM)      :: INSIZE  ! New size of current field, after receiving new block
CHARACTER(LEN=100)      :: YLTAG   ! Field name
CHARACTER(LEN=300)      :: YLAXIS,YLAXIS2  ! 
CHARACTER(LEN=300)      :: YLDOMAIN
CHARACTER(LEN=300)      :: YLGRID
CHARACTER(LEN=300)      :: YLCOMMENT
INTEGER                 :: IFREQOP 
LOGICAL                 :: GLISDEF, GWORK
LOGICAL, SAVE           :: GALLOCATED=.FALSE.
TYPE(XIOS_DURATION)     :: YLFREQ_OP, YLFREQ_OFFSET, YLDUR
INTEGER(KIND=JPIM)      :: ITSS, IFREQOPS, IOFFSETS ! timestep, freq_op and offest values in seconds
TYPE(XIOS_DATE)         :: YLDATE, YLDAT2
CHARACTER(1000)         :: YDUMMY
LOGICAL                 :: G_AT_CURR_TSTEP
CHARACTER(100)          :: YLAXISNAME
INTEGER                 :: I_AXIS
!
#endif
!
REAL(KIND=JPRB)          :: ZHOOK_HANDLE
REAL(KIND=JPRB)          :: ZHOOK_HANDLE2
!
!#include "abor1.intfb.h"
!
IF (LHOOK) CALL DR_HOOK('SFX_XIOS_SEND_BLOCK',0,ZHOOK_HANDLE)
!
IF (.NOT. LXIOS) THEN
   IF (LHOOK) CALL DR_HOOK('SFX_XIOS_SEND_BLOCK',1,ZHOOK_HANDLE)
   RETURN
ENDIF
!
#ifdef WXIOS
!
YLTAG=TRIM(HDTAG)
!
G_AT_CURR_TSTEP=.TRUE.
!
IF (LXIOS_DEF_CLOSED) THEN 
   IF (.NOT.XIOS_FIELD_IS_ACTIVE(YLTAG,G_AT_CURR_TSTEP)) THEN 
      IF (LHOOK) CALL DR_HOOK('SFX_XIOS_SEND_BLOCK',1,ZHOOK_HANDLE)
      RETURN
   ENDIF
ENDIF
!
!$OMP SINGLE
!
IF ( .NOT. GALLOCATED) THEN 
   ALLOCATE(YLFIELDS(ISIZE))
   YLFIELDS(:)%YLNAME=''
   YLFIELDS(:)%ILEV=0
   GALLOCATED=.TRUE.
ENDIF
!
!
!   Search if field is known - a simple loop on the table -
!   probably not much quick ...
!
IL=0
DO I=1,ISIZE
   IF (YLFIELDS(I)%YLNAME == YLTAG) THEN 
      IL=I
      EXIT
   ENDIF
ENDDO
!
IF(IL/=0)THEN
  GWORK=(YLFIELDS(IL)%ILEV==0)
ELSE
  GWORK=.FALSE.
ENDIF
!
! If no block was ever received
IF (IL==0.OR.GWORK) THEN
   !
   ! Field is not yet fully recorded -> ask XIOS if field is known,  and what's its size
   ! By exception : if XIOS still in init phase, declare field domain if needed
   !
   IF (.NOT. LXIOS_DEF_CLOSED) THEN
      YLDOMAIN='' ; IF (PRESENT(HDOMAIN))   YLDOMAIN =TRIM(HDOMAIN)
      YLCOMMENT=''; IF (PRESENT(HDCOMMENT)) YLCOMMENT=TRIM(HDCOMMENT)
      IFREQOP=0   ; IF (PRESENT(KFREQOP))   IFREQOP  =KFREQOP

      IF (PRESENT(PFIELD)) THEN 
         CALL SFX_XIOS_DECLARE_FIELD(YLTAG, YLDOMAIN, HCOMMENT=YLCOMMENT, &
              KFREQOP=IFREQOP)
      ELSEIF (PRESENT(PFIELD2)) THEN
         YLAXIS='' ; IF (PRESENT(HAXIS)) YLAXIS=TRIM(HAXIS)
         CALL SFX_XIOS_DECLARE_FIELD(YLTAG, YLDOMAIN, HAXIS=YLAXIS, &
              KLEV=SIZE(PFIELD2,2), HCOMMENT=YLCOMMENT,KFREQOP=IFREQOP)
      ELSEIF (PRESENT(PFIELD3)) THEN 
         YLAXIS ='' ; IF (PRESENT(HAXIS))  YLAXIS =TRIM(HAXIS)
         YLAXIS2='' ; IF (PRESENT(HAXIS2)) YLAXIS2=TRIM(HAXIS2)
         CALL SFX_XIOS_DECLARE_FIELD(YLTAG, YLDOMAIN, &
              HAXIS =YLAXIS , KLEV =SIZE(PFIELD3,2), &
              HAXIS2=YLAXIS2, KLEV2=SIZE(PFIELD3,3), HCOMMENT=YLCOMMENT,KFREQOP=IFREQOP)
      ELSE
         CALL ABOR1_SFX("SFX_XIOS_SEND_BLOCK: NO PFIELDx FOR "//TRIM(YLTAG))
      ENDIF
   ELSE
      IF (.NOT. XIOS_IS_VALID_FIELD(YLTAG)) THEN
         CALL ABOR1_SFX("SFX_XIOS_SEND_BLOCK: FIELD "//TRIM(YLTAG)//&
              &" WASN'T DECLARED TO XIOS (NEITHER IN XML CONFIG FILE, NOR SOON ENOUGH FROM CODE)")
      ENDIF
      IF ( IL==0 ) THEN 
         !
         ! Find an empty location (index IEMPTY) to record the new field
         !
         IEMPTY=0
         DO I=1,ISIZE
            IF (TRIM(YLFIELDS(I)%YLNAME) == '') THEN 
               IEMPTY=I
               EXIT
            ENDIF
         ENDDO
         IF ( IEMPTY == 0 ) THEN
            ! The fields table is full. Allocate a new one and copy the content
            IF (ISIZE > IMAXSIZE) THEN 
               CALL ABOR1_SFX("SFX_XIOS_SEND_BLOCK: MAX BUFFER ENTRIES NUMBER WAS REACHED")
            ENDIF
            ALLOCATE(YLTEMP(ISIZE))
            YLTEMP=YLFIELDS
            DEALLOCATE(YLFIELDS)
            ALLOCATE(YLFIELDS(ISIZE+INCR))
            YLFIELDS(1:ISIZE)=YLTEMP(1:ISIZE)
            DEALLOCATE(YLTEMP)
            YLFIELDS(ISIZE+1:ISIZE+INCR)%YLNAME=''
            YLFIELDS(ISIZE+1:ISIZE+INCR)%ILEV=0
            IEMPTY=ISIZE+1
            ISIZE=ISIZE+INCR
         ENDIF
      ELSE
         IEMPTY=IL
      ENDIF
      !
      ! Record the new field attributes (not its data)
      !
      YLF => YLFIELDS(IEMPTY)
      YLF%YLNAME   =TRIM(YLTAG)
      YLF%G3DLEV = .FALSE.   
      CALL XIOS_IS_DEFINED_FIELD_ATTR(TRIM(YLTAG), grid_ref=GLISDEF)
      IF (GLISDEF)  THEN 
         CALL XIOS_GET_FIELD_ATTR(YLTAG, grid_ref=YLGRID)
         IF (YLGRID(1:4)=='FULL') THEN 
            YLDOMAIN='FULL'
         ELSEIF (YLGRID(1:3)=='SEA') THEN 
            YLDOMAIN='SEA'
         ELSEIF (YLGRID(1:5)=='WATER') THEN 
            YLDOMAIN='WATER'
         ELSEIF (YLGRID(1:6)=='NATURE') THEN 
            YLDOMAIN='NATURE'
         ELSEIF (YLGRID(1:4)=='TOWN') THEN 
            YLDOMAIN='TOWN'
         ELSE
            CALL ABOR1_SFX('SFX_XIOS_SEND_BLOCK : GRID '//TRIM(YLGRID)//' FOR FIELD '&
                 &//TRIM(YLTAG)//' DOESN T HAVE A VALID PREFIX')
         ENDIF
         ! Determine if the dimension of a 3D field is klev or klev_half
         ! via the suffix name of the grid_ref
         I_AXIS = INDEX(YLGRID, "_")
         YLAXISNAME = YLGRID(I_AXIS+1:)
         IF (YLAXISNAME(1:4)=='klev') THEN
           YLF%G3DLEV = .TRUE.
         ELSEIF (YLAXISNAME(1:9)=='klev_half') THEN
           YLF%G3DLEV = .TRUE.
         ENDIF        
      ELSE
         CALL XIOS_IS_DEFINED_FIELD_ATTR(YLTAG, domain_ref=GLISDEF)
         IF (GLISDEF)  THEN 
            CALL XIOS_GET_FIELD_ATTR(YLTAG, domain_ref=YLDOMAIN)
         ELSE
            CALL ABOR1_SFX('SFX_XIOS_SEND_BLOCK : FIELD '//TRIM(YLTAG)//' HAS NO DOMAIN')
         ENDIF
      ENDIF
      CALL XIOS_GET_DOMAIN_ATTR(YLDOMAIN, data_ni=IDIM)
      YLF%ISIZEMAX =IDIM
      INFIELDS=0
      IF (PRESENT(PFIELD)) THEN
         YLF%ILEV=1  ; YLF%ILEV2=1  ; YLF%INDIM=1 ; INFIELDS=INFIELDS+1
      ENDIF
      IF (PRESENT(PFIELD2)) THEN
         YLF%ILEV=SIZE(PFIELD2,2)  ; YLF%ILEV2=1  ; YLF%INDIM=2 ; INFIELDS=INFIELDS+1
      ENDIF
      IF (PRESENT(PFIELD3)) THEN
         YLF%ILEV=SIZE(PFIELD3,2)  ; YLF%ILEV2=SIZE(PFIELD3,3)  ; YLF%INDIM=3 ; INFIELDS=INFIELDS+1
      ENDIF
      IF (INFIELDS == 1 ) THEN
         ALLOCATE(YLF%ZFIELD(YLF%ISIZEMAX,YLF%ILEV,YLF%ILEV2))
         YLF%ISIZE    =0
         YLF%IBLOCK   =0
      ELSE
         ! SS : rétablissement de l'abort
         CALL ABOR1_SFX('SFX_XIOS_SEND_BLOCK : TOO FEW OR MANY PFIELDx ARGS FOR '//YLTAG)
      ENDIF
      !
      ! Convert timestep to seconds
      CALL XIOS_GET_START_DATE(YLDATE)
      ITSS=XIOS_DATE_CONVERT_TO_SECONDS(YLDATE+XIOS_TIMESTEP) - XIOS_DATE_CONVERT_TO_SECONDS(YLDATE)
      !
      ! Compute IFREQ :the number of timesteps corresponding to field's freq_op
      !
      YLF%IFREQ=1
      CALL XIOS_IS_DEFINED_FIELD_ATTR(YLTAG, freq_op=GLISDEF)
      IF (GLISDEF) THEN 
         CALL XIOS_GET_FIELD_ATTR(YLTAG,freq_op=YLFREQ_OP)
         ! Convert freq_op to seconds
         IFREQOPS=XIOS_DATE_CONVERT_TO_SECONDS(YLDATE+YLFREQ_OP) - XIOS_DATE_CONVERT_TO_SECONDS(YLDATE)
         ! And then to timesteps
         YLF%IFREQ=INT(IFREQOPS/ITSS)
      ENDIF
      ! 
      ! Compute IOFFSET :the sampling offset (in number of timesteps)
      !
      YLF%IOFFSET=0 
      CALL XIOS_IS_DEFINED_FIELD_ATTR(YLTAG, freq_offset=GLISDEF)
      IF (GLISDEF) THEN 
         CALL XIOS_GET_FIELD_ATTR(YLTAG,freq_offset=YLFREQ_OFFSET)
         ! Convert offset to seconds
         IOFFSETS=XIOS_DATE_CONVERT_TO_SECONDS(YLDATE+YLFREQ_OFFSET) - XIOS_DATE_CONVERT_TO_SECONDS(YLDATE)
         ! And then to timesteps
         YLF%IOFFSET=INT(IOFFSETS/ITSS)
      ENDIF
   ENDIF
!
ELSE
!
   YLF => YLFIELDS(IL)
!
ENDIF
!
IF (LXIOS_DEF_CLOSED) THEN
!
!  NO XIOS_SEND_FIELD AT NTIMESTEP=0 (SEND NOT TAKEN INTO ACCOUNT BY XIOS)
!  => BUGFIX FIELD WITH OPERATION='ONCE' BECAUSE FIELD IS NOT SEND LATER)
!
   IF (NTIMESTEP /= 0 .AND. MOD(NTIMESTEP,YLF%IFREQ)==YLF%IOFFSET)   THEN 
         !
         ! WRITE(*,*) 'SNDBLCK : freqok = ', YLTAG
         !
         ! Add the block data to the field buffer and send the field if it is
         ! complete
         !
         YLF%IBLOCK=YLF%IBLOCK+1
         !
         IF (YLF%INDIM ==1 ) THEN
            !
            ITAKE=SIZE(PFIELD)
            INSIZE=YLF%ISIZE + ITAKE
            IF (INSIZE > YLF%ISIZEMAX) THEN 
               IF (YLF%IBLOCK .NE. NBLOCK) THEN 
                  CALL ABOR1_SFX("SFX_XIOS_SEND_BLOCK: FIELD "//TRIM(YLTAG)//&
                       " OVERFLOWS - CHECK ITS DECLARATION TO XIOS (MAYBE TWO INCONSISTENT DECLARATIONS ?)")
               ENDIF
               INSIZE=YLF%ISIZEMAX
               ITAKE=INSIZE-YLF%ISIZE
            ENDIF
            YLF%ZFIELD(YLF%ISIZE+1:INSIZE,1,1)=PFIELD(1:ITAKE)
            ! update fields size
            YLF%ISIZE=INSIZE
            IF (YLF%IBLOCK==NBLOCK) THEN
               IF (PRESENT(KFREQOP)) THEN 
                  IF (LHOOK) CALL DR_HOOK('XIOS_SEND_FIELD_2D_A_'//TRIM(HDOMAIN),0,ZHOOK_HANDLE2)
               ELSE
                  IF (LHOOK) CALL DR_HOOK('XIOS_SEND_FIELD_2D_'//TRIM(HDOMAIN),0,ZHOOK_HANDLE2)
               ENDIF
               CALL XIOS_SEND_FIELD(YLTAG,YLF%ZFIELD(:,1,1))
               IF (PRESENT(KFREQOP)) THEN 
                  IF (LHOOK) CALL DR_HOOK('XIOS_SEND_FIELD_2D_A_'//TRIM(HDOMAIN),1,ZHOOK_HANDLE2)
               ELSE
                  IF (LHOOK) CALL DR_HOOK('XIOS_SEND_FIELD_2D_'//TRIM(HDOMAIN),1,ZHOOK_HANDLE2)
               ENDIF
               YLF%IBLOCK=0
               YLF%ISIZE=0
               !DEALLOCATE(YLF%ZFIELD)  -> No : rather keep it for next timestep
            ENDIF
            !
         ELSEIF (YLF%INDIM ==2 ) THEN
            !
            ITAKE=SIZE(PFIELD2,1)
            INSIZE=YLF%ISIZE + ITAKE
            IF (INSIZE > YLF%ISIZEMAX) THEN 
               IF (YLF%IBLOCK .NE. NBLOCK) THEN 
                  CALL ABOR1_SFX("SFX_XIOS_SEND_BLOCK: FIELD "//TRIM(YLTAG)//&
                       " OVERFLOWS - CHECK ITS DECLARATION TO XIOS (MAYBE TWO INCONSISTENT DECLARATIONS ?)")
               ENDIF
               INSIZE=YLF%ISIZEMAX
               ITAKE=INSIZE-YLF%ISIZE
            ENDIF
            YLF%ZFIELD(YLF%ISIZE+1:INSIZE,:,1)=PFIELD2(1:ITAKE,:)
            ! update fields size
            YLF%ISIZE=INSIZE
            IF (YLF%IBLOCK==NBLOCK) THEN
               IF (LHOOK) CALL DR_HOOK('XIOS_SEND_FIELD_3D_'//TRIM(HDOMAIN),0,ZHOOK_HANDLE2)
               IF (LXIOS_INVERT_LEVELS .AND. YLF%G3DLEV) THEN
                 CALL XIOS_SEND_FIELD(YLTAG,YLF%ZFIELD(:,YLF%ILEV:1:-1,1))
               ELSE
                 CALL XIOS_SEND_FIELD(YLTAG,YLF%ZFIELD(:,:,1))
               ENDIF
               IF (LHOOK) CALL DR_HOOK('XIOS_SEND_FIELD_3D_'//TRIM(HDOMAIN),1,ZHOOK_HANDLE2)
               YLF%IBLOCK=0
               YLF%ISIZE=0
            ENDIF
            !
         ELSEIF (YLF%INDIM ==3 ) THEN
            !
            ITAKE=SIZE(PFIELD3,1)
            INSIZE=YLF%ISIZE + ITAKE
            IF (INSIZE > YLF%ISIZEMAX) THEN 
               IF (YLF%IBLOCK .NE. NBLOCK) THEN 
                  CALL ABOR1_SFX("SFX_XIOS_SEND_BLOCK: FIELD "//TRIM(YLTAG)//&
                       " OVERFLOWS - CHECK ITS DECLARATION TO XIOS (MAYBE TWO INCONSISTENT DECLARATIONS ?)")
               ENDIF
               INSIZE=YLF%ISIZEMAX
               ITAKE=INSIZE-YLF%ISIZE
            ENDIF
            YLF%ZFIELD(YLF%ISIZE+1:INSIZE,:,:)=PFIELD3(1:ITAKE,:,:)
            ! update fields size
            YLF%ISIZE=INSIZE
            IF (YLF%IBLOCK==NBLOCK) THEN
               IF (LHOOK) CALL DR_HOOK('XIOS_SEND_FIELD_4D_'//TRIM(HDOMAIN),0,ZHOOK_HANDLE2)
               IF (LXIOS_INVERT_LEVELS .AND. YLF%G3DLEV) THEN
                 CALL XIOS_SEND_FIELD(YLTAG,YLF%ZFIELD(:,YLF%ILEV:1:-1,:))
               ELSE
                 CALL XIOS_SEND_FIELD(YLTAG,YLF%ZFIELD(:,:,:))
               ENDIF
               IF (LHOOK) CALL DR_HOOK('XIOS_SEND_FIELD_4D_'//TRIM(HDOMAIN),1,ZHOOK_HANDLE2)
               YLF%IBLOCK=0
               YLF%ISIZE=0
            ENDIF
            !
      ENDIF
   ENDIF
ENDIF
!
!$OMP END SINGLE
#endif
IF (LHOOK) CALL DR_HOOK('SFX_XIOS_SEND_BLOCK',1,ZHOOK_HANDLE)
END SUBROUTINE SFX_XIOS_SEND_BLOCK
