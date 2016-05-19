!-----------------------------------------------------------------
!--------------- special set of characters for RCS information
!-----------------------------------------------------------------
! $Source$ $Revision$
! MASDEV4_7 les 2006/08/30 18:41:37
!-----------------------------------------------------------------
!     ######################
      SUBROUTINE LES_SPEC_n
!     ######################
!
!
!!****  *LES_SPEC_n* computes and writes the LES spectra for model $n
!!
!!
!!    PURPOSE
!!    -------
!!
!!
!!    METHOD
!!    ------
!!
!!    Simple Fourier transforms are used, because there is no condition
!!    on the number of points in each direction (they can be different from
!!    factors of 2,3 and 5, because of possible subdomains).
!!
!!    However, the computations are performed only once, at the end of the run.
!!
!!    EXTERNAL
!!    --------
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!
!!    REFERENCE
!!    ---------
!!
!!    AUTHOR
!!    ------
!!      V. Masson
!!
!!    MODIFICATIONS
!!    -------------
!!      Original         07/02/00
!!                       01/02/01 (D. Gazen) add module MODD_NSV for NSV variable
!!                       01/04/03 (V. Masson) bug in spectra normalization
!!
!! --------------------------------------------------------------------------
!
!*      0. DECLARATIONS
!          ------------
!
USE MODD_LES
USE MODD_LES_n
!
USE MODD_CONF_n
USE MODD_LUNIT_n
USE MODD_GRID_n
USE MODD_LBC_n, ONLY : CLBCX, CLBCY
USE MODD_NSV, ONLY : NSV
!
USE MODD_CONF
!
USE MODE_ll
USE MODE_FM
!
USE MODE_LES_DIACHRO
USE MODI_WRITE_LES_n
!
IMPLICIT NONE
!
!
!*      0.1  declarations of arguments
!
!
!*      0.2  declaration of local variables
!
INTEGER :: IRESP, ILUOUT
!
INTEGER :: JSV       ! scalar loop counter
!
CHARACTER(len=5)                      :: YGROUP
!
REAL, DIMENSION(:,:,:,:), ALLOCATABLE :: ZSPECTRAX ! spectra coeffcients for
REAL, DIMENSION(:,:,:,:), ALLOCATABLE :: ZSPECTRAY ! x and y direction spectra
!
INTEGER :: ISPECTRA_NI
INTEGER :: ISPECTRA_NJ
!-------------------------------------------------------------------------------
!
IF (CLBCX(1)=='CYCL') THEN
  ISPECTRA_NI = (NSPECTRA_NI+1)/2 - 1
ELSE
  ISPECTRA_NI =  NSPECTRA_NI - 1
END IF
!
IF (CLBCY(1)=='CYCL') THEN
  ISPECTRA_NJ = (NSPECTRA_NJ+1)/2 - 1
ELSE
  ISPECTRA_NJ =  NSPECTRA_NJ - 1
END IF
!
CALL FMLOOK_ll(CLUOUT,CLUOUT,ILUOUT,IRESP)
!
ALLOCATE( ZSPECTRAX(ISPECTRA_NI,2,NSPECTRA_K,NLES_TIMES) )
ALLOCATE( ZSPECTRAY(ISPECTRA_NJ,2,NSPECTRA_K,NLES_TIMES) )
!
!
!*      1.   (ni,z,t) and (nj,z,t) spectra
!            -----------------------------
!
IF (NSPECTRA_K>0) THEN
  CALL LES_SPEC('X',XCORRi_UU,    ZSPECTRAX)
  CALL LES_SPEC('Y',XCORRj_UU,    ZSPECTRAY)
  CALL LES_DIACHRO_SPEC("UU   ","U*U     spectra","m3/s2",ZSPECTRAX,ZSPECTRAY)
!
  CALL LES_SPEC('X',XCORRi_VV,    ZSPECTRAX)
  CALL LES_SPEC('Y',XCORRj_VV,    ZSPECTRAY)
  CALL LES_DIACHRO_SPEC("VV   ","V*V     spectra","m3/s2",ZSPECTRAX,ZSPECTRAY)
!
  CALL LES_SPEC('X',XCORRi_WW,    ZSPECTRAX)
  CALL LES_SPEC('Y',XCORRj_WW,    ZSPECTRAY)
  CALL LES_DIACHRO_SPEC("WW   ","W*W     spectra","m3/s2",ZSPECTRAX,ZSPECTRAY)
!
  CALL LES_SPEC('X',XCORRi_UV,    ZSPECTRAX)
  CALL LES_SPEC('Y',XCORRj_UV,    ZSPECTRAY)
  CALL LES_DIACHRO_SPEC("UV   ","U*V     spectra","m3/s2",ZSPECTRAX,ZSPECTRAY)
!
  CALL LES_SPEC('X',XCORRi_WU,    ZSPECTRAX)
  CALL LES_SPEC('Y',XCORRj_WU,    ZSPECTRAY)
  CALL LES_DIACHRO_SPEC("WU   ","W*U     spectra","m3/s2",ZSPECTRAX,ZSPECTRAY)
!
  CALL LES_SPEC('X',XCORRi_WV,    ZSPECTRAX)
  CALL LES_SPEC('Y',XCORRj_WV,    ZSPECTRAY)
  CALL LES_DIACHRO_SPEC("WV   ","W*V     spectra","m3/s2",ZSPECTRAX,ZSPECTRAY)
!
  CALL LES_SPEC('X',XCORRi_ThTh,  ZSPECTRAX)
  CALL LES_SPEC('Y',XCORRj_ThTh,  ZSPECTRAY)
  CALL LES_DIACHRO_SPEC("THTH ","Th*Th   spectra","mK2  ",ZSPECTRAX,ZSPECTRAY)
!
  IF (LUSERC) THEN
    CALL LES_SPEC('X',XCORRi_ThlThl,  ZSPECTRAX)
    CALL LES_SPEC('Y',XCORRj_ThlThl,  ZSPECTRAY)
    CALL LES_DIACHRO_SPEC("TLTL ","Thl*Thl spectra","mK2  ",ZSPECTRAX,ZSPECTRAY)
  END IF
!
  CALL LES_SPEC('X',XCORRi_WTh,  ZSPECTRAX)
  CALL LES_SPEC('Y',XCORRj_WTh,  ZSPECTRAY)
  CALL LES_DIACHRO_SPEC("WTH  ","W*Th    spectra","m2K/s",ZSPECTRAX,ZSPECTRAY)
!
  IF (LUSERC) THEN
    CALL LES_SPEC('X',XCORRi_WThl,  ZSPECTRAX)
    CALL LES_SPEC('Y',XCORRj_WThl,  ZSPECTRAY)
    CALL LES_DIACHRO_SPEC("WTHL ","W*Thl   spectra","m2K/s",ZSPECTRAX,ZSPECTRAY)
  END IF
  !
  IF (LUSERV) THEN
    CALL LES_SPEC('X',XCORRi_RvRv,  ZSPECTRAX)
    CALL LES_SPEC('Y',XCORRj_RvRv,  ZSPECTRAY)
    CALL LES_DIACHRO_SPEC("RVRV ","rv*rv   spectra","m    ",ZSPECTRAX,ZSPECTRAY)
    !
    CALL LES_SPEC('X',XCORRi_ThRv,  ZSPECTRAX)
    CALL LES_SPEC('Y',XCORRj_ThRv,  ZSPECTRAY)
    CALL LES_DIACHRO_SPEC("THRV ","th*rv   spectra","Km   ",ZSPECTRAX,ZSPECTRAY)
    !
    IF (LUSERC) THEN
      CALL LES_SPEC('X',XCORRi_ThlRv,  ZSPECTRAX)
      CALL LES_SPEC('Y',XCORRj_ThlRv,  ZSPECTRAY)
      CALL LES_DIACHRO_SPEC("TLRV ","thl*rv  spectra","Km   ",ZSPECTRAX,ZSPECTRAY)
    END IF
    !
    CALL LES_SPEC('X',XCORRi_WRv,  ZSPECTRAX)
    CALL LES_SPEC('Y',XCORRj_WRv,  ZSPECTRAY)
    CALL LES_DIACHRO_SPEC("WRV ","W*rv     spectra","mK/s ",ZSPECTRAX,ZSPECTRAY)
  END IF
  IF (LUSERC) THEN
    CALL LES_SPEC('X',XCORRi_RcRc,  ZSPECTRAX)
    CALL LES_SPEC('Y',XCORRj_RcRc,  ZSPECTRAY)
    CALL LES_DIACHRO_SPEC("RCRC ","rc*rc   spectra","m    ",ZSPECTRAX,ZSPECTRAY)
    !
    CALL LES_SPEC('X',XCORRi_ThRc,  ZSPECTRAX)
    CALL LES_SPEC('Y',XCORRj_ThRc,  ZSPECTRAY)
    CALL LES_DIACHRO_SPEC("THRC ","th*rc   spectra","Km   ",ZSPECTRAX,ZSPECTRAY)
    !
    CALL LES_SPEC('X',XCORRi_ThlRc,  ZSPECTRAX)
    CALL LES_SPEC('Y',XCORRj_ThlRc,  ZSPECTRAY)
    CALL LES_DIACHRO_SPEC("TLRC ","thl*rc  spectra","Km   ",ZSPECTRAX,ZSPECTRAY)
    !
    CALL LES_SPEC('X',XCORRi_WRc,  ZSPECTRAX)
    CALL LES_SPEC('Y',XCORRj_WRc,  ZSPECTRAY)
    CALL LES_DIACHRO_SPEC("WRC ","W*rc     spectra","mK/s ",ZSPECTRAX,ZSPECTRAY)
  END IF
  IF (LUSERI) THEN
    CALL LES_SPEC('X',XCORRi_RiRi,  ZSPECTRAX)
    CALL LES_SPEC('Y',XCORRj_RiRi,  ZSPECTRAY)
    CALL LES_DIACHRO_SPEC("RIRI ","ri*ri   spectra","m    ",ZSPECTRAX,ZSPECTRAY)
    !
    CALL LES_SPEC('X',XCORRi_ThRi,  ZSPECTRAX)
    CALL LES_SPEC('Y',XCORRj_ThRi,  ZSPECTRAY)
    CALL LES_DIACHRO_SPEC("THRI ","th*ri   spectra","Km   ",ZSPECTRAX,ZSPECTRAY)
    !
    CALL LES_SPEC('X',XCORRi_ThlRi,  ZSPECTRAX)
    CALL LES_SPEC('Y',XCORRj_ThlRi,  ZSPECTRAY)
    CALL LES_DIACHRO_SPEC("TLRI ","thl*ri  spectra","Km   ",ZSPECTRAX,ZSPECTRAY)
    !
    CALL LES_SPEC('X',XCORRi_WRi,  ZSPECTRAX)
    CALL LES_SPEC('Y',XCORRj_WRi,  ZSPECTRAY)
    CALL LES_DIACHRO_SPEC("WRI ","W*ri     spectra","mK/s ",ZSPECTRAX,ZSPECTRAY)
  END IF
  DO JSV=1,NSV
    WRITE (YGROUP,FMT="(A2,I3.3)") "SS",JSV
    CALL LES_SPEC('X',XCORRi_SvSv(:,:,:,JSV),  ZSPECTRAX)
    CALL LES_SPEC('Y',XCORRj_SvSv(:,:,:,JSV),  ZSPECTRAY)
    CALL LES_DIACHRO_SPEC(YGROUP,"Sv*Sv    spectra","m    ",ZSPECTRAX,ZSPECTRAY)
  END DO
  DO JSV=1,NSV
    WRITE (YGROUP,FMT="(A2,I3.3)") "WS",JSV
    CALL LES_SPEC('X',XCORRi_WSv(:,:,:,JSV),  ZSPECTRAX)
    CALL LES_SPEC('Y',XCORRj_WSv(:,:,:,JSV),  ZSPECTRAY)
    CALL LES_DIACHRO_SPEC(YGROUP,"W*Sv    spectra","m2/s  ",ZSPECTRAX,ZSPECTRAY)
  END DO
END IF
!
DEALLOCATE( ZSPECTRAX )
DEALLOCATE( ZSPECTRAY )
!
!------------------------------------------------------------------------------
!
CONTAINS
!
!------------------------------------------------------------------------------
!
SUBROUTINE LES_SPEC(HDIR,ZCORR,ZSPECTRA)
!
USE MODD_CST
!
CHARACTER(len=1),         INTENT(IN)  :: HDIR        ! direction of spectra
REAL, DIMENSION(:,:,:),   INTENT(IN)  :: ZCORR       ! 2 pts correlation data
REAL, DIMENSION(:,:,:,:), INTENT(OUT) :: ZSPECTRA    ! spectra
!
INTEGER                               :: INK      ! number of wavelength
INTEGER                               :: JK       ! loop counter
REAL                                  :: ZK       ! wavelength
INTEGER                               :: JR       ! loop counter
INTEGER                               :: ITIME    ! number of averaging points
!
REAL                                  :: ZDX      ! grid mesh
!------------------------------------------------------------------------------
!
!
IF ((L2D) .AND. HDIR=='Y') RETURN
!
INK = SIZE(ZCORR,1)
!
ZSPECTRA(:,:,:,:)=0.
!
!
!* loop on wavelengths
!
DO JK=1,SIZE(ZSPECTRA,1)
!
!* actual wavelength
!
  ZK = (2.*XPI) / JK
!
!* Fourier summation
!
! warning : index JR=1 corresponds to autocorrelations.
! warning : integration on wavelengths is done only for the half of the
!           domain, in order (i) to avoid extra counting of the spectra
!           in CYCL case, or (ii) to take into account only wavelentgh
!           with a significant number of occurences in other boundary
!           conditions.
!
  DO JR=1,INK/2
    ZSPECTRA(JK,1,:,:) = ZSPECTRA(JK,1,:,:) + ZCORR(JR,:,:)*COS(-ZK*(JR-1))
    ZSPECTRA(JK,2,:,:) = ZSPECTRA(JK,2,:,:) + ZCORR(JR,:,:)*SIN(-ZK*(JR-1))
  END DO
END DO
!
IF (HDIR=='X') THEN
  ZDX=(XXHAT(2)-XXHAT(1))
ELSE IF (HDIR=='Y') THEN
  ZDX=(XYHAT(2)-XYHAT(1))
END IF
!
ZSPECTRA(:,:,:,:) = ZSPECTRA(:,:,:,:) / (2.*XPI*ZDX*(INK/2))
!
!------------------------------------------------------------------------------
!
END SUBROUTINE LES_SPEC
!
!------------------------------------------------------------------------------
!
END SUBROUTINE LES_SPEC_n
