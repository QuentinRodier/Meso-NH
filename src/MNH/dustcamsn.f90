
!--------------- special set of characters for RCS information
!-----------------------------------------------------------------
! $Source: /home/cvsroot/MNH-VX-Y-Z/src/MNH/dustlfin.f90,v $ $Revision: 1.1.2.2.2.1.2.1 $
! MASDEV4_7 newsrc 2007/01/25 13:13:15
!-----------------------------------------------------------------
!     ########################
      MODULE MODI_DUSTCAMS_n
!     ########################
!
INTERFACE
!
SUBROUTINE DUSTCAMS_n(PSV, PMASSCAMS, PRHODREF)
IMPLICIT NONE
REAL,       DIMENSION(:,:,:,:),INTENT(INOUT) :: PSV
REAL,       DIMENSION(:,:,:,:), INTENT(IN) :: PMASSCAMS
REAL,       DIMENSION(:,:,:), INTENT(IN) :: PRHODREF
END SUBROUTINE DUSTCAMS_n
!
END INTERFACE
!
END MODULE MODI_DUSTCAMS_n
!
!
!     ############################################################
      SUBROUTINE DUSTCAMS_n(PSV, PMASSCAMS,PRHODREF)
!     ############################################################
!
!!    PURPOSE
!!    -------
!!    Initialise le champs  de dusts à partir des analyses CAMS
!!
!!    REFERENCE
!!    ---------
!!    none
!!
!!    AUTHOR
!!    ------
!!    Pierre TULET (LACy)
!!
!!    MODIFICATIONS
!!    -------------
!!    none
!!
!!    EXTERNAL
!!    --------
!!    None
!!
!-------------------------------------------------------------------------------
!
!*       0.     DECLARATIONS
!               ------------
!
USE MODD_DUST
USE MODD_NSV
USE MODD_CSTS_DUST
USE MODE_DUST_PSD
! 
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
REAL,   DIMENSION(:,:,:,:), INTENT(INOUT) :: PSV
REAL,   DIMENSION(:,:,:,:), INTENT(IN) :: PMASSCAMS
REAL,   DIMENSION(:,:,:), INTENT(IN) :: PRHODREF
!
!
!*      0.2    declarations local variables
!
REAL   :: ZDEN2MOL, ZRHOI, ZMI, ZFAC, ZRGMIN
REAL,DIMENSION(:,:,:,:), ALLOCATABLE  :: ZCTOTA
REAL,DIMENSION(:,:,:,:), ALLOCATABLE  :: ZM
REAL,DIMENSION(:,:,:),   ALLOCATABLE  :: ZSIGMA
REAL,DIMENSION(:,:,:,:), ALLOCATABLE  :: ZMASS
INTEGER,DIMENSION(:),    ALLOCATABLE  :: IM0, IM3, IM6
REAL,DIMENSION(:),       ALLOCATABLE  :: ZMMIN
REAL,DIMENSION(:),       ALLOCATABLE  :: ZINIRADIUS, ZINISIGMA
REAL    :: ZRHOMIN
INTEGER :: IKU, IMOMENTS
INTEGER :: JJ, JN, JK  ! loop counter
INTEGER :: IMODEIDX  ! index mode
!
!-------------------------------------------------------------------------------
!
!*       1.     TRANSFER FROM GAS TO AEROSOL MODULE
!               -----------------------------------
!
!        1.1    initialisation 
!
IKU = SIZE(PSV,3)
ZRHOMIN=MINVAL(PRHODREF)
!
ALLOCATE (IM0(NMODE_DST))
ALLOCATE (IM3(NMODE_DST))
ALLOCATE (IM6(NMODE_DST))
ALLOCATE (ZCTOTA(SIZE(PSV,1), SIZE(PSV,2), SIZE(PSV,3), NMODE_DST))
ALLOCATE (ZM(SIZE(PSV,1), SIZE(PSV,2), SIZE(PSV,3), NMODE_DST*3))
ALLOCATE (ZSIGMA(SIZE(PSV,1), SIZE(PSV,2), SIZE(PSV,3)))
ALLOCATE (ZINIRADIUS(NMODE_DST))
ALLOCATE (ZINISIGMA(NMODE_DST))
ALLOCATE (ZMMIN(NMODE_DST*3))
ALLOCATE (ZMASS(SIZE(PSV,1), SIZE(PSV,2), SIZE(PSV,3),NMODE_DST))
!
!
DO JN = 1, NMODE_DST
  IM0(JN) = 1 + (JN - 1) * 3
  IM3(JN) = 2 + (JN - 1) * 3
  IM6(JN) = 3 + (JN - 1) * 3
  !
  !Get the dust mode we are talking about, MODE 2 is treated first, then mode 3, then 1
  !This index is only needed to get the right radius out of the XINIRADIUS array and the
  !right XINISIG out of the XINISIG-array
  IMODEIDX = JPDUSTORDER(JN)
  !
  !Convert initial mass median radius to number median radius
  IF (CRGUNITD=="MASS") THEN
    ZINIRADIUS(JN) = XINIRADIUS(IMODEIDX) * EXP(-3.*(LOG(XINISIG(IMODEIDX)))**2)
  ELSE
    ZINIRADIUS(JN) = XINIRADIUS(IMODEIDX)
  END IF
  ZINISIGMA(JN)  = XINISIG(IMODEIDX)
  !
  ZMMIN(IM0(JN)) = XN0MIN(IMODEIDX)
  ZRGMIN   = ZINIRADIUS(JN)
  ZMMIN(IM3(JN)) = XN0MIN(IMODEIDX) * (ZRGMIN**3)*EXP(4.5 * LOG(ZINISIGMA(JN))**2) 
  ZMMIN(IM6(JN)) = XN0MIN(IMODEIDX) * (ZRGMIN**6)*EXP(18. * LOG(ZINISIGMA(JN))**2)

  IF (JPDUSTORDER(JN) == 1) ZMASS(:,:,:,JN) = MAX(PMASSCAMS(:,:,:,1), 1E-16) ! fin mode 
  IF (JPDUSTORDER(JN) == 2) ZMASS(:,:,:,JN) = MAX(PMASSCAMS(:,:,:,2), 1E-15) ! median mode 
  IF (JPDUSTORDER(JN) == 3) ZMASS(:,:,:,JN) = MAX(PMASSCAMS(:,:,:,3), 1E-15) ! large mode

ENDDO

!
!
ZRHOI = XDENSITY_DUST !1.8e3 !++changed alfgr
ZMI   = XMOLARWEIGHT_DUST
ZDEN2MOL = 1E-6 * XAVOGADRO / XMD
ZFAC = (4. / 3.) * XPI * ZRHOI * 1.e-9

!
DO JN = 1, NMODE_DST

!*       1.1    calculate moment 0 from ZMASS
!
  ZM(:,:,:,IM0(JN)) = ZMASS(:,:,:,JN)                    &![kg_{dust}/kg_{air}  
                    / XDENSITY_DUST                      &![kg__{dust}/m3_{dust}==>m3_{dust}/m3{air}
                    * (6.d0 / XPI)                       &
                    / (2.d0 * ZINIRADIUS(JN) * 1.d-6)**3 &![particle/m_dust^{-3}]==> particle/m3
                    * EXP(-4.5*(LOG(ZINISIGMA(JN)))**2) !Take into account distribution
!
  ZM(:,:,:,IM0(JN)) = MAX(ZMMIN(IM0(JN)), ZM(:,:,:,IM0(JN)))
!
!*       1.2    calculate moment 3 from m0,  RG and SIG 
!
  ZM(:,:,:,IM3(JN)) = ZM(:,:,:,IM0(JN)) * &
                      (ZINIRADIUS(JN)**3) * &
                      EXP(4.5*LOG(ZINISIGMA(JN))**2) 

!  ZM(:,:,:,IM3(JN)) = MAX(ZMMIN(IM3(JN)), ZM(:,:,:,IM3(JN)))
!
!*       1.3    calculate moment 6 from m0,  RG and SIG 
!
  ZM(:,:,:,IM6(JN))= ZM(:,:,:,IM0(JN)) * ((ZINIRADIUS(JN)**6) * &
                     EXP(18.*(LOG(ZINISIGMA(JN)))**2))
!
!  ZM(:,:,:,IM6(JN)) = MAX(ZMMIN(IM6(JN)), ZM(:,:,:,IM6(JN)))
!
!*       1.4    output concentration
!
  IMOMENTS = INT(NSV_DSTEND - NSV_DSTBEG+1) / NMODE_DST
  IF (IMOMENTS == 3) THEN
   PSV(:,:,:,1+(JN-1)*3) = ZM(:,:,:,IM0(JN)) * XMD / (XAVOGADRO*PRHODREF(:,:,:))
   XSVMIN(NSV_DSTBEG-1+1+(JN-1)*3) = ZMMIN(IM0(JN)) * XMD / (XAVOGADRO*ZRHOMIN)

   PSV(:,:,:,2+(JN-1)*3) = ZM(:,:,:,IM3(JN)) * XMD*XPI * 4./3. * ZRHOI  / &
                           (ZMI*XM3TOUM3*PRHODREF(:,:,:))
    XSVMIN(NSV_DSTBEG-1+2+(JN-1)*3) = ZMMIN(IM3(JN))  * XMD * XPI * 4. / 3. * ZRHOI / &
                            (ZMI*XM3TOUM3**ZRHOMIN)

   PSV(:,:,:,3+(JN-1)*3) = ZM(:,:,:,IM6(JN)) *  XMD / (XAVOGADRO*1.d-6*PRHODREF(:,:,:))
   XSVMIN(NSV_DSTBEG-1+3+(JN-1)*3) = ZMMIN(IM6(JN))  *  XMD / (XAVOGADRO*1.d-6* ZRHOMIN)

  ELSE IF (IMOMENTS == 2) THEN
   PSV(:,:,:,1+(JN-1)*2) = ZM(:,:,:,IM0(JN)) * XMD / (XAVOGADRO*PRHODREF(:,:,:))
    XSVMIN(NSV_DSTBEG-1+1+(JN-1)*2) = ZMMIN(IM0(JN)) * XMD / (XAVOGADRO*ZRHOMIN)

   PSV(:,:,:,2+(JN-1)*2) = ZM(:,:,:,IM3(JN)) * XMD*XPI * 4./3. * ZRHOI  / &
                           (ZMI*XM3TOUM3*PRHODREF(:,:,:))
    XSVMIN(NSV_DSTBEG-1+2+(JN-1)*2) = ZMMIN(IM3(JN))  * XMD * XPI * 4. / 3. * ZRHOI / &
                            (ZMI*XM3TOUM3**ZRHOMIN)
  ELSE 
   PSV(:,:,:,JN) = ZM(:,:,:,IM3(JN)) * XMD*XPI * 4./3. * ZRHOI  / &
                           (ZMI*XM3TOUM3*PRHODREF(:,:,:))
   XSVMIN(NSV_DSTBEG-1+JN) = ZMMIN(IM3(JN))  * XMD * XPI * 4. / 3. * ZRHOI / &
                            (ZMI*XM3TOUM3**ZRHOMIN)

  END IF
END DO

!
DEALLOCATE(ZMMIN)
DEALLOCATE(ZINISIGMA)
DEALLOCATE(ZINIRADIUS)
DEALLOCATE(ZSIGMA)
DEALLOCATE(ZM)
DEALLOCATE(ZCTOTA)
DEALLOCATE(IM6)
DEALLOCATE(IM3)
DEALLOCATE(IM0)
DEALLOCATE(ZMASS)
!
END SUBROUTINE DUSTCAMS_n
