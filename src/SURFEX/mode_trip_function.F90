!########################
MODULE MODE_TRIP_FUNCTION
!########################
!
!!****  *MODE_TRIP_FUNCTION*
!!
!!    PURPOSE
!!    -------
!    
!      The purpose of this routine is to store here all functions 
!      used by MODE_TRIP_INIT.
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
!-------------------------------------------------------------------------------
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
CONTAINS
!-------------------------------------------------------------------------------
!
!     ###############################################
      FUNCTION IRNXTX(IX,NX,IRIV) RESULT(KNEXTX)
!     ###############################################
!
IMPLICIT NONE
!
INTEGER, INTENT(IN)  :: IX,NX,IRIV 
INTEGER              :: KNEXTX
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
IF (LHOOK) CALL DR_HOOK('MODE_TRIP_FUNCTION:IRNXTX',0,ZHOOK_HANDLE)
IF(IRIV==1.OR.IRIV==5)THEN
  KNEXTX = IX 
ELSEIF(IRIV==8.OR.IRIV==7.OR.IRIV==6)THEN
  IF(IX==1)THEN
    KNEXTX = NX
  ELSE
    KNEXTX = IX-1
  ENDIF
ELSEIF(IRIV==2.OR.IRIV==3.OR.IRIV==4)THEN
  IF(IX==NX)THEN
    KNEXTX = 1
  ELSE
    KNEXTX = IX+1
  ENDIF
ELSE
    KNEXTX = 0
ENDIF
IF (LHOOK) CALL DR_HOOK('MODE_TRIP_FUNCTION:IRNXTX',1,ZHOOK_HANDLE)
!
END FUNCTION IRNXTX
!
!-------------------------------------------------------------------------------
!
!     ###############################################
      FUNCTION IRNXTY(IY,NY,IRIV) RESULT(KNEXTY)
!     ###############################################
!
IMPLICIT NONE
!
INTEGER, INTENT(IN)  :: IY,NY,IRIV 
INTEGER              :: KNEXTY
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
IF (LHOOK) CALL DR_HOOK('MODE_TRIP_FUNCTION:IRNXTY',0,ZHOOK_HANDLE)
IF(IRIV==7.OR.IRIV==3)THEN
  KNEXTY = IY 
ELSEIF(IRIV==6.OR.IRIV==5.OR.IRIV==4)THEN
  KNEXTY = IY-1
ELSEIF(IRIV==8.OR.IRIV==1.OR.IRIV==2)THEN
  IF(IY==NY)THEN
    KNEXTY = 0
  ELSE
    KNEXTY = IY+1
  ENDIF
ELSE
  KNEXTY = 0
ENDIF
IF (LHOOK) CALL DR_HOOK('MODE_TRIP_FUNCTION:IRNXTY',1,ZHOOK_HANDLE)
!
END FUNCTION IRNXTY
!
!-------------------------------------------------------------------------------
!
!     ###############################################
      FUNCTION GETLON(IX,NX) RESULT(PLON0)
!     ###############################################
!
IMPLICIT NONE
!
INTEGER, INTENT(IN)  :: IX,NX 
REAL                 :: PLON0
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
IF (LHOOK) CALL DR_HOOK('MODE_TRIP_FUNCTION:GETLON',0,ZHOOK_HANDLE)
PLON0 = 360.0 * (REAL(IX)-0.5) / REAL(NX) - 180.0
IF (LHOOK) CALL DR_HOOK('MODE_TRIP_FUNCTION:GETLON',1,ZHOOK_HANDLE)
!
END FUNCTION GETLON
!
!-------------------------------------------------------------------------------
!
!     ###############################################
      FUNCTION GETLAT(IY,NY) RESULT(PLAT0)
!     ###############################################
!
IMPLICIT NONE
!
INTEGER, INTENT(IN)  :: IY,NY 
REAL                 :: PLAT0
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
IF (LHOOK) CALL DR_HOOK('MODE_TRIP_FUNCTION:GETLAT',0,ZHOOK_HANDLE)
PLAT0 = 180.0 * (REAL(IY)-0.5) / REAL(NY) - 90.0
IF (LHOOK) CALL DR_HOOK('MODE_TRIP_FUNCTION:GETLAT',1,ZHOOK_HANDLE)
!
END FUNCTION GETLAT
!
!-------------------------------------------------------------------------------
!
!     ###############################################
      FUNCTION GIVELON(ZY) RESULT(PDLON)
!     ###############################################
!
USE MODD_TRIP_PAR, ONLY : XPI_T, XRAD_T
!
IMPLICIT NONE
!
REAL, INTENT(IN)  :: ZY
REAL              :: PDLON
!
REAL, PARAMETER   :: ZE2 = 0.006694470
REAL :: ZR, ZY_RAD, ZRA
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
IF (LHOOK) CALL DR_HOOK('MODE_TRIP_FUNCTION:GIVELON',0,ZHOOK_HANDLE)
ZRA = XRAD_T/1000.0
!
ZY_RAD = ZY * XPI_T / 180.
!
PDLON = XPI_T / 180.0 * ZRA * COS(ZY_RAD) / SQRT(1.0 - ZE2 * SIN(ZY_RAD) * SIN(ZY_RAD))
IF (LHOOK) CALL DR_HOOK('MODE_TRIP_FUNCTION:GIVELON',1,ZHOOK_HANDLE)
!
END FUNCTION GIVELON
!
!-------------------------------------------------------------------------------
!
!     ###############################################
      FUNCTION GIVELAT(ZY) RESULT(PDLAT)
!     ###############################################
!
USE MODD_TRIP_PAR, ONLY : XPI_T, XRAD_T
!
IMPLICIT NONE
!
REAL, INTENT(IN)  :: ZY
REAL              :: PDLAT
!
REAL, PARAMETER   :: ZE2 = 0.006694470
REAL :: ZR, ZY_RAD, ZRA
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
IF (LHOOK) CALL DR_HOOK('MODE_TRIP_FUNCTION:GIVELAT',0,ZHOOK_HANDLE)
ZRA = XRAD_T/1000.0
!
ZY_RAD = ZY * XPI_T / 180.
!
PDLAT = XPI_T / 180.0 * ZRA * (1.0-ZE2) / SQRT( (1.0 - ZE2 * SIN(ZY_RAD) * SIN(ZY_RAD))**3. )
IF (LHOOK) CALL DR_HOOK('MODE_TRIP_FUNCTION:GIVELAT',1,ZHOOK_HANDLE)
!
END FUNCTION GIVELAT
!
!-------------------------------------------------------------------------------
!
!     ###############################################
      FUNCTION GIVERAD(ZY) RESULT(PRAD)
!     ###############################################
!
USE MODD_TRIP_PAR, ONLY : XRAD_T, XPI_T
!
IMPLICIT NONE
!
REAL, INTENT(IN)  :: ZY
REAL              :: PRAD
!
REAL, PARAMETER   :: ZE2 = 0.006694470
REAL :: ZR, ZY_RAD, ZRN, ZRA
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
IF (LHOOK) CALL DR_HOOK('MODE_TRIP_FUNCTION:GIVERAD',0,ZHOOK_HANDLE)
ZRA = XRAD_T/1000.0
!
ZY_RAD = ZY * XPI_T / 180.
!
ZRN = ZRA / SQRT(1.0 - ZE2 *  SIN(ZY_RAD) * SIN(ZY_RAD) )
!
PRAD = ZRN * SQRT( 1.0 - ZE2 * SIN(ZY_RAD) + ZE2 * ZE2 * SIN(ZY_RAD) * SIN(ZY_RAD) )
IF (LHOOK) CALL DR_HOOK('MODE_TRIP_FUNCTION:GIVERAD',1,ZHOOK_HANDLE)
!
END FUNCTION GIVERAD
!
!-------------------------------------------------------------------------------
!
!     ###############################################
      FUNCTION GIVELEN(ZX,ZY,ZX_N,ZY_N) RESULT(PLEN0)
!     ###############################################
!
IMPLICIT NONE
!
REAL, INTENT(IN)  :: ZX,ZY,ZX_N,ZY_N
REAL              :: PLEN0
!
REAL :: ZLAT, ZDX, ZDY, ZRAD, ZDLON, ZDLAT
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
IF (LHOOK) CALL DR_HOOK('MODE_TRIP_FUNCTION:GIVELEN',0,ZHOOK_HANDLE)
ZDLON = ABS(ZX-ZX_N)
ZDLAT = ABS(ZY-ZY_N)
!
IF(ZDLON>=180.0)ZDLON = ABS(360.0 - ZDLON)
!
PLEN0 = 0.0
!
IF(ZX==ZX_N)THEN
  ZLAT  = (ZY+ZY_N) / 2.0
  PLEN0 = GIVELAT(ZLAT) * ZDLAT
ELSEIF(ZY==ZY_N)THEN
  ZLAT  = ZY
  PLEN0 = GIVELON(ZLAT) * ZDLON
ELSE
  ZLAT  = (ZY+ZY_N) / 2.0
  ZRAD  = GIVERAD(ZLAT)
  ZDX   = GIVELON(ZLAT) * ZDLON / ZRAD
  ZDY   = GIVELAT(ZLAT) * ZDLAT / ZRAD
  PLEN0 = ACOS(COS(ZDX)*COS(ZDY)) * ZRAD
ENDIF
IF (LHOOK) CALL DR_HOOK('MODE_TRIP_FUNCTION:GIVELEN',1,ZHOOK_HANDLE)
!
END FUNCTION GIVELEN
!
!-------------------------------------------------------------------------------
!
!     ##############################################
      FUNCTION FUNCVEL(PRC,PL,PW,PX,PSIN) RESULT(PY)
!     ##############################################
!
USE MODD_TRIP_PAR, ONLY : XM_EXP, XRHOLW_T
!
IMPLICIT NONE
!
REAL, INTENT(IN)  :: PRC,PL,PW,PX,PSIN
!
REAL              :: PY
REAL              :: ZHS,ZRADIUS,ZVEL
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
IF (LHOOK) CALL DR_HOOK('MODE_TRIP_FUNCTION:FUNCVEL',0,ZHOOK_HANDLE)
ZHS=0.0
IF(PRC>0.0)THEN
   ZHS=PX/(XRHOLW_T*PL*PW)
ENDIF
!
ZVEL=DIAGVEL(PRC,PL,PW,ZHS)
!
PY= PSIN-ZVEL*PX/PL
IF (LHOOK) CALL DR_HOOK('MODE_TRIP_FUNCTION:FUNCVEL',1,ZHOOK_HANDLE)
!
END FUNCTION FUNCVEL
!
!-------------------------------------------------------------------------------
!
!     ##############################################
      FUNCTION DIAGVEL(PRC,PL,PW,PX) RESULT(PY)
!     ##############################################
!
USE MODD_TRIP_n,   ONLY : XTRIP_TSTEP,XCVEL
USE MODD_TRIP_PAR, ONLY : XM_EXP,XRHOLW_T
!
IMPLICIT NONE
!
REAL, INTENT(IN)  :: PRC,PL,PW,PX
!
REAL              :: PY
REAL              :: ZRADIUS, ZVV
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
IF (LHOOK) CALL DR_HOOK('MODE_TRIP_FUNCTION:DIAGVEL',0,ZHOOK_HANDLE)
!
IF(PRC>0.0)THEN
  ZRADIUS=PW*PX/(PW+2.0*PX)
  ZVV=MIN(PRC*(ZRADIUS**XM_EXP),PL/XTRIP_TSTEP)
  PY=MAX(0.1,ZVV)
ELSE
  PY=XCVEL
ENDIF
!
IF (LHOOK) CALL DR_HOOK('MODE_TRIP_FUNCTION:DIAGVEL',1,ZHOOK_HANDLE)
!
END FUNCTION DIAGVEL
!
!-------------------------------------------------------------------------------
!
!     ##################################################################
      FUNCTION FUNCFLOOD(PX,PF,PHF,PLF,PWF,PHC,PNF,PW,PL,PD,PFF) RESULT(PY)
!     ##################################################################
!
USE MODD_TRIP_PAR, ONLY : XRHOLW_T
!
IMPLICIT NONE
!
REAL, INTENT(IN)  :: PX,PF,PHF,PHC,PNF,PLF,PWF,PW,PL,PD,PFF
REAL              :: PY
REAL              :: ZMEX,ZMDEF,ZHT,ZVIN,ZVOUT,ZHS
REAL              :: ZQIN,ZQOUT,ZFL,ZHIN,ZHOUT,ZHLIM
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
IF (LHOOK) CALL DR_HOOK('MODE_TRIP_FUNCTION:FUNCFLOOD',0,ZHOOK_HANDLE)
PY=0.0
IF(PHC==0.0.OR.PD==1.0 .AND. LHOOK) CALL DR_HOOK('MODE_TRIP_FUNCTION:FUNCFLOOD',1,ZHOOK_HANDLE)
IF(PHC==0.0.OR.PD==1.0)RETURN
!
ZHS=PX/(XRHOLW_T*PL*PW)
!
ZFL=MAX(1.E-3,PLF)
!
! * water mass exchanged
!
ZHIN =MAX(0.0,ZHS-PHC-PHF)
ZHOUT=MAX(0.0,PHF+PHC-ZHS)
!
ZMEX =ZFL*ZHIN *PW*XRHOLW_T 
ZMDEF=ZFL*ZHOUT*PW*XRHOLW_T
!
ZMDEF=MIN(ZMDEF,PF)
!
! * water velocity
!
 CALL VELFLOOD(ZHS,PFF,PHF,PLF,PWF,PHC,PNF,PW,PD,ZVIN,ZVOUT)
!
! * inflow or outflow water flux
!
ZQIN =ZVIN *ZMEX /(PW+PWF)
ZQOUT=ZVOUT*ZMDEF/(PW+PWF)
!
! * total water flux
!
PY=ZQIN-ZQOUT
IF (LHOOK) CALL DR_HOOK('MODE_TRIP_FUNCTION:FUNCFLOOD',1,ZHOOK_HANDLE)
!
END FUNCTION FUNCFLOOD
!
!-------------------------------------------------------------------------------
!
!     ##############################################
      SUBROUTINE VELFLOOD(PHS,PFF,PHF,PLF,PWF,PHC, &
                            PNF,PW,PD,PVIN,PVOUT )  
!     ##############################################
!
USE MODD_TRIP_n,   ONLY : XTRIP_TSTEP
USE MODD_TRIP_PAR, ONLY : XM_EXP
!
IMPLICIT NONE
!
REAL, INTENT(IN)  :: PHS,PHF,PLF,PWF,PHC,PNF,PW,PD,PFF
REAL, INTENT(OUT) :: PVIN,PVOUT
!
REAL              :: ZSLOPE_IN,ZSLOPE_OUT,ZFL, &
                       ZRADIN,ZRADOUT,ZHEX  
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
IF (LHOOK) CALL DR_HOOK('MODE_TRIP_FUNCTION:VELFLOOD',0,ZHOOK_HANDLE)
PVIN =0.0
PVOUT=0.0
!
IF(PHC==0.0.OR.PD==1.0 .AND. LHOOK) CALL DR_HOOK('MODE_TRIP_FUNCTION:VELFLOOD',1,ZHOOK_HANDLE)
IF(PHC==0.0.OR.PD==1.0)RETURN
!
ZFL=MAX(1.E-3,PLF)
!
! * water slope
!
ZSLOPE_IN =2.0*MAX(0.0,PHS-PHC-PHF)/(PW+PWF)
ZSLOPE_OUT=2.0*MAX(0.0,PHF+PHC-PHS)/(PW+PWF)
!
! * manning velocity
!
ZHEX =MAX(0.0,PHS-PHC)
!
ZRADIN  = ZHEX*ZFL/(ZFL+2.0*ZHEX)
ZRADOUT = PHF *ZFL/(ZFL+2.0*PHF)
!
IF(PFF<1.0)THEN
   PVIN =(ZRADIN **XM_EXP)*SQRT(ZSLOPE_IN )/PNF
ENDIF
!
PVOUT=(ZRADOUT**XM_EXP)*SQRT(ZSLOPE_OUT)/PNF
!
IF(PVIN >0.0)PVIN =MAX(0.001,PVIN)
IF(PVOUT>0.0)PVOUT=MAX(0.001,PVOUT)
!
PVIN =MIN(PVIN, (PW+PWF)/XTRIP_TSTEP)
PVOUT=MIN(PVOUT,(PW+PWF)/XTRIP_TSTEP)
IF (LHOOK) CALL DR_HOOK('MODE_TRIP_FUNCTION:VELFLOOD',1,ZHOOK_HANDLE)
!
END SUBROUTINE VELFLOOD
!
!-------------------------------------------------------------------------------
!
!     ###########################################################
      FUNCTION DELTA_FLOOD(PX,PF,PHF,PLF,PWF,PHC,PW,PL) RESULT(PY)
!     ###########################################################
!
USE MODD_TRIP_PAR, ONLY : XRHOLW_T
!
IMPLICIT NONE
!
REAL, INTENT(IN)  :: PX,PF,PHF,PLF,PWF,PHC,PW,PL
REAL              :: PY
REAL              :: ZMDEF,ZHDEF,ZHS,ZFL
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
IF (LHOOK) CALL DR_HOOK('MODE_TRIP_FUNCTION:DELTA_FLOOD',0,ZHOOK_HANDLE)
PY=0.0
IF(PHC==0.0 .AND. LHOOK) CALL DR_HOOK('MODE_TRIP_FUNCTION:DELTA_FLOOD',1,ZHOOK_HANDLE)
IF(PHC==0.0)RETURN
!
ZHS=PX/(XRHOLW_T*PL*PW)
!
ZFL=MAX(1.E-3,PLF)
!
IF(PHF==0.0)THEN
  ZHDEF=0.0
ELSE
  ZHDEF=MAX(0.0,PHF+PHC-ZHS)
ENDIF
!
ZMDEF=ZHDEF*XRHOLW_T*PW*ZFL
IF(PF<ZMDEF)THEN
  PY=1.0
ELSE
  PY=0.0
ENDIF
IF (LHOOK) CALL DR_HOOK('MODE_TRIP_FUNCTION:DELTA_FLOOD',1,ZHOOK_HANDLE)
!
END FUNCTION DELTA_FLOOD
!
!-------------------------------------------------------------------------------
!
END MODULE MODE_TRIP_FUNCTION   
