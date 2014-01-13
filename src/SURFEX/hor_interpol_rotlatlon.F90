!SURFEX_LIC Copyright 1994-2014 Meteo-France 
!SURFEX_LIC This is part of the SURFEX software governed by the CeCILL-C  licence
!SURFEX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!SURFEX_LIC for details. version 1.
!     #########
SUBROUTINE HOR_INTERPOL_ROTLATLON(KLUOUT,PFIELDIN,PFIELDOUT)
!     #################################################################################
!
!!****  *HOR_INTERPOL_ROTLATLON * - Interpolation from a rotated lat/lon grid
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
!!     U. Andrae
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    10/2007
!!------------------------------------------------------------------
!
!
!
USE MODD_PREP,       ONLY : XLAT_OUT, XLON_OUT
USE MODD_GRID_ROTLATLON
USE MODD_SURF_PAR,   ONLY : XUNDEF
USE MODD_GRID_GRIB,  ONLY : NNI

!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
INTEGER,              INTENT(IN)  :: KLUOUT    ! logical unit of output listing
REAL, DIMENSION(:,:), INTENT(IN)  :: PFIELDIN  ! field to interpolate horizontally
REAL, DIMENSION(:,:), INTENT(OUT) :: PFIELDOUT ! interpolated field
!
!*      0.2    declarations of local variables
!

 INTEGER, ALLOCATABLE :: ii(:),jj(:)

 REAL,    ALLOCATABLE :: XLAT_IND(:),XLON_IND(:),  &
                           XRAT_OUT(:),XRON_OUT(:),  &
                           w00(:),w01(:),            &
                           w10(:),w11(:)  

 LOGICAL, ALLOCATABLE :: LMASK(:)

INTEGER :: I,J,K,L,IJ,IJ00,IJ01,IJ10,IJ11,INO,JL
REAL    :: WX,WY,WSUM
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('HOR_INTERPOL_ROTLATLON',0,ZHOOK_HANDLE)
WRITE(KLUOUT,'(A)')' | Running rotated latlon interpolation'

INO = SIZE(XLAT_OUT)

!
!*      1.    Allocations
!
ALLOCATE(XRAT_OUT(INO),       &
           XRON_OUT(INO),       &
           XLAT_IND(INO),       &
           XLON_IND(INO),       &
                 II(INO),       &
                 JJ(INO),       &
                W00(INO),       &
                W01(INO),       &
                W10(INO),       &
                W11(INO))  

ALLOCATE(LMASK(NNI))
!
!*  Transformation of latitudes/longitudes into rotated coordinates

    WRITE(KLUOUT,*)'XLAT_OUT',XLAT_OUT(10:10)
    WRITE(KLUOUT,*)'XLON_OUT',XLON_OUT(10:10)

    CALL REGROT(XLON_OUT,XLAT_OUT,   &
                  XRON_OUT,XRAT_OUT,   &
                  INO,1,INO,1,         &
                  XRLOP,XRLAP,1)  

    WRITE(KLUOUT,*)'XRAT_OUT',XRAT_OUT(10:10)
    WRITE(KLUOUT,*)'XRON_OUT',XRON_OUT(10:10)

    DO IJ=1,INO
       XLAT_IND(IJ) = ( XRAT_OUT(IJ) - XRILA1) / XRDY + 1.
       XLON_IND(IJ) = ( XRON_OUT(IJ) - XRILO1) / XRDX + 1.
    ENDDO

    PFIELDOUT(:,:) = XUNDEF

    DO JL=1,SIZE(PFIELDIN,2)

    LMASK= .TRUE.
    WHERE ( ABS(PFIELDIN(:,JL)-XUNDEF) < 1.e-6 ) LMASK = .FALSE.

    DO IJ=1,INO

         II(IJ)  = INT(XLON_IND(IJ))
         JJ(IJ)  = INT(XLAT_IND(IJ))

         WX  = XLON_IND(IJ) - FLOAT(II(IJ))
         WY  = XLAT_IND(IJ) - FLOAT(JJ(IJ))

         W00(IJ) = (1.-WX)*(1.-WY)
         W01(IJ) = (1.-WX)*    WY
         W10(IJ) =     WX *(1.-WY)
         W11(IJ) =     WX *    WY

         K    = II(IJ)
         L    = JJ(IJ)
         IJ00 = k   + NRX*(l   -1)
         IJ01 = k   + NRX*(l+1 -1)
         IJ10 = k+1 + NRX*(l   -1)
         IJ11 = k+1 + NRX*(l+1 -1)

         IF (.NOT. LMASK(IJ00)) w00(IJ) = 0.
         IF (.NOT. LMASK(IJ01)) w01(IJ) = 0.
         IF (.NOT. LMASK(IJ10)) w10(IJ) = 0.
         IF (.NOT. LMASK(IJ11)) w11(IJ) = 0.

            wsum = w00(IJ) + w01(IJ) + &
                     w10(IJ) + w11(IJ)  

            IF ( ABS(wsum) < 1.e-6 ) CYCLE

            w00(IJ) = w00(IJ) / wsum
            w01(IJ) = w01(IJ) / wsum
            w10(IJ) = w10(IJ) / wsum
            w11(IJ) = w11(IJ) / wsum

    ENDDO

    !
    ! Bi linear
    !

    WRITE(KLUOUT,*)'NRX,NRY',NRX,NRY

       DO IJ=1,INO

          K    = II(IJ)
          L    = JJ(IJ)
          IJ00 = k   + NRX*(l   -1)
          IJ01 = k   + NRX*(l+1 -1)
          IJ10 = k+1 + NRX*(l   -1)
          IJ11 = k+1 + NRX*(l+1 -1)

          WRITE(KLUOUT,*)PFIELDIN(IJ00,JL)

          PFIELDOUT(IJ,JL) = w00(IJ)*PFIELDIN(IJ00,JL) +       &
                               w01(IJ)*PFIELDIN(IJ01,JL) +       &
                               w10(IJ)*PFIELDIN(IJ10,JL) +       &
                               w11(IJ)*PFIELDIN(IJ11,JL)  
 
       ENDDO
    ENDDO


!
!*      5.    Deallocations
!
DEALLOCATE(XRAT_OUT,XRON_OUT,    &
             XLAT_IND,XLON_IND,    &
             II,JJ,                &
             W00,W01,W10,W11,      &
             LMASK)  
!
IF (LHOOK) CALL DR_HOOK('HOR_INTERPOL_ROTLATLON',1,ZHOOK_HANDLE)
CONTAINS
!
      SUBROUTINE REGROT(PXREG,PYREG,PXROT,PYROT,KXDIM,KYDIM,KX,KY, &
                         PXCEN,PYCEN,KCALL)  
!
!
      IMPLICIT NONE
!
!-----------------------------------------------------------------------
!
!*    CONVERSION BETWEEN REGULAR AND ROTATED SPHERICAL COORDINATES.
!*
!*    PXREG     LONGITUDES OF THE REGULAR COORDINATES
!*    PYREG     LATITUDES OF THE REGULAR COORDINATES
!*    PXROT     LONGITUDES OF THE ROTATED COORDINATES
!*    PYROT     LATITUDES OF THE ROTATED COORDINATES
!*              ALL COORDINATES GIVEN IN DEGREES N (NEGATIVE FOR S)
!*              AND DEGREES E (NEGATIVE VALUES FOR W)
!*    KXDIM     DIMENSION OF THE GRIDPOINT FIELDS IN THE X-DIRECTION
!*    KYDIM     DIMENSION OF THE GRIDPOINT FIELDS IN THE Y-DIRECTION
!*    KX        NUMBER OF GRIDPOINT IN THE X-DIRECTION
!*    KY        NUMBER OF GRIDPOINTS IN THE Y-DIRECTION
!*    PXCEN     REGULAR LONGITUDE OF THE SOUTH POLE OF THE ROTATED GRID
!*    PYCEN     REGULAR LATITUDE OF THE SOUTH POLE OF THE ROTATED GRID
!*
!*    KCALL=-1: FIND REGULAR AS FUNCTIONS OF ROTATED COORDINATES.
!*    KCALL= 1: FIND ROTATED AS FUNCTIONS OF REGULAR COORDINATES.
!*
!*    J.E. HAUGEN   HIRLAM   JUNE -92
!
!-----------------------------------------------------------------------
!
      INTEGER KXDIM,KYDIM,KX,KY,KCALL
      REAL PXREG(KXDIM,KYDIM),PYREG(KXDIM,KYDIM), &
            PXROT(KXDIM,KYDIM),PYROT(KXDIM,KYDIM), &
            PXCEN,PYCEN  
!
!-----------------------------------------------------------------------
!
      REAL PI,ZRAD,ZSYCEN,ZCYCEN,ZXMXC,ZSXMXC,ZCXMXC,ZSYREG,ZCYREG, &
            ZSYROT,ZCYROT,ZCXROT,ZSXROT,ZRADI  
      INTEGER JY,JX
      REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-----------------------------------------------------------------------
!
      IF (LHOOK) CALL DR_HOOK('REGROT',0,ZHOOK_HANDLE)
      PI = 4.*ATAN(1.)
      ZRAD = PI/180.
      ZRADI = 1./ZRAD
      ZSYCEN = SIN(ZRAD*(PYCEN+90.))
      ZCYCEN = COS(ZRAD*(PYCEN+90.))
!
      IF (KCALL.EQ.1) THEN
!
      DO JY = 1,KY
      DO JX = 1,KX
!
      ZXMXC  = ZRAD*(PXREG(JX,JY) - PXCEN)
      ZSXMXC = SIN(ZXMXC)
      ZCXMXC = COS(ZXMXC)
      ZSYREG = SIN(ZRAD*PYREG(JX,JY))
      ZCYREG = COS(ZRAD*PYREG(JX,JY))
      ZSYROT = ZCYCEN*ZSYREG - ZSYCEN*ZCYREG*ZCXMXC
      ZSYROT = MAX(ZSYROT,-1.0)
      ZSYROT = MIN(ZSYROT,+1.0)
!
      PYROT(JX,JY) = ASIN(ZSYROT)*ZRADI
!
      ZCYROT = COS(PYROT(JX,JY)*ZRAD)
      ZCXROT = (ZCYCEN*ZCYREG*ZCXMXC +     &
                 ZSYCEN*ZSYREG)/ZCYROT  
      ZCXROT = MAX(ZCXROT,-1.0)
      ZCXROT = MIN(ZCXROT,+1.0)
      ZSXROT = ZCYREG*ZSXMXC/ZCYROT
!
      PXROT(JX,JY) = ACOS(ZCXROT)*ZRADI
!
      IF (ZSXROT.LT.0.0) PXROT(JX,JY) = -PXROT(JX,JY)
!
      ENDDO
      ENDDO
!
      ELSEIF (KCALL.EQ.-1) THEN
!
      DO JY = 1,KY
      DO JX = 1,KX
!
      ZSXROT = SIN(ZRAD*PXROT(JX,JY))
      ZCXROT = COS(ZRAD*PXROT(JX,JY))
      ZSYROT = SIN(ZRAD*PYROT(JX,JY))
      ZCYROT = COS(ZRAD*PYROT(JX,JY))
      ZSYREG = ZCYCEN*ZSYROT + ZSYCEN*ZCYROT*ZCXROT
      ZSYREG = MAX(ZSYREG,-1.0)
      ZSYREG = MIN(ZSYREG,+1.0)
!
      PYREG(JX,JY) = ASIN(ZSYREG)*ZRADI
!
      ZCYREG = COS(PYREG(JX,JY)*ZRAD)
      ZCXMXC = (ZCYCEN*ZCYROT*ZCXROT -     &
                 ZSYCEN*ZSYROT)/ZCYREG  
      ZCXMXC = MAX(ZCXMXC,-1.0)
      ZCXMXC = MIN(ZCXMXC,+1.0)
      ZSXMXC = ZCYROT*ZSXROT/ZCYREG
      ZXMXC  = ACOS(ZCXMXC)*ZRADI
      IF (ZSXMXC.LT.0.0) ZXMXC = -ZXMXC
!
      PXREG(JX,JY) = ZXMXC + PXCEN
!
      ENDDO
      ENDDO
!
      ELSE
      WRITE(6,'(1X,''INVALID KCALL IN REGROT'')')
      CALL ABORT
      ENDIF
      IF (LHOOK) CALL DR_HOOK('REGROT',1,ZHOOK_HANDLE)
!
      END SUBROUTINE REGROT
!
!-------------------------------------------------------------------------------------
END SUBROUTINE HOR_INTERPOL_ROTLATLON
