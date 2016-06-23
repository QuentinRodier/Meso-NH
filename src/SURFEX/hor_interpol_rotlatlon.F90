!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
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
!!      P. Samuelsson 10/2014 Moved subroutine REGROT to separate routine
!!------------------------------------------------------------------
!
!
!
USE MODD_PREP,       ONLY : XLAT_OUT, XLON_OUT
USE MODD_GRID_ROTLATLON
USE MODD_SURF_PAR,   ONLY : XUNDEF
USE MODD_GRID_GRIB,  ONLY : NNI
!
!RJ: missing modi
USE MODI_REGROT_LONLAT_ROT
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

    CALL REGROT_LONLAT_ROT(XLON_OUT,XLAT_OUT,   &
                  XRON_OUT,XRAT_OUT,            &
                  INO,1,INO,1,                  &
                  XRLOP,XRLAP,1                 )  

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
!-------------------------------------------------------------------------------------
END SUBROUTINE HOR_INTERPOL_ROTLATLON
