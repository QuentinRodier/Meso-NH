!SURFEX_LIC Copyright 1994-2014 Meteo-France 
!SURFEX_LIC This is part of the SURFEX software governed by the CeCILL-C  licence
!SURFEX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!SURFEX_LIC for details. version 1.
!     #########
    FUNCTION VEGTYPE_TO_PATCH(IVEGTYPE,INPATCH ) RESULT(IPATCH_NB) 
!   ####################################################
!!
!!    PURPOSE
!!    -------
!
!  Calculation of patch indices coresponding to different  vegtype
!          according to the  number of patch  (NPATCH).
!             
!!**  METHOD
!!    ------
!!
!!    EXTERNAL
!!    --------
!!    none
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!      
!!    none
!!
!!    REFERENCE
!!    ---------
!!
!!      
!!    AUTHOR
!!    ------
!    F.Solmon/V.Masson 06/00
!-------------------------------------------------------------------------------
!
!*       0.     DECLARATIONS
!               ------------
!
USE MODD_DATA_COVER_PAR, ONLY : NVT_NO, NVT_ROCK, NVT_SNOW,   &
                                  NVT_C3, NVT_C4, NVT_IRR,      &
                                  NVT_CONI, NVT_TREE, NVT_EVER, &
                                  NVT_TROG, NVT_PARK, NVT_GRAS  
!
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
USE MODI_ABOR1_SFX
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
INTEGER,        INTENT(IN) :: IVEGTYPE !indices of vegetation type           
INTEGER,        INTENT(IN) :: INPATCH  !total number of PATCHES used 
INTEGER                    :: IPATCH_NB! PATCH index corresponding to the vegtype IVEGTYPE  
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!*      0.2    declarations of local variables
!
!-----------------------------------------------------------------

IF (LHOOK) CALL DR_HOOK('VEGTYPE_TO_PATCH',0,ZHOOK_HANDLE)
IF (INPATCH==1) THEN
IPATCH_NB = 1 ! default case
END IF

!forest 
IF (INPATCH==2) THEN
   IF (IVEGTYPE== NVT_TREE  .OR. IVEGTYPE== NVT_CONI .OR. IVEGTYPE== NVT_EVER) THEN
       IPATCH_NB=2
   ELSE
       IPATCH_NB=1
   END IF
END IF

!forest + low vegeation differenciation
IF (INPATCH==3) THEN
   IF (IVEGTYPE== NVT_NO .OR. IVEGTYPE== NVT_ROCK .OR. IVEGTYPE== NVT_SNOW ) IPATCH_NB= 1
   IF (IVEGTYPE== NVT_TREE .OR. IVEGTYPE== NVT_CONI .OR. IVEGTYPE== NVT_EVER)  IPATCH_NB=2
   IF (IVEGTYPE== NVT_GRAS .OR. IVEGTYPE== NVT_TROG .OR. IVEGTYPE== NVT_PARK &
    .OR. IVEGTYPE== NVT_C3   .OR. IVEGTYPE== NVT_C4   .OR. IVEGTYPE== NVT_IRR )&
    IPATCH_NB=3  
END IF
!
!differenciation between irrigated crops and grassland and other low vegetation
IF (INPATCH==4) THEN
   IF (IVEGTYPE== NVT_NO .OR. IVEGTYPE== NVT_ROCK .OR. IVEGTYPE== NVT_SNOW ) IPATCH_NB= 1
   IF (IVEGTYPE== NVT_TREE .OR. IVEGTYPE== NVT_CONI .OR. IVEGTYPE== NVT_EVER)  IPATCH_NB=2
   IF (IVEGTYPE== NVT_GRAS .OR. IVEGTYPE== NVT_TROG .OR. &
         IVEGTYPE== NVT_C3   .OR. IVEGTYPE== NVT_C4 )  IPATCH_NB=3  
   IF (IVEGTYPE ==  NVT_IRR .OR. IVEGTYPE ==  NVT_PARK)  IPATCH_NB=4
END IF
!
!differenciation between crops and other low vegetation
IF (INPATCH==5) THEN
   IF (IVEGTYPE== NVT_NO .OR. IVEGTYPE== NVT_ROCK .OR. IVEGTYPE== NVT_SNOW ) IPATCH_NB= 1
   IF (IVEGTYPE== NVT_TREE .OR. IVEGTYPE== NVT_CONI .OR. IVEGTYPE== NVT_EVER)  IPATCH_NB=2
   IF (IVEGTYPE ==  NVT_C3 .OR. IVEGTYPE== NVT_C4 )  IPATCH_NB=3
   IF (IVEGTYPE ==  NVT_IRR .OR. IVEGTYPE ==  NVT_PARK)  IPATCH_NB=4
   IF (IVEGTYPE ==  NVT_GRAS .OR. IVEGTYPE== NVT_TROG )  IPATCH_NB=5
END IF
!
!differenciation between irrigated crops and gardens
IF (INPATCH==6) THEN
   IF (IVEGTYPE== NVT_NO .OR. IVEGTYPE== NVT_ROCK .OR. IVEGTYPE== NVT_SNOW ) IPATCH_NB= 1
   IF (IVEGTYPE== NVT_TREE .OR. IVEGTYPE== NVT_CONI .OR. IVEGTYPE== NVT_EVER)  IPATCH_NB=2
   IF (IVEGTYPE ==  NVT_C3 .OR. IVEGTYPE== NVT_C4 )  IPATCH_NB=3
   IF (IVEGTYPE ==  NVT_IRR )  IPATCH_NB=4
   IF (IVEGTYPE ==  NVT_GRAS .OR. IVEGTYPE== NVT_TROG)  IPATCH_NB=5
   IF (IVEGTYPE ==  NVT_PARK)  IPATCH_NB=6
END IF
!
!differenciation between snow and other bare soils
IF (INPATCH==7) THEN
   IF (IVEGTYPE ==  NVT_NO   .OR. IVEGTYPE== NVT_ROCK ) IPATCH_NB= 1
   IF (IVEGTYPE ==  NVT_SNOW)  IPATCH_NB=2
   IF (IVEGTYPE ==  NVT_TREE .OR. IVEGTYPE== NVT_CONI .OR. IVEGTYPE== NVT_EVER)  IPATCH_NB=3
   IF (IVEGTYPE ==  NVT_C3   .OR. IVEGTYPE== NVT_C4   )  IPATCH_NB=4
   IF (IVEGTYPE ==  NVT_IRR )  IPATCH_NB=5
   IF (IVEGTYPE ==  NVT_GRAS .OR. IVEGTYPE== NVT_TROG)  IPATCH_NB=6
   IF (IVEGTYPE ==  NVT_PARK)  IPATCH_NB=7
END IF

!
!differenciation between C3 and C4 crops
IF (INPATCH==8) THEN
   IF (IVEGTYPE== NVT_NO .OR. IVEGTYPE== NVT_ROCK ) IPATCH_NB= 1
   IF (IVEGTYPE ==  NVT_SNOW)  IPATCH_NB=2
   IF (IVEGTYPE ==  NVT_TREE .OR. IVEGTYPE== NVT_CONI .OR. IVEGTYPE== NVT_EVER)  IPATCH_NB=3
   IF (IVEGTYPE ==  NVT_C3  )  IPATCH_NB=4
   IF (IVEGTYPE ==  NVT_C4  )  IPATCH_NB=5
   IF (IVEGTYPE ==  NVT_IRR )  IPATCH_NB=6
   IF (IVEGTYPE ==  NVT_GRAS .OR. IVEGTYPE== NVT_TROG)  IPATCH_NB=7
   IF (IVEGTYPE ==  NVT_PARK)  IPATCH_NB=8
END IF

!
!differenciation between coniferous and broadleaf forests
IF (INPATCH==9) THEN
   IF (IVEGTYPE== NVT_NO .OR. IVEGTYPE== NVT_ROCK ) IPATCH_NB= 1
   IF (IVEGTYPE ==  NVT_SNOW)  IPATCH_NB=2
   IF (IVEGTYPE ==  NVT_TREE .OR. IVEGTYPE== NVT_EVER)  IPATCH_NB=3
   IF (IVEGTYPE ==  NVT_CONI)  IPATCH_NB=4
   IF (IVEGTYPE ==  NVT_C3  )  IPATCH_NB=5
   IF (IVEGTYPE ==  NVT_C4  )  IPATCH_NB=6
   IF (IVEGTYPE ==  NVT_IRR )  IPATCH_NB=7
   IF (IVEGTYPE ==  NVT_GRAS .OR. IVEGTYPE== NVT_TROG)  IPATCH_NB=8
   IF (IVEGTYPE ==  NVT_PARK)  IPATCH_NB=9
END IF

!
!differenciation between evergreen and deciduous broadleaf forests
IF (INPATCH==10) THEN
   IF (IVEGTYPE== NVT_NO .OR. IVEGTYPE== NVT_ROCK ) IPATCH_NB= 1
   IF (IVEGTYPE ==  NVT_SNOW)  IPATCH_NB=2
   IF (IVEGTYPE ==  NVT_TREE)  IPATCH_NB=3
   IF (IVEGTYPE ==  NVT_CONI)  IPATCH_NB=4
   IF (IVEGTYPE ==  NVT_EVER)  IPATCH_NB=5
   IF (IVEGTYPE ==  NVT_C3  )  IPATCH_NB=6
   IF (IVEGTYPE ==  NVT_C4  )  IPATCH_NB=7
   IF (IVEGTYPE ==  NVT_IRR )  IPATCH_NB=8
   IF (IVEGTYPE ==  NVT_GRAS .OR. IVEGTYPE== NVT_TROG)  IPATCH_NB=9
   IF (IVEGTYPE ==  NVT_PARK)  IPATCH_NB=10
END IF

!
!differenciation between rocks and flat bare soil
IF (INPATCH==11) THEN
   IF (IVEGTYPE ==  NVT_NO  )  IPATCH_NB=1
   IF (IVEGTYPE ==  NVT_ROCK)  IPATCH_NB=2
   IF (IVEGTYPE ==  NVT_SNOW)  IPATCH_NB=3
   IF (IVEGTYPE ==  NVT_TREE)  IPATCH_NB=4
   IF (IVEGTYPE ==  NVT_CONI)  IPATCH_NB=5
   IF (IVEGTYPE ==  NVT_EVER)  IPATCH_NB=6
   IF (IVEGTYPE ==  NVT_C3  )  IPATCH_NB=7
   IF (IVEGTYPE ==  NVT_C4  )  IPATCH_NB=8
   IF (IVEGTYPE ==  NVT_IRR )  IPATCH_NB=9
   IF (IVEGTYPE ==  NVT_GRAS .OR. IVEGTYPE== NVT_TROG)  IPATCH_NB=10
   IF (IVEGTYPE ==  NVT_PARK)  IPATCH_NB=11
END IF
!
!differenciation between tropical and temperate grasslands
IF (INPATCH==12) THEN
   IF (IVEGTYPE ==  NVT_NO  )  IPATCH_NB=1
   IF (IVEGTYPE ==  NVT_ROCK)  IPATCH_NB=2
   IF (IVEGTYPE ==  NVT_SNOW)  IPATCH_NB=3
   IF (IVEGTYPE ==  NVT_TREE)  IPATCH_NB=4
   IF (IVEGTYPE ==  NVT_CONI)  IPATCH_NB=5
   IF (IVEGTYPE ==  NVT_EVER)  IPATCH_NB=6
   IF (IVEGTYPE ==  NVT_C3  )  IPATCH_NB=7
   IF (IVEGTYPE ==  NVT_C4  )  IPATCH_NB=8
   IF (IVEGTYPE ==  NVT_IRR )  IPATCH_NB=9
   IF (IVEGTYPE ==  NVT_GRAS)  IPATCH_NB=10
   IF (IVEGTYPE ==  NVT_TROG)  IPATCH_NB=11
   IF (IVEGTYPE ==  NVT_PARK)  IPATCH_NB=12
END IF
!
IF (INPATCH>12) THEN
  CALL ABOR1_SFX('VEGTYPE_TO_PATCH: NPATCH BIGGER THAN 12 IS TOO LARGE')
END IF
IF (LHOOK) CALL DR_HOOK('VEGTYPE_TO_PATCH',1,ZHOOK_HANDLE)

!
END FUNCTION VEGTYPE_TO_PATCH
