!MNH_LIC Copyright 2021-2023 CNRS, Meteo-France and Universite Paul Sabatier
!MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
!MNH_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!MNH_LIC for details. version 1.
!-----------------------------------------------------------------
!       #####################
        MODULE MODI_RECYCL_FLUC
!       #####################
!
INTERFACE
!
SUBROUTINE RECYCL_FLUC (PPTABU,PPTABV,PPTABW,PTHT,PDZZ,ONR_COUNT,OPT_COUNT,PMINW,PMINN,PMINE,PMINS, &
                          PFLUCTUNW,PFLUCTVNN,PFLUCTUTN,PFLUCTVTW,PFLUCTWTW,PFLUCTWTN,              &
                          PFLUCTUNE,PFLUCTVNS,PFLUCTUTS,PFLUCTVTE,PFLUCTWTE,PFLUCTWTS               )

  INTEGER                        ,INTENT(IN)    :: ONR_COUNT,OPT_COUNT,PMINW,PMINN,PMINE,PMINS
  REAL, DIMENSION(:,:,:)         ,INTENT(IN)    :: PPTABU,PPTABV,PPTABW,PTHT,PDZZ
  REAL, DIMENSION(:,:)           ,INTENT(INOUT) :: PFLUCTUNW,PFLUCTVTW,PFLUCTVNN,PFLUCTUTN,PFLUCTWTW,PFLUCTWTN
  REAL, DIMENSION(:,:)           ,INTENT(INOUT) :: PFLUCTUNE,PFLUCTVTE,PFLUCTVNS,PFLUCTUTS,PFLUCTWTE,PFLUCTWTS

END SUBROUTINE RECYCL_FLUC
!
END INTERFACE
!
END MODULE MODI_RECYCL_FLUC
!
!
!
!       ####################################
        SUBROUTINE RECYCL_FLUC (PPTABU,PPTABV,PPTABW,PTHT,PDZZ,ONR_COUNT,OPT_COUNT,PMINW,PMINN,PMINE,PMINS,  &
                                  PFLUCTUNW,PFLUCTVNN,PFLUCTUTN,PFLUCTVTW,PFLUCTWTW,PFLUCTWTN,               &
                                  PFLUCTUNE,PFLUCTVNS,PFLUCTUTS,PFLUCTVTE,PFLUCTWTE,PFLUCTWTS                )

!       ####################################
!
!!****     *RECYCL_FLUC*  - routine calculating the velocity forcing fluctuations
!                              
!!
!!      PURPOSE
!!      -------
!         RECYCLING METHOD
!
!!      METHOD
!!      ------
!!!
!!      EXTERNAL
!!      --------
!!        NONE
!!
!!      IMPLICIT ARGUMENTS
!!      ------------------
!!
!!      REFERENCE
!!      ---------
!!
!!      AUTHOR
!!      ------
!!        Tim Nagel       * Meteo-France *
!!
!!      MODIFICATIONS
!!      -------------
!!        Original          01/02/2021
!  P. Wautelet 09/02/2023: XTMOY, XTMOYCOUNT and XNUMBELT => NTMOY, NTMOYCOUNT and NNUMBELT
!------------------------------------------------------------------------------
!       
!**** 0. DECLARATIONS
!     ---------------
!
! module
USE MODE_POS
USE MODE_ll
USE MODE_IO
USE MODI_SHUMAN
!
! declaration
USE MODD_VAR_ll,          ONLY: IP, NPROC
USE MODD_CONF,            ONLY: NHALO
!
USE MODD_RECYCL_PARAM_n
!
USE MODD_PARAMETERS
USE MODD_CONF
!
USE MODD_CST
!
USE MODD_DIM_n
USE MODD_CONF
USE MODD_CONF_n
USE MODD_GRID
USE MODD_GRID_n
USE MODD_METRICS_n
USE MODD_TIME
USE MODD_TIME_n
USE MODD_DYN_n
USE MODD_FIELD_n
USE MODD_CURVCOR_n
USE MODI_GRADIENT_M
USE MODI_GRADIENT_W
USE MODI_GRADIENT_U
USE MODI_GRADIENT_V
USE MODE_GRIDPROJ
USE MODD_REF
USE MODD_LATZ_EDFLX
!
USE MODI_MEAN_Z
!
IMPLICIT NONE
!
!------------------------------------------------------------------------------
!
!       0.1  declarations of arguments
  INTEGER                        ,INTENT(IN)    :: ONR_COUNT,OPT_COUNT,PMINW,PMINN,PMINE,PMINS
  REAL, DIMENSION(:,:,:)         ,INTENT(IN)    :: PPTABU,PPTABV,PPTABW,PTHT,PDZZ
  REAL, DIMENSION(:,:)           ,INTENT(INOUT) :: PFLUCTUNW,PFLUCTVTW,PFLUCTVNN,PFLUCTUTN,PFLUCTWTW,PFLUCTWTN
  REAL, DIMENSION(:,:)           ,INTENT(INOUT) :: PFLUCTUNE,PFLUCTVTE,PFLUCTVNS,PFLUCTUTS,PFLUCTWTE,PFLUCTWTS
!
!------------------------------------------------------------------------------
!
!       0.2  declaration of local variables
INTEGER                                     :: IIU,IJU,IKU,IIP,JJ,JI,JK,IIB,IJB,IIE,IJE,IKE,IKB
INTEGER                                     :: ICOUNT,JCOUNT,IIMAX_ll,IJMAX_ll                                  
REAL,DIMENSION(:,:)         ,ALLOCATABLE    :: ZTMPUTN,ZTMPVNN,ZTMPWTN                         !Velocity in the recycling Plan, NORTH
REAL,DIMENSION(:,:)         ,ALLOCATABLE    :: ZTMPFUTN,ZTMPFVNN,ZTMPFWTN                      !Fluctuations in the recycling Plan, NORTH
REAL,DIMENSION(:,:)         ,ALLOCATABLE    :: ZTMPUNW,ZTMPVTW,ZTMPWTW                         !Velocity in the recycling Plan, WEST
REAL,DIMENSION(:,:)         ,ALLOCATABLE    :: ZTMPFUNW,ZTMPFVTW,ZTMPFWTW                      !Fluctuations in the recycling Plan, WEST
REAL,DIMENSION(:,:)         ,ALLOCATABLE    :: ZTMPUNE,ZTMPVTE,ZTMPWTE                         !Velocity in the recycling Plan EAST
REAL,DIMENSION(:,:)         ,ALLOCATABLE    :: ZTMPFUNE,ZTMPFVTE,ZTMPFWTE                      !Fluctuations in the recycling Plan, EAST
REAL,DIMENSION(:,:)         ,ALLOCATABLE    :: ZTMPUTS,ZTMPVNS,ZTMPWTS                         !Velocity in the recycling Plan, SOUTH
REAL,DIMENSION(:,:)         ,ALLOCATABLE    :: ZTMPFUTS,ZTMPFVNS,ZTMPFWTS                      !Fluctuations in the recycling Plan, SOUTH
REAL,DIMENSION(:,:)         ,ALLOCATABLE    :: ZTMPZ
REAL,DIMENSION(:,:)         ,ALLOCATABLE    :: ZALPNORTH,ZALPWEST,ZALPSOUTH,ZALPEAST           !Coefficient for the fluctuation (ZALP IN [0-1])
REAL, DIMENSION(:,:)        ,ALLOCATABLE    :: ZTMPNDW,ZTMPNDN,ZTMPNDE,ZTMPNDS                 !Brunt Vaisala frequency
REAL, DIMENSION(:,:,:)      ,ALLOCATABLE    :: ZND,ZWORK32                                     !Brunt Vaisala frequency (3D fields)
INTEGER                                     :: IINFO_ll

!------------------------------------------------------------------------------
!
! *** Allocation and dimension
!
CALL GET_INDICE_ll(IIB,IJB,IIE,IJE)
CALL GET_DIM_EXT_ll('B',IIU,IJU)
CALL GET_GLOBALDIMS_ll(IIMAX_ll,IJMAX_ll)
IKU = SIZE(PPTABU,3)
IKE=IKU-JPVEXT
IKB =   1 + JPVEXT

ALLOCATE(ZWORK32(SIZE(PTHT,1),SIZE(PTHT,2),SIZE(PTHT,3)))
ALLOCATE(ZND(SIZE(PTHT,1),SIZE(PTHT,2),SIZE(PTHT,3)))

!
! *** Dry Brunt Vaisala frequency
!
ZWORK32(:,:,:)=DZM(PTHT(:,:,:))/ MZM(PTHT(:,:,:))
DO JK=1,(IKE+1)
   DO JJ=1,(IJE+1)
      DO JI=1,(IIE+1)
         IF(ZWORK32(JI,JJ,JK)<0.) THEN
           ZND(JI,JJ,JK)= -1.*SQRT( ABS( XG*ZWORK32(JI,JJ,JK)/ PDZZ(JI,JJ,JK) ))
         ELSE
           ZND(JI,JJ,JK)= SQRT( ABS( XG*ZWORK32(JI,JJ,JK)/ PDZZ(JI,JJ,JK) ) )
         ENDIF
      ENDDO
   ENDDO
ENDDO
ZND(:,:,:) = ABS(ZND(:,:,:))
DO JK=1,(IKE+1)
   DO JJ=1,(IJE+1)
      DO JI=1,(IIE+1)
         IF(ZND(JI,JJ,JK)>1.E6) THEN
           ZND(JI,JJ,JK)= 1.E6
         ELSEIF(ZND(JI,JJ,JK)<1.E-6) THEN
           ZND(JI,JJ,JK)= 1.E-6
         ENDIF
         ZND(JI,JJ,JK) = 1./ZND(JI,JJ,JK)
      ENDDO
   ENDDO
ENDDO
IF (LWEST_ll ()) ZND(IIB-1,:,:)=ZND(IIB,:,:)
IF (LNORTH_ll()) ZND(:,IJE+1,:)=ZND(:,IJE,:)
IF (LEAST_ll ()) ZND(IIE+1,:,:)=ZND(IIE,:,:)
IF (LSOUTH_ll()) ZND(:,IJB-1,:)=ZND(:,IJB,:)
ZND(:,:,IKE+1)=ZND(:,:,IKE)
ZND(:,:,IKB-1)=ZND(:,:,IKB)


IF (LRECYCLW) THEN
  !-------------------------------------------------------
  !-----------WEST
  !------------------------------------------------------
  ALLOCATE(ZTMPUNW (IJU,IKU))
  ALLOCATE(ZTMPVTW (IJU,IKU))
  ALLOCATE(ZTMPWTW (IJU,IKU))
  ALLOCATE(ZTMPZ (IJU,IKU))
  ALLOCATE(ZTMPNDW (IJU,IKU))
  ALLOCATE(ZALPWEST (IJU,IKU))
  ALLOCATE(ZTMPFUNW (IJU,IKU))
  ALLOCATE(ZTMPFVTW (IJU,IKU))
  ALLOCATE(ZTMPFWTW (IJU,IKU))
  ZTMPUNW =0.
  ZTMPVTW =0.
  ZTMPWTW =0.
  ZTMPZ   =0.
  ZTMPNDW =0.
  ZALPWEST=0.
  CALL GET_2DSLICE_ll(PPTABU,'Y',PMINW,ZTMPUNW(1:IJU,1:IKU), &
                      1,IJU,1,IKU,IINFO_ll)
  CALL GET_2DSLICE_ll(PPTABV,'Y',PMINW,ZTMPVTW(1:IJU,1:IKU), &
                      1,IJU,1,IKU,IINFO_ll)
  CALL GET_2DSLICE_ll(PPTABW,'Y',PMINW,ZTMPWTW(1:IJU,1:IKU), &
                      1,IJU,1,IKU,IINFO_ll)
  CALL GET_2DSLICE_ll(PDZZ,'Y',PMINW,ZTMPZ(1:IJU,1:IKU), &
                      1,IJU,1,IKU,IINFO_ll)
  CALL GET_2DSLICE_ll(ZND,'Y',1+JPHEXT,ZTMPNDW(1:IJU,1:IKU), &
                      1,IJU,1,IKU,IINFO_ll)
  !
  ! *** Mean and fluctuations calculation
  !
  IF(ONR_COUNT<=NTMOY.AND.MOD(ONR_COUNT,NTMOYCOUNT)==0) THEN
     ICOUNT=ONR_COUNT/NTMOYCOUNT
     XUMEANW(:,:,ICOUNT)=ZTMPUNW(:,:)
     XVMEANW(:,:,ICOUNT)=ZTMPVTW(:,:)
     XWMEANW(:,:,ICOUNT)=ZTMPWTW(:,:)
  ENDIF
  IF(ONR_COUNT>NTMOY.AND.MOD(ONR_COUNT,NTMOYCOUNT)==0.AND.OPT_COUNT/=1) THEN
     DO JCOUNT=1,NNUMBELT-1
        XUMEANW(:,:,JCOUNT)=XUMEANW(:,:,JCOUNT+1)
        XVMEANW(:,:,JCOUNT)=XVMEANW(:,:,JCOUNT+1)
        XWMEANW(:,:,JCOUNT)=XWMEANW(:,:,JCOUNT+1)
     ENDDO
     XUMEANW(:,:,NNUMBELT)=ZTMPUNW(:,:)
     XVMEANW(:,:,NNUMBELT)=ZTMPVTW(:,:)
     XWMEANW(:,:,NNUMBELT)=ZTMPWTW(:,:)
   ENDIF
   IF (LWEST_ll( )) THEN
     DO JJ = 1,IJU-1
        DO JK = 1,IKU-1
           IF (ZTMPNDW(JJ,JK)>XTBVTOP) THEN
              ZALPWEST(JJ,JK)=1.
           ELSE IF (ZTMPNDW(JJ,JK)<XTBVBOT) THEN
              ZALPWEST(JJ,JK)=0.
           ELSE
              ZALPWEST(JJ,JK)=1./ABS(XTBVTOP-XTBVBOT)*ABS(ZTMPNDW(JJ,JK)-XTBVBOT)*1.
           ENDIF
        ENDDO
     ENDDO
     IF(NR_COUNT>NTMOY) THEN
       ZTMPFUNW =ZTMPUNW(:,:)-(SUM(XUMEANW,DIM=3)/NNUMBELT)
       ZTMPFVTW =ZTMPVTW(:,:)-(SUM(XVMEANW,DIM=3)/NNUMBELT)
       ZTMPFWTW =ZTMPWTW(:,:)-(SUM(XWMEANW,DIM=3)/NNUMBELT)
       PFLUCTUNW(1+JPHEXT:IJU-JPHEXT,1+JPVEXT:IKU-JPVEXT)= ZTMPFUNW(1+JPHEXT:IJU-JPHEXT,1+JPVEXT:IKU-JPVEXT)* &
                                                           ZALPWEST(1+JPHEXT:IJU-JPHEXT,1+JPVEXT:IKU-JPVEXT)
       PFLUCTVTW(1+JPHEXT:IJU-JPHEXT,1+JPVEXT:IKU-JPVEXT)= ZTMPFVTW(1+JPHEXT:IJU-JPHEXT,1+JPVEXT:IKU-JPVEXT)* &
                                                           ZALPWEST(1+JPHEXT:IJU-JPHEXT,1+JPVEXT:IKU-JPVEXT)
       PFLUCTWTW(1+JPHEXT:IJU-JPHEXT,1+JPVEXT:IKU-JPVEXT)= ZTMPFWTW(1+JPHEXT:IJU-JPHEXT,1+JPVEXT:IKU-JPVEXT)* &
                                                           ZALPWEST(1+JPHEXT:IJU-JPHEXT,1+JPVEXT:IKU-JPVEXT)
     ENDIF
   ENDIF
   DEALLOCATE(ZTMPUNW,ZTMPVTW,ZTMPWTW,ZTMPZ,ZTMPNDW,ZALPWEST,ZTMPFUNW,ZTMPFVTW,ZTMPFWTW)
ENDIF

IF (LRECYCLN) THEN
  !-------------------------------------------------------
  !-----------NORTH
  !------------------------------------------------------
  ALLOCATE(ZTMPUTN (IIU,IKU))
  ALLOCATE(ZTMPVNN (IIU,IKU))
  ALLOCATE(ZTMPWTN (IIU,IKU))
  ALLOCATE(ZTMPZ (IIU,IKU))
  ALLOCATE(ZTMPNDN (IIU,IKU))
  ALLOCATE(ZALPNORTH (IIU,IKU))
  ALLOCATE(ZTMPFUTN (IIU,IKU))
  ALLOCATE(ZTMPFVNN (IIU,IKU))
  ALLOCATE(ZTMPFWTN (IIU,IKU))
  ZTMPUTN =0.
  ZTMPVNN =0.
  ZTMPWTN =0.
  ZTMPZ   =0.
  ZTMPNDN =0.
  ZALPNORTH=0.
  CALL GET_2DSLICE_ll(PPTABU,'X',PMINN,ZTMPUTN(1:IIU,1:IKU), &
                      1,IIU,1,IKU,IINFO_ll)
  CALL GET_2DSLICE_ll(PPTABV,'X',PMINN,ZTMPVNN(1:IIU,1:IKU), &
                      1,IIU,1,IKU,IINFO_ll)
  CALL GET_2DSLICE_ll(PPTABW,'X',PMINN,ZTMPWTN(1:IIU,1:IKU), &
                      1,IIU,1,IKU,IINFO_ll)
  CALL GET_2DSLICE_ll(PDZZ,'X',PMINN,ZTMPZ(1:IIU,1:IKU), &
                      1,IIU,1,IKU,IINFO_ll)
  CALL GET_2DSLICE_ll(ZND,'X',IJMAX_ll+JPHEXT,ZTMPNDN(1:IIU,1:IKU), &
                      1,IIU,1,IKU,IINFO_ll)
  !
  ! *** Mean and fluctuations calculation
  !
  IF(ONR_COUNT<=NTMOY.AND.MOD(ONR_COUNT,NTMOYCOUNT)==0) THEN
     ICOUNT=ONR_COUNT/NTMOYCOUNT
     XUMEANN(:,:,ICOUNT)=ZTMPUTN(:,:)
     XVMEANN(:,:,ICOUNT)=ZTMPVNN(:,:)
     XWMEANN(:,:,ICOUNT)=ZTMPWTN(:,:)
  ENDIF
  IF(ONR_COUNT>NTMOY.AND.MOD(ONR_COUNT,NTMOYCOUNT)==0.AND.OPT_COUNT/=1) THEN
     DO JCOUNT=1,NNUMBELT-1
        XUMEANN(:,:,JCOUNT)=XUMEANN(:,:,JCOUNT+1)
        XVMEANN(:,:,JCOUNT)=XVMEANN(:,:,JCOUNT+1)
        XWMEANN(:,:,JCOUNT)=XWMEANN(:,:,JCOUNT+1)
     ENDDO
     XUMEANN(:,:,NNUMBELT)=ZTMPUTN(:,:)
     XVMEANN(:,:,NNUMBELT)=ZTMPVNN(:,:)
     XWMEANN(:,:,NNUMBELT)=ZTMPWTN(:,:)
  ENDIF

  IF (LNORTH_ll( )) THEN
     DO JJ = 1,IIU-1
        DO JK = 1,IKU-1
           IF (ZTMPNDN(JJ,JK)>XTBVTOP) THEN
              ZALPNORTH(JJ,JK)=1.
           ELSE IF (ZTMPNDN(JJ,JK)<XTBVBOT) THEN
              ZALPNORTH(JJ,JK)=0.
           ELSE
              ZALPNORTH(JJ,JK)=1./(XTBVTOP-XTBVBOT)*(ZTMPNDN(JJ,JK)-XTBVBOT)*1.
           ENDIF
        ENDDO
     ENDDO
     IF(NR_COUNT>NTMOY) THEN
        ZTMPFUTN =ZTMPUTN(:,:)-(SUM(XUMEANN,DIM=3)/NNUMBELT)
        ZTMPFVNN =ZTMPVNN(:,:)-(SUM(XVMEANN,DIM=3)/NNUMBELT)
        ZTMPFWTN =ZTMPWTN(:,:)-(SUM(XWMEANN,DIM=3)/NNUMBELT)
        PFLUCTVNN(1+JPHEXT:IIU-JPHEXT,1+JPVEXT:IKU-JPVEXT)= ZTMPFVNN(1+JPHEXT:IIU-JPHEXT,1+JPVEXT:IKU-JPVEXT)* &
                                                            ZALPNORTH(1+JPHEXT:IIU-JPHEXT,1+JPVEXT:IKU-JPVEXT)
        PFLUCTUTN(1+JPHEXT:IIU-JPHEXT,1+JPVEXT:IKU-JPVEXT)= ZTMPFUTN(1+JPHEXT:IIU-JPHEXT,1+JPVEXT:IKU-JPVEXT)* &
                                                            ZALPNORTH(1+JPHEXT:IIU-JPHEXT,1+JPVEXT:IKU-JPVEXT)
        PFLUCTWTN(1+JPHEXT:IIU-JPHEXT,1+JPVEXT:IKU-JPVEXT)= ZTMPFWTN(1+JPHEXT:IIU-JPHEXT,1+JPVEXT:IKU-JPVEXT)* &
                                                            ZALPNORTH(1+JPHEXT:IIU-JPHEXT,1+JPVEXT:IKU-JPVEXT)
     ENDIF
  ENDIF
  DEALLOCATE(ZTMPVNN,ZTMPUTN,ZTMPWTN,ZTMPZ,ZTMPNDN,ZALPNORTH,ZTMPFVNN,ZTMPFUTN,ZTMPFWTN)
ENDIF

IF (LRECYCLE) THEN
  !-------------------------------------------------------
  !-----------EAST
  !------------------------------------------------------
  ALLOCATE(ZTMPUNE (IJU,IKU))
  ALLOCATE(ZTMPVTE (IJU,IKU))
  ALLOCATE(ZTMPWTE (IJU,IKU))
  ALLOCATE(ZTMPZ (IJU,IKU))
  ALLOCATE(ZTMPNDE (IJU,IKU))
  ALLOCATE(ZALPEAST (IJU,IKU))
  ALLOCATE(ZTMPFUNE (IJU,IKU))
  ALLOCATE(ZTMPFVTE (IJU,IKU))
  ALLOCATE(ZTMPFWTE (IJU,IKU))
  ZTMPUNE =0.
  ZTMPVTE =0.
  ZTMPWTE =0.
  ZTMPZ   =0.
  ZTMPNDE =0.
  ZALPEAST=0.
  CALL GET_2DSLICE_ll(PPTABU,'Y',PMINE,ZTMPUNE(1:IJU,1:IKU), &
                      1,IJU,1,IKU,IINFO_ll)
  CALL GET_2DSLICE_ll(PPTABV,'Y',PMINE,ZTMPVTE(1:IJU,1:IKU), &
                      1,IJU,1,IKU,IINFO_ll)
  CALL GET_2DSLICE_ll(PPTABW,'Y',PMINE,ZTMPWTE(1:IJU,1:IKU), &
                      1,IJU,1,IKU,IINFO_ll)
  CALL GET_2DSLICE_ll(PDZZ,'Y',PMINE,ZTMPZ(1:IJU,1:IKU), &
                      1,IJU,1,IKU,IINFO_ll)
  CALL GET_2DSLICE_ll(ZND,'Y',IIMAX_ll+JPHEXT,ZTMPNDE(1:IJU,1:IKU), &
                      1,IJU,1,IKU,IINFO_ll)
  !
  ! *** Mean and fluctuations calculation
  !
  IF(ONR_COUNT<=NTMOY.AND.MOD(ONR_COUNT,NTMOYCOUNT)==0) THEN
     ICOUNT=ONR_COUNT/NTMOYCOUNT
     XUMEANE(:,:,ICOUNT)=ZTMPUNE(:,:)
     XVMEANE(:,:,ICOUNT)=ZTMPVTE(:,:)
     XWMEANE(:,:,ICOUNT)=ZTMPWTE(:,:)
  ENDIF
  IF(ONR_COUNT>NTMOY.AND.MOD(ONR_COUNT,NTMOYCOUNT)==0.AND.OPT_COUNT/=1) THEN
     DO JCOUNT=1,NNUMBELT-1
        XUMEANE(:,:,JCOUNT)=XUMEANE(:,:,JCOUNT+1)
        XVMEANE(:,:,JCOUNT)=XVMEANE(:,:,JCOUNT+1)
        XWMEANE(:,:,JCOUNT)=XWMEANE(:,:,JCOUNT+1)
     ENDDO
     XUMEANE(:,:,NNUMBELT)=ZTMPUNE(:,:)
     XVMEANE(:,:,NNUMBELT)=ZTMPVTE(:,:)
     XWMEANE(:,:,NNUMBELT)=ZTMPWTE(:,:)
  ENDIF
  IF (LEAST_ll( )) THEN
     DO JJ = 1,IJU-1
        DO JK = 1,IKU-1
           IF (ZTMPNDE(JJ,JK)>XTBVTOP) THEN
              ZALPEAST(JJ,JK)=1.
           ELSE IF (ZTMPNDE(JJ,JK)<XTBVBOT) THEN
              ZALPEAST(JJ,JK)=0.
           ELSE
              ZALPEAST(JJ,JK)=1./ABS(XTBVTOP-XTBVBOT)*ABS(ZTMPNDE(JJ,JK)-XTBVBOT)*1.
           ENDIF
        ENDDO
     ENDDO
     IF(NR_COUNT>NTMOY) THEN
       ZTMPFUNE =ZTMPUNE(:,:)-(SUM(XUMEANE,DIM=3)/NNUMBELT)
       ZTMPFVTE =ZTMPVTE(:,:)-(SUM(XVMEANE,DIM=3)/NNUMBELT)
       ZTMPFWTE =ZTMPWTE(:,:)-(SUM(XWMEANE,DIM=3)/NNUMBELT)
       PFLUCTUNE(1+JPHEXT:IJU-JPHEXT,1+JPVEXT:IKU-JPVEXT)= ZTMPFUNE(1+JPHEXT:IJU-JPHEXT,1+JPVEXT:IKU-JPVEXT)* &
                                                           ZALPEAST(1+JPHEXT:IJU-JPHEXT,1+JPVEXT:IKU-JPVEXT)
       PFLUCTVTE(1+JPHEXT:IJU-JPHEXT,1+JPVEXT:IKU-JPVEXT)= ZTMPFVTE(1+JPHEXT:IJU-JPHEXT,1+JPVEXT:IKU-JPVEXT)* &
                                                           ZALPEAST(1+JPHEXT:IJU-JPHEXT,1+JPVEXT:IKU-JPVEXT)
       PFLUCTWTE(1+JPHEXT:IJU-JPHEXT,1+JPVEXT:IKU-JPVEXT)= ZTMPFWTE(1+JPHEXT:IJU-JPHEXT,1+JPVEXT:IKU-JPVEXT)* &
                                                           ZALPEAST(1+JPHEXT:IJU-JPHEXT,1+JPVEXT:IKU-JPVEXT)
     ENDIF
   ENDIF
   DEALLOCATE(ZTMPUNE,ZTMPVTE,ZTMPWTE,ZTMPZ,ZTMPNDE,ZALPEAST,ZTMPFUNE,ZTMPFVTE,ZTMPFWTE)
ENDIF
!
IF (LRECYCLS) THEN
  !-------------------------------------------------------
  !-----------SOUTH
  !------------------------------------------------------
  ALLOCATE(ZTMPUTS (IIU,IKU))
  ALLOCATE(ZTMPVNS (IIU,IKU))
  ALLOCATE(ZTMPWTS (IIU,IKU))
  ALLOCATE(ZTMPZ (IIU,IKU))
  ALLOCATE(ZTMPNDS (IIU,IKU))
  ALLOCATE(ZALPSOUTH (IIU,IKU))
  ALLOCATE(ZTMPFUTS (IIU,IKU))
  ALLOCATE(ZTMPFVNS (IIU,IKU))
  ALLOCATE(ZTMPFWTS (IIU,IKU))
  ZTMPUTS =0.
  ZTMPVNS =0.
  ZTMPWTS =0.
  ZTMPZ   =0.
  ZTMPNDS =0.
  ZALPSOUTH=0.
  CALL GET_2DSLICE_ll(PPTABU,'X',PMINS,ZTMPUTS(1:IIU,1:IKU), &
                      1,IIU,1,IKU,IINFO_ll)
  CALL GET_2DSLICE_ll(PPTABV,'X',PMINS,ZTMPVNS(1:IIU,1:IKU), &
                      1,IIU,1,IKU,IINFO_ll)
  CALL GET_2DSLICE_ll(PPTABW,'X',PMINS,ZTMPWTS(1:IIU,1:IKU), &
                      1,IIU,1,IKU,IINFO_ll)
  CALL GET_2DSLICE_ll(PDZZ,'X',PMINS,ZTMPZ(1:IIU,1:IKU), &
                      1,IIU,1,IKU,IINFO_ll)
  CALL GET_2DSLICE_ll(ZND,'X',1+JPHEXT,ZTMPNDS(1:IIU,1:IKU), &
                      1,IIU,1,IKU,IINFO_ll)
  !
  ! *** Mean and fluctuations calculation
  !
  IF(ONR_COUNT<=NTMOY.AND.MOD(ONR_COUNT,NTMOYCOUNT)==0) THEN
     ICOUNT=ONR_COUNT/NTMOYCOUNT
     XUMEANS(:,:,ICOUNT)=ZTMPUTS(:,:)
     XVMEANS(:,:,ICOUNT)=ZTMPVNS(:,:)
     XWMEANS(:,:,ICOUNT)=ZTMPWTS(:,:)
  ENDIF
  IF(ONR_COUNT>NTMOY.AND.MOD(ONR_COUNT,NTMOYCOUNT)==0.AND.OPT_COUNT/=1) THEN
     DO JCOUNT=1,NNUMBELT-1
        XUMEANS(:,:,JCOUNT)=XUMEANS(:,:,JCOUNT+1)
        XVMEANS(:,:,JCOUNT)=XVMEANS(:,:,JCOUNT+1)
        XWMEANS(:,:,JCOUNT)=XWMEANS(:,:,JCOUNT+1)
     ENDDO
     XUMEANS(:,:,NNUMBELT)=ZTMPUTS(:,:)
     XVMEANS(:,:,NNUMBELT)=ZTMPVNS(:,:)
     XWMEANS(:,:,NNUMBELT)=ZTMPWTS(:,:)
  ENDIF
  IF (LSOUTH_ll( )) THEN
     DO JJ = 1,IIU-1
        DO JK = 1,IKU-1
           IF (ZTMPNDS(JJ,JK)>XTBVTOP) THEN
              ZALPSOUTH(JJ,JK)=1.
           ELSE IF (ZTMPNDS(JJ,JK)<XTBVBOT) THEN
              ZALPSOUTH(JJ,JK)=0.
           ELSE
              ZALPSOUTH(JJ,JK)=1./(XTBVTOP-XTBVBOT)*(ZTMPNDS(JJ,JK)-XTBVBOT)*1.
           ENDIF
        ENDDO
     ENDDO
     IF(NR_COUNT>NTMOY) THEN
        ZTMPFUTS =ZTMPUTS(:,:)-(SUM(XUMEANS,DIM=3)/NNUMBELT)
        ZTMPFVNS =ZTMPVNS(:,:)-(SUM(XVMEANS,DIM=3)/NNUMBELT)
        ZTMPFWTS =ZTMPWTS(:,:)-(SUM(XWMEANS,DIM=3)/NNUMBELT)
        PFLUCTVNS(1+JPHEXT:IIU-JPHEXT,1+JPVEXT:IKU-JPVEXT)= ZTMPFVNS(1+JPHEXT:IIU-JPHEXT,1+JPVEXT:IKU-JPVEXT)* &
                                                            ZALPSOUTH(1+JPHEXT:IIU-JPHEXT,1+JPVEXT:IKU-JPVEXT)
        PFLUCTUTS(1+JPHEXT:IIU-JPHEXT,1+JPVEXT:IKU-JPVEXT)= ZTMPFUTS(1+JPHEXT:IIU-JPHEXT,1+JPVEXT:IKU-JPVEXT)* &
                                                            ZALPSOUTH(1+JPHEXT:IIU-JPHEXT,1+JPVEXT:IKU-JPVEXT)
        PFLUCTWTS(1+JPHEXT:IIU-JPHEXT,1+JPVEXT:IKU-JPVEXT)= ZTMPFWTS(1+JPHEXT:IIU-JPHEXT,1+JPVEXT:IKU-JPVEXT)* &
                                                            ZALPSOUTH(1+JPHEXT:IIU-JPHEXT,1+JPVEXT:IKU-JPVEXT)
     ENDIF
  ENDIF
  DEALLOCATE(ZTMPVNS,ZTMPUTS,ZTMPWTS,ZTMPZ,ZTMPNDS,ZALPSOUTH,ZTMPFVNS,ZTMPFUTS,ZTMPFWTS)
ENDIF

DEALLOCATE(ZWORK32,ZND)

RETURN

END SUBROUTINE RECYCL_FLUC
