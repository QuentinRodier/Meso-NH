MODULE MODD_MEGAN
!
INTEGER, PARAMETER :: NLAYERS = 5
! LENGTH OF THE TIME STEP  (DAYS)
INTEGER, PARAMETER :: NTSTLEN = 30
INTEGER, PARAMETER :: NMAXSTYPES = 11
!
REAL, PARAMETER ::  XSOLARCONSTANT = 1367,   &     ! SOLAR CONSTANT [W/M2]
     XWATERAIRRATIO = 18.016/28.97  ! RATIO BETWEEN WATER AND AIR MOLECULES
!
REAL, PARAMETER :: XPSTD_SUN=200.0, XPSTD_SHADE=50.0
REAL ,PARAMETER :: XCCE=0.56
!
REAL,PARAMETER :: XSB = 0.0000000567
!
!     REAL,PARAMETER :: CONVERTPPFD = 4.766   
REAL,PARAMETER :: XCONVERTSHADEPPFD = 4.6
REAL,PARAMETER :: XCONVERTSUNPPFD = 4.0
!  
REAL,PARAMETER :: XPI = 3.14159, XRPI180 = 57.29578
!
REAL,PARAMETER :: XDIHIGH = -0.5, XDILOW = -5
!
REAL,PARAMETER :: XCTM2 = 230
REAL,PARAMETER :: XCT2 =200.0  
!
REAL,PARAMETER :: XTS = 303.15
!
! PARAMETER FOR UNIT CONVERSION
REAL, PARAMETER :: XUG2TONNE = 1E-12  ! CONVERT MICROGRAM TO METRIC TONNE
REAL, PARAMETER :: XHR2SEC = 3600     ! CONVERT HR TO SECOND
REAL, PARAMETER :: XUG2G = 1E-6       ! CONVERT MICROGRAM TO GRAM
REAL, PARAMETER :: XN2NO    = 2.142857   ! CONVERT HR TO SECOND
!
REAL, DIMENSION(NMAXSTYPES) :: XSATURATION=&
        (/0.395, 0.410, 0.435, 0.485, 0.451, 0.420, 0.477, 0.476, 0.426, 0.482, 0.482/)
!
REAL, PARAMETER :: XISMAX=1.344, XH=1.4614
REAL, PARAMETER :: XCSTAR=585
!=======================================================================
!  CANOPY.EXT
!  THIS INCLUDE FILE CONTAINS MEGAN SPECIES
!
!  WHO                   WHEN       WHAT
!  ---------------------------------------------------------------------
!  XUEMEI WANG          06/16/2009 - CREATES THIS FILE
!=======================================================================

INTEGER, PARAMETER :: N_MGN_SPC  = 20

CHARACTER(LEN=6), DIMENSION(N_MGN_SPC) :: &
        CMGN_SPC=(/'ISOP  ','MYRC  ','SABI  ','LIMO  ','A_3CAR','OCIM  ','BPIN  ','APIN  ','OMTP  ',&
                  'FARN  ','BCAR  ','OSQT  ','MBO   ','MEOH  ','ACTO  ','CO    ','NO    ','BIDER ',&
                  'STRESS','OTHER '/)

REAL, DIMENSION(N_MGN_SPC), PARAMETER :: &
        XCLEO=(/2.,1.83,1.83,1.83,1.83,1.83,1.83,1.83,1.83,2.37,2.37,2.37,2.,1.6,1.83,1.6,1.86,2.,1.83,1.83/)

REAL, DIMENSION(N_MGN_SPC), PARAMETER :: &
        XCTM1=(/95.,80.,80.,80.,80.,80.,80.,80.,80.,130.,130.,130.,95.,60.,80.,60.,80.,95.,80.,80./)

REAL, DIMENSION(N_MGN_SPC), PARAMETER :: &
        XTDF_PRM=(/0.13,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.17,0.17,0.17,0.13,0.08,0.1,0.08,0.1,0.13,0.1,0.1/)

REAL, DIMENSION(N_MGN_SPC), PARAMETER :: &
        XLDF_FCT=(/0.999,0.6,0.6,0.4,0.4,0.4,0.4,0.6,0.4,0.5,0.5,0.5,0.999,0.8,0.2,0.999,0.,0.8,0.8,0.2/)

REAL, DIMENSION(N_MGN_SPC), PARAMETER :: &
        XMGN_MWT=(/1.,1.,1.,1.,1.,1.,1.,1.,1.,1.,1.,1.,1.,1.,1.,1.,1.,1.,1.,1./)
!
INTEGER, DIMENSION(N_MGN_SPC), PARAMETER :: &
        NREA_INDEX=(/5,2,2,2,2,2,2,2,2,3,3,3,5,4,1,1,1,1,1,1/)
!
!**************************************************************************************************************
!
INTEGER,PARAMETER :: N_MGN_PFT = 16
!
!CHARACTER(LEN=10), DIMENSION(N_MGN_PFT), PARAMETER :: &
!        CMGN_PFT=(/'NT_EG_TEMP','NT_DC_BORL','NT_EG_BORL','BT_EG_TROP','BT_EG_TEMP','BT_DC_TROP',&
!                   'BT_DC_TEMP','BT_DC_BORL','SG_EG_TEMP','SB_DC_TEMP','SB_DC_BORL',&
!                   'GS_C3_COLD','GS_C3_COOL','GS_C3_WARM','CORN      ','CROP      '/)
!
!CHARACTER(LEN=35), DIMENSION(N_MGN_PFT), PARAMETER :: &
!        CMGN_NAM=(/'Needleaf evergreen temperate tree  ','Needleaf deciduous boreal tree     ',&
!                   'Needleaf evergreen boreal tree     ','Broadleaf evergreen tropical tree  ',&
!                   'Broadleaf evergreen tropical tree  ','Broadleaf deciduous tropical tree  ',&
!                   'Broadleaf deciduous temperate tree ','Broadleaf deciduous boreal tree    ',&
!                   'Broadleaf evergreen temperate shrub','Broadleaf deciduous temperate shrub',&
!                   'Broadleaf deciduous boreal shrub   ','Cold C3 grass                      ',&
!                   'Cool C3 grass                      ','Warm C3 grass                      ',&
!                   'Corn                               ','Other crops                        '/)
!
INTEGER,PARAMETER :: N_CAT = 5
!
REAL, DIMENSION(N_CAT) :: XANEW=(/1.,2.  ,0.4 ,3.5,0.05/)
REAL, DIMENSION(N_CAT) :: XAGRO=(/1.,1.8 ,0.6 ,3. ,0.6 /)
REAL, DIMENSION(N_CAT) :: XAMAT=(/1.,1.  ,1.  ,1. ,1.  /)
REAL, DIMENSION(N_CAT) :: XAOLD=(/1.,1.05,0.95,1.2,0.9 /)
!
!**********************************************************************************************************

INTEGER, PARAMETER ::  NRCHA = 16
! 1  = canopy depth
! 2  = leaf width
! 3  = leaf length
! 4  = canopy height
! 5  = scattering coefficient for PPFD
! 6  = scattering coefficient for near IR
! 7  = reflection coefficient for diffuse PPFD
! 8  = reflection coefficient for diffuse near IR
! 9  = clustering coefficient (accounts for leaf clumping influence on mean
!    projected leaf area in the direction of the suns beam)
!    use 0.85 for default, corn=0.4-0.9; Pine=0.6-1.0; oak=0.53-0.67; 
!    tropical rainforest=1.1
! 10 = leaf IR emissivity
! 11 = leaf stomata and cuticle factor: 1=hypostomatous, 2=amphistomatous,
!     1.25=hypostomatous but with some transpiration through cuticle
! 12 = daytime temperature lapse rate (K m-1)
! 13 = nighttime temperature lapse rate (K m-1)
! 14 = warm (>283K) canopy total humidity change (Pa)
! 15 = cool (>= 283K) canopy total humidity change (Pa)
! 16 = normalized canopy depth where wind is negligible
!     NT NT NT TF BT TF BT BT SB SB SB HB HB HB CR CR

REAL,DIMENSION(NRCHA,N_MGN_PFT) :: XCANOPYCHAR = RESHAPE(&
   (/ 16.,   16.,   16.,   16.,   16.,   16.,   16.,   16.,    1.,    1.,    1., 0.756, 0.756, 0.756,    1.,    1., & 
     0.05,  0.05,  0.05,  0.05,  0.05,  0.05,  0.05,  0.05, 0.015, 0.015, 0.015, 0.015, 0.015, 0.015,  0.02,  0.02, &
      0.1,   0.1,   0.1,   0.1,   0.1,   0.1,   0.1,   0.1,   0.1,   0.1,   0.1,  0.15,  0.15,  0.15,  0.15,  0.15, &
      24.,   24.,   24.,   24.,   24.,   24.,   24.,   24.,    2.,    2.,    2.,  0.75,  0.75,  0.75,    1.,    1., & 
      0.2,   0.2,   0.2,   0.2,   0.2,   0.2,   0.2,   0.2,   0.2,   0.2,   0.2,   0.2,   0.2,   0.2,   0.2,   0.2, &
      0.8,   0.8,   0.8,   0.8,   0.8,   0.8,   0.8,   0.8,   0.8,   0.8,   0.8,   0.8,   0.8,   0.8,   0.8,   0.8, &
    0.057, 0.057, 0.057, 0.057, 0.057, 0.057, 0.057, 0.057, 0.057, 0.057, 0.057, 0.057, 0.057, 0.057, 0.057, 0.057, &
    0.389, 0.389, 0.389, 0.389, 0.389, 0.389, 0.389, 0.389, 0.389, 0.389, 0.389, 0.389, 0.389, 0.389, 0.389, 0.389, &
     0.85,  0.85,  0.85,   1.1,  0.95,   1.1,  0.95,  0.95,  0.85,  0.85,  0.85,  0.76,  0.76,  0.76,  0.65,  0.65, &
     0.95,  0.95,  0.95,  0.95,  0.95,  0.95,  0.95,  0.95,  0.95,  0.95,  0.95,  0.95,  0.95,  0.95,  0.95,  0.95, &
     1.25,  1.25,  1.25,  1.25,  1.25,  1.25,  1.25,  1.25,  1.00,  1.00,  1.00,  1.25,  1.25,  1.25,  1.25,  1.25, &
     0.06,  0.06,  0.06,  0.06,  0.06,  0.06,  0.06,  0.06,  0.06,  0.06,  0.06,  0.06,  0.06,  0.06,  0.06,  0.06, &
    -0.06, -0.06, -0.06, -0.06, -0.06, -0.06, -0.06, -0.06, -0.06, -0.06, -0.06, -0.06, -0.06, -0.06, -0.06, -0.06, &
     700.,  700.,  700.,  700.,  700.,  700.,  700.,  700.,  700.,  700.,  700.,  700.,  700.,  700.,  700.,  700., &
     150.,  150.,  150.,  150.,  150.,  150.,  150.,  150.,  150.,  150.,  150.,  150.,  150.,  150.,  150.,  150., &
      0.7,   0.7,   0.7,   0.7,   0.7,   0.7,   0.7,   0.7,   0.7,   0.7,   0.7,   0.7,   0.7,   0.7,   0.7,   0.7/)&
        ,SHAPE=(/NRCHA,N_MGN_PFT/) ,ORDER=(/2,1/) )


END MODULE MODD_MEGAN
