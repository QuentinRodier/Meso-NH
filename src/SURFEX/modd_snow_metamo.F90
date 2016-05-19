! ajoutEB
! correction de l'erreur interversion de VTANG2 et VTANG3
!######################
      MODULE MODD_SNOW_METAMO
!     ######################
!
!!****  *MODD_SNOW_METAMO* - declaration of parameters related
!!                          to snow metamorphism!!
!!    PURPOSE
!!    -------
!       The purpose of this declarative module is to specify  the 
!     parameters related to the metamorphism parameterization of snow.
!
!!
!!**  IMPLICIT ARGUMENTS
!!    ------------------
!!      None 
!!
!!    REFERENCE
!!    ---------
!!      
!!
!!    AUTHOR
!!    ------
!!	V. Vionnet   *Meteo France*
!!
!!    MODIFICATIONS
!!    -------------
!!      Original       02/2008                
!-------------------------------------------------------------------------------
!
!*       0.   DECLARATIONS
!             ------------
!
IMPLICIT NONE
!
!-------------------------------------------------------------------------------
!
! Optical diameter properties
REAL, PARAMETER                 :: XDIAET=1.E-4
REAL, PARAMETER                 :: XDIAGF=3.E-4
REAL, PARAMETER                 :: XDIAFP=4.E-4

! 
! Maximum value for TPSNOW%GRAN2
REAL, PARAMETER                 :: VGRAN1=99.
REAL, PARAMETER                 :: XGRAN=99.

!
  INTEGER, PARAMETER            :: NVHIS1=1                
  INTEGER, PARAMETER            :: NVHIS2=2                
  INTEGER, PARAMETER            :: NVHIS3=3                
  INTEGER, PARAMETER            :: NVHIS4=4                
  INTEGER, PARAMETER            :: NVHIS5=5 

! Properties of fresh snow
REAL, PARAMETER                 :: XNDEN1=17.12
REAL, PARAMETER                 :: XNDEN2=128.
REAL, PARAMETER                 :: XNDEN3=-20.
REAL, PARAMETER                 :: XNSPH1=7.87
REAL, PARAMETER                 :: XNSPH2=38.
REAL, PARAMETER                 :: XNSPH3=50.
REAL, PARAMETER                 :: XNSPH4=90.
!
! 
REAL, PARAMETER                 :: UEPSI=1.E-8
REAL, PARAMETER                 :: XEPSI=1.E-8
REAL, PARAMETER                 :: UPOURC=100.

!!
! Parameters for Marbouty's function
!
REAL, PARAMETER                 ::  VTANG1=40.              
REAL, PARAMETER                 ::  VTANG2=6.              
REAL, PARAMETER                 ::  VTANG3=22.              
REAL, PARAMETER                 ::  VTANG4=.7               
REAL, PARAMETER                 ::  VTANG5=.3               
REAL, PARAMETER                 ::  VTANG6=6.               
REAL, PARAMETER                 ::  VTANG7=1.               
REAL, PARAMETER                 ::  VTANG8=.8               
REAL, PARAMETER                 ::  VTANG9=16.              
REAL, PARAMETER                 ::  VTANGA=.2               
REAL, PARAMETER                 ::  VTANGB=.2               
REAL, PARAMETER                 ::  VTANGC=18.             
REAL, PARAMETER                 ::  VRANG1=400.               
REAL, PARAMETER                 ::  VRANG2=150.              
REAL, PARAMETER                 ::  VGANG1=70.               
REAL, PARAMETER                 ::  VGANG2=25.              
REAL, PARAMETER                 ::  VGANG3=40.               
REAL, PARAMETER                 ::  VGANG4=50.               
REAL, PARAMETER                 ::  VGANG5=.1               
REAL, PARAMETER                 ::  VGANG6=15.              
REAL, PARAMETER                 ::  VGANG7=.1               
REAL, PARAMETER                 ::  VGANG8=.55              
REAL, PARAMETER                 ::  VGANG9=.65              
REAL, PARAMETER                 ::  VGANGA=.2               
REAL, PARAMETER                 ::  VGANGB=.85              
REAL, PARAMETER                 ::  VGANGC=.15      

! Parameters for snow metamorphism

REAL, PARAMETER                 ::  VDENT1=2314.81481
REAL, PARAMETER                 ::   VDENT2=7.2338E-7 
REAL, PARAMETER                 ::   VGRAN6=51.              
REAL, PARAMETER                 ::   VVAP1=-6000.            
REAL, PARAMETER                 ::   VVAP2=.4                
REAL, PARAMETER                 ::   VDIAM1=4.E-4               
REAL, PARAMETER                 ::   VDIAM2=5.E-4              
REAL, PARAMETER                 ::   VDIAM3=3.E-4               
REAL, PARAMETER                 ::   VDIAM4=2.E-4
REAL, PARAMETER                 ::   VDIAM5=1.E-4
REAL, PARAMETER                 ::   VDIAM6=1.E-4               
REAL, PARAMETER                 ::   VSPHE1=1.               
REAL, PARAMETER                 ::   VSPHE2=11574.074             
REAL, PARAMETER                 ::   VSPHE3=.5               
REAL, PARAMETER                 ::   VSPHE4=.1               
REAL, PARAMETER                 ::   VTAIL1=1.28E-17          
REAL, PARAMETER                 ::   VTAIL2=4.22E-19         
REAL, PARAMETER                 ::   VGRAT1=5.              
REAL, PARAMETER                 ::   VGRAT2=15.             
REAL, PARAMETER                 ::   VFI=1.0417E-9                 
REAL, PARAMETER                 ::   VTELV1=0.005 
INTEGER,PARAMETER               ::   NVDENT1=3
!
END MODULE MODD_SNOW_METAMO



