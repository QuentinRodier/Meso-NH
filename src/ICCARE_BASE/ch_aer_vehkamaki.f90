!ORILAM_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!ORILAM_LIC This is part of the ORILAM software governed by the CeCILL-C licence
!ORILAM_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!ORILAM_LIC for details.
!-----------------------------------------------------------------
!--------------- special set of characters for RCS information
!-----------------------------------------------------------------
! $Source: /home/cvsroot/MNH-VX-Y-Z/src/MNH/ch_aer_nucl.f90,v $ $Revision: 1.1.4.1.18.1 $
! MASDEV4_7 chimie 2006/05/18 13:07:25
!-----------------------------------------------------------------
!!    ################################ 
MODULE MODI_CH_AER_VEHKAMAKI
!!    ################################ 
!!
INTERFACE
  !!
  SUBROUTINE CH_AER_VEHKAMAKI(PRH,PTEMP,PSULF,PJNUC,PRC)
  IMPLICIT NONE
  !!
  REAL,    DIMENSION(:), INTENT(IN)    :: PRH, PTEMP, PSULF          
  REAL,    DIMENSION(:), INTENT(INOUT) :: PJNUC, PRC
  !!
  END SUBROUTINE CH_AER_VEHKAMAKI
  !!
END INTERFACE
!!
END MODULE MODI_CH_AER_VEHKAMAKI
!!
!! #########################################################################
SUBROUTINE CH_AER_VEHKAMAKI(PRH,PTEMP,PSULF,PJNUC,PRC)       
!! #########################################################################
!!                   
!!    PURPOSE
!!    -------
!!      
!!    Compute nucleation rate for binary sulfate/H2O
!!    This is the Vhekamaki parametrization (2002)
!!
!!    Valid for : 
!!    230.15 < T < 305.15     (K)
!!    0.01 < RH < 100          (%)
!!    10⁴ < [H2SO4]gas < 10¹¹  (molec/cm3)
!!
!!
!!    AUTHOR
!!    ------
!!    B. Foucart (18/06/2018) 
!!
!!    MODIFICATIONS
!!    -------------   
!!
!----------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
USE MODD_CONF      , ONLY : NVERB
USE MODD_CH_AEROSOL
!
!
IMPLICIT NONE
!
!*       0.1   Declarations of arguments :
!
REAL, DIMENSION(:), INTENT(IN)    :: PRH,PTEMP, PSULF     
REAL, DIMENSION(:), INTENT(INOUT) :: PJNUC, PRC                ! Nucleation rate (#/cm3/s) , Radius of the critical cluster (nm)
!
!*       0.2   Declarations of local variables :
!
REAL, DIMENSION(SIZE(PSULF,1)) :: ZCOJA,ZCOJB,ZCOJC,ZCOJD,ZCOJE,ZCOJF,ZCOJG,ZCOJH,ZCOJI,ZCOJJ
REAL, DIMENSION(SIZE(PSULF,1)) :: ZCOENA,ZCOENB,ZCOENC,ZCOEND,ZCOENE,ZCOENF,ZCOENG,ZCOENH,ZCOENI,ZCOENJ
REAL, DIMENSION(SIZE(PSULF,1)) :: ZSULF
REAL, DIMENSION(SIZE(PSULF,1)) :: ZNTOT,ZRC,ZAL
REAL, PARAMETER :: ZCSTAVOG = 6.0221367E+11             ! Avogadro number
INTEGER :: II, ITEST           
!
!----------------------------------------------------------------------------
!
! Parameters initialization 
!
ZSULF(:) = 1.E4    ! must vary between 10E4 and 10E11 
ZAL(:) = 0.17      ! must vary between 0.17 and 0.62 
PJNUC(:) = 1E-7       ! must vary between 10E-7 and 10E9 cm3.s-1
PRC(:) = 0.35      ! must vary between 0.35 and 0.92 nm
ZNTOT(:) = 10.     ! must vary between 4 and 70 molecules
ZCOJA(:) = 0.
ZCOJB(:) = 0.
ZCOJC(:) = 0.
ZCOJD(:) = 0.
ZCOJE(:) = 0.
ZCOJF(:) = 0.
ZCOJG(:) = 0.
ZCOJH(:) = 0.
ZCOJI(:) = 0.
ZCOJJ(:) = 0.
ZCOENA(:) = 0.
ZCOENB(:) = 0.
ZCOENC(:) = 0.
ZCOEND(:) = 0.
ZCOENE(:) = 0.
ZCOENF(:) = 0.
ZCOENG(:) = 0.
ZCOENH(:) = 0.
ZCOENI(:) = 0.
ZCOENJ(:) = 0.
!
!    **** Define a local variable for PSUFL that we convert in to molec/cm3 for calculations ****
!
!    a. Restrictions for nucleation

!
! ZSULF(:) = MAX(MIN(PSULF(:),1.E11), 0.)
!
  ZSULF(:) = PSULF(:)
!
!    b. ZSULF from ug/m3 to  molec/cm3 
! 
  ZSULF(:) = ZSULF(:)*ZCSTAVOG / XH2SO4 
!
!----------------------------------------------------------------------------
!
!!    **** START Vehkamaki calculations ****
!
ITEST = 0.
!
!  Conditions
!
WHERE ((ZSULF(:) > 1.E4 .AND. ZSULF(:) < 1.E11).AND.(PRH(:) > 0.01).AND.(PTEMP(:)>230.15))
!
!      1) Mole fraction of H2SO4 in the critical cluster (no unity)
!
ZAL(:) = 0.740997-0.00266379*PTEMP(:)-&
         0.00349998*LOG(ZSULF(:))+0.0000504022*PTEMP(:)*LOG(ZSULF(:))+&                     
         0.00201048*LOG(PRH(:))-0.000183289*PTEMP(:)*LOG(PRH(:))+&                                       
         0.00157407*(LOG(PRH(:)))**2-0.0000179059*PTEMP(:)*(LOG(PRH(:)))**2+&                                
         0.000184403*(LOG(PRH(:)))**3-1.50345E-6*PTEMP(:)*LOG(PRH(:))**3  
!
!      2) Coefficient calculations for the NUCLEATION RATE (function of temperature and mole fraction)
!
ZCOJA(:) = 0.14309+2.21956*PTEMP(:)-0.0273911*(PTEMP(:))**2+&
           0.0000722811*(PTEMP(:))**3+(5.91822/ZAL(:))
!
ZCOJB(:) = 0.117489+0.462532*PTEMP(:)-0.0118059*(PTEMP(:))**2+&
           0.0000404196*(PTEMP(:))**3+(15.7963/ZAL(:))
!
ZCOJC(:) = -0.21554-0.0810269*PTEMP(:)+0.001143581*(PTEMP(:))**2-&
           4.7758E-6*(PTEMP(:))**3-(2.91297/ZAL(:))           
!
ZCOJD(:) = -3.58856+0.049508*PTEMP(:)-0.00021382*(PTEMP(:))**2+&
           3.10801E-7*(PTEMP(:))**3-(0.0293333/ZAL(:))         
!
ZCOJE(:) = 1.14598-0.600796*PTEMP(:)+0.00864245*(PTEMP(:))**2-&
           0.0000228947*(PTEMP(:))**3-(8.44985/ZAL(:))
!
ZCOJF(:) = 2.15855+0.0808121*PTEMP(:)-0.000407382*(PTEMP(:))**2-&
           4.01957E-7*(PTEMP(:))**3+(0.721326/ZAL(:))         
!
ZCOJG(:) = 1.6241-0.0160106*PTEMP(:)+0.0000377124*(PTEMP(:))**2+&
           3.21794E-8*(PTEMP(:))**3-(0.0113255/ZAL(:))        
!
ZCOJH(:) = 9.71682-0.115048*PTEMP(:)+0.000157098*(PTEMP(:))**2+&
           4.00914E-7*(PTEMP(:))**3+(0.71186/ZAL(:))           
!
ZCOJI(:) = -1.05611+0.00903378*PTEMP(:)-0.0000198417*(PTEMP(:))**2+&
           2.46048E-8*(PTEMP(:))**3-(0.0579087/ZAL(:))     
!
ZCOJJ(:) = -0.148712+0.00283508*PTEMP(:)-9.24619E-6*(PTEMP(:))**2+&
           5.00427E-9*(PTEMP(:))**3-(0.0127081/ZAL(:))  
!
!      3) NUCLEATION RATE calculation (part.cm-3.s-1)
!
PJNUC(:) = EXP(ZCOJA(:)+ZCOJB(:)*LOG(PRH(:))+&
        ZCOJC(:)*(LOG(PRH(:)))**2+ZCOJD(:)*(LOG(PRH(:)))**3+&
        ZCOJE(:)*LOG(ZSULF(:))+ZCOJF(:)*LOG(PRH(:))*LOG(ZSULF(:))+&
        ZCOJG(:)*(LOG(PRH(:)))**2*LOG(ZSULF(:))+ZCOJH(:)*(LOG(ZSULF(:)))**2+&
        ZCOJI(:)*LOG(PRH(:))*(LOG(ZSULF(:)))**2+ZCOJJ(:)*(LOG(ZSULF(:)))**3)
!
!     4) Coefficient calculations for the MOLECULE NUMBER in the critical cluster (function of temperature and mole fraction)
!
ZCOENA(:) = -0.00295413-0.0976834*PTEMP(:)+0.00102485*(PTEMP(:))**2-2.18646E-6*(PTEMP(:))**3-(0.101717/ZAL(:))
!
ZCOENB(:) = -0.00205064-0.00758504*PTEMP(:)+0.000192654*(PTEMP(:))**2-6.7043E-7*(PTEMP(:))**3-(0.255774/ZAL(:))
!
ZCOENC(:) = 0.00322308+0.000852637*PTEMP(:)-0.0000154757*(PTEMP(:))**2+5.66661E-8*(PTEMP(:))**3+(0.0338444/ZAL(:))
!
ZCOEND(:) = 0.0474323-0.000625104*PTEMP(:)+2.65066E-6*(PTEMP(:))**2-3.67471E-9*(PTEMP(:))**3-(0.000267251/ZAL(:))
!
ZCOENE(:) = -0.0125211+0.00580655*PTEMP(:)-0.000101674*(PTEMP(:))**2+2.88195E-7*(PTEMP(:))**3+(0.0942243/ZAL(:))
!
ZCOENF(:) = -0.038546-0.000672316*PTEMP(:)+2.60288E-6*(PTEMP(:))**2+1.19416E-8*(PTEMP(:))**3-(0.00851515/ZAL(:))
!
ZCOENG(:) = -0.0183749+0.000172072*PTEMP(:)-3.71766E-7*(PTEMP(:))**2-5.14875E-10*(PTEMP(:))**3+(0.00026866/ZAL(:))
!
ZCOENH(:) = -0.0619974+0.000906958*PTEMP(:)-9.11728E-7*(PTEMP(:))**2-5.36796E-9*(PTEMP(:))**3-(0.00774234/ZAL(:))
!
ZCOENI(:) = 0.0121827-0.00010665*PTEMP(:)+2.5346E-7*(PTEMP(:))**2-3.63519E-10*(PTEMP(:))**3+(0.000610065/ZAL(:))
!
ZCOENJ(:) = 0.000320184-0.0000174762*PTEMP(:)+6.06504E-8*(PTEMP(:))**2-1.42177E-11*(PTEMP(:))**3+(0.000135751/ZAL(:))
!
!    5) MOLECULE NUMBER in the critical cluster calculation (should be between 4
!    and 70)
!
ZNTOT(:) = EXP(ZCOENA(:)+ZCOENB(:)*LOG(PRH(:))+ZCOENC(:)*(LOG(PRH(:)))**2+ZCOEND(:)*(LOG(PRH(:)))**3+&
           ZCOENE(:)*LOG(ZSULF(:))+ZCOENF(:)*LOG(PRH(:))*LOG(ZSULF(:))+ZCOENG(:)*(LOG(PRH(:)))**2*LOG(ZSULF(:))+&
           ZCOENH(:)*(LOG(ZSULF(:)))**2+ZCOENI(:)*LOG(PRH(:))*(LOG(ZSULF(:)))**2+ZCOENJ(:)*(LOG(ZSULF(:)))**3)
!
!    6) Cluster's radius in nm (should be between 0.35 and 0.92)
!
PRC(:) = EXP(-1.6524245 + 0.42316402 * ZAL(:) + 0.3346648 * LOG(ZNTOT(:)))
!
END WHERE

!
!
RETURN
END SUBROUTINE CH_AER_VEHKAMAKI
