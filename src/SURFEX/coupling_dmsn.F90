SUBROUTINE COUPLING_DMS_n(KI,& !! number of sea points
         PWIND, & !! wind (m s-1)
         PSST,&  !! sea surface temperature (K)
         DMS_OCEANIC,& !! DMS oceanic content (mol m-3)
         PSFDMS)  !! DMS emssion flux (mol m-2 s-1)
! 
 implicit none

integer, intent(in)             :: KI          !! number of sea points
real,dimension(KI), intent(in)  :: PWIND       !! wind (m s-1)
real, dimension(KI), intent(in) :: PSST        !!  sea surface temperature (K)
real, dimension(KI), intent(in) :: DMS_OCEANIC !! DMS ocenanic content (mol m-3)
real,dimension(KI), intent(out) :: PSFDMS      !! DMS emission flux (mol m-2 s-1)

!!! local variables

real,dimension(KI) :: sc_dms         !! Schmidt number for DMS
real,parameter     :: sc_co2 = 600.  !! Schmidt number for CO2
real,dimension(KI) :: zsst            !! sea surface temperature (°C)
real,dimension(KI) :: k600           !! standard air-sea exchange coefficient for CO2 (m s-1)
real,dimension(KI) :: k_dms          !! air-sea exchange coefficient for DMS (m s-1)

! sea surface temperature (in °C) must be comprised between 5 and 30 °C
                
ZSST(:) = PSST(:) - 273.15
where (ZSST(:) < 5.)
       ZSST(:) = 5.
endwhere  

where (ZSST(:) > 30.)
       ZSST(:) = 30.
endwhere     


! Schmidt number for DMS, using the sst in celsius, from
! Saltzman et al., 1993 (without unit)

sc_dms(:) = 2674.0 - (147.12*ZSST(:)) + (3.726*(ZSST(:)**2.0)) - (0.038*(ZSST(:)**3.0))

! k600: Sea - air exchange coefficient from Nightingale et al. 2000 (in cm/hour)
! k600 is the standard air-sea exchange coefficient for CO2 gas, related to
! a Schmidt number of 600

k600(:) = 0.222*(PWIND(:)**2.0) + 0.333*PWIND(:)
! conversion into m s-1
k600(:) = k600(:) *1.0e-2/3600.


! k_dms : air-sea exchange coefficient for DMS in m s-1

k_dms(:) = k600(:)*(sc_dms(:)/sc_co2)**(-0.5)

! DMS emsission flux in mol m-2 s-1

PSFDMS(:) = k_dms(:) * DMS_OCEANIC(:)

END SUBROUTINE COUPLING_DMS_n                 

