MODULE MODD_NETCDF
IMPLICIT NONE 

INTEGER,PARAMETER :: IDCDF_KIND = SELECTED_INT_KIND(8)

TYPE IOCDF
   TYPE(DIMCDF), POINTER :: DIM_NI      => NULL()
   TYPE(DIMCDF), POINTER :: DIM_NJ      => NULL()
   TYPE(DIMCDF), POINTER :: DIM_LEVEL   => NULL()
   TYPE(DIMCDF), POINTER :: DIM_NI_U    => NULL()
   TYPE(DIMCDF), POINTER :: DIM_NJ_U    => NULL()
   TYPE(DIMCDF), POINTER :: DIM_NI_V    => NULL()
   TYPE(DIMCDF), POINTER :: DIM_NJ_V    => NULL()
   TYPE(DIMCDF), POINTER :: DIM_LEVEL_W => NULL()
   TYPE(DIMCDF), POINTER :: DIMTIME     => NULL()
   TYPE(DIMCDF), POINTER :: DIMSTR      => NULL()
   TYPE(DIMCDF), POINTER :: DIMLIST     => NULL()
END TYPE IOCDF

TYPE DIMCDF
   CHARACTER(LEN=8)         :: NAME = ''
   INTEGER(KIND=IDCDF_KIND) :: LEN  = 0
   INTEGER(KIND=IDCDF_KIND) :: ID   = -1
   TYPE(DIMCDF), POINTER    :: NEXT => NULL()
END TYPE DIMCDF

TYPE(DIMCDF),DIMENSION(3,0:4) :: NCOORDID !X,Y,Z coordinates for the 4 Arakawa points
                                          !0 2nd-dimension is to treat NGRID=0 case without crash

END MODULE MODD_NETCDF
