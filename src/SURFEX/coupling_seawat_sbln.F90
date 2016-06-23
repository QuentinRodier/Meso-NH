!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     ###############################################################################
SUBROUTINE COUPLING_SEAWAT_SBL_n !
!!****  *COUPLING_SEAWAT_SBL_n * - Adds a SBL into SEAFLUX
!!
!!    PURPOSE
!!    -------
!
!!**  METHOD
!!    ------
!!
!!    REFERENCE
!!    ---------
!!      
!!
!!    AUTHOR
!!    ------
!!     V. Masson 
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    09/2007
!!      V. Masson   05/2009 Implicitation of momentum fluxes
!!      S. Riette   06/2009 Initialisation of XT, PQ, XU and XTKE on canopy levels
!!      S. Riette   10/2009 Iterative computation of XZ0
!!      S. Riette   01/2010 Use of interpol_sbl to compute 10m wind diagnostic
!!      B. Decharme  04/2013 new coupling variables
!----------------------------------------------------------------
!
!!
END SUBROUTINE COUPLING_SEAWAT_SBL_n
