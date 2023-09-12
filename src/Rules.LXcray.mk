#MNH_LIC Copyright 1994-2022 CNRS, Meteo-France and Universite Paul Sabatier
#MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
#MNH_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
#MNH_LIC for details. version 1.
##########################################################
#                                                        #
# Compiler Options                                       #
#                                                        #
##########################################################
#
#   craytfn version
CFV=$(shell crayftn --version | grep -E -m1 -o ' [[:digit:]\.]{2,}( |$$)'  | sed 's/\.//g' )
#
#OBJDIR_PATH=/home/escj/azertyuiopqsdfghjklm/wxcvbn/azertyuiopqsdfghjklmwxcvbn
#
OPT_BASE   =  -hpic -emf -h acc_model=auto_async_none:no_fast_addr:no_deep_copy -halias=none
OPT_PERF0  =  -G0 -O0 
OPT_PERF1  =  -G2 -O1 -hflex_mp=intolerant -Ofp0 -hnofma -hnoomp -K trap=fp -hipa0 
OPT_PERF2  =  -G2 -O2 -hflex_mp=intolerant -Ofp0 -hnofma -hnoomp -hipa0 
#
OPT_NOOPENACC     = -hnoacc
OPT_OPENACC       = -hacc
#
OPT_CHECK  =  -Rbc
OPT_I8     =  -sinteger64
OPT_R8     =  -sreal64
#
ifeq "$(VER_USER)" "ZSOLVER"
CPPFLAGS  += -DCARTESIANGEOMETRY -DPIECEWISELINEAR
PROG_LIST += MG_MAIN MG_MAIN_MNH_ALL
endif
#
# Real/integer 4/8 option
#
MNH_REAL  ?=8
MNH_INT   ?=4
#
ifneq "$(MNH_REAL)" "4"
OPT_BASE           += $(OPT_R8)
endif
#
OPT_BASE_I4       := $(OPT_BASE)
ifeq "$(MNH_INT)" "8"
OPT_BASE          += $(OPT_I8)
LFI_INT           ?=8
else
LFI_INT           ?=4
endif
#
OPT       = $(OPT_BASE) $(OPT_PERF2) 
OPT0      = $(OPT_BASE) $(OPT_PERF0) 
OPT_NOCB  = $(OPT_BASE) $(OPT_PERF2)
#
ifeq "$(OPTLEVEL)" "DEBUG"
OPT       = $(OPT_BASE) $(OPT_PERF0) $(OPT_CHECK)
OPT0      = $(OPT_BASE) $(OPT_PERF0) $(OPT_CHECK)
OPT_NOCB  = $(OPT_BASE) $(OPT_PERF0)
CFLAGS   += -g -gdwarf-4
endif
#
ifeq "$(OPTLEVEL)" "OPENACC"
MNH_BITREP_OMP=YES
CPPFLAGS    += -DMNH_OPENACC -DMNH_GPUDIRECT
OPT       = $(OPT_BASE) $(OPT_PERF2) $(OPT_OPENACC)
OPT0      = $(OPT_BASE) $(OPT_PERF0) $(OPT_OPENACC)
OPT_NOCB  = $(OPT_BASE) $(OPT_PERF2) $(OPT_OPENACC)
#CXXFLAGS = -fopenmp
#OBJS_REPROD= spll_mode_sum_ll.o
#$(OBJS_REPROD) : OPT = $(OPT_BASE) $(OPT_PERF2) $(OPT_OPENACC) -Mvect=nosimd -Minfo=all -g
#OBJS_O1_OPENACC= spll_ice4_tendencies.o spll_turb_ver_thermo_flux.o
#OBJS_O1_OPENACC= spll_rain_ice_red.o
#$(OBJS_O1_OPENACC) : OPT = $(OPT_BASE) $(OPT_PERF0) $(OPT_OPENACC)

#OBJS_ASYNAll=spll_turb_ver_thermo_flux.o spll_turb_ver_thermo_corr.o spll_turb_ver_dyn_flux.o \
#             spll_turb_hor_uv.o spll_turb_hor_uw.o spll_turb_hor_vw.o spll_mode_turb.o\
#             spll_turb_hor_dyn_corr.o spll_turb_hor_thermo_flux.o spll_turb_hor_sv_corr.o  
# spll_mode_turb.o spll_p_abs.o spll_advection_metsv.o \
#             spll_advection_uvw.o spll_resolved_cloud.o
#$(OBJS_ASYNAll) : OPT = $(OPT_BASE) $(OPT_PERF2) $(OPT_OPENACC) -h acc_model=auto_async_all

OBJS_CONCURRENT=spll_multigrid.o spll_turb_ver_thermo_flux.o
#spll_resolved_cloud.o 
$(OBJS_CONCURRENT) : OPT = $(OPT_BASE) $(OPT_PERF2) $(OPT_OPENACC) -h concurrent -rimo

endif
ifeq "$(OPTLEVEL)" "OPENACCNOBITREP"
#MNH_BITREP_OMP=YES
CPPFLAGS    += -DMNH_OPENACC -DMNH_GPUDIRECT
OPT       = $(OPT_BASE) $(OPT_PERF2) $(OPT_OPENACC)
OPT0      = $(OPT_BASE) $(OPT_PERF0) $(OPT_OPENACC)
OPT_NOCB  = $(OPT_BASE) $(OPT_PERF2) $(OPT_OPENACC)
#CXXFLAGS = -fopenmp
#OBJS_REPROD= spll_mode_sum_ll.o
#$(OBJS_REPROD) : OPT = $(OPT_BASE) $(OPT_PERF2) $(OPT_OPENACC) -Mvect=nosimd -Minfo=all -g
#OBJS_O1_OPENACC= spll_ice4_tendencies.o spll_turb_ver_thermo_flux.o
#OBJS_O1_OPENACC= spll_rain_ice_red.o
#$(OBJS_O1_OPENACC) : OPT = $(OPT_BASE) $(OPT_PERF0) $(OPT_OPENACC)

#OBJS_ASYNAll=spll_turb_ver_thermo_flux.o spll_turb_ver_thermo_corr.o spll_turb_ver_dyn_flux.o \
#             spll_turb_hor_uv.o spll_turb_hor_uw.o spll_turb_hor_vw.o spll_mode_turb.o\
#             spll_turb_hor_dyn_corr.o spll_turb_hor_thermo_flux.o spll_turb_hor_sv_corr.o  
# spll_mode_turb.o spll_p_abs.o spll_advection_metsv.o \
#             spll_advection_uvw.o spll_resolved_cloud.o
#$(OBJS_ASYNAll) : OPT = $(OPT_BASE) $(OPT_PERF2) $(OPT_OPENACC) -h acc_model=auto_async_all

OBJS_CONCURRENT=spll_multigrid.o spll_turb_ver_thermo_flux.o
# spll_resolved_cloud.o 
$(OBJS_CONCURRENT) : OPT = $(OPT_BASE) $(OPT_PERF2) $(OPT_OPENACC) -h concurrent -rdimo

endif
#

ifeq "$(OPTLEVEL)" "OPENACCDEFONLY"
MNH_BITREP_OMP=YES
CPPFLAGS    += -DMNH_OPENACC -D_FAKEOPENACC
OPT       = $(OPT_BASE) $(OPT_PERF2) $(OPT_NOOPENACC)  
OPT0      = $(OPT_BASE) $(OPT_PERF0) $(OPT_NOOPENACC) 
OPT_NOCB  = $(OPT_BASE) $(OPT_PERF2) $(OPT_NOOPENACC)
#CXXFLAGS = -Kieee -Mnofma $(OPT_NOOPENACC)
#OBJS_REPROD= spll_mode_sum_ll.o
#$(OBJS_REPROD) : OPT = $(OPT_BASE) $(OPT_PERF2) $(OPT_NOOPENACC) -Mvect=nosimd -Minfo=all -g
endif
#
ifeq "$(OPTLEVEL)" "NOOPENACC"
OPT       = $(OPT_BASE) $(OPT_PERF2) $(OPT_NOOPENACC)
OPT0      = $(OPT_BASE) $(OPT_PERF0) $(OPT_NOOPENACC)
OPT_NOCB  = $(OPT_BASE) $(OPT_PERF2) $(OPT_NOOPENACC)
#CXXFLAGS = -Kieee -Mnofma $(OPT_NOOPENACC)
#OBJS_REPROD= spll_mode_sum_ll.o
#$(OBJS_REPROD) : OPT = $(OPT_BASE) $(OPT_PERF2) $(OPT_NOOPENACC) -Mvect=nosimd -Minfo=all -g
endif
#
ifeq "$(OPTLEVEL)" "O2NOVEC"
OPT       = $(OPT_BASE) $(OPT_PERF2) -O vector0
OPT0      = $(OPT_BASE) $(OPT_PERF0) -O vector0
OPT_NOCB  = $(OPT_BASE) $(OPT_PERF2) -O vector0
endif
ifeq "$(OPTLEVEL)" "O2"
OPT       = $(OPT_BASE) $(OPT_PERF2)
OPT0      = $(OPT_BASE) $(OPT_PERF0)
OPT_NOCB  = $(OPT_BASE) $(OPT_PERF2)
endif
ifeq "$(OPTLEVEL)" "O1"
OPT       = $(OPT_BASE) $(OPT_PERF1) 
OPT0      = $(OPT_BASE) $(OPT_PERF0) 
OPT_NOCB  = $(OPT_BASE) $(OPT_PERF1) 
endif
#
#
FC = ftn
FCFLAGS = -emf
CC=cc
CXX=CC
export FC CC FCFLAGS
ifeq "$(VER_MPI)" "MPIAUTO"
F90 = mpif90
#CPPFLAGS_SURCOUCHE = -DUSE_MPI
else
F90 = ftn
CPPFLAGS_SURCOUCHE = -DUSE_MPI
endif
F90FLAGS  =  $(OPT)
F77  = $(F90)
F77FLAGS  =  $(OPT) 
# -132
FX90 = $(F90)
FX90FLAGS =  $(OPT)
# -132 
#
#LDFLAGS    =  -Wl,-noinhibit-exec  -Wl,-warn-once $(PAR)
LDFLAGS    =   -Wl,-warn-once $(PAR) $(OPT_BASE)
#
# preprocessing flags 
#
#CPP = cpp -P -Wcomment -traditional -D_OPENACC=201306
CPP = mnh_expand -D_OPENACC=201306
#
CPPFLAGS_C         = -DLITTLE_endian
CPPFLAGS_SURFEX    = 
CPPFLAGS_SURCOUCHE +=  -DDEV_NULL -DMNH_COMPILER_CCE
CPPFLAGS_RAD       = 
CPPFLAGS_NEWLFI    = -DSWAPIO -DLINUX -DLFI_INT=${LFI_INT}
CPPFLAGS_MNH       = -DMNH=MNH -DSFX_MNH -DMNH_NO_MPI_LOGICAL48 -DMNH_COMPILER_CCE
ifeq ($(shell test $(CFV) -ge 1402 ; echo $$?),0)
CPPFLAGS_MNH       += -DMNH_COMPILER_CCE_1403
endif
CPPFLAGS_MNH       += -DMNH_EXPAND -DMNH_EXPAND_LOOP -DMNH_EXPAND_OPENACC
#CPPFLAGS_MNH       += -imacros MNH_OPENACC_NV_CR.CPP
ifdef VER_GA
CPPFLAGS_SURCOUCHE += -DMNH_GA
INC                += -I${GA_ROOT}/include
LIBS               += -L${GA_ROOT}/lib -larmci -lga -lgfortran
endif
#
# BITREP flags
#
#if MNH_BITREP exists => compile with the BITREP library
MNH_BITREP = NO
ifeq "$(MNH_BITREP)" "YES"
CPPFLAGS_MNH += -DMNH_BITREP
endif
#
# Test of bitrep with OMP compilation 
#
ifeq "$(MNH_BITREP_OMP)" "YES"
CXXFLAGS = -fopenmp
CPPFLAGS_MNH += -DMNH_BITREP_OMP
DIR_MASTER += LIB/BITREP
VPATH += LIB/BITREP
OBJS_LISTE_MASTER += br_transcendentals.o
LIBS += -lstdc++
%.o : %.cpp
	$(CXX) $(INC) $(CXXFLAGS) $(CPPFLAGS) -c $< -o $(OBJDIR)/$(*F).o
#
# Add gu.c , for Zero Floating Point Graduate UnderFlow on CPU & GPU
#
DIR_MASTER += ARCH_SRC/cray_gu
VPATH += ARCH_SRC/cray_gu
OBJS_LISTE_MASTER += gu.o
gu.o : CC=gcc
gu.o : INC=
gu.o : CPPFLAGS_C=
endif
#
# LIBTOOLS flags
#
#if MNH_TOOLS exists => compile the tools
MNH_TOOLS = yes
#
## IOLFI flag
#
#if MNH_IOLFI exists => compile the NEWLFI library (for LFI files)
MNH_IOLFI=yes
#
## COMPRESS flag
#
#if MNH_COMPRESS exists => compile the COMPRESS library (for LFI files)
MNH_COMPRESS=yes
#
## S4PY flag
#
#if MNH_S4PY exists => compile the libs4py library (for epygram)
#MNH_S4PY=no
#
## ecCodes or grib_api selection
#MNH_GRIBAPI: if set to no:  use ecCodes
#             if set to yes: use grib_api (deprecated library)
#
MNH_GRIBAPI=no
#
NETCDF_SUPPFLAGS='-emf'
ECCODES_FFLAGS='-emf -hsystem_alloc'
ECCODES_CFLAGS=' -gdwarf-4 '
EC_CONF='-DIEEE_LE=1'
CDF_CONF= CPP=cpp 
HDF_CONF= CPP=cpp 
#
# Bypat nvidia70 problème
ifeq "$(CRAY_ACCEL_TARGET)" "nvidia70"
CPPFLAGS_MNH       += -DTARGET_NV70
endif
##########################################################
#                                                        #
# Source of MESONH PACKAGE  Distribution                 #
#                                                        #
##########################################################
#DIR_SURCOUCHE   += ARCH_SRC/bug_surcouche
#DIR_MNH         += ARCH_SRC/bug_mnh
#DIR_RAD         += ARCH_SRC/bug_rad
#DIR_SURFEX      += ARCH_SRC/surfex
#
include Makefile.MESONH.mk
#
##########################################################
#                                                        #
# extra VPATH, Compilation flag modification             #
#         systeme module , etc ...                       #
#         external precompiled module librairie          #
#         etc ...                                        #
#                                                        #
##########################################################
# Juan & Maud 20/03/2008 --> Ifort 10.1.008 Bug O2 optimization
#OPT_PERF1  =  -O1
OBJS_O1= spll_schu.o spll_ps2str.o spll_ini_one_way_n.o spll_urban_solar_abs.o spll_mode_ekf.o mode_ekf.mod
#spll_p_abs.o
$(OBJS_O1) : OPT = $(OPT_BASE) $(OPT_PERF1)
OBJS_O0= mode_gridproj.mod spll_mode_gridproj.o spll_ini_dynamics.o spll_sunpos_n.o spll_average_diag.o spll_write_lfifm1_for_diag.o \
         spll_write_aircraft_balloon.o spll_mode_write_les_n.o mode_write_les_n.mod modd_mnh_surfex_n.mod
# spll_ground_param_n.o

$(OBJS_O0) : OPT = $(OPT_BASE) $(OPT_PERF0)

ifneq "$(findstring 8,$(LFI_INT))" ""
OBJS_I8=spll_NEWLFI_ALL.o
$(OBJS_I8) : OPT = $(OPT_BASE) $(OPT_PERF2) $(OPT_I8)
endif

ifeq "$(MNH_INT)" "8"
OBJS_I4=spll_modd_netcdf.o modd_netcdf.mod
$(OBJS_I4) : OPT = $(OPT_BASE_I4)
endif

OBJS_LST = spll_condensation.o spll_turb_ver_thermo_flux.o spll_rotate_wind.o spll_mode_mnh_zwork.o mode_mnh_zwork.mod spll_contrav_device.o \
           spll_advection_metsv.o \
           spll_mym_device.o spll_myf_device.o spll_dyf_device.o \
           spll_gx_m_u_device.o spll_gy_m_v_device.o \
           spll_gravity.o spll_gravity_impl.o \
           spll_advection_uvw.o spll_resolved_cloud.o spll_mode_tools.o mode_tools.mod \
           spll_mode_rain_ice_sedimentation_split.o mode_rain_ice_sedimentation_split.mod spll_ice_adjust.o \
           spll_countjv1d_device.o spll_countjv2d_device.o spll_countjv3d_device.o \
           spll_mode_turb.o mode_turb.mod spll_phys_param_n.o spll_model_n.o spll_tridiag_tke.o \
           spll_turb_ver_thermo_corr.o spll_tke_eps_sources.o spll_turb_ver_dyn_flux.o \
           spll_turb_hor_uv.o spll_turb_hor_uw.o spll_turb_hor_vw.o \
           spll_turb_hor_dyn_corr.o spll_turb_hor_thermo_flux.o \
           spll_pressurez.o spll_p_abs.o spll_zsolver.o spll_zsolver_inv.o spll_dotprod.o \
           spll_communication.o spll_discretisation.o spll_multigrid.o spll_datatypes.o \
           spll_get_halo_start_d.o spll_get_halo_stop_d.o spll_mass_leak.o \
           spll_ppm_01_x.o spll_ppm_01_y.o spll_ppm_01_z.o \
           spll_ppm_s0_x.o spll_ppm_s0_y.o spll_ppm_s0_z.o \
           spll_mode_rain_ice_slow.o mode_rain_ice_slow.mod spll_mode_rain_ice_fast_rs.o mode_rain_ice_fast_rs.mod \
           spll_prandtl.o spll_mode_rain_ice_fast_rg.o mode_rain_ice_fast_rg.mod spll_mode_rain_ice_fast_ri.o mode_rain_ice_fast_ri.mod \
           spll_mode_rain_ice.o mode_rain_ice.mod spll_mode_rain_ice_warm.o mode_rain_ice_warm.mod \
           spll_ground_param_n.o spll_advec_ppm_algo.o \
           spll_turb_hor_sv_corr.o spll_rain_ice.o spll_mode_rain_ice_nucleation.o mode_rain_ice_nucleation.mod \
           spll_mode_prandtl.o mode_prandtl.mod spll_turb_ver.o spll_mode_repro_sum.o mode_repro_sum.mod \
           spll_tridiag_thermo.o spll_tridiag_wind.o spll_tridiag_w.o \
           spll_tridiag_tke.o spll_advec_weno_k_2_my.o spll_advec_weno_k_2_mx.o \
           spll_advec_weno_k_2_vy.o spll_advec_weno_k_2_ux.o spll_mxm_device.o \
           spll_mym_device.o spll_mzm_device.o
           
#$(OBJS_LST) : OPT += -rdimo

OPT += -rimo

SPLL = spll_lst

