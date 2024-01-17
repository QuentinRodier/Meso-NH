#################################################################################
# Christelle Barthe (LAERO)
# 24/10/2023
# Trace de diagnostics electriques pour le cas test KW78 utilise avec ELE4
#################################################################################

import copy
import sys
import matplotlib as mpl
import matplotlib.colors as mcolors
import matplotlib.pyplot as plt
from matplotlib.gridspec import GridSpec
import numpy as np
from read_MNHfile import read_netcdf
from misc_functions import oblique_proj
from svgtocmap import svgconv

#---------------------------------------------------------------------------------

def plot_CH_color(ax, fig, nx, ny, var, cmap, norm, lev_col, titre):

    '''
    Trace une coupe horizontale de la variable var en couleurs
    '''

    # coupe horizontale de var
    plot = ax.contourf(nx/1000, ny/1000, var, 
                       cmap=cmap, norm=norm, levels=lev_col, extend='both')

    ax.set_title(titre, fontsize=13, fontweight='normal', loc='left')
    fig.colorbar(plot, shrink=1., pad=0.05, ax=ax, location='bottom')

    # X and Y labels setting
    ax.set_xlabel('X (km)', fontsize=10)
    ax.set_ylabel('Y (km)', fontsize=10)
    ax.tick_params(axis='both', labelsize=10)

    # Zoom (ou pas) sur une partie de la figure
    plt.xlim(0,40)
    plt.ylim(0,40)

#---------------------------------------------------------------------------------

def plot_CH_color_isoline(ax, fig, nx, ny, var_col, var_iso, cmap, norm, lev_col, lev_iso, titre):

    '''
    Trace une coupe horizontale de la variable var_col en couleurs
    et superpose des isolignes pour var_iso
    '''

    # coupe horizontale couleurs de var_col
    plot_col = ax.contourf(nx/1000, ny/1000, var_col, 
                           cmap=cmap, norm=norm, levels=lev_col, extend='both')

    ax.set_title(titre, fontsize=13, fontweight='normal', loc='left')
    fig.colorbar(plot_col, shrink=1., pad=0.05, ax=ax, location='bottom')

    # isolignes pour var_iso
    plot_iso = ax.contour(nx/1000, ny/1000, var_iso, 
                          lev_iso, colors='black', linewidths=2, linestyles='solid')

    # X and Y labels setting
    ax.set_xlabel('X (km)', fontsize=10)
    ax.set_ylabel('Y (km)', fontsize=10)
    ax.tick_params(axis='both', labelsize=10)

    # Zoom (ou pas) sur une partie de la figure
    plt.xlim(0,40)
    plt.ylim(0,40)

#---------------------------------------------------------------------------------

def plot_CV_color(ax, fig, nx, ny, nz, var, i_beg, j_beg, i_end, j_end, cmap, norm, lev_col, titre):

    '''
    Trace une coupe verticale de la variable var en couleurs
    '''

    # Calcul des projections sur un plan vertical
    angle_sec1, var_sec1, axe_m1 = oblique_proj(var, nx, ny, nz, i_beg, j_beg, i_end, j_end)

    # Creation d'un tableau 2D pour l'axe des X
    nx_2d = copy.deepcopy(var_sec1)
    for i in range(var_sec1.shape[1]):
        nx_2d[:,i] = axe_m1[i]

    # Coupe verticale couleurs de var
    plot = ax.contourf(axe_m1/1000, nz/1000, var_sec1, 
                       cmap=cmap, norm=norm, levels=lev_col, extend='both')

    # Ajout d'un titre
    ax.set_title(titre, fontsize=13, fontweight='normal', loc='left')

    # Colorbar
    fig.colorbar(plot, shrink=1., pad=0.05, ax=ax, location='bottom')

    # X and Y labels setting
    ax.set_xlabel('Distance horizontale (km)', fontsize=10)
    ax.set_ylabel('Altitude (km)', fontsize=10)
    ax.tick_params(axis='both', labelsize=10)

#---------------------------------------------------------------------------------

def plot_CV_color_isoline(ax, fig, nx, ny, nz, var_col, var_iso, i_beg, j_beg, i_end, j_end, cmap, norm, lev_col, lev_iso, titre):

    '''
    Trace une coupe verticale de la variable var_col en couleurs
    et superpose des isolignes pour var_iso
    '''

    # Calcul des projections sur un plan vertical
    angle_sec1, var_col_sec1, axe_m1 = oblique_proj(var_col, nx, ny, nz, i_beg, j_beg, i_end, j_end)
    angle_sec1, var_iso_sec1, axe_m1 = oblique_proj(var_iso, nx, ny, nz, i_beg, j_beg, i_end, j_end)

    # Creation d'un tableau 2D pour l'axe des X
    nx_2d = copy.deepcopy(var_col_sec1)
    for i in range(var_col_sec1.shape[1]):
        nx_2d[:,i] = axe_m1[i]

    # coupe horizontale couleurs de var_col
    plot_col = ax.contourf(axe_m1/1000, nz/1000, var_col_sec1, 
                           cmap=cmap, norm=norm, levels=lev_col, extend='both')

    ax.set_title(titre, fontsize=13, fontweight='normal', loc='left')
    fig.colorbar(plot_col, shrink=1., pad=0.05, ax=ax, location='bottom')

    # isolignes pour var_iso
    plot_iso = ax.contour(axe_m1/1000, nz/1000, var_iso_sec1, 
                          lev_iso, colors='black', linewidths=2, linestyles='solid')

    # X and Y labels setting
    ax.set_xlabel('Distance horizontale (km)', fontsize=10)
    ax.set_ylabel('Altitude (km)', fontsize=10)
    ax.tick_params(axis='both', labelsize=10)

#---------------------------------------------------------------------------------

def coupes_MNH(simu, dom, seg, hour, path_in, path_out):

    '''
    Cette fonction retourne 3 figures de plusieurs panneaux representant differentes coupes de variables Meso-NH
        
    Format des fichiers en entree : Sorties Meso-NH de type 'simu'.'dom'.'seg'.'hour'.nc
    '''

    # Nom de fichier et variables a lire
    LnameFiles = [simu+'.'+dom+'.'+seg+'.'+hour+'.nc']
    Dvar_input = {'f1':['level','ni','nj','ZS','UT','VT','WT','RCT','RRT','RIT','RST','RGT','QNIONP','QNIONN','QCELEC','QRELEC','QIELEC','QSELEC','QGELEC','EFIELDU','EFIELDV','EFIELDW','NI_IAGGS','NI_IDRYG','NI_SDRYG','INDUC_CG','AREA_IC','AREA_CG','TRIG_IC','IMPACT_CG']}


    # Lecture des variables dans le fichier MesoNH
    Dvar = {}
    Dvar = read_netcdf(LnameFiles, Dvar_input, path=path_in, removeHALO=True)


    # Calcul de variables supplementaires
    ## charge totale portee par les ions (C/m3)
    Dvar['f1']['QIONS'] = (Dvar['f1']['QNIONP'] - Dvar['f1']['QNIONN']) * 1.e-19
    ## densite de charge totale (C/m3)
    Dvar['f1']['QTOT'] = Dvar['f1']['QIONS'] + Dvar['f1']['QCELEC'] + Dvar['f1']['QRELEC'] + Dvar['f1']['QIELEC'] + Dvar['f1']['QSELEC'] + Dvar['f1']['QGELEC']


    # Changements d'unites
    ## densite de charge electrique : C/m3 --> nC/m3
    Dvar['f1']['QTOT'] = Dvar['f1']['QTOT'] * 1.e9
    Dvar['f1']['QCELEC'] = Dvar['f1']['QCELEC'] * 1.e9
    Dvar['f1']['QRELEC'] = Dvar['f1']['QRELEC'] * 1.e9
    Dvar['f1']['QIELEC'] = Dvar['f1']['QIELEC'] * 1.e9
    Dvar['f1']['QSELEC'] = Dvar['f1']['QSELEC'] * 1.e9
    Dvar['f1']['QGELEC'] = Dvar['f1']['QGELEC'] * 1.e9
    Dvar['f1']['QIONS'] = Dvar['f1']['QIONS'] * 1.e9
    ## rapport de melange : kg/kg --> g/kg
    Dvar['f1']['RCT'] = Dvar['f1']['RCT'] * 1.e3
    Dvar['f1']['RRT'] = Dvar['f1']['RRT'] * 1.e3
    Dvar['f1']['RIT'] = Dvar['f1']['RIT'] * 1.e3
    Dvar['f1']['RST'] = Dvar['f1']['RST'] * 1.e3
    Dvar['f1']['RGT'] = Dvar['f1']['RGT'] * 1.e3
    ## champ electrique : V/m --> kV/m
    Dvar['f1']['EFIELDU'] = Dvar['f1']['EFIELDU'] * 1.e-3
    Dvar['f1']['EFIELDV'] = Dvar['f1']['EFIELDV'] * 1.e-3
    Dvar['f1']['EFIELDW'] = Dvar['f1']['EFIELDW'] * 1.e-3    

    # Indices de la projection sur un plan vertical
    i_beg = 1
    j_beg = 0
    i_end = 38
    j_end = 39  


    # Definition des colorbars
    ## negatif/positif
    svg = svgconv('BlueWhiteOrangeRed.svg',smooth=False)
    cmap_negpos = svg.makecmap(reverse=False)
    ## evolution
    svg = svgconv('sunshine_9lev.svg',smooth=False)
    cmap_evol = svg.makecmap(reverse=False)


    # Definition des isolignes/isocontours
    ## vitesse verticale
    levels_w = [-20,-10,-5,-4,-3,-2,-1,-0.5,0.5,1,2,3,4,5,10,20]
    norm_w = mcolors.BoundaryNorm(levels_w, cmap_negpos.N, clip=True)
    ## charges électriques
    levels_q = [-5,-1,-0.5,-0.1,-0.01,-0.001,0.001,0.01,0.05,0.1,0.5,1,5]
    norm_q = mpl.colors.BoundaryNorm(levels_q, cmap_negpos.N)
    ## processus NI et inductif
    levels_n = [-100,-50,-10,-5,-1,-0.1,0.1,1,5,10,50,100]
    norm_n = mcolors.BoundaryNorm(levels_n, cmap_negpos.N, clip=True)
    ## eclairs
    levels_f = [0.1,1,5,10,20,30,40,50,60]
    norm_f = mcolors.BoundaryNorm(levels_f, cmap_evol.N, clip=True)
    ## champ electrique
    levels_e = [-100,-50,-10,-5,-1,-0.5,-0.1,-0.01,0.01,0.1,0.5,1,5,10,50,100]
    norm_e = mcolors.BoundaryNorm(levels_e, cmap_negpos.N, clip=True)
    ## rapports de melange especes non precipitantes
    lev_cld = [0.01,0.1,1]
    ## rapports de melange especes precipitantes
    lev_precip = [0.1,1,5]
    ## densite de points de declenchement / points foudroyes
    lev_flash = [1,10]


# Figure 1 : COUPES HORIZONTALES

    fig = plt.figure(layout='constrained', figsize=(10, 12))

    gs = GridSpec(2, 2, figure=fig)


    ## vitesse verticale a 5 km
    ax1 = fig.add_subplot(gs[0,0])

    titre = 'w (m s$^{-1}$) @ 5000 m'

    plot_CH_color(ax=ax1, fig=fig,
                  nx=Dvar['f1']['ni'], ny=Dvar['f1']['nj'], var=Dvar['f1']['WT'][10,:,:], 
                  cmap=cmap_negpos, norm=norm_w, lev_col=levels_w, 
                  titre=titre)

    ### ajout d'un segment pour localiser les coupes verticales
    ax1.plot([i_beg,i_end],[j_beg,j_end], color='black', linewidth=2)


    ## densite de points IC
    ax2 = fig.add_subplot(gs[1,0])

    titre = 'IC flashes'

    plot_CH_color_isoline(ax=ax2, fig=fig,
                          nx=Dvar['f1']['ni'], ny=Dvar['f1']['nj'], 
                          var_col=Dvar['f1']['AREA_IC'][:,:], var_iso=Dvar['f1']['AREA_IC'][:,:],
                          cmap=cmap_evol, norm=norm_f, lev_col=levels_f, lev_iso=lev_flash, titre=titre)


    ## densite de points CG
    ax3 = fig.add_subplot(gs[1,1])

    titre = 'CG flashes'

    plot_CH_color_isoline(ax=ax3, fig=fig,
                          nx=Dvar['f1']['ni'], ny=Dvar['f1']['nj'], 
                          var_col=Dvar['f1']['AREA_CG'][:,:], var_iso=Dvar['f1']['IMPACT_CG'][:,:],
                          cmap=cmap_evol, norm=norm_f, lev_col=levels_f, lev_iso=lev_flash,
                          titre=titre)

    ## Sauvegarde des CH dans un fichier png
    plt.savefig(path_out+'Fig_kw78_elec_CH_'+seg+'_'+hour+'.png', dpi=300, bbox_inches='tight')
    #plt.show()


# Figure 2 : COUPES VERTICALES

#   -----------------------
#  | w     | qtot  | qions |
#   -----------------------
#  | qc+rc | qr+rr | qi+ri |
#   -----------------------
#  | qs+rc | qg+rg |       |
#   -----------------------

    fig = plt.figure(layout='constrained', figsize=(12, 12))

    gs = GridSpec(3, 3, figure=fig)

    # vitesse verticale
    ax1 = fig.add_subplot(gs[0,0])

    titre = 'Vitesse verticale (m s$^{-1}$)'
    
    plot_CV_color(ax=ax1, fig=fig,
                  nx=Dvar['f1']['ni'], ny=Dvar['f1']['nj'], nz=Dvar['f1']['level'], var=Dvar['f1']['WT'],
                  i_beg=i_beg, j_beg=j_beg, i_end=i_end, j_end=j_end,
                  cmap=cmap_negpos, norm=norm_w, lev_col=levels_w, titre=titre)

    # charge electrique totale
    ax2 = fig.add_subplot(gs[0,1])

    titre = 'Q$_{tot}$ (nC m$^{-3}$)'
    
    plot_CV_color(ax=ax2, fig=fig,
                  nx=Dvar['f1']['ni'], ny=Dvar['f1']['nj'], nz=Dvar['f1']['level'], var=Dvar['f1']['QTOT'],
                  i_beg=i_beg, j_beg=j_beg, i_end=i_end, j_end=j_end,
                  cmap=cmap_negpos, norm=norm_q, lev_col=levels_q, titre=titre)

    # charge electrique des ions
    ax3 = fig.add_subplot(gs[0,2])

    titre = 'Q$_{ions}$ (nC m$^{-3}$)'
    
    plot_CV_color(ax=ax3, fig=fig,
                  nx=Dvar['f1']['ni'], ny=Dvar['f1']['nj'], nz=Dvar['f1']['level'], var=Dvar['f1']['QIONS'],
                  i_beg=i_beg, j_beg=j_beg, i_end=i_end, j_end=j_end,
                  cmap=cmap_negpos, norm=norm_q, lev_col=levels_q, titre=titre)

    # charge electrique de l'eau nuageuse
    ax4 = fig.add_subplot(gs[1,0])

    titre = 'Q$_c$ (nC m$^{-3}$) et r$_c$ (g kg$^{-1}$)'
    
    plot_CV_color_isoline(ax=ax4, fig=fig,
                          nx=Dvar['f1']['ni'], ny=Dvar['f1']['nj'], nz=Dvar['f1']['level'], 
                          var_col=Dvar['f1']['QCELEC'], var_iso=Dvar['f1']['RCT'],
                          i_beg=i_beg, j_beg=j_beg, i_end=i_end, j_end=j_end,
                          cmap=cmap_negpos, norm=norm_q, lev_col=levels_q, lev_iso=lev_cld, titre=titre)

    # charge electrique des gouttes de pluie
    ax5 = fig.add_subplot(gs[1,1])

    titre = 'Q$_r$ (nC m$^{-3}$) et r$_r$ (g kg$^{-1}$)'
    
    plot_CV_color_isoline(ax=ax5, fig=fig,
                          nx=Dvar['f1']['ni'], ny=Dvar['f1']['nj'], nz=Dvar['f1']['level'], 
                          var_col=Dvar['f1']['QRELEC'], var_iso=Dvar['f1']['RRT'],
                          i_beg=i_beg, j_beg=j_beg, i_end=i_end, j_end=j_end,
                          cmap=cmap_negpos, norm=norm_q, lev_col=levels_q, lev_iso=lev_precip, titre=titre)

    # charge electrique de la glace primaire
    ax6 = fig.add_subplot(gs[1,2])

    titre = 'Q$_i$ (nC m$^{-3}$) et r$_i$ (g kg$^{-1}$)'
    
    plot_CV_color_isoline(ax=ax6, fig=fig,
                          nx=Dvar['f1']['ni'], ny=Dvar['f1']['nj'], nz=Dvar['f1']['level'], 
                          var_col=Dvar['f1']['QIELEC'], var_iso=Dvar['f1']['RIT'],
                          i_beg=i_beg, j_beg=j_beg, i_end=i_end, j_end=j_end,
                          cmap=cmap_negpos, norm=norm_q, lev_col=levels_q, lev_iso=lev_cld, titre=titre)

    # charge electrique de la neige
    ax7 = fig.add_subplot(gs[2,0])

    titre = 'Q$_s$ (nC m$^{-3}$) et r$_s$ (g kg$^{-1}$)'
    
    plot_CV_color_isoline(ax=ax7, fig=fig,
                          nx=Dvar['f1']['ni'], ny=Dvar['f1']['nj'], nz=Dvar['f1']['level'], 
                          var_col=Dvar['f1']['QSELEC'], var_iso=Dvar['f1']['RST'],
                          i_beg=i_beg, j_beg=j_beg, i_end=i_end, j_end=j_end,
                          cmap=cmap_negpos, norm=norm_q, lev_col=levels_q, lev_iso=lev_precip, titre=titre)

    # charge electrique du graupel
    ax8 = fig.add_subplot(gs[2,1])

    titre = 'Q$_g$ (nC m$^{-3}$) et r$_g$ (g kg$^{-1}$)'
    
    plot_CV_color_isoline(ax=ax8, fig=fig,
                          nx=Dvar['f1']['ni'], ny=Dvar['f1']['nj'], nz=Dvar['f1']['level'], 
                          var_col=Dvar['f1']['QGELEC'], var_iso=Dvar['f1']['RGT'],
                          i_beg=i_beg, j_beg=j_beg, i_end=i_end, j_end=j_end,
                          cmap=cmap_negpos, norm=norm_q, lev_col=levels_q, lev_iso=lev_precip, titre=titre)


    # Sauvegarde des CV dans un fichier png
    plt.savefig(path_out+'Fig_kw78_elec_CVq_'+seg+'_'+hour+'.png', dpi=300, bbox_inches='tight')
    #plt.show()


# Figure 3 : COUPES VERTICALES

#   -----------------------
#  | Ex    | Ey    | Ez    |
#   -----------------------
#  | NI_is | NI_ig | NI_sg |
#   -----------------------
#  | Induc |       |       |
#   -----------------------

    fig = plt.figure(layout='constrained', figsize=(12, 12))

    gs = GridSpec(3, 3, figure=fig)

    # composante U du champ electrique
    ax1 = fig.add_subplot(gs[0,0])

    titre = 'E$_x$ (V m$^{-1}$)'
    
    plot_CV_color(ax=ax1, fig=fig,
                  nx=Dvar['f1']['ni'], ny=Dvar['f1']['nj'], nz=Dvar['f1']['level'], var=Dvar['f1']['EFIELDU'],
                  i_beg=i_beg, j_beg=j_beg, i_end=i_end, j_end=j_end,
                  cmap=cmap_negpos, norm=norm_e, lev_col=levels_e, titre=titre)


    # composante V du champ electrique
    ax2 = fig.add_subplot(gs[0,1])

    titre = 'E$_y$ (V m$^{-1}$)'
    
    plot_CV_color(ax=ax2, fig=fig,
                  nx=Dvar['f1']['ni'], ny=Dvar['f1']['nj'], nz=Dvar['f1']['level'], var=Dvar['f1']['EFIELDV'],
                  i_beg=i_beg, j_beg=j_beg, i_end=i_end, j_end=j_end,
                  cmap=cmap_negpos, norm=norm_e, lev_col=levels_e, titre=titre)

    # composante W du champ electrique
    ax3 = fig.add_subplot(gs[0,2])

    titre = 'E$_z$ (V m$^{-1}$)'
    
    plot_CV_color(ax=ax3, fig=fig,
                  nx=Dvar['f1']['ni'], ny=Dvar['f1']['nj'], nz=Dvar['f1']['level'], var=Dvar['f1']['EFIELDW'],
                  i_beg=i_beg, j_beg=j_beg, i_end=i_end, j_end=j_end,
                  cmap=cmap_negpos, norm=norm_e, lev_col=levels_e, titre=titre)

    # chargement non-inductif I-S
    ax4 = fig.add_subplot(gs[1,0])

    titre = 'NI$_{iaggs}$ (pC m$^{-3}$)'
    
    plot_CV_color(ax=ax4, fig=fig,
                  nx=Dvar['f1']['ni'], ny=Dvar['f1']['nj'], nz=Dvar['f1']['level'], var=Dvar['f1']['NI_IAGGS'],
                  i_beg=i_beg, j_beg=j_beg, i_end=i_end, j_end=j_end,
                  cmap=cmap_negpos, norm=norm_n, lev_col=levels_n, titre=titre)

    # chargement non-inductif I-G
    ax5 = fig.add_subplot(gs[1,1])

    titre = 'NI$_{idryg}$ (pC m$^{-3}$)'
    
    plot_CV_color(ax=ax5, fig=fig,
                  nx=Dvar['f1']['ni'], ny=Dvar['f1']['nj'], nz=Dvar['f1']['level'], var=Dvar['f1']['NI_IDRYG'],
                  i_beg=i_beg, j_beg=j_beg, i_end=i_end, j_end=j_end,
                  cmap=cmap_negpos, norm=norm_n, lev_col=levels_n, titre=titre)

    # chargement non-inductif S-G
    ax6 = fig.add_subplot(gs[1,2])

    titre = 'NI$_{sdryg}$ (pC m$^{-3}$)'
    
    plot_CV_color(ax=ax6, fig=fig,
                  nx=Dvar['f1']['ni'], ny=Dvar['f1']['nj'], nz=Dvar['f1']['level'], var=Dvar['f1']['NI_SDRYG'],
                  i_beg=i_beg, j_beg=j_beg, i_end=i_end, j_end=j_end,
                  cmap=cmap_negpos, norm=norm_n, lev_col=levels_n, titre=titre)

    # mecanisme inductif
    ax7 = fig.add_subplot(gs[2,0])

    titre = 'INDUC$_{cg}$ (pC m$^{-3}$)'
    
    plot_CV_color(ax=ax7, fig=fig,
                  nx=Dvar['f1']['ni'], ny=Dvar['f1']['nj'], nz=Dvar['f1']['level'], var=Dvar['f1']['INDUC_CG'],
                  i_beg=i_beg, j_beg=j_beg, i_end=i_end, j_end=j_end,
                  cmap=cmap_negpos, norm=norm_n, lev_col=levels_n, titre=titre)


    # Sauvegarde des CV dans un fichier png
    plt.savefig(path_out+'Fig_kw78_elec_CVe_'+seg+'_'+hour+'.png', dpi=300, bbox_inches='tight')
    #plt.show()

    return()

##############################################################

# verification des arguments
print(sys.argv)
seg = sys.argv[1]
hour = sys.argv[2]

# 4 simulations :
# - ICE3+LRED=F avec ELE3: repertoire sur nuwa = old / CSEG = 'IC3E3'
# - ICE3+LRED=T avec ELE4: repertoire sur nuwa = ice3 / CSEG = 'IC3E4'
# - LIMA 2-moment partiel avec ELE4: repertoire sur nuwa = lima / CSEG = 'LI1E4'
# - LIMA 2-moment total avec ELE4: repertoire sur nuwa = lima2 / CSEG = 'LI2E4'

rep_simu = {'IC3E3':'old', 'IC3E4':'ice3', 'LI1E4':'lima', 'LI2E4':'lima2'}
#rep_simu = {'LI1E4':'lima', 'SCONV':'sconv', 'SKAFR':'sconv', 'CORED':'sconv', 'TURB1':'turb'}

path_in = '../002_mesonh/'+rep_simu[seg]+'/'
path_out = ''
simu = 'KW78U'
dom = '1'

print(coupes_MNH(simu, dom, seg, hour, path_in, path_out))

