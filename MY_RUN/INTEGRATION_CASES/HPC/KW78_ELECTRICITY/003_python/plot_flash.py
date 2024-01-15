#################################################################################
# Christelle Barthe (LAERO)
# 24/10/2023
# Trace de diagnostics electriques pour le cas test KW78 utilise avec ELE3/ELE4
#################################################################################

import matplotlib as mpl
import matplotlib.pyplot as plt
import numpy as np
import sys
from matplotlib.gridspec import GridSpec

#---------------------------------------------------------------------------------

# verification des arguments
print(sys.argv)
seg = sys.argv[1]

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

# definition de listes vides pour contenir les donnees
nb,time,fltype,nbseg,etrig,ztrig,neutpos,neutneg=[],[],[],[],[],[],[],[]


# 1. Lecture du fichier *_fgeom_diag.asc

LnameFile = simu+'_'+seg+'_fgeom_diag.asc'
with open(path_in+LnameFile,'r') as f:
    # extraction des donnees ligne par ligne
    for l in f:
        # on ne lit pas l'en-tete du fichier
        if "-" in l[0]:
            continue
        if "*" in l[0]:
            continue
        donnee=[str(d) for d in l.split()]
        # conversion des donnees d d'une ligne
        nbi,timei,fltypei,nbsegi,etrigi,ztrigi,neutposi,neutnegi = float(donnee[0]),float(donnee[1]),float(donnee[4]),float(donnee[5]),float(donnee[6]),float(donnee[9]),float(donnee[10]),float(donnee[11])
        # ajout des donnees dans les listes
        nb.append(nbi), time.append(timei), fltype.append(fltypei), nbseg.append(nbsegi), etrig.append(etrigi), ztrig.append(ztrigi), neutpos.append(neutposi), neutneg.append(neutnegi)


# 2. Prints de qq stats

print("nb total d'eclairs dans ce fichier = ", len(nb))
print("Moyenne nb de segments = ", np.mean(nbseg))
print("Moyenne champ de declenchement = ", np.mean(etrig))
print("Moyenne altitude de declenchement = ", np.mean(ztrig))
print("Moyenne charge positive neutralisee = ", np.mean(neutpos))
print("Moyenne charge negative neutralisee = ", np.mean(neutneg))
print("Ecart type nb de segments = ", np.std(nbseg))
print("Ecart type champ de declenchement = ", np.std(etrig))
print("Ecart type altitude de declenchement = ", np.std(ztrig))
print("Ecart type charge positive neutralisee = ", np.std(neutpos))
print("Ecart type charge negative neutralisee = ", np.std(neutneg))


# 3. Calcul des parametres a tracer 

# 3.1 frequence d'eclairs par minute

# duree max de l'orage
lenstorm = max(time)

# decoupage en intervalles de 59 s : 1er bin = [0,60[, 2eme bin = [60,120[ ...
timesec = np.arange(0,lenstorm,60)
timemin = np.divide(timesec,60)  # s --> min

# calcul du nombre d'eclairs par minute
hist, bin1 = np.histogram(time,bins=timesec)


# 3.2 Histogramme nb de segments / eclair
seg_ord = np.sort(nbseg)  # on ordonne les segments par ordre croissant
bin_seg = np.arange(0,500,20)
hist_seg, bin_seg = np.histogram(seg_ord,bins=bin_seg)
# il faut prendre la valeur moyenne du bin pour tracer l'histogramme
bin2 = bin_seg + 10
binplot1 = bin2[0:len(bin2)-1]

# 3.3 Histogramme champ de declencement
etrig_ord = np.sort(etrig)  # on ordonne les segments par ordre croissant
bin_etrig = np.arange(0,300,10)
hist_etrig, bin_etrig = np.histogram(etrig_ord,bins=bin_etrig)
# il faut prendre la valeur moyenne du bin pour tracer l'histogramme
bin3 = bin_etrig + 5
binplot2 = bin3[0:len(bin3)-1]

# 3.4 Histogramme altitude de declenchement
ztrig_ord = np.sort(ztrig)  # on ordonne les segments par ordre croissant
bin_ztrig = np.arange(0,20,0.5)
hist_ztrig, bin_ztrig = np.histogram(ztrig_ord,bins=bin_ztrig)
# il faut prendre la valeur moyenne du bin pour tracer l'histogramme
bin4 = bin_ztrig + 0.25
binplot3 = bin4[0:len(bin4)-1]

# 3.4 Histogramme charge positive neutralisee
qpos_ord = np.sort(neutpos)  # on ordonne les segments par ordre croissant
bin_qpos = np.arange(0,20,0.5)
hist_qpos, bin_qpos = np.histogram(qpos_ord,bins=bin_qpos)
# il faut prendre la valeur moyenne du bin pour tracer l'histogramme
bin5 = bin_qpos + 0.25 #10
binplot4 = bin5[0:len(bin5)-1]


# 4. Traces

fig = plt.figure(layout='constrained', figsize=(12, 12))

gs = GridSpec(3, 2, figure=fig)

fontsize_axes = 12

# trace de la frequence d'eclairs
ax0 = fig.add_subplot(gs[0,:])

# il faut inserer une valeur 0 a l'instant 0
zero = np.array([0])
histo = np.concatenate((zero, hist))

ax0.plot(timemin, histo)
ax0.set_xlabel('Time (min)', fontsize=fontsize_axes)
ax0.set_ylabel('Flash frequency (fl/min)', fontsize=fontsize_axes)
ax0.grid(True, linestyle='-.')  # ajout de la grille
ax0.set_xlim(0, 60)
ax0.set_ylim(0, 40)
ax0.text(2, 35, str(len(nb))+' éclairs', fontsize=12, fontweight='bold', color='red')

fig.suptitle('KW78 - '+seg)

# trace des histogrammes

ax1 = fig.add_subplot(gs[1,0])
ax1.bar(binplot1, hist_seg, width=15, edgecolor="white", linewidth=0.7) #, label=bar_labels, color=bar_colors)
ax1.set_ylabel('Frequency', fontsize=fontsize_axes)
ax1.set_xlabel('Nb of segments per flash', fontsize=fontsize_axes)
titre = str("{:.2f}".format(np.mean(nbseg)))+' $\pm$ '+str("{:.2f}".format(np.std(nbseg)))+' segments/eclair'
ax1.set_title(titre, fontsize=12, fontweight='bold', color='red', loc='left')

ax2 = fig.add_subplot(gs[1,1])
ax2.bar(binplot2, hist_etrig, width=7.5, edgecolor="white", linewidth=0.7) #, label=bar_labels, color=bar_colors)
ax2.set_xlabel('Triggering electric field (kV/m)', fontsize=fontsize_axes)
ax2.set_ylabel('Frequency', fontsize=fontsize_axes)
titre = str("{:.2f}".format(np.mean(etrig)))+' $\pm$ '+str("{:.2f}".format(np.std(etrig)))+' kV/m'
ax2.set_title(titre, fontsize=12, fontweight='bold', color='red', loc='left')

ax3 = fig.add_subplot(gs[2,0])
ax3.bar(binplot3, hist_ztrig, width=0.4, edgecolor="white", linewidth=0.7) #, label=bar_labels, color=bar_colors)
ax3.set_ylabel('Frequency', fontsize=fontsize_axes)
ax3.set_xlabel('Triggering altitude (km)', fontsize=fontsize_axes)
titre = str("{:.2f}".format(np.mean(ztrig)))+' $\pm$ '+str("{:.2f}".format(np.std(ztrig)))+' km'
ax3.set_title(titre, fontsize=12, fontweight='bold', color='red', loc='left')

ax4 = fig.add_subplot(gs[2,1])
ax4.bar(binplot4, hist_qpos, width=0.4, edgecolor="white", linewidth=0.7) #, label=bar_labels, color=bar_colors)
ax4.set_xlabel('Positive charge neutralized (C)', fontsize=fontsize_axes)
ax4.set_ylabel('Frequency', fontsize=fontsize_axes)
titre = str("{:.2f}".format(np.mean(neutpos)))+' $\pm$ '+str("{:.2f}".format(np.std(neutpos)))+' C/eclair'
ax4.set_title(titre, fontsize=12, fontweight='bold', color='red', loc='left')


# 5. Sauvegarde de la figure

fig.savefig(path_out+'Fig_kw78_elec_flash_'+seg+'.png')
plt.show()
