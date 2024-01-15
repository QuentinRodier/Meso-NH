from pylab import *
import re
import numpy as np
from matplotlib.colors import *
from matplotlib.ticker import IndexLocator,FixedLocator
import os

class svgconv:

    "Converts svg files from http://soliton.vm.bytemark.co.uk/pub/cpt-city/ into a Python Colormap using the LinearSegmentedColormap function. The steps are: read the svg file and retrieve the colors. Create a dictionnary similar to default matplotlib (cf _cm.py). Interpolates linearly on 256 colors. It can also take as an input  numpy array of dimension N*3 (linear interpolation) or a list of color string (linear interpolation). smooth option determines wether we create a nicely gradiented colormap or wether we keep the rough gradients on"

    def __init__(self, cmapname,smooth=True):

        #dirin = os.sep.join([os.path.dirname(__file__), 'SVGFILES'])+os.sep
        dirin = os.path.dirname(__file__)+os.sep

        "Read the file, retrieve colors and offsets"
        
        if type(cmapname) is str:

            offset=[]
            col=[]
            
            f=open(dirin+cmapname,'r')
            lines=f.readlines()
            regexp=re.compile('stop-color')
            for l in lines:
                if(regexp.search(l)):
                    sss=l.split()
                    soff=list(sss[1])
                    toto=''.join(soff[8:-2])
                    offset.append(float(float(toto)))
                
                    scol=list(sss[2])
                    scol=scol[16:-2]
                    scol=''.join(scol)
                    xxx=scol.split(',')
                    colint=[int(xxx[0]),int(xxx[1]),int(xxx[2])]
                    col.append(colint)

            # Correct two adjacent offset (two identical offset means that we have to change the former one)
            coltest=[0,0,0]
            col=np.array(col)/255.
            offset=np.array(offset)
    
            """for p in range(0,len(offset)-1):
                if offset[p]==offset[p+1]:
                    offset[p]=offset[p]
                else:
                    offset[p]=offset[p]
            """
            
            offset=offset/100.
            
            self.offset=offset
            self.col=col
            self.cmapname=cmapname

        elif ((type(cmapname) is list) or (type(cmapname) is np.ndarray)):

            if(type(cmapname) is np.ndarray):
                colshape=cmapname.shape
                if(colshape[0]==3):
                    cmapname=np.transpose(cmapname)
                    
            col=[]
            offset=[]
            for v in cmapname:
                rgb = colorConverter.to_rgb(v)
                col.append(rgb)
            col=np.array(col)
            colshape=col.shape
            if smooth==True:
                offset=np.linspace(0,1,len(col[:,0]))
                self.offset=offset
                self.col=col
                self.cmapname='toto'
            else:
                offset=np.linspace(0,1,len(col[:,0])+1)
                N=len(offset)         
                offint=[]
                colint=[]
                for p in range(0,N-1):
                    colint.append(col[p,:])
                    offint.append(offset[p])
                    colint.append(col[p,:])
                    offint.append(offset[p+1])
                colint=np.array(colint)
                offint=np.array(offint)
                offint[0]=0
                offint[-1]=1
                self.offset=offint
                self.col=colint
                self.cmapname='toto'                 
        
    def makesegment(self):

        "Create the dictionnary"

        red=[]
        green=[]
        blue=[]

        for p in range(0,len(self.offset)):
            red.append((self.offset[p],self.col[p,0],self.col[p,0]))
            green.append((self.offset[p],self.col[p,1],self.col[p,1]))
            blue.append((self.offset[p],self.col[p,2],self.col[p,2]))

        dicto={'red':   red,
              'green': green,
              'blue':  blue}

        return dicto

    def writesegtofile(self,foutname='colors.txt'):

        "Write the dictionnary in a file, similarly with the _cm.py file. Can be used to create a user _cm.py file"

        dict=self.makesegment()
        
        f=open(foutname,'a')
        f.write('\n')

        st='_'+self.cmapname+'={\n'
        f.write(st)
    
        st='\'red\'  :  ('
        f.write(st)
        red=dict['red']
        for p in range(0,len(red)):
            inte=red[p]
        if(p!=len(red)-1):
            st='(%f,%f,%f),'%(inte[0],inte[1],inte[2])
            f.write(st)
        else:
            st='(%f,%f,%f)),'%(inte[0],inte[1],inte[2])
            f.write(st)

        f.write('\n')

        st='\'green\'  :  ('
        f.write(st)
        red=dict['green']
        for p in range(0,len(red)):
            inte=red[p]
            if(p!=len(red)-1):
                st='(%f,%f,%f),'%(inte[0],inte[1],inte[2])
                f.write(st)
            else:
                st='(%f,%f,%f)),'%(inte[0],inte[1],inte[2])
                f.write(st)

        f.write('\n')

        st='\'blue\'  :  ('
        f.write(st)
        red=dict['blue']
        for p in range(0,len(red)):
            inte=red[p]
            if(p!=len(red)-1):
                st='(%f,%f,%f),'%(inte[0],inte[1],inte[2])
                f.write(st)
            else:
                st='(%f,%f,%f)),'%(inte[0],inte[1],inte[2])
                f.write(st)

        f.write('\n')

        st='\'offset\'  :  ('
        f.write(st)
        for p in range(0,len(red)):
            inte=red[p]
            if(p!=len(red)-1):
                st='(%f),'%(self.offset[p])
                f.write(st)
            else:
                st='(%f))}'%(self.offset[p])
                f.write(st)
        f.write('\n')

        st='datadnico[\''+self.cmapname+'\']=_'+self.cmapname
        f.write(st)

        f.write('\n')
    
    def makecmap(self,N=256,reverse=False,writeout=False,foutname='colors.txt'):

        "Create the colormap that will be given in the contourplot. N=256 by default (number of points in which the interpolation is done). No writting by default"

        namelist=list(self.cmapname)
        for p in range(0,len(namelist)):
            if namelist[p]=='-':
                namelist[p]='_'
        cmapname=''.join(namelist)
        cmapname.replace('-','_')

        if reverse:
            print(self.col.shape)
            self.col=self.col[::-1,:]
            self.offset=1-self.offset[::-1]
        
        dict=self.makesegment()
    
        if writeout:
            if cmapname not in datadnico:
               self.writesegtofile(dict,cmapname,offset,foutname=foutname)
            
        cmap=LinearSegmentedColormap('toto',dict,N=N)
    
        return cmap

    def create_lbticks(self,levels):

        "This function permits to define the ticks when unsing irregular colormap with few levels (njavgcntyq.svg for example)"
        
        lmin=levels[0]
        dl=levels[1]-lmin
        lout=[lmin]
        for p in range(1,len(self.offset)):
            lout.append(lmin+self.offset[p]*dl)
        lout=np.unique(np.array(lout))
        
        return lout
