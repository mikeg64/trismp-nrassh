
# coding: utf-8

# In[44]:

import glob
import h5py as h5
import numpy as np
import os
import sys

from pylab import *


#import cv2




from matplotlib import use
use('Agg')
from matplotlib import pyplot as plt

#get_ipython().magic('matplotlib inline')

plt.ion()
# In[2]:


#basedir='/mnt/xdrive/Users/cs1mkg/temp/' #For linux desktop remote mount to research data store
basedir='/shared/sp2rc2/Users/cs1mkg/temp/' #For automounted research data storage from ShARC
output='output-th85ph90'

files=sorted(glob.glob(basedir+output+'/flds.tot*'))
prtfiles=sorted(glob.glob(basedir+output+'/prtl.tot*'))


#files=sorted(glob.glob('output/flds.tot*'))
#prtfiles=sorted(glob.glob('output/prtl.tot*'))
# go through the files
start=0
end=len(files)
interval=5
global d
d=[]


# In[3]:

for filenum in range(start,end,interval):
    print "reading",files[filenum]
    f = h5.File(files[filenum],"r")
    f1 = h5.File(prtfiles[filenum],"r")
    dict={'bz':np.squeeze(f['bz']),'dens':np.squeeze(f['dens']),
          'v3x':np.squeeze(f['v3x']),'v3xi':np.squeeze(f['v3xi']),
          'pxi':np.squeeze(f1['ui']),'pyi':np.squeeze(f1['vi']),
          'pzi':np.squeeze(f1['wi']),'pxe':np.squeeze(f1['ue']),
          'pye':np.squeeze(f1['ve']),'pze':np.squeeze(f1['we']),
          'xi':np.squeeze(f1['xi']),'xe':np.squeeze(f1['xe'])}    
    d.append(dict)

itstep=1
it=2

#compute average density over y-direction for each time step
rhoavit=np.zeros([40,652])
dtemp=np.zeros([130])

for it in range(0,39,itstep):
    dens=d[it]['dens']
    for ix in range(0,651,1):
        for iy in range(0,129,1):
            dtemp[iy]=dens[iy][ix]    
        rhoavit[it][ix]=np.mean(dtemp)


#compute average bz over y-direction for each time step
bzavit=np.zeros([40,652])
bztemp=np.zeros([130])

for it in range(0,39,itstep):
    bz=d[it]['bz']
    for ix in range(0,651,1):
        for iy in range(0,129,1):
            bztemp[iy]=bz[iy][ix]    
        bzavit[it][ix]=np.mean(bztemp)


itstep=1
it=39


#
# Plot density field
#

fig4,ax4 = plt.subplots(2,2,num=1)

#ax1[0].imshow(rho)
#ax1[1].imshow(bz)

cs1=ax4[0][0].contourf(d[it]['bz'])
ax4[0][0].set_xlabel('x')
ax4[0][0].set_ylabel('y')
ax4[0][0].set_title('Magnetic Field (Bz): for iteration:'+str(it))
cbar1 = fig4.colorbar(cs1,ax=ax4[0][0],shrink=0.9)



cs2=ax4[0][1].contourf(d[it]['dens'])
ax4[0][1].set_xlabel('x')
ax4[0][1].set_ylabel('y')
ax4[0][1].set_title('Density: for iteration:'+str(it))
cbar2 = fig4.colorbar(cs2,ax=ax4[0][1],shrink=0.9)

cs3=ax4[1][0].contourf(bzavit)
ax4[1][0].set_ylabel('time')
ax4[1][0].set_xlabel('Bz')
ax4[1][0].set_title('Bz Averaged over y')
cbar3 = fig4.colorbar(cs3,ax=ax4[1][0],shrink=0.9)

cs4=ax4[1][1].contourf(rhoavit)
ax4[1][1].set_ylabel('time')
ax4[1][1].set_xlabel('x')
ax4[1][1].set_title('Density Averaged over y')
cbar4 = fig4.colorbar(cs4,ax=ax4[1][1],shrink=0.9)

# In[ ]:

#cv2.waitKey(0)
#show()


#input a number to end
n=int(input('n'))

