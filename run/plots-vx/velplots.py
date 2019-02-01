
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
itstep=1

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
          'xi':np.squeeze(f1['xi']),'xe':np.squeeze(f1['xe']),
	   'yi':np.squeeze(f1['yi']),'ye':np.squeeze(f1['ye'])}    
    d.append(dict)




#compute average velocity field over y-direction for each time step
#electrons
vxavit=np.zeros([40,652])
vxtemp=np.zeros([130])

for it in range(0,39,itstep):
    v3x=d[it]['v3x']
    for ix in range(0,651,1):
        for iy in range(0,129,1):
            vxtemp[iy]=v3x[iy][ix]    
        vxavit[it][ix]=np.mean(vxtemp)



#ions
vxiavit=np.zeros([40,652])
vxitemp=np.zeros([130])

for it in range(0,39,itstep):
    v3x=d[it]['v3xi']
    for ix in range(0,651,1):
        for iy in range(0,129,1):
            vxitemp[iy]=v3x[iy][ix]    
        vxiavit[it][ix]=np.mean(vxitemp)




itstep=1
it=39



f1=d[it]['v3xi']
f2=d[it]['v3x']
#
# Plot velocity field
#
fig5,ax5 = plt.subplots(2,2,num=1)



print(np.shape(f1))
print(np.shape(f2))



cs1=ax5[0][0].contourf(f1)
ax5[0][0].set_xlabel('x')
ax5[0][0].set_ylabel('y')
ax5[0][0].set_title('Vx Field (Ions): iteration:'+str(it))
cbar1 = fig5.colorbar(cs1,ax=ax5[0][0],shrink=0.9)


cs2=ax5[0][1].contourf(f2)
ax5[0][1].set_xlabel('x')
ax5[0][1].set_ylabel('y')
ax5[0][1].set_title('Vx Field (Electrons): iteration:'+str(it))
cbar2 = fig5.colorbar(cs2,ax=ax5[0][1],shrink=0.9)

cs3=ax5[1][0].contourf(vxiavit)
ax5[1][0].set_xlabel('x')
ax5[1][0].set_ylabel('t')
ax5[1][0].set_title('Vx Field (Ions) mean over x direction')
cbar3 = fig5.colorbar(cs3,ax=ax5[1][0],shrink=0.9)

cs4=ax5[1][1].contourf(vxavit)
ax5[1][1].set_xlabel('x')
ax5[1][1].set_ylabel('t')
ax5[1][1].set_title('Vx Field (Electrons) mean over x direction')
cbar4 = fig5.colorbar(cs4,ax=ax5[1][1],shrink=0.9)



#pyplot.show()











# In[ ]:

#cv2.waitKey(0)
#show()


#input a number to end
n=int(input('n'))

