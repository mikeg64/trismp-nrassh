
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

files=sorted(glob.glob(basedir+'output-th85ph90/flds.tot*'))
prtfiles=sorted(glob.glob(basedir+'output-th85ph90/prtl.tot*'))


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
          'pxi':np.squeeze(f1['ui']),'pyi':np.squeeze(f1['vi']),
          'pzi':np.squeeze(f1['wi']),'pxe':np.squeeze(f1['ue']),
          'pye':np.squeeze(f1['ve']),'pze':np.squeeze(f1['we']),
          'xi':np.squeeze(f1['xi']),'xe':np.squeeze(f1['xe'])}    
    d.append(dict)


# In[4]:

#print len(dict['pxi'])


# In[29]:

print len(d)
de=d[5]
rho=de['dens']
bz=de['bz']
print np.shape(rho)


# In[30]:

#
# Plot density field
#
#fig1,ax1 = plt.subplots(1,2,num=1)

#ax1[0].imshow(rho)
#ax1[1].imshow(bz)


# In[43]:

#fig2, ax2 = pyplot.subplots()
#itstep=1
#it=20

#cs = ax2.contourf(d[it]['bz'])
#cbar = fig2.colorbar(cs)

#plt.show()  


# In[ ]:




# In[19]:

#print(files)


# In[49]:

#itstep=1
#it=20
#rhot=np.zeros([40,652])
#for it in range(0,39,itstep):
#    dens=d[it]['dens']
#    rhot[it]=dens[64][:]


# In[50]:

#print(np.shape(rhot))


# In[51]:

#fig3, ax3 = plt.subplots()
#itstep=1
#it=20

#cs = ax3.contourf(rhot)
#cbar = fig3.colorbar(cs)

plt.show()  

#compute average density over x-direction
rhoav=np.zeros([652])
dtemp=np.zeros([130])
it=39
dens=d[it]['dens']
for ix in range(0,651,1):
    for iy in range(0,129,1):
        dtemp[iy]=dens[iy][ix]    
    rhoav[ix]=np.mean(dtemp)


#compute mean density for each time step
itstep=1
it=20
rhot=np.zeros([40,652])
for it in range(0,39,itstep):
    dens=d[it]['dens']
    rhot[it]=dens[64][:]





#compute average density over y-direction for each time step
rhoavit=np.zeros([652,40])
dtemp=np.zeros([130])

for it in range(0,39,itstep):
    dens=d[it]['dens']
    for ix in range(0,651,1):
        for iy in range(0,129,1):
            dtemp[iy]=dens[iy][ix]    
        rhoavit[ix][it]=np.mean(dtemp)



#
# Plot density field
#
it=39
fig4,ax4 = plt.subplots(2,2,num=1)

#ax1[0].imshow(rho)
#ax1[1].imshow(bz)

ax4[0][0].contourf(d[it]['bz'])
ax4[0][0].set_xlabel('x')
ax4[0][0].set_ylabel('y')
ax4[0][0].set_title('Magnetic Field (Bz)')

ax4[0][1].contourf(d[it]['dens'])
ax4[0][1].set_xlabel('x')
ax4[0][1].set_ylabel('y')
ax4[0][1].set_title('Density')

ax4[1][0].plot(rhoav)
ax4[1][0].set_xlabel('x')
ax4[1][0].set_ylabel('Density')
ax4[1][0].set_title('Density Averaged over y')



ax4[1][1].contourf(rhoavit)
ax4[1][1].set_xlabel('time')
ax4[1][1].set_ylabel('x')
ax4[1][1].set_title('Density Averaged over y')

# In[ ]:

#cv2.waitKey(0)
#show()


#input a number to end
n=int(input('n'))

