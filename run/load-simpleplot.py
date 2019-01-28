
# coding: utf-8

# In[44]:

import glob
import h5py as h5
import numpy as np
import os
import sys




from matplotlib import use
use('Agg')
from matplotlib import pyplot

get_ipython().magic('matplotlib inline')


# In[2]:

files=sorted(glob.glob('output/flds.tot*'))
prtfiles=sorted(glob.glob('output/prtl.tot*'))
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

print len(dict['pxi'])


# In[29]:

print len(d)
de=d[39]
rho=de['dens']
bz=de['bz']
print np.shape(rho)


# In[30]:

#
# Plot density field
#
fig1,ax1 = pyplot.subplots(1,2,num=1)

ax1[0].imshow(rho)
ax1[1].imshow(bz)


# In[43]:

fig2, ax2 = pyplot.subplots()
itstep=1
it=20

cs = ax2.contourf(d[it]['bz'])
cbar = fig2.colorbar(cs)

pyplot.show()  


# In[ ]:




# In[19]:

print(files)


# In[49]:

itstep=1
it=20
rhot=np.zeros([40,652])
for it in range(0,39,itstep):
    dens=d[it]['dens']
    rhot[it]=dens[64][:]


# In[50]:

print(np.shape(rhot))


# In[51]:

fig3, ax3 = pyplot.subplots()
itstep=1
it=20

cs = ax3.contourf(rhot)
cbar = fig3.colorbar(cs)

pyplot.show()  


# In[ ]:



