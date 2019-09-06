
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
#need to work out correct particle distributions

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


it=5

Xi=d[it]['xi']
Yi=d[it]['yi']
Ui=d[it]['pxi']
Vi=d[it]['pyi']



Xe=d[it]['xe']
Ye=d[it]['ye']
Ue=d[it]['pxe']
Ve=d[it]['pye']


print(Xe.size)

for it in range(0,Xe.size,5):
	print(Xe[it],Ye[it],Ue[it],Ve[it])
#	print
#print(np.shape(Xe),np.amax(Xe),np.amin(Xe))
#print(np.shape(Ye))
#print(np.shape(Ue))
#print(np.shape(Ve))


fig, ax = plt.subplots()
q = ax.quiver(Xe, Ye, Ue, Ve)
ax.quiverkey(q, X=0.3, Y=1.1, U=10,
             label='Quiver key, length = 10', labelpos='E')

plt.show()





# In[ ]:

#cv2.waitKey(0)
#show()


#input a number to end
n=int(input('n'))

