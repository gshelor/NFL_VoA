# -*- coding: utf-8 -*-
"""
Created on Sun Sep 12 11:23:09 2021

@author: GEOG407607
"""

######### Working with Pandas and Simple Plots #############        

import matplotlib.pyplot as plt
import pandas as pd
# This imports the pandas library. This comes along with the Anaconda Distribution,
# and as we'll see, makes working with data much simpler than using some of the 
# more basic ways to open files than those we just worked with. 'Pandas' is shorthand for
# 'Python Data Analysis Library' - it creates a Python object with rows and columns in a dataframe
# from data you import (like .csv or .txt files). For those used to working in 
# R, this will seem fairly familiar.

#Make sure you know where your file is saved. You may have to 
#change the code on the line below to point it to the .csv file
d = pd.read_csv('Washoe_County.csv')
elev = d['Elevation']
prom = d['Prominence']

# Make a histogram!
legend = ['Elevation', 'Prominence']
bins = [400,1000,2000,3000,4000,5000,6000,7000,8000,9000,10000,11000]
plt.hist([elev, prom], bins,color=['orange', 'green'])
plt.xlabel("Elevation & Prominence")
plt.ylabel("Frequency")
plt.ylim(0,150,10)
plt.xlim(0,11000,500)
plt.xticks()
plt.legend(legend)
plt.title('Washoe County Summits')
plt.show()

# Make 3 changes/improvements to the graph and upload to Canvas/WebCampus at the assignment link
