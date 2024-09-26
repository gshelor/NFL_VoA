# -*- coding: utf-8 -*-
"""
Census Activity - Activity #1
"""

# This module requires finding, installing, and loading a package that is 
# not included in the original download of the Anaconda Distribution

# This is a good place to talk about setting up envrionments and how that 
# relates to installing packages

# Let's first start by setting up a new environment
# Go to the terminal (I'll show you how to do this) and enter the following
# conda create -n (yournamehere) spyder
# This may take a few minutes to run, and when prompted activate your new environment 
# conda activate (yournamehere)

# That command above will create a new environment AND install spyder again
# I could easily add package names after the environment name if I wanted to
# such as geopandas, fiona, pysal, and others that are relevant to geography

# But, for this activity, let's all work with the same package
# After your new environment has been established
# Go back to the terminal in your environment and type
# conda install -c conda-forge census
# then
# pip install matplotlib

# This particular package is available at that location. Be aware, though, 
# that many Python packages are installed using pip - Python's package installer

# About conda/pip for installing packages
# Both install packages - Pip is the Python Packaging Authority rec'd tool
# for installing packages from the Python Packaging Index (PYPI). Pip is 
# included in Python's standard library, but is specific to Python packages

# Conda installs packages that could be written in Python, or other languages

# conda is also useful for setting up specific/isolated environments, including
# different versions of Python or different packages (and versions of them)
# But, there are 150,000+ packages available on pip, and 1,500 in Anaconda...

from census import Census

Washoe = '031'

# This is my US Census API key
# You can get your own and replace in the code by going here and filling out
# this simple form: https://api.census.gov/data/key_signup.html
c = Census("7e8401ce249220dcba1ef497a3903ced266c772f")

#Question - what does the 'E' stand for in the table name
WashoeTractBike = c.acs.state_county_tract('B08006_014E', 32, Washoe, Census.ALL)
WashoeTractWalk = c.acs.state_county_tract('B08006_015E', 32, Washoe, Census.ALL)

# These data are just a sample of what can be extracted from census data
# for full documentation and look-up values, go here:
# https://api.census.gov/data.html

def WalkTotalByTract(walkdata):
    WashoeWalk = []
    for i in walkdata:
        z = list(i.items())
        WalkByTract = z[0][1]
        WashoeWalk.append(WalkByTract)
    return WashoeWalk

def BikeTotalByTract(bikedata):
    WashoeBike = []
    for i in bikedata:
        z = list(i.items())
        BikeByTract = z[0][1]
        WashoeBike.append(BikeByTract)
    return WashoeBike

Walk=WalkTotalByTract(WashoeTractWalk)
Bike=BikeTotalByTract(WashoeTractBike)

print ("The total sum (by tract) of those who walk to work in Washoe County is:", sum(Walk))
print ("The total sum (by tract) of those who bike to work in Washoe County is:", sum(Bike))

import matplotlib.pyplot as plt

n, bins, patches = plt.hist(Bike,10,density=0,facecolor='red',alpha=0.75)
plt.xlabel('# Commuters')
plt.ylabel('Count - Census Tracts')
plt.title('Frequency Distribution of Commuters')
plt.grid(True)
plt.show()





    
    
