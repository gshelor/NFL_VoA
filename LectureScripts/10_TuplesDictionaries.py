# -*- coding: utf-8 -*-
"""
Tuples and Dictionaries
Intro to Python Workshop
"""

######## Tuples ############

# Tuples are another common data type, and really well-suited to geographic data storage,
# Especially when storing (x,y) coordinate pairs. Their uses of course extend beyond that

# Tuples are readily identifiable with the () designations,
# while lists are designated by []

# Like lists, they are a sequence of elements and can be indexed starting from pos 0

CarsonRange = ('Rose','Houghton','Tamarack','Relay')
print(CarsonRange[1])
CarsonRange.index('Rose')

# Those all look familiar, but here is the difference:
# Tuples are immutable - meaning you cannot alter them (like strings)
CarsonRange = ('Rose','Houghton','Tamarack','Relay')
CarsonRange[2] = 'Snowflower'
# This line will trip an error message
CR = list(CarsonRange)

# Some of the functions we love with lists are not usable here
# .sort(), .append() won't work - the list of allowable operations is shorter
# You'll see only .count(), and .index() as available

# So - why use them?  For one, they aren't modifiable, so that makes them
# simpler, and require less memory to store

# Tuples can be set in the same way as variable names
(a,b,c,d) = ('Rose','Houghton','Tamarack','Relay')
print (b)

######### Tuples Example ###########
import numpy as np

# Working with a list of tuples - lat/long coordinate pairs
latlongpairs = [(-119.453,35.9563),(-118.7845,35.34565),(-116.3453,36.7532)]

long = []
lat = []
for i in latlongpairs:
    print (i)
    long.append(i[0])
    lat.append(i[1])
# Then call the lists
print (np.mean(long))
print (np.mean(lat))

# Then put them back together
latlong_again = list(zip(long,lat))
for i in latlong_again:
    print (i)

######## Dictionaries ##############

# Dictionaries are a format you may come across when accessing data from
# external locations. This format allows us to store multiple types of values with a 
# single object. Lists do that, too, but they are always accessed sequentially
# Dictionaries contain a label for a value or values
# They allow us to index and access values quickly

# In lists, there is an index that is based on the position in the list,
# But in dictionaries, there isn't necessarily an order, the index is made
# in a dictionary with a "lookup tag" - they use keys instead of numbers

mtns = dict()
mtns['elevation'] = 7533
mtns['prominence'] = 1813
print (mtns)
mtns['prominence'] = 1853
mtns['prominence']
#Can also create a new one by:
mtns = {}

# You'll see that dictionaries use {} as designators, and use a key:value pair
gc_summits = {'brahma temple': 7551,'vishnu temple': 7533, 'coronado butte': 7110}

# Searching for items in dictionaries
print ('vishnu temple' in gc_summits)
print ('temple' in gc_summits)

# Accessing items in dictionaries with definite loops
# Two iteration variables to print (key,value) pairs
for i,j in gc_summits.items():
    print (i,j)

# You can convert to list, but with some additional considerations 
list(gc_summits)
# Access items in a dictionary
print (gc_summits.keys())
print (gc_summits.values())
# You will see this generates tuples (key,value)
print (gc_summits.items())


# Populating a dictionary with counts. Here, the 'county' is the key, and values will be the population values
NVcounties = " Washoe, Clark, Lyon, Carson City, Pershing, Douglas, White Pine, Nye, Humboldt, Lincoln, Esmeralda, Mineral, Elko, Storey, Eureka, Churchill, Lander"
NVPopulations2017 = [54745,24230,2204079,48309,52649,850,1961,16826,5693,5223,54122,4457,44202,6508,4006,460587,9592]
NVList = NVcounties.split(",")
NV = [i.strip() for i in NVList]
NV.sort()

# Converted a zipped object to a dictionary
zipNV = zip(NV,NVPopulations2017)
dictNVPop = dict(zipNV)

# Set unique ID key for NV counties, sorted alphabetically
# The i+1 ensures that the ID starts at 1, not 0
dictNV = {i+1 :NV[i] for i in range (0,len(NV))}

# Sort by descending order of population, by accessing the Values
# The lambda function passes a key function that returns the element 
# in position [1] - in our case, the population value, reverse=true ensures descending order
sorted(dictNVPop.keys())   
CountyTuples = sorted(dictNVPop.items() , reverse=True, key=lambda x: x[1])
for county in CountyTuples:
    print(county[0],county[1]) 
