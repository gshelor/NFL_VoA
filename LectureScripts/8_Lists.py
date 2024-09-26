# -*- coding: utf-8 -*-
"""
List Data Examples and Operations
"""
# List Data are NOT immutable, unlike string data
testlist = [10,11,13,7,8,3]
print (testlist)
print (testlist[2])

for i in testlist:
    print(i)

a = [10,11,13,7,8,3]
b = [17,10,5,4,9,15]

# Concatenating lists
c = a + b
print (c)
  
# Review the allowable operations for lists
dir(c)

# Here is a major tool in your toolkit and it's made some 
# appearance already - setting an empty list, which you add items to
mylist = []
# Another way
myotherlist = list()
# Now add elements to it. They will stay in the order in which they are added
# New elements go to the end of the list
myotherlist.append('Two Step')
myotherlist.append(41)

# There are a number of built-in functions that apply to lists
print (len(c))
print (max(c))
print (min(c))
print (sum(c))
print (sum(c)/len(c))

# Working with lists
NVPopulations2017 = [54745,24230,2204079,48309,52649,850,1961,16826,5693,5223,54122,4457,44202,6508,4006,460587,9592]
NVCounties = []
NVcountynames = " Washoe, Clark, Lyon, Carson City, Pershing, Douglas, White Pine, Nye, Humboldt, Lincoln, Esmeralda, Mineral, Elko, Storey, Eureka, Churchill, Lander"
NVList = NVcountynames.split(",")
NVList.sort()

# Add items to an empty list
for i in NVList:
    x = i.strip()
    NVCounties.append(x)
print ("In List Form(!) - that's Nevada!", NVCounties)

# A more compact way to add items to a list
NVCounties = [i.strip() for i in NVList]


NVCounties2 = []
#####
# Add items to an empty list
for i in NVList:
    x = i.strip()
    if x != 'Carson City':
        NVCounties2.append(x + " County")
    else:
        NVCounties2.append(x)
print ("In List Form(!) - that's Nevada!", NVCounties2)