# -*- coding: utf-8 -*-
"""
Opening Files and Reading Data
"""

# The open() function is the basic ticket to reading in and accessing data
# This function returns a file handler. We usually want to open and 
# read data from files, and sometimes write data to them.  It is good
# practice to close() the files after you're done working with them

# Note - there are other more concise and advanced ways to do this, and we'll
# get to those in a bit, but let's start from the basics

# You'll need to point at this file location on your directory
# For me, this is how I'm accessing the file. I'm listing out the directory
# explicitly so you can see how yours may look
import os
os.getcwd()
os.chdir("C:/Users/sbkel/Dropbox/UNR_Teaching/GEOG_407_607/Lecture/PythonModules/Data")
NVData = open("NV_Cities.txt","r")

# Read the lines of the file to see its contents
# The output is a sequence of strings
linelist = []
for line in NVData:
    print (line)
    linelist.append(line)
print (linelist)
NVData.close() # Closes the file

# Looping through lines helps to see what's there, but use the .read() command
NVData = open("NV_Cities.txt","r")
NVdata = NVData.read()
# Each new line in the file is desginated by the \n character
NVDataSplit = NVdata.split('\n')
# Notice that it returns a single string
NVDataSplit[1]
NVData.close()

# We can conditionally read in lines, too. This one gets at the first row
NVData = open("NV_Cities.txt","r")
for line in NVData:
    if line.startswith('City') :
        continue # In our example, we may only want the data, not the header
    else:
        print (line)
NVData.close()

# Access the city names and put them in a list of their own 
#Use the 'r' - read command, then leverage what we know about the \n and \t characters
txtfile = open("NV_Cities.txt","r")
cityname = []
for line in txtfile:
    sline = line.strip('\n')
    row = sline.split('\t')
    city = row[0]
    cityname.append(city)
cityname.pop(0)
print (cityname)
txtfile.close() 
    
# Access the csv module for more functions specific to csv files
# More elegantly(?)...but same output as above
import csv
cities = []
with open("NVCities.csv", "r") as csvfile:
    reader = csv.reader(csvfile)
    RowNum = 0
    for line in reader:
        if RowNum >= 1:
            cities.append(line[0])
        RowNum = RowNum + 1
print (cities)
    
# Using the CSV writer
with open("Other_Cities.csv", "w",newline='') as csvfile:
    NVwriter = csv.writer(csvfile, delimiter=',')
    NVwriter.writerow(['City','x','y','Pop'])
    NVwriter.writerow(['Ely',-114.8742,39.2533,3968])
    NVwriter.writerow(['Mesquite',-114.0672,36.8055,18541])
    NVwriter.writerow(['Fernley',-119.2518,39.6080,19863])
    NVwriter.writerow(['Fallon',-118.7770,39.4749,8409])

######### Working with Pandas #############        

import matplotlib.pyplot as plt
import pandas as pd
# This imports the pandas library. This comes along with the Anaconda Distribution,
# and as we'll see, makes working with data much simpler than using some of the 
# more basic ways to open files than those we just worked with. 'Pandas' is shorthand for
# 'Python Data Analysis Library' - it creates a Python object with rows and columns in a dataframe
# from data you import (like .csv or .txt files). For those used to working in 
# R, this will seem fairly familiar.

d = pd.read_csv('Washoe_County.csv')
elev = d['Elevation']
prom = d['Prominence']

# Make a histogram!
legend = ['Elevation', 'Prominence']
bins = [400,1000,2000,3000,4000,5000,6000,7000,8000,9000,10000,11000]
plt.hist([elev, prom], bins,color=['orange', 'green'])
plt.xlabel("Elevation & Prominence")
plt.ylabel("Frequency")
plt.ylim(0,150)
plt.xlim(0,11000)
plt.legend(legend)
plt.title('Washoe County Summits')
plt.show()

# This is part of your homework file for this week. If you need a fresh copy,
# you can always copy/paste from here