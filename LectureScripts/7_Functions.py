# -*- coding: utf-8 -*-
"""
Functions
"""
# Functions are reusable blocks of code that we call when needed
# You can name them anything you want, just don't use one of Python's 
# built-in functions (such as input(), type(), float(), etc.)
# Functions are called using the the def() reserved word

# Structure: function name, arguments, parameters, return
# an argument is a value passed into the function as its input when called
# a parameter is a variable used in function defintion
# 'return' - returns a value from executing the function and "sends back" result

# Here is a simple function, which prints three lines of a song
print ('Lithium')
def print_lyrics():
    print ("I'm so happy because today")
    print ("I've found my friends")
    print ("They're in my head") 
print_lyrics()


# Note some things about the architecture in the statement above
# You need the def statement, a name, and then parameters (as needed)
# You often will want to return values for these, as you're often passing
# them off to be used later. More relevant examples to GIS work below

x = [10,15,3,4,11,15]
y = [2,20,3,6,7,22]

def get_means(xvals,yvals):
    xcoords = (sum(xvals)/len(xvals))
    ycoords = (sum(yvals)/len(yvals))
    return xcoords,ycoords

vals = get_means(x,y)
print ('The mean center is: ' '%.1f' % vals[0],',',vals[1])


# Standard Distance Function 
# Another example of calling a library that isn't already loaded
import math

def getsd(xvals,yvals):
    meanx = sum(xvals)/len(xvals)
    meany = sum(yvals)/len(yvals)
    evalx = []
    evaly = []
    for i in xvals:
        xl = (i-meanx)**2
        evalx.append(xl)
    for i in yvals:
        yl = (i-meany)**2
        evaly.append(yl)
    z = math.sqrt((((sum(evalx)/len(evalx)) + (sum(evaly)/len(evaly)))))
    return z

sd = getsd(x,y)
print ('The standard distance is: ' '%.2f' % sd)
        