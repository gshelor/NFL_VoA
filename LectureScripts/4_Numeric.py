# -*- coding: utf-8 -*-
"""
Working With Integer and Float Data
"""

# Python works with numeric data in specific ways
# First, it works under the PEMDAS workflow
# Second, here are your operators
# +, -, *, /, **, %

# addition, subtraction, multiplication, division, exponent, remainder
a = 5 + 11 ** 2 / (3 * 5)
print(a)

remain = 20 % 3
print(remain)

# Be aware that in Python 2.7, integer division produces an integer:
# e.g., 10/3 = 3. This changed in 3.x. You can change to an integer if you want
print(10/3)
d = (10/2)
int(d)

x = 11/7
type(x) # The type function is helpful to use to assess data type, and therefore, allowable operations
print(str(x)) # You can convert, but it may not look great
print('%.1f' % x) # This is a string formatting operator
# Another version
print('%.3f' % x)
# Multiple operators
y = 4.56788983
print('%.1f' % x, '%.2f' % y)

# Addition, subtraction, and multiplication of integers produce integers, though
y = 4 + 7
type(y)
z = 4 * 7
type(z)

# You can also get user input as follows using the input function
x = input('Enter an elevation in meters:')
feet = float(x) * 3.28084
print('Feet: ' '%.1f' % feet)

############  
# Accessing Index Values
###########

# This is a pretty handy way to get at elements within data

x = 'This is my sentence'

# Explore the index position of each characters using a simple definite loop:
for i in x:
    print(i)

print(x[0]) # This index position is the first in the ordered sequence
print(x[-1]) # This gets at the last value
print(x[4]) # Note that any character counts, including spaces

# You can also call a range of values
print(x[0:4]) # Of note - the upper boundary is NOT included in the return
print(x[-8:-1]) # Notice how the upper boundary came into play here
print(x[-8:]) # That's better - this includes all characters past the 'slice'
print(x[:4]) # Same in the other direction



    






