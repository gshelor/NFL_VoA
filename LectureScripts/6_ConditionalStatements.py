# -*- coding: utf-8 -*-
"""
Conditional Statements and Flow Control
"""
# Comparison Operators
# We use Boolean Expressions to help control flow
# In Python, here are the ones you can use:
# <  Less than
# <= Less than or Equal to
# == Equal to ('=' is used for assignment)
# >= Greater than or Equal to
# > Greater than
# != Not equal

# We often combine these with conditional statements
# These are based on one-way, two-way, and multi-way decisions

# if, else, and elif are used to operationalize these
# if you have two choices - use if/else

### Else ###
# in Python, 'else' is a bit of a 'catch all' for when a condition is not met,
# but must be preceded by an 'if' test.  And can only be called once

## Elif ##
# shorthand for 'else if'. Used when choices > 2.  These are handy: they check
# if multiple conditions are true, executing a block of code once one of the
# conditions evaluates to true. And, you can call as many of these as you want

# Simple Flow Control
x = input('Enter your favorite number:')
if float(x) < 5:
    print ('Go bigger or go home')
elif float(x) < 7:
    print ('Keep going')
elif float(x) < 9:
    print ('Getting better')
else:
    print ('In double digits, nicely done')
#There is a logical error in this code, though - can you find an fix it?

    
#########
# Back to our example from previous section
# Let's use an 'if' statement to address the Carson City issue
# Definite Loop - Basic/Numeric
NVcounties = " Washoe, Clark, Lyon, Carson City, Pershing, Douglas, White Pine, Nye, Humboldt, Lincoln, Esmeralda, Mineral, Elko, Storey, Eureka, Churchill, Lander"
NVList = NVcounties.split(",") #String->List again, splitting on the "," character
NVList.sort() #Again sorts alphabetically
for i in NVList: # Initiate definite list with iterator variable
    x = i.strip() # set variable where spaces have been removed
        # Recall the previous module, regarding the Carson City issue
    if i.find('City') == -1: #There are many ways to fix this - try another!
        print (x, 'County')  #There are many ways to fix this - try another!
    else:
        print (x)
print ("That's Nevada!")

###########







