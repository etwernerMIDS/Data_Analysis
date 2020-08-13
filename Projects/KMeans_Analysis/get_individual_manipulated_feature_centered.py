# -*- coding: utf-8 -*-
"""
Created on Wed Jul 11 16:42:05 2018

@author: werner23
"""
#imports
import numpy as np
import seaborn as sns; sns.set(style="ticks", color_codes=True)

#Get Individual Manipulated Feature Centered
def get_individual_manipulated_feature_centered(record,sensor,bins=100):
    """Function that generates the individual centered manipulated feature 
       vector for the indicated sensor. This is done by breaking up the 
       positional VPS into bins and finding the area within each space.

    Args:
        record: The record processsed from the id in te database.
        sensor: The number (0,1,2, or 3) that indicates which sensor to use.
        bins: The number of sections in which to divide the 
            positional VPS into.

    Returns:
        The centered manipulated feature vector for the specified sensor.
    """
    
    #accesses the record's motion sensor
    ana=Analysis()
    ana.processRecord(record)   
    motion=MotionProfileV2.extract(record)
    m = motion.vpsInDistance.toArray(sensor)
    
    #initializes variables
    my_range = np.linspace(-1,25,bins)
    d = np.zeros((len(my_range),1))
    prev=0
    index=0
    
    #iterates through the linspace vector
    for i in range(0,len(my_range)):  
        cp=np.zeros((len(m),2))
        count=0
        
        #makes a copy of the values that fall within the given bin
        for j in range(0,len(m)):
            if m[j][0]+ ((25-record.motion.vehicleLength)/2-m[0][0]) >= my_range[i] and m[j][0]+ ((25-record.motion.vehicleLength)/2-m[0][0]) <= my_range[i+1]:
                cp[count][0]=m[j][0] + ((25-record.motion.vehicleLength)/2-m[0][0])
                cp[count][1]=m[j][1]
                count+=1

        #if there ARE changes within the bin (sensor switches from 0 or 1)
        if cp[0][0] != 0:
            
            #if there is ONLY ONE switch within the bin
            if cp[1][0] == 0:
                
                #if the sensor switches from 1 to 0
                if prev == 1:
                    #finds the area
                    d[index] = 1 - ((my_range[i+1] - cp[0][0])/(my_range[i+1]-my_range[i]))
                    #increments the index and updates 'prev' accordingly
                    index+=1
                    prev=cp[0][1]
                    
                #if the sensor switches from 0 to 1    
                else:
                    #finds the are
                    d[index] = ((my_range[i+1] - cp[0][0])/(my_range[i+1]-my_range[i]))
                    #increments the index and updates 'prev' accordingly
                    index+=1
                    prev=cp[0][1]
                    
            #if there are MORE than one switch within the bin        
            else:
                value=0              
                #if the sensor switches from 1 to 0 then back any number of times
                if cp[0][1] == 1:
                    #iterates through the copied matrix
                    for j in range(0,len(cp),2):
                        
                        #finds the cumulative area
                        if j+1<len(cp):
                            if cp[j+1][0] == 0 and cp[j][0] != 0:
                                value += my_range[i+1]-cp[j][0]
                                prev=cp[j][1]
                            else:
                                value += cp[j+1][0] - cp[j][0]
                                
                    #adds the total area within the bin to the vector            
                    d[index] = value/(my_range[i+1]-my_range[i])
                    index+=1
                    
                #if the sensor switches from 0 to 1 then back any number of times    
                else: 
                    #iterates through the copied matrix
                    for j in range(0,len(cp),2):
                        
                        #finds the cumulative area
                        if j+1<len(cp):
                            if j == 0:
                                value += cp[j][0] - my_range[i]
                                prev=cp[j][1]
                            elif cp[j][0] == 0 and cp[j-1][0] != 0:
                                value += my_range[i+1]-cp[j-1][0]
                                prev=cp[j-1][1]
                            else:
                                value += cp[j][0] - cp[j-1][0]
                                
                    #adds the total area within the bin to the vector             
                    d[index] = value/(my_range[i+1]-my_range[i])
                    index+=1
                                 
        #if there ARE NOT changes within the bin (sensor stays either 0 or 1)
        elif cp[0][0] == 0:
            
            #changes the 'prev' variable accordingly and increments the index 
            if prev == 0:
                d[index] = 0
                index+=1
            elif prev == 1:
                d[index] = 1
                index+=1
    
    #returns the individual sensor feature vector
    return(d)
    
    
    
    
    
    
