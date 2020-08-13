# -*- coding: utf-8 -*-
"""
Created on Thu Aug  9 18:58:20 2018

@author: werner23
"""
#imports
import pandas
import numpy as np
import seaborn as sns; sns.set(style="ticks", color_codes=True)
from extractFeatureVectors import extractFeatureVectors
from sklearn.cluster import KMeans
from collections import Counter
from extractTesting import extractTesting
import matplotlib.pylab as plt
import seaborn as sns; sns.set(style="ticks", color_codes=True)
import random
import seaborn as sns; sns.set(style="ticks", color_codes=True)

# Confusion Matrix
def confusionMatrix(n_samples,n_clusters,byLength = False,normalized = False):
    """Function that displays the confusion matrix for the set of clusters 
       in regards to the cluster index that an id gets mapped to.

    Args:
        n_samples: The number of recordID samples to use for clustering.
        n_clusters: The number of clusters used in the K-Means clustering.

    Returns:
        The confusion matrix that shows the predicted cluster number assignment
        versus the actual cluster number assignment for a given record and its
        multiple manipulations. 
        X axes: predicted index, the likely cluster number.
        Y axes: true index, the actual cluster number.
    """
    
    #reads the training file and extracts the manipulated records
    training = pandas.read_csv("FinalTrainingSet.csv")
    n_samples = n_samples
    DF = extractFeatureVectors(training,n_samples)
    newFV = DF[0]
    idList = DF[1]
    
    #runs the K-Means algorithm
    n_clusters = n_clusters     
    kmeans = KMeans(n_clusters=n_clusters,random_state=0).fit(newFV)
    
    #forms the data frame for the actual cluster assignments
    uniqueID = np.unique(idList)
    df = pandas.DataFrame(idList)
    df['clusterNum'] = kmeans.labels_
    df.columns = ['recordID','clusterNum']
    
    #reads the testing file and extracts the original records
    testing = pandas.read_csv("FinalTestingSet.csv")
    testDF = extractTesting(testing,int(n_samples/10))
    origFV = testDF[0]
    testID = testDF[1]
    testID = testID[testID < max(df.recordID)+1]
    origFV = origFV[:len(testID)]
    
    #finds the diffences in record ID's and deletes them
    differences = np.setdiff1d(uniqueID, testID)
    for dif in differences:
        print(dif)
        df = df.ix[~(df['recordID'] == dif)]
    
    #initializes empty arrays to append to
    clusterNumber = np.array([])
    #features = []
    recordID = np.array([])
    
    #iterates through all the ids
    for myID in idList:
        
        #filters out the bad ids
        if myID not in differences:
            print(myID)
            
            #finds the most common cluster assignment for the set of manipulations
            recordID = np.append(recordID,[myID],0)
            temp = df[df.recordID==myID]
            b = Counter(temp.clusterNum)
            value = b.most_common(1)
            clusterNum = value[0][0]
            clusterNumber = np.append(clusterNumber,[clusterNum],0)

            
    #sets the data frame for the predicted cluster assignments        
    myDF = pandas.DataFrame(recordID)
    myDF['clusterNumber'] = clusterNumber
    myDF.columns = ['recordID','clusterNumber']
           
    #######################################################################
    
    #turns the cluster assignments (predictions/actual) into series
    series1 = pandas.Series(myDF['clusterNumber'], dtype="category")
    series2 = pandas.Series(df['clusterNum'], dtype="category")
    
    #turns the series into categorical objects
    categories = np.linspace(0,n_clusters,n_clusters+1)
    s1 = pandas.Categorical(series1, categories = categories)
    s2 = pandas.Categorical(series2, categories = categories)
    
    #creates a matrix from the categorical objects
    matrix = pandas.crosstab(s1,s2)
    matrix = matrix.sort_index(ascending=True)
    
    if byLength==True:
        #order the matrix by length of the vehicle
        if n_clusters==32:
            index = [7.0,20.0,28.0,11.0,1.0,24.0,10.0,18.0,6.0,2.0,21.0,13.0,
                     31.0,5.0,22.0,29.0,26.0,14.0,3.0,8.0,15.0,16.0,0.0,17.0,9.0,25.0,
                     12.0,27.0,30.0,23.0,4.0,19.0]
            temp = matrix[[7.0,20.0,28.0,11.0,1.0,24.0,10.0,18.0,6.0,2.0,21.0,13.0,
                           31.0,5.0,22.0,29.0,26.0,14.0,3.0,8.0,15.0,16.0,0.0,17.0,
                           9.0,25.0,12.0,27.0,30.0,23.0,4.0,19.0]]
            temp2 = temp.reindex(index)
            matrix = temp2
        else:
            myDF['length'] = np.zeros(len(myDF))
            for i in range(0,n_clusters):
                cluster = myDF[myDF.clusterNumber==i]
                cluster
                uniqueID = np.unique(cluster)
                avg = 0
                count = 0
                for j in range(0,5):
                    id = random.choice(uniqueID)
                    if id >= min(idList):
                        dbc=Database()
                        record=dbc.getRecord(int(id))
                        ana=Analysis()
                        ana.processRecord(record) 
                        motion=MotionProfileV2.extract(record)
                        length = motion.vehicleLength
                        avg = avg + length
                        count = count + 1
                avg = avg/count
                avg
                myDF.loc[myDF.clusterNumber == i, 'length'] = avg
            
            df = myDF.drop_duplicates()
            t = df[['clusterNumber','length']]
            df = t.drop_duplicates()
            t = df.sort_values('length')
            
            index = t.clusterNumber.values
            
            temp = matrix[index]
            temp2 = temp.reindex(index)
            matrix = temp2
            
    if normalized == True:
        matrix = matrix.values
        
        #normalizes the matrix
        temp = np.zeros(shape=(n_clusters,n_clusters))
        row_sum = np.sum(matrix, axis=1)
        for i in range(0,n_clusters):
            for j in range(0,n_clusters):
                temp[i][j] = matrix[i][j]/row_sum[i]  
        matrix = temp
    
    #plots the conusion matrix
    cmap=plt.cm.Blues
    title = 'Confusion Matrix'
    classes = categories
    plt.imshow(matrix, interpolation='nearest', cmap=cmap)
    plt.title(title)
    plt.colorbar()
    tick_marks = np.arange(len(classes))
    plt.xticks(tick_marks, classes, rotation=45)
    plt.yticks(tick_marks, classes)
    plt.tight_layout()
    plt.ylabel('True label')
    plt.xlabel('Predicted label')
    
        
