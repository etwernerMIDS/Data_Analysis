"""
2018 LLNL - DHS DNDO Summer Internship
File that calls the functions I made to perform each task.
@author: werner23
"""
#imports
#import numpy as np
from gov.llnl.ernie import Database
import matplotlib.pylab as plt
import seaborn as sns; sns.set(style="ticks", color_codes=True)
import pandas
from motion_sensor_plot import motion_sensor_plot
from motion_sensor_plot import motion_sensor_color_plot
from feature_vector_centered import feature_vector_centered
from sklearn.cluster import KMeans
from plotClusterFeatureVector import plotClusterFeatureVector
from clusterDistbHist import clusterDistbHist
from clusterCenterDistBarplot import clusterCenterDistBarplot
from clusterCenterDistHist import clusterCenterDistHist
from plotSamples import plotSamples
from extractFeatureVectors import extractFeatureVectors
from extractTesting import extractTestingFeatures
from inertiaAcrossNumClusters import inertiaAcrossNumClusters
from outlierRepresentation import outlierRepresentation
from generateTestTrainingSets import generateTestTrainingSets
from numSamplesPerCluster import numSamplesPerCluster
from outlierRepresentation import outlierRepresentationLogscale
from averageVPS import averageVPS
from confusionMatrix import confusionMatrix
from Fscore import Fscore
from Fscore import FscoreNormalized
from predictClusters import predictClusters


#%% Visualizations of sensor plots and the feature vector.  
print("Overall Feature Vector and Plots")
motion_sensor_plot(record_id)
motion_sensor_color_plot(record_id)
plt.plot(feature_vector_centered(record_id,bins=100))
plt.yticks([0,1])
plt.tick_params(axis='both', which='major', labelsize=16)
    
    
#%% Creates test/training sset from unmanipulated/manipulated records.
#SHOULD BE RUN ON SUPER COMPUTER - TAKES A VERY LONG TIME
generateTestTrainingSets(n_fold=0)


#%% Reads in training set and performs K-Means clustering algorithm.
training = pandas.read_csv("FinalTrainingSet.csv")
n_samples = 10000
DF = extractFeatureVectors(training,n_samples)
newFV = DF[0]
idList = DF[1]

n_clusters = 32     
kmeans = KMeans(n_clusters=n_clusters,random_state=0).fit(newFV)


#%% Cluster Distribution Histogram
print("Displays the number of clusters a unique ID is divided amongst.")

clusterDistbHist(idList,kmeans,n_clusters)         


#%% Cluster Center Distance Barplot 
print("Displays the average distance to the cluster center for each cluster.")

clusterCenterDistBarplot(kmeans,idList,newFV,n_clusters)


#%% Individual Cluster Center Distance Histogram
print("Displays the varying distance to the cluster center for a single cluster.")

clusterNumber = 4
clusterCenterDistHist(idList,kmeans,clusterNumber,newFV)


#%% Samples from the clusterCanterDistHist
print("Shows sample feature vectors from varying distance fom the cluster center.")

plotSamples(idList,kmeans,clusterNumber,newFV)


#%% Cluster with Cluster Center
print("Displays the features vectors and the cluster center for a single cluster.")

plotClusterFeatureVector(newFV,kmeans,clusterNumber)


#%% Red/Blue Image of Clusters
print("Displays the avergae VPS.")

averageVPS(kmeans,n_clusters,n_samples)  
   
    
#%% Inertia vs # Clusters Distribution  
print("Displays the progression of inertia in relation to the number of clusters.")   

inertiaAcrossNumClusters(newFV)          


#%% Outlier Representation vs # of Clusters
print("Displays the progression of outlier representation in relation to the number of clusters.")

outlierRepresentation(newFV,idList)


#%% Outlier on log scale
print("Displays the progression of outlier representation in relation to the number of clusters.")

outlierRepresentationLogscale(newFV,idList)
   

#%% Shows the distribution of samples per cluster given varying numbers of clusters.
print("Displays the number of samples per cluster in relation to different numbers of clusters.") 

numSamplesPerCluster(newFV,idList,n_samples)


#%% Shows the confusion matrix for the predicted versus actual cluster label assignment.
print("Displays the confusion matrix for the predicted vs actual cluster label.")

confusionMatrix(n_samples,n_clusters)


#%%
print("Ordered by the length of the vehicle.")

confusionMatrix(n_samples,n_clusters,byLength=True)


#%% Shows the normalized confusion matrix for the predicted versus actual cluster label assignment.
print("Displays the normalized confusion matrix for the predicted vs actual cluster label.")

confusionMatrix(n_samples,n_clusters,normalized=True)


#%%
print("Ordered by the length of the vehicle.")

confusionMatrix(n_samples,n_clusters,byLength=True,normalized=True)


#%% Computes the Fscore of both confusion matrices.
print("Computes the F score for the confusion matrix.")

F = Fscore(n_samples,n_clusters)
print(F[2])


#%%
print("Computes the F score for the normalized confusion matrix.")

Fnorm = FscoreNormalized(n_samples,n_clusters)
print(Fnorm[2])


#%% Makes predictions based on the input test set.
testing = pandas.read_csv("FinalTestingSet.csv")
features = extractTestingFeatures(testing,10000)

predictions = predictClusters(kmeans,features)


#%%











