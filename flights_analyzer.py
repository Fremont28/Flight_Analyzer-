#This script compares the flight patterns of two airports 
#comparing where each airport flies to the most and the cities
#that both airports have access points to as well as unique destinations 

from bs4 import BeautifulSoup 
import requests
import numpy as np 
import pandas as pd 
import re 
import csv 
import matplotlib.pyplot as plt
import seaborn as sns 
import math 


class Flights_Analysis(): 
    def __init__(self):
        pass

    def compare_flights(self,csv1,csv2):
        df1=pd.read_csv(csv1)
        df2=pd.read_csv(csv2) 
  
        df1_ori=df1['origin'] 
        df1_ori=pd.DataFrame(df1_ori)
        df1_ori=df1_ori.apply(pd.value_counts) 
        df1_ori=pd.DataFrame(df1_ori)
        df1_ori.reset_index(level=0,inplace=True)

        df2_ori=df2['origin']
        df2_ori=pd.DataFrame(df2_ori)
        df2_ori=df2_ori.apply(pd.value_counts) 
        df2_ori=pd.DataFrame(df2_ori)
        df2_ori.reset_index(level=0,inplace=True)

        #flights with the same origin 
        same=pd.merge(df1_ori,df2_ori,on="index")
        print(same)
        city1_same=sum(same['origin_x'])/sum(df1_ori['origin'])*100
        city2_same=sum(same['origin_y'])/sum(df2_ori['origin'])*100
        print(city1_same)
        print(city2_same)

        same.to_csv("misma.csv")
        df1_ori.to_csv("df1.csv")
        df2_ori.to_csv("df2.csv")

        #different (unique) destinations 
        df1_dest=df1_ori['index'].tolist()
        df2_dest=df2_ori['index'].tolist()

        df1_only=[]
        for i in df1_dest:
            if i not in df2_dest:
                df1_only.append(i)

        df2_only=[]
        for i in df2_dest:
            if i not in df1_dest:
                df2_only.append(i)
        
        print(df1_only)
        print(df2_only)

        #percent different for cada aureopuerto
        unq_df1=(len(df1_only)/len(df1_ori))*100
        unq_df2=(len(df2_only)/len(df2_ori))*100 

        print(unq_df1)
        print(len(df1_dest))
        print(unq_df2)
        print(len(df2_dest))

        #how are flights distributed (first city)
        fig=plt.figure()
        fig,ax=plt.subplots(figsize=(12,8))
        plt.hist(df1_ori['origin'],bins=10)
        plt.xlabel('Percentage of Flights From Unique Airports')
        plt.ylabel('Frequency')
        plt.title('Flight Distribution Histogram')
        plt.grid(True)
        plt.savefig("tostones1.png")

        #how are flights distributed (second city)
        fig=plt.figure()
        fig,ax=plt.subplots(figsize=(12,8))
        plt.hist(df2_ori['origin'],bins=10)
        plt.xlabel('Percentage of Flights From Unique Airports')
        plt.ylabel('Frequency')
        plt.title('Flight Distribution Histogram')
        plt.grid(True)
        plt.savefig("tostones2.png")

    def overall_flights(self,csv1): 
        ov=pd.read_csv(csv1)
        origin=ov['origin']
        origin=pd.DataFrame(origin)
        origin=origin.apply(pd.value_counts)
        origin=pd.DataFrame(origin)
        origin.to_csv("fried_pollo.csv")

        #origins that fly frequently into other cities 
        oc=ov.groupby(['origin']).count()['aircraft']
        oc=pd.DataFrame(oc)
        air_sum=sum(oc['aircraft'])
        oc['sum']=air_sum 
        oc['pct']=(oc['aircraft']/oc['sum'])*100 

        ho=oc[oc.pct>2.87] #baseline mean 
        ho=pd.DataFrame(ho)
        print(ho)

        mean_origin=oc['pct'].mean()
        print(mean_origin)
        sd_origin=oc['pct'].std() #baseline standard deviation 
        print(sd_origin)

if __name__== '__main__':
    coca=Flights_Analysis()
    coca.compare_flights("sau_paulo.csv","bsas.csv")
    coca.overall_flights("bsas.csv")
