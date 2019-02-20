
#This script scrapes data from FlightAware.com and exports the scraped flight data into a csv file 

from bs4 import BeautifulSoup 
import requests
import numpy as np 
import pandas as pd 
import re 
import csv 

class Flights_Scrape:
    def __init__(self):
        pass
    def flight_metrics(self,url):
        page=requests.get(url)
        soup=BeautifulSoup(page.content)

        soup_div=soup.find_all('div')
        soup_div1=soup.find_all('td',class_=re.compile('smallrow1'))
        soup_div2=soup.find_all('td',class_=re.compile('smallrow2'))

        #page 1 (div1)
        flights=[]
        for f in soup_div1:
            print(f.get_text()) 
            flights.append(f.get_text())
        
        #page 2 (div2)
        flights1=[]
        for f in soup_div2:
            print(f.get_text()) 
            flights1.append(f.get_text())
        
        #convert list to numpy array
        flights_array=np.array(flights)
        flights_a1=flights_array.reshape(10,6) 
        df1=pd.DataFrame(flights_a1) 

        flights_array1=np.array(flights1) 
        flights_a2=flights_array1.reshape(10,6) 
        df2=pd.DataFrame(flights_a2)

        df_final=pd.concat([df1,df2],axis=0) 
        df_final1=df_final.drop_duplicates()
        df_final1.columns=['flight_no','aircraft','origin','sch_dep_time','dep_time','est_arr_time']
        df_final1.to_csv("df6.csv") 
        
if __name__== '__main__': 
    flights=Flights_Scrape()
    flights.flight_metrics("https://flightaware.com/live/airport/KSAN/enroute?;offset=120;order=estimatedarrivaltime;sort=ASC")


