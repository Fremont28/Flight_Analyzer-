#This script combines seperate csv files into a single workable csv file 

import pandas as pd 

class DataFrame_Merge(): 
    def __init__(self):
        pass

    def merger(self):
        df1=pd.read_csv("df1.csv")
        df2=pd.read_csv("df2.csv")
        df3=pd.read_csv("df3.csv")
        df4=pd.read_csv("df4.csv")
        df5=pd.read_csv("df5.csv")
        df6=pd.read_csv("df6.csv")
        df7=pd.read_csv("df7.csv")
       
        df_final=pd.concat([df1,df2,df3,df4,df5,df6,df7],axis=0)
        df_final=df_final.drop_duplicates()
        df_final.to_csv("san_diego.csv") 
       
if __name__== '__main__':
    f=DataFrame_Merge()
    f.merger()

