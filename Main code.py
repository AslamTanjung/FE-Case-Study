import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import zipfile as zip
import os

######################################################################################
## FUNCTIONS #########################################################################

def read_zip(filename):
    z = zip.ZipFile("Data/" + filename, "r")
    df = pd.read_csv(z.open(z.namelist()[0]), header = 0)
    z.close()
    return df

######################################################################################
## INLEZEN ###########################################################################
files = os.listdir('Data')
li = []   
for filename in files:
    df = read_zip(filename)
    li.append(df)

disney = df = pd.concat(li)

######################################################################################
## MANIPULEREN #######################################################################
## P2
disney = disney[disney["PRICE"] != 0.00]

## P3
disney = disney[disney['EX'] == "T"]

## T1
disney = disney[disney['CORR'] == 0]

## T2: E, F have a problem
disney = disney[disney['COND'].isin(['@', 'F', '6'])]

## T3: gebruik mean
disney = disney.groupby(["DATE", "TIME"]).agg({'SIZE': 'sum', 'PRICE':['median']})