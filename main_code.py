import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import zipfile as zip
import matplotlib.pyplot as plt

######################################################################################
## FUNCTIONS #########################################################################

def read_zip(filename):
    z = zip.ZipFile(filename, "r")
    df = pd.read_csv(z.open(z.namelist()[0]))
    return df

######################################################################################
## INLEZEN ###########################################################################
z = zip.ZipFile("Data/2qjral2ukjqiov77_csv.zip", "r")
df_2013 = pd.read_csv(z.open(z.namelist()[0]))
