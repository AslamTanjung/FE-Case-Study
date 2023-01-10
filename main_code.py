import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import zipfile as zip

######################################################################################
## FUNCTIONS #########################################################################

def read_zip(filename):
    z = zip.ZipFile(filename, "r")
    df = pd.read_csv(z.open(z.namelist()[0]))
    z.close()
    return df

######################################################################################
## INLEZEN ###########################################################################
df = read_zip("Data/2011.zip")
