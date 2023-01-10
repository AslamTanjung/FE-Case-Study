import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import zipfile as zip

z = zip.ZipFile("Data/2qjral2ukjqiov77_csv.zip", "r")
file = pd.read_csv(z.open(z.namelist()[0]))
