import pandas as pd
import glob
import os

import math
from collections import Counter




def calcDescriptivInfo(df):
    key_liste = sorted(df.stack().value_counts().index.tolist())
    
    numSequenzes = 0
    totalLength = 0
    for index, row in df.iterrows():
        numSequenzes += 1
        # Wir wandeln die Zeile in eine Liste um und entfernen leere Werte (NaN)
        werte = row.dropna().tolist()
        #TODO man könnte eine leere liste abfangen
        totalLength += len(werte)
    
    print(f"Anzahl States: {len(key_liste)}")
    print(f"Anzahl Sequences: {numSequenzes}")
    print(f"Total Length: {totalLength}")
    



# Pfad zum Verzeichnis ('.' bedeutet aktuelles Verzeichnis)
path = './' 
# Findet alle Dateien, die auf .csv enden
files = glob.glob(os.path.join(path, "*.csv"))

# Liste, um die einzelnen DataFrames zu speichern
dataframes = []

for f in files:
    # Dateien mit deinen Spezifikationen laden
    temp_df = pd.read_csv(f, sep=';', header=None)
    
    # Optional: Dateiname hinzufügen, damit man weiß, woher die Daten kommen
    
    # tupel aus dateinamen und daterFrame
    dataframes.append((os.path.basename(f),temp_df))
    #print(f"Datei geladen: {f}")

    #todo entfernen um alle csv dateien einzulesen
    #break




for (name,df) in dataframes:
    
    print(f"=== Analysiere: {name} ===")
    calcDescriptivInfo(df)
    
