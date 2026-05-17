import pandas as pd
import glob
import os

def dictToMatrix(dict):
    matrix_df = pd.DataFrame.from_dict(dict, orient='index')

    # Optional: Spalten sortieren, damit sie die gleiche Reihenfolge wie die Zeilen haben
    matrix_df = matrix_df.reindex(sorted(matrix_df.columns), axis=1).sort_index()
    return matrix_df

def calcProb(df):
    # Alle Werte des DataFrames in eine lange Liste umwandeln und zählen
    #wert_counts = df.stack().value_counts()
    # Als Dictionary (Key-Value Pair)
    #ergebnis_dict = wert_counts.to_dict()
    #gesamt_anzahl = df.count().sum()
    #print(ergebnis_dict)

    relative_anteile = df.stack().value_counts(normalize=True).sort_index()

    # Als Dictionary (Key: Wert, Value: Wahrscheinlichkeit)
    wahrscheinlichkeiten_dict = relative_anteile.to_dict()

    print("Indices der Laute:")
    print( sorted(list(wahrscheinlichkeiten_dict.keys())) )
    
    print("\nRelative Häufigkeiten:")
    print(wahrscheinlichkeiten_dict)

def nachfolger(dict,liste):
    for i in range(1,len(liste)):
        a = liste[i-1]
        b = liste[i]
        dict[a][b] += 1
    return dict

def conditionalProb(df):
    key_liste = sorted(df.stack().value_counts().index.tolist())
    doppel_dict = {outer_key: {inner_key: 0 for inner_key in key_liste} for outer_key in key_liste}

    #!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    #problem was ist mit ersten elementen diese haben kein vorheriges element todo ????!!!!!
    #!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    for index, row in df.iterrows():
        # Wir wandeln die Zeile in eine Liste um und entfernen leere Werte (NaN)
        werte = row.dropna().tolist()
        doppel_dict = nachfolger(doppel_dict,werte)
    for outer_key, inner_dict in doppel_dict.items():
        # 1. Summe aller Werte im inneren Dictionary berechnen
        total_sum = sum(inner_dict.values())
    
        # 2. Falls die Summe > 0 ist, jeden Wert durch die Summe teilen
        if total_sum > 0:
            for inner_key in inner_dict:
                inner_dict[inner_key] = inner_dict[inner_key] / total_sum
    
    
    matrix_df = dictToMatrix(doppel_dict)

    print("\nÜbergangsmatrix:")
    print("Zeile ist der zustand von dem man los geht und spalte gibt an wohin")
    print(matrix_df)
    #print(doppel_dict)

def nachfolger2(dict,liste):
    for i in range(2,len(liste)):
        a = liste[i-2]
        b = liste[i-1]
        c = liste[i]
        dict[a][b][c] += 1
    return dict

def conConditionalProb(df):
    key_liste = sorted(df.stack().value_counts().index.tolist())
    multi_dict = {outer_key_1:{outer_key_2:{inner_key: 0 for inner_key in key_liste} for outer_key_2 in key_liste}  for outer_key_1 in key_liste}
    #print(multi_dict)

    #!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    #problem was ist mit ersten elementen diese haben kein vorheriges element todo ????!!!!!
    #!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    for index, row in df.iterrows():
        # Wir wandeln die Zeile in eine Liste um und entfernen leere Werte (NaN)
        werte = row.dropna().tolist()
        multi_dict = nachfolger2(multi_dict,werte)
    
    
    for outer_key, inner_dict in multi_dict.items():
        for inner_key, inner_inner_dict in inner_dict.items():
            # 1. Summe aller Werte im inneren Dictionary berechnen
            total_inner_sum = sum(inner_inner_dict.values())
    
            # 2. Falls die Summe > 0 ist, jeden Wert durch die Summe teilen
            if total_inner_sum > 0:
                for inner_inner_key in inner_inner_dict:
                    inner_inner_dict[inner_inner_key] = inner_inner_dict[inner_inner_key] / total_inner_sum
    
    print("\nÜbergang anhand der vorherigen 2 werte:")
    for outer_key, innder_dict in multi_dict.items():
        print("\n",outer_key)
        print(dictToMatrix(innder_dict))
    

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
    print(name)
    calcProb(df)
    conditionalProb(df)
    conConditionalProb(df)
    print("\n")
    

#old
# Load the CSV into a DataFrame
#df = pd.read_csv('rspb20141370supp2_Orangutan.csv',sep=';',header=None)


#print(df.iloc[2])