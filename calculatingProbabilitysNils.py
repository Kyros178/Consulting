import pandas as pd
import glob
import os

import math
from collections import Counter




def calcInitialStates(df):
    key_liste = sorted(df.stack().value_counts().index.tolist())
    absoluteHauefigkeiten = {inner_key: 0 for inner_key in key_liste}
    relativeHauefigkeiten = {inner_key: 0 for inner_key in key_liste}

    for index, row in df.iterrows():
        # Wir wandeln die Zeile in eine Liste um und entfernen leere Werte (NaN)
        werte = row.dropna().tolist()
        #TODO man könnte eine leere liste abfangen
         
        absoluteHauefigkeiten[werte[0]] += 1

    total_sum = sum(absoluteHauefigkeiten.values())
    for key, anzahl in absoluteHauefigkeiten.items():
        # 2. Falls die Summe > 0 ist, jeden Wert durch die Summe teilen
        if total_sum > 0:
            relativeHauefigkeiten[key] = anzahl / total_sum
    
    print("\nInitial State Probability:")
    print(relativeHauefigkeiten,"\n")
    return relativeHauefigkeiten

def calcInitial2States(df):
    key_liste = sorted(df.stack().value_counts().index.tolist())
    absoluteHauefigkeiten = {outer_key: {inner_key: 0 for inner_key in key_liste} for outer_key in key_liste}
    relativeHauefigkeiten = {outer_key: {inner_key: 0 for inner_key in key_liste} for outer_key in key_liste}

    total_sum = 0
    for index, row in df.iterrows():
        # Wir wandeln die Zeile in eine Liste um und entfernen leere Werte (NaN)
        werte = row.dropna().tolist()
        #TODO man könnte eine leere liste abfangen bzw eine mit einem element
         
        absoluteHauefigkeiten[werte[0]][werte[1]] += 1
        total_sum += 1

    if total_sum <= 0:
        print("Something went wrong")
        return None
    
    for key1, innerList in absoluteHauefigkeiten.items():
        # 2. Falls die Summe > 0 ist, jeden Wert durch die Summe teilen
        for key2, anzahl in innerList.items():
            relativeHauefigkeiten[key1][key2] = anzahl / total_sum

    matrix_rel = dictToMatrix(relativeHauefigkeiten)
    print("\nInitial State Probability for first two states:")
    print(matrix_rel,"\n")
    return relativeHauefigkeiten



def dictToMatrix(dict):
    matrix_df = pd.DataFrame.from_dict(dict, orient='index')

    # Optional: Spalten sortieren, damit sie die gleiche Reihenfolge wie die Zeilen haben
    matrix_df = matrix_df.reindex(sorted(matrix_df.columns), axis=1).sort_index()
    return matrix_df

def calcProb(df):
    relative_anteile = df.stack().value_counts(normalize=True).sort_index()

    # Als Dictionary (Key: Wert, Value: Wahrscheinlichkeit)
    wahrscheinlichkeiten_dict = relative_anteile.to_dict()

    print("Indices der Laute:")
    print( sorted(list(wahrscheinlichkeiten_dict.keys())) )
    
    print("\nRelative Häufigkeiten:")
    print(wahrscheinlichkeiten_dict)
    return wahrscheinlichkeiten_dict

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
    return doppel_dict

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
    return multi_dict

def calculate_log_likelihood(df, order, prob0, init1, init2, trans1, trans2):
    """
    Berechnet die Log-Likelihood des gesamten DataFrames basierend auf den Modellen.
    """
    total_log_likelihood = 0.0
    epsilon = 1e-10 # Verhindert math.log(0) falls Parameter auf 0 geschätzt wurden
    
    for index, row in df.iterrows():
        werte = row.dropna().tolist()
        n = len(werte)
        if n == 0:
            continue
            
        if order == 0:
            for w in werte:
                p = prob0.get(w, 0)
                #TODO entscheiden ob das epsiolon wirklich sein sollte
                total_log_likelihood += math.log(p ) #math.log(p + epsilon)
                
        elif order == 1:
            if n >= 1:
                # Initiale Wahrscheinlichkeit für erstes Element P(x_0)
                p_init = init1.get(werte[0], 0)
                #TODO entscheiden ob das epsiolon wirklich sein sollte
                total_log_likelihood += math.log(p_init) #math.log(p_init + epsilon)
                # Übergangswahrscheinlichkeiten P(x_t | x_t-1)
                for i in range(1, n):
                    p_trans = trans1.get(werte[i-1], {}).get(werte[i], 0)
                    #TODO entscheiden ob das epsiolon wirklich sein sollte
                    total_log_likelihood += math.log(p_trans) #math.log(p_trans + epsilon)
                    
        elif order == 2:
            if n == 1:
                p_init = init1.get(werte[0], 0)
                #TODO entscheiden ob das epsiolon wirklich sein sollte
                total_log_likelihood += math.log(p_init) #math.log(p_init + epsilon)
            elif n >= 2:
                # Da calcInitial2States direkt P(x_0, x_1) berechnet hat, 
                # können wir das als Startwert für die ersten beiden Töne nehmen.
                p_init2 = init2.get(werte[0], {}).get(werte[1], 0)
                #TODO entscheiden ob das epsiolon wirklich sein sollte
                total_log_likelihood += math.log(p_init2) #math.log(p_init2 + epsilon)
                
                # Übergangswahrscheinlichkeiten P(x_t | x_t-1, x_t-2)
                for i in range(2, n):
                    p_trans = trans2.get(werte[i-2], {}).get(werte[i-1], {}).get(werte[i], 0)
                    #TODO entscheiden ob das epsiolon wirklich sein sollte
                    total_log_likelihood += math.log(p_trans) #math.log(p_trans + epsilon)
                    
    return total_log_likelihood
    

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
    if  name in "rspb20141370supp2_bat.csv":
        continue

    print(f"=== Analysiere: {name} ===")
    
    # 1. Parameter trainieren (MLE)
    init1_probs = calcInitialStates(df)
    prob0_probs = calcProb(df)
    trans1_probs = conditionalProb(df)
    init2_probs = calcInitial2States(df)
    trans2_probs = conConditionalProb(df)
    print("\n")
    
    # 2. Log-Likelihood des Datensatzes berechnen
    ll_0 = calculate_log_likelihood(df, 0, prob0_probs, init1_probs, init2_probs, trans1_probs, trans2_probs)
    ll_1 = calculate_log_likelihood(df, 1, prob0_probs, init1_probs, init2_probs, trans1_probs, trans2_probs)
    ll_2 = calculate_log_likelihood(df, 2, prob0_probs, init1_probs, init2_probs, trans1_probs, trans2_probs)
    
    print("-" * 40)
    print("LOG-LIKELIHOOD ERGEBNISSE:")
    print(f"Ordnung 0: {ll_0:.4f}")
    print(f"Ordnung 1: {ll_1:.4f}")
    print(f"Ordnung 2: {ll_2:.4f}")
    print("=" * 40 + "\n")
    

#old
# Load the CSV into a DataFrame
#df = pd.read_csv('rspb20141370supp2_Orangutan.csv',sep=';',header=None)


#print(df.iloc[2])