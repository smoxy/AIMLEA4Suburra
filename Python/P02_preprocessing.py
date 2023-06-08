import pandas as pd
import numpy as np
import os
from time import sleep

import nltk
from nltk.corpus import stopwords
from nltk.tokenize import RegexpTokenizer

import string

import pickle

import requests    # read file without download



wd = os.getcwd()
#nltk.download('stopwords')
#nltk.download('wordnet')


def convert_ticks_to_time(ticks):
    tick_rate = 10000000
    seconds = ticks / tick_rate
    minutes = int(seconds // 60)
    seconds %= 60
    milliseconds = int((seconds - int(seconds)) * 1000)
    seconds = int(seconds)

    time = f"{minutes:02d}:{seconds:02d}.{milliseconds:03d}"

    return time


# 1. Read data and setting up
def get_data(fileName = "01_Suburra_data.csv"):
    sleep(0.5)
    if fileName[fileName.rfind('.'):] == '.csv':
        df = pd.read_csv(wd+os.sep+'DATA'+os.sep+fileName, encoding='UTF-8')
    elif fileName[fileName.rfind('.'):] == '.pkl':
        with open(wd+os.sep+'DATA'+os.sep+fileName, "rb") as file:
            df = pickle.load(file)
    try:
        df["character"] = df["character"].astype("category")
    except KeyError:
        pass
    try:
        df["season"] = df["season"].astype("category")
    except KeyError:
        pass
    try:
        df["episode"] = df["episode"].astype("Int8")    
    except KeyError:
        pass
    try:
        df["bad_words"] = df["bad_words"].astype("Int8")
    except KeyError:
        pass
    try:
        df["is_male"] = df["is_male"].astype("Int8")
    except KeyError:
        pass
    try:
        df["is_character"] = df["is_character"].astype("Int8")
    except KeyError:
        pass
    return df


def get_badwords():
    bad_w1 = requests.get("https://raw.githubusercontent.com/LDNOOBW/List-of-Dirty-Naughty-Obscene-and-Otherwise-Bad-Words/master/it")
    bad_w2 = requests.get("https://raw.githubusercontent.com/napolux/paroleitaliane/master/paroleitaliane/lista_badwords.txt")

    badwords = list()
    for line in bad_w2.text.split('\n'):
        badwords.append(line.strip().lower())
    # Now they are 454 bad words    print(len(badwords))

    badwords.sort()


    for line in bad_w1.text.split('\n'):
        line =line.strip().lower()
        if line not in badwords:
            badwords.append(line.strip())
    # Now they are 573 bad words    print(len(badwords))

    badwords.sort()
    return badwords



# 2. lower case all script lines
def lower_case(df):
    for index, row in df.iterrows():
        df.at[index,'script_line'] = row['script_line'].lower()
    df.to_csv(wd+os.sep+'DATA'+os.sep+'01_Suburra_data.csv', index=False, encoding='UTF-8')




# 3. collapse script lines of the same character that are consecutive and drops empty rows
def collapse_consecutive_lines(df, hybrid = False):
    for index, row in df.iterrows():
        if index > 0:
            if hybrid:
                if (row['character'] == df.at[index-1,'character'] and pd.notna(df.at[index, 'character']) and (int(row['episode_index']) == int(df.at[index-1,'episode_index'])+1)) and (row['character'] in ["Alberto Anacleti", "Amedeo Cinaglia", "Aureliano Adami", "Gabriele Marchilli", "Samurai Valerio", "Sara Monaschi", "Livia Adami"]):
                    df.at[index,'script_line'] = str(df.at[index-1,'script_line']) + ' ' + str(row['script_line'])
                    df.at[index-1,'script_line'] = np.nan

                    df.at[index,'duration'] = int(df.at[index,'duration'])+int(df.at[index-1,'duration'])
                    df.at[index-1,'duration'] = np.nan

                    df.at[index,'begin'] = df.at[index-1,'begin']
            else:
                if row['character'] == df.at[index-1,'character'] and pd.notna(df.at[index, 'character']) and (int(row['episode_index']) == int(df.at[index-1,'episode_index'])+1):
                    df.at[index,'script_line'] = str(df.at[index-1,'script_line']) + ' ' + str(row['script_line'])
                    df.at[index-1,'script_line'] = np.nan
    
                    df.at[index,'duration'] = int(df.at[index,'duration'])+int(df.at[index-1,'duration'])
                    df.at[index-1,'duration'] = np.nan
    
                    df.at[index,'begin'] = df.at[index-1,'begin']

    df = df.dropna(subset=['script_line'])
    #df = df.dropna(subset=['character']) # drop rows that are not identified as a character
    if hybrid:
        df.to_csv(wd+os.sep+'DATA'+os.sep+'03_Suburra_data_hybrid.csv', index=False, encoding='UTF-8')
    else:
        df.to_csv(wd+os.sep+'DATA'+os.sep+'02_Suburra_data_collapsed.csv', index=False, encoding='UTF-8')



# 3.5. drop rows that surely are not a character
def drop_not_characters(fileName):
    df = get_data(fileName)
    for index, row in df.iterrows():
        break
        str_line = str(df.at[index,'script_line']).strip()
        if df.at[index,'is_character'] == 0:    # or pd.isna(df.at[index, 'character']):
            if '♪' in df.at[index,'script_line']:
                df.at[index,'is_character'] = 0
            elif '[' in str_line:
                if str_line[0] == '[' and str_line[-1]== ']':
                    df.at[index,'is_character'] = 0
                    print(str_line)
                else:
                    is_character = input('\n'+str(df.at[index,'script_line'])+'| is a character? Y/N ').strip()
                    if is_character[0].lower() == 'Y'.lower():
                        str_line = str_line[str_line.rfind(']')+1:len(str_line)].strip()
                        df.at[index,'is_character'] = 1 
                        df.at[index,'script_line'] = str_line
                    else:
                        df.at[index,'is_character'] = 0
            else:
                is_character = input('\n'+str(df.at[index,'script_line'])+'| is a character? Y/N ').strip()
                if is_character[0].lower() == 'Y'.lower():
                    df.at[index,'is_character'] = 1 
                else:
                    df.at[index,'is_character'] = 0
        
        elif pd.isna(df.at[index, 'character']):
            if '[' in str_line:
                if str_line[0] == '[' and str_line[-1]== ']':
                    df.at[index,'is_character'] = 0
                    previous_index = int(index)
                else:
                    is_character = input('\n'+str(df.at[index,'script_line'])+'| is a character? Y/N ').strip()
                    if is_character[0].lower() == 'Y'.lower():
                        str_line = str_line[str_line.rfind(']')+1:len(str_line)].strip()
                        df.at[index,'is_character'] = 1 
                        df.at[index,'script_line'] = str_line
                    else:
                        df.at[index,'is_character'] = 0
            else:
                df.at[index,'is_character'] = 1
       #print('\n'+str(df.at[index,'is_character']),str_line)
    df = df.drop(df[df['is_character'] == 0].index)
    df.to_csv(wd+os.sep+'DATA'+os.sep+fileName, index=False, encoding='UTF-8')



# 4. set bad words
def set_badwords(fileName):
    df = get_data(fileName)
    badwords = get_badwords()
    for index, row in df.iterrows():
        for word in row['script_line'].split(' '):
            if word not in badwords:
                df.at[index,'bad_words'] = 0
            else:
                df.at[index,'bad_words'] = 1
                break

    df.to_csv(wd+os.sep+'DATA'+os.sep+fileName, index=False, encoding='UTF-8')



# 5. remove words inside brackets and transform tick to time
def remove_brackets(fileName):
    df = get_data(fileName)
    for index, row in df.iterrows():
        str_line = str(row['script_line']).strip()
        if '[' in str_line and ']' in str_line:
            str_line = str_line[str_line.find(']')+1:len(str_line)].strip()
            if len(str_line) > 0:
                df.at[index,'script_line'] = str_line
            else:
                df.at[index,'is_character'] = 0
        
        ###################################################################### Maybe its better if they remain as ticks
        #df.at[index,'begin'] = convert_ticks_to_time(int(row['begin']))
        #df.at[index,'end'] = convert_ticks_to_time(int(row['end']))
        #df.at[index,'duration'] = convert_ticks_to_time(int(row['duration']))
    df.to_csv(wd+os.sep+'DATA'+os.sep+fileName, index=False, encoding='UTF-8')


# 5.5 remove punctuation
def remove_punctuation(fileName):
    df = get_data(fileName)
    for index, row in df.iterrows():
        # Replace punctuation with a blank space
        cleaned_line =               row['script_line'].translate(str.maketrans(string.punctuation, ' ' * len(string.punctuation)))
        cleaned_line = ' '.join(cleaned_line.split())
        #df.at[index,'script_line'] = row['script_line'].translate(str.maketrans('', '', string.punctuation))
        df.at[index,'script_line'] = cleaned_line.strip()
    df.to_csv(wd+os.sep+'DATA'+os.sep+fileName, index=False, encoding='UTF-8')


# 5.6 remove rows of not relevant characters
def subset(fileName: str, allowed_characters: list = ["Alberto Anacleti", "Amedeo Cinaglia", "Aureliano Adami", "Gabriele Marchilli", "Samurai Valerio", "Sara Monaschi", "Livia Adami", "Contessa Sveva Della Rocca Croce", "Angelica Sale", "Manfredi Anacleti"]):
    df = get_data(fileName)
    new_df = df[df['character'].isin(allowed_characters)]
    
    new_df = new_df.drop(['season', 'is_character'], axis=1)        # dropping also the not needed columns

    fileName = fileName[:fileName.find('_')+1] + 'subset' + fileName[fileName.rfind('_'):]
    new_df.to_csv(wd+os.sep+'DATA'+os.sep+fileName, index=False, encoding='UTF-8')
    return new_df


def personal_tokenizer(df, fileName: str):
    new_df = df
    stop_words = set(stopwords.words("italian"))

    tokenizer = RegexpTokenizer(r'\w+')  # Rimuove numeri e simboli, mantiene solo parole
    new_df["script_line"] = df["script_line"].apply(lambda x: tokenizer.tokenize(x))
    df = new_df
    
    # Stopwords removing delle parole italiane
    stop_words = set(stopwords.words("italian"))
    new_df["script_line"] = new_df["script_line"].apply(lambda x: [word for word in x if word.lower() not in stop_words])
    # Stemming delle parole italiane
    stemmer = nltk.stem.SnowballStemmer("italian")
    new_df["script_line"] = new_df["script_line"].apply(lambda x: [stemmer.stem(word) for word in x])
    
    # Salva il dataframe
    with open(wd+os.sep+'DATA'+os.sep+fileName+".pkl", "wb") as file:
        pickle.dump(df, file)
    with open(wd+os.sep+'DATA'+os.sep+fileName+"_stemStop.pkl", "wb") as file:
        pickle.dump(new_df, file)





if __name__ == "__main__":
    #print('dropping rows of non characters ...')
    #drop_not_characters('01_Suburra_data.csv')

    #print('lowering case ...')
    #lower_case(get_data())

    #print('removing brackets from script lines...')
    #remove_brackets('01_Suburra_data.csv')
    #print('collapsing consecutive lines ...')
    #collapse_consecutive_lines(get_data())
    #collapse_consecutive_lines(get_data(), hybrid=True)

    #print('dropping rows of non characters ...')
    #drop_not_characters('02_Suburra_data_collapsed.csv')
    #drop_not_characters('02_Suburra_data_collapsed.csv')

    #print('setting bad words ...')
    #set_badwords('01_Suburra_data.csv')
    #set_badwords('02_Suburra_data_collapsed.csv')
    #set_badwords('03_Suburra_data_hybrid.csv')

    #print('removing brackets from script lines...')
    #remove_brackets('02_Suburra_data_collapsed.csv')
    #remove_brackets('03_Suburra_data_hybrid.csv')

    #print('removing punctuation ...')
    #remove_punctuation('01_Suburra_data.csv')
    #remove_punctuation('02_Suburra_data_collapsed.csv')
    #remove_punctuation('03_Suburra_data_hybrid.csv')

    print('subsetting ...')
    df = subset('01_Suburra_data.csv')
    df_collapsed = subset('02_Suburra_data_collapsed.csv')
    df_hybrid = subset('03_Suburra_data_hybrid.csv')

    print("tokenizing ...")
    personal_tokenizer(df, '04')
    personal_tokenizer(df_collapsed, '05_collapsed')
    personal_tokenizer(df_hybrid, '06_hybrid')
