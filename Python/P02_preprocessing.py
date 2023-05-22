import pandas as pd
import numpy as np
import os

from nltk import word_tokenize

import string

import requests    # read file without download



wd = os.getcwd()



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
    df = pd.read_csv(wd+os.sep+'DATA'+os.sep+fileName, encoding='UTF-8')
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
print('lowering case ...')
lower_case(get_data())



# 3. collapse script lines of the same character that are consecutive and drops empty rows
def collapse_consecutive_lines(df):
    for index, row in df.iterrows():
        if index > 0:
            if row['character'] == df.at[index-1,'character'] and pd.notna(df.at[index, 'character']):
                df.at[index,'script_line'] = df.at[index-1,'script_line'] + ' ' + row['script_line']
                df.at[index-1,'script_line'] = np.nan

                df.at[index,'duration'] = int(df.at[index,'duration'])+int(df.at[index-1,'duration'])
                df.at[index-1,'duration'] = np.nan

                df.at[index,'begin'] = df.at[index-1,'begin']

    df = df.dropna(subset=['script_line'])
    #df = df.dropna(subset=['character']) # drop rows that are not identified as a character
    df.to_csv(wd+os.sep+'DATA'+os.sep+'02_Suburra_data_collapsed.csv', index=False, encoding='UTF-8')
print('collapsing consecutive lines ...')
collapse_consecutive_lines(get_data())


# 3.5. drop rows that surely are not a character
def drop_not_characters(fileName):
    df = get_data(fileName)
    for index, row in df.iterrows():
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

print('dropping rows of non characters ...')

drop_not_characters('01_Suburra_data.csv')
drop_not_characters('02_Suburra_data_collapsed.csv')



# 4. set bad words
def set_badwords(df):
    badwords = get_badwords()
    for index, row in df.iterrows():
        for word in row['script_line'].split(' '):
            if word in badwords:
                df.at[index,'bad_words'] = 1
                break
    df.to_csv(wd+os.sep+'DATA'+os.sep+'01_Suburra_data.csv', index=False, encoding='UTF-8')
print('setting bad words ...')
set_badwords(get_data())



# 5. remove words inside brackets and transform tick to time
def remove_brackets(fileName):
    df = get_data(fileName)
    for index, row in df.iterrows():
        str_line = str(row['script_line']).strip()
        if '[' in str_line and ']' in str_line:
            df.at[index,'script_line'] = str_line[str_line.find(']')+1:len(str_line)].strip()
        
        # Maybe its better if they remain as ticks
        #df.at[index,'begin'] = convert_ticks_to_time(int(row['begin']))
        #df.at[index,'end'] = convert_ticks_to_time(int(row['end']))
        #df.at[index,'duration'] = convert_ticks_to_time(int(row['duration']))
print('removing brackets from script lines...')
remove_brackets('01_Suburra_data.csv')
remove_brackets('02_Suburra_data_collapsed.csv')

# 5.5 remove punctuation
def remove_punctuation(fileName):
    df = get_data(fileName)
    for index, row in df.iterrows():
        df.at[index,'script_line'] = row['script_line'].translate(str.maketrans('', '', string.punctuation))
    df.to_csv(wd+os.sep+'DATA'+os.sep+fileName, index=False, encoding='UTF-8')
print('removing punctuation ...')
remove_punctuation('01_Suburra_data.csv')
remove_punctuation('02_Suburra_data_collapsed.csv')


# 5. tokenization

# 5. remove stopwords

# 6. remove accents

# 7. remove special characters

