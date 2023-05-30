import pandas as pd
import numpy as np
import os
import re
import json
import platform


def clear_cli():
    if platform.system().lower() == "windows":
        os.system('cls') #clear console for linux
    else:
        os.system('clear') #clear console for linux

# ROW = 2463
# Time = E5
# 17.03.2022

################## GET full path for raw and py dir
wd = os.getcwd()
py_wd = wd + os.sep + 'Python'
raw_path = wd + os.sep + 'RAW'
diz = json.load(open(py_wd + os.sep + 'shortcut.json', 'r', encoding="utf-8"))

clear_cli()



def convert_ticks_to_time(ticks):
    tick_rate = 10000000
    seconds = ticks / tick_rate
    minutes = int(seconds // 60)
    seconds %= 60
    milliseconds = int((seconds - int(seconds)) * 1000)
    seconds = int(seconds)

    time = f"{minutes:02d}:{seconds:02d}.{milliseconds:03d}"

    return time



def xmlCleaner(line):
    """
    return plain text from a line of XML
    """
    begin = str()
    end = str()
    duration = str()
    while '<' in line:
        line = line.strip()
        #print("loop 1",end=' ')
        s = line.find('<')
        e = line.find('>') + 1
        try:
            while '<' == line[e]:
                #print("loop 2",end=' ')
                e = line[e:len(line)].find('>') + e +1
                #print(f'{e}')
        except IndexError:
            pass
        to_rep = line[s:e]
        #print(len(line),len(to_rep), sep='\n')
        if len(line)==len(to_rep):
            return '', begin, end, duration
        #print(line,to_rep, sep='\n')
        try:
            begin, end = re.findall(r'begin="([0-9]*)t" end="([0-9]*)t"', line)[0]
        except IndexError:
            pass
        line = line.replace(to_rep, ' ').strip()
    while '  ' in line:
        line = line.replace('  ',' ')
    while '- ' in line:
        line = line.replace('- ','\n').strip()
    if line.count('[') > 1:
        while '[' in line:
            line = line.replace(line[line.find('['):line.find(']')+2],'\n').strip()

    duration = int(end) - int(begin)

    return line, begin, end, duration



def listScriptLines(file):
    """
    Return a list with all script lines in a episode
    """
    script_lines = list()
    begin = list()
    end = list()
    duration = list()

    for line in file:
        n_line, n_begin, n_end, n_duration = xmlCleaner(line)
        if len(n_line)>0:
            if '\n' in n_line:
                for row in n_line.split('\n'):
                    script_lines.append(row.strip())
                    begin.append(n_begin)
                    end.append(n_end)
                    duration.append(n_duration)

            else:
                script_lines.append(n_line)
                begin.append(n_begin)
                end.append(n_end)
                duration.append(n_duration)

    return script_lines, begin, end, duration



def make_df():
    old_df = pd.read_csv(py_wd+os.sep+'Suburra_data.csv', encoding='UTF-8')
    personaggi = old_df["personaggio"].unique().tolist()
    Personaggi = [name for name in personaggi if type(name) == type(str())]
    ###################### CREATE df
    df = pd.DataFrame(columns=['character','script_line','season','episode','begin','end','duration','episode_index','bad_words','is_male','is_character'])
    df['bad_words'] = df['bad_words'].astype(int)
    df['is_male'] = df['is_male'].astype(int)
    df['is_character'] = df['is_character'].astype(int)
    ######################
    os.chdir(raw_path)
    files = os.listdir()
    for file in files:
        extension = file[file.rfind('.')+1:].lower()
        if extension == 'xml':
            season = int(file[file.rfind('S')+1:file.rfind('S')+3])
            episode = int(file[file.rfind('E')+1:file.rfind('E')+3])
            condition1 = (old_df['episodio'] == episode)
            if season < 2:
                print(file)
                fin = open(raw_path+os.sep+file, encoding='UTF-8')
                script_lines, begins, ends, duration = listScriptLines(fin)
                if len(begins) != len(ends) and len(begins) != len(script_lines):
                    print(f'ERROR: {file} has {len(begins)} begins, {len(ends)} ends and {len(script_lines)} script lines')
                    return None
                for i in range(0,len(script_lines)):
                    condition2 = (old_df['episode_index'] == i+1)
                    character_name = str(old_df.loc[condition1 & condition2, 'personaggio'].values[0]).strip()
                    if character_name in Personaggi:
                        is_character = 1
                    else:
                        is_character = 0
                        
                    if character_name in ['Adelaide Anacleti', 'Alice', 'Angelica Sale', 'Contessa Sveva Della Rocca Croce', 'Gabriella', 'Isabelle Mbamba', 'Livia Adami', 'Madre Samurai', 'Mara Guagli', 'Sara Monaschi', 'Anna']:
                        is_male = 0
                    elif is_character:
                        is_male = 1
                    else:
                        is_male = np.nan

                    data = {
                        'character': character_name,
                        'script_line': script_lines[i],
                        'season': season,
                        'episode': episode,
                        'begin': begins[i],
                        'end': ends[i],
                        'duration': duration[i],
                        'episode_index': i+1,
                        'bad_words': 0,
                        'is_male': is_male,
                        'is_character': is_character
                    }
                    df = pd.concat([df, pd.DataFrame(data, index=[0])], ignore_index=True)
    os.chdir(py_wd)
    df = df.sort_values(by=['episode','episode_index'],kind='heapsort',ignore_index=True)
    df.to_csv(wd+os.sep+'DATA'+os.sep+'01_Suburra_data.csv', index=False, encoding='UTF-8')
    return (df)


############################ MAKE df ########################################################
try:
    df = pd.read_csv(wd+os.sep+'DATA'+os.sep+'01_Suburra_data.csv', encoding='UTF-8')
except FileNotFoundError:
    df = make_df()
#############################################################################################



def helper():
    s = ''
    diz = json.load(open(py_wd + os.sep + 'shortcut.json', 'r', encoding="utf-8"))
    for k in diz.keys():
        if k.lower() in diz[k].lower():
            i = diz[k].lower().find(k.lower())
            s += diz[k][:i]+'('+diz[k][i].upper()+')'+diz[k][i+1:]+'\t'
        else:
            s += '('+k+')'+diz[k]+'\t'
        if len(diz[k]) < 6:
            s += '\t'
        #if ' ' in diz[k]:
        #    s += k+':'+diz[k][diz[k].find(' ')+1:len(diz[k])]+'\t'
        #else:
        #    s += k+':'+diz[k]+'\t'
    s += '\n\t:'
    return s



def shortcut():
    ################# ASSEGNAZIONE TASTI SHORTCUT JSON
    personaggi_Season01 = ['Adelaide Anacleti', 'Alberto Anacleti', 'Alice', 'Amedeo Cinaglia', 'Angelica Sale', 'Aureliano Adami', 'Boris', 'Cardinale Cosimo Giunti', 'Cardinale Pascagni', 'Contessa Sveva Della Rocca Croce', 'Ezio Quirino', 'Ferdinando Badali', 'Franco Marchilli', 'Gabriele Marchilli', 'Gabriella', 'Giacomo Finucci', 'Gianni Taccon', 'Isabelle Mbamba', 'Livia Adami', 'Madre Samurai', 'Manfredi Anacleti', 'Mara Guagli', 'Monsignor Theodosiou', 'Romolo Lucci', 'Samurai Valerio', 'Sandro Monaschi', 'Sara Monaschi', 'Saverio Boiardo Guerri', 'Stefano Forsini', 'Tullio Adami']
    js_file = open(py_wd + os.sep + 'shortcut.json', 'r', encoding="utf-8")
    diz = json.load(js_file)
    js_file.close()
    for p in personaggi_Season01:
        done = False
        if p not in diz.values():
            while not done:
                key = input(f'che tasto vuoi assegnare {p}').upper()
                if key not in diz.keys():
                    diz[key] = p
                    done = True
                else:
                    print(f"I seguenti tasti sono già stati assegnati {list(diz.keys()).sort()}")
    js_file = open(py_wd + os.sep + 'shortcut.json', 'w', encoding="utf-8")
    json.dump(obj=diz, fp=js_file, indent=4)
    js_file.close()



################ LABELLING
def labelling(df, start=0):
    for i in range(start,len(df)):
        time = convert_ticks_to_time(int(df.loc[i,'begin']))
        current = df.loc[i,'character']
        season = df.loc[i,'season']
        episode = str(df.loc[i,'episode'])

        print('\n',i,'\t',df.loc[i,'script_line'],'\ncurrent: ',current,"\n(0) SAVE\t' ' nan\t\tepisode: ",episode,'\t',time, sep='')
        label = input(helper()).lower()
        if len(label)==0:
            label = df.loc[i-1,'character']
            df.loc[i,'character'] = label
        elif label == '!' or label == '?' or label == '#':
            df.loc[i,'is_character'] = 0
        elif label[0].upper() in diz.keys() and len(label)<3:
            label = diz[label[0].upper()]
            df.loc[i,'character'] = label
        elif label == '0':
            df.to_csv(wd+os.sep+'DATA'+os.sep+'01_Suburra_data.csv', index=False, encoding='UTF-8')
            break
        elif label == ' ':
            label = np.nan
            df.loc[i,'character'] = label
        else:
            label = ' '.join([name.capitalize() for name in label.split(' ')])
            df.loc[i,'character'] = label.strip()
        clear_cli()
        print('\t',label,':\t',df.loc[i,'script_line'],sep='')

    df.to_csv(wd+os.sep+'DATA'+os.sep+'01_Suburra_data.csv', index=False, encoding='UTF-8')



done = True
while not done:
    #start = input("da quale battuta vuoi ripartire? ")
    start = df[df['character'].isnull()].index[0]
    print(f"Starting from index {start}")
    try:
        start = int(start)
        done = True
    except ValueError:
        pass

    labelling(df, start)