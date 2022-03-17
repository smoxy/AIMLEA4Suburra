import pandas as pd
import numpy as np
import os
import json

os.system('clear') #clear console for linux

# ROW = 2463
# Time = E5
# 17.03.2022

################## GET full path for raw and py dir
wd = os.getcwd()
py_wd = wd + os.sep + 'Python'
raw_path = wd + os.sep + 'RAW'
diz = json.load(open(py_wd + os.sep + 'shortcut.json', 'r', encoding="utf-8"))



def xmlCleaner(line):
    """
    return plain text from a line of XML
    """
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
            return ''
        #print(line,to_rep, sep='\n')
        line = line.replace(to_rep, ' ').strip()
    while '  ' in line:
        line = line.replace('  ',' ')
    while '- ' in line:
        line = line.replace('- ','\n').strip()
    if line.count('[') > 1:
        while '[' in line:
            line = line.replace(line[line.find('['):line.find(']')+2],'\n').strip()

    return line



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



def listBattute(file):
    """
    Return a list with all Battute in a episode
    """
    episode = list()
    for line in file:
        n_line = xmlCleaner(line)
        if len(n_line)>0:
            if '\n' in n_line:
                for row in n_line.split('\n'):
                    episode.append(row.strip())
            else:
                episode.append(n_line)
    return episode



def make_df():
    ###################### CREATE df
    df = pd.DataFrame(columns=['personaggio','battuta','stagione','episodio','index'])
    ######################
    os.chdir(raw_path)
    files = os.listdir()
    for file in files:
        season = int(file[file.rfind('S')+1:file.rfind('S')+3])
        episode = int(file[file.rfind('E')+1:file.rfind('E')+3])
        if season < 2:
            print(file)
            fin = open(raw_path+os.sep+file, encoding='UTF-8')
            list_battute = listBattute(fin)
            for i in range(0,len(list_battute)):
                df = df.append({
                    'personaggio': str(),
                    'battuta': list_battute[i],
                    'stagione': season,
                    'episodio': episode,
                    'index': i+1
                },ignore_index=True)
    os.chdir(py_wd)
    df = df.sort_values(by=['episodio','index'],kind='heapsort',ignore_index=True)
    df.to_csv(py_wd+os.sep+'Suburra_data.csv', index=False, encoding='UTF-8')
    return (df)


############################ MAKE df ########################################################
try:
    df = pd.read_csv(py_wd+os.sep+'Suburra_data.csv', encoding='UTF-8')
except FileNotFoundError:
    df = make_df()
#############################################################################################


def shortcut():
    ################# ASSEGNAZIONE TASTI SHORTCUT JSON
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
        print('\n',i,'\t',df.loc[i,'battuta'], sep='')
        label = input(helper()).lower()
        if len(label)==0:
            label = df.loc[i-1,'personaggio']
            df.loc[i,'personaggio'] = label
        elif label[0].upper() in diz.keys() and len(label)<3:
            label = diz[label[0].upper()]
            df.loc[i,'personaggio'] = label
        elif label == '0':
            df.to_csv(py_wd+os.sep+'Suburra_data.csv', index=False, encoding='UTF-8')
            break
        elif label == ' ':
            label = np.nan
            df.loc[i,'personaggio'] = label
        else:
            df.loc[i,'personaggio'] = label
        os.system('clear')
        print('\t',label)



done = False
while not done:
    start = input("da quale battuta vuoi ripartire? ")
    try:
        start = int(start)
        done = True
    except ValueError:
        pass

labelling(df, start)