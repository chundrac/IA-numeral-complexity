import urllib.request
import random
import numpy as np
from collections import defaultdict
from sklearn.linear_model import LogisticRegression
from scipy.stats import pearsonr
import copy
import editdistance
import sys

random.seed(1234)

dataset = sys.argv[1]

url = ''

if dataset == 'sand':
    url = 'https://raw.githubusercontent.com/numeralbank/sand/refs/heads/main/cldf/forms.csv' #downloaded 31 Mar 2025
    f = urllib.request.urlopen(url)
    text = f.read()
    text = text.decode('utf8')
    text = [l.split(',') for l in text.split('\n')][:-1]
    f.close()
    text_aux = [l.strip('\n').split(',') for l in open('../extra_data/forms.csv','r')]
    text = text+text_aux[1:]
    
if dataset == 'uninum':
    url = 'https://raw.githubusercontent.com/numeralbank/googleuninum/refs/heads/master/cldf/forms.csv' #downloaded 31 Mar 2025
    f = urllib.request.urlopen(url)
    text = f.read()
    text = text.decode('utf8')
    text = [l.split(',') for l in text.split('\n')][:-1]
    f.close()

langs = sorted(set([l[2] for l in text[1:]]))

def speak(lang):
    decoded = []
    text_ = [l for l in text if l[2]==lang]
    for i,l in enumerate(text_):
        text_[i][3] = text_[i][3].split('_')[0]
        if len(text_[i][3]) == 1:
            text_[i][3] = '0'+text_[i][3]
        text_[i][5] = text_[i][5].replace(' ','').replace('_','').replace('-','')
    forms = [tuple(list(l[5].replace(' ',''))) for l in text_]
    formdict = {l[3]:tuple(list(l[5].replace(' ',''))) for l in text_ if int(l[3]) in range(1,100)}
    if len(formdict.keys()) == 99:
        ngrams = []
        for form in formdict.values():
            w = tuple(['#']+list(form)+['$'])
            ngrams_ = []
            for i in range(len(w)-2):
                ngrams_.append(w[i:i+3])
            ngrams.append(ngrams_)
        ngram_types = sorted(set([s for ngram in ngrams for s in ngram]))
        N = len(ngram_types)
        phon = np.zeros([99,N])
        sem = np.zeros([99,2])
        for i,key in enumerate(formdict.keys()):
            sem[i,0] = int(key[0])
            sem[i,1] = int(key[1])
            for ngram in ngrams[i]:
                phon[i,ngram_types.index(ngram)] = 1
        for i in range(len(sem)):
            sem_ = np.delete(sem,[i],axis=0)
            phon_ = np.delete(phon,[i],axis=0)
            clf0 = LogisticRegression(random_state=0).fit(phon_,sem_[:,0])
            clf1 = LogisticRegression(random_state=0).fit(phon_,sem_[:,1])
            pred0 = int(clf0.predict(phon[i:i+1,:])[0])
            pred1 = int(clf1.predict(phon[i:i+1,:])[0])
            l_ = [lang,list(formdict.keys())[i],''.join(list(formdict.values())[i]).strip('"'),str(pred0)+str(pred1)]
            if list(formdict.keys())[i]==str(pred0)+str(pred1):
                l_.append('1')
            else:
                l_.append('0')
            decoded.append(l_)
    return(decoded)

all_data = [['language','number','form','predicted','accuracy']]
for lang in langs:
    print(langs.index(lang)/len(langs))
    decoded = speak(lang)
    all_data += decoded

f = open('LDL_comprehension_{}.tsv'.format(dataset),'w')
for l in all_data:
    print('\t'.join(l),file=f)

f.close()