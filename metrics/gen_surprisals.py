import urllib.request
import random
import numpy as np
from collections import defaultdict
import sys
import unicodedata

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

def heldout_surprisals(lang):
    text_ = [l for l in text if l[2]==lang]
    formdict = {l[3].split('_')[0]:tuple(list(unicodedata.normalize('NFD',l[5].replace(' ','')))) for l in text_ if int(l[3].split('_')[0]) in range(1,100)}
    alpha = .1
    V = len(set([s for v in formdict.values() for s in v]))
    all_surprisals = []
    for key in formdict.keys():
        keys_ = [k for k in formdict.keys() if key != k]
        bigram_counts = defaultdict(int)
        trigram_counts = defaultdict(int)
        for k in keys_:
            w = ['#','#']+list(formdict[k])+['$']
            for i in range(len(w)-1):
                bigram_counts[tuple(w[i:i+2])] += 1
            for i in range(len(w)-2):
                trigram_counts[tuple(w[i:i+3])] += 1
        bigram_prob = defaultdict(float)
        w_ = ['#','#']+list(formdict[key])+['$']
        surprisals = []
        for i in range(2,len(w_)):
            s = -np.log((trigram_counts[tuple(w_[i-2:i+1])] + alpha)/(bigram_counts[tuple(w_[i-2:i])] + V*alpha))
            surprisals.append(s)
        all_surprisals.append([lang,key,str(np.mean(surprisals))])
    return(all_surprisals)

metrics = [['language','number','surprisal']]
for lang in langs:
    print(langs.index(lang)/len(langs))
    print(lang)
    metrics += heldout_surprisals(lang)

f = open('surprisals_{}.tsv'.format(dataset),'w')

for l in metrics:
    print('\t'.join(l),file=f)

f.close()