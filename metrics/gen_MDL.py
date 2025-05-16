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

def get_MDL(lang):
    text_ = [l for l in text if l[2]==lang]
    formdict = {l[3].split('_')[0]:tuple(list(unicodedata.normalize('NFD',l[5].replace(' ','')))) for l in text_ if int(l[3].split('_')[0]) in range(1,100)}
    V = len(set([s for v in formdict.values() for s in v]))
    desc_length = 'NA'
    if len(formdict.keys()) == 99:
        segments = {}
        for key in formdict.keys():
            segments[key] = defaultdict(list)
            w = list(formdict[key])
            if int(key) in range(1,11):
                i = 'None'
                segments[key][i].append(tuple(w))
            elif int(key) in range(11,21) or int(key) in range(20,100,10):
                i = 'None'
                segments[key][i].append(tuple(w))
                for i in range(1,len(w)):
                    segments[key][i].append(tuple(w[:i]))
                    segments[key][i].append(tuple(w[i:]))
            else:
                for i in range(1,len(w)):
                    segments[key][i].append(tuple(w[:i]))
                    segments[key][i].append(tuple(w[i:]))
                for i in range(1,len(w)-1):
                    for j in range(i+1,len(w)):
                        segments[key][(i,j)].append(tuple(w[:i]))
                        segments[key][(i,j)].append(tuple(w[i:j]))
                        segments[key][(i,j)].append(tuple(w[j:]))
        cachelen = []
        for chain in range(10):
            cache = []
            curr_segmentation = {}
            for key in segments.keys():
                if key not in curr_segmentation.keys():
                    z = random.sample(segments[key].keys(),1)[0]
                    curr_segmentation[key] = z
                    for w in segments[key][z]:
                        cache.append(w)
            logliks = []
            T = 1000
            for t in range(T):
                loglik = 0
                keys_ = random.sample(segments.keys(),len(segments.keys()))
                for key in keys_:
                    z = curr_segmentation[key]
                    for w in segments[key][z]:
                        cache.pop(cache.index(w))
                    pi = {}
                    for k in segments[key].keys():
                        pi[k] = - np.log(len([s for w in set(cache+segments[key][k]) for s in w]))
                    pi_ = np.exp(np.array(list(pi.values())))
                    pi_ = pi_/sum(pi_)
                    z_ = np.argmax(pi_)
                    z = list(pi.keys())[z_]
                    curr_segmentation[key] = z
                    loglik += np.log(pi_[z_])
                    for w in segments[key][z]:
                        cache.append(w)
                logliks.append(loglik)
                if t > 1 and logliks[t] == logliks[t-1]:
                    break
            cachelen.append(len(set(cache)))
        desc_length = min(cachelen)
    return(desc_length)

metrics = []
for lang in langs:
    print(langs.index(lang)/len(langs))
    print(lang)
    metrics.append([lang,str(get_MDL(lang))])

f = open('MDL_{}.tsv'.format(dataset),'w')
print('\t'.join(['language','MDL']),file=f)
for l in metrics:
    print('\t'.join(l),file=f)

f.close()