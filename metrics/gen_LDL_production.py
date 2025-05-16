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
    continuation = defaultdict(list)
    # get all possible continuations for each ngram type
    def get_next_ngram(s,ngram_types):
        """get all ngrams that start with the last two characters of the input ngram"""
        return([s_ for s_ in ngram_types if s_[:2] == s[1:]])
    # assumes global variable `lists`, maybe can be passed as variable to function
    def get_sequence(l, s):
        l_ = copy.copy(l)
        if len(l_) < 20:
            l_.append(s)
            if s[-1] == '$':
                lists.append(l_)
            #candidates = sorted([s for s in continuation[s] if s not in l_[-2:]], key=lambda x: weights[x])[-K_:]
            candidates = sorted([s for s in continuation[s] if l_[-2:].count(s) <= 1], key=lambda x: weights[x])[-K_:]
            for s_ in candidates:
                get_sequence(l_, s_)
    for s in ngram_types:
        candidates = get_next_ngram(s,ngram_types)
        for s_ in candidates:
            if s_ not in continuation[s]:
                continuation[s].append(s_)
    K_ = 2  # how many next step candidates to consider
    phon = np.zeros([99,N])
    sem = np.zeros([99,20])
    for i,key in enumerate(formdict.keys()):
        sem[i,int(key[0])] = 1
        sem[i,10+int(key[1])] = 1
        for ngram in ngrams[i]:
            phon[i,ngram_types.index(ngram)] = 1
    for i in range(len(sem)):
        sem_ = np.delete(sem,[i],axis=0)
        phon_ = np.delete(phon,[i],axis=0)
        W = np.linalg.lstsq(sem_,phon_,rcond=None)
        S = np.linalg.lstsq(phon_,sem_,rcond=None)
        w = np.dot(sem[i, :], W[0])
        weights = {s: w[ngram_types.index(s)] for s in ngram_types}
        starts = sorted([s for s in ngram_types if s[0] == '#'],key=lambda x: weights[x])[-3:]
        lists = []
        for start in starts:
            get_sequence([], start)
        # multi-hot encode candidate sequences
        candidate_ngram_bin = np.zeros([len(lists), len(ngram_types)])
        for j, l in enumerate(lists):
            for s in l:
                candidate_ngram_bin[j, ngram_types.index(s)] += 1
        # Compute correlation coef for each semantic vector predicted on the basis of a candidate string and the input semantic vector
        rs = []
        for j in range(candidate_ngram_bin.shape[0]):
            candidate_vector = np.dot(candidate_ngram_bin[j, :], S[0])
            # below, index [0] is needed to get the correlation coefficient, rather than the tuple (r, p-value)
            r = pearsonr(sem[i, :], candidate_vector)[0]
            rs.append(r)
        original_form = ''.join(list(formdict.values())[i])
        decoded_form = ''.join([s[1] for s in lists[np.argmax(rs)]])
        l_ = [lang,list(formdict.keys())[i],original_form.strip('"'),decoded_form.strip('"')]
        if original_form == decoded_form:
            l_.append('0')
        else:
            l_.append('1')
        l_.append(str(editdistance.distance(original_form,decoded_form)/max([len(original_form),len(decoded_form)])))
        decoded.append(l_)
  return(decoded)

all_data = [['language','number','form','predicted','WER','PER']]
for lang in langs:
    print(langs.index(lang)/len(langs))
    decoded = speak(lang)
    all_data += decoded

f = open('LDL_production_{}.tsv'.format(dataset),'w')
for l in all_data:
    print('\t'.join(l),file=f)

f.close()