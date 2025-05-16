import os
import n2w

languages = ['kalasha', 'dhivehi', 'bagri', 'lamani', 'torwali', 'palula', 'siraiki', 'domaki', 'gurezi_shina', 'kalam_kohistani']

vigesimal_langs = ['kalasha', 'lamani', 'torwali', 'palula', 'domaki', 'gurezi_shina', 'kalam_kohistani']

vigesimal = dict([(l,'Vigesimal') if l in vigesimal_langs else (l,'Decimal') for l in languages])

df = [['ID','Local_ID','Language_ID','Parameter_ID','Value','Form	Segments','Comment','Source','Cognacy','Loan','Graphemes','Profile','NumeralAnalysis']]

for lang in languages:
    f = open('language_data/{}.txt'.format(lang),'r')
    text = f.read().split('\n')
    f.close()
    for i,l in enumerate(text):
        ll = ['{}-{}'.format(lang,i+1),'',lang,'{}_{}'.format(i+1,n2w.convert(i+1).replace(' ','')),l.replace(' ','').replace('=','').replace('-',''),l.replace(' ','').replace('=','').replace('-',''),'','','','','','','','']
        df.append(ll)

f = open('forms.csv','w')

for l in df:
    print(','.join(l),file=f)

f.close()

glottolog_name_key = {
    'kalasha':'Kalasha', 
    'dhivehi':'Dhivehi', 
    'bagri':'Bagri', 
    'lamani':'Lambadi', 
    'torwali':'Torwali', 
    'palula':'Phalura', 
    'siraiki':'Saraiki', 
    'domaki':'Domaaki', 
    'gurezi_shina':'Shina', 
    'kalam_kohistani':'Indus Kohistani'
}

glottolog = [l.strip().split(',') for l in open('glottologLanguages.csv','r')]

glottocode = {l[1]:l[0] for l in glottolog}

lonlat = {l[1]:(l[6],l[5]) for l in glottolog}

df = [['ID','Name','Glottocode','Glottolog_Name','ISO639P3code','Macroarea','Latitude','Longitude','Family','Comment','Base','Sources']]

for lang in languages:
    ll = [lang,glottolog_name_key[lang],glottocode[glottolog_name_key[lang]],glottolog_name_key[lang],'','Eurasia',lonlat[glottolog_name_key[lang]][1],lonlat[glottolog_name_key[lang]][0],'Indo-Aryan','',vigesimal[lang],'']
    df.append(ll)

f = open('languages.csv','w')

for l in df:
    print(','.join(l),file=f)

f.close()
