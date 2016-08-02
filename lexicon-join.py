#!/usr/bin/python
# -*- coding: utf-8 -*-

#======================================================================#
# 
# Description: A Python script to perform fast join operations on CoNLL formatted corpora.
# In this case, joins a polarity lexicon by lemma/PoS pairs and computes aggregate polarity score.
#
# Usage: 
# python lexicon-join.py [input directory] [lexicon] 
# 
# Author: L. Rheault 
# 
#======================================================================#

from __future__ import division
import os, sys, re, time
import pandas as pd
from time import time
import csv

#---------------------------------------------------------------------------------------------------------------------------------------------------------------#

filelist = sorted(os.listdir(sys.argv[1]))
inpath = str(sys.argv[1])

lex1 = pd.read_table(str(sys.argv[2]),delimiter=',',dtype=object,header=0,encoding="utf-8")

# To also join the general-purpose lexicons, uncomment the following lines:
# --for nrc:
#lex2 = pd.read_table(str(sys.argv[3]),delimiter='\t',dtype=object,header=0) 
# --for of:
#lex3 = pd.read_table(str(sys.argv[4]),delimiter='\t',dtype=object,header=0)
# --for swn:
#lex4 = pd.read_table(str(sys.argv[5]),delimiter='\t',dtype=object,header=0)

with open("emotion-dataset.csv","w") as g:
    writer = csv.writer(g)
    writer.writerow( ("date","polar","wc","sumpolar","lexcount") )
    for f in filelist:
        t0 = time()
        temp = pd.read_table(inpath+'/'+f, delimiter='\t', dtype=object, header=0, encoding="utf-8")
		
        # Performs fast SQL-type join.
        temp = pd.merge(temp,lex1,how='left',on=['lemma','pos1'])
        #temp = pd.merge(temp,lex2,how='left',on=['lemma'])
        #temp = pd.merge(temp,lex3,how='left',on=['lemma','pos1'])
        #temp = pd.merge(temp,lex4,how='left',on=['lemma','pos1'])
        
        temp[['l','s','w','valence','polarity']] = temp[['l','s','w','valence','polarity']].apply(pd.to_numeric)
        temp.sort_values(['l','s','w'],ascending=[True, True, True],inplace=True)
		
        # Adjusting for valence. (1-temp.valence) is denoted \theta in the text. 
        temp['polarity_val'] = (1 - temp.valence)*temp.polarity
        
        # Numerator and denominator from equation [3]. 
        sumpolar = temp.polarity_val.sum(skipna=True)
        lexcount = temp.polarity.count()
		
        # The polarity measure y_t.
        polar = sumpolar/lexcount
        # A general word count.
        wc = temp.shape[0]

        # Writes the data by quarter.  Polar is the quarterly variable y_t.  
        # Variables sumpolar and lexcount can be summed by year to produce the yearly y_t.
        writer.writerow( (f[:6], polar, wc, sumpolar, lexcount) )

        # Updates CoNLL file with joined polarity scores:
        temp.to_csv(inpath+'/'+f, sep='\t', index=False, encoding="utf-8")

        print "File %s processed in %0.3fs." % (f, (time()-t0) )
