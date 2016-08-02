#!/usr/bin/python
# -*- coding: utf-8 -*-

#=====================================================================#
# 
# Description: 
# A script to evaluate polarity classifiers on the IMDb corpus.    
#
# Usage: 
# python movie-classifier.py train-imdb.csv test-imdb.csv [full/extreme] [none/bow/all] [K]
#
# --option full or extreme will either use the full testing set or the one restricted to extreme reviews.
# --option none, bow or all will use the polarity score only, add the bag-of-words models, or use all available features.
# --option K, if provided, will restrict the number of features to the K best using a chi-square test. 
# Dependencies: train-imdb.csv and test-imdb.csv
# 
# The script will display accuracy statistics via standard output.
#
# Results reported in text (Table SI3) are obtained with:
# python movie-classifier.py train-imdb.csv test-imdb.csv full none   
# python movie-classifier.py train-imdb.csv test-imdb.csv full bow 20000
# python movie-classifier.py train-imdb.csv test-imdb.csv full all 20000 
# python movie-classifier.py train-imdb.csv test-imdb.csv extreme none
# python movie-classifier.py train-imdb.csv test-imdb.csv extreme bow 5000 
# python movie-classifier.py train-imdb.csv test-imdb.csv extreme all 5000
#
# Author: L. Rheault
# 
#=====================================================================#

from __future__ import division
from operator import itemgetter
import os, sys, codecs, re
import pandas as pd
import numpy as np
from time import time
from scipy import sparse
from scipy.sparse import hstack
from sklearn import metrics
from sklearn.feature_extraction import DictVectorizer
from sklearn.preprocessing import OneHotEncoder
from sklearn.metrics import roc_auc_score
from sklearn.datasets import load_svmlight_file
from sklearn.feature_extraction.text import CountVectorizer, TfidfVectorizer, TfidfTransformer
from sklearn.pipeline import Pipeline, FeatureUnion
from sklearn.feature_selection import SelectKBest, chi2
from sklearn.cross_validation import StratifiedKFold,StratifiedShuffleSplit,KFold,ShuffleSplit,train_test_split
from sklearn.svm import LinearSVC, SVC
from sklearn.linear_model import SGDClassifier
from sklearn.linear_model import Perceptron
from sklearn.neighbors import KNeighborsClassifier
from sklearn.neighbors.nearest_centroid import NearestCentroid
from sklearn.naive_bayes import MultinomialNB, BernoulliNB
from sklearn.base import BaseEstimator, TransformerMixin
import cPickle as pickle

#=====================================================================#
# Feature classes.

class polarityTransformer(BaseEstimator, TransformerMixin):
    def fit(self, X, y=None):
        return self
    def transform(self, X):
        polarity = np.asarray(X['polar'].apply(pd.to_numeric))
        return [{'polar': polar} for polar in polarity]

class movieIdTransformer(BaseEstimator, TransformerMixin):
    def fit(self, X, y=None):
        return self
    def transform(self, X):
        movies = np.asarray(X['imdb'].astype('category'))
        return [{'movie': movie} for movie in movies]

class movieGenreTransformer(BaseEstimator, TransformerMixin):
    def fit(self, X, y=None):
        return self
    def transform(self, X):
        genres = [ 'com','rom','cri' ,'war','dra','hor','fno','thr','ani','fam','adv','fan','sci','mys','bio','doc']
        genrescol = X[genres].apply(pd.to_numeric)
        genreDict = genrescol.T.to_dict().values()
        return genreDict

class movieRatingTransformer(BaseEstimator, TransformerMixin):
    def fit(self, X, y=None):
        return self
    def transform(self, X):
        ratings = np.asarray(X['rating'].apply(pd.to_numeric))
        return [{'rating': rating} for rating in ratings]
    
class textTransformer(BaseEstimator, TransformerMixin):
    def fit(self, X, y=None):
        return self
    def transform(self, X):
        text = X['text'].tolist()
        return text
    
#=====================================================================#
# For export.

def exportModel(X, y, clf, features):
	clf.fit(X, y)
	print "Saving the models as a Pickle archive."
	with open('topic-classifier-models.pkl', 'wb') as p:
		pickle.dump(clf, p)
	print "Saving the features as a Pickle archive."
	with open('features.pkl', 'wb') as q:	
		pickle.dump(features, q)

# Full testing set or subset.
def dataImport(trainData, testData, subsetOption):
    inpathTrain = trainData
    inpathTest = testData
    if subsetOption=="full":
        a = pd.read_table(inpathTrain,delimiter=",",header=0,dtype=object,encoding='utf-8')
        a['bscore'] = a.bscore.apply(pd.to_numeric)
        # Converting polarity score to positive range; required only for SelectKBest(chi2) script.        
        a['polar'] = a.polar.apply(pd.to_numeric) 
        a['polar'] = a.polar+1
        X_train = a[['polar','imdb','rating', 'com','rom','cri' ,'war','dra','hor','fno','thr','ani','fam','adv','fan','sci','mys','bio','doc','text']]
        y_train = a['bscore'].values.ravel()
        b = pd.read_table(inpathTest,delimiter=",",header=0,dtype=object,encoding='utf-8')
        b['bscore'] = b.bscore.apply(pd.to_numeric)
        # Converting polarity score to positive range; required only for SelectKBest(chi2) script.
        b['polar'] = b.polar.apply(pd.to_numeric) 
        b['polar'] = b.polar+1
        X_test = b[['polar','imdb','rating','com','rom','cri' ,'war','dra','hor','fno','thr','ani','fam','adv','fan','sci','mys','bio','doc','text']]
        y_test = b['bscore'].values.ravel()
        return (X_train, y_train, X_test, y_test)
    elif subsetOption=="extreme":
        a = pd.read_table(inpathTrain,delimiter=",",header=0,dtype=object,encoding='utf-8')
        a = a[pd.notnull(a.bscore10)]
        a['bscore10'] = a.bscore10.apply(pd.to_numeric)
        # Converting polarity score to positive range; required only for SelectKBest(chi2) script.        
        a['polar'] = a.polar.apply(pd.to_numeric) 
        a['polar'] = a.polar+1
        X_train = a[['polar','imdb','rating', 'com','rom','cri' ,'war','dra','hor','fno','thr','ani','fam','adv','fan','sci','mys','bio','doc','text']]
        y_train = a['bscore10'].values.ravel()
        b = pd.read_table(inpathTest,delimiter=",",header=0,dtype=object,encoding='utf-8')
        b = b[pd.notnull(b.bscore10)]
        b['bscore10'] = b.bscore10.apply(pd.to_numeric)
        # Converting polarity score to positive range; required only for SelectKBest(chi2) script.
        b['polar'] = b.polar.apply(pd.to_numeric) 
        b['polar'] = b.polar+1
        X_test = b[['polar','imdb','rating','com','rom','cri' ,'war','dra','hor','fno','thr','ani','fam','adv','fan','sci','mys','bio','doc','text']]
        y_test = b['bscore10'].values.ravel()
        return (X_train, y_train, X_test, y_test)
    else:
        sys.exit("Choose between the 'full' or 'extreme' review sets.")

def featureSelect(X_train, y_train, X_test, y_test, subsetChoice, featChoice, K):
    
    moviePolarity = Pipeline([
		('polar', polarityTransformer()),
		('dict-vect', DictVectorizer())
		])

    movieId = Pipeline([
		('movie', movieIdTransformer()),
		('dict-vect', DictVectorizer())
		])

    movieGenre = Pipeline([
		('genre', movieGenreTransformer()),
		('dict-vect', DictVectorizer())
		])

    movieRating = Pipeline([
		('rating', movieRatingTransformer()),
		('dict-vect', DictVectorizer())
		])

    mainText = Pipeline([
		('text', textTransformer()),
		('tfidf', TfidfVectorizer(stop_words='english',ngram_range=(1,3)))   
		])

    if featChoice=="none": 
        features = FeatureUnion([
		    ('moviePolar',  moviePolarity)
		    ])
        X_train = features.fit_transform(X_train)
        X_test = features.transform(X_test)
    elif featChoice=="bow":
        features = FeatureUnion([
		    ('moviePolar',  moviePolarity),
            ('text',  mainText)
		    ])
        kbest = SelectKBest(chi2, k = K)
        X_train = features.fit_transform(X_train)
        X_train = kbest.fit_transform(X_train, y_train)
        X_test = features.transform(X_test)
        X_test = kbest.transform(X_test)
    elif featChoice=="all":
        features = FeatureUnion([
		    ('moviePolar',  moviePolarity),
       		('movieGenre',  movieGenre),
        	('movieRating',  movieRating),
            ('text',  mainText)
		    ])
        kbest = SelectKBest(chi2, k = K)
        X_train = features.fit_transform(X_train)
        X_train = kbest.fit_transform(X_train, y_train)
        X_test = features.transform(X_test)
        X_test = kbest.transform(X_test)
    else:
        sys.exit("Choose between 'none', 'bow' or 'all' additional features.")
   
    print "Training and evaluating the model(s)."
    models = (
		(LinearSVC(), "Support Vector Machine (Default)"),
        (LinearSVC(penalty="l1", dual=False, tol=1e-3), "Support Vector Machine: l1, dual=False"),
        (LinearSVC(penalty="l2", dual=False, tol=1e-3), "Support Vector Machine: l2, dual=False")
		)

    for clf, name in models:
        print('-' * 76)
        print(name)
        print('-' * 76)
        clf.fit(X_train,y_train)
        pred = clf.predict(X_test)
        pcp = metrics.accuracy_score(y_test,pred)
        f1 = metrics.f1_score(y_test,pred,average="binary")
        mc = metrics.classification_report(y_test, pred)
        cm = metrics.confusion_matrix(y_test, pred)
        if subsetChoice=="full":
            pre = (12500 - (int(cm[0][1]) + int(cm[1][0]))) / 12500
        elif subsetChoice=="extreme":
            pre = (4999 - (int(cm[0][1]) + int(cm[1][0])))  / 4999
        roc = roc_auc_score(y_test, pred)
        print "The percent correctly predicted is %0.3f" %pcp
        print "The F1-score is %0.3f" %f1
        print "The proportional reduction in errors is %0.3f" %pre
        print "The confusion matrix:"
        print cm
        print "The classification report:"
        print mc
        print "The area under the ROC curve is %0.3f" %roc        
        print('-' * 76)

def uFeatures(X_train, y_train, X_test, y_test, subsetChoice, featChoice):
    
    moviePolarity = Pipeline([
		('polar', polarityTransformer()),
		('dict-vect', DictVectorizer())
		])

    movieId = Pipeline([
		('movie', movieIdTransformer()),
		('dict-vect', DictVectorizer())
		])

    movieGenre = Pipeline([
		('genre', movieGenreTransformer()),
		('dict-vect', DictVectorizer())
		])

    movieRating = Pipeline([
		('rating', movieRatingTransformer()),
		('dict-vect', DictVectorizer())
		])

    mainText = Pipeline([
		('text', textTransformer()),
		('tfidf', TfidfVectorizer(stop_words='english',ngram_range=(1,3)))   
		])

    if featChoice=="none": 
        features = FeatureUnion([
		    ('moviePolar',  moviePolarity)
		    ])
        X_train = features.fit_transform(X_train)
        X_test = features.transform(X_test)
    elif featChoice=="bow":
        features = FeatureUnion([
		    ('moviePolar',  moviePolarity),
            ('text',  mainText)
		    ])
        X_train = features.fit_transform(X_train)
        X_test = features.transform(X_test)
    elif featChoice=="all":
        features = FeatureUnion([
		    ('moviePolar',  moviePolarity),
       		('movieGenre',  movieGenre),
        	('movieRating',  movieRating),
            ('text',  mainText)
		    ])
        X_train = features.fit_transform(X_train)
        X_test = features.transform(X_test)
    else:
        sys.exit("Choose between 'none', 'bow' or 'all' additional features.")
   
    print "Training and evaluating the model(s)."
    models = (
		(LinearSVC(), "Support Vector Machine (Default)"),
        (LinearSVC(penalty="l1", dual=False, tol=1e-3), "Support Vector Machine: l1, dual=False"),
        (LinearSVC(penalty="l2", dual=False, tol=1e-3), "Support Vector Machine: l2, dual=False")
		)

    for clf, name in models:
        print('-' * 76)
        print(name)
        print('-' * 76)
        clf.fit(X_train,y_train)
        pred = clf.predict(X_test)
        pcp = metrics.accuracy_score(y_test,pred)
        f1 = metrics.f1_score(y_test,pred,average="binary")
        mc = metrics.classification_report(y_test, pred)
        cm = metrics.confusion_matrix(y_test, pred)
        if subsetChoice=="full":
            pre = (12500 - (int(cm[0][1]) + int(cm[1][0]))) / 12500
        elif subsetChoice=="extreme":
            pre = (4999 - (int(cm[0][1]) + int(cm[1][0])))  / 4999
        roc = roc_auc_score(y_test, pred)
        print "The percent correctly predicted is %0.3f" %pcp
        print "The F1-score is %0.3f" %f1
        print "The proportional reduction in errors is %0.3f" %pre
        print "The confusion matrix:"
        print cm
        print "The classification report:"
        print mc
        print "The area under the ROC curve is %0.3f" %roc        
        print('-' * 76)

#=====================================================================#

if __name__=="__main__":

    t0 = time()    
    print "Opening and preparing the corpus."
    inpathTrain = str(sys.argv[1])
    inpathTest = str(sys.argv[2])
    subsetChoice = str(sys.argv[3])
    featureChoice = str(sys.argv[4])
    X_train, y_train, X_test, y_test = dataImport(inpathTrain, inpathTest, subsetChoice)
    if len(sys.argv)>5:
        K = int(sys.argv[5])
        featureSelect(X_train, y_train, X_test, y_test, subsetChoice, featureChoice, K)
    else:
        uFeatures(X_train, y_train, X_test, y_test, subsetChoice, featureChoice)

    print("Done in %0.3fs" % (time() - t0))  
	
