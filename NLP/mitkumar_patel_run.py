import nltk
import pickle
import random
from re import split
from nltk.corpus import stopwords
import nltk.classify
from sklearn.svm import LinearSVC
nltk.download('stopwords')
from os import path

documents = []
#Load Amezon labelled File
read_file = "amazon_cells_labelled.txt"
with open(read_file,'r', encoding='utf-8',  errors='ignore') as r:
  c=0
  for line in r:
    splitted = line.strip().split('\t')
    msg = ('').join(splitted[:-1])
    is_class = splitted[-1]
    documents.extend([ dict(doc=msg.lower(),category=is_class)])
    
#Load IMDB labelled File
read_file = "imdb_labelled.txt"
with open(read_file,'r', encoding='utf-8',  errors='ignore') as r:
  c=0
  for line in r:
    splitted = line.strip().split('\t')
    msg = ('').join(splitted[:-1])
    is_class = splitted[-1]
    documents.extend([ dict(doc=msg.lower(),category=is_class)])
    
#Load yelp labelled File
read_file = "yelp_labelled.txt"
with open(read_file,'r', encoding='utf-8',  errors='ignore') as r:
  c=0
  for line in r:
    splitted = line.strip().split('\t')
    msg = ('').join(splitted[:-1])
    is_class = splitted[-1]
    documents.extend([ dict(doc=msg.lower(),category=is_class)])

#Extarct words from the sentences
for n in range(len(documents)):
  documents[n]['words'] = split('\W+', documents[n]['doc'])
  
#remove stop words from the list
all_words = nltk.FreqDist(w.lower() for d in documents for w in d['words'] if w not in stopwords.words()and not w.isdigit())

#Take only 25000 useful words
word_features = all_words.most_common(25000)
def document_features(document):
  if not document.get('words'):
    document['words']=split('\W+', document['doc'])
  document_words=set(document['words']) 
  features={}
  for word_t in word_features:
    word = word_t[0]
    features["contains('{0}')".format(word)]=(word in document_words)
  return features

#shuffle word list
random.shuffle(documents)

#converted in traing formate
featuresets=[(document_features(d),d['category']) for d in documents]

#split data into training and testing
train_set,test_set = featuresets[:2000],featuresets[2000:]

#Check whether model is present or not
#If yes read from Current working dir 
if(path.exists("my_classifierN.pickle")):  
  f = open('my_classifierN.pickle', 'rb')
  classifierN = pickle.load(f)
  print("Accuracy of Naive bayes is: "+str(nltk.classify.accuracy(classifierN, test_set)))
  f.close()
#If no create new model
else:
  f = open('my_classifierN.pickle', 'wb')
  classifierN = nltk.NaiveBayesClassifier.train(train_set)
  print("Accuracy of Naive bayes is: "+str(nltk.classify.accuracy(classifierN, test_set)))
  pickle.dump(classifierN, f)
  f.close()
  
#same but create Decision tree classifier
if(path.exists("my_classifierD.pickle")):  
  f = open('my_classifierD.pickle', 'rb')
  classifierD = pickle.load(f)
  print("Accuracy of Decision Tree: "+str(nltk.classify.accuracy(classifierD, test_set)))
  f.close()
else:
  f = open('my_classifierD.pickle', 'wb')
  classifierD = nltk.DecisionTreeClassifier.train(train_set)
  print("Accuracy of Decision Tree: "+str(nltk.classify.accuracy(classifierD, test_set)))
  pickle.dump(classifierD, f)
  f.close()
#same but create Support Vector Classifier
if(path.exists("my_classifierS.pickle")):  
  f = open('my_classifierS.pickle', 'rb')
  classifierS = pickle.load(f)
  print("Accuracy of Support Vector Classifier: "+str(nltk.classify.accuracy(classifierS, test_set)))
  f.close()
else:
  f = open('my_classifierS.pickle', 'wb')
  classifierS = nltk.classify.SklearnClassifier(LinearSVC())
  classifierS.train(train_set)
  print("Accuracy of Support Vector Classifier: "+str(nltk.classify.accuracy(classifierS, test_set)))
  pickle.dump(classifierS, f)
  f.close()

#check accuracy
#print(classifierN.classify(document_features({'doc':"Mit is a nice person"})))
#print(classifierD.classify(document_features({'doc':"Mit is a nice person"})))
#print(classifierS.classify(document_features({'doc':"Mit is a nice person"})))
