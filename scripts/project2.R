# install tm - uncomment if you haven't already installed
#install.packages("tm")
#install.packages("NLP")
#install.packages("ggplot2")
library(ggplot2)
#install.packages("openNLP")
#install.packages("openNLPmodels.en")
#install.packages("openNLPmodels.en", repos = "http://datacube.wu.ac.at/", type = "source")
library(openNLP)
library(openNLPmodels.en)
library(NLP)
library(tm)
#install.packages("topicmodels")
library(topicmodels)
#install.packages("stringr")
library(stringr)

#install.packages("reshape")
library(reshape)
#install.packages("wordcloud")
#install.packages("RColorBrewer")
library(RColorBrewer)
library(wordcloud)
#install.packages("dplyr")
#library(dplyr)

# use tm
library(tm)

#============================================
#set directory and read files
#============================================

#set the directory
setwd("../BDA_P1-master")
getwd()

# read in the emails. Col 1 is emailer, Col 2 is the text of the email as a "document"
emails = read.csv("../allTopEmailers.csv",sep='|')

# turn the documents into a corpus, as in lecture 4
corpus = VCorpus(VectorSource(emails$txt))

# inspect VCorpus
inspect(corpus)

# define function discussed in ppt
removeNumPunct = function(x) gsub("[^[:alpha:][:space:]]*", "", x)

# remove stopwords in an intermediate corpus
myStopwords <- c(stopwords('english'))
intermed = tm_map(corpus,removeWords,myStopwords)

# apply it to each document using tm_map
cleanCorpus = tm_map(intermed,content_transformer(removeNumPunct))

# get the TDMatrix
termDocMat = TermDocumentMatrix(cleanCorpus)


# ---------------LDA Topic Modeling----------------------
# use Latent Dirichlet Allocation to determine topics:
topicModel = LDA(termDocMat,20)

# get topic-document matrix
topDocMat = as.data.frame(topicModel@gamma)

# get top topics per doc and count them 
toptopics = as.data.frame(cbind(document = row.names(topDocMat),
                  topic = apply(topDocMat,1,function(x) names(topDocMat)[which(x==max(x))[1]])))

# print the number of documents with that topic among its top topics
table(toptopics$topic)


# ---------------Get top 25, top 5 documents-----------------

# subset to the longest documents
asDF = data.frame(text=unlist(sapply(corpus, `[`, "content")), 
                  stringsAsFactors=F)
asDF$lengths = nchar(asDF$text)

# top 25 longets emails in a dataframe
asDFTrunc = asDF[with(asDF,order(-lengths)),][1:26,1]


#----------------work through top 25 info --------------

top25Corp = VCorpus(VectorSource(asDFTrunc))
intermed = tm_map(top25Corp,removeWords,myStopwords)
top25Corp = tm_map(intermed,content_transformer(removeNumPunct))
top25TM = removeSparseTerms(TermDocumentMatrix(top25Corp),sparse = 0.75)

distMat = dist(scale(top25TM))
fit25 <- hclust(distMat,method = "ward.D2")
plot(fit25)

wordcloud(top25Corp[1])
wordcloud(top25Corp[2])
wordcloud(top25Corp[3])
wordcloud(top25Corp[4])
wordcloud(top25Corp[5])


#----------LONGEST SENTENCES-------------------
convert_text_to_sentences <- function(text, lang = "en") {
  sentence_token_annotator <- Maxent_Sent_Token_Annotator(language = lang)
  # Convert text to class String from package NLP
  text <- as.String(text)
  # Sentence boundaries in text
  sentence.boundaries <- annotate(text, sentence_token_annotator)
  # Extract sentences
  sentences <- text[sentence.boundaries]
  # return sentences
  return(sentences)
}
reshape_corpus <- function(current.corpus, FUN, ...) {
  # Extract the text from each document in the corpus and put into a list
  text <- lapply(current.corpus, content)
  # Basically convert the text
  docs <- lapply(text, FUN, ...)
  docs <- as.vector(unlist(docs))
  # Create a new corpus structure and return it
  new.corpus <- Corpus(VectorSource(docs))
  return(new.corpus)
}
sentenceCorp = reshape_corpus(intermed,convert_text_to_sentences)
cleanSentenceCorp = tm_map(sentenceCorp,content_transformer(removeNumPunct))
lens = sapply(cleanSentenceCorp,nchar)
ldf = as.data.frame(table(lens))

# get sentence lengths
#------------------update------------------------------------
#the functions I've tried, sorry didn't fit the variable name

#============================================
#data initialization
#============================================
#Create a Term Document Matrix
corpusTDM=TermDocumentMatrix(corpus)
corpusTDM

inspect(corpusTDM[100:110,1:10])

#============================================
#data cleaning
#============================================

#Convert the corpus to lower case
corpusLowCase <- tm_map(corpus, content_transformer(tolower))
corpusLowCase

##remove extra white space
##corpusTrans<- tm_map(corpusLowCase, content_transformer(stripWhitespace))
##corpusTrans2<- tm_map(corpusLowCase, stripWhitespace)

##remove numbers
##corpusTrans<- tm_map(corpusTrans, content_transformer(removeNumbers))
##corpusTrans2<- tm_map(corpusTrans, removeNumbers)

#Function containing regex pattern to remove email id
RemoveEmail <- function(x) {
  require(stringr)
  str_replace_all(x,"[a-zA-Z0-9_.+-]+@[a-zA-Z0-9-]+\\.[a-zA-Z0-9-.]+", "")
} 

corpusTrans<- tm_map(corpusLowCase, content_transformer(RemoveEmail) )
corpusTrans2<- tm_map(corpusLowCase, RemoveEmail)
inspect(corpusTrans2)

#Remove anything other than English letters or spaces
#removeNumPunct <-
#  function(x) gsub("[^[:alpha:][:space:]]*", "", x)

#corpusClean <- tm_map(corpusLowCase,content_transformer(removeNumPunct)) 

corpusTrans <- tm_map(corpusTrans,content_transformer(removeNumPunct))
corpusTrans2 <- tm_map(corpusTrans2,removeNumPunct)
#remove extra white space again,
#since remove number and punctuation cause extra white space
corpusClean<- tm_map(corpusTrans,content_transformer(stripWhitespace) )
corpusClean2<- tm_map(corpusTrans2,stripWhitespace)
inspect(corpusClean)
inspect(corpusClean2)


#corpusClean <- tm_map(corpusTrans,content_transformer(tolower))
cleanTDM <- TermDocumentMatrix(corpusClean)
cleanTDM



#inspect(SATcltdm[1:10,1:10])

#Remove stopwords from the corpus
myStopwords <- c(stopwords('english')) 
myStopwords

removeStop <- tm_map(corpusClean, removeWords, myStopwords) 
inspect(removeStop[1:10])

corpusTDM2<-TermDocumentMatrix(removeStop, control = list(wordLengths = c(1,Inf))) 
corpusTDM2

removeSparse<- removeSparseTerms(corpusTDM2, 0.98)
removeSparse

#with the different weighting schemes
corpusTDM3<-TermDocumentMatrix(removeStop, control = list(wordLengths = c(1,Inf),
                                                          weighting = weightBin) )
corpusTDM3

#corpusTDM4<-TermDocumentMatrix(removeStop, control = list(wordLengths = c(1,Inf),
#                                                          weighting= weightTfIdf)) 
#corpusTDM4


#Find terms with a frequency of 5 or more
freq.term=findFreqTerms(removeSparse,lowfreq = 5)
freq.term

#Find words associated with ???america???
findAssocs(corpusTDM2, "america", 0.25)

freq.term3=findFreqTerms(corpusTDM2, lowfreq = 5)
freq.term3

#find the frequency of each term
term.freq<- rowSums(as.matrix(removeSparse))
term.freq<-subset(term.freq, term.freq>5)
df<- data.frame(term = names(term.freq), freq = term.freq)
term.freq
df

#============================================
#plot
#============================================

ggplot(df,aes(x= term, y = freq)) + geom_bar(stat = "identity")+ 
  xlab("terms")+ylab("count")+coord_flip()


disMatrix<- dist(scale(removeSparse))
disMatrix

#hierarchical cluster analysis
hcl
fit <- hclust(disMatrix,method = "ward.D2")
plot(fit)
#hightlight the corresponding cluster
rh<- rect.hclust(fit, k=10)

freqwords<- sort(rowSums(as.matrix(removeSparse)), decreasing = TRUE)
freqwords

#Get the top50 words
top50<- melt(freqwords[1:60])
top50

#k-means clustering
tm<- t(disMatrix)
k<- 6
kr<- kmeans(tm, k)
round(kr$centers, digits = 3)

#wordcloud
pal <- brewer.pal(9, "BuGn")
pal <-pal[-(1:4)]
wordcloud(words = names(term.freq), freq = term.freq, min.freq = 20,
          random.order = FALSE, colors = pal)


