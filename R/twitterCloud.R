twitterCloud <-
function(x, stopwordLanguages=NULL, wordLength=0, wordFrequencies=1, 
                      scale=c(3.5, 0.3), colors=NULL){
  x<-as.character(x)
  # remove URL
  x<-gsub('http\\S+\\s*', '', x)
  myCorpus<-Corpus(VectorSource(x))
  #Cleaning#
  ############################
  myCorpus<-tm_map(myCorpus, content_transformer(tolower))
  myCorpus<-tm_map(myCorpus, removePunctuation)
  myCorpus<-tm_map(myCorpus, removeNumbers)
  myStopwords<-stopwordLanguages
  myCorpus <- tm_map(myCorpus, removeWords, myStopwords)
  myCorpus<-tm_map(myCorpus, stripWhitespace)
  #Stemming#
  ###################
  dictCorpus<-myCorpus
  myCorpus<-tm_map(myCorpus, stemDocument)
  stemCompletion_mod <- function(x,dict=dictCorpus) {
    PlainTextDocument(stripWhitespace(paste(stemCompletion(unlist(strsplit(as.character(x)," ")),dictionary=dict),sep="", collapse=" ")))
  }
  myCorpus <- tm_map(myCorpus, stemCompletion_mod)
  #WordCloud#
  #######################
  myDtm <- TermDocumentMatrix(myCorpus, control = list(wordLengths =c(wordLength, Inf)))
  new_myDtm<-findFreqTerms(myDtm, lowfreq=wordFrequencies)
  m<-as.matrix(myDtm)
  v<-sort(rowSums(m), decreasing=TRUE)
  myNames<-names(v)
  d<-data.frame(word=myNames, freq=v, row.names = NULL)
  wordcloud(d$word, d$freq, scale=scale, min.freq=wordFrequencies,
            random.color = FALSE, colors=colors)
}
