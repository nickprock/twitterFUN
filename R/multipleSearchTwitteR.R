multipleSearchTwitteR <-
function(searchQuery, number, language=NULL, 
           myConsumer_key, myConsumer_secret,
           myAccess_token=NULL, myAccess_secret=NULL){

setup_twitter_oauth(consumer_key =  myConsumer_key, myConsumer_secret = myConsumer_secret, 
                        myAccess_token = myAccess_token, myAccess_secret = myAccess_secret)
    #####################################################
    # esporta e immagazina tutto in una lista
    multidf<-list()
    for (i in 1:length(searchQuery)){
      tweets<-searchTwitteR(searchQuery[i], n=number, lang = language )
      df <- do.call("rbind", lapply(tweets, as.data.frame))
      df$searchQuery<-searchQuery[i]
      multidf[[i]]<-df
    }
    #####################################################
    # merge dei data.frame nella lista
    mymergedata<-do.call("rbind", multidf)
    return(mymergedata)
  }
