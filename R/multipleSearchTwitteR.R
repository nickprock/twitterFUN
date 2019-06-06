multipleSearchTwitteR <-
function(searchQuery, number, language=NULL, 
           myConsumer_key, myConsumer_secret,
           myAccess_token, myAccess_secret){

setup_twitter_oauth(consumer_key =  myConsumer_key, consumer_secret = myConsumer_secret, 
                        access_token = myAccess_token, access_secret = myAccess_secret)
    #####################################################
    # esporta e immagazina tutto in una lista
    multidf<-list()
    for (i in 1:length(searchQuery)){
      tweets<-searchTwitteR(searchQuery[i], n=number, lang = language )
      df <- twListToDF(tweets)
      df$searchQuery<-searchQuery[i]
      multidf[[i]]<-df
    }
    #####################################################
    # merge dei data.frame nella lista
    mymergedata<-do.call("rbind", multidf)
    return(mymergedata)
  }
