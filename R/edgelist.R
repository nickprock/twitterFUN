edgelist <-
function(dati, type){
  
          if(type=="RT"){
            RT<-str_extract_all(dati$text, "^RT (@[[:alnum:]_]*)")
            RT<-sub("^RT @*","",RT) # elimina caratteri superflui
            RT[RT=="character(0)"] <-"NA" # sostituisce la stringa con un'altra
            dati$RT<-RT # crea nuova colonna nel data.frame
            new_df <- dati[dati$RT!="NA",] # crea nuova colonna nel data.frame
            edgelist <- data.frame(user1 = new_df$screenName, user2 = new_df$RT)
          } else{
            if (type == "hashtag"){
              hashtag.regex <- perl("(?<=^|\\s)#\\S+")
              hashtag<-str_extract_all(dati$text, hashtag.regex)
              hashtag[hashtag=="character(0)"] <-"NA"
              # inserisce una lista in ogni cella del data.frame
              for(i in 1:length(dati$text)){
                dati$hashtag[i]<-(paste(unlist(hashtag[[i]]), collapse = " && "))
              }
              dati$hashtag <- unlist(dati$hashtag)
              # per trovare il massimo numero di parole co-occorrenti
              # e quindi di colonne del data.frame
              test <- strsplit(dati$hashtag, split = " && ")
              dim<-vector()
              for (i in 1:length(test)){
                dim[i]<-length(test[[i]])
              }
              mdim<-max(dim)
              # creo il nuovo data.frame
              df <- as.data.frame(setNames(replicate(mdim,character(length(dati$hashtag)), simplify = F),
                                           paste0("word_",seq(1,mdim))))
              # inizializzo le colonne altrimenti mi dà un errore dovuto alla sparsita' dei dati
              for (i in 1:mdim){
                df[,i]<-"NA"
              }
              # creo una lista di termini
              l<-strsplit(dati$hashtag,' && ')
              # popolo il dataset
              for (j in 1:mdim){
                for (i in 1:length(l)){
                  df[i,j]<-l[[i]][j]
                }
              }
              # per fare i confronti mi serve un data.frame pieno
              # uso la stringa fittizia "NA" al posto di NA
              df<-replace(df, is.na(df), "NA")
              ########################################
              # bisogna creare tutte le combinazioni
              listofdataframes<-list()
              for (i in 1:(mdim-1)){
                listofdataframes[[i]]<-data.frame(e1=rep(df[,i],mdim-i),
                                                  e2=cbind(unlist(df[,(i+1):mdim])))
              }
              #########################################
              # creo la edgelist
              edgelist<-do.call("rbind",listofdataframes)
              # tenere solo i casi senza NA
              rownames(edgelist)<-NULL
              edgelist<-edgelist[edgelist$e1!="NA",]
              edgelist<-edgelist[edgelist$e2!="NA",]
            } else {
              print("ERROR: WRONG TYPE")
              
            }
          }
          return(edgelist)
          
          }
