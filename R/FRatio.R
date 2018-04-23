FRatio <-
function(x){
  user<-getUser(paste0('@', x))
  FRatio<-(user$followersCount/user$friendsCount)
  user<-list(screenName=user,follower=user$followersCount,following=user$friendsCount,FRatio=FRatio)
  return(user)
}
