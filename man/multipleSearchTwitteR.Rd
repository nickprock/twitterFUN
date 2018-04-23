\name{multipleSearchTwitteR}
\alias{multipleSearchTwitteR}
\title{
function to scrape from twitter's API with multiple search query
}
\description{
The function scrapes from official twitter's API using multiple search query and creates a dataset about output of API adding the column saercQuery
}
\usage{
multipleSearchTwitteR(searchQuery, number, language = NULL, myConsumer_key, myConsumer_secret, myAccess_token = NULL, myAccess_secret = NULL)
}
\arguments{
  \item{searchQuery}{
a character string
}
  \item{number}{
the number of tweets for each searchQuery
}
  \item{language}{
the language of tweets
}
  \item{myConsumer_key}{
your consumer key
}
  \item{myConsumer_secret}{
your consumer secret
}
\item{myAccess_token}{
your access token
}
\item{myAccess_secret}{
your access secret
}
}
\details{
To use this feature you need a twitter account and twitter app. To create the app go here www.twitter.com/app.
Work with direct authentication.
}
\value{
a data.frame
}
\references{
http://www.inside-r.org/packages/cran/twitter/docs/Rtweets
}
\author{
Nicola Procopio
}
\examples{
## not run
# sq<-c("obama", "#googledoodle", "david bowie")
# dati<-multipleSearchTwitteR(searchQuery = sq, number = 3, language = "en",
# myConsumer_key = "yourConsumerKey", 
# myConsumer_secret = "yourConsumerSecret",
# myAccess_token = "myAccess_token", 
# myAccess_secret = "myAccess_secret")
}