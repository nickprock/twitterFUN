\name{tweets}
\alias{tweets}
\docType{data}
\title{
Tweets scraping with API and function multipleSearchTwitteR
}
\description{
The dataset is the result of scraping by API of twitter, i add the column searchQuery 
}
\usage{data("tweets")}
\format{
  A data frame with 15 observations on the following 17 variables.
  \describe{
    \item{\code{text}}{a character vector}
    \item{\code{favorited}}{a logical vector}
    \item{\code{favoriteCount}}{a numeric vector}
    \item{\code{replyToSN}}{a logical vector}
    \item{\code{created}}{a POSIXct}
    \item{\code{truncated}}{a logical vector}
    \item{\code{replyToSID}}{a logical vector}
    \item{\code{id}}{a character vector}
    \item{\code{replyToUID}}{a logical vector}
    \item{\code{statusSource}}{a character vector}
    \item{\code{screenName}}{a character vector}
    \item{\code{retweetCount}}{a numeric vector}
    \item{\code{isRetweet}}{a logical vector}
    \item{\code{retweeted}}{a logical vector}
    \item{\code{longitude}}{a logical vector}
    \item{\code{latitude}}{a logical vector}
    \item{\code{searchQuery}}{a character vector}
  }
}
\source{
Twitter API
}
\examples{
data(tweets)
## maybe str(tweets) ; plot(tweets) ...
}
\keyword{datasets}