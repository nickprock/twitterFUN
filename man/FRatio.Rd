\name{FRatio}
\alias{FRatio}
\title{
give follower, following e Follower/Following Ratio about an user
}
\description{
Use a getUser() function in package twitteR to calculate the Follower/Following ratio
}
\usage{
FRatio(x)
}
\arguments{
  \item{x}{
a character string
}
}
\details{
To use this feature you need a twitter account and twitter app. To create the app go here www.twitter.com/apps
}
\value{
a list
  \item{screenName }{the user's screenName}
  \item{follower}{the user's follower}
  \item{following}{the user's following}
  \item{FRatio }{the user's follower/following ratio}
}
\author{
Nicola Procopio
}
\examples{
## test<-FRatio('nytimes')
}
