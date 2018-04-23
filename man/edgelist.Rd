\name{edgelist}
\alias{edgelist}
\title{
function to create a retweets or hashtags edgelist
}
\description{
This function creates an edgelist about retweets or hashtag
}
\usage{
edgelist(dati, type)
}
\arguments{
  \item{dati}{
a data.frame, it needs a column named "text"
}
  \item{type}{
a character string
}
}
\details{
To use this feature you need a twitter account and twitter app or a data.frame with a text column named text. To create the app go here www.twitter.com/apps
}
\value{
a data.frame
}
\author{
Nicola Procopio
}
\examples{
## edgelist(dataframe, type="RT")
## edgelist(dataframe, type="hashtag")
}
\keyword{ edgelist }
\keyword{ twitter }
