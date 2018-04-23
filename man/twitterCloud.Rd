\name{twitterCloud}
\alias{twitterCloud}
\title{
function to create a wordcloud
}
\description{
This function creates a wordcloud after preprocessed data (claeaning, stemming ...)
}
\usage{
twitterCloud(x, stopwordLanguages = NULL, wordLength = 0, wordFrequencies = 1, scale = c(3.5, 0.3), colors = NULL)
}
\arguments{
  \item{x}{
a character string
}
  \item{stopwordLanguages}{
the language of stopword to be deleted
}
  \item{wordLength}{
the length of terms to be included in wordcloud. Default 0 (all terms)
}
  \item{wordFrequencies}{
the frequency of terms to be included in wordcloud. Default 1 (all terms)
}
  \item{scale}{
the dimension of terms in the wordcloud (figure). Default c(3.5, 0.3)
}
  \item{colors}{
The colors of wordcloud. Default NULL (backgroud white, terms black)
}
}
\details{
To use this feature you need a twitter account and twitter app. To create the app go here www.twitter.com/apps
}
\references{
http://www.rdatamining.com/examples/text-mining
}
\author{
Nicola Procopio
}
\examples{
## data(tweets)
## twitterCloud(tweets$text, stopwordLanguages = stopwords('en'), wordLength = 5, wordFrequencies = 2, colors = brewer.pal(5,'Dark2'))
}