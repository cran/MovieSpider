\name{Imdb}
\alias{Imdb}
\title{
Obtain movie data from IMDB website(www.imdb.com)
}
\description{
With this function, users can obtain the real-time movie data from IMDB. The type of data returned depends on the value of argument 'type'.
}
\usage{
Imdb(type = "top")
}
\arguments{
  \item{type}{
  The type of movie data.
  Available options includes "pop" and "top" for the data of top 100 most popular movies and data of 250 top rated movies respectively.
}
}
\value{
A dataframe including the movies'names,release time as well as their rating_scores
(if type="top",the dataframe would also include the total number of people who rated).
}
\author{
Rongze Zheng,Jingyu Hao
}

\examples{
Imdb()
Imdb('top')
Imdb('pop')
}
