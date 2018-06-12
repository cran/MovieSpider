\name{Douban}
\alias{Douban}
\title{
Obtain movie data from Douban website(movie.douban.com)
}
\description{
With this function, users can obtain the real-time movie data from Douban. The type of data returned depends on the value of argument 'type'.
}
\usage{
Douban(type = "top")
}

\arguments{
  \item{type}{
  The type of movie data.
  Available options includes "latest" and "top" for the data of the latest popular movies in China and data of 25 top rated movies respectively.

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
Douban()
Douban('latest')
Douban('top')
}