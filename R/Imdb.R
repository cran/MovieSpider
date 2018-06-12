Imdb <-
function(type='top'){
  if(type=='pop'){
    #'pop'
    URL1='https://www.imdb.com/chart/moviemeter?ref_=nv_mv_mpm_8'
    webdata <- readLines(URL1,encoding="UTF-8")
    nameRow <- grep('title=\".+>.+</a>',webdata)
    namedata <- webdata[nameRow]
    namedata<-namedata[2:length(namedata)]
    timeRow <- grep('<span class=\"secondaryInfo\">\\(\\d+\\)',webdata)
    timedata <- webdata[timeRow]
    rateRow <- grep('<td class=\"ratingColumn imdbRating\">',webdata)
    ratedata <- webdata[rateRow+1]
    ratenullRow<-grep('</td>',ratedata)
    mnames <- gregexpr('>.+<', namedata)
    mtimes <- gregexpr('\\(\\d+\\)', timedata)
    mrates <- gregexpr('>\\S+<', ratedata)
    len = length(mnames)
    movie.names = matrix(0, nrow = len, ncol = 1)
    movie.times = matrix(0, nrow = len, ncol = 1)
    movie.rates = matrix(0, nrow = len, ncol = 1)
    for(i in 1:len){
      movie.names[i,1] = substring(namedata[i], mnames[[i]]+1, (mnames[[i]]+attr(mnames[[i]], 'match.length'))[[1]]-2)
      movie.times[i,1] = substring(timedata[i], mtimes[[i]]+1, (mtimes[[i]]+attr(mtimes[[i]], 'match.length'))[[1]]-2)
      movie.rates[i,1] = substring(ratedata[i], mrates[[i]]+1, (mrates[[i]]+attr(mrates[[i]], 'match.length'))[[1]]-2)
    }
    movie.rates[ratenullRow]<-'Not Rated Yet'
    moviedata<-data.frame('Name'=movie.names,'Time'=movie.times,'Rating'=movie.rates)
    return(moviedata)
  }else if(type=='top'){
    #top
    URL2='https://www.imdb.com/chart/top'
    webdata <- readLines(URL2,encoding="UTF-8")
    nameRow <- grep('title=\".+>.+</a>',webdata)
    namedata <- webdata[nameRow]
    namedata<-namedata[2:length(namedata)]
    timeRow <- grep('<span class=\"secondaryInfo\">\\(\\d+\\)',webdata)
    timedata <- webdata[timeRow]
    rateRow <- grep('<strong title=\"',webdata)
    ratedata <- webdata[rateRow]
    mnames <- gregexpr('>.+<', namedata)
    mtimes <- gregexpr('\\(\\d+\\)', timedata)
    mrates <- gregexpr('>\\S+<', ratedata)
    mrated<-gregexpr('on \\S+ user', ratedata)
    len = length(mnames)
    movie.names = matrix(0, nrow = len, ncol = 1)
    movie.times = matrix(0, nrow = len, ncol = 1)
    movie.rates = matrix(0, nrow = len, ncol = 1)
    movie.rated = matrix(0, nrow = len, ncol = 1)
    for(i in 1:len){
      movie.names[i,1] = substring(namedata[i], mnames[[i]]+1, (mnames[[i]]+attr(mnames[[i]], 'match.length'))[[1]]-2)
      movie.times[i,1] = substring(timedata[i], mtimes[[i]]+1, (mtimes[[i]]+attr(mtimes[[i]], 'match.length'))[[1]]-2)
      movie.rates[i,1] = substring(ratedata[i], mrates[[i]]+1, (mrates[[i]]+attr(mrates[[i]], 'match.length'))[[1]]-2)
      movie.rated[i,1] = substring(ratedata[i], mrated[[i]]+3, (mrated[[i]]+attr(mrated[[i]], 'match.length'))[[1]]-5)
    }
    moviedata<-data.frame('Name'=movie.names,'Release_Time'=movie.times,'Rating'=movie.rates,'Total_num_of_people_rated'=movie.rated)
    return(moviedata)
  }else{
    stop("The type entered should be 'pop' or 'top'")
  }
  #6.17 HBD to HJY
}
