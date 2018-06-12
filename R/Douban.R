Douban <-
function(type='top'){
  if(type=='latest'){
#latest
URL='https://movie.douban.com/chart'
webdata <- readLines(URL,encoding="UTF-8")
nameRow <- grep('<a class=\"nbg\"',webdata)
namedata <- webdata[nameRow]
timeRow <- grep('<p class=\"pl\"',webdata)
timedata <- webdata[timeRow]
rateRow <- grep('<span class=\"rating_nums\">',webdata)
ratedata <- webdata[rateRow]
mnames <- gregexpr('title=\".+\"', namedata)
mtimes <- gregexpr('>\\S+', timedata)
mrates <- gregexpr('>\\w+', ratedata)
len = length(mnames)
movie.names = matrix(0, nrow = len, ncol = 1)
for(i in 1:len){
  movie.names[i,1] = substring(namedata[i], mnames[[i]]+7, (mnames[[i]]+attr(mnames[[i]], 'match.length'))[[1]]-2)
}
movie.times = matrix(0, nrow = len, ncol = 1)
for(i in 1:len){
  movie.times[i,1] = substring(timedata[i], mtimes[[i]]+1, (mtimes[[i]]+attr(mtimes[[i]], 'match.length'))[[1]]-1)
}
movie.rates = matrix(0, nrow = len, ncol = 1)
for(i in 1:len){
  movie.rates[i,1] = substring(ratedata[i], mrates[[i]]+1, (mrates[[i]]+attr(mrates[[i]], 'match.length'))[[1]]+1)
}
movie.rates<-as.numeric(movie.rates)
moviedata<-data.frame('Name'=movie.names,'Release_Time'=movie.times,'Rating'=movie.rates)
#View(moviedata)
return(moviedata)
}else if(type=='top'){
#top
URL = 'https://movie.douban.com/top250?start=0&filter='
webdata <- readLines(URL,encoding="UTF-8")
nameRow <- grep('<span class=\"title\">[\u4e00-\u9fa5]',webdata)
ratedRow <- grep('<span>\\d+',webdata)
rateRow<- grep('<span class=\"rating_num\"',webdata)
timeRow<-grep('\\d+&nbsp',webdata)
names <- webdata[nameRow]
rated<-webdata[ratedRow]
rates<-webdata[rateRow]
times<-webdata[timeRow]
mnames <- gregexpr('>\\w+', names)
mtimes<-gregexpr('\\d+', times)
mrates<-gregexpr('>\\w+', rates)
mrated<-gregexpr('\\d+', rated)
len1 = length(mnames)
len2=length(mtimes)
movie.names = matrix(0, nrow = len1, ncol = 1)
movie.times = matrix(0, nrow = len2, ncol = 1)
movie.rates = matrix(0, nrow = len2, ncol = 1)
movie.rated = matrix(0, nrow = len2, ncol = 1)
for(i in 1:len1){
  movie.names[i,1] = substring(names[i], mnames[[i]]+1, (mnames[[i]]+attr(mnames[[i]], 'match.length'))[[1]]-1)
}
for(i in 1:len2){
  movie.times[i,1] = substring(times[i], mtimes[[i]], (mtimes[[i]]+attr(mtimes[[i]], 'match.length'))[[1]]-1)
  movie.rates[i,1] = substring(rates[i], mrates[[i]]+1, (mrates[[i]]+attr(mrates[[i]], 'match.length'))[[1]]+1)
  movie.rated[i,1] = substring(rated[i], mrated[[i]], (mrated[[i]]+attr(mrated[[i]], 'match.length'))[[1]]-1)
  }
moviedata<-data.frame('Name'=movie.names,'Release_Time'=movie.times,'Rating'=movie.rates,'Total_num_of_people_rated'=movie.rated)
#View(moviedata)
return(moviedata)
}else{
  stop("The type entered should be 'latest' or 'top'")
}
#6.17 HBD to HJY
}
