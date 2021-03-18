install.packages("selectr")
install.packages("xml2")
install.packages("rvest")

install.packages("stringr")
install.packages("jsonlite")

library(selectr)
library(xml2)
library(rvest)
library(stringr)
library(jsonlite)
install.packages("dplyr")
library(dplyr)

movie <- NULL
movie_genre <- NULL
movie_genre_tmp <- NULL
movie_rating <- NULL
movie_rating_tmp <- NULL
movie_title <- NULL
movie_title_tmp <- NULL
movie_year <- NULL
movie_year_tmp <- NULL
r_index <- NULL

url <- "https://www.imdb.com/search/title/?title_type=feature&year=2019-12-01,2021-02-28&sort=year,asc"

url_list <- list()

for(i in seq(1,12666, 50)){
  if(i==1){
    url_temp <- url
  } else {
    url_temp <- paste(url,"&start=", i, "&ref_=adv_nxt", sep="")
  }
  
  url_list <- append(url_list, url_temp)
}

url_list




for(url in url_list){
  
  read_html(url) -> imdb
  
  movie <- imdb %>%
    html_nodes("div.lister-list > div.lister-item > div.lister-item-content")
  
  # ?????? ??????
 #  movie_title <- c(movie_title, movie %>%
#                    html_nodes("h3.lister-item-header > a") %>%
 #                   html_text(movie_title_tmp)) 
 
  # ?????? ??????
 # movie_year <- c(movie_year, movie %>%
  #                html_nodes("h3.lister-item-header > span.lister-item-year") %>%
  #                html_text(movie_year_tmp)) 
  
  
  # ??????
  # movie_genre <-  c(movie_genre, movie %>%
   #                  html_nodes("p.text-muted > span.genre") %>%
  #                   html_text(movie_genre_tmp))
  
  # ?????? 
   movie_rating <- c(movie_rating, movie %>%
                    html_nodes("div.ratings-bar") %>%
                    html_text(movie_rating_tmp))
 c(movie_rating, gsub(" |\n", "", movie_rating)) -> movie_rating
  c(r_index, regexpr("R", movie_rating)) -> r_index
 c(movie_rating, substr(movie_rating, 0, r_index-1)) -> movie_rating
  
}


memory.limit(size=500000)  
  gsub("[[:punct:]]|I", "", movie_year) -> movie_year 
  gsub("V", "", movie_year) -> movie_year 
  gsub(" |\n", "", movie_year) -> movie_year
  gsub("X", "", movie_year) -> movie_year 
  gsub("XX", "", movie_year) -> movie_year
gsub(" |\n", "", movie_genre) -> movie_genre
  
  
  gsub("\n", " ", movie_rating) -> movie_rating
  regexpr("R", movie_rating) -> r_index
  substr(movie_rating, 0, r_index-1) -> movie_rating
  gsub(" ","",movie_rating) -> movie_rating
  
getwd()

print(movie_title)
print(movie_year)
print(movie_rating)
View(movie_year)
length(movie_genre)
length(movie_year)

print(movie_genre)
movie_anal2 <- cbind(movie_year[1:23602],movie_genre[1:23602])
movie_anal <- cbind(movie_year[1:1319588],movie_genre[1:1319588], movie_rating[1:1319588])
as.data.frame(movie_anal2)
colnames(movie_anal) <- c("year", "genre", "rating")


install.packages("plyr")
library(plyr)

count(movie_anal2[,1])


View(movie_title)
View(movie_anal2)
View(movie_rating)

install.packages("stringr")
library("stringr")
s <- str_split_fixed(movie_anal2[,2], ",",5)
library(tidyr)
library(dplyr)
separate_rows(movie_anal2, movie_anal2[,2], convert = TRUE)
install.packages("splitstackshape")
library(splitstackshape)
cSplit(movie_anal2, movie_anal2[,2], sep = ",", direction = "long")
movie_anal2 <- str_split_fixed(movie_anal2[,2], ",",5)
movie_anal2 <- cbind(movie_year[1:length(movie_year)],movie_anal2)
movie_anal2 <- na.omit(movie_anal2)
movie_anal3 <- cbind(movie_year[1:length(movie_year)],movie_anal2[1:length(movie_year),2], movie_rating[1:length(movie_year)])
movie_anal3 <- na.omit(movie_anal3)
table(movie_anal2[,1], movie_anal2[,2])
table(movie_anal3[,1], movie_anal3[,2],movie_anal3[,3])


plot(as.factor(movie_anal2[,2]))

require(ggplot2)
qplot(movie_anal2[,2])


print(movie_anal2)


## PCA
cwPr <- prcomp(voice_data[,1:21], center = TRUE, scale = TRUE)
summ_cwPr <- summary(cwPr)
plot(cwPr, type = "l")
biplot(cwPr, scale = 0)

## Extract PC Scores
str(cwPr)
cwPr$rotation #identify eigenvalue 
voice_data[,22:25] <- cwPr$x[,1:4]
colnames(voice_data)[22] = c("PC1")
colnames(voice_data)[23] = c("PC2")
colnames(voice_data)[24] = c("PC3")
colnames(voice_data)[25] = c("PC4")

## neural with PC (Hidden3)
NN2_PC = neuralnet(label ~ PC1+PC2+PC3+PC4, datatrain, algorithm = "rprop+", hidden=3, stepmax = 1e6)
predict_testNN2_PC = compute(NN2_PC, datatest[,c(21:25)])
results2_PC <- data.frame(actual = datatest$label, prediction = predict_testNN2_PC$net.result)
plot(NN2_PC)

##predicting and making confusion matrix
roundedresults4 <- sapply(results,round,digits=0)
roundedresultsdf4=data.frame(roundedresults4)
attach(roundedresultsdf4)
table(actual,prediction)
