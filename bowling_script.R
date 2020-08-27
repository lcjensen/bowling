library(httr)
library(jsonlite)
library(data.table)
link <- "http://13.74.31.101/api/points"
get <- fromJSON(txt=link) # hent data
points <- as.data.frame(get$points) # gem data som dataframe

points$presum <- points$V1+points$V2 #lav simpel sum

points$sum <- ifelse(points$V1==10, shift(points$presum+10, 1L, type="lead"),
                     ifelse(points$presum==10, shift(points$V1+10, 1L, type="lead"), 
                             points$presum)) #udregn strike og spare

points$points <- ifelse(is.na(points$sum), points$presum,points$sum) #genindfør simpel sum på linje 1
points <- within(points, points <- cumsum(points)) #beregn kumulativ sum

body <- list(token = get$token, points=points$points) #lav liste med token og resultater

output <- POST(link, body = body, encode = "json",verbose()) #send til POST

verification <- fromJSON(content(output,type="text")) #verificer at pointscoren er korrekt
