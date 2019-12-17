library(yelpr)
library(tidyverse)
library(magrittr)
library(leaflet)
library(maps)


# install.packages("textreadr")

key <- textreadr::read_rtf("API.rtf")


## Location: New York City, San Francisco, Dallas
## categories : Restaurants
## term: Janpanese

## Build a function to acquire business automatically
get_business <- function(api_key, location,term){
    ## get business end info from given criteria
    business <- business_search(api_key = key,
                                location = location,
                                categories = "Restaurants",
                                term = term,
                                limit = 50)
    ## if total business number is less than 50, then just return the searched
    ## business. If number is greater than 50, then we need to use multiple
    ## query by setting proper offsets
    
    business_df <- get_df(business = business)
    
    if(business$total>50){ 
        loop_i <- get_offset_i(business)
        for(i in 1:loop_i){
            temp_df <- business_search(api_key = key,
                                       location = location,
                                       categories = "Restaurants",
                                       term = term,
                                       limit = 50,
                                       offset = i*50)
            temp_df <- get_df(business = temp_df)
            business_df <- dplyr::bind_rows(business_df,temp_df)}
    }
    return(business_df)
}

## supplimental function, get index for offset loop
get_offset_i <- function(business){
    temp <-  business$total/50
    temp_int <- round(temp)
    rem_add <- if_else(temp-temp_int>0,true = 1,false = 0)
    i <-  temp_int+rem_add-1
    i <- min(i,19)
    return(i)
}

## Data Wrangling Process
get_df <- function(business){
    business_coor <- business$businesses$coordinates
    
    business_loc <- business$businesses$location%>%
        dplyr::select(-display_address)
    
    business_df <- business$businesses%>%
        dplyr::select(-categories,-transactions,-coordinates,-location)
    
    business_df <- bind_cols(business_coor,business_loc,business_df)
    return(business_df)
}

## Use the above implementation to get citywise restaurant data based on genre
get_city_df <- function(genre,location){
    for(i in 1:length(genre)){
        if(i==1) df <- get_business(api_key = key,
                                    location = location,
                                    term = genre[i])%>%
                dplyr::mutate(genre = genre[i])
        
        else df <- dplyr::bind_rows(df,
                                    get_business(api_key = key,location = location,term = genre[i])%>%
                                        dplyr::mutate(genre = genre[i]))
    }
    return(df)
}

##------------------------------------------------------------------------------
##
## Get actual Data:
## 1. specify genre
genre <- "Japanese"

## get NYC, SF and Dallas data

new_york <- get_city_df(genre = genre,location = "New York City") %>% distinct()

sf <- get_city_df(genre = genre,location = "San Francisco") %>% distinct()

dallas <- get_city_df(genre = genre,location = "Dallas") %>% distinct()


ny_id <- new_york$id

sf_id <- sf$id

test <- yelpr::business_search_review(api_key = key,business_id = ny_id[1])


## store data in data folder
write_csv(new_york,"./data/ny.csv")
write_csv(sf,"./data/sf.csv")
write_csv(dallas,"./data/dallas.csv")

id_ny <- new_york%>%
    dplyr::select(id)%>%
    dplyr::group_by(id)%>%
    dplyr::summarise( n())%>%
    dplyr::filter(`n()`>1)%>%
    dplyr::select(id)%>%
    pull()
View(new_york%>%dplyr::filter(id %in% id_ny))


####### Select ratings >= 4.5 in NY and SF

ny_high <- dplyr::filter(new_york,rating>=4.5)
sf_high <- dplyr::filter(sf,rating>=4.5)
dl_high <- dplyr::filter(dallas,rating>=4.5)


## extract reveiw text using business_search_review with each busidess id 
review <- NULL
combine_review <- function(id){
    text <- yelpr::business_search_review(api_key = key,business_id = id)$reviews["text"]
    review <- data.frame(id=id,test=text,stringsAsFactors = F)
    return(review) 
}

review_ny <- lapply(ny_high$id, FUN=combine_review)

length(review_ny)

review_sf <- lapply(sf_high$id, FUN=combine_review)

length(review_sf)


review_dl <- lapply(dl_high$id, FUN=combine_review)

length(review_dl)



## bind_rows NY
ny_high_review <- NULL

for (i in 1:length(review_ny)){
  
    ny_high_review <- dplyr::bind_rows(ny_high_review,review_ny[[i]])

}

View(ny_high_review)


## bind_rows SF
sf_high_review <- NULL

for (i in 1:length(review_sf)){
    
    sf_high_review <- dplyr::bind_rows(sf_high_review,review_sf[[i]])
    
}

View(sf_high_review)


## bind_rows dallas
dl_high_review <- NULL

for (i in 1:length(review_dl)){
    
    dl_high_review <- dplyr::bind_rows(dl_high_review,review_dl[[i]])
    
}

View(dl_high_review)


## store data in data folder
write_csv(ny_high_review,"./data/ny_high_review.csv")
write_csv(sf_high_review,"./data/sf_high_review.csv")
write_csv(dl_high_review,"./data/dl_high_review.csv")


### Average review ratings by state.

# new_york
# sf
# dallas


business_all <- bind_rows(mutate(new_york, city = "NYC"),
          mutate(sf, city = "SF"),
          mutate(dallas, city = "Dallas"))

## Average Japanese food review ratings by City.

dataGroupByCityStar <- business_all %>% 
    mutate(tsum = n()) %>% 
    group_by(city, rating) 

dataForTableByCityStar <- dataGroupByCityStar %>% group_by(city) %>%
    summarise(total_business = n(), total_reviews = sum(review_count), avg_rating = round(mean(rating),2)) %>%
                  arrange(desc(avg_rating))

library(pander)
panderOptions("digits", 3)
pander(dataForTableByCityStar, caption = "Japanese Food")

#View(dataForTableByCityStar)


#### Map data --------------------------------------------------------------------
## one row with NA latitude and longitude, remove that row
df_map <- business_all[-which(is.na(business_all$latitude)),]


