### MAP

## Loading the data on the Leaflet map


## one row with NA latitude and longitude, remove that row

new_york <- read.csv("./data/ny.csv",stringsAsFactors = FALSE)
sf <- read.csv("./data/sf.csv",stringsAsFactors = FALSE)
dallas <- read.csv("./data/dallas.csv",stringsAsFactors = FALSE)


business_all <- dplyr::bind_rows(mutate(new_york, city = "NYC"),
                                 mutate(sf, city = "SF"),
                                 mutate(dallas, city = "Dallas"))

df_map <- business_all[-which(is.na(business_all$latitude)),]
#########--
getColor <- function(df_map) {
    sapply(df_map$rating, function(rating) {
        if(rating <=3.5) {
            "blue"
        } else if(rating <=4) {
            "orange"
        } else {
            "red"
        } })
}


icons <- awesomeIcons(
    icon = 'cutlery',
    iconColor = 'black',
    library = 'fa',
    markerColor = getColor(df_map),
    spin=T
)



mapStates = map("state", fill = TRUE, plot = FALSE)

leaflet() %>%    
    addProviderTiles(providers$CartoDB.Positron)%>%
    addPolygons(data = mapStates,fillColor = topo.colors(10, alpha = NULL), stroke = FALSE)%>% 
    addAwesomeMarkers(data = df_map, ~longitude, ~latitude, icon=icons,
                      label=~as.character(price),clusterOptions = T) 








