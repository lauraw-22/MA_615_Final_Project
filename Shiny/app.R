library(shiny)
library(shinydashboard)
library(plotly)
library(readxl)
library(DT)
library(tidyverse)
library(magrittr)
library(gridExtra)
library(scales)
library(shinydashboardPlus)
library(highcharter)
library(shinycssloaders)
library(shinyWidgets)
library(leaflet)
library(pander)
library(maps)
library(tidytext)
library(stringr)
library(wordcloud)
library(reshape2)

### read data ###
new_york <- read.csv("./data/ny.csv",stringsAsFactors = FALSE)
sf <- read.csv("./data/sf.csv",stringsAsFactors = FALSE)
dallas <- read.csv("./data/dallas.csv",stringsAsFactors = FALSE)


business_all <- dplyr::bind_rows(mutate(new_york, city = "NYC"),
                          mutate(sf, city = "SF"),
                          mutate(dallas, city = "Dallas"))

df_map <- business_all[-which(is.na(business_all$latitude)),]

city_list <- unique(df_map$city)
price_list <- unique(df_map$price)

### Text Mining Data Preparation
ny_high <- dplyr::filter(new_york,rating>=4.5)
sf_high <- dplyr::filter(sf,rating>=4.5)
dl_high <- dplyr::filter(dallas,rating>=4.5)

ny_high_review_r <- read.csv("./data/ny_high_review.csv",stringsAsFactors = FALSE)
sf_high_review_r <- read.csv("./data/sf_high_review.csv",stringsAsFactors = FALSE)
dl_high_review_r <- read.csv("./data/dl_high_review.csv",stringsAsFactors = FALSE)



### NY
tidy_ny <- ny_high_review_r %>% unnest_tokens(word, text)

tidy_ny <- tidy_ny %>%
    anti_join(stop_words)


tidy_ny %>%
    count(word, sort = TRUE) 

### SF
tidy_sf <- sf_high_review_r %>% unnest_tokens(word, text)

tidy_sf <- tidy_sf %>%
    anti_join(stop_words)


tidy_sf %>%
    count(word, sort = TRUE) 


### Dallas
tidy_dl <- dl_high_review_r %>% unnest_tokens(word, text)

tidy_dl <- tidy_dl %>%
    anti_join(stop_words)


tidy_dl %>%
    count(word, sort = TRUE) 

## customize stop words for words cloud
custom_stop_words <- bind_rows(tibble(word = c("food","restaurant"), 
                                      lexicon = c("custom")), 
                               stop_words)

#### Maping function
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
    icon = 'ios-restaurant',
    iconColor = 'black',
    library = 'ion',
    markerColor = getColor(df_map),
    spin=T
)



mapStates = map("state", fill = TRUE, plot = FALSE)

# Define UI for application that draws a histogram
ui <- dashboardPagePlus(
    skin = "green",  ## App skin color
    #######################-SideBar-##################
    dashboardHeaderPlus(title = h4(strong("Yelp Project Report"))),
    dashboardSidebar(
        sidebarMenu(
            menuItem("Home Page", tabName = "Home", icon = icon("dashboard")),
            menuItem("EDA", tabName = "EDA", icon = icon("th")),
            menuItem("MAPS", tabName = "MAP", icon = icon("map")),
            menuItem("Text Mining", tabName = "TM", icon = icon("book", lib = "glyphicon"),
                     menuSubItem("Frequencies Analysis", tabName = "FA"),
                     menuSubItem("Sentiment Analysis", tabName = "SA"))
        )#sidebarMenu_end
    ),#dashboardSidebar_end,
    dashboardBody(
        tabItems(
            ################-Home Page-###############
            tabItem(tabName = "Home", 
                    #Greetings
                    h2(strong("Yelp Project Report-Japanese Restaurant in NY, SF and Dallas")),
                    br(),
                    #Heading image
                    fluidRow(h3(""),h3(""),h3(""),column(12,mainPanel(imageOutput("janpanese_food"))),
                    h5("image from: https://allabout-japan.com/en/article/6856/")),
                    hr(),
                    h3(strong("Purpose:")),
                    wellPanel(
                    h4("This App will help you look into Japanese restaurant in NY, SF and also Dallas these three polular cities.
                        You can find distributions of restaurant ratings in different cities, and also their geographical location under Map section.
                       You can also find the frequency of popular words in the reviews of high-reviewing restaurants and words clouds at the sametime."))
            ),#hometabitem_end,
            tabItem(tabName = "EDA", 
                    
                                wellPanel(  
                                fluidRow(
                                   column(4,
                                   selectInput("plot_city_id", "Select City:",city_list,width = NULL)),
                                   column(4,
                                   selectInput("plot_price_id", "Select Price Level:",c("$"
                                                                                   ,"$$"
                                                                                   ,"$$$",
                                                                                   "$$$$"),width = NULL))
                                    )), # wellPanel end
                            fluidRow(
                            column(6,
                            wellPanel(align="center",
                            h4(strong("Summary of Janpanese Restaurant Data")),
                            DT::dataTableOutput('tb_summary'),
                            hr(),
                            br(),
                            h4(strong("Rating Distribution")),
                            plotlyOutput("rating_plot")
                            )),#column end
                            
                            column(6,
                            wellPanel(align="center",
                            h4(strong("Rating Distribution by City")),
                            plotlyOutput("rating_plot_city"),
                            br(),
                            h4(strong("Rating Distribution by Price")),
                            plotlyOutput("rating_plot_price")
                               )# wellPanel end
                        )#column end
                        
                    )#fluidRow end
            ),#EDAtabitem_end
            tabItem(tabName = "MAP",
                    fluidRow(
                        column(3,
                            wellPanel(  
                               selectInput("map_city_id", "Select City:",city_list,width = NULL),
                               selectInput("price_id", "Select Price Level:",c("$"
                                                                        ,"$$"
                                                                        ,"$$$",
                                                                        "$$$$"),width = NULL),
                               hr(),
                               h5("Blue--Rating <= 3.5"),
                               h5("Orange--Rating = 4"),
                               h5("Red--Rating >= 4.5")
                               
                            )
                            ), #column end
                    column(9,
                        wellPanel(   
                        h4(strong("Japanese Restaurant by City")),
                        leafletOutput("mymap_city"),
                        br(),
                        h4(strong("Japanese Restaurant by Price")),
                        leafletOutput("mymap_city_price")
                        )
                    )# column end
                    )#fluidRow end
        ), # tabItem end
        tabItem(tabName ="FA",
                wellPanel(align="center",
                h3(strong("Most frequent word for high rating restaurants' reveiws >=4.5")),
            fluidRow(
                column(4,
                       wellPanel(align="center",
                                 h4(strong("New York City")),
                           plotlyOutput("ny_most_freq")
                           
                       )),
                column(4,
                       wellPanel(align="center",
                                 h4(strong("Dallas")),
                                 plotlyOutput("dl_most_freq")
                       )),
                column(4,
                       wellPanel(align="center",
                                 h4(strong("San Francisco")),
                                 plotlyOutput("sf_most_freq")
                       ))
                ),### column 6 end
            fluidRow(
                column(12,
                    wellPanel(align="center",
                              h3(strong("Frequencies Similarity Comparation")),
                    plotOutput("freq_compare")
                    ) # wellPanel end
                ) # column end
                
                )
       )), # tabItem FA end
        
        tabItem(tabName="SA",
                wellPanel(align="center",
                h3(strong("Words Cloud for high rating restaurants' reveiws >=4.5 in 3 Cities")),
                fluidRow(
                    column(4,
                           wellPanel(align="center",
                                     h4(strong("New York City")),
                                     plotOutput("ny_cloud")
                                     
                           )),
                    column(4,
                           wellPanel(align="center",
                                     h4(strong("Dallas")),
                                     plotOutput("dl_cloud")
                           )),
                    column(4,
                           wellPanel(align="center",
                                     h4(strong("San Francisco")),
                                     plotOutput("sf_cloud")
                           ))
                ),
                h3(strong("Most Important Positive and Negative Words")),
                fluidRow(
                    column(4,
                           wellPanel(align="center",
                                     h4(strong("New York City")),
                                     plotOutput("ny_cloud_2")
                                     
                           )),
                    column(4,
                           wellPanel(align="center",
                                     h4(strong("Dallas")),
                                     plotOutput("dl_cloud_2")
                           )),
                    column(4,
                           wellPanel(align="center",
                                     h4(strong("San Francisco")),
                                     plotOutput("sf_cloud_2")
                           ))
                )
                
                
               ) )# tabItem SA end
        
) # tabItems end
) # dashboardBody end
) # dashboardPage end

# Define server logic required to draw a histogram
server <- function(input, output) {
    output$mymap_city <- renderLeaflet({
        
    df_city <- df_map[df_map$city==input$map_city_id,]
    
    leaflet() %>%
    addProviderTiles(providers$CartoDB.Positron) %>%
    setView(lng = mean(df_city$longitude),lat = mean(df_city$latitude),zoom = 10) %>%
    addPolygons(data=mapStates, fillColor = topo.colors(10, alpha = NULL), stroke = FALSE)%>% 
    addAwesomeMarkers(data = df_city, ~longitude, ~latitude, icon=icons,
                              label=~as.character(price),clusterOptions = T) 
    
        
        
    })
    
output$mymap_city_price <- renderLeaflet({
    
    df_city <- df_map[df_map$city==input$map_city_id,]
    df_city_price <- df_city[df_city$price==input$price_id,]
    leaflet() %>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        setView(lng = mean(df_city$longitude),lat = mean(df_city$latitude),zoom = 10) %>%
        addPolygons(data=mapStates, fillColor = topo.colors(10, alpha = NULL), stroke = FALSE)%>% 
        addAwesomeMarkers(data = df_city_price, ~longitude, ~latitude, icon=icons,
                          label=~as.character(price),clusterOptions = T) 
})


output$ tb_summary <- DT::renderDataTable({
    
    
    dataGroupByCityStar <- business_all %>% 
        dplyr::mutate(tsum = n()) %>% 
        dplyr::group_by(city, rating)
     

    dataForTableByCityStar <- dataGroupByCityStar %>% dplyr::group_by(city) %>%
        dplyr::summarise(total_business = n(), 
                         total_reviews = sum(review_count),
                         avg_rating = round(mean(rating),2)) %>%
        dplyr:: arrange(desc(avg_rating))
    
    
    DT::datatable(dataForTableByCityStar)



})
    
output$rating_plot <- renderPlotly({

    
   plot1 <-  ggplot(df_map,aes(x=rating)) + geom_bar(aes(color=city,fill=city)) +labs(y = "Counts",x="Rating")
    
   ggplotly(plot1)
    
}) # rating_plot end


output$rating_plot_city <- renderPlotly({
    
    df_city <- df_map[df_map$city==input$plot_city_id,]
    
    plot2 <-  ggplot(df_city,aes(x=rating)) + geom_bar(aes(fill=city),binwidth = 0.25,color = "white") +
        labs(y = "Counts",x="Rating")
    
    ggplotly(plot2)
    
}) # rating_plot end


output$rating_plot_price <- renderPlotly({
    
    df_city <- df_map[df_map$city==input$plot_city_id,]
    df_city_price <- df_city[df_city$price==input$plot_price_id,]
    plot2 <-  ggplot(df_city_price,aes(x=rating)) + geom_bar(aes(fill=city),binwidth = 0.25,color = "white") +
        labs(y = "Counts",x="Rating",
             title = paste0("Price Levle",": ",input$plot_price_id))
    
    ggplotly(plot2)
    
}) # rating_plot end




### test mining server
output$ny_most_freq <- renderPlotly({
 ny_1 <- tidy_ny %>%
        count(word, sort = TRUE) %>%
        filter(n > 50) %>%
        mutate(word = reorder(word, n)) %>%
        ggplot(aes(word, n)) +
        geom_col() +
        xlab(NULL) +
        coord_flip()
   
    ggplotly(ny_1)
})

output$dl_most_freq <-  renderPlotly({
    
sf_1 <-  tidy_sf %>%
        count(word, sort = TRUE) %>%
        filter(n > 20) %>%
        mutate(word = reorder(word, n)) %>%
        ggplot(aes(word, n)) +
        geom_col() +
        xlab(NULL) +
        coord_flip()
 ggplotly(sf_1)
 
})


output$sf_most_freq <-  renderPlotly({
dl_1 <-  tidy_dl %>%
        count(word, sort = TRUE) %>%
        filter(n > 20) %>%
        mutate(word = reorder(word, n)) %>%
        ggplot(aes(word, n)) +
        geom_col() +
        xlab(NULL) +
        coord_flip()
    
ggplotly(dl_1)
})

output$freq_compare <-  renderPlot({
    
    frequency <- bind_rows(mutate(tidy_ny, city = "NYC"),
                           mutate(tidy_sf, city = "SF"),
                           mutate(tidy_dl, city = "Dallas")) %>% 
        mutate(word = str_extract(word, "[a-z']+")) %>%
        count(city, word) %>%
        group_by(city) %>%
        mutate(proportion = n / sum(n)) %>% 
        select(-n) %>% 
        spread(city, proportion) %>% 
        gather(city, proportion, `NYC`:`SF`)
    
plot_comp <- ggplot(frequency, aes(x = proportion, y = `Dallas`, color = abs(`Dallas` - proportion))) +
        geom_abline(color = "gray40", lty = 2) +
        geom_jitter(alpha = 0.1, size = 2.5, width = 0.3, height = 0.3) +
        geom_text(aes(label = word), check_overlap = TRUE, vjust = 1.5) +
        scale_x_log10(labels = percent_format()) +
        scale_y_log10(labels = percent_format()) +
        scale_color_gradient(limits = c(0, 0.001), low = "darkslategray4", high = "gray75") +
        facet_wrap(~city, ncol = 2) +
        theme(legend.position="none") +
        labs(y = "Dallas", x = NULL)
   
 plot_comp 

})


output$ny_cloud <-  renderPlot({
tidy_ny %>%
        anti_join(custom_stop_words) %>%
        count(word) %>%
        with(wordcloud(word, n, max.words = 50))

})

output$dl_cloud<-  renderPlot({

tidy_dl %>%
        anti_join(custom_stop_words) %>%
        count(word) %>%
        with(wordcloud(word, n, max.words = 50))

})

output$sf_cloud<-  renderPlot({
tidy_sf %>%
        anti_join(custom_stop_words) %>%
        count(word) %>%
        with(wordcloud(word, n, max.words = 50))

})

output$ny_cloud_2<-  renderPlot({
tidy_ny %>%
        inner_join(get_sentiments("bing")) %>%
        count(word, sentiment, sort = TRUE) %>%
        acast(word ~ sentiment, value.var = "n", fill = 0) %>%
        comparison.cloud(colors = c("gray20", "gray80"),
                         max.words = 20)
})

output$dl_cloud_2<-  renderPlot({
tidy_dl %>%
        inner_join(get_sentiments("bing")) %>%
        count(word, sentiment, sort = TRUE) %>%
        acast(word ~ sentiment, value.var = "n", fill = 0) %>%
        comparison.cloud(colors = c("gray20", "gray80"),
                         max.words = 20)
})

output$sf_cloud_2<-  renderPlot({
tidy_sf %>%
        inner_join(get_sentiments("bing")) %>%
        count(word, sentiment, sort = TRUE) %>%
        acast(word ~ sentiment, value.var = "n", fill = 0) %>%
        comparison.cloud(colors = c("gray20", "gray80"),
                         max.words = 20)
   
})

output$janpanese_food<- renderImage({Leg<-"jap.jpg"
list(src=Leg)
},deleteFile = FALSE)


} #server_end


# Run the application 
shinyApp(ui = ui, server = server)


