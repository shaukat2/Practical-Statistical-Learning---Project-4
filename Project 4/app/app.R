## ui.R
# 
# Reference: https://github.com/pspachtholz/BookRecommender
# 
# =============================================
library(shiny)
library(shinydashboard)
library(recommenderlab)
library(data.table)
library(ShinyRatingInput)
library(shinyjs)
library(readr)

set.seed(8831)
source('functions/helpers.R')
source('functions/setting.R')


isdebug = FALSE

version = "1.0.0.0"
settingFile = "setting.txt"
setting = readSetting(settingFile)
systemI_AlgorithmKey = "SystemI_Algorithm"
systemII_AlgorithmKey = "SystemII_Algorithm"
modelpath = paste0("model/", getSetting(setting, systemII_AlgorithmKey)  ,"_model.rds")
defaultmodelpath = "model/UBCF_Z_C.rds"
databasepath ="data/"
moviesListFileName = "aggr.dat"
numberofmovierecommend = 4 * 6





moviesList1 <- read(paste0(databasepath,"aggr200.dat"), "::")
moviesList <- read_csv(paste0(databasepath,"HighRatingMovies.csv"))
movies = read(paste0(databasepath,"movies.dat"), "::")
ratingsdata = read(paste0(databasepath,"ratings.dat"), "::")
users = read(paste0(databasepath,"users.dat"), "::")
movies_clean <- read(paste0(databasepath,"movies_clean.dat"), "::")
#colnames(moviesList) = c( 'MovieID', 'AveRating', 'title', 'genres')
colnames(moviesList1) = c( 'MovieID', 'AveRating', 'title', 'genres')
colnames(moviesList) = c("MovieID", "title", "AveRating" , "genres")
colnames(movies) = c('MovieID', 'title', 'genres')
colnames(ratingsdata) = c('UserID', 'MovieID', 'Rating', 'Timestamp')
colnames(users) = c('UserID', 'Gender', 'Age', 'Occupation', 'Zip-code')
colnames(movies_clean) = c("MovieID", "title", "year", 'genres')

moviesList1$MovieID <- as.numeric(moviesList1$MovieID)

genre_list = c("Action", "Adventure", "Animation", "Children", 
                "Comedy", "Crime","Documentary", "Drama", "Fantasy",
                "Film-Noir", "Horror", "Musical", "Mystery","Romance",
                "Sci-Fi", "Thriller", "War", "Western")

systemI_algorithm_list = c("Method1") 
                          
systemII_algorithm_list = c("UBCF_Z_C")


systemI_algorithm_Description_list = c("Method1: randomly pick from rating >= 4(optional) and  genre1 or genre2 or genre3")

systemII_algorithm_Description_list = c("UBCF_Z_C: User-Based CF, normalize = Z-score',method='Cosine'")



loadModel = function(){
   model =  readModel(modelpath)
   if (is.na(model)){
      model = readModel(defaultmodelpath)
   }
   return(model)
}
model = loadModel()


getSystemAlgorithm = function(description){
  result = strsplit(description, ":")[[1]]
  return(result[1])
}

getSystemAlgorithmDesc = function(code){
  index = which(grepl(code, systemII_algorithm_Description_list, fixed = TRUE))[1]
  if(!is.na(index)){
      return(systemII_algorithm_Description_list[index])
  }
  return(systemI_algorithm_Description_list[which(grepl(code, systemI_algorithm_Description_list, fixed = TRUE))])
}



ui <- dashboardPage(

  dashboardHeader(title="Top Movies"),
  dashboardSidebar(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "css/movies.css")
    ),
    uiOutput("userpanel"),
    sidebarMenu(id = "tabs",
      menuItem("System I (HighRated - Genre)", tabName="first", icon=icon("calendar")),
      menuItem("System II (UBCF)", tabName = "second", icon=icon("th"))
    )
  ),
  
  dashboardBody(
                tabItems(
                       tabItem(tabName = "first",
                               fluidRow(
                                 box(width = 12,title = "Step 1: Select movie genres", status = "info", solidHeader = TRUE, collapsible = TRUE,
                                     div(class = "genres",
                                         selectInput("input_genre1", "Genre #1",genre_list)
                                     )
                                 )
                               ),
                               fluidRow(
                                 useShinyjs(),
                                 box(
                                   width = 12, status = "info", solidHeader = TRUE,
                                   title = "Step 2: Discover movies you might like",
                                   br(),
                                   withBusyIndicatorUI(
                                     actionButton("btn_genre", "Click here to get your recommendations", class = "btn-warning")
                                   ),
                                   br(),
                                   tableOutput("results_genre")
                                 )
                               )
                       ),
                       tabItem(tabName = "second",
                              fluidRow(
                                box(width = 12, title = "Step 1: Rate as many movies as possible", status = "info", solidHeader = TRUE, collapsible = TRUE,
                                   div(class = "rateitems",
                                       uiOutput('ratings')
                                   )
                                )
                              ),
                              fluidRow(
                                useShinyjs(),
                                box(
                                  width = 12, status = "info", solidHeader = TRUE,
                                  title = "Step 2: Discover movies you might like",
                                  br(),
                                  withBusyIndicatorUI(
                                    actionButton("btn", "Click here to get your recommendations", class = "btn-warning")
                                  ),
                                  br(),
                                  tableOutput("results")
                                )
                             )
                      )
                )
   )
)






server <- function(input, output){
 #   load_data()
  
    output$userpanel <- renderUI({
        # session$user is non-NULL only in authenticated sessions
        sidebarUserPanel(
           span("NetID: shaukat2")
           )
    })
  
    # show the movies to be rated
    output$ratings <- renderUI({
      num_movies_disaply = 6
      num_rows_display = 5
      
      logfile = paste0("app", toString(as.integer(Sys.time()))  ,".log")

      # Randamly picked movie to display
      moviesDisplay <- moviesList1[sample(nrow(moviesList1), 200),]

      lapply(1:num_rows_display, function(i) {
        list(fluidRow(lapply(1:num_movies_disaply, function(j) {
          list(box(width = 2,
                   div(style = "text-align:center", img(src = paste0( "movieImages/", moviesDisplay$MovieID[(i - 1) * num_movies_disaply + j], ".jpg"), onerror="this.onerror=null;this.src='movieImages/existent-image.jpg';", height="60%", width="60%")),
                   div(style = "text-align:center", paste0( moviesDisplay$title[(i - 1) * num_movies_disaply + j]) ),
                   div(style = "text-align:center; font-size: 150%; color: #f0ad4e;", ratingInput(paste0("select_", moviesDisplay$MovieID[(i - 1) * num_movies_disaply + j]), label = "", dataStop = 5))))
        })))
      })
    })
    
    # Calculate recommendations for System I when the submit button is clicked 
    df_system1 <- eventReactive(input$btn_genre, {
      withBusyIndicatorServer("btn_genre", { # showing the busy indicator
        num_rows = 4
        num_movies = 6

        systemI_Algorithm = getSystemAlgorithm(systemI_algorithm_list)

          systemresult = subset(moviesList,AveRating>=4 & (grepl(input$input_genre1, genres, fixed = TRUE)) )
        systemresult = systemresult[sample(nrow(systemresult), ifelse(nrow(systemresult)>=numberofmovierecommend,numberofmovierecommend,nrow(systemresult))),]
        outputToFile(systemresult, paste0("app1", toString(as.integer(Sys.time()))  ,".log"), isdebug)
        systemresult
       })
    })
    
    
    
    # display the recommendations System I
    output$results_genre <- renderUI({
      showNotification(paste0("System Message: System I Algorithm - ", getSystemAlgorithmDesc(getSetting(setting, systemI_AlgorithmKey))), duration = 5, type = "message" )
      
      recom_result1 <- df_system1()
      if (nrow(recom_result1) > 0){
          systemI_num_movies = 6
          systemI_num_movies = ifelse(nrow(recom_result1) >= systemI_num_movies, systemI_num_movies, nrow(recom_result1)) 
          systemI_num_rows = nrow(recom_result1) %/% systemI_num_movies

          lapply(1:systemI_num_rows, function(i) {
            list(fluidRow(lapply(1:systemI_num_movies, function(j) {
                box(width = 2, status = "success", solidHeader = TRUE, title = paste0("Rank ", (i - 1) * systemI_num_movies + j),
                div(style = "text-align:center", img(src = paste0( "movieImages/", recom_result1$MovieID[(i - 1) * systemI_num_movies + j], ".jpg"), onerror="this.onerror=null;this.src='movieImages/existent-image.jpg';", height="60%", width="60%")),
                div(style = "text-align:center; color: #999999; font-size: 80%", 
                    paste0(recom_result1$title[(i - 1) * systemI_num_movies + j])
                ),
                div(style="text-align:center; font-size: 100%", 
                    strong(paste0(recom_result1$title[(i - 1) * systemI_num_movies + j]))
                )
            )        
          }))) # columns
         }) # rows
      }else{
        showNotification("System Message: there is no movie found", duration = 4, type = "error")
      }

    }) # renderUI function
    
    
    
    
    # Calculate recommendations II when the submit button is clicked
    df_system2 <- eventReactive(input$btn, {
      withBusyIndicatorServer("btn", { # showing the busy indicator
        # hide the rating container
        useShinyjs()
        jsCode <- "document.querySelector('[data-widget=collapse]').click();"
        runjs(jsCode)
        
        # get the user's rating data
        value_list <- reactiveValuesToList(input)
        user_ratings <- get_user_ratings(value_list, ratingsdata) 
        pred <- predict(model, newdata = user_ratings, n = numberofmovierecommend)

        recom_resultID = as(pred, 'list')[[1]]
        recom_results <- subset(movies, movies$MovieID %in% recom_resultID)
        
        outputToFile(recom_resultID, paste0("recom_resultID", toString(as.integer(Sys.time()))  ,".log"), isdebug)
        outputToFile(recom_results, paste0("app", toString(as.integer(Sys.time()))  ,".log"), isdebug)

        recom_results

      })
      
    }) # clicked on button
    
    
    # display the recommendations System II
    output$results <- renderUI({
      showNotification(paste0("System Message: System II Algorithm - ",  getSystemAlgorithmDesc(getSetting(setting, systemII_AlgorithmKey)) ), duration = 5, type = "message" )
      
      num_rows = 4
      num_movies = 6

      recom_result <- df_system2()
      if (nrow(recom_result) > 0){
         systemII_num_movies = ifelse(nrow(recom_result) >= num_movies, num_movies, nrow(recom_result)) 
         systemII_num_rows = nrow(recom_result) %/% systemII_num_movies
         
         lapply(1:systemII_num_rows, function(i) {
            list(fluidRow(lapply(1:systemII_num_movies, function(j) {
             box(width = 2, status = "success", solidHeader = TRUE, title = paste0("Rank ", (i - 1) * num_movies + j),
              
                div(style = "text-align:center", img(src = paste0( "movieImages/", recom_result$MovieID[(i - 1) * num_movies + j], ".jpg"), onerror="this.onerror=null;this.src='movieImages/existent-image.jpg';", height="60%", width="60%")),
                div(style = "text-align:center; color: #999999; font-size: 80%", 
                    paste0( recom_result$title[(i - 1) * num_movies + j])
                ),
                div(style="text-align:center; font-size: 100%", strong(paste0( recom_result$title[(i - 1) * num_movies + j])))
              
          )        
          })))
        })
      }else{
        showNotification("System Message: there is no movie found", duration = 4, type = "error")
      }
      
    })


    observeEvent(input$btn_setting, {
      # Save the ID for removal later

      systemI_Algorithm = getSystemAlgorithm(input$input_SystemI_Algorithm)
      systemII_Algorithm = getSystemAlgorithm(input$input_systemII_Algorithm)
      setting <<- setSetting(setting, systemI_AlgorithmKey, systemI_Algorithm)
      setting <<- setSetting(setting, systemII_AlgorithmKey, systemII_Algorithm)
      write(setting, settingFile, ":")
      modelpath <<- paste0("model/", getSetting(setting, systemII_AlgorithmKey)  ,"_model.rds")
      model <<- loadModel()
      
      showNotification("System Message: Setting Saved", duration = 5, type = "message" )
      #showNotification(paste0("System Message: Algorithm - ",  getSetting(setting, systemII_AlgorithmKey) )  , duration = 5, type = "message" )
    })
    


}


shinyApp(ui = ui, server = server)
#runApp('app', host = "0.0.0.0", port = 80)