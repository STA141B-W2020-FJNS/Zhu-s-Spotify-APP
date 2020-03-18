#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(spotifyr)
library(jsonlite)
library(httr)
library(tidyverse)
library(lubridate)
library(shiny)
library(shinydashboard)

# Define UI for application that draws a histogram
ui <- dashboardPage(
  dashboardHeader(title = "Spotify Analysis"),
  dashboardSidebar(),
  dashboardBody(
    
    fluidRow(
      box(
        title = "Your First Song",
        solidHeader = TRUE,
        status = "primary",
        textOutput("first_song")
      ),
      
      box(
        title = "The Earliest Song",
        solidHeader = TRUE,
        status = "primary",
        textOutput("earliest_song")
      ),
      
      box(
        title = "Popular vs. Niche",
        solidHeader = TRUE,
        status = "primary",
        textOutput("popular_niche")
      )
    ),
    
    
    
    fluidRow(
      box(
        title = "Who's Your Favorite Artist",
        plotOutput("artists_pie", height = 500)
      ),
      
      box(
        title = "Emotional Analysis",
        plotOutput("emotion_plot", height = 500)
      )
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$first_song <- renderText({
    
    # get authorization code
    get_spotify_authorization_code()
    # get user's saved track
    saved_tracks <- get_my_saved_tracks()
    
    # Find user's first saved song and its performer
    first_song <- saved_tracks$track.name[dim(saved_tracks)[1]]
    first_song_artist <- saved_tracks$track.artists[[dim(saved_tracks)[1]]]$name
    
    # print output
    paste("The song you saved in Spotify is", first_song, "performed by", first_song_artist, ".")
  })
  
  
  output$earliest_song <- renderText({

    # get authorization code
    get_spotify_authorization_code()
    # get user's saved track
    saved_tracks <- get_my_saved_tracks()  
    # get saved tracks' info
    tracks_info <- get_tracks(ids = saved_tracks$track.id) 
    
    # find the earliest song and its release date
    earliest_song_date <- min(tracks_info$album.release_date)
    earliest_song <- tracks_info$name[tracks_info$album.release_date == earliest_song_date]
    
    # print output
    paste("The earliest song you have saved is", earliest_song, ", it is released at", earliest_song_date, ".")
  })
  
  output$popular_niche <- renderText({
    
    # get authorization code
    get_spotify_authorization_code()
    # get user's saved track
    saved_tracks <- get_my_saved_tracks()  
    # get saved tracks' info
    tracks_info <- get_tracks(ids = saved_tracks$track.id)
    
    # find popular and niche song
    popular_song <- tracks_info$name[tracks_info$popularity == max(tracks_info$popularity)]
    niche_song <- tracks_info$name[tracks_info$popularity == min(tracks_info$popularity)]
    
    # print the output
    paste("The most popular song in your list is", popular_song, ".",
          "Meanwhile, you also have a niche taste.", niche_song, "in your list doesn't seem to be appreciated by the most of people.")
  })
  
  output$emotion_plot <- renderPlot({
    # get authorization code
    get_spotify_authorization_code()
    # get user's saved track
    saved_tracks <- get_my_saved_tracks(limit = 50)  
    # get saved tracks' features
    features <- get_track_audio_features(ids = saved_tracks$track.id)
    
    ### Emotion Analysis
    ggplot(features, aes(x = valence, y = energy)) +
      geom_point() +
      geom_hline(aes(yintercept = 0.5)) +
      geom_vline(aes(xintercept = 0.5)) +
      xlim(0,1) +
      ylim(0,1) +
      labs(title = 'Emotions of Your Saved Songs', x = 'Valence', y = 'Energy') +
      annotate("text", x = 0.1, y = 0, label = "Sad/Depressing") +
      annotate("text", x = 0.1, y = 1, label = "Turbulent/Angry") +
      annotate("text", x = 0.9, y = 0, label = "Chill/Peaceful") +
      annotate("text", x = 0.9, y = 1, label = "Happy/Joyful")
    
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

