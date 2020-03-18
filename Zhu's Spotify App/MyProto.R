################### Packages
library(spotifyr)
library(jsonlite)
library(httr)
library(tidyverse)
library(lubridate)
library(plotly)
################## Get access token and authorization code
token <- get_spotify_access_token()
authorizarion.header <- paste0("Bearer ", token)

get_spotify_authorization_code()

################### Get saved tracks and top artists
saved_tracks <- get_my_saved_tracks(limit = 50)
top_artists <- get_my_top_artists_or_tracks(type = "artists", limit = 10)
################### Get tracks features and other info
features <- get_track_audio_features(ids = saved_tracks$track.id)
tracks_info <- get_tracks(ids = saved_tracks$track.id)
################### Text Output stats
### Earliest song
earliest_song_date <- min(tracks_info$album.release_date)
earliest_song <- tracks_info$name[tracks_info$album.release_date == earliest_song_date]
### most popular song
popular_song <- tracks_info$name[tracks_info$popularity == max(tracks_info$popularity)]
niche_song <- tracks_info$name[tracks_info$popularity == min(tracks_info$popularity)]
### first song
first_song <- saved_tracks$track.name[dim(saved_tracks)[1]]
first_song_artist <- saved_tracks$track.artists[[dim(saved_tracks)[1]]]$name
################### Graph Outputs
### artists pie


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


get_my_saved
