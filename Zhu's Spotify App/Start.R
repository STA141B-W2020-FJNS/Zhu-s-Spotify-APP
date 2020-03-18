########### Import packages
library(spotifyr)
library(jsonlite)
library(httr)
library(tidyverse)
########### Get token using spotifyr
Sys.setenv(SPOTIFY_CLIENT_ID = 'd226eb27d90f42adb636abd655693339')
Sys.setenv(SPOTIFY_CLIENT_SECRET = 'e305478d083e49eda7da8eccdae456b9')
token <- get_spotify_access_token()

### Get my playlist
get_my_playlists("6cilansnnu32gg3gahuf0ylgc")

########### APP Settings & Authorization Guides #######################

### Using POST to get token
response <- POST("https://accounts.spotify.com/api/token",
                 accept_json(),
                 authenticate(Sys.getenv('SPOTIFY_CLIENT_ID'), Sys.getenv('SPOTIFY_CLIENT_SECRET')),
                 body = list(grant_type = 'client_credentials'),
                 encode = 'form',
                 verbose()
                 )

token <- content(response)$access_token
authorization.header = paste0("Bearer ", token)


test <- GET("https://api.spotify.com/v1/albums/5QidVMllpZso7AHkI8LBsJ",
            config = add_headers(authorization = authorization.header))

test <- fromJSON(content(test, as = "text"))
test$artists


### Authorization code flow ####### still in progress ########
GET("https://accounts.spotify.com/authorize",
                      query = list(
                        client_id = Sys.getenv("SPOTIFY_CLIENT_ID"),
                        response_type = "code",
                        redirect_uri = "http://localhost:1410/",
                        scope = "user-read-private"
                      ))

### Using spotifyr
scope1 <- c("user-read-private", "user-library-read")
get_spotify_authorization_code <- function (client_id = Sys.getenv("SPOTIFY_CLIENT_ID"), 
                                              client_secret = Sys.getenv("SPOTIFY_CLIENT_SECRET"), 
                                              scope = scope1) 
{
  endpoint <- oauth_endpoint(authorize = "https://accounts.spotify.com/authorize", 
                             access = "https://accounts.spotify.com/api/token")
  app <- oauth_app("spotifyr", client_id, client_secret)
  oauth2.0_token(endpoint = endpoint, app = app, scope = scope)
}

get_my_playlists()
get_spotify_authorization_code()
