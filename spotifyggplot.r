library(spotifyr)
library(httr)
library(knitr)
library(tidyverse)
library(lubridate)
library(ggplot2)

#rewrite function to fix this nightmare of a package, DO NOT TOUCH!!!
get_spotify_authorization_code <- function (client_id = Sys.getenv("8a7badd4858e43b0a6574ddb9910dd44"), client_secret = Sys.getenv("82f57ad4fdf4413a8980c6c9c650953c"), 
    scope = scopes()) 
{
    endpoint <- oauth_endpoint(authorize = "https://accounts.spotify.com/authorize", 
        access = "https://accounts.spotify.com/api/token")
    app <- oauth_app("spotifyr", client_id, client_secret, redirect_uri = "http://127.0.0.1:1410/")
    token <- (purrr::safely(.f = oauth2.0_token))(endpoint = endpoint, 
        app = app, scope = scope)
    if (!is.null(token$error)) {
        token$error
    }
    else {
        token$result
    }
}
id <- '8a7badd4858e43b0a6574ddb9910dd44'
secret <- '82f57ad4fdf4413a8980c6c9c650953c'
Sys.setenv(SPOTIFY_CLIENT_ID = id)
Sys.setenv(SPOTIFY_CLIENT_SECRET = secret)
access_token <- get_spotify_access_token()

#artist ids for reference
talking_heads <- '64mPnRMMeudAet0E62ypkx'
fiona_apple <- '3g2kUQ6tHLLbmkV7T4GPtL'
sade <- '47zz7sob9NUcODy0BTDvKx'

#insert artist id
name_input <- readline(prompt = "Enter Artist Name:")

#search spotify
searched_id <- (search_spotify(
  name_input,
  type = c("artist"),
  market = NULL,
  limit = 20,
  offset = 0,
  include_external = NULL,
  authorization = get_spotify_access_token(),
  include_meta_info = FALSE
)$id[1])

#compile albums together
all_albums <- (get_artist_albums(
  searched_id,
  include_groups = c("album"),
  market = 'US',
  limit = 20,
  offset = 0,
  authorization = get_spotify_access_token(),
  include_meta_info = FALSE
))

big_frame <- data.frame()

album_names <- c()

for (i in all_albums$id)
    {

        album <- get_album_tracks(
    i,
    limit = 20,
    offset = 0,
    market = NULL,
    authorization = get_spotify_access_token(),
    include_meta_info = FALSE

    )

    big_frame <- rbind(big_frame, album)
    }

for (i in all_albums$total_tracks)
    {
        big_list <- rep(all_albums$name,times = all_albums$total_tracks)
    }
big_frame$album_name <- big_list

ggplot(big_frame,aes(x = seq_along(id), y = duration_ms/1000/60, colour = album_name))+ geom_point(size = 10)

print(str_to_title(paste("You entered:", name_input)))
