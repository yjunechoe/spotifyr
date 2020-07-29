library(tidyverse)
library(ggrepel)
library(ggtext)
library(lemon)
library(mdthemes)
library(shiny)
library(glue)

Sys.setenv(SPOTIFY_CLIENT_ID = 'XXXXXXXXXXXXXXX')
Sys.setenv(SPOTIFY_CLIENT_SECRET = 'XXXXXXXXXXXXXXX')
access_token <- get_spotify_access_token()

extrafont::loadfonts(device = "win")

theme_set(md_theme_classic())
theme_update(
  plot.title = element_markdown(hjust = 0.5),
  text = element_text(family = "Verdana")
)

# fetch playlist info
playlists_raw <- bind_rows(
  get_my_playlists(limit = 50),
  get_my_playlists(limit = 50, offset = 50),
  get_my_playlists(limit = 16, offset = 100)
)

# clean playlist data
playlists_cleaned <- playlists_raw %>% 
  as_tibble() %>% 
  select(name, tracks.total, id) %>% 
  filter(str_detect(name, paste0("(", paste(month.name, collapse = "|"), ")")) |
           name == "Summer 2017") %>% 
  mutate(name = ifelse(name == "Summer 2017", "June 2017", name)) %>%
  separate(name, c("Month", "Year"), " ") %>% 
  mutate(Month = str_sub(Month, 1, 3)) %>% 
  mutate(Month = factor(Month, str_sub(month.name, 1, 3))) %>% 
  rename(Tracks = tracks.total)


# Imputation
monthly_playlists <- crossing(Year = playlists_cleaned$Year,
                      Month = playlists_cleaned$Month) %>% 
  filter(!(Year == 2014 & as.numeric(Month) < 9) &
           !(Year == 2020 & as.numeric(Month) > 7)) %>% 
  full_join(playlists_cleaned, by = c("Year", "Month")) %>% 
  # carry forward last observation
  mutate(skipped = is.na(id)) %>% 
  mutate_if(~any(is.na(.x)), ~zoo::na.locf(.x))


# Tracks through monthly playlists
landmarks <- tribble(~Year, ~Month, ~Tracks, ~Text, ~nudge_x,
                     2017, 5, 1, '"American Girl" by Mitsky', -.5,
                     2017, 11, 20, 'EDM phase', -1.5)

monthly_plot <- monthly_playlists %>% 
  ggplot(aes(Month, Tracks)) +
  geom_col(aes(fill = skipped)) +
  geom_text_repel(data = landmarks,
                  aes(label = Text),
                  point.padding = 1, nudge_x = landmarks$nudge_x, nudge_y = 35) +
  facet_wrap(~Year, ncol = 2) +
  scale_fill_manual(values = c('grey50', 'red')) +
  labs(title = "**Number of songs in monthly playlists**",
       subtitle = glue("Playlists are carried forward in {span('**missing months**', style='color:red')}")) +
  theme(panel.grid.major.y = element_line('white', size = 0.5),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.ontop = TRUE,
        legend.position = 0)


# Playlist features
playlist_features_raw <- monthly_playlists %>% 
  mutate(features = map(id, ~{
    get_playlist_audio_features(playlist_uris = .x) %>% 
      select(Song = track.name, SongID = track.id, danceability:tempo)
  }))

playlist_features <- playlist_features_raw %>% 
  rowwise(Year, Month) %>% 
  mutate(summarize_if(features, is.numeric, ~mean(.x, na.rm = TRUE))) %>% 
  select(-key, -mode, -speechiness, -instrumentalness, -loudness, -liveness) %>% 
  pivot_longer(danceability:valence, "feature", "mean") %>% 
  mutate(Time = paste(Month, Year),
         Time = fct_inorder(Time),
         feature = str_to_title(feature))

# by month
monthly_ft_plot <- playlist_features %>% 
  ggplot(aes(feature, value)) +
  geom_line(aes(group = Time, color = Year),
            size = .8, alpha = .3) +
  geom_label(aes(x = 2.5, y = 0.31), inherit.aes = FALSE,
             label = "May 2017", color = "#21908CFF") + 
  scale_colour_viridis_d() +
  labs(x = "", y = "") +
  guides(color = guide_legend(override.aes = list(alpha = 1))) +
  theme(legend.position = c(.07, .8),
        text = element_text(size = 16))

# by year
yearly_ft_plot <- playlist_features %>% 
  group_by(Year, feature) %>% 
  summarize(value = mean(value)) %>% 
  ggplot(aes(feature, value)) +
  geom_line(aes(group = Year, color = Year),
            size = 1, alpha = .7) +
  scale_colour_viridis_d() +
  guides(color = guide_legend(override.aes = list(alpha = 1))) +
  labs(x = "", y = "") +
  theme(legend.position = c(.07, .8),
        text = element_text(size = 16))

# by Quarter
quarterly_ft <- playlist_features %>% 
  filter(Year > 2016 |
           (Year == 2016 & as.numeric(Month) > 8)) %>% 
  mutate(Quarter = case_when(Month %in% c("Sep", "Oct", "Nov") ~ "Fall",
                             Month %in% c("Jan", "Feb", "Mar") ~ "Winter",
                             Month %in% c("Apr", "May", "Jun") ~ "Spring",
                             TRUE ~ "Break")) %>% 
  mutate(Quarter = factor(Quarter, levels = c("Fall", "Winter", "Spring", "Break"))) %>% 
  mutate(SchoolYear = rep(c("Freshman", "Sophomore", "Junior", "Senior"), each = 48)[1:n()],
         SchoolYear = fct_inorder(SchoolYear))

quarterly_ft_plot <- quarterly_ft %>% 
  group_by(SchoolYear, Quarter, feature) %>% 
  summarize(value = mean(value))
  ggplot(aes(feature, value)) +
  geom_line(aes(group = Quarter, color = Quarter)) +
  facet_rep_wrap(~SchoolYear) +
  coord_capped_cart(bottom = 'both', left = 'both') +
  labs(x = "", y = "") +
  theme(legend.position = c(.06, .88),
        text = element_text(size = 12))



# Yearly

top_songs <- playlists_raw %>% 
  as_tibble() %>% 
  filter(str_detect(name, "^Your Top")) %>% 
  select(name, id) %>% 
  mutate(Year = str_match(name, "\\d{4}$")[,1]) %>% 
  mutate(features = map(id, ~{
    get_playlist_audio_features(playlist_uris = .x) %>% 
      select(Song = track.name, SongID = track.id, danceability:tempo)
  })) %>% 
  unnest(features) %>% 
  select(-key, -mode, -speechiness, -instrumentalness, -loudness, -liveness) %>% 
  pivot_longer(danceability:valence, "feature", "mean") %>% 
  mutate(feature = str_to_title(feature)) %>% 
  group_by(Year) %>% 
  mutate(top_10 = row_number() <= 10 * 4)

top_songs_plot <- top_songs %>% 
  ggplot(aes(feature, value)) +
  geom_line(aes(group = SongID, alpha = top_10),
            color = "grey40") +
  geom_line(data = top_songs %>% 
              group_by(Year, feature) %>% 
              summarize(value = mean(value)),
            aes(group = Year),
            size = 1.5, color = "red") +
  facet_rep_wrap(~Year) +
  scale_alpha_manual(values = c(0.1, 1)) +
  coord_capped_cart(bottom = 'both', left = 'both') +
  labs(title = "Profile of My Top 100 Songs-of-the-Year Playlists",
       subtitle = glue("{span('**Yearly Means**', style='color:red')} and **Top 10 Songs**"),
       x = "", y = "", color = "") +
  theme(legend.position = 0,
        text = element_text(size = 12))

top_songs_nested <- top_songs %>% 
  group_by(Year, Song) %>% 
  nest()



# Tempo
quarterly_ft %>% 
  select(SchoolYear, Quarter, tempo) %>% 
  group_by(SchoolYear, Quarter) %>% 
  distinct() %>% 
  summarize(tempo_mean = mean(tempo), tempo_sd = sd(tempo)) %>% 
  ggplot(aes(Quarter, tempo_mean)) +
  geom_line(aes(group = SchoolYear, color = SchoolYear)) +
  scale_color_viridis_d()
