library(tidyverse)
library(rvest)
library(glue)
library(janitor)
library(ggpmisc)

# function for getting relevant forward individual stats
get_f_stats <- function(season){
  
  # season format: 20192020, 20182019, etc
  # stats for forwards who played at least 700 mins at 5v5

  url <- "https://www.naturalstattrick.com/playerteams.php?fromseason={season}&thruseason={season}&stype=2&sit=5v5&score=all&stdoi=std&rate=y&team=ALL&pos=F&loc=B&toi=700&gpfilt=none&fd=&td=&tgp=410&lines=single&draftteam=ALL" %>% 
    glue()
  
  player_stats <- url %>%
    read_html() %>% 
    html_nodes("table") %>% 
    html_table() %>% 
    pluck(1) %>% 
    clean_names() %>% 
    mutate(season = as.character(season)) %>% 
    mutate(season = str_sub(season, start = -4, end = -1), 
           season = as.numeric(season)) %>% 
    relocate(season)
  
  return(player_stats)
  
}

# get the data from the past six seasons
player_stats <- c(20142015, 20152016, 20162017, 20172018, 20182019, 
                  20192020, 20202021, 20212022, 20222023, 20232024) %>% 
  map_df( ~get_f_stats( . ) ) %>% 
  select(season, player, gp, toi, toi_gp:ipp, ix_g_60)

# get the past data frame
past_stats <- player_stats %>% 
  mutate(season = season + 1) %>% 
  rename_with(.fn = ~paste("past_", ., sep = ""), .cols = !player)

# join past data frame to current data frame
tib <- player_stats %>% 
  inner_join(past_stats, by = join_by(season == past_season, player))

# create a data frame with an indication of players by percentile 
# Do by total scoring rate, goal scoring rate, toi per game

tib_perc <- tib %>% 
  mutate(
    toi_weight = toi_gp * gp, 
    scoring_weight = total_points_60 * toi, 
    goal_weight = goals_60 * toi
    ) %>% 
  group_by(player) %>% 
  summarise(
    avg_toi = sum(toi_weight) / sum(gp), 
    avg_scoring = sum(scoring_weight) / sum(toi), 
    avg_goal = sum(goal_weight) / sum(toi)
    ) %>% 
  ungroup() %>% 
  mutate(
    toi_tile = ntile(desc(avg_toi), 5 ),
    scoring_tile = ntile( desc(avg_scoring), 5 ), 
    goal_tile = ntile(desc(avg_goal), 5 )
    ) %>% 
  select( player, toi_tile:last_col() )

# add quintiles to main data frame
df <- tib %>% 
  left_join( tib_perc, by = join_by( player ) ) %>% 
  mutate(
    toi_tile = case_when(
      toi_tile == 1 ~ "Upper Quintile", 
      toi_tile == 2 ~ "60th to 80th Percentile", 
      toi_tile == 3 ~ "40th to 60th Percentile", 
      toi_tile == 4 ~ "20th to 40th Percentile", 
      toi_tile == 5 ~ "Bottom Quintile"
    ), 
    scoring_tile = case_when(
      scoring_tile == 1 ~ "Upper Quintile", 
      scoring_tile == 2 ~ "60th to 80th Percentile", 
      scoring_tile == 3 ~ "40th to 60th Percentile", 
      scoring_tile == 4 ~ "20th to 40th Percentile", 
      scoring_tile == 5 ~ "Bottom Quintile"
    ), 
    goal_tile = case_when(
      goal_tile == 1 ~ "Upper Quintile", 
      goal_tile == 2 ~ "60th to 80th Percentile", 
      goal_tile == 3 ~ "40th to 60th Percentile", 
      goal_tile == 4 ~ "20th to 40th Percentile", 
      goal_tile == 5 ~ "Bottom Quintile"
    )
  ) %>% 
  mutate(
    toi_tile = factor(toi_tile, 
                      levels = c( "Upper Quintile", "60th to 80th Percentile", 
                                  "40th to 60th Percentile", "20th to 40th Percentile", 
                                  "Bottom Quintile" ) ), 
    scoring_tile = factor(scoring_tile, 
                      levels = c( "Upper Quintile", "60th to 80th Percentile", 
                                  "40th to 60th Percentile", "20th to 40th Percentile", 
                                  "Bottom Quintile" ) ),
    goal_tile = factor(goal_tile, 
                      levels = c( "Upper Quintile", "60th to 80th Percentile", 
                                  "40th to 60th Percentile", "20th to 40th Percentile", 
                                  "Bottom Quintile" ) ),
  )

# create R2 plots faceting by percentile
df %>% 
  ggplot( aes( past_second_assists_60, second_assists_60 ) ) + 
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq( use_label("R2"), formula = y ~ x ) + 
  theme_bw() + 
  labs(x = "Secondary Assists per 60 Year N", y = "Secondary Assists per 60 Year N + 1", 
       title = "Secondary Assist YoY Consistency for Avg TOI Quintiles", 
       subtitle = "Pairs for Forwards with 700 EV TOI in Each Season Since 2015", 
       caption = "Data via NaturalStatTrick") +  
  facet_wrap( vars( toi_tile ) )

df %>% 
  ggplot( aes( past_second_assists_60, second_assists_60 ) ) + 
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq( use_label("R2"), formula = y ~ x ) + 
  theme_bw() + 
  labs(x = "Secondary Assists per 60 Year N", y = "Secondary Assists per 60 Year N + 1", 
       title = "Secondary Assist YoY Consistency for Avg Scoring Rate Quintiles", 
       subtitle = "Pairs for Forwards with 700 EV TOI in Each Season Pair Since 2015", 
       caption = "Data via NaturalStatTrick") +  
  facet_wrap( vars( scoring_tile ) )

df %>% 
  ggplot( aes( past_second_assists_60, second_assists_60 ) ) + 
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq( use_label("R2"), formula = y ~ x ) + 
  theme_bw() + 
  labs(x = "Secondary Assists per 60 Year N", y = "Secondary Assists per 60 Year N + 1", 
       title = "Secondary Assist YoY Consistency for Avg Goal Rate Quintiles", 
       subtitle = "Pairs for Forwards with 700 EV TOI in Each Season Since 2015", 
       caption = "Data via NaturalStatTrick") +  
  facet_wrap( vars( goal_tile ) )


