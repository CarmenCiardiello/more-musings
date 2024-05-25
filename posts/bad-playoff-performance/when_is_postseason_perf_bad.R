library(tidyverse)
library(hockeyR)
library(glue)
library(janitor)
library(duckplyr)


####################### LOAD in PBP ######################
load_multiple_pbp <- function(season){
  
  if(season == 2024){
    
    df <- load_pbp(season) %>% 
      mutate(
        game_date = as_date(game_date)
      )
    
  } else{
    
    df <- load_pbp(season) %>% 
      mutate(
        season = as.numeric(season), 
        home_id = as.integer(home_id), 
        away_id = as.integer(away_id)
      )
    
  }
  
  return(df)
  
}

pbp <- 2018:2024 %>% 
  map_df( ~load_multiple_pbp(.) ) %>% 
  as_duckplyr_df()
################################################################################

##################### GET REGULAR AND PLAYOFF STATS FOR PLAYERS #######################

get_individ_stats <- function(pbp, season, game_type, strength = "all"){
  # game_type should either by "R" or "P"
  # game_strength would be "5v5", "4v5", "5v4", etc. Defaults to "all"
  
  season_input <- as.numeric(paste(season - 1, season, sep = ""))
  
  player_stats <- pbp %>% 
    filter(season == season_input) %>% 
    calculate_individual(
      type = game_type, game_strength = strength
    ) %>% 
    mutate(
      season = season, game_type = game_type, strength = strength
    ) %>% 
    relocate(
      player_name:team, season, game_type, strength
    )
  
  return(player_stats)
  
}

reg_stats <- 2018:2024 %>% 
  map_df( ~get_individ_stats(pbp, ., "R")) %>% 
  select(
    player_name, player_id, team, season, strength, 
    gp, ixg, goals, assists, points, assists_primary, isog, icf
  ) %>% 
  rename(
    gp_r = gp, 
    ixgr = ixg, 
    goals_r = goals, 
    assists_r = assists, 
    points_r = points, 
    assists_primary_r = assists_primary, 
    isog_r = isog, 
    icf_r = icf
  )

playoff_stats <- 2018:2024 %>% 
  map_df( ~get_individ_stats(pbp, ., "P")) %>% 
  select(
    player_name, player_id, season, strength, 
    gp, ixg, goals, assists, points, assists_primary, isog, icf
  ) %>% 
  rename(
    gp_p = gp, 
    ixg_p = ixg, 
    goals_p = goals, 
    assists_p = assists, 
    points_p = points, 
    assists_primary_p = assists_primary, 
    isog_p = isog, 
    icf_p = icf
  )
################################################################################

###################### MERGE REG AND PLAYOFFS AND ADD PER GAME STATS ####################

player_stats <- reg_stats %>% 
  left_join(
    playoff_stats, 
    by = join_by(player_name, player_id, season, strength)
    ) %>% 
  mutate(
    player_name = str_replace_all(
      string = player_name, 
      pattern = "\\.",
      replacement = " "
    )
  ) %>% 
  mutate(player_name = case_when(player_name == "Drew O Connor" ~ "Drew O'Connor",
                               player_name == "Logan O Connor" ~ "Logan O'Connor",
                               player_name == "Liam O Brien" ~ "Liam O'Brien",
                               player_name == "Ryan O Reilly" ~ "Ryan O'Reilly",
                               player_name == "Jean Gabriel Pageau" ~ "Jean-Gabriel Pageau",
                               player_name == "K Andre Miller" ~ "K'Andre Miller",
                               player_name == "Marc Edouard Vlasic" ~ "Marc-Edouard Vlasic",
                               player_name == "Pierre Edouard Bellemare" ~ "Pierre-Edouard Bellemare",
                               player_name == "Nicolas Aube Kubel" ~ "Nicolas Aube-Kubel",
                               player_name == "Oliver Ekman Larsson" ~ "Oliver Ekman-Larsson",
                               player_name == "Pierre Luc Dubois" ~ "Pierre-Luc Dubois",
                               player_name == "Ryan Nugent Hopkins" ~ "Ryan Nugent-Hopkins",
                               player_name == "Zach Aston Reese" ~ "Zach Aston-Reese",
                               .default = player_name)
  ) %>% 
  as_duckplyr_df()

df <- player_stats %>% 
  mutate(
    ixgr_gp = ixgr / gp_r, 
    goals_r_gp = goals_r / gp_r, 
    assists_r_gp = assists_r / gp_r,
    points_r_gp = points_r / gp_r, 
    assists_primary_r_gp = assists_primary_r / gp_r, 
    isog_r_gp = isog_r / gp_r, 
    icf_r_gp = icf_r / gp_r,
    ixg_p_gp = ixg_p / gp_p, 
    goals_p_gp = goals_p / gp_p, 
    assists_p_gp = assists_p / gp_p,
    points_p_gp = points_p / gp_p, 
    assists_primary_p_gp = assists_primary_p / gp_p, 
    isog_p_gp = isog_p / gp_p, 
    icf_p_gp = icf_p / gp_p
  ) %>% 
  mutate(
    ixg_diff = ixg_p_gp - ixgr_gp, 
    goals_diff = goals_p_gp - goals_r_gp, 
    assists_diff = assists_p_gp - assists_r_gp, 
    points_diff = points_p_gp - points_r_gp, 
    assists_primary_diff = assists_primary_p_gp - assists_primary_r_gp, 
    isog_diff = isog_p_gp - isog_r_gp, 
    icf_diff = icf_p_gp - icf_r_gp
  )
################################################################################

######################## ANALYSIS OF RESULTS ##############################

# weighted average of per game rate changes between regular season and playoffs
df %>% 
  na.omit() %>%
  mutate(gp = 1 / (1/gp_r + 1/gp_p)) %>% 
  summarise(
    ixg_diff = sum( ixg_diff*gp ) / sum(gp),
    goals_diff = sum( goals_diff*gp ) / sum(gp),
    assists_diff = sum( assists_diff*gp ) / sum(gp),
    points_diff = sum( points_diff*gp ) / sum(gp),
    assists_primary_diff = sum( assists_primary_diff*gp ) / sum(gp),
    isog_diff = sum( isog_diff*gp ) / sum(gp)
  )

# get data frame with just the differences columns
tib <- df %>% 
  na.omit() %>% 
  select(player_name, player_id, team, season, gp_r, gp_p, ixg_diff:last_col())

#################################################################################

######################## BUILD THE MODEL ###########################################

# build a logistic regression model
# parameters are past playoff games played and career difference between ppg in reg vs playoffs
# target is whether you exceed average difference
# use cumsum functionality in dplyr

df1 <- df %>% 
  mutate(
    points_p = ifelse(is.na(points_p), 0, points_p), 
    gp_p = ifelse(is.na(gp_p), 0, gp_p)
  ) %>% 
  arrange(player_name, season) %>% 
  group_by(player_name, player_id) %>% 
  mutate(
    gp_r_career = cumsum(gp_r),
    gp_p_career = cumsum(gp_p), 
    points_r_career = cumsum(points_r), 
    points_p_career = cumsum(points_p), 
    ppg_r_career = points_r_career / gp_r_career
  ) %>% 
  select(player_name:gp_r, points_r, gp_p, points_p, gp_r_career:last_col()) %>% 
  filter(team != "0") %>% 
  ungroup() %>% 
  mutate(
    career_ppg_diff = (points_p_career/gp_p_career) - (points_r_career/gp_r_career), 
    career_ppg_diff = ifelse(gp_p_career == 0, NA, career_ppg_diff), 
    fail = ifelse(points_p / gp_p < points_r_career / gp_r_career, "yes", "no"), 
    fail = as.factor(fail)
  ) %>% 
  na.omit()

# create model recipe
mod_recipe <- recipe(
  fail ~ career_ppg_diff + gp_p_career + ppg_r_career, 
  data = df1
)

# set model specification and create modelling workflow
lr_spec <- logistic_reg(penalty = tune(), mixture = 1) %>% 
  set_engine("glmnet")

lr_wf <- workflow() %>% 
  add_model(lr_spec) %>% 
  add_recipe(mod_recipe)

sim_folds <- vfold_cv(df1, v = 5, strata = fail)

grid <- tibble(penalty = 10^seq(-3, 0, length.out = 20))

set.seed(3)
lr_res <- lr_wf %>% 
  tune_grid(resamples = sim_folds, grid = grid)

best_mod <- lr_res %>% select_best(metric = "accuracy")

final_wf <- lr_wf %>% finalize_workflow(best_mod)

final_fit <- fit(final_wf, data = df1)

df1 <- df1 %>% 
  bind_cols(
    predict(final_fit, df1, type = "prob")
  )
################################################################################

######################## LOOK AT ISSUE WITH BRUTE FORCE ########################

# Look at playoff games accumulated and chances the following year you under-perform
df1 <- df1 %>%
  group_by(player_name, player_id) %>% 
  mutate(
    under_perform = ifelse(points_p/gp_p - ppg_r_career < -0.08, 1, 0), 
    career_games = max(gp_p_career) + max(gp_r_career), 
    career_playoff_games = max(gp_p_career), 
    delta_p_ppg = points_p_career/gp_p_career - lag( points_p_career/gp_p_career ),
    delta_diff_ppg = career_ppg_diff - lag( career_ppg_diff )
  )

df1 %>% 
  ggplot( aes( gp_p_career, delta_p_ppg ) ) + 
  geom_point() +
  theme_bw() + 
  labs(
    x = "Career Playoff Games to Date", 
    y = "Year over Year Change in Playoff PPG", 
    title = "Changes in Career Playoff Scoring Averages as a Function of Games Played", 
    subtitle = "Data from 2018-2023 Postseasons"   
  )

df1 %>% 
  ggplot( aes( gp_p_career, delta_p_ppg ) ) + 
  geom_smooth(span = 2) +
  theme_bw() + 
  labs(
    x = "Career Playoff Games to Date", 
    y = "Year over Year Change in Playoff PPG", 
    title = "Changes in Career Playoff Scoring Averages (Smoothed)", 
    subtitle = "Data from 2018-2023 Postseasons"   
  )

df1 %>% 
  ggplot( aes( gp_p_career, delta_diff_ppg ) ) + 
  geom_point() +
  theme_bw() + 
  labs(
    x = "Career Playoff Games to Date", 
    y = "Year over Year Change in Scoring", 
    title = "Changes in Career Delta Between Reg and Postseason Scoring as a Function of Games Played", 
    subtitle = "Data from 2018-2023 Postseasons"   
  )

df1 %>% 
  ggplot( aes( gp_p_career, delta_diff_ppg ) ) + 
  geom_smooth() +
  theme_bw() + 
  labs(
    x = "Career Playoff Games to Date", 
    y = "Year over Year Change in Scoring", 
    title = "Changes in Career Delta Between Reg and Postseason Scoring (Smoothed)", 
    subtitle = "Data from 2018-2023 Postseasons"   
  )

df1 %>%
  filter(career_playoff_games >= 25) %>% 
  ggplot( aes( career_ppg_diff ) ) + 
  geom_histogram() + 
  theme_bw() + 
  labs(
    x = "Difference in Career Scoring Averages in Regular Season and Postseason"
  )

################################################################################
