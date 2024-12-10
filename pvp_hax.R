# TODO:
#adjust scoring?
#warning when type not found!
#verify atk/def weights
#change defense rating to assume they'll use strongest move?

library(dplyr)
library(readr)
library(crayon)
library(stringr)

setwd('~/git/pokehax')

# setup ####

all_types <- read_csv('data/ul.csv') %>%
    rename(Name = Pokemon) %>%
    mutate(Name = tolower(Name),
           `Type 2` = if_else(`Type 2` == 'none', '', `Type 2`),
           `Type 1` = str_to_sentence(`Type 1`),
           `Type 2` = str_to_sentence(`Type 2`))

moves <- read_csv('data/moves.csv')

source('helpers.R')

# config ####

#attacking type effectiveness chart
atk_chart <- list(
    Fairy = c(Dragon = 1.6, Dark = 1.6, Fighting = 1.6, Steel = 0.62, Poison = 0.62, Fire = 0.62),
    Normal = c(Ghost = 0.39, Rock = 0.62, Steel = 0.62),
    Electric = c(Water = 1.6, Flying = 1.6, Ground = 0.39),
    Psychic = c(Fighting = 1.6, Poison = 1.6, Dark = 0.39, Steel = 0.62, Psychic = 0.62),
    Dark = c(Psychic = 1.6, Ghost = 1.6, Fighting = 0.62, Dark = 0.62, Fairy = 0.62),
    Poison = c(Fairy = 1.6, Grass = 1.6, Ground = 0.62, Rock = 0.62, Ghost = 0.62, Poison = 0.62, Steel = 0.39),
    Rock = c(Flying = 1.6, Bug = 1.6, Fire = 1.6, Ice = 1.6, Fighting = 0.62, Ground = 0.62, Steel = 0.62),
    Ground = c(Electric = 1.6, Fire = 1.6, Rock = 1.6, Poison = 1.6, Steel = 1.6, Flying = 0.39, Grass = 0.62, Bug = 0.62),
    Steel = c(Ice = 1.6, Rock = 1.6, Fairy = 1.6, Fire = 0.62, Water = 0.62, Electric = 0.62, Steel = 0.62),
    Ice = c(Dragon = 1.6, Flying = 1.6, Grass = 1.6, Ground = 1.6, Fire = 0.62, Ice = 0.62, Steel = 0.62, Water = 0.62),
    Fighting = c(Dark = 1.6, Ice = 1.6, Normal = 1.6, Rock = 1.6, Steel = 1.6, Bug = 0.62, Fairy = 0.62, Flying = 0.62, Ghost = 0.39, Poison = 0.62, Psychic = 0.62),
    Water = c(Fire = 1.6, Ground = 1.6, Rock = 1.6, Dragon = 0.62, Grass = 0.62, Water = 0.62),
    Grass = c(Ground = 1.6, Rock = 1.6, Water = 1.6, Bug = 0.62, Dragon = 0.62, Fire = 0.62, Flying = 0.62, Grass = 0.62, Poison = 0.62, Steel = 0.62),
    Ghost = c(Ghost = 1.6, Psychic = 1.6, Dark = 0.62, Normal = 0.39)
)

def_chart <- list(
    Bug = c(Fire = 1.6, Flying = 1.6, Rock = 1.6, Fighting = 0.62, Grass = 0.62, Ground = 0.62),
    Dark = c(Bug = 1.6, Fairy = 1.6, Fighting = 1.6, Dark = 0.62, Ghost = 0.62, Psychic = 0.39),
    Dragon = c(Dragon = 1.6, Fairy = 1.6, Ice = 1.6, Electric = 0.62, Fire = 0.62, Grass = 0.62, Water = 0.62),
    Electric = c(Ground = 1.6, Electric = 0.62, Flying = 0.62, Steel = 0.62),
    Fairy = c(Poison = 1.6, Steel = 1.6, Bug = 0.62, Dark = 0.62, Dragon = 0.39, Fighting = 0.62),
    Fighting = c(Fairy = 1.6, Flying = 1.6, Psychic = 1.6, Bug = 0.62, Dark = 0.62, Rock = 0.62),
    Fire = c(Ground = 1.6, Rock = 1.6, Water = 1.6, Bug = 0.62, Fairy = 0.62, Grass = 0.62, Ice = 0.62, Steel = 0.62),
    Flying = c(Electric = 1.6, Ice = 1.6, Rock = 1.6, Bug = 0.62, Fighting = 0.62, Normal = 0.39, Poison = 0.62),
    Ghost = c(Dark = 1.6, Ghost = 1.6, Bug = 0.62, Fighting = 0.39, Normal = 0.39, Poison = 0.62),
    Grass = c(Bug = 1.6, Fire = 1.6, Flying = 1.6, Ice = 1.6, Poison = 1.6, Electric = 0.62, Grass = 0.62, Ground = 0.62, Water = 0.62),
    Ground = c(Grass = 1.6, Ice = 1.6, Water = 1.6, Electric = 0.39, Poison = 0.62, Rock = 0.62),
    Ice = c(Fighting = 1.6, Fire = 1.6, Rock = 1.6, Steel = 1.6, Ice = 0.62),
    Normal = c(Fighting = 1.6, Ghost = 0.39),
    Psychic = c(Bug = 1.6, Dark = 1.6, Ghost = 1.6, Fighting = 0.62, Psychic = 0.62),
    Poison = c(Ground = 1.6, Psychic = 1.6, Bug = 0.62, Fairy = 0.62, Fighting = 0.62, Grass = 0.62, Poison = 0.62),
    Psychic = c(Bug = 1.6, Dark = 1.6, Ghost = 1.6, Fighting = 0.62, Psychic = 0.62),
    Rock = c(Fighting = 1.6, Grass = 1.6, Ground = 1.6, Steel = 1.6, Water = 1.6, Fire = 0.62, Flying = 0.62, Normal = 0.62, Poison = 0.62),
    Steel = c(Fighting = 1.6, Fire = 1.6, Ground = 1.6, Bug = 0.62, Dragon = 0.62, Fairy = 0.62, Flying = 0.62, Grass = 0.62, Ice = 0.62, Normal = 0.62, Poison = 0.39, Psychic = 0.62, Rock = 0.62, Steel = 0.62),
    Water = c(Electric = 1.6, Grass = 1.6, Fire = 0.62, Ice = 0.62, Steel = 0.62, Water = 0.62)
)

quick_mapping <- c(gr = 'Grass',
                   le = 'Grass', ##
                   pl = 'Grass', ##
                   fi = 'Fire',
                   wa = 'Water',
                   da = 'Dark',
                   ps = 'Psychic',
                   gh = 'Ghost',
                   bu = 'Bug',
                   fl = 'Flying',
                   el = 'Electric',
                   li = 'Electric', ##
                   ro = 'Rock',
                   st = 'Steel',
                   ic = 'Ice',
                   fa = 'Fairy',
                   go = 'Ground', #
                   po = 'Poison',
                   no = 'Normal',
                   fg = 'Fighting', #
                   dr = 'Dragon')

#available attack types for each team member
team_atk <- list(
    Clefable = c("Fairy", "Normal", "Fairy"),
    # Poliwrath = c("Fighting", "Water", "Ice"),
    # Poliwrath = c("Fighting", "Water", "Fighting"),
    # Leafeon = c('Normal', 'Grass', 'Grass'),
    # Steelix = c("Electric", "Psychic", "Dark"),
    Steelix = c("Electric", "Steel", "Dark"),
    Clodsire = c("Poison", "Rock", "Ground")
)

team_def <- list(
    Clefable = 'Fairy',
    Steelix = c("Steel", "Ground"),
    # Poliwrath = c('Water', 'Fighting'),
    # Leafeon = 'Grass',
    Clodsire = c('Poison', 'Ground')
)

# battle zone ####

rank_team(      'icst')
hax(autocomplete(`feraligatr (shadow)` =  T))
hax(autocomplete(T))
