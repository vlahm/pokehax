# TODO:
#adjust scoring?
#consider filtering Shadow varietals

library(dplyr)
library(readr)
library(crayon)
library(stringr)

# setwd('~/git/pokehax')
setwd('C:/Users/vlahm/Desktop/git/pokehax/')

# setup ####

all_types <- read_csv('data/ul.csv', show_col_types = FALSE) %>%
# all_types <- read_csv('data/cp1500_all_leads_rankings.csv', show_col_types = FALSE) %>%
# all_types <- read_csv('data/cp1500_color_leads_rankings.csv', show_col_types = FALSE) %>%
    rename(Name = Pokemon) %>%
    # filter(! grepl('(Shadow)', Name)) %>%
    mutate(Name = tolower(Name),
           `Type 2` = if_else(`Type 2` == 'none', '', `Type 2`),
           `Type 1` = str_to_sentence(`Type 1`),
           `Type 2` = str_to_sentence(`Type 2`)) %>%
    distinct(Name, .keep_all = TRUE)

moves <- read_csv('data/moves.csv', show_col_types = FALSE)

source('helpers.R')

#attacking type effectiveness chart
atk_chart <- list(
    Bug = c(Dark = 1.6, Grass = 1.6, Psychic = 1.6, Fairy = 0.625, Fighting = 0.625, Fire = 0.625, Flying = 0.625, Ghost = 0.625, Poison = 0.625, Steel = 0.625),
    Dark = c(Psychic = 1.6, Ghost = 1.6, Fighting = 0.625, Dark = 0.625, Fairy = 0.625),
    Dragon = c(Dragon = 1.6, Fairy = 0.39, Steel = 0.625),
    Electric = c(Water = 1.6, Flying = 1.6, Ground = 0.39, Dragon = 0.625, Electric = 0.625, Grass = 0.625),
    Fairy = c(Dragon = 1.6, Dark = 1.6, Fighting = 1.6, Steel = 0.625, Poison = 0.625, Fire = 0.625),
    Fighting = c(Dark = 1.6, Ice = 1.6, Normal = 1.6, Rock = 1.6, Steel = 1.6, Bug = 0.625, Fairy = 0.625, Flying = 0.625, Ghost = 0.39, Poison = 0.625, Psychic = 0.625),
    Fire = c(Bug = 1.6, Grass = 1.6, Ice = 1.6, Steel = 1.6, Dragon = 0.625, Fire = 0.625, Rock = 0.625, Water = 0.625),
    Flying = c(Bug = 1.6, Fighting = 1.6, Grass = 1.6, Electric = 0.625, Rock = 0.625, Steel = 0.625),
    Ghost = c(Ghost = 1.6, Psychic = 1.6, Dark = 0.625, Normal = 0.39),
    Grass = c(Ground = 1.6, Rock = 1.6, Water = 1.6, Bug = 0.625, Dragon = 0.625, Fire = 0.625, Flying = 0.625, Grass = 0.625, Poison = 0.625, Steel = 0.625),
    Ground = c(Electric = 1.6, Fire = 1.6, Rock = 1.6, Poison = 1.6, Steel = 1.6, Flying = 0.39, Grass = 0.625, Bug = 0.625),
    Ice = c(Dragon = 1.6, Flying = 1.6, Grass = 1.6, Ground = 1.6, Fire = 0.625, Ice = 0.625, Steel = 0.625, Water = 0.625),
    Normal = c(Ghost = 0.39, Rock = 0.625, Steel = 0.625),
    Poison = c(Fairy = 1.6, Grass = 1.6, Ground = 0.625, Rock = 0.625, Ghost = 0.625, Poison = 0.625, Steel = 0.39),
    Psychic = c(Fighting = 1.6, Poison = 1.6, Dark = 0.39, Steel = 0.625, Psychic = 0.625),
    Rock = c(Flying = 1.6, Bug = 1.6, Fire = 1.6, Ice = 1.6, Fighting = 0.625, Ground = 0.625, Steel = 0.625),
    Steel = c(Ice = 1.6, Rock = 1.6, Fairy = 1.6, Fire = 0.625, Water = 0.625, Electric = 0.625, Steel = 0.625),
    Water = c(Fire = 1.6, Ground = 1.6, Rock = 1.6, Dragon = 0.625, Grass = 0.625, Water = 0.625)
)

def_chart <- list(
    Bug = c(Fire = 1.6, Flying = 1.6, Rock = 1.6, Fighting = 0.625, Grass = 0.625, Ground = 0.625),
    Dark = c(Bug = 1.6, Fairy = 1.6, Fighting = 1.6, Dark = 0.625, Ghost = 0.625, Psychic = 0.39),
    Dragon = c(Dragon = 1.6, Fairy = 1.6, Ice = 1.6, Electric = 0.625, Fire = 0.625, Grass = 0.625, Water = 0.625),
    Electric = c(Ground = 1.6, Electric = 0.625, Flying = 0.625, Steel = 0.625),
    Fairy = c(Poison = 1.6, Steel = 1.6, Bug = 0.625, Dark = 0.625, Dragon = 0.39, Fighting = 0.625),
    Fighting = c(Fairy = 1.6, Flying = 1.6, Psychic = 1.6, Bug = 0.625, Dark = 0.625, Rock = 0.625),
    Fire = c(Ground = 1.6, Rock = 1.6, Water = 1.6, Bug = 0.625, Fairy = 0.625, Grass = 0.625, Ice = 0.625, Steel = 0.625, Fire = 0.625),
    Flying = c(Electric = 1.6, Ice = 1.6, Rock = 1.6, Bug = 0.625, Fighting = 0.625, Grass = 0.625, Ground = 0.39),
    Ghost = c(Dark = 1.6, Ghost = 1.6, Bug = 0.625, Fighting = 0.39, Normal = 0.39, Poison = 0.625),
    Grass = c(Bug = 1.6, Fire = 1.6, Flying = 1.6, Ice = 1.6, Poison = 1.6, Electric = 0.625, Grass = 0.625, Ground = 0.625, Water = 0.625),
    Ground = c(Grass = 1.6, Ice = 1.6, Water = 1.6, Electric = 0.39, Poison = 0.625, Rock = 0.625),
    Ice = c(Fighting = 1.6, Fire = 1.6, Rock = 1.6, Steel = 1.6, Ice = 0.625),
    Normal = c(Fighting = 1.6, Ghost = 0.39),
    Psychic = c(Bug = 1.6, Dark = 1.6, Ghost = 1.6, Fighting = 0.625, Psychic = 0.625),
    Poison = c(Ground = 1.6, Psychic = 1.6, Bug = 0.625, Fairy = 0.625, Fighting = 0.625, Grass = 0.625, Poison = 0.625),
    Rock = c(Fighting = 1.6, Grass = 1.6, Ground = 1.6, Steel = 1.6, Water = 1.6, Fire = 0.625, Flying = 0.625, Normal = 0.625, Poison = 0.625),
    Steel = c(Fighting = 1.6, Fire = 1.6, Ground = 1.6, Bug = 0.625, Dragon = 0.625, Fairy = 0.625, Flying = 0.625, Grass = 0.625, Ice = 0.625, Normal = 0.625, Poison = 0.39, Psychic = 0.625, Rock = 0.625, Steel = 0.625),
    Water = c(Electric = 1.6, Grass = 1.6, Fire = 0.625, Ice = 0.625, Steel = 0.625, Water = 0.625)
)

quick_mapping <- c(gr = 'Grass',
                   fi = 'Fire',
                   wa = 'Water',
                   da = 'Dark',
                   ps = 'Psychic',
                   gh = 'Ghost',
                   bu = 'Bug',
                   fl = 'Flying',
                   el = 'Electric',
                   ro = 'Rock',
                   st = 'Steel',
                   ic = 'Ice',
                   fa = 'Fairy',
                   go = 'Ground', #
                   po = 'Poison',
                   no = 'Normal',
                   fg = 'Fighting', #
                   dr = 'Dragon')

# config ####

#GL

team_atk <- list(

    # Poliwrath = c("Fighting", "Water", "Ice"),
    # Poliwrath = c("Fighting", "Water", "Fighting"),
    # Leafeon = c('Normal', 'Grass', 'Grass'),
    # Steelix = c("Electric", "Psychic", "Dark"),

    #GL
    # # Gastrodon = c('Ground', 'Ground', 'Normal'),
    # Primeape = c('Fighting', 'Ghost', 'Fighting'),
    # Mandibuzz = c('Dark', 'Flying', 'Dark'),
    # Clodsire = c("Poison", "Rock", "Ground")
    # # Miltank = c('Rock', 'Electric', 'Normal')

    #UL
    # Clefable = c("Fairy", "Normal", "Fairy"),
    # Steelix = c("Electric", "Steel", "Dark"),
    # Clodsire = c("Poison", "Rock", "Ground")
    Giratina = c('Ghost', 'Dragon', 'Ghost'),
    # Annihilape = c('Fighting', 'Ghost', 'Fighting'),
    Feraligatr = c('Ghost', 'Water', 'Ice'),
    Typhlosion = c('Fire', 'Fire', 'Electric')

    # color cup
    # Gastrodon = c('Ground', 'Ground', 'Normal'),
    # `Marowak (Alolan)` = c('Fire', 'Ghost', 'Ground'),
    # Amoongus = c('Ghost', 'Grass', 'Poison')
)

team_def <- list(
    # Poliwrath = c('Water', 'Fighting'),
    # Leafeon = 'Grass',

    #GL
    # # Gastrodon = c('Ground', 'Water'),
    # Primeape = 'Fighting',
    # Mandibuzz = c('Dark', 'Flying'),
    # Clodsire = c('Poison', 'Ground')
    # # Miltank = c('Normal')

    #UL
    # Clefable = 'Fairy',
    # Steelix = c("Steel", "Ground"),
    # Clodsire = c('Poison', 'Ground')
    Giratina = c('Ghost', 'Dragon'),
    Feraligatr = c('Water'),
    Typhlosion = c('Fire')
    # Annihilape = c('Fighting', 'Ghost'),

    #color cup
    # Gastrodon = c('Ground', 'Ground', 'Normal'),
    # `Marowak (Alolan)` = c('Ghost', 'Fire'),
    # Amoongus = c('Grass', 'Poison')
)
