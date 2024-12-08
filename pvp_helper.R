library(dplyr)
library(readr)
library(crayon)
library(stringr)

all_types <- read_csv('~/Dropbox/stuff_2/game_notes/pokemon_go/types.csv') %>%
    filter(! grepl('Full Belly', Form)) %>%
    mutate(Form = if_else(grepl('Hangry', Form), NA, Form)) %>%
    # filter(! grepl('Mega', Form)) %>%
    filter(is.na(Form)) %>%
    select(-Form) %>%
    mutate(Name = tolower(Name)) %>%
    distinct()

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
    Grass = c(Ground = 1.6, Rock = 1.6, Water = 1.6, Bug = 0.62, Dragon = 0.62, Fire = 0.62, Flying = 0.62, Grass = 0.62, Poison = 0.62, Steel = 0.62)
)

def_chart <- list(
    Fairy = c(Poison = 1.6, Steel = 1.6, Bug = 0.62, Dark = 0.62, Dragon = 0.39, Fighting = 0.62),
    Normal = c(Fighting = 1.6, Ghost = 0.39),
    Electric = c(Ground = 1.6, Electric = 0.62, Flying = 0.62, Steel = 0.62),
    Psychic = c(Bug = 1.6, Dark = 1.6, Ghost = 1.6, Fighting = 0.62, Psychic = 0.62),
    Dark = c(Bug = 1.6, Fairy = 1.6, Fighting = 1.6, Dark = 0.62, Ghost = 0.62, Psychic = 0.39),
    Poison = c(Ground = 1.6, Psychic = 1.6, Bug = 0.62, Fairy = 0.62, Fighting = 0.62, Grass = 0.62, Poison = 0.62),
    Rock = c(Fighting = 1.6, Grass = 1.6, Ground = 1.6, Steel = 1.6, Water = 1.6, Fire = 0.62, Flying = 0.62, Normal = 0.62, Poison = 0.62),
    Ground = c(Grass = 1.6, Ice = 1.6, Water = 1.6, Electric = 0.39, Poison = 0.62, Rock = 0.62),
    Steel = c(Fighting = 1.6, Fire = 1.6, Ground = 1.6, Bug = 0.62, Dragon = 0.62, Fairy = 0.62, Flying = 0.62, Grass = 0.62, Ice = 0.62, Normal = 0.62, Poison = 0.39, Psychic = 0.62, Rock = 0.62, Steel = 0.62),
    Ice = c(Fighting = 1.6, Fire = 1.6, Rock = 1.6, Steel = 1.6, Ice = 0.62),
    Fighting = c(Fairy = 1.6, Flying = 1.6, Psychic = 1.6, Bug = 0.62, Dark = 0.62, Rock = 0.62),
    Water = c(Electric = 1.6, Grass = 1.6, Fire = 0.62, Ice = 0.62, Steel = 0.62, Water = 0.62),
    Grass = c(Bug = 1.6, Fire = 1.6, Flying = 1.6, Ice = 1.6, Poison = 1.6, Electric = 0.62, Grass = 0.62, Ground = 0.62, Water = 0.62)
)

quick_mapping <- c(gr = 'Grass',
                   le = 'Grass',
                   pl = 'Grass',
                   fi = 'Fire',
                   wa = 'Water',
                   da = 'Dark',
                   ps = 'Psychic',
                   gh = 'Ghost',
                   bu = 'Bug',
                   fl = 'Flying',
                   el = 'Electric',
                   li = 'Electric',
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

atk_effectiveness <- function(pokemon_moves, enemy_types) {
    type_scores <- sapply(pokemon_moves, function(move) {
        score <- 1
        for (enemy_type in enemy_types) {
            if (move %in% names(atk_chart) && enemy_type %in% names(atk_chart[[move]])) {
                score <- score * atk_chart[[move]][[enemy_type]]
            }
        }
        return(score)
    })
    return(type_scores)
}

def_effectiveness <- function(team_types, enemy_types) {
    type_scores <- sapply(enemy_types, function(enemy_type) {
        score <- 1
        for (type in team_types) {
            if (type %in% names(def_chart) && enemy_type %in% names(def_chart[[type]])) {
                score <- score * def_chart[[type]][[enemy_type]]
            }
        }
        return(score)
    })
    return(type_scores)
}

fix_992 <- function(x){
    ifelse(x == 0.992, 1, x)
}

rank_team <- function(types_condensed) {

    cat(paste0(magenta(types_condensed), '\n\n'))
    type1 <- substr(types_condensed, 1, 2)
    type2 <- substr(types_condensed, 3, 4)

    type1 <- unname(quick_mapping[names(quick_mapping) == type1])
    type2 <- unname(quick_mapping[names(quick_mapping) == type2])

    enemy_types <- unique(c(type1, type2))
    rankings <- lapply(names(team_atk), function(pokemon) {

        atk_scores <- atk_effectiveness(team_atk[[pokemon]], enemy_types)
        atk_scores_comparable <- c(atk_scores[1], max(atk_scores[2:length(atk_scores)]))
        atk_score_total <- mean(atk_scores_comparable, na.rm = TRUE)

        def_scores <- def_effectiveness(team_def[[pokemon]], enemy_types)
        def_scores_comparable <- c(def_scores[1], mean(def_scores[2:length(def_scores)]))
        def_score_total <- mean(def_scores_comparable, na.rm = TRUE)

        list(
            Pokemon = pokemon,
            AttackingTypes = atk_scores,
            DefendingTypes = def_scores,
            # AttackingTypes = sort(atk_scores, decreasing = TRUE),
            # DefendingTypes = sort(def_scores, decreasing = FALSE),
            TotalScoreAtk = round(atk_score_total, 2),
            TotalScoreDef = round(def_score_total, 2),
            TotalScore = round(atk_score_total - def_score_total + 1, 2)
        )
    })

    # Sort by total scores
    ranked_team <- rankings[order(sapply(rankings, function(x) x$TotalScore), decreasing = TRUE)]

    # Format the output
    output <- lapply(ranked_team, function(entry) {
        list(
            Pokemon = entry$Pokemon,
            AttackingTypes = entry$AttackingTypes,
            DefendingTypes = entry$DefendingTypes,
            TotalScoreAtk = entry$TotalScoreAtk,
            TotalScoreDef = entry$TotalScoreDef,
            TotalScore = entry$TotalScore
        )
    })

    for (member in output) {

    member$TotalScore <- fix_992(member$TotalScore)
    member$AttackingTypes <- fix_992(member$AttackingTypes)
    member$DefendingTypes <- fix_992(member$DefendingTypes)
    member$TotalScoreDef <- fix_992(member$TotalScoreDef)
    member$TotalScoreAtk <- fix_992(member$TotalScoreAtk)

        tot <- member$TotalScore
        tot <- case_when(tot > 1 ~ green(tot),
                         tot == 1 ~ silver(tot),
                         tot < 1 ~ red(tot))

        atk <- member$TotalScoreAtk
        atk <- case_when(atk > 1 ~ green(atk),
                         atk == 1 ~ silver(atk),
                         atk < 1 ~ red(atk))

        def <- member$TotalScoreDef
        def <- case_when(def > 1 ~ red(def),
                         def == 1 ~ silver(def),
                         def < 1 ~ green(def))

        fast_move <- member$AttackingTypes[1]
        fast_type <- names(member$AttackingTypes[1])
        fast_move_ <- str_pad(fast_move, side = 'both', width = nchar(fast_type), pad = ' ')
        fast_move <- case_when(fast_move > 1 ~ green(fast_move_),
                               fast_move == 1 ~ silver(fast_move_),
                               fast_move < 1 ~ red(fast_move_))

        chg_move1 <- member$AttackingTypes[2]
        chg1_type <- names(member$AttackingTypes[2])
        chg_move1_ <- str_pad(chg_move1, side = 'both', width = nchar(chg1_type), pad = ' ')
        chg_move1 <- case_when(chg_move1 > 1 ~ green(chg_move1_),
                               chg_move1 == 1 ~ silver(chg_move1_),
                               chg_move1 < 1 ~ red(chg_move1_))

        chg_move2 <- member$AttackingTypes[3]
        chg2_type <- names(member$AttackingTypes[3])
        chg_move2_ <- str_pad(chg_move2, side = 'both', width = nchar(chg2_type), pad = ' ')
        chg_move2 <- case_when(chg_move2 > 1 ~ green(chg_move2_),
                               chg_move2 == 1 ~ silver(chg_move2_),
                               chg_move2 < 1 ~ red(chg_move2_))

        def1 <- member$DefendingTypes[1]
        def_type1 <- names(member$DefendingTypes[1])
        def1_ <- str_pad(def1, side = 'both', width = nchar(def_type1), pad = ' ')
        def1 <- case_when(def1 > 1 ~ green(def1_),
                          def1 == 1 ~ silver(def1_),
                          def1 < 1 ~ red(def1_))

        if(length(member$DefendingTypes) == 2){
            def2 <- member$DefendingTypes[2]
            def_type2 <- names(member$DefendingTypes[2])
            def2_ <- str_pad(def2, side = 'both', width = nchar(def_type2), pad = ' ')
            def2 <- case_when(def2 > 1 ~ green(def2_),
                              def2 == 1 ~ silver(def2_),
                              def2 < 1 ~ red(def2_))
        }

        cat(paste0(blue(member$Pokemon), ' (', tot, ')\n'))
        cat(paste0(yellow('Atk: '), atk, '\n'))
        cat(paste0('\t', fast_type, magenta(' | '), chg1_type, ' | ', chg2_type, '\n',
                   '\t', fast_move, magenta(' | '), chg_move1, ' | ', chg_move2))
        cat(paste0(yellow('\nDef: '), def, '\n'))
        if(length(member$DefendingTypes) == 2){
            cat(paste0('\t', def_type1, cyan(' | '), def_type2, '\n',
                       '\t', def1, cyan(' | '), def2))
        } else {
            cat(paste0('\t', def_type1, '\n\t', def1))
        }
        cat('\n\n')
    }
}

type_lookup <- function(name){

    types <- filter(all_types, Name == tolower(name)) %>%
        select(-Name) %>%
        unlist(use.names = FALSE)

    short_form <- paste(names(quick_mapping[quick_mapping %in% types]), collapse = '')
    return(short_form)
}

type_lookup('charizard')

# autocomplete_function <- function(token) {
#     grep(paste0("^", token), all_types$Name, value = TRUE)
# }
#
# utils::rc.settings(custom.completer = autocomplete_function)

rank_team('fifl')
rank_team('flel')
rank_team('gogo')

# allow autocomplete of pokemon names

define_params <- function(param_names) {
    # Convert parameter names into formals (arguments)
    args <- setNames(vector("list", length(param_names)), param_names)

    # body <- quote({
    #     args
    # })
    body <- quote({
        specified <- names(Filter(function(x) ! is.null(x), as.list(environment(), all=TRUE)))
        return(specified)
    })

    # Create the function
    func <- eval(call("function", as.pairlist(args), body))

    return(func)
}

autocomplete <- define_params(param_names = all_types$Name)

hax <- function(name){
    cat(paste0(magenta(toupper(name)), '\n'))
    rank_team(type_lookup(name))le
}

########################
##### BATTLE ZONE ######
########################

galarian stunfisk: gost
bastodon?: rost
#should be use leavanny?
#turbo weaknesses
#forrestres (has electric and ground!?)
rank_team(      'icst')
hax(autocomplete( T))
hax(autocomplete(excadrill = T))
#deoxys not listed
#warning when type not found!
#galarian etc
#change defense rating to assume they'll use strongest move
