atk_effectiveness <- function(pokemon_moves, enemy_types) {
    type_scores <- sapply(pokemon_moves, function(move) {
        score <- 1

        if(! move %in% names(atk_chart)) warning(paste(move, 'missing from atk chart!'))

        for (enemy_type in enemy_types) {
            if (enemy_type %in% names(atk_chart[[move]])) {
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

            if(! type %in% names(def_chart)) warning(paste(type, 'missing from def chart!'))

            if (enemy_type %in% names(def_chart[[type]])) {
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

#obsolete unless plugging in types directly
rank_team <- function(types_condensed) {

    types_condensed <- sub('^li|li$', 'el', types_condensed)
    types_condensed <- sub('^le|le$', 'gr', types_condensed)
    types_condensed <- sub('^pl|pl$', 'gr', types_condensed)

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

type_lookup_def <- function(name){

    types <- filter(all_types, Name == tolower(name)) %>%
        select(-Name) %>%
        unlist(use.names = FALSE)

    short_form <- paste(names(quick_mapping[quick_mapping %in% types]), collapse = '')
    return(short_form)
}

type_lookup_atk <- function(name){

    movetypes <- suppressMessages(filter(all_types, Name == tolower(name)) %>%
                                      select(`Fast Move`:`Charged Move 2`) %>%
                                      t() %>%
                                      unname() %>%
                                      as_tibble(.name_repair = 'unique') %>%
                                      left_join(moves, by = c(`...1` = 'Move')) %>%
                                      select(Type) %>%
                                      unlist(use.names = FALSE))

    qm <- names(quick_mapping)
    names(qm) <- unname(quick_mapping)
    short_form <- paste(recode(movetypes, !!!qm), collapse = '')

    return(short_form)
}

rank_team2 <- function(def_types_condensed, atk_types_condensed) {

    dtyp <- def_types_condensed
    atyp <- atk_types_condensed

    #clean types
    dtyp <- sub('^li|li$', 'el', dtyp)
    dtyp <- sub('^le|le$', 'gr', dtyp)
    dtyp <- sub('^pl|pl$', 'gr', dtyp)

    atyp <- sub('^li|li$', 'el', atyp)
    atyp <- sub('^le|le$', 'gr', atyp)
    atyp <- sub('^pl|pl$', 'gr', atyp)
    if(str_sub(atyp, 3, 4) == 'li') atyp <- sub('(..)li(..)', '\\1el\\2', atyp, perl = TRUE)
    if(str_sub(atyp, 3, 4) == 'le') atyp <- sub('(..)le(..)', '\\1gr\\2', atyp, perl = TRUE)
    if(str_sub(atyp, 3, 4) == 'pl') atyp <- sub('(..)pl(..)', '\\1gr\\2', atyp, perl = TRUE)

    cat(paste0(magenta(dtyp), '\n\n'))

    #expand types
    type1_def <- substr(dtyp, 1, 2)
    type2_def <- substr(dtyp, 3, 4)

    type1_atk <- substr(atyp, 1, 2)
    type2_atk <- substr(atyp, 3, 4)
    type3_atk <- substr(atyp, 5, 6)

    type1_def <- unname(quick_mapping[names(quick_mapping) == type1_def])
    type2_def <- unname(quick_mapping[names(quick_mapping) == type2_def])

    type1_atk <- unname(quick_mapping[names(quick_mapping) == type1_atk])
    type2_atk <- unname(quick_mapping[names(quick_mapping) == type2_atk])
    type3_atk <- unname(quick_mapping[names(quick_mapping) == type3_atk])

    enemy_def_types <- unique(c(type1_def, type2_def))
    enemy_atk_types <- unique(c(type1_atk, type2_atk, type3_atk))

    #compare our atk to their def, vice-versa
    rankings <- lapply(names(team_atk), function(pokemon) {

        atk_scores <- atk_effectiveness(team_atk[[pokemon]], enemy_def_types)
        atk_scores_comparable <- c(atk_scores[1], max(atk_scores[2:length(atk_scores)]))
        atk_score_total <- mean(atk_scores_comparable, na.rm = TRUE)

        def_scores <- def_effectiveness(team_def[[pokemon]], enemy_atk_types)
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

        def2 <- member$DefendingTypes[2]
        def_type2 <- names(member$DefendingTypes[2])
        def2_ <- str_pad(def2, side = 'both', width = nchar(def_type2), pad = ' ')
        def2 <- case_when(def2 > 1 ~ green(def2_),
                          def2 == 1 ~ silver(def2_),
                          def2 < 1 ~ red(def2_))

        if(length(member$DefendingTypes) == 3){
            def3 <- member$DefendingTypes[3]
            def_type3 <- names(member$DefendingTypes[3])
            def3_ <- str_pad(def3, side = 'both', width = nchar(def_type3), pad = ' ')
            def3 <- case_when(def3 > 1 ~ green(def3_),
                              def3 == 1 ~ silver(def3_),
                              def3 < 1 ~ red(def3_))
        }

        if(member$Pokemon == names(team_def)[1]){
            cat(paste0(inverse(blue(member$Pokemon)), ' (', tot, ')\n'))
        } else if(member$Pokemon == names(team_def)[2]){
            cat(paste0(bold(underline(blue(member$Pokemon))), ' (', tot, ')\n'))
        } else {
            cat(paste0(italic(blue(member$Pokemon)), ' (', tot, ')\n'))
        }

        cat(paste0(yellow('Atk: '), atk, '\n'))
        cat(paste0('\t', fast_type, magenta(' | '), chg1_type, ' | ', chg2_type, '\n',
                   '\t', fast_move, magenta(' | '), chg_move1, ' | ', chg_move2))
        cat(paste0(yellow('\nDef: '), def, '\n'))
        if(length(member$DefendingTypes) == 3){
            cat(paste0('\t', def_type1, cyan(' | '), def_type2, ' | ', def_type3, '\n',
                       '\t', def1, cyan(' | '), def2, ' | ', def3))
        } else {
            cat(paste0('\t', def_type1, cyan(' | '), def_type2, '\n',
                       '\t', def1, cyan(' | '), def2))
        }
        cat('\n\n')
    }
}

## allow autocomplete of pokemon names

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
    rank <- which(all_types$Name == name)
    cat(paste0(magenta(toupper(name)), ': rank ', rank, '\n'))
    rank_team2(type_lookup_def(name), type_lookup_atk(name))
}
