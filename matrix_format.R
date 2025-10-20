library(readr)

types <- names(atk_chart)
type_matrix <- matrix("",
    nrow = length(types), ncol = length(types),
    dimnames = list(types, types),
)
for (atk in types) {
    for (def in atk_chart[atk]) {
        def_type <- names(def)
        def_value <- unname(def)
        type_matrix[atk, def_type] <- def_value
    }
}

type_matrix[type_matrix == "1.6"] <- "+"
type_matrix[type_matrix == "0.625"] <- "/"
type_matrix[type_matrix == "0.39"] <- "0"

type_matrix <- as.data.frame(type_matrix)
write.csv(type_matrix, "~/git/pokehax/type_chart.csv")
