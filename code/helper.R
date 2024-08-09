library(spacyr)

spacy_initialize(model = "en_core_web_sm")

stop <- readLines(here("code/stopwords.txt")) |>
  str_c(collapse = " ", sep = "") |>
  str_split("\\s+", simplify=T) 

funct <- c("DET", "PRON", "ADP", "CCONJ", "SCONJ", "AUX", "PART", "PUNCT", "SYM", "X", "INTJ", "SPACE", "NUM")

do_parse <- function(text) {
  str_replace_all(text, regex("\\W+"), " ") |> str_squish() |> str_to_lower() |> spacy_parse() |> 
    filter(!lemma %in% stop) |> 
    pull(lemma) |>
    str_c(collapse = " ") |> str_replace_all("leave", "left") |> str_replace_all("kneeling", "kneel")
}

post_process <- function(phrase) {
  left <- spacy_parse(phrase) |>
    filter(!pos %in% funct) 
  return(nrow(left))
}

complete_only <- function(df){
  input |>
    filter(numPlayers == 4) |>
    filter(!is.na(text)) |>
    select(gameId, trialNum) |>
    unique() |>
    group_by(gameId) |>
    tally() |>
    filter(n == 72) |> 
    select(gameId)
}
