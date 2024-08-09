library(tidyverse)
library(here)

source(here("code/helper.R"))

input <- read_csv("https://raw.githubusercontent.com/vboyce/multiparty-tangrams/main/data/study1/filtered_chat.csv")

location <- "intermediates/dialign_inputs"

do_write <- function(gameId, tangram, data) {
  loc <- str_c(gameId, "_", tangram, ".tsv")
  unnest(data) |> write_tsv(here(location, loc), col_names = F)
}


selected <- input |>
  filter(numPlayers == 4) |> 
  inner_join(complete_only(input)) |>
  filter(!is.chitchat) |>
  mutate(tangram = str_sub(target, -5, -5)) |>
  select(gameId, role, spellchecked, tangram) |>
  rowwise() |>
  mutate(parse = do_parse(spellchecked)) |>
  select(gameId, tangram, role, parse) |>
  group_by(gameId, tangram) |>
  mutate(role = ifelse(row_number() %% 2 == 1, "A", "B")) |>
  nest(data = c(role, parse))

selected |> pwalk(do_write)

