library(tidyverse)
library(here)

source(here("code/helper.R"))

out_location <- "intermediates/dialign_outputs"

input <- read_csv("https://raw.githubusercontent.com/vboyce/multiparty-tangrams/main/data/study1/filtered_chat.csv")

do_combine <- function(gameId, tangram) {
  a <- read_tsv(here(out_location, str_c(gameId, "_", tangram, "_tsv-lexicon.tsv")), show_col_types=F)
  b <- read_tsv(here(out_location, str_c(gameId, "_", tangram, "_tsv-lexicon-self-rep-A.tsv")), show_col_types=F)
  c <- read_tsv(here(out_location, str_c(gameId, "_", tangram, "_tsv-lexicon-self-rep-B.tsv")), show_col_types=F)
  a |>
    bind_rows(b) |>
    bind_rows(c) |>
    select(Words = `Surface Form`) |>
    unique()
}

do_item <- function(df, phrase) {
  df |>
    filter(str_detect(parse, phrase))
}

said <- input |>
  filter(numPlayers == 4) |>
  inner_join(complete_only(input)) |>
  filter(!is.chitchat) |>
  mutate(tangram = str_sub(target, -5, -5)) |>
  select(gameId, tangram, trialNum, repNum, playerId, role, spellchecked) |>
  rowwise() |>
  mutate(parse = do_parse(spellchecked)) |>
  group_by(gameId, tangram) |>
  nest()


extracted <- input |>
  filter(numPlayers == 4) |>
  select(gameId, target) |>
  unique() |>
  inner_join(complete_only(input)) |>
  mutate(tangram = str_sub(target, -5, -5)) |>
  select(gameId, tangram) |>
  mutate(words = pmap(list(gameId, tangram), do_combine)) |>
  unnest(words) |>
  rowwise() |>
  mutate(keep = post_process(Words)) |>
  filter(!is.na(keep)) |>
  filter(keep > 0) |>
  select(-keep) |>
  filter(!is.na(Words)) |>
  group_by(gameId, tangram) |>
  nest() |>
  rename(phrases = data) |>
  left_join(said) |>
  unnest(phrases) |>
  rowwise() |>
  mutate(source = list(do_item(data, Words))) |>
  select(-data) |>
  unnest(source) |>
  write_csv(here("processed/sourced_4p.csv"))
