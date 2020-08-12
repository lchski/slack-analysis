library(tidyverse)
library(jsonlite)
library(janitor)
library(lubridate)

library(helpers)

users <- read_json("data/source/export/users.json", simplifyVector = TRUE, flatten = TRUE) %>%
  as_tibble %>%
  clean_names %>%
  nest(profile = starts_with("profile_"))

conversation_file_prefix <- "data/source/export/"

load_message_file <- function(file_path_to_load) {
  read_json(file_path_to_load, simplifyDataFrame = TRUE, flatten = TRUE) %>%
    as_tibble %>%
    clean_names %>%
    nest(
      user_profile = starts_with("user_profile_"),
      root = starts_with("root_"),
      message = starts_with("message_"),
      original = starts_with("original_"),
      bot = starts_with("bot_"),
      profile = starts_with("profile_")
    )
}

user_ids_real_names <- users %>%
  select(id, name, real_name) %>%
  mutate(use_name = ifelse(is.na(real_name), name, real_name)) %>%
  select(id, use_name) %>%
  mutate(id = paste0("<@", id, ">")) %>%
  mutate(use_name = paste0("<@", use_name, ">")) %>%
  deframe()

messages <- fs::dir_ls(conversation_file_prefix, glob = "*.json", recurse = TRUE, type = "file") %>%
  .[! . %in% paste0(conversation_file_prefix, c("users.json", "channels.json", "integration_logs.json"))] %>% ## filter out the top-level non-message files
  map_dfr(load_message_file, .id = "source_file_path") %>%
  mutate(conversation = str_remove(source_file_path, conversation_file_prefix)) %>%
  separate(conversation, into = c("conversation", "conversation_date"), "/") %>%
  mutate(conversation_date = as_date(str_remove(conversation_date, fixed(".json")))) %>%
  select(source_file_path, conversation, conversation_date, everything()) %>%
  mutate(datetime = as_datetime(as.integer(ts))) %>%
  mutate(root_msg_ts = ifelse(is.na(thread_ts), ts, thread_ts)) %>%
  mutate(is_thread_start = ! is.na(reply_count)) %>%
  mutate(is_thread_reply = ! is.na(thread_ts) & ! is_thread_start) %>%
  mutate(text = str_replace_all(text, user_ids_real_names))

msgs <- messages %>%
  select(conversation, root_msg_ts, ts, datetime, type, subtype, user, text, is_thread_start, is_thread_reply, reply_count) %>%
  left_join(users %>% select(user = id, name, real_name))
