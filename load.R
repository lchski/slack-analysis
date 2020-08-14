library(tidyverse)
library(jsonlite)
library(janitor)
library(lubridate)

#devtools::install_github("lchski/r-helpers")
library(helpers)

users <- read_json("data/source/export/users.json", simplifyVector = TRUE, flatten = TRUE) %>%
  as_tibble %>%
  clean_names %>%
  nest(profile = starts_with("profile_")) %>%
  mutate(use_name = ifelse(is.na(real_name), name, real_name))

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
  mutate(text = str_replace_all(text, user_ids_real_names)) %>%
  left_join(user_use_names)

user_ids_real_names <- users %>%
  select(id, use_name) %>%
  mutate(id = paste0("<@", id, ">")) %>%
  mutate(use_name = paste0("<@", use_name, ">")) %>%
  deframe()

#user_use_names <-
  
users %>%
  select(user = id, user_name = name, user_real_name = real_name, user_use_name = use_name)

messages %>%
  select(user, user_profile) %>%
  unnest(c(user_profile)) %>%
  select(user, user_profile_real_name, user_profile_display_name, user_profile_name) %>%
  filter(! (is.na(user_profile_real_name) & is.na(user_profile_display_name) & is.na(user_profile_name))) %>%
  distinct()
  hoist(user_profile, user_profile_real_name = "user_profile_real_name", .remove = FALSE) %>%
  select(user, user_use_name, user_profile_real_name)

messages <- messages %>%
  mutate(text = str_replace_all(text, user_ids_real_names)) %>%
  left_join(user_use_names)



image_urls <- messages %>%
  select(conversation, ts, files) %>%
  unnest(c(files)) %>%
  select(conversation, ts, mimetype, url_private) %>%
  filter(str_detect(mimetype, "image")) %>%
  select(-mimetype) %>%
  mutate(url_private_html = str_glue('<img src="{url_private}">')) %>%
  group_by(conversation, ts) %>%
  nest(image_urls = c(url_private, url_private_html)) %>%
  mutate(
    image_urls_text = map_chr(
      image_urls,
      ~ (.) %>% pull(url_private) %>% paste0(collapse = "\n\n")
    ),
    image_urls_html = map_chr(
      image_urls,
      ~ (.) %>% pull(url_private_html) %>% paste0(collapse = "\n\n")
    )
  ) %>%
  select(-image_urls)

messages <- messages %>%
  left_join(image_urls)

msgs <- messages %>%
  select(
    conversation,
    root_msg_ts,
    ts,
    datetime,
    type,
    subtype,
    user,
    user_use_name,
    text,
    is_thread_start,
    is_thread_reply,
    reply_count,
    image_urls_text,
    image_urls_html
  )
  
