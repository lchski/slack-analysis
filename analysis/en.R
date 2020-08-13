en_channels <- msgs %>%
  select(conversation) %>%
  distinct() %>%
  filter(str_detect(conversation, "exposure|$en-|-en-")) %>%
  pull(conversation)

en_messages <- messages %>%
  filter(conversation %in% en_channels)

en_msgs <- msgs %>%
  filter(conversation %in% en_channels) %>%
  filter(is.na(subtype) | subtype == "thread_broadcast") %>%
  select(-type, -subtype)

en_msgs %>%
  arrange(conversation, root_msg_ts, datetime) %>%
  mutate(text = ifelse(is_thread_reply, paste("->", text), text)) %>%
  select(conversation, datetime, user_use_name, text) %>%
  write_csv("data/out/en-msgs.csv")


messages %>%
  mutate(files_count = map_int(files, length)) %>%
  select(conversation, ts, text, user_use_name, files, files_count) %>%
  unnest(c(files)) %>%
  count_group(mode)

messages %>%
  mutate(files_count = map_int(files, length)) %>%
  select(conversation, ts, text, user_use_name, files, files_count) %>%
  filter(files_count > 0) %>%
  mutate(
    attached_images = map(
      files,
      ~ (.) %>%
        as_tibble %>%
        filter(str_detect(mimetype, "image"))
    )
  )

messages %>%
  mutate(files_count = map_int(files, length)) %>%
  select(conversation, ts, text, user_use_name, files, files_count) %>%
  filter(files_count > 0) %>%
  mutate(
    attached_images = map(
      files,
      ~ filter(as_tibble(.), str_detect(mimetype, "image"))
    )
  )


messages %>%
  mutate(files_count = map_int(files, length)) %>%
  select(conversation, ts, text, user_use_name, files_count, files) %>%
  filter(files_count > 0) %>%
  slice(1:15) %>%
  # unnest(c(files), keep_empty = TRUE) %>%
  # nest(files = id:last_col()) %>%
  mutate(attached_images = map(
    files,
    ~ filter(., str_detect(mimetype, "image"))
  )) %>%
  mutate(image_urls = map_chr(
    attached_images,
    ~ filter(., str_detect(mimetype, "image")) %>% pull(., url_private) %>% paste0(collapse = "\n\n")
  )) %>% select(conversation, ts, image_urls)

en_messages %>%
  mutate(files_count = map_int(files, length)) %>%
  select(conversation, ts, text, user_use_name, files_count, files) %>%
  slice(1:15) %>%
  # unnest(c(files), keep_empty = TRUE) %>%
  # nest(files = id:last_col()) %>%
  mutate(image_urls = if_else(
    files_count > 0,
    map_chr(
      files,
      ~ filter(., str_detect(mimetype, "image")) %>%
        pull(url_private) %>%
        paste0(collapse = "\n\n")
    ),
    ""
  )) %>%
  select(conversation, ts, image_urls)

en_messages %>%
  slice(1:15) %>%
  mutate(files_count = map_int(files, length)) %>%
  select(conversation, ts, text, user_use_name, files_count, files) %>%
  unnest(c(files), keep_empty = TRUE) %>%
  mutate(image_urls = if_else(
    is.na(url_private),
    "",
    paste0(url_private, collapse = "\n\n")
  ))

collapse_strings_with_newlines <- function(x) paste0(x, collapse = "\n\n")

en_messages %>%
  select(conversation, ts, text, user_use_name, files) %>%
  hoist(files, mimetype = "mimetype", url_private = "url_private") %>%
  unnest(c(mimetype, url_private), keep_empty = TRUE) %>%
  filter(is.na(mimetype) | str_detect(mimetype, "image")) %>%
  select(-mimetype) %>%
  nest(url_private = c(url_private))
  mutate(image_urls = map_chr(url_private, collapse_strings_with_newlines)) %>%
  select(-url_private, -files)




filter(., str_detect(mimetype, "image"))
  

  
  mutate(url = map(files, ~ (.) %>% pull(url_private)))
  
  


  slice(1)
