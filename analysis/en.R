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



