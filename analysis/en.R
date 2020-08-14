en_channels <- msgs %>%
  select(conversation) %>%
  distinct() %>%
  filter(str_detect(conversation, "exposure|$en-|-en-|exp")) %>%
  pull(conversation)

en_messages <- messages %>%
  filter(conversation %in% en_channels)

en_msgs <- msgs %>%
  filter(conversation %in% en_channels) %>%
  filter(is.na(subtype) | subtype == "thread_broadcast") %>%
  select(-type, -subtype)

en_msgs %>%
  arrange(conversation, root_msg_ts, datetime) %>%
  #mutate(text = ifelse(is_thread_reply, paste("->", text), text)) %>%
  mutate(text_with_images = if_else(
    is.na(image_urls_html),
    text,
    paste0(text, "\n\n", image_urls_html)
  )) %>%
  select(
    conversation,
    datetime,
    user_use_name,
    text,
    text_with_images,
    image_urls_text,
    image_urls_html,
    is_thread_start,
    is_thread_reply, 
    root_msg_ts
  ) %>%
  write_csv("data/out/en-msgs.csv")
