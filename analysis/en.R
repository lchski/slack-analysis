en_channels <- msgs %>%
  select(conversation) %>%
  distinct() %>%
  filter(str_detect(conversation, "exposure|$en-|-en-")) %>%
  pull(conversation)

en_msgs <- msgs %>%
  filter(conversation %in% en_channels) %>%
  filter(is.na(subtype) | subtype == "thread_broadcast") %>%
  select(-type, -subtype)

en_msgs %>%
  arrange(conversation, root_msg_ts, datetime) %>%
  mutate(text = ifelse(is_thread_reply, paste("->", text), text)) %>%
  select(conversation, datetime, name, real_name, text) %>%
  View()
