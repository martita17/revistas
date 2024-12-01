errores_join <- errores_join %>% 
  rename(error = "...1") %>% 
  mutate(urls_caidos = str_extract(error, "http://.*?lang=es"))


errores_join <- errores_join %>% 
  mutate(fecha_scraping = as_date("2024-11-18"))


errores_join <- errores_join %>% 
  left_join(urls_arg, by = join_by(urls_caidos == url))
