dane %>%
  filter(Director == 'Woody Allen') %>%
  arrange(Release.Date)%>%
  head(1) %>%
select(Title, Release.Date)
