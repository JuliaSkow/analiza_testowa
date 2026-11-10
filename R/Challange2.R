library(dplyr)

dane |>
  filter(Genre == 'Comedy' & Languages == 'Polish') |>
  arrange(desc(IMDb.Score)) |>
  select(Title, IMDb.Score) |>
  head(3)
