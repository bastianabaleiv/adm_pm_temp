
# Grid --------------------------------------------------------------------

order_list <- tibble("p" = seq(1, 2),
                   "d" = seq(0, 1),
                   "q" = seq(2, 2)) %>%
  cross() %>% 
  map(purrr:::lift(c))

season_list <- tibble("P" = seq(1, 2),
                    "D" = seq(0, 1),
                    "Q" = seq(1, 2),
                    "period" = 7)  %>%
  cross() %>%
  map(purrr::lift(c))

sarima_hp <- cross_df(order_list,
                      season_list)
