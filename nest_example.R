# fit a simple linear regression model for each country and extract the model coeficient for "year"
slopes <- countrypops %>% 
  nest(-country_name) %>% 
  mutate(model = map(data, ~broom::tidy(lm(population ~ year, data = .)))) %>% 
  unnest(model) %>% 
  filter(term == "year") 

slopes %>% 
  top_n(5, wt = estimate) %>% 
  arrange(desc(estimate)) %>% 
  select(-term, -statistic) %>% 
  gt() %>% 
  fmt_number(vars(estimate, std.error), decimals = 0) %>% 
  fmt_number(vars(p.value), decimals = 3) %>% 
  tab_header("Model estimated yearly population increases", subtitle = "1960 to 2017")
