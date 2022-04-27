library(dplyr)
library(VGAM)

set.seed(123)
d <- expand.grid(age = rep(20:80, each = 50)) %>%
  mutate(vaccine = ifelse(runif(n()) < plogis(2*(age - 50)/18), "yes", "no")) %>%
  mutate(p1 = plogis(-1 + ifelse(vaccine == "yes", 0, 1))) %>% 
  mutate(p2 = plogis(-2 + (age - 50)/18 + ifelse(vaccine == "yes", 0, 1))) %>%
  mutate(u = runif(n())) %>%
  mutate(status = case_when(
    u < 1 - p1 ~ "uninfected",
    u < 1 - p1 + p1 * (1 - p2) ~ "infected",
    TRUE ~ "hospitalized"
  )) %>% group_by(age, vaccine, status) %>%
  summarize(count = n()) %>%
  pivot_wider(names_from = status, values_from = count, values_fill = 0) %>%
  select(age, vaccine, uninfected, infected, hospitalized)

m <- vglm(cbind(uninfected,infected,hospitalized) ~ vaccine,
          family = cratio(link = "logitlink"), data = d)


m <- vglm(cbind(uninfected,infected,hospitalized) ~ vaccine + age,
  family = cratio(link = "logitlink"), data = d)
