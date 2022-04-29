library(dplyr)
library(VGAM)

# set.seed(123)
# d <- expand.grid(age = rep(20:80, each = 50)) %>%
#   mutate(vaccine = ifelse(runif(n()) < plogis(2*(age - 50)/18), "yes", "no")) %>%
#   mutate(p1 = plogis(-1 + ifelse(vaccine == "yes", 0, 1))) %>% 
#   mutate(p2 = plogis(-2 + (age - 50)/18 + ifelse(vaccine == "yes", 0, 1))) %>%
#   mutate(u = runif(n())) %>%
#   mutate(status = case_when(
#     u < 1 - p1 ~ "uninfected",
#     u < 1 - p1 + p1 * (1 - p2) ~ "infected",
#     TRUE ~ "hospitalized"
#   )) %>% group_by(age, vaccine, status) %>%
#   summarize(count = n()) %>%
#   pivot_wider(names_from = status, values_from = count, values_fill = 0) %>%
#   select(age, vaccine, uninfected, infected, hospitalized)

set.seed(123)
d <- expand.grid(age = rep(c("<50","50+"), c(20000, 10000))) %>% 
  mutate(u = runif(n())) %>% 
  mutate(vaccine = case_when(
    age == "<50" ~ ifelse(u < 0.50, "y", "n"),
    age == "50+" ~ ifelse(u < 0.90, "y", "n")
  )) %>%
  mutate(p1 = ifelse(vaccine == "y", 0.05, 0.20)) %>%
  mutate(p2 = case_when(
    age == "<50" & vaccine == "y" ~ 0.01,
    age == "<50" & vaccine == "n" ~ 0.05,
    age == "50+" & vaccine == "y" ~ 0.02,
    age == "50+" & vaccine == "n" ~ 0.10
  )) %>%
  mutate(y1 = rbinom(n(), 1, p1)) %>%
  mutate(y2 = ifelse(y1 == 1, rbinom(n(), 1, p2), 0)) %>%
  mutate(status = case_when(
    y1 == 0 ~ "uninfected",
    y2 == 0 ~ "infected",
    TRUE ~ "hospitalized"
  )) %>%    
  group_by(age, vaccine, status) %>% summarize(f = n()) %>%
  pivot_wider(names_from = status, values_from = f, values_fill = 0) %>%
  select(age, vaccine, uninfected, infected, hospitalized)

d

options(digits = 3)


m <- vglm(cbind(uninfected,infected,hospitalized) ~ vaccine,
          family = cratio(link = "logitlink"), data = d)
trtools::lincon(m, tf = exp)

m <- vglm(cbind(uninfected,infected,hospitalized) ~ vaccine + age,
  family = cratio(link = "logitlink"), data = d)
trtools::lincon(m, tf = exp)