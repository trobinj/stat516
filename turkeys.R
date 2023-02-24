turkeys <- data.frame(
  weight = c(674,764,795,796,826,782,834,836,830),
  pens = c(10,5,2,2,5,5,2,2,5),
  source = c(rep("a",5), rep("b",4)),
  dose = c(0,rep(c(0.12,0.22,0.32,0.44), 2))
)

library(dplyr)

m <- nls(weight ~ case_when(
  source == "a" ~ t1 + (t2 - t1)*2^(-dose/t3a),
  source == "b" ~ t1 + (t2 - t1)*2^(-dose/t3b)),
  start = list(t1 = 882, t2 = 698, t3a = 0.2, t3b = 0.2),
  data = turkeys, weights = pens)
summary(m)$coefficients

turkeys$source <- NULL
turkeys$dose <- NULL

turkeys$dosea <- c(0, c(0.12,0.22,0.32,0.44), rep(0, 4))
turkeys$doseb <- c(rep(0, 5), c(0.12,0.22,0.32,0.44))

turkeys <- turkeys %>% 
  mutate(dose = dosea + doseb) %>%
  mutate(source = case_when(
    dose == 0 ~ "none",
    dosea > 0 ~ "a",
    doseb > 0 ~ "b")
  )

m <- nls(weight ~ t1 + (t2 - t1)*2^(-(dosea/t3a + doseb/t3b)),
  data = turkeys, start = list(t1 = 882, t2 = 698, t3a = 0.2, t3b = 0.2))
summary(m)$coefficients
trtools::lincon(m, c(0,0,1,-1))

m <- nls(weight ~ t1 + (t2 - t1)*2^(-(dosea/t3a + doseb/t3b)), weights = pens,
  data = turkeys, start = list(t1 = 882, t2 = 698, t3a = 0.2, t3b = 0.2))
summary(m)$coefficients
trtools::lincon(m, c(0,0,1,-1))

