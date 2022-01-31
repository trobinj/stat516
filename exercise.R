library(abd)     # for the data
library(ggplot2) # for plotting
library(cowplot) # for use of the plot_grid function
library(trtools) # trtools package (best package ever)

options(digits = 3)

# boxplot
p1 <- ggplot(MoleRats, aes(x = caste, y = ln.energy)) + 
  geom_boxplot() + geom_point() + theme_classic() + 
  labs(x = "Caste", y = "Log Energy Expenditure (log kJ/Day)")

# scatterplot
p2 <- ggplot(MoleRats, aes(x = ln.mass, y = ln.energy, color = caste)) +
  geom_point() + theme_classic() + theme(legend.position = c(0.9, 0.2)) +
  labs(x = "Log Mass (log g)", y = NULL, color = "Caste")

# plot both plots side-by-side
plot_grid(p1, p2, align = "h", rel_widths = c(1,3))

# 1. Model.

m <- lm(ln.energy ~ caste, data = MoleRats)
cbind(summary(m)$coefficients, confint(m))

# 2. 

lincon(m, a = c(1,0)) # 1 x beta0 + 0 x beta1 = beta0
contrast(m, a = list(caste = "lazy"))

lincon(m, a = c(1,1)) # 1 x beta0 + 1 x beta1 = beta0 + beta1
contrast(m, a = list(caste = "worker"))

contrast(m, a = list(caste = c("lazy","worker")),
 cnames = c("lazy","worker"))

lincon(m, a = c(0,1)) # beta1
contrast(m,
  a = list(caste = "worker"),
  b = list(caste = "lazy"),
  cnames = "worker-lazy")

# 3. w/mass

m <- lm(ln.energy ~ caste + ln.mass, data = MoleRats)
summary(m)$coefficients

# 4

d <- expand.grid(caste = c("lazy","worker"),
  ln.mass = c(3.85, 5.26))
d$yhat <- predict(m, newdata = d)

p2 <- p2 + geom_line(aes(y = yhat), d) + ylab("Hi!")
plot(p2)

# 5

contrast(m, a = list(caste = c("lazy","worker"), ln.mass = 4.5),
  cnames = c("lazy","worker"))

contrast(m, 
  a = list(caste = "worker", ln.mass = c(4,4.5,5)),
  b = list(caste = "lazy", ln.mass = c(4,4.5,5)),
  cnames = c(4,4.5,5))

#6 

summary(m)$coefficients
contrast(m,
  a = list(caste = c("lazy","worker"), ln.mass = 5),
  b = list(caste = c("lazy","worker"), ln.mass = 4),
  cnames = c("lazy slope","worker slope"))

#7 

cbind(summary(m)$coefficients, confint(m))
m.null <- lm(ln.energy ~ caste, data = MoleRats)

d$yhat <- predict(m.null, newdata = d)
p2 <- p2 + geom_line(aes(y = yhat), data = d, linetype= 2)
plot(p2)

anova(m.null, m)
summary(m)$coefficients






