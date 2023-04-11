vecprnt <- function(x, conjunction = "and") {
  if (is.na(conjunction)) {
    y <- paste(x, collapse = ", ")
  } else {
    n <- length(x)
    y <- paste(paste(x[-n], collapse = ", "), ", ", conjunction, " ", x[n], sep = "")    
  }
  return(y)
}

tex <- function(x, html = knitr::opts_chunk$get("dev") %in% c("png","pdf")) {
  ifelse(html, latex2exp::TeX(x), x)
}

rnd <- function(x, d = 0, word = "approximately") {
  if (round(x, d) != x) {
    return(paste(word, format(round(x,d), scientific = FALSE)))
  } else {
    return(format(round(x,d), scientific = FALSE))
  }
}

ktbl <- function(d, caption = NULL, align = "c") {
  d %>% kable(format = ifelse(knitr::is_html_output(), "html", "latex"), 
    booktabs = TRUE, linesep = "", align = align, escape = FALSE, caption = caption) %>%
    kable_styling(full_width = FALSE, bootstrap_options = "hover", latex_options = "hold_position")
}

headtail <- function(d, n) {
  for (i in 1:ncol(d)) {
    d[,i] <- as.character(d[,i])
  }
  d1 <- d[1:n,]
  d2 <- rep("$\\vdots$", ncol(d))
  d3 <- d[nrow(d),]
  y <- rbind(d1, d2, d3)
  rownames(y) <- NULL
  return(y)
}

# Simulate a vector of realizations from an arbitrary truncated distribution.
rtrunc <- function(f, n, a, b, ...) {
  y <- rep(NA,n)
  for (i in 1:n) {
    repeat{
      y[i] <- f(n = 1, ...)
      if (a <= y[i] & y[i] <= b) {
        break
      }
    }
  }
  return(y)
}

# Enumerate the sample space and the value of a statistic for a simple random sampling design.
srs <- function(x, n, stat = mean, labels = 1:length(x), replace = FALSE) {
  if (replace) {
    y <- apply(expand.grid(rep(list(x), n)), 1, stat)
    l <- apply(expand.grid(rep(list(labels), n)), 1, function(z) paste(z, collapse = ", ")) 
    return(data.frame(sample = l, stat = y))
  } else {
    y <- apply(combn(x, n), 2, stat)
    l <- apply(combn(labels, n), 2, function(z) paste(z, collapse = ", "))
    return(data.frame(sample = l, stat = y))
  }
}

## ggplot2 theme settings

noyaxis <- theme(axis.title.y = element_blank(), axis.text.y = element_blank(), 
  axis.ticks.y = element_blank(), axis.line.y = element_blank())

noxaxis <- theme(axis.title.x = element_blank(), axis.text.x = element_blank(), 
  axis.ticks.x = element_blank(), axis.line.x = element_blank())

nogrid <- theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())


