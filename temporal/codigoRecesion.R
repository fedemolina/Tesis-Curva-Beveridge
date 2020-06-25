# economics is contained in ggplot2 package
load("./Ideas-Pruebas-Codigos/recess.RData")
ggplot(economics, aes(x = date, y = unemploy/pop)) +
    geom_rect(data = recess,
              aes(xmin = begin, xmax = end, ymin = -Inf, ymax = Inf),
              inherit.aes = FALSE, fill = "red", alpha = 0.2) +
    geom_line()
