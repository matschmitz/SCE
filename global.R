if (!require("pacman")) install.packages("pacman")
pacman::p_load(shiny, ggplot2, data.table, scales)

# APA theme (for plots)
apatheme <- theme_bw() + theme(panel.grid.major = element_blank(),
                               panel.grid.minor = element_blank(),
                               panel.border     = element_blank(),
                               axis.line        = element_line(),
                               text             = element_text(size = 17, family = "serif"))

blue <- "#4472C4"
green <- "#548235"
red <- "#C55A11"
redblood <- "#C00000"
grey <- "#BEBEBE"

DT <- data.table(student = c("Jane", "Sam", "Tim", "Kate", "Claire"),
                 hours = c(2, 3, 1, 1, 4),
                 y = c(10, 14, 8, 9, 19))
DT[, student := factor(student, levels = student)]
DT[, id := 1:nrow(DT)]
