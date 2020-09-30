# Description --------------------------------------------------------------------------------------
# Sampling distribution: Chapter 3

# Setup --------------------------------------------------------------------------------------------
library(data.table)
library(ggplot2)


# Parameters ---------------------------------------------------------------------------------------
set.seed(19)                        # for reproductibility
eDist <- round(rnorm(100, 0, 3), 0) # errors distribution (urns)
trueYmean <- 12                     # beta 0, the true mean of Y
n1 <- 20                            # first sample size of values
n2 <- 60                            # second sample size of values
boots <- 10000                      # number of iterations


# Distributions ------------------------------------------------------------------------------------
# For n1
D1 <- data.table(iteration = rep(1:boots, each = n1))
D1[, edraw := sample(eDist, n1), iteration]
D1[, ye := trueYmean + edraw]

SampD1 <- D1[, .(M = round(mean(ye), 2), Med = round(median(ye), 2)), iteration]
SampDL1 <- data.table(tidyr::gather(SampD1, statistic, value, M:Med))
SampDL1[, statistic := ifelse(statistic == "M", "Mean", "Median")]

# For n2
D2 <- data.table(iteration = rep(1:boots, each = n2))
D2[, edraw := sample(eDist, n2), iteration]
D2[, ye := trueYmean + edraw]

SampD2 <- D2[, .(M = round(mean(ye), 2), Med = round(median(ye), 2)), iteration]
SampDL2 <- data.table(tidyr::gather(SampD2, statistic, value, M:Med))
SampDL2[, statistic := ifelse(statistic == "M", "Mean", "Median")]

# Bind the two tables
SampDL <- rbindlist(list(SampDL1, SampDL2))
SampDL[, n := rep(c(paste0("n = ", n1), paste0("n = ", n2)), each = nrow(SampDL)/2)]


# Plot ---------------------------------------------------------------------------------------------
# APA theme (for plots)
apatheme <- theme_bw() + theme(panel.grid.major = element_blank(),
                               panel.grid.minor = element_blank(),
                               panel.border     = element_blank(),
                               axis.line        = element_line(),
                               text             = element_text(size = 17, family = "serif"))

gg <- ggplot(SampDL, aes(value, y = ..density.., color = statistic, fill = statistic)) +
  geom_density(alpha = .1, adjust = 10) +
  scale_color_manual(labels = c("Moyenne", "Médiane"), values = c("steelblue4", "firebrick4")) +
  scale_fill_manual(labels = c("Moyenne", "Médiane"), values = c("steelblue4", "firebrick4")) +
  scale_x_continuous(limits = c(trueYmean -5, trueYmean + 5), breaks = 0:20) +
  geom_vline(xintercept = trueYmean, linetype = "dashed", color = "grey30") +
  facet_wrap(. ~ n) +
  apatheme +
  labs(x = "note estimée",
       y = "densité (%)",
       fill = "statistique",
       color = "statistique")

plot(gg)
