# To create tufte style graphs 
# browse to the link below to seemore examples
# http://motioninsocial.com/tufte/

# to plot these examples you will need to make sure these packges are installed.
list.of.packages <- c("CarletonStats", "devtools", "epanetReader", "fmsb", "ggplot2", "ggthemes", 
                      "latticeExtra", "MASS", "PerformanceAnalytics", "psych", 
                      "plyr", "prettyR", "plotrix", "proto", "RCurl", "reshape", "reshape2")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)


library(ggplot2)
library(ggthemes)
library(devtools)
library(RCurl)
library(plyr)
source_url("https://raw.githubusercontent.com/jkeirstead/r-slopegraph/master/slopegraph.r")
d <- read.csv(text = getURL("https://raw.githubusercontent.com/jkeirstead/r-slopegraph/master/cancer_survival_rates.csv"))
df <- build_slopegraph(d, x="year", y="value", group="group", method="tufte", min.space=0.04)
df <- transform(df, x=factor(x, levels=c(5,10,15,20), 
                             labels=c("5 years","10 years","15 years","20 years")), y=round(y))
plot_slopegraph(df) + labs(title="Estimates of % survival rates") +
  theme_tufte(base_size=16, ticks=F) + theme(axis.title=element_blank())


# this will create a pdf that contains the plot, it will be saved in your work directory
getwd() # shows your current work directory
library(ggplot2)
library(ggthemes)
library(dplyr)
library(reshape)
library(RCurl)
dd <- read.csv(text = getURL("https://gist.githubusercontent.com/GeekOnAcid/da022affd36310c96cd4/raw/9c2ac2b033979fcf14a8d9b2e3e390a4bcc6f0e3/us_nr_of_crimes_1960_2014.csv"))
d <- melt(dd, id="Year")
names(d) <- c("Year","Crime.Type","Crime.Rate")
d$Crime.Rate <- round(d$Crime.Rate,0)
mins <- group_by(d, Crime.Type) %>% slice(which.min(Crime.Rate))
maxs <- group_by(d, Crime.Type) %>% slice(which.max(Crime.Rate))
ends <- group_by(d, Crime.Type) %>% filter(Year == max(Year))
quarts <- d %>% group_by(Crime.Type) %>%
  summarize(quart1 = quantile(Crime.Rate, 0.25),
            quart2 = quantile(Crime.Rate, 0.75)) %>%
  right_join(d)
pdf("sparklines_ggplot.pdf", height=10, width=8)
ggplot(d, aes(x=Year, y=Crime.Rate)) + 
  facet_grid(Crime.Type ~ ., scales = "free_y") + 
  geom_ribbon(data = quarts, aes(ymin = quart1, max = quart2), fill = 'grey90') +
  geom_line(size=0.3) +
  geom_point(data = mins, col = 'red') +
  geom_point(data = maxs, col = 'blue') +
  geom_text(data = mins, aes(label = Crime.Rate), vjust = -1) +
  geom_text(data = maxs, aes(label = Crime.Rate), vjust = 2.5) +
  geom_text(data = ends, aes(label = Crime.Rate), hjust = 0, nudge_x = 1) +
  geom_text(data = ends, aes(label = Crime.Type), hjust = 0, nudge_x = 5) +
  expand_limits(x = max(d$Year) + (0.25 * (max(d$Year) - min(d$Year)))) +
  scale_x_continuous(breaks = seq(1960, 2010, 10)) +
  scale_y_continuous(expand = c(0.1, 0)) +
  theme_tufte(base_size = 15, base_family = "Helvetica") +
  theme(axis.title=element_blank(), axis.text.y = element_blank(), 
        axis.ticks = element_blank(), strip.text = element_blank())
dev.off()