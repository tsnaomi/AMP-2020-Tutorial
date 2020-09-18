# (35)  Load helpful R packages
library(languageR)	# Baayen and Shafaei-Bajestan 2019
library(lme4)		    # Bates et al. 2015
library(lmerTest)	  # Kuznetsova et al. 2017
library(MASS)		    # Venables and Ripley 2003
library(ggplot2)	  # Wickham 2016
library(mgcv)		    # Wood 2017

# (38)  Read in the inaugurals data frame, which contains the inaugural
#       address data for Bush Jr. (2001) and Obama (2009)
pres2 <- read.csv("inaugurals.csv", header = T)

# (40)  How well do the SPE stress rules (NSR + CSR) work?
ggplot(pres2, aes(x=norm_mean, y=norm_perc)) +
  geom_smooth() +
  facet_wrap(~president)

# (43)  Does perceived stress depend on the word’s linear position in the
#       sentence?
ggplot(pres2, aes(x=norm_widx, y=norm_perc)) +
  geom_smooth() +
  facet_wrap(~president)

# (45)  Does a word’s lexical frequency matter to the perception of stress?
ggplot(pres2, aes(x=log(corpus.freq), y=norm_perc)) + 
  geom_smooth() +
  facet_wrap(~president)

# (47)  Do more informative words receive higher stress?
ggplot(pres2, aes(x=c.inform.2, y=norm_perc)) + 
  geom_smooth() +
  facet_wrap(~president)

# (51)  What if we split the data by part of speech (instead of by president)?

# Let's first exclude words that don't qualify as adjectives, nouns, verbs, or
# function words, then sort the remaining categories in that order:
pres2.filtered <- subset(pres2, !is.na(category))
pres2.filtered$category <- factor(
  pres2.filtered$category,
  levels = c("ADJ", "NOUN", "VERB", "FUNC")
  )

# Now we'll do the plot from (51)!
ggplot(pres2.filtered, aes(x=c.inform.2, y=norm_perc)) +
  geom_smooth(method = "lm") +
  facet_wrap(~category)

# (55)  What about Mechanical stress vs. bigram informativity?
ggplot(pres2, aes(x=norm_mean, y=c.inform.2)) +
  geom_smooth() +
  facet_wrap(~president)

# (57)  Part-of-speech effects violin plot:
ggplot(pres2.filtered, aes(x=category, y=norm_perc)) +
  geom_violin() +
  stat_summary(fun=median, geom="point", size=2, color="red")

# (60)  Linear regression model:
presidents.fit = lm(
  perc ~ norm_mean + log(corpus.freq) + log(nseg) + norm_widx + category,
  data = pres2
  )

summary(presidents.fit)

# (61)  Mixed-effects regression model:
presidents.lmer = lmer(
  perc ~ norm_mean + log(corpus.freq) + log(nseg) + norm_widx + category +
    (1|president) + (1|annotator),
  data = pres2
  )

summary(presidents.lmer)
