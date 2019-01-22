library(tidyverse)
library(sf)
data(alldat)
source('R/funcs.R')
# dups <- alldat[duplicated(alldat$MasterID),]

dupstat <- '801WE1132'
st_geometry(alldat) <- NULL

toplo1 <- alldat %>%
  filter(MasterID %in% dupstat) %>%
  select(date, CSCI, ASCI, PCT_SAFN, H_AqHab, H_SubNat, Ev_FlowHab, XCMG, indexscore_cram, TN2, TP, Cond) %>%
  rename(
    CRAM = indexscore_cram,
    TN = TN2
  ) %>%
  gather('var', 'val', -date) %>%
  mutate(
    cls = ifelse(
      var %in% c('CSCI', 'ASCI'), 'Biology',
      ifelse(var %in% c('TN', 'TP', 'Cond'), 'Chemistry', 'Habitat')
      )
  ) %>%
  group_by(var) %>%
  mutate(
    valscl = scales::rescale(val, to = c(0, 1))
  )


ggplot(toplo1, aes(x = factor(date), y = val, group = 1)) +
  geom_point(aes(fill = valscl), pch = 22, size = 52, alpha = 0.6) +
  geom_point(size = 5) +
  stat_summary(fun.y= sum, geom='line') +
  scale_fill_gradientn(colours = rev(getdsccol(palout = T))) +
  facet_wrap(cls~var, ncol = 1, scales = 'free_y', strip.position = 'left') +
  # scale_x_discrete(expand = c(0, 0)) +
  theme_minimal() +
  theme(
    strip.placement = 'outside',
    axis.title.y = element_blank(),
    axis.title.x = element_blank(),
    strip.background = element_blank(),
    legend.position = 'none'#,
    # panel.grid = element_blank()
  )
