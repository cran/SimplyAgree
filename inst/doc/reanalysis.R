## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  eval = TRUE,
  comment = "#>"
)

## ----setup, message=FALSE,warning=FALSE---------------------------------------
library(SimplyAgree)
#library(tidyverse)
library(dplyr)
library(tidyr)
library(ggplot2)
library(magrittr)
data("temps")
df_temps = temps

## ----fig.cap="Example of the Line of Identity"--------------------------------
qplot(1,1) + geom_abline(intercept = 0, slope = 1)

## ----pltsrec,fig.cap="Concordance Plots of Rectal Temperature",echo=FALSE,fig.width=8.5,fig.height=5----
df_rec.delta = df_temps %>%
  mutate(id_spec = paste0(id,"_",trial_condition)) %>%
  select(id,id_spec,trec_delta,tod,trial_condition) %>%
  pivot_wider(id_cols = c(id,id_spec,trial_condition),
              names_from = tod,
              values_from = trec_delta) %>%
  mutate(diff = PM - AM,
         Average = (AM + PM)/2)

df_rec.post = df_temps %>%
  mutate(id_spec = paste0(id,"_",trial_condition)) %>%
  select(id,id_spec,trec_post,tod,trial_condition) %>%
  pivot_wider(id_cols = c(id,id_spec,trial_condition),
              names_from = tod,
              values_from = trec_post) %>%
  mutate(diff = PM - AM,
         Average = (AM + PM)/2)

p_rec.delta <- ggplot(df_rec.delta, aes(x=AM, y=PM)) +
  stat_smooth(aes(color=trial_condition), 
              method = "lm",
              formula = 'y ~ x',
              fullrange = TRUE,
              geom='line', alpha=0.5, se=FALSE)+
  scale_x_continuous("AM - Trec (delta)",
                     limits = c(.15,1.1))+
  scale_y_continuous("PM - Trec (delta)",
                     limits = c(.15,1.1))+
  geom_abline(intercept = 0, slope = 1, size = .85,
              alpha = .75) +
  geom_point(aes(color=trial_condition),
             alpha = .75) +
  theme_classic() +
  theme(legend.position = "bottom") +
  scale_color_viridis_d()

p_rec.post <- ggplot(df_rec.post, aes(x=AM, y=PM)) +
  stat_smooth(aes(color=trial_condition), 
              method = "lm",
              formula = 'y ~ x',
              fullrange = TRUE,
              geom='line', alpha=0.75, se=FALSE)+
  scale_x_continuous("AM - Trec (post)",
                     limits = c(36.4,38.5))+
  scale_y_continuous("PM - Trec (post)",
                     limits = c(36.4,38.5))+
  geom_abline(intercept = 0, slope = 1, size = .85,
              alpha = .75) +
  geom_point(aes(color=trial_condition),
             alpha = .75) +
  theme_classic() +
  theme(legend.position = "bottom") +
  scale_color_viridis_d()
p_rec.post
p_rec.delta

## ----pltseso,fig.cap="Concordance Plots of Esophageal Temperature",echo=FALSE,fig.width=8.5,fig.height=5----
df_eso.delta = df_temps %>%
  mutate(id_spec = paste0(id,"_",trial_condition)) %>%
  select(id,id_spec,teso_delta,tod,trial_condition) %>%
  pivot_wider(id_cols = c(id,id_spec,trial_condition),
              names_from = tod,
              values_from = teso_delta) %>%
  mutate(diff = PM - AM,
         Average = (AM + PM)/2)

df_eso.post = df_temps %>%
  mutate(id_spec = paste0(id,"_",trial_condition)) %>%
  select(id,id_spec,teso_post,tod,trial_condition) %>%
  pivot_wider(id_cols = c(id,id_spec,trial_condition),
              names_from = tod,
              values_from = teso_post) %>%
  mutate(diff = PM - AM,
         Average = (AM + PM)/2)

p_eso.delta <- ggplot(df_eso.delta, aes(x=AM, y=PM)) +
  stat_smooth(aes(color=trial_condition), 
              method = "lm",
              formula = 'y ~ x',
              fullrange = TRUE,
              geom='line', alpha=0.5, se=FALSE)+
  scale_x_continuous("AM - Teso (delta)",
                     limits = c(.15,1.1))+
  scale_y_continuous("PM - Teso (delta)",
                     limits = c(.15,1.1))+
  geom_abline(intercept = 0, slope = 1, size = .85,
              alpha = .75) +
  geom_point(aes(color=trial_condition),
             alpha = .75) +
  theme_classic() +
  theme(legend.position = "bottom") +
  scale_color_viridis_d()

p_eso.post <- ggplot(df_eso.post, aes(x=AM, y=PM)) +
  stat_smooth(aes(color=trial_condition), 
              method = "lm",
              formula = 'y ~ x',
              fullrange = TRUE,
              geom='line', alpha=0.75, se=FALSE)+
  scale_x_continuous("AM - Teso (post)",
                     limits = c(36.4,38.5))+
  scale_y_continuous("PM - Teso (post)",
                     limits = c(36.4,38.5))+
  geom_abline(intercept = 0, slope = 1, size = .85,
              alpha = .75) +
  geom_point(aes(color=trial_condition),
             alpha = .75) +
  theme_classic() +
  theme(legend.position = "bottom") +
  scale_color_viridis_d()

p_eso.post

p_eso.delta

## ----recpost------------------------------------------------------------------
# note: more accurate tolerance limits are given by tol_method = "perc"
rec.post_tol = tolerance_limit(
  data = df_rec.post,
  x = "PM",
  y = "AM",
  id = "id",
  condition = "trial_condition"
)



## -----------------------------------------------------------------------------
print(rec.post_tol)

## ----fig.cap="Tolerance Limits for Trec Post Exercise",fig.width=7,fig.height=5----
plot(rec.post_tol)

## -----------------------------------------------------------------------------

rec.delta_tol = tolerance_limit(
  x = "PM",
  y = "AM",
  condition = "trial_condition",
  id = "id",
  data = df_rec.delta
)
  
rec.delta_tol

## ----fig.cap="Tolerance Limits for Delta Trec",fig.width=7,fig.height=5-------

# Plot Maximal Allowable Difference with delta argument
plot(rec.delta_tol,
     delta = .25)

## -----------------------------------------------------------------------------

eso.post_tol = tolerance_limit(
  x = "AM",
  y = "PM",
  condition = "trial_condition",
  id = "id",
  data = df_eso.post
)

eso.delta_tol = tolerance_limit(
  x = "AM",
  y = "PM",
  condition = "trial_condition",
  id = "id",
  data = df_eso.delta
)

## -----------------------------------------------------------------------------
eso.post_tol

## ----fig.cap="Limits of Agreement for Teso Post Exercise",fig.width=7,fig.height=5----
plot(eso.post_tol)

## -----------------------------------------------------------------------------
eso.delta_tol

## ----fig.cap="Limits of Agreement for Delta Teso",fig.width=7,fig.height=5----
plot(eso.delta_tol,
     delta = .25)

