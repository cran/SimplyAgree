library(SimplyAgree)
library(tidyverse)
data("temps")
df_temps = temps

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
# Analysis ----
rec.post_loa = SimplyAgree::loa_lme(diff = "diff",
                                    condition = "trial_condition",
                                    id = "id",
                                    avg = "Average",
                                    data = df_rec.post,
                                    conf.level = .95,
                                    agree.level = .95,
                                    replicates = 199,
                                    type = "perc")

rec.delta_loa = SimplyAgree::loa_lme(diff = "diff",
                                    condition = "trial_condition",
                                    id = "id",
                                    avg = "Average",
                                    data = df_rec.delta,
                                    conf.level = .95,
                                    agree.level = .95,
                                    replicates = 199,
                                    type = "perc")

eso.post_loa = SimplyAgree::loa_lme(diff = "diff",
                                    condition = "trial_condition",
                                    id = "id",
                                    avg = "Average",
                                    data = df_eso.post,
                                    conf.level = .95,
                                    agree.level = .95,
                                    replicates = 199,
                                    type = "perc")

eso.delta_loa = SimplyAgree::loa_lme(diff = "diff",
                                     condition = "trial_condition",
                                     id = "id",
                                     avg = "Average",
                                     data = df_eso.delta,
                                     conf.level = .95,
                                     agree.level = .95,
                                     replicates = 199,
                                     type = "perc")

rec.post_loa = readr::write_rds(rec.post_loa,"rec_post_loa.rds")
rec.delta_loa = readr::write_rds(rec.delta_loa,"rec_delta_loa.rds")

eso.post_loa = readr::write_rds(eso.post_loa,"eso_post_loa.rds")
eso.delta_loa = readr::write_rds(eso.delta_loa,"eso_delta_loa.rds")
