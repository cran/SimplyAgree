data("temps")

df_temps = temps

df_rec.delta = df_temps %>%
  #select(id,tod,trial_condition,trec_delta) %>%
  mutate(id_spec = paste0(id,"_",trial_condition)) %>%
  select(id,id_spec,trec_delta,tod,trial_condition) %>%
  pivot_wider(id_cols = c(id,id_spec,trial_condition),
              names_from = tod,
              values_from = trec_delta) %>%
  mutate(diff = PM - AM,
         Average = (AM + PM)/2)

df_rec.post = df_temps %>%
  #select(id,tod,trial_condition,trec_post) %>%
  mutate(id_spec = paste0(id,"_",trial_condition)) %>%
  select(id,id_spec,trec_post,tod,trial_condition) %>%
  pivot_wider(id_cols = c(id,id_spec,trial_condition),
              names_from = tod,
              values_from = trec_post) %>%
  mutate(diff = PM - AM,
         Average = (AM + PM)/2)

df_eso.delta = df_temps %>%
  #select(id,tod,trial_condition,teso_delta) %>%
  mutate(id_spec = paste0(id,"_",trial_condition)) %>%
  select(id,id_spec,teso_delta,tod,trial_condition) %>%
  pivot_wider(id_cols = c(id,id_spec,trial_condition),
              names_from = tod,
              values_from = teso_delta) %>%
  mutate(diff = PM - AM,
         Average = (AM + PM)/2)

df_eso.post = df_temps %>%
  #select(id,tod,trial_condition,teso_post) %>%
  mutate(id_spec = paste0(id,"_",trial_condition)) %>%
  select(id,id_spec,teso_post,tod,trial_condition) %>%
  pivot_wider(id_cols = c(id,id_spec,trial_condition),
              names_from = tod,
              values_from = teso_post) %>%
  mutate(diff = PM - AM,
         Average = (AM + PM)/2)

# Rectal -----

rec.post_loa = SimplyAgree::loa_mixed(diff = "diff",
                                      condition = "trial_condition",
                                      id = "id",
                                      plot.xaxis = "Average",
                                      data = df_rec.post,
                                      conf.level = .95,
                                      agree.level = .95,
                                      replicates = 199,
                                      type = "bca")
write_rds(rec.post_loa,"vignettes/rec_post_loa.rds")


rec.delta_loa = SimplyAgree::loa_mixed(diff = "diff",
                                       condition = "trial_condition",
                                       id = "id",
                                       plot.xaxis = "Average",
                                       data = df_rec.delta,
                                       conf.level = .95,
                                       agree.level = .95,
                                       replicates = 199,
                                       type = "bca")

write_rds(rec.delta_loa,"vignettes/rec_delta_loa.rds")



# Esophageal --------

eso.post_loa = SimplyAgree::loa_mixed(diff = "diff",
                                      condition = "trial_condition",
                                      id = "id",
                                      plot.xaxis = "Average",
                                      data = df_eso.post,
                                      conf.level = .95,
                                      agree.level = .95,
                                      replicates = 199,
                                      type = "perc")
write_rds(eso.post_loa,"vignettes/eso_post_loa.rds")


eso.delta_loa = SimplyAgree::loa_mixed(diff = "diff",
                                       condition = "trial_condition",
                                       id = "id",
                                       plot.xaxis = "Average",
                                       data = df_eso.delta,
                                       conf.level = .95,
                                       agree.level = .95,
                                       replicates = 199,
                                       type = "bca")

write_rds(eso.delta_loa,"vignettes/eso_delta_loa.rds")


