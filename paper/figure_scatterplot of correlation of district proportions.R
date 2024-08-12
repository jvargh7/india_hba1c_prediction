rm(list=ls());gc();source(".Rprofile")

nfhs5_district_estimates <- read_csv("analysis/ihpan02_nfhs5 district estimates.csv") %>% 
  mutate(dm_control_hba1c = dm_control_hba1c*100,
         dm_control_hba1c_lt7 = dm_control_hba1c_lt7*100,
         dm_control_rbg = dm_control_rbg*100) %>% 
  mutate(n_ge30 = case_when(n >= 30 ~ ">=30 observations",
                            TRUE ~ "<30 observations"))

library(ggpubr)

figA = nfhs5_district_estimates %>% 
  ggplot(data=.,aes(x=dm_control_rbg,y = dm_control_hba1c_lt7)) +
  geom_point(aes(col = n_ge30)) +
  geom_smooth(method="lm",col="black",fill="pink") +
  stat_cor(method="spearman",cor.coef.name = "rho",p.digits = 3) +
  xlab("Random glucose < 180 mg/dL") +
  ylab("Predicted HbA1c < 7.0%") +
  theme_bw() +
  scale_color_manual(name="",values = c("grey80","darkblue")) +
  theme (legend.position = "bottom")

figA %>% 
  ggsave(.,filename=paste0(path_india_hba1c_box_folder,"/figures/scatterplot of correlation of district proportions.jpg"),width=6,height = 5)
