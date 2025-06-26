#------------------------------------#
### Bison Space-Use Analysis       ###
#------------------------------------#
## R. Ritson, 6/18/2025             ##
#------------------------------------#
### Statistical Tests  ###
#------------------------------------#
## Load libraries
require(rstatix)
require(dplyr)
library("XLConnect")
library("xlsx")

## Load Data
df <- data.table::fread("C:/Users/r2j2r/Documents/Research Projects/Bison_SpaceUse/BisonSpaceUse_sizes.csv")
df$Regime <- ifelse(df$Fence == 1,"Fenced","Free-Range")
df$Regime <- factor(df$Regime,levels = c("Free-Range","Fenced"))
df$Study_Area<- factor(df$Study_Area,levels=c("Henry Mountains","Book Cliffs","American Prairie Reserve","Medano-Zapata Ranch","Caprock Canyon"))

p_correction <- "BH" #benjamini-hochberg p-value correction (repeated tests)

## Summarize
df_summary <- df %>%
  dplyr::group_by(Study_Area,Season) %>%
  dplyr::summarise(HR_avg = mean(homerange,na.rm=T),
                   FPT_avg = mean(fpt,na.rm = T),
                   N_ID = length(unique(tag)),
                   N_IDyr = length(unique(ID2)))
df_summary

df_summary2 <- df %>%
  dplyr::group_by(Regime,Season) %>%
  dplyr::summarise(HR_avg = mean(homerange,na.rm=T),
                   FPT_avg = mean(fpt,na.rm = T),
                   N_ID = length(unique(tag)),
                   N_IDyr = length(unique(ID2)))
df_summary2



## Friedman Test ####
# seasons by regime 
table(df$tag,df$Season)

## Homerange
hr_stat <- df %>%
  group_by(tag) %>%
  filter(n_distinct(Season) == length(unique(df$Season))) %>%
  dplyr::group_by(tag,Season) %>%
  dplyr::summarise(homerange_avg = mean(homerange), tag= tag, Season = Season,Regime=Regime) %>%
  dplyr::distinct(.)

hr_stat_summary <- hr_stat %>%
  ungroup() %>%
  dplyr::group_by(Season) %>%
  dplyr::summarise(count = n())
hr_stat_summary

hr_ft_fence <- hr_stat %>%
  dplyr::filter(Regime == "Fenced") %>% #captive herds
  ungroup(.) %>%
  rstatix::friedman_test(homerange_avg ~ Season | tag)
hr_ft_fence #p-value not significant

hr_ft_free <- hr_stat %>%
  dplyr::filter(Regime == "Free-Range") %>% #free-range herds
  ungroup(.) %>%
  rstatix::friedman_test(homerange_avg ~ Season | tag)
hr_ft_free #p-value significant, proceed with posthoc

hr_w_free <- hr_stat %>%
  dplyr::filter(Regime == "Free-Range") %>% #free-range herds
  ungroup(.) %>%
  rstatix::pairwise_wilcox_test(homerange_avg ~ Season, p.adjust.method = p_correction,paired=T,detailed = T)
hr_w_free

hr_eff_free <- hr_stat %>%
  dplyr::filter(Regime == "Free-Range") %>%
  ungroup(.) %>%
  rstatix::friedman_effsize(homerange_avg ~ Season | tag, ci=T, conf.level = 0.95)
hr_eff_free

setwd("C:/Users/r2j2r/Documents/Research Projects/Bison_SpaceUse/")
write.xlsx2(hr_ft_fence,"friedman_test_results_06182025.xlsx", sheetName = "hr_fence")
write.xlsx2(hr_ft_free,"friedman_test_results_06182025.xlsx", sheetName = "hr_free",append=T)
write.xlsx2(hr_w_free,"friedman_test_results_06182025.xlsx", sheetName = "hr_free_posthoc",append=T)
write.xlsx2(hr_eff_free,"friedman_test_results_06182025.xlsx", sheetName = "hr_free_effect",append=T)

## FPT
fpt_stat <- df %>%
  group_by(tag) %>%
  filter(n_distinct(Season) == length(unique(df$Season))) %>%
  dplyr::group_by(tag,Season) %>%
  dplyr::summarise(fpt_avg = mean(fpt,na.rm=T), tag= tag, Season = Season,Regime=Regime) %>%
  dplyr::distinct(.) 

fpt_stat_summary <- fpt_stat %>%
  ungroup() %>%
  dplyr::group_by(Season) %>%
  dplyr::summarise(count = n())
fpt_stat_summary

fpt_ft_fence <- fpt_stat %>%
  dplyr::filter(Regime == "Fenced") %>% #captive herds
  ungroup(.) %>%
  rstatix::friedman_test(fpt_avg ~ Season | tag)
fpt_ft_fence #p-value significant, proceed with posthoc

fpt_w_fence <- fpt_stat %>%
  dplyr::filter(Regime == "Fenced") %>% #free-range herds
  ungroup(.) %>%
  rstatix::pairwise_wilcox_test(fpt_avg ~ Season, p.adjust.method = p_correction,paired=T)
fpt_w_fence

fpt_eff_fence <- fpt_stat %>%
  dplyr::filter(Regime == "Fenced") %>%
  ungroup(.) %>%
  rstatix::friedman_effsize(fpt_avg ~ Season | tag, ci=T, conf.level = 0.95)
fpt_eff_fence


fpt_ft_free <- fpt_stat %>%
  dplyr::filter(Regime == "Free-Range") %>% #free-range herds
  ungroup(.) %>%
  rstatix::friedman_test(fpt_avg ~ Season | tag)
fpt_ft_free #p-value significant, proceed with posthoc

fpt_w_free <- fpt_stat %>%
  dplyr::filter(Regime == "Free-Range") %>% #free-range herds
  ungroup(.) %>%
  rstatix::pairwise_wilcox_test(fpt_avg ~ Season, p.adjust.method = p_correction,paired=T)
fpt_w_free

fpt_eff_free <- fpt_stat %>%
  dplyr::filter(Regime == "Free-Range") %>%
  ungroup(.) %>%
  rstatix::friedman_effsize(fpt_avg ~ Season | tag, ci=T, conf.level = 0.95)
fpt_eff_free

write.xlsx2(fpt_ft_fence,"friedman_test_results_06182025.xlsx", sheetName = "fpt_fence",append = T)
write.xlsx2(fpt_w_fence,"friedman_test_results_06182025.xlsx", sheetName = "fpt_fence_posthoc",append=T)
write.xlsx2(fpt_eff_fence,"friedman_test_results_06182025.xlsx", sheetName = "fpt_fence_effect",append=T)
write.xlsx2(fpt_ft_free,"friedman_test_results_06182025.xlsx", sheetName = "fpt_free",append=T)
write.xlsx2(fpt_w_free,"friedman_test_results_06182025.xlsx", sheetName = "fpt_free_posthoc",append=T)
write.xlsx2(fpt_eff_free,"friedman_test_results_06182025.xlsx", sheetName = "fpt_free_effect",append=T)

# seasons by study area 
table(df$tag,df$Season)

## Homerange
hr_stat2 <- df %>%
  group_by(tag) %>%
  filter(n_distinct(Season) == length(unique(df$Season))) %>%
  dplyr::group_by(tag,Season) %>%
  dplyr::summarise(homerange_avg = mean(homerange), tag= tag, Season = Season,Study_Area=Study_Area) %>%
  dplyr::distinct(.)

hr_stat_summary2 <- hr_stat2 %>%
  ungroup() %>%
  dplyr::group_by(Study_Area,Season) %>%
  dplyr::summarise(count = n())
hr_stat_summary2

hr_ft_hm <- hr_stat2 %>%
  dplyr::filter(Study_Area == "Henry Mountains") %>% #hm
  ungroup(.) %>%
  rstatix::friedman_test(homerange_avg ~ Season | tag)
hr_ft_hm 

hr_w_hm <- hr_stat2 %>%
  dplyr::filter(Study_Area == "Henry Mountains") %>% #hm
  ungroup(.) %>%
  rstatix::pairwise_wilcox_test(homerange_avg ~ Season, p.adjust.method = p_correction,paired=T)
hr_w_hm

hr_eff_hm <- hr_stat2 %>%
  dplyr::filter(Study_Area == "Henry Mountains") %>% #hm
  ungroup(.) %>%
  rstatix::friedman_effsize(homerange_avg ~ Season | tag, ci=T, conf.level = 0.95)
hr_eff_hm

hr_ft_bc <- hr_stat2 %>%
  dplyr::filter(Study_Area == "Book Cliffs") %>% #bc
  ungroup(.) %>%
  rstatix::friedman_test(homerange_avg ~ Season | tag)
hr_ft_bc 

hr_w_bc <- hr_stat2 %>%
  dplyr::filter(Study_Area == "Book Cliffs") %>% #bc
  ungroup(.) %>%
  rstatix::pairwise_wilcox_test(homerange_avg ~ Season, p.adjust.method = p_correction,paired=T)
hr_w_bc

hr_bc <- hr_stat2 %>%
  dplyr::filter(Study_Area == "Book Cliffs") #bc

write.xlsx2(hr_ft_bc,"friedman_test_results_06182025.xlsx", sheetName = "hr_bc",append=T)
write.xlsx2(hr_ft_hm,"friedman_test_results_06182025.xlsx", sheetName = "hr_hm",append=T)
write.xlsx2(hr_w_hm,"friedman_test_results_06182025.xlsx", sheetName = "hr_hm_posthoc",append=T)
write.xlsx2(hr_eff_hm,"friedman_test_results_06182025.xlsx", sheetName = "hr_hm_effect",append=T)

## FPT
fpt_stat2 <- df %>%
  group_by(tag) %>%
  filter(n_distinct(Season) == length(unique(df$Season))) %>%
  dplyr::group_by(tag,Season) %>%
  dplyr::summarise(fpt_avg = mean(fpt,na.rm=T), tag= tag, Season = Season,Study_Area = Study_Area) %>%
  dplyr::distinct(.) 

fpt_stat_summary2 <- fpt_stat2 %>%
  ungroup() %>%
  dplyr::group_by(Study_Area,Season) %>%
  dplyr::summarise(count = n())
fpt_stat_summary2

fpt_ft_hm <- fpt_stat2 %>%
  dplyr::filter(Study_Area == "Henry Mountains") %>% #hm
  ungroup(.) %>%
  rstatix::friedman_test(fpt_avg ~ Season | tag)
fpt_ft_hm #p-value significant, proceed with posthoc

fpt_w_hm <- fpt_stat2 %>%
  dplyr::filter(Study_Area == "Henry Mountains") %>% #hm
  ungroup(.) %>%
  rstatix::pairwise_wilcox_test(fpt_avg ~ Season, p.adjust.method = p_correction,paired=T)
fpt_w_hm

fpt_eff_hm <- fpt_stat2 %>%
  dplyr::filter(Study_Area == "Henry Mountains") %>% #hm
  ungroup(.) %>%
  rstatix::friedman_effsize(fpt_avg ~ Season | tag, ci=T, conf.level = 0.95)
fpt_eff_hm

fpt_ft_bc <- fpt_stat2 %>%
  dplyr::filter(Study_Area == "Book Cliffs") %>% #bc
  ungroup(.) %>%
  rstatix::friedman_test(fpt_avg ~ Season | tag)
fpt_ft_bc #p-value significant, proceed with posthoc

fpt_w_bc <- fpt_stat2 %>%
  dplyr::filter(Study_Area == "Book Cliffs") %>% #bc
  ungroup(.) %>%
  rstatix::pairwise_wilcox_test(fpt_avg ~ Season, p.adjust.method = p_correction,paired=T)
fpt_w_bc

fpt_eff_bc <- fpt_stat2 %>%
  dplyr::filter(Study_Area == "Book Cliffs") %>% #bc
  ungroup(.) %>%
  rstatix::friedman_effsize(fpt_avg ~ Season | tag, ci=T, conf.level = 0.95)
fpt_eff_bc

fpt_ft_apr <- fpt_stat2 %>%
  dplyr::filter(Study_Area == "American Prairie Reserve") %>% #apr
  ungroup(.) %>%
  rstatix::friedman_test(fpt_avg ~ Season | tag)
fpt_ft_apr #p-value significant, proceed with posthoc

fpt_w_apr <- fpt_stat2 %>%
  dplyr::filter(Study_Area == "American Prairie Reserve") %>% #apr
  ungroup(.) %>%
  rstatix::pairwise_wilcox_test(fpt_avg ~ Season, p.adjust.method = p_correction,paired=T)
fpt_w_apr

fpt_eff_apr <- fpt_stat2 %>%
  dplyr::filter(Study_Area == "American Prairie Reserve") %>% #apr
  ungroup(.) %>%
  rstatix::friedman_effsize(fpt_avg ~ Season | tag, ci=T, conf.level = 0.95)
fpt_eff_apr

fpt_ft_mzr <- fpt_stat2 %>%
  dplyr::filter(Study_Area == "Medano-Zapata Ranch") %>% #mzr
  ungroup(.) %>%
  rstatix::friedman_test(fpt_avg ~ Season | tag)
fpt_ft_mzr #p-value not significant

fpt_ft_cc <- fpt_stat2 %>%
  dplyr::filter(Study_Area == "Caprock Canyon") %>% #mzr
  ungroup(.) %>%
  rstatix::friedman_test(fpt_avg ~ Season | tag)
fpt_ft_cc #p-value significant, proceed with posthoc

fpt_w_cc <- fpt_stat2 %>%
  dplyr::filter(Study_Area == "Caprock Canyon") %>% #cc
  ungroup(.) %>%
  rstatix::pairwise_wilcox_test(fpt_avg ~ Season, p.adjust.method = p_correction,paired=T)
fpt_w_cc

fpt_eff_cc <- fpt_stat2 %>%
  dplyr::filter(Study_Area == "Caprock Canyon") %>% #cc
  ungroup(.) %>%
  rstatix::friedman_effsize(fpt_avg ~ Season | tag, ci=T, conf.level = 0.95)
fpt_eff_cc

write.xlsx2(fpt_ft_hm,"friedman_test_results_06182025.xlsx", sheetName = "fpt_hm",append = T)
write.xlsx2(fpt_w_hm,"friedman_test_results_06182025.xlsx", sheetName = "fpt_hm_posthoc",append=T)
write.xlsx2(fpt_eff_hm,"friedman_test_results_06182025.xlsx", sheetName = "fpt_hm_effect",append=T)
write.xlsx2(fpt_ft_bc,"friedman_test_results_06182025.xlsx", sheetName = "fpt_bc",append=T)
write.xlsx2(fpt_w_bc,"friedman_test_results_06182025.xlsx", sheetName = "fpt_bc_posthoc",append=T)
write.xlsx2(fpt_eff_bc,"friedman_test_results_06182025.xlsx", sheetName = "fpt_bc_effect",append=T)
write.xlsx2(fpt_ft_apr,"friedman_test_results_06182025.xlsx", sheetName = "fpt_apr",append=T)
write.xlsx2(fpt_w_apr,"friedman_test_results_06182025.xlsx", sheetName = "fpt_apr_posthoc",append=T)
write.xlsx2(fpt_eff_apr,"friedman_test_results_06182025.xlsx", sheetName = "fpt_apr_effect",append=T)
write.xlsx2(fpt_ft_mzr,"friedman_test_results_06182025.xlsx", sheetName = "fpt_mzr",append=T)
write.xlsx2(fpt_ft_cc,"friedman_test_results_06182025.xlsx", sheetName = "fpt_cc",append=T)
write.xlsx2(fpt_w_cc,"friedman_test_results_06182025.xlsx", sheetName = "fpt_cc_posthoc",append=T)
write.xlsx2(fpt_eff_cc,"friedman_test_results_06182025.xlsx", sheetName = "fpt_cc_effect",append=T)

#####
## Kruskal-Wallis ####
df_summary <- df %>%
  dplyr::group_by(Season) %>%
  dplyr::summarise(count = n())
df_summary

# Compare same season for each regime!
hr_kw_season <- df %>%
  dplyr::group_by(Season) %>%
  kruskal_test(homerange ~ Regime) # all are significant, follow-up with Dunn's Test
hr_kw_season 

hr_dunn_season <- df %>%
  dplyr::filter(Season %in% hr_kw_season$Season[which(hr_kw_season$p < 0.05)]) %>%
  dplyr::group_by(Season) %>%
  dunn_test(homerange ~ Regime, p.adjust.method = p_correction, detailed = T)
hr_dunn_season 

hr_eff_season <- df %>%
  dplyr::filter(Season %in% hr_kw_season$Season[which(hr_kw_season$p < 0.05)]) %>%
  dplyr::group_by(Season) %>%
  rstatix::kruskal_effsize(homerange ~ Regime, ci=T, conf.level = 0.95)
hr_eff_season

fpt_kw_season <- df %>%
  dplyr::group_by(Season) %>%
  kruskal_test(fpt ~ Regime) # only growing is significant, follow-up with Dunn's Test
fpt_kw_season 

fpt_dunn_season <- df %>%
  dplyr::filter(Season %in% fpt_kw_season$Season[which(fpt_kw_season$p < 0.05)]) %>%
  dplyr::group_by(Season) %>%
  dunn_test(fpt ~ Regime, p.adjust.method = p_correction)
fpt_dunn_season 

fpt_eff_season <- df %>%
  dplyr::filter(Season %in% fpt_kw_season$Season[which(fpt_kw_season$p < 0.05)]) %>%
  dplyr::group_by(Season) %>%
  rstatix::kruskal_effsize(fpt ~ Regime, ci=T, conf.level = 0.95)
fpt_eff_season

write.xlsx2(hr_kw_season,"kruskalwallis_test_results_06182025.xlsx", sheetName = "hr_regime")
write.xlsx2(hr_dunn_season,"kruskalwallis_test_results_06182025.xlsx", sheetName = "hr_regime_posthoc",append=T)
write.xlsx2(hr_eff_season,"kruskalwallis_test_results_06182025.xlsx", sheetName = "hr_regime_effect",append=T)
write.xlsx2(fpt_kw_season,"kruskalwallis_test_results_06182025.xlsx", sheetName = "fpt_regime",append=T)
write.xlsx2(fpt_dunn_season,"kruskalwallis_test_results_06182025.xlsx", sheetName = "fpt_regime_posthoc",append=T)
write.xlsx2(fpt_eff_season,"kruskalwallis_test_results_06182025.xlsx", sheetName = "fpt_regime_effect",append=T)

# compare same season for each study area!
hr_kw_season2 <- df %>%
  dplyr::group_by(Season) %>%
  kruskal_test(homerange ~ Study_Area) # all are significant, follow-up with Dunn's Test
hr_kw_season2 

hr_dunn_season2 <- df %>%
  dplyr::filter(Season %in% hr_kw_season2$Season[which(hr_kw_season2$p < 0.05)]) %>%
  dplyr::group_by(Season) %>%
  dunn_test(homerange ~ Study_Area, p.adjust.method = p_correction)
hr_dunn_season2 

hr_eff_season2 <- df %>%
  dplyr::filter(Season %in% hr_kw_season2$Season[which(hr_kw_season2$p < 0.05)]) %>%
  dplyr::group_by(Season) %>%
  rstatix::kruskal_effsize(homerange ~ Study_Area, ci=T, conf.level = 0.95)
hr_eff_season2

fpt_kw_season2 <- df %>%
  dplyr::group_by(Season) %>%
  kruskal_test(fpt ~ Study_Area) # only growing is significant, follow-up with Dunn's Test
fpt_kw_season2 

fpt_dunn_season2 <- df %>%
  dplyr::filter(Season %in% fpt_kw_season2$Season[which(fpt_kw_season2$p < 0.05)]) %>%
  dplyr::group_by(Season) %>%
  dunn_test(fpt ~ Study_Area, p.adjust.method = p_correction)
fpt_dunn_season2

fpt_eff_season2 <- df %>%
  dplyr::filter(Season %in% fpt_kw_season2$Season[which(fpt_kw_season2$p < 0.05)]) %>%
  dplyr::group_by(Season) %>%
  rstatix::kruskal_effsize(fpt ~ Study_Area, ci=T, conf.level = 0.95)
fpt_eff_season2

write.xlsx2(hr_kw_season2,"kruskalwallis_test_results_06182025.xlsx", sheetName = "hr_studyarea",append=T)
write.xlsx2(hr_dunn_season2,"kruskalwallis_test_results_06182025.xlsx", sheetName = "hr_studyarea_posthoc",append=T)
write.xlsx2(hr_eff_season2,"kruskalwallis_test_results_06182025.xlsx", sheetName = "hr_studyarea_effect",append=T)
write.xlsx2(fpt_kw_season2,"kruskalwallis_test_results_06182025.xlsx", sheetName = "fpt_studyarea",append=T)
write.xlsx2(fpt_dunn_season2,"kruskalwallis_test_results_06182025.xlsx", sheetName = "fpt_studyarea_posthoc",append=T)
write.xlsx2(fpt_eff_season2,"kruskalwallis_test_results_06182025.xlsx", sheetName = "fpt_studyarea_effect",append=T)
#####
