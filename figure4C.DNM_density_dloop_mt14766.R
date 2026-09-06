rm(list=ls())
library(dplyr)
library(xlsx)
library(data.table)
library(tidyverse)
library(Hmisc)
library(ggplot2)
library(tidyr)
library(ggridges)
## Load cached, skip recomputation
# load("D:\\biosoft\\1000thal\\mtDNA\\R script/maternal_inherit.RData")
ped164 <- readxl::read_excel("D:\\biosoft\\1000thal\\mtDNA\\Maternal inheritance/232ped.xlsx",sheet = "164TRIOS_MOTHER")
ped68 <- readxl::read_excel("D:\\biosoft\\1000thal\\mtDNA\\Maternal inheritance/232ped.xlsx",sheet = "68SINGON")
header1020 <- read.table("D:\\biosoft\\1000thal\\mtDNA\\genemut\\header.1020",sep = "\t")
header409 <- read.table("D:\\biosoft\\1000thal\\mtDNA\\genemut\\409/header.409",sep = "\t")
sampID1020 <- header1020[7:1026]
sampID409 <- header409[7:415]

dfID1020 <- readxl::read_excel("D:/biosoft/1000thal/千人/ID对应(1020+409)2023.12.22.xlsx", sheet = "1020")
phe_child <- readxl::read_excel("D:/biosoft/1000thal/千人/RNO.1 Basic statistics of 1020 β-thalassemia patients.xlsx", sheet = "Sheet1")

###
phe_child[phe_child == "Irregular transfusions"] <- NA
phe_child[phe_child == "Untransfused"] <- NA
phe_child[phe_child == "NA"] <- NA

phe_child[,c("ID", "six_genotype_category", "HBB-HGVS",
             "HBB-classic", "HBA-HGVS", "HBA-classic", "HBB_genotype_category",
             "HBA_genotype_category", "Clinical_staging", "Transfusion_Dependence", 
             "Regular_transfusion", "Thalassaemia_face", "Jaundice", "Gallstones", 
             "Splenic", "Hepatomegaly", "Splenomegaly")] <- 
  lapply(phe_child[,c("ID", "six_genotype_category", "HBB-HGVS",
                      "HBB-classic", "HBA-HGVS", "HBA-classic", "HBB_genotype_category",
                      "HBA_genotype_category", "Clinical_staging", "Transfusion_Dependence", 
                      "Regular_transfusion", "Thalassaemia_face", "Jaundice", "Gallstones", 
                      "Splenic", "Hepatomegaly", "Splenomegaly")], as.character)

phe_child[, c("Age", "Height", "Weight", "Survival_time_without_transfusion", 
              "Annual_transfusions", "Pre-transfusion_HGB", 
              "Post-transfusion_HGB", "HGB_before_first_transfusion", 
              "HbF", "C-peptide", "C-peptide_60_minutes_postprandial", 
              "C-peptide_120_minutes_postprandial", "White_blood_cell_count", 
              "mutant_hemoglobins", "Unsaturated_iron_binding_capacity", "Ultrasensitive-Thyrotropin", 
              "Large_platelet_ratio", "Percentage_of_monocytes", "Absolute_monocyte_count", 
              "Percentage_of_reticulocytes_with_low_fluorescence_intensity", 
              "Percentage_of_reticulocytes_with_high_fluorescence_intensity", 
              "Hematocrit", "Coefficient_of_variation_of_red_blood_cell_distribution_width", 
              "Red_blood_cell_distribution_width_standard_deviation", "Red_blood_cell_count", 
              "Activated_partial_thromboplastin_time", "Fetal_hemoglobin", 
              "Soluble_transferrin_receptor", "Percentage_of_lymphocytes", 
              "Absolute_lymphocyte_count", "Thrombin_Time", "Prothrombin_time", 
              "Mean_corpuscular_volume", "Mean_corpuscular_hemoglobin", "Mean_corpuscular_hemoglobin_concentration", 
              "Mean_Platelet_volume", "Glucose", "Glucose_60_minutes_postprandial", 
              "Glucose_120_minutes_postprandial", "Percentage_of_basophils", 
              "Absolute_basophils_count", "Percentage_of_eosinophils", "Absolute_eosinophil_count", 
              "Iron", "Serum_Ferritin", "Percentage_of_reticulocytes", "Reticulocyte_count", 
              "Fibrinogen", "Hemoglobin", "Hemoglobin_A", "Hemoglobin_A2", 
              "Transferrin", "Platelet_specific_volume", "Platelet_distribution_width", 
              "Platelet_Count", "Insulin", "Insulin_120_minutes_postprandial", 
              "Insulin_60_minutes_postprandial", "Free_Thyroxine", "Free_Triiodothyronine", 
              "Percentage_of_immature_reticulocytes", "Percentage_of_neutrophils", 
              "Absolute_neutrophil_count", "Median_fluorescence_Intensity_Reticulocyte_ratio", 
              "Transferrin_Saturation", "Total_iron_binding_capacity")] <- 
  lapply(phe_child[, c("Age", "Height", "Weight", "Survival_time_without_transfusion", 
                       "Annual_transfusions", "Pre-transfusion_HGB", 
                       "Post-transfusion_HGB", "HGB_before_first_transfusion", 
                       "HbF", "C-peptide", "C-peptide_60_minutes_postprandial", 
                       "C-peptide_120_minutes_postprandial", "White_blood_cell_count", 
                       "mutant_hemoglobins", "Unsaturated_iron_binding_capacity", "Ultrasensitive-Thyrotropin", 
                       "Large_platelet_ratio", "Percentage_of_monocytes", "Absolute_monocyte_count", 
                       "Percentage_of_reticulocytes_with_low_fluorescence_intensity", 
                       "Percentage_of_reticulocytes_with_high_fluorescence_intensity", 
                       "Hematocrit", "Coefficient_of_variation_of_red_blood_cell_distribution_width", 
                       "Red_blood_cell_distribution_width_standard_deviation", "Red_blood_cell_count", 
                       "Activated_partial_thromboplastin_time", "Fetal_hemoglobin", 
                       "Soluble_transferrin_receptor", "Percentage_of_lymphocytes", 
                       "Absolute_lymphocyte_count", "Thrombin_Time", "Prothrombin_time", 
                       "Mean_corpuscular_volume", "Mean_corpuscular_hemoglobin", "Mean_corpuscular_hemoglobin_concentration", 
                       "Mean_Platelet_volume", "Glucose", "Glucose_60_minutes_postprandial", 
                       "Glucose_120_minutes_postprandial", "Percentage_of_basophils", 
                       "Absolute_basophils_count", "Percentage_of_eosinophils", "Absolute_eosinophil_count", 
                       "Iron", "Serum_Ferritin", "Percentage_of_reticulocytes", "Reticulocyte_count", 
                       "Fibrinogen", "Hemoglobin", "Hemoglobin_A", "Hemoglobin_A2", 
                       "Transferrin", "Platelet_specific_volume", "Platelet_distribution_width", 
                       "Platelet_Count", "Insulin", "Insulin_120_minutes_postprandial", 
                       "Insulin_60_minutes_postprandial", "Free_Thyroxine", "Free_Triiodothyronine", 
                       "Percentage_of_immature_reticulocytes", "Percentage_of_neutrophils", 
                       "Absolute_neutrophil_count", "Median_fluorescence_Intensity_Reticulocyte_ratio", 
                       "Transferrin_Saturation", "Total_iron_binding_capacity")], as.numeric)
#####
mtgff3 <- read.table("D:\\biosoft\\1000thal\\mtDNA\\genemut\\1020bedout\\mt.gff3.bed",sep = "\t")
mtgff3$V2 <- mtgff3$V2 + 1
mtgff3$"with" <- mtgff3$V3 - mtgff3$V2 + 1
mtgff3[1,6] <- "D_loop"
mtgff3[39,6] <- "D_loop"
colnames(mtgff3) <- c("chrom","start","end","id","strand","gene","with")
mtgff3_PART <- mtgff3 %>% 
  filter(gene %in% c("ATP8","ND4L"))

setDT(mtgff3)
setDT(mtgff3_PART)
##

hl1020 <- read.table("D:\\biosoft\\1000thal\\mtDNA\\genemut\\1020.mt.ano.filter.ft2onefilt")
hl409 <- read.table("D:\\biosoft\\1000thal\\mtDNA\\genemut\\409.mt.ano.filter.ft2onefilt") 
colname1020 <- c("chrom","pos","id","ref","alt","info",sampID1020)
colname409 <- c("chrom","pos","id","ref","alt","info",sampID409)
colnames(hl1020) <- colname1020
colnames(hl409) <- colname409

### 将小于等于0.05的值变为0,大于等于0.95的值变为0 ###
# hl1020[ , 7:1026][hl1020[ , 7:1026] <= 0.05] <- 0
# hl409[ , 7:415][hl409[ , 7:415] <= 0.05] <- 0
# hl1020[ , 7:1026][hl1020[ , 7:1026] >= 0.95] <- 0
# hl409[ , 7:415][hl409[ , 7:415] >= 0.95] <- 0

### 将小于等于0.1的值变为0,大于等于0.9的值变为0 ###
# hl1020[ , 7:1026][hl1020[ , 7:1026] <= 0.1] <- 0
# hl409[ , 7:415][hl409[ , 7:415] <= 0.1] <- 0
# hl1020[ , 7:1026][hl1020[ , 7:1026] >= 0.9] <- 0
# hl409[ , 7:415][hl409[ , 7:415] >= 0.9] <- 0



ped <- rbind(ped164,ped68)



ox_dnm <- c("146_T_C", "183_A_G", "204_T_C", "16093_T_C", "16129_G_A", 
  "16179_CAA_C", "16180_A_AC", "16183_A_C", "16183_A_AC", "16183_A_ACCC", 
  "16183_A_ACCCC", "16266_C_T", "16297_T_C", "16357_T_C", "16362_T_C"
)






######
dfsub <- hl1020 %>% 
  filter(id %in% ox_dnm) %>% 
  # filter(id %in% c("14766_C_T", "14766_C_A")) %>% 
  t() %>% 
  `colnames<-`(.[3, ]) %>%  # Set列名为第3行
  .[7:nrow(.), ] %>%        # 取第7行到最后
  as.data.frame() %>% 
  mutate(HID = rownames(.), .before = 1) %>%  # 将Row names作为第一列
  mutate(across(-HID, as.numeric))            # ID列外的其他列转为数值

dfsub2 <- hl409 %>% 
  filter(id %in% c("14766_C_T", "14766_C_A")) %>% 
  t() %>% 
  `colnames<-`(.[3, ]) %>%  # Set列名为第3行
  .[7:nrow(.), ] %>%        # 取第7行到最后
  as.data.frame() %>% 
  mutate(MHID = rownames(.), .before = 1) %>%  # 将Row names作为第一列
  mutate(across(-MHID, as.numeric))            # ID列外的其他列转为数值
###
### 将宽格式数据转换为长格式 ###
df_long <- dfsub %>%
  gather(key = "variable", value = "value") %>% 
  filter(variable != "HID")
##
df_long$value <- as.numeric(df_long$value)

variable_order <- c("146_T_C", "204_T_C", "16093_T_C", "16129_G_A", 
                    "16179_CAA_C", "16183_A_C", "16183_A_AC", 
                    "16183_A_ACCC", "16183_A_ACCCC")
df_long <- df_long %>% 
  mutate(value2 = ifelse(value > 0.1 & value < 0.9, value, 0)) %>% 
  filter(!variable %in% c("183_A_G", "16362_T_C","16357_T_C","16297_T_C","16180_A_AC", "16266_C_T")) %>% 
  mutate(variable = factor(variable, levels = variable_order))

col <- c("#43978F","#9EC4BE","#ABD0F1","#DCE9F4","#E56F5E","#F19685","#F6C957","#FFB77F","#FBE8D5")


p1 <- ggplot(df_long, aes(x = value2, y = variable, fill = variable)) +
  geom_density_ridges(alpha = 0.7) +
  scale_fill_manual(values = col) +  # Specify颜色
  # scale_x_continuous(
  #   limits = c(0.1, 0.9),
  #   breaks = seq(0.1, 0.9, by = 0.1)
  # ) +
  labs(
    title = "",
    x = "Heteroplasmy levels",
    y = "De novo mutations in D-loop"
  ) +
  theme_classic() +
  theme(legend.position = "none")  # 脊线图通常隐藏图例

p1
# p1 %>% 
#   ggsave(
#     filename = "D:/biosoft/1000thal/mtDNA/figure/figure4C.D-loop DNM density plot 2026.pdf",
#     width = 8,
#     height = 6,
#     dpi = 300
#   )
####
df_long <- dfsub %>%
  gather(key = "variable", value = "value") %>% 
  filter(variable != "HID")

df_long$value <- as.numeric(df_long$value)

variable_order2 <- c("183_A_G","16180_A_AC" , "16266_C_T", "16297_T_C", "16357_T_C", "16362_T_C")

df_long <- df_long %>% 
  # mutate(value2 = ifelse(value > 0.1 & value < 0.9, value, 0)) %>% 
  filter(variable %in% c("183_A_G","16180_A_AC" ,"16297_T_C","16266_C_T", "16362_T_C","16357_T_C")) %>% 
  mutate(variable = factor(variable, levels = variable_order2))


col2<- c("#43978F","#ABD0F1","#E56F5E","#F19685","#F6C957","#FFB77F")
p11 <- ggplot(df_long, aes(x = value, y = variable, fill = variable)) +
  geom_density_ridges(alpha = 0.7) +
  scale_fill_manual(values = col2) +  # Specify颜色
  scale_x_continuous(
      limits = c(0.1, 1),
      breaks = seq(0.1, 1, by = 0.1)
    # limits = c(0, 1),
    # breaks = seq(0, 1, by = 0.1)
  ) +
  labs(
    title = "",
    x = "Heteroplasmy levels",
    y = "De novo mutations in D-loop"
  ) +
  theme_classic() +
  theme(legend.position = "none")  # 脊线图通常隐藏图例

p11

# p1 %>% ggexport(filename = "D:/biosoft/1000thal/mtDNA/figure/figure4C.D-loop DNM density plot 2026.pdf",width = 8,height = 6,res=300)
p11%>%
  ggsave(
    filename = "D:/biosoft/1000thal/mtDNA/figure/figure4C2.D-loop DNM density plot 2026.pdf",
    width = 8,
    height = 6,
    dpi = 300
  )


# Plot多个Density图
ggplot(df_long, aes(x = value, fill = variable)) +
  geom_density(alpha = 0.5) +
  # facet_wrap(~ variable, scales = "free") +
  labs(title = "Density Distribution of All Columns",
       x = "Value", y = "Density") +
  theme_minimal() +
  theme(legend.position = "none")  # 隐藏图例

df_long2 <- dfsub2 %>%
  gather(key = "variable", value = "value")


merge <- ped %>% left_join(dfsub,by = "HID") %>% 
  left_join(dfsub2, by = c("MHID" = "MHID")) %>% 
  select(c( "TID","14766_C_T.x", "14766_C_T.y"))


### 转换为长格式 ###
merge_long <- merge %>%
  pivot_longer(
    cols = c(`14766_C_T.x`, `14766_C_T.y`),
    names_to = "group",
    values_to = "value"
  ) %>%
  mutate(
    group = case_when(
      group == "14766_C_T.x" ~ "HID (子代/父代)",
      group == "14766_C_T.y" ~ "MHID (母代)",
      TRUE ~ group
    )
  )

# PlotDensity图
# ggplot(merge_long, aes(x = value, fill = group)) +
#   geom_density(alpha = 0.8) +
#   scale_fill_manual(
#     values = c("HID (子代/父代)" = "#DF3027", "MHID (母代)" = "#2A9698"),
#     name=NULL,
#     labels=c("Thalassemia", "Mother")
#     ) +
#   labs(title = "chrM:14766:C>T",
#        x = "Heteroplasmy",
#        y = "Density",
#        fill = "") +
#   theme_classic() +
#   theme(legend.position = "top")

P1 <- ggplot(merge_long, aes(x = value, color = group)) +  # 将 fill Change to color
  geom_density(alpha = 0.8, fill = NA, linewidth = 1) +  # Set fill = NA
  scale_color_manual(  # 将 scale_fill_manual 改为 scale_color_manual
    values = c("HID (子代/父代)" = "#DF3027", "MHID (母代)" = "#2A9698"),
    name = NULL,
    labels = c("Thalassemia", "Mother")
  ) +
  labs(
    title = "chrM:14766:C>T",
    x = "Heteroplasmy",
    y = "Density",
    color = ""  # 将 fill Change to color
  ) +
  theme_classic() +
  theme(legend.position = "top")

P1
# P1 %>% ggexport(filename = "D:/biosoft/1000thal/mtDNA/figure/figure4C.M14766 density plot 202512.pdf",width = 8,height = 6,res=300)

# ggsave(
#   filename = "D:/biosoft/1000thal/mtDNA/figure/figure4C.M14766 density plot 202512.pdf",
#   plot = P1,
#   width = 8,
#   height = 6,
#   device = "pdf"
# )
# %>% 
#   select(c("TID.x", "ID.x", "HID", "SEX.x", "MID.x", "MHID.x", "TID.y", "14766_C_A", "14766_C_T")) %>% 
#   left_join(dfsub2, by = c("MHID.x" = "MHID")) 
# 
# %>% 
#   select(c( "TID.x","14766_C_T.x", "14766_C_T.y"))
  







# write.table(inherit_cleaned_filter,'D:\\biosoft\\1000thal/mtDNA/Maternal inheritance/inherit.tsv',sep = "\t",quote = F,row.names = F,na = "")
# write.table(inherit_final,'D:\\biosoft\\1000thal/mtDNA/Maternal inheritance/inherit_final202512.tsv',sep = "\t",quote = F,row.names = F,na = "")
# write.table(dnm_cleaned_filter,'D:\\biosoft\\1000thal/mtDNA/Maternal inheritance/dnm.tsv',sep = "\t",quote = F,row.names = F,na = "")
# write.table(dnm_cleaned_filter2,'D:\\biosoft\\1000thal/mtDNA/Maternal inheritance/dnm_abs.tsv',sep = "\t",quote = F,row.names = F,na = "")
# 
# write.table(dnm_cleaned_filter,'D:\\biosoft\\1000thal/mtDNA/Maternal inheritance/dnm_cutoff202512.tsv',sep = "\t",quote = F,row.names = F,na = "")

## Save所有数据框
# setwd("D:\\biosoft\\1000thal\\mtDNA\\R script/")
# save.image("maternal_inherit.RData")
