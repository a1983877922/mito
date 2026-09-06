rm(list=ls())
library(dplyr)
library(tidyr)
library(xlsx)
library(data.table)
library(tidyverse)
library(Hmisc)
library(ggpubr)
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

#
child_info <- subset(hl1020,select = c("chrom","pos","id","ref","alt","info"))
mother_info <- subset(hl409,select = c("chrom","pos","id","ref","alt","info"))

all_id <- full_join(child_info,mother_info,by="id") %>% 
  mutate(chrom = coalesce(chrom.x, chrom.y),
         pos = coalesce(pos.x,pos.y),
         ref = coalesce(ref.x,ref.y),
         alt = coalesce(alt.x,alt.y),
         info = coalesce(info.x,info.y)) %>% 
  dplyr::select(c(chrom,pos,id,ref,alt,info))

#
all_inherit_df <- all_id
all_dnm_df <- all_id
all_dnm_df2 <- all_id

mergedf <- data.frame(
  HID = character(0),    # 数值型列
  MHID = character(0),   # 字符型列
  DIFF2norm = numeric(0),      # 逻辑型列
  DIFF2norm_abs = numeric(0), 
  DIFF2filtsyn = numeric(0), 
  DIFF2filtsyn_abs = numeric(0), 
  sum_abs = numeric(0), 
  sum_all = numeric(0)
)

all_mergedf <- data.frame(
  GENE = character(0),
  DIFF2norm = numeric(0),      # 逻辑型列
  DIFF2norm_abs = numeric(0), 
  DIFF2filtsyn = numeric(0), 
  DIFF2filtsyn_abs = numeric(0),
  HID = character(0),    # 数值型列
  MHID = character(0)   # 字符型列
)

mergedf3 <- data.frame(
  HID = character(0),    # 数值型列
  MHID = character(0),   # 字符型列
  DIFF = numeric(0)     # 逻辑型列
)

for (i in 1:nrow(ped)) {
  
  child <- as.character(ped[i,'HID'])
  mother <- as.character(ped[i,'MHID'])
  child_col <- subset(hl1020,select = child)
  child_df <- cbind(child_info,child_col)
  mother_col <- subset(hl409,select = mother)
  mother_df <- cbind(mother_info,mother_col) 
  
  
  inherit_df <- full_join(child_df,mother_df,by="id") %>% 
    mutate(chrom = coalesce(chrom.x, chrom.y),
           pos = coalesce(pos.x,pos.y),
           ref = coalesce(ref.x,ref.y),
           alt = coalesce(alt.x,alt.y),
           info = coalesce(info.x,info.y)) %>% 
    dplyr::select(-c(chrom.x,pos.x,ref.x,alt.x,info.x,
                     chrom.y,pos.y,ref.y,alt.y,info.y)) %>% 
    mutate(across(everything(), ~ifelse(is.na(.), 0, .))) %>% 
    mutate(diff = !!sym(child) - !!sym(mother)) %>% 
    filter(diff !=0) %>% 
    mutate(mean_diff1 = mean(diff)) %>% 
    mutate(mean_diff2 = mean(abs(diff))) %>% 
    mutate(mean_diff5 = sum(abs(diff))) %>%
    mutate(mean_diff6= sum(diff))
  
  
  inherit_df2 <- full_join(child_df,mother_df,by="id") %>% 
    mutate(chrom = coalesce(chrom.x, chrom.y),
           pos = coalesce(pos.x,pos.y),
           ref = coalesce(ref.x,ref.y),
           alt = coalesce(alt.x,alt.y),
           info = coalesce(info.x,info.y)) %>% 
    dplyr::select(-c(chrom.x,pos.x,ref.x,alt.x,info.x,
                     chrom.y,pos.y,ref.y,alt.y,info.y)) %>% 
    mutate(across(everything(), ~ifelse(is.na(.), 0, .))) %>% 
    filter(info != "synonymous_variant") %>% 
    mutate(diff = !!sym(child) - !!sym(mother)) %>% 
    filter(diff !=0) %>% 
    mutate(mean_diff3 = mean(diff)) %>% 
    mutate(mean_diff4 = mean(abs(diff)))
  mergedf[i,1] <- child
  mergedf[i,2] <- mother 
  mergedf[i,3] <- inherit_df$mean_diff1[1]
  mergedf[i,4] <- inherit_df$mean_diff2[1]
  mergedf[i,5] <- inherit_df2$mean_diff3[1]
  mergedf[i,6] <- inherit_df2$mean_diff4[1]
  mergedf[i,7] <- inherit_df$mean_diff5[1]
  mergedf[i,8] <- inherit_df$mean_diff6[1]
}

for (i in 1:nrow(ped)) {
  child <- as.character(ped[i,'HID'])
  mother <- as.character(ped[i,'MHID'])
  child_col <- subset(hl1020,select = child)
  child_df <- cbind(child_info,child_col)
  mother_col <- subset(hl409,select = mother)
  mother_df <- cbind(mother_info,mother_col) 
  
  
  inherit_df <- full_join(child_df,mother_df,by="id") %>% 
    mutate(chrom = coalesce(chrom.x, chrom.y),
           pos = coalesce(pos.x,pos.y),
           ref = coalesce(ref.x,ref.y),
           alt = coalesce(alt.x,alt.y),
           info = coalesce(info.x,info.y)) %>% 
    dplyr::select(-c(chrom.x,pos.x,ref.x,alt.x,info.x,
                     chrom.y,pos.y,ref.y,alt.y,info.y)) %>% 
    mutate(across(everything(), ~ifelse(is.na(.), 0, .))) %>% 
    mutate(diff = !!sym(child) - !!sym(mother)) %>% 
    filter(diff !=0) 
  
  inherit_df2 <- full_join(child_df,mother_df,by="id") %>% 
    mutate(chrom = coalesce(chrom.x, chrom.y),
           pos = coalesce(pos.x,pos.y),
           ref = coalesce(ref.x,ref.y),
           alt = coalesce(alt.x,alt.y),
           info = coalesce(info.x,info.y)) %>% 
    dplyr::select(-c(chrom.x,pos.x,ref.x,alt.x,info.x,
                     chrom.y,pos.y,ref.y,alt.y,info.y)) %>% 
    mutate(across(everything(), ~ifelse(is.na(.), 0, .))) %>% 
    filter(info != "synonymous_variant") %>% 
    mutate(diff = !!sym(child) - !!sym(mother)) %>% 
    filter(diff !=0)
  
  setDT(inherit_df)
  setDT(inherit_df2)
  
  inherit_df_spec <- inherit_df
  inherit_df2_spec <- inherit_df2
  #
  inherit_df[mtgff3, GENE := i.gene, on = .(pos >= start, pos <= end)] 
  
  inherit_df$GENE[is.na(inherit_df$GENE)] <- "intergenic"
  
  
  inherit_df_spec[mtgff3_PART, GENE := i.gene, on = .(pos >= start, pos <= end)] 
  
  inherit_df_spec<- inherit_df_spec %>% 
    filter(GENE %in% c("ATP8","ND4L"))
  
  inherit1 <- rbind(inherit_df,inherit_df_spec) %>% 
    group_by(GENE) %>% 
    mutate(DIFF2norm = mean(diff)) %>% 
    mutate(DIFF2norm_abs = mean(abs(diff))) %>% 
    distinct(GENE,DIFF2norm,DIFF2norm_abs)
  
  
  
  
  inherit_df2[mtgff3, GENE := i.gene, on = .(pos >= start, pos <= end)] 
  
  inherit_df2$GENE[is.na(inherit_df2$GENE)] <- "intergenic"
  
  
  
  inherit_df2_spec[mtgff3_PART, GENE := i.gene, on = .(pos >= start, pos <= end)] 
  
  inherit_df2_spec<- inherit_df2_spec %>% 
    filter(GENE %in% c("ATP8","ND4L"))
  
  
  inherit2 <- rbind(inherit_df2,inherit_df2_spec) %>% 
    group_by(GENE) %>% 
    mutate(DIFF2filtsyn = mean(diff)) %>% 
    mutate(DIFF2filtsyn_abs = mean(abs(diff))) %>% 
    distinct(GENE,DIFF2filtsyn,DIFF2filtsyn_abs)
  
  mergedf2 <- full_join(inherit1,inherit2) %>% 
    mutate(HID = child,
           MHID = mother)
  
  all_mergedf <- rbind(all_mergedf,mergedf2) 
  
}

for (i in 1:nrow(ped)) {
  child <- as.character(ped[i,'HID'])
  mother <- as.character(ped[i,'MHID'])
  child_col <- subset(hl1020,select = child)
  child_df <- cbind(child_info,child_col)
  mother_col <- subset(hl409,select = mother)
  mother_df <- cbind(mother_info,mother_col) 
  
  
  inherit_df <- full_join(child_df,mother_df,by="id") %>% 
    mutate(chrom = coalesce(chrom.x, chrom.y),
           pos = coalesce(pos.x,pos.y),
           ref = coalesce(ref.x,ref.y),
           alt = coalesce(alt.x,alt.y),
           info = coalesce(info.x,info.y)) %>% 
    dplyr::select(-c(chrom.x,pos.x,ref.x,alt.x,info.x,
                     chrom.y,pos.y,ref.y,alt.y,info.y)) %>% 
    mutate(across(everything(), ~ifelse(is.na(.), 0, .))) %>% 
    filter(id =="14766_C_T") %>% 
    mutate(diff = !!sym(child) - !!sym(mother)) 
  
  mergedf3[i,1] <- child
  mergedf3[i,2] <- mother 
  mergedf3[i,3] <- inherit_df$diff 
  
}



# diff <- c("DIFF2norm" ,"DIFF2norm_abs","DIFF2filtsyn" ,"DIFF2filtsyn_abs")
diff <- c("DIFF2norm" ,"DIFF2norm_abs","DIFF2filtsyn" ,"DIFF2filtsyn_abs","sum_abs","sum_all")
phe <- c("Age", "Height", "Weight", "Survival_time_without_transfusion", 
  "Annual_transfusions", 
  "HbF", "Fetal_hemoglobin",  "Hemoglobin", "Hemoglobin_A", "Hemoglobin_A2",
  "Iron", "Serum_Ferritin", "Transferrin", "Transferrin_Saturation",   
  "Unsaturated_iron_binding_capacity", "Soluble_transferrin_receptor", "Total_iron_binding_capacity")
###
phe_child[phe] <- scale(phe_child[phe], center = TRUE, scale = TRUE)
####
input <- mergedf %>% left_join(dfID1020,by="HID") %>% 
  left_join(phe_child,by="ID") %>% 
  filter(Transfusion_Dependence == "NTDT") 


input2 <- all_mergedf %>% left_join(dfID1020,by="HID") %>% 
  left_join(phe_child,by="ID") %>% 
  filter(GENE =="CYTB")

input3 <- mergedf3 %>% left_join(dfID1020,by="HID") %>% 
  left_join(phe_child,by="ID") 

### 提取需要分析的变量 ###
diff_vars <- diff
phe_vars <- phe

# Create分析用的数据子集
analysis_data <- input %>%
  as.data.frame() %>% 
  select(all_of(c(diff_vars, phe_vars))) %>% 
  na.omit()

### 使用自定义函数计算相关系数andp-values ###
cor_test_matrix <- function(data) {
  n_vars <- ncol(data)
  cor_matrix <- matrix(NA, nrow = n_vars, ncol = n_vars)
  p_matrix <- matrix(NA, nrow = n_vars, ncol = n_vars)
  colnames(cor_matrix) <- rownames(cor_matrix) <- colnames(data)
  colnames(p_matrix) <- rownames(p_matrix) <- colnames(data)
  
  for (i in 1:n_vars) {
    for (j in 1:n_vars) {
      ### remove缺失值 ###
      complete_cases <- complete.cases(data[, i], data[, j])
      x <- data[complete_cases, i]
      y <- data[complete_cases, j]
      
      if (length(x) >= 3) {  # 至少需要3个观测值
        cor_test <- cor.test(x, y, method = "pearson")
        cor_matrix[i, j] <- cor_test$estimate
        p_matrix[i, j] <- cor_test$p.value
      }
    }
  }
  
  return(list(cor = cor_matrix, p = p_matrix))
}

### 计算相关系数andp-values ###
cor_results <- cor_test_matrix(analysis_data)

### 提取我们关心的部分：diff变量与phe变量的相关系数andp-values ###
diff_phe_cor <- cor_results$cor[diff_vars, phe_vars]
diff_phe_p <- cor_results$p[diff_vars, phe_vars]

### 转换为长格式便于分析 ###
cor_long <- as.data.frame(diff_phe_cor) %>%
  rownames_to_column(var = "diff_var") %>%
  pivot_longer(cols = -diff_var, 
               names_to = "phe_var", 
               values_to = "correlation")

p_long <- as.data.frame(diff_phe_p) %>%
  rownames_to_column(var = "diff_var") %>%
  pivot_longer(cols = -diff_var, 
               names_to = "phe_var", 
               values_to = "p_value")

# Merge相关系数andp-values
cor_long <- cor_long %>%
  left_join(p_long, by = c("diff_var", "phe_var")) %>%
  mutate(
    significance = case_when(
      p_value < 0.001 ~ "***",
      p_value < 0.01 ~ "**",
      p_value < 0.05 ~ "*",
      TRUE ~ "ns"
    ),
    fdr_adjusted = p.adjust(p_value, method = "fdr"),
    fdr_significance = case_when(
      fdr_adjusted < 0.001 ~ "***",
      fdr_adjusted < 0.01 ~ "**",
      fdr_adjusted < 0.05 ~ "*",
      TRUE ~ "ns"
    )
  )

### 显示相关系数最高的前20个组合 ###
cor_long %>%
  filter(diff_var == "DIFF2norm") %>% 
  arrange(desc(abs(correlation))) %>%
  head(20)

### 或者可以Inspect最显著的相关性 ###
cor_long %>%
  filter(diff_var == "DIFF2norm") %>% 
  arrange(p_value) %>%
  head(20)




######

input4 <- input %>%  as.data.frame() %>% 
  select(all_of(c(diff_vars, phe_vars))) %>% 
  na.omit() 
##
input5 <- input %>%  as.data.frame() %>% 
  filter(Transfusion_Dependence == "NTDT") %>% 
  select(all_of(c(diff_vars, phe_vars))) %>% 
  na.omit() 
### 为Annual_transfusionsCreate图形 ###
p1 <- ggscatter(input4, x = "Annual_transfusions", y = "DIFF2norm",
                add = "reg.line", conf.int = TRUE,
                cor.coef = TRUE, cor.method = "pearson",
                xlab = "Annual Transfusions", ylab = "DIFF2norm",
                title = "",
                color = "steelblue") +
  theme_bw()
p1
### 为Hemoglobin_ACreate图形 ###
p2 <- ggscatter(input4, x = "Hemoglobin_A", y = "DIFF2norm",
                add = "reg.line", conf.int = TRUE,
                cor.coef = TRUE, cor.method = "pearson",
                xlab = "Hemoglobin A", ylab = "",
                title = "",
                color = "darkred") +
  theme_bw()

p3 <- ggscatter(input5, x = "Annual_transfusions", y = "DIFF2norm",
                add = "reg.line", conf.int = TRUE,
                cor.coef = TRUE, cor.method = "pearson",
                xlab = "Annual Transfusions", ylab = "",
                title = "",
                color = "steelblue") +
  theme_bw()
p3

p4 <- ggscatter(input5, x = "HbF", y = "DIFF2norm",
                add = "reg.line", conf.int = TRUE,
                cor.coef = TRUE, cor.method = "pearson",
                xlab = "Hemoglobin A", ylab = "",
                title = "",
                color = "darkred") +
  theme_bw()
p4
# Composite plot
# CreateComposite plot
combined_plot <- ggarrange(p1, p2, ncol = 2, nrow = 1)
combined_plot

combined_plot2 <- ggarrange(p3, p4, ncol = 2, nrow = 1)
combined_plot2
# Save为 PDF（Method 1：使用 ggsave）
ggsave("D:\\biosoft\\1000thal\\mtDNA\\figure\\figure4E.corr of diff HL.pdf", 
       plot = combined_plot,
       width = 12,    # PDF宽度（英寸）
       height = 6,    # PDF高度（英寸）
       device = "pdf")

ggsave("D:\\biosoft\\1000thal\\mtDNA\\figure\\figure4E.corr of diff HL in NTDT.pdf", 
       plot = combined_plot2,
       width = 12,    # PDF宽度（英寸）
       height = 6,    # PDF高度（英寸）
       device = "pdf")
# # 或者方法2：直接使用 pdf() 函数
# pdf("combined_plot.pdf", width = 12, height = 6)
# print(combined_plot)
# dev.off()













#####
diff <- c("DIFF")
### Check必要的列whether or not存在 ###
all_vars <- c(diff, phe)
missing_vars <- setdiff(all_vars, colnames(input3))
if (length(missing_vars) > 0) {
  warning(paste("以下变量在数据中不存在:", paste(missing_vars, collapse = ", ")))
}

# Create分析用的数据子集
analysis_data <- input3 %>%
  as.data.frame() %>%
  select(any_of(all_vars)) %>%  # 使用 any_of 避免不存在的变量报错
  na.omit() 



### Checkwhether or not有enough data ###
if (nrow(analysis_data) < 3) {
  stop("数据不足，至少需要3个完整观测值")
}

### 简化相关系数计算函数 ###
cor_test_pairwise <- function(data, x_vars, y_vars) {
  results <- list()
  
  for (x_var in x_vars) {
    for (y_var in y_vars) {
      if (x_var %in% colnames(data) && y_var %in% colnames(data)) {
        complete_cases <- complete.cases(data[[x_var]], data[[y_var]])
        x <- data[[x_var]][complete_cases]
        y <- data[[y_var]][complete_cases]
        
        if (length(x) >= 3) {
          cor_test <- cor.test(x, y, method = "pearson")
          results[[paste(x_var, y_var, sep = "_")]] <- data.frame(
            diff_var = x_var,
            phe_var = y_var,
            correlation = cor_test$estimate,
            p_value = cor_test$p.value,
            n = length(x)
          )
        }
      }
    }
  }
  
  if (length(results) == 0) {
    return(NULL)
  }
  
  return(bind_rows(results))
}

### 计算相关系数 ###
cor_results <- cor_test_pairwise(analysis_data, diff, phe)

if (is.null(cor_results)) {
  stop("无法计算任何相关系数，请检查数据")
}

# Add significance markersandFDRcorrection
cor_results <- cor_results %>%
  mutate(
    significance = case_when(
      p_value < 0.001 ~ "***",
      p_value < 0.01 ~ "**",
      p_value < 0.05 ~ "*",
      TRUE ~ "ns"
    ),
    fdr_adjusted = p.adjust(p_value, method = "fdr"),
    fdr_significance = case_when(
      fdr_adjusted < 0.001 ~ "***",
      fdr_adjusted < 0.01 ~ "**",
      fdr_adjusted < 0.05 ~ "*",
      TRUE ~ "ns"
    )
  ) %>%
  arrange(desc(abs(correlation)))

### 显示Results ###
print("相关系数Results:")
print(cor_results)

### 显示相关系数最高的前20个组合 ###
if (nrow(cor_results) > 0) {
  cat("\n相关系数最高的组合:\n")
  top_results <- head(cor_results, min(20, nrow(cor_results)))
  print(top_results)
  
  ### 也可以使用 ggplot 可视化 ###
  library(ggplot2)
  
  # CreateHeatmap（如果有多对组合）
  if (nrow(cor_results) > 1) {
    p <- ggplot(cor_results, aes(x = phe_var, y = diff_var, fill = correlation)) +
      geom_tile() +
      geom_text(aes(label = paste0(round(correlation, 2), "\n", significance)), 
                size = 3) +
      scale_fill_gradient2(low = "blue", mid = "white", high = "red", 
                           midpoint = 0, limits = c(-1, 1)) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      labs(title = "M14766变量与Phenotype变量的相关系数Heatmap",
           x = "Phenotype变量", y = "Diff变量")
    print(p)
  }
}

# write.table(inherit_cleaned_filter,'D:\\biosoft\\1000thal/mtDNA/Maternal inheritance/inherit.tsv',sep = "\t",quote = F,row.names = F,na = "")
# write.table(inherit_final,'D:\\biosoft\\1000thal/mtDNA/Maternal inheritance/inherit_final202512.tsv',sep = "\t",quote = F,row.names = F,na = "")
# write.table(dnm_cleaned_filter,'D:\\biosoft\\1000thal/mtDNA/Maternal inheritance/dnm.tsv',sep = "\t",quote = F,row.names = F,na = "")
# write.table(dnm_cleaned_filter2,'D:\\biosoft\\1000thal/mtDNA/Maternal inheritance/dnm_abs.tsv',sep = "\t",quote = F,row.names = F,na = "")
# 
# write.table(dnm_cleaned_filter,'D:\\biosoft\\1000thal/mtDNA/Maternal inheritance/dnm_cutoff202512.tsv',sep = "\t",quote = F,row.names = F,na = "")

## Save所有数据框
# setwd("D:\\biosoft\\1000thal\\mtDNA\\R script/")
# save.image("maternal_inherit.RData")
