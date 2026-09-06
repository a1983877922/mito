rm(list=ls())
library(dplyr)
library(xlsx)
library(data.table)
library(tidyverse)
##
hl1020 <- read.table("D:\\biosoft\\1000thal\\mtDNA\\genemut\\1020.mt.ano.filter.ft2onefilt")
hl409 <- read.table("D:\\biosoft\\1000thal\\mtDNA\\genemut\\409.mt.ano.filter.ft2onefilt") 
header1020 <- read.table("D:\\biosoft\\1000thal\\mtDNA\\genemut\\header.1020",sep = "\t")
header409 <- read.table("D:\\biosoft\\1000thal\\mtDNA\\genemut\\409/header.409",sep = "\t")

sampID1020 <- header1020[7:1026]
sampID409 <- header409[7:415]
colname1020 <- c("chrom","pos","id","ref","alt","info",sampID1020)
colname409 <- c("chrom","pos","id","ref","alt","info",sampID409)

colnames(hl1020) <- colname1020
colnames(hl409) <- colname409
dfID1020 <- readxl::read_excel("D:/biosoft/1000thal/千人/ID对应(1020+409)2023.12.22.xlsx", sheet = "1020")
phe_child <- readxl::read_excel("D:/biosoft/1000thal/千人/RNO.1 Basic statistics of 1020 β-thalassemia patients.xlsx", sheet = "Sheet1")
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


phe <- c("Survival_time_without_transfusion", "Annual_transfusions", 
         "HbF", "Fetal_hemoglobin",  "Hemoglobin", "Hemoglobin_A", "Hemoglobin_A2",
         "Iron", "Serum_Ferritin", "Transferrin", "Transferrin_Saturation",   
         "Unsaturated_iron_binding_capacity", "Soluble_transferrin_receptor", "Total_iron_binding_capacity")
# phe_child[phe] <- scale(phe_child[phe], center = TRUE, scale = TRUE)


df1 <- hl1020 %>% 
  filter(id =="14766_C_T") %>% 
  select(-c("chrom","pos","ref","alt","info")) %>% 
  pivot_longer(cols = -id,          # 除了id列以外的所有列
               names_to = "HID", # 新列名（来自原列名）
               values_to = "HL") %>%  # 新值列名
  mutate(group = case_when(
    HL >= 0.9 ~ "Homo",
    HL <= 0.1 ~ "WT",
    TRUE ~ "Het"  # HL在0.1到0.9之间的情况
  )
) %>% left_join(dfID1020,by="HID") %>% 
  left_join(phe_child,by="ID") 

df2 <- df1 %>% 
  select(group,Soluble_transferrin_receptor) %>% 
  filter(!is.na(Soluble_transferrin_receptor)) %>% 
  filter(Soluble_transferrin_receptor !=0)
write_tsv(df2,"D:\\biosoft\\1000thal\\mtDNA\\CYTB\\m14766.group.tsv")

# CreateResults数据框
results_wilcox <- data.frame(
  phenotype = character(),
  chi_squared = numeric(),
  p_value = numeric(),
  median_Homo = numeric(),
  median_Het = numeric(),
  median_WT = numeric(),
  stringsAsFactors = FALSE
)

### 对每个Phenotype进行 Kruskal-Wallis test ###
for(pheno in phe) {
  ### CheckPhenotypewhether or not存在且为数值型 ###
  if(pheno %in% names(df1) && is.numeric(df1[[pheno]])) {
    ### 进行 Kruskal-Wallis test ###
    kw_test <- kruskal.test(as.formula(paste(pheno, "~ group")), data = df1)
    
    ### 获取各组Median ###
    group_medians <- aggregate(as.formula(paste(pheno, "~ group")), 
                               data = df1, 
                               median, 
                               na.rm = TRUE)
    
    ### 存储Results ###
    results_wilcox <- rbind(results_wilcox, data.frame(
      phenotype = pheno,
      chi_squared = kw_test$statistic,
      p_value = kw_test$p.value,
      median_Homo = group_medians[group_medians$group == "Homo", 2],
      median_Het = group_medians[group_medians$group == "Het", 2],
      median_WT = group_medians[group_medians$group == "WT", 2]
    ))
  } else {
    warning(paste("表型", pheno, "不存在或不是数值型"))
  }
}

### Adjustp-values（Multiple-testing correction） ###
results_wilcox$p_adj <- p.adjust(results_wilcox$p_value, method = "BH")
results_wilcox$significant <- ifelse(results_wilcox$p_adj < 0.05, "Yes", "No")

### 按p-values排序 ###
results_wilcox <- results_wilcox[order(results_wilcox$p_adj), ]


median(df2[df2$group =="Het",]$Soluble_transferrin_receptor)
# Inspect results
print(results_wilcox)

### 提取显著的Phenotype ###
significant_pheno <- "Soluble_transferrin_receptor"

# Inspect该Phenotype在各组的详细统计
library(dplyr)

df1 %>%
  filter(!is.na(.data[[significant_pheno]])) %>% 
  filter(Soluble_transferrin_receptor!= 0) %>% 
  group_by(group) %>%
  summarise(
    n = n(),
    median = median(.data[[significant_pheno]], na.rm = TRUE),
    mean = mean(.data[[significant_pheno]], na.rm = TRUE),
    sd = sd(.data[[significant_pheno]], na.rm = TRUE),
    min = min(.data[[significant_pheno]], na.rm = TRUE),
    max = max(.data[[significant_pheno]], na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(group)

library(FSA)

### 进行 Dunn 两两Compare ###
dunn_results <- dunnTest(df1[[significant_pheno]] ~ as.factor(df1$group),
                         method = "bh")

print(dunn_results)

### 或者使用 pairwise.wilcox.test 进行两两 Wilcoxon test ###
pairwise_results <- pairwise.wilcox.test(df1[[significant_pheno]], 
                                         df1$group,
                                         p.adjust.method = "BH")

print(pairwise_results)

library(ggplot2)
library(ggpubr)

# Create详细的箱线图
p_sig <- ggplot(df1, aes(x = group, y = .data[[significant_pheno]], fill = group)) +
  geom_boxplot(alpha = 0.7, width = 0.6, outlier.shape = NA) +
  geom_jitter(width = 0.2, alpha = 0.6, size = 2, color = "black") +
  stat_summary(fun = median, geom = "point", shape = 18, 
               size = 4, color = "red") +
  
  # AddMedian标签
  stat_summary(fun = median, geom = "text", 
               aes(label = round(..y.., 2)),
               vjust = -1.5, size = 4, fontface = "bold") +
  
  # AddSample量标签
  geom_text(data = df1 %>% 
              group_by(group) %>% 
              summarise(n = sum(!is.na(.data[[significant_pheno]]))),
            aes(x = group, y = min(df1[[significant_pheno]], na.rm = TRUE) * 0.9,
                label = paste("n =", n)),
            vjust = 2, size = 4) +
  
  labs(
    title = "Soluble Transferrin Receptor by Genotype Group",
    subtitle = paste("Kruskal-Wallis p =", format(3.303361e-06, scientific = TRUE, digits = 3)),
    x = "Genotype Group",
    y = "Soluble Transferrin Receptor",
    caption = paste("Homo: HL ≥ 0.9, Het: 0.1 < HL < 0.9, WT: HL ≤ 0.1")
  ) +
  
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5, color = "red"),
    axis.title = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 11),
    legend.position = "none",
    panel.grid.major = element_line(color = "grey90"),
    panel.grid.minor = element_blank()
  ) +
  
  # Add p 值标记
  annotate("text", x = 2, y = max(df1[[significant_pheno]], na.rm = TRUE) * 1.15,
           label = "*** p < 0.001", color = "red", size = 5, fontface = "bold") +
  
  # Set colors
  scale_fill_manual(values = c("Homo" = "#E74C3C", "Het" = "#F39C12", "WT" = "#3498DB"))

print(p_sig)

# Save figure
ggsave("soluble_transferrin_receptor_by_group.png", 
       plot = p_sig, width = 10, height = 8, dpi = 300)