# Load所需库
rm(list = ls())
library(readxl)
library(writexl)
library(openxlsx)
library(Hmisc)
library(dplyr)
library(linkET)
library(ggplot2)

### 读取数据 ###
phe_child <- readxl::read_excel("D:/biosoft/1000thal/千人/RNO.1 Basic statistics of 1020 β-thalassemia patients.xlsx", sheet = "Sheet1")
dfID1020 <- read_excel("D:/biosoft/1000thal/千人/ID对应(1020+409)2023.12.22.xlsx", sheet = "1020")
dfmt1 <- read.table("D:/biosoft/1000thal/mtDNA/1020mtDNA_out/1020.mtCN_mean.tsv", header = T,sep = "\t") %>% 
  mutate(mtCN = mtCN_mean)
dfmt11 <- read.table("D:/biosoft/1000thal/mtDNA/1020mtDNA_out/1020.mtCN_median.tsv", header = T,sep = "\t")
###
phe_child[phe_child == "Irregular transfusions"] <- NA
phe_child[phe_child == "Untransfused"] <- NA
phe_child[phe_child == "NA"] <- NA
phe_child$Date_of_birth <- convertToDateTime(phe_child$Date_of_birth)
###
phe_child$Assessment_time <- convertToDateTime(phe_child$Assessment_time)


### 生成assessment_centre ###
phe_child<-phe_child %>% mutate(assessment_centre = substring(`ID`, 1, 2))
### 生成sex列 ###

phe_child[,c("Ethnic", "Sex", "assessment_centre")] <- lapply(phe_child[,c("Ethnic", "Sex", "assessment_centre")], as.factor)
### 生成province ###
phe_child<-phe_child %>% mutate(Province = substring(`ID`, 1, 2))
###

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

###

###merge all phenotypes 
#
phe_child_merge <- phe_child %>% left_join(dfID1020,by=c("ID")) %>% 
  left_join(dfmt1,by=c("HID")) %>% 
  left_join(dfmt11,by=c("HID")) %>% 
  select(-c("NUC_mean_coverage.y")) %>% 
  rename("NUC_mean_coverage" = "NUC_mean_coverage.x")
# #

##
blood_phenos <- c( "Red_blood_cell_count","Mean_corpuscular_volume",
                   "Mean_corpuscular_hemoglobin",
                   "Mean_corpuscular_hemoglobin_concentration",
                   
                   "Percentage_of_reticulocytes_with_low_fluorescence_intensity",
                   "Median_fluorescence_Intensity_Reticulocyte_ratio",
                   "Percentage_of_reticulocytes_with_high_fluorescence_intensity",
                   
                   "Percentage_of_basophils",
                   "Percentage_of_eosinophils",
                   "Percentage_of_lymphocytes",
                   "Percentage_of_neutrophils",
                   "Percentage_of_monocytes",
                   "Percentage_of_immature_reticulocytes",
                   "Percentage_of_reticulocytes",
                   
                   "Mean_Platelet_volume",
                   "Platelet_specific_volume",
                   "Platelet_distribution_width")

iron_phenos <- c("Iron","Serum_Ferritin","Transferrin",
                 "Soluble_transferrin_receptor","Transferrin_Saturation",
                 "Unsaturated_iron_binding_capacity","Total_iron_binding_capacity")
mito_phenos <- c( "MT_mean_coverage", "NUC_mean_coverage", "mtCN", 
                  "MT_median_coverage", "mtCN_median")
thal_phenos <- c("Survival_time_without_transfusion","Annual_transfusions","six_genotype_category",
                 "Clinical_staging", "Transfusion_Dependence","Thalassaemia_face", "Jaundice",
                 "Gallstones","Splenic", "Hepatomegaly", "Splenomegaly", "HbF")
basic_phenos <- c("ID", "Assessment_time", "Ethnic", "Sex", "Age", "assessment_centre")

### 提取目标因子and环境因子 ###
target_factors <- c("mtCN", "HbF")
env_factors <- c("Survival_time_without_transfusion","Annual_transfusions", iron_phenos)

df <- phe_child_merge %>% 
  select(all_of(c(target_factors,env_factors)))

### remove列名的_为空格 ###
target_factors <-gsub("_", " ", target_factors)
env_factors  <-gsub("_", " ", env_factors)
names(df) <- gsub("_", " ", names(df))
### 分离环境因子数据 ###
env <- df [, env_factors]
### 计算相关性矩阵and P 值矩阵 ###
df_rcorr <- rcorr(as.matrix(df), type = "pearson")

r <- df_rcorr$r  # 相关系数矩阵
p <- df_rcorr$P  # P 值矩阵

### 初始化Results数据框 ###
results <- data.frame(P = character(), R = numeric(), Y = character(), X = character(), sig = character(), type = character(), stringsAsFactors = FALSE)

### Iterate over每个目标因子and环境因子的组合，计算相关性 ###

for (y in target_factors) {
  for (x in env_factors) {
    r_value <- r[x, y]
    p_value <- p[x, y]
    
    ### 确定显著性水平and相关性方向 ###
    significance <- ifelse(p_value < 0.05, "< 0.05", ">= 0.05")
    sig <- ifelse(r_value > 0, ">0", "<0")
    type <- ifelse(r_value > 0, "r > 0", "r < 0")
    
    # AddResults到数据框
    results <- rbind(results, data.frame(P = significance,
                                         R = round(r_value, 2),
                                         Y = y, X = x, sig = sig, type = type))
  }
}

# Inspect results
print(results)

# Set colors映射
cols <- c(">= 0.05" = "grey", "< 0.05" = "#1B9E77", "< 0.01" = "#D95F02")
#

# Plot相关性图，带Significance markers
qcorrplot(correlate(env), type = "lower", diag = FALSE) +
  geom_square() +  
  geom_text(aes(label = ifelse(p < 0.001, "***",
                               ifelse(p < 0.01, "**",
                                      ifelse(p < 0.05, "*", "")))),
            size = 5, color = "black") +  # 根据 p 值Set星号
  geom_couple(aes(    colour = P,          # 使用显著性 P 值来Set线的颜色
                      size = abs(R),       # 根据相关系数 R 的绝对值Adjust线条粗细
                      linetype = type,     # 根据 type 列选择实线或虚线
                      from = Y, to = X     # from 和 to 指定因变量和环境因子
                      ), data = results, curvature = 0.15) +  
  scale_fill_gradientn(colours = RColorBrewer::brewer.pal(3, "RdBu")) + 
  scale_size_continuous(range = c(0.1, 1)) +  # Set线条粗细范围
  scale_colour_manual(values = cols) +
  scale_linetype_manual(values = c("r > 0" = "solid", "r < 0" = "dashed")) + 
  # Set线型映射
  guides(size = guide_legend(title = "Correlation Strength", 
                                 override.aes = list(colour = "grey35"), order = 2),
         colour = guide_legend(title = "P value",
                               override.aes = list(size = 3), order = 1),
         fill = guide_colorbar(title = "Pearson's r", order = 3),
         linetype = guide_legend(title = "Line Type", order = 4)  ) +
  theme_minimal() +
  theme(panel.grid = element_blank(),  # Remove网格
        axis.text.y = element_text(color = "black",face = 'bold'),
        axis.text.x = element_text(angle = 45, hjust = 1, face = 'bold')) +  # x轴标签旋转
  labs(title = NULL,
       x = NULL, y =NULL)

ggsave("D:\\biosoft\\1000thal\\mtDNA\\figure/figure4D.mtCN.vs.HbF.Correlation_with_clinical_and_iron_related_phenotypes.pdf",width = 10,height = 7,device="pdf")

 
