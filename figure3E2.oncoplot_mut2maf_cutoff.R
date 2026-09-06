rm(list=ls())
library(ComplexHeatmap)
library(ggplot2)
library(maftools)
library(dplyr)
library(tidyr)
library(reshape2)
library(readxl)
library(circlize)

### Write ###
# dfsum1020 <- read.table("D:\\biosoft\\1000thal\\mtDNA\\genemut\\mut_profile2het1020cutoff.tsv",sep = "\t",header = T)
# dfsum409 <- read.table("D:\\biosoft\\1000thal\\mtDNA\\genemut\\mut_profile2het409cutoff.tsv",sep = "\t",header = T)
# dfsum58 <- read.table("D:\\biosoft\\1000thal\\mtDNA\\genemut\\mut_profile2het58cutoff.tsv",sep = "\t",header = T)
dfsum1020 <- read.table("D:\\biosoft\\1000thal\\mtDNA\\genemut\\mut_profile2het1020cutoff202512.tsv",sep = "\t",header = T)
dfsum409 <- read.table("D:\\biosoft\\1000thal\\mtDNA\\genemut\\mut_profile2het409cutoff202512.tsv",sep = "\t",header = T)
dfsum58 <- read.table("D:\\biosoft\\1000thal\\mtDNA\\genemut\\mut_profile2het58cutoff202512.tsv",sep = "\t",header = T)


df2mut<- function(df){
  df1 <- as.data.frame(t(df))
  colnames(df1) <- df1[1,]
  df2 <- df1[-1,] 
  ### Remove数据框 mut 中contains任何缺失值（NA）的行 ###
  df2 <- df2[complete.cases(df2), ]
  # df2[] <- lapply(df2, remove_vari)
  df2 <- replace_muti_hit(df2)
  df2[is.na(df2)] <- ""
  return(df2)
}


replace_muti_hit <- function(df) {
  ### Iterate over数据框中的每一列 ###
  for (col in names(df)) {
    ### Checkcontaining分号的值，并Substitution为 "multi_hit" ###
    df[[col]][grepl(";", df[[col]])] <- "multi_hit"
  }
  
  ### ReturnModify后的数据框 ###
  return(df)
}



mut1020 <- df2mut(dfsum1020)
mut409 <- df2mut(dfsum409)
mut58 <- df2mut(dfsum58)



# mut <- as.data.frame(t(dfsum))
# colnames(mut) <- mut[1,]
# mut <- mut[-1,]  

# 
# # Define a function来Remove from each column "_variant"
# remove_vari <- function(x) {
#   if (is.character(x)) {
#     gsub("_variant", "", x)
#   } else {
# x  # 如果不are字符型，则直接Return原列
#   }
# }
# replace_muti_hit <- function(df) {
# # Iterate over数据框中的每一列
#   for (col in names(df)) {
# # Checkcontaining分号的值，并Substitution为 "multi_hit"
#     df[[col]][grepl(";", df[[col]])] <- "multi_hit"
#   }
#   
# # ReturnModify后的数据框
#   return(df)
# }


### Remove数据框 mut 中contains任何缺失值（NA）的行， ###
# mut <- mut[complete.cases(mut), ]
# 
# mut[] <- lapply(mut, remove_vari)
# mut <- replace_muti_hit(mut)
# mut[is.na(mut)] <- ""
### Inspect mutation types and adjust figure dimensions accordingly， ###
### Colors, etc.。We observe 4 mutation types; configure plot parameters accordingly ###

# matMuttmp = mut
# matMuttmp$gene = row.names(matMuttmp)
# mat_long <- melt(matMuttmp, id.vars = "gene", value.name = "Variant_Classification")
# levels(factor(mat_long$Variant_Classification))

## Plot and remove Samples without Mutation
mut1020 <- mut1020[, colSums(is.na(mut1020)) != nrow(mut1020)]
mut409 <- mut409[, colSums(is.na(mut409)) != nrow(mut409)]
mut58 <- mut58[, colSums(is.na(mut58)) != nrow(mut58)]
# 1. ClinicalData processing
# Clinical信息需要我们DistinguishContinuous variable、Discrete variable还areCategorical variable，
### 这样在Setlegend时颜色的Filter有非常大的不同。 ###

phe_child  <- readxl::read_excel("D:/biosoft/1000thal/千人/RNO.1 Basic statistics of 1020 β-thalassemia patients.xlsx", sheet = "Sheet1")
dfID1020 <- readxl::read_excel("D:/biosoft/1000thal/千人/ID对应(1020+409)2023.12.22.xlsx", sheet = "1020")
#
phe_child[phe_child == "NA"] <- NA
phe_child[phe_child == "Irregular transfusions"] <- NA
phe_child[phe_child == "Untransfused"] <- NA
###
phe_child[,c("ID", "Ethnic", "Sex","six_genotype_category", "HBB-HGVS",
             "HBB-classic", "HBA-HGVS", "HBA-classic", "HBB_genotype_category",
             "HBA_genotype_category", "Clinical_staging", "Transfusion_Dependence", 
             "Regular_transfusion", "Thalassaemia_face", "Jaundice", "Gallstones", 
             "Splenic", "Hepatomegaly", "Splenomegaly")] <- 
  lapply(phe_child[,c("ID", "Ethnic", "Sex","six_genotype_category", "HBB-HGVS",
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

###merge all phenotypes 
#
phe_child_merge <- phe_child %>% left_join(dfID1020,by=c("ID"))

phe_child_merge <- phe_child_merge %>% 
  mutate(age_group = case_when(
    Age < 72 ~ "Toddler(<6)",
    Age >= 72 & Age <= 144 ~ "Child(6~12)",
    Age > 144 & Age < 216 ~ "Adolescent(12~18)",
    Age >= 216 ~ "Adult(>18)"
  ))

phe_child_merge$age_group <- factor(
  phe_child_merge$age_group,
  levels = c("Toddler(<6)", "Child(6~12)", "Adolescent(12~18)", "Adult(>18)")
)

###

pdata <- subset(phe_child_merge,select=c("HID","Sex","six_genotype_category","Clinical_staging", 
                                         "Transfusion_Dependence","Survival_time_without_transfusion",
                                         "Annual_transfusions","HbF","Serum_Ferritin","age_group","Splenomegaly"))

# pdata$Serum_Ferritin <- ifelse(pdata$Serum_Ferritin<=5000,"lowSF","highSF")
pdata$Serum_Ferritin_Group <- cut(
  pdata$Serum_Ferritin,
  breaks = c(-Inf, 1500, 5000, Inf),
  labels = c("lowSF", "midSF", "highSF"),
  right = FALSE  # whether or notcontains右边界（here 1500 <= x < 5000）
)

pdata$six_genotype_category[pdata$six_genotype_category!="β0/β0"] <- "other"

# pdata$Serum_Ferritin=factor(pdata$Serum_Ferritin)
pdata$Sex = factor(pdata$Sex)
pdata$six_genotype_category = factor(pdata$six_genotype_category)
pdata$Clinical_staging = factor(pdata$Clinical_staging)
pdata$Transfusion_Dependence = factor(pdata$Transfusion_Dependence)
pdata$age_group = factor(pdata$age_group)
pdata$Splenomegaly = factor(pdata$Splenomegaly)
str(pdata)

sample_order <- pdata %>% arrange(Serum_Ferritin_Group) %>% pull(HID)
# sample_order <- pdata %>% arrange(six_genotype_category) %>% pull(HID)
# sample_order <- pdata %>% arrange(Transfusion_Dependence) %>% pull(HID)
# sample_order <- pdata %>% arrange(age_group) %>% pull(HID)
# sample_order <- pdata %>% arrange(Splenomegaly) %>% pull(HID)
#
# 2. 指定变异形状h*0.8  w-unit(0.2, "mm") h-unit(0.5, "mm")
### SpecifyVariant的形状，x,y,w,hrepresentVariant的Position(x,y)andwidth(w)，height ###
alter_fun <- list(
  background = function(x, y, w, h) {
    grid.rect(x, y, w*0.9, h, 
              gp = gpar(fill = "white", col = NA))
  },
  
  single = function(x, y, w, h) {
    grid.rect(x, y, w*0.9, h, 
              gp = gpar(fill = col["single"], col = NA))
  },
  multi = function(x, y, w, h) {
    grid.rect(x, y, w*0.9, h, 
              gp = gpar(fill = col["multi"], col = NA))
  }
  
  
  # frameshift = function(x, y, w, h) {
  #   grid.rect(x, y, w*0.9, h, 
  #             gp = gpar(fill = col["frameshift"], col = NA))
  # },
  # inframe_deletion = function(x, y, w, h) {
  #   grid.rect(x, y, w*0.9, h, 
  #             gp = gpar(fill = col["inframe_deletion"], col = NA))
  # },
  # inframe_insertion = function(x, y, w, h) {
  #   grid.rect(x, y, w*0.9, h,  
  #             gp = gpar(fill = col["inframe_insertion"], col = NA))
  # },
  # missense = function(x, y, w, h) {
  #   grid.rect(x, y, w*0.9, h,  
  #             gp = gpar(fill = col["missense"], col = NA))
  # },
  # protein_altering = function(x, y, w, h) {
  #   grid.rect(x, y, w*0.9, h, 
  #             gp = gpar(fill = col["protein_altering"], col = NA))
  # },
  # start_lost = function(x, y, w, h) {
  #   grid.rect(x, y, w*0.9, h,   
  #             gp = gpar(fill = col["start_lost"], col = NA))
  # },
  # stop_gained = function(x, y, w, h) {
  #   grid.rect(x, y, w*0.9, h, 
  #             gp = gpar(fill = col["stop_gained"], col = NA))
  # },
  # stop_lost = function(x, y, w, h) {
  #   grid.rect(x, y, w*0.9, h,  
  #             gp = gpar(fill = col["stop_lost"], col = NA))
  # },
  # multi_hit = function(x, y, w, h) {
  #   grid.rect(x, y, w*0.9, h,  
  #             gp = gpar(fill = col["multi_hit"], col = NA))
  # }
  
  
)

# 3. SpecifyVariant类型的标签，and数据中的类型对应
### Change tosingleandmulti ###
heatmap_legend_param <- list(title = "Alternations",
                             at = c("single", "multi"), 
                             labels = c("single", "multi"))
# heatmap_legend_param <- list(title = "Alternations",
#                              at = c("frameshift", "inframe_deletion","inframe_insertion", "missense","protein_altering","start_lost","stop_gained","stop_lost","multi_hit"), 
#                              labels = c("frameshift", "inframe_deletion","inframe_insertion", "missense","protein_altering","start_lost","stop_gained","stop_lost","multi_hit"))

# 4. Specify颜色
### Specify颜色includeHeatmapMutation类型的颜色以及SampleAnnotation colors， ###
# GeneAnnotation的颜色等，我们这里只对Mutation类型和Sample信息Annotation的颜色。
"#1E78B5"
### Specify颜色, Adjust颜色代码即可 ###
col <- c(single = "#4CB0C8",multi="#CD151B")

# col <- c(frameshift = "#6AB4C1", inframe_deletion = "#FFE0C1", inframe_insertion = "#FFA040", 
#          missense = "#A5C2E2", protein_altering = "black", start_lost = "#F28080",
#          stop_gained = "#A89215",stop_lost="#70B48F",multi_hit="#FF6100")
### Define Annotation info, custom colors, set colors for continuous variables (outer) ###

# col_SF = colorRamp2(c(100, 10000), c("white", "red"))


# 1. Check哪些Sample（列名）有Mutation

keep_rows <-  c("CYTB","ND5","D_loop","COX3","ND2","ND1","COX1","ND4","ATP6","RNR1","intergenic","RNR2","ND3")
mut1020_filtered <- mut1020[keep_rows, , drop = FALSE]


keep_rows409 <-  c("ND5","CYTB","COX3","ND1","D_loop","ND2","ND4","intergenic","ATP6","COX1","RNR1")
mut409_filtered <- mut409[keep_rows409, , drop = FALSE]

keep_rows58 <-  c("D_loop","ND2","ND4","ND6","COX2","intergenic","TRNL1","TRNT")
mut58_filtered <- mut58[keep_rows58, , drop = FALSE]
# 3. 同步FilterClinical数据（假设 pdata 的行名是Sample名）
# pdata_filtered <- pdata %>% 
#   filter(HID %in% non_empty_samples)



# 5. SetSampleAnnotation
### Use HeatmapAnnotation to set sample annotation，as follows: ###
ha <- HeatmapAnnotation(
  SF = pdata$Serum_Ferritin_Group,
  Genotype = pdata$six_genotype_category,
  TD = pdata$Transfusion_Dependence,
  # age = pdata$age_group,
  Splec = pdata$Splenomegaly,
  # col = list(SF = col_SF),
  col = list(
    SF = c("lowSF" = "#F2A1A2","midSF" = "#7DC69B","highSF" = "#98D7F3"),
    Genotype=c("β0/β0" = "#CAC1C4","other" = "#FCE6CF"),
    TD = c("TDT" = "#D5EAD9","NTDT" = "#B092B6"),
    Splec = c("Normal" = "#EECA40","Splenomegaly" = "#FD763F","Splenectomy"="#23BAC5")
    # age=c("Toddler(<6)" = "green","Child(6~12)" = "red","Adolescent(12~18)" = "yellow","Adult(>18)" = "black")
  ),
  show_annotation_name = TRUE,
  annotation_name_gp = gpar(fontsize = 7))


# 6. Set title
column_title <- "This is Oncoplot"


# 7. Simple waterfall plot
### Simple waterfall plot只includeHeatmap部分，legend就areMutation类型，as follows: ###

oncoPrint1020 <- oncoPrint(mut1020, alter_fun = alter_fun, col = col, alter_fun_is_vectorized = FALSE)
oncoPrint1020
oncoPrint1020_filtered <- oncoPrint(mut1020_filtered, alter_fun = alter_fun, col = col, alter_fun_is_vectorized = FALSE)
oncoPrint1020_filtered


oncoPrint409 <- oncoPrint(mut409, alter_fun = alter_fun, col = col, alter_fun_is_vectorized = FALSE)
oncoPrint409
oncoPrint409_filtered <- oncoPrint(mut409_filtered, alter_fun = alter_fun, col = col, alter_fun_is_vectorized = FALSE)
oncoPrint409_filtered

oncoPrint58 <- oncoPrint(mut58, alter_fun = alter_fun, col = col, alter_fun_is_vectorized = FALSE)
oncoPrint58
oncoPrint58_filtered <- oncoPrint(mut58_filtered, alter_fun = alter_fun, col = col, alter_fun_is_vectorized = FALSE)
oncoPrint58_filtered
### Write output ###

# pdf("D:\\biosoft\\1000thal\\mtDNA\\figure/onco瀑布图het1020cutoff.pdf", width = 10, height = 8)
# # Plot heatmap
# draw(oncoPrint1020)
# # Close PDF device
# dev.off()

# pdf("D:\\biosoft\\1000thal\\mtDNA\\figure/onco瀑布图het1020 filtered cutoff202512.pdf", width = 10, height = 8)
# # Plot heatmap
# draw(oncoPrint1020_filtered)
# # Close PDF device
# dev.off()
# 
# pdf("D:\\biosoft\\1000thal\\mtDNA\\figure/onco瀑布图het409cutoff.pdf202512", width = 10, height = 8)
# # Plot heatmap
# draw(oncoPrint409)
# # Close PDF device
# dev.off()
# 
# 
# pdf("D:\\biosoft\\1000thal\\mtDNA\\figure/onco瀑布图het409 filtered cutoff202512.pdf", width = 10, height = 8)
# # Plot heatmap
# draw(oncoPrint409_filtered)
# # Close PDF device
# dev.off()

# pdf("D:\\biosoft\\1000thal\\mtDNA\\figure/onco瀑布图het58 filtered cutoff202512.pdf", width = 10, height = 8)
# # Plot heatmap
# draw(oncoPrint58)
# # Close PDF device
# dev.off()
# 8. AddAnnotation
# Add样子AnnotationResults，as follows:
# oncoPrint(mat = mut,
# bottom_annotation = ha, #Annotation信息在底部
#           #   top_annotation=top_annotation,
#           #right_annotation=NULL,
#           alter_fun = alter_fun, 
#           col = col,  column_order = sample_order,
#           column_title = column_title, 
#           heatmap_legend_param = heatmap_legend_param,
#           row_names_side = "left",
#           pct_side = "right",
#           # column_order=sample_order,
#           #       column_split=3
#           alter_fun_is_vectorized = FALSE
# )



# 9. AdjustAnnotation的Position
### Waterfall plot提供三种Annotation方式，一种就areMutation类型的Annotation，而另一种就areSampleAnnotation， ###
### 当然Gene也可以Annotation，而Annotation的Position需要根据draw给出来的参数自行Adjust，放在Waterfall plot的上下左右等四个Position， ###
### 举几个例子稍微说明一下。Firstare通过oncoPrint函数获得绘图参数，as follows: ###

oncoplot_anno <- oncoPrint(mut1020_filtered,
                           bottom_annotation = ha, #Annotation信息在底部
                           #   top_annotation=top_annotation,
                           #right_annotation=NULL,
                           alter_fun = alter_fun, 
                           col = col,  
                           column_title = "", 
                           remove_empty_rows = TRUE,  # 自动Remove全空列
                           remove_empty_columns = TRUE, 
                           heatmap_legend_param = heatmap_legend_param,
                           row_names_side = "left",
                           pct_side = "right",
                           # column_order=sample_order, #order
                           #       column_split=3
                           alter_fun_is_vectorized = FALSE
)

### 将Sample的Annotation放在左边，as follows: ###
draw(oncoplot_anno, annotation_legend_side = "left")

# pdf("D:\\biosoft\\1000thal\\mtDNA\\genemut\\onco瀑布图ano0.05_SF.pdf", width = 10, height = 8)
pdf("D:\\biosoft\\1000thal\\mtDNA\\figure/onco瀑布图het1020ano.pdf", width = 10, height = 8)
# Plot heatmap
draw(oncoplot_anno, annotation_legend_side = "left")
### Close PDF device ###
dev.off()


### SampleAnnotation放在左边Position，而Mutation类型也就areHeatmapAnnotation放在右边，as follows: ###

draw(oncoplot_anno, annotation_legend_side = "left", heatmap_legend_side = "right")

# 
# # AdjustPosition与上面Position正好相反，as follows:
# draw(oncoplot_anno, annotation_legend_side = "right", heatmap_legend_side = "left")
# 
# # 将热图注释放在瀑布图的最下面，并通过参数align_heatmap_legend调整位于中央位置，如下：
# draw(oncoplot_anno, annotation_legend_side = "right", heatmap_legend_side = "bottom",
#      align_heatmap_legend = "global_center")
# 
