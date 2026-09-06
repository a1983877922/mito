rm(list=ls())#clear Global Environment
# Load packages
library(ggplot2) # Create Elegant Data Visualisations Using the Grammar of Graphics
library(ggsignif) # Significance Brackets for 'ggplot2'
library(gghalves) # Compose Half-Half Plots Using Your Favourite Geoms
library(dplyr)
library(patchwork)
library(readxl)
library(ggstatsplot)
library(tidyr)
library(ggpubr)
library(Cairo)
### data ###
# dfdnm <- read.table("D:/biosoft/1000thal/mtDNA/1020 DNM per sample.tsv",header = T)
dfmutation <- read.table("D:/biosoft/1000thal/mtDNA/1020mutations per sample cutoff.tsv",header = T) %>% 
  rename(HID = id)

dfiron <- read_excel("D:/biosoft/1000thal/mtDNA/manuscript/iron of 1020 β-thalassemia patients.xlsx") 
dfmt <- read.table("D:/biosoft/1000thal/mtDNA/manuscript/1020.mtCN_mean.tsv",header = T,sep = "\t") 
dfmt409 <- read.table("D:/biosoft/1000thal/mtDNA/manuscript/409.mtCN_mean.tsv",header = T,sep = "\t") 
dfmtadj <- read.table("D:/biosoft/1000thal/mtDNA/thala_mtcn_corrected_raw_lasso_add_homoglobin.tsv",header = T,sep = "\t") 
dfID1020 <- read_excel("D:/biosoft/1000thal/千人/ID对应(1020+409)2023.12.22.xlsx", sheet = "1020")
phe_child <- readxl::read_excel("D:/biosoft/1000thal/千人/RNO.1 Basic statistics of 1020 β-thalassemia patients.xlsx", sheet = "Sheet1")

colnames(dfmtadj)
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
dput(colnames(dfmutation))

counts <- c("counts_snv", "counts_snv_cut1", "counts_snv_cut2", 
  "counts_snv_cut3", "counts_snv_cut4", "counts_indel", "counts_indel_cut1", 
  "counts_indel_cut2", "counts_indel_cut3", "counts_indel_cut4", 
  "counts_ox", "counts_ox_cut1", "counts_ox_cut2", "counts_ox_cut3", 
  "counts_ox_cut4", "counts_ncr", "counts_ncr_cut1", "counts_ncr_cut2", 
  "counts_ncr_cut3", "counts_ncr_cut4")

input <- dfmutation %>% 
  left_join(dfID1020,by=c("HID")) %>% 
  left_join(dfmt,by=c("HID")) %>% 
  left_join(dfmtadj,by=c("ID")) %>% 
  rename("mtCN,raw" ="mtCN_mean.x") %>% 
  select(!c("mtCN_mean.y","log_mtcn","log_pred","log_resid","mtCN_adj")) %>% 
  rename("mtCN,adj"="mtCN_adj2") %>% 
  left_join(phe_child,by=c("ID")) %>% 
  select(
    all_of(c(c("HID", "ID", "BID",  "MT_mean_coverage", "NUC_mean_coverage", "mtCN,raw", 
      "mtCN,adj",  "Survival_time_without_transfusion", "Sex",
      "Annual_transfusions", "six_genotype_category",  "HBB_genotype_category",
      "Clinical_staging", "Transfusion_Dependence",  
      "Thalassaemia_face", "Jaundice", "Gallstones",  "Splenic", 
      "Splenomegaly", "HbF","Fetal_hemoglobin", 
      "Unsaturated_iron_binding_capacity", 
      "Soluble_transferrin_receptor", "Iron", "Serum_Ferritin", 
      "Transferrin","Transferrin_Saturation", "Total_iron_binding_capacity"),counts)
      
      )
  ) %>% 
  mutate(
    `Iron overload` = case_when(
      Serum_Ferritin > 5000 ~ "Iron overload (>5000)",
      Serum_Ferritin < 1500 ~ "Iron overload (<1500)",
      .default = "Iron overload (1500~5000)"
    )
  )


input$`Iron overload` <- factor(
  input$`Iron overload`,  # 需要因子化的列
  levels = c(
    "Iron overload (<1500)",        # 第一级：轻度
    "Iron overload (1500~5000)",    # 第二级：中度
    "Iron overload (>5000)"         # 第三级：重度
  ),
  ordered = TRUE                   # Set为有序因子
)
input$`Splenomegaly` <- factor(
  input$`Splenomegaly`,  # 需要因子化的列
  levels = c(
    "Normal",        # 第一级：轻度
    "Splenomegaly",    # 第二级：中度
    "Splenectomy"         # 第三级：重度
  ),
  ordered = TRUE                   # Set为有序因子
)
input$sum_counts <- input$counts_snv + input$counts_indel

### Iron overload ###
my_comparisons=list(c("Iron overload (<1500)","Iron overload (1500~5000)"),
                    c("Iron overload (1500~5000)","Iron overload (>5000)"),
                    c("Iron overload (<1500)","Iron overload (>5000)"))
p12 <- ggbetweenstats(
  data = input,
  x = `Iron overload`,
  y = counts_ox,
  plot.type = "boxviolin",
  bf.message = F,
  var.equal = TRUE,
  # type = "parametric",
  # type = "nonparametric",
  pairwise.display = "significant",#significant
  p.adjust.method = "none",
  title = NULL, #"Distribution of sepal length across Iris species",
  xlab = "",
  ylab = "number of mtDNA mutations (n)",
  results.subtitle = T,#决定是否将Statistical test的Results显示为副标题（默认TRUE）;如果设置为FALSE,则仅返回绘图
  subtitle = NULL,#副标题,默认显示统计test results,自定义则results.subtitle=FALSE
  outlier.tagging = TRUE,#whether or not标记离群异常值，默认FALSE
  outlier.shape = 19,#异常值形状,可Set为NA将其隐藏（不are删除，因此不会影响统计test results）
  outlier.color = "pink",#异常值颜色
  outlier.label.args = list(size = 4),#异常值标签大小
  point.args = list(position = ggplot2::position_jitterdodge(dodge.width = 0.6),
                    alpha = 0.5, size = 3, stroke = 0),#传递给geom_point的参数Set
  ggplot.component = list(
    scale_x_discrete(labels = c("Iron overload (<1500)", "Iron overload (1500~5000)","Iron overload (>5000)")),
    theme(axis.text= element_text(size = 12, face = "bold"),
          axis.title.y.right = element_blank(), 
          axis.text.y.right = element_blank(), 
          axis.ticks.y.right = element_blank(), 
          axis.title.y = element_text(face = "bold", size = 20))
  ),
  violin.args = list(width = 0.4, alpha = 0.2),#传递给geom_violin的参数Set
  ggtheme = theme_classic(),#主题修改，可直接调用ggplot2的主题，默认主题为ggstatsplot::theme_ggstatsplot()
  package = "ggsci",#提取调色板所需的包
  palette = "uniform_startrek"#选择提取包中的调色板
)
p12
# ggsave(p12,filename ="D:\\biosoft\\1000thal\\mtDNA\\figure/figure5B.number of mutation Across Iron Overload Severity.pdf",
#        device = pdf,width = 10,height = 8,dpi = 600)

### sex ###

p22 <- ggbetweenstats(
  data = input,
  x = `Sex`,
  y = counts_ox,
  plot.type = "boxviolin",
  bf.message = F,
  var.equal = TRUE,
  # type = "parametric",
  # type = "nonparametric",
  pairwise.display = "significant",#significant
  p.adjust.method = "none",
  title = NULL, #"Distribution of sepal length across Iris species",
  xlab = "",
  ylab = "number of mtDNA mutations (n)",
  results.subtitle = T,#决定是否将Statistical test的Results显示为副标题（默认TRUE）;如果设置为FALSE,则仅返回绘图
  subtitle = NULL,#副标题,默认显示统计test results,自定义则results.subtitle=FALSE
  outlier.tagging = TRUE,#whether or not标记离群异常值，默认FALSE
  outlier.shape = 19,#异常值形状,可Set为NA将其隐藏（不are删除，因此不会影响统计test results）
  outlier.color = "pink",#异常值颜色
  outlier.label.args = list(size = 4),#异常值标签大小
  point.args = list(position = ggplot2::position_jitterdodge(dodge.width = 0.6),
                    alpha = 0.5, size = 3, stroke = 0),#传递给geom_point的参数Set
  ggplot.component = list(
    scale_x_discrete(labels = c("Female", "Male")),
    theme(axis.text= element_text(size = 12, face = "bold"),
          axis.title.y.right = element_blank(), 
          axis.text.y.right = element_blank(), 
          axis.ticks.y.right = element_blank(), 
          axis.title.y = element_text(face = "bold", size = 20))
  ),
  violin.args = list(width = 0.4, alpha = 0.2),#传递给geom_violin的参数Set
  ggtheme = theme_classic(),#主题修改，可直接调用ggplot2的主题，默认主题为ggstatsplot::theme_ggstatsplot()
  package = "ggsci",#提取调色板所需的包
  palette = "uniform_startrek"#选择提取包中的调色板
)
p22 
# ggsave(p22,filename ="D:\\biosoft\\1000thal\\mtDNA\\figure/figure5B.number of mutations Across Sex.pdf",
#        device = pdf,width = 10,height = 8,dpi = 600)
### Splenomegaly ###

p32 <- ggbetweenstats(
  data = input,
  x = `Splenomegaly`,
  y = counts_ox,
  plot.type = "boxviolin",
  bf.message = F,
  var.equal = TRUE,
  # type = "parametric",
  # type = "nonparametric",
  pairwise.display = "significant",#significant
  p.adjust.method = "none",
  title = NULL, #"Distribution of sepal length across Iris species",
  xlab = "",
  ylab = "number of mtDNA mutations (n)",
  results.subtitle = T,#决定是否将Statistical test的Results显示为副标题（默认TRUE）;如果设置为FALSE,则仅返回绘图
  subtitle = NULL,#副标题,默认显示统计test results,自定义则results.subtitle=FALSE
  outlier.tagging = TRUE,#whether or not标记离群异常值，默认FALSE
  outlier.shape = 19,#异常值形状,可Set为NA将其隐藏（不are删除，因此不会影响统计test results）
  outlier.color = "pink",#异常值颜色
  outlier.label.args = list(size = 4),#异常值标签大小
  point.args = list(position = ggplot2::position_jitterdodge(dodge.width = 0.6),
                    alpha = 0.5, size = 3, stroke = 0),#传递给geom_point的参数Set
  ggplot.component = list(
    scale_x_discrete(labels = c("Normal", "Splenomegaly", "Splenectomy")),
    theme(axis.text= element_text(size = 12, face = "bold"),
          axis.title.y.right = element_blank(), 
          axis.text.y.right = element_blank(), 
          axis.ticks.y.right = element_blank(), 
          axis.title.y = element_text(face = "bold", size = 20))
  ),
  violin.args = list(width = 0.4, alpha = 0.2),#传递给geom_violin的参数Set
  ggtheme = theme_classic(),#主题修改，可直接调用ggplot2的主题，默认主题为ggstatsplot::theme_ggstatsplot()
  package = "ggsci",#提取调色板所需的包
  palette = "uniform_startrek"#选择提取包中的调色板
)
p32
# ggsave(p32,filename ="D:\\biosoft\\1000thal\\mtDNA\\figure/figure5B.number of mutations Across Splenomegaly.pdf",
#        device = pdf,width = 10,height = 8,dpi = 600)
### six genotype category ###

p42 <- ggbetweenstats(
  data = input,
  x = `six_genotype_category`,
  y = counts_ox,
  plot.type = "boxviolin",
  bf.message = F,
  var.equal = TRUE,
  # type = "parametric",
  # type = "nonparametric",
  pairwise.display = "significant",#significant
  p.adjust.method = "none",
  title = NULL, #"Distribution of sepal length across Iris species",
  xlab = "",
  ylab = "number of mtDNA mutations (n)",
  results.subtitle = F,#决定是否将Statistical test的Results显示为副标题（默认TRUE）;如果设置为FALSE,则仅返回绘图
  subtitle = NULL,#副标题,默认显示统计test results,自定义则results.subtitle=FALSE
  outlier.tagging = TRUE,#whether or not标记离群异常值，默认FALSE
  outlier.shape = 19,#异常值形状,可Set为NA将其隐藏（不are删除，因此不会影响统计test results）
  outlier.color = "pink",#异常值颜色
  outlier.label.args = list(size = 4),#异常值标签大小
  point.args = list(position = ggplot2::position_jitterdodge(dodge.width = 0.6),
                    alpha = 0.5, size = 3, stroke = 0),#传递给geom_point的参数Set
  ggplot.component = list(
    scale_x_discrete(labels = c("β-thal and α-thal", "β+/β+", "β0/HPFP","β0/β+","β0/β0","β0/βN or β0/βN co-ααα")),
    theme(axis.text= element_text(size = 12, face = "bold"),
          axis.title.y.right = element_blank(), 
          axis.text.y.right = element_blank(), 
          axis.ticks.y.right = element_blank(), 
          axis.title.y = element_text(face = "bold", size = 20))
  ),
  violin.args = list(width = 0.4, alpha = 0.2),#传递给geom_violin的参数Set
  ggtheme = theme_classic(),#主题修改，可直接调用ggplot2的主题，默认主题为ggstatsplot::theme_ggstatsplot()
  package = "ggsci",#提取调色板所需的包
  palette = "uniform_startrek"#选择提取包中的调色板
)

p42

# ggsave(p42,filename ="D:\\biosoft\\1000thal\\mtDNA\\figure/figure5B.number of mutations across six genotype_category.pdf",
#        device = pdf,width = 10,height = 8,dpi = 600)
### Transfusion_Dependence ###


p52 <- ggbetweenstats(
  data = input,
  x = `Transfusion_Dependence`,
  y = counts_ox,
  plot.type = "boxviolin",
  bf.message = F,
  var.equal = TRUE,
  # type = "parametric",
  # type = "nonparametric",
  pairwise.display = "significant",#significant
  p.adjust.method = "none",
  title = NULL, #"Distribution of sepal length across Iris species",
  xlab = "",
  ylab = "number of mtDNA mutations (n)",
  results.subtitle = T,#决定是否将Statistical test的Results显示为副标题（默认TRUE）;如果设置为FALSE,则仅返回绘图
  subtitle = NULL,#副标题,默认显示统计test results,自定义则results.subtitle=FALSE
  outlier.tagging = TRUE,#whether or not标记离群异常值，默认FALSE
  outlier.shape = 19,#异常值形状,可Set为NA将其隐藏（不are删除，因此不会影响统计test results）
  outlier.color = "pink",#异常值颜色
  outlier.label.args = list(size = 4),#异常值标签大小
  point.args = list(position = ggplot2::position_jitterdodge(dodge.width = 0.6),
                    alpha = 0.5, size = 3, stroke = 0),#传递给geom_point的参数Set
  ggplot.component = list(
    scale_x_discrete(labels = c("NTDT", "TDT")),
    theme(axis.text= element_text(size = 12, face = "bold"),
          axis.title.y.right = element_blank(), 
          axis.text.y.right = element_blank(), 
          axis.ticks.y.right = element_blank(), 
          axis.title.y = element_text(face = "bold", size = 20))
  ),
  violin.args = list(width = 0.4, alpha = 0.2),#传递给geom_violin的参数Set
  ggtheme = theme_classic(),#主题修改，可直接调用ggplot2的主题，默认主题为ggstatsplot::theme_ggstatsplot()
  package = "ggsci",#提取调色板所需的包
  palette = "uniform_startrek"#选择提取包中的调色板
)
# +
#   geom_signif(comparisons = list(c("Thalassemia", "Carriers")), test.args = list(exact = FALSE))

p52
# ggsave(p52,filename ="D:\\biosoft\\1000thal\\mtDNA\\figure/figure5B.number of mutations Transfusion Dependence.pdf",
#        device = pdf,width = 10,height = 8,dpi = 600)
### HBB genotype category ###

inputsub <- input %>% 
  filter(HBB_genotype_category %in% c("β0/β+","β0/β0","β+/β+")) %>% 
  filter(Splenomegaly != "Splenectomy")

p62 <- ggbetweenstats(
  data = inputsub,
  x = `HBB_genotype_category`,
  y = "mtCN,adj",
  plot.type = "boxviolin",
  bf.message = F,
  var.equal = TRUE,
  # type = "parametric",
  # type = "nonparametric",
  pairwise.display = "significant",#significant
  p.adjust.method = "none",
  title = NULL, #"Distribution of sepal length across Iris species",
  xlab = "",
  # ylab = "number of mtDNA mutations (n)",
  ylab = "mtCN,adj",
  results.subtitle = T,#决定是否将Statistical test的Results显示为副标题（默认TRUE）;如果设置为FALSE,则仅返回绘图
  subtitle = NULL,#副标题,默认显示统计test results,自定义则results.subtitle=FALSE
  outlier.tagging = TRUE,#whether or not标记离群异常值，默认FALSE
  outlier.shape = 19,#异常值形状,可Set为NA将其隐藏（不are删除，因此不会影响统计test results）
  outlier.color = "pink",#异常值颜色
  outlier.label.args = list(size = 4),#异常值标签大小
  point.args = list(position = ggplot2::position_jitterdodge(dodge.width = 0.6),
                    alpha = 0.5, size = 3, stroke = 0),#传递给geom_point的参数Set
  ggplot.component = list(
    scale_x_discrete(labels = c("β+/β+", "β0/β+","β0/β0")),
    theme(axis.text= element_text(size = 12, face = "bold"),
          axis.title.y.right = element_blank(), 
          axis.text.y.right = element_blank(), 
          axis.ticks.y.right = element_blank(), 
          axis.title.y = element_text(face = "bold", size = 20))
  ),
  violin.args = list(width = 0.4, alpha = 0.2),#传递给geom_violin的参数Set
  ggtheme = theme_classic(),#主题修改，可直接调用ggplot2的主题，默认主题为ggstatsplot::theme_ggstatsplot()
  package = "ggsci",#提取调色板所需的包
  palette = "uniform_startrek"#选择提取包中的调色板
)

p62
# ggsave(p62,filename ="D:\\biosoft\\1000thal\\mtDNA\\figure/figure5B.number of mutations HBB genotype_category.pdf",
#        device = pdf,width = 10,height = 8,dpi = 600)
ggsave(p62,filename ="D:\\biosoft\\1000thal\\mtDNA\\figure/figure5B.mtCN,adj HBB genotype_category.pdf",
       device = pdf,width = 10,height = 8,dpi = 600)
### ggscatterhis ###
# # 1. Create画布
# Cairo::CairoPDF( 
#   file = "D:\\biosoft\\1000thal\\mtDNA\\figure/mtCN,adj across six genotype category.pdf", # 文件名称
# width = 8,           # 宽
# height = 6,          # 高
#   bg = "transparent",
# dpi = 300)           # 分辨率
# # 2. 绘图
# p1 
# # 3. 关闭画布
# dev.off() 
c("HID", "counts_ox", "counts_indel", "ID", "BID", "counts_dnm", 
  "counts_inherit", "MT_mean_coverage", "NUC_mean_coverage", "mtCN,raw", 
  "mtCN,adj",  "Survival_time_without_transfusion", "Sex",
  "Annual_transfusions", "six_genotype_category",  "HBB_genotype_category",
  "Clinical_staging", "Transfusion_Dependence",  
  "Thalassaemia_face", "Jaundice", "Gallstones",  "Splenic", 
  "Splenomegaly", "HbF","Fetal_hemoglobin", 
  "Unsaturated_iron_binding_capacity", 
  "Soluble_transferrin_receptor", "Iron", "Serum_Ferritin", 
  "Transferrin","Transferrin_Saturation", "Total_iron_binding_capacity")

# mtCN,adj,  
# Iron,Serum_Ferritin,Transferrin,Transferrin_Saturation

p7 <- ggstatsplot::ggscatterstats		(
  input, 
  x = "mtCN,adj", 
  y = "sum_counts",
  type = "nonparametric",
  color = "#00AFBB", size = 3, alpha = 0.6,
  margin.params = list(fill ="#FC4E07", color = "black", size = 0.2),
  xlab = "Soluble transferrin receptor",
  ylab = "umber of mtDNA mutations (n)",
)
p7
# df7 <- extract_stats(p7)
# df71 <- df7$subtitle_data
# df71
p71 <- ggstatsplot::ggscatterstats(
  data = input, 
  x = "mtCN,adj", 
  y = "counts_ox",
  type = "nonparametric",
  color = "#00AFBB", 
  size = 3, 
  alpha = 0.6,
  xlab = "Soluble transferrin receptor",
  ylab = "Number of mtDNA mutations (n)",
  
  # 🎯 Adjust X 轴边际直方图（顶部）
  xsidehistogram.args = list(
    bins = 80,               # 分箱数量（或使用 `binwidth`）
    fill = "#FC4E07",        # 填充颜色
    color = "black",         # 边框颜色
    alpha = 0.6,             # 透明度
    linewidth = 0.2          # 边框粗细
  ),
  
  # 🎯 Adjust Y 轴边际直方图（右侧）
  ysidehistogram.args = list(
    bins = 80,               # 分箱数量（或 `binwidth = 1`）
    fill = "#FC4E07", 
    color = "black",
    alpha = 0.6,
    linewidth = 0.2
  )
)
p71
p71 <- ggscatterstats(
  data = input,
  x = "mtCN,adj",
  y = "counts_ox",
  type = "nonparametric",
  color = "#00AFBB",
  size = 3,
  alpha = 0.6,
  xlab = "Soluble transferrin receptor",
  ylab = "Number of mtDNA mutations (n)",
  marginal  = FALSE  # 关闭默认边际图
) +
  # AddDensity曲线（折线图）
  ggside::geom_xsidedensity(
    aes(y = after_stat(density)),
    color = "#FC4E07",
    linewidth = 1,
    fill = NA  # without填充
  ) +
  ggside::geom_ysidedensity(
    aes(x = after_stat(density)),
    color = "#FC4E07",
    linewidth = 1,
    fill = NA
  ) +
  ### Adjust坐标轴Ratio ###
  ggside::scale_xsidey_continuous(expand = c(0, 0)) +
  ggside::scale_ysidex_continuous(expand = c(0, 0))
p71

p71 <- ggstatsplot::ggscatterstats(
  input,
  x = "p71",
  y = "counts_ox",
  type = "nonparametric",
  color = "#00AFBB", size = 3, alpha = 0.6,
  margin.params = list(fill = "#FC4E07", color = "black", size = 0.2),
  xlab = "Soluble transferrin receptor",
  ylab = "Number of mtDNA mutations (n)",
  # Modify边际直方图参数
  marginal = FALSE  # 先关闭默认的边际图
) +
  ### 手动Add边际直方图，自定义分箱 ###
  ggside::geom_xsidehistogram(
    bins = 20,  # 或使用 binwidth = 0.5
    fill = "#FC4E07", color = "black", alpha = 0.6
  ) +
  ggside::geom_ysidehistogram(
    bins = 15,  # 或使用 binwidth = 1
    fill = "#FC4E07", color = "black", alpha = 0.6
  ) +
  ggside::scale_xsidey_continuous(expand = c(0, 0)) +  # Adjust坐标轴
  ggside::scale_ysidex_continuous(expand = c(0, 0))


p71

p72 <- ggpubr::ggscatterhist			(
  input, 
  x = "mtCN,raw", 
  y = "counts_ox",
  color = "#00AFBB", size = 3, alpha = 0.6,
  stat_xsidebin(binwidth = 10),
  margin.params = list(fill ="#FC4E07", color = "black", size = 0.2),
  xlab = "Soluble transferrin receptor",
  ylab = "umber of mtDNA mutations (n)",
)
p72

