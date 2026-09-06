# Load所需库
rm(list = ls())
library(readxl)
library(writexl)
library(openxlsx)
library(dplyr)
library(ggplot2)
library(nnet)
library(forestploter)
library(forestplot)
library(grid)
library(ggpubr)
### 读取数据 ###
phe_child <- readxl::read_excel("D:/biosoft/1000thal/千人/RNO.1 Basic statistics of 1020 β-thalassemia patients.xlsx", sheet = "Sheet1")
dfID1020 <- read_excel("D:/biosoft/1000thal/千人/ID对应(1020+409)2023.12.22.xlsx", sheet = "1020")
dfmt1 <- read.table("D:/biosoft/1000thal/mtDNA/1020mtDNA_out/1020.mtCN_mean.tsv", header = T,sep = "\t")
dfmt11 <- read.table("D:/biosoft/1000thal/mtDNA/1020mtDNA_out/1020.mtCN_median.tsv", header = T,sep = "\t")
# dfmtcn <- read.table("D:/biosoft/1000thal/mtDNA/thal_mtcn_corrected_raw.tsv", header = T,sep = "\t")
dfmtcn <- read.table("D:/biosoft/1000thal/mtDNA/thala_mtcn_corrected_raw_lasso_add_homoglobin.tsv",header = T,sep = "\t") 
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
  rename("NUC_mean_coverage" = "NUC_mean_coverage.x") %>% 
  left_join(dfmtcn,by=c("ID")) %>% 
  select(!c("mtCN_mean.y")) %>% 
  rename("mtCN_mean" = "mtCN_mean.x")
# #
# mito_phenos <- c( "MT_mean_coverage", "NUC_mean_coverage", "mtCN_mean", 
#                   "MT_median_coverage", "mtCN_median","mtcn_mean_fit","mtcn_mean_re2")
# #
mito_phenos <- c( "MT_mean_coverage", "NUC_mean_coverage", "mtCN_mean", 
                  "MT_median_coverage", "mtCN_median","mtCN_adj2")
thal_phenos <- c("Clinical_staging", "Transfusion_Dependence","Thalassaemia_face", "Jaundice",
                 "Gallstones","Splenic", "Hepatomegaly", "Splenomegaly")
basic_phenos <- c("ID", "Assessment_time", "Ethnic", "Sex", "Age", "assessment_centre")
##

df <- phe_child_merge %>% 
  select(all_of(c(basic_phenos,thal_phenos,mito_phenos,"HbF","six_genotype_category")))



# df[,c("Clinical_staging", "Transfusion_Dependence","Thalassaemia_face", "Jaundice",
#       "Gallstones","Splenic", "Hepatomegaly", "Splenomegaly")] <- 
#   lapply(df[,c("Clinical_staging", "Transfusion_Dependence","Thalassaemia_face", "Jaundice",
#                       "Gallstones","Splenic", "Hepatomegaly", "Splenomegaly")], as.factor)
df$Clinical_staging <- factor(df$Clinical_staging, levels = c("TI", "TM"), labels = c(0, 1))
df$Transfusion_Dependence <- factor(df$Transfusion_Dependence , levels = c("NTDT", "TDT"), labels = c(0, 1))
df$Thalassaemia_face <- factor(df$Thalassaemia_face, levels = c("NO", "YES"), labels = c(0, 1))
df$Jaundice <- factor(df$Jaundice, levels = c("NO", "YES"), labels = c(0, 1))
df$Gallstones <- factor(df$Gallstones, levels = c("NO", "YES"), labels = c(0, 1))
df$Splenic <- factor(df$Splenic, levels = c("NO", "YES"), labels = c(0, 1))
df$Hepatomegaly <- factor(df$Hepatomegaly, levels = c("Normal", "Hepatomegaly"), labels = c(0, 1))
df$Splenomegaly <- factor(df$Splenomegaly, levels = c("Normal", "Splenomegaly","Splenectomy"), labels = c(0, 1,2))


### 检测因变量类别数并选择合适的模型 ###
fit_logistic_regression <- function(df, var, predictor) {
  ### 构建公式 ###
  formula <- as.formula(paste(var, "~", predictor))
  
  ### 检测因变量的类别数（忽略NA值） ###
  y <- df[[var]]
  y <- y[!is.na(y)]
  num_levels <- length(unique(y))
  
  ### 初始化Results数据框 ###
  result <- data.frame(
    Characteristics = character(),
    Test_Type = character(),
    P_value = numeric(),  # P值保持原始精度
    OR = numeric(),
    SE = numeric(),
    Conf_Int_Lower = numeric(),
    Conf_Int_Upper = numeric(),
    Number_Percent = character(),  # 新增列
    stringsAsFactors = FALSE
  )
  
  if (length(y) == 0) {
    warning(paste("All values are NA for Characteristics:", var))
    return(result)
  }
  
  ### 定义保留3位小数的函数（不应用于P值） ###
  format_num <- function(x) {
    ifelse(is.na(x), NA, round(x, 3))
  }
  
  ### 计算各Categorical的Sample数量and百分比 ###
  get_number_percent <- function(data, var_name) {
    counts <- table(data[[var_name]], useNA = "no")
    percents <- prop.table(counts) * 100
    paste0(counts, "/", format_num(percents), "%")
  }
  
  if (num_levels == 2) {
    ### 二Categorical逻辑回归 ###
    fit <- tryCatch(
      {
        glm(formula, data = df, family = binomial)
      },
      error = function(e) {
        warning(paste("Error fitting binomial model for", var, ":", e$message))
        return(NULL)
      }
    )
    
    if (!is.null(fit)) {
      coefficients <- summary(fit)$coefficients
      if (predictor %in% rownames(coefficients)) {
        p_value <- coefficients[predictor, "Pr(>|z|)"]  # 保持原始P值
        coef_value <- coefficients[predictor, "Estimate"]
        or_values <- exp(coef_value)
        se <- coefficients[predictor, "Std. Error"]
        
        ### 计算置信区间 ###
        ci <- tryCatch(
          {
            suppressMessages(confint(fit, parm = predictor))
          },
          error = function(e) {
            warning(paste("Error computing CI for", var, ":", e$message))
            return(c(NA, NA))
          }
        )
        
        ci_or <- exp(ci)
        
        ### 获取各Categorical的Sample数量and百分比 ###
        np <- get_number_percent(df[!is.na(df[[var]]), ], var)
        
        result <- data.frame(
          Characteristics = var,
          Test_Type = "binom",
          P_value = p_value,  # 不应用format_num
          OR = format_num(or_values),
          SE = format_num(se),
          Conf_Int_Lower = format_num(ci_or[1]),
          Conf_Int_Upper = format_num(ci_or[2]),
          Number_Percent = paste(np, collapse = "; "),  # 用分号分隔不同Categorical
          stringsAsFactors = FALSE
        )
      }
    }
  } else {
    ### 多项逻辑回归 ###
    fit <- tryCatch(
      {
        nnet::multinom(formula, data = df, trace = FALSE)
      },
      error = function(e) {
        warning(paste("Error fitting multinomial model for", var, ":", e$message))
        return(NULL)
      }
    )
    
    if (!is.null(fit)) {
      coefficients <- summary(fit)$coefficients
      se <- summary(fit)$standard.errors
      
      if (predictor %in% colnames(coefficients)) {
        ### 获取各Categorical的Sample数量and百分比 ###
        np <- get_number_percent(df[!is.na(df[[var]]), ], var)
        np_str <- paste(np, collapse = "; ")
        
        for (i in 1:nrow(coefficients)) {
          category <- rownames(coefficients)[i]
          p_value <- coefficients[i, predictor]  # 保持原始P值
          coef_value <- coefficients[i, predictor]
          or_values <- exp(coef_value)
          se_value <- se[i, predictor]
          
          ### 计算置信区间 ###
          ci <- tryCatch(
            {
              suppressMessages(confint(fit))[predictor, , i]
            },
            error = function(e) {
              warning(paste("Error computing CI for", var, "category", i, ":", e$message))
              return(c(NA, NA))
            }
          )
          
          ci_or <- exp(ci)
          
          result <- rbind(result, data.frame(
            Characteristics = paste0(var, " (ref vs ", category, ")"),
            Test_Type = "multinom",
            P_value = p_value,  # 不应用format_num
            OR = format_num(or_values),
            SE = format_num(se_value),
            Conf_Int_Lower = format_num(ci_or[1]),
            Conf_Int_Upper = format_num(ci_or[2]),
            Number_Percent = np_str,  # 使用相同的Sample数量/百分比字符串
            stringsAsFactors = FALSE
          ))
        }
      }
    }
  }
  
  return(result)
}


mtcn_or_df <- data.frame()
mtcnadj_or_df <- data.frame()
hbf_or_df <- data.frame()
### Iterate over所有因变量列名并拟合模型 ###
for (var in thal_phenos) {
  result1 <- fit_logistic_regression(df, var, "mtCN_mean")
  mtcn_or_df <- mtcn_or_df %>% bind_rows(result1)
  
  result2 <- fit_logistic_regression(df, var, "mtCN_adj2")
  mtcnadj_or_df <- mtcnadj_or_df %>% bind_rows(result2)
  
  result3 <- fit_logistic_regression(df, var, "HbF")
  hbf_or_df <- hbf_or_df %>% bind_rows(result3)
  
}


# rownames(mtcn_or_df) <- 1:nrow(mtcn_or_df)
# rownames(mtcnadj_or_df) <- 1:nrow(mtcnadj_or_df)
# rownames(hbf_or_df) <- 1:nrow(hbf_or_df)


df_trans_or <- function(inputdf){
  outdf <- inputdf %>% 
    mutate(Characteristics = dplyr::recode(Characteristics,
                                           "Splenomegaly (ref vs 1)" = "Splenomegaly",
                                           "Splenomegaly (ref vs 2)" = "Splenectomy")) %>% 
    mutate(`OR(95%CI)` = paste0(OR, "(", Conf_Int_Lower, ", ", Conf_Int_Upper, ")")) %>% 
    mutate(`P value` = case_when(
      is.na(P_value) ~ "",
      P_value > 0.05 ~ "ns",
      P_value <= 0.05 & P_value > 0.01 ~ "< 0.05",
      P_value <= 0.01 & P_value > 0.001 ~ "< 0.01",
      P_value <= 0.001 ~ "< 0.001")) %>% 
    rename(`Number(%)` = Number_Percent)
  
  outdfadj<- mtcnadj_or_df %>% 
    mutate(Characteristics = dplyr::recode(Characteristics,
                                           "Splenomegaly (ref vs 1)" = "Splenomegaly",
                                           "Splenomegaly (ref vs 2)" = "Splenectomy")) %>% 
    mutate(`OR(95%CI)` = paste0(OR, "(", Conf_Int_Lower, ", ", Conf_Int_Upper, ")")) %>% 
    mutate(`P value` = case_when(
      is.na(P_value) ~ "",
      P_value > 0.05 ~ "ns",
      P_value <= 0.05 & P_value > 0.01 ~ "< 0.05",
      P_value <= 0.01 & P_value > 0.001 ~ "< 0.01",
      P_value <= 0.001 ~ "< 0.001")) %>% 
    rename(`Number(%)` = Number_Percent)
  
  df_hbf <- hbf_or_df %>% 
    mutate(Characteristics = dplyr::recode(Characteristics,
                                           "Splenomegaly (ref vs 1)" = "Splenomegaly",
                                           "Splenomegaly (ref vs 2)" = "Splenectomy")) %>% 
    mutate(`OR(95%CI)` = paste0(OR, "[", Conf_Int_Lower, ", ", Conf_Int_Upper, "])")) %>% 
    mutate(`P value` = case_when(
      is.na(P_value) ~ "",
      P_value > 0.05 ~ "ns",
      P_value <= 0.05 & P_value > 0.01 ~ "< 0.05",
      P_value <= 0.01 & P_value > 0.001 ~ "< 0.01",
      P_value <= 0.001 ~ "< 0.001")) %>% 
    rename(`Number(%)` = Number_Percent)
  
  ### 其他代码保持不变... ###
  
  return(outdf)
}

df_mtcn <- df_trans_or(mtcn_or_df)
df_mtcnadj <- df_trans_or(mtcnadj_or_df)
df_hbf <- df_trans_or(hbf_or_df)

######

fig1 <- forestplot(
  df_mtcn[, c(1, 8, 9, 10)],   # 选择要在森林图中显示的数据列，第1、5、6列
  mean = df_mtcn$OR,     # Specify均值数据列（HR），它将显示为森林图的小方块
  lower = df_mtcn$Conf_Int_Lower,    # Specify95%置信区间的下限数据列
  upper = df_mtcn$Conf_Int_Upper,    # Specify95%置信区间的上限数据列，这些数据将显示为线段穿过方块
  zero = 1,               # Set零线或参考线为HR=1，这arex轴的垂直线
  boxsize = 0.1,          # Set小方块的大小
  graph.pos = 2           # Specify森林图应该插入到图形中的Position，这里are第2列
)

fig2 <- forestplot(
  df_mtcnadj[, c(1, 8, 9, 10)],   # 选择要在森林图中显示的数据列，第1、5、6列
  mean = df_mtcnadj$OR,     # Specify均值数据列（HR），它将显示为森林图的小方块
  lower = df_mtcnadj$Conf_Int_Lower,    # Specify95%置信区间的下限数据列
  upper = df_mtcnadj$Conf_Int_Upper,    # Specify95%置信区间的上限数据列，这些数据将显示为线段穿过方块
  zero = 1,               # Set零线或参考线为HR=1，这arex轴的垂直线
  boxsize = 0.1,          # Set小方块的大小
  graph.pos = 2           # Specify森林图应该插入到图形中的Position，这里are第2列
)

fig3 <- forestplot(
  df_hbf[, c(1, 8, 9, 10)],   # 选择要在森林图中显示的数据列，第1、5、6列
  mean = df_hbf$OR,     # Specify均值数据列（HR），它将显示为森林图的小方块
  lower = df_hbf$Conf_Int_Lower,    # Specify95%置信区间的下限数据列
  upper = df_hbf$Conf_Int_Upper,    # Specify95%置信区间的上限数据列，这些数据将显示为线段穿过方块
  zero = 1,               # Set零线或参考线为HR=1，这arex轴的垂直线
  boxsize = 0.1,          # Set小方块的大小
  graph.pos = 2           # Specify森林图应该插入到图形中的Position，这里are第2列
)


fig1
fig1 %>% ggexport(filename = "D:/biosoft/1000thal/mtDNA/figure/figure4E.OR_of_mtcn_in_predicting_8_clinical_indicators_in_thalassemia2.pdf", width = 10,height = 8,res=600)
fig2
fig3

###
# Set森林图主题
tm <- forest_theme(
  base_size = 10,        # Set文本的基础大小
  
  # Set可信区间的外观
  ci_pch = 15,           # 可信区间点的形状
  ci_col = "#2D5662",    # 可信区间的边框颜色
  ci_fill = "#2D5662",      # 可信区间的填充颜色
  ci_alpha = 0.8,        # 可信区间的透明度
  ci_lty = 1,            # 可信区间的线型
  ci_lwd = 1.5,          # 可信区间的线宽
  ci_Theight = 0.2,      # SetT字在可信区间末端的height，默认areNULL
  
  # Set参考线的外观
  refline_lwd = 1,         # 参考线的线宽
  refline_lty = "dashed",  # 参考线的线型
  refline_col = "grey20",  # 参考线的颜色
  
  # Set垂直线的外观
  vertline_lwd = 1,         # 垂直线的线宽，可以Add一条额外的垂直线，如果没有就不显示
  vertline_lty = "dashed",  # 垂直线的线型
  vertline_col = "grey20",  # 垂直线的颜色
  
  # Set脚注的字体大小、字体样式and颜色
  footnote_cex = 0.6,            # 脚注字体大小
  footnote_fontface = "italic",  # 脚注字体样式
  footnote_col = "red4"          # 脚注文本的颜色
)


ticks_mtcn <- c(1, 1.0125, 1.025,1.0375)
xlim_mtcn <- c(1,1.0375)
ticks_mtcnadj <- c(0.975, 1, 1.025,1.05)
xlim_mtcnadj <- c(0.975,1.05)
ticks_hbf <-  c(0.875, 0.925,1, 1.05,1.125)
xlim_hbf <- c(0.875,1.125)
plot_function <- function(df,ticks,xlim){
  p1mtch <- forest(
    df[,c(1,8,9,11,10)],  # 选择要在森林图中使用的数据列
    est = list(
      df$OR
    ),
    lower = list(
      df$Conf_Int_Lower
    ),
    upper = list(
      df$Conf_Int_Upper
    ),
    ci_column = c(4),         # SpecifyCI列
    xlim = xlim,#这里areSet的森林图x轴范围
    ticks_at = ticks,
    ref_line = 1.0,                # Add参考线
    # vert_line = c(1.01, 0.98),       # Add垂直线
    nudge_y = 0.1 ,             # 垂直Adjust标签Position
    theme = tm,
    align = c("l", "c", "c", "l", "r"),  # Set列对齐方式：左对齐、居中、居中、左对齐、左对齐
    footnote = "OR (error bars are 95% CI)"
  )                  # 应用自定义主题
  p1mtch
  
  p2mtch <- add_border(p1mtch, part = "header", where = c("top"))
  p3mtch <- add_border(p2mtch, part = "header", where = c("bottom"))
  p4mtch<- add_border(p3mtch,  row = 26, where = c("bottom"))
  return(p4mtch)
}

p2mtch <- plot_function(df_mtcn,ticks_mtcn,xlim_mtcn)
p2mtchadj <- plot_function(df_mtcnadj,ticks_mtcnadj,xlim_mtcnadj)
p2hbf <- plot_function(df_hbf,ticks_hbf,xlim_hbf)

p2mtch
p2mtchadj
p2hbf

p2mtch %>% ggexport(filename = "D:/biosoft/1000thal/mtDNA/figure/figure4E.OR_of_raw_mtCN_in_predicting_8_clinical_indicators_in_thalassemia.pdf", res=300)
p2mtchadj %>% ggexport(filename = "D:/biosoft/1000thal/mtDNA/figure/figure4E.OR_of_adj_mtCN_in_predicting_8_clinical_indicators_in_thalassemia.pdf", res=300)
p2hbf %>% ggexport(filename = "D:/biosoft/1000thal/mtDNA/figure/figure4E.OR_of_hbf_in_predicting_8_clinical_indicators_in_thalassemia.pdf", res=300)

# ggsave(filename = "figure4E2.OR_of_raw_and_corrected_mtCN_in_predicting_8_clinical_indicators_in_thalassemia.pdf",device = "pdf",plot = g2,
#        path = "D:/biosoft/1000thal/mtDNA/figure", width = 10 ,height = 8, dpi = 600)



