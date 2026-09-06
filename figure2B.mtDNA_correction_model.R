### 安装andLoad必要的包 ###
if (!require("broom")) install.packages("broom")
if (!require("dplyr")) install.packages("dplyr")
library(broom)
library(dplyr)
##
##
blood_phenos2 <- c("Red_blood_cell_count", "Mean_corpuscular_volume",
                  "Mean_corpuscular_hemoglobin",
                  "Mean_corpuscular_hemoglobin_concentration",
                  "Hemoglobin","Hemoglobin_A", "Hemoglobin_A2","Fetal_hemoglobin",
                  "Percentage_of_reticulocytes_with_low_fluorescence_intensity",
                  "Median_fluorescence_Intensity_Reticulocyte_ratio",
                  "Percentage_of_reticulocytes_with_high_fluorescence_intensity",
                  "Percentage_of_basophils", "Percentage_of_eosinophils",
                  "Percentage_of_monocytes",
                  "Percentage_of_immature_reticulocytes","Platelet_wbc_ratio",
                  "Percentage_of_reticulocytes", "Mean_Platelet_volume",
                  "Platelet_specific_volume", "Platelet_distribution_width")
thala_phenos <- c("Hemoglobin","Hemoglobin_A", "Hemoglobin_A2","Fetal_hemoglobin")

### 技术协变量模型 ###
technical_model2 <- "Assessment_time + assessment_centre"

### 完整模型（contains血小板参数） ###
full_model_formula <- reformulate(
  c(technical_model2, blood_phenos2, "Sex", "Age","Ethnic"),
  response = "log(mtCN_mean)"
)
#####
### 定义模型公式 ###
# 1. 只有截距的基准模型
null_model_formula <- as.formula("log(mtCN_mean) ~ 1")

# 2. 只有技术协变量的模型
tech_model_formula <- reformulate(technical_model2, response = "log(mtCN_mean)")

# 3. 技术协变量 + 血液Phenotype变量
tech_blood_model_formula <- reformulate(
  c(technical_model2, blood_phenos2), 
  response = "log(mtCN_mean)"
)

# # 3.1 + 血液Phenotype变量
# blood_thala_model_formula <- reformulate(
#   c(blood_phenos, thala_phenos), 
#   response = "log(mtCN_mean)"
# )


# 4. 技术协变量 + 地中海贫血相关变量
tech_thala_model_formula <- reformulate(
  c(technical_model2, thala_phenos), 
  response = "log(mtCN_mean)"
)

# 5. 完整模型（所有变量）
full_model_formula <- reformulate(
  c(technical_model2, blood_phenos2, "Platelet_wbc_ratio", "Sex", "Age"),
  response = "log(mtCN_mean)"
)

### 拟合所有模型 ###
null_model <- lm(null_model_formula, data = model_data)
blood_thala_model <- lm(blood_thala_model_formula, data = model_data)
tech_model <- lm(tech_model_formula, data = model_data)
tech_blood_model <- lm(tech_blood_model_formula, data = model_data)
tech_thala_model <- lm(tech_thala_model_formula, data = model_data)
full_model <- lm(full_model_formula, data = model_data)


### 计算各模型的R² ###
r2_results <- data.frame(
  Model = c("Null", "Technical", "Technical + Blood", "Technical + Thalassemia", "Full"),
  R_squared = c(
    summary(null_model)$r.squared,
    summary(tech_model)$r.squared,
    summary(tech_blood_model)$r.squared,
    summary(tech_thala_model)$r.squared,
    summary(full_model)$r.squared
  ),
  Adjusted_R_squared = c(
    summary(null_model)$adj.r.squared,
    summary(tech_model)$adj.r.squared,
    summary(tech_blood_model)$adj.r.squared,
    summary(tech_thala_model)$adj.r.squared,
    summary(full_model)$adj.r.squared
  )
)

### 计算各变量组的增量R²（ΔR²） ###
r2_results$Incremental_R2 <- c(
  NA,
  r2_results$R_squared[2] - r2_results$R_squared[1],  # Technical only
  r2_results$R_squared[3] - r2_results$R_squared[2],  # Blood phenos added
  r2_results$R_squared[4] - r2_results$R_squared[2],  # Thalassemia phenos added
  r2_results$R_squared[5] - r2_results$R_squared[3]   # Additional vars added
)

print(r2_results)

### 更详细的方差分解分析 ###
cat("\n=== 方差分解分析 ===\n")
cat(sprintf("1. 技术协变量单独解释的方差: %.4f (%.2f%%)\n", 
            r2_results$R_squared[2], r2_results$R_squared[2] * 100))

cat(sprintf("2. 血液表型变量增加的方差: %.4f (%.2f%%)\n", 
            r2_results$Incremental_R2[3], r2_results$Incremental_R2[3] * 100))

cat(sprintf("3. 地中海贫血相关变量增加的方差: %.4f (%.2f%%)\n", 
            r2_results$Incremental_R2[4], r2_results$Incremental_R2[4] * 100))

cat(sprintf("4. 总解释方差 (全模型): %.4f (%.2f%%)\n", 
            r2_results$R_squared[5], r2_results$R_squared[5] * 100))

### 使用ANOVA进行正式的模型Compare ###
cat("\n=== 模型比较 (ANOVA) ===\n")
anova_results <- anova(null_model, tech_model, tech_blood_model, full_model)
print(anova_results)

### 可视化Results ###
library(ggplot2)

pr <- ggplot(r2_results[-1, ], aes(x = Model, y = R_squared)) +
  geom_bar(stat = "identity", fill = "steelblue", alpha = 0.7) +
  geom_text(aes(label = Model, y = R_squared/2),  # 模型名称在柱子中间
            color = "white", fontface = "bold", size = 3.5,
            angle = 90, hjust = 0.5, vjust = 0.5) +
  geom_text(aes(label = sprintf("%.3f", R_squared)),  # R²值在柱子顶部
            vjust = -0.5, size = 3.5, color = "black") +
  labs(title = NULL, x = "Lasso model",y = "R²,ram log mtcn") +
  theme_classic() +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.x = element_blank()) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1)))  # 为顶部标签留空间

pr
ggsave(pr,filename ="D:\\biosoft\\1000thal\\mtDNA\\figure/figure4G.R² Plot of mtCN lasso model add hoomoglobin in Thalassemia.pdf",
       device = pdf,width = 6,height = 8,dpi = 600)

# Create增量R²的可视化
incremental_data <- data.frame(
  Variable_Group = c("Technical", "Blood Phenos", "Thalassemia Phenos", "Other"),
  Incremental_R2 = c(r2_results$Incremental_R2[2:5])
)

ggplot(incremental_data, aes(x = Variable_Group, y = Incremental_R2)) +
  geom_bar(stat = "identity", fill = "coral", alpha = 0.7) +
  geom_text(aes(label = sprintf("%.3f", Incremental_R2)), vjust = -0.5) +
  labs(title = "各变量组对解释方量的增量贡献(ΔR²)",
       x = "变量组", y = "ΔR²") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
