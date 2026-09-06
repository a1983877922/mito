rm(list=ls())#clear Global Environment
library(vcfR)
library(dplyr)
library(tidyr)
library(ggplot2)
library(reshape2)
library(patchwork)
########

AC_results <- read.csv("D:\\biosoft\\1000thal\\mtDNA\\gwas\\human_mito_genes\\AC couunrs results.csv")
AC_analysis <- read.csv("D:\\biosoft\\1000thal\\mtDNA\\gwas\\human_mito_genes\\AC analsis.csv")
daf_counts <- read.csv("D:\\biosoft\\1000thal\\mtDNA\\gwas\\human_mito_genes\\DAF couunrs results.csv")
daf_analysis <- read.csv("D:\\biosoft\\1000thal\\mtDNA\\gwas\\human_mito_genes\\DAF analsis.csv")


# daf_counts$DAF_Category <- factor(
#   daf_counts$DAF_Category,
#   levels = c("DAF 0.1%", "DAF 1%", "DAF 5%", "DAF > 5%"),
#   labels = c("DAF 0.1%", "DAF 1%", "DAF 5%", "DAF > 5%")
# )
# 
# daf_analysis$DAF_Category <- factor(
#   daf_analysis$DAF_Category,
#   levels = c("DAF 0.1%", "DAF 1%", "DAF 5%", "DAF > 5%"),
#   labels = c("DAF 0.1%", "DAF 1%", "DAF 5%", "DAF > 5%")
# )
AC_analysis$group <- AC_analysis$AC
AC_analysis$group[2:3] <- "1<AC≤3"
AC_analysis$group[4:5] <- "3<AC≤5"

merged_AC2 <- AC_analysis %>%
  group_by(group) %>%
  summarise(
    Count_Thalassemia = ifelse(group[1] %in% c("1<AC≤3", "3<AC≤5"),
                               sum(Count_Thalassemia),
                               first(Count_Thalassemia)),
    Count_Healthy = ifelse(group[1] %in% c("1<AC≤3", "3<AC≤5"),
                           sum(Count_Healthy),
                           first(Count_Healthy)),
    freq_Thalassemia = mean(freq_Thalassemia),
    freq_Healthy = mean(freq_Healthy),
    ratio_case_control = freq_Thalassemia/freq_Healthy,
    fold_change = freq_Thalassemia/freq_Healthy,
    direction = first(direction)
  ) %>%
  rename(AC = group) %>%
  ### 重新排序行 ###
  arrange(factor(AC, levels = c("AC=1", "1<AC≤3", "3<AC≤5", "AC>5"))) %>%
  filter(AC != "AC=1")

merged_AC <- AC_results %>%
  mutate(
    new_AC = case_when(
      AC == "AC=1" ~ "AC=1",
      AC %in% c("AC=2", "AC=3") ~ "1<AC≤3",
      AC %in% c("AC=4", "AC=5") ~ "3<AC≤5",
      AC == "AC>5" ~ "AC>5",
      TRUE ~ AC
    )
  ) %>%
  group_by(Group, new_AC) %>%
  summarise(
    Count = sum(Count),
    ### 如果需要加权平均，但这里freq已经areFrequency，通常取Mean即可 ###
    freq = mean(freq),
    ### 也可以计算加权Frequency（如果原始Frequencyare基于计数的） ###
    # freq_weighted = sum(freq * Count) / sum(Count),
    .groups = 'drop'
  ) %>%
  rename(AC = new_AC) %>%
  mutate(AC = factor(AC, levels = c("AC=1", "1<AC≤3", "3<AC≤5", "AC>5"))) %>%
  arrange(Group, AC) %>%
  select(AC, Count, Group, freq) %>% 
  filter(AC!="AC=1")

merged_daf <- daf_counts %>%
  ### 标记需要Merge的行 ###
  mutate(new_category = ifelse(DAF_Category %in% c("DAF 0.1%", "DAF 1%"), 
                               "DAF 1% (含0.1%)", 
                               DAF_Category)) %>%
  group_by(Group, new_category) %>%
  summarise(
    Variant_Count = sum(Variant_Count),
    # Mean_DAF 需要加权平均
    Mean_DAF = sum(Mean_DAF * Variant_Count) / sum(Variant_Count),
    Sample_Size = first(Sample_Size),
    Variants_per_Individual = sum(Variant_Count) / first(Sample_Size),
    Group_Label = first(Group_Label)
  ) %>%
  ungroup() %>%
  rename(DAF_Category = new_category) %>%
  ### 重新排序 ###
  arrange(Group, factor(DAF_Category, 
                        levels = c("DAF 1% (含0.1%)", "DAF 5%", "DAF > 5%"))) %>%
  select(Group, DAF_Category, Variant_Count, Mean_DAF, Sample_Size, 
         Variants_per_Individual, Group_Label)

merged_daf <- daf_counts %>%
  mutate(new_category = ifelse(DAF_Category %in% c("DAF 0.1%", "DAF 1%"), 
                               "DAF ≤1%", 
                               ifelse(DAF_Category == "DAF 5%", 
                                      "1% < DAF ≤5%", 
                                      "DAF > 5%"))) %>%
  group_by(Group, new_category) %>%
  summarise(
    Variant_Count = sum(Variant_Count),
    Mean_DAF = sum(Mean_DAF * Variant_Count) / sum(Variant_Count),
    Sample_Size = first(Sample_Size),
    Variants_per_Individual = sum(Variant_Count) / first(Sample_Size),
    Group_Label = first(Group_Label)
  ) %>%
  ungroup() %>%
  rename(DAF_Category = new_category) %>%
  mutate(DAF_Category = factor(DAF_Category, 
                               levels = c("DAF ≤1%", "1% < DAF ≤5%", "DAF > 5%"))) %>%
  arrange(Group, DAF_Category) %>%
  select(Group, DAF_Category, Variant_Count, Mean_DAF, Sample_Size, 
         Variants_per_Individual, Group_Label)

merged_daf2 <- daf_analysis %>%
  # Create新类别
  mutate(new_category = ifelse(DAF_Category %in% c("DAF 0.1%", "DAF 1%"), 
                               "DAF ≤1%", 
                               DAF_Category)) %>%
  ### 对于新类别，我们需要重新计算 ###
  group_by(new_category) %>%
  summarise(
    # Healthy: 合并 DAF 0.1% 和 DAF 1% 的每人变异数
    Healthy = ifelse(new_category[1] == "DAF ≤1%", 
                     sum(Healthy), 
                     first(Healthy)),
    # Thalassemia: 合并 DAF 0.1% 和 DAF 1% 的每人变异数
    Thalassemia = ifelse(new_category[1] == "DAF ≤1%", 
                         sum(Thalassemia), 
                         first(Thalassemia))
  ) %>%
  ### 重新计算 Fold_Change, Log2_FC 等 ###
  mutate(
    Fold_Change = Thalassemia / Healthy,
    Log2_FC = log2(Fold_Change),
    Direction = ifelse(Fold_Change > 1, 
                       "Higher in Thalassemia", 
                       ifelse(Fold_Change < 1, 
                              "Higher in Healthy", 
                              "Equal")),
    Significance = ifelse(abs(Log2_FC) > 1.0 & Fold_Change > 2, 
                          "***",
                          ifelse(abs(Log2_FC) > 0.5 & Fold_Change > 1.5, 
                                 "**", 
                                 "NS"))
  ) %>%
  rename(DAF_Category = new_category) %>%
  ### 重命名其他类别 ###
  mutate(DAF_Category = case_when(
    DAF_Category == "DAF 5%" ~ "1% < DAF ≤5%",
    DAF_Category == "DAF > 5%" ~ "DAF > 5%",
    TRUE ~ DAF_Category
  )) %>%
  # Set因子顺序
  mutate(DAF_Category = factor(DAF_Category, 
                               levels = c("DAF ≤1%", "1% < DAF ≤5%", "DAF > 5%"))) %>%
  arrange(DAF_Category) %>%
  select(DAF_Category, Healthy, Thalassemia, Fold_Change, Log2_FC, Direction, Significance)
# 2. Case/Control比值图

# 
# p_main <- ggplot(daf_counts, 
#                  aes(x = DAF_Category, y = Variants_per_Individual, fill = Group)) +
#   geom_bar(position = position_dodge(width = 0.8), stat = "identity", width = 0.7) +
#   scale_fill_manual(
#     values = c("Thalassemia" = "#F13333", "Healthy" = "#3284FF"),
#     name = "Group"
#   ) +
#   labs(
#     x = "",
#     y = "Number of variants per individual"
#   ) +
#   theme_classic() +
#   theme(
#     axis.title = element_text(size = 11, face = "bold"),
#     axis.text = element_text(size = 10),
#     axis.text.x = element_text(angle = 45, hjust = 1),
#     legend.position = "none",
#     plot.margin = margin(t = 5, r = 5, b = 5, l = 5)
#   ) +
#   geom_text(
#     aes(label = round(Variants_per_Individual, 2), group = Group),
#     position = position_dodge(width = 0.8),
#     vjust = -0.5,
#     size = 3
#   ) +
#   scale_y_continuous(
#     expand = expansion(mult = c(0, 0.15))
#   )
# p_main 
# Fold Change图

# # 组合图
# combined_plot2 <- p_main / p_fc +
#   plot_layout(heights = c(2, 1.5)) +
#   plot_annotation(
#     title = "Distribution of Damaging Variants by Derived Allele Frequency",
#     subtitle = paste("Chi-square test: χ² =", round(chi_test$statistic, 1), 
#                      ", df =", chi_test$parameter, ", p < 2.2e-16"),
#     tag_levels = 'A',
#     theme = theme(
#       plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
#       plot.subtitle = element_text(hjust = 0.5, size = 12, color = "red")
#     )
#   )
# 
# print(combined_plot2)

# 1. Create左侧的AC图（p1Modify）
p1_left <- ggplot(merged_AC, aes(x = AC, y = freq, fill = Group)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.7), width = 0.6) +
  geom_text(aes(label = round(freq, 2)), 
            position = position_dodge(width = 0.7), 
            vjust = -0.5, size = 3) +
  scale_fill_manual(values = c("Thalassemia" = "#E41A1C", "Healthy" = "#377EB8")) +
  labs(x = "", 
       y = "Number of damaging variants per individual (AC)",
       fill = "Group") +
  theme_classic() +
  theme(
    axis.title = element_text(size = 11, face = "bold"),
    axis.title.x = element_blank(),
    axis.text = element_text(size = 10),
    # axis.text.x = element_text(angle = 0, hjust = 0.5),
    legend.position = c(0.85, 0.85),
    legend.background = element_rect(fill = "white", color = "black", linewidth = 0.3),
    legend.title = element_text(face = "bold"),
    plot.margin = margin(t = 10, r = 0, b = 10, l = 10)  # 右边距设为0
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1)))
p1_left
# 2. Create右侧的DAF图（p_mainModify）
p_main_right <- ggplot(merged_daf, 
                       aes(x = DAF_Category, y = Variants_per_Individual, fill = Group)) +
  geom_bar(position = position_dodge(width = 0.8), stat = "identity", width = 0.7) +
  scale_fill_manual(
    values = c("Thalassemia" = "#E41A1C", "Healthy" = "#377EB8"),
    name = "Group"
  ) +
  labs(
    x = "",
    y = "Number of damaging variants per individual (DAF)"
  ) +
  theme_classic() +
  theme(
    axis.title = element_text(size = 11, face = "bold"),
    axis.title.x = element_blank(),
    axis.text = element_text(size = 10),
    # axis.text.x = element_text(angle = 45, hjust = 1),
    axis.title.y.right = element_text(color = "black"),
    axis.text.y.right = element_text(color = "black"),
    legend.position = "none",
    # plot.margin = margin(t = 10, r = 10, b = 10, l = 5)
    plot.margin = margin(t = 10, r = 10, b = 10, l = 0)  # 左边距设为0
  ) +
  geom_text(
    aes(label = round(Variants_per_Individual, 2), group = Group),
    position = position_dodge(width = 0.8),
    vjust = -0.5,
    size = 3
  ) +
  scale_y_continuous(
    expand = expansion(mult = c(0, 0.15)),
    position = "right"  # 将y轴移到右侧
  )
p_main_right 
# 3. 将两个图并排组合
combined_horizontal <- p1_left + p_main_right +
  plot_layout(
    ncol = 2,
    widths = c(1, 1),  # Adjust两个图的widthRatio
    guides = "collect"  # 收集图例
  ) +
  plot_annotation(
    # tag_levels = 'A',
    theme = theme(
      plot.tag = element_text(size = 14, face = "bold")
    )
  )

### 显示组合图 ###
combined_horizontal
ggsave(
  filename = "D:/biosoft/1000thal/mtDNA/figure/figure6A.damaging counts Analysis.pdf",
  plot = combined_horizontal,
  width = 14, 
  height = 10, 
  device = cairo_pdf,  # 使用 Cairo PDF 设备
  dpi = 300
)



pAC_fc <- ggplot(merged_AC2, aes(x = AC, y = ratio_case_control)) +
  geom_bar(stat = "identity", fill = ifelse(merged_AC2$ratio_case_control > 1, "#E41A1C", "#377EB8"),
           width = 0.6) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "gray40", linewidth = 0.8) +
  geom_text(aes(label = paste0(round(ratio_case_control, 2), "")), 
            vjust = ifelse(merged_AC2$ratio_case_control > 1, -0.5, 1.5),
            size = 3.5, fontface = "bold") +
  labs(x = "", 
       y = "Fold Change\n(Thalassemia/Healthy)") +
  theme_classic() +
  theme(
    axis.title = element_text(size = 11, face = "bold"),
    axis.text = element_text(size = 10),
    plot.margin = margin(t = 10, r = 0, b = 10, l = 10)  # 右边距设为0
  ) +
  scale_y_continuous(
    breaks = c(0, 0.5, 1, 1.5, 2.0),
    labels = c("0", "0.5", "1.0", "1.5", "2.0"),  # 明确Specify刻度标签
    limits = c(0, 2.5),  # 明确Sety轴上限为2.0
    expand = expansion(mult = c(0, 0.1)))  # 为标签留出更多空间


pAC_fc


p_daf_fc <- ggplot(merged_daf2, aes(x = DAF_Category, y = Fold_Change)) +
  geom_bar(stat = "identity", 
           fill = ifelse(merged_daf2$Fold_Change > 1, "#E41A1C", "#377EB8"),
           width = 0.6) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "gray40", linewidth = 0.8) +
  geom_text(
    aes(label = paste0(round(Fold_Change, 2))),
    vjust = ifelse(merged_daf2$Fold_Change > 1, -0.3, 1.3),
    size = 3,
    fontface = "bold"
  ) +
  labs(
    x = "",
    y = "Fold Change\n(Thalassemia/Healthy)"
  ) +
  theme_classic() +
  theme(
    axis.title = element_text(size = 11, face = "bold"),
    axis.text = element_text(size = 10),
    # axis.text.x = element_text(angle = 45, hjust = 1),
    plot.margin = margin(t = 10, r = 10, b = 10, l = 0)  # 左边距设为0
  ) +
  scale_y_continuous(
    breaks = c(0, 0.5, 1, 1.5, 2.0),
    labels = c("0", "0.5", "1.0", "1.5", "2.0"),  # 明确Specify刻度标签
    limits = c(0, 2.5),  # 明确Sety轴上限为2.0
    expand = expansion(mult = c(0, 0.1)),
    position = "right"  # 将y轴移到右侧
  )

p_daf_fc



# 3. 将两个图并排组合
combined_horizontal2 <- pAC_fc + p_daf_fc +
  plot_layout(
    # ncol = 2,
    # widths = c(1, 1),  # Adjust两个图的widthRatio
    guides = "collect" # 收集图例

  ) +
  plot_annotation(
    caption = "Derived allele frequency",
    theme = theme(
      plot.caption = element_text(
        size = 11,
        face = "bold",
        hjust = 0.5,
        vjust = 2.5,  # 垂直Adjust，正值向上
        margin = margin(b = 5)  # 底部边距
      ),
      plot.margin = margin(b = 25)  # 整体底部边距
    )
  )
  # plot_annotation(
  #   # tag_levels = 'A',
  #   theme = theme(
  #     plot.tag = element_text(size = 14, face = "bold")
  #   )
  # )

combined_horizontal2
# Save图形
ggsave(
  filename = "D:/biosoft/1000thal/mtDNA/figure/figure6B.damaging fc Analysis.pdf",
  plot = combined_horizontal2,
  width = 14, 
  height = 10, 
  device = cairo_pdf,  # 使用 Cairo PDF 设备
  dpi = 300
)


