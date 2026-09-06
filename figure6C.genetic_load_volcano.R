rm(list=ls())#clear Global Environment
# Load packages
library(readxl)
library(tidyr)
library(data.table)
library(tibble)
library(dplyr)
library(broom)
library(ggplot2)
library(patchwork)
library(pheatmap)
library(ggrepel)
library(purrr)
#####
phe_child <- readxl::read_excel("D:/biosoft/1000thal/千人/RNO.1 Basic statistics of 1020 β-thalassemia patients.xlsx", sheet = "Sheet1") %>% 
  select(ID,HBB_genotype_category) %>% 
  filter(! HBB_genotype_category %in% c("β0/HPFH","β0/βN or β0/βN co-ααα"))
dfID1020 <- read_excel("D:/biosoft/1000thal/千人/ID对应(1020+409)2023.12.22.xlsx", sheet = "1020")

df1020 <- read.table("D:\\biosoft\\1000thal\\mtDNA\\gwas\\human_mito_genes\\genetic load\\1020genetic_load_normalized_wide_format.csv",
                     sep = ",",header = T)
df2450 <- read.table("D:\\biosoft\\1000thal\\mtDNA\\gwas\\human_mito_genes\\genetic load\\healthy genetic_load_normalized_wide_format.csv",
                     sep = ",",header = T)

df1020sub <- df1020 %>%
  select(Sample, contains("h_0.5")) %>% 
  rename_all(~ gsub("_h_0.5_normalized", "", .))
# %>% 
  # mutate(Total_Score = rowSums(select(., -Sample), na.rm = TRUE))

df1020sub2 <- df1020 %>%
  select(Sample, contains("h_0.5")) %>% 
  rename_all(~ gsub("_h_0.5_normalized", "", .)) %>% 
  left_join(dfID1020,by=c("Sample"="HID")) %>% 
  left_join(phe_child,by="ID") %>% 
  select(!c("ID","BID.y")) %>% 
  filter(!is.na(HBB_genotype_category)) %>% 
  mutate(Total_Score = rowSums(select(., -c(Sample,HBB_genotype_category)), na.rm = TRUE))


df2450sub <- df2450 %>%
  select(Sample, contains("h_0.5")) %>% 
  rename_all(~ gsub("_h_0.5_normalized", "", .))


### 先Check数据结构and维度 ###
cat("df1020sub 维度:", dim(df1020sub), "\n")
cat("df2450sub 维度:", dim(df2450sub), "\n")

# InspectGene列名（排除Sample列）
gene_names <- names(df1020sub)[-1]
cat("共有", length(gene_names), "个基因\n")
print(gene_names[1:10])  # Inspect前10个Gene名



combined_data <- bind_rows(df1020sub, df2450sub)

# AddCohortIdentifier
combined_data <- combined_data %>%
  mutate(Cohort = c(rep("Thalassemia", nrow(df1020sub)), 
                    rep("Control", nrow(df2450sub))))




gene_stats <- combined_data %>%
  pivot_longer(cols = -c(Sample, Cohort), 
               names_to = "Gene", 
               values_to = "Expression") %>%
  group_by(Cohort, Gene) %>%
  summarise(
    Mean = mean(Expression, na.rm = TRUE),
    SD = sd(Expression, na.rm = TRUE),
    Median = median(Expression, na.rm = TRUE),
    Q1 = quantile(Expression, 0.25, na.rm = TRUE),
    Q3 = quantile(Expression, 0.75, na.rm = TRUE),
    N = n(),
    .groups = 'drop'
  )

# gene_stats_in_thal <- df1020sub2 %>%
#   pivot_longer(cols = -c(Sample, HBB_genotype_category), 
#                names_to = "Gene", 
#                values_to = "Expression") %>%
#   group_by(HBB_genotype_category, Gene) %>%
#   summarise(
#     Mean = mean(Expression, na.rm = TRUE),
#     SD = sd(Expression, na.rm = TRUE),
#     Median = median(Expression, na.rm = TRUE),
#     Q1 = quantile(Expression, 0.25, na.rm = TRUE),
#     Q3 = quantile(Expression, 0.75, na.rm = TRUE),
#     N = n(),
#     .groups = 'drop'
#   )
### Internal comparison within Thalassemia ###
### First compute basic statistics ###
gene_stats_basic <- df1020sub2 %>%
  pivot_longer(cols = -c(Sample, HBB_genotype_category), 
               names_to = "Gene", 
               values_to = "Expression") %>%
  group_by(HBB_genotype_category, Gene) %>%
  summarise(
    Mean = mean(Expression, na.rm = TRUE),
    SD = sd(Expression, na.rm = TRUE),
    Median = median(Expression, na.rm = TRUE),
    Q1 = quantile(Expression, 0.25, na.rm = TRUE),
    Q3 = quantile(Expression, 0.75, na.rm = TRUE),
    N = sum(!is.na(Expression)),
    .groups = 'drop'
  )

### Method 1: Statistical test using grouped operations ###
gene_test_results <- df1020sub2 %>%
  pivot_longer(cols = -c(Sample, HBB_genotype_category), 
               names_to = "Gene", 
               values_to = "Expression") %>%
  group_by(Gene) %>%
  summarise(
    ### Checkwhether or not有enough data ###
    n_groups = length(unique(HBB_genotype_category[!is.na(Expression)])),
    n_obs = sum(!is.na(Expression)),
    
    # ANOVAtest
    anova_p = {
      if (n_groups >= 2 && n_obs >= 3) {
        tryCatch({
          aov_result <- aov(Expression ~ HBB_genotype_category)
          tidy(aov_result) %>% 
            filter(term == "HBB_genotype_category") %>% 
            pull(p.value)
        }, error = function(e) NA_real_)
      } else {
        NA_real_
      }
    },
    
    # Kruskal-Wallistest
    kruskal_p = {
      if (n_groups >= 2 && n_obs >= 3) {
        tryCatch({
          kruskal.test(Expression ~ HBB_genotype_category) %>% 
            tidy() %>% 
            pull(p.value)
        }, error = function(e) NA_real_)
      } else {
        NA_real_
      }
    },
    .groups = 'drop'
  )

# Merge statistics and test results
gene_stats_complete <- gene_stats_basic %>%
  left_join(gene_test_results, by = "Gene") %>%
  group_by(Gene) %>%
  mutate(
    ### Multiple-testing correction of p-values (by Gene count) ###
    anova_p_adj = p.adjust(first(anova_p), method = "BH"),
    kruskal_p_adj = p.adjust(first(kruskal_p), method = "BH"),
    
    # Add significance markers
    sig_anova = case_when(
      first(anova_p_adj) < 0.001 ~ "***",
      first(anova_p_adj) < 0.01 ~ "**",
      first(anova_p_adj) < 0.05 ~ "*",
      TRUE ~ "ns"
    ),
    sig_kruskal = case_when(
      first(kruskal_p_adj) < 0.001 ~ "***",
      first(kruskal_p_adj) < 0.01 ~ "**",
      first(kruskal_p_adj) < 0.05 ~ "*",
      TRUE ~ "ns"
    )
  ) %>%
  ungroup()

# Inspect results
head(gene_stats_complete)


### Post-hoc test (for significant Genes) ###
significant_genes <- gene_stats_complete %>%
  filter(sig_anova != "ns") %>%
  distinct(Gene) %>%
  pull(Gene)

### 进行Tukey HSDPost-hoc test ###
perform_tukey_test <- function(gene_name) {
  data_subset <- df1020sub2 %>%
    select(Sample, HBB_genotype_category, all_of(gene_name)) %>%
    rename(Expression = all_of(gene_name)) %>%
    filter(!is.na(Expression))
  
  if (length(unique(data_subset$HBB_genotype_category)) < 2) {
    return(NULL)
  }
  
  tryCatch({
    aov_result <- aov(Expression ~ HBB_genotype_category, data = data_subset)
    tukey_result <- TukeyHSD(aov_result)$HBB_genotype_category
    
    as.data.frame(tukey_result) %>%
      tibble::rownames_to_column("comparison") %>%
      as_tibble() %>%
      mutate(Gene = gene_name)
  }, error = function(e) {
    tibble(
      comparison = NA_character_,
      diff = NA_real_,
      lwr = NA_real_,
      upr = NA_real_,
      p.adj = NA_real_,
      Gene = gene_name
    )
  })
}
library(purrr)
### 对前20个significant GenesPost-hoc test ###
posthoc_results <- map_df(head(significant_genes, 30), perform_tukey_test)

# InspectPost-hoc testResults
head(posthoc_results)

### 导出Results ###
# write.csv(gene_stats_complete, "gene_expression_stats_with_tests.csv", row.names = FALSE)
# write.csv(posthoc_results, "gene_posthoc_results.csv", row.names = FALSE)

# Inspect统计摘要
summary_stats <- gene_stats_complete %>%
  group_by(Gene) %>%
  slice(1) %>%
  ungroup() %>%
  summarise(
    total_genes = n_distinct(Gene),
    sig_anova = sum(sig_anova != "ns", na.rm = TRUE),
    sig_kruskal = sum(sig_kruskal != "ns", na.rm = TRUE),
    anova_sig_rate = mean(sig_anova != "ns", na.rm = TRUE) * 100,
    kruskal_sig_rate = mean(sig_kruskal != "ns", na.rm = TRUE) * 100
  )


p_significant_genes <- df1020sub2 %>%
  pivot_longer(cols = -c(Sample, HBB_genotype_category), 
               names_to = "Gene", 
               values_to = "Expression") %>%
  filter(Gene %in% head(significant_genes,25)) %>%
  ggplot(aes(x = HBB_genotype_category, y = -log10(Expression), fill = HBB_genotype_category)) +
  geom_boxplot() +
  facet_wrap(~ Gene, scales = "free_y") +
  theme_minimal() +
  labs(title = "Significant Genes Expression by Genotype Category",
       x = "HBB Genotype Category",
       y = "Expression Level")
p_significant_genes
# Save为PDF
# ggsave("D:\\biosoft\\1000thal\\mtDNA\\figure/figure6.significant_genes_boxplot in thal.pdf", 
#        plot = p_significant_genes,
# width = 16,     # width（英寸）
# height = 12,    # height（英寸）
# device = "pdf") # Specify输出格式
### Internal comparison within Thalassemia ###

### 对每个Gene进行ttest ###
# ttest_results <- lapply(gene_names, function(gene) {
#   t_test <- t.test(df1020sub[[gene]], df2450sub[[gene]])
#   tidy(t_test) %>% 
#     mutate(Gene = gene,
#            Mean_diff = estimate1 - estimate2)
# }) %>% bind_rows()
# 
# # Adjustp-values（Multiple-testing correction）
# ttest_results$adj_pvalue <- p.adjust(ttest_results$p.value, method = "fdr")
# 
# # Inspect显著差异Gene
# significant_genes <- ttest_results %>%
#   filter(adj_pvalue < 0.05) %>%
#   filter(adj_pvalue != 0) %>%
#   arrange(adj_pvalue)
# 
# 
# ttest_results <- ttest_results %>%
#   filter(!is.na(adj_pvalue))




### 使用效应量（Effect Size）而不仅仅are均值差 ###
calculate_effect_size <- function(df1, df2) {
  gene_names <- names(df1)[-1]  # 排除Sample列
  
  effect_results <- lapply(gene_names, function(gene) {
    x <- df1[[gene]]
    y <- df2[[gene]]
    
    # Cohen's d（效应量）
    n1 <- length(x)
    n2 <- length(y)
    pooled_sd <- sqrt(((n1-1)*var(x) + (n2-1)*var(y)) / (n1 + n2 - 2))
    cohens_d <- (mean(x) - mean(y)) / pooled_sd
    
    # Hedge's g（小Samplecorrection）
    hedge_g <- cohens_d * (1 - (3/(4*(n1+n2) - 9)))
    
    ### 均值比（适合小数值） ###
    mean_ratio <- mean(x) / mean(y)
    log2_fold_change <- log2(mean_ratio)
    
    # Wilcoxon秩andtest（非参数，对异常值稳健）
    wilcox_test <- wilcox.test(x, y)
    
    data.frame(
      Gene = gene,
      Mean_1020 = mean(x),
      Mean_2450 = mean(y),
      Mean_diff = mean(x) - mean(y),
      Fold_change =  mean_ratio,
      Log2FC = log2_fold_change,
      Cohens_d = cohens_d,
      Hedges_g = hedge_g,
      Wilcox_p = wilcox_test$p.value
    )
  }) %>% bind_rows()
  
  ### Multiple-testing correction ###
  effect_results$adj_pvalue <- p.adjust(effect_results$Wilcox_p, method = "fdr")
  
  return(effect_results)
}




# label_data <- effect_results %>%
#   filter(adj_pvalue < 0.05 & abs(Log2FC) > 1) %>%
#   mutate(label = Gene) %>%  # AssumeGene名列aregene
# # 选择最显著的20个点，或者变化最大的
#   arrange(adj_pvalue) %>%
#   head(30)

### 使用效应量或log变换后的值 ###

# (1) 火山图显示差异表达Gene
### Method 1：使用效应量（Cohen's d） ###
# volcano_effect <- ggplot(effect_results, 
#                          aes(x = Cohens_d, 
#                              y = -log10(adj_pvalue))) +
#   geom_point(aes(color = ifelse(adj_pvalue < 0.05 & abs(Cohens_d) > 0.5, 
#                                 "Significant", "Not Significant")), 
#              alpha = 0.7, size = 1.5) +
#   geom_vline(xintercept = c(-0.5, 0, 0.5), linetype = "dashed", alpha = 0.5) +
#   geom_hline(yintercept = -log10(0.05), linetype = "dashed", alpha = 0.5) +
#   scale_color_manual(values = c("Significant" = "red", 
#                                 "Not Significant" = "gray60")) +
#   labs(x = "Cohen's d (Effect Size)", 
#        y = "-log10(adjusted p-value)",
#        title = "Volcano Plot by Effect Size",
#        subtitle = "Absolute value > 0.5 considered biologically meaningful") +
#   theme_classic() +
  # scale_x_continuous(limits = c(-3, 3))  # 限制效应量范围
# volcano_effect
### 方法2：使用Log2 Fold Change ###
### First处理数据，CreateCategorical列 ###
effect_results <- calculate_effect_size(df1020sub,df2450sub) %>% 
  filter(!is.na(Fold_change)) 

# %>% 
  # filter(Mean_1020!=0)

effect_results <- effect_results %>%
  ### Removewithout效值 ###
  # filter(is.finite(Log2FC) & adj_pvalue != 0) %>%
  # Create显著性Categorical
  mutate(
    significance = case_when(
      adj_pvalue < 0.05 & Log2FC > 1 ~ "Higher Load",
      adj_pvalue < 0.05 & Log2FC < -1 ~ "Lower Load",
      TRUE ~ "Not Significant"
    ),
    ### Add要标注的名称（Assume有gene_name列，如果没有用rownames或其他Identifier） ###
    label = ifelse(significance != "Not Significant", Gene, NA)
  )

### First，确保significance列are因子类型，并Set正确的顺序 ###
effect_results$significance <- factor(effect_results$significance,
                                      levels = c("Higher Load", "Not Significant", "Lower Load"))
write.table(effect_results,"D:\\biosoft\\1000thal\\mtDNA\\gwas\\human_mito_genes\\volcano resluts.tab",sep = "\t",row.names = F,quote = F)
#sig genes
genelist3 <- c("BID","BCL2","HSDL1","PDPR","CASP8","COMT","MCEE","MLYCD","MRPS27","CBR4",
               "CPT1C","FAM185A","MIPEP","UQCRFS1","CMPK2","C3orf33","CARS2","ATP5PO",
               "MTARC2","BCO2","BCL2L2")
### 然后计算每个类别的数量 ###
up_count <- sum(effect_results$significance == "Higher Load", na.rm = TRUE)
down_count <- sum(effect_results$significance == "Lower Load", na.rm = TRUE)
ns_count <- sum(effect_results$significance == "Not Significant", na.rm = TRUE)

### 绘图代码保持不变 ###
volcano_log2fc <- ggplot(effect_results,
                         aes(x = Log2FC, 
                             y = -log10(adj_pvalue))) +
  geom_point(aes(color = significance), 
             alpha = 0.7, size = 1.5) +
  
  # Add标注 - 只标注显著的点
  geom_text_repel(
    data = subset(effect_results, significance != "Not Significant"),
    aes(label = label),
    size = 3,
    max.overlaps = 20,
    box.padding = 0.5,
    segment.color = "grey50",
    segment.size = 0.2,
    min.segment.length = 0.2,
    show.legend = FALSE
  ) +
  
  # Add参考线
  geom_vline(xintercept = c(-1, 0, 1), linetype = "dashed", alpha = 0.5) +
  geom_hline(yintercept = -log10(0.05), linetype = "dashed", alpha = 0.5) +
  
  scale_color_manual(
    values = c(
      "Higher Load" = "red",
      "Not Significant" = "gray60",
      "Lower Load" = "blue"
    ),
    labels = c(
      paste0("Higher Load(n = ", up_count, ")"),
      paste0("Not Significant (n = ", ns_count, ")"),
      paste0("Lower Load (n = ", down_count, ")")
    ),
    name = "Genetic Load"
  ) +
  
  ### 标签and标题（可选：在标题或副标题中显示统计信息） ###
  labs(
    x = "Log2 Fold Change (Thalassemia/healthy)", 
    y = "-log10(adjusted wilcox p-value)",
    title = "Volcano Plot of Differential Genetic Load",
    subtitle = paste0("Higher: ", up_count, " genes | Lower: ", down_count, " genes | Total: ", nrow(effect_results), " genes")
  ) +
  
  ### 主题Set ###
  theme_classic() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 10),
    legend.position = "right",
    legend.text = element_text(size = 10)
  )

print(volcano_log2fc)

# ggsave(volcano_log2fc ,filename ="D:\\biosoft\\1000thal\\mtDNA\\figure/figure6C.volcano of genetic load.pdf",
#        device = pdf,width = 8,height = 8,dpi = 600)





# CreateFilter后的数据
effect_results_filtered <- effect_results %>%
  ### RemoveNA值 ###
  filter(!is.na(Log2FC), !is.na(adj_pvalue)) %>%
  ### 计算-log10(p-values) ###
  mutate(neg_log10_p = -log10(adj_pvalue)) %>%
  # Filter极端值
  filter(abs(Log2FC) <= 10, neg_log10_p <= 40) %>%
  ### 重新计算显著性Categorical ###
  mutate(
    significance = case_when(
      adj_pvalue < 0.05 & Log2FC > 1 ~ "Higher Load",
      adj_pvalue < 0.05 & Log2FC < -1 ~ "Lower Load",
      TRUE ~ "Not Significant"
    ),
    ### 只标记Specify的21个Gene ###
    label = ifelse(Gene %in% genelist3, Gene, NA)
  )

### CheckSpecify的Genewhether or not在数据中 ###

### 重新计算各类别数量 ###
effect_results_filtered$significance <- factor(
  effect_results_filtered$significance,
  levels = c("Higher Load", "Not Significant", "Lower Load")
)


# Create火山图（只标注Specify的21个Gene）
volcano_log2fc_specific <- ggplot(effect_results_filtered,
                                  aes(x = Log2FC, 
                                      y = neg_log10_p)) +
  geom_point(aes(color = significance), 
             alpha = 0.7, size = 1.5) +
  
  # Add标注 - 只标注Specify的21个Gene
  geom_text_repel(
    data = subset(effect_results_filtered, !is.na(label)),
    aes(label = label),
    size = 3.5,  # 稍微增大字体
    max.overlaps = 100,  # 增加最大重叠数，确保所有Gene都显示
    box.padding = 0.5,  # 增加内边距
    point.padding = 0.2,  # 增加点到标签的距离
    segment.color = "black",  # 连接线颜色
    segment.size = 0.3,  # 连接线粗细
    segment.alpha = 0.6,  # 连接线透明度
    min.segment.length = 0,  # 总are显示连接线
    nudge_x = 0.15,  # 水平偏移
    nudge_y = 0.15,  # 垂直偏移
    direction = "both",  # 允许双向Adjust
    force = 0.8,  # Adjust排斥力
    force_pull = 1.5,  # 增加向点拉的力
    max.time = 3,  # 增加计算时间
    max.iter = 30000,  # 增加迭代次数
    show.legend = FALSE
  ) +
  
  # Add参考线
  geom_vline(xintercept = c(-1, 0, 1), 
             linetype = c("dashed", "solid", "dashed"), 
             alpha = 0.5) +
  geom_hline(yintercept = -log10(0.05), 
             linetype = "dashed", 
             alpha = 0.5) +
  
  ### 颜色Set ###
  scale_color_manual(
    values = c(
      "Higher Load" = "red",
      "Not Significant" = "gray60",
      "Lower Load" = "blue"
    ),
    labels = c(
      paste0("Higher Load (n = ", up_count, ")"),
      paste0("Not Significant (n = ", ns_count, ")"),
      paste0("Lower Load (n = ", down_count, ")")
    ),
    name = "Genetic Load"
  ) +
  
  ### 坐标轴Set ###
  scale_x_continuous(
    limits = c(-7, 7),
    breaks = seq(-7, 7, by = 2)
  ) +
  scale_y_continuous(
    limits = c(0, 25),
    breaks = seq(0, 25, by = 5)
  ) +
  
  ### 标签 ###
  labs(
    x = "Log2 Fold Change (Thalassemia/healthy)", 
    y = "-log10(adjusted p-value)",
    title = "Volcano Plot of Differential Genetic Load",
    subtitle = paste0("Higher: ", up_count, " genes | Lower: ", down_count, 
                      " genes | Total: ", nrow(effect_results), " genes"),
    caption = paste0("|Log2FC| > 10 and -log10(p) > 40 genes were filtered out\n",
                     "Labeled: ", length(present_genes), " specified genes")
  ) +
  
  ### 主题 ###
  theme_classic() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    plot.subtitle = element_text(hjust = 0.5, size = 11),
    plot.caption = element_text(hjust = 0.5, size = 9, color = "gray50"),
    legend.position = "bottom",
    legend.box = "horizontal",
    legend.title = element_text(size = 11, face = "bold"),
    legend.text = element_text(size = 10),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10)
  ) +
  
  ### 图例样式 ###
  guides(color = guide_legend(
    override.aes = list(alpha = 1, size = 3),
    nrow = 1
  ))

### 显示图形 ###
print(volcano_log2fc_specific)


# ggsave(volcano_log2fc_specific ,filename ="D:\\biosoft\\1000thal\\mtDNA\\figure/figure6C.volcano of genetic load mini.pdf",
#        device = pdf,width = 8,height = 8,dpi = 600)












sig_genes <- effect_results %>% 
  filter(!is.na(label)) %>% 
  select(label) %>% 
  pull(label)

# MA图：M=log2 fold change, A=average expression
### 准备标注数据 ###
ma_plot_data <- ma_plot_data %>%
  mutate(
    ### 计算显著性分数 ###
    score = -log10(adj_pvalue) * abs(M),
    ### 选择要标注的Gene - top 20最显著的 ###
    label = ifelse(rank(-score) <= 20 & Significant == TRUE, 
                   Gene,  # Assume有gene_name列
                   NA)
  )

ma_plot <- ggplot(ma_plot_data, aes(x = log10(A), y = M)) +
  geom_point(aes(color = Significant, alpha = Significant), size = 1.2) +
  
  # Add标注
  geom_text_repel(
    aes(label = label),
    size = 3,
    max.overlaps = 20,
    box.padding = 0.5,
    segment.color = "grey50",
    segment.size = 0.2,
    min.segment.length = 0,
    force = 2,
    show.legend = FALSE
  ) +
  
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray40") +
  geom_smooth(method = "loess", color = "blue", se = FALSE) +
  scale_color_manual(values = c("FALSE" = "gray60", "TRUE" = "red")) +
  scale_alpha_manual(values = c("FALSE" = 0.3, "TRUE" = 0.8)) +
  labs(x = "log10(Average Genetic Load)", 
       y = "Log2 Fold Change",
       title = "MA Plot of Genetic Load Differences",
       subtitle = paste(sum(ma_plot_data$Significant, na.rm = TRUE), "significant genes")) +
  theme_classic()

print(ma_plot)

# ggsave(ma_plot ,filename ="D:\\biosoft\\1000thal\\mtDNA\\figure/figure6.MAplot of genetic load.pdf",
#        device = pdf,width = 8,height = 8,dpi = 600)

# (2) 箱线图显示前几个差异最显著的Gene
top_genes <- head(significant_genes$Gene, 9)

top_genes_data <- combined_data %>%
  pivot_longer(cols = all_of(top_genes), 
               names_to = "Gene", 
               values_to = "Genetic Load") %>%
  mutate(Gene = factor(Gene, levels = top_genes))

boxplot <- ggplot(top_genes_data, aes(x = Cohort, y = `Genetic Load`, fill = Cohort)) +
  geom_boxplot() +
  facet_wrap(~ Gene, scales = "free_y", ncol = 3) +
  labs(title = "Genetic Load of Top Differential Genes") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

boxplot
# ggsave(boxplot ,filename ="D:\\biosoft\\1000thal\\mtDNA\\figure/figure6.boxplot of genetic load from top genes.pdf",
#        device = pdf,width = 8,height = 8,dpi = 600)

# (3) Heatmap显示所有Gene的表达模式

# # 准备Heatmap数据（以Sample为行，Gene为列）
# heatmap_data <- combined_data %>%
#   column_to_rownames("Sample") %>%
#   select(-Cohort) %>%
# t()  # 转置，Gene在行，Sample在列
# 
# # AddCohortAnnotation
# annotation_col <- data.frame(
#   Cohort = combined_data$Cohort,
#   row.names = combined_data$Sample
# )
# 
# # Plot heatmap
# pheatmap(heatmap_data[, 1:50],  # 只显示前50个Sample，避免过大
#          annotation_col = annotation_col,
#          show_colnames = FALSE,
#          main = "Gene Expression Heatmap (First 50 samples)")
# 












### 如果需要做功能Enrichment分析 ###
library(clusterProfiler)
library(org.Hs.eg.db)

### AssumeGene名areEntrez ID或Symbol ###
### 这里需要根据你的GeneIdentifier符类型Adjust ###

### 示例：如果Gene名areSymbol ###

sig_gene_list <- significant_genes$Gene

# GOEnrichment分析
ego <- enrichGO(gene          = sig_gene_list,
                OrgDb         = org.Hs.eg.db,
                keyType       = "SYMBOL",  # 根据实际Adjust
                ont           = "BP",
                pAdjustMethod = "BH",
                qvalueCutoff  = 0.05)

# Inspect results
p <-dotplot(ego, showCategory = 20, title = "GO Enrichment Analysis")



ggsave("D:\\biosoft\\1000thal\\mtDNA\\figure/figure6.GO_Enrichment_highres.pdf", p, 
       width = 10, height = 8, 
       dpi = 300,  # 分辨率
       device = "pdf")









