rm(list=ls())#clear Global Environment
# Load packages
library(ggplot2) # Create Elegant Data Visualisations Using the Grammar of Graphics
library(ggsignif) # Significance Brackets for 'ggplot2'
library(gghalves) # Compose Half-Half Plots Using Your Favourite Geoms
library(dplyr)
library(readxl)
library(ggstatsplot)
library(tidyr)
library(ggpubr)
library(data.table)
library(survival)
library(survminer)
library(ggsci)
library(ggbeeswarm)
#####
header1020 <- read.table("D:\\biosoft\\1000thal\\mtDNA\\genemut\\header.1020",sep = "\t")
header409 <- read.table("D:\\biosoft\\1000thal\\mtDNA\\genemut\\409/header.409",sep = "\t")
sampID1020 <- header1020[7:1026]
sampID409 <- header409[7:415]

#
hl1020 <- read.table("D:\\biosoft\\1000thal\\mtDNA\\genemut\\1020.mt.ano.filter.hl2zero") %>% 
  filter(!V2 %in% c(301, 302, 310, 316, 3107, 16182)) %>%  #FiltergnomAD认定的容易错的点
  mutate(V3=paste(V2,V4,V5,sep = "_")) %>%
  # filter(rowSums(. >= 0.05) > 0) %>%
  filter(rowSums(across(7:ncol(.), ~ . >= 0.05)) > 0) %>%  # 只对第7列到最后一列进行Filter
  distinct(.keep_all = TRUE,V3) %>% 
  filter(!grepl(",",V5))


hl409 <- read.table("D:\\biosoft\\1000thal\\mtDNA\\genemut\\409\\409.mt.ano.filter.hl2zero") %>% 
  filter(!V2 %in% c(301, 302, 310, 316, 3107, 16182)) %>%  #FiltergnomAD认定的容易错的点
  mutate(V3=paste(V2,V4,V5,sep = "_")) %>%
  # filter(rowSums(. >= 0.05) > 0) %>%
  filter(rowSums(across(7:ncol(.), ~ . >= 0.05)) > 0) %>%  # 只对第7列到最后一列进行Filter
  distinct(.keep_all = TRUE,V3) %>% 
  filter(!grepl(",",V5))

colname1020 <- c("chrom","pos","id","ref","alt","info",sampID1020)
colname409 <- c("chrom","pos","id","ref","alt","info",sampID409)
colnames(hl1020) <- colname1020
colnames(hl409) <- colname409

### 将大于等于0.05的值变为0 ###
hl1020[ , 7:1026][hl1020[ , 7:1026] <= 0.1] <- 0
hl409[ , 7:415][hl409[ , 7:415] <= 0.1] <- 0

hl1020[ , 7:1026][hl1020[ , 7:1026] >= 0.9] <- 0
hl409[ , 7:415][hl409[ , 7:415] >= 0.9] <- 0

# mtDNA gene features
mtgff3 <- read.table("D:\\biosoft\\1000thal\\mtDNA\\genemut\\1020bedout\\mt.gff3.bed",sep = "\t")
mtgff3$V2 <- mtgff3$V2 + 1
mtgff3$"with" <- mtgff3$V3 - mtgff3$V2 + 1
mtgff3[1,6] <- "D_loop"
mtgff3[39,6] <- "D_loop"
colnames(mtgff3) <- c("chrom","start","end","id","strand","gene","with")
coding_bed <- c("ND1","ND2","COX1","COX2","ATP6","COX3","ND3","ND4","ND5","ND6","CYTB","ATP8","ND4L")
##
setDT(hl1020)
setDT(hl409)
setDT(mtgff3)
#
hl1020[mtgff3, GENE := i.gene, on = .(pos >= start, pos <= end)] 
hl409[mtgff3, GENE := i.gene, on = .(pos >= start, pos <= end)]

hl1020$GENE[is.na(hl1020$GENE)] <- "intergenic"
hl409$GENE[is.na(hl409$GENE)] <- "intergenic"
###
child_info <- subset(hl1020,select = c("chrom","pos","id","ref","alt","info"))
parents_info <- subset(hl409,select = c("chrom","pos","id","ref","alt","info"))


all_id <- full_join(child_info,parents_info,by="id") %>% 
  mutate(chrom = coalesce(chrom.x, chrom.y),
         pos = coalesce(pos.x,pos.y),
         ref = coalesce(ref.x,ref.y),
         alt = coalesce(alt.x,alt.y),
         info = coalesce(info.x,info.y)) %>% 
  select(c(chrom,pos,id,ref,alt,info))

re_colname1020 <- colname1020[7:1026]

bed_df <- data.frame(Chrom = character(),
                     Start = numeric(),
                     End = numeric(),
                     GENE = character(),
                     HL = numeric(),
                     Sample = character(),
                     id = character(),
                     info = character(),
                     type = character(),
                     stringsAsFactors = FALSE)

re_colname409 <- colname409[7:415]

bed_df2 <- data.frame(Chrom = character(),
                      Start = numeric(),
                      End = numeric(),
                      GENE = character(),
                      HL = numeric(),
                      Sample = character(),
                      id=character(),
                      info = character(),
                      type = character(),
                      stringsAsFactors = FALSE)
####
for (i in 1:length(re_colname1020)) {
  child <- as.character(re_colname1020[i])
  #
  child_col <- subset(hl1020,select = c(child,"GENE"))
  child_df <- cbind(child_info,child_col)
  
  snv_df <- full_join(child_df,all_id,by="id") %>% 
    mutate(chrom = coalesce(chrom.x, chrom.y),
           pos = coalesce(pos.x,pos.y),
           ref = coalesce(ref.x,ref.y),
           alt = coalesce(alt.x,alt.y),
           info = coalesce(info.x,info.y)) %>% 
    select(-c(chrom.x,pos.x,ref.x,alt.x,info.x,
              chrom.y,pos.y,ref.y,alt.y,info.y)) %>% 
    mutate(across(everything(), ~ifelse(is.na(.), 0, .))) %>% 
    filter(!grepl(",", id)) %>% 
    # filter(!(nchar(alt) != 1 | nchar(ref) != 1)) %>% 
    filter(!!sym(child)>0.05) %>% 
    filter(!!sym(child)<0.95)
  
  bed_output <- snv_df %>%
    mutate(
      Chrom = "mtDNA",                  # BED 第1列：染色体
      Start = pos - 1,                # BED 第2列：起始位置（0-based）
      End = pos + nchar(alt) - 1,     # BED 第3列：结束位置（计算变异长度）
      GENE = GENE,  # BED 第4列：名称（自定义格式）
      HL = !!sym(child),           # BED 第5列：分值（这里用HL值）
      Sample = child,                  # BED 第6列：链（默认未知）
      id = id,
      info=info ,
      type = ifelse(nchar(alt) == 1 & nchar(ref) == 1,"SNV","INDEL") %>% as.character()
    ) %>%
    select(Chrom, Start, End, GENE, HL, Sample,id,info,type)  # 标准BED6格式
  
  bed_df <- bind_rows(
    bed_df,  # 原有数据（如果有）
    bed_output
  )
}


for (i in 1:length(re_colname409)) {
  parents <- as.character(re_colname409[i])
  
  parents_col <- subset(hl409,select = c(parents,"GENE"))
  parents_df <- cbind(parents_info,parents_col)
  
  snv_df <- full_join(parents_df,all_id,by="id") %>% 
    mutate(chrom = coalesce(chrom.x, chrom.y),
           pos = coalesce(pos.x,pos.y),
           ref = coalesce(ref.x,ref.y),
           alt = coalesce(alt.x,alt.y),
           info = coalesce(info.x,info.y)) %>% 
    select(-c(chrom.x,pos.x,ref.x,alt.x,info.x,
              chrom.y,pos.y,ref.y,alt.y,info.y)) %>% 
    mutate(across(everything(), ~ifelse(is.na(.), 0, .))) %>% 
    filter(!grepl(",", id)) %>% 
    # filter(!(nchar(alt) != 1 | nchar(ref) != 1)) %>% 
    filter(!!sym(parents)>0.05) %>% 
    filter(!!sym(parents)<0.95)
  
  bed_output2 <- snv_df %>%
    mutate(
      Chrom = "mtDNA",                  # BED 第1列：染色体
      Start = pos - 1,                # BED 第2列：起始位置（0-based）
      End = pos + nchar(alt) - 1,     # BED 第3列：结束位置（计算变异长度）
      GENE = GENE,  # BED 第4列：名称（自定义格式）
      HL = !!sym(parents),           # BED 第5列：分值（这里用HL值）
      Sample = parents,                    # BED 第6列：链（默认未知）
      id = id,
      info=info ,
      type = ifelse(nchar(alt) == 1 & nchar(ref) == 1,"SNV","INDEL") %>% as.character()
    ) %>%
    select(Chrom, Start, End, GENE, HL, Sample,id,info,type)  # 标准BED6格式
  
  bed_df2 <- bind_rows(
    bed_df2,  # 原有数据（如果有）
    bed_output2
  )
  
  
}

bed2freq <- bed_df %>% 
  group_by(id) %>% 
  mutate(sum = n()) %>% 
  distinct(End,id,sum)%>% 
  filter(sum >51)
# %>%
  # filter(!(between(End, 66, 71) | 
  #            between(End, 300, 316) | 
  #            between(End, 513, 525) | 
  #            between(End, 3106, 3107) | 
  #            between(End, 12418, 12425) | 
  #            between(End, 16182, 16194)))

#(66–71, 300–316, 513–525, 3106–3107, 12418–12425 and 16182–16194) were excluded.


#
# dfdnm <- read.table("D:/biosoft/1000thal/mtDNA/1020 DNM per sample.tsv",header = T)
dfmutation <- read.table("D:/biosoft/1000thal/mtDNA/1020mutations per sample cutoff.tsv",header = T) %>%
  rename(HID = id) %>%
  filter(!is.na(HID))

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
colnames(dfmtadj)

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
               "mtCN,adj",  "Survival_time_without_transfusion", "Sex","Age",
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

bed2pos1 <- bed_df %>% 
  filter(id == "14755_A_C") %>% 
  select(Sample,HL) %>% 
  rename(HL14755 = HL ,
         HID = Sample)

bed2pos2 <- bed_df %>% 
  filter(id == "14775_T_C") %>% 
  select(Sample,HL) %>% 
  rename(HL14775 = HL ,
         HID = Sample)

bed2pos3 <- bed_df %>% 
  filter(id == "16179_CAA_C") %>% 
  select(Sample,HL) %>% 
  rename(HL16179 = HL ,
         HID = Sample)

# table(input2$HBB_genotype_category,input2$group1)

input_add <- input %>% 
  left_join(bed2pos1,by=c("HID")) %>% 
  left_join(bed2pos2,by=c("HID")) %>% 
  left_join(bed2pos3,by=c("HID")) %>% 
  replace_na(list(HL14775 = 0, HL14755 = 0, HL16179 = 0))


group_stat_test <- function(df, group_col, phen_cols, 
                            test_type = c("t.test", "wilcox"),
                            p_adjust = "none") {
  
  ### 参数Check ###
  test_type <- match.arg(test_type)
  if (!all(c(group_col, phen_cols) %in% names(df))) {
    stop("指定的列名不在数据框中")
  }
  
  ### 确保分组列are因子并获取所有组别 ###
  df[[group_col]] <- as.factor(df[[group_col]])
  groups <- levels(df[[group_col]])
  if (length(groups) < 2) stop("分组变量需要至少2个水平")
  
  ### 生成所有两两组合 ###
  group_pairs <- combn(groups, 2, simplify = FALSE)
  
  ### 准备Results数据框 ###
  results <- data.frame()
  
  ### 对每个Phenotype进行分析 ###
  for (phen in phen_cols) {
    ### 对每对组别进行Compare ###
    for (pair in group_pairs) {
      group1 <- pair[1]
      group2 <- pair[2]
      
      ### 提取两组数据 ###
      data1 <- df[df[[group_col]] == group1, phen, drop = TRUE]
      data2 <- df[df[[group_col]] == group2, phen, drop = TRUE]
      
      ### RemoveNA值 ###
      data1 <- data1[!is.na(data1)]
      data2 <- data2[!is.na(data2)]
      
      ### 计算每组的均值andSample量 ###
      mean1 <- mean(data1)
      mean2 <- mean(data2)
      n1 <- length(data1)
      n2 <- length(data2)
      
      ### 只有当两组都有数据时才进行test ###
      if (n1 >= 2 && n2 >= 2) {
        if (test_type == "t.test") {
          test_result <- tryCatch(
            t.test(data1, data2)$p.value,
            error = function(e) NA
          )
        } else {
          test_result <- tryCatch(
            wilcox.test(data1, data2)$p.value,
            error = function(e) NA
          )
        }
      } else {
        test_result <- NA
      }
      
      # Add到Results
      results <- rbind(results, data.frame(
        phenotype = phen,
        group1 = group1,
        group2 = group2,
        mean_group1 = mean1,
        mean_group2 = mean2,
        n_group1 = n1,
        n_group2 = n2,
        p_value = test_result,
        stringsAsFactors = FALSE
      ))
    }
  }
  
  # p-valuescorrection
  if (p_adjust != "none") {
    results$adjusted_p <- p.adjust(results$p_value, method = p_adjust)
  }
  
  ### 重命名p-values列以反映test类型 ###
  colnames(results)[colnames(results) == "p_value"] <- paste0(test_type, "_p_value")
  
  return(results)
}

#####
input3<- input_add %>%
  # mutate(across(all_of(c("counts_dnm","counts_inherit")), ~ifelse(is.na(.), 0, .))) %>% 
  mutate(
    group = case_when(
      HL16179 <= 0.1 ~ "no-mutate",
      HL16179  > 0.1 ~ "mt-mutate"
    )
  ) %>% 
  filter(HBB_genotype_category %in% c("β0/β0"))
# filter(six_genotype_category %in% c("β0/β0"))
table(input3$group)

input4<- input_add %>%
  # mutate(across(all_of(c("counts_dnm","counts_inherit")), ~ifelse(is.na(.), 0, .))) %>% 
  mutate(
    group = case_when(
      HL16179 <= 0.1 ~ "no-mutate",
      HL16179  > 0.1 ~ "mt-mutate"
    )
  ) %>%
  # filter(HBB_genotype_category %in% c("β0/β+"))
  filter(six_genotype_category %in% c("β0/β+"))
table(input4$group)

#####
input5<- input_add %>%
  # mutate(across(all_of(c("counts_dnm","counts_inherit")), ~ifelse(is.na(.), 0, .))) %>% 
  mutate(
    group = case_when(
      HL14755 <= 0.1 ~ "NO-MUT",
      HL14755  > 0.1 ~ "MT14755-MUT"
    )
  ) %>% 
  filter(HBB_genotype_category %in% c("β0/β0"))
# filter(six_genotype_category %in% c("β0/β0"))
table(input5$group)

input6<- input_add %>%
  # mutate(across(all_of(c("counts_dnm","counts_inherit")), ~ifelse(is.na(.), 0, .))) %>% 
  mutate(
    group = case_when(
      HL14755 <= 0.1 ~ "NO-MUT",
      HL14755  > 0.1 ~ "MT14755-MUT"
    )
  ) %>%
  filter(HBB_genotype_category %in% c("β0/β+"))
# filter(six_genotype_category %in% c("β0/β+"))
table(input6$group)
#####
input7<- input_add %>%
  # mutate(across(all_of(c("counts_dnm","counts_inherit")), ~ifelse(is.na(.), 0, .))) %>% 
  mutate(
    group = case_when(
      HL14775 <= 0.1  ~ "NO-MUT",
      HL14775  > 0.1 ~ "MT14775-MUT"
    )
  ) %>% 
  filter(HBB_genotype_category %in% c("β0/β0"))
# filter(six_genotype_category %in% c("β0/β0"))
table(input7$group)

input8<- input_add %>%
  # mutate(across(all_of(c("counts_dnm","counts_inherit")), ~ifelse(is.na(.), 0, .))) %>% 
  mutate(
    group = case_when(
      HL14775 <=0.1 ~ "NO-MUT",
      HL14775  > 0.1 ~ "MT14775-MUT"
    )
  ) %>%
  filter(HBB_genotype_category %in% c("β0/β+"))
# filter(six_genotype_category %in% c("β0/β+"))
table(input8$group)
#Annual_transfusions #HbF #Serum_Ferritin Survival_time_without_transfusion

write.csv(input8,"D:\\biosoft\\1000thal\\mtDNA\\CYTB\\MTSNV/MT14775.beta+.phe.csv")
### stat_test ###
result_t <- group_stat_test(
  df = input8,
  group_col = "group",
  phen_cols = c("Annual_transfusions", "Survival_time_without_transfusion", "HbF",
                "Soluble_transferrin_receptor", "Iron", "Serum_Ferritin", "Transferrin","Transferrin_Saturation","Unsaturated_iron_binding_capacity","Total_iron_binding_capacity" ),
  test_type = "wilcox", p_adjust = "BH"
)


# my_comparisons <- list(c("mt-mutate", "no-mutate"))



### test ###
p1 <- ggbetweenstats(
  data = input5,
  x = group,
  y = HbF,  #Annual_transfusions #HbF #Serum_Ferritin
  plot.type = "boxviolin",
  # bf.message = F,
  # var.equal = TRUE,
  # type = "parametric",
  type = "nonparametric",
  pairwise.display = "significant",#significant
  p.adjust.method = "none",
  title = NULL, #"Distribution of sepal length across Iris species",
  xlab = "",
  ylab = "Serum Ferritin in β0/β0",
  results.subtitle = T,#决定是否将Statistical test的Results显示为副标题（默认TRUE）;如果设置为FALSE,则仅返回绘图
  subtitle = NULL,#副标题,默认显示统计test results,自定义则results.subtitle=FALSE
  outlier.tagging = TRUE,#whether or not标记离群异常值，默认FALSE
  outlier.shape = 19,#异常值形状,可Set为NA将其隐藏（不are删除，因此不会影响统计test results）
  outlier.color = "pink",#异常值颜色
  outlier.label.args = list(size = 4),#异常值标签大小
  point.args = list(position = ggplot2::position_jitterdodge(dodge.width = 0.6),
                    alpha = 0.5, size = 3, stroke = 0),#传递给geom_point的参数Set
  ggplot.component = list(
    # scale_x_discrete(labels = c("Iron overload (<1500)", "Iron overload (1500~5000)","Iron overload (>5000)")),
    theme(axis.text= element_text(size = 12, face = "bold"),
          axis.title.y.right = element_blank(), 
          axis.text.y.right = element_blank(), 
          axis.ticks.y.right = element_blank(), 
          axis.title.y = element_text(face = "bold", size = 20))
  ),
  scale_color_manual(values = c("MT14755-MUT" = "#FF6B6B", "NO-MUT" = "#4ECDC4")),  # 散点/箱线图边框颜色
  scale_fill_manual(values = c("MT14755-MUT" = "#FF6B6B", "NO-MUT" = "#4ECDC4"))   # 箱线图填充色
  # violin.args = list(width = 0.4, alpha = 0.2),#传递给geom_violin的参数Set
  # ggtheme = theme_classic(),#主题修改，可直接调用ggplot2的主题，默认主题为ggstatsplot::theme_ggstatsplot()
  # package = "ggsci",#提取调色板所需的包
  # palette = "uniform_startrek"#选择提取包中的调色板
)



p1
# ggsave(p1,filename ="D:\\biosoft\\1000thal\\mtDNA\\figure/figure5D.Serum Ferritin across type of mutation in β0β0.pdf",
#        device = pdf,width = 12,height = 8,dpi = 600)




df1 <- input5 %>% 
  rename(Sur_time = Survival_time_without_transfusion) %>% 
  mutate(status = ifelse(is.na(Sur_time),0,1)) %>%
  mutate(Sur_time = coalesce(Sur_time, Age))

coxph(Surv(Sur_time, status) ~ group, data=df1)
fit1 <- survfit(Surv(Sur_time, status) ~ group, data=df1)

ggsurvplot(fit1, data=df1, pval=TRUE, conf.int=TRUE)





# ggsave(p2,filename ="D:\\biosoft\\1000thal\\mtDNA\\figure/figure5D.Serum Ferritin across type of mutation in β0β+.pdf",
#        device = pdf,width = 12,height = 8,dpi = 600)










######HL14775###HbF######

p11 <- ggbetweenstats(
  data = input7,
  x = group,
  y = HbF,  #Annual_transfusions #HbF #Serum_Ferritin
  plot.type = "boxviolin",
  # bf.message = F,
  # var.equal = TRUE,
  # type = "parametric",
  type = "nonparametric",
  # pairwise.display = "none",#significant
  # p.adjust.method = "none",
  title = NULL, #"Distribution of sepal length across Iris species",
  xlab = "",
  ylab = "HbF in β0/β0",
  results.subtitle = T,#决定是否将Statistical test的Results显示为副标题（默认TRUE）;如果设置为FALSE,则仅返回绘图
  subtitle = NULL,#副标题,默认显示统计test results,自定义则results.subtitle=FALSE
  # outlier.tagging = FALSE,#whether or not标记离群异常值，默认FALSE
  # outlier.shape = NA,#异常值形状,可Set为NA将其隐藏（不are删除，因此不会影响统计test results）
  # outlier.color = "pink",#异常值颜色
  # outlier.label.args = list(size = 4),#异常值标签大小
  centrality.type = "parameteric",
  centrality.label.args = list(size = 8, nudge_x = 0.4, segment.linetype = 4,
                               min.segment.length = 0),
  point.args = list(
    position = ggplot2::position_jitterdodge(dodge.width = 0.6),
    alpha = 0.5, size = 3, stroke = 0
  ),
  violin.args = list(
    width = 0.3,       # 减小小提琴width（默认0.4）
    alpha = 0.2,
    linewidth = 0.2    # 减小小提琴边框粗细（默认1）
  ),
  boxplot.args = list(
    width = 0.1,       # 减小箱线图width（默认0.2）
    linewidth = 0.2,   # 减小箱线图边框粗细（默认1）
    alpha = 0.5
  ),
  ggplot.component = list(
    scale_x_discrete(labels = c("MT14775-MUT", "NO-MUT")),
    scale_y_continuous(limits = c(0, 70)),  # Add这行Sety轴范围
    theme(axis.text= element_text(size = 15, face = "bold"),
          axis.title.y.right = element_blank(), 
          axis.text.y.right = element_blank(), 
          axis.ticks.y.right = element_blank(), 
          axis.title.y = element_text(face = "bold", size = 20))
  ),
  ggtheme = theme_classic(),#主题修改，可直接调用ggplot2的主题，默认主题为ggstatsplot::theme_ggstatsplot()
  package = "ggsci",#提取调色板所需的包
  palette = "uniform_startrek"#选择提取包中的调色板
) + 
  ggsignif::geom_signif(
  comparisons = list(c("MT14775-MUT", "NO-MUT")),  # SpecifyCompare组
  test = "wilcox.test",  # 使用非参数test
  map_signif_level = TRUE,  # 显示星号（*）而非 p 值
  y_position = 60,         # Adjust标记的 y 轴Position
  tip_length = 0.01,       # Adjust横线两端的短竖线长度
  textsize = 5,            # Adjust文本大小
  vjust = 0.2              # Adjust文本垂直Position
)

p21 <- ggbetweenstats(
  data = input8,
  x = group,
  y = HbF,  #Annual_transfusions #HbF #Serum_Ferritin
  plot.type = "boxviolin",
  # bf.message = F,
  # var.equal = TRUE,
  # type = "parametric",
  type = "nonparametric",
  pairwise.display = "significant",#significant
  p.adjust.method = "none",
  title = NULL, #"Distribution of sepal length across Iris species",
  xlab = "",
  ylab = "HbF in β0/β+",
  results.subtitle = T,#决定是否将Statistical test的Results显示为副标题（默认TRUE）;如果设置为FALSE,则仅返回绘图
  subtitle = NULL,#副标题,默认显示统计test results,自定义则results.subtitle=FALSE
  # outlier.tagging = TRUE,#whether or not标记离群异常值，默认FALSE
  # outlier.shape = 19,#异常值形状,可Set为NA将其隐藏（不are删除，因此不会影响统计test results）
  # outlier.color = "pink",#异常值颜色
  # outlier.label.args = list(size = 4),#异常值标签大小
  centrality.type = "parameteric",
  centrality.label.args = list(size = 8, nudge_x = 0.4, segment.linetype = 4,
                               min.segment.length = 0),
  point.args = list(position = ggplot2::position_jitterdodge(dodge.width = 0.6),
                    alpha = 0.5, size = 3, stroke = 0),#传递给geom_point的参数Set
  violin.args = list(
    width = 0.3,       # 减小小提琴width（默认0.4）
    alpha = 0.2,
    linewidth = 0.2    # 减小小提琴边框粗细（默认1）
  ),
  boxplot.args = list(
    width = 0.1,       # 减小箱线图width（默认0.2）
    linewidth = 0.2,   # 减小箱线图边框粗细（默认1）
    alpha = 0.5
  ),
  ggplot.component = list(
    scale_x_discrete(labels = c("MT14775-MUT", "NO-MUT")),
    scale_y_continuous(limits = c(0, 70)),  # Add这行Sety轴范围
    theme(axis.text= element_text(size = 15, face = "bold"),
          axis.title.y.right = element_blank(), 
          axis.text.y.right = element_blank(), 
          axis.ticks.y.right = element_blank(), 
          axis.title.y = element_text(face = "bold", size = 20))
  ),
  ggtheme = theme_classic(),#主题修改，可直接调用ggplot2的主题，默认主题为ggstatsplot::theme_ggstatsplot()
  package = "ggsci",#提取调色板所需的包
  palette = "uniform_startrek"#选择提取包中的调色板
) + 
  ggsignif::geom_signif(
  comparisons = list(c("MT14775-MUT", "NO-MUT")),  # SpecifyCompare组
  test = "wilcox.test",  # 使用非参数test
  map_signif_level = TRUE,  # 显示星号（*）而非 p 值
  y_position = 60,         # Adjust标记的 y 轴Position
  tip_length = 0.01,       # Adjust横线两端的短竖线长度
  textsize = 5,            # Adjust文本大小
  vjust = 0.2              # Adjust文本垂直Position
)
p11
p21
ggsave(p11,filename ="D:\\biosoft\\1000thal\\mtDNA\\figure/figure5G.HbF across MT14775 in β0β0.pdf",
       device = pdf,width = 12,height = 8,dpi = 600)
ggsave(p21,filename ="D:\\biosoft\\1000thal\\mtDNA\\figure/figure5G.HbF across MT14775 in β0β+.pdf",
       device = pdf,width = 12,height = 8,dpi = 600)

######HL14775###Soluble_transferrin_receptor######
p12 <- ggbetweenstats(
  data = input7,
  x = group,
  y = Soluble_transferrin_receptor,  #Annual_transfusions #HbF #Serum_Ferritin
  plot.type = "boxviolin",
  # bf.message = F,
  # var.equal = TRUE,
  # type = "parametric",
  type = "nonparametric",
  pairwise.display = "significant",#significant
  p.adjust.method = "none",
  title = NULL, #"Distribution of sepal length across Iris species",
  xlab = "",
  ylab = "Soluble transferrin receptor in β0/β0",
  results.subtitle = T,#决定是否将Statistical test的Results显示为副标题（默认TRUE）;如果设置为FALSE,则仅返回绘图
  subtitle = NULL,#副标题,默认显示统计test results,自定义则results.subtitle=FALSE
  outlier.tagging = TRUE,#whether or not标记离群异常值，默认FALSE
  outlier.shape = 19,#异常值形状,可Set为NA将其隐藏（不are删除，因此不会影响统计test results）
  outlier.color = "pink",#异常值颜色
  outlier.label.args = list(size = 4),#异常值标签大小
  centrality.type = "parameteric",
  centrality.label.args = list(size = 8, nudge_x = 0.4, segment.linetype = 4,
                               min.segment.length = 0),
  point.args = list(
    position = ggplot2::position_jitterdodge(dodge.width = 0.6),
    alpha = 0.5, size = 3, stroke = 0
  ),
  violin.args = list(
    width = 0.3,       # 减小小提琴width（默认0.4）
    alpha = 0.2,
    linewidth = 0.2    # 减小小提琴边框粗细（默认1）
  ),
  boxplot.args = list(
    width = 0.1,       # 减小箱线图width（默认0.2）
    linewidth = 0.2,   # 减小箱线图边框粗细（默认1）
    alpha = 0.5
  ),
  ggplot.component = list(
    scale_x_discrete(labels = c("MT14775-MUT", "NO-MUT")),
    scale_y_continuous(limits = c(0, 7)),  # Add这行Sety轴范围
    theme(axis.text= element_text(size = 15, face = "bold"),
          axis.title.y.right = element_blank(), 
          axis.text.y.right = element_blank(), 
          axis.ticks.y.right = element_blank(), 
          axis.title.y = element_text(face = "bold", size = 20))
  ),
  ggtheme = theme_classic(),#主题修改，可直接调用ggplot2的主题，默认主题为ggstatsplot::theme_ggstatsplot()
  package = "ggsci",#提取调色板所需的包
  palette = "uniform_startrek"#选择提取包中的调色板
)+
  ggsignif::geom_signif(
    comparisons = list(c("MT14775-MUT", "NO-MUT")),  # SpecifyCompare组
    test = "wilcox.test",  # 使用非参数test
    map_signif_level = TRUE,  # 显示星号（*）而非 p 值
    y_position = 6,         # Adjust标记的 y 轴Position
    tip_length = 0.01,       # Adjust横线两端的短竖线长度
    textsize = 5,            # Adjust文本大小
    vjust = 0.2              # Adjust文本垂直Position
  )



p12


p22 <- ggbetweenstats(
  data = input8,
  x = group,
  y = Soluble_transferrin_receptor,  #Annual_transfusions #HbF #Serum_Ferritin
  plot.type = "boxviolin",
  # bf.message = F,
  # var.equal = TRUE,
  # type = "parametric",
  type = "nonparametric",
  pairwise.display = "significant",#significant
  p.adjust.method = "none",
  title = NULL, #"Distribution of sepal length across Iris species",
  xlab = "",
  ylab = "Soluble transferrin receptor in β0/β+",
  results.subtitle = T,#决定是否将Statistical test的Results显示为副标题（默认TRUE）;如果设置为FALSE,则仅返回绘图
  subtitle = NULL,#副标题,默认显示统计test results,自定义则results.subtitle=FALSE
  outlier.tagging = TRUE,#whether or not标记离群异常值，默认FALSE
  outlier.shape = 19,#异常值形状,可Set为NA将其隐藏（不are删除，因此不会影响统计test results）
  outlier.color = "pink",#异常值颜色
  outlier.label.args = list(size = 4),#异常值标签大小
  centrality.type = "parameteric",
  centrality.label.args = list(size = 8, nudge_x = 0.4, segment.linetype = 4,
                               min.segment.length = 0),
  point.args = list(
    position = ggplot2::position_jitterdodge(dodge.width = 0.6),
    alpha = 0.5, size = 3, stroke = 0
  ),
  violin.args = list(
    width = 0.3,       # 减小小提琴width（默认0.4）
    alpha = 0.2,
    linewidth = 0.2    # 减小小提琴边框粗细（默认1）
  ),
  boxplot.args = list(
    width = 0.1,       # 减小箱线图width（默认0.2）
    linewidth = 0.2,   # 减小箱线图边框粗细（默认1）
    alpha = 0.5
  ),
  ggplot.component = list(
    scale_x_discrete(labels = c("MT14775-MUT", "NO-MUT")),
    scale_y_continuous(limits = c(0, 7)),  # Add这行Sety轴范围
    theme(axis.text= element_text(size = 15, face = "bold"),
          axis.title.y.right = element_blank(), 
          axis.text.y.right = element_blank(), 
          axis.ticks.y.right = element_blank(), 
          axis.title.y = element_text(face = "bold", size = 20))
  ),
  ggtheme = theme_classic(),#主题修改，可直接调用ggplot2的主题，默认主题为ggstatsplot::theme_ggstatsplot()
  package = "ggsci",#提取调色板所需的包
  palette = "uniform_startrek"#选择提取包中的调色板
)+
  ggsignif::geom_signif(
    comparisons = list(c("MT14775-MUT", "NO-MUT")),  # SpecifyCompare组
    test = "wilcox.test",  # 使用非参数test
    map_signif_level = TRUE,  # 显示星号（*）而非 p 值
    y_position = 6,         # Adjust标记的 y 轴Position
    tip_length = 0.01,       # Adjust横线两端的短竖线长度
    textsize = 5,            # Adjust文本大小
    vjust = 0.2              # Adjust文本垂直Position
  )



p22
p12
ggsave(p12,filename ="D:\\biosoft\\1000thal\\mtDNA\\figure/figure5G.Soluble_transferrin_receptor across MT14775 in β0β0.pdf",
       device = pdf,width = 12,height = 8,dpi = 600)
ggsave(p22,filename ="D:\\biosoft\\1000thal\\mtDNA\\figure/figure5G.Soluble_transferrin_receptor across MT14775 in β0β+.pdf",
       device = pdf,width = 12,height = 8,dpi = 600)

######HL14775###Serum_Ferritin######
p13 <- ggbetweenstats(
  data = input5,
  x = group,
  y = Serum_Ferritin,  #Annual_transfusions #HbF #Serum_Ferritin
  plot.type = "boxviolin",
  # bf.message = F,
  # var.equal = TRUE,
  # type = "parametric",
  type = "nonparametric",
  pairwise.display = "significant",#significant
  p.adjust.method = "none",
  title = NULL, #"Distribution of sepal length across Iris species",
  xlab = "",
  ylab = "Serum Ferritin in β0/β0",
  results.subtitle = T,#决定是否将Statistical test的Results显示为副标题（默认TRUE）;如果设置为FALSE,则仅返回绘图
  subtitle = NULL,#副标题,默认显示统计test results,自定义则results.subtitle=FALSE
  outlier.tagging = TRUE,#whether or not标记离群异常值，默认FALSE
  outlier.shape = 19,#异常值形状,可Set为NA将其隐藏（不are删除，因此不会影响统计test results）
  outlier.color = "pink",#异常值颜色
  outlier.label.args = list(size = 4),#异常值标签大小
  centrality.type = "parameteric",
  centrality.label.args = list(size = 8, nudge_x = 0.4, segment.linetype = 4,
                               min.segment.length = 0),
  point.args = list(
    position = ggplot2::position_jitterdodge(dodge.width = 0.6),
    alpha = 0.5, size = 3, stroke = 0
  ),
  violin.args = list(
    width = 0.3,       # 减小小提琴width（默认0.4）
    alpha = 0.2,
    linewidth = 0.2    # 减小小提琴边框粗细（默认1）
  ),
  boxplot.args = list(
    width = 0.1,       # 减小箱线图width（默认0.2）
    linewidth = 0.2,   # 减小箱线图边框粗细（默认1）
    alpha = 0.5
  ),
  ggplot.component = list(
    scale_x_discrete(labels = c("MT14775-MUT", "NO-MUT")),
    scale_y_continuous(limits = c(0, 15000)),  # Add这行Sety轴范围
    theme(axis.text= element_text(size = 15, face = "bold"),
          axis.title.y.right = element_blank(), 
          axis.text.y.right = element_blank(), 
          axis.ticks.y.right = element_blank(), 
          axis.title.y = element_text(face = "bold", size = 20))
  ),
  ggtheme = theme_classic(),#主题修改，可直接调用ggplot2的主题，默认主题为ggstatsplot::theme_ggstatsplot()
  package = "ggsci",#提取调色板所需的包
  palette = "uniform_startrek"#选择提取包中的调色板
)+
  ggsignif::geom_signif(
    comparisons = list(c("MT14775-MUT", "NO-MUT")),  # SpecifyCompare组
    test = "wilcox.test",  # 使用非参数test
    map_signif_level = TRUE,  # 显示星号（*）而非 p 值
    y_position = 15000,         # Adjust标记的 y 轴Position
    tip_length = 0.01,       # Adjust横线两端的短竖线长度
    textsize = 5,            # Adjust文本大小
    vjust = 0.2              # Adjust文本垂直Position
  )
# +
#   ggsignif::geom_signif(
#     comparisons = list(c("MT14775-MUT", "NO-MUT")),  # SpecifyCompare组
# test = "wilcox.test",  # 使用非参数test
#     map_signif_level = TRUE,  # 显示星号（*）而非 p 值
#     y_position = 15000,         # Adjust标记的 y 轴Position
#     tip_length = 0.01,       # Adjust横线两端的短竖线长度
# textsize = 5,            # Adjust文本大小
# vjust = 0.2              # Adjust文本垂直Position
#   )

p13

p23 <- ggbetweenstats(
  data = input6,
  x = group,
  y = Serum_Ferritin,  #Annual_transfusions #HbF #Serum_Ferritin
  plot.type = "boxviolin",
  # bf.message = F,
  # var.equal = TRUE,
  # type = "parametric",
  type = "nonparametric",
  pairwise.display = "significant",#significant
  p.adjust.method = "none",
  title = NULL, #"Distribution of sepal length across Iris species",
  xlab = "",
  ylab = "Serum Ferritin in β0/β+",
  results.subtitle = T,#决定是否将Statistical test的Results显示为副标题（默认TRUE）;如果设置为FALSE,则仅返回绘图
  subtitle = NULL,#副标题,默认显示统计test results,自定义则results.subtitle=FALSE
  outlier.tagging = TRUE,#whether or not标记离群异常值，默认FALSE
  outlier.shape = 19,#异常值形状,可Set为NA将其隐藏（不are删除，因此不会影响统计test results）
  outlier.color = "pink",#异常值颜色
  outlier.label.args = list(size = 4),#异常值标签大小
  centrality.type = "parameteric",
  centrality.label.args = list(size = 8, nudge_x = 0.4, segment.linetype = 4,
                               min.segment.length = 0),
  point.args = list(
    position = ggplot2::position_jitterdodge(dodge.width = 0.6),
    alpha = 0.5, size = 3, stroke = 0
  ),
  violin.args = list(
    width = 0.3,       # 减小小提琴width（默认0.4）
    alpha = 0.2,
    linewidth = 0.2    # 减小小提琴边框粗细（默认1）
  ),
  boxplot.args = list(
    width = 0.1,       # 减小箱线图width（默认0.2）
    linewidth = 0.2,   # 减小箱线图边框粗细（默认1）
    alpha = 0.5
  ),
  ggplot.component = list(
    scale_x_discrete(labels = c("MT14775-MUT", "NO-MUT")),
    scale_y_continuous(limits = c(0, 15000)),  # Add这行Sety轴范围
    theme(axis.text= element_text(size = 15, face = "bold"),
          axis.title.y.right = element_blank(), 
          axis.text.y.right = element_blank(), 
          axis.ticks.y.right = element_blank(), 
          axis.title.y = element_text(face = "bold", size = 20))
  ),
  ggtheme = theme_classic(),#主题修改，可直接调用ggplot2的主题，默认主题为ggstatsplot::theme_ggstatsplot()
  package = "ggsci",#提取调色板所需的包
  palette = "uniform_startrek"#选择提取包中的调色板
)+
  ggsignif::geom_signif(
    comparisons = list(c("MT14775-MUT", "NO-MUT")),  # SpecifyCompare组
    test = "wilcox.test",  # 使用非参数test
    map_signif_level = TRUE,  # 显示星号（*）而非 p 值
    y_position = 15000,         # Adjust标记的 y 轴Position
    tip_length = 0.01,       # Adjust横线两端的短竖线长度
    textsize = 5,            # Adjust文本大小
    vjust = 0.2              # Adjust文本垂直Position
  )

p23
p13
ggsave(p13,filename ="D:\\biosoft\\1000thal\\mtDNA\\figure/figure5G.Serum Ferritin across MT14775 in β0β0.pdf",
       device = pdf,width = 12,height = 8,dpi = 600)
ggsave(p23,filename ="D:\\biosoft\\1000thal\\mtDNA\\figure/figure5G.Serum Ferritin across MT14775 in β0β+.pdf",
       device = pdf,width = 12,height = 8,dpi = 600)

######HL14755###Iron######
p14 <- ggbetweenstats(
  data = input7,
  x = group,
  y = Iron,  #Annual_transfusions #HbF #Serum_Ferritin
  plot.type = "boxviolin",
  # bf.message = F,
  # var.equal = TRUE,
  # type = "parametric",
  type = "nonparametric",
  pairwise.display = "significant",#significant
  p.adjust.method = "none",
  title = NULL, #"Distribution of sepal length across Iris species",
  xlab = "",
  ylab = "Iron in β0/β0",
  results.subtitle = T,#决定是否将Statistical test的Results显示为副标题（默认TRUE）;如果设置为FALSE,则仅返回绘图
  subtitle = NULL,#副标题,默认显示统计test results,自定义则results.subtitle=FALSE
  outlier.tagging = TRUE,#whether or not标记离群异常值，默认FALSE
  outlier.shape = 19,#异常值形状,可Set为NA将其隐藏（不are删除，因此不会影响统计test results）
  outlier.color = "pink",#异常值颜色
  outlier.label.args = list(size = 4),#异常值标签大小
  centrality.type = "parameteric",
  centrality.label.args = list(size = 8, nudge_x = 0.4, segment.linetype = 4,
                               min.segment.length = 0),
  point.args = list(
    position = ggplot2::position_jitterdodge(dodge.width = 0.6),
    alpha = 0.5, size = 3, stroke = 0
  ),
  violin.args = list(
    width = 0.3,       # 减小小提琴width（默认0.4）
    alpha = 0.2,
    linewidth = 0.2    # 减小小提琴边框粗细（默认1）
  ),
  boxplot.args = list(
    width = 0.1,       # 减小箱线图width（默认0.2）
    linewidth = 0.2,   # 减小箱线图边框粗细（默认1）
    alpha = 0.5
  ),
  ggplot.component = list(
    scale_x_discrete(labels = c("MT14775-MUT", "NO-MUT")),
    scale_y_continuous(limits = c(0, 110)),  # Add这行Sety轴范围
    theme(axis.text= element_text(size = 15, face = "bold"),
          axis.title.y.right = element_blank(), 
          axis.text.y.right = element_blank(), 
          axis.ticks.y.right = element_blank(), 
          axis.title.y = element_text(face = "bold", size = 20))
  ),
  ggtheme = theme_classic(),#主题修改，可直接调用ggplot2的主题，默认主题为ggstatsplot::theme_ggstatsplot()
  package = "ggsci",#提取调色板所需的包
  palette = "uniform_startrek"#选择提取包中的调色板
)+
  ggsignif::geom_signif(
    comparisons = list(c("MT14775-MUT", "NO-MUT")),  # SpecifyCompare组
    test = "wilcox.test",  # 使用非参数test
    map_signif_level = TRUE,  # 显示星号（*）而非 p 值
    y_position = 100,         # Adjust标记的 y 轴Position
    tip_length = 0.01,       # Adjust横线两端的短竖线长度
    textsize = 5,            # Adjust文本大小
    vjust = 0.2              # Adjust文本垂直Position
  )

p14


p24 <- ggbetweenstats(
  data = input8,
  x = group,
  y = Iron,  #Annual_transfusions #HbF #Serum_Ferritin
  plot.type = "boxviolin",
  # bf.message = F,
  # var.equal = TRUE,
  # type = "parametric",
  type = "nonparametric",
  pairwise.display = "significant",#significant
  p.adjust.method = "none",
  title = NULL, #"Distribution of sepal length across Iris species",
  xlab = "",
  ylab = "Iron in β0/β+",
  results.subtitle = T,#决定是否将Statistical test的Results显示为副标题（默认TRUE）;如果设置为FALSE,则仅返回绘图
  subtitle = NULL,#副标题,默认显示统计test results,自定义则results.subtitle=FALSE
  outlier.tagging = TRUE,#whether or not标记离群异常值，默认FALSE
  outlier.shape = 19,#异常值形状,可Set为NA将其隐藏（不are删除，因此不会影响统计test results）
  outlier.color = "pink",#异常值颜色
  outlier.label.args = list(size = 4),#异常值标签大小
  centrality.type = "parameteric",
  centrality.label.args = list(size = 8, nudge_x = 0.4, segment.linetype = 4,
                               min.segment.length = 0),
  point.args = list(
    position = ggplot2::position_jitterdodge(dodge.width = 0.6),
    alpha = 0.5, size = 3, stroke = 0
  ),
  violin.args = list(
    width = 0.3,       # 减小小提琴width（默认0.4）
    alpha = 0.2,
    linewidth = 0.2    # 减小小提琴边框粗细（默认1）
  ),
  boxplot.args = list(
    width = 0.1,       # 减小箱线图width（默认0.2）
    linewidth = 0.2,   # 减小箱线图边框粗细（默认1）
    alpha = 0.5
  ),
  ggplot.component = list(
    scale_x_discrete(labels = c("MT14775-MUT", "NO-MUT")),
    scale_y_continuous(limits = c(0, 110)),  # Add这行Sety轴范围
    theme(axis.text= element_text(size = 15, face = "bold"),
          axis.title.y.right = element_blank(), 
          axis.text.y.right = element_blank(), 
          axis.ticks.y.right = element_blank(), 
          axis.title.y = element_text(face = "bold", size = 20))
  ),
  ggtheme = theme_classic(),#主题修改，可直接调用ggplot2的主题，默认主题为ggstatsplot::theme_ggstatsplot()
  package = "ggsci",#提取调色板所需的包
  palette = "uniform_startrek"#选择提取包中的调色板
)+
  ggsignif::geom_signif(
    comparisons = list(c("MT14775-MUT", "NO-MUT")),  # SpecifyCompare组
    test = "wilcox.test",  # 使用非参数test
    map_signif_level = TRUE,  # 显示星号（*）而非 p 值
    y_position = 100,         # Adjust标记的 y 轴Position
    tip_length = 0.01,       # Adjust横线两端的短竖线长度
    textsize = 5,            # Adjust文本大小
    vjust = 0.2              # Adjust文本垂直Position
  )

p24
p14
ggsave(p14,filename ="D:\\biosoft\\1000thal\\mtDNA\\figure/figure5G.Iron across MT14775 in β0β0.pdf",
       device = pdf,width = 12,height = 8,dpi = 600)
ggsave(p24,filename ="D:\\biosoft\\1000thal\\mtDNA\\figure/figure5G.Iron across MT14775 in β0β+.pdf",
       device = pdf,width = 12,height = 8,dpi = 600)


######HL14755###Transferrin######
p15 <- ggbetweenstats(
  data = input7,
  x = group,
  y = Transferrin,  #Annual_transfusions #HbF #Serum_Ferritin
  plot.type = "boxviolin",
  # bf.message = F,
  # var.equal = TRUE,
  # type = "parametric",
  type = "nonparametric",
  pairwise.display = "significant",#significant
  p.adjust.method = "none",
  title = NULL, #"Distribution of sepal length across Iris species",
  xlab = "",
  ylab = "Transferrin in β0/β0",
  results.subtitle = T,#决定是否将Statistical test的Results显示为副标题（默认TRUE）;如果设置为FALSE,则仅返回绘图
  subtitle = NULL,#副标题,默认显示统计test results,自定义则results.subtitle=FALSE
  outlier.tagging = TRUE,#whether or not标记离群异常值，默认FALSE
  outlier.shape = 19,#异常值形状,可Set为NA将其隐藏（不are删除，因此不会影响统计test results）
  outlier.color = "pink",#异常值颜色
  outlier.label.args = list(size = 4),#异常值标签大小
  centrality.type = "parameteric",
  centrality.label.args = list(size = 8, nudge_x = 0.4, segment.linetype = 4,
                               min.segment.length = 0),
  point.args = list(
    position = ggplot2::position_jitterdodge(dodge.width = 0.6),
    alpha = 0.5, size = 3, stroke = 0
  ),
  violin.args = list(
    width = 0.3,       # 减小小提琴width（默认0.4）
    alpha = 0.2,
    linewidth = 0.2    # 减小小提琴边框粗细（默认1）
  ),
  boxplot.args = list(
    width = 0.1,       # 减小箱线图width（默认0.2）
    linewidth = 0.2,   # 减小箱线图边框粗细（默认1）
    alpha = 0.5
  ),
  ggplot.component = list(
    scale_x_discrete(labels = c("MT14775-MUT", "NO-MUT")),
    # scale_y_continuous(limits = c(0, 3)),  # Add这行Sety轴范围
    theme(axis.text= element_text(size = 15, face = "bold"),
          axis.title.y.right = element_blank(), 
          axis.text.y.right = element_blank(), 
          axis.ticks.y.right = element_blank(), 
          axis.title.y = element_text(face = "bold", size = 20))
  ),
  ggtheme = theme_classic(),#主题修改，可直接调用ggplot2的主题，默认主题为ggstatsplot::theme_ggstatsplot()
  package = "ggsci",#提取调色板所需的包
  palette = "uniform_startrek"#选择提取包中的调色板
)+
  ggsignif::geom_signif(
    comparisons = list(c("MT14775-MUT", "NO-MUT")),  # SpecifyCompare组
    test = "wilcox.test",  # 使用非参数test
    map_signif_level = TRUE,  # 显示星号（*）而非 p 值
    y_position = 3,         # Adjust标记的 y 轴Position
    tip_length = 0.01,       # Adjust横线两端的短竖线长度
    textsize = 5,            # Adjust文本大小
    vjust = 0.2              # Adjust文本垂直Position
  )

p15


p25 <- ggbetweenstats(
  data = input8,
  x = group,
  y = Transferrin,  #Annual_transfusions #HbF #Serum_Ferritin
  plot.type = "boxviolin",
  # bf.message = F,
  # var.equal = TRUE,
  # type = "parametric",
  type = "nonparametric",
  pairwise.display = "significant",#significant
  p.adjust.method = "none",
  title = NULL, #"Distribution of sepal length across Iris species",
  xlab = "",
  ylab = "Transferrin in β0/β+",
  results.subtitle = T,#决定是否将Statistical test的Results显示为副标题（默认TRUE）;如果设置为FALSE,则仅返回绘图
  subtitle = NULL,#副标题,默认显示统计test results,自定义则results.subtitle=FALSE
  outlier.tagging = TRUE,#whether or not标记离群异常值，默认FALSE
  outlier.shape = 19,#异常值形状,可Set为NA将其隐藏（不are删除，因此不会影响统计test results）
  outlier.color = "pink",#异常值颜色
  outlier.label.args = list(size = 4),#异常值标签大小
  centrality.type = "parameteric",
  centrality.label.args = list(size = 8, nudge_x = 0.4, segment.linetype = 4,
                               min.segment.length = 0),
  point.args = list(
    position = ggplot2::position_jitterdodge(dodge.width = 0.6),
    alpha = 0.5, size = 3, stroke = 0
  ),
  violin.args = list(
    width = 0.3,       # 减小小提琴width（默认0.4）
    alpha = 0.2,
    linewidth = 0.2    # 减小小提琴边框粗细（默认1）
  ),
  boxplot.args = list(
    width = 0.1,       # 减小箱线图width（默认0.2）
    linewidth = 0.2,   # 减小箱线图边框粗细（默认1）
    alpha = 0.5
  ),
  ggplot.component = list(
    scale_x_discrete(labels = c("MT14775-MUT", "NO-MUT")),
    # scale_y_continuous(limits = c(0, 60)),  # Add这行Sety轴范围
    theme(axis.text= element_text(size = 15, face = "bold"),
          axis.title.y.right = element_blank(), 
          axis.text.y.right = element_blank(), 
          axis.ticks.y.right = element_blank(), 
          axis.title.y = element_text(face = "bold", size = 20))
  ),
  ggtheme = theme_classic(),#主题修改，可直接调用ggplot2的主题，默认主题为ggstatsplot::theme_ggstatsplot()
  package = "ggsci",#提取调色板所需的包
  palette = "uniform_startrek"#选择提取包中的调色板
)+
  ggsignif::geom_signif(
    comparisons = list(c("MT14775-MUT", "NO-MUT")),  # SpecifyCompare组
    test = "wilcox.test",  # 使用非参数test
    map_signif_level = TRUE,  # 显示星号（*）而非 p 值
    y_position = 3,         # Adjust标记的 y 轴Position
    tip_length = 0.01,       # Adjust横线两端的短竖线长度
    textsize = 5,            # Adjust文本大小
    vjust = 0.2              # Adjust文本垂直Position
  )

p25
p15

ggsave(p15,filename ="D:\\biosoft\\1000thal\\mtDNA\\figure/figure5G.Transferrin across MT14775 in β0β0.pdf",
       device = pdf,width = 12,height = 8,dpi = 600)
ggsave(p25,filename ="D:\\biosoft\\1000thal\\mtDNA\\figure/figure5G.Transferrin across MT14775 in β0β+.pdf",
       device = pdf,width = 12,height = 8,dpi = 600)


######HL14755###Annual_transfusions######
p16 <- ggbetweenstats(
  data = input7,
  x = group,
  y = Annual_transfusions,  #Annual_transfusions #HbF #Serum_Ferritin
  plot.type = "boxviolin",
  # bf.message = F,
  # var.equal = TRUE,
  # type = "parametric",
  # type = "nonparametric",
  pairwise.display = "significant",#significant
  p.adjust.method = "none",
  title = NULL, #"Distribution of sepal length across Iris species",
  xlab = "",
  ylab = "Annual transfusions in β0/β0",
  results.subtitle = T,#决定是否将Statistical test的Results显示为副标题（默认TRUE）;如果设置为FALSE,则仅返回绘图
  subtitle = NULL,#副标题,默认显示统计test results,自定义则results.subtitle=FALSE
  outlier.tagging = TRUE,#whether or not标记离群异常值，默认FALSE
  outlier.shape = 19,#异常值形状,可Set为NA将其隐藏（不are删除，因此不会影响统计test results）
  outlier.color = "pink",#异常值颜色
  outlier.label.args = list(size = 4),#异常值标签大小
  centrality.type = "parameteric",
  centrality.label.args = list(size = 8, nudge_x = 0.4, segment.linetype = 4,
                               min.segment.length = 0),
  point.args = list(
    position = ggplot2::position_jitterdodge(dodge.width = 0.6),
    alpha = 0.5, size = 3, stroke = 0
  ),
  violin.args = list(
    width = 0.3,       # 减小小提琴width（默认0.4）
    alpha = 0.2,
    linewidth = 0.2    # 减小小提琴边框粗细（默认1）
  ),
  boxplot.args = list(
    width = 0.1,       # 减小箱线图width（默认0.2）
    linewidth = 0.2,   # 减小箱线图边框粗细（默认1）
    alpha = 0.5
  ),
  ggplot.component = list(
    scale_x_discrete(labels = c("MT14775-MUT", "NO-MUT")),
    # scale_y_continuous(limits = c(0, 60)),  # Add这行Sety轴范围
    theme(axis.text= element_text(size = 15, face = "bold"),
          axis.title.y.right = element_blank(), 
          axis.text.y.right = element_blank(), 
          axis.ticks.y.right = element_blank(), 
          axis.title.y = element_text(face = "bold", size = 20))
  ),
  ggtheme = theme_classic(),#主题修改，可直接调用ggplot2的主题，默认主题为ggstatsplot::theme_ggstatsplot()
  package = "ggsci",#提取调色板所需的包
  palette = "uniform_startrek"#选择提取包中的调色板
)+
  ggsignif::geom_signif(
    comparisons = list(c("MT14775-MUT", "NO-MUT")),  # SpecifyCompare组
    test = "wilcox.test",  # 使用非参数test
    map_signif_level = TRUE,  # 显示星号（*）而非 p 值
    y_position = 50,         # Adjust标记的 y 轴Position
    tip_length = 0.01,       # Adjust横线两端的短竖线长度
    textsize = 5,            # Adjust文本大小
    vjust = 0.2              # Adjust文本垂直Position
  )

p16


p26 <- ggbetweenstats(
  data = input8,
  x = group,
  y = Annual_transfusions,  #Annual_transfusions #HbF #Serum_Ferritin
  plot.type = "boxviolin",
  # bf.message = F,
  # var.equal = TRUE,
  # type = "parametric",
  # type = "nonparametric",
  pairwise.display = "significant",#significant
  p.adjust.method = "none",
  title = NULL, #"Distribution of sepal length across Iris species",
  xlab = "",
  ylab = "Annual transfusions in β0/β+",
  results.subtitle = T,#决定是否将Statistical test的Results显示为副标题（默认TRUE）;如果设置为FALSE,则仅返回绘图
  subtitle = NULL,#副标题,默认显示统计test results,自定义则results.subtitle=FALSE
  outlier.tagging = TRUE,#whether or not标记离群异常值，默认FALSE
  outlier.shape = 19,#异常值形状,可Set为NA将其隐藏（不are删除，因此不会影响统计test results）
  outlier.color = "pink",#异常值颜色
  outlier.label.args = list(size = 4),#异常值标签大小
  centrality.type = "parameteric",
  centrality.label.args = list(size = 8, nudge_x = 0.4, segment.linetype = 4,
                               min.segment.length = 0),
  point.args = list(
    position = ggplot2::position_jitterdodge(dodge.width = 0.6),
    alpha = 0.5, size = 3, stroke = 0
  ),
  violin.args = list(
    width = 0.3,       # 减小小提琴width（默认0.4）
    alpha = 0.2,
    linewidth = 0.2    # 减小小提琴边框粗细（默认1）
  ),
  boxplot.args = list(
    width = 0.1,       # 减小箱线图width（默认0.2）
    linewidth = 0.2,   # 减小箱线图边框粗细（默认1）
    alpha = 0.5
  ),
  ggplot.component = list(
    scale_x_discrete(labels = c("MT14775-MUT", "NO-MUT")),
    # scale_y_continuous(limits = c(0, 60)),  # Add这行Sety轴范围
    theme(axis.text= element_text(size = 15, face = "bold"),
          axis.title.y.right = element_blank(), 
          axis.text.y.right = element_blank(), 
          axis.ticks.y.right = element_blank(), 
          axis.title.y = element_text(face = "bold", size = 20))
  ),
  ggtheme = theme_classic(),#主题修改，可直接调用ggplot2的主题，默认主题为ggstatsplot::theme_ggstatsplot()
  package = "ggsci",#提取调色板所需的包
  palette = "uniform_startrek"#选择提取包中的调色板
)+
  ggsignif::geom_signif(
    comparisons = list(c("MT14775-MUT", "NO-MUT")),  # SpecifyCompare组
    test = "wilcox.test",  # 使用非参数test
    map_signif_level = TRUE,  # 显示星号（*）而非 p 值
    y_position = 50,         # Adjust标记的 y 轴Position
    tip_length = 0.01,       # Adjust横线两端的短竖线长度
    textsize = 5,            # Adjust文本大小
    vjust = 0.2              # Adjust文本垂直Position
  )

p26


p16

ggsave(p16,filename ="D:\\biosoft\\1000thal\\mtDNA\\figure/figure5G.Annual transfusions across MT14775 in β0β0.pdf",
       device = pdf,width = 12,height = 8,dpi = 600)
ggsave(p26,filename ="D:\\biosoft\\1000thal\\mtDNA\\figure/figure5G.Annual transfusions across MT14775 in β0β+.pdf",
       device = pdf,width = 12,height = 8,dpi = 600)


###HL14775###Sur_time####
df1 <- input7 %>% 
  rename(Sur_time = Survival_time_without_transfusion) %>% 
  mutate(status = ifelse(is.na(Sur_time),0,1)) %>%
  mutate(Sur_time = coalesce(Sur_time, Age)) %>% 
  filter(`Splenomegaly`  == "Splenectomy")


coxph(Surv(Sur_time, status) ~ group, data=df1)
fit1 <- survfit(Surv(Sur_time, status) ~ group, data=df1)

p17 <- ggsurvplot(fit1, data=df1, pval=TRUE, conf.int=TRUE)

df2 <- input8 %>% 
  rename(Sur_time = Survival_time_without_transfusion) %>% 
  mutate(status = ifelse(is.na(Sur_time),0,1)) %>%
  mutate(Sur_time = coalesce(Sur_time, Age)) %>% 
  filter(`Transfusion_Dependence`  != "NTDT")

coxph(Surv(Sur_time, status) ~ group, data=df2)
fit2 <- survfit(Surv(Sur_time, status) ~ group, data=df2)

p27 <-ggsurvplot(fit2, data=df2, pval=TRUE, conf.int=TRUE)

p17
p27
# ggsave(p17$plot,filename ="D:\\biosoft\\1000thal\\mtDNA\\figure/figure5G.survplot across MT14775 in β0β0.pdf",
#        device = pdf,width = 12,height = 8,dpi = 600)
# ggsave(p27$plot,filename ="D:\\biosoft\\1000thal\\mtDNA\\figure/figure5G.survplot across MT14775 in β0β+.pdf",
#        device = pdf,width = 12,height = 8,dpi = 600)










### 列出所有可能的Categorical variable ###
categorical_vars <- c("Sex", "Clinical_staging", "Thalassaemia_face", "Jaundice", 
                      "Gallstones", "Splenic","Splenomegaly")  # 根据你的实际变量名Modify

### 或者自动识别Categorical variable ###
categorical_vars <- names(df1)[sapply(df1, function(x) is.factor(x) | is.character(x))]
categorical_vars <- setdiff(categorical_vars, c("HID","ID","BID","Iron overload","HBB_genotype_category","six_genotype_category","group", "Sur_time", "status"))  # 排除已使用的变量

### 对每个Categorical variable进行生存分析 ###
results <- list()

for (var in categorical_vars) {
  if (length(unique(df1[[var]])) > 1) {  # 确保有多个类别
    formula <- as.formula(paste("Surv(Sur_time, status) ~", var))
    cox_fit <- coxph(formula, data = df1)
    p_value <- summary(cox_fit)$coefficients[1, 5]
    
    results[[var]] <- list(
      p_value = p_value,
      cox_summary = summary(cox_fit),
      groups = table(df1[[var]])
    )
  }
}

# Inspect results，寻找p-values最小的变量
p_values <- sapply(results, function(x) x$p_value)
significant_vars <- names(p_values[p_values < 0.05])


















