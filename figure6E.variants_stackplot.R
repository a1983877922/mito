rm(list=ls())#clear Global Environment
# Load packages
# Load必要的包
library(vcfR)
library(tidyverse)
library(ggridges)
library(ggplot2)
library(patchwork)
library(RColorBrewer)  # Add这个包

# 1. 读取数据
case_vcf_file <- "D:\\biosoft\\1000thal\\mtDNA\\gwas\\human_mito_genes\\case/case1035.vep.damaging.ano.vcf.gz"
case_vcf <- read.vcfR(case_vcf_file, verbose = FALSE)
control_vcf_file <- "D:\\biosoft\\1000thal\\mtDNA\\gwas\\human_mito_genes\\control/control1078.merged.vep.damaging.ano.hg38_multianno.newchr.vcf.gz"
control_vcf <- read.vcfR(control_vcf_file, verbose = FALSE)
### 读取Gene信息 ###
genes <- read.table("D:\\biosoft\\1000thal\\mtDNA\\gwas\\human_mito_genes\\mito_genes_hg38.tsv", header = TRUE) %>% 
  arrange(hg38_Chromosome) %>%
  mutate(Gene_Length = hg38_End - hg38_Start + 1,
         hg38_Chromosome = paste0("chr", hg38_Chromosome))

### 关注的Gene列表 c("BCL2A1", "MMAB", "MTARC2","MRPL18", "FAM185A", "BID") ###
# significant_genes <- c("ARMCX1","FTMT","ATP5O","LIPT2","COQ8A","DNM1L","HIBCH","COX7A2L",
#                        "MICU1","C150f48","COM7D1","FHHD1", "BCL2A1", "MMAB", "MTARC2","MRPL18", "FAM185A", "BID")

# 
# significant_genes <- c("DHRS4", "BCL2A1", "MMAB", "C15orf48", "NUDT13", "MTARC2", 
#                        "MRPL18", "FAM185A", "BID", "CCDC127", "ATP5PO", "BCL2L11", "BPHL", 
#                        "CA5A", "ALDH1L2", "NDUFA10", "PRELID2", "C15orf61", "TDRKH","LIPT2")




significant_genes <- c("AK2", "ATAD3A", "ATAD3B", "BOLA1", "CASP9", "KMO", "MTARC2",
  "TDRKH", "COMTD1", "NUDT13", "BCO2", "CAT", "SDHAF2", "TIMM10B",
  "TMEM126B", "ALDH1L2", "ATP23", "CRY1", "DIABLO", "GATC", "MMAB",
  "MRPL42", "USP30", "CARS2", "MIPEP", "BCL2L2", "DHRS4", "BCL2A1",
  "C15orf48", "C15orf61", "CYP11A1", "MTFMT", "ACSM2A", "CA5A",
  "CMC2", "DNAJA3", "HSDL1", "MLYCD", "NME3", "NME4", "PAM16",
  "PDPR", "ACACA", "C1QBP", "CISD3", "LYRM9", "MRM1", "MRPL45",
  "MYO19", "SLC25A35", "SPATA20", "BCL2", "FAM210A", "RBFA", "CLPP",
  "CPT1C", "NDUFA3", "QTRT1", "RDH13", "TIMM44", "UQCRFS1", "BCL2L11",
  "BCS1L", "BOK", "BOLA3", "C2orf69", "CASP8", "CMPK2", "DBI",
  "FAHD2A", "MCEE", "MRPL44", "NDUFA10", "NEU4", "COX4I2", "MRPS26",
  "ATP5PF", "ATP5PO", "CBR3", "DNAJC28", "BCL2L13", "BID", "BIK",
  "COMT", "GCAT", "NDUFA6", "ACAA1", "C3orf33", "SUCLG2", "CCDC58",
  "CBR4", "MRPL1", "CCDC127", "CKMT2", "MCCC2", "MRPS27", "PRELID2",
  "MRPS36", "BPHL", "HSD17B8", "MCCD1", "MRPL18", "MRPS18B", "TOMM6",
  "VARS2", "ARF5", "CHCHD2", "CHCHD3", "FAM185A", "NDUFA5", "SLC25A13",
  "BNIP3L", "C8orf82", "CYP11B1", "DECR1", "LYPLA1", "PDP1", "TMEM65",
  "GLDC", "MRRF", "STOM", "ABCB7", "ACOT9", "AIFM1", "ALAS2", "APOOL",
  "ARMCX2", "CA5B", "PDHA1", "TMLHE", "TRMT2B","HIBCH","COQ8A","LIPT2")

# 2. 提取VCF信息并转换为数据框
### 需要提取每个个体的Genotype信息 ###
vcf_df1 <- data.frame(
  CHROM = getCHROM(case_vcf),
  POS = getPOS(case_vcf),
  REF = getREF(case_vcf),
  ALT = getALT(case_vcf),
  stringsAsFactors = FALSE
)

vcf_df2 <- data.frame(
  CHROM = getCHROM(control_vcf),
  POS = getPOS(control_vcf),
  REF = getREF(control_vcf),
  ALT = getALT(control_vcf),
  stringsAsFactors = FALSE
)

### 提取个体ID（Sample名） ###
sample_ids1020 <- colnames(case_vcf@gt)[-1]  # 去掉第一个"FORMAT"列
sample_ids2450 <- colnames(control_vcf@gt)[-1]  # 去掉第一个"FORMAT"列
### 提取Genotype矩阵 ###
gt_matrix1 <- extract.gt(case_vcf, element = "GT")
gt_matrix2 <- extract.gt(control_vcf, element = "GT")
# 3. 为VCF位点AnnotationGene信息
annotate_vcf_with_genes <- function(vcf_df, genes_df) {
  result <- data.frame()
  
  for(i in 1:nrow(vcf_df)) {
    chrom <- vcf_df$CHROM[i]
    pos <- vcf_df$POS[i]
    
    ### 查找contains该Position的Gene ###
    overlapping_genes <- genes_df %>%
      filter(hg38_Chromosome == chrom, 
             hg38_Start <= pos, 
             hg38_End >= pos) %>%
      pull(Symbol)
    
    ### 如果有多个Gene重叠，用逗号分隔 ###
    gene_symbol <- ifelse(length(overlapping_genes) > 0, 
                          paste(unique(overlapping_genes), collapse = ","), 
                          "Intergenic")
    
    # CreateResults行
    result_row <- vcf_df[i, ]
    result_row$Gene_Symbol <- gene_symbol
    result <- bind_rows(result, result_row)
  }
  
  return(result)
}

### 执行Annotation ###
annotated_vcf1 <- annotate_vcf_with_genes(vcf_df1, genes)
annotated_vcf2 <- annotate_vcf_with_genes(vcf_df2, genes)
# 4. 处理Genotype数据
### 将Genotype转换为0/1/2表示（0=REF/REF, 1=REF/ALT, 2=ALT/ALT或复杂情况） ###
gt_numeric1 <- matrix(0, nrow = nrow(gt_matrix1), ncol = ncol(gt_matrix1))
colnames(gt_numeric1) <- sample_ids1020

for(i in 1:nrow(gt_matrix1)) {
  for(j in 1:ncol(gt_matrix1)) {
    gt <- gt_matrix1[i, j]
    if(is.na(gt) || gt == "." || gt == "./." || gt == ".|.") {
      gt_numeric1[i, j] <- 0  # 缺失数据当作0
    } else {
      ### Simple处理：只要有ALT等位Gene就计数为1 ###
      ### 更精确的处理可以Distinguish0/1, 1/1等情况 ###
      alleles <- unlist(strsplit(gt, "[/|]"))
      alt_count <- sum(alleles != "0" & alleles != ".")
      gt_numeric1[i, j] <- min(alt_count, 1)  # 二值化：0或1
    }
  }
}

gt_numeric2 <- matrix(0, nrow = nrow(gt_matrix2), ncol = ncol(gt_matrix2))
colnames(gt_numeric2) <- sample_ids2450

for(i in 1:nrow(gt_matrix2)) {
  for(j in 1:ncol(gt_matrix2)) {
    gt <- gt_matrix2[i, j]
    if(is.na(gt) || gt == "." || gt == "./." || gt == ".|.") {
      gt_numeric2[i, j] <- 0  # 缺失数据当作0
    } else {
      ### Simple处理：只要有ALT等位Gene就计数为1 ###
      ### 更精确的处理可以Distinguish0/1, 1/1等情况 ###
      alleles <- unlist(strsplit(gt, "[/|]"))
      alt_count <- sum(alleles != "0" & alleles != ".")
      gt_numeric2[i, j] <- min(alt_count, 1)  # 二值化：0或1
    }
  }
}

# 5. Create每个个体每个Gene的Mutation计数数据
### FirstFiltersignificant_genes的位点 ###
significant_positions1 <- annotated_vcf1 %>%
  mutate(Gene_List = strsplit(Gene_Symbol, ",")) %>%
  unnest(Gene_List) %>%
  filter(Gene_List %in% significant_genes) %>%
  distinct(CHROM, POS, REF, ALT, Gene_List) %>%
  rename(Gene_Symbol = Gene_List)

significant_positions2 <- annotated_vcf2 %>%
  mutate(Gene_List = strsplit(Gene_Symbol, ",")) %>%
  unnest(Gene_List) %>%
  filter(Gene_List %in% significant_genes) %>%
  distinct(CHROM, POS, REF, ALT, Gene_List) %>%
  rename(Gene_Symbol = Gene_List)
### 获取这些位点在Genotype矩阵中的行索引 ###
position_indices1 <- which(annotated_vcf1$POS %in% significant_positions1$POS)
position_indices2 <- which(annotated_vcf2$POS %in% significant_positions2$POS)
# Create一个数据框来存储每个个体每个Gene的Mutation数量
mutation_counts1 <- data.frame()

### 为每个Geneand每个个体计算Mutation数量 ###
for(gene in significant_genes) {
  ### 获取该Gene的所有位点 ###
  gene_positions <- significant_positions1 %>%
    filter(Gene_Symbol == gene) %>%
    pull(POS)
  
  if(length(gene_positions) > 0) {
    ### 获取这些位点的索引 ###
    gene_indices <- which(annotated_vcf1$POS %in% gene_positions)
    
    if(length(gene_indices) > 0) {
      ### 提取该Gene的Genotype数据 ###
      gene_gt <- gt_numeric1[gene_indices, , drop = FALSE]
      
      ### 计算每个个体的Mutation总数 ###
      for(sample in sample_ids1020) {
        sample_counts <- data.frame(
          Sample = sample,
          Gene_Symbol = gene,
          Mutation_Count = sum(gene_gt[, sample], na.rm = TRUE)
        )
        mutation_counts1 <- bind_rows(mutation_counts1, sample_counts)
      }
    }
  }
}


mutation_counts2 <- data.frame()

### 为每个Geneand每个个体计算Mutation数量 ###
for(gene in significant_genes) {
  ### 获取该Gene的所有位点 ###
  gene_positions <- significant_positions2 %>%
    filter(Gene_Symbol == gene) %>%
    pull(POS)
  
  if(length(gene_positions) > 0) {
    ### 获取这些位点的索引 ###
    gene_indices <- which(annotated_vcf2$POS %in% gene_positions)
    
    if(length(gene_indices) > 0) {
      ### 提取该Gene的Genotype数据 ###
      gene_gt <- gt_numeric2[gene_indices, , drop = FALSE]
      
      ### 计算每个个体的Mutation总数 ###
      for(sample in sample_ids2450) {
        sample_counts <- data.frame(
          Sample = sample,
          Gene_Symbol = gene,
          Mutation_Count = sum(gene_gt[, sample], na.rm = TRUE)
        )
        mutation_counts2 <- bind_rows(mutation_counts2, sample_counts)
      }
    }
  }
}


# 1.1 计算每个Gene的VariantFrequency
freq_data1 <- mutation_counts1 %>%
  mutate(Has_Mutation = ifelse(Mutation_Count > 0, 1, 0)) %>%
  group_by(Gene_Symbol) %>%
  summarise(
    Mutation_Frequency = mean(Has_Mutation),
    Mean_Count = mean(Mutation_Count),
    Samples_With_Mutation = sum(Has_Mutation)
  ) %>%
  arrange(desc(Mutation_Frequency))

freq_data2 <- mutation_counts2 %>%
  mutate(Has_Mutation = ifelse(Mutation_Count > 0, 1, 0)) %>%
  group_by(Gene_Symbol) %>%
  summarise(
    Mutation_Frequency = mean(Has_Mutation),
    Mean_Count = mean(Mutation_Count),
    Samples_With_Mutation = sum(Has_Mutation)
  ) %>%
  arrange(desc(Mutation_Frequency))


### 计算每个Gene的Variant计数分布 ###
dist_data1 <- mutation_counts1 %>%
  mutate(Mutation_Category = factor(case_when(
    Mutation_Count == 0 ~ "0个变异",
    Mutation_Count == 1 ~ "1个变异",
    Mutation_Count == 2 ~ "2个变异",
    Mutation_Count == 3 ~ "3个变异",
    Mutation_Count >= 4 ~ "≥4个变异"
  ), levels = c("0个变异", "1个变异", "2个变异", "3个变异", "≥4个变异"))) %>%
  count(Gene_Symbol, Mutation_Category) %>%
  group_by(Gene_Symbol) %>%
  mutate(Percentage = n / sum(n) * 100,
         Label = ifelse(Percentage > 5, paste0(round(Percentage, 1), "%"), ""))

dist_data2 <- mutation_counts2 %>%
  mutate(Mutation_Category = factor(case_when(
    Mutation_Count == 0 ~ "0个变异",
    Mutation_Count == 1 ~ "1个变异",
    Mutation_Count == 2 ~ "2个变异",
    Mutation_Count == 3 ~ "3个变异",
    Mutation_Count >= 4 ~ "≥4个变异"
  ), levels = c("0个变异", "1个变异", "2个变异", "3个变异", "≥4个变异"))) %>%
  count(Gene_Symbol, Mutation_Category) %>%
  group_by(Gene_Symbol) %>%
  mutate(Percentage = n / sum(n) * 100,
         Label = ifelse(Percentage > 5, paste0(round(Percentage, 1), "%"), ""))
### 按总VariantFrequency排序 ###
# input_gene <- c("HIBCH", "LIPT2", "MRPL18", "BCL2A1", "FAM185A", "MMAB", "COQ8A", 
#                 "BID", "MTARC2", "COX7A2L")

# input_gene<- c("BCL2L2","BCO2","CBR4","CMC2","MIPEP","SPATA20","UQCRFS1","C3orf33","CBR3",
#                       "ACSM2A","CASP8","GCAT","CASP9","RBFA","ATAD3B","MRPL18","BCL2A1","SUGL2",
#                       "CYP11B1","FAM185A","MMAB","PDPR","MCEE","COMT","C29orf69","HSDL1",
#                       "CARS2","MLYCD","ALDH2L2","BCL2","CMPK2","MRPS27","BID","MTARC2",
#                       "MIFMT","ATP5PO","CPT1C","HIBCH","COQ8A","LIPT2")

input_gene<-c("MTARC2","FAM185A","C3orf33","BCL2L2","CMPK2","BCO2","MIPEP","UQCRFS1","CBR4","CPT1C",
"CARS2","ATP5PO","GCAT","CASP9","MLYCD","RBFA","LIPT2","MRPS27","MCEE","PDPR","COMT",
"CASP8","COQ8A","BCL2","SPATA20","HSDL1","BCL2A1","MRPL18","MMAB","ACSM2A","CYP11B1","CBR3",
"CMC2","ATAD3B","BID","HIBCH")
gene_order1 <- freq_data1$Gene_Symbol
dist_data1$Gene_Symbol <- factor(dist_data1$Gene_Symbol, levels = gene_order1)
dist_data1 <- dist_data1 %>% filter(Gene_Symbol %in% input_gene)



p_stacked1 <- ggplot(dist_data1, aes(x = Gene_Symbol, y = Percentage, fill = Mutation_Category)) +
  geom_bar(stat = "identity", position = "stack") +
  geom_text(aes(label = Label), 
            position = position_stack(vjust = 0.5), 
            size = 2.5, color = "white") +
  scale_fill_brewer(palette = "RdYlBu", direction = -1, name = "Variant数量") +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  labs(title = "各GeneVariant数量分布Ratio",
       subtitle = "展示每个Variant计数的SampleRatio",
       x = "Gene", 
       y = "SampleRatio") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom")

print(p_stacked1)

# Modify函数
### Method 1：先确保dist_data2的Gene_Symbolare字符型而不are因子 ###
dist_data2$Gene_Symbol <- as.character(dist_data2$Gene_Symbol)
gene_order2<- freq_data2$Gene_Symbol
dist_data2$Gene_Symbol <- factor(dist_data2$Gene_Symbol, levels = gene_order2)
### 重新编写函数 ###
filter_and_complete_dist_data <- function(dist_data2, input_genes) {
  
  ### 确保dist_data2的Gene_Symbolare字符型 ###
  if(is.factor(dist_data2$Gene_Symbol)) {
    dist_data2$Gene_Symbol <- as.character(dist_data2$Gene_Symbol)
  }
  
  ### 获取dist_data2中现有的Gene ###
  existing_genes <- unique(dist_data2$Gene_Symbol)
  cat("现有基因:", paste(existing_genes, collapse = ", "), "\n")
  
  ### 找出缺失的Gene ###
  missing_genes <- setdiff(input_genes, existing_genes)
  cat("需要补充的基因:", paste(missing_genes, collapse = ", "), "\n")
  
  # Create缺失Gene的补充数据
  if(length(missing_genes) > 0) {
    missing_data <- data.frame(
      Gene_Symbol = missing_genes,
      Mutation_Category = "0个Variant",
      n = 2450,
      Percentage = 100.0,
      Label = "100%",
      stringsAsFactors = FALSE  # 重要：不要自动转为因子
    )
    
    # Merge data
    result_data <- bind_rows(
      dist_data2 %>% filter(Gene_Symbol %in% input_genes),
      missing_data
    )
  } else {
    result_data <- dist_data2 %>% filter(Gene_Symbol %in% input_genes)
  }
  
  ### 现在转换为因子，按input_genes顺序 ###
  result_data$Gene_Symbol <- factor(result_data$Gene_Symbol, levels = input_genes)
  result_data$Mutation_Category <- factor(
    result_data$Mutation_Category,
    levels = c("0个变异", "1个变异", "2个变异", "3个变异", "≥4个变异")
  )
  
  ### 按GeneandVariant类别排序 ###
  result_data <- result_data %>%
    arrange(Gene_Symbol, Mutation_Category)
  
  return(result_data)
}

### 测试一下 ###

result <- filter_and_complete_dist_data(dist_data2, input_gene)

common_genes <- intersect(gene_order1, unique(result$Gene_Symbol))
result <- result[result$Gene_Symbol %in% common_genes, ]
result$Gene_Symbol <- factor(result$Gene_Symbol, levels = common_genes)


p_stacked2 <- ggplot(result, aes(x = Gene_Symbol, y = Percentage, fill = Mutation_Category)) +
  geom_bar(stat = "identity", position = "stack") +
  geom_text(aes(label = Label), 
            position = position_stack(vjust = 0.5), 
            size = 2.5, color = "white") +
  scale_fill_brewer(palette = "RdYlBu", direction = -1, name = "Variant数量") +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  labs(title = "各GeneVariant数量分布Ratio",
       subtitle = "展示每个Variant计数的SampleRatio",
       x = "Gene", 
       y = "SampleRatio") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom")

print(p_stacked2)
ggsave("D:\\biosoft\\1000thal\\mtDNA\\figure/figure6E1.stacked1_cairo.pdf", 
       plot = p_stacked1,
       width = 12,
       height = 8,
       device = cairo_pdf)  # 使用cairo PDF设备
ggsave("D:\\biosoft\\1000thal\\mtDNA\\figure/figure6E2.stacked2_cairo.pdf", 
       plot = p_stacked2,
       width = 12,
       height = 8,
       device = cairo_pdf)  # 使用cairo PDF设备
