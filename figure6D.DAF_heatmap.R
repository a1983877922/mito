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
library(vcfR)
#####
### 读取数据 ###
case_vcf_file1 <- "D:\\biosoft\\1000thal\\mtDNA\\gwas\\human_mito_genes\\case/case1035.vep.damaging.ano.AA.b+.vcf.gz"
case_vcf_file2 <- "D:\\biosoft\\1000thal\\mtDNA\\gwas\\human_mito_genes\\case/case1035.vep.damaging.ano.AA.b0+.vcf.gz"
case_vcf_file3 <- "D:\\biosoft\\1000thal\\mtDNA\\gwas\\human_mito_genes\\case/case1035.vep.damaging.ano.AA.b00.vcf.gz"
case_vcf_file4 <- "D:\\biosoft\\1000thal\\mtDNA\\gwas\\human_mito_genes\\case/case1035.vep.damaging.ano.AA.vcf.gz"

case_vcf11 <- read.vcfR(case_vcf_file1, verbose = FALSE)
case_vcf01 <- read.vcfR(case_vcf_file2, verbose = FALSE)
case_vcf00 <- read.vcfR(case_vcf_file3, verbose = FALSE)
case_vcf <- read.vcfR(case_vcf_file4, verbose = FALSE)

### 计算DAF的函数 ###
calculate_daf <- function(vcf) {
  ### 提取Genotype矩阵 (GT字段) ###
  gt <- extract.gt(vcf, element = "GT")
  
  ### 提取祖先等位Gene信息 (INFO字段中的AA) ###
  aa_info <- extract.info(vcf, element = "AA")
  
  ### 获取参考等位Geneand替代等位Gene ###
  ref_allele <- getREF(vcf)
  alt_allele <- getALT(vcf)
  
  ### 初始化存储Results的向量 ###
  daf <- numeric(nrow(gt))
  ancestral_allele <- character(nrow(gt))
  derived_allele <- character(nrow(gt))
  
  ### Iterate over每个Variant位点 ###
  for (i in 1:nrow(gt)) {
    ### 获取当前位点的Genotype ###
    gt_i <- gt[i, ]
    
    ### 获取祖先等位Gene ###
    aa_i <- aa_info[i]
    
    ### 处理可能的缺失值 ###
    if (is.na(aa_i) || aa_i == ".") {
      daf[i] <- NA
      ancestral_allele[i] <- NA
      derived_allele[i] <- NA
      next
    }
    
    ### 获取参考and替代等位Gene ###
    ref_i <- ref_allele[i]
    alt_i <- alt_allele[i]
    
    ### 将祖先等位Gene转换为大写，以与参考等位Gene匹配 ###
    aa_i <- toupper(aa_i)
    
    ### 确定衍生等位Gene ###
    if (aa_i == ref_i) {
      ancestral <- ref_i
      derived <- alt_i
    } else if (aa_i == alt_i) {
      ancestral <- alt_i
      derived <- ref_i
    } else {
      ### 如果祖先等位Gene与参考/替代都不匹配 ###
      daf[i] <- NA
      ancestral_allele[i] <- aa_i
      derived_allele[i] <- NA
      next
    }
    
    ### 计算等位Gene计数 ###
    alleles <- c()
    for (g in gt_i) {
      if (!is.na(g) && g != "./." && g != ".|.") {
        ### 将Genotype拆分为单个等位Gene ###
        alleles_split <- unlist(strsplit(g, split = "[/|]"))
        alleles <- c(alleles, alleles_split)
      }
    }
    
    ### Remove缺失值 ###
    alleles <- alleles[alleles != "."]
    
    ### 计算衍生等位GeneFrequency ###
    if (length(alleles) > 0) {
      derived_count <- sum(alleles == "1" & derived == alt_i) + 
        sum(alleles == "0" & derived == ref_i)
      daf[i] <- derived_count / length(alleles)
    } else {
      daf[i] <- NA
    }
    
    ancestral_allele[i] <- ancestral
    derived_allele[i] <- derived
  }
  
  # CreateResults数据框
  result <- data.frame(
    CHROM = getCHROM(vcf),
    POS = getPOS(vcf),
    ID = getID(vcf),
    REF = ref_allele,
    ALT = alt_allele,
    AA = aa_info,
    Ancestral_Allele = ancestral_allele,
    Derived_Allele = derived_allele,
    DAF = daf,
    N_Samples = rowSums(!is.na(gt) & gt != "./." & gt != ".|."),
    stringsAsFactors = FALSE
  )
  
  return(result)
}


### 可选：SaveResults到文件 ###
# write.csv(case_daf_plp, "case_PLP_DAF.csv", row.names = FALSE)
# write.csv(control_daf_plp, "control_PLP_DAF.csv", row.names = FALSE)
### 计算caseandcontrol组的DAF ###
case_daf11 <- calculate_daf(case_vcf11)
case_daf01 <- calculate_daf(case_vcf01)
case_daf00 <- calculate_daf(case_vcf00)
case_daf <- calculate_daf(case_vcf)



genes <- read.table("D:\\biosoft\\1000thal\\mtDNA\\gwas\\human_mito_genes\\mito_genes_hg38.tsv", header = TRUE) %>% 
  arrange(hg38_Chromosome) %>%
  mutate(Gene_Length = hg38_End - hg38_Start + 1,
         hg38_Chromosome= paste0("chr",hg38_Chromosome))    # 计算Gene长度


case_daf11_filt<-case_daf11 %>% 
  select(c("CHROM" ,"POS","REF" ,"ALT","AA","Ancestral_Allele","Derived_Allele","DAF")) %>% 
  filter(!is.na(Ancestral_Allele)) %>% 
  filter(!is.na(Derived_Allele)) %>% 
  filter(DAF !=0 ) %>% 
  filter(DAF !=1 ) %>% 
  mutate(Group = "β+/β+")

case_daf01_filt<-case_daf01 %>% 
  select(c("CHROM" ,"POS","REF" ,"ALT","AA","Ancestral_Allele","Derived_Allele","DAF")) %>% 
  filter(!is.na(Ancestral_Allele)) %>% 
    filter(!is.na(Derived_Allele)) %>% 
    filter(DAF !=0 ) %>% 
    filter(DAF !=1 ) %>% 
    mutate(Group = "β0/β+")
case_daf00_filt<-case_daf00 %>% 
  select(c("CHROM" ,"POS","REF" ,"ALT","AA","Ancestral_Allele","Derived_Allele","DAF")) %>% 
  filter(!is.na(Ancestral_Allele)) %>% 
    filter(!is.na(Derived_Allele)) %>% 
    filter(DAF !=0 ) %>% 
    filter(DAF !=1 ) %>% 
    mutate(Group = "β0/β0")

case_daf_filt<-case_daf %>% 
  select(c("CHROM" ,"POS","REF" ,"ALT","AA","Ancestral_Allele","Derived_Allele","DAF")) %>% 
  filter(!is.na(Ancestral_Allele)) %>% 
  filter(!is.na(Derived_Allele)) %>% 
  filter(DAF !=0 ) %>% 
  filter(DAF !=1 ) %>% 
  mutate(Group = "ThaLassemia")
  
# control_daf <- read.csv("D:\\biosoft\\1000thal\\mtDNA\\gwas\\human_mito_genes\\control/control_DAF_results.csv") %>% 
#   select(c("CHROM" ,"POS","REF" ,"ALT","AA","Ancestral_Allele","Derived_Allele","DAF")) %>% 
#   filter(!is.na(Ancestral_Allele)) %>% 
#   filter(!is.na(Derived_Allele)) %>% 
#   filter(DAF !=0 ) %>% 
#   filter(DAF !=1 ) %>% 
#   mutate(Group = "βN/βN")

control_daf <- read.csv("D:\\biosoft\\1000thal\\mtDNA\\gwas\\human_mito_genes\\control/control_DAF_results.csv") %>% 
  select(c("CHROM" ,"POS","REF" ,"ALT","AA","Ancestral_Allele","Derived_Allele","DAF")) %>% 
  filter(!is.na(Ancestral_Allele)) %>% 
  filter(!is.na(Derived_Allele)) %>% 
  filter(DAF !=0 ) %>% 
  filter(DAF !=1 ) %>% 
  mutate(Group = "Healthy")

# significant_genes <- c("AASS", "ABCB7", "ABHD10", "ACSM4", "ACSM5", "AKR7A2", "ARMCX1", 
#                        "ATP5PO", "C15orf48", "CA5B", "CAT", "CCDC90B", "COMTD1", "COQ8A", 
#                        "COX7A2L", "DMAC2L", "DNM1L", "FAHD1", "FASTK", "FKBP10", "FPGS", 
#                        "FTMT", "GHITM", "HADH", "HIBCH", "IARS2", "KYAT3", "LIPT2", 
#                        "LYRM9", "MICU1", "MLYCD", "MRPS10", "MRPS16", "MRPS25", "MTFR1", 
#                        "MTX3", "NME3", "NT5DC3", "NUBPL", "PDK1", "RCC1L", "SDHAF2", 
#                        "SFXN2", "SLC25A29", "SMIM20", "SUPV3L1", "TIMM44", "TMEM143", 
#                        "TRIT1", "TRMU")

# significant_genes <- c("AK2", "ATAD3A", "ATAD3B", "BOLA1", "CASP9", "KMO", "MTARC2", 
#   "TDRKH", "COMTD1", "NUDT13", "BCO2", "CAT", "SDHAF2", "TIMM10B", 
#   "TMEM126B", "ALDH1L2", "ATP23", "CRY1", "DIABLO", "GATC", "MMAB", 
#   "MRPL42", "USP30", "CARS2", "MIPEP", "BCL2L2", "DHRS4", "BCL2A1", 
#   "C15orf48", "C15orf61", "CYP11A1", "MTFMT", "ACSM2A", "CA5A", 
#   "CMC2", "DNAJA3", "HSDL1", "MLYCD", "NME3", "NME4", "PAM16", 
#   "PDPR", "ACACA", "C1QBP", "CISD3", "LYRM9", "MRM1", "MRPL45", 
#   "MYO19", "SLC25A35", "SPATA20", "BCL2", "FAM210A", "RBFA", "CLPP", 
#   "CPT1C", "NDUFA3", "QTRT1", "RDH13", "TIMM44", "UQCRFS1", "BCL2L11", 
#   "BCS1L", "BOK", "BOLA3", "C2orf69", "CASP8", "CMPK2", "DBI", 
#   "FAHD2A", "MCEE", "MRPL44", "NDUFA10", "NEU4", "COX4I2", "MRPS26", 
#   "ATP5PF", "ATP5PO", "CBR3", "DNAJC28", "BCL2L13", "BID", "BIK", 
#   "COMT", "GCAT", "NDUFA6", "ACAA1", "C3orf33", "SUCLG2", "CCDC58", 
#   "CBR4", "MRPL1", "CCDC127", "CKMT2", "MCCC2", "MRPS27", "PRELID2", 
#   "MRPS36", "BPHL", "HSD17B8", "MCCD1", "MRPL18", "MRPS18B", "TOMM6", 
#   "VARS2", "ARF5", "CHCHD2", "CHCHD3", "FAM185A", "NDUFA5", "SLC25A13", 
#   "BNIP3L", "C8orf82", "CYP11B1", "DECR1", "LYPLA1", "PDP1", "TMEM65", 
#   "GLDC", "MRRF", "STOM", "ABCB7", "ACOT9", "AIFM1", "ALAS2", "APOOL", 
#   "ARMCX2", "CA5B", "PDHA1", "TMLHE", "TRMT2B")
# top_n <- 30  # 想要显示的行数
# 
# # 计算方差
# row_variance <- apply(heat_matrix, 1, var, na.rm = TRUE)
# 
# # Checkwhether or not有足够的行
# if (nrow(heat_matrix) <= top_n) {
#   heat_matrix_filtered <- heat_matrix
#   warning(paste("原始矩阵只有", nrow(heat_matrix), "行，无需筛选"))
# } else {
# # 选择方差最大的行
#   top_indices <- order(row_variance, decreasing = TRUE)[1:top_n]
#   heat_matrix_filtered <- heat_matrix[top_indices, ]
#   
# # 打印Filter信息
#   cat("从", nrow(heat_matrix), "行中筛选出", top_n, "行\n")
#   cat("方差范围:", round(range(row_variance, na.rm = TRUE), 3), "\n")
#   cat("筛选阈值:", round(sort(row_variance, decreasing = TRUE)[top_n], 3), "\n")
# }
# 
# # InspectFilterResults
# print(dim(heat_matrix_filtered))
# print(head(rownames(heat_matrix_filtered)))
# significant_genes <- c("DHRS4", "BCL2A1", "MMAB", "C15orf48", "NUDT13", "MTARC2", 
#   "MRPL18", "FAM185A", "BID", "CCDC127", "ATP5PO", "BCL2L11", "BPHL", 
#   "CA5A", "ALDH1L2", "NDUFA10", "PRELID2", "C15orf61", "TDRKH","LIPT2")


significant_genes<- c("BCL2L2","BCO2","CBR4","CMC2","MIPEP","SPATA20","UQCRFS1","C3orf33","CBR3",
                "ACSM2A","CASP8","GCAT","CASP9","RBFA","ATAD3B","MRPL18","BCL2A1","SUGL2",
                "CYP11B1","FAM185A","MMAB","PDPR","MCEE","COMT","C29orf69","HSDL1",
                "CARS2","MLYCD","ALDH2L2","BCL2","CMPK2","MRPS27","BID","MTARC2",
                "MIFMT","ATP5PO","CPT1C","HIBCH","COQ8A","LIPT2")
daf_combined<- rbind(case_daf00_filt , case_daf01_filt,case_daf11_filt)%>%
  mutate(
    DAF_Category = case_when(
      DAF <= 0.001 ~ "DAF ≤ 0.1%",
      DAF > 0.001 & DAF <= 0.01 ~ "0.1% < DAF ≤ 1%",
      DAF > 0.01 & DAF <= 0.05 ~ "1% < DAF ≤ 5%",
      DAF > 0.05 ~ "DAF > 5%",
      TRUE ~ NA_character_
    ),
    DAF_Category = factor(
      DAF_Category,
      levels = c("DAF ≤ 0.1%", "0.1% < DAF ≤ 1%", "1% < DAF ≤ 5%", "DAF > 5%"),
      labels = c("DAF 0.1%", "DAF 1%", "DAF 5%", "DAF > 5%")
    )
  )

daf_combined2<- rbind(case_daf_filt ,control_daf)%>%
  mutate(
    DAF_Category = case_when(
      DAF <= 0.001 ~ "DAF ≤ 0.1%",
      DAF > 0.001 & DAF <= 0.01 ~ "0.1% < DAF ≤ 1%",
      DAF > 0.01 & DAF <= 0.05 ~ "1% < DAF ≤ 5%",
      DAF > 0.05 ~ "DAF > 5%",
      TRUE ~ NA_character_
    ),
    DAF_Category = factor(
      DAF_Category,
      levels = c("DAF ≤ 0.1%", "0.1% < DAF ≤ 1%", "1% < DAF ≤ 5%", "DAF > 5%"),
      labels = c("DAF 0.1%", "DAF 1%", "DAF 5%", "DAF > 5%")
    )
  )

### 将数据框转换为data.table ###
setDT(daf_combined)
setDT(daf_combined2)
setDT(genes)

### 确保POSandPosition列are数值型 ###
daf_combined[, POS := as.numeric(POS)]
daf_combined2[, POS := as.numeric(POS)]
### 进行区间连接 ###
result <- genes[
  daf_combined, 
  on = .(hg38_Chromosome = CHROM, 
         hg38_Start <= POS, 
         hg38_End >= POS),
  .(CHROM = i.CHROM,
    POS = i.POS,
    # ID = i.ID,
    REF = i.REF,
    ALT = i.ALT,
    AA = i.AA,
    Ancestral_Allele = i.Ancestral_Allele,
    Derived_Allele = i.Derived_Allele,
    DAF = i.DAF,
    # N_Samples = i.N_Samples,
    Group = i.Group,
    Symbol = Symbol),
  allow.cartesian = TRUE
] 
result2 <- genes[
  daf_combined2, 
  on = .(hg38_Chromosome = CHROM, 
         hg38_Start <= POS, 
         hg38_End >= POS),
  .(CHROM = i.CHROM,
    POS = i.POS,
    # ID = i.ID,
    REF = i.REF,
    ALT = i.ALT,
    AA = i.AA,
    Ancestral_Allele = i.Ancestral_Allele,
    Derived_Allele = i.Derived_Allele,
    DAF = i.DAF,
    # N_Samples = i.N_Samples,
    Group = i.Group,
    Symbol = Symbol),
  allow.cartesian = TRUE
] 

heatdf <- result %>% 
  filter(Symbol %in% significant_genes) %>% 
  group_by(Group, Symbol) %>% 
  mutate(
    n_sites = n(),  # 该Gene在该组中的位点总数
    sum_daf = sum(DAF) / n_sites
  ) %>% 
  distinct(Group, Symbol, sum_daf,n_sites )

heatdf2 <- result2 %>% 
  filter(Symbol %in% significant_genes) %>% 
  group_by(Group, Symbol) %>% 
  mutate(
    n_sites = n(),  # 该Gene在该组中的位点总数
    sum_daf = sum(DAF) / n_sites
  ) %>% 
  distinct(Group, Symbol, sum_daf,n_sites )

heatdf_with_ci <- result %>% 
  filter(Symbol %in% significant_genes) %>% 
  group_by(Group, Symbol) %>% 
  summarise(
    mean_daf = mean(DAF),
    se_daf = sd(DAF) / sqrt(n()),
    ci_lower = mean_daf - 1.96 * se_daf,
    ci_upper = mean_daf + 1.96 * se_daf,
    n_sites = n(),
    .groups = "drop"
  )
head(heatdf)

heatdf_with_ci2 <- result2 %>% 
  filter(Symbol %in% significant_genes) %>% 
  group_by(Group, Symbol) %>% 
  summarise(
    mean_daf = mean(DAF),
    se_daf = sd(DAF) / sqrt(n()),
    ci_lower = mean_daf - 1.96 * se_daf,
    ci_upper = mean_daf + 1.96 * se_daf,
    n_sites = n(),
    .groups = "drop"
  )

### Assume您的数据名为 heatdf ###
### 如果您需要按 Group and sum_daf 排序，可以Add排序步骤 ###
heatdf_sorted <- heatdf %>%
  arrange(Group, desc(sum_daf))

heatdf_sorted2 <- heatdf2 %>%
  arrange(Group, desc(sum_daf))

### 保持 Symbol 的顺序（按排序后的顺序） ###
heatdf_sorted$Symbol <- factor(heatdf_sorted$Symbol, levels = unique(heatdf_sorted$Symbol))
heatdf_sorted2$Symbol <- factor(heatdf_sorted2$Symbol, levels = unique(heatdf_sorted2$Symbol))
### 准备矩阵格式数据 ###
# heat_matrix <- as.matrix(reshape2::acast(heatdf_sorted, Symbol ~ Group, value.var = "sum_daf"))
# 
# 
# heat_matrix[is.na(heat_matrix)] <- 0
# heat_matrix <- heat_matrix[rowSums(heat_matrix) != 0, ]


#######
# heat_matrix2 <- as.matrix(reshape2::acast(heatdf_sorted2, Symbol ~ Group, value.var = "sum_daf")) 
# 
# heat_matrix2[is.na(heat_matrix2)] <- 0
# heat_matrix2 <- heat_matrix2[rowSums(heat_matrix2) != 0, ]
####
# 1. 确定统一的Gene顺序（按第一个数据集的顺序）
common_genes <- unique(heatdf_sorted$Symbol)

# 2. 确保两个数据框使用相同的因子水平
heatdf_sorted$Symbol <- factor(heatdf_sorted$Symbol, levels = common_genes)
heatdf_sorted2$Symbol <- factor(heatdf_sorted2$Symbol, levels = common_genes)

# 3. 获取所有组
all_groups <- sort(unique(c(unique(heatdf_sorted$Group), unique(heatdf_sorted2$Group))))

# 4. Create矩阵，确保使用相同的行列顺序
heat_matrix <- as.matrix(reshape2::acast(
  heatdf_sorted,
  Symbol ~ Group,
  value.var = "sum_daf",
  fun.aggregate = sum, 
  drop = FALSE
))

heat_matrix2 <- as.matrix(reshape2::acast(
  heatdf_sorted2,
  Symbol ~ Group,
  value.var = "sum_daf",
  fun.aggregate = sum, 
  drop = FALSE
))

# 5. 按相同的顺序重新排列列（组）
existing_order <- intersect(all_groups, colnames(heat_matrix))
heat_matrix <- heat_matrix[, existing_order, drop = FALSE]
# heat_matrix <- heat_matrix[, all_groups, drop = FALSE]
existing_order2 <- intersect(all_groups, colnames(heat_matrix2))
heat_matrix2 <- heat_matrix2[, existing_order2, drop = FALSE]

# 6. 用0填充NA
heat_matrix[is.na(heat_matrix)] <- 0
heat_matrix2[is.na(heat_matrix2)] <- 0

#####
genelist3 <- c("BID","BCL2","HSDL1","PDPR","CASP8","COMT","MCEE","MLYCD","MRPS27","CBR4",
"CPT1C","FAM185A","MIPEP","UQCRFS1","CMPK2","C3orf33","CARS2","ATP5PO",
"MTARC2","BCO2","BCL2L2")

genelist4 <- c("BCL2A1","MRPL18","MMAB","CYP11B1","CMC2","CASP9","COQ8A","SPATA20",
  "LIPT2","GCAT","CBR3","RBFA","ACSM2A","HIBCH","ATAD3B")

heat_matrix3 <- heat_matrix[rownames(heat_matrix) %in% genelist3, ]
heat_matrix4 <- heat_matrix[rownames(heat_matrix) %in% genelist4,c("β0/β+","β0/β0") ]

#####
p1 <- pheatmap(
  heat_matrix,
  color = colorRampPalette(c("blue", "#E0ECF5", "red"))(100),
  breaks = seq(min(heatdf_sorted$sum_daf), max(heatdf_sorted$sum_daf), length.out = 100),
  main = "Heatmap of sum_daf by Group and Symbol",
  cluster_rows = T,
  cluster_cols = T,
  show_rownames = TRUE,
  show_colnames = TRUE,
  fontsize_row = 8,
  angle_col = 45,
  border_color = "white",
  cellwidth = 30,
  cellheight = 10,
  # Add数值显示
  display_numbers = matrix(
    ifelse(is.na(heat_matrix), "", sprintf("%.4f", heat_matrix)),
    nrow = nrow(heat_matrix),
    ncol = ncol(heat_matrix)
  ),
  number_color = "black",
  fontsize_number = 6  # 数值字体大小
)

p1

# p1 <- pheatmap(
# t(heat_matrix),  # 关键：使用 t() 转置矩阵
#   color = colorRampPalette(c("blue", "#E0ECF5", "red"))(100),
#   breaks = seq(min(heatdf_sorted$sum_daf), max(heatdf_sorted$sum_daf), length.out = 100),
#   main = "Heatmap of sum_daf by Group and Symbol",
#   cluster_rows = FALSE,
#   cluster_cols = FALSE,
#   show_rownames = TRUE,
#   show_colnames = TRUE,
#   fontsize_row = 8,
#   angle_col = 45,
#   border_color = "white",
#   cellwidth = 30,
#   cellheight = 10,
# # Add数值显示 - 注意也要转置
#   display_numbers = matrix(
#     ifelse(is.na(t(heat_matrix)), "", sprintf("%.4f", t(heat_matrix))),
#     nrow = nrow(t(heat_matrix)),
#     ncol = ncol(t(heat_matrix))
#   ),
#   number_color = "black",
# fontsize_number = 6  # 数值字体大小
# )
# p1
p2 <- pheatmap(
  heat_matrix2,
  color = colorRampPalette(c("blue", "#E0ECF5", "red"))(100),
  breaks = seq(min(heatdf_sorted2$sum_daf), max(heatdf_sorted2$sum_daf), length.out = 100),
  main = "Heatmap of sum_daf by Group and Symbol",
  cluster_rows = T,
  cluster_cols = FALSE,
  show_rownames = TRUE,
  show_colnames = TRUE,
  fontsize_row = 8,
  angle_col = 45,
  border_color = "white",
  cellwidth = 30,
  cellheight = 10,
  # Add数值显示
  display_numbers = matrix(
    ifelse(is.na(heat_matrix2), "", sprintf("%.4f", heat_matrix2)),
    nrow = nrow(heat_matrix2),
    ncol = ncol(heat_matrix2)
  ),
  number_color = "black",
  fontsize_number = 6  # 数值字体大小
)
p2

# 
# 
# 
# 
# p2

# p2 <- pheatmap(
# t(heat_matrix2),  # 关键：使用 t() 转置矩阵
#   color = colorRampPalette(c("blue", "#E0ECF5", "red"))(100),
#   breaks = seq(min(heatdf_sorted2$sum_daf), max(heatdf_sorted2$sum_daf), length.out = 100),
#   main = "Heatmap of sum_daf by Group and Symbol",
#   cluster_rows = FALSE,
#   cluster_cols = FALSE,
#   show_rownames = TRUE,
#   show_colnames = TRUE,
#   fontsize_row = 8,
#   angle_col = 45,
#   border_color = "white",
#   cellwidth = 30,
#   cellheight = 10,
# # 注意：显示数值的矩阵也需要转置
#   display_numbers = matrix(
#     ifelse(is.na(t(heat_matrix2)), "", sprintf("%.4f", t(heat_matrix2))),
#     nrow = nrow(t(heat_matrix2)),
#     ncol = ncol(t(heat_matrix2))
#   ),
#   number_color = "black",
#   fontsize_number = 5
# )
# p2 

p3 <- pheatmap(
  heat_matrix3,
  color = colorRampPalette(c("blue", "#E0ECF5", "red"))(100),
  breaks = seq(min(heatdf_sorted$sum_daf), max(heatdf_sorted$sum_daf), length.out = 100),
  main = "Heatmap of sum_daf by Group and Symbol",
  cluster_rows = T,
  cluster_cols = T,
  show_rownames = TRUE,
  show_colnames = TRUE,
  fontsize_row = 8,
  angle_col = 45,
  border_color = "white",
  cellwidth = 30,
  cellheight = 10,
  # Corrected：使用Filter后的矩阵Create显示数值
  display_numbers = matrix(
    ifelse(is.na(heat_matrix3), "", sprintf("%.4f", heat_matrix3)),
    nrow = nrow(heat_matrix3),
    ncol = ncol(heat_matrix3)
  ),
  number_color = "black",
  fontsize_number = 6
)

p3

p4 <- pheatmap(
  heat_matrix4,
  color = colorRampPalette(c("blue", "#E0ECF5", "red"))(100),
  breaks = seq(min(heatdf_sorted$sum_daf), max(heatdf_sorted$sum_daf), length.out = 100),
  main = "Heatmap of sum_daf by Group and Symbol",
  cluster_rows = T,
  cluster_cols = T,
  show_rownames = TRUE,
  show_colnames = TRUE,
  fontsize_row = 8,
  angle_col = 45,
  border_color = "white",
  cellwidth = 30,
  cellheight = 10,
  # Corrected：使用Filter后的矩阵Create显示数值
  display_numbers = matrix(
    ifelse(is.na(heat_matrix4), "", sprintf("%.4f", heat_matrix4)),
    nrow = nrow(heat_matrix4),
    ncol = ncol(heat_matrix4)
  ),
  number_color = "black",
  fontsize_number = 6
)

p4

save_pheatmap_pdf <- function(x, filename, width=10, height=8) {
  pdf(filename, width = width, height = height)
  grid::grid.newpage()
  grid::grid.draw(x$gtable)
  dev.off()
}
save_pheatmap_pdf(p1, "D:\\biosoft\\1000thal\\mtDNA\\figure/figure6D1.heatmap_pheatmap.pdf")
save_pheatmap_pdf(p2, "D:\\biosoft\\1000thal\\mtDNA\\figure/figure6D2.heatmap_pheatmap.pdf")
save_pheatmap_pdf(p4, "D:\\biosoft\\1000thal\\mtDNA\\figure/figure6D4.heatmap_pheatmap.pdf")
