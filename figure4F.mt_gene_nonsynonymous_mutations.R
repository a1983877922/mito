rm(list=ls())
library(dplyr)
library(xlsx)
library(broom)
library(data.table)
library(ggpubr) # 继承ggplot语法
library(patchwork) # Composite plot包
library(ggsci) #配色包
library(tidyr)
library(dplyr)
### Load cached, skip recomputation ###
# load("D:\\biosoft\\1000thal\\mtDNA\\R script/Mitochondria_Mutational_Burden.RData")
header1020 <- read.table("D:\\biosoft\\1000thal\\mtDNA\\genemut\\header.1020",sep = "\t")
header409 <- read.table("D:\\biosoft\\1000thal\\mtDNA\\genemut\\409/header.409",sep = "\t")
ped164 <- readxl::read_excel("D:\\biosoft\\1000thal\\mtDNA\\Maternal inheritance/232ped.xlsx",sheet = "164TRIOS_MOTHER")
ped68 <- readxl::read_excel("D:\\biosoft\\1000thal\\mtDNA\\Maternal inheritance/232ped.xlsx",sheet = "68SINGON")
ped <- rbind(ped164,ped68) %>% 
  select(MHID) %>% 
  pull()
###
hl1020 <- read.table("D:\\biosoft\\1000thal\\mtDNA\\genemut\\1020.mt.ano.filter.ft2onefilt")
hl409 <- read.table("D:\\biosoft\\1000thal\\mtDNA\\genemut\\409.mt.ano.filter.ft2onefilt") 


# hl1020 <- read.table("D:\\biosoft\\1000thal\\mtDNA\\genemut\\1020.mt.ano.filter.hl2zero") %>% 
#   filter(!V2 %in% c(301, 302, 310, 316, 3107, 16182)) %>%  #FiltergnomAD认定的容易错的点
#   mutate(V3=paste(V2,V4,V5,sep = "_")) %>%
#   # filter(rowSums(. >= 0.05) > 0) %>%
#   filter(rowSums(across(7:ncol(.), ~ . >= 0.05)) > 0) %>%  # 只对第7列到最后一列进行Filter
#   distinct(.keep_all = TRUE,V3) %>% 
#   filter(!grepl(",",V5))

colnames(hl1020) <- header1020

# hl409 <- read.table("D:\\biosoft\\1000thal\\mtDNA\\genemut/409/409.mt.ano.filter.hl2zero") %>% 
#   filter(!V2 %in% c(301, 302, 310, 316, 3107, 16182)) %>%  #FiltergnomAD认定的容易错的点
#   mutate(V3=paste(V2,V4,V5,sep = "_")) %>%
#   # filter(rowSums(. >= 0.05) > 0) %>%
#   filter(rowSums(across(7:ncol(.), ~ . >= 0.05)) > 0) %>%  # 只对第7列到最后一列进行Filter
#   distinct(.keep_all = TRUE,V3) %>% 
#   filter(!grepl(",",V5))

colnames(hl409) <- header409
hl409 <- hl409 %>% 
  select(CHROM, POS, ID, REF, ALT, INFO, all_of(ped))
### mtDNA gene features ###
mtgff3 <- read.table("D:\\biosoft\\1000thal\\mtDNA\\genemut\\1020bedout\\mt.gff3.bed",sep = "\t")
mtgff3$V2 <- mtgff3$V2 + 1
mtgff3$"with" <- mtgff3$V3 - mtgff3$V2 + 1
mtgff3[1,6] <- "D_loop"
mtgff3[39,6] <- "D_loop"
colnames(mtgff3) <- c("chrom","start","end","id","strand","gene","with")

##
# hl1020_het <- hl1020
hl1020_het <- hl1020 %>%
  mutate(across(7:ncol(.), ~ case_when(
    . <= 0.05 ~ 0,
    . >= 0.95 ~ 0,#
    TRUE ~ .
  )))

# hl409_het <- hl409
hl409_het <- hl409 %>%
  mutate(across(7:ncol(.), ~ case_when(
    . <= 0.05 ~ 0,
    . >= 0.95 ~ 0,#
    TRUE ~ .
  )))
##
mtgff3_PART <- mtgff3 %>% 
  filter(gene %in% c("ATP8","ND4L"))
##
setDT(hl1020_het)
setDT(hl409_het)
setDT(mtgff3)
setDT(mtgff3_PART)
#
hl1020_het_spec <- hl1020_het
hl409_het_spec <- hl409_het
#
hl1020_het[mtgff3, GENE := i.gene, on = .(POS >= start, POS <= end)] 

hl1020_het$GENE[is.na(hl1020_het$GENE)] <- "intergenic"


hl1020_het_spec[mtgff3_PART, GENE := i.gene, on = .(POS >= start, POS <= end)] 

hl1020_het_spec<- hl1020_het_spec %>% 
  filter(GENE %in% c("ATP8","ND4L"))


hl409_het[mtgff3, GENE := i.gene, on = .(POS >= start, POS <= end)] 

hl409_het$GENE[is.na(hl409_het$GENE)] <- "intergenic"


hl409_het_spec[mtgff3_PART, GENE := i.gene, on = .(POS >= start, POS <= end)] 

hl409_het_spec<- hl409_het_spec %>% 
  filter(GENE %in% c("ATP8","ND4L"))

#
coding_bed <- c("ND1","ND2","COX1","COX2","ATP6","COX3","ND3","ND4","ND5","ND6","CYTB")
coding_bed1 <- c("ATP8","ND4L")
### nd5 ###
combine_gene_frame <- function(gene_name){
  hl1020_max <- hl1020_het %>% 
    filter(GENE == gene_name) %>% 
    filter(INFO != "synonymous_variant") %>% 
    select(!c("CHROM", "REF", "ALT", "INFO", "GENE")) %>% 
    rowwise() %>% 
    mutate(Max_Value = max(c_across(-c("ID", "POS")), na.rm = TRUE)) %>% 
    ungroup() %>% 
    ### 只保留IDand最大值列 ###
    select(ID,POS, Max_Value) %>% 
    ### 重命名ID列为"ID" ###
    filter(Max_Value != 0)
  
  
  hl409_max <- hl409_het %>% 
    filter(GENE == gene_name) %>% 
    filter(INFO != "synonymous_variant") %>% 
    select(!c("CHROM", "REF", "ALT", "INFO", "GENE")) %>% 
    rowwise() %>% 
    mutate(Max_Value = max(c_across(-c("ID", "POS")), na.rm = TRUE)) %>% 
    ungroup() %>% 
    ### 只保留IDand最大值列 ###
    select(ID,POS, Max_Value) %>% 
    ### 重命名ID列为"ID" ###
    filter(Max_Value != 0)
  
  
  combine_df <- full_join(hl1020_max,hl409_max,by="ID") %>% 
    mutate(
      POS = coalesce(POS.x, POS.y)
    ) %>% 
    select(-POS.x, -POS.y) %>%   # Remove原始的两列（可选）
    rename(
      Thalassemia = Max_Value.x,
      carriers =Max_Value.y
    ) %>% 
    mutate(
      Thalassemia = replace_na(Thalassemia, 0),    # NA → 0
      carriers = replace_na(carriers, 0)  # NA → 0
    )
  
  combin_long <- combine_df %>%
    pivot_longer(
      cols = c(Thalassemia, carriers), # 要转换的列名
      names_to = "Group",        # 新列名，表示之前列的名字
      values_to = "MAX_HL"       # 新列名，表示之前列的值
    ) %>% 
    mutate(Group = factor(Group, levels = c("Thalassemia", "carriers")))  # Specify顺序
  
  return(combin_long)
}
combine_gene_frame_spec <- function(gene_name){
  hl1020_max <- hl1020_het_spec %>% 
    filter(GENE == gene_name) %>% 
    filter(INFO != "synonymous_variant") %>% 
    select(!c("CHROM", "REF", "ALT", "INFO", "GENE")) %>% 
    rowwise() %>% 
    mutate(Max_Value = max(c_across(-c("ID", "POS")), na.rm = TRUE)) %>% 
    ungroup() %>% 
    ### 只保留IDand最大值列 ###
    select(ID,POS, Max_Value) %>% 
    ### 重命名ID列为"ID" ###
    filter(Max_Value != 0)
  
  
  hl409_max <- hl409_het_spec %>% 
    filter(GENE == gene_name) %>% 
    filter(INFO != "synonymous_variant") %>% 
    select(!c("CHROM", "REF", "ALT", "INFO", "GENE")) %>% 
    rowwise() %>% 
    mutate(Max_Value = max(c_across(-c("ID", "POS")), na.rm = TRUE)) %>% 
    ungroup() %>% 
    ### 只保留IDand最大值列 ###
    select(ID,POS, Max_Value) %>% 
    ### 重命名ID列为"ID" ###
    filter(Max_Value != 0)
  
  
  combine_df <- full_join(hl1020_max,hl409_max,by="ID") %>% 
    mutate(
      POS = coalesce(POS.x, POS.y)
    ) %>% 
    select(-POS.x, -POS.y) %>%   # Remove原始的两列（可选）
    rename(
      Thalassemia = Max_Value.x,
      carriers =Max_Value.y
    ) %>% 
    mutate(
      Thalassemia = replace_na(Thalassemia, 0),    # NA → 0
      carriers = replace_na(carriers, 0)  # NA → 0
    )
  
  combin_long <- combine_df %>%
    pivot_longer(
      cols = c(Thalassemia, carriers), # 要转换的列名
      names_to = "Group",        # 新列名，表示之前列的名字
      values_to = "MAX_HL"       # 新列名，表示之前列的值
    ) %>% 
    mutate(Group = factor(Group, levels = c("Thalassemia", "carriers")))  # Specify顺序
  
  return(combin_long)
}
combine_gene_frame_dloop <- function(gene_name){
  hl1020_max <- hl1020_het %>% 
    filter(GENE == gene_name) %>% 
    select(!c("CHROM", "REF", "ALT", "INFO", "GENE")) %>% 
    rowwise() %>% 
    mutate(Max_Value = max(c_across(-c("ID", "POS")), na.rm = TRUE)) %>% 
    ungroup() %>% 
    ### 只保留IDand最大值列 ###
    select(ID,POS, Max_Value) %>% 
    ### 重命名ID列为"ID" ###
    filter(Max_Value != 0)
  
  
  hl409_max <- hl409_het %>% 
    filter(GENE == gene_name) %>% 
    select(!c("CHROM", "REF", "ALT", "INFO", "GENE")) %>% 
    rowwise() %>% 
    mutate(Max_Value = max(c_across(-c("ID", "POS")), na.rm = TRUE)) %>% 
    ungroup() %>% 
    ### 只保留IDand最大值列 ###
    select(ID,POS, Max_Value) %>% 
    ### 重命名ID列为"ID" ###
    filter(Max_Value != 0)
  
  
  combine_df <- full_join(hl1020_max,hl409_max,by="ID") %>% 
    mutate(
      POS = coalesce(POS.x, POS.y)
    ) %>% 
    select(-POS.x, -POS.y) %>%   # Remove原始的两列（可选）
    rename(
      Thalassemia = Max_Value.x,
      carriers =Max_Value.y
    ) %>% 
    mutate(
      Thalassemia = replace_na(Thalassemia, 0),    # NA → 0
      carriers = replace_na(carriers, 0)  # NA → 0
    )
  
  combin_long <- combine_df %>%
    pivot_longer(
      cols = c(Thalassemia, carriers), # 要转换的列名
      names_to = "Group",        # 新列名，表示之前列的名字
      values_to = "MAX_HL"       # 新列名，表示之前列的值
    ) %>% 
    mutate(Group = factor(Group, levels = c("Thalassemia", "carriers")))  # Specify顺序
  
  return(combin_long)
}

for (i in coding_bed){
  print(i)
  df_name <- paste0("combine_",i)
  assign(df_name, combine_gene_frame(i))
}

for (i in coding_bed1){
  print(i)
  df_name <- paste0("combine_",i)
  assign(df_name, combine_gene_frame_spec(i))
}

combine_Dloop<- combine_gene_frame_dloop("D_loop")
combine_Dloop <- combine_Dloop %>%
  mutate(POS = if_else(POS < 15000, POS + 16569, POS))
### plot ###

plot_funt <- function(df){
  
  labs_name <- paste0("mtDNA ",i)
  
  p <- ggplot(df, aes(x = POS)) +
    geom_point(aes(y = MAX_HL, color = Group)) +
    geom_segment(aes(y = 0, yend = MAX_HL, color = Group), linewidth=0.1) +
    scale_colour_manual(
      values=c(Thalassemia="#DF3027", carriers="#2A9698"),
      name=NULL,
      labels=c("Thalassemia", "Carriers")
    ) +
    labs(y="MAX heteroplasmy levels of non-synonymous variants", x=labs_name) +
    theme_classic() +
    theme(
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      panel.grid.minor.y = element_blank(),
      axis.ticks.y = element_blank(),
      axis.text.x = element_text(size=12, face='bold'),
      axis.title.x = element_text(size=15, face='bold'),
      axis.text.y = element_text(size=12, face='bold'),
      axis.title.y = element_text(size=15, face='bold'),
      # panel.border = element_rect(fill=NA, color="black", size=1.5, linetype="solid")
    ) +
    scale_y_continuous(limits = c(0, 1),expand = c(0, 0))

  return(p)
}

for (i in c(coding_bed,coding_bed1,"Dloop")){
  print(i)
  plot_name <- paste0("p_",i)
  df_name <- paste0("combine_",i)
  p <- plot_funt(get(df_name))
  assign(plot_name, p)
}

p_ND1 %>% ggexport(filename = "D:/biosoft/1000thal/mtDNA/figure/figure2B.ND1 nonsynonymous variants lolliplot202512.pdf",width = 8,height = 6,res=300)

p_ND2 %>% ggexport(filename = "D:/biosoft/1000thal/mtDNA/figure/figure2B.ND2 nonsynonymous variants lolliplot202512.pdf",width = 8,height = 6,res=300)

p_ND3 %>% ggexport(filename = "D:/biosoft/1000thal/mtDNA/figure/figure2B.ND3 nonsynonymous variants lolliplot202512.pdf",width = 8,height = 6,res=300)

p_ND4 %>% ggexport(filename = "D:/biosoft/1000thal/mtDNA/figure/figure2B.ND4 nonsynonymous variants lolliplot202512.pdf",width = 8,height = 6,res=300)

p_ND4L %>% ggexport(filename = "D:/biosoft/1000thal/mtDNA/figure/figure2B.ND4L nonsynonymous variants lolliplot202512.pdf",width = 8,height = 6,res=300)

p_ND5 %>% ggexport(filename = "D:/biosoft/1000thal/mtDNA/figure/figure2B.ND5 nonsynonymous variants lolliplot202512.pdf",width = 8,height = 6,res=300)

p_ND6 %>% ggexport(filename = "D:/biosoft/1000thal/mtDNA/figure/figure2B.ND6 nonsynonymous variants lolliplot202512.pdf",width = 8,height = 6,res=300)
p_COX1 %>% ggexport(filename = "D:/biosoft/1000thal/mtDNA/figure/figure2B.COX1 nonsynonymous variants lolliplot202512.pdf",width = 8,height = 6,res=300)
p_COX2 %>% ggexport(filename = "D:/biosoft/1000thal/mtDNA/figure/figure2B.COX2 nonsynonymous variants lolliplot202512.pdf",width = 8,height = 6,res=300)
p_COX3 %>% ggexport(filename = "D:/biosoft/1000thal/mtDNA/figure/figure2B.COX3 nonsynonymous variants lolliplot202512.pdf",width = 8,height = 6,res=300)
p_ATP6 %>% ggexport(filename = "D:/biosoft/1000thal/mtDNA/figure/figure2B.ATP6 nonsynonymous variants lolliplot202512.pdf",width = 8,height = 6,res=300)
p_ATP8 %>% ggexport(filename = "D:/biosoft/1000thal/mtDNA/figure/figure2B.ATP8 nonsynonymous variants lolliplot202512.pdf",width = 8,height = 6,res=300)
p_CYTB %>% ggexport(filename = "D:/biosoft/1000thal/mtDNA/figure/figure2B.CYTB nonsynonymous variants lolliplot202512.pdf",width = 8,height = 6,res=300)
p_Dloop %>% ggexport(filename = "D:/biosoft/1000thal/mtDNA/figure/figure2B.Dloop nonsynonymous variants lolliplot202512.pdf",width = 8,height = 6,res=300)

p_ND1 
p_ND2
p_ND3
p_ND4
p_ND4L
p_ND5
p_ND6
p_COX1 # 特殊位点
p_COX2 # x
p_COX3 # x
p_ATP6
p_ATP8 #
p_CYTB #
p_Dloop

# p1 <- ggplot(combine_ND5, aes(x = POS)) +
#   geom_point(aes(y = MAX_HL, color = Group)) +
#   geom_segment(aes(y = 0, yend = MAX_HL, color = Group), linewidth=0.1) +
#   scale_colour_manual(
#     values=c(Thalassemia="#DF3027", carriers="#2A9698"),
#     name=NULL,
#     labels=c("Thalassemia", "Carriers")
#     ) +
#   labs(y="MAX heteroplasmy levels of non-synonymous variants", x="mtDNA ND5 (12337 - 14148)") +
#   theme_classic() +
#   theme(
#     panel.grid.major.x = element_blank(),
#     panel.grid.minor.x = element_blank(),
#     panel.grid.minor.y = element_blank(),
#     axis.ticks.y = element_blank(),
#     axis.text.x = element_text(size=12, face='bold'),
#     axis.title.x = element_text(size=15, face='bold'),
#     axis.text.y = element_text(size=12, face='bold'),
#     axis.title.y = element_text(size=15, face='bold'),
#     # panel.border = element_rect(fill=NA, color="black", size=1.5, linetype="solid")
#   ) +
#   scale_y_continuous(limits = c(0, 1),expand = c(0, 0))
#   # guides(color=guide_legend(ncol = 1, order=2, label.position='right')) +
#   # theme(legend.position = c(0.45,0.95), legend.background = element_rect(fill='white', colour='black'))
# 
# p1
# p1 %>% ggexport(filename = "D:/biosoft/1000thal/mtDNA/figure/figure2B.ND5 nonsynonymous variants lolliplot.pdf",width = 8,height = 6,res=300)


library(dplyr)
library(purrr)

# 1. First定义计算单个数据框平均差值的函数
calculate_mean_diff <- function(df) {
  ### 确保数据框contains必要的列 ###
  required_cols <- c("ID", "POS", "Group", "MAX_HL")
  if (!all(required_cols %in% names(df))) {
    warning("数据框缺少必要的列，跳过处理")
    return(NA)
  }
  
  ### 计算每个位点的差值，然后求平均 ###
  mean_diff <- df %>%
    pivot_wider(
      id_cols = c(ID, POS),
      names_from = Group,
      values_from = MAX_HL
    ) %>%
    mutate(DIFF = Thalassemia - carriers) %>%
    summarise(MEAN_DIFF = mean(DIFF, na.rm = TRUE)) %>%
    pull(MEAN_DIFF)
  
  return(mean_diff)
}

# 2. 如果有多个数据框在一个列表中
process_multiple_dataframes <- function(df_list, df_names = NULL) {
  ### 为每个数据框命名 ###
  if (is.null(df_names)) {
    df_names <- if (!is.null(names(df_list))) {
      names(df_list)
    } else {
      paste0("DataFrame_", seq_along(df_list))
    }
  }
  
  ### 计算每个数据框的平均差值 ###
  results <- map_dfr(df_list, function(df) {
    tibble(
      MEAN_DIFFERENCE = calculate_mean_diff(df)
    )
  }, .id = "DataFrame") %>%
    mutate(DataFrame = df_names[as.numeric(DataFrame)])
  
  return(results)
}



# 4. 增强版：Return每个数据框的详细统计
calculate_detailed_summary <- function(df_list, df_names = NULL) {
  if (is.null(df_names)) {
    df_names <- if (!is.null(names(df_list))) {
      names(df_list)
    } else {
      paste0("DF_", seq_along(df_list))
    }
  }
  
  results <- map_dfr(seq_along(df_list), function(i) {
    df <- df_list[[i]]
    name <- df_names[i]
    
    ### 计算每个位点的差值 ###
    diff_data <- df %>%
      pivot_wider(
        id_cols = c(ID, POS),
        names_from = Group,
        values_from = MAX_HL
      ) %>%
      mutate(DIFF = Thalassemia - carriers)
    
    ### 计算各种statistics ###
    diff_data %>%
      summarise(
        DataFrame = name,
        MEAN_DIFFERENCE = mean(DIFF, na.rm = TRUE),
        MEDIAN_DIFFERENCE = median(DIFF, na.rm = TRUE),
        SD_DIFFERENCE = sd(DIFF, na.rm = TRUE),
        MEAN_ABS_DIFFERENCE = mean(abs(DIFF), na.rm = TRUE),
        N_POSITIONS = n(),
        N_POSITIVE = sum(DIFF > 0, na.rm = TRUE),
        N_NEGATIVE = sum(DIFF < 0, na.rm = TRUE),
        PROP_POSITIVE = mean(DIFF > 0, na.rm = TRUE),
        MIN_DIFFERENCE = min(DIFF, na.rm = TRUE),
        MAX_DIFFERENCE = max(DIFF, na.rm = TRUE)
      )
  })
  
  return(results)
}

dfname <- c("ATP6", "ATP8", "COX1", "COX2", "COX3","CYTB", "Dloop", "ND1", "ND2", "ND3", "ND4", "ND4L", "ND5", "ND6")

results <- process_multiple_dataframes(df_list, dfname)


detailed_results <- calculate_detailed_summary(df_list,df_names =dfname)


write.table(detailed_results,"D:\\biosoft\\1000thal\\mtDNA\\differences of nonsynonymous variants across cohort.tsv",sep = "\t",row.names = F)
### no ###


## Save所有数据框
# setwd("D:\\biosoft\\1000thal\\mtDNA\\R script/")
# save.image("Mitochondria_Mutational_Burden.RData")
