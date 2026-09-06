rm(list = ls())
library(circlize)
library(dplyr)
library(tibble)
library(ggplot2)
library(tidyverse)
library(rtracklayer)
library(ComplexHeatmap)    #Plot图例
library(grid)
library(data.table)
library(stringr) 
## Load cached, skip recomputation
# load("D:\\biosoft\\1000thal\\mtDNA\\R script/circos图谱数据.RData")
mt_gc <- read.delim("D:/biosoft/1000thal/mtDNA/mtDNA_gc.list", header = T, stringsAsFactors = FALSE)
mt_coverage <- read.table("D:\\biosoft\\1000thal\\mtDNA\\1020mtDNA_out\\mt_coverage\\1020coverage.tsv.final", sep = "\t",header = T)


header1020 <- read.table("D:\\biosoft\\1000thal\\mtDNA\\genemut\\header.1020",sep = "\t")
header409 <- read.table("D:\\biosoft\\1000thal\\mtDNA\\genemut\\409/header.409",sep = "\t")
sampID1020 <- header1020[7:1026]
sampID409 <- header409[7:415]

#
# hl1020 <- read.table("D:\\biosoft\\1000thal\\mtDNA\\genemut\\1020.mt.ano.filter.hl2zero") %>% 
#   filter(!V2 %in% c(301, 302, 310, 316, 3107, 16182)) %>%  #FiltergnomAD认定的容易错的点
#   mutate(V3=paste(V2,V4,V5,sep = "_")) %>%
#   # filter(rowSums(. >= 0.05) > 0) %>%
#   filter(rowSums(across(7:ncol(.), ~ . >= 0.05)) > 0) %>%  # 只对第7列到最后一列进行Filter
#   distinct(.keep_all = TRUE,V3) %>% 
#   filter(!grepl(",",V5))


# hl409 <- read.table("D:\\biosoft\\1000thal\\mtDNA\\genemut\\409\\409.mt.ano.filter.hl2zero") %>% 
#   filter(!V2 %in% c(301, 302, 310, 316, 3107, 16182)) %>%  #FiltergnomAD认定的容易错的点
#   mutate(V3=paste(V2,V4,V5,sep = "_")) %>%
#   # filter(rowSums(. >= 0.05) > 0) %>%
#   filter(rowSums(across(7:ncol(.), ~ . >= 0.05)) > 0) %>%  # 只对第7列到最后一列进行Filter
#   distinct(.keep_all = TRUE,V3) %>% 
#   filter(!grepl(",",V5))
hl1020 <- read.table("D:\\biosoft\\1000thal\\mtDNA\\genemut\\1020.mt.ano.filter.ft2onefilt")
hl409 <- read.table("D:\\biosoft\\1000thal\\mtDNA\\genemut\\409.mt.ano.filter.ft2onefilt") 

colname1020 <- c("chrom","pos","id","ref","alt","info",sampID1020)
colname409 <- c("chrom","pos","id","ref","alt","info",sampID409)
colnames(hl1020) <- colname1020
colnames(hl409) <- colname409

### 将大于等于0.05的值变为0 ###
hl1020[ , 7:1026][hl1020[ , 7:1026] < 0.1] <- 0
hl409[ , 7:415][hl409[ , 7:415] < 0.1] <- 0

hl1020[ , 7:1026][hl1020[ , 7:1026] > 0.9] <- 0
hl409[ , 7:415][hl409[ , 7:415] > 0.9] <- 0

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
    filter(!!sym(child)>0.1) %>% 
    filter(!!sym(child)<0.9)
  
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

mt_mean_snv <- bed_df2 %>%
  mutate(type2= case_when(
    info == "upstream_gene_variant" ~ "Upstream gene variant",
    info == "missense_variant" ~ "Missense variant",
    info == "synonymous_variant" ~ "Synonymous variant",
    TRUE ~ "Other variant"
  )
  )
##
mt_gff = import('D:/biosoft/1000thal/mtDNA/prefiles/MT.gff3') %>% as.data.frame()
mt_struct <- mt_gff %>% dplyr::select(c("start", "end", "width", "strand", "type", 
                                        "gene", "product"))
input <- mt_struct[mt_struct$type=="tRNA" |mt_struct$type=="D_loop"|mt_struct$type=="CDS"|mt_struct$type=="rRNA"|mt_struct$type=="D_loop",]
mt_CN <- read.table("D:\\biosoft\\1000thal\\mtDNA\\1020mtDNA_out\\1020.mtCN_mean.tsv", sep="\t",header = T) 
#
mtCN <- round(mean(mt_CN$mtCN_mean),2)
MTgenome_GC <- 44.36
cohort_name <- 'Beta Thalassemia'	#测序Sample名称
### 增加空行，Modifyhcr对应的第一行and最后一行 ###
input_h <- rbind(rep(NA,ncol(input)), input)
input_h[1, ] <- c(1, 576,576,"+","D_loop","HCR-1","HCR-1")
input_h[nrow(input_h), ] <- c(16024, 16569,546,"+","D_loop","HCR","HCR")
input_h[22, ] <- c(8366, 9207,842,"+","CDS","ATP8-6","ATPase 8/6")
input_h <- input_h[-23,]
# input_h <- rbind(input_h[1:22,], NA, input_h[23:nrow(input_h),])
# input_h[22, ] <- c(8366, 8527,162,"+","CDS","ATP8","ATPase 8")
# input_h[23, ] <- c(8527, 8572, 46,"+","CDS","ATP8-6","ATPase 8/6")
# input_h[24, ] <- c(8572, 9207,636,"+","CDS","ATP6","ATPase 6")
input_h[, c(1:3)] <- sapply(input_h[, c(1:3)], as.numeric)
input_h$seq_id <- "mtDNA"
input_h <- input_h[c("seq_id", setdiff(names(input_h), "seq_id"))]#提取列到第一列
input_h$mean <- input_h$start + input_h$width/2
rownames(input_h) <- 1:nrow(input_h)
input_h$Hstrand <- ifelse(input_h$strand=="+", input_h$gene,paste0("non-coding",rownames(input_h)))

input_h$Lstrand <- ifelse(input_h$strand=="-", input_h$gene,paste0("non-coding",rownames(input_h)))

input_h$color <- ifelse(input_h$type=="tRNA", "#329B64",
                        ifelse(input_h$type=="rRNA", "#A4C8D9",
                               ifelse(input_h$type=="CDS", "#F8984F", "#C74647")))

input_h$Hcolor <- input_h$color
input_h$Hcolor[grepl("non-coding", input_h$Hstrand)] <- "grey"
input_h$Lcolor <- input_h$color
input_h$Lcolor[grepl("non-coding", input_h$Lstrand)] <- "grey"
color = dput(input_h$color)
Hcolor = dput(input_h$Hcolor)
Lcolor = dput(input_h$Lcolor)

### 覆盖度 ###
### 计算滑窗Mean ###
window_avg_depth <- function(df, window_size, depth_col){
  
  n_windows <- ceiling(nrow(df)/window_size)
  
  avg_values <- numeric(n_windows)
  results_df <- data.frame(start_pos=integer(),
                           end_pos=integer(),  
                           avg_depth=numeric(),
                           stringsAsFactors = FALSE)
  
  for(i in 1:n_windows) {
    
    start_idx <- (i-1)*window_size + 1
    end_idx <- min(i*window_size, nrow(df))
    
    window_df <- df[start_idx:end_idx,]
    
    avg_values[i] <- mean(window_df[[depth_col]])
    
    results_df[i,1] <- window_df$pos[1]
    results_df[i,2] <- window_df$pos[nrow(window_df)]
    results_df[i,3] <- avg_values[i] 
    
  }
  
  return(results_df)
  
}

mt_mean_cov <- window_avg_depth(mt_coverage, 20, "target")
mt_mean_cov$seq_id<- "mtDNA"
mt_mean_cov <- mt_mean_cov [c("seq_id", setdiff(names(mt_mean_cov), "seq_id"))]#提取列到第一列
mt_genome_cov <- round(mean(mt_coverage$target), 2)
##
mt_coverage$seq_id <- "mtDNA"
mt_coverage$start <- mt_coverage$pos-1
mt_coverage$end <- mt_coverage$pos




#
Hdf <- input_h[,c("Hstrand","mean")] %>% filter(!grepl("non-coding", Hstrand))
Ldf <- input_h[,c("Lstrand","mean")] %>% filter(!grepl("non-coding", Lstrand))
#mtDNA
mt_seq <- input_h[1,1:3]
mt_seq$end <- 16569


### circos ###
### 使用circos.text将标签放在圈外 ###
labels <- c("D_loop", "TRNF", "RNR1", "TRNV", "RNR2", "TRNL1", "ND1", "TRNI", 
            "TRNQ", "TRNM", "ND2", "TRNW", "TRNA", "TRNN", "TRNC", "TRNY", 
            "COX1", "TRNS1", "TRND", "COX2", "TRNK", "ATP8-6", "COX3", "TRNG",
            "ND3", "TRNR", "ND4L", "ND4", "TRNH", "TRNS2", "TRNL2", "ND5", 
            "ND6", "TRNE", "CYTB", "TRNT", "TRNP", "D_loop")
circos.clear()
circos.par(start.degree = 90, cell.padding = c(0,0,0,0), gap.degree = 0)

# 1. 初始化（保留默认轴and标签）
circos.genomicInitialize(mt_seq, plotType = c('axis', 'labels'), major.by = 2000, track.height = 0.02)

# 2. 在初始化轨道外推标签（覆盖默认标签）
circos.track(ylim = c(0, 1), track.height = 0.01, bg.border = NA, track.index = 1,
             panel.fun = function(x, y) {
               for(i in seq_along(input_h$seq_id)) {
                 circos.text(input_h$mean[i], 1.5,  # y=1.5 向外推
                             labels[i], 
                             facing = "downward",  # 向下避免重叠
                             adj = c(0.5, 0.7),
                             cex = 0.7, 
                             col = "#B99D87")
               }
             })

# 3. 其他轨道（现在会Plot在标签轨道内侧）
circos.genomicTrackPlotRegion(
  input_h, track.height = 0.02, stack = TRUE, track.margin = c(0,0),
  panel.fun = function(region, value, ...) {
    circos.genomicRect(region, value, col = input_h$Hcolor, border = NA, ...)
  })

set_track_gap(gap = 0)
circos.genomicTrackPlotRegion(
  input_h, track.height = 0.02, stack = TRUE, track.margin = c(0,0),
  panel.fun = function(region, value, ...) {
    circos.genomicRect(region, value, col = input_h$Lcolor, border = NA, ...)
  })

circos.genomicTrack(mt_mean_snv, track.height = 0.6, ylim = c(0, 1), bg.col = NA, bg.border = NA,
                    panel.fun = function(region, value, ...) {
                      circos.genomicPoints(region, value, border = 'white', 
                                           cex = case_when(
                                             value$GENE == "D_loop" ~ 0.5,
                                             TRUE ~ 0.8,
                                           ),
                                           pch = case_when(
                                             value$type2 == "Upstream gene variant" ~ 18,
                                             value$type2 == "Missense variant" ~ 20,
                                             value$type2 == "Synonymous variant" ~ 2,
                                             value$type2 == "Other variant" ~ 3
                                           ),
                                           col = case_when(
                                             value$GENE == "D_loop" ~ "#C74647",
                                             value$GENE %in% coding_bed ~ "#F8984F",
                                             str_detect(value$GENE, "RNR") ~ "#A4C8D9",
                                             str_detect(value$GENE, "TR") ~ "#329B64",
                                             TRUE ~ "grey"
                                           )
                      )
                      circos.yaxis(labels.cex = 0.2, lwd = 0.1, tick.length = convert_x(0.15, 'mm'))
                      circos.axis(h = "bottom", labels = FALSE, major.tick = FALSE)
                    })





### old ###
# circos.clear()
# circos.par(start.degree = 90,cell.padding = c(0,0,0,0),
#            gap.degree=0)
# 
# circos.genomicInitialize(mt_seq, plotType = c('axis', 'labels'),major.by = 2000, track.height = 0.02)
# 
# 
# circos.genomicTrack(mt_mean_snv, track.height = 0.6, ylim = c(0, 1), bg.col = NA, bg.border = NA,
#                     panel.fun = function(region, value, ...) {
#                       circos.genomicPoints(region, value, border = 'white', 
#                                            
#                                            cex = case_when(
#                                            value$GENE == "D_loop" ~ 0.5,
#                                            # value$GENE %in% coding_bed ~ 4,
#                                            # str_detect(value$GENE, "RNR") ~ 16,
#                                            # str_detect(value$GENE, "TR") ~ 19,
#                                            TRUE ~ 0.8,
#                       ),
#                                            pch = case_when(
#                                              value$type2 == "Upstream gene variant" ~ 18,
#                                              value$type2 == "Missense variant" ~ 20,
#                                              value$type2 == "Synonymous variant" ~ 2,
#                                              value$type2 == "Other variant" ~ 3
#                                            )
#                                            ,
#                                            col = case_when(
#                                              value$GENE == "D_loop" ~ "#a2c986",
#                                              value$GENE %in% coding_bed ~ "#FFBE7A",
#                                              str_detect(value$GENE, "RNR") ~ "#66C2A5",
#                                              str_detect(value$GENE, "TR") ~ "#82B0D2",
#                                              TRUE ~ "grey"
#                                            )
#                                            
#                                            # col =  ifelse(value$type2 == "SNV",'#BF1D2D', '#293890') 
#                                            )
#                       circos.yaxis(
#                         # at=c(1,100,500,1000),
#                         labels.cex = 0.2, lwd = 0.1, tick.length = convert_x(0.15, 'mm'))
#                       circos.axis(h = "bottom",labels=F,major.tick = F)
#                     })
# 
# 
# 
# # #覆盖度柱状
# # circos.genomicTrack(
# #   mt_mean_cov, track.height = 0.08, ylim = c(0, (max(mt_mean_cov$avg_depth) + 1)), bg.col = NA, bg.border = NA,
# #   panel.fun = function(region,value, ...) {
# #     circos.genomicRect(region, value, ytop.column = 1, ybottom = 0, border = 'white', lwd = 0.02, col = '#F07673', ...)
# #     circos.lines(c(0, max(region)), c(mt_genome_cov, mt_genome_cov), col = 'red3', lwd = 0.15, lty = 2)
# #     circos.yaxis(labels.cex = 0.2, lwd = 0.1, tick.length = convert_x(0.15, 'mm'))
# #     circos.axis(h = "bottom",labels=F,major.tick = F)
# #   } )
# # 
# 
# 
# #
# circos.genomicTrackPlotRegion(
#   input_h, track.height = 0.02, stack = TRUE,track.margin=c(0,0), #bg.border = NA,
#   panel.fun = function(region, value, ...) {
#     circos.genomicRect(region, value, col = input_h$Hcolor, border = NA, ...)
#   } )
# set_track_gap(gap = 0)
# circos.genomicTrackPlotRegion(
#   input_h, track.height = 0.02, stack = TRUE,track.margin=c(0,0), #bg.border = NA,
#   panel.fun = function(region, value, ...) {
#     circos.genomicRect(region, value, col = input_h$Lcolor, border = NA, ...)
#     circos.axis(h = "bottom",labels.cex = 0.6,labels.facing = "inside",direction="inside",
#                 major.at = c(1, 2000, 4000, 6000, 8000,10000,12000,14000,16000),
#                 labels = c("0 kb", "2 kb", "4 kb", "6 kb", "8 kb", "10 kb", "12 kb", "14 kb", "16 kb"),
#                 major.tick.length = mm_y(3))
#   } )
# circos.labels(input_h$seq_id, line_col = "#B99D87",
#               x = input_h$mean,  cex = 0.4,
#               labels =c("D_loop", "TRNF", "RNR1", "TRNV", "RNR2", "TRNL1", "ND1", "TRNI", 
#                         "TRNQ", "TRNM", "ND2", "TRNW", "TRNA", "TRNN", "TRNC", "TRNY", 
#                         "COX1", "TRNS1", "TRND", "COX2", "TRNK", "ATP8-6", "COX3", "TRNG",
#                         "ND3", "TRNR", "ND4L", "ND4", "TRNH", "TRNS2", "TRNL2", "ND5", 
#                         "ND6", "TRNE", "CYTB", "TRNT", "TRNP", "D_loop"))
# ####

### 测序深度、覆盖度图例 ###
depth_legend <- Legend(
  at = 1, labels = str_c(' Depth ( average: ', mt_genome_cov, ' X )'), labels_gp = gpar(fontsize = 8),
  title = str_c('mtCN ( average: ', mtCN,' )'), title_gp = gpar(fontsize = 9),
  grid_height = unit(0.4, 'cm'), grid_width = unit(0.4, 'cm'), type = 'points', pch = NA, background = '#F07673')

# CDS & rRNA & tRNA 图例
gene_legend <- Legend(
  at = c(4, 3, 2, 1), labels = c(' OXPHOS', ' D-loop',' rRNA', ' tRNA',' intergenic'), labels_gp = gpar(fontsize = 8),
  title = '', title_gp = gpar(fontsize = 9), 
  grid_height = unit(0.4, 'cm'), grid_width = unit(0.4, 'cm'), type = 'points', pch = NA, background = c("#F8984F", "#C74647","#A4C8D9", "#329B64","grey"))

# Snv indel 数量 图例
snv_legend <- Legend(
  at = c(1, 2), labels = c(' SNV', ' INDEL'),
  labels_gp = gpar(fontsize = 8), grid_height = unit(0.5, 'cm'), grid_width = unit(0.5, 'cm'),
  type = c('points', 'points'), pch = NA, background = c('#BF1D2D', '#293890'),
  legend_gp = gpar(col = c(NA, NA), lwd = 1),
  title = 'Heteroplasmic Variants counts', title_gp = gpar(fontsize = 9))


## pch图例
pch_legend <- Legend(
  at = c(1, 2), 
  labels = c(' Upstream gene variant', ' Missense variant'," Synonymous variant"," Other variant"),
  labels_gp = gpar(fontsize = 8), 
  grid_height = unit(0.5, 'cm'), 
  grid_width = unit(0.5, 'cm'),
  type = 'points',  # 统一使用 points（如果两个都are点状图例）
  pch = c(18,20,2,3),  # 修改 pch，分别对应 SNV（2=空心三角）和 INDEL（18=实心菱形）
  background = c(NA, NA),  # 保持颜色不变
  # legend_gp = gpar(col = c('#BF1D2D', '#293890'), lwd = 1),  # Set点的边框颜色
  title = 'Heteroplasmic Variants counts', 
  title_gp = gpar(fontsize = 9)
)






# 
### 左侧统计总览（涵括了上文大部分的统计概况，例如 SNP Substitution类型统计等） ###
# stat_legend <- Legend(
#   at = 1, labels = '1', labels_gp = gpar(fontsize = 0), title_gp = gpar(fontsize = 9),
#   grid_height = unit(0, 'cm'), grid_width = unit(0, 'cm'), type = 'points', pch = NA, background = NA,
#   title = str_c('Cohort: ', cohort_name, '\nRefer size: ', '16569', ' bp\nRefer GC: ', MTgenome_GC, ' %\n\n\nTotal SNP: ', mt_snv_change_ti + mt_snv_change_tv, '\nTransitions: ', mt_snv_change_ti, '\nTransversions: ', mt_snv_change_tv, '\nTi/Tv: ', round(mt_snv_change_ti / mt_snv_change_tv, 2), '\nA>T|T>A: ', mt_snv_change_at, '\nA>G|T>C: ', mt_snv_change_ag, '\nA>C|T>G: ', mt_snv_change_ac, '\nG>A|C>T: ', mt_snv_change_ga, '\nG>T|C>A: ', mt_snv_change_gt, '\nG>C|C>G: ', mt_snv_change_gc, '\n\n\nTotal InDel: ', mt_indel_length_insert + mt_indel_length_delet, '\nInsert: ', mt_indel_length_insert, '\nDelet: ', mt_indel_length_delet))

### 最后Add图例，图例在图中的存放Position自己看着调吧 ###
y_coord <- 0.8
x_coord <- 0.87

pushViewport(viewport(x = x_coord +0.01 , y = y_coord))
grid.draw(depth_legend)
y_coord <- y_coord - 0.1
upViewport()

pushViewport(viewport(x = x_coord + 0.04, y = y_coord))
grid.draw(gene_legend)
y_coord <- y_coord - 0.19
upViewport()

pushViewport(viewport(x = x_coord + 0.04, y = y_coord - 0.0373))
grid.draw(pch_legend)
y_coord <- y_coord - 0.19
upViewport()

# pushViewport(viewport(x = 0.12, y = 0.5))
# grid.draw(snv_legend)
# upViewport()
circos.clear()



# ##Save所有数据框
# setwd("D:\\biosoft\\1000thal\\mtDNA\\R script/")
# save.image("circosAtlas数据.RData")









