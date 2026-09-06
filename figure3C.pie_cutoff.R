rm(list=ls())#clear Global Environment
# library(ggsankey)
library(ggsci)
library(ggplot2)
library(dplyr)
library(tidyr)
library(patchwork)  # For compositing ggplot figures
library(ggrepel)
library(plotly)
# df <- read.table("D:\\biosoft\\1000thal\\mtDNA\\254variants.tsv", sep = "\t",header = T)
df <- read.table("D:\\biosoft\\1000thal\\mtDNA\\185variants.tsv", sep = "\t",header = T)
head(df)





bed <- c("ATP6","COX3","ND2","ND5","TRNA","TRNF","TRNK","TRNN","TRNS1","TRNW","ATP8","CYTB","ND3",
         "ND6","TRNC","TRNG","TRNL1","TRNP","TRNS2","TRNY","COX1","D_loop","ND4","RNR1","TRND","TRNH",
         "TRNL2","TRNQ","TRNT","COX2","ND1","ND4L","RNR2","TRNE","TRNI","TRNM","TRNR","TRNV")

coding_bed <- c("ATP6","COX3","ND2","ND5","ATP8","CYTB","ND3","ND6","COX1","ND4","COX2","ND1","ND4L")

nocoding_bed <- c("TRNA", "TRNV", "TRNF","TRNK","TRNN","TRNS1","TRNW","intergenic",
                  "TRNC","TRNG","TRNL1","TRNP","TRNS2","TRNY","D_loop","RNR1","TRND","TRNH","TRNR",
                  "TRNL2","TRNQ","TRNT","RNR2","TRNE","TRNI","TRNM")
rRNA_bed <- c("RNR1","RNR2")
tRNA_bed <- c("TRNF", "TRNV", "TRNL1",  "TRNI", "TRNQ", "TRNM", "TRNW", "TRNA", "TRNN", "TRNC", "TRNY", 
              "TRNS1", "TRND",  "TRNK", "TRNG", "TRNR", "TRNH", "TRNS2", "TRNL2", "TRNE", "TRNT", "TRNP")


input <- as.data.frame(df) %>%   # Cast to data.frame
  mutate(OXPHS= case_when(
    GENE %in% rRNA_bed ~ "rRNA",
    GENE %in% tRNA_bed ~ "tRNA",
    GENE %in% coding_bed ~ "OXPHS",
    GENE == "D_loop" ~ "D_loop",
    TRUE ~ "intergenic"
  )
  )
input2 <- input %>% 
  filter(OXPHS == "OXPHS")



gene_counts <- table(input$OXPHS)
INFO_counts <- table(input2$INFO)







### 将表格数据转换为适合ggplot的数据框 ###
gene_df <- as.data.frame(gene_counts)
colnames(gene_df) <- c("Category", "Count")
info_df <- as.data.frame(INFO_counts)
colnames(info_df) <- c("Category", "Count")

# Create first pie chart
library(ggrepel)

p1 <- ggplot(gene_df, aes(x = 2, y = Count, fill = Category)) +
    geom_bar(stat = "identity", width = 1) +
    coord_polar("y", start = 0) +
    geom_text_repel(  # 使用 ggrepel 防重叠
      aes(label = paste0(Count, " (", round(100*Count/sum(Count), 1), "%)")),
      position = position_stack(vjust = 0.5),
      size = 3,
      # box.padding = 0.2,  # Adjust标签间距
      # max.overlaps = Inf   # 允许without限尝试Adjust
    ) +
    scale_fill_npg() +  # Use Nature palette
  # scale_fill_manual(values = c("#F0988C","#0c84c6","#f74d4d" ,"#A1A9D0",
  #                              "#f8cb7f")) +
    xlim(0.5, 2.5) +  # Control ring size
    labs(title = "Variants by GENE") +
    theme_void()+
    theme(legend.position = "left")  # Legend on the left
p1

p2 <- ggplot(info_df, aes(x = 2, y = Count, fill = Category)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  geom_text_repel(
    aes(
      label = ifelse(Count/sum(Count) > 0.02,  # 只显示Proportion>2%的标签
                     paste0(Count, " (", round(100*Count/sum(Count), 1), "%)"), 
                     "")
      ),
    position = position_stack(vjust = 0.5),
    size = 3
  ) +
  scale_fill_npg() +  # Use Nature palette
  # scale_fill_manual(values = c("#3b6291","#943c39","#41b7ac" , "#ffbd66" ,"#779043" ,"#624c7c" , "#388498" ,
  #                              "#bf7334")) +
  xlim(0.5, 2.5) +  # Control ring size
  labs(title = "Variants by INFO (OXPHS only)") +
  theme_void() +
  theme(legend.position = "right")
p2



# Composite plot
p3 <- p1 + p2
p3
ggsave(p3 ,
  filename = "D:\\biosoft\\1000thal\\mtDNA\\figure/figure3E.pie cutoff202512.pdf",
  device = "pdf",
  width = 12,
  height = 5,
  units = "in"
)

# Composite plot

