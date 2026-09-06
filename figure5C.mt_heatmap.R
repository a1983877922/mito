rm(list=ls())#clear Global Environment
# Load packages
library(ggplot2) # Create Elegant Data Visualisations Using the Grammar of Graphics
library(readxl)
library(tidyr)
library(dplyr)
library(scales)
library(ggsci)

#####
df1 <- read_xlsx("D:\\biosoft\\1000thal\\mtDNA\\CYTB\\MTSNV/MT14755 and MT14775 趋势.xlsx",sheet = "wilcox")
df2 <- read_xlsx("D:\\biosoft\\1000thal\\mtDNA\\CYTB\\MTSNV/MT14755 and MT14775 趋势.xlsx",sheet = "Sheet2")

### 数据预处理 ###
startrek_colors <- pal_startrek("uniform")(7)
show_col(startrek_colors)
custom_order <- c(
  "Fetal Hemoglobin",
  "Annual transfusions",
  "Survival time without transfusion",
  "Serum Iron",
  "Serum Ferritin",
  "Transferrin",
  "Soluble Transferrin Receptor",
  "Transferrin_Saturation",
  "Unsaturated_iron_binding_capacity",
  "Total_iron_binding_capacity"
)
df2 <- df2 %>% 
  mutate(phenotype = factor(phenotype, levels = rev(custom_order)))
# CreateHeatmap
p1 <- ggplot(df2, aes(x = interaction(group, mutation), y = phenotype, fill = factor(beta))) +
  geom_tile(color = "white", width = 0.9, height = 0.9) +
  geom_text(aes(label = ifelse(sig == "*", "*", 
                               ifelse(sig == "**", "**",
                                      ifelse(sig == "***", "***", "")))), 
            size = 10, vjust = 0.8) +
  scale_fill_manual(
    name = "Effect Direction",
    values = c("-1" = "#3399FF", "1" = "#cc0000"),
    labels = c("-1" = "Decrease", "1" = "Increase")
  ) +
  scale_x_discrete(labels = c("β0.MT14755" = "MT14755\n(β0/β0)",
                              "β+.MT14755" = "MT14755\n(β0/β+)",
                              "β0.MT14775" = "MT14775\n(β0/β0)",
                              "β+.MT14775" = "MT14775\n(β0/β+)")) +
  labs(
    x = "",
    y = "",
    title = "Phenotypic Effects of MT14755 and MT14775 Mutations",
    subtitle = "* p<0.05, ** p<0.01, *** p<0.001"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 0, hjust = 0.5, face = "bold"),
    axis.text.y = element_text(face = "bold"),
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5),
    panel.grid = element_blank()
  )
p1
ggsave(p1,filename ="D:\\biosoft\\1000thal\\mtDNA\\figure/figure5G.heatmap between MT14755 and MT14775.pdf",
       device = pdf,width = 12,height = 8,dpi = 600)
