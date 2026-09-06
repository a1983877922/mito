rm(list = ls())
library(readxl)
library(writexl)
library(openxlsx)
library(dplyr)
library(tibble)
library(ggstatsplot)
library(ggsignif)
library(ggplot2)
library(tidyverse)
library(ggpubr)
library(patchwork)
library(paletteer)
# library(ggsignif)
# library(ggpubr)
# library(rstatix)
######
dfID1020 <- read_excel("D:/biosoft/1000thal/千人/ID对应(1020+409)2023.12.22.xlsx", sheet = "1020")
dfID409 <- read_excel("D:/biosoft/1000thal/千人/ID对应(1020+409)2023.12.22.xlsx", sheet = "409")

dfmt1 <- read.table("D:/biosoft/1000thal/mtDNA/1020mtDNA_out/1020.mtCN_mean.tsv", header = T,sep = "\t")
dfmt2 <- read.table("D:/biosoft/1000thal/mtDNA/1020mtDNA_out/409.mtCN_mean.tsv", header = T,sep = "\t") 
dfmt3 <- read.table("D:/biosoft/1000thal/mtDNA/1020mtDNA_out/norm.mtCN_mean.tsv", header = T,sep = "\t") 

#
# phe_child_merge <- dfmt1 %>% left_join(dfID1020,by=c("HID")) %>% 
#   left_join(dfmt1,by=c("HID")) %>% 
#   select(-c("NUC_mean_coverage.y")) %>% 
#   rename("NUC_mean_coverage" = "NUC_mean_coverage.x")
# # #
# phe_parent_merge <- phe_parent %>% left_join(dfmt2,by=c("HID")) %>% 
#   left_join(dfmt22,by=c("HID")) %>% 
#   select(-c("NUC_mean_coverage.y")) %>% 
#   rename("NUC_mean_coverage" = "NUC_mean_coverage.x")
seven_colors <- c("#ff3b30","#ff9500","#ffcc00","#4cd964","#5ac8fa","#007aff","#5856d6")
### Scatter plot ###
# rescale 是一个函数，用于将数值重缩放到 [0, 1] 区间，以便于与 colors 向量对应
rescale <- function(x, from = range(x), to = c(0, 1)) {
  (x - from[1]) * diff(to) / diff(from) + to[1]
}
distplot1<-ggplot(data=dfmt1, aes(x=NUC_mean_coverage, y=MT_mean_coverage, color=mtCN_mean))+
  geom_point(size=3)+ 
  # scale_color_gradient(low="green", high="red")+
  scale_color_gradientn(
    colors = c("#d9fedd","green", "yellow","orange", "red"),
    # colors = c("#fbdfde","#f6bfbe", "#f29f9d","#ed7f7d", "#e95f5c"),
    values = rescale(c(0,200, 400, 600,800), from = range(0,800)),
    guide = guide_colorbar(
      title = "Thalassemia",
      labels = c(0,200, 400, 600,800),
      breaks = c(0,200, 400, 600,800),
      direction = "horizontal",
      label.theme = element_text(angle = 60, hjust = 1,size = 10),  # Rotate labels 60 deg
      title.position = "top",  # Legend title on top
      title.hjust = 0.5,  # Center legend title
      title.theme = element_text(size = 20, face = "bold")  # Legend title style
    )
  ) +
  labs(title = NULL, x = "nucDNA mean coverage", y = "mtDNA mean coverage") +
  theme_bw()+   #Remove gray background, set font size
  theme(
    legend.position = c(0.95, 0.95),
    legend.justification = c(1, 1),   # Anchor legend to top-right
    panel.border = element_blank(),
    panel.grid = element_blank(),
    legend.text = element_text(size=20),#Legend text size
    panel.background = element_rect(fill = "transparent",colour = NA),
    # panel.border = element_rect(size = 1.5),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.background = element_rect(fill = "transparent",colour = NA),
    axis.text.x = element_text(color = "black", size = 12,face = 'bold'),
    axis.title.x = element_text(size = 15,face = 'bold'),
    axis.text.y = element_text(color = "black",size = 12,face = 'bold'),
    axis.title.y = element_text(size = 15,face = 'bold'),
    axis.line = element_line(colour = "black",size=1.2)
    # axis.ticks = element_line(color="black",linewidth = 1.2)
  )+
  # scale_y_continuous(labels = function(x) paste0(x/1000, "k"))+
  scale_y_continuous(
    breaks = seq(0, 30000, by = 10000),  # Set y-axis ticks
    labels = function(x) paste0(x/1000, "k"),  # Set y-axis labels
    limits = c(0, 30000)  # Ensure y-axis range covers all tick labels
  ) +
  scale_x_continuous(limits = c(20,120),breaks = c(20,40,60,80,100,120))
distplot1

distplot1.1<-ggplot(data=dfmt1, aes(x=NUC_mean_coverage, y=MT_mean_coverage))+
  geom_point(size=3, color="#e95f5c")+ 
  labs(title = NULL, x = "nucDNA mean coverage", y = "mtDNA mean coverage") +
  theme_bw()+   #Remove gray background, set font size
  theme(
    legend.position = c(0.95, 0.95),
    legend.justification = c(1, 1),   # Anchor legend to top-right
    panel.border = element_blank(),
    panel.grid = element_blank(),
    legend.text = element_text(size=20),#Legend text size
    panel.background = element_rect(fill = "transparent",colour = NA),
    # panel.border = element_rect(size = 1.5),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.background = element_rect(fill = "transparent",colour = NA),
    axis.text.x = element_text(color = "black", size = 12,face = 'bold'),
    axis.title.x = element_text(size = 15,face = 'bold'),
    axis.text.y = element_text(color = "black",size = 12,face = 'bold'),
    axis.title.y = element_text(size = 15,face = 'bold'),
    axis.line = element_line(colour = "black",size=1.2)
    # axis.ticks = element_line(color="black",linewidth = 1.2)
  )+
  scale_y_continuous(
    breaks = seq(0, 30000, by = 10000),  # Set y-axis ticks
    labels = function(x) paste0(x/1000, "k"),  # Set y-axis labels
    limits = c(0, 30000)  # Ensure y-axis range covers all tick labels
  ) +
  scale_x_continuous(limits = c(20,120),breaks = c(20,40,60,80,100,120))
distplot1.1

distplot2<-ggplot(data=dfmt2, aes(x=NUC_mean_coverage, y=MT_mean_coverage, color=mtCN_mean))+
  geom_point(size=3)+ 
  # scale_color_gradient(low="#afeeee", high="#191970")+
  scale_color_gradientn(
    colors = c("#e4f9f9","#afeeee", "#5c7ada","#191970", "#312d50"),
    values = rescale(c(0,200, 400, 600,800), from = range(0,800)),
    guide = guide_colorbar(
      title = "Carriers",
      labels = c(0,200, 400, 600,800),
      breaks = c(0,200, 400, 600,800),
      direction = "horizontal", #Horizontal legend
      label.theme = element_text(angle = 60, hjust = 1,size = 10),  # Rotate labels 60 deg
      title.position = "top",  # Legend title on top
      title.hjust = 0.5,  # Center legend title
      title.theme = element_text(size = 20, face = "bold")  # Legend title style
    )
  ) +
  labs(title = NULL, x = "nucDNA mean coverage", y = NULL) +
  theme_bw()+   #Remove gray background, set font size
  theme(
    legend.position = c(0.95, 0.95),
    legend.justification = c(1, 1),   # Anchor legend to top-right
    panel.border = element_blank(),
    panel.grid = element_blank(),
    legend.text = element_text(size=20),#Legend text size
    panel.background = element_rect(fill = "transparent",colour = NA),
    # panel.border = element_rect(size = 1.5),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.background = element_rect(fill = "transparent",colour = NA),
    axis.text.x = element_text(color = "black", size = 12,face = 'bold'),
    axis.title.x = element_text(size = 15,face = 'bold'),
    axis.line = element_line(colour = "black",size=1.2),
    axis.line.x = element_line(colour = "black", size=1.2),  # Keep x-axis line
    axis.line.y = element_blank(),  # Remove y-axis line
    axis.ticks.y = element_blank(),  # Remove y-axis ticks
    axis.text.y = element_blank(),  # Remove y-axis labels
    axis.title.y = element_blank()  # Remove y-axis title
  )+
  # scale_y_continuous(labels = function(x) paste0(x/1000, "k"))+
  scale_y_continuous(
    breaks = seq(0, 30000, by = 10000),  # Set y-axis ticks
    labels = function(x) paste0(x/1000, "k"),  # Set y-axis labels
    limits = c(0, 30000)  # Ensure y-axis range covers all tick labels
  ) +
  scale_x_continuous(limits = c(20,120),breaks = c(20,40,60,80,100,120))

distplot2

distplot2.2<-ggplot(data=dfmt2, aes(x=NUC_mean_coverage, y=MT_mean_coverage))+
  geom_point(size=3, color="#EDBE6C")+ 
  # scale_color_gradient(low="#afeeee", high="#191970")+
  # scale_color_gradientn(
  #   colors = c("#e4f9f9","#afeeee", "#5c7ada","#191970", "#312d50"),
  #   values = rescale(c(0,200, 400, 600,800), from = range(0,800)),
  #   guide = guide_colorbar(
  #     title = "Carriers",
  #     labels = c(0,200, 400, 600,800),
  #     breaks = c(0,200, 400, 600,800),
  # direction = "horizontal", #Horizontal legend
  #     label.theme = element_text(angle = 60, hjust = 1,size = 10),  # Rotate labels 60 deg
  # title.position = "top",  # Legend title on top
  # title.hjust = 0.5,  # Center legend title
  #     title.theme = element_text(size = 20, face = "bold")  # Legend title style
  #   )
  # ) +
  labs(title = NULL, x = "nucDNA mean coverage", y = NULL) +
  theme_bw()+   #Remove gray background, set font size
  theme(
    legend.position = c(0.95, 0.95),
    legend.justification = c(1, 1),   # Anchor legend to top-right
    panel.border = element_blank(),
    panel.grid = element_blank(),
    legend.text = element_text(size=20),#Legend text size
    panel.background = element_rect(fill = "transparent",colour = NA),
    # panel.border = element_rect(size = 1.5),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.background = element_rect(fill = "transparent",colour = NA),
    axis.text.x = element_text(color = "black", size = 12,face = 'bold'),
    axis.title.x = element_text(size = 15,face = 'bold'),
    axis.line = element_line(colour = "black",size=1.2),
    axis.line.x = element_line(colour = "black", size=1.2),  # Keep x-axis line
    axis.line.y = element_blank(),  # Remove y-axis line
    axis.ticks.y = element_blank(),  # Remove y-axis ticks
    axis.text.y = element_blank(),  # Remove y-axis labels
    axis.title.y = element_blank()  # Remove y-axis title
  )+
  # scale_y_continuous(labels = function(x) paste0(x/1000, "k"))+
  scale_y_continuous(
    breaks = seq(0, 30000, by = 10000),  # Set y-axis ticks
    labels = function(x) paste0(x/1000, "k"),  # Set y-axis labels
    limits = c(0, 30000)  # Ensure y-axis range covers all tick labels
  ) +
  scale_x_continuous(limits = c(20,120),breaks = c(20,40,60,80,100,120))

distplot2.2


healthy_color <- paletteer_c("ggthemes::Blue-Green Sequential", 10)

distplot3<-ggplot(data=dfmt3, aes(x=NUC_mean_coverage, y=MT_mean_coverage, color=mtCN_mean))+
  geom_point(size=3)+ 
  # scale_color_gradient(low="#b8ffce", high="#10b53a")+ 
  scale_color_gradientn(
    colors =  c("#F6FCC8FF","#CDEDB1FF","#94D6B7FF","#41B7C4FF" ),
    values = rescale(c(0,1000,1250, 1500, 1750,2000), from = range(0,2000)),
    guide = guide_colorbar(
      title = "Healthy",
      labels = c(0,1000,1250, 1500, 1750,2000),
      breaks = c(0,1000,1250, 1500, 1750,2000),
      direction = "horizontal", #Horizontal legend
      label.theme = element_text(angle = 60, hjust = 1,size = 10),  # Rotate labels 60 deg
      title.position = "top",  # Legend title on top
      title.hjust = 0.5,  # Center legend title
      title.theme = element_text(size = 20, face = "bold")  # Legend title style
    )
  ) +
  labs(title = NULL, x = "nucDNA mean coverage", y = NULL) +
  theme_bw()+   #Remove gray background, set font size
  theme(
    legend.position = c(0.95, 0.95),
    legend.justification = c(1, 1),   # Anchor legend to top-right
    panel.border = element_blank(),
    panel.grid = element_blank(),
    legend.text = element_text(size=20),#Legend text size
    panel.background = element_rect(fill = "transparent",colour = NA),
    # panel.border = element_rect(size = 1.5),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.background = element_rect(fill = "transparent",colour = NA),
    axis.text.x = element_text(color = "black", size = 12,face = 'bold'),
    axis.title.x = element_text(size = 15,face = 'bold'),
    axis.line = element_line(colour = "black",size=1.2),
    axis.line.x = element_line(colour = "black", size=1.2),  # Keep x-axis line
    axis.line.y = element_blank(),  # Remove y-axis line
    axis.ticks.y = element_blank(),  # Remove y-axis ticks
    axis.text.y = element_blank(),  # Remove y-axis labels
    axis.title.y = element_blank()  # Remove y-axis title
  )+
  scale_y_continuous(
    breaks = seq(0, 30000, by = 10000),  # Set y-axis ticks
    labels = function(x) paste0(x/1000, "k"),  # Set y-axis labels
    limits = c(0, 30000)  # Ensure y-axis range covers all tick labels
  ) 

distplot3

distplot3.3<-ggplot(data=dfmt3, aes(x=NUC_mean_coverage, y=MT_mean_coverage))+
  geom_point(size=3, color="#4DAB8F")+ 
  # scale_color_gradient(low="#b8ffce", high="#10b53a")+ 
  # scale_color_gradientn(
  #   colors =  c("#F6FCC8FF","#CDEDB1FF","#94D6B7FF","#41B7C4FF" ),
  #   values = rescale(c(0,1000,1250, 1500, 1750,2000), from = range(0,2000)),
  #   guide = guide_colorbar(
  #     title = "Healthy",
  #     labels = c(0,1000,1250, 1500, 1750,2000),
  #     breaks = c(0,1000,1250, 1500, 1750,2000),
  # direction = "horizontal", #Horizontal legend
  #     label.theme = element_text(angle = 60, hjust = 1,size = 10),  # Rotate labels 60 deg
  # title.position = "top",  # Legend title on top
  # title.hjust = 0.5,  # Center legend title
  #     title.theme = element_text(size = 20, face = "bold")  # Legend title style
  #   )
  # ) +
  labs(title = NULL, x = "nucDNA mean coverage", y = NULL) +
  theme_bw()+   #Remove gray background, set font size
  theme(
    legend.position = c(0.95, 0.95),
    legend.justification = c(1, 1),   # Anchor legend to top-right
    panel.border = element_blank(),
    panel.grid = element_blank(),
    legend.text = element_text(size=20),#Legend text size
    panel.background = element_rect(fill = "transparent",colour = NA),
    # panel.border = element_rect(size = 1.5),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.background = element_rect(fill = "transparent",colour = NA),
    axis.text.x = element_text(color = "black", size = 12,face = 'bold'),
    axis.title.x = element_text(size = 15,face = 'bold'),
    axis.line = element_line(colour = "black",size=1.2),
    axis.line.x = element_line(colour = "black", size=1.2),  # Keep x-axis line
    axis.line.y = element_blank(),  # Remove y-axis line
    axis.ticks.y = element_blank(),  # Remove y-axis ticks
    axis.text.y = element_blank(),  # Remove y-axis labels
    axis.title.y = element_blank()  # Remove y-axis title
  )+
  scale_y_continuous(
    breaks = seq(0, 30000, by = 10000),  # Set y-axis ticks
    labels = function(x) paste0(x/1000, "k"),  # Set y-axis labels
    limits = c(0, 30000)  # Ensure y-axis range covers all tick labels
  ) 

distplot3.3


### Composite plot

merge1 <-distplot1 + distplot2 +plot_layout(axis_titles='collect', design = "AB")

merge2 <-distplot1 + distplot2 + distplot3 + plot_layout(axis_titles='collect')

merge1

merge2



merge2.2 <-distplot1.1 + distplot2.2 + distplot3.3 + plot_layout(axis_titles='collect')

merge2.2 


# merge2 <-distplot11|distplot22
# merge2
### Save figure
# ggsave(filename = "Children双变量.pdf",device = "pdf",plot = distplot1,
#        path = "D:/biosoft/1000thal/mtDNA/figure", width = 15 ,height = 15, dpi = 600,bg="transparent")
# ggsave(filename = "Parents双变量.pdf",device = "pdf",plot = distplot2,
#        path = "D:/biosoft/1000thal/mtDNA/figure", width = 15 ,height = 15, dpi = 600,bg="transparent")
# ggsave(filename = "figure4A.mtDNA vs nuclear coverage.pdf",device = "pdf",plot = merge1,
#        path = "D:/biosoft/1000thal/mtDNA/figure/", width = 15 ,height = 10, dpi = 600,bg="transparent")
# ggsave(filename = "figure4A.mtDNA vs nuclear coverage2.pdf",device = "pdf",plot = merge2,
#        path = "D:/biosoft/1000thal/mtDNA/figure/", width = 15 ,height = 10, dpi = 600,bg="transparent")

# ggsave(filename = "双Cohort双变量2.pdf",device = "pdf",plot = merge2,
#        path = "D:/biosoft/1000thal/mtDNA/mtCN_SNV", width = 15 ,height = 10, dpi = 600,bg="transparent")
# ggsave(filename = "figure1B.双Cohort双变量2.2.pdf",device = "pdf",plot = merge2.2,
#        path = "D:/biosoft/1000thal/mtDNA/figure/", width = 15 ,height = 10, dpi = 600,bg="transparent")


### Density折线图 ###
density_plot1 <- ggplot() +
  geom_density(data = dfmt1, aes(x = mtCN_mean, colour = "#4798b3"),linewidth=1.4) +
  geom_density(data = dfmt2, aes(x = mtCN_mean, colour = "#ff8c00"),linewidth=1.4) +
  labs(title = NULL, x = "mtCN", y = "density") +
  theme_bw()+   #Remove gray background, set font size
  theme(
    legend.position = c(0.95, 0.95),
    legend.justification = c(1, 1),   # Anchor legend to top-right
    panel.border = element_blank(),
    panel.grid = element_blank(),
    legend.text = element_text(size=20),#Legend text size
    panel.background = element_rect(fill = "transparent",colour = NA),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.background = element_rect(fill = "transparent",colour = NA),
    axis.text.x = element_text(color = "black", size = 12,face = 'bold'),
    axis.title.x = element_text(size = 15,face = 'bold'),
    axis.text.y = element_text(color = "black",size = 12,face = 'bold'),
    axis.title.y = element_text(size = 15,face = 'bold'),
    axis.line = element_line(colour = "black",size=1.2)
    # axis.ticks = element_line(color="black",linewidth = 1.2)
  )+
  
  guides(color = guide_legend(title = NULL))+
  scale_y_continuous(expand = c(0,0)) +
  scale_color_discrete(breaks=c("#4798b3","#ff8c00"),
                       labels=c("Thalassemia","Carriers")) 

density_plot1
# ggsave(filename = "figure4B.mtCNDensity.pdf",device = "pdf",plot = density_plot,path = "D:/biosoft/1000thal/mtDNA/figure", width = 10 ,height = 8, dpi = 600)


density_plot2 <- ggplot() +
  geom_density(data = dfmt1, aes(x = mtCN_mean, color = "Thalassemia"), linewidth=1.4) +
  geom_density(data = dfmt2, aes(x = mtCN_mean, color = "Carriers"), linewidth=1.4) +
  geom_density(data = dfmt3, aes(x = mtCN_mean, color = "Healthy"), linewidth=1.4) +
  labs(title = NULL, x = "mtCN", y = "density") +
  theme_bw() +
  theme(
    legend.position = c(0.95, 0.95),
    legend.justification = c(1, 1),
    panel.border = element_blank(),
    panel.grid = element_blank(),
    legend.text = element_text(size=20),
    panel.background = element_rect(fill = "transparent", colour = NA),
    plot.background = element_rect(fill = "transparent", colour = NA),
    axis.text.x = element_text(color = "black", size = 12, face = 'bold'),
    axis.title.x = element_text(size = 15, face = 'bold'),
    axis.text.y = element_text(color = "black", size = 12, face = 'bold'),
    axis.title.y = element_text(size = 15, face = 'bold'),
    axis.line = element_line(colour = "black", size=1.2)
  ) +
  guides(color = guide_legend(title = NULL)) +
  scale_y_continuous(expand = c(0,0)) +
  scale_color_manual(
    values = c("Thalassemia" = "#e95f5c", 
               "Carriers" = "#EDBE6C", 
               "Healthy" = "#4DAB8F")
  )

density_plot2
### Save figure
# ggsave(filename = "figure4B.mtCNDensityvshealthy.pdf",device = "pdf",plot = density_plot,path = "D:/biosoft/1000thal/mtDNA/figure", width = 10 ,height = 8, dpi = 600)
### Compare线粒体深度 ###

# Add group column
dfmt1$group <- "Thalassemia"
dfmt2$group <- "Carriers"
dfmt3$group <- "Healthy"

# Merge data
combined_data <- bind_rows(dfmt1, dfmt2, dfmt3)

combined_data$group <- factor(
  combined_data$group,
  
  levels = c(
    "Thalassemia",        # 第一级：轻度
    "Carriers",    # 第二级：中度
    "Healthy"         # 第三级：重度
  ),
  ordered = TRUE                   # Set为有序因子
)

## Compute mean and SD per group
combined_summary <- combined_data %>%
  group_by(group) %>%
  summarise(
    mean_coverage = mean(MT_mean_coverage, na.rm = TRUE),
    sd = sd(MT_mean_coverage, na.rm = TRUE),       # SD
    se = sd / sqrt(n()),                  # SEM
    .groups = "drop"
  )
# Inspect group means
aggregate(MT_mean_coverage ~ group, data = combined_data, mean)

my_comparisons <- list(c("Thalassemia", "Carriers"), c("Thalassemia", "Healthy"),c("Carriers", "Healthy"))

combined_data_sub <- combined_data %>% filter(group !="Healthy")
# p_values <- combined_data %>%
#   t_test(MT_mean_coverage ~ group, ref.group = "WT") %>%
#   adjust_pvalue(method = "BH") %>%
#   add_significance("p") %>%
#   mutate(
#     label = case_when(
#       p < 0.001 ~ "***",
#       p < 0.01 ~ "**",
#       p < 0.05 ~ "*",
#       TRUE ~ ""
#     )
#   )
vio_plot1<- ggplot(combined_data_sub, aes(x = group, y = MT_mean_coverage,fill = group))+
  geom_violin(trim = FALSE) +
  geom_boxplot(width = 0.1, outlier.shape = NA, alpha = 0.5) +
  scale_fill_manual(values = c("#e95f5c","#EDBE6C"))+ #Point colors
  labs(title="",x="",y="Average mtDNA coverage (X)") + # Add标题，x轴，y轴内容
  theme_classic(base_line_size = 0.6)+  
  theme(legend.position = "none",
        plot.margin = margin(t = 5, r = 30, b = 5, l = 30, unit = "mm"),
        plot.title = element_text(size = 15,
                                  colour = "black",
                                  hjust = 0.5),
        axis.title.y = element_text(size = 15, 
                                    # family = "myFont", 
                                    color = "black",
                                    face = "bold", 
                                    vjust = 10, 
                                    hjust = 0.5, 
                                    angle = 90),
        # legend.title = element_text(color="black", # Modify legend title
        #                             size=15, 
        #                             face="bold"),
        # legend.text = element_text(color="black", # Set legend labels
        #                            size = 10, 
        #                            face = "bold"),
        axis.text.x = element_text(size = 13,  # ModifyX轴上字体大小，
                                   color = "black", # 颜色
                                   face = "bold", #  face取值：plain普通，bold加粗，italic斜体，bold.italic斜体加粗
                                   # vjust = 0.95, # Position
                                   # hjust = 1,
                                   # angle = 0
        ), #Angle
        axis.text.y = element_text(size = 13,  
                                   color = "black",
                                   face = "bold", 
                                   vjust = 0.5, 
                                   hjust = 0.5, 
                                   angle = 0) 
  )+
  scale_y_continuous(limits = c(0, 30000), expand = c(0, 0))


vio_plot1
# ggsave(filename = "figure4B.mtDNA coverage比对.pdf",device = "pdf",plot = vio_plot1,path = "D:/biosoft/1000thal/mtDNA/figure", width = 10 ,height = 8, dpi = 600)


vio_plot2<- ggplot(combined_data, aes(x = group, y = MT_mean_coverage,fill = group))+
  geom_violin(trim = FALSE) +
  geom_boxplot(width = 0.1, outlier.shape = NA, alpha = 0.5) +
  scale_fill_manual(values = c("#e95f5c","#EDBE6C","#4DAB8F"))+ #Point colors
  labs(title="",x="",y="Average mtDNA coverage (X)") + # Add标题，x轴，y轴内容
  theme_classic(base_line_size = 0.6)+  
  theme(legend.position = "none",
        plot.margin = margin(t = 5, r = 30, b = 5, l = 30, unit = "mm"),
        plot.title = element_text(size = 15,
                                  colour = "black",
                                  hjust = 0.5),
        axis.title.y = element_text(size = 15, 
                                    # family = "myFont", 
                                    color = "black",
                                    face = "bold", 
                                    vjust = 10, 
                                    hjust = 0.5, 
                                    angle = 90),
        # legend.title = element_text(color="black", # Modify legend title
        #                             size=15, 
        #                             face="bold"),
        # legend.text = element_text(color="black", # Set legend labels
        #                            size = 10, 
        #                            face = "bold"),
        axis.text.x = element_text(size = 13,  # ModifyX轴上字体大小，
                                   color = "black", # 颜色
                                   face = "bold", #  face取值：plain普通，bold加粗，italic斜体，bold.italic斜体加粗
                                   # vjust = 0.95, # Position
                                   # hjust = 1,
                                   # angle = 0
                                   ), #Angle
        axis.text.y = element_text(size = 13,  
                                   color = "black",
                                   face = "bold", 
                                   vjust = 0.5, 
                                   hjust = 0.5, 
                                   angle = 0) 
  )+
  scale_y_continuous(limits = c(0, 30000), expand = c(0, 0))


vio_plot2
# ggsave(filename = "figure4B.mtDNA coverage比对2.pdf",device = "pdf",plot = vio_plot2,path = "D:/biosoft/1000thal/mtDNA/figure", width = 10 ,height = 8, dpi = 600)
