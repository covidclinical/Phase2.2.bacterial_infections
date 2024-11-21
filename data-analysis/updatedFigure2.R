library(ggplot2)
library(dplyr)
library(ggpubr)
library(gridExtra)

load("dat_combined.RData")
unique(dat_combined$site)
dat_combined$site <- gsub("MICHIGAN", "UMICH", dat_combined$site )
dat_combined$site <- gsub("UPitt", "UPMC", dat_combined$site )
dat_combined$site <- factor(dat_combined$site, levels = c("BCH","CCHMC", "CCMC", "UMICH", "UPMC","H12O", "HCUV", "GOSH"))

### panel A
panelA <- ggplot(dat_combined) +
  geom_line(aes(x = timeToGroup, y = bacterial, colour = site, linetype = site)) +
  # Update colors for each site to match the new plot
  scale_color_manual(name = "Site", values = c(
    "BCH" = "#1F78B4",    # Blue
    "CCHMC" = "#FF7F00",  # Orange
    "CCMC" = "#33A02C",   # Green
    "UMICH" = "#E31A1C",  # Red
    "UPMC" = "#FB9A99",   # Pink
    "H12O" = "#6A3D9A",   # Purple
    "HCUV" = "#A6CEE3",   # Light Blue
    "GOSH" = "#B2DF8A"    # Light Green
  )) +
  # Define line types for each site: solid for the first four, dashed for others
  scale_linetype_manual(values = c(
    "BCH" = "solid",
    "CCHMC" = "solid",
    "CCMC" = "solid",
    "UMICH" = "solid",
    "UPMC" = "dashed",
    "H12O" = "dashed",
    "HCUV" = "dashed",
    "GOSH" = "dashed"
  )) +
  # Remove gray background
  theme_minimal() +
  labs(x = "time", y = "number of infection") +
  theme(
    legend.position = "none",
    panel.grid.major = element_line(color = "gray90"), # Light gray for major gridlines
    panel.grid.minor = element_line(color = "gray90"), # Even lighter gray for minor gridlines
    panel.grid.major.x = element_line(color = "gray90"), # Keep major gridlines on x-axis
    panel.grid.minor.x = element_line(color = "gray90"),
    panel.border = element_rect(color = "black", fill = NA, size = 0.5)
    
  )+
  guides(
    colour = guide_legend(order = 1, title = "Site"),
    linetype = guide_legend(order = 1, title = "Site")
  )
panelA

### panel B
p1= ggplot(dat_combined %>% filter(site=="BCH")) + geom_col(aes(x=timeToGroup,y=bacterial))+geom_line(aes(x=timeToGroup,y=pred))+ggtitle("BCH")+labs(x="",y="bacterial infection")+ theme_minimal() + theme(panel.border = element_rect(color = "black", fill = NA, size = 0.5))
p2= ggplot(dat_combined %>% filter(site=="UMICH")) + geom_col(aes(x=timeToGroup,y=bacterial))+geom_line(aes(x=timeToGroup,y=pred))+ggtitle("UMICH")+labs(x="",y="bacterial infection")+ theme_minimal() + theme(panel.border = element_rect(color = "black", fill = NA, size = 0.5))
p3= ggplot(dat_combined %>% filter(site=="HCUV")) + geom_col(aes(x=timeToGroup,y=bacterial))+geom_line(aes(x=timeToGroup,y=pred))+ggtitle("HCUV")+labs(x="",y="bacterial infection")+ theme_minimal() + theme(panel.border = element_rect(color = "black", fill = NA, size = 0.5))
p4= ggplot(dat_combined %>% filter(site=="H12O")) + geom_col(aes(x=timeToGroup,y=bacterial))+geom_line(aes(x=timeToGroup,y=pred))+ggtitle("H12O")+labs(x="",y="bacterial infection")+ theme_minimal() + theme(panel.border = element_rect(color = "black", fill = NA, size = 0.5))
p5= ggplot(dat_combined %>% filter(site=="CCMC")) + geom_col(aes(x=timeToGroup,y=bacterial))+geom_line(aes(x=timeToGroup,y=pred))+ggtitle("CCMC")+labs(x="",y="bacterial infection")+ theme_minimal() + theme(panel.border = element_rect(color = "black", fill = NA, size = 0.5))
p6= ggplot(dat_combined %>% filter(site=="GOSH")) + geom_col(aes(x=timeToGroup,y=bacterial))+geom_line(aes(x=timeToGroup,y=pred))+ggtitle("GOSH")+labs(x="",y="bacterial infection")+ theme_minimal() + theme(panel.border = element_rect(color = "black", fill = NA, size = 0.5))
p7= ggplot(dat_combined %>% filter(site=="UPMC")) + geom_col(aes(x=timeToGroup,y=bacterial))+geom_line(aes(x=timeToGroup,y=pred))+ggtitle("UPMC")+labs(x="",y="bacterial infection")+ theme_minimal() + theme(panel.border = element_rect(color = "black", fill = NA, size = 0.5))
p8= ggplot(dat_combined %>% filter(site=="CCHMC")) + geom_col(aes(x=timeToGroup,y=bacterial))+geom_line(aes(x=timeToGroup,y=pred))+ggtitle("CCHMC")+labs(x="",y="bacterial infection")+ theme_minimal() + theme(panel.border = element_rect(color = "black", fill = NA, size = 0.5))

panelB <- grid.arrange(p1,p8,p5,p2,p7,p4,p3,p6,ncol=4)

### panel C
gray_area <- data.frame(
  xmin = as.Date("2020-03-01"),
  xmax = as.Date("2021-03-01"),
  ymin = -Inf,
  ymax = Inf,
  fill = "Full NPI period"  # Dummy variable for legend
)

load("panelC_data.RData")
panelC_data$vline_label <- ifelse( panelC_data$date %in% as.Date(c("2020-01-01","2021-01-01","2022-09-01","2022-12-01")), "Change Point","")

panelC_data$vline_x <- panelC_data$date
for( i in 1:nrow( panelC_data)){
  if( panelC_data$vline_label[i] == ""){
    panelC_data$vline_x[i] = "1900-01-01"
  }
}


panelC <- ggplot(data = panelC_data, aes(x = date, y = pred, colour = site, linetype = site)) +
  # Add lines with updated color and style
  geom_line() +
  # Add gray region for NPI period
  geom_rect(
    data = gray_area,
    aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = fill),
    alpha = 0.2, inherit.aes = FALSE) +  # Add vertical lines with the same dotted style
  geom_vline(aes(xintercept = vline_x), color = "black", linetype = "dotted") +
  # Update line and color styles to match Panel A
  scale_color_manual(name = "Site", values = c(
    "BCH" = "#1F78B4",    # Blue
    "CCHMC" = "#FF7F00",  # Orange
    "CCMC" = "#33A02C",   # Green
    "UMICH" = "#E31A1C",  # Red
    "UPMC" = "#FB9A99",   # Pink
    "H12O" = "#6A3D9A",   # Purple
    "HCUV" = "#A6CEE3",   # Light Blue
    "GOSH" = "#B2DF8A"    # Light Green
  )) +
  scale_linetype_manual(name = "Site", values = c(
    "BCH" = "solid",
    "CCHMC" = "solid",
    "CCMC" = "solid",
    "UMICH" = "solid",
    "UPMC" = "dashed",
    "H12O" = "dashed",
    "HCUV" = "dashed",
    "GOSH" = "dashed"
  )) +
  scale_fill_manual(name = "Event", values = c("Full NPI period" = "gray")) +  # Ensure gray color
  # Guides for combined legends
  guides(
    colour = guide_legend(order = 1, title = "Site"),
    linetype = guide_legend(order = 1, title = "Site"),
    fill = guide_legend(order = 2, title = "Event"),
    linetype = guide_legend(order = 2, title = "Event")
  ) +
  # Add labels
  labs(y = "predicted bacterial infection", x = "date") +
  # Minimal theme
  theme_minimal() +
  theme(
    panel.grid.major = element_line(color = "gray80"),
    panel.grid.minor = element_line(color = "gray90"),
    panel.border = element_rect(color = "black", fill = NA, size = 0.5),
    axis.line = element_line(color = "black")
  ) 

panelC


# organize final plot
top_row = ggarrange(panelA, panelC, ncol = 2, labels = c("A", "C"))
bottom_row = ggarrange( panelB, ncol = 1, labels = c("B"))
final_plot = ggarrange(top_row, bottom_row, ncol = 1)

final_plot

# export as tiff
tiff(filename = "figure2_panelFigure.tif",
     width    = 2400, 
     height   = 1800,
     units = "px", 
     res = 300)
final_plot
dev.off()
