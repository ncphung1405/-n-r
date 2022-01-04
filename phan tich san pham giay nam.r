# Phan tich thong ke mo ta so lieu giay nam tren lazada.

install.packages("dplyr")
install.packages("tidyverse")

library(tidyverse)
library(dplyr)

setwd("C:\\Users\\PHI HUNG\\lazada1")
dulieu <- read.csv("datagiaynam.csv",encoding = "UTF-8" , header = T, stringsAsFactors = F)


#I.lam sach du lieu.

data1 <- dulieu 
data1[data1 == "" ] <- NA  
data1[is.na(data1)]= 0

#II. Chuyen doi gia tri.  
summary(data1) 
data1$danh_gia <- as.numeric(as.factor(data1$danh_gia))
data1$danh_gia_tich_cuc <- as.numeric(as.factor(data1$danh_gia_tich_cuc))
data1$gia <- as.numeric(as.factor(data1$gia))
data1$giao_hang_dung_hen <- as.numeric(as.factor(data1$giao_hang_dung_hen))
data1$phi_van_chuyen <- as.numeric(as.factor(data1$phi_van_chuyen))
data1$phan_hoi <- as.numeric(as.factor(data1$phan_hoi))
data1$sale <- as.numeric(as.factor(data1$sale))


#III. Tinh cac chi so thong ke mo ta.
desc <- function(x)
{
  trung_binh <- mean(x)
  yeu_vi <- mode(x)       
  trung_vi <- median(x)
  phuong_sai <- var(x)      
  do_lech_chuan <- sd(x)    
  nho_nhat <- min(x)
  lon_nhat <- max(x)
    
  c(MEAN = trung_binh, MODE = yeu_vi, MEDIAN = trung_vi, VAR = phuong_sai,
    SD = do_lech_chuan, MIN = nho_nhat, MAX = lon_nhat)
} 
desc(data1$gia)

#IV. Ve cac bieu do.

#1) Dot Plots.
ggplot(data1, aes(x =giao_hang_dung_hen)) + 
  geom_dotplot(fill = "green", color = "black", binwidth = .5) +
  labs(title = "BIEU DO DOT PLOTS THE HIEN GIAO HANG DUNG HEN CUA SAN PHAM GIAY NAM", x = "giao hang dung hen")

#2) Histogram.
ggplot(data1, aes(x=phi_van_chuyen)) +
  geom_histogram(fill = "pink", color = "yellow", bins = 6) +
  labs(title = "BIEU DO HISTOGRAM THE HIEN PHI VAN CHUYEN CUA SAN PHAM GIAY NAM", x = "phi van chuyen")

#3. Pie Chart.
data2 <- data1 %>%
  count(gia) %>%
  arrange(desc(gia)) %>%
  mutate(prop = round(n * 100 / sum(n), 1),
         lab.ypos = cumsum(prop) - 0.5 * prop)  
ggplot(data2, aes(x="", y = prop, fill = gia)) +
  geom_bar(width = 2, stat = "identity", color = "black") +
  coord_polar("y", start = 0, direction = 2) +
  labs(title = "BIEU DO PIE CHART SO SANH GIA BAN CUA SAN PHAM GIAY NAM") +
  theme_void()

#4. Bar Plot.
ggplot(data1, aes(x=danh_gia, y=danh_gia_tich_cuc)) + 
  geom_bar(stat = "identity", fill = "red", color = "blue") + 
  labs(x = "danh_gia", y = "danh_gia_tich_cuc", title = "BIEU DO BAR PLOT SO SANH GIUA DANH GIA VA DANH GIA TICH CUC ")

#5) Box plots.
ggplot(data1, aes(x = sale, y = shop)) +
  geom_boxplot() + 
  labs(title = "BIEU DO BOX PLOTS THE HIEN SU GIAM GIA CUA CAC SHOP SAN PHAN GIAY NAM", x = "sale", y = "shop")







