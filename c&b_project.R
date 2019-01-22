
# ASSOCIATION RULES MINING AN APPLICATION ON A CAFE SALES DATA AT AIRPORT ---------------------

options(digits = 8)

pacman::p_load("RColorBrewer","dplyr","data.table","arules","arulesViz","formattable","arulesCBA",
               "lubridate","plotly","ggplot2","ggdendro","dendextend","psych","readr","ggthemes",
               "ggpubr","gridExtra","readxl")

pacman::p_isloaded("RColorBrewer","dplyr","data.table","arules","arulesViz","formattable",
                   "arulesCBA","lubridate","plotly","ggplot2","ggdendro","dendextend","psych",
                   "readr","ggthemes","ggpubr","gridExtra","readxl")

setwd("C:/Users/mehmetali.varol/Google Drive/data/Proje/bin")

# DATA LOAD -----------------------------------------------------------------------------------

cb_sales_all <- read_csv("cb_sales_2018_31102018.csv", 
                                   col_names = FALSE, locale = locale())
Cost <- read_excel("Cost.xlsx", sheet = "R")

colnames(cb_sales_all) <- c("FICHE_ID","SALES_CATEGORY_ID","SALES_CATEGORY","STCODE","STDESC",
                        "UNITS1","QTY","SALES_PRICE","DATE","FICHE_NO","WHNO","WHNAME")

# NUMBER OF EXCLUDED OBSERVATIONS -------------------------------------------------------------

nrow(cb_sales_all[which(cb_sales_all$STCODE == "30.128.0092"), ]) #PB DE FILTRE KAHVE PERSONEL
nrow(cb_sales_all[which(cb_sales_all$STCODE == "30.127.0141"), ]) #PB CAY KUCUK
nrow(cb_sales_all[which(cb_sales_all$STCODE == "30.127.0140"), ]) #PB CAY BUYUK
nrow(cb_sales_all[which(cb_sales_all$STCODE == "30.306.0073"), ]) #PB SIMIT ANKARA
nrow(cb_sales_all[which(cb_sales_all$SALES_CATEGORY_ID == "75"), ]) #Hediyelik Ticari Ürün
nrow(cb_sales_all[which(cb_sales_all$SALES_CATEGORY_ID == "77"), ]) #Kitap
nrow(cb_sales_all[which(cb_sales_all$STCODE == "30.131.0037"), ]) #OPEN FOOD
nrow(cb_sales_all[which(cb_sales_all$STCODE == "30.131.0038"), ]) #OPEN BEVERAGE
nrow(cb_sales_all[which(cb_sales_all$STCODE == "30.131.0213"), ]) #OPEN MERCHANDISE VAT 18 %
nrow(cb_sales_all[which(cb_sales_all$STCODE == "30.131.0214"), ]) #OPEN MERCHANDISE VAT 18 %
nrow(cb_sales_all[which(cb_sales_all$STCODE == "30.133.0002"), ]) #OPEN MERCHANDISE VAT 18 %
nrow(cb_sales_all[which(cb_sales_all$STCODE == "30.105.0168"), ]) #TAZE DELAY KASAR BAGET
nrow(cb_sales_all[which(cb_sales_all$STCODE == "30.105.0169"), ]) #TAZE DELAY KARISIK BAGET
nrow(cb_sales_all[which(cb_sales_all$STCODE == "30.131.0200"), ]) #MUHTELİF GIDA

# CLEAN THE REDUNDANT OBSERVATIONS  -----------------------------------------------------------

cb_sales_all <- cb_sales_all[cb_sales_all$STCODE %like% "^30.", ]
cb_sales_all <- cb_sales_all[-which(cb_sales_all$STCODE == "30.128.0092"), ]
cb_sales_all <- cb_sales_all[-which(cb_sales_all$STCODE == "30.127.0141"), ]
cb_sales_all <- cb_sales_all[-which(cb_sales_all$STCODE == "30.127.0140"), ]
cb_sales_all <- cb_sales_all[-which(cb_sales_all$STCODE == "30.306.0073"), ]
cb_sales_all <- cb_sales_all[-which(cb_sales_all$SALES_CATEGORY_ID == "75"), ]
cb_sales_all <- cb_sales_all[-which(cb_sales_all$SALES_CATEGORY_ID == "77"), ]
cb_sales_all <- cb_sales_all[-which(cb_sales_all$STCODE == "30.131.0037"), ]
cb_sales_all <- cb_sales_all[-which(cb_sales_all$STCODE == "30.131.0038"), ]
cb_sales_all <- cb_sales_all[-which(cb_sales_all$STCODE == "30.131.0213"), ]
cb_sales_all <- cb_sales_all[-which(cb_sales_all$STCODE == "30.131.0214"), ]
cb_sales_all <- cb_sales_all[-which(cb_sales_all$STCODE == "30.133.0002"), ]
cb_sales_all <- cb_sales_all[-which(cb_sales_all$STCODE == "30.105.0168"), ]
cb_sales_all <- cb_sales_all[-which(cb_sales_all$STCODE == "30.105.0169"), ]
cb_sales_all <- cb_sales_all[-which(cb_sales_all$STCODE == "30.131.0200"), ]


# CREATE NEW ATTRIBUTES -----------------------------------------------------------------------

cb_sales_all <- mutate(cb_sales_all, DESC= paste(substr(cb_sales_all$STCODE,4,11),
                                         gsub(" ","_",cb_sales_all$STDESC),sep="-"))

cb_sales_all <- mutate(cb_sales_all, TERMINAL= gsub("ADM_Dom_Cakes&Bakes","ADM",
                                                    cb_sales_all$WHNAME))
cb_sales_all <- mutate(cb_sales_all, TERMINAL= gsub("AHL_Dom_Cakes_ Bakes_Dep_Air","AHL",
                                                    cb_sales_all$TERMINAL))
cb_sales_all <- mutate(cb_sales_all, TERMINAL= gsub("ESB_Dom_Cakes&Bakes_Dep_Air","ESB",
                                                    cb_sales_all$TERMINAL))

cb_sales_all <- mutate(cb_sales_all, MONTH=month(cb_sales_all$DATE))
cb_sales_all <- mutate(cb_sales_all, MONTHS= gsub("^1$","JAN",cb_sales_all$MONTH))
cb_sales_all <- mutate(cb_sales_all, MONTHS= gsub("^2$","FEB",cb_sales_all$MONTHS))
cb_sales_all <- mutate(cb_sales_all, MONTHS= gsub("^3$","MAR",cb_sales_all$MONTHS))
cb_sales_all <- mutate(cb_sales_all, MONTHS= gsub("^4$","APR",cb_sales_all$MONTHS))
cb_sales_all <- mutate(cb_sales_all, MONTHS= gsub("^5$","MAY",cb_sales_all$MONTHS))
cb_sales_all <- mutate(cb_sales_all, MONTHS= gsub("^6$","JUN",cb_sales_all$MONTHS))
cb_sales_all <- mutate(cb_sales_all, MONTHS= gsub("^7$","JUL",cb_sales_all$MONTHS))
cb_sales_all <- mutate(cb_sales_all, MONTHS= gsub("^8$","AUG",cb_sales_all$MONTHS))
cb_sales_all <- mutate(cb_sales_all, MONTHS= gsub("^9$","SEP",cb_sales_all$MONTHS))
cb_sales_all <- mutate(cb_sales_all, MONTHS= gsub("^10$","OCT",cb_sales_all$MONTHS))

cb_sales_all <- mutate(cb_sales_all, CATEGORY= gsub("^1$","Kahvalti Grubu",
                                                    cb_sales_all$SALES_CATEGORY_ID))
cb_sales_all <- mutate(cb_sales_all, CATEGORY= gsub("^2$","Corbalar",cb_sales_all$CATEGORY))
cb_sales_all <- mutate(cb_sales_all, CATEGORY= gsub("^3$","Salatalar",cb_sales_all$CATEGORY))
cb_sales_all <- mutate(cb_sales_all, CATEGORY= gsub("^5$","Sandviçler",cb_sales_all$CATEGORY))
cb_sales_all <- mutate(cb_sales_all, CATEGORY= gsub("^7$","Makarna ve Pilav Grubu",
                                                    cb_sales_all$CATEGORY))
cb_sales_all <- mutate(cb_sales_all, CATEGORY= gsub("^8$","Pide Grubu",cb_sales_all$CATEGORY))
cb_sales_all <- mutate(cb_sales_all, CATEGORY= gsub("^10$","Pizza Grubu",cb_sales_all$CATEGORY))
cb_sales_all <- mutate(cb_sales_all, CATEGORY= gsub("^19$","Unlu Mamüller",cb_sales_all$CATEGORY))
cb_sales_all <- mutate(cb_sales_all, CATEGORY= gsub("^20$","Yas Pastalar",cb_sales_all$CATEGORY))
cb_sales_all <- mutate(cb_sales_all, CATEGORY= gsub("^23$","Meyveli Tatlilar",
                                                    cb_sales_all$CATEGORY))
cb_sales_all <- mutate(cb_sales_all, CATEGORY= gsub("^25$","Yiyecek Grubu_Diger",
                                                    cb_sales_all$CATEGORY))
cb_sales_all <- mutate(cb_sales_all, CATEGORY= gsub("^26$","Açik Içecekler",cb_sales_all$CATEGORY))
cb_sales_all <- mutate(cb_sales_all, CATEGORY= gsub("^27$","Çay Grubu",cb_sales_all$CATEGORY))
cb_sales_all <- mutate(cb_sales_all, CATEGORY= gsub("^28$","Kahve Grubu",cb_sales_all$CATEGORY))
cb_sales_all <- mutate(cb_sales_all, CATEGORY= gsub("^30$","Dondurmalı Ürünler",
                                                    cb_sales_all$CATEGORY))
cb_sales_all <- mutate(cb_sales_all, CATEGORY= gsub("^31$","Büfeler",cb_sales_all$CATEGORY))
cb_sales_all <- mutate(cb_sales_all, CATEGORY= gsub("^33$","Su ve Maden Sulari",
                                                    cb_sales_all$CATEGORY))
cb_sales_all <- mutate(cb_sales_all, CATEGORY= gsub("^34$","Kutulu Içecekler",
                                                    cb_sales_all$CATEGORY))
cb_sales_all <- mutate(cb_sales_all, CATEGORY= gsub("^35$","Siseli Içecekler",
                                                    cb_sales_all$CATEGORY))
cb_sales_all <- mutate(cb_sales_all, CATEGORY= gsub("^39$","Enerji Içecekleri",
                                                    cb_sales_all$CATEGORY))
cb_sales_all <- mutate(cb_sales_all, CATEGORY= gsub("^40$","Diger Alkolsüz Içecekler",
                                                    cb_sales_all$CATEGORY))
cb_sales_all <- mutate(cb_sales_all, CATEGORY= gsub("^41$","Bira Grubu",cb_sales_all$CATEGORY))
cb_sales_all <- mutate(cb_sales_all, CATEGORY= gsub("^54$","Kirmizi Sarap",cb_sales_all$CATEGORY))
cb_sales_all <- mutate(cb_sales_all, CATEGORY= gsub("^55$","Beyaz Sarap",cb_sales_all$CATEGORY))
cb_sales_all <- mutate(cb_sales_all, CATEGORY= gsub("^56$","Pembe Sarap",cb_sales_all$CATEGORY))
cb_sales_all <- mutate(cb_sales_all, CATEGORY= gsub("^59$","Çikolatalar",cb_sales_all$CATEGORY))
cb_sales_all <- mutate(cb_sales_all, CATEGORY= gsub("^61$","Sekerler",cb_sales_all$CATEGORY))
cb_sales_all <- mutate(cb_sales_all, CATEGORY= gsub("^64$","Hazir Unlu Mamül ve Pastalar",
                                                    cb_sales_all$CATEGORY))
cb_sales_all <- mutate(cb_sales_all, CATEGORY= gsub("^67$","Paketli Dondurmalar",
                                                    cb_sales_all$CATEGORY))
cb_sales_all <- mutate(cb_sales_all, CATEGORY= gsub("^72$","Hediyelik Ticari Yiyecek",
                                                    cb_sales_all$CATEGORY))
cb_sales_all <- mutate(cb_sales_all, CATEGORY= gsub("^84$","Hediyelik Ticari Yiyecek II",
                                                    cb_sales_all$CATEGORY))

cb_sales_all <- mutate(cb_sales_all, ID= paste(cb_sales_all$FICHE_ID,cb_sales_all$FICHE_NO,
                                               cb_sales_all$WHNO,sep=""))
cb_sales_all <- mutate(cb_sales_all, DAY=weekdays(as.Date(cb_sales_all$DATE)))
cb_sales_all <- mutate(cb_sales_all, UN = paste(cb_sales_all$DESC,
                                                cb_sales_all$SALES_CATEGORY_ID,sep="-"))
cb_sales_all <- mutate(cb_sales_all, AMOUNT = 
                         (as.numeric(cb_sales_all$QTY) * cb_sales_all$SALES_PRICE))

cb_sales_all <- merge(cb_sales_all,Cost, "STCODE",all.x = TRUE)
cb_sales_all <- mutate(cb_sales_all, PROFIT = 
                     as.numeric(cb_sales_all$AMOUNT -(cb_sales_all$QTY*cb_sales_all$`COST PRICE`)))

# CLEAN THE ROWS WHICH HAS GOT UNIQUE ID ------------------------------------------------------

fiche <- cb_sales_all%>%
  group_by(ID)%>%
  summarise(ROW = n(),QTY=sum(QTY))

discard<-fiche[which(fiche$ROW == 1),]
discard<-discard[which(discard$QTY==1),]          
cb_sales_poly <- cb_sales_all[-which(cb_sales_all$ID %in% discard$ID),]
cb_sales_single <- cb_sales_all[which(cb_sales_all$ID %in% discard$ID),]

# SAVE THE FILES ------------------------------------------------------------------------------

saveRDS(cb_sales_all, "cb_sales_all_2018.rds")
saveRDS(cb_sales_poly, "cb_sales_poly_2018.rds")
saveRDS(cb_sales_single, "cb_sales_single_2018.rds")

# LEVELS --------------------------------------------------------------------------------------

cb_sales_poly$TERMINAL<-factor(cb_sales_poly$TERMINAL,levels=c("AHL","ESB","ADM"))
cb_sales_poly$MONTHS<-factor(cb_sales_poly$MONTHS,levels=c("JAN","FEB","MAR","APR","MAY","JUN",
                                                           "JUL","AUG","SEP","OCT"))
cb_sales_poly$DAY<-factor(cb_sales_poly$DAY,levels=c("Monday","Tuesday","Wednesday","Thursday",
                                                     "Friday","Saturday","Sunday"))

# FREQUENCY TABLES ----------------------------------------------------------------------------

options(digits = 4)

# Table 3.2 Terminal
statterminal <- cb_sales_poly%>%
  group_by(TERMINAL)%>%
  summarise(AMOUNT = sum(AMOUNT),QTY = sum(as.numeric(QTY)),ROW = n(),AVG= (AMOUNT/QTY))
statterminal_format <- formattable(head(statterminal,10),list(AMOUNT=color_bar("lightblue"),
                                                              QTY=color_bar("lightblue"),
                                                              ROW=color_bar("lightblue"),
                                                              AVG=color_bar("lightblue")))
statterminal_format

#Table 3.3 Months
statmonth <- cb_sales_poly%>%
  group_by(MONTHS)%>%
  summarise(AMOUNT = sum(AMOUNT),QTY = sum(as.numeric(QTY)),ROW = n(),AVG= (AMOUNT/QTY))
statmonth_format <- formattable(head(statmonth,10),list(AMOUNT=color_bar("lightblue"),
                                                        QTY=color_bar("lightblue"),
                                                        ROW=color_bar("lightblue"),
                                                        AVG=color_bar("lightblue")))
statmonth_format

#Table 3.4 Day
statday <- cb_sales_poly%>%
  group_by(DAY)%>%
  summarise(AMOUNT = sum(AMOUNT),QTY = sum(as.numeric(QTY)),ROW = n(),AVG= (AMOUNT/QTY))
statday_format <- formattable(head(statday,10),list(AMOUNT=color_bar("lightblue"),
                                                    QTY=color_bar("lightblue"),
                                                    ROW=color_bar("lightblue"),
                                                    AVG=color_bar("lightblue")))
statday_format

# Table 3.5 Top ten sales category
statcat <- cb_sales_poly%>%
  group_by(CATEGORY)%>%
  summarise(AMOUNT = sum(AMOUNT),QTY = sum(as.numeric(QTY)),ROW = n(),AVG= (AMOUNT/QTY))
statcat <- statcat[order(statcat$AMOUNT,decreasing = TRUE),]
statcat_format <- formattable(head(statcat,10),list(AMOUNT=color_bar("lightblue"),
                                                    QTY=color_bar("lightblue"),
                                                    ROW=color_bar("lightblue"),
                                                    AVG=color_bar("lightblue")))
statcat_format

# Table 3.6 Top ten sales items
statdesc <- cb_sales_poly%>%
  group_by(STDESC)%>%
  summarise(AMOUNT = sum(AMOUNT),QTY = sum(as.numeric(QTY)),ROW = n(),AVG= (AMOUNT/QTY))
statdesc <- statdesc[order(statdesc$AMOUNT,decreasing = TRUE),]
statdesc_format <- formattable(head(statdesc,10),list(AMOUNT=color_bar("lightblue"),
                                                      QTY=color_bar("lightblue"),
                                                      ROW=color_bar("lightblue"),
                                                      AVG=color_bar("lightblue")))
statdesc_format


# GRAPHICS ------------------------------------------------------------------------------------
coul = brewer.pal(9, "Blues") 
coul <- colorRampPalette(coul)(11)
coul <- coul[2:11]
fix_strips <- theme(strip.text.y = element_text(angle = 0, hjust = 0, vjust = 0.1, size = 14),
                    strip.background = element_blank(),legend.position = "bottom")
cb_fill <- scale_fill_brewer("TERMINAL", palette = "Blues")

#Figure 3.1 Terminal amount & qty
bar_terminal_amount <- ggplot(cb_sales_poly, aes(x = TERMINAL, y = AMOUNT,fill = TERMINAL))
p1<-bar_terminal_amount + stat_summary(fun.y = sum, geom = "bar")+cb_fill+theme_tufte()+fix_strips+
  theme(axis.title.x=element_blank(),axis.text.x=element_blank(),axis.ticks.x=element_blank())+
  scale_y_continuous(labels = comma)+ggtitle("TERMINALS BY SALES AMOUNT")

bar_terminal_qty <- ggplot(cb_sales_poly, aes(x = TERMINAL, y = QTY,fill = TERMINAL))
p2<-bar_terminal_qty + stat_summary(fun.y = sum, geom = "bar") + cb_fill + theme_tufte()+fix_strips+
  theme(axis.title.x=element_blank(),axis.text.x=element_blank(),axis.ticks.x=element_blank())+
  scale_y_continuous(labels = comma)+ggtitle("TERMINALS BY SALES QUANTITY")

grid.arrange(p1,p2,nrow = 1)

# Figure 3.2 Month vs amount&qty facet terminal
bar_month_amount <- ggplot(cb_sales_poly, aes(x = MONTHS, y = AMOUNT, fill = MONTHS))
p3<-bar_month_amount + stat_summary(fun.y = sum,geom = "bar") + scale_fill_manual(values = coul)+
  theme_tufte()+fix_strips+theme(axis.title.x=element_blank(),axis.text.x=element_blank(),
                                 axis.ticks.x=element_blank())+scale_y_continuous(labels = comma)+
  facet_wrap(.~TERMINAL,strip.position = "bottom")+ggtitle("SALES AMOUNT BY MONTHS")

bar_month_qty <- ggplot(cb_sales_poly, aes(x = MONTHS, y = QTY, fill = MONTHS))
p4<-bar_month_qty + stat_summary(fun.y = sum,geom = "bar")+scale_fill_manual(values = coul) +
  theme_tufte() + fix_strips+theme(axis.title.x=element_blank(),axis.text.x=element_blank(),
                                   axis.ticks.x=element_blank())+scale_y_continuous(labels = comma)+
  facet_wrap(.~TERMINAL,strip.position = "bottom")+ggtitle("SALES QUANTITY BY MONTHS")

grid.arrange(p3,p4,nrow = 1)

# Figure 3.3 Day vs Amount&qty facet terminal
bar_day_amount <- ggplot(cb_sales_poly, aes(x = DAY, y = AMOUNT, fill = DAY))
p5<-bar_day_amount + stat_summary(fun.y = sum,geom = "bar")+scale_fill_manual(values = coul) + 
  theme_tufte() + fix_strips+  theme(axis.title.x=element_blank(),axis.text.x=element_blank(),
                                     axis.ticks.x=element_blank())+
  scale_y_continuous(labels = comma)+facet_wrap(.~TERMINAL,strip.position = "bottom")+
  ggtitle("SALES AMOUNT BY DAYS")

bar_day_qty <- ggplot(cb_sales_poly, aes(x = DAY, y = QTY, fill = DAY))
p6<-bar_day_qty + stat_summary(fun.y = sum,geom = "bar")+scale_fill_manual(values = coul) + 
  theme_tufte() + fix_strips+ theme(axis.title.x=element_blank(),axis.text.x=element_blank(),
                                    axis.ticks.x=element_blank())+scale_y_continuous(labels= comma)+
  facet_wrap(.~TERMINAL,strip.position = "bottom")+ggtitle("SALES QUANTITY BY DAYS")

grid.arrange(p5,p6,nrow = 1)

# Figure 3.4 Terminal vs amount and quantity facet category
cb_sales_poly$CATEGORY<-factor(cb_sales_poly$CATEGORY,levels=c(unique(statcat$CATEGORY)))

statcat_term <- cb_sales_poly%>%
  group_by(SALES_CATEGORY_ID,CATEGORY,TERMINAL)%>%
  summarise(AMOUNT = sum(AMOUNT),QTY = sum(as.numeric(QTY)),ROW = n(),AVG= (AMOUNT/QTY))

statcat_term_10 <- statcat_term[which(statcat_term$SALES_CATEGORY_ID == 5), ]
statcat_term_10 <- bind_rows(statcat_term_10,(statcat_term
                                              [which(statcat_term$SALES_CATEGORY_ID == 27), ]))
statcat_term_10 <- bind_rows(statcat_term_10,(statcat_term
                                              [which(statcat_term$SALES_CATEGORY_ID == 19), ]))
statcat_term_10 <- bind_rows(statcat_term_10,(statcat_term
                                              [which(statcat_term$SALES_CATEGORY_ID == 28), ]))
statcat_term_10 <- bind_rows(statcat_term_10,(statcat_term
                                              [which(statcat_term$SALES_CATEGORY_ID == 35), ]))
statcat_term_10 <- bind_rows(statcat_term_10,(statcat_term
                                              [which(statcat_term$SALES_CATEGORY_ID == 20), ]))
statcat_term_10 <- bind_rows(statcat_term_10,(statcat_term
                                              [which(statcat_term$SALES_CATEGORY_ID == 41), ]))
statcat_term_10 <- bind_rows(statcat_term_10,(statcat_term
                                              [which(statcat_term$SALES_CATEGORY_ID == 64), ]))
statcat_term_10 <- bind_rows(statcat_term_10,(statcat_term
                                              [which(statcat_term$SALES_CATEGORY_ID == 1), ]))
statcat_term_10 <- bind_rows(statcat_term_10,(statcat_term
                                              [which(statcat_term$SALES_CATEGORY_ID == 33), ]))
View(statcat_term_10)

bar_statcat_term_10_amount <- ggplot(statcat_term_10, aes(x = TERMINAL, y = AMOUNT, fill = TERMINAL))
p7<-bar_statcat_term_10_amount + stat_summary(fun.y = sum,geom = "bar")+
  scale_fill_brewer("TERMINAL", palette = "Blues") + theme_tufte() + fix_strips+
  theme(axis.title.x=element_blank(),axis.text.x=element_blank(),axis.ticks.x=element_blank())+
  scale_y_continuous(labels = comma)+facet_wrap(.~ CATEGORY,nrow = 2,strip.position = "bottom")+
  ggtitle("TOP 10 SALES CATEGORY BY AMOUNT IN TERMINALS")

bar_statcat_term_10_qty <- ggplot(statcat_term_10, aes(x = TERMINAL, y = QTY, fill = TERMINAL))
p8<-bar_statcat_term_10_qty + stat_summary(fun.y = sum,geom = "bar")+
  scale_fill_brewer("TERMINAL", palette = "Blues") + theme_tufte() + fix_strips+
  theme(axis.title.x=element_blank(),axis.text.x=element_blank(),axis.ticks.x=element_blank())+
  scale_y_continuous(labels = comma)+facet_wrap(.~ CATEGORY,nrow = 2,strip.position = "bottom")+
  ggtitle("TOP 10 SALES CATEGORY BY QUANTITY IN TERMINALS")

grid.arrange(p7,p8,nrow=2)

# Figure 3.5 Terminal vs amount facet products
statdesc_term <- cb_sales_poly%>%
  group_by(STCODE,STDESC,TERMINAL,DAY,MONTHS)%>%
  summarise(AMOUNT = sum(AMOUNT),QTY = sum(as.numeric(QTY)),ROW = n(),AVG= (AMOUNT/QTY))

statdesc_term_10 <- statdesc_term[which(statdesc_term$STCODE  == "30.127.0138"), ]

statdesc_term_10 <- bind_rows(statdesc_term_10,(statdesc_term
                                                [which(statdesc_term$STCODE == "30.127.0137"), ]))
statdesc_term_10 <- bind_rows(statdesc_term_10,(statdesc_term
                                                [which(statdesc_term$STCODE == "30.105.0661"), ]))
statdesc_term_10 <- bind_rows(statdesc_term_10,(statdesc_term
                                                [which(statdesc_term$STCODE == "30.201.0005"), ]))
statdesc_term_10 <- bind_rows(statdesc_term_10,(statdesc_term
                                                [which(statdesc_term$STCODE == "30.105.0612"), ]))
statdesc_term_10 <- bind_rows(statdesc_term_10,(statdesc_term
                                                [which(statdesc_term$STCODE == "30.105.0297"), ]))
statdesc_term_10 <- bind_rows(statdesc_term_10,(statdesc_term
                                                [which(statdesc_term$STCODE == "30.105.0506"), ]))
statdesc_term_10 <- bind_rows(statdesc_term_10,(statdesc_term
                                                [which(statdesc_term$STCODE == "30.119.0318"), ]))
statdesc_term_10 <- bind_rows(statdesc_term_10,(statdesc_term
                                                [which(statdesc_term$STCODE == "30.105.0298"), ]))
statdesc_term_10 <- bind_rows(statdesc_term_10,(statdesc_term
                                                [which(statdesc_term$STCODE == "30.105.0450"), ]))

statdesc_term_10$STCODE<-factor(statdesc_term_10$STCODE,
                                levels= c("30.127.0138","30.127.0137","30.105.0661","30.201.0005",
                                          "30.105.0612","30.105.0297","30.105.0506","30.119.0318",
                                          "30.105.0298","30.105.0450"))

stdesc_names <-    c("30.127.0138"="CAY KUCUK",
                     "30.127.0137"="CAY BUYUK",
                     "30.105.0661"="SANDVIC KABURGALI PEYNIRLI",
                     "30.201.0005"="SU PET SISE",
                     "30.105.0612"="EZINE PEYNIRLI ACMA",
                     "30.105.0297"="TOST KASARLI CHEDDARLI",
                     "30.105.0506"="SANDVIC ACMA DIL PEYNIRLI",
                     "30.119.0318"="SIMIT PASTANE ",
                     "30.105.0298"="TOST KASAR CHEDDAR SUCUK",
                     "30.105.0450"="SANDVIC TAVUKLU UCGEN")

bar_statdesc_term_10_amount <- ggplot(statdesc_term_10, aes(x = TERMINAL, 
                                                            y = AMOUNT, fill = TERMINAL))
p9<-bar_statdesc_term_10_amount + stat_summary(fun.y = sum,geom = "bar")+
  scale_fill_brewer("TERMINAL", palette = "Blues") + theme_tufte() + fix_strips+
  theme(axis.title.x=element_blank(),axis.text.x=element_blank(),axis.ticks.x=element_blank())+
  scale_y_continuous(labels = comma)+facet_wrap(.~STCODE,labeller = as_labeller(stdesc_names),
                                                nrow = 2,strip.position = "bottom")+
  ggtitle("TOP 10 SALES PRODUCTS BY AMOUNT IN TERMINALS")

bar_statdesc_term_10_qty <- ggplot(statdesc_term_10, aes(x = TERMINAL, y = QTY, fill = TERMINAL))
p10<-bar_statdesc_term_10_qty + stat_summary(fun.y = sum,geom = "bar")+
  scale_fill_brewer("TERMINAL", palette = "Blues") + theme_tufte() + fix_strips+
  theme(axis.title.x=element_blank(),axis.text.x=element_blank(),axis.ticks.x=element_blank())+
  scale_y_continuous(labels = comma)+facet_wrap(.~STCODE,
                                                labeller = as_labeller(stdesc_names),nrow = 2,
                                                strip.position = "bottom")+
  ggtitle("TOP 10 SALES PRODUCTS BY QUANTITY IN TERMINALS")

grid.arrange(p9,p10,nrow=2)


# APRIORI ALGORITHM ON PRODUCTS ---------------------------------------------------------------

# TRANSACTIONS
cb_tran_2018 <- paste(cb_sales_poly$ID,cb_sales_poly$UN, sep="\n")
write(cb_tran_2018, file = "cb_transactions_2018")
cb_2018 <- read.transactions("cb_transactions_2018", format = "single",cols = c(1,2))

# ECLAT
itemset_desc <- eclat (cb_2018, parameter = list(supp = 0.01, minlen = 2)) 
inspect(head(sort(itemset_desc,by = "support"),25))

# TOP 10 FREQUENT ITEMS
itemFrequencyPlot(cb_2018,topN=10,col=brewer.pal(3,"Blues"),
                  main='Top 10 Products',
                  type="relative",
                  ylab="Item Frequency(Relative)",cex  = 1)

# APRIORI
rules_desc <- apriori (cb_2018, parameter = list(supp = 0.001,conf = 0.05, minlen=2))
sortrules_desc <- sort (rules_desc, by="support", decreasing=TRUE)

# CLEAR REDUNDANT RULES AND VISUALISING
redrules_desc <- sortrules_desc[!is.redundant(sortrules_desc)]
signrules_desc<-redrules_desc[is.significant(redrules_desc,cb_2018,
                                             method = "fisher",
                                             alpha = .01,
                                             adjust = "bonferroni")]
summary(signrules_desc)
signrules_desc<-sort(sortrules_desc,by="support", decreasing=TRUE)
inspect(head(signrules_desc,20))

plot(signrules_desc, method = "graph", control = list(verbose = TRUE), engine = "html")

# Figure 5.5 Rules of product scatter plot
plot(signrules_desc, jitter=10,engine = "plotly")

#127.0138-CAY_KUCUK-27
rules_cay <- apriori (data=cb_2018, parameter=list (supp=0.001,conf = 0.05), appearance = 
                         list(rhs = "127.0138-CAY_KUCUK-27"), control = list (verbose=F))
rules_cay <- sort (rules_cay, by="confidence", decreasing=TRUE) 

redrules_simit <- rules_simit[!is.redundant(rules_simit)]
signrules_simit <-redrules_simit[is.significant(redrules_simit,cb_2018,
                                                method = "fisher",
                                                alpha = .01,
                                                adjust = "bonferroni")]
plot(head(sort(rules_cay,by="support",decreasing = TRUE),20), method = "graph", engine = "html")

options(digits = 2)
inspect(head(sort(rules_cay,by="support",decreasing = TRUE),20))

#105.0660-SANDVIC_PEYNIRLI_MINI_ESMER_EKMEKTE-5
rules_sand <- apriori (data=cb_2018, parameter=list (supp=0.00001,conf = 0.05), 
                       appearance = list(lhs = "105.0660-SANDVIC_PEYNIRLI_MINI_ESMER_EKMEKTE-5"), 
                       control = list (verbose=F))
rules_sand <- sort (rules_sand, by="confidence", decreasing=TRUE) 
length(rules_sand)
redrules_sand <- rules_sand[!is.redundant(rules_sand)]
signrules_sand <-redrules_sand[is.significant(redrules_sand,cb_2018,
                                              method = "fisher",
                                              alpha = .01,
                                              adjust = "bonferroni")]
plot(head(sort(signrules_sand,by="support",decreasing = TRUE),99), 
     method = "graph", 
     engine = "html")

# APRIORI ALGORITHM ON CATEGORIES ---------------------------------------------------------------
# TRANSACTIONS
cb_cat_2018 <- paste(cb_sales_poly$ID,cb_sales_poly$CATEGORY, sep="\n")
write(cb_cat_2018, file = "cb_cat_2018")
cat_2018 <- read.transactions("cb_cat_2018", format = "single",cols = c(1,2))

# ECLAT
itemset_cat <- eclat (cat_2018, parameter = list(supp = 0.01, minlen = 2)) 
inspect(head(sort(itemset_cat,by = "support"),25))

# Figure 5.10 Top ten sales categories
itemFrequencyPlot(cat_2018,topN=10,col=brewer.pal(3,"Blues"),
                  main='Top 10 Sales Categories',
                  type="relative",
                  ylab="Item Frequency(Relative)",cex  = 0.8)

# APRIORI
rules_cat <- apriori (cat_2018, parameter = list(supp = 0.001, conf = 0.05,minlen=2))
sortrules_cat <- sort (rules_cat, by="support", decreasing=TRUE)

# CLEAR REDUNDANT RULES AND VISUALISING
redrules_cat <- sortrules_cat[!is.redundant(sortrules_cat)]
signrules_cat<-redrules_cat[is.significant(redrules_cat,cb_2018,
                                           method = "fisher",
                                           alpha = .01,
                                           adjust = "bonferroni")]
summary(signrules_cat)
inspect(head(signrules_cat,20))

# Figure 5.12 Rules of categories
plot(signrules_cat, method = "graph", control = list(verbose = TRUE), engine = "html")

# Figure 5.13 Rules of categories scatter plot
plot(signrules_cat, jitter=10,engine = "plotly")

#ÇAY => SANDVİÇ GRUBU
# Figure 5.14 Çay-Sandviç Rules
cay <- cb_sales_poly[which(cb_sales_poly$SALES_CATEGORY_ID == 27), ]
cay <- cay$UN
cay <- levels(as.factor(cay))

sandvic_grubu <- cb_sales_poly[which(cb_sales_poly$SALES_CATEGORY_ID == 5), ]
sandvic_grubu <- sandvic_grubu$UN
sandvic_grubu <- levels(as.factor(sandvic_grubu))

rules_cs <- apriori (data=cb_2018, parameter=list (supp=0.0001,conf = 0.05), 
                     appearance = list(lhs = cay,rhs = sandvic_grubu), control = list (verbose=F))
rules_cs <- sort (rules_cs, by="confidence", decreasing=TRUE) 
length(rules_cs)
redrules_cs <- rules_cs[!is.redundant(rules_cs)]
signrules_cs <-redrules_cs[is.significant(redrules_cs,cb_2018,
                                          method = "fisher",
                                          alpha = .01,
                                          adjust = "bonferroni")]
plot(head(sort(redrules_cs,by="support",decreasing = TRUE),99), method = "graph", engine = "html")


#SANDVİÇ => ÇAY GRUBU
# Figure 5.15 Sandviçler-Çay Rules
rules_sc <- apriori (data=cb_2018, parameter=list (supp=0.1,conf = 0.5), 
                     appearance = list(rhs = cay,lhs = sandvic_grubu), control = list (verbose=F))
rules_sc <- sort (rules_sc, by="confidence", decreasing=TRUE) 
length(rules_sc)
redrules_sc <- rules_sc[!is.redundant(rules_cs)]
signrules_sc <-redrules_sc[is.significant(redrules_sc,cb_2018,
                                          method = "fisher",
                                          alpha = .01,
                                          adjust = "bonferroni")]
plot(head(sort(redrules_sc,by="support",decreasing = TRUE),40), method = "graph", engine = "html")


#KAHVALTI GRUBU => BÜFELER
# Figure 5.16 Kahvalti Büfe Rules
bufe <- cb_sales_poly[which(cb_sales_poly$SALES_CATEGORY_ID == 31), ]
bufe <- bufe$UN
bufe <- levels(as.factor(bufe))

kahvalti_grubu <- cb_sales_poly[which(cb_sales_poly$SALES_CATEGORY_ID == 1), ]
kahvalti_grubu <- kahvalti_grubu$UN
kahvalti_grubu <- levels(as.factor(kahvalti_grubu))

rules_bk <- apriori (data=cb_2018, parameter=list (supp=0.00001,conf = 0.30), 
                     appearance = list(rhs = bufe,lhs = kahvalti_grubu), control = list (verbose=F))
rules_bk <- sort (rules_bk, by="confidence", decreasing=TRUE) 
length(rules_bk)
redrules_bk <- rules_bk[!is.redundant(rules_bk)]
signrules_bk <-redrules_bk[is.significant(redrules_bk,cb_2018,
                                          method = "fisher",
                                          alpha = .01,
                                          adjust = "bonferroni")]
plot(head(sort(redrules_bk,by="support",decreasing = TRUE),99), method = "graph", engine = "html")

#KAHVE => UNLU
# Figure 5.17 Kahve Unlu Rules
kahve <- cb_sales_poly[which(cb_sales_poly$SALES_CATEGORY_ID == 28), ]
kahve <- kahve$UN
kahve <- levels(as.factor(kahve))

unlu <- cb_sales_poly[which(cb_sales_poly$SALES_CATEGORY_ID == 19), ]
unlu <- unlu$UN
unlu <- levels(as.factor(unlu))

rules_ku <- apriori (data=cb_2018, parameter=list (supp=0.0001,conf = 0.05), 
                     appearance = list(rhs = unlu,lhs = kahve), control = list (verbose=F))
rules_ku <- sort (rules_ku, by="confidence", decreasing=TRUE) 
length(rules_ku)
redrules_ku <- rules_ku[!is.redundant(rules_ku)]
signrules_ku <-redrules_ku[is.significant(redrules_ku,cb_2018,
                                          method = "fisher",
                                          alpha = .01,
                                          adjust = "bonferroni")]
plot(head(sort(redrules_ku,by="support",decreasing = TRUE),99), method = "graph", engine = "html")


# SIMILARITY ON CATEGORIES--------------------------------------------------------------------
scat <- cat_2018[ ,itemFrequency(cat_2018)>0.0005]
d_jaccard <- dissimilarity(scat, which = "items")
dend<-hclust(d_jaccard,method = "ward.D2")
dend <- as.dendrogram(dend)
dend <- color_branches(dend, h = 1.1)
# Figure 5.18 Dendogram for categories
plot(dend)

# ANALYSIS OF THE EXCLUDED DATA ---------------------------------------------------------------

# Table 5.1 Sales categories for sold alone
singlecat <- cb_sales_single%>%
  group_by(CATEGORY)%>%
  summarise(AMOUNT = sum(AMOUNT),
            QTY = sum(as.numeric(QTY)),
            AVG= (AMOUNT/QTY))
singlecat <- singlecat[order(singlecat$AMOUNT,decreasing = TRUE),]
singlecat_format <- formattable(head(singlecat,10),
                                  list(AMOUNT=color_bar("lightblue"),
                                       AVG=color_bar("lightblue")))
singlecat_format

# TRANSACTIONS
sing_cat_2018 <- paste(cb_sales_single$ID,cb_sales_single$CATEGORY, sep="\n")
write(sing_cat_2018, file = "sing_cat_2018")
sing_cat_2018 <- read.transactions("sing_cat_2018", format = "single",cols = c(1,2))

#APRIORI
singrules_cat <- apriori(sing_cat_2018, parameter = list(supp = 0.01, conf = 0.05))
signsort_cat <- sort(singrules_cat, by="confidence", decreasing=TRUE)

# Figure 5.19 Rules for sold alone categories
plot(signsort_cat, method = "graph", control = list(verbose = TRUE), engine = "html")


