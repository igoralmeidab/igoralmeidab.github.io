#assertive place 2019
#packages
library(readxl)
library(tidyverse)
library(viridis)
library(ggplot2)
#
a_place2019 <- read_excel("content/post/assertive_place2019.xlsx")
collec <- read_excel("content/post/collec.xlsx")
p_romaji <- read_excel("content/post/p_romaji.xlsx")


jp_collectivism<- read_delim("content/post/jp_collectivism.csv",
                             ";", escape_double = FALSE, trim_ws = TRUE)

A map of collectivism in Japan

# institutional research

# Collectivism



#
# Location Data
JP_plot <- tribble(
  ~Prefecture_jp,~Prefecture ,~Code, ~x, ~y,
  "北海道", "Hokkaido","HK", 16, 14,
  "青森県","Aomori"　,"AO", 15, 11,
  "岩手県","Iwate"　,"IT", 15, 10,
  "宮城県","Miyagi"　,"MG", 15, 9,
  "秋田県","Akita"　,"AK", 14, 10,
  "山形県","Yamagata"　,"YG", 14, 9,
  "福島県","Fukushima"　,"FS", 15, 8,
  "茨城県","Ibaraki"　,"IB", 15, 6,
  "栃木県","Tochigi"　,"TC", 15, 7,
  "群馬県","Gunma"　,"GU", 14, 7,
  "埼玉県","Saitama"　,"ST", 14, 6,
  "千葉県","Chiba"　,"CB", 15, 5,
  "東京都",　"Tokyo","TY", 14, 5,
  "神奈川県","Kanagawa",　"KN", 14, 4,
  "新潟県","Niigata",　"NI", 14, 8,
  "富山県","Toyama",　"TM", 13, 7,
  "石川県","Ishikawa",　"IS", 12, 7,
  "福井県","Fukui",　"FI", 12, 6,
  "山梨県","Yamanashi",　"YN", 13, 5,
  "長野県","Nagano",　"NA", 13, 6,
  "岐阜県","Gifu",　"GI", 12, 5,
  "静岡県","Shizuoka",　"SZ", 13, 4,
  "愛知県","Aichi",　"AI", 12, 4,
  "三重県","Mie",　"ME", 11, 4,
  "滋賀県","Shiga",　"SI", 11, 5,
  "京都府","Kyoto",　"KY", 10, 5,
  "大阪府","Osaka",　"OS", 9, 5,
  "兵庫県","Hyogo",　"HG", 8, 5,
  "奈良県","Nara",　"NR", 10, 4,
  "和歌山県","Wakayama",　"WA", 9, 4,
  "鳥取県","Tottori",　"TT", 7, 5,
  "島根県","Shimane",　"SM", 6, 5,
  "岡山県","Okayama",　"OY", 7, 4,
  "広島県","Hiroshima",　"HS", 6, 4,
  "山口県","Yamaguchi",　"YA", 5, 5,
  "徳島県","Tokushima"　,"TK", 7, 1,
  "香川県","Kagawa"　,"KA", 7, 2,
  "愛媛県","Ehime"　,"EH", 6, 2,
  "高知県","Kochi",　"KO", 6, 1,
  "福岡県","Fukuoka"　,"FO", 4, 3,
  "佐賀県","Saga"　,"SG", 3, 3,
  "長崎県","Nagasaki",　"NS", 2, 3,
  "熊本県","Kumamoto"　,"KU", 3, 2,
  "大分県","Oita"　,"OI", 4, 2,
  "宮崎県","Miyazaki"　,"MZ", 4, 1,
  "鹿児島県","Kagoshima" ,"KG", 3, 1,
  "沖縄県","Okinawa"　,"OK", 1, 1)

# Define Map sizes
JP_plot<- JP_plot %>% mutate(scale = c(3,rep(1,46)))# because hokkaido is larger than the other prefectures

#JP_plot$Scale <- c(3,rep(1,46)) #北海道だけ少し大きく


#putting together
jp_collectivism<- jp_collectivism %>%  left_join(JP_plot,by= "Prefecture")

##########



jp_collectivism %>% # data itself
  ggplot(aes(x = x, # x axis
             y = y, # y axis
             width = scale, # scale of width
             height = scale)) +  #scale of height
  geom_tile(aes(fill = Score), color = "black") + # variable used to calculate the colour filling of the tiles
 geom_text(aes(label = Code), size = 2.7, color = "red" ) +   # label and its size and colour
  coord_fixed(ratio = 1) +  # to keep the ratio between the representation of data and axis
  theme( # no background, axis, title, ticks or legend
    panel.background = element_blank(),
    panel.grid = element_blank(),
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    legend.position = "none")  +

  scale_fill_gradient2(low="black", # gradient colours
                     mid="#DDDDDD",
                     high="grey50",
                     midpoint = 50) # gradient midpoint




# In japanese

JP_plot$Prefec <- gsub("県", "", JP_plot$Prefec)
JP_plot$Prefec <- gsub("府", "", JP_plot$Prefec)
JP_plot$Prefec <- gsub("東京都", "東京", JP_plot$Prefec)

library(ggrepel)

jp_collectivism %>%  mutate( Prefecture_jp = str_remove_all(Prefecture_jp,"県|府"),
                             Prefecture_jp = str_replace(Prefecture_jp, "東京都","東京")) %>%

#jp_collectivism %>% # data itself
  ggplot(aes(x = x, # x axis
             y = y, # y axis
             width = scale, # scale of width
             height = scale)) +  #scale of height
  geom_tile(aes(fill = Score), color = "black") + # variable used to calculate the colour filling of the tiles
  geom_text(aes(label = Prefecture), size = 1.8, color = "red" ) +   # label and its size and colour
  coord_fixed(ratio = 1) +  # to keep the ratio between the representation of data and axis
  theme( # no background, axis, title, ticks or legend
    panel.background = element_blank(),
    panel.grid = element_blank(),
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank()
    #,
    #legend.position = "none"
    )  +

  scale_fill_gradient2(low="black", # gradient colours
                       mid="#DDDDDD",
                       high="grey50",
                       midpoint = 50) # gradient midpoint

#labs(title = "アサーティブプログラム")

#scale_fill_viridis(na.value = "#E1E1E1", option = "D", begin = 0, end = 1)

# マニュアルで色（3色）を指定する場合


####################################
# Abbreviated Prefectures
JP_plot$Prefec <- JP_plot$Prefecture
JP_plot$Prefec <- gsub("県", "", JP_plot$Prefec)
JP_plot$Prefec <- gsub("府", "", JP_plot$Prefec)
JP_plot$Prefec <- gsub("東京都", "東京", JP_plot$Prefec)

#####################################
JP_plot<- JP_plot %>%  left_join(a_place2019 %>%  count(所在地),by=c("Prefecture"="所在地")) %>%
  replace_na(list(n=0))

JP_plot<- JP_plot %>%  left_join(p_romaji %>%  mutate(P_romaji = str_to_title(P_romaji)),by="Prefecture") %>%  left_join(collec,by = c("P_romaji"="Prefecture"))

#################################
JP_plot<-JP_plot %>%  mutate(quant= case_when(n==0~"no",
                                     n<10~"few",
                                     n<200~"mid",
                                     n<1000~"many"))
###################
JP_plot %>%
  ggplot(aes(x = x,
             y = y,
             width = Scale,
             height = Scale)) +
  geom_tile(aes(fill = quant), color = "white") +
  geom_text(aes(label = paste0(Prefec,"\n",n)), size = 2.7, color = "white") +

  # Codeをラベルとして表示する場合
  #geom_text(aes(label = Code), size = 2.7, color = "white") +

  coord_fixed(ratio = 1) +
  theme(
    panel.background = element_blank(),
    panel.grid = element_blank(),
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    legend.position = "none")  +
  scale_fill_manual(values= c("steelblue","navy","blue","grey60"))+
  labs(title = "アサーティブプログラム")

#scale_fill_viridis(na.value = "#E1E1E1", option = "D", begin = 0, end = 1)

# マニュアルで色（3色）を指定する場合
 scale_fill_gradient2(low="#777777",
                      mid="#DDDDDD",
                      high="#2A465C",
                      midpoint = 50)




######################################################################################

 JP_plot %>%
   ggplot(aes(x = x,
              y = y,
              width = Scale,
              height = Scale)) +
   geom_tile(aes(fill = Score), color = "white") +
   geom_text(aes(label = paste0(P_romaji,"\n",Rank)), size = 2.7, color = "white") +

   # Codeをラベルとして表示する場合
   #geom_text(aes(label = Code), size = 2.7, color = "white") +

   coord_fixed(ratio = 1) +
   theme(
     panel.background = element_blank(),
     panel.grid = element_blank(),
     axis.title = element_blank(),
     axis.text = element_blank(),
     axis.ticks = element_blank(),
  #   legend.position = "none"
     )  +
  # scale_fill_manual(values= c("steelblue","navy","blue","grey60"))

 #scale_fill_viridis(na.value = "#E1E1E1", option = "D", begin = 0, end = 1)

 # マニュアルで色（3色）を指定する場合
#scale_fill_brewer(type="seq",palette=2)
  scale_fill_gradient2(low="blue",
                     mid="yellow",
                    high="red",
                   midpoint = 49.5)
