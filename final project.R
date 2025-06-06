library(readr)


a <- read_csv("a.csv", locale = locale(encoding = "BIG5"))


head(a)
library(tidyverse)
library(dplyr)

dplyr::last_dplyr_warnings()
library(dplyr)
glimpse(a)

library(dplyr)

a_clean <- a %>%
  mutate(
    # 先去除逗號，確保是字串
    原預算數_clean = gsub(",", "", 原預算數),
    預算增減數_clean = gsub(",", "", 預算增減數),
    
    # 把空字串或不適合數字的字串先改成 NA
    原預算數_clean = na_if(原預算數_clean, ""),
    預算增減數_clean = na_if(預算增減數_clean, ""),
    
    原預算數_clean = ifelse(原預算數_clean %in% c("無資料", "-", "NA"), NA, 原預算數_clean),
    預算增減數_clean = ifelse(預算增減數_clean %in% c("無資料", "-", "NA"), NA, 預算增減數_clean),
    
    # 再轉成數字
    原預算數_num = as.numeric(原預算數_clean),
    預算增減數_num = as.numeric(預算增減數_clean)
  )

# 查看轉換失敗的列
a_clean %>%
  filter(is.na(原預算數_num) | is.na(預算增減數_num)) %>%
  select(原預算數, 預算增減數, 原預算數_clean, 預算增減數_clean)
glimpse(a)
library(dplyr)

a_clean <- a %>%
  mutate(
    # 去除逗號，轉字串
    原預算數_clean = gsub(",", "", 原預算數),
    預算增減數_clean = gsub(",", "", 預算增減數),
    
    # 轉成數字，轉失敗會變 NA
    原預算數_num = suppressWarnings(as.numeric(原預算數_clean)),
    預算增減數_num = suppressWarnings(as.numeric(預算增減數_clean)),
    
    # 預算增減數_num 轉失敗 (NA) 的話，保留原字串（去逗號後的字串）
    預算增減數_final = ifelse(
      is.na(預算增減數_num),
      預算增減數_clean,
      as.character(預算增減數_num)
    ),
    
    # 同理，若需要也可以做原預算數_final（這裡示範保留原數字）
    原預算數_final = ifelse(
      is.na(原預算數_num),
      原預算數_clean,
      as.character(原預算數_num)
    )
  )

# 看結果
glimpse(a_clean)
a_clean %>% select(原預算數, 預算增減數, 原預算數_final, 預算增減數_final)


library(dplyr)
library(tidyr)
library(ggplot2)

df_plot <- a_clean %>%
  select(項次, 原預算數_num, 預算增減數_num) %>%
  mutate(項次 = as.factor(項次)) %>%   # 項次改成分類方便長條圖
  pivot_longer(cols = c(原預算數_num, 預算增減數_num),
               names_to = "類別", values_to = "金額") %>%
  filter(!is.na(金額))

ggplot(df_plot, aes(x = 項次, y = 金額, fill = 類別)) +
  geom_col(position = "dodge") +  # 分組長條
  labs(
    title = "項次 vs 預算金額長條圖",
    x = "項次",
    y = "金額",
    fill = "類別"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

