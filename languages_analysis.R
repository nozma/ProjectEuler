## library ----
library(rvest)
# install.packages("RSelenium")
library(RSelenium)

## web scraping ----
# selenium serverをローカルで起動しておく
# % docker run -d -p 4445:4444  selenium/standalone-firefox
remDr <- remoteDriver(remoteServerAddr = "localhost",
                      port = 4445L,
                      browserName = "firefox")
remDr$open()

## ログイン情報入力ページへ
remDr$navigate("https://projecteuler.net/languages")

## usernameとpassword入力
webElem <- remDr$findElement(using="id", value = "username")
webElem$sendKeysToElement(list("Rion778"))
webElem <- remDr$findElement(using="id", value = "password")
webElem$sendKeysToElement(list("wjb7HC2i"))

## captcha情報確認のためスクリーンショットをとる
remDr$maxWindowSize()
remDr$screenshot(display=TRUE)

## captcha入力
webElem <- remDr$findElement(using="name", value = "captcha")
webElem$sendKeysToElement(list(readline("confirmation code: ")))

## サインインボタンクリック
webElem <- remDr$findElement(using="name", value = "sign_in")
webElem$clickElement()

## ログインできていることを確認
remDr$screenshot(display=TRUE)

## 目的のページへ移動
remDr$navigate("https://projecteuler.net/languages")

## rvestでテーブル取得
df <- remDr$getPageSource()[[1]] %>% read_html %>% html_table %>% `[[`(1)

## data arrangement ----
library(dplyr)
library(magrittr)
df %<>%
  mutate_at(3:4, function(x){sub("%", "", x) %>% as.numeric})

## Visualization ----
library(ggplot2)

plotdf <- function(target){
  df %>%
    filter(rank(eval(target)) > nrow(df)/2) %>%
    ggplot(aes(x = reorder(Language, eval(target)), y = eval(target))) +
    geom_col() +
    coord_flip() +
    xlab("Language") +
    ylab(target) +
    theme_classic()
}

plotdf(quote(`Total Number of Members`))
plotdf(quote(`Percentage of Members at Level 1+`))
plotdf(quote(`Mean Percentage of Problems Solved`))

df %>%
  filter(rank(`Total Number of Members`) > nrow(df)/2,
         rank(`Mean Percentage of Problems Solved`) > nrow(df)/2) %>%
  arrange(desc(.[[2]]))

plot(df[,-1])
