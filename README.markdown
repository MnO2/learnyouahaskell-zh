
# Learn you a Haskell for Great Good 中文版 (草稿)

## 如何撰寫文章

所有文件都放在 `source/_posts/` 底下，為純文字檔案。
可以用一些小語法來強調文字，標題等。  
文章撰寫完成之後，可以用 `rake generate` 來產生 `html` 檔，  
他預設會放在public底下，  
`generate` 完成之後可以用 `rake preview` 在 local 端開一個 server，  
可以用瀏覽器預覽撰寫情形。  
預設 `port` 是 5000，可以從 `Rakefile` 裡面改設定。  


## 如何產生電子書 mobi 檔

所有電子書相關的 scripts 是放在 ebook/ 資料夾中。由於 script 是從已產生的 `html` 來轉換為 mobi 格式。所以請先跑過 `rake generate` 一遍，確保 `public` 資料夾底下已經有 `html`。
才在 `ebook` 資料夾下打 `make` 來產生 mobi 檔。相關的檔案包含 `kindlegen` 皆已複製一份至 `ebook` 目錄。


### Mac OS X 用户安装说明
```
brew update
brew install opencc
cd learnyouahaskell-zh-tw.github.com/
bundle install
```

### Arch 用户安装说明
```
pacman -S opencc
cd learnyouahaskell-zh-tw.github.com/
bundle install
```
