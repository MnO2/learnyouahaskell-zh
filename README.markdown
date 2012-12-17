
# Learn you a Haskell for Great Good 中文版 (草稿)

## 如何撰寫文章

所有文件都放在 `source/_posts/` 底下，為純文字檔案。  
可以用一些小語法來強調文字，標題等。  
文章撰寫完成之後，可以用 `rake generate` 來產生 `html` 檔，  
他預設會放在public底下，  
`generate` 完成之後可以用 `rake preview` 在 local 端開一個 server，  
可以用瀏覽器預覽撰寫情形。  
預設 `port` 是 5000，可以從 `Rakefile` 裡面改設定。  

## Dependency

需要 `ruby-1.9.2` ，並安裝 `rake`，其他 `tools` 資料夾底下有 erb, fdoc 等內附的 lib

### Arch 用户安装说明

`ruby-1.9.3` 环境下也可以运行, 需要安装以下依赖:

```
yaourt -Ss opencc
gem install ropencc
gem install rack
gem install bundler
gem install sinatra
```