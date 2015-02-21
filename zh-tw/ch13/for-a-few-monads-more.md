# 再來看看更多 Monad

[$../img/clint.png]

我們已經看過 Monad 是如何接受具有 context 的值，並如何用函數操作他們 還有如何用 ``>>=`` 跟 ``do`` 來減輕我們對 context 的關注，集中精神在 value 本身。

我們也看過了 ``Maybe`` 是如何把值加上一個可能會失敗的 context。我們學習到 List Monad 是如何加進多重結果的 context。我們也了解 ``IO`` Monad 如何運作，而且我們在知道什麼是 Monad 之前就已經知道他了。

在這個章節，我們會介紹一些其他的 Monad。他們可以把值變成 monadic value，因此可以讓我們的程式更簡潔清晰。多見識幾個 Monad 也可以敏銳我們對 Monad 的直覺。

我們即將要介紹的 Monad 都包含在 ``mtl`` 這個套建中。一個 Haskell package 包含了一堆模組。而 ``mtl`` 已經包含在 Haskell Platform 中，所以你可能不用另外安裝。要檢查你有沒有這套件，你可以下 ``ghc-pkg list``。這會列出你已經安裝的套件，其中應該包含 ``mtl`` 後面接著對應的版號。


## 你所不知道的 Writer Monad
我們已經看過 ``Maybe``, list 以及 ``IO`` Monad。現在我們要來看看 ``Writer`` Monad。

相對於 ``Maybe`` 是加入可能失敗的 context，list 是加入 non-deterministic 的 context，``Writer`` 則是加進一個附加值的 context，好比 log 一般。``Writer`` 可以讓我們在計算的同時蒐集所有 log 紀錄，並匯集成一個 log 並附加在結果上。

例如我們想要附加一個 String 好說明我們的值在幹麼（有可能是為了除錯）。想像有一個函數接受一個代表幫派人數的數字，然後會回傳值告訴我們這是否算是一個龐大的幫派：

```
isBigGang :: Int -> Bool  
isBigGang x = x > 9  
```

現在我們希望他不只是回傳 ``True`` 或 ``False``，我們還希望他能夠多回傳一個字串代表 log。這很容易，只要多加一個 ``String`` 在 ``Bool`` 旁邊就好了。

```
isBigGang :: Int -> (Bool, String)  
isBigGang x = (x > 9, "Compared gang size to 9.")  
```

我們現在回傳了一個 Tuple，第一個元素是原來的布林值，第二個元素是一個 String。現在我們的值有了一個 context。

```
ghci> isBigGang 3  
(False,"Compared gang size to 9.")  
ghci> isBigGang 30  
(True,"Compared gang size to 9.")  
```

[^../img/tuco.png]

到目前為止都還不錯，``isBigGang`` 回傳一個值跟他的 context。對於正常的數值來說這樣的寫法都能運作良好。但如果我們想要把一個已經具有 context 的值，像是 ``(3, "Smallish gang.")``，餵給 ``isBigGang`` 呢？我們又面對了同樣的問題：如果我們有一個能接受正常數值並回傳一個具有 context 值的 function，那我們要如何餵給他一個具有 context 的值？

當我們在研究 ``Maybe`` monad 的時候，我們寫了一個 ``applyMaybe``。他接受一個 ``Maybe a`` 值跟一個 ``a -> Maybe b`` 型態的函數，他會把 ``Maybe a`` 餵給這個 function，即便這個 function 其實是接受 ``a`` 而非 ``Maybe a``。``applyMaybe`` 有針對這樣的 context 做處理，也就是會留意有可能發生的失敗情況。但在 ``a -> Maybe b`` 裡面，我們可以只專心處理正常數值即可。因為 ``applyMaybe`` (之後變成了 ``>>=``)會幫我們處理需要檢查 ``Nothing`` 或 ``Just`` 的情況。

我們再來寫一個接受附加 log 值的函數，也就是 ``(a, String)`` 型態的值跟 ``a -> (b, String)`` 型態的函數。我們稱呼這個函數為 ``applyLog``。這個函數有的 context 是附加 log 值，而不是一個可能會失敗的 context，因此 ``applyLog`` 會確保原有的 log 被保留，並附上從函數產生出的新的 log。這邊我們來看一下實作：

```
applyLog :: (a,String) -> (a -> (b,String)) -> (b,String)  
applyLog (x,log) f = let (y,newLog) = f x in (y,log ++ newLog)  
```

當我們想把一個具有 context 的值餵給一個函數的時候，我們會嘗試把值跟他的 context 分開，然後把值餵給函數再重新接回 context。在 ``Maybe`` monad 的情況，我們檢查值是否為 ``Just x``，如果是，便將 ``x`` 餵給函數。而在 log 的情況，我們知道 pair 的其中一個 component 是 log 而另一個是值。所以我們先取出值 ``x``，將 ``f`` apply 到 ``x``，便獲取 ``(y,newLog)``，其中 ``y`` 是新的值而 ``newLog`` 則是新的 log。但如果我們回傳 ``newLog``，舊的 log 便不會包含進去，所以我們要回傳的是 ``(y, log ++ newLog)``。我們用 ``++`` 來把新的 log 接到舊的上面。

來看看 ``applyLog`` 運作的情形：

```
ghci> (3, "Smallish gang.") `applyLog` isBigGang  
(False,"Smallish gang.Compared gang size to 9")  
ghci> (30, "A freaking platoon.") `applyLog` isBigGang  
(True,"A freaking platoon.Compared gang size to 9")  
```

跟之前的結果很像，只差在我們多了伴隨產生的 log。再來多看幾個例子：

```
ghci> ("Tobin","Got outlaw name.") `applyLog` (\x -> (length x, "Applied length."))  
(5,"Got outlaw name.Applied length.")  
ghci> ("Bathcat","Got outlaw name.") `applyLog` (\x -> (length x, "Applied length"))  
(7,"Got outlaw name.Applied length")  
```

可以看到在 lambda 裡面 ``x`` 只是個正常的字串而不是 tuple，且 ``applyLog`` 幫我們處理掉附加 log 的動作。

### Monoids 的好處

    請確定你了解什麼是 Monoids。

到目前為止 ``applyLog`` 接受 ``(a,String)`` 型態的值，但為什麼 log 一定要是 ``String`` 呢？我們使用 ``++`` 來附加新的 log，難道 ``++`` 並不能運作在任何形式的 list，而一定要限制我們在 ``String`` 上呢？我們當然可以擺脫 ``String``，我們可以如下改變他的型態：

```
applyLog :: (a,[c]) -> (a -> (b,[c])) -> (b,[c])      
```

我們用一個 List 來代表 Log。包含在 List 中的元素型態必須跟原有的 List 跟回傳的 List 型態相同，否則我們沒辦法用 ``++`` 來把他們接起來。

這能夠運作在 bytestring 上嗎？絕對沒問題。只是我們現在的型態只對 List 有效。我們必須要另外做一個 bytestring 版本的 ``applyLog``。但我們注意到 List 跟 bytestring 都是 monoids。因此他們都是 ``Monoid`` type class 的 instance，那代表他們都有實作 ``mappend``。對 List 以及 bytestring 而言，``mappend`` 都是拿來串接的。

```
ghci> [1,2,3] `mappend` [4,5,6]  
[1,2,3,4,5,6]  
ghci> B.pack [99,104,105] `mappend` B.pack [104,117,97,104,117,97]  
Chunk "chi" (Chunk "huahua" Empty)  
```

修改後我們的 ``applyLog`` 可以運作在任何 monoid 上。我們必須要修改型態宣告來表示這件事，同時也要在實作中把 ``++`` 改成 ``mappend``：

```
applyLog :: (Monoid m) => (a,m) -> (a -> (b,m)) -> (b,m)  
applyLog (x,log) f = let (y,newLog) = f x in (y,log `mappend` newLog)  
```

由於現在包含的值可以是任何 monoid，我們不再需要把 tuple 想成包含一個值跟對應的 log，我們可以想成他包含一個值跟一個對應的 monoid。舉例來說，可以說我們有一個 tuple 包含一個產品名稱跟一個符合 monoid 特性的產品價格。我們可以定義一個 ``Sum`` 的 newtype 來保證我們在操作產品的時候也會把價錢跟著加起來。

```
import Data.Monoid  
  
type Food = String  
type Price = Sum Int  

addDrink :: Food -> (Food,Price)  
addDrink "beans" = ("milk", Sum 25)  
addDrink "jerky" = ("whiskey", Sum 99)  
addDrink _ = ("beer", Sum 30)  
```

我們用 string 來代表食物，用 ``newtype`` 重新定義 ``nInt`` 為 ``Sum``，來追蹤總共需要花多少錢。可以注意到我們用 ``mappend`` 來操作 ``Sum`` 的時候，價錢會被一起加起來。

```
ghci> Sum 3 `mappend` Sum 9  
Sum {getSum = 12}  
```

``addDrink`` 的實作很簡單，如果我們想吃豆子，他會回傳 ``"milk"`` 以及伴隨的 ``Sum 25``，同樣的如果我們要吃 "jerky"，他就會回傳 "whiskey"，要吃其他東西的話，就會回傳 "beer"。乍看之下這個函數沒什麼特別，但如果用 ``applyLog`` 的話就會有趣些。

```
ghci> ("beans", Sum 10) `applyLog` addDrink  
("milk",Sum {getSum = 35})  
ghci> ("jerky", Sum 25) `applyLog` addDrink  
("whiskey",Sum {getSum = 124})  
ghci> ("dogmeat", Sum 5) `applyLog` addDrink  
("beer",Sum {getSum = 35})  
```

牛奶價值 ``25`` 美分，但如果我們也吃了價值 ``10`` 美分的豆子的話，總共需要付 ``35`` 美分。這樣很清楚地展示了伴隨的值不一定需要是 log，他可以是任何 monoid。至於兩個值要如何結合，那要看 monoid 中怎麼定義。當我們需要的是 log 的時候，他們是串接，但這個 case 裡面，數字是被加起來。

由於 ``addDrink`` 回傳一個 ``(Food,Price)``，我們可以再把結果重新餵給 ``addDrink``，這可以很容易告訴我們總共喝了多少錢：

```
ghci> ("dogmeat", Sum 5) `applyLog` addDrink `applyLog` addDrink  
("beer",Sum {getSum = 65})  
```

將狗食跟 30 美分的啤酒加在一起會得到 ``("beer", Sum 35)``。如果我們用 ``applyLog`` 將上面的結果再餵給 ``addDrink``，我們會得到 ``("beer", Sum 65)`` 這樣的結果。


### The Writer type

我們認識了一個附加 monoid 的值其實表現出來的是一個 monad，我們來再來看看其他類似的 ``Monad`` instance。``Control.Monad.Writer`` 這模組含有 ``Writer w a`` 的一個型態，裏面定義了他 ``Monad`` 的 instance，還有一些操作這些值的函數。

首先，我們來看一下型態。要把一個 monoid 附加給一個值，只需要定義一個 tuple 就好了。``Writer w a`` 這型態其實是一個 ``newtype`` wrapper。他的定義很簡單：

```
newtype Writer w a = Writer { runWriter :: (a, w) }      
```

他包在一個 ``newtype`` 裏面，並且可以是一個 ``Monad`` 的 instance，而且這樣定義的好處是可以跟單純 tuple 的型態區分開來。``a`` 這個型態參數代表是包含的值的型態，而 ``w`` 則是附加的 monoid 的型態。

他 ``Monad`` instance 的定義如下：

```
instance (Monoid w) => Monad (Writer w) where  
    return x = Writer (x, mempty)  
    (Writer (x,v)) >>= f = let (Writer (y, v')) = f x in Writer (y, v `mappend` v')  
```

[$../img/angeleyes.png]

首先，我們來看看 ``>>=``。他的實作基本上就是 ``applyLog``，只是我們的 tuple 現在是包在一個 ``Writer`` 的 ``newtype`` 中，我們可以用 pattern matching 的方式把他給 unwrap。我們將 ``x`` 餵給 ``f``。這會回給我們 ``Writer w a``。接著可以用 ``let`` expression 來做 pattern matching。把結果綁定到 ``y`` 這個名字上，然後用 ``mappend`` 來結合舊的 monoid 值跟新的 monoid 值。最後把結果跟 monoid 值用 ``Writer`` constructor 包起來，形成我們最後的 ``Writer`` value。


那 ``return`` 呢？回想 ``return`` 的作用是接受一個值，並回傳一個具有意義的最小 context 來裝我們的值。那究竟什麼樣的 context 可以代表我們的 ``Writer`` 呢？如果我們希望 monoid 值所造成的影響愈小愈好，那 ``mempty`` 是個合理的選擇。``mempty`` 是被當作 identity monoid value，像是 ``""`` 或 ``Sum 0``，或是空的 bytestring。當我們對 ``mempty`` 用 ``mappend`` 跟其他 monoid 值結合，結果會是其他的 monoid 值。所以如果我們用 ``return`` 來做一個 ``Writer``，然後用 ``>>=`` 來餵給其他的函數，那函數回傳的便是算出來的 monoid。下面我們試著用 ``return`` 搭配不同 context 來回傳 ``3``：

```
ghci> runWriter (return 3 :: Writer String Int)  
(3,"")  
ghci> runWriter (return 3 :: Writer (Sum Int) Int)  
(3,Sum {getSum = 0})  
ghci> runWriter (return 3 :: Writer (Product Int) Int)  
(3,Product {getProduct = 1})  
```

因為 ``Writer`` 並沒有定義成 ``Show`` 的 instance，我們必須用 ``runWriter`` 來把我們的 ``Writer`` 轉成正常的 tuple。對於 ``String``，monoid 的值就是空字串。而對於 ``Sum`` 來說則是 ``0``，因為 ``0`` 加上其他任何值都會是對方。而對 ``Product`` 來說，則是 ``1``。

這裡的 ``Writer`` instance 並沒有定義 ``fail``，所以如果 pattern matching 失敗的話，就會呼叫 ``error``。


### Using do notation with Writer

既然我們定義了 ``Monad`` 的 instance，我們自然可以用 ``do`` 串接 ``Writer`` 型態的值。這在我們需要對一群 ``Writer`` 型態的值做處理時顯得特別方便。就如其他的 monad，我們可以把他們當作具有 context 的值。在現在這個 case 中，所有的 monoid 的值都會用 ``mappend`` 來連接起來並得到最後的結果。這邊有一個簡單的範例，我們用 ``Writer`` 來相乘兩個數。

```
import Control.Monad.Writer  
  
logNumber :: Int -> Writer [String] Int  
logNumber x = Writer (x, ["Got number: " ++ show x])  
  
multWithLog :: Writer [String] Int  
multWithLog = do  
    a <- logNumber 3  
    b <- logNumber 5  
    return (a*b)  
```

``logNumber`` 接受一個數並把這個數做成一個 ``Writer``。我們再用一串 string 來當作我們的 monoid 值，每一個數都跟著一個只有一個元素的 list，說明我們只有一個數。``multWithLog`` 式一個 ``Writer``，他將 ``3`` 跟 ``5`` 相乘並確保相乘的紀錄有寫進最後的 log 中。我們用 ``return`` 來做成 ``a*b`` 的結果。我們知道 ``return`` 會接受某個值並加上某個最小的 context，我們可以確定他不會多添加額外的 log。如果我們執行程式會得到：

```
ghci> runWriter multWithLog  
(15,["Got number: 3","Got number: 5"])  
```

有時候我們就是想要在某個時間點放進某個 Monoid value。``tell`` 正是我們需要的函數。他實作了 ``MonadWriter`` 這個 type class，而且在當 ``Writer`` 用的時候也能接受一個 monoid value，好比說 ``["This is going on"]``。我們能用他來把我們的 monoid value 接到任何一個 dummy value ``()`` 上來形成一個 Writer。當我們拿到的結果是 ``()`` 的時候，我們不會把他綁定到變數上。來看一個 ``multWithLog`` 的範例：

```
multWithLog :: Writer [String] Int  
multWithLog = do  
    a <- logNumber 3  
    b <- logNumber 5  
    tell ["Gonna multiply these two"]  
    return (a*b)  
```

``return (a*b)`` 是我們的最後一行，還記得在一個 ``do`` 中的最後一行代表整個 ``do`` 的結果。如果我們把 ``tell`` 擺到最後，則 ``do`` 的結果則會是 ``()``。我們會因此丟掉乘法運算的結果。除此之外，log 的結果是不變的。

```
ghci> runWriter multWithLog  
(15,["Got number: 3","Got number: 5","Gonna multiply these two"])  
```


### Adding logging to programs
歐幾里得算法是找出兩個數的最大公因數。Haskell 已經提供了 ``gcd`` 的函數，但我們來實作一個具有 log 功能的 gcd：

```
gcd' :: Int -> Int -> Int  
gcd' a b   
    | b == 0    = a  
    | otherwise = gcd' b (a `mod` b)  
```

演算法的內容很簡單。首先他檢查第二個數字是否為零。如果是零，那就回傳第一個數字。如果不是，那結果就是第二個數字跟將第一個數字除以第二個數字的餘數兩個數字的最大公因數。舉例來說，如果我們想知道 8 跟 3 的最大公因數，首先可以注意到 3 不是 0。所以我們要求的是 3 跟 2 的最大公因數(8 除以 3 餘二)。接下去我可以看到 2 不是 0，所以我們要再找 2 跟 1 的最大公因數。同樣的，第二個數不是 0，所以我們再找 1 跟 0 的最大公因數。最後第二個數終於是 0 了，所以我們得到最大公因數是 1。

```
ghci> gcd' 8 3  
1  
```

答案真的是這樣。接著我們想加進 context，context 會是一個 monoid value 並且像是一個 log 一樣。就像之前的範例，我們用一串 string 來當作我們的 monoid。所以 ``gcd'`` 會長成這樣：

```
gcd' :: Int -> Int -> Writer [String] Int  
```

而他的程式碼會像這樣：

```
import Control.Monad.Writer  
  
gcd' :: Int -> Int -> Writer [String] Int  
gcd' a b  
  | b == 0 = do  
      tell ["Finished with " ++ show a]  
      return a  
  | otherwise = do  
      tell [show a ++ " mod " ++ show b ++ " = " ++ show (a `mod` b)]  
      gcd' b (a `mod` b)  
```

這個函數接受兩個 ``Int`` 並回傳一個 ``Writer [String] Int``，也就是說是一個有 log context 的 ``Int``。當 ``b`` 等於 ``0`` 的時候，我們用一個 ``do`` 來組成一個 ``Writer`` 的值。我們先用 ``tell`` 來寫入我們的 log，然後用 ``return`` 來當作 ``do`` 的結果。當然我們也可以這樣寫：

```
Writer (a, ["Finished with " ++ show a])  
```

但我想 ``do`` 的表達方式是比較容易閱讀的。接下來我們看看當 ``b`` 不等於 ``0`` 的時候。我們會把 ``mod`` 的使用情況寫進 log。然後在 ``do`` 當中的第二行遞迴呼叫 ``gcd'``。``gcd'`` 現在是回傳一個 ``Writer`` 的型態，所以 ``gcd' b (a `mod` b)`` 這樣的寫法是完全沒問題的。

儘管去 trace 這個 ``gcd'`` 對於理解十分有幫助，但我想了解整個大概念，把值視為具有 context 是更加有用的。

接著來試試跑我們的 ``gcd'``，他的結果會是 ``Writer [String] Int``，如果我們把他從 ``newtype`` 中取出來，我們會拿到一個 tuple。tuple 的第一個部份就是我們要的結果：

```
ghci> fst $ runWriter (gcd' 8 3)  
1  
```

至於 log 呢，由於 log 是一連串 string，我們就用 ``mapM_ putStrLn`` 來把這些 string 印出來：

```
ghci> mapM_ putStrLn $ snd $ runWriter (gcd' 8 3)  
8 mod 3 = 2  
3 mod 2 = 1  
2 mod 1 = 0  
Finished with 1  
```

把普通的演算法轉換成具有 log 是很棒的經驗，我們不過是把普通的 value 重寫成 Monadic value，剩下的就靠 ``>>=`` 跟 ``Writer`` 來幫我們處理一切。用這樣的方法我們幾乎可以對任何函數加上 logging 的功能。我們只要把普通的值換成 ``Writer``，然後把一般的函數呼叫換成 ``>>=`` (當然也可以用 ``do``)

### Inefficient list construction
當製作 ``Writer`` Monad 的時候，要特別注意你是使用哪種 monoid。使用 list 的話效能有時候是沒辦法接受的。因為 list 是使用 ``++`` 來作為 ``mappend`` 的實現。而 ``++`` 在 list 很長的時候是非常慢的。

在之前的 ``gcd'`` 中，log 並不會慢是因為 list append 的動作實際上看起來是這樣：

```
a ++ (b ++ (c ++ (d ++ (e ++ f))))  
```

list 是建立的方向是從左到右，當我們先建立左邊的部份，而把另一串 list 加到右邊的時候效能會不錯。但如果我們不小心使用，而讓 ``Writer`` monad 實際在操作 list 的時候變成像這樣的話。

```
((((a ++ b) ++ c) ++ d) ++ e) ++ f 
```

這會讓我們的操作是 left associative，而不是 right associative。這非常沒有效率，因為每次都是把右邊的部份加到左邊的部份，而左邊的部份又必須要從頭開始建起。

下面這個函數跟 ``gcd'`` 差不多，只是 log 的順序是相反的。他先紀錄剩下的操作，然後紀錄現在的步驟。

```
import Control.Monad.Writer  
  
gcdReverse :: Int -> Int -> Writer [String] Int  
gcdReverse a b  
  | b == 0 = do  
      tell ["Finished with " ++ show a]  
      return a  
    | otherwise = do  
      result <- gcdReverse b (a `mod` b)  
      tell [show a ++ " mod " ++ show b ++ " = " ++ show (a `mod` b)]  
      return result  
```

他先遞迴呼叫，然後把結果綁定到 ``result``。然後把目前的動作寫到 log，在遞迴的結果之後。最後呈現的就是完整的 log。

``` 
ghci> mapM_ putStrLn $ snd $ runWriter (gcdReverse 8 3)  
Finished with 1  
2 mod 1 = 0  
3 mod 2 = 1  
8 mod 3 = 2  
```

這沒效率是因為他讓 ``++`` 成為 left associative 而不是 right associative。


### Difference lists

[^../img/cactus.png]

由於 list 在重複 append 的時候顯得低效，我們最好能使用一種支援高效 appending 的資料結構。其中一種就是 difference list。difference list 很類似 list，只是他是一個函數。他接受一個 list 並 prepend 另一串 list 到他前面。一個等價於 ``[1,2,3]`` 的 difference list 是這樣一個函數 ``\xs -> [1,2,3] ++ xs``。一個等價於 ``[]`` 的 difference list 則是 ``\xs -> [] ++ xs``。

Difference list 最酷的地方在於他支援高效的 appending。當我們用 ``++`` 來實現 appending 的時候，他必須要走到左邊的 list 的尾端，然後把右邊的 list 一個個從這邊接上。那 difference list 是怎麼作的呢？appending 兩個 difference list 就像這樣

```
f `append` g = \xs -> f (g xs)  
```

``f`` 跟 ``g`` 這邊是兩個函數，他們都接受一個 list 並 prepend 另一串 list。舉例來說，如果 ``f`` 代表 ``("dog"++)``（可以寫成 ``\xs -> "dog" ++ xs``）而 ``g`` 是 ``("meat"++)``，那 ``f `append` g`` 就會做成一個新的函數，等價於：

```
\xs -> "dog" ++ ("meat" ++ xs)  
```

append 兩個 difference list 其實就是用一個函數，這函數先餵一個 list 給第一個 difference list，然後再把結果餵給第二個 difference list。

我們可以用一個 ``newtype`` 來包起來

``` 
newtype DiffList a = DiffList { getDiffList :: [a] -> [a] }  
```

我們包起來的型態是 ``[a] -> [a]``，因為 difference list 不過就是一個轉換一個 list 到另一個 list 的函數。要把普通 list 轉換成 difference list 也很容易。

``` 
toDiffList :: [a] -> DiffList a  
toDiffList xs = DiffList (xs++)  
  
fromDiffList :: DiffList a -> [a]  
fromDiffList (DiffList f) = f []  
```

要把一個普通 list 轉成 difference list 不過就是照之前定義的，作一個 prepend 另一個 list 的函數。由於 difference list 只是一個 prepend 另一串 list 的一個函數，假如我們要轉回來的話，只要餵給他空的 list 就行了。

這邊我們給一個 difference list 的 ``Monoid`` 定義

```
instance Monoid (DiffList a) where  
    mempty = DiffList (\xs -> [] ++ xs)  
    (DiffList f) `mappend` (DiffList g) = DiffList (\xs -> f (g xs))  
```

我們可以看到 ``mempty`` 不過就是 ``id``，而 ``mappend`` 其實是 function composition。

```
ghci> fromDiffList (toDiffList [1,2,3,4] `mappend` toDiffList [1,2,3])  
[1,2,3,4,1,2,3]  
```

現在我們可以用 difference list 來加速我們的 ``gcdReverse``

```
import Control.Monad.Writer  
  
gcd' :: Int -> Int -> Writer (DiffList String) Int  
gcd' a b  
  | b == 0 = do  
      tell (toDiffList ["Finished with " ++ show a])  
      return a  
  | otherwise = do  
      result <- gcd' b (a `mod` b)  
      tell (toDiffList [show a ++ " mod " ++ show b ++ " = " ++ show (a `mod` b)])  
      return result  
```

我們只要把 monoid 的型態從 ``[String]`` 改成 ``DiffList String``，並在使用 ``tell`` 的時候把普通的 list 用 ``toDiffList`` 轉成 difference list 就可以了。

```
ghci> mapM_ putStrLn . fromDiffList . snd . runWriter $ gcdReverse 110 34  
Finished with 2  
8 mod 2 = 0  
34 mod 8 = 2  
110 mod 34 = 8  
```

我們用 ``runWriter`` 來取出 ``gcdReverse 110 34`` 的結果，然後用 ``snd`` 取出 log，並用 ``fromDiffList`` 轉回普通的 list 印出來。


### Comparing Performance

要體會 Difference List 能如何增進效率，考慮一個從某數數到零的 case。我們紀錄的時候就像 ``gcdReverse`` 一樣是反過來記的，所以在 log 中實際上是從零數到某個數。

```
finalCountDown :: Int -> Writer (DiffList String) ()  
finalCountDown 0 = do  
    tell (toDiffList ["0"])  
finalCountDown x = do  
    finalCountDown (x-1)  
    tell (toDiffList [show x])  
```

如果我們餵 ``0``，他就只 log 0。如果餵其他正整數，他會先倒數到 ``0`` 然後 append 那些數到 log 中，所以如果我們呼叫 ``finalCountDown`` 並餵給他 ``100``，那 log 的最後一筆就會是 ``"100"``。

如果你把這個函數 load 進 GHCi 中並餵給他一個比較大的整數 ``500000``，你會看到他無停滯地從 ``0`` 開始數起：

```
ghci> mapM_ putStrLn . fromDiffList . snd . runWriter $ finalCountDown 500000  
0  
1  
2  
```

但如果我們用普通的 list 而不用 difference list

```
finalCountDown :: Int -> Writer [String] ()  
finalCountDown 0 = do  
    tell ["0"]  
finalCountDown x = do  
    finalCountDown (x-1)  
    tell [show x]  
```

並下同樣的指令

```
ghci> mapM_ putStrLn . snd . runWriter $ finalCountDown 500000  
```

我們會看到整個運算卡卡的。

當然這不是一個嚴謹的測試方法，但足以表顯出 difference list 是比較有效率的寫法。


## Reader Monad

[^../img/revolver.png]

在講 Applicative 的章節中，我們說過了 ``(->) r`` 的型態只是 ``Functor`` 的一個 instance。要將一個函數 ``f`` map over 一個函數 ``g``，基本上等價於一個函數，他可以接受原本 ``g`` 接受的參數，先套用 ``g`` 然後再把其結果丟給 ``f``。

```
ghci> let f = (*5)  
ghci> let g = (+3)
ghci> (fmap f g) 8
```

我們已經見識過函數當作 applicative functors 的例子。這樣能讓我們對函數的結果直接進行操作。

```
ghci> let f = (+) <$> (*2) <*> (+10)
ghci> f 3
19
```

``(+) <$> (*2) <*> (+10)`` 代表一個函數，他接受一個數值，分別把這數值交給 ``(*2)`` 跟 ``(+10)``。然後把結果加起來。例如說，如果我們餵 ``3`` 給這個函數，他會分別對 ``3`` 做 ``(*2)`` 跟 ``(+10)`` 的動作。而得到 ``6`` 跟 ``13``。然後呼叫 ``(+)``，而得到 ``19``。

其實 ``(->) r`` 不只是一個 functor 跟一個 applicative functor，他也是一個 monad。就如其他 monadic value 一般，一個函數也可以被想做是包含一個 context 的。這個 context 是說我們期待某個值，他還沒出現，但我們知道我們會把他當作函數的參數，呼叫函數來得到結果。

我們已經見識到函數是怎樣可以看作 functor 或是 applicative functors 了。再來讓我們看看當作 ``Monad`` 的一個 instance 時會是什麼樣子。你可以在 ``Control.Monad.Instances`` 裡面找到，他看起來像這樣：

```
instance Monad ((->) r) where  
    return x = \_ -> x  
    h >>= f = \w -> f (h w) w  
```

我們之前已經看過函數的 ``pure`` 實作了，而 ``return`` 差不多就是 ``pure``。他接受一個值並把他放進一個 minimal context 裡面。而要讓一個函數能夠是某個定值的唯一方法就是讓他完全忽略他的參數。

而 ``>>=`` 的實作看起來有點難以理解，我們可以仔細來看看。當我們使用 ``>>=`` 的時候，餵進去的是一個 monadic value，處理他的是一個函數，而吐出來的也是一個 monadic value。在這個情況下，當我們將一個函數餵進一個函數，吐出來的也是一個函數。這就是為什麼我們在最外層使用了一個 lambda。在我們目前看過的實作中，``>>=`` 幾乎都是用 lambda 將內部跟外部隔開來，然後在內部來使用 ``f``。這邊也是一樣的道理。要從一個函數得到一個結果，我們必須餵給他一些東西，這也是為什麼我們先用 ``(h w)`` 取得結果，然後將他丟給 ``f``。而 ``f`` 回傳一個 monadic value，在這邊這個 monadic value 也就是一個函數。我們再把 ``w`` 餵給他。

如果你還不太懂 ``>>=`` 怎麼寫出來的，不要擔心，因為接下來的範例會讓你曉得這真的是一個簡單的 Monad。我們造一個 ``do`` expression 來使用這個 Monad。

``` 
import Control.Monad.Instances  
  
addStuff :: Int -> Int  
addStuff = do  
  a <- (*2)  
  b <- (+10)  
  return (a+b)  
```

這跟我們之前寫的 applicative expression 差不多，只差在他是運作在 monad 上。一個 ``do`` expression 的結果永遠會是一個 monadic vlaue，這個也不例外。而這個 monadic value 其實是一個函數。只是在這邊他接受一個數字，然後套用 ``(*2)``，把結果綁定到 ``a`` 上面。而 ``(+10)`` 也同用被套用到同樣的參數。結果被綁定到 ``b`` 上。``return`` 就如其他 monad 一樣，只是製作一個簡單的 monadic value 而不會作多餘的事情。這讓整個函數的結果是 ``a+b``。如果我們試著跑跑看，會得到之前的結果。

```
ghci> addStuff 3  
19  
```

其中 ``3`` 會被餵給 ``(*2)`` 跟 ``(+10)``。而且他也會被餵給 ``return (a+b)``，只是他會忽略掉 ``3`` 而永遠回傳 ``a+b`` 正因為如此，function monad 也被稱作 reader monad。所有函數都從一個固定的地方讀取。要寫得更清楚一些，可以把 ``addStuff`` 改寫如下：

```
addStuff :: Int -> Int  
addStuff x = let  
    a = (*2) x  
    b = (+10) x  
    in a+b  
```

我們見識了把函數視作具有 context 的值很自然的可以表達成 reader monad。只要我們當作我們知道函數會回傳什麼值就好。他作的就是把所有的函數都黏在一起做成一個大的函數，然後把這個函數的參數都餵給全部組成的函數，這有點取出他們未來的值的意味。實作做完了然後 ``>>=`` 就會保證一切都能正常運作。

## State Monad

[^../img/texas.png]

Haskell 是一個純粹的語言，正因為如此，我們的程式是有一堆沒辦法改變全域狀態或變數的函數所組成，他們只會作些處理並回傳結果。這樣的性質讓我們很容易思考我們的程式在幹嘛，因為我們不需要擔心變數在某一個時間點的值是什麼。然而，有一些領域的問題根本上就是依賴於隨著時間而改變的狀態。雖然我們也可以用 Haskell 寫出這樣的程式，但有時候寫起來蠻痛苦的。這也是為什麼 Haskell 要加進 State Monad 這個特性。這讓我們在 Haskell 中可以容易地處理狀態性的問題，並讓其他部份的程式還是保持純粹性。


當我們處理亂數的時候，我們的函數接受一個 random generator 並回傳一個新的亂數跟一個新的 random generator。如果我們需要很多個亂數，我們可以用前一個函數回傳的 random generator 繼續做下去。當我們要寫一個接受 ``StdGen`` 的函數並產生丟三個硬幣結果的函數，我們會這樣寫：

```
threeCoins :: StdGen -> (Bool, Bool, Bool)  
threeCoins gen =   
    let (firstCoin, newGen) = random gen  
        (secondCoin, newGen') = random newGen  
        (thirdCoin, newGen''') = random newGen'  
    in  (firstCoin, secondCoin, thirdCoin)  
```

他接受一個 ``gen`` 然後用 ``random gen`` 產生一個 ``Bool`` 型態的值以及新的 generator。要模擬丟第二個硬幣的話，便使用新的 generator。在其他語言中，多半除了亂數之外不需要多回傳一個 generator。那是因為我們可以對現有的進行修改。但 Haskell 是純粹的語言，我們沒辦法那麼做，所以我們必須要接受一個狀態，產生結果然後回傳一個新的狀態，然後用新的狀態來繼續做下去。

一般來講你應該不會喜歡這麼寫，在程式中有赤裸裸的狀態，但我們又不想放棄 Haskell 的純粹性質。這就是 State Monad 的好處了，他可以幫我們處理這些瑣碎的事情，又讓我們保持 Haskell 的純粹性。

為了深入理解狀態性的計算，我們先來看看應該給他們什麼樣的型態。我們會說一個狀態性的計算是一個函數，他接受一個狀態，回傳一個值跟一個新的狀態。寫起來會像這樣：

```
s -> (a,s) 
```

``s`` 是狀態的型態，而 ``a`` 是計算結果的型態。


    在其他的語言中，賦值大多是被當作會改變狀態的操作。舉例來說，當我們在命令式語言寫 ``x = 5``，這通常代表的是把 ``5`` 指定給 ``x`` 這變數。而且這邊 ``5`` 是一個 expression。
    
    如果你用函數語言的角度去思考，你可以把他想做是一個函數，接受一個狀態，並回傳結果跟新的狀態。那新的狀態代表所有已指定的值與新加入的變數。

這種改變狀態的計算，除了想做是一個接受狀態並回傳結果跟新狀態的函數外，也可以想做是具有 context 的值。
實際的值是結果。然而要得到結果，我們必須要給一個初始的狀態，才能得到結果跟最後的狀態。


### Stack and Stones

考慮現在我們要對一個堆疊的操作建立模型。你可以把東西推上堆疊頂端，或是把東西從頂端拿下來。如果你要的元素是在堆疊的底層的話，你必須要把他上面的東西都拿下來才能拿到他。

我們用一個 list 來代表我們的堆疊。而我們把 list 的頭當作堆疊的頂端。為了正確的建立模型，我們要寫兩個函數：``pop`` 跟 ``push``。``pop`` 會接受一個堆疊，取下一個元素並回傳一個新的堆疊，這個新的堆疊不包含取下的元素。``push`` 會接受一個元素，把他堆到堆疊中，並回傳一個新的堆疊，其包含這個新的元素。

```
type Stack = [Int]  
  
pop :: Stack -> (Int,Stack)  
pop (x:xs) = (x,xs)  

push :: Int -> Stack -> ((),Stack)  
push a xs = ((),a:xs)  
```

我們用 ``()`` 來當作 pushing 的結果，畢竟推上堆疊並不需要什麼回傳值，他的重點是在改變堆疊。注意到 ``push`` 跟 ``pop`` 都是改變狀態的計算，可以從他們的型態看出來。

我們來寫一段程式來模擬一個堆疊的操作。我們接受一個堆疊，把 ``3`` 推上去，然後取出兩個元素。

```
stackManip :: Stack -> (Int, Stack)  
stackManip stack = let  
    ((),newStack1) = push 3 stack  
    (a ,newStack2) = pop newStack1  
    in pop newStack2 
```

我們拿一個 ``stack`` 來作 ``push 3 stack`` 的動作，其結果是一個 tuple。tuple 的第一個部份是 ``()``，而第二個部份是新的堆疊，我們把他命名成 ``newStack1``。然後我們從 ``newStack1`` 上 pop 出一個數字。其結果是我們之前 push 上去的一個數字 ``a``，然後把這個更新的堆疊叫做 ``newStack2``。然後我們從 ``newStack2`` 上再 pop 出一個數字 ``b``，並得到 ``newStack3``。我們回傳一個 tuple 跟最終的堆疊。

```
ghci> stackManip [5,8,2,1]  
(5,[8,2,1])  
```

結果就是 ``5`` 跟新的堆疊 ``[8,2,1]``。注意到 ``stackManip`` 是一個會改變狀態的操作。我們把一堆會改變狀態的操作綁在一起操作，有沒有覺得很耳熟的感覺。

``stackManip`` 的程式有點冗長，因為我們要寫得太詳細，必須把狀態給每個操作，然後把新的狀態再餵給下一個。如果我們可以不要這樣作的話，那程式應該會長得像這樣：

``` 
stackManip = do  
    push 3  
    a <- pop  
    pop  
```


這就是 State Monad 在做的事。有了他，我們便可以免除於要把狀態操作寫得太明白的窘境。


### The State Monad

``Control.Monad.State`` 這個模組提供了一個 ``newtype`` 包起來的型態。

```
newtype State s a = State { runState :: s -> (a,s) }  
```

一個 ``State s a`` 代表的是一個改變狀態的操作，他操縱的狀態為型態 ``s``，而產生的結果是 ``a``。

我們已經見識過什麼是改變狀態的操作，以及他們是可以被看成具有 context 的值。接著來看看他們 ``Monad`` 的 instance：

```
instance Monad (State s) where  
    return x = State $ \s -> (x,s)  
    (State h) >>= f = State $ \s -> let (a, newState) = h s  
                                        (State g) = f a  
                                    in  g newState  
```

我們先來看看 ``return`` 那一行。我們 ``return`` 要作的事是接受一個值，並做出一個改變狀態的操作，讓他永遠回傳那個值。所以我們才做了一個 lambda 函數，``\s -> (x,s)``。我們把 ``x`` 當成是結果，並且狀態仍然是 ``s``。這就是 ``return`` 要完成的 minimal context。

[$../img/badge.png]

那 ``>>=`` 的實作呢？很明顯的把改變狀態的操作餵進 ``>>=`` 也必須要丟出另一個改變狀態的操作。所以我們用 ``State`` 這個 ``newtype`` wrapper 來把一個 lambda 函數包住。這個 lambda 會是新的一個改變狀態的操作。但裡面的內容是什麼？首先我們應該要從接受的操作取出結果。由於 lambda 是在一個大的操作中，所以我們可以餵給 ``h`` 我們現在的狀態，也就是 ``s``。那會產生 ``(a, newState)``。到目前為止每次我們在實作 ``>>=`` 的時候，我們都會先從 monadic value 中取出結果，然後餵給 ``f`` 來得到新的 monadic value。在寫 ``Writer`` 的時候，我們除了這樣作還要確保 context 是用 ``mappend`` 把舊的 monoid value 跟新的接起來。在這邊我們則是用 ``f a`` 得到一個新的操作 ``g``。現在我們有了新的操作跟新的狀態（叫做 ``newState``），我們就把 ``newState`` 餵給 ``g``。結果便是一個 tuple，裡面包含了最後的結果跟最終的狀態。


有了 ``>>=``，我們便可以把兩個操作黏在一起，只是第二個被放在一個函數中，專門接受第一個的結果。由於 ``pop`` 跟 ``push`` 已經是改變狀態的操作了，我們可以把他們包在 ``State`` 中

```
import Control.Monad.State  
  
pop :: State Stack Int  
pop = State $ \(x:xs) -> (x,xs)  

push :: Int -> State Stack ()  
push a = State $ \xs -> ((),a:xs)  
```

``pop`` 已經滿足我們的條件，而 ``push`` 要先接受一個 ``Int`` 才會回傳我們要的操作。所以我們可以改寫先前的範例如下：

```
import Control.Monad.State  
  
stackManip :: State Stack Int  
stackManip = do  
  push 3  
  a <- pop  
  pop  
```


看到我們是怎麼把一個 ``push`` 跟兩個 ``pop`` 黏成一個操作嗎？當我們將他們從一個 ``newtype`` 取出，其實就是需要一個能餵進初始狀態的函數：

```
ghci> runState stackManip [5,8,2,1]  
(5,[8,2,1])  
```

我們不須綁定第二個 ``pop``，因為我們根本不會用到 ``a``，所以可以寫成下面的樣子：

```
stackManip :: State Stack Int  
stackManip = do  
    push 3  
    pop  
    pop  
```

再來嘗試另外一種方式，先從堆疊上取下一個數字，看看他是不是 ``5``，如果是的話就把他放回堆疊上，如果不是的話就堆上 ``3`` 跟 ``8``。

```
stackStuff :: State Stack ()  
stackStuff = do  
    a <- pop  
    if a == 5  
        then push 5  
        else do  
            push 3  
            push 8 
```

很直覺吧！我們來看看初始的堆疊的樣子。

```
ghci> runState stackStuff [9,0,2,1,0]  
((),[8,3,0,2,1,0]) 
```

還記得我們說過 ``do`` 的結果會是一個 monadic value，而在 ``State`` monad 的 case，``do`` 也就是一個改變狀態的函數。而由於 ``stackManip`` 跟 ``stackStuff`` 都是改變狀態的計算，因此我們可以把他們黏在一起：

```
moreStack :: State Stack ()  
moreStack = do  
    a <- stackManip  
    if a == 100  
        then stackStuff  
        else return ()  
```

如果 ``stackManip`` 的結果是 ``100``，我們就會跑 ``stackStuff``，如果不是的話就什麼都不做。``return ()`` 不過就是什麼是都不做，全部保持原樣。

``Contorl.Monad.State`` 提供了一個 ``MonadState`` 的 typeclass，他有兩個有用的函數，分別是 ``get`` 跟 ``put``。對於 ``State`` 來說，``get`` 的實作就像這樣：

```
get = State $ \s -> (s,s)
```

他只是取出現在的狀態除此之外什麼也不做。而 ``put`` 函數會接受一個狀態並取代掉現有的狀態。

```
put newState = State $ \s -> ((),newState)  
```

有了這兩個狀態，我們便可以看到現在堆疊中有什麼，或是把整個堆疊中的元素換掉。

```
stackyStack :: State Stack ()  
stackyStack = do  
    stackNow <- get  
    if stackNow == [1,2,3]  
        then put [8,3,1]  
        else put [9,2,1]  
```

我們可以看看對於 ``State`` 而言，``>>=`` 的型態會是什麼：

```
(>>=) :: State s a -> (a -> State s b) -> State s b  
```

我們可以看到狀態的型態都是 ``s``，而結果從型態 ``a`` 變成型態 ``b``。這代表我們可以把好幾個改變狀態的計算黏在一起，這些計算的結果可以都不一樣，但狀態的型態會是一樣的。舉例來說，對於 ``Maybe`` 而言，``>>=`` 的型態會是：

```
(>>=) :: Maybe a -> (a -> Maybe b) -> Maybe b  
```

``Maybe`` 不變是有道理的，但如果用 ``>>=`` 來把兩種不同的 monad 接起來是沒道理的。但對於 state monad 而言，monad 其實是 ``State s``，所以如果 ``s`` 不一樣，我們就要用 ``>>=`` 來把兩個 monad 接起來。

### 隨機性與 state monad

在章節的一開始，我們知道了在 Haskell 中要產生亂數的不方便。我們要拿一個產生器，並回傳一個亂數跟一個新的產生器。接下來我們還一定要用新的產生器不可。但 State Monad 讓我們可以方便一些。

``System.Random`` 中的 ``random`` 函數有下列的型態：

```
random :: (RandomGen g, Random a) => g -> (a, g)  
```

代表他接受一個亂數產生器，並產生一個亂數跟一個新的產生器。很明顯他是一個會改變狀態的計算，所以我們可以用 ``newtype`` 把他包在一個 ``State`` 中，然後把他當作 monadic value 來操作。

```
import System.Random  
import Control.Monad.State  
  
randomSt :: (RandomGen g, Random a) => State g a  
randomSt = State random  
```

這樣我們要丟三個硬幣的結果可以改寫成這樣：

```
import System.Random  
import Control.Monad.State  
  
threeCoins :: State StdGen (Bool,Bool,Bool)  
threeCoins = do  
  a <- randomSt  
  b <- randomSt  
  c <- randomSt  
  return (a,b,c)  
```

``threeCoins`` 是一個改變狀態的計算，他接受一個初始的亂數產生器，他會把他餵給 ``randomSt``，他會產生一個數字跟一個新的產生器，然後會一直傳遞下去。我們用 ``return (a,b,c)`` 來呈現 ``(a,b,c)``，這樣並不會改變最近一個產生器的狀態。

```
ghci> runState threeCoins (mkStdGen 33)  
((True,False,True),680029187 2103410263)
```

要完成像這樣要改變狀態的任務便因此變得輕鬆了很多。

## Error Monad

我們知道 ``Maybe`` 是拿來賦予一個值具有可能失敗的 context。一個值可能會是 ``Just something`` 或是一個 ``Nothing``。儘管這很有用，但當我們拿到了一個 ``Nothing``，我們只知道他失敗了，但我們沒辦法塞進一些有用的資訊，告訴我們究竟是在什麼樣的情況下失敗了。

而 ``Either e a`` 則能讓我們可以加入一個可能會發生錯誤的 context，還可以增加些有用的訊息，這樣能讓我們知道究竟是什麼東西出錯了。一個 ``Either e a`` 的值可以是代表正確的 ``Right``，或是代表錯誤的 ``Left``，例如說：

```
ghci> :t Right 4  
Right 4 :: (Num t) => Either a t  
ghci> :t Left "out of cheese error"  
Left "out of cheese error" :: Either [Char] b  
```

這就像是加強版的 ``Maybe``，他看起來實在很像一個 monad，畢竟他也可以當作是一個可能會發生錯誤的 context，只是多了些訊息罷了。

在 ``Control.Monad.Error`` 裡面有他的 ``Monad`` instance。

```
instance (Error e) => Monad (Either e) where  
    return x = Right x   
    Right x >>= f = f x  
    Left err >>= f = Left err  
    fail msg = Left (strMsg msg)  
```

``return`` 就是建立起一個最小的 context，由於我們用 ``Right`` 代表正確的結果，所以他把值包在一個 ``Right`` constructor 裡面。就像實作 ``Maybe`` 時的 ``return`` 一樣。

``>>=`` 會檢查兩種可能的情況：也就是 ``Left`` 跟 ``Right``。如果進來的是 ``Right``，那我們就呼叫 ``f``，就像我們在寫 ``Just`` 的時候一樣，只是呼叫對應的函數。而在錯誤的情況下，``Left`` 會被傳出來，而且裡面保有描述失敗的值。


``Either e`` 的 ``Monad`` instance 有一項額外的要求，就是包在 ``Left`` 中的型態，也就是 ``e``，必須是 ``Error`` typeclass 的 instance。``Error`` 這個 typeclass 描述一個可以被當作錯誤訊息的型態。他定義了 ``strMsg`` 這個函數，他接受一個用字串表達的錯誤。一個明顯的範例就是 ``String`` 型態，當他是 ``String`` 的時候，``strMsg`` 只不過回傳他接受到的字串。

```
ghci> :t strMsg  
strMsg :: (Error a) => String -> a  
ghci> strMsg "boom!" :: String  
"boom!"  
```

但因為我們通常在用 ``Either`` 來描述錯誤的時候，是用 ``String`` 來裝錯誤訊息，所以我們也不用擔心這一點。當在 ``do`` 裡面做 pattern match 失敗的時候，``Left`` 的值會拿來代表失敗。

總之來看看一個範例吧：

```
ghci> Left "boom" >>= \x -> return (x+1)  
Left "boom"  
ghci> Right 100 >>= \x -> Left "no way!"  
Left "no way!" 
```


當我們用 ``>>=`` 來把一個 ``Left`` 餵進一個函數，函數的運算會被忽略而直接回傳丟進去的 ``Left`` 值。當我們餵 ``Right`` 值給函數，函數就會被計算而得到結果，但函數還是產生了一個 ``Left`` 值。

當我們試著餵一個 ``Right`` 值給函數，而且函數也成功地計算，我們卻碰到了一個奇怪的 type error。

```
ghci> Right 3 >>= \x -> return (x + 100)  
  
<interactive>:1:0:  
  Ambiguous type variable `a' in the constraints:  
    `Error a' arising from a use of `it' at <interactive>:1:0-33  
    `Show a' arising from a use of `print' at <interactive>:1:0-33  
  Probable fix: add a type signature that fixes these type variable(s)  
```

Haskell 警告說他不知道要為 ``e`` 選擇什麼樣的型態，儘管我們是要印出 ``Right`` 的值。這是因為 ``Error e`` 被限制成 ``Monad``。把 ``Either`` 當作 Monad 使用就會碰到這樣的錯誤，你只要明確寫出 type signature 就行了：

```
ghci> Right 3 >>= \x -> return (x + 100) :: Either String Int  
Right 103  
```

這樣就沒問題了。


撇除這個小毛病，把 ``Either`` 當 Monad 使用就像使用 ``Maybe`` 一樣。在前一章中，我們展示了 ``Maybe`` 的使用方式。你可以把前一章的範例用 ``Either`` 重寫當作練習。

## 一些實用的 Moandic functions

在這個章節，我們會看看一些操作 monadic value 的函數。這樣的函數通常我們稱呼他們為 monadic function。其中有些你是第一次見到，但有些不過是 ``filter`` 或 ``foldl`` 的變形。讓我們來看看吧！

### liftM 

[$../img/wolf.png]

當我們開始學習 Monad 的時候，我們是先學習 functors，他代表可以被 map over 的事物。接著我們學了 functors 的加強版，也就是 applicative functors，他可以對 applicative values 做函數的套用，也可以把一個一般值放到一個預設的 context 中。最後，我們介紹在 applicative functors 上更進一步的 monad，他讓這些具有 context 的值可以被餵進一般函數中。 

也就是說每一個 monad 都是個 applicative functor，而每一個 applicative functor 也都是一個 functor。``Applicative`` typeclass 中有加入限制，讓每一個 ``Applicative`` 都是 ``Functor``。但 ``Monad`` 卻沒有這樣的限制，讓每個 ``Monad`` 都是 ``Applicative``。這是因為 ``Monad`` 這個 typeclass 是在 ``Applicative`` 引入前就存在的緣故。

但即使每個 monad 都是一個 functor，但我們不需要依賴 ``Functor`` 的定義。那是因為我們有 ``liftM`` 這個函數。他會接受一個函數跟一個 monadic value，然後把函數 map over 那些 monadic value。所以他其實就是 ``fmap``，以下是他的型態：

```
liftM :: (Monad m) => (a -> b) -> m a -> m b  
```

而這是 ``fmap`` 的型態：

```
fmap :: (Functor f) => (a -> b) -> f a -> f b
```

如果 ``Functor`` 跟 ``Monad`` 的 instance 遵守 functor 跟 monad 的法則（到目前為止我們看過的 monad 都遵守），那這兩個函數其實是等價的。這就像 ``pure`` 跟 ``return`` 其實是同一件事，只是一個在 ``Applicative`` 中，而另外一個在 ``Monad`` 裡面，我們來試試看 ``liftM`` 吧：

```
ghci> liftM (*3) (Just 8)  
Just 24  
ghci> fmap (*3) (Just 8)  
Just 24  
ghci> runWriter $ liftM not $ Writer (True, "chickpeas")  
(False,"chickpeas")  
ghci> runWriter $ fmap not $ Writer (True, "chickpeas")  
(False,"chickpeas")  
ghci> runState (liftM (+100) pop) [1,2,3,4]  
(101,[2,3,4])  
ghci> runState (fmap (+100) pop) [1,2,3,4]  
(101,[2,3,4]) 
```

我們已經知道 ``fmap`` 是如何運作在 ``Maybe`` 上。而 ``liftM`` 又跟 ``fmap`` 等價。對於 ``Writer`` 型態的值而言，函數只有對他的第一個 component 做處理。而對於改變狀態的計算，``fmap`` 跟 ``liftM`` 也都是產生另一個改變狀態的計算。我們也看過了 ``(+100)`` 當作用在 ``pop`` 上會產生 ``(1, [2,3,4])``。

來看看 ``liftM`` 是如何被實作的：
```
liftM :: (Monad m) => (a -> b) -> m a -> m b  
liftM f m = m >>= (\x -> return (f x)) 
```

或者用 ``do`` 來表示得清楚些

```
liftM :: (Monad m) => (a -> b) -> m a -> m b  
liftM f m = do  
    x <- m  
    return (f x)  
```

我們餵一個 monadic value ``m`` 給函數，我們套用那個函數然後把結果放進一個預設的 context。由於遵守 monad laws，這保證這操作不會改變 context，只會呈現最後的結果。我們可以看到實作中 ``liftM`` 也沒有用到 ``Functor`` 的性質。這代表我們能只用 monad 提供給我們的就實作完 ``fmap``。這特性讓我們可以得到 monad 比 functor 性質要強的結論。

``Applicative`` 讓我們可以操作具有 context 的值就像操作一般的值一樣。
就像這樣：

```
ghci> (+) <$> Just 3 <*> Just 5  
Just 8  
ghci> (+) <$> Just 3 <*> Nothing  
Nothing  
```

使用 applicative 的特性讓事情變得很精簡。
``<$>`` 不過就是 ``fmap``，而 ``<*>`` 只是一個具有下列型態的函數：

```
(<*>) :: (Applicative f) => f (a -> b) -> f a -> f b  
```

他有點像 ``fmap``，只是函數本身有一個 context。我們必須把他從 context 中抽出，對 ``f a`` 做 map over 的東做，然後再放回 context 中。由於在 Haskel 中函數預設都是 curried，我們便能用 ``<$>`` 以及 ``<*>`` 來讓接受多個參數的函數也能接受 applicative 種類的值。

總之 ``<*>`` 跟 ``fmap`` 很類似，他也能只用 ``Monad`` 保證的性質實作出來。``ap`` 這個函數基本上就是 ``<*>``，只是他是限制在 ``Monad`` 上而不是 ``Applicative`` 上。這邊是他的定義：

```
ap :: (Monad m) => m (a -> b) -> m a -> m b  
ap mf m = do  
    f <- mf  
    x <- m  
    return (f x)  
```


``mf`` 是一個 monadic value，他的結果是一個函數。由於函數跟值都是放在 context 中，假設我們從 context 取出的函數叫 ``f``，從 context 取出的值叫 ``x``，我們把 ``x`` 餵給 ``f`` 然後再把結果放回 context。像這樣：

```
ghci> Just (+3) <*> Just 4  
Just 7  
ghci> Just (+3) `ap` Just 4  
Just 7  
ghci> [(+1),(+2),(+3)] <*> [10,11]  
[11,12,12,13,13,14]  
ghci> [(+1),(+2),(+3)] `ap` [10,11]  
[11,12,12,13,13,14]  
```


由於我們能用 ``Monad`` 提供的函數實作出 ``Applicative`` 的函數，因此我們看到 Monad 有比 applicative 強的性質。事實上，當我們知道一個型態是 monad 的時候，大多數會先定義出 ``Monad`` 的 instance，然後才定義 ``Applicative`` 的 instance。而且只要把 ``pure`` 定義成 ``return``，``<*>`` 定義成 ``ap`` 就行了。同樣的，如果你已經有了 ``Monad`` 的 instance，你也可以簡單的定義出 ``Functor``，只要把 ``fmap`` 定義成 ``liftM`` 就行了。

``liftA2`` 是一個方便的函數，他可以把兩個 applicative 的值餵給一個函數。他的定義很簡單：

```
liftA2 :: (Applicative f) => (a -> b -> c) -> f a -> f b -> f c  
liftA2 f x y = f <$> x <*> y  
```

``liftM2`` 也是做差不多的事情，只是多了 ``Monad`` 的限制。在函式庫中其實也有 ``liftM3``，``liftM4`` 跟 ``liftM5``。

我們看到了 monad 相較於 applicative 跟 functor 有比較強的性質。儘管 moand 有 functor 跟 applicative functor 的性質，但他們不見得有 ``Functor`` 跟 ``Applicative`` 的 instance 定義。所以我們檢視了一些在 monad 中定義，且等價於 functor 或 applicative functor 所具有的函數。


### The join function

如果一個 monadic value 的結果是另一個 monadic value，也就是其中一個 monadic value 被包在另一個裡面，你能夠把他們變成一個普通的 monadic value 嗎？就好像把他們打平一樣。譬如說，我們有 ``Just (Just 9)``，我們能夠把他變成 ``Just 9`` 嗎？事實上是可以的，這也是 monad 的一個性質。也就是我要看的 ``join`` 函數，他的型態是這樣：

```
join :: (Monad m) => m (m a) -> m a  
```

他接受一個包在另一個 monadic value 中的 monadic value，然後會回給我們一個普通的 monadic value。這邊有一些 ``Maybe`` 的範例：

```
ghci> join (Just (Just 9))  
Just 9  
ghci> join (Just Nothing)  
Nothing  
ghci> join Nothing  
Nothing  
```

第一行是一個計算成功的結果包在另一個計算成功的結果，他們應該要能結合成為一個比較大的計算成功的結果。第二行則是一個 ``Nothing`` 包在一個 ``Just`` 中。我們之前在處理 ``Maybe`` 型態的值時，會用 ``<*>`` 或 ``>>=`` 把他們結合起來。輸入必須都是 ``Just`` 時結果出來才會是 ``Just``。如果中間有任何的失敗，結果就會是一個失敗的結果。而第三行就是這樣，我們嘗試把失敗的結果接合起來，結果也會是一個失敗。

要 ``join`` 一個 list 也是很簡單：

```
ghci> join [[1,2,3],[4,5,6]]  
[1,2,3,4,5,6]  
```

你可以看到，對於 list 而言 ``join`` 不過就是 ``concat``。
而要 ``join`` 一個包在 ``Writer`` 中的 ``Writer``，
我們必須用 ``mappend``：

```
ghci> runWriter $ join (Writer (Writer (1,"aaa"),"bbb"))  
(1,"bbbaaa")  
```

``"bbb"`` 先被加到 monoid 中，接著 ``"aaa"`` 被附加上去。你想要檢視 ``Writer`` 中的值的話，必須先把值寫進去才行。

要對 ``Either`` 做 ``join`` 跟對 ``Maybe`` 做 ``join`` 是很類似的：

```
ghci> join (Right (Right 9)) :: Either String Int  
Right 9  
ghci> join (Right (Left "error")) :: Either String Int  
Left "error"  
ghci> join (Left "error") :: Either String Int  
Left "error"  
```

如果我們對一個包了另外一個改變狀態的計算的進行改變狀態的計算，要作 ``join`` 的動作會讓外面的先被計算，然後才是計算裡面的：

```
ghci> runState (join (State $ \s -> (push 10,1:2:s))) [0,0,0]  
((),[10,1,2,0,0,0])  
```

這邊的 lambda 函數接受一個狀態，並把 ``2`` 跟 ``1`` 放到堆疊中，並把 ``push 10`` 當作他的結果。當對整個東西做 ``join`` 的時候，他會先把 ``2`` 跟 ``1`` 放到堆疊上，然後進行 ``push 10`` 的計算，因而把 ``10`` 放到堆疊的頂端。

``join`` 的實作像是這樣：

```
join :: (Monad m) => m (m a) -> m a  
join mm = do  
    m <- mm  
    m  
```

因為 ``mm`` 的結果會是一個 monadic value，我們單獨用 ``m <- mm`` 拿取他的結果。這也可以說明 ``Maybe`` 只有當外層跟內層的值都是 ``Just`` 的時候才會是 ``Just``。如果把 ``mm`` 的值設成 ``Just (Just 8)`` 的話，他看起來會是這樣：

```
joinedMaybes :: Maybe Int  
joinedMaybes = do  
    m <- Just (Just 8)  
    m  
```

[$../img/tipi.png]

最有趣的是對於一個 monadic value 而言，用 ``>>=`` 把他餵進一個函數其實等價於對 monad 做 mapping over 的動作，然後用 ``join`` 來把值從 nested 的狀態變成扁平的狀態。也就是說 ``m >>= f`` 其實就是 ``join (fmap f m)``。如果你仔細想想的話其實很明顯。``>>=`` 的使用方式是，把一個 monadic value 餵進一個接受普通值的函數，但他卻會回傳 monadic value。如果我們 map over 一個 monadic value，我們會做成一個 monadic value 包了另外一個 monadic value。例如說，我們現在手上有 ``Just 9`` 跟 ``\x -> Just (x+1)``。如果我們把這個函數 map over ``Just 9``，我們會得到 ``Just (Just 10)``

事實上 ``m >>= f`` 永遠等價於 ``join (fmap f m)`` 這性質非常有用。如果我們要定義自己的 ``Monad`` instance，要知道怎麼把 nested monadic value 變成扁平比起要定義 ``>>=`` 是比較容易的一件事。


### filterM

``filter`` 函數是 Haskell 中不可或缺的要素。他接受一個斷言(predicate)跟一個 list 來過濾掉斷言為否的部份並回傳一個新的 list。他的型態是這樣：

```
filter :: (a -> Bool) -> [a] -> [a]  
```

predicate 能接 list 中的一個元素並回傳一個 ``Bool`` 型態的值。但如果 ``Bool`` 型態其實是一個 monadic value 呢？也就是他有一個 context。例如說除了 ``True`` 跟 ``False`` 之外還伴隨一個 monoid，像是 ``["Accepted the number 5"]``，或 ``["3 is too small"]``。照前面所學的聽起來是沒問題，而且產出的 list 也會跟隨 context，在這個例子中就是 log。所以如果 ``Bool`` 會回傳伴隨 context 的布林值，我們會認為最終的結果也會具有 context。要不然這些 context 都會在處理過程中遺失。

在 ``Control.Monad`` 中的 ``filterM`` 函數正是我們所需要的，他的型態如下：

```
filterM :: (Monad m) => (a -> m Bool) -> [a] -> m [a]  
```

predicate 會回傳一個 monadic value，他的結果會是 ``Bool`` 型態，由於他是 monadic value，他的 context 有可能會是任何 context，譬如說可能的失敗，non-determinism，甚至其他的 context。一旦我們能保證 context 也會被保存在最後的結果中，結果也就是一個 monadic value。


我們來寫一個接受 list 然後過濾掉小於 4 的函數。先嘗試使用 ``filter`` 函數：

```
ghci> filter (\x -> x < 4) [9,1,5,2,10,3]  
[1,2,3] 
```

很簡單吧。接著我們在做個 predicate，除了表達 ``True`` 或 ``False`` 之外，還提供了一個 log。我們會用 ``Writer`` monad 來表達這件事：

```
keepSmall :: Int -> Writer [String] Bool  
keepSmall x  
    | x < 4 = do  
        tell ["Keeping " ++ show x]  
        return True  
    | otherwise = do  
        tell [show x ++ " is too large, throwing it away"]  
        return False  
```

這個函數會回傳 ``Writer [String] Bool`` 而不是一個單純的 ``Bool``。他是一個 monadic predicate。如果掃到的數字小於 ``4`` 的話，我們就會回報要保存他，而且回傳 ``return True``。

接著，我們把他跟一個 list 餵給 ``filterM``。由於 predicate 會回傳 ``Writer``，所以結果仍會是一個 ``Writer`` 值。

```
ghci> fst $ runWriter $ filterM keepSmall [9,1,5,2,10,3]  
[1,2,3]  
```

要檢查 ``Writer`` 的結果，我們想要印出 log 看看裡面有什麼東西：

```
ghci> mapM_ putStrLn $ snd $ runWriter $ filterM keepSmall [9,1,5,2,10,3]  
9 is too large, throwing it away  
Keeping 1  
5 is too large, throwing it away  
Keeping 2  
10 is too large, throwing it away  
Keeping 3  
```

提供 monadic predicate 給 ``filterM``，我們便能夠做 filter 的動作，同時還能保有 monadic context。

一個比較炫的技巧是用 ``filterM`` 來產生一個 list 的 powerset。一個 powerset 就是一個集合所有子集所形成的集合。如果說我們的 list 是 ``[1,2,3]``，那他個 powerset 就會是：

```
[1,2,3]  
[1,2]  
[1,3]  
[1]  
[2,3]  
[2]  
[3]  
[]  
```

換句話說，要產生一個 powerset 就是要列出所有要丟掉跟保留的組合。``[2,3]`` 只不過代表我們把 ``1`` 給丟掉而已。


我們要依賴 non-determinism 來寫我們這產生 powerset 的函數。我們接受一個 list ``[1,2,3]`` 然後查看第一個元素，這個例子中是 ``1``，我們會問：我們要保留他呢？還是丟掉他呢？答案是我們都要做。所以我們會用一個 non-determinism 的 predicate 來過濾我的 list。也就是我們的 ``powerset`` 函數：

```
powerset :: [a] -> [[a]]  
powerset xs = filterM (\x -> [True, False]) xs 
```

等等，我們已經寫完了嗎？沒錯，就這麼簡單，我們可以同時丟掉跟保留每個元素。只要我們用 non-deterministic predicate，那結果也就是一個 non-deterministic value，也便是一個 list 的 list。試著跑跑看：

```
ghci> powerset [1,2,3]  
[[1,2,3],[1,2],[1,3],[1],[2,3],[2],[3],[]]  
```

這樣的寫法需要讓你好好想一下，但如果你能接受 list 其實就是 non-deterministic value 的話，那要想通會比較容易一些。


### foldM

``foldl`` 的 monadic 的版本叫做 ``foldM``。如果你還有印象的話，``foldl`` 會接受一個 binary 函數，一個起始累加值跟一串 list，他會從左邊開始用 binary 函數每次帶進一個值來 fold。``foldM`` 也是做同樣的事，只是他接受的這個 binary 函數會產生 monadic value。不意外的，他的結果也會是 monadic value。``foldl`` 的型態是：

```
foldl :: (a -> b -> a) -> a -> [b] -> a 
```

而 ``foldM`` 的型態則是：

```
foldM :: (Monad m) => (a -> b -> m a) -> a -> [b] -> m a  
```

binary 函數的回傳值是 monadic，所以結果也會是 monadic。我們來試著把 list 的值用 fold 全部加起來：

```
ghci> foldl (\acc x -> acc + x) 0 [2,8,3,1]  
14  
```

這邊起始的累加值是 ``0``，首先 ``2`` 會被加進去，變成 ``2``。然後 ``8`` 被加進去變成 ``10``，直到我們沒有值可以再加，那便是最終的結果。

但如果我們想額外加一個條件，也就是當碰到一個數字大於 ``9`` 時候，整個運算就算失敗呢？一種合理的修改就是用一個 binary 函數，他會檢查現在這個數是否大於 ``9``，如果是便引發失敗，如果不是就繼續。由於有失敗的可能性，我們便需要這個 binary 函數回傳一個 ``Maybe``，而不是一個普通的值。我們來看看這個函數：

```
binSmalls :: Int -> Int -> Maybe Int  
binSmalls acc x  
    | x > 9     = Nothing  
    | otherwise = Just (acc + x)  
```

由於這邊的 binary 函數是 monadic function，我們不能用普通的 ``foldl``，我們必須用 ``foldM``：

```
ghci> foldM binSmalls 0 [2,8,3,1]  
Just 14  
ghci> foldM binSmalls 0 [2,11,3,1]  
Nothing  
```

由於這串 list 中有一個數值大於 ``9``，所以整個結果會是 ``Nothing``。另外你也可以嘗試 fold 一個回傳 ``Writer`` 的 binary 函數，他會在 fold 的過程中紀錄你想紀錄的資訊。


### Making a safe RPN calculator

[^../img/miner.png]

之前的章節我們實作了一個 RPN 計算機，但我們沒有做錯誤的處理。他只有在輸入是合法的時候才會運算正確。假如有東西出錯了，整個程式便會當掉。我們在這章看到了要怎樣把程式碼轉換成 monadic 的版本，我們先嘗適用 ``Maybe`` monad 來幫我們的 RPN 計算機加上些錯誤處理。

我們的 RPN 計算機接受一個像 ``"1 3 + 2 *"`` 這樣的字串，把他斷成 word，變成 ``["1","3","+","2","*"]`` 這樣。然後用一個 binary 函數，跟一個空的堆疊，從左邊開始或是將數值推進堆疊中，或是操作堆疊最上層的兩個元素。

以下便是程式的核心部份：

```
import Data.List  
  
solveRPN :: String -> Double  
solveRPN = head . foldl foldingFunction [] . words  
```

我們把輸入變成一個字串的 list，從左邊開始 fold，當堆疊中只剩下一個元素的時候，他便是我們要的答案。以下是我們的 folding 函數：

```
foldingFunction :: [Double] -> String -> [Double]  
foldingFunction (x:y:ys) "*" = (x * y):ys  
foldingFunction (x:y:ys) "+" = (x + y):ys  
foldingFunction (x:y:ys) "-" = (y - x):ys  
foldingFunction xs numberString = read numberString:xs  
```

這邊我們的累加元素是一個堆疊，我們用一個 ``Double`` 的 list 來表示他。當我們在做 folding 的過程，如果當前的元素是一個 operator，他會從堆疊上拿下兩個元素，用 operator 施行運算然後把結果放回堆疊。如果當前的元素是一個表示成字串的數字，他會把字串轉換成數字，並回傳一個新的堆疊包含了轉換後的數字。

我們首先把我們的 folding 函數加上處理錯誤的 case，所以他的型態會變成這樣：

```
foldingFunction :: [Double] -> String -> Maybe [Double]  
```

他不是回傳一個 ``Just`` 的堆疊就是回傳 ``Nothing``。

``reads`` 函數就像 ``read`` 一樣，差別在於他回傳一個 list。在成功讀取的情況下 list 中只包含讀取的那個元素。如果他失敗了，他會回傳一個空的 list。除了回傳讀取的元素，他也回傳剩下讀取失敗的元素。他必須要看完整串輸入，我們想把他弄成一個 ``readMaybe`` 的函數，好方便我們進行。

```
readMaybe :: (Read a) => String -> Maybe a  
readMaybe st = case reads st of [(x,"")] -> Just x  
                                _ -> Nothing  
```

測試結果如下：

```
ghci> readMaybe "1" :: Maybe Int  
Just 1  
ghci> readMaybe "GO TO HELL" :: Maybe Int  
Nothing  
```

看起來運作正常。我們再把他變成一個可以處理失敗情況的 monadic 函數

```
foldingFunction :: [Double] -> String -> Maybe [Double]  
foldingFunction (x:y:ys) "*" = return ((x * y):ys)  
foldingFunction (x:y:ys) "+" = return ((x + y):ys)  
foldingFunction (x:y:ys) "-" = return ((y - x):ys)  
foldingFunction xs numberString = liftM (:xs) (readMaybe numberString)  
```

前三種 case 跟前面的很像，只差在堆疊現在是包在 ``Just`` 裡面（我們常常是用 ``return`` 來做到這件事，但其實我們也可以用 ``Just``）。在最後一種情況，我們用 ``readMaybe numberString`` 然後我們用 ``(:xs)`` map over 他。所以如果堆疊 ``xs`` 是 ``[1.0,2.0]`` 且 ``readMaybe numberString`` 產生 ``Just 3.0``，那結果便是 ``Just [3.0,1.0,2.0]``。如果 ``readyMaybe numberString`` 產生 ``Nothing`` 那結果便是 ``Nothing``。我們來試著跑跑看 folding 函數

```
ghci> foldingFunction [3,2] "*"  
Just [6.0]  
ghci> foldingFunction [3,2] "-"  
Just [-1.0]  
ghci> foldingFunction [] "*"  
Nothing  
ghci> foldingFunction [] "1"  
Just [1.0]  
ghci> foldingFunction [] "1 wawawawa"  
Nothing  
```

看起來正常運作。我們可以用他來寫一個新的 ``solveRPN``。

```
import Data.List  
  
solveRPN :: String -> Maybe Double  
solveRPN st = do  
  [result] <- foldM foldingFunction [] (words st)  
  return result  
```

我們仍是接受一個字串把他斷成一串 word。然後我們用一個空的堆疊來作 folding 的動作，只差在我們用的是 ``foldM`` 而不是 ``foldl``。``foldM`` 的結果會是 ``Maybe``，``Maybe`` 裡面包含了一個只有一個元素的 list。我們用 ``do`` expression 來取出值，把他綁定到 ``result`` 上。當 ``foldM`` 回傳 ``Nothing`` 的時候，整個結果就變成 ``Nothing``。也特別注意我們有在 ``do`` 裡面做 pattern match 的動作，所以如果 list 中不是只有一個元素的話，最後結果便會是 ``Nothing``。最後一行我們用 ``return result`` 來展示 RPN 計算的結果，把他包在一個 ``Maybe`` 裡面。

```
ghci> solveRPN "1 2 * 4 +"  
Just 6.0  
ghci> solveRPN "1 2 * 4 + 5 *"  
Just 30.0  
ghci> solveRPN "1 2 * 4"  
Nothing  
ghci> solveRPN "1 8 wharglbllargh"  
Nothing  
```

第一個例子會失敗是因為 list 中不是只有一個元素，所以 ``do`` 裡面的 pattern matching 失敗了。第二個例子會失敗是因為 ``readMaybe`` 回傳了 ``Nothing``。

### Composing monadic functions

當我們介紹 monad law 的時候，我們說過 ``<=<`` 就像是函數合成一樣，只差在一個是作用在普通函數 ``a -> b``。一個是作用在 monadic 函數 ``a -> m b``。

```
ghci> let f = (+1) . (*100)  
ghci> f 4  
401  
ghci> let g = (\x -> return (x+1)) <=< (\x -> return (x*100))  
ghci> Just 4 >>= g  
Just 401  
```

在這個例子中我們合成了兩個普通的函數，並餵給給他 ``4``。我們也合成了兩個 monadic 函數並用 ``>>=`` 餵給他 ``Just 4``。

如果我們在 list 中有一大堆函數，我們可以把他們合成一個巨大的函數。用 ``id`` 當作累加的起點，``.`` 當作 binary 函數，用 fold 來作這件事。

```
ghci> let f = foldr (.) id [(+1),(*100),(+1)]  
ghci> f 1  
201  
```

``f`` 接受一個數字，然後會幫他加 ``1``，乘以 ``100``，再加 ``1``。我們也可以將 monadic 函數用同樣的方式做合成，只是不用 ``.`` 而用 ``<=<``，不用 ``id`` 而用 ``return``。我們不需要 ``foldM``，由於 ``<=<`` 只用 ``foldr`` 就足夠了。

當我們在之前的章節介紹 list monad 的時候， 我們用他來解決一個騎士是否能在三步內走到另一點的問題。 那個函數叫做 ``moveKnight``， 他接受一個座標然後回傳所有可能的下一步。 然後產生出所有可能三步的移動。

```
in3 start = return start >>= moveKnight >>= moveKnight >>= moveKnight   
```

要檢查我們是否能只用三步從 ``start`` 走到 ``end``，我們用下列函數

```
canReachIn3 :: KnightPos -> KnightPos -> Bool  
canReachIn3 start end = end `elem` in3 start  
```

如果使用 monadic 版本的合成的話，我們也可以做一個類似的 ``in3``，但我們希望他不只有三步的版本，而希望有任意步的版本。如果你仔細觀察 ``in3``，他只不過用 ``>>=`` 跟 ``moveKnight`` 把之前所有可能結果餵到下一步。把他一般化，就會像下面的樣子：

```
import Data.List  
  
inMany :: Int -> KnightPos -> [KnightPos]  
inMany x start = return start >>= foldr (<=<) return (replicate x moveKnight)  
```

首先我們用 ``replicate`` 來做出一個 list，裡面有 ``x`` 份的 ``moveKnight``。然後我們把所有函數都合成起來，就會給我們從起點走 ``x`` 步內所有可能的的位置。然後我們只需要把起始位置餵給他就好了。

我們也可以一般化我們的 ``canReachIn3``：

```
canReachIn :: Int -> KnightPos -> KnightPos -> Bool  
canReachIn x start end = end `elem` inMany x start  
```


## 定義自己的 Monad

[../img/spearhead.png]

在這一章節，我們會帶你看看究竟一個型態是怎麼被辨認，確認是一個 monad 而且正確定義出 ``Monad`` 的 instance。我們通常不會為了定義 monad 而定義。比較常發生的是，我們想要針對一個問題建立模型，卻稍後發現我們定義的型態其實是一個 Monad，所以就定義一個 ``Monad`` 的 instance。

正如我們看到的，list 是被拿來當作 non-deterministic values。對於 ``[3,5,9]``，我們可以看作是一個 non-deterministic value，我們不能知道究竟是哪一個。當我們把一個 list 用 ``>>=`` 餵給一個函數，他就是把一串可能的選擇都丟給函數，函數一個個去計算在那種情況下的結果，結果也便是一個 list。

如果我們把 ``[3,5,9]`` 看作是 ``3``,``5``,``9`` 各出現一次，但這邊沒有每一種數字出現的機率。如果我們把 non-deterministic 的值看作是 ``[3,5,9]``，但 ``3`` 出現的機率是 50%，``5`` 跟 ``9`` 出現的機率各是 25%呢？我們來試著用 Haskell 描述看看。

如果說 list 中的每一個元素都伴隨著他出現的機率。那下面的形式就蠻合理的：

```
[(3,0.5),(5,0.25),(9,0.25)]  
```

在數學上，機率通常不是用百分比表示，而是用介於 0 跟 1 的實數表示。0 代表不可能會發生，而 1 代表絕對會發生。但浮點數很有可能很快隨著運算失去精準度，所以 Haskell 有提供有理數。他的型態是擺在 ``Data.Ratio`` 中，叫做 ``Rational``。要創造出一個 ``Rational``，我們會把他寫成一個分數的形式。分子跟分母用 ``%`` 分隔。這邊有幾個例子：

```
ghci> 1%4  
1 % 4  
ghci> 1%2 + 1%2  
1 % 1  
ghci> 1%3 + 5%4  
19 % 12  
```

第一行代表四分之一，第二行代表兩個二分之一加起來變成一。而第三行我們把三分之一跟四分之五加起來變成十二分之十九。所以我們來用 ``Rational`` 取代浮點數來當作我們的機率值吧。

```
ghci> [(3,1%2),(5,1%4),(9,1%4)]  
[(3,1 % 2),(5,1 % 4),(9,1 % 4)]  
```

所以 ``3`` 有二分之一的機會出現，而 ``5`` 跟 ``9`` 有四分之一的機會出現。


可以看到我們幫 list 加上了一些額外的 context。再我們繼續深入之前，我們用一個 ``newtype`` 把他包起來，好讓我們幫他寫 instance。

```
import Data.Ratio

newtype Prob a = Prob { getProb :: [(a,Rational)] } deriving Show  
```

接著我們想問，這是一個 functor 嗎？list 是一個 functor，所以很有可能他也是一個 functor，畢竟我們只是在 list 上多加一些東西而已。在 list 的情況下，我們可以針對每個元素用函數做處理。這邊我們也是用函數針對每個元素做處理，只是我們是輸出機率值。所以我們就來寫個 functor 的 instance 吧。

```
instance Functor Prob where  
    fmap f (Prob xs) = Prob $ map (\(x,p) -> (f x,p)) xs 
```

我們可以用 pattern matching 的方式把 ``newtype`` 解開來，套用函數 ``f`` 之後再包回去。過程中不會動到機率值。

```
ghci> fmap negate (Prob [(3,1%2),(5,1%4),(9,1%4)])  
Prob {getProb = [(-3,1 % 2),(-5,1 % 4),(-9,1 % 4)]}  
```

要注意機率的和永遠是 ``1``。如果我們沒有漏掉某種情形的話，沒有道理他們加起來的值不為 ``1``。一個有 75% 機率是正面以及 50% 機率是反面的硬幣根本沒什麼道理。

接著要問一個重要的問題，他是一個 monad 嗎？我們知道 list 是一個 monad，所以他很有可能也是一個 monad。首先來想想 ``return``。他在 list 是怎麼運作的？他接受一個普通的值並把他放到一個 list 中變成只有一個元素的 list。那在這邊又如何？由於他是一個最小的 context，他也應該是一個元素的 list。那機率值呢？``return x`` 的值永遠都是 ``x``，所以機率值不應該是 ``0``，而應該是 ``1``。


至於 ``>>=`` 呢？看起來有點複雜，所以我們換種方式來思考，我們知道 ``m >>= f`` 會等價於 ``join (fmap f m)``，所以我們來想要怎麼把一串包含 probability list 的 list 弄平。舉個例子，考慮一個 list，``'a'`` 跟 ``'b'`` 恰出現其中一個的機率為 25%，兩個出現的機率相等。而 ``'c'`` 跟 ``'d'`` 恰出現其中一個的機率為 75%，兩個出現的機率也是相等。這邊有一個圖將情形畫出來。


[^../img/prob.png]

每個字母發生的機率有多高呢？如果我們用四個盒子來代表每個字母，那每個盒子的機率為何？每個盒子的機率是他們所裝有的機率值相乘的結果。``'a'`` 的機率是八分之一，``'b'`` 同樣也是八分之一。八分之一是因為我們把二分之一跟四分之一相乘得到的結果。而 ``'c'`` 發生的機率是八分之三，是因為二分之一乘上四分之三。``'d'`` 同樣也是八分之三。如果把所有的機率加起來，就會得到一，符合機率的規則。

來看看怎麼用一個 list 表達我們要說明的東西：

```
thisSituation :: Prob (Prob Char)  
thisSituation = Prob  
    [( Prob [('a',1%2),('b',1%2)] , 1%4 )  
    ,( Prob [('c',1%2),('d',1%2)] , 3%4 )  
    ]
```

注意到這邊的型態是 ``Prob (Prob Char)``。所以我們要思考的是如何把一串包含機率 list 的 list 打平。如果能成功寫出這樣的邏輯，``>>=`` 不過就是 ``join (fmap f m)``，我們便得到了一個 monad。我們這邊寫了一個 ``flatten`` 來做這件事。

```
flatten :: Prob (Prob a) -> Prob a  
flatten (Prob xs) = Prob $ concat $ map multAll xs  
    where multAll (Prob innerxs,p) = map (\(x,r) -> (x,p*r)) innerxs  
```

``multAll`` 接受一個 tuple，裡面包含一個 probability list 跟一個伴隨的機率值 ``p``，所以我們要作的事是把 list 裡面的機率值都乘以 ``p``，並回傳一個新的 tuple 包含新的 list 跟新的機率值。我們將 ``multAll`` map over 到我們的 probability list 上，我們就成功地打平了我們的 list。

現在我們就能定義我們的 ``Monad`` instance。

```
instance Monad Prob where  
    return x = Prob [(x,1%1)]  
    m >>= f = flatten (fmap f m)  
    fail _ = Prob []  
```

[$../img/ride.png]

由於我們已經把所有苦工的做完了，定義這個 instance 顯得格外輕鬆。我們也定義了 ``fail``，我們定義他的方式跟定義 list 一樣。如果在 ``do`` 中發生了失敗的 pattern match，那就會呼叫 ``fail``。

檢查我們定義的 instance 是否遵守 monad law 也是很重要的。monad law 的第一個定律是 ``return x >>= f`` 應該要等價於 ``f x``。要寫出嚴格的證明會很麻煩，但我們可以觀察到下列事實：首先用 ``return`` 做一個最小的 context，然後用 ``fmap`` 將一個函數 map over 這個 context，再將他打平。這樣做出來的 probability list，每一個機率值都相當於將我們最初放到 minimal context 中的值乘上 ``1%1``。同樣的邏輯，也可以看出 ``m >>= return`` 是等價於 ``m``。第三個定律是 ``f <=< (g <=< h)`` 應該要等價於 ``(f <=< g) <=< h``。我們可以從乘法有結合律的性質，以及 list monad 的特性上推出 probability monad 也符合這個定律。``1%2 * (1%3 * 1%5)`` 等於 ``(1%2 * 1%3) * 1%5``。

現在我們有了一個 monad，這樣有什麼好處呢？他可以幫助我們計算機率值。我們可以把機率事件看作是具有 context 的 value，而 probability monad 可以保證機率值能正確地被計算成最終的結果。

好比說我們現在有兩個普通的硬幣以及一個灌鉛的硬幣。灌鉛的硬幣十次中有九次會出現正面，只有一次會出現反面。如果我們一次丟擲這三個硬幣，有多大的機會他們都會出現正面呢？讓我們先來表達丟擲硬幣這件事，分別丟的是灌鉛的跟普通的硬幣。


```
data Coin = Heads | Tails deriving (Show, Eq)  

coin :: Prob Coin  
coin = Prob [(Heads,1%2),(Tails,1%2)]  

loadedCoin :: Prob Coin  
loadedCoin = Prob [(Heads,1%10),(Tails,9%10)]  
```

最後，來看看擲硬幣的函數：

```
import Data.List (all)  
  
flipThree :: Prob Bool  
flipThree = do  
  a <- coin  
  b <- coin  
  c <- loadedCoin  
  return (all (==Tails) [a,b,c])  
```

試著跑一下的話，我們會看到儘管我們用了不公平的硬幣，三個反面的機率還是不高。

```
ghci> getProb flipThree  
[(False,1 % 40),(False,9 % 40),(False,1 % 40),(False,9 % 40),  
 (False,1 % 40),(False,9 % 40),(False,1 % 40),(True,9 % 40)]  
```

同時出現正面的機率是四十分之九，差不多是 25%的機會。我們的 monad 並沒有辦法 join 所有都是 ``False`` 的情形，也就是所有硬幣都是出現反面的情況。不過那不是個嚴重的問題，可以寫個函數來將同樣的結果變成一種結果，這就留給讀者當作習題。

在這章節中，我們從提出問題到真的寫出型態，並確認這個型態是一個 monad，寫出他的 instance 並實際操作他。這是個很棒的經驗。現在讀者們應該對於 monad 有不少的了解才是。
