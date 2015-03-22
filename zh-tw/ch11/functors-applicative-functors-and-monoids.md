# Functors, Applicative Functors 與 Monoids

Haskell 的一些特色，像是純粹性，高階函數，algebraic data types，typeclasses，這些讓我們可以從更高的角度來看到 polymorphism 這件事。不像 OOP 當中需要從龐大的型態階層來思考。我們只需要看看手邊的型態的行為，將他們跟適當地 typeclass 對應起來就可以了。像 ``Int`` 的行為跟很多東西很像。好比說他可以比較相不相等，可以從大到小排列，也可以將他們一一窮舉出來。

Typeclass 的運用是很隨意的。我們可以定義自己的資料型態，然後描述他可以怎樣被操作，跟 typeclass 關聯起來便定義了他的行為。由於 Haskell 強大的型態系統，這讓我們只要讀函數的型態宣告就可以知道很多資訊。typeclass 可以定義得很抽象很 general。我們之前有看過 typeclass 定義了可以比較兩個東西是否相等，或是定義了可以比較兩個東西的大小。這些是既抽象但又描述簡潔的行為，但我們不會認為他們有什麼特別之處，因為我們時常碰到他們。最近我們看過了 functor，基本上他們是一群可以被 map over 的物件。這是其中一個例子能夠抽象但又漂亮地描述行為。在這一章中，我們會詳加闡述 functors，並會提到比較強一些的版本，也就是 applicative functors。我們也會提到 monoids。


## 溫習 Functors

![](frogtor.png)

我們已經在之前的章節提到 functors。如果你還沒讀那個章節，也許你應該先去看看。或是你直接假裝你已經讀過了。

來快速複習一下：Functors 是可以被 map over 的物件，像是 lists，``Maybe``，trees 等等。在 Haskell 中我們是用 ``Functor`` 這個 typeclass 來描述他。這個 typeclass 只有一個 method，叫做 ``fmap``，他的型態是 ``fmap :: (a -> b) ->  f a -> f b``。這型態說明了如果給我一個從 ``a`` 映到 ``b`` 的函數，以及一個裝了 ``a`` 的盒子，我會回給你一個裝了 ``b`` 的盒子。就好像用這個函數將每個元素都轉成 ``b`` 一樣

    *給一點建議*。這盒子的比喻嘗試讓你抓到些 functors 是如何運作的感覺。在之後我們也會用相同的比喻來比喻 applicative functors 跟 monads。在多數情況下這種比喻是恰當的，但不要過度引申，有些 functors 是不適用這個比喻的。一個比較正確的形容是 functors 是一個計算語境（computational context）。這個語境可能是這個 computation 可能帶有值，或是有可能會失敗（像 ``Maybe`` 跟 ``Either a``），或是他可能有多個值（像 lists），等等。

如果一個 type constructor 要是 ``Functor`` 的 instance，那他的 kind 必須是 ``* -> *``，這代表他必須剛好接受一個 type 當作 type parameter。像是 ``Maybe`` 可以是 Functor 的一個 instance，因為他接受一個 type parameter，來做成像是 ``Maybe Int``，或是 ``Maybe String``。如果一個 type constructor 接受兩個參數，像是 ``Either``，我們必須給他兩個 type parameter。所以我們不能這樣寫：``instance Functor Either where``，但我們可以寫 ``instance Functor (Either a) where``，如果我們把 ``fmap`` 限縮成只是 ``Either a`` 的，那他的型態就是 ``fmap :: (b -> c) -> Either a b -> Either a c``。就像你看到的，``Either a`` 的是固定的一部分，因為 ``Either a`` 只恰好接受一個 type parameter，但 ``Either`` 則要接受兩個 type parameters。這樣 fmap 的型態變成 ``fmap :: (b -> c) -> Either b -> Either c``，這不太合理。

我們知道有許多型態都是 ``Functor`` 的 instance，像是 ``[]``，``Maybe``，``Either a`` 以及我們自己寫的 ``Tree``。我們也看到了如何用一個函數 map 他們。在這一章節，我們再多舉兩個例子，也就是 ``IO`` 跟 ``(->) r``。

如果一個值的型態是 ``IO String``，他代表的是一個會被計算成 String 結果的 I/O action。我們可以用 do syntax 來把結果綁定到某個名稱。我們之前把 I/O action 比喻做長了腳的盒子，會到真實世界幫我們取一些值回來。我們可以檢視他們取了什麼值，但一旦看過，我們必須要把值放回盒子中。用這個比喻，``IO`` 的行為就像是一個 functor。

我們來看看 ``IO`` 是怎麼樣的一個 ``Functor`` instance。當我們 ``fmap`` 用一個 function 來 map over I/O action 時，我們會想要拿回一個裝著已經用 function 映射過值的 I/O action。

```haskell
instance Functor IO where
    fmap f action = do
        result <- action
        return (f result)
```

對一個 I/O action 做 map over 動作的結果仍會是一個 I/O action，所以我們才用 do syntax 來把兩個 I/O action 黏成一個。在 ``fmap`` 的實作中，我們先執行了原本傳進的 I/O action，並把結果綁定成 ``result``。然後我們寫了 ``return (f result)``。``return`` 就如你所知道的，是一個只會回傳包了你傳給他東西的 I/O action。還有一個 do block 的回傳值一定是他最後一個 I/O action 的回傳值。這也是為什麼我們需要 return。其實他只是回傳包了 ``f result`` 的 I/O action。

我們可以再多實驗一下來找到些感覺。來看看這段 code：

```haskell
main = do line <- getLine   
        let line' = reverse line  
        putStrLn $ "You said " ++ line' ++ " backwards!"  
        putStrLn $ "Yes, you really said" ++ line' ++ " backwards!"  
```

這程式要求使用者輸入一行文字，然後印出一行反過來的。
我們可以用 ``fmap`` 來改寫：

```haskell
main = do line <- fmap reverse getLine  
            putStrLn $ "You said " ++ line ++ " backwards!"  
            putStrLn $ "Yes, you really said" ++ line ++ " backwards!"  
```

![](alien.png)

就像我們用 ``fmap`` ``reverse`` 來 map over ``Just "blah"`` 會得到 ``Just "halb"``，我們也可以 ``fmap`` ``reverse`` 來 map over ``getLine``。``getLine`` 是一個 I/O action，他的 type 是 ``IO String``，而用 ``reverse`` 來 map over 他會回傳一個取回一個字串並 ``reverse`` 他的 I/O action。就像我們 apply 一個 function 到一個 ``Maybe`` 一樣，我們也可以 apply 一個 function 到一個 ``IO``，只是這個 ``IO`` 會跑去外面拿回某些值。然後我們把結果用 ``<-`` 綁定到某個名稱，而這個名稱綁定的值是已經 ``reverse`` 過了。

而 ``fmap (++"!") getLine`` 這個 I/O action 表現得就像 ``getLine``，只是他的結果多了一個 ``"!"`` 在最後。

如果我們限縮 ``fmap`` 到 ``IO`` 型態上，那 fmap 的型態是 ``fmap :: (a -> b) -> IO a -> IO b``。``fmap`` 接受一個函數跟一個 I/O action，並回傳一個 I/O action 包含了已經 apply 過 function 的結果。

如果你曾經注意到你想要將一個 I/O action 綁定到一個名稱上，只是為了要 apply 一個 function。你可以考慮使用 ``fmap``，那會更漂亮地表達這件事。或者你想要對 functor 中的資料做 transformation，你可以先將你要用的 function 寫在 top level，或是把他作成一個 lambda function，甚至用 function composition。

```haskell
import Data.Char  
import Data.List  
  
main = do line <- fmap (intersperse '-' . reverse . map toUpper) getLine  
          putStrLn line  
```

```haskell
$ runhaskell fmapping_io.hs  
hello there  
E-R-E-H-T- -O-L-L-E-H  
```

正如你想的，``intersperse '-' . reverse . map toUpper`` 合成了一個 function，他接受一個字串，將他轉成大寫，然後反過來，再用 ``intersperse '-'`` 安插'-'。他是比較漂亮版本的 ``(\xs -> intersperse '-' (reverse (map toUpper xs)))``。

另一個 ``Functor`` 的案例是 ``(->) r``，只是我們先前沒有注意到。你可能會困惑到底 ``(->) r`` 究竟代表什麼？一個 ``r -> a`` 的型態可以寫成 ``(->) r a``，就像是 ``2 + 3`` 可以寫成 ``(+) 2 3`` 一樣。我們可以從一個不同的角度來看待 ``(->) r a``，他其實只是一個接受兩個參數的 type constructor，好比 ``Either``。但記住我們說過 ``Functor`` 只能接受一個 type constructor。這也是為什麼 ``(->)`` 不是 ``Functor`` 的一個 instance，但 ``(->) r`` 則是。如果程式的語法允許的話，你也可以將 ``(->) r`` 寫成 ``(r ->)``。就如 ``(2+)`` 代表的其實是 ``(+) 2``。至於細節是如何呢？我們可以看看 ``Control.Monad.Instances``。

    我們通常說一個接受任何東西以及回傳隨便一個東西的函數型態是 ``a -> b``。``r -> a`` 是同樣意思，只是把符號代換了一下。

```haskell
instance Functor ((->) r) where  
    fmap f g = (\x -> f (g x))  
```

如果語法允許的話，他可以被寫成

```haskell
instance Functor (r ->) where  
    fmap f g = (\x -> f (g x))  
```

但其實是不允許的，所以我們必須寫成第一種的樣子。

首先我們來看看 ``fmap`` 的型態。他的型態是 ``fmap :: (a -> b) -> f a -> f b``。我們把所有的 ``f`` 在心裡代換成 ``(->) r``。則 ``fmap`` 的型態就變成 ``fmap :: (a -> b) -> ((->) r a) -> ((->) r b)``。接著我們把 ``(->) r a`` 跟 ``(->) r b`` 換成 ``r -> a`` 跟 ``r -> b``。則我們得到 ``fmap :: (a -> b) -> (r -> a) -> (r -> b)``。

從上面的結果看到將一個 function map over 一個 function 會得到另一個 function，就如 map over 一個 function 到 ``Maybe`` 會得到一個 ``Maybe``，而 map over 一個 function 到一個 list 會得到一個 list。而 ``fmap :: (a -> b) -> (r -> a) -> (r -> b)`` 告訴我們什麼？他接受一個從 ``a`` 到 ``b`` 的 function，跟一個從 ``r`` 到 ``a`` 的 function，並回傳一個從 ``r`` 到 ``b`` 的 function。這根本就是 function composition。把 ``r -> a`` 的輸出接到 ``a -> b`` 的輸入，的確是 function composition 在做的事。如果你再仔細看看 instance 的定義，會發現真的就是一個 function composition。

```haskell
instance Functor ((->) r) where  
    fmap = (.)  
```

這很明顯就是把 ``fmap`` 當 composition 在用。可以用 ``:m + Control.Monad.Instances`` 把模組裝載進來，並做一些嘗試。

```haskell
ghci> :t fmap (*3) (+100)  
fmap (*3) (+100) :: (Num a) => a -> a  
ghci> fmap (*3) (+100) 1  
303  
ghci> (*3) `fmap` (+100) $ 1  
303  
ghci> (*3) . (+100) $ 1  
303  
ghci> fmap (show . (*3)) (*100) 1  
"300"  
```

我們呼叫 ``fmap`` 的方式是 infix 的方式，這跟 ``.`` 很像。在第二行，我們把 ``(*3)`` map over 到 ``(+100)`` 上，這會回傳一個先把輸入值 ``(+100)`` 再 ``(*3)`` 的 function，我們再用 ``1`` 去呼叫他。

到這邊為止盒子的比喻還適用嗎？如果你硬是要解釋的話還是解釋得通。當我們將 ``fmap (+3)`` map over ``Just 3`` 的時候，對於 ``Maybe`` 我們很容易把他想成是裝了值的盒子，我們只是對盒子裡面的值 ``(+3)``。但對於 ``fmap (*3) (+100)`` 呢？你可以把 ``(+100)`` 想成是一個裝了值的盒子。有點像把 I/O action 想成長了腳的盒子一樣。對 ``(+100)`` 使用 ``fmap (*3)`` 會產生另一個表現得像 ``(+100)`` 的 function。只是在算出值之前，會再多計算 ``(*3)``。這樣我們可以看出來 ``fmap`` 表現得就像 ``.`` 一樣。

``fmap`` 等同於 function composition 這件事對我們來說並不是很實用，但至少是一個有趣的觀點。這也讓我們打開視野，看到盒子的比喻不是那麼恰當，functors 其實比較像 computation。function 被 map over 到一個 computation 會產生經由那個 function 映射過後的 computation。

![](lifter.png)

在我們繼續看 ``fmap`` 該遵守的規則之前，我們再看一次 ``fmap`` 的型態，他是 ``fmap :: (a -> b) -> f a -> f b``。很明顯我們是在討論 Functor，所以為了簡潔，我們就不寫 ``(Functor f) =>`` 的部份。當我們在學 curry 的時候，我們說過 Haskell 的 function 實際上只接受一個參數。一個型態是 ``a -> b -> c`` 的函數實際上是接受 ``a`` 然後回傳 ``b -> c``，而 ``b -> c`` 實際上接受一個 ``b`` 然後回傳一個 ``c``。如果我們用比較少的參數呼叫一個函數，他就會回傳一個函數需要接受剩下的參數。所以 ``a -> b -> c`` 可以寫成 ``a -> (b -> c)``。這樣 curry 可以明顯一些。

同樣的，我們可以不要把 ``fmap`` 想成是一個接受 function 跟 functor 並回傳一個 function 的 function。而是想成一個接受 function 並回傳一個新的 function 的 function，回傳的 function 接受一個 functor 並回傳一個 functor。他接受 ``a -> b`` 並回傳 ``f a -> f b``。這動作叫做 lifting。我們用 GHCI 的 ``:t`` 來做的實驗。

```haskell
ghci> :t fmap (*2)  
fmap (*2) :: (Num a, Functor f) => f a -> f a  
ghci> :t fmap (replicate 3)  
fmap (replicate 3) :: (Functor f) => f a -> f [a]  
```

``fmap (*2)`` 接受一個 functor ``f``，並回傳一個基於數字的 functor。那個 functor 可以是 list，可以是 ``Maybe``，可以是 ``Either String``。``fmap (replicate 3)`` 可以接受一個基於任何型態的 functor，並回傳一個基於 list 的 functor。

    當我們提到 functor over numbers 的時候，你可以想像他是一個 functor 包含有許多數字在裡面。前面一種說法其實比較正確，但後面一種說法比較容易讓人理解。

這樣的觀察在我們只有綁定一個部份套用的函數，像是 ``fmap (++"!")``，的時候會顯得更清楚，

你可以把 ``fmap`` 想做是一個函數，他接受另一個函數跟一個 functor，然後把函數對 functor 每一個元素做映射，或你可以想做他是一個函數，他接受一個函數並把他 lift 到可以在 functors 上面操作。兩種想法都是正確的，而且在 Haskell 中是等價。

``fmap (replicate 3) :: (Functor f) => f a -> f [a]`` 這樣的型態代表這個函數可以運作在任何 functor 上。至於確切的行為則要看究竟我們操作的是什麼樣的 functor。如果我們是用 ``fmap (replicate 3)`` 對一個 list 操作，那我們會選擇 ``fmap`` 針對 list 的實作，也就是只是一個 ``map``。如果我們是碰到 ``Maybe a``。那他在碰到 ``Just`` 型態的時候，會對裡面的值套用 ``replicate 3``。而碰到 ``Nothing`` 的時候就回傳 ``Nothing``。

```haskell
ghci> fmap (replicate 3) [1,2,3,4]  
[[1,1,1],[2,2,2],[3,3,3],[4,4,4]]  
ghci> fmap (replicate 3) (Just 4)  
Just [4,4,4]  
ghci> fmap (replicate 3) (Right "blah")  
Right ["blah","blah","blah"]  
ghci> fmap (replicate 3) Nothing  
Nothing  
ghci> fmap (replicate 3) (Left "foo")  
Left "foo"  
```


接下來我們來看看 functor laws。一個東西要成為 functor，必須要遵守某些定律。不管任何一個 functor 都被要求具有某些性質。他們必須是能被 map over 的。對他們呼叫 ``fmap`` 應該是要用一個函數 map 每一個元素，不多做任何事情。這些行為都被 functor laws 所描述。對於 ``Functor`` 的 instance 來說，總共兩條定律應該被遵守。不過他們不會在 Haskell 中自動被檢查，所以你必須自己確認這些條件。

functor law 的第一條說明，如果我們對 functor 做 map ``id``，那得到的新的 functor 應該要跟原來的一樣。如果寫得正式一點，他代表 ``fmap id = id``。基本上他就是說對 functor 呼叫 ``fmap id``，應該等同於對 functor 呼叫 ``id`` 一樣。畢竟 ``id`` 只是 identity function，他只會把參數照原樣丟出。他也可以被寫成 ``\x -> x``。如果我們對 functor 的概念就是可以被 map over 的物件，那 ``fmap id = id`` 的性就顯而易見。

我們來看看這個定律的幾個案例：

```haskell
ghci> fmap id (Just 3)  
Just 3  
ghci> id (Just 3)  
Just 3  
ghci> fmap id [1..5]  
[1,2,3,4,5]  
ghci> id [1..5]  
[1,2,3,4,5]  
ghci> fmap id []  
[]  
ghci> fmap id Nothing  
Nothing  
```

如果我們看看 ``Maybe`` 的 ``fmap`` 的實作，我們不難發現第一定律為何被遵守。

```haskell
instance Functor Maybe where  
    fmap f (Just x) = Just (f x)  
    fmap f Nothing = Nothing  
```

我們可以想像在 ``f`` 的位置擺上 ``id``。我們看到 ``fmap id`` 拿到 ``Just x`` 的時候，結果只不過是 ``Just (id x)``，而 ``id`` 有只回傳他拿到的東西，所以可以知道 ``Just (id x)`` 等價於 ``Just x``。所以說我們可以知道對 ``Maybe`` 中的 ``Just`` 用 ``id`` 去做 map over 的動作，會拿回一樣的值。

而將 ``id`` map over ``Nothing`` 會拿回 ``Nothing`` 並不稀奇。所以從這兩個 ``fmap`` 的實作，我們可以看到的確 ``fmap id = id`` 有被遵守。


![](justice.png)


*第二定律描述說先將兩個函數合成並將結果 map over 一個 functor 的結果，應該跟先將第一個函數 map over 一個 functor，再將第二個函數 map over 那個 functor 的結果是一樣的。*正式地寫下來的話就是 ``fmap (f . g) = fmap f . fmap g``。或是用另外一種寫法，對於任何一個 functor F，下面這個式子應該要被遵守：``fmap (f . g) F = fmap f (fmap g F)``。


如果我們能夠證明某個型別遵守兩個定律，那我們就可以保證他跟其他 functor 對於映射方面都擁有相同的性質。我們知道如果對他用 ``fmap``，我們知道不會有除了 mapping 以外的事會發生，而他就僅僅會表現成某個可以被 map over 的東西。也就是一個 functor。你可以再仔細檢視 ``fmap`` 對於某些型別的實作來了解第二定律。正如我們先前對 ``Maybe`` 檢視第一定律一般。

如果你需要的話，我們能在這邊演練一下 ``Maybe`` 是如何遵守第二定律的。首先 ``fmap (f . g)`` 來 map over ``Nothing`` 的話，我們會得到 ``Nothing``。因為用任何函數來 ``fmap`` ``Nothing`` 的話都會回傳 ``Nothing``。如果我們 ``fmap f (fmap g Nothing)``，我們會得到 ``Nothing``。可以看到當面對 ``Nothing`` 的時候，``Maybe`` 很顯然是遵守第二定律的。
那對於 ``Just something`` 呢？如果我們使用 ``fmap (f . g) (Just x)`` 的話，從實作的程式碼中我可以看到 ``Just ((f . g ) x)``，也就是 ``Just (f (g x))``。如果我們使用 ``fmap f (fmap g (Just x))`` 的話我們可以從實作知道 ``fmap g (Just x)`` 會是 ``Just (g x)``。``fmap f (fmap g (Just x))`` 跟 ``fmap f (Just (g x))`` 相等。而從實作上這又會相等於 ``Just (f (g x))``。

如果你不太理解這邊的說明，別擔心。只要確定你了解什麼是函數合成就好。在多數的情況下你可以直覺地對應到這些型別表現得就像 containers 或函數一樣。或是也可以換種方法，只要多嘗試對型別中不同的值做操作你就可以看看型別是否有遵守定律。

我們來看一些經典的例子。這些型別建構子雖然是 ``Functor`` 的 instance，但實際上他們並不是 functor，因為他們並不遵守這些定律。我們來看看其中一個型別。

```haskell
data CMaybe a = CNothing | CJust Int a deriving (Show)      
```

C 這邊代表的是計數器。他是一種看起來像是 ``Maybe a`` 的型別，只差在 ``Just`` 包含了兩個 field 而不是一個。在 ``CJust`` 中的第一個 field 是 ``Int``，他是扮演計數器用的。而第二個 field 則為型別 ``a``，他是從型別參數來的，而他確切的型別當然會依據我們選定的 ``CMaybe a`` 而定。我們來對他作些操作來獲得些操作上的直覺吧。

```haskell
ghci> CNothing  
CNothing  
ghci> CJust 0 "haha"  
CJust 0 "haha"  
ghci> :t CNothing  
CNothing :: CMaybe a  
ghci> :t CJust 0 "haha"  
CJust 0 "haha" :: CMaybe [Char]  
ghci> CJust 100 [1,2,3]  
CJust 100 [1,2,3]  
```


如果我們使用 ``CNothing``，就代表不含有 field。如果我們用的是 ``CJust``，那第一個 field 是整數，而第二個 field 可以為任何型別。我們來定義一個 ``Functor`` 的 instance，這樣每次我們使用 ``fmap`` 的時候，函數會被套用在第二個 field，而第一個 field 會被加一。

```haskell
instance Functor CMaybe where  
    fmap f CNothing = CNothing  
    fmap f (CJust counter x) = CJust (counter+1) (f x)  
```

這種定義方式有點像是 ``Maybe`` 的定義方式，只差在當我們使用 ``fmap`` 的時候，如果碰到的不是空值，那我們不只會套用函數，還會把計數器加一。我們可以來看一些範例操作。


```haskell
ghci> fmap (++"ha") (CJust 0 "ho")  
CJust 1 "hoha"  
ghci> fmap (++"he") (fmap (++"ha") (CJust 0 "ho"))  
CJust 2 "hohahe"  
ghci> fmap (++"blah") CNothing  
CNothing  
```


這些會遵守 functor laws 嗎？要知道有不遵守的情形，只要找到一個反例就好了。


```haskell
ghci> fmap id (CJust 0 "haha")  
CJust 1 "haha"  
ghci> id (CJust 0 "haha")  
CJust 0 "haha"  
```


我們知道 functor law 的第一定律描述當我們用 ``id`` 來 map over 一個 functor 的時候，他的結果應該跟只對 functor 呼叫 ``id`` 的結果一樣。但我們可以看到這個例子中，這對於 ``CMaybe`` 並不遵守。儘管他的確是 ``Functor`` typeclass 的一個 instace。但他並不遵守 functor law 因此不是一個 functor。如果有人使用我們的 ``CMaybe`` 型別，把他當作 functor 用，那他就會期待 functor laws 會被遵守。但 ``CMaybe`` 並沒辦法滿足，便會造成錯誤的程式。當我們使用一個 functor 的時候，函數合成跟 map over 的先後順序不應該有影響。但對於 ``CMaybe`` 他是有影響的，因為他紀錄了被 map over 的次數。如果我們希望 ``CMaybe`` 遵守 functor law，我們必須要讓 ``Int`` 欄位在做 ``fmap`` 的時候維持不變。


乍看之下 functor laws 看起來不是很必要，也容易讓人搞不懂，但我們知道如果一個型別遵守 functor laws，那我們就能對他作些基本的假設。如果遵守了 functor laws，我們知道對他做 ``fmap`` 不會做多餘的事情，只是用一個函數做映射而已。這讓寫出來的程式碼足夠抽象也容易擴展。因為我們可以用定律來推論型別的行為。


所有在標準函式庫中的 ``Functor`` 的 instance 都遵守這些定律，但你可以自己檢查一遍。下一次你定義一個型別為 ``Functor`` 的 instance 的時候，花點時間確認他確實遵守 functor laws。一旦你操作過足夠多的 functors 時，你就會獲得直覺，知道他們會有什麼樣的性質跟行為。而且 functor laws 也會覺得顯而易見。但就算沒有這些直覺，你仍然可以一行一行地來找看看有沒有反例讓這些定律失效。


我們可以把 functor 看作輸出具有 context 的值。例如說 ``Just 3`` 就是輸出 ``3``，但他又帶有一個可能沒有值的 context。``[1,2,3]`` 輸出三個值，``1``,``2`` 跟 ``3``，同時也帶有可能有多個值或沒有值的 context。``(+3)`` 則會帶有一個依賴於參數的 context。


如果你把 functor 想做是輸出值這件事，那你可以把 map over 一個 functor 這件事想成在 functor 輸出的後面再多加一層轉換。當我們做 ``fmap (+3) [1,2,3]`` 的時候，我們是把 ``(+3)`` 接到 ``[1,2,3]`` 後面，所以當我們檢視任何一個 list 的輸出的時候，``(+3)`` 也會被套用在上面。另一個例子是對函數做 map over。當我們做 ``fmap (+3) (*3)``，我們是把 ``(+3)`` 這個轉換套用在 ``(*3)`` 後面。這樣想的話會很自然就會把 ``fmap`` 跟函數合成關聯起來（``fmap (+3) (*3)`` 等價於 ``(+3) . (*3)``，也等價於 ``\x -> ((x*3)+3)``），畢竟我們是接受一個函數 ``(*3)`` 然後套用 ``(+3)`` 轉換。最後的結果仍然是一個函數，只是當我們餵給他一個數字的時候，他會先乘上三然後做轉換加上三。這基本上就是函數合成在做的事。

## Applicative functors

![](present.png)

在這個章節中，我們會學到 applicative functors，也就是加強版的 functors，在 Haskell 中是用在 ``Control.Applicative`` 中的 ``Applicative`` 這個 typeclass 來定義的。

你還記得 Haskell 中函數預設就是 Curried 的，那代表接受多個參數的函數實際上是接受一個參數然後回傳一個接受剩餘參數的函數，以此類推。如果一個函數的型別是 ``a -> b -> c``，我們通常會說這個函數接受兩個參數並回傳 ``c``，但他實際上是接受 ``a`` 並回傳一個 ``b -> c`` 的函數。這也是為什麼我們可以用 ``(f x) y`` 的方式呼叫 ``f x y``。這個機制讓我們可以 partially apply 一個函數，可以用比較少的參數呼叫他們。可以做成一個函數再餵給其他函數。

到目前為止，當我們要對 functor map over 一個函數的時候，我們用的函數都是只接受一個參數的。但如果我們要 map 一個接受兩個參數的函數呢？我們來看幾個具體的例子。如果我們有 ``Just 3`` 然後我們做 ``fmap (*) (Just 3)``，那我們會獲得什麼樣的結果？從 ``Maybe`` 對 ``Functor`` 的 instance 實作來看，我們知道如果他是 ``Just something``，他會對在 ``Just`` 中的 ``something`` 做映射。因此當 ``fmap (*) (Just 3)`` 會得到 ``Just ((*) 3)``，也可以寫做 ``Just (* 3)``。我們得到了一個包在 ``Just`` 中的函數。

```haskell
ghci> :t fmap (++) (Just "hey")  
fmap (++) (Just "hey") :: Maybe ([Char] -> [Char])  
ghci> :t fmap compare (Just 'a')  
fmap compare (Just 'a') :: Maybe (Char -> Ordering)  
ghci> :t fmap compare "A LIST OF CHARS"  
fmap compare "A LIST OF CHARS" :: [Char -> Ordering]  
ghci> :t fmap (\x y z -> x + y / z) [3,4,5,6]  
fmap (\x y z -> x + y / z) [3,4,5,6] :: (Fractional a) => [a -> a -> a]  
```

如果我們 map ``compare`` 到一個包含許多字元的 list 呢？他的型別是 ``(Ord a) => a -> a -> Ordering``，我們會得到包含許多 ``Char -> Ordering`` 型別函數的 list，因為 ``compare`` 被 partially apply 到 list 中的字元。他不是包含許多 ``(Ord a) => a -> Ordering`` 的函數，因為第一個 ``a`` 碰到的型別是 ``Char``，所以第二個 ``a`` 也必須是 ``Char``。


我們看到如何用一個多參數的函數來 map functor，我們會得到一個包含了函數的 functor。那現在我們能對這個包含了函數的 functor 做什麼呢？我們能用一個吃這些函數的函數來 map over 這個 functor，這些在 functor 中的函數都會被當作參數丟給我們的函數。

```haskell
ghci> let a = fmap (*) [1,2,3,4]  
ghci> :t a  
a :: [Integer -> Integer]  
ghci> fmap (\f -> f 9) a  
[9,18,27,36]  
```

但如果我們的有一個 functor 裡面是 ``Just (3 *)`` 還有另一個 functor 裡面是 ``Just 5``，但我們想要把第一個 ``Just (3 *)`` map over ``Just 5`` 呢？如果是普通的 functor，那就沒救了。因為他們只允許 map 一個普通的函數。即使我們用 ``\f -> f 9`` 來 map 一個裝了很多函數的 functor，我們也是使用了普通的函數。我們是無法單純用 ``fmap`` 來把包在一個 functor 的函數 map 另一個包在 functor 中的值。我們能用模式匹配 ``Just`` 來把函數從裡面抽出來，然後再 map ``Just 5``，但我們是希望有一個一般化的作法，對任何 functor 都有效。

我們來看看 ``Applicative`` 這個 typeclass。他位在 ``Control.Applicative`` 中，在其中定義了兩個函數 ``pure`` 跟 ``<*>``。他並沒有提供預設的實作，如果我們想使用他必須要為他們 applicative functor 的實作。typeclass 定義如下：

```haskell
class (Functor f) => Applicative f where  
    pure :: a -> f a  
    (<*>) :: f (a -> b) -> f a -> f b  
```

這簡簡單單的三行可以讓我們學到不少。首先來看第一行。他開啟了 ``Applicative`` 的定義，並加上 class contraint。描述了一個型別構造子要是 ``Applicative``，他必須也是 ``Functor``。這就是為什麼我們說一個型別構造子屬於 ``Applicative`` 的話，他也會是 ``Functor``，因此我們能對他使用 ``fmap``。

第一個定義的是 ``pure``。他的型別宣告是 ``pure :: a -> f a``。``f`` 代表 applicative functor 的 instance。由於 Haskell 有一個優秀的型別系統，其中函數又是將一些參數映射成結果，我們可以從型別宣告中讀出許多訊息。``pure`` 應該要接受一個值，然後回傳一個包含那個值的 applicative functor。我們這邊是用盒子來作比喻，即使有一些比喻不完全符合現實的情況。儘管這樣，``a -> f a`` 仍有許多豐富的資訊，他確實告訴我們他會接受一個值並回傳一個 applicative functor，裡面裝有結果。

對於 ``pure`` 比較好的說法是把一個普通值放到一個預設的 context 下，一個最小的 context 但仍然包含這個值。

``<*>`` 也非常有趣。他的型別是 ``f (a -> b) -> f a -> f b``。這有讓你聯想到什麼嗎？沒錯！就是 ``fmap :: (a -> b) -> f a -> f b``。他有點像加強版的 ``fmap``。然而 ``fmap`` 接受一個函數跟一個 functor，然後套用 functor 之中的函數。``<*>`` 則是接受一個裝有函數的 functor 跟另一個 functor，然後取出第一個 functor 中的函數將他對第二個 functor 中的值做 map。

我們來看看 ``Maybe`` 的 ``Applicative`` 實作：

```haskell
instance Applicative Maybe where  
    pure = Just  
    Nothing <*> _ = Nothing  
    (Just f) <*> something = fmap f something  
```

從 class 的定義我們可以看到 ``f`` 作為 applicative functor 會接受一個具體型別當作參數，所以我們是寫成 ``instance Applicative Maybe where`` 而不是寫成 ``instance Applicative (Maybe a) where``。

首先看到 ``pure``。他只不過是接受一個東西然後包成 applicative functor。我們寫成 ``pure = Just`` 是因為 ``Just`` 不過就是一個普通函數。我們其實也可以寫成 ``pure x = Just x``。

接著我們定義了 ``<*>``。我們無法從 ``Nothing`` 中抽出一個函數，因為 ``Nothing`` 並不包含一個函數。所以我們說如果我們要嘗試從 ``Nothing`` 中取出一個函數，結果必定是 ``Nothing``。如果你看看 ``Applicative`` 的定義，你會看到他有 ``Functor`` 的限制，他代表 ``<*>`` 的兩個參數都會是 functors。如果第一個參數不是 ``Nothing``，而是一個裝了函數的 ``Just``，而且我們希望將這個函數對第二個參數做 map。這個也考慮到第二個參數是 ``Nothing`` 的情況，因為 ``fmap`` 任何一個函數至 ``Nothing`` 會回傳 ``Nothing``。

對於 ``Maybe`` 而言，如果左邊是 ``Just``，那 ``<*>`` 會從其中抽出了一個函數來 map 右邊的值。如果有任何一個參數是 ``Nothing``。那結果便是 ``Nothing``。

來試試看吧！

```haskell
ghci> Just (+3) <*> Just 9  
Just 12  
ghci> pure (+3) <*> Just 10  
Just 13  
ghci> pure (+3) <*> Just 9  
Just 12  
ghci> Just (++"hahah") <*> Nothing  
Nothing  
ghci> Nothing <*> Just "woot"  
Nothing  
```

我們看到 ``pure (+3)`` 跟 ``Just (+3)`` 在這個 case 下是一樣的。如果你是在 applicative context 底下跟 ``Maybe`` 打交道的話請用 ``pure``，要不然就用 ``Just``。前四個輸入展示了函數是如何被取出並做 map 的動作，但在這個 case 底下，他們同樣也可以用 unwrap 函數來 map over functors。最後一行比較有趣，因為我們試著從 ``Nothing`` 取出函數並將他 map 到某個值。結果當然是 ``Nothing``。

對於普通的 functors，你可以用一個函數 map over 一個 functors，但你可能沒辦法拿到結果。而 applicative functors 則讓你可以用單一一個函數操作好幾個 functors。看看下面一段程式碼：

```haskell
ghci> pure (+) <*> Just 3 <*> Just 5  
Just 8  
ghci> pure (+) <*> Just 3 <*> Nothing  
Nothing  
ghci> pure (+) <*> Nothing <*> Just 5  
Nothing  
```

![](whale.png)

究竟我們寫了些什麽？我們來一步步看一下。``<*>`` 是 left-associative，也就是說 ``pure (+) <*> Just 3 <*> Just 5`` 可以寫成 ``(pure (+) <*> Just 3) <*> Just 5``。首先 ``+`` 是擺在一個 functor 中，在這邊剛好他是一個 ``Maybe``。所以首先，我們有 ``pure (+)``，他等價於 ``Just (+)``。接下來由於 partial application 的關係，``Just (+) <*> Just 3`` 等價於 ``Just (3+)``。把一個 ``3`` 餵給 ``+`` 形成另一個只接受一個參數的函數，他的效果等於加上 3。最後 ``Just (3+) <*> Just 5`` 被運算，其結果是 ``Just 8``。


這樣很棒吧！用 applicative style 的方式來使用 applicative functors。像是 ``pure f <*> x <*> y <*> ...`` 就讓我們可以拿一個接受多個參數的函數，而且這些參數不一定是被包在 functor 中。就這樣來套用在多個在 functor context 的值。這個函數可以吃任意多的參數，畢竟 ``<*>`` 只是做 partial application 而已。


如果我們考慮到 ``pure f <*> x`` 等於 ``fmap f x`` 的話，這樣的用法就更方便了。這是 applicative laws 的其中一條。我們稍後會更仔細地檢視這條定律。現在我們先依直覺來使用他。就像我們先前所說的，``pure`` 把一個值放進一個預設的 context 中。如果我們要把一個函數放在一個預設的 context，然後把他取出並套用在放在另一個 applicative functor 的值。我們會做的事就是把函數 map over 那個 applicative functor。但我們不會寫成 ``pure f <*> x <*> y <*> ...``，而是寫成 ``fmap f x <*> y <*> ...``。這也是為什麽 ``Control.Applicative`` 會 export 一個函數 ``<$>``，他基本上就是中綴版的 ``fmap``。他是這麼被定義的：

```haskell
(<$>) :: (Functor f) => (a -> b) -> f a -> f b  
f <$> x = fmap f x  
```haskell
	
    
    要記住型別變數跟參數的名字還有值綁定的名稱不衝突。``f`` 在函數的型別宣告中是型別變數，說明 ``f`` 應該要滿足 ``Functor`` typeclass 的條件。而在函數本體中的 ``f`` 則表示一個函數，我們將他 map over x。我們同樣用 ``f`` 來表示他們並代表他們是相同的東西。


``<$>`` 的使用顯示了 applicative style 的好處。如果我們想要將 ``f`` 套用三個 applicative functor。我們可以寫成 ``f <$> x <*> y <*> z``。如果參數不是 applicative functor 而是普通值的話。我們則寫成 ``f x y z``。

我們再仔細看看他是如何運作的。我們有一個 ``Just "johntra"`` 跟 ``Just "volta"`` 這樣的值，我們希望將他們結合成一個 ``String``，並且包含在 ``Maybe`` 中。我們會這樣做：

```haskell
ghci> (++) <$> Just "johntra" <*> Just "volta"  
Just "johntravolta"  
```

可以將上面的跟下面這行比較一下：

```haskell
ghci> (++) "johntra" "volta"  
"johntravolta"  
```

可以將一個普通的函數套用在 applicative functor 上真不錯。只要稍微寫一些 ``<$>`` 跟 ``<*>`` 就可以把函數變成 applicative style，可以操作 applicatives 並回傳 applicatives。


總之當我們在做 ``(++) <$> Just "johntra" <*> Just "volta"`` 時，首先我們將 ``(++)`` map over 到 ``Just "johntra"``，然後產生 ``Just ("johntra"++)``，其中 ``(++)`` 的型別為 ``(++) :: [a] -> [a] -> [a]``，``Just ("johntra"++)`` 的型別為 ``Maybe ([Char] -> [Char])``。注意到 ``(++)`` 是如何吃掉第一個參數，以及我們是怎麼決定 ``a`` 是 ``Char`` 的。當我們做 ``Just ("johntra"++) <*> Just "volta"``，他接受一個包在 ``Just`` 中的函數，然後 map over ``Just "volta"``，產生了 ``Just "johntravolta"``。如果兩個值中有任意一個為 ``Nothing``，那整個結果就會是 ``Nothing``。


到目前為止我們只有用 ``Maybe`` 當作我們的案例，你可能也會想說 applicative functor 差不多就等於 ``Maybe``。不過其實有許多其他 ``Applicative`` 的 instance。我們來看看有哪些。


List 也是 applicative functor。很驚訝嗎？來看看我們是怎麼定義 ``[]`` 為 ``Applicative`` 的 instance 的。

```haskell
instance Applicative [] where  
    pure x = [x]  
    fs <*> xs = [f x | f <- fs, x <- xs]  
```


早先我們說過 ``pure`` 是把一個值放進預設的 context 中。換種說法就是一個會產生那個值的最小 context。而對 list 而言最小 context 就是 ``[]``，但由於空的 list 並不包含一個值，所以我們沒辦法把他當作 ``pure``。這也是為什麼 ``pure`` 其實是接受一個值然後回傳一個包含單元素的 list。同樣的，``Maybe`` 的最小 context 是 ``Nothing``，但他其實表示的是沒有值。所以 ``pure`` 其實是被實作成 ``Just`` 的。

```haskell
ghci> pure "Hey" :: [String]  
["Hey"]  
ghci> pure "Hey" :: Maybe String  
Just "Hey"  
```

至於 ``<*>`` 呢？如果我們假定 ``<*>`` 的型別是限制在 list 上的話，我們會得到 ``(<*>) :: [a -> b] -> [a] -> [b]``。他是用 list comprehension 來實作的。``<*>`` 必須要從左邊的參數取出函數，將他 map over 右邊的參數。但左邊的 list 有可能不包含任何函數，也可能包含一個函數，甚至是多個函數。而右邊的 list 有可能包含多個值。這也是為什麼我們用 list comprehension 的方式來從兩個 list 取值。我們要對左右任意的組合都做套用的動作。而得到的結果就會是左右兩者任意組合的結果。

```haskell
ghci> [(*0),(+100),(^2)] <*> [1,2,3]  
[0,0,0,101,102,103,1,4,9]  
```

左邊的 list 包含三個函數，而右邊的 list 有三個值。所以結果會是有九個元素的 list。在左邊 list 中的每一個函數都被套用到右邊的值。如果我們今天在 list 中的函數是接收兩個參數的，我們也可以套用到兩個 list 上。

```haskell
ghci> [(+),(*)] <*> [1,2] <*> [3,4]  
[4,5,5,6,3,4,6,8]  
```

由於 ``<*>`` 是 left-associative，也就是說 ``[(+),(*)] <*> [1,2]`` 會先運作，產生 ``[(1+),(2+),(1*),(2*)]``。由於左邊的每一個函數都套用至右邊的每一個值。也就產生 ``[(1+),(2+),(1*),(2*)] <*> [3,4]``，其便是最終結果。

list 的 applicative style 是相當有趣的：

```haskell
ghci> (++) <$> ["ha","heh","hmm"] <*> ["?","!","."]  
["ha?","ha!","ha.","heh?","heh!","heh.","hmm?","hmm!","hmm."] 
```

看看我們是如何將一個接受兩個字串參數的函數套用到兩個 applicative functor 上的，只要用適當的 applicative 運算子就可以達成。

你可以將 list 看作是一個 non-deterministic 的計算。而對於像 ``100`` 或是 ``"what"`` 這樣的值則是 deterministic 的計算，只會有一個結果。而 ``[1,2,3]`` 則可以看作是沒有確定究竟是哪一種結果。所以他代表的是所有可能的結果。當你在做 ``(+) <$> [1,2,3] <*> [4,5,6]``，你可以想做是把兩個 non-deterministic 的計算做 ``+``，只是他會產生另一個 non-deterministic 的計算，而且結果更加不確定。


Applicative style 對於 list 而言是一個取代 list comprehension 的好方式。在第二章中，我們想要看到 ``[2,5,10]`` 跟 ``[8,10,11]`` 相乘的結果，所以我們這樣做：

```haskell
ghci> [ x*y | x <- [2,5,10], y <- [8,10,11]]     
[16,20,22,40,50,55,80,100,110]     
```

我們只是從兩個 list 中取出元素，並將一個函數套用在任何元素的組合上。這也可以用 applicative style 的方式來寫：

```haskell
ghci> (*) <$> [2,5,10] <*> [8,10,11]  
[16,20,22,40,50,55,80,100,110]  
```

這寫法對我來說比較清楚。可以清楚表達我們是要對兩個 non-deterministic 的計算做 ``*``。如果我們想要所有相乘大於 50 可能的計算結果，我們會這樣寫：

```haskell
ghci> filter (>50) $ (*) <$> [2,5,10] <*> [8,10,11]  
[55,80,100,110]  
```

很容易看到 ``pure f <*> xs`` 等價於 ``fmap f xs``。而 ``pure f`` 就是 ``[f]``，而且 ``[f] <*> xs`` 可將左邊的每個函數套用至右邊的每個值。但左邊其實只有一個函數，所以他做起來就像是 mapping。

另一個我們已經看過的 ``Applicative`` 的 instance 是 ``IO``，來看看他是怎麼實作的：

```haskell
instance Applicative IO where  
    pure = return  
    a <*> b = do  
        f <- a  
        x <- b  
        return (f x)  
```

![](knight.png)

由於 ``pure`` 是把一個值放進最小的 context 中，所以將 ``return`` 定義成 ``pure`` 是很合理的。因為 ``return`` 也是做同樣的事情。他做了一個不做任何事情的 I/O action，他可以產生某些值來作為結果，但他實際上並沒有做任何 I/O 的動作，例如說印出結果到終端或是檔案。

如果 ``<*>`` 被限定在 ``IO`` 上操作的話，他的型別會是 ``(<*>) :: IO (a -> b) -> IO a -> IO b``。他接受一個產生函數的 I/O action，還有另一個 I/O action，並從以上兩者創造一個新的 I/O action，也就是把第二個參數餵給第一個參數。而得到回傳的結果，然後放到新的 I/O action 中。我們用 do 的語法來實作他。你還記得的話 do 就是把好幾個 I/O action 黏在一起，變成一個大的 I/O action。

而對於 ``Maybe`` 跟 ``[]`` 而言，我們可以把 ``<*>`` 想做是從左邊的參數取出一個函數，然後套用到右邊的參數上。至於 ``IO``，這種取出的類比方式仍然適用，但我們必須多加一個 sequencing 的概念，因為我們是從兩個 I/O action 中取值，也是在 sequencing，把他們黏成一個。我們從第一個 I/O action 中取值，但要取出 I/O action 的結果，他必須要先被執行過。

考慮下面這個範例：

```haskell
myAction :: IO String  
myAction = do  
    a <- getLine  
    b <- getLine  
    return $ a ++ b  
```


這是一個提示使用者輸入兩行並產生將兩行輸入串接在一起結果的一個 I/O action。我們先把兩個 ``getLine`` 黏在一起，然後用一個 ``return``，這是因為我們想要這個黏成的 I/O action 包含 ``a ++ b`` 的結果。我們也可以用 applicative style 的方式來描述：

```haskell
myAction :: IO String  
myAction = (++) <$> getLine <*> getLine  
```

我們先前的作法是將兩個 I/O action 的結果餵給函數。還記得 ``getLine`` 的型別是 ``getLine :: IO String``。當我們對 applicative functor 使用 ``<*>`` 的時候，結果也會是 applicative functor。

如果我們再使用盒子的類比，我們可以把 ``getLine`` 想做是一個去真實世界中拿取字串的盒子。而 ``(++) <$> getLine <*> getLine`` 會創造一個比較大的盒子，這個大盒子會派兩個盒子去終端拿取字串，並把結果串接起來放進自己的盒子中。

``(++) <$> getLine <*> getLine`` 的型別是 ``IO String``，他代表這個表達式式一個再普通不過的 I/O action，他裡面也裝著某種值。這也是為什麼我們可以這樣寫：

```haskell
main = do  
    a <- (++) <$> getLine <*> getLine  
    putStrLn $ "The two lines concatenated turn out to be: " ++ a  
```

如果你發現你是在做 binding I/O action 的動作，而且在 binding 之後還呼叫一些函數，最後用 ``return`` 來將結果包起來。
那你可以考慮使用 applicative style，這樣可以更簡潔。

另一個 ``Applicative`` 的 instance 是 ``(->) r``。雖然他們通常是用在 code golf 的情況，但他們還是十分有趣的例子。所以我們還是來看一下他們是怎麼被實作的。

	如果你忘記 ``(->) r`` 的意思，回去翻翻前一章節我們介紹 ``(->) r`` 作為一個 functor 的範例。

```haskell
instance Applicative ((->) r) where  
    pure x = (\_ -> x)  
    f <*> g = \x -> f x (g x)  
```

當我們用 ``pure`` 將一個值包成 applicative functor 的時候，他產生的結果永遠都會是那個值。也就是最小的 context。那也是為什麼對於 function 的 ``pure`` 實作來講，他就是接受一個值，然後造一個函數永遠回傳那個值，不管他被餵了什麼參數。如果你限定 ``pure`` 的型別至 ``(->) r`` 上，他就會是 ``pure :: a -> (r -> a)``。

```haskell
ghci> (pure 3) "blah"  
3  
```

由於 currying 的關係，函數套用是 left-associative，所以我們忽略掉括弧。

```haskell
ghci> pure 3 "blah"  
3  
```

而 ``<*>`` 的實作是比較不容易瞭解的，我們最好看一下怎麼用 applicative style 的方式來使用作為 applicative functor 的 function。

```haskell
ghci> :t (+) <$> (+3) <*> (*100)  
(+) <$> (+3) <*> (*100) :: (Num a) => a -> a  
ghci> (+) <$> (+3) <*> (*100) $ 5  
508 
```

將兩個 applicative functor 餵給 ``<*>`` 可以產生一個新的 applicative functor，所以如果我們丟給他兩個函數，我們能得到一個新的函數。所以是怎麼一回事呢？當我們做 ``(+) <$> (+3) <*> (*100)``，我們是在實作一個函數，他會將 ``(+3)`` 跟 ``(*100)`` 的結果再套用 ``+``。要看一個實際的範例的話，可以看一下 ``(+) <$> (+3) <*> (*100) $ 5`` 首先 ``5`` 被丟給 ``(+3)`` 跟 ``(*100)``，產生 ``8`` 跟 ``500``。然後 ``+`` 被套用到 ``8`` 跟 ``500``，得到 ``508``。


```haskell
ghci> (\x y z -> [x,y,z]) <$> (+3) <*> (*2) <*> (/2) $ 5  
[8.0,10.0,2.5]  
```

![](jazzb.png)

這邊也一樣。我們創建了一個函數，他會呼叫 ``\x y z -> [x,y,z]``，而丟的參數是 ``(+3)``, ``(*2)`` 跟 ``(/2)``。``5`` 被丟給以上三個函數，然後他們結果又接到 `` \x y z -> [x, y, z]``。


你可以將函數想做是裝著最終結果的盒子，所以 ``k <$> f <*> g`` 會製造一個函數，他會將 ``f`` 跟 ``g`` 的結果丟給 ``k``。當我們做 ``(+) <$> Just 3 <*> Just 5``，我們是用 ``+`` 套用在一些可能有或可能沒有的值上，所以結果也會是可能有或沒有。當我們做 ``(+) <$> (+10) <*> (+5)``，我們是將 ``+`` 套用在 ``(+10)`` 跟 ``(+5)`` 的結果上，而結果也會是一個函數，當被餵給一個參數的時候會產生結果。


我們通常不會將函數當作 applicative 用，不過仍然值得當作練習。對於 ``(->) r`` 怎麼定義成 ``Applicative`` 的並不是真的那麼重要，所以如果你不是很懂的話也沒關係。這只是讓你獲得一些操作上的直覺罷了。


一個我們之前還沒碰過的 ``Applicative`` 的 instance 是 ``ZipList``，他是包含在 ``Control.Applicative`` 中。


對於 list 要作為一個 applicative functor 可以有多種方式。我們已經介紹過其中一種。如果套用 ``<*>``，左邊是許多函數，而右邊是許多值，那結果會是函數套用到值的所有組合。如果我們做 ``[(+3),(*2)] <*> [1,2]``。那 ``(+3)`` 會先套用至 ``1`` 跟 ``2``。接著 ``(*2)`` 套用至 ``1`` 跟 ``2``。而得到 ``[4,5,2,4]``。


然而 ``[(+3),(*2)] <*> [1,2]`` 也可以這樣運作:把左邊第一個函數套用至右邊第一個值，接著左邊第二個函數套用右邊第二個值，以此類推。這樣得到的會是 ``[4,4]``。或是 ``[1 + 3, 2 * 2]``。


由於一個型別不能對同一個 typeclass 定義兩個 instance，所以才會定義了 ``ZipList a``，他只有一個構造子 ``ZipList``，他只包含一個欄位，他的型別是 list。

```haskell
instance Applicative ZipList where  
        pure x = ZipList (repeat x)  
        ZipList fs <*> ZipList xs = ZipList (zipWith (\f x -> f x) fs xs)  
```

``<*>`` 做的就是我們之前說的。他將第一個函數套用至第一個值，第二個函數套用第二個值。這也是 ``zipWith (\f x -> f x) fs xs`` 做的事。由於 ``zipWith`` 的特性，所以結果會跟 list 中比較短的那個一樣長。


``pure`` 也值得我們討論一下。他接受一個值，把他重複地放進一個 list 中。``pure "haha"`` 就會是 ``ZipList (["haha","haha","haha"...``。這可能會造成些混淆，畢竟我們說過 ``pure`` 是把一個值放進一個最小的 context 中。而你會想說無限長的 list 不可能會是一個最小的 context。但對於 zip list 來說這是很合理的，因為他必須在 list 的每個位置都有值。這也遵守了 ``pure f <*> xs`` 必須要等價於 ``fmap f xs`` 的特性。如果 ``pure 3`` 只是回傳 ``ZipList [3]``，那 ``pure (*2) <*> ZipList [1,5,10]`` 就只會算出 ``ZipList [2]``，因為兩個 zip list 算出結果的長度會是比較短的那個的長度。如果我們 zip 一個有限長的 list 以及一個無限長的 list，那結果的長會是有限長的 list 的長度。


那 zip list 是怎麼用 applicative style 操作的呢？我們來看看，``ZipList a`` 型別並沒有定義成 ``Show`` 的 instance，所以我們必須用 ``getZipList`` 函數來從 zip list 取出一個普通的 list。


```haskell
ghci> getZipList $ (+) <$> ZipList [1,2,3] <*> ZipList [100,100,100]  
[101,102,103]  
ghci> getZipList $ (+) <$> ZipList [1,2,3] <*> ZipList [100,100..]  
[101,102,103]  
ghci> getZipList $ max <$> ZipList [1,2,3,4,5,3] <*> ZipList [5,3,1,2]  
[5,3,3,4]  
ghci> getZipList $ (,,) <$> ZipList "dog" <*> ZipList "cat" <*> ZipList "rat"  
[('d','c','r'),('o','a','a'),('g','t','t')]  
```

    ``(,,)`` 函數跟 ``\x y z -> (x,y,z)`` 是等價的，而 ``(,)`` 跟 ``\x y -> (x,y)`` 是等價的。

除了 ``zipWith``，標準函式庫中也有 ``zipWith3``, ``zipWith4`` 之類的函數，最多支援到 7。``zipWith`` 接受一個接受兩個參數的函數，並把兩個 list zip 起來。``zipWith3`` 則接受一個接受三個參數的函數，然後把三個 list zip 起來。以此類推。用 applicative style 的方式來操作 zip list 的話，我們就不需要對每個數量的 list 都定義一個獨立的 zip 函數來 zip 他們。我們只需要用 applicative style 的方式來把任意數量的 list zip 起來就可以了。


``Control.Applicative`` 定義了一個函數叫做 ``liftA2``，他的型別是 ``liftA2 :: (Applicative f) => (a -> b -> c) -> f a -> f b -> f c``。他定義如下：


```haskell
liftA2 :: (Applicative f) => (a -> b -> c) -> f a -> f b -> f c  
liftA2 f a b = f <$> a <*> b  
```

並沒有太難理解的東西，他不過就是對兩個 applicatives 套用函數而已，而不用我們剛剛熟悉的 applicative style。我們提及他的理由只是要展示為什麼 applicative functors 比起一般的普通 functor 要強。如果只是普通的 functor 的話，我們只能將一個函數 map over 這個 functor。但有了 applicative functor，我們可以對好多個 functor 套用一個函數。看看這個函數的型別，他會是 ``(a -> b -> c) -> (f a -> f b -> f c)``。當我們從這樣的角度來看他的話，我們可以說 ``liftA2`` 接受一個普通的二元函數，並將他升級成一個函數可以運作在兩個 functor 之上。

另外一個有趣的概念是，我們可以接受兩個 applicative functor 並把他們結合成一個 applicative functor，這個新的將這兩個 applicative functor 裝在 list 中。舉例來說，我們現在有 ``Just 3`` 跟 ``Just 4``。我們假設後者是一個只包含單元素的 list。

```haskell
ghci> fmap (\x -> [x]) (Just 4)  
Just [4]  
```

所以假設我們有 ``Just 3`` 跟 ``Just [4]``。我們有怎麼得到 ``Just [3,4]`` 呢？很簡單。

```haskell
ghci> liftA2 (:) (Just 3) (Just [4])  
Just [3,4]  
ghci> (:) <$> Just 3 <*> Just [4]  
Just [3,4]  
```

還記得 ``:`` 是一個函數，他接受一個元素跟一個 list，並回傳一個新的 list，其中那個元素已經接在前面。現在我們有了 ``Just [3,4]``，我們能夠將他跟 ``Just 2`` 綁在一起變成 ``Just [2,3,4]`` 嗎？當然可以。我們可以將任意數量的 applicative 綁在一起變成一個 applicative，裡面包含一個裝有結果的 list。我們試著實作一個函數，他接受一串裝有 applicative 的 list，然後回傳一個 applicative 裡面有一個裝有結果的 list。我們稱呼他為 ``sequenceA``。

```haskell
sequenceA :: (Applicative f) => [f a] -> f [a]  
sequenceA [] = pure []  
sequenceA (x:xs) = (:) <$> x <*> sequenceA xs  
```

居然用到了遞迴！首先我們來看一下他的型別。他將一串 applicative 的 list 轉換成一個 applicative 裝有一個 list。從這個資訊我們可以推測出邊界條件。如果我們要將一個空的 list 變成一個裝有 list 的 applicative。我們只要把這個空的 list 放進一個預設的 context。現在來看一下我們怎麼用遞迴的。如果們有一個可以分成頭跟尾的 list（``x`` 是一個 applicative 而 ``xs`` 是一串 applicatve），我們可以對尾巴呼叫 ``sequenceA``，便會得到一個裝有 list 的 applicative。然後我們只要將在 ``x`` 中的值把他接到裝有 list 的 applicative 前面就可以了。


所以如果我們做 ``sequenceA [Just 1, Just 2]``，也就是 ``(:) <$> Just 1 <*> sequenceA [Just 2]``。那會等價於 ``(:) <$> Just 1 <*> ((:) <$> Just 2 <*> sequenceA [])``。我們知道 ``sequenceA []`` 算出來會是 ``Just []``，所以運算式就變成 ``(:) <$> Just 1 <*> ((:) <$> Just 2 <*> Just [])``，也就是 ``(:) <$> Just 1 <*> Just [2]``，算出來就是 ``Just [1,2]``。


另一種實作 ``sequenceA`` 的方式是用 fold。要記得幾乎任何需要走遍整個 list 並 accumulate 成一個結果的都可以用 fold 來實作。

```haskell
sequenceA :: (Applicative f) => [f a] -> f [a]  
sequenceA = foldr (liftA2 (:)) (pure [])  
```

我們從右往左走，並且起始的 accumulator 是用 ``pure []``。我們是用 ``liftA2 (:)`` 來結合 accumulator 跟 list 中最後的元素，而得到一個 applicative，裡面裝有一個單一元素的一個 list。然後我們再用 ``liftA2 (:)`` 來結合 accumulator 跟最後一個元素，直到我們只剩下 accumulator 為止，而得到一個 applicative，裡面裝有所有結果。


我們來試試看套用在不同 applicative 上。

```haskell
ghci> sequenceA [Just 3, Just 2, Just 1]  
Just [3,2,1]  
ghci> sequenceA [Just 3, Nothing, Just 1]  
Nothing  
ghci> sequenceA [(+3),(+2),(+1)] 3  
[6,5,4]  
ghci> sequenceA [[1,2,3],[4,5,6]]  
[[1,4],[1,5],[1,6],[2,4],[2,5],[2,6],[3,4],[3,5],[3,6]]  
ghci> sequenceA [[1,2,3],[4,5,6],[3,4,4],[]]  
[]  
```

很酷吧。當我們套用在 ``Maybe`` 上時，``sequenceA`` 創造一個新的 ``Maybe``，他包含了一個 list 裝有所有結果。如果其中一個值是 ``Nothing``，那整個結果就會是 ``Nothing``。如果你有一串 ``Maybe`` 型別的值，但你只在乎當結果不包含任何 ``Nothing`` 的情況，這樣的特性就很方便。


當套用在函數時，``sequenceA`` 接受裝有一堆函數的 list，並回傳一個回傳 list 的函數。在我們的範例中，我們寫了一個函數，他只接受一個數值作為參數，他會把他套用至 list 中的每一個函數，並回傳一個包含結果的 list。``sequenceA [(+3),(+2),(+1)] 3`` 會將 ``3`` 餵給 ``(+3)``, ``(+2)`` 跟 ``(+1)``，然後將所有結果裝在一個 list 中。


而 ``(+) <$> (+3) <*> (*2)`` 會創見一個接受單一參數的一函數，將他同時餵給 ``(+3)`` 跟 ``(*2)``，然後呼叫 ``+`` 來將兩者加起來。同樣的道理，``sequenceA [(+3),(*2)]`` 是製造一個接受單一參數的函數，他會將他餵給所有包含在 list 中的函數。但他最後不是呼叫 ``+``，而是呼叫 ``:`` 跟 ``pure []`` 來把結果接成一個 list，得到最後的結果。


當我們有一串函數，我們想要將相同的輸入都餵給他們並檢視結果的時候，``sequenceA`` 非常好用。例如說，我們手上有一個數值，但不知道他是否滿足一串 predicate。一種實作的方式是像這樣：

```haskell
ghci> map (\f -> f 7) [(>4),(<10),odd]  
[True,True,True]  
ghci> and $ map (\f -> f 7) [(>4),(<10),odd]  
True  
```


記住 ``and`` 接受一串布林值，並只有在全部都是 ``True`` 的時候才回傳 ``True``。
另一種實作方式是用 ``sequenceA``：


```haskell
ghci> sequenceA [(>4),(<10),odd] 7  
[True,True,True]  
ghci> and $ sequenceA [(>4),(<10),odd] 7  
True  
```

``sequenceA [(>4),(<10),odd]`` 接受一個函數，他接受一個數值並將他餵給所有的 predicate，包含 ``[(>4),(<10),odd]``。然後回傳一串布林值。他將一個型別為 ``(Num a) => [a -> Bool]`` 的 list 變成一個型別為 ``(Num a) => a -> [Bool]`` 的函數，很酷吧。


由於 list 要求裡面元素的型別要一致，所以包含在 list 中的所有函數都是同樣型別。你不能創造一個像是 ``[ord, (+3)]`` 這樣的 list，因為 ``ord`` 接受一個字元並回傳一個數值，然而 ``(+3)`` 接受一個數值並回傳一個數值。


當跟 ``[]`` 一起使用的時候，``sequenceA`` 接受一串 list，並回傳另一串 list。他實際上是創建一個包含所有可能組合的 list。為了方便說明，我們比較一下使用 ``sequenceA`` 跟 list comprehension 的差異：

```haskell
ghci> sequenceA [[1,2,3],[4,5,6]]  
[[1,4],[1,5],[1,6],[2,4],[2,5],[2,6],[3,4],[3,5],[3,6]]  
ghci> [[x,y] | x <- [1,2,3], y <- [4,5,6]]  
[[1,4],[1,5],[1,6],[2,4],[2,5],[2,6],[3,4],[3,5],[3,6]]  
ghci> sequenceA [[1,2],[3,4]]  
[[1,3],[1,4],[2,3],[2,4]]  
ghci> [[x,y] | x <- [1,2], y <- [3,4]]  
[[1,3],[1,4],[2,3],[2,4]]  
ghci> sequenceA [[1,2],[3,4],[5,6]]  
[[1,3,5],[1,3,6],[1,4,5],[1,4,6],[2,3,5],[2,3,6],[2,4,5],[2,4,6]]  
ghci> [[x,y,z] | x <- [1,2], y <- [3,4], z <- [5,6]]  
[[1,3,5],[1,3,6],[1,4,5],[1,4,6],[2,3,5],[2,3,6],[2,4,5],[2,4,6]]  
```

這可能有點難以理解，但如果你多做點嘗試，你會比較能看出來些眉目。假設我們在做 ``sequenceA [[1,2],[3,4]]``。要知道這是怎麼回事，我們首先用 ``sequenceA`` 的定義 ``sequenceA (x:xs) = (:) <$> x <*> sequenceA xs`` 還有邊界條件 ``sequenceA [] = pure []`` 來看看。你不需要實際計算，但他可以幫助你理解 ``sequenceA`` 是怎麼運作在一串 list 上，畢竟這有點複雜。

    # 我們從 ``sequenceA [[1,2],[3,4]]`` 開始
    # 那可以被計算成 ``(:) <$> [1,2] <*> sequenceA [[3,4]]``
    # 計算內層的 ``sequenceA``，會得到 ``(:) <$> [1,2] <*> ((:) <$> [3,4] <*> sequenceA [])``
    # 我們碰到了邊界條件，所以會是 ``(:) <$> [1,2] <*> ((:) <$> [3,4] <*> [[]])``
    # 現在我們計算 ``(:) <$> [3,4] <*> [[]] `` 的部份，我們會對左邊 list 中的每一個值 (也就是 ``3`` 跟 ``4``) 跟右邊的每一個值 (只有 ``[]``)套用 ``:``，而得到 ``[3:[], 4:[]]``，也就是 ``[[3],[4]]``。所以我們有 ``(:) <$> [1,2] <*> [[3],[4]]``
    # 而對於左邊的每一個值(``1`` 跟 ``2``)以及右邊可能的值（``[3]`` 跟 ``[4]``）我們套用 ``:`` 而得到 ``[1:[3], 1:[4], 2:[3], 2:[4]]``，他等於 ``[[1,3],[1,4],[2,3],[2,4]]``


計算 ``(+) <$> [1,2] <*> [4,5,6]`` 會得到一個 non-deterministic 的結果 ``x + y``，其中 ``x`` 代表 ``[1,2]`` 中的每一個值，而 ``y`` 代表 ``[4,5,6]`` 中的每一個值。我們用 list 來表示每一種可能的情形。同樣的，當我們在做 ``sequence [[1,2],[3,4],[5,6],[7,8]]``，他的結果會是 non-deterministic 的 ``[x,y,z,w]``，其中 ``x`` 代表 ``[1,2]`` 中的每一個值，而 ``y`` 代表 ``[3,4]`` 中的每一個值。以此類推。我們用 list 代表 non-deterministic 的計算，每一個元素都是一個可能的情形。這也是為什麽會用到 list of list。


當使用在 I/O action 上的時候，``sequenceA`` 跟 ``sequence`` 是等價的。他接受一串 I/O action 並回傳一個 I/O action，這個 I/O action 會計算 list 中的每一個 I/O action，並把結果放在一個 list 中。要將型別為 ``[IO a]`` 的值轉換成 ``IO [a]`` 的值，也就是會產生一串 list 的一個 I/O action，那這些 I/O action 必須要一個一個地被計算，畢竟對於這些 I/O action 你沒辦法不計算就得到結果。

```haskell
ghci> sequenceA [getLine, getLine, getLine]  
heyh  
ho  
woo  
["heyh","ho","woo"]  
```


就像普通的函數一樣，applicative functors 也遵循一些定律。其中最重要的一個是我們之前提過的 ``pure f <*> x = fmap f x``。你可以證明一些我們之前介紹過的 applicative functor 遵守這個定律當作練習。其他的 functors law 有：

    # ``pure id <*> v = v``
    # ``pure (.) <*> u <*> v <*> w = u <*> (v <*> w)``
    # ``pure f <*> pure x = pure (f x)``
    # ``u <*> pure y = pure ($ y) <*> u``

我們不會一項一項地細看，那樣會花費很大的篇幅而且對讀者來說很無聊，但如果你有興趣，你可以針對某些 instance 看看他們會不會遵守。


結論就是 applicative functor 不只是有趣而且實用， 他允許我們結合不同種類的計算，像是 I/O 計算，non-deterministic 的計算，有可能失敗的計算等等。而使用 ``<$>`` 跟 ``<*>`` 我們可以將普通的函數來運作在任意數量的 applicative functors 上。



## 關鍵字"newtype"

![](maoi.png)

到目前為止，我們已經看過了如何用 ``data`` 關鍵字定義自己的 algebraic data type。我們也學習到了如何用 ``type`` 來定義 type synonyms。在這個章節中，我們會看一下如何使用 ``newtype`` 來從一個現有的型別中定義出新的型別，並說明我們為什麽會想要那麼做。


在之前的章節中，我們瞭解到其實 list 有很多種方式可以被視為一種 applicative functor。一中方式是定義 ``<*>`` 將左邊的每一個值跟右邊的每一個值組合，而得到各種組合的結果。


```haskell
ghci> [(+1),(*100),(*5)] <*> [1,2,3]  
[2,3,4,100,200,300,5,10,15]  
```

第二種方式是將 ``<*>`` 定義成將左邊的第一個函數套用至右邊的第一個值，然後將左邊第二個函數套用至右邊第二個值。以此類推。最終，這表現得有點像將兩個 list 用一個拉鍊拉起來一樣。但由於 list 已經被定義成 ``Applicaitive`` 的 instance 了，所以我們要怎麼要讓 list 可以被定義成第二種方式呢？如果你還記得我們說過我們是有很好的理由定義了 ``ZipList a``，其中他裡面只包含一個值構造子跟只包含一個欄位。其實他的理由就是要讓 ``ZipList`` 定義成用拉鍊的方式來表現 applicative 行為。我們只不過用 ``ZipList`` 這個構造子將他包起來，然後用 ``getZipList`` 來解開來。


```haskell
ghci> getZipList $ ZipList [(+1),(*100),(*5)] <*> ZipList [1,2,3]  
[2,200,15]  
```

所以這跟 newtype 這個關鍵字有什麽關係呢？想想看我們是怎麼宣告我們的 ``ZipList a`` 的，一種方式是像這樣：


```haskell
data ZipList a = ZipList [a]      
```

也就是一個只有一個值構造子的型別而且那個構造子裡面只有一個欄位。我們也可以用 record syntax 來定義一個解開的函數：

```haskell
data ZipList a = ZipList { getZipList :: [a] }      
```

這樣聽起來不錯。這樣我們就有兩種方式來讓一個型別來表現一個 typeclass，我們可以用 ``data`` 關鍵字來把一個型別包在另一個裡面，然後再將他定義成第二種表現方式。

而在 Haskell 中 ``newtype`` 正是為了這種情形，我們想將一個型別包在另一個型別中。在實際的函式庫中 ``ZipList a`` 是這樣定義了：

```haskell
newtype ZipList a = ZipList { getZipList :: [a] }      
```

這邊我們不用 ``data`` 關鍵字反而是用 ``newtype`` 關鍵字。這是為什麽呢？第一個理由是 ``newtype`` 比較快速。如果你用 ``data`` 關鍵字來包一個型別的話，在你執行的時候會有一些包起來跟解開來的成本。但如果你用 ``newtype`` 的話，Haskell 會知道你只是要將一個現有的型別包成一個新的型別，你想要內部運作完全一樣但只是要一個全新的型別而已。有了這個概念，Haskell 可以將包裹跟解開來的成本都去除掉。


那為什麽我們不是一直使用 ``newtype`` 呢？當你用 ``newtype`` 來製作一個新的型別時，你只能定義單一一個值構造子，而且那個構造子只能有一個欄位。但使用 ``data`` 的話，你可以讓那個型別有好幾個值構造子，並且每個構造子可以有零個或多個欄位。

```haskell
data Profession = Fighter | Archer | Accountant  
  
data Race = Human | Elf | Orc | Goblin  

data PlayerCharacter = PlayerCharacter Race Profession  
```

當使用 ``newtype 的時候，你是被限制只能用一個值構造子跟單一欄位。


對於 ``newtype`` 我們也能使用 ``deriving`` 關鍵字。我們可以 derive 像是 ``Eq``, ``Ord``, ``Enum``, ``Bounded``, ``Show`` 跟 ``Read`` 的 instance。如果我們想要對新的型別做 derive，那原本的型別必須已經在那個 typeclass 中。這樣很合理，畢竟 ``newtype`` 就是要將現有的型別包起來。如果我們按照下面的方式定義的話，我們就能對我們的型別做印出以及比較相等性的操作：


```haskell
newtype CharList = CharList { getCharList :: [Char] } deriving (Eq, Show)      
```

我們來跑跑看：

```haskell
ghci> CharList "this will be shown!"  
CharList {getCharList = "this will be shown!"}  
ghci> CharList "benny" == CharList "benny"  
True  
ghci> CharList "benny" == CharList "oisters"  
False  
```

對於這個 ``newtype``，他的值構造子有下列型別：

```haskell
CharList :: [Char] -> CharList      
```

他接受一個 ``[Char]`` 的值，例如 ``"my sharona"`` 並回傳一個 ``CharList`` 的值。從上面我們使用 ``CharList`` 的值構造子的範例中，我們可以看到的確是這樣。相反地，``getCharList`` 具有下列的型別。

```haskell
getCharList :: CharList -> [Char]      
```

他接受一個 ``CharList`` 的值並將他轉成 ``[Char]``。你可以將這個想成包裝跟解開的動作，但你也可以將他想成從一個型別轉成另一個型別。


### Using newtype to make type class instances

有好幾次我們想要讓我們的型別屬於某個 typeclass，但型別變數並沒有符合我們想要的。要把 ``Maybe`` 定義成 ``Functor`` 的 instance 很容易，因為 ``Functor`` 這個 typeclass 被定義如下：

```haskell
class Functor f where  
    fmap :: (a -> b) -> f a -> f b  
```

我們先定義如下：

```haskell
instance Functor Maybe where       
```

然後我們實作 ``fmap``。當所有的型別變數被填上時，由於 ``Maybe`` 取代了 ``Functor`` 中 ``f`` 的位置，所以如果我們看看 ``fmap`` 運作在 ``Maybe`` 上時是什麽樣，他會像這樣：


```haskell
fmap :: (a -> b) -> Maybe a -> Maybe b      
```

![](shamrock.png)

看起來不錯吧？現在我們想要 tuple 成為 ``Functor`` 的一個 instance，所以當我們用 ``fmap`` 來 map over 一個 tuple 時，他會先套用到 tuple 中的第一個元素。這樣當我們做 ``fmap (+3) (1,1)`` 會得到 ``(4,1)``。不過要定義出這樣的 instance 有些困難。對於 ``Maybe``，我們只要寫 ``instance Functor Maybe where``，這是因為對於只吃一個參數的型別構造子我們很容易定義成 ``Functor`` 的 instance。但對於 ``(a,b)`` 這樣的就沒辦法。要繞過這樣的困境，我們可以用 ``newtype`` 來重新定義我們的 tuple，這樣第二個型別參數就代表了 tuple 中的第一個元素部份。

```haskell
newtype Pair b a = Pair { getPair :: (a,b) }      
```

現在我們可以將他定義成 ``Functor`` 的 instance，所以函數被 map over tuple 中的第一個部份。

```haskell
instance Functor (Pair c) where  
    fmap f (Pair (x,y)) = Pair (f x, y)  
```

正如你看到的，我們可以對 newtype 定義的型別做模式匹配。我們用模式匹配來拿到底層的 tuple，然後我們將 ``f`` 來套用至 tuple 的第一個部份，然後我們用 ``Pair`` 這個值構造子來將 tuple 轉換成 ``Pair b a``。如果我們問 ``fmap`` 的型別究竟是什麽，他會是：

```haskell
fmap :: (a -> b) -> Pair c a -> Pair c b      
```

我們說過 ``instance Functor (Pair c) where`` 跟 ``Pair c`` 取代了 ``Functor`` 中 ``f`` 的位置：

```haskell
class Functor f where  
    fmap :: (a -> b) -> f a -> f b  
```

如果我們將一個 tuple 轉換成 ``Pair b a``，我們可以用 ``fmap`` 來 map over 第一個部份。

```haskell
ghci> getPair $ fmap (*100) (Pair (2,3))  
(200,3)  
ghci> getPair $ fmap reverse (Pair ("london calling", 3))  
("gnillac nodnol",3)  
```

### On newtype laziness

我們提到 ``newtype`` 一般來講比 ``data`` 來得有效率。``newtype`` 能做的唯一一件事就是將現有的型別包成新的型別。這樣 Haskell 在內部就能將新的型別的值用舊的方式來操作。只是要記住他們還是不同的型別。這代表 ``newtype`` 並不只是有效率，他也具備 lazy 的特性。我們來說明一下這是什麽意思。


就像我們之前說得，Haskell 預設是具備 lazy 的特性，這代表只有當我們要將函數的結果印出來的時候計算才會發生。或者說，只有當我們真的需要結果的時候計算才會發生。在 Haskell 中 ``undefined`` 代表會造成錯誤的計算。如果我們試著計算他，也就是將他印到終端中，Haskell 會丟出錯誤。

```haskell
ghci> undefined  
*** Exception: Prelude.undefined  
```

然而，如果我們做一個 list，其中包含一些 ``undefined`` 的值，但卻要求一個不是 ``undefined`` 的 head，那一切都會順利地被計算，因為 Haskell 並不需要 list 中其他元素來得到結果。我們僅僅需要看到第一個元素而已。

```haskell
ghci> head [3,4,5,undefined,2,undefined]  
3  
```

現在們考慮下面的型別：

```haskell
data CoolBool = CoolBool { getCoolBool :: Bool }      
```

這是一個用 ``data`` 關鍵字定義的 algebraic data type。他有一個值建構子並只有一個型別為 ``Bool`` 的欄位。我們寫一個函數來對 ``CoolBool`` 做模式匹配，並回傳一個 ``"hello"`` 的值。他並不會管 ``CoolBool`` 中裝的究竟是 ``True`` 或 ``False``。

```haskell
helloMe :: CoolBool -> String  
helloMe (CoolBool _) = "hello"  
```

這次我們不餵給這個函數一個普通的 ``CoolBool``，而是丟給他一個 ``undefined``。

``` 
ghci> helloMe undefined  
"*** Exception: Prelude.undefined  "
```

結果收到了一個 Exception。是什麽造成這個 Exception 的呢？用 ``data`` 定義的型別可以有好幾個值構造子（儘管 ``CoolBool`` 只有一個）所以當我們要看看餵給函數的值是否是 ``(CoolBool _)`` 的形式，Haskell 會需要做一些基本的計算來看看是哪個值構造子被用到。但當我們計算 ``undefined`` 的時候，就算是一點也會丟出 Exception。


我們不用 ``data`` 來定義 ``CoolBool`` 而用 ``newtype``：

```haskell
newtype CoolBool = CoolBool { getCoolBool :: Bool }      
```

我們不用修改 ``helloMe`` 函數，因為對於模式匹配使用 ``newtype`` 或 ``data`` 都是一樣。我們再來將 ``undefined`` 餵給 ``helloMe``。

```haskell
ghci> helloMe undefined  
"hello" 
```

居然正常運作！為什麽呢？正如我們說過得，當我們使用 ``newtype`` 的時候，Haskell 內部可以將新的型別用舊的型別來表示。他不必加入另一層 box 來包住舊有的型別。他只要注意他是不同的型別就好了。而且 Haskell 會知道 ``newtype`` 定義的型別一定只會有一個構造子，他不必計算餵給函數的值就能確定他是 ``(CoolBool _)`` 的形式，因為 ``newtype`` 只有一個可能的值跟單一欄位！


這樣行為的差異可能沒什麼關係，但實際上他非常重要。因為他讓我們認知到儘管從撰寫程式的觀點來看沒什麽差異，但他們的確是兩種不同的機制。儘管 ``data`` 可以讓你從無到有定義型別，``newtype`` 是從一個現有的型別做出來的。對 ``newtype`` 做模式匹配並不是像從盒子中取出東西，他比較像是將一個型別轉換成另一個型別。


### type vs newtype vs data

到目前為止，你也許對於 ``type``,``data`` 跟 ``newtype`` 之間的差異還不是很瞭解，讓我們快速複習一遍。


``type`` 關鍵字是讓我們定義 type synonyms。他代表我們只是要給一個現有的型別另一個名字，假設我們這樣做：

```haskell
type IntList = [Int]      
```

這樣做可以允許我們用 ``IntList`` 的名稱來指稱 ``[Int]``。我們可以交換地使用他們。但我們並不會因此有一個 ``IntList`` 的值構造子。因為 ``[Int]`` 跟 ``IntList`` 只是兩種指稱同一個型別的方式。我們在指稱的時候用哪一個並無所謂。

```haskell
ghci> ([1,2,3] :: IntList) ++ ([1,2,3] :: [Int])  
[1,2,3,1,2,3]  
```

當我們想要讓 type signature 更清楚一些，給予我們更瞭解函數的 context 的時候，我們會定義 type synonyms。舉例來說，當我們用一個型別為 ``[(String,String)]`` 的 association list 來代表一個電話簿的時候，我們可以定義一個 ``PhoneBook`` 的 type synonym，這樣 type signature 會比較容易讀。

 
``newtype`` 關鍵字將現有的型別包成一個新的型別，大部分是為了要讓他們可以是特定 typeclass 的 instance 而這樣做。當我們使用 ``newtype`` 來包裹一個現有的型別時，這個型別跟原有的型別是分開的。如果我們將下面的型別用 ``newtype`` 定義：

```haskell
newtype CharList = CharList { getCharList :: [Char] }      
```

我們不能用 ``++`` 來將 ``CharList`` 跟 ``[Char]`` 接在一起。我們也不能用 ``++`` 來將兩個 ``CharList`` 接在一起，因為 ``++`` 只能套用在 list 上，而 ``CharList`` 並不是 list，儘管你會說他包含一個 list。但我們可以將兩個 ``CharList`` 轉成 list，將他們 ``++`` 然後再轉回 ``CharList``。


當我們在 ``newtype`` 宣告中使用 record syntax 的時候，我們會得到將新的型別轉成舊的型別的函數，也就是我們 ``newtype`` 的值構造子，以及一個函數將他的欄位取出。新的型別並不會被自動定義成原有型別所屬的 typeclass 的一個 instance，所以我們必須自己來 derive 他們。


實際上你可以將 ``newtype`` 想成是只能定義一個構造子跟一個欄位的 ``data`` 宣告。如果你碰到這種情形，可以考慮使用 ``newtype``。


使用 ``data`` 關鍵字是為了定義自己的型別。他們可以在 algebraic data type 中放任意數量的構造子跟欄位。可以定義的東西從 list, ``Maybe`` 到 tree。

如果你只是希望你的 type signature 看起來比較乾淨，你可以只需要 type synonym。如果你想要將現有的型別包起來並定義成一個 type class 的 instance，你可以嘗試使用 newtype。如果你想要定義完全新的型別，那你應該使用 ``data`` 關鍵字。



## Monoids


Haskell 中 typeclass 是用來表示一個型別之間共有的行為，是一種 interface。我們介紹過 ``Eq``，他定義型別是否可以比較相等性，以及 ``Ord``，他表示可以被排序的型別。還介紹了更有趣的像是 ``Functor`` 跟 ``Applicative``。


當我們定義一個型別時，我們會想說他應該要支援的行為。也就是表現的行為是什麽，並且要讓他屬於哪些 typeclass。如果希望他可以比較相等與否，那我們就應該定義他成為 ``Eq`` 的一個 instance。如果我們想要看看型別是否是一種 functor，我們可以定義他是 ``Functor`` 的一個 instance。以此類推。


考慮 ``*`` 是一個將兩個數值相乘的一個函數。如果我們將一個數值乘上 ``1``，那就會得到自身的數值。我們實際上是做 ``1 * x`` 或 ``x * 1`` 並沒有差別。結果永遠會是 ``x``。同樣的，``++`` 是一個接受兩個參數並回傳新的值的一個函數。只是他不是相乘而是將兩個 list 接在一起。而類似 ``*``，他也有一個特定的值，當他跟其他值使用 ``++`` 時會得到同樣的值。那個值就是空的 list ``[]``。


```haskell
ghci> 4 * 1  
4  
ghci> 1 * 9  
9  
ghci> [1,2,3] ++ []  
[1,2,3]  
ghci> [] ++ [0.5, 2.5]  
[0.5,2.5]  
```

看起來 ``*`` 之於 ``1`` 跟 ``++`` 之於 ``[]`` 有類似的性質：

    # 函數同樣接受兩個參數
    # 參數跟回傳值是同樣的型別
    # 同樣存在某些值當套用二元函數時並不會改變其他值

關於這兩種操作還有另一個比較難察覺的性質就是，當我們對這個二元函數對三個以上的值操作並化簡，函數套用的順序並不會影響到結果。不論是 ``(3 * 4) * 5`` 或是 ``3 * (4 * 5)``，兩種方式都會得到 ``60``。而 ``++`` 也是相同的。

```haskell
ghci> (3 * 2) * (8 * 5)  
240  
ghci> 3 * (2 * (8 * 5))  
240  
ghci> "la" ++ ("di" ++ "da")  
"ladida"  
ghci> ("la" ++ "di") ++ "da"  
"ladida"  
```

我們稱呼這樣的性質為結合律(associativity)。``*`` 遵守結合律，``++`` 也是。但 ``-`` 就不遵守。``(5 - 3) - 4`` 跟 ``5 - (3 - 4)`` 得到的結果是不同的。


注意到這些性質並具體地寫下來，就可以得到 monoid。一個 monoid 是你有一個遵守結合律的二元函數還有一個可以相對於那個函數作為 identity 的值。當某個值相對於一個函數是一個 identity，他表示當我們將這個值丟給函數時，結果永遠會是另外一邊的那個值本身。``1`` 是相對於 ``*`` 的 identity，而 ``[]`` 是相對於 ``++`` 的 identity。在 Haskell 中還有許多其他的 monoid，這也是為什麽我們定義了 ``Monoid`` 這個 typeclass。他描述了表現成 monoid 的那些型別。我們來看看這個 typeclass 是怎麼被定義的：

```haskell
class Monoid m where  
    mempty :: m  
    mappend :: m -> m -> m  
    mconcat :: [m] -> m  
    mconcat = foldr mappend mempty  
```

![](balloondog.png)

``Monoid`` typeclass 被定義在 ``import Data.Monoid`` 中。我們來花些時間好好瞭解他。


首先我們看到只有具體型別才能定義成 ``Monoid`` 的 instance。由於在 typeclass 定義中的 ``m`` 並不接受任何型別參數。這跟 ``Functor`` 以及 ``Applicative`` 不同，他們要求他們的 instance 必須是一個接受單一型別參數的型別構造子。


第一個函數是 ``mempty``，由於他不接受任何參數，所以他並不是一個函數，而是一個 polymorphic 的常數。有點像是 ``Bounded`` 中的 ``minBound`` 一樣。``mempty`` 表示一個特定 monoid 的 identity。


再來我們看到 ``mappend``，你可能已經猜到，他是一個接受兩個相同型別的值的二元函數，並回傳同樣的型別。不過要注意的是他的名字不太符合他真正的意思，他的名字隱含了我們要將兩個東西接在一起。儘管在 list 的情況下 ``++`` 的確將兩個 list 接起來，但 ``*`` 則否。他只不過將兩個數值做相乘。當我們再看到其他 ``Monoid`` 的 instance 時，我們會看到他們大部分都沒有接起來的做，所以不要用接起來的概念來想像 ``mappend``，只要想像他們是接受兩個 monoid 的值並回傳另外一個就好了。


在 typeclass 定義中的最後一個函數是 ``mconcat``。他接受一串 monoid 值，並將他們用 ``mappend`` 簡化成單一的值。他有一個預設的實作，就是從 ``mempty`` 作為起始值，然後用 ``mappend`` 來 fold。由於對於大部分的 instance 預設的實作就沒什麼問題，我們不會想要實作自己的 ``mconcat``。當我們定義一個型別屬於 ``Monoid`` 的時候，多半實作 ``mempty`` 跟 ``mappend`` 就可以了。而 ``mconcat`` 就是因為對於一些 instance，有可能有比較有效率的方式來實作 ``mconcat``。不過大多數情況都不需要。



在我們繼續接下去看幾個 ``Monoid`` 的例子前，我們來看一下 monoid law。我們提過必須有一個值作為 identity 以及一個遵守結合律的二元函數當作前提。我們是可以定義一個 ``Monoid`` 的 instance 卻不遵守這些定律的，但這樣寫出來的 instance 就沒有用了，因為我們在使用 ``Monoid`` 的時候都是依靠這些定律才可以稱作實質上的 monoid。所以我們必須確保他們遵守：

    # ``mempty `mappend` x = x``
    # ``x `mappend` mempty = x``
    # ``(x `mappend` y) `mappend` z = x `mappend` (y `mappend` z)``

前兩個描述了 ``mempty`` 相對於 ``mappend`` 必須要表現成 identity。而第三個定律說了 ``mappend`` 必須要遵守結合律。也就是說我們做 ``mappend`` 順序並不重要。Haskell 不會自己檢查這些定律是否有被遵守。所以你必須自己小心地檢查他們。



### Lists are monoids

沒錯，list 是一種 monoid。正如我們先前看到的，``++`` 跟空的 list ``[]`` 共同形成了一個 monoid。他的 instance 很簡單：


```haskell
instance Monoid [a] where  
    mempty = []  
    mappend = (++)  
```

list 是 ``Monoid`` typeclass 的一個 instance，這跟他們裝的元素的型別無關。注意到我們寫 ``instance Monoid [a]`` 而非 ``instance Monoid []``，這是因為 ``Monoid`` 要求 instance 必須是具體型別。

我們試著跑跑看，得到我們預期中的結果：

```haskell
ghci> [1,2,3] `mappend` [4,5,6]  
[1,2,3,4,5,6]  
ghci> ("one" `mappend` "two") `mappend` "tree"  
"onetwotree"  
ghci> "one" `mappend` ("two" `mappend` "tree")  
"onetwotree"  
ghci> "one" `mappend` "two" `mappend` "tree"  
"onetwotree"  
ghci> "pang" `mappend` mempty  
"pang"  
ghci> mconcat [[1,2],[3,6],[9]]  
[1,2,3,6,9]  
ghci> mempty :: [a]  
[]  
```

![](smug.png)

注意到最後一行我們明白地標記出型別。這是因為如果只些 ``mempty`` 的話，GHCi 不會知道他是哪一個 instance 的 ``mempty``，所以我們必須清楚說出他是 list instance 的 mempty。我們可以使用一般化的型別 ``[a]``，因為空的 list 可以看作是屬於任何型別。


由於 ``mconcat`` 有一個預設的實作，我們將某個型別定義成 ``Monoid`` 的型別時就可以自動地得到預設的實作。但對於 list 而言，``mconcat`` 其實就是 ``concat``。他接受一個裝有 list 的 list，並把他用 ``++`` 來扁平化他。


list 的 instance 也遵守 monoid law。當我們有好幾個 list 並且用 ``mappend`` 來把他們串起來，先後順序並不是很重要，因為他們都是接在最後面。而且空的 list 也表現得如 identity 一樣。注意到 monoid 並不要求 ``a `mappend` b`` 等於 ``b `mappend` a``。在 list 的情況下，他們明顯不相等。

```haskell
ghci> "one" `mappend` "two"  
"onetwo"  
ghci> "two" `mappend` "one"  
"twoone"  
```

這樣並沒有關系。``3 * 5`` 跟 ``5 * 3`` 會相等只不過是乘法的性質而已，但沒有保證所有 monoid 都要遵守。

### Product and Sum

我們已經描述過將數值表現成一種 monoid 的方式。只要將 ``*`` 當作二元函數而 ``1`` 當作 identity 就好了。而且這不是唯一一種方式，另一種方式是將 ``+`` 作為二元函數而 ``0`` 作為 identity。

```haskell
ghci> 0 + 4  
4  
ghci> 5 + 0  
5  
ghci> (1 + 3) + 5  
9  
ghci> 1 + (3 + 5)  
9  
```

他也遵守 monoid law，因為將 0 加上其他數值，都會是另外一者。而且加法也遵守結合律。所以現在我們有兩種方式來將數值表現成 monoid，那要選哪一個呢？其實我們不必要強迫定下來，還記得當同一種型別有好幾種表現成某個 typeclass 的方式時，我們可以用 ``newtype`` 來包裹現有的型別，然後再定義新的 instance。這樣就行了。


``Data.Monoid`` 這個模組匯出了兩種型別，``Product`` 跟 ``Sum``。``Product`` 定義如下：

```haskell
newtype Product a =  Product { getProduct :: a }  
    deriving (Eq, Ord, Read, Show, Bounded)  
```

簡單易懂，就是一個單一型別參數的 ``newtype``，並 derive 一些性質。他的 ``Monoid`` 的 instance 長得像這樣：

```haskell
instance Num a => Monoid (Product a) where  
    mempty = Product 1  
    Product x `mappend` Product y = Product (x * y)  
```

``mempty`` 只不過是將 ``1`` 包在 ``Product`` 中。``mappend`` 則對 ``Product`` 的構造子做模式匹配，將兩個取出的數值相乘後再將結果放回去。就如你看到的，typeclass 定義前面有 ``Num a`` 的條件限制。所以他代表 ``Product a`` 對於所有屬於 ``Num`` 的 ``a`` 是一個 ``Monoid``。要將 ``Product a`` 作為一個 monoid 使用，我們需要用 newtype 來做包裹跟解開的動作。

```haskell
ghci> getProduct $ Product 3 `mappend` Product 9  
27  
ghci> getProduct $ Product 3 `mappend` mempty  
3  
ghci> getProduct $ Product 3 `mappend` Product 4 `mappend` Product 2  
24  
ghci> getProduct . mconcat . map Product $ [3,4,2]  
24  
```

這當作 ``Monoid`` 的一個演練還不錯，但並不會有人覺得這會比 ``3 * 9`` 跟 ``3 * 1`` 這種方式來做乘法要好。但我們稍後會說明儘管像這種顯而易見的定義還是有他方便的地方。

``Sum`` 跟 ``Product`` 定義的方式類似，我們也可以用類似的方式操作：

```haskell
ghci> getSum $ Sum 2 `mappend` Sum 9  
11  
ghci> getSum $ mempty `mappend` Sum 3  
3  
ghci> getSum . mconcat . map Sum $ [1,2,3]  
6  
```


### Any and ALL

另一種可以有兩種表示成 monoid 方式的型別是 ``Bool``。第一種方式是將 ``||`` 當作二元函數，而 ``False`` 作為 identity。這樣的意思是只要有任何一個參數是 ``True`` 他就回傳 ``True``，否則回傳 ``False``。所以如果我們使用 ``False`` 作為 identity，他會在跟 ``False`` 做 OR 時回傳 ``False``，跟 ``True`` 做 OR 時回傳 ``True``。``Any`` 這個 newtype 是 ``Monoid`` 的一個 instance，並定義如下：

```haskell
newtype Any = Any { getAny :: Bool }  
    deriving (Eq, Ord, Read, Show, Bounded)  
```

他的 instance 長得像這樣：

```haskell
instance Monoid Any where  
    mempty = Any False  
    Any x `mappend` Any y = Any (x || y)  
```

他叫做 ``Any`` 的理由是 ``x `mappend` y`` 當有任何一個是 ``True`` 時就會是 ``True``。就算是更多個用 ``mappend`` 串起來的 ``Any``，他也會在任何一個是 ``True`` 回傳 ``True``。


```haskell
ghci> getAny $ Any True `mappend` Any False  
True  
ghci> getAny $ mempty `mappend` Any True  
True  
ghci> getAny . mconcat . map Any $ [False, False, False, True]  
True  
ghci> getAny $ mempty `mappend` mempty  
False  
```

另一種 ``Bool`` 表現成 ``Monoid`` 的方式是用 ``&&`` 作為二元函數，而 ``True`` 作為 identity。只有當所有都是 ``True`` 的時候才會回傳 ``True``。下面是他的 newtype 定義：

```haskell
newtype All = All { getAll :: Bool }  
        deriving (Eq, Ord, Read, Show, Bounded)  
```

而這是他的 instance：

```haskell
instance Monoid All where  
        mempty = All True  
        All x `mappend` All y = All (x && y)  
```

當我們用 ``mappend`` 來串起 ``All`` 型別的值時，結果只有當所有 ``mappend`` 的值是 ``True`` 時才會是 ``True``：

```haskell
ghci> getAll $ mempty `mappend` All True  
True  
ghci> getAll $ mempty `mappend` All False  
False  
ghci> getAll . mconcat . map All $ [True, True, True]  
True  
ghci> getAll . mconcat . map All $ [True, True, False]  
False  
```


就如乘法跟加法一樣，我們通常寧願用二元函數來操作他們也不會用 newtype 來將他們包起來。不會將他們包成 ``Any`` 或 ``All`` 然後用 ``mappend``，``mempty`` 或 ``mconcat`` 來操作。通常使用 ``or`` 跟 ``and``，他們接受一串 ``Bool``，並只有當任意一個或是所有都是 ``True`` 的時候才回傳 ``True``。


###  The Ordering monoid


還記得 ``Ordering`` 型別嗎?他是比較運算之後得到的結果，包含三個值：``LT``，``EQ`` 跟 ``GT``，分別代表小於，等於跟大於：

```haskell
ghci> 1 `compare` 2  
LT  
ghci> 2 `compare` 2  
EQ  
ghci> 3 `compare` 2  
GT  
```

針對 list，數值跟布林值而言，要找出 monoid 的行為只要去檢視已經定義的函數，然後看看有沒有展現出 monoid 的特性就可以了，但對於 ``Ordering``，我們就必須要更仔細一點才能看出來是否是一個 monoid，但其實他的 ``Monoid`` instance 還蠻直覺的：

```haskell
instance Monoid Ordering where  
    mempty = EQ  
    LT `mappend` _ = LT  
    EQ `mappend` y = y  
    GT `mappend` _ = GT  
```

![](bear.png)

這個 instance 定義如下：當我們用 ``mappend`` 兩個 ``Ordering`` 型別的值時，左邊的會被保留下來。除非左邊的值是 ``EQ``，那我們就會保留右邊的當作結果。而 identity 就是 ``EQ``。乍看之下有點隨便，但實際上他是我們比較兩個英文字時所用的方法。我們先比較兩個字母是否相等，如果他們不一樣，那我們就知道那一個字在字典中會在前面。而如果兩個字母相等，那我們就繼續比較下一個字母，以此類推。


舉例來說，如果我們字典順序地比較 ``"ox"`` 跟 ``"on"`` 的話。我們會先比較兩個字的首個字母，看看他們是否相等，然後繼續比較第二個字母。我們看到 ``'x'`` 是比 ``'n'`` 要來得大，所以我們就知道如何比較兩個字了。而要瞭解為何 ``EQ`` 是 identity，我們可以注意到如果我們在兩個字中間的同樣位置塞入同樣的字母，那他們之間的字典順序並不會改變。``"oix"`` 仍然比 ``"oin"`` 要大。


很重要的一件事是在 ``Ordering`` 的 ``Monoid`` 定義裡 ``x `mappend` y`` 並不等於 ``y `mappend` x``。因為除非第一個參數是 ``EQ``，不然結果就會是第一個參數。所以 ``LT `mappend` GT`` 等於 ``LT``，然而 ``GT `mappend` LT`` 等於 ``GT``。

```haskell
ghci> LT `mappend` GT  
LT  
ghci> GT `mappend` LT  
GT  
ghci> mempty `mappend` LT  
LT  
ghci> mempty `mappend` GT  
GT  
```

所以這個 monoid 在什麽情況下會有用呢？假設你要寫一個比較兩個字串長度的函數，並回傳 ``Ordering``。而且當字串一樣長的時候，我們不直接回傳 ``EQ``，反而繼續用字典順序比較他們。一種實作的方式如下：


```haskell
lengthCompare :: String -> String -> Ordering  
lengthCompare x y = let a = length x `compare` length y   
                        b = x `compare` y  
                    in  if a == EQ then b else a  
```

我們稱呼比較長度的結果為 ``a``，而比較字典順序的結果為 ``b``，而當長度一樣時，我們就回傳字典順序。

如果善用我們 ``Ordering`` 是一種 monoid 這項知識，我們可以把我們的函數寫得更簡單些：

```haskell
import Data.Monoid

lengthCompare :: String -> String -> Ordering  
lengthCompare x y = (length x `compare` length y) `mappend`  
                    (x `compare` y)  
```

我們可以試著跑跑看：

```haskell
ghci> lengthCompare "zen" "ants"  
LT  
ghci> lengthCompare "zen" "ant"  
GT  
```

要記住當我們使用 ``mappend``。他在左邊不等於 ``EQ`` 的情況下都會回傳左邊的值。相反地則回傳右邊的值。這也是為什麽我們將我們認為比較重要的順序放在左邊的參數。如果我們要繼續延展這個函數，要讓他們比較母音的順序，並把這順序列為第二重要，那我們可以這樣修改他：

```haskell
import Data.Monoid  
  
lengthCompare :: String -> String -> Ordering  
lengthCompare x y = (length x `compare` length y) `mappend`  
                    (vowels x `compare` vowels y) `mappend`  
                    (x `compare` y)  
    where vowels = length . filter (`elem` "aeiou")  
```

我們寫了一個輔助函數，他接受一個字串並回傳他有多少母音。他是先用 filter 來把字母濾到剩下 ``"aeiou"``，然後再用 ``length`` 計算長度。

```haskell
ghci> lengthCompare "zen" "anna"  
LT  
ghci> lengthCompare "zen" "ana"  
LT  
ghci> lengthCompare "zen" "ann"  
GT  
```

在第一個例子中我們看到長度不同所以回傳 ``LT``，明顯地 ``"zen"`` 要短於 ``"anna"``。在第二個例子中，長度是一樣的，但第二個字串有比較多的母音，所以結果仍然是 ``LT``。在第三個範例中，兩個長度都相等，他們也有相同個數的母音，經由字典順序比較後得到 ``"zen"`` 比較大。


``Ordering`` 的 monoid 允許我們用不同方式比較事物，並將這些順序也定義了依重要度不同的一個順序。


### Maybe the monoid


我們來看一下 ``Maybe a`` 是怎樣有多種方式來表現成 ``Monoid`` 的，並且說明哪些是比較有用的。一種將 ``Maybe a`` 當作 monoid 的方式就是他的 ``a`` 也是一個 monoid，而我們將 ``mappend`` 實作成使用包在 ``Just`` 裡面的值對應的 ``mappend``。並且用 ``Nothing`` 當作 identity。所以如果我 ``mappend`` 兩個參數中有一個是 ``Nothing``。那結果就會是另一邊的值。他的 instance 定義如下：	


```haskell
instance Monoid a => Monoid (Maybe a) where  
    mempty = Nothing  
    Nothing `mappend` m = m  
    m `mappend` Nothing = m  
    Just m1 `mappend` Just m2 = Just (m1 `mappend` m2)  
```

留意到 class constraint。他說明 ``Maybe a`` 只有在 ``a`` 是 ``Monoid`` 的情況下才會是一個 ``Monoid``。如果我們 ``mappend`` 某個東西跟 ``Nothing``。那結果就會是某個東西。如果我們 ``mappend`` 兩個 ``Just``，那 ``Just`` 包住的結果就會 ``mappended`` 在一起並放回 ``Just``。我們能這麼做是因為 class constraint 保證了在 ``Just`` 中的值是 ``Monoid``。

```haskell
ghci> Nothing `mappend` Just "andy"  
Just "andy"  
ghci> Just LT `mappend` Nothing  
Just LT  
ghci> Just (Sum 3) `mappend` Just (Sum 4)  
Just (Sum {getSum = 7})  
```

這當你在處理有可能失敗的 monoid 的時候比較有用。有了這個 instance，我們就不必一一去檢查他們是否失敗，是否是 ``Nothing`` 或是 ``Just``，我們可以直接將他們當作普通的 monoid。


但如果在 ``Maybe`` 中的型別不是 ``Monoid`` 呢？注意到在先前的 instance 定義中，唯一有依賴於 monoid 限制的情況就是在 ``mappend`` 兩個 ``Just`` 的時候。但如果我們不知道包在 ``Just`` 裡面的值究竟是不是 monoid，我們根本無法用 ``mappend`` 操作他們，所以該怎麼辦呢？一種方式就是直接丟掉第二個值而留下第一個值。這就是 ``First a`` 存在的目的，而這是他的定義：

```haskell
newtype First a = First { getFirst :: Maybe a }  
    deriving (Eq, Ord, Read, Show) 
```

我們接受一個 ``Maybe a`` 並把他包成 newtype，``Monoid`` 的定義如下：

```haskell
instance Monoid (First a) where  
    mempty = First Nothing  
    First (Just x) `mappend` _ = First (Just x)  
    First Nothing `mappend` x = x  
```

正如我們說過得，``mempty`` 就是包在 ``First`` 中的 ``Nothing``。如果 ``mappend`` 的第一個參數是 ``Just``，我們就直接忽略第二個參數。如果第一個參數是 ``Nothing``，那我們就將第二個參數當作結果。並不管他究竟是 ``Just`` 或是 ``Nothing``：	

```haskell
ghci> getFirst $ First (Just 'a') `mappend` First (Just 'b')  
Just 'a'  
ghci> getFirst $ First Nothing `mappend` First (Just 'b')  
Just 'b'  
ghci> getFirst $ First (Just 'a') `mappend` First Nothing  
Just 'a'  
```

``First`` 在我們有一大串 ``Maybe`` 而且想知道他們之中就竟有沒有 ``Just`` 的時候很有用。可以利用 ``mconcat``：

```haskell
ghci> getFirst . mconcat . map First $ [Nothing, Just 9, Just 10]  
Just 9  
```

如果我們希望定義一個 ``Maybe a`` 的 monoid，讓他當 ``mappend`` 的兩個參數都是 ``Just`` 的時候將第二個參數當作結果。``Data.Monoid`` 中有一個現成的 ``Last a``，他很像是 ``First a``，只差在 ``mappend`` 跟 ``mconcat`` 會保留最後一個非 ``Nothing`` 的值。

```haskell
ghci> getLast . mconcat . map Last $ [Nothing, Just 9, Just 10]  
Just 10  
ghci> getLast $ Last (Just "one") `mappend` Last (Just "two")  
Just "two" 
```


### Using monoids to fold data structures


另一種有趣的 monoid 使用方式就是讓他來幫助我們 fold 一些資料結構。到目前為止我們只有 fold list。但 list 並不是唯一一種可以 fold 的資料結構。我們幾乎可以 fold 任何一種資料結構。像是 tree 也是一種常見的可以 fold 的資料結構。


由於有太多種資料結構可以 fold 了，所以我們定義了 ``Foldable`` 這個 typeclass。就像 ``Functor`` 是定義可以 map over 的結構。``Foldable`` 是定義可以 fold 的結構。在 ``Data.Foldable`` 中有定義了一些有用的函數，但他們名稱跟 ``Prelude`` 中的名稱衝突。所以最好是用 qualified 的方式 import 他們：

```haskell
import qualified Foldable as F      
```

為了少打一些字，我們將他們 import qualified 成 ``F``。所以這個 typeclass 中定義了哪些函數呢？有 ``foldr``，``foldl``，``foldr1`` 跟 ``foldl1``。你會說我們已經知道這些函數了，他們有什麽不一樣的地方嗎？我們來比較一下 ``Foldable`` 中的 ``foldr`` 跟 ``Prelude`` 中的 ``foldr`` 的型別異同：

```haskell
ghci> :t foldr  
foldr :: (a -> b -> b) -> b -> [a] -> b  
ghci> :t F.foldr  
F.foldr :: (F.Foldable t) => (a -> b -> b) -> b -> t a -> b  
```

儘管 ``foldr`` 接受一個 list 並將他 fold 起來，``Data.Foldable`` 中的 ``foldr`` 接受任何可以 fold 的型別。並不只是 list。
而兩個 ``foldr`` 對於 list 的結果是相同的：

```haskell
ghci> foldr (*) 1 [1,2,3]  
6  
ghci> F.foldr (*) 1 [1,2,3]  
6  
```

那有哪些資料結構支援 fold 呢？首先我們有 ``Maybe``：

```haskell
ghci> F.foldl (+) 2 (Just 9)  
11  
ghci> F.foldr (||) False (Just True)  
True 
```


但 fold 一個 ``Maybe`` 並沒什麽新意。畢竟當他是 ``Just`` 的時候表現得像是只有單一元素的 list，而當他是 ``Nothing`` 的時候就像是空的 list 一樣。所以我們來看一些比較複雜的資料結構。


還記得 Making Our Own Types and Typeclass 章節中的樹狀的資料結構嗎？我們是這樣定義的：

```haskell
data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Show, Read, Eq)      
```

我們說一棵樹要不就是一棵空的樹要不然就是一個包含值的節點，並且還指向另外兩棵樹。定義他之後，我們將他定義成 ``Functor`` 的 instance，因此可以 ``fmap`` 他。現在我們要將他定義成 ``Foldable`` 的 instance，這樣我們就可以 fold 他。要定義成 ``Foldable`` 的一種方式就是實作 ``foldr``。但另一種比較簡單的方式就是實作 ``foldMap``，他也屬於 ``Foldable`` typeclass。``foldMap`` 的型別如下：

```haskell
foldMap :: (Monoid m, Foldable t) => (a -> m) -> t a -> m  
```

第一個參數是一個函數，這個函數接受 foldable 資料結構中包含的元素的型別，並回傳一個 monoid。他第二個參數是一個 foldable 的結構，並包含型別 ``a`` 的元素。他將第一個函數來 map over 這個 foldable 的結構，因此得到一個包含 monoid 的 foldable 結構。然後用 ``mappend`` 來簡化這些 monoid，最後得到單一的一個 monoid。這個函數聽起來不太容易理解，但我們下面會看到他其實很容易實作。而且好消息是只要實作了這個函數就可以讓我們的函數成為 ``Foldable``。所以我們只要實作某個型別的 ``foldMap``，我們就可以得到那個型別的 ``foldr`` 跟 ``foldl``。


這就是我們如何定義 ``Tree`` 成為 ``Foldable`` 的：

```haskell
instance F.Foldable Tree where  
    foldMap f Empty = mempty  
    foldMap f (Node x l r) = F.foldMap f l `mappend`  
                                f x           `mappend`  
                                F.foldMap f r  
```


![](accordion.png)

我們是這樣思考的：如果我們寫一個函數，他接受樹中的一個元素並回傳一個 monoid，那我們要怎麼簡化整棵樹到只有單一一個 monoid？當我們在對樹做 ``fmap`` 的時候，我們將那函數套用至節點上，並遞迴地套用至左子樹以及右子樹。這邊我們不只是 map 一個函數而已，我們還要求要把結果用 ``mappend`` 簡化成只有單一一個 monoid 值。首先我們考慮樹為空的情形，一棵沒有值也沒有子樹的情形。由於沒有值我們也沒辦法將他套用上面轉換成 monoid 的函數，所以當樹為空的時候，結果應該要是 ``mempty``。


在非空節點的情形下比較有趣，他包含一個值跟兩棵子樹。在這種情況下，我們遞迴地做 ``foldMap``，用 ``f`` 來套用到左子樹跟右子樹上。要記住我們的 ``foldMap`` 只會得到單一的 monoid 值。我們也會套用 ``f`` 到節點中的值。這樣我們就得到三個 monoid 值，有兩個來自簡化子樹的結果，還有一個是套用 ``f`` 到節點中的值的結果。而我們需要將這三個值整合成單一個值。要達成這個目的我們使用 ``mappend``，而且自然地會想到照左子樹，節點值以及右子樹的順序來簡化。


注意到我們並不一定要提供一個將普通值轉成 monoid 的函數。我們只是把他當作是 ``foldMap`` 的參數，我們要決定的只是如何套用那個函數，來把得到的 monoid 們簡化成單一結果。


現在我們有樹的 ``Foldable`` instance，而 ``foldr`` 跟 ``foldl`` 也有預設的實作了。考慮下面這棵樹：


```haskell
testTree = Node 5  
            (Node 3  
             (Node 1 Empty Empty)  
             (Node 6 Empty Empty)  
            )  
            (Node 9  
             (Node 8 Empty Empty)  
             (Node 10 Empty Empty)  
            )  
```

他的 root 是 ``5``，而他左邊下來分別是 ``3``，再來是 ``1`` 跟 ``6``。而右邊下來是 ``9``，再來是 ``8`` 跟 ``10``。有了 ``Foldable`` 的定義，我們就能像對 list 做 fold 一樣對樹做 fold：

```haskell
ghci> F.foldl (+) 0 testTree  
42  
ghci> F.foldl (*) 1 testTree  
64800  
```

``foldMap`` 不只是定義 ``Foldable`` 新的 instance 有用。他也對簡化我們的結構至單一 monoid 值有用。舉例來說，如果我們想要知道我們的樹中有沒有 ``3``，我們可以這樣做：

```haskell
ghci> getAny $ F.foldMap (\x -> Any $ x == 3) testTree  
True 
```

這邊 ``\x -> Any $ x == 3`` 是一個接受一個數值並回傳一個 monoid 的函數，也就是一個包在 ``Any`` 中的 ``Bool``。``foldMap`` 將這個函數套用至樹的每一個節點，並把結果用 ``mappend`` 簡化成單一 monoid。如果我們這樣做：

```haskell
ghci> getAny $ F.foldMap (\x -> Any $ x > 15) testTree  
False 
```

經過套用 lambda 之後我們所有的節點都會是 ``Any False``。但 ``mappend`` 必須要至少吃到一個 ``True`` 才能讓最後的結果變成 ``True``。這也是為什麽結果會是 ``False``，因為我們樹中所有的值都大於 ``15``。


我們也能將 ``foldMap`` 配合 ``\x -> [x]`` 使用來將我們的樹轉成 list。經過套用那個函數後，所有節點都變成包含單一元素的 list。最後用 ``mappend`` 將這些單一元素的 list 轉成一個裝有全部元素的 list：

```haskell
ghci> F.foldMap (\x -> [x]) testTree  
[1,3,6,5,8,9,10] 
```

這個小技巧並不限於樹而已，他可以被套用在任何 ``Foldable`` 上。
