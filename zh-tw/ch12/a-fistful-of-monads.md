# 來看看幾種 Monad

當我們第一次談到 Functor 的時候，我們了解到他是一個抽象概念，代表是一種可以被 map over 的值。然後我們再將其概念提升到 Applicative Functor，他代表一種帶有 context 的型態，我們可以用函數操作他而且同時還保有他的 context。

在這一章，我們會學到 Monad，基本上他是一種加強版的 Applicative Functor，正如 Applicative Functor 是 Functor 的加強版一樣。

![](smugpig.png)

我們介紹到 Functor 是因為我們觀察到有許多型態都可以被 function 給 map over，了解到這個目的，便抽象化了 ``Functor`` 這個 typeclass 出來。但這讓我們想問：如果給定一個 ``a -> b`` 的函數以及 ``f a`` 的型態，我們要如何將函數 map over 這個型態而得到 ``f b``？我們知道要如何 map over ``Maybe a``，``[a]`` 以及 ``IO a``。我們甚至還知道如何用 ``a -> b`` map over ``r -> a``，並且會得到 ``r -> b``。要回答這個問題，我們只需要看 ``fmap`` 的型態就好了：

```
fmap :: (Functor f) => (a -> b) -> f a -> f b      
```

然後只要針對 ``Functor`` instance 撰寫對應的實作。

之後我們又看到一些可以針對 Functor 改進的地方，例如 ``a -> b`` 也被包在一個 Functor value 裡面呢？像是 ``Just (*3)``，我們要如何 apply ``Just 5`` 給他？如果我們不要 apply ``Just 5`` 而是 ``Nothing`` 呢？甚至給定 ``[(*2),(+4)]``，我們要如何 apply 他們到 ``[1,2,3]`` 呢？對於此，我們抽象出 ``Applicative`` typeclass，這就是我們想要問的問題：

```
(<*>) :: (Applicative f) => f (a -> b) -> f a -> f b     
```

我們也看到我們可以將一個正常的值包在一個資料型態中。例如說我們可以拿一個 ``1`` 然後把他包成 ``Just 1``。或是把他包成 ``[1]``。也可以是一個 I/O action 會產生一個 ``1``。這樣包裝的 function 我們叫他做 ``pure``。

如我們說得，一個 applicative value 可以被看作一個有附加 context 的值。例如說，``'a'`` 只是一個普通的字元，但 ``Just 'a'`` 是一個附加了 context 的字元。他不是 ``Char`` 而是 ``Maybe Char``，這型態告訴我們這個值可能是一個字元，也可能什麼都沒有。

來看看 ``Applicative`` typeclass 怎樣讓我們用普通的 function 操作他們，同時還保有 context：
```
ghci> (*) <$> Just 2 <*> Just 8  
Just 16  
ghci> (++) <$> Just "klingon" <*> Nothing  
Nothing  
ghci> (-) <$> [3,4] <*> [1,2,3]  
[2,1,0,3,2,1]  
```

所以我們可以視他們為 applicative values，``Maybe a`` 代表可能會失敗的 computation，``[a]`` 代表同時有好多結果的 computation (non-deterministic computation)，而 ``IO a`` 代表會有 side-effects 的 computation。

Monad 是一個從 Applicative functors 很自然的一個演進結果。對於他們我們主要考量的點是：如果你有一個具有 context 的值 ``m a``，你能如何把他丟進一個只接受普通值 ``a`` 的函數中，並回傳一個具有 context 的值？也就是說，你如何套用一個型態為 ``a -> m b`` 的函數至 ``m a``？基本上，我們要求的函數是：

```
(>>=) :: (Monad m) => m a -> (a -> m b) -> m b
```

如果我們有一個漂亮的值跟一個函數接受普通的值但回傳漂亮的值，那我們要如何要把漂亮的值丟進函數中？這就是我們使用 Monad 時所要考量的事情。我們不寫成 ``f a`` 而寫成 ``m a`` 是因為 ``m`` 代表的是 ``Monad``，但 monad 不過就是支援 ``>>=`` 操作的 applicative functors。``>>=`` 我們稱呼他為 bind。

當我們有一個普通值 ``a`` 跟一個普通函數 ``a -> b``，要套用函數是一件很簡單的事。但當你在處理具有 context 的值時，就需要多考慮些東西，要如何把漂亮的值餵進函數中，並如何考慮他們的行為，但你將會了解到他們其實不難。

## 動手做做看: Maybe Monad

![](buddha.png)

現在對於什麼是 Monad 已經有了些模糊的概念，
我們來看看要如何讓這概念更具體一些。

不意外地，``Maybe`` 是一個 Monad，
所以讓我們對於他多探討些，看看是否能跟我們所知的 Monad 概念結合起來。

    到這邊要確定你了解什麼是 Applicatives。如果你知道好幾種 ``Applicative`` 的 instance 還有他們代表的意含就更好了，因為 monad 不過就是對 applicative 的概念進行一次升級。


一個 ``Maybe a`` 型態的值代表型態為 ``a`` 的值而且具備一個可能造成錯誤的 context。而 ``Just "dharma"`` 的值代表他不是一個 ``"dharma"`` 的字串就是字串不見時的 ``Nothing``。如果你把字串當作計算的結果，``Nothing`` 就代表計算失敗了。

當我們把 ``Maybe`` 視作 functor，我們其實要的是一個 ``fmap`` 來把一個函數針對其中的元素做套用。他會對 ``Just`` 中的元素進行套用，要不然就是保留 ``Nothing`` 的狀態，其代表裡面根本沒有元素。

```
ghci> fmap (++"!") (Just "wisdom")  
Just "wisdom!"  
ghci> fmap (++"!") Nothing  
Nothing  
```

或者視為一個 applicative functor，他也有類似的作用。只是 applicative 也把函數包了起來。``Maybe`` 作為一個 applicative functor，我們能用 ``<*>`` 來套用一個存在 ``Maybe`` 中的函數至包在另外一個 ``Maybe`` 中的值。他們都必須是包在 ``Just`` 來代表值存在，要不然其實就是 ``Nothing``。當你在想套用函數到值上面的時候，缺少了函數或是值都會造成錯誤，所以這樣做是很合理的。

```
ghci> Just (+3) <*> Just 3  
Just 6  
ghci> Nothing <*> Just "greed"  
Nothing  
ghci> Just ord <*> Nothing  
Nothing  
```

當我們用 applicative 的方式套用函數至 ``Maybe`` 型態的值時，就跟上面描述的差不多。過程中所有值都必須是 ``Just``，要不然結果一定會是 ``Nothing``。

```
ghci> max <$> Just 3 <*> Just 6  
Just 6  
ghci> max <$> Just 3 <*> Nothing  
Nothing  
```

我們來思考一下要怎麼為 ``Maybe`` 實作 ``>>=``。正如我們之前提到的，``>>=`` 接受一個 monadic value，以及一個接受普通值的函數，這函數會回傳一個 monadic value。``>>=`` 會幫我們套用這個函數到這個 monadic value。在函數只接受普通值的情況俠，函數是如何作到這件事的呢？要作到這件事，他必須要考慮到 monadic value 的 context。

在這個案例中，``>>=`` 會接受一個 ``Maybe a`` 以及一個型態為 ``a -> Maybe b`` 的函數。他會套用函數到 ``Maybe a``。要釐清他怎麼作到的，首先我們注意到 ``Maybe`` 的 applicative functor 特性。假設我們有一個函數 ``\x -> Just (x+1)``。他接受一個數字，把他加 ``1`` 後再包回 ``Just``。

```
ghci> (\x -> Just (x+1)) 1  
Just 2  
ghci> (\x -> Just (x+1)) 100  
Just 101 
```

如果我們餵給函數 ``1``，他會計算成 ``Just 2``。如果我們餵給函數 ``100``，那結果便是 ``Just 101``。但假如我們餵一個 ``Maybe`` 的值給函數呢？如果我們把 ``Maybe`` 想成一個 applicative functor，那答案便很清楚。如果我們拿到一個 ``Just``，就把包在 ``Just`` 裡面的值餵給函數。如果我們拿到一個 ``Nothing``，我們就說結果是 ``Nothing``。

我們呼叫 ``applyMaybe`` 而不呼叫 ``>>=``。他接受 ``Maybe a`` 跟一個回傳 ``Maybe b`` 的函數，並套用函數至 ``Maybe a``。

```
applyMaybe :: Maybe a -> (a -> Maybe b) -> Maybe b  
applyMaybe Nothing f  = Nothing  
applyMaybe (Just x) f = f x  
```

我們套用一個 infix 函數，這樣 ``Maybe`` 的值可以寫在左邊且函數是在右邊：

```
ghci> Just 3 `applyMaybe` \x -> Just (x+1)  
Just 4  
ghci> Just "smile" `applyMaybe` \x -> Just (x ++ " :")""  
Just "smile :""  
ghci> Nothing `applyMaybe` \x -> Just (x+1)  
Nothing  
ghci> Nothing `applyMaybe` \x -> Just (x ++ " :")")  
Nothing 
```

在上述的範例中，我們看到在套用 ``applyMaybe`` 的時候，函數是套用在 ``Just`` 裡面的值。當我們試圖套用到 ``Nothing``，那整個結果便是 ``Nothing``。假如函數回傳 ``Nothing`` 呢？

```
ghci> Just 3 `applyMaybe` \x -> if x > 2 then Just x else Nothing  
Just 3  
ghci> Just 1 `applyMaybe` \x -> if x > 2 then Just x else Nothing  
Nothing  
```

這正是我們期待的結果。如果左邊的 monadic value 是 ``Nothing``，那整個結果就是 ``Nothing``。如果右邊的函數是 ``Nothing``，那結果也會是 ``Nothing``。這跟我們之前把 ``Maybe`` 當作 applicative 時，過程中有任何一個 ``Nothing`` 整個結果就會是 ``Nothing`` 一樣。

對於 ``Maybe`` 而言，我們已經找到一個方法處理漂亮值的方式。我們作到這件事的同時，也保留了 ``Maybe`` 代表可能造成錯誤的計算的意義。

你可能會問，這樣的結果有用嗎？由於 applicative functors 讓我們可以拿一個接受普通值的函數，並讓他可以操作具有 context 的值，這樣看起來 applicative functors 好像比 monad 強。但我們會看到 monad 也能作到，因為他只是 applicative functors 的升級版。他們同時也能作到 applicative functors 不能作到的事情。

稍候我們會再繼續探討 ``Maybe``，但我們先來看看 monad 的 type class。


## Monad type class

正如 functors 有 ``Functor`` 這個 type class，而 applicative functors 有一個 ``Applicative`` 這個 type class，monad 也有他自己的 type class：``Monad`` 他看起來像這樣：

```
class Monad m where  
    return :: a -> m a  

    (>>=) :: m a -> (a -> m b) -> m b  

    (>>) :: m a -> m b -> m b  
    x >> y = x >>= \_ -> y  

    fail :: String -> m a  
    fail msg = error msg  
```

![](kid.png)

我們從第一行開始看。他說 ``class Monad m where``。但我們之前不是提到 monad 是 applicative functors 的加強版嗎？不是應該有一個限制說一個型態必須先是一個 applicative functor 才可能是一個 monad 嗎？像是 ``class (Applicative m) = > Monad m where``。他的確應該要有，但當 Haskell 被創造的早期，人們沒有想到 applicative functor 適合被放進語言中，所以最後沒有這個限制。但的確每個 monad 都是 applicative functor，即使 ``Monad`` 並沒有這麼宣告。

在 ``Monad`` typeclass 中定義的第一個函數是 ``return``。他其實等價於 ``pure``，只是名字不同罷了。他的型態是 ``(Monad m) => a -> m a``。他接受一個普通值並把他放進一個最小的 context 中。也就是說他把普通值包進一個 monad 裡面。他跟 ``Applicative`` 裡面 ``pure`` 函數做的事情一樣，所以說其實我們已經認識了 ``return``。我們已經用過 ``return`` 來處理一些 I/O。我們用他來做一些假的 I/O，印出一些值。對於 ``Maybe`` 來說他就是接受一個普通值然後包進 ``Just``。


    提醒一下：``return`` 跟其他語言中的 ``return`` 是完全不一樣的。他並不是結束一個函數的執行，他只不過是把一個普通值包進一個 context 裡面。


![](tur2.png)

接下來定義的函數是 bind: ``>>=``。他就像是函數套用一樣，只差在他不接受普通值，他是接受一個 monadic value（也就是具有 context 的值）並且把他餵給一個接受普通值的函數，並回傳一個 monadic value。


接下來，我們定義了 ``>>``。我們不會介紹他，因為他有一個事先定義好的實作，基本上我們在實作 ``Monad`` typeclass 的時候都不會去理他。

最後一個函數是 ``fail``。我們通常在我們程式中不會具體寫出來。他是被 Haskell 用在處理語法錯誤的情況。我們目前不需要太在意 ``fail``。

我們知道了 ``Monad`` typeclass 長什麼樣子，我們來看一下 ``Maybe`` 的 ``Monad`` instance。

```
instance Monad Maybe where  
    return x = Just x  
    Nothing >>= f = Nothing  
    Just x >>= f  = f x  
    fail _ = Nothing  
```

``return``跟``pure``是等價的。這沒什麼困難的。我們跟我們在定義``Applicative``的時候做一樣的事，只是把他用``Just``包起來。

``>>=``跟我們的``applyMaybe``是一樣的。當我們將``Maybe a``塞給我們的函數，我們保留住context，並且在輸入是``Nothing``的時候回傳``Nothing``。畢竟當沒有值的時候套用我們的函數是沒有意義的。當輸入是``Just``的時候則套用``f``並將他包在``Just``裡面。


我們可以試著感覺一下``Maybe``是怎樣表現成Monad的。

```
ghci> return "WHAT" :: Maybe String  
Just "WHAT"  
ghci> Just 9 >>= \x -> return (x*10)  
Just 90  
ghci> Nothing >>= \x -> return (x*10)  
Nothing 
```

第一行沒什麼了不起，我們已經知道 ``return`` 就是 ``pure`` 而我們又對 ``Maybe`` 操作過 ``pure`` 了。至於下兩行就比較有趣點。

留意我們是如何把 ``Just 9`` 餵給 ``\x -> return (x*10)``。在函數中 ``x`` 綁定到 ``9``。他看起好像我們能不用 pattern matching 的方式就從 ``Maybe`` 中抽取出值。但我們並沒有喪失掉 ``Maybe`` 的 context，當他是 ``Nothing`` 的時候，``>>=`` 的結果也會是 ``Nothing``。


## 走鋼索

![](pierre.png)

我們已經知道要如何把 ``Maybe a`` 餵進 ``a -> Maybe b`` 這樣的函數。我們可以看看我們如何重複使用 ``>>=`` 來處理多個 ``Maybe a`` 的值。

首先來說個小故事。皮爾斯決定要辭掉他的工作改行試著走鋼索。他對走鋼索蠻在行的，不過仍有個小問題。就是鳥會停在他拿的平衡竿上。他們會飛過來停一小會兒，然後再飛走。這樣的情況在兩邊的鳥的數量一樣時並不是個太大的問題。但有時候，所有的鳥都會想要停在同一邊，皮爾斯就失去了平衡，就會讓他從鋼索上掉下去。

我們這邊假設兩邊的鳥差異在三個之內的時候，皮爾斯仍能保持平衡。所以如果是右邊有一隻，左邊有四隻的話，那還撐得住。但如果左邊有五隻，那就會失去平衡。

我們要寫個程式來模擬整個情況。我們想看看皮爾斯究竟在好幾隻鳥來來去去後是否還能撐住。例如說，我們想看看先來了一隻鳥停在左邊，然後來了四隻停在右邊，然後左邊那隻飛走了。之後會是什麼情形。

我們用一對整數來代表我們的平衡竿狀態。頭一個位置代表左邊的鳥的數量，第二個位置代表右邊的鳥的數量。

```
type Birds = Int  
type Pole = (Birds,Birds)  
```

由於我們用整數來代表有多少隻鳥，我們便先來定義 ``Int`` 的同義型態，叫做 ``Birds``。然後我們把 ``(Birds, Birds)`` 定義成 ``Pole``。

接下來，我們定義一個函數他接受一個數字，然後把他放在竿子的左邊，還有另外一個函數放在右邊。

```
landLeft :: Birds -> Pole -> Pole  
landLeft n (left,right) = (left + n,right)  
  
landRight :: Birds -> Pole -> Pole  
landRight n (left,right) = (left,right + n)  
```

我們來試著執行看看：

```
ghci> landLeft 2 (0,0)  
(2,0)  
ghci> landRight 1 (1,2)  
(1,3)  
ghci> landRight (-1) (1,2)  
(1,1)  
```

要模擬鳥飛走的話我們只要給定一個負數就好了。 由於這些操作是接受 ``Pole`` 並回傳 ``Pole``， 所以我們可以把函數串在一起。

```
ghci> landLeft 2 (landRight 1 (landLeft 1 (0,0)))  
(3,1)
```

當我們餵 ``(0,0)`` 給 ``landLeft 1`` 時，我們會得到 ``(1,0)``。接著我們模擬右邊又停了一隻鳥，狀態就變成 ``(1,1)``。最後又有兩隻鳥停在左邊，狀態變成 ``(3,1)``。我們這邊的寫法是先寫函數名稱，然後再套用參數。但如果先寫 pole 再寫函數名稱會比較清楚，所以我們會想定義一個函數

```
x -: f = f x
```

我們能先套用參數然後再寫函數名稱：

```
ghci> 100 -: (*3)  
300  
ghci> True -: not  
False  
ghci> (0,0) -: landLeft 2  
(2,0)  
```

有了這個函數，我們便能寫得比較好讀一些：

```
ghci> (0,0) -: landLeft 1 -: landRight 1 -: landLeft 2  
(3,1)  
```

這個範例跟先前的範例是等價的，只不過好讀許多。很清楚的看出我們是從 ``(0,0)`` 開始，然後停了一隻在左邊，接著右邊又有一隻，最後左邊多了兩隻。

到目前為止沒什麼問題，但如果我們要停 10 隻在左邊呢？

``` 
ghci> landLeft 10 (0,3)  
(10,3)  
```

你說左邊有 10 隻右邊卻只有 3 隻？那不是早就應該掉下去了？這個例子太明顯了，如果換個比較不明顯的例子。

```
ghci> (0,0) -: landLeft 1 -: landRight 4 -: landLeft (-1) -: landRight (-2)  
(0,2)  
```

表面看起來沒什麼問題，但如果你仔細看的話，有一瞬間是右邊有四隻，但左邊沒有鳥。要修正這個錯誤，我們要重新檢視 ``landLeft`` 跟 ``landRight``。我們其實是希望這些函數產生失敗的情況。那就是在維持平衡的時候回傳新的 pole，但失敗的時候告訴我們失敗了。這時候 ``Maybe`` 就剛剛好是我們要的 context 了。我們用 ``Maybe`` 重新寫一次：

```
landLeft :: Birds -> Pole -> Maybe Pole  
landLeft n (left,right)  
    | abs ((left + n) - right) < 4 = Just (left + n, right)  
    | otherwise                    = Nothing  
          
landRight :: Birds -> Pole -> Maybe Pole  
landRight n (left,right)  
    | abs (left - (right + n)) < 4 = Just (left, right + n)  
    | otherwise                    = Nothing  
```

現在這些函數不回傳 ``Pole`` 而回傳 ``Maybe Pole`` 了。他們仍接受鳥的數量跟舊的的 pole，但他們現在會檢查是否有太多鳥會造成皮爾斯失去平衡。我們用 guards 來檢查是否有差異超過三的情況。如果沒有，那就包一個在 ``Just`` 中的新的 pole，如果是，那就回傳 ``Nothing``。

再來執行看看：

```
ghci> landLeft 2 (0,0)  
Just (2,0)  
ghci> landLeft 10 (0,3)  
Nothing  
```

一如預期，當皮爾斯不會掉下去的時候，我們就得到一個包在 ``Just`` 中的新 pole。當太多鳥停在同一邊的時候，我們就會拿到 ``Nothing``。這樣很棒，但我們卻不知道怎麼把東西串在一起了。我們不能做 ``landLeft 1 (landRight 1 (0,0))``，因為當我們對 ``(0,0)`` 使用 ``landRight 1`` 時，我們不是拿到 ``Pole`` 而是拿到 ``Maybe Pole``。``landLeft 1`` 會拿到 ``Pole`` 而不是拿到 ``Maybe Pole``。

我們需要一種方法可以把拿到的 ``Maybe Pole`` 塞到拿 ``Pole`` 的函數中，然後回傳 ``Maybe Pole``。而我們有 ``>>=``，他對 ``Maybe`` 做的事就是我們要的

```
ghci> landRight 1 (0,0) >>= landLeft 2  
Just (2,1)  
```

``landLeft 2`` 的型態是 ``Pole -> Maybe Pole``。我們不能餵給他 ``Maybe Pole`` 的東西。而 ``landRight 1 (0,0)`` 的結果就是 ``Maybe Pole``，所以我們用 ``>>=`` 來接受一個有 context 的值然後拿給 ``landLeft 2``。``>>=`` 的確讓我們把 ``Maybe`` 當作有 context 的值，因為當我們丟 ``Nothing`` 給 ``landLeft 2`` 的時候，結果會是 ``Nothing``。

``` 
ghci> Nothing >>= landLeft 2  
Nothing  
```

這樣我們可以把這些新寫的用 ``>>=`` 串在一起。讓 monadic value 可以餵進只吃普通值的函數。

來看看些例子：

```
ghci> return (0,0) >>= landRight 2 >>= landLeft 2 >>= landRight 2  
Just (2,4)  
```

我們最開始用 ``return`` 回傳一個 pole 並把他包在 ``Just`` 裡面。我們可以像往常套用 ``landRight 2``，不過我們不那麼做，我們改用 ``>>=``。``Just (0,0)`` 被餵到 ``landRight 2``，得到 ``Just (0,2)``。接著被餵到 ``landLeft 2``，得到 ``Just (2,2)``。

還記得我們之前引入失敗情況的例子嗎？

```
ghci> (0,0) -: landLeft 1 -: landRight 4 -: landLeft (-1) -: landRight (-2)  
(0,2)  
```

之前的例子並不會反應失敗的情況。但如果我們用 ``>>=`` 的話就可以得到失敗的結果。

```
ghci> return (0,0) >>= landLeft 1 >>= landRight 4 >>= landLeft (-1) >>= landRight (-2)  
Nothing  
```

![](banana.png)

正如預期的，最後的情形代表了失敗的情況。我們再進一步看看這是怎麼產生的。首先 ``return`` 把 ``(0,0)`` 放到一個最小的 context 中，得到 ``Just (0,0)``。然後是 ``Just (0.0) >>= landLeft 1``。由於 ``Just (0,0)`` 是一個 ``Just`` 的值。``landLeft 1`` 被套用至 ``(0,0)`` 而得到 ``Just (1,0)``。這反應了我們仍保持在平衡的狀態。接著是 ``Just (1,0) >>= landright 4`` 而得到了 ``Just (1,4)``。距離不平衡只有一步之遙了。他又被餵給 ``landLeft (-1)``，這組合成了 ``landLeft (-1) (1,4)``。由於失去了平衡，我們變得到了 ``Nothing``。而我們把 ``Nothing`` 餵給 ``landRight (-2)``，由於他是 ``Nothing``，也就自動得到了 ``Nothing``。

如果只把 ``Maybe`` 當作 applicative 用的話是沒有辦法達到我們要的效果的。你試著做一遍就會卡住。因為 applicative functor 並不允許 applicative value 之間有彈性的互動。他們最多就是讓我們可以用 applicative style 來傳遞參數給函數。applicative operators 能拿到他們的結果並把他用 applicative 的方式餵給另一個函數，並把最終的 applicative 值放在一起。但在每一步之間並沒有太多允許我們作手腳的機會。而我們的範例需要每一步都倚賴前一步的結果。當每一隻鳥降落的時候，我們都會把前一步的結果拿出來看看。好知道結果到底應該成功或失敗。

我們也能寫出一個函數，完全不管現在究竟有幾隻鳥停在竿子上，只是要害皮爾斯滑倒。我們可以稱呼這個函數叫做 ``banana``：

```
banana :: Pole -> Maybe Pole  
banana _ = Nothing  
```

現在我們能把香蕉皮串到我們的過程中。他絕對會讓遇到的人滑倒。他完全不管前面的狀態是什麼都會產生失敗。

```
ghci> return (0,0) >>= landLeft 1 >>= banana >>= landRight 1  
Nothing  
```

``Just (1,0)`` 被餵給 ``banana``，而產生了 ``Nothing``，之後所有的結果便都是 ``Nothing`` 了。

要同樣表示這種忽略前面的結果，只注重眼前的 monadic value 的情況，其實我們可以用 ``>>`` 來表達。

```
(>>) :: (Monad m) => m a -> m b -> m b  
m >> n = m >>= \_ -> n  
```

一般來講，碰到一個完全忽略前面狀態的函數，他就應該只會回傳他想回傳的值而已。但碰到 Monad，他們的 context 還是必須要被考慮到。來看一下 ``>>`` 串接 ``Maybe`` 的情況。

```
ghci> Nothing >> Just 3  
Nothing  
ghci> Just 3 >> Just 4  
Just 4  
ghci> Just 3 >> Nothing  
Nothing  
```

如果你把 ``>>`` 換成 ``>>= \_ ->``，那就很容易看出他的意思。

我們也可以把 ``banana`` 改用 ``>>`` 跟 ``Nothing`` 來表達：

```
ghci> return (0,0) >>= landLeft 1 >> Nothing >>= landRight 1  
Nothing 
```

我們得到了保證的失敗。


我們也可以看看假如我們故意不用把 ``Maybe`` 視為有 context 的值的寫法。他會長得像這樣：

```
routine :: Maybe Pole  
routine = case landLeft 1 (0,0) of  
    Nothing -> Nothing  
    Just pole1 -> case landRight 4 pole1 of   
            Nothing -> Nothing  
            Just pole2 -> case landLeft 2 pole2 of  
                    Nothing -> Nothing  
                    Just pole3 -> landLeft 1 pole3  
```

![](centaur.png)

左邊先停了一隻鳥，然後我們停下來檢查有沒有失敗。當失敗的時候我們回傳 ``Nothing``。當成功的時候，我們在右邊停一隻鳥，然後再重複前面做的事情。把這些瑣事轉換成 ``>>=`` 證明了 ``Maybe`` Monad 的力量，可以省去我們不少的時間。

注意到 ``Maybe`` 對 ``>>=`` 的實作，他其實就是在做碰到 ``Nothing`` 就會傳 ``Nothing``，碰到正確值就繼續用 ``Just`` 傳遞值。

在這個章節中，我們看過了好幾個函數，也見識了用 ``Maybe`` monad 來表示失敗的 context 的力量。把普通的函數套用換成了 ``>>=``，讓我們可以輕鬆地應付可能會失敗的情況，並幫我們傳遞 context。這邊的 context 就代表失敗的可能性，當我們套用函數到 context 的時候，就代表考慮進了失敗的情況。

## do 表示法

Monad 在 Haskell 中是十分重要的，所以我們還特別為了操作他設置了特別的語法：``do`` 表示法。我們在介紹 I/O 的時候已經用過 ``do`` 來把小的 I/O action 串在一起了。其實 ``do`` 並不只是可以用在 ``IO``，他可以用在任何 monad 上。他的原則是簡單明瞭，把 monadic value 串成一串。我們這邊來細看 ``do`` 是如何使用，以及為什麼我們十分倚賴他。

來看一下熟悉的例子：

```
ghci> Just 3 >>= (\x -> Just (show x ++ "!"))  
Just "3!"  
```

你說這沒什麼了不起，不過就是把 monadic value 餵給一個函數罷了。其中 ``x`` 就指定成 ``3``。也從 monadic value 變成了普通值。那如果我們要在 lambda 中使用 ``>>=`` 呢？

```
ghci> Just 3 >>= (\x -> Just "!" >>= (\y -> Just (show x ++ y)))  
Just "3!"  
```

我們嵌一個 ``>>=`` 在另外一個 ``>>=`` 中。在外層的 lambda，我們把 ``Just "!"`` 餵給 ``\y -> Just (show x ++ y)``。在內層的 lambda，``y`` 被指定成 ``"!"``。``x`` 仍被指定成 ``3``，是因為我們是從外層的 lambda 取值的。這些行為讓我們回想到下列式子：

```
ghci> let x = 3; y = "!" in show x ++ y  
"3!"  
```

差別在於前述的值是 monadic，具有失敗可能性的 context。我們可以把其中任何一步代換成失敗的狀態：

```
ghci> Nothing >>= (\x -> Just "!" >>= (\y -> Just (show x ++ y)))  
Nothing  
ghci> Just 3 >>= (\x -> Nothing >>= (\y -> Just (show x ++ y)))  
Nothing  
ghci> Just 3 >>= (\x -> Just "!" >>= (\y -> Nothing))  
Nothing  
```

第一行中，把 ``Nothing`` 餵給一個函數，很自然地會回傳 ``Nothing``。第二行裡，我們把 ``Just 3`` 餵給一個函數，所以 ``x`` 就成了 ``3``。但我們把 ``Nothing`` 餵給內層的 lambda 所有的結果就成了 ``Nothing``，這也進一步使得外層的 lambda 成了 ``Nothing``。這就好比我們在 ``let`` expression 中來把值指定給變數一般。只差在我們這邊的值是 monadic value。


要再說得更清楚點，我們來把 script 改寫成每行都處理一個 ``Maybe``：

```
foo :: Maybe String  
foo = Just 3   >>= (\x -> 
      Just "!" >>= (\y -> 
      Just (show x ++ y)))  
```

為了擺脫這些煩人的 lambda，Haskell 允許我們使用 ``do`` 表示法。他讓我們可以把先前的程式寫成這樣：

```
foo :: Maybe String  
foo = do  
    x <- Just 3  
    y <- Just "!"  
    Just (show x ++ y)  
```

![](owld.png)

這看起來好像讓我們不用在每一步都去檢查 ``Maybe`` 的值究竟是 ``Just`` 或 ``Nothing``。這蠻方便的，如果在任何一個步驟我們取出了 ``Nothing``。那整個 ``do`` 的結果就會是 ``Nothing``。我們把整個責任都交給 ``>>=``，他會幫我們處理所有 context 的問題。這邊的 ``do`` 表示法不過是另外一種語法的形式來串連所有的 monadic value 罷了。


在 ``do`` expression 中，每一行都是一個 monadic value。要檢查處理的結果的話，就要使用 ``<-``。如果我們拿到一個 ``Maybe String``，並用 ``<-`` 來綁定給一個變數，那個變數就會是一個 ``String``，就像是使用 ``>>=`` 來將 monadic value 帶給 lambda 一樣。至於 ``do`` expression 中的最後一個值，好比說 ``Just (show x ++ y)``，就不能用 ``<-`` 來綁定結果，因為那樣的寫法當轉換成 ``>>=`` 的結果時並不合理。他必須要是所有 monadic value 黏起來後的總結果，要考慮到前面所有可能失敗的情形。


舉例來說，來看看下面這行：

```
ghci> Just 9 >>= (\x -> Just (x > 8))  
Just True  
```

由於 ``>>=`` 左邊的參數是一個 ``Just`` 型態的值，當 lambda 被套用至 ``9`` 就會得到 ``Just True``。如果我們重寫整個式子，改用 ``do`` 表示法：我們會得到：

```
marySue :: Maybe Bool  
marySue = do   
    x <- Just 9  
    Just (x > 8)  
```

如果我們比較這兩種寫法，就很容易看出為什麼整個 monadic value 的結果會是在 ``do`` 表示法中最後一個 monadic value 的值。他串連了全面所有的結果。


我們走鋼索的模擬程式也可以改用 ``do`` 表示法重寫。``landLeft`` 跟 ``landRight`` 接受一個鳥的數字跟一個竿子來產生一個包在 ``Just`` 中新的竿子。而在失敗的情況會產生 ``Nothing``。我們使用 ``>>=`` 來串連所有的步驟，每一步都倚賴前一步的結果，而且都帶有可能失敗的 context。這邊有一個範例，先是有兩隻鳥停在左邊，接著有兩隻鳥停在右邊，然後是一隻鳥停在左邊：

```
routine :: Maybe Pole  
routine = do  
    start <- return (0,0)  
    first <- landLeft 2 start  
    second <- landRight 2 first  
    landLeft 1 second  
```

我們來看看成功的結果：

```
ghci> routine  
Just (3,2) 
```

當我們要把這些 routine 用具體寫出的 ``>>=``，我們會這樣寫：``return (0,0) >>= landLeft 2``，而有了 ``do`` 表示法，每一行都必須是一個 monadic value。所以我們清楚地把前一個 ``Pole`` 傳給 ``landLeft`` 跟 ``landRight``。如果我們檢視我們綁定 ``Maybe`` 的變數，``start`` 就是 ``(0,0)``，而 ``first`` 就會是 ``(2,0)``。

由於 ``do`` 表示法是一行一行寫，他們會看起來很像是命令式的寫法。但實際上他們只是代表序列而已，每一步的值都倚賴前一步的結果，並帶著他們的 context 繼續下去。

我們再重新來看看如果我們沒有善用 ``Maybe`` 的 monad 性質的程式：

```
routine :: Maybe Pole  
    routine =   
        case Just (0,0) of   
            Nothing -> Nothing  
            Just start -> case landLeft 2 start of  
                Nothing -> Nothing  
                Just first -> case landRight 2 first of  
                    Nothing -> Nothing  
                    Just second -> landLeft 1 second  
```

在成功的情形下，``Just (0,0)`` 變成了 ``start``，
而 ``landLeft 2 start`` 的結果成了 ``first``。

如果我們想在 ``do`` 表示法裡面對皮爾斯丟出香蕉皮，我們可以這樣做：

```
routine :: Maybe Pole  
routine = do  
    start <- return (0,0)  
    first <- landLeft 2 start  
    Nothing  
    second <- landRight 2 first  
    landLeft 1 second  
```

當我們在 ``do`` 表示法寫了一行運算，但沒有用到 ``<-`` 來綁定值的話，其實實際上就是用了 ``>>``，他會忽略掉計算的結果。我們只是要讓他們有序，而不是要他們的結果，而且他比寫成 ``_ <- Nothing`` 要來得漂亮的多。

你會問究竟我們何時要使用 ``do`` 表示法或是 ``>>=``，這完全取決於你的習慣。在這個例子由於有每一步都倚賴於前一步結果的特性，所以我們使用 ``>>=``。如果用 ``do`` 表示法，我們就必須清楚寫出鳥究竟是停在哪根竿子上，但其實每一次都是前一次的結果。不過他還是讓我們了解到怎麼使用 ``do``。

在 ``do`` 表示法中，我們其實可以用模式匹配來綁定 monadic value，就好像我們在 ``let`` 表達式，跟函數參數中使用模式匹配一樣。這邊來看一個在 ``do`` 表示法中使用模式匹配的範例： 

```
justH :: Maybe Char  
justH = do  
    (x:xs) <- Just "hello"  
    return x 
```

我們用模式匹配來取得 ``"hello"`` 的第一個字元，然後回傳結果。所以 ``justH`` 計算會得到 ``Just 'h'``。

如果模式匹配失敗怎麼辦？當定義一個函數的時候，一個模式不匹配就會跳到下一個模式。如果所有都不匹配，那就會造成錯誤，整個程式就當掉。另一方面，如果在 ``let`` 中進行模式匹配失敗會直接造成錯誤。畢竟在 ``let`` 表達式的情況下並沒有失敗就跳下一個的設計。至於在 ``do`` 表示法中模式匹配失敗的話，那就會呼叫 ``fail`` 函數。他定義在 ``Monad`` 的 type class 定義豬。他允許在現在的 monad context 底下，失敗只會造成失敗而不會讓整個程式當掉。他預設的實作如下：

```
fail :: (Monad m) => String -> m a  
fail msg = error msg  
```

可見預設的實作的確是讓程式掛掉，但在某些考慮到失敗的可能性的 Monad（像是 ``Maybe``）常常會有他們自己的實作。對於 ``Maybe``，他的實作像是這樣：

```
fail _ = Nothing
```

他忽略錯誤訊息，並直接回傳 ``Nothing``。所以當在 ``do`` 表示法中的 ``Maybe`` 模式匹配失敗的時候，整個結果就會是 ``Nothing``。這種方式比起讓程式掛掉要好多了。這邊來看一下 ``Maybe`` 模式匹配失敗的範例：

```
wopwop :: Maybe Char  
wopwop = do  
    (x:xs) <- Just ""  
    return x  
```

模式匹配的失敗，所以那一行的效果相當於一個 ``Nothing``。我們來看看執行結果：

```
ghci> wopwop  
Nothing  
```

這樣模式匹配的失敗只會限制在我們 monad 的 context 中，而不是整個程式的失敗。這種處理方式要好多了。


## List Monad

![](deadcat.png)

我們已經了解了 ``Maybe`` 可以被看作具有失敗可能性 context 的值，也見識到如何用 ``>>=`` 來把這些具有失敗考量的值傳給函數。在這一個章節中，我們要看一下如何利用 list 的 monadic 的性質來寫 non-deterministic 的程式。

我們已經討論過在把 list 當作 applicatives 的時候他們具有 non-deterministic 的性質。像 ``5`` 這樣一個值是 deterministic 的。他只有一種結果，而且我們清楚的知道他是什麼結果。另一方面，像 ``[3,8,9]`` 這樣的值包含好幾種結果，所以我們能把他看作是同時具有好幾種結果的值。把 list 當作 applicative functors 展示了這種特性：

```
ghci> (*) <$> [1,2,3] <*> [10,100,1000]  
[10,100,1000,20,200,2000,30,300,3000]  
```

將左邊 list 中的元素乘上右邊 list 中的元素這樣所有的組合全都被放進結果的 list 中。當處理 non-determinism 的時候，這代表我們有好幾種選擇可以選，我們也會每種選擇都試試看，因此最終的結果也會是一個 non-deterministic 的值。只是包含更多不同可能罷了。

non-determinism 這樣的 context 可以被漂亮地用 monad 來考慮。所以我們這就來看看 list 的 ``Monad`` instance 的定義：

```
instance Monad [] where  
    return x = [x]  
    xs >>= f = concat (map f xs)  
    fail _ = []  
```


``return`` 跟 ``pure`` 是做同樣的事，所以我們應該算已經理解了 ``return`` 的部份。他接受一個值，並把他放進一個最小的一個 context 中。換種說法，就是他做了一個只包含一個元素的 list。這樣對於我們想要操作普通值的時候很有用，可以直接把他包起來變成 non-deterministic value。

要理解 ``>>=`` 在 list monad 的情形下是怎麼運作的，讓我們先來回歸基本。``>>=`` 基本上就是接受一個有 context 的值，把他餵進一個只接受普通值的函數，並回傳一個具有 context 的值。如果操作的函數只會回傳普通值而不是具有 context 的值，那 ``>>=`` 在操作一次後就會失效，因為 context 不見了。讓我們來試著把一個 non-deterministic value 塞到一個函數中：

```
ghci> [3,4,5] >>= \x -> [x,-x]  
[3,-3,4,-4,5,-5]  
```

當我們對 ``Maybe`` 使用 ``>>=``，是有考慮到可能失敗的 context。在這邊 ``>>=`` 則是有考慮到 non-determinism。``[3,4,5]`` 是一個 non-deterministic value，我們把他餵給一個回傳 non-deterministic value 的函數。那結果也會是 non-deterministic。而且他包含了所有從 ``[3,4,5]`` 取值，套用 ``\x -> [x,-x]`` 後的結果。這個函數他接受一個數值並產生兩個數值，一個原來的數值與取過負號的數值。當我們用 ``>>=`` 來把一個 list 餵給這個函數，所有在 list 中的數值都保留了原有的跟取負號過的版本。``x`` 會針對 list 中的每個元素走過一遍。

要看看結果是如何算出來的，只要看看實作就好了。首先我們從 ``[3,4,5]`` 開始。然後我們用 lambda 映射過所有元素得到：

```
[[3,-3],[4,-4],[5,-5]]      
```

lambda 會掃過每個元素，所以我們有一串包含一堆 list 的 list，最後我們在把這些 list 壓扁，得到一層的 list。這就是我們得到 non-deterministic value 的過程。

non-determinism 也有考慮到失敗的可能性。``[]`` 其實等價於 ``Nothing``，因為他什麼結果也沒有。所以失敗等同於回傳一個空的 list。所有的錯誤訊息都不用。讓我們來看看範例：

```
ghci> [] >>= \x -> ["bad","mad","rad"]  
[]  
ghci> [1,2,3] >>= \x -> []  
[] 
```

第一行裡面，一個空的 list 被丟給 lambda。因為 list 沒有任何元素，所以函數收不到任何東西而產生空的 list。這跟把 ``Nothing`` 餵給函數一樣。第二行中，每一個元素都被餵給函數，但所有元素都被丟掉，而只回傳一個空的 list。因為所有的元素都造成了失敗，所以整個結果也代表失敗。

就像 ``Maybe`` 一樣，我們可以用 ``>>=`` 把他們串起來：

```
ghci> [1,2] >>= \n -> ['a','b'] >>= \ch -> return (n,ch)  
[(1,'a'),(1,'b'),(2,'a'),(2,'b')]  
```

![](concatmap.png)

``[1,2]`` 被綁定到 ``n`` 而 ``['a','b']`` 被綁定到 ``ch``。最後我們用 ``return (n,ch)`` 來把他放到一個最小的 context 中。在這個案例中，就是把 ``(n,ch)`` 放到 list 中，這代表最低程度的 non-determinism。整套結構要表達的意思就是對於 ``[1,2]`` 的每個元素，以及 ``['a','b']`` 的每個元素，我們產生一個 tuple，每項分別取自不同的 list。

一般來說，由於 ``return`` 接受一個值並放到最小的 context 中，他不會多做什麼額外的東西僅僅是展示出結果而已。

    當你要處理 non-deterministic value 的時候，你可以把 list 中的每個元素想做計算路線的一個 branch。

這邊把先前的表達式用 ``do`` 重寫：

```
listOfTuples :: [(Int,Char)]  
listOfTuples = do  
    n <- [1,2]  
    ch <- ['a','b']  
    return (n,ch)  
```

這樣寫可以更清楚看到 ``n`` 走過 ``[1,2]`` 中的每一個值，而 ``ch`` 則取過 ``['a','b']`` 中的每個值。正如 ``Maybe`` 一般，我們從 monadic value 中取出普通值然後餵給函數。``>>=`` 會幫我們處理好一切 context 相關的問題，只差在這邊的 context 指的是 non-determinism。

使用 ``do`` 來對 list 操作讓我們回想起之前看過的一些東西。來看看下列的片段：

```
ghci> [ (n,ch) | n <- [1,2], ch <- ['a','b'] ]  
[(1,'a'),(1,'b'),(2,'a'),(2,'b')]  
```

沒錯，就是 list comprehension。在先前的範例中，``n`` 會走過 ``[1,2]`` 的每個元素，而 ``ch`` 會走過 ``['a','b']`` 的每個元素。同時我們又把 ``(n,ch)`` 放進一個 context 中。這跟 list comprehension 的目的一樣，只是我們在 list comprehension 裡面不用在最後寫一個 ``return`` 來得到 ``(n,ch)`` 的結果。

實際上，list comprehension 不過是一個語法糖。不論是 list comprehension 或是用 ``do`` 表示法來表示，他都會轉換成用 ``>>=`` 來做計算。

List comprehension 允許我們 filter 我們的結果。舉例來說，我們可以只要包含 ``7`` 在表示位數裡面的數值。

```
ghci> [ x | x <- [1..50], '7' `elem` show x ]  
[7,17,27,37,47]  
```

我們用 ``show`` 跟 ``x`` 來把數值轉成字串，然後檢查 ``'7'`` 是否包含在字串裡面。要看看 filtering 要如何轉換成用 list monad 來表達，我們可以考慮使用 ``guard`` 函數，還有 ``MonadPlus`` 這個 type class。``MonadPlus`` 這個 type class 是用來針對可以同時表現成 monoid 的 monad。下面是他的定義：

```
class Monad m => MonadPlus m where  
    mzero :: m a  
    mplus :: m a -> m a -> m a  
```

``mzero`` 是其實是 ``Monoid`` 中 ``mempty`` 的同義詞，而 ``mplus`` 則對應到 ``mappend``。因為 list 同時是 monoid 跟 monad，他們可以是 ``MonadPlus`` 的 instance。

```
instance MonadPlus [] where  
    mzero = []  
    mplus = (++) 
```

對於 list 而言，``mzero`` 代表的是不產生任何結果的 non-deterministic value，也就是失敗的結果。而 ``mplus`` 則把兩個 non-deterministic value 結合成一個。``guard`` 這個函數被定義成下列形式：

```
guard :: (MonadPlus m) => Bool -> m ()  
guard True = return ()  
guard False = mzero  
```

這函數接受一個布林值，如果他是 ``True`` 就回傳一個包在預設 context 中的 ``()``。如果他失敗就產生 mzero。

```
ghci> guard (5 > 2) :: Maybe ()  
Just ()  
ghci> guard (1 > 2) :: Maybe ()  
Nothing  
ghci> guard (5 > 2) :: [()]  
[()]  
ghci> guard (1 > 2) :: [()]  
[]  
```

看起來蠻有趣的，但用起來如何呢？我們可以用他來過濾 non-deterministic 的計算。

```
ghci> [1..50] >>= (\x -> guard ('7' `elem` show x) >> return x)  
[7,17,27,37,47]  
```

這邊的結果跟我們之前 list comprehension 的結果一致。究竟 ``guard`` 是如何辦到的？我們先看看 ``guard`` 跟 ``>>`` 是如何互動：

```
ghci> guard (5 > 2) >> return "cool" :: [String]  
["cool"]  
ghci> guard (1 > 2) >> return "cool" :: [String]  
[]  
```

如果 ``guard`` 成功的話，結果就會是一個空的 tuple。接著我們用 ``>>`` 來忽略掉空的 tuple，而呈現不同的結果。另一方面，如果 ``guard`` 失敗的話，後面的 ``return`` 也會失敗。這是因為用 ``>>=`` 把空的 list 餵給函數總是會回傳空的 list。基本上 ``guard`` 的意思就是：如果一個布林值是 ``False`` 那就產生一個失敗狀態，不然的話就回傳一個基本的 ``()``。這樣計算就可以繼續進行。


這邊我們把先前的範例用 ``do`` 改寫：

```
sevensOnly :: [Int]  
sevensOnly = do  
    x <- [1..50]  
    guard ('7' `elem` show x)  
    return x  
```

如果我們不寫最後一行 ``return x``，那整個 list 就會是包含一堆空 tuple 的 list。

把上述範例寫成 list comprehension 的話就會像這樣：

```
ghci> [ x | x <- [1..50], '7' `elem` show x ]  
[7,17,27,37,47]  
```

所以 list comprehension 的 filtering 基本上跟 ``guard`` 是一致的。

### A knight's quest

這邊來看一個可以用 non-determinism 解決的問題。假設你有一個西洋棋盤跟一隻西洋棋中的騎士擺在上面。我們希望知道是否這隻騎士可以在三步之內移到我們想要的位置。我們只要用一對數值來表示騎士在棋盤上的位置。第一個數值代表棋盤的行，而第二個數值代表棋盤的列。

![](chess.png)

我們先幫騎士的位置定義一個 type synonym。

```
type KnightPos = (Int,Int)      
```

假設騎士現在是在 ``(6,2)``。究竟他能不能夠在三步內移動到 ``(6,1)`` 呢？你可能會先考慮究竟哪一步是最佳的一步。但不如全部一起考慮吧！要好好利用所謂的 non-determinism。所以我們不是只選擇一步，而是選擇全部。我們先寫一個函數回傳所有可能的下一步：

```
moveKnight :: KnightPos -> [KnightPos]  
moveKnight (c,r) = do  
    (c',r') <- [(c+2,r-1),(c+2,r+1),(c-2,r-1),(c-2,r+1)  
                ,(c+1,r-2),(c+1,r+2),(c-1,r-2),(c-1,r+2)  
                ]  
    guard (c' `elem` [1..8] && r' `elem` [1..8])
    return (c',r')  
```

騎士有可能水平或垂直移動一步或二步，但問題是他們必須要同時水平跟垂直移動。``(c',r')`` 走過 list 中的每一個元素，而 ``guard`` 會保證產生的結果會停留在棋盤上。如果沒有，那就會產生一個空的 list，表示失敗的結果，``return (c',r')`` 也就不會被執行。


這個函數也可以不用 list monad 來寫，但我們這邊只是寫好玩的。下面是一個用 ``filter`` 實現的版本：

```
moveKnight :: KnightPos -> [KnightPos]  
moveKnight (c,r) = filter onBoard  
    [(c+2,r-1),(c+2,r+1),(c-2,r-1),(c-2,r+1)  
    ,(c+1,r-2),(c+1,r+2),(c-1,r-2),(c-1,r+2)  
    ]  
    where onBoard (c,r) = c `elem` [1..8] && r `elem` [1..8]
```

兩個函數做的都是相同的事，所以選個你喜歡的吧。

```
ghci> moveKnight (6,2)  
[(8,1),(8,3),(4,1),(4,3),(7,4),(5,4)]  
ghci> moveKnight (8,1)  
[(6,2),(7,3)] 
```

我們接受一個位置然後產生所有可能的移動方式。所以我們有一個 non-deterministic 的下一個位置。我們用 ``>>=`` 來餵給 ``moveKnight``。接下來我們就可以寫一個三步內可以達到的所有位置：

```
in3 :: KnightPos -> [KnightPos]  
in3 start = do   
    first <- moveKnight start  
    second <- moveKnight first  
    moveKnight second  
```

如果你傳 ``(6,2)``，得到的 list 會很大，因為會有不同種方式來走到同樣的一個位置。我們也可以不用 ``do`` 來寫：

```
in3 start = return start >>= moveKnight >>= moveKnight >>= moveKnight      
```

第一次 ``>>=`` 給我們移動一步的所有結果，第二次 ``>>=`` 給我們移動兩步的所有結果，第三次則給我們移動三步的所有結果。

用 ``return`` 來把一個值放進預設的 context 然後用 ``>>=`` 餵給一個函數其實跟函數呼叫是同樣的，只是用不同的寫法而已。
接著我們寫一個函數接受兩個位置，然後可以測試是否可以在三步內從一個位置移到另一個位置：

```
canReachIn3 :: KnightPos -> KnightPos -> Bool  
canReachIn3 start end = end `elem` in3 start  
```


我們產生所有三步的可能位置，然後看看其中一個位置是否在裡面。所以我們可以看看是否可以在三步內從 ``(6,2)`` 走到 ``(6,1)``：

```
ghci> (6,2) `canReachIn3` (6,1)  
True 
```

那從 ``(6,2)`` 到 ``(7,3)`` 呢？

```
ghci> (6,2) `canReachIn3` (7,3)  
False  
```

答案是不行。你可以修改函數改成當可以走到的時候，他還會告訴你實際的步驟。之後你也可以改成不只限定成三步，可以任意步。


## Monad laws (單子律)

![](judgedog.png)

正如 applicative functors 以及 functors，Monad 也有一些要遵守的定律。我們定義一個 ``Monad`` 的 instance 並不代表他是一個 monad，只代表他被定義成那個 type class 的 instance。一個型態要是 monad，則必須遵守單子律。這些定律讓我們可以對這個型態的行為做一些合理的假設。

Haskell 允許任何型態是任何 type class 的 instance。但他不會檢查單子律是否有被遵守，所以如果我們要寫一個 ``Monad`` 的 instance，那最好我們確定他有遵守單子律。我們可以不用擔心標準函式庫中的型態是否有遵守單子律。但之後我們定義自己的型態時，我們必須自己檢查是否有遵守單子律。不用擔心，他們不會很複雜。

### Left identity

單子律的第一項說當我們接受一個值，將他用 ``return`` 放進一個預設的 context 並把他用 ``>>=`` 餵進一個函數的結果，應該要跟我們直接做函數呼叫的結果一樣。

  * ``retrun x >>= f`` 應該等於 ``f x``

如果你是把 monadic value 視為把一個值放進最小的 context 中，僅僅是把同樣的值放進結果中的話， 那這個定律應該很直覺。因為把這個值放進 context 中然後丟給函數，應該要跟直接把這個值丟給函數做呼叫應該沒有差別。

對於 ``Maybe`` monad，``return`` 被定義成 ``Just``。``Maybe`` monad 講的是失敗的可能性，如果我們有普通值要把他放進 context 中，那把這個動作當作是計算成功應該是很合理的，畢竟我們都知道那個值是很具體的。這邊有些範例：

```
ghci> return 3 >>= (\x -> Just (x+100000))  
Just 100003  
ghci> (\x -> Just (x+100000)) 3  
Just 100003  
```

對於 list monad 而言，``return`` 是把值放進一個 list 中，變成只有一個元素的 list。``>>=`` 則會走過 list 中的每個元素，並把他們丟給函數做運算，但因為在單一元素的 list 中只有一個值，所以跟直接對那元素做運算是等價的：

```
ghci> return "WoM" >>= (\x -> [x,x,x])  
["WoM","WoM","WoM"]  
ghci> (\x -> [x,x,x]) "WoM"  
["WoM","WoM","WoM"]  
```

至於 ``IO``，我們已經知道 ``return`` 並不會造成副作用，只不過是在結果中呈現原有值。所以這個定律對於 ``IO`` 也是有效的。

### Right identity

單子律的第二個規則是如果我們有一個 monadic value，而且我們把他用 ``>>=`` 餵給 ``return``，那結果就會是原有的 monadic value。

  * ``m >>= return`` 會等於 ``m``

這一個可能不像第一定律那麼明顯，但我們還是來看看為什麼會遵守這條。當我們把一個 monadic value 用 ``>>=`` 餵給函數，那些函數是接受普通值並回傳具有 context 的值。``return`` 也是在他們其中。如果你仔細看他的型態，``return`` 是把一個普通值放進一個最小 context 中。這就表示，對於 ``Maybe`` 他並沒有造成任何失敗的狀態，而對於 list 他也沒有多加 non-determinism。

```
ghci> Just "move on up" >>= (\x -> return x)  
Just "move on up"  
ghci> [1,2,3,4] >>= (\x -> return x)  
[1,2,3,4]  
ghci> putStrLn "Wah!" >>= (\x -> return x)  
Wah!  
```

如果我們仔細檢視 list monad 的範例，會發現 ``>>=`` 的實作是：

```
xs >>= f = concat (map f xs)      
```

所以當我們將 ``[1,2,3,4]`` 丟給 ``return``，第一個 ``return`` 會把 ``[1,2,3,4]`` 映射成 ``[[1],[2],[3],[4]]``，然後再把這些小 list 串接成我們原有的 list。

Left identity 跟 right identity 是描述 ``return`` 的行為。他重要的原因是因為他把普通值轉換成具有 context 的值，如果他出錯的話會很頭大。

### Associativity

單子律最後一條是說當我們用 ``>>=`` 把一串 monadic function 串在一起，他們的先後順序不應該影響結果：

  * ``(m >>= f) >>= g`` 跟 ``m >>= (\x -> f x >>= g)`` 是相等的

究竟這邊說的是什麼呢？我們有一個 monadic value ``m``，以及兩個 monadic function ``f`` 跟 ``g``。當我們寫下 ``(m >>= f) >>= g``，代表的是我們把 ``m`` 餵給 ``f``，他的結果是一個 monadic value。然後我們把這個結果餵給 ``g``。而在 ``m >>= (\x -> f x >>= g)`` 中，我們接受一個 monadic value 然後餵給一個函數，這個函數會把 ``f x`` 的結果丟給 ``g``。我們不太容易直接看出兩者相同，所以先來看個範例比較好理解。

還記得之前皮爾斯的範例嗎？要模擬鳥停在他的平衡竿上，我們把好幾個函數串在一起

```
ghci> return (0,0) >>= landRight 2 >>= landLeft 2 >>= landRight 2  
Just (2,4) 
```

從 ``Just (0,0)`` 出發，然後把值傳給 ``landRight 2``。他的結果又被綁到下一個 monadic function，以此類推。如果我們用括號清楚標出優先順序的話會是這樣：

```
ghci> ((return (0,0) >>= landRight 2) >>= landLeft 2) >>= landRight 2  
Just (2,4)  
```

我們也可以改寫成這樣：

```
return (0,0) >>= (\x -> 
landRight 2 x >>= (\y -> 
landLeft 2 y >>= (\z -> 
landRight 2 z)))     
```

``return (0,0)`` 等價於 ``Just (0,0)``，當我們把他餵給 lambda，裡面的 ``x`` 就等於 ``(0,0)``。``landRight`` 接受一個數值跟 pole，算出來的結果是 ``Just (0,2)`` 然後把他餵給另一個 lambda，裡面的 ``y`` 就變成了 ``(0,2)``。這樣的操作持續下去，直到最後一隻鳥降落，而得到 ``Just (2,4)`` 的結果，這也是整個操作的總結果。

這些 monadic function 的優先順序並不重要，重點是他們的意義。從另一個角度來看這個定律：考慮兩個函數 ``f`` 跟 ``g``，將兩個函數組合起來的定義像是這樣：

```
(.) :: (b -> c) -> (a -> b) -> (a -> c)  
f . g = (\x -> f (g x))  
```

如果 ``g`` 的型態是 ``a -> b`` 且 ``f`` 的型態是 ``b -> c``，我們可以把他們合成一個型態是 ``a -> c`` 的新函數。所以中間的參數都有自動帶過。現在假設這兩個函數是 monadic function，也就是說如果他們的回傳值是 monadic function？如果我們有一個函數他的型態是 ``a -> m b``，我們並不能直接把結果丟給另一個型態為 ``b -> m c`` 的函數，因為後者只接受型態為 ``b`` 的普通值。然而，我們可以用 ``>>=`` 來做到我們想要的事。有了 ``>>=``，我們可以合成兩個 monadic function：

```
(<=<) :: (Monad m) => (b -> m c) -> (a -> m b) -> (a -> m c)  
f <=< g = (\x -> g x >>= f)  
```

所以現在我們可以合成兩個 monadic functions：

```
ghci> let f x = [x,-x]  
ghci> let g x = [x*3,x*2]  
ghci> let h = f <=< g  
ghci> h 3  
[9,-9,6,-6]  
```

至於這跟結合律有什麼關係呢？當我們把這定律看作是合成的定律，他就只是說了 ``f <=< (g <=< h)`` 跟 ``(f <=< g) <=< h`` 應該等價。只是他是針對 monad 而已。

如果我們把頭兩個單子律用 ``<=<`` 改寫，那 left identity 不過就是說對於每個 monadic function ``f``，``f <=< return`` 跟 ``f`` 是等價，而 right identity 說 ``return <=< f`` 跟 ``f`` 是等價。

如果看看普通函數的情形，就會發現很像，``(f . g) . h`` 等價於 ``f . (g . h)``，``f . id`` 跟 ``f`` 等價，且 ``id . f`` 等價於 ``f``。

在這一章中，我們檢視了 monad 的基本性質，而且也了解了 ``Maybe`` monad 跟 list monad 的運作方式。在下一章，我們會看看其他一些有特色的 monad，我們也會學到如何定義自己的 monad。
