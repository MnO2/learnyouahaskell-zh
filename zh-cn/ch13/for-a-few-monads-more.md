# 再来看看更多 Monad

![](clint.png)

我们已经看过 Monad 是如何接受具有 context 的值，并如何用函数操作他们 还有如何用 ``>>=`` 跟 ``do`` 来减轻我们对 context 的关注，集中精神在 value 本身。

我们也看过了 ``Maybe`` 是如何把值加上一个可能会失败的 context。我们学习到 List Monad 是如何加进多重结果的 context。我们也了解 ``IO`` Monad 如何运作，而且我们在知道什么是 Monad 之前就已经知道他了。

在这个章节，我们会介绍一些其他的 Monad。他们可以把值变成 monadic value，因此可以让我们的程序更简洁清晰。多见识几个 Monad 也可以敏锐我们对 Monad 的直觉。

我们即将要介绍的 Monad 都包含在 ``mtl`` 这个套建中。一个 Haskell package 包含了一堆模块。而 ``mtl`` 已经包含在 Haskell Platform 中，所以你可能不用另外安装。要检查你有没有这套件，你可以下 ``ghc-pkg list``。这会列出你已经安装的套件，其中应该包含 ``mtl`` 后面接着对应的版号。


## 你所不知道的 Writer Monad
我们已经看过 ``Maybe``, list 以及 ``IO`` Monad。现在我们要来看看 ``Writer`` Monad。

相对于 ``Maybe`` 是加入可能失败的 context，list 是加入 non-deterministic 的 context，``Writer`` 则是加进一个附加值的 context，好比 log 一般。``Writer`` 可以让我们在计算的同时搜集所有 log 纪录，并汇集成一个 log 并附加在结果上。

例如我们想要附加一个 String 好说明我们的值在干么（有可能是为了除错）。想像有一个函数接受一个代表帮派人数的数字，然后会回传值告诉我们这是否算是一个庞大的帮派：

```haskell
isBigGang :: Int -> Bool  
isBigGang x = x > 9  
```

现在我们希望他不只是回传 ``True`` 或 ``False``，我们还希望他能够多回传一个字串代表 log。这很容易，只要多加一个 ``String`` 在 ``Bool`` 旁边就好了。

```haskell
isBigGang :: Int -> (Bool, String)  
isBigGang x = (x > 9, "Compared gang size to 9.")  
```

我们现在回传了一个 Tuple，第一个元素是原来的布林值，第二个元素是一个 String。现在我们的值有了一个 context。

```haskell
ghci> isBigGang 3  
(False,"Compared gang size to 9.")  
ghci> isBigGang 30  
(True,"Compared gang size to 9.")  
```

![](tuco.png)

到目前为止都还不错，``isBigGang`` 回传一个值跟他的 context。对于正常的数值来说这样的写法都能运作良好。但如果我们想要把一个已经具有 context 的值，像是 ``(3, "Smallish gang.")``，喂给 ``isBigGang`` 呢？我们又面对了同样的问题：如果我们有一个能接受正常数值并回传一个具有 context 值的 function，那我们要如何喂给他一个具有 context 的值？

当我们在研究 ``Maybe`` monad 的时候，我们写了一个 ``applyMaybe``。他接受一个 ``Maybe a`` 值跟一个 ``a -> Maybe b`` 型态的函数，他会把 ``Maybe a`` 喂给这个 function，即便这个 function 其实是接受 ``a`` 而非 ``Maybe a``。``applyMaybe`` 有针对这样的 context 做处理，也就是会留意有可能发生的失败情况。但在 ``a -> Maybe b`` 里面，我们可以只专心处理正常数值即可。因为 ``applyMaybe`` (之后变成了 ``>>=``)会帮我们处理需要检查 ``Nothing`` 或 ``Just`` 的情况。

我们再来写一个接受附加 log 值的函数，也就是 ``(a, String)`` 型态的值跟 ``a -> (b, String)`` 型态的函数。我们称呼这个函数为 ``applyLog``。这个函数有的 context 是附加 log 值，而不是一个可能会失败的 context，因此 ``applyLog`` 会确保原有的 log 被保留，并附上从函数产生出的新的 log。这边我们来看一下实作：

```haskell
applyLog :: (a,String) -> (a -> (b,String)) -> (b,String)  
applyLog (x,log) f = let (y,newLog) = f x in (y,log ++ newLog)  
```

当我们想把一个具有 context 的值喂给一个函数的时候，我们会尝试把值跟他的 context 分开，然后把值喂给函数再重新接回 context。在 ``Maybe`` monad 的情况，我们检查值是否为 ``Just x``，如果是，便将 ``x`` 喂给函数。而在 log 的情况，我们知道 pair 的其中一个 component 是 log 而另一个是值。所以我们先取出值 ``x``，将 ``f`` apply 到 ``x``，便获取 ``(y,newLog)``，其中 ``y`` 是新的值而 ``newLog`` 则是新的 log。但如果我们回传 ``newLog``，旧的 log 便不会包含进去，所以我们要回传的是 ``(y, log ++ newLog)``。我们用 ``++`` 来把新的 log 接到旧的上面。

来看看 ``applyLog`` 运作的情形：

```haskell
ghci> (3, "Smallish gang.") `applyLog` isBigGang  
(False,"Smallish gang.Compared gang size to 9")  
ghci> (30, "A freaking platoon.") `applyLog` isBigGang  
(True,"A freaking platoon.Compared gang size to 9")  
```

跟之前的结果很像，只差在我们多了伴随产生的 log。再来多看几个例子：

```haskell
ghci> ("Tobin","Got outlaw name.") `applyLog` (\x -> (length x, "Applied length."))  
(5,"Got outlaw name.Applied length.")  
ghci> ("Bathcat","Got outlaw name.") `applyLog` (\x -> (length x, "Applied length"))  
(7,"Got outlaw name.Applied length")  
```

可以看到在 lambda 里面 ``x`` 只是个正常的字串而不是 tuple，且 ``applyLog`` 帮我们处理掉附加 log 的动作。

### Monoids 的好处

    请确定你了解什么是 Monoids。

到目前为止 ``applyLog`` 接受 ``(a,String)`` 型态的值，但为什么 log 一定要是 ``String`` 呢？我们使用 ``++`` 来附加新的 log，难道 ``++`` 并不能运作在任何形式的 list，而一定要限制我们在 ``String`` 上呢？我们当然可以摆脱 ``String``，我们可以如下改变他的型态：

```haskell
applyLog :: (a,[c]) -> (a -> (b,[c])) -> (b,[c])      
```

我们用一个 List 来代表 Log。包含在 List 中的元素型态必须跟原有的 List 跟回传的 List 型态相同，否则我们没办法用 ``++`` 来把他们接起来。

这能够运作在 bytestring 上吗？绝对没问题。只是我们现在的型态只对 List 有效。我们必须要另外做一个 bytestring 版本的 ``applyLog``。但我们注意到 List 跟 bytestring 都是 monoids。因此他们都是 ``Monoid`` type class 的 instance，那代表他们都有实作 ``mappend``。对 List 以及 bytestring 而言，``mappend`` 都是拿来串接的。

```haskell
ghci> [1,2,3] `mappend` [4,5,6]  
[1,2,3,4,5,6]  
ghci> B.pack [99,104,105] `mappend` B.pack [104,117,97,104,117,97]  
Chunk "chi" (Chunk "huahua" Empty)  
```

修改后我们的 ``applyLog`` 可以运作在任何 monoid 上。我们必须要修改型态宣告来表示这件事，同时也要在实作中把 ``++`` 改成 ``mappend``：

```haskell
applyLog :: (Monoid m) => (a,m) -> (a -> (b,m)) -> (b,m)  
applyLog (x,log) f = let (y,newLog) = f x in (y,log `mappend` newLog)  
```

由于现在包含的值可以是任何 monoid，我们不再需要把 tuple 想成包含一个值跟对应的 log，我们可以想成他包含一个值跟一个对应的 monoid。举例来说，可以说我们有一个 tuple 包含一个产品名称跟一个符合 monoid 特性的产品价格。我们可以定义一个 ``Sum`` 的 newtype 来保证我们在操作产品的时候也会把价钱跟着加起来。

```haskell
import Data.Monoid  
  
type Food = String  
type Price = Sum Int  

addDrink :: Food -> (Food,Price)  
addDrink "beans" = ("milk", Sum 25)  
addDrink "jerky" = ("whiskey", Sum 99)  
addDrink _ = ("beer", Sum 30)  
```

我们用 string 来代表食物，用 ``newtype`` 重新定义 ``nInt`` 为 ``Sum``，来追踪总共需要花多少钱。可以注意到我们用 ``mappend`` 来操作 ``Sum`` 的时候，价钱会被一起加起来。

```haskell
ghci> Sum 3 `mappend` Sum 9  
Sum {getSum = 12}  
```

``addDrink`` 的实作很简单，如果我们想吃豆子，他会回传 ``"milk"`` 以及伴随的 ``Sum 25``，同样的如果我们要吃 "jerky"，他就会回传 "whiskey"，要吃其他东西的话，就会回传 "beer"。乍看之下这个函数没什么特别，但如果用 ``applyLog`` 的话就会有趣些。

```haskell
ghci> ("beans", Sum 10) `applyLog` addDrink  
("milk",Sum {getSum = 35})  
ghci> ("jerky", Sum 25) `applyLog` addDrink  
("whiskey",Sum {getSum = 124})  
ghci> ("dogmeat", Sum 5) `applyLog` addDrink  
("beer",Sum {getSum = 35})  
```

牛奶价值 ``25`` 美分，但如果我们也吃了价值 ``10`` 美分的豆子的话，总共需要付 ``35`` 美分。这样很清楚地展示了伴随的值不一定需要是 log，他可以是任何 monoid。至于两个值要如何结合，那要看 monoid 中怎么定义。当我们需要的是 log 的时候，他们是串接，但这个 case 里面，数字是被加起来。

由于 ``addDrink`` 回传一个 ``(Food,Price)``，我们可以再把结果重新喂给 ``addDrink``，这可以很容易告诉我们总共喝了多少钱：

```haskell
ghci> ("dogmeat", Sum 5) `applyLog` addDrink `applyLog` addDrink  
("beer",Sum {getSum = 65})  
```

将狗食跟 30 美分的啤酒加在一起会得到 ``("beer", Sum 35)``。如果我们用 ``applyLog`` 将上面的结果再喂给 ``addDrink``，我们会得到 ``("beer", Sum 65)`` 这样的结果。


### The Writer type

我们认识了一个附加 monoid 的值其实表现出来的是一个 monad，我们来再来看看其他类似的 ``Monad`` instance。``Control.Monad.Writer`` 这模块含有 ``Writer w a`` 的一个型态，里面定义了他 ``Monad`` 的 instance，还有一些操作这些值的函数。

首先，我们来看一下型态。要把一个 monoid 附加给一个值，只需要定义一个 tuple 就好了。``Writer w a`` 这型态其实是一个 ``newtype`` wrapper。他的定义很简单：

```haskell
newtype Writer w a = Writer { runWriter :: (a, w) }      
```

他包在一个 ``newtype`` 里面，并且可以是一个 ``Monad`` 的 instance，而且这样定义的好处是可以跟单纯 tuple 的型态区分开来。``a`` 这个型态参数代表是包含的值的型态，而 ``w`` 则是附加的 monoid 的型态。

他 ``Monad`` instance 的定义如下：

```haskell
instance (Monoid w) => Monad (Writer w) where  
    return x = Writer (x, mempty)  
    (Writer (x,v)) >>= f = let (Writer (y, v')) = f x in Writer (y, v `mappend` v')  
```

![](angeleyes.png)

首先，我们来看看 ``>>=``。他的实作基本上就是 ``applyLog``，只是我们的 tuple 现在是包在一个 ``Writer`` 的 ``newtype`` 中，我们可以用 pattern matching 的方式把他给 unwrap。我们将 ``x`` 喂给 ``f``。这会回给我们 ``Writer w a``。接着可以用 ``let`` expression 来做 pattern matching。把结果绑定到 ``y`` 这个名字上，然后用 ``mappend`` 来结合旧的 monoid 值跟新的 monoid 值。最后把结果跟 monoid 值用 ``Writer`` constructor 包起来，形成我们最后的 ``Writer`` value。


那 ``return`` 呢？回想 ``return`` 的作用是接受一个值，并回传一个具有意义的最小 context 来装我们的值。那究竟什么样的 context 可以代表我们的 ``Writer`` 呢？如果我们希望 monoid 值所造成的影响愈小愈好，那 ``mempty`` 是个合理的选择。``mempty`` 是被当作 identity monoid value，像是 ``""`` 或 ``Sum 0``，或是空的 bytestring。当我们对 ``mempty`` 用 ``mappend`` 跟其他 monoid 值结合，结果会是其他的 monoid 值。所以如果我们用 ``return`` 来做一个 ``Writer``，然后用 ``>>=`` 来喂给其他的函数，那函数回传的便是算出来的 monoid。下面我们试着用 ``return`` 搭配不同 context 来回传 ``3``：

```haskell
ghci> runWriter (return 3 :: Writer String Int)  
(3,"")  
ghci> runWriter (return 3 :: Writer (Sum Int) Int)  
(3,Sum {getSum = 0})  
ghci> runWriter (return 3 :: Writer (Product Int) Int)  
(3,Product {getProduct = 1})  
```

因为 ``Writer`` 并没有定义成 ``Show`` 的 instance，我们必须用 ``runWriter`` 来把我们的 ``Writer`` 转成正常的 tuple。对于 ``String``，monoid 的值就是空字串。而对于 ``Sum`` 来说则是 ``0``，因为 ``0`` 加上其他任何值都会是对方。而对 ``Product`` 来说，则是 ``1``。

这里的 ``Writer`` instance 并没有定义 ``fail``，所以如果 pattern matching 失败的话，就会调用 ``error``。


### Using do notation with Writer

既然我们定义了 ``Monad`` 的 instance，我们自然可以用 ``do`` 串接 ``Writer`` 型态的值。这在我们需要对一群 ``Writer`` 型态的值做处理时显得特别方便。就如其他的 monad，我们可以把他们当作具有 context 的值。在现在这个 case 中，所有的 monoid 的值都会用 ``mappend`` 来连接起来并得到最后的结果。这边有一个简单的范例，我们用 ``Writer`` 来相乘两个数。

```haskell
import Control.Monad.Writer  
  
logNumber :: Int -> Writer [String] Int  
logNumber x = Writer (x, ["Got number: " ++ show x])  
  
multWithLog :: Writer [String] Int  
multWithLog = do  
    a <- logNumber 3  
    b <- logNumber 5  
    return (a*b)  
```

``logNumber`` 接受一个数并把这个数做成一个 ``Writer``。我们再用一串 string 来当作我们的 monoid 值，每一个数都跟着一个只有一个元素的 list，说明我们只有一个数。``multWithLog`` 式一个 ``Writer``，他将 ``3`` 跟 ``5`` 相乘并确保相乘的纪录有写进最后的 log 中。我们用 ``return`` 来做成 ``a*b`` 的结果。我们知道 ``return`` 会接受某个值并加上某个最小的 context，我们可以确定他不会多添加额外的 log。如果我们执行程序会得到：

```haskell
ghci> runWriter multWithLog  
(15,["Got number: 3","Got number: 5"])  
```

有时候我们就是想要在某个时间点放进某个 Monoid value。``tell`` 正是我们需要的函数。他实作了 ``MonadWriter`` 这个 type class，而且在当 ``Writer`` 用的时候也能接受一个 monoid value，好比说 ``["This is going on"]``。我们能用他来把我们的 monoid value 接到任何一个 dummy value ``()`` 上来形成一个 Writer。当我们拿到的结果是 ``()`` 的时候，我们不会把他绑定到变量上。来看一个 ``multWithLog`` 的范例：

```haskell
multWithLog :: Writer [String] Int  
multWithLog = do  
    a <- logNumber 3  
    b <- logNumber 5  
    tell ["Gonna multiply these two"]  
    return (a*b)  
```

``return (a*b)`` 是我们的最后一行，还记得在一个 ``do`` 中的最后一行代表整个 ``do`` 的结果。如果我们把 ``tell`` 摆到最后，则 ``do`` 的结果则会是 ``()``。我们会因此丢掉乘法运算的结果。除此之外，log 的结果是不变的。

```haskell
ghci> runWriter multWithLog  
(15,["Got number: 3","Got number: 5","Gonna multiply these two"])  
```


### Adding logging to programs
欧几里得算法是找出两个数的最大公因数。Haskell 已经提供了 ``gcd`` 的函数，但我们来实作一个具有 log 功能的 gcd：

```haskell
gcd' :: Int -> Int -> Int  
gcd' a b   
    | b == 0    = a  
    | otherwise = gcd' b (a `mod` b)  
```

算法的内容很简单。首先他检查第二个数字是否为零。如果是零，那就回传第一个数字。如果不是，那结果就是第二个数字跟将第一个数字除以第二个数字的余数两个数字的最大公因数。举例来说，如果我们想知道 8 跟 3 的最大公因数，首先可以注意到 3 不是 0。所以我们要求的是 3 跟 2 的最大公因数(8 除以 3 余二)。接下去我可以看到 2 不是 0，所以我们要再找 2 跟 1 的最大公因数。同样的，第二个数不是 0，所以我们再找 1 跟 0 的最大公因数。最后第二个数终于是 0 了，所以我们得到最大公因数是 1。

```haskell
ghci> gcd' 8 3  
1  
```

答案真的是这样。接着我们想加进 context，context 会是一个 monoid value 并且像是一个 log 一样。就像之前的范例，我们用一串 string 来当作我们的 monoid。所以 ``gcd'`` 会长成这样：

```haskell
gcd' :: Int -> Int -> Writer [String] Int  
```

而他的代码会像这样：

```haskell
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

这个函数接受两个 ``Int`` 并回传一个 ``Writer [String] Int``，也就是说是一个有 log context 的 ``Int``。当 ``b`` 等于 ``0`` 的时候，我们用一个 ``do`` 来组成一个 ``Writer`` 的值。我们先用 ``tell`` 来写入我们的 log，然后用 ``return`` 来当作 ``do`` 的结果。当然我们也可以这样写：

```haskell
Writer (a, ["Finished with " ++ show a])  
```

但我想 ``do`` 的表达方式是比较容易阅读的。接下来我们看看当 ``b`` 不等于 ``0`` 的时候。我们会把 ``mod`` 的使用情况写进 log。然后在 ``do`` 当中的第二行递归调用 ``gcd'``。``gcd'`` 现在是回传一个 ``Writer`` 的型态，所以 ``gcd' b (a `mod` b)`` 这样的写法是完全没问题的。

尽管去 trace 这个 ``gcd'`` 对于理解十分有帮助，但我想了解整个大概念，把值视为具有 context 是更加有用的。

接着来试试跑我们的 ``gcd'``，他的结果会是 ``Writer [String] Int``，如果我们把他从 ``newtype`` 中取出来，我们会拿到一个 tuple。tuple 的第一个部份就是我们要的结果：

```haskell
ghci> fst $ runWriter (gcd' 8 3)  
1  
```

至于 log 呢，由于 log 是一连串 string，我们就用 ``mapM_ putStrLn`` 来把这些 string 印出来：

```haskell
ghci> mapM_ putStrLn $ snd $ runWriter (gcd' 8 3)  
8 mod 3 = 2  
3 mod 2 = 1  
2 mod 1 = 0  
Finished with 1  
```

把普通的算法转换成具有 log 是很棒的经验，我们不过是把普通的 value 重写成 Monadic value，剩下的就靠 ``>>=`` 跟 ``Writer`` 来帮我们处理一切。用这样的方法我们几乎可以对任何函数加上 logging 的功能。我们只要把普通的值换成 ``Writer``，然后把一般的函数调用换成 ``>>=`` (当然也可以用 ``do``)

### Inefficient list construction
当制作 ``Writer`` Monad 的时候，要特别注意你是使用哪种 monoid。使用 list 的话性能有时候是没办法接受的。因为 list 是使用 ``++`` 来作为 ``mappend`` 的实现。而 ``++`` 在 list 很长的时候是非常慢的。

在之前的 ``gcd'`` 中，log 并不会慢是因为 list append 的动作实际上看起来是这样：

```haskell
a ++ (b ++ (c ++ (d ++ (e ++ f))))  
```

list 是建立的方向是从左到右，当我们先建立左边的部份，而把另一串 list 加到右边的时候性能会不错。但如果我们不小心使用，而让 ``Writer`` monad 实际在操作 list 的时候变成像这样的话。

```haskell
((((a ++ b) ++ c) ++ d) ++ e) ++ f 
```

这会让我们的操作是 left associative，而不是 right associative。这非常没有效率，因为每次都是把右边的部份加到左边的部份，而左边的部份又必须要从头开始建起。

下面这个函数跟 ``gcd'`` 差不多，只是 log 的顺序是相反的。他先纪录剩下的操作，然后纪录现在的步骤。

```haskell
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

他先递归调用，然后把结果绑定到 ``result``。然后把目前的动作写到 log，在递归的结果之后。最后呈现的就是完整的 log。

```haskell
ghci> mapM_ putStrLn $ snd $ runWriter (gcdReverse 8 3)  
Finished with 1  
2 mod 1 = 0  
3 mod 2 = 1  
8 mod 3 = 2  
```

这没效率是因为他让 ``++`` 成为 left associative 而不是 right associative。


### Difference lists

![](cactus.png)

由于 list 在重复 append 的时候显得低效，我们最好能使用一种支持高效 appending 的数据结构。其中一种就是 difference list。difference list 很类似 list，只是他是一个函数。他接受一个 list 并 prepend 另一串 list 到他前面。一个等价于 ``[1,2,3]`` 的 difference list 是这样一个函数 ``\xs -> [1,2,3] ++ xs``。一个等价于 ``[]`` 的 difference list 则是 ``\xs -> [] ++ xs``。

Difference list 最酷的地方在于他支持高效的 appending。当我们用 ``++`` 来实现 appending 的时候，他必须要走到左边的 list 的尾端，然后把右边的 list 一个个从这边接上。那 difference list 是怎么作的呢？appending 两个 difference list 就像这样

```haskell
f `append` g = \xs -> f (g xs)  
```

``f`` 跟 ``g`` 这边是两个函数，他们都接受一个 list 并 prepend 另一串 list。举例来说，如果 ``f`` 代表 ``("dog"++)``（可以写成 ``\xs -> "dog" ++ xs``）而 ``g`` 是 ``("meat"++)``，那 ``f `append` g`` 就会做成一个新的函数，等价于：

```haskell
\xs -> "dog" ++ ("meat" ++ xs)  
```

append 两个 difference list 其实就是用一个函数，这函数先喂一个 list 给第一个 difference list，然后再把结果喂给第二个 difference list。

我们可以用一个 ``newtype`` 来包起来

```haskell
newtype DiffList a = DiffList { getDiffList :: [a] -> [a] }  
```

我们包起来的型态是 ``[a] -> [a]``，因为 difference list 不过就是一个转换一个 list 到另一个 list 的函数。要把普通 list 转换成 difference list 也很容易。

```haskell
toDiffList :: [a] -> DiffList a  
toDiffList xs = DiffList (xs++)  
  
fromDiffList :: DiffList a -> [a]  
fromDiffList (DiffList f) = f []  
```

要把一个普通 list 转成 difference list 不过就是照之前定义的，作一个 prepend 另一个 list 的函数。由于 difference list 只是一个 prepend 另一串 list 的一个函数，假如我们要转回来的话，只要喂给他空的 list 就行了。

这边我们给一个 difference list 的 ``Monoid`` 定义

```haskell
instance Monoid (DiffList a) where  
    mempty = DiffList (\xs -> [] ++ xs)  
    (DiffList f) `mappend` (DiffList g) = DiffList (\xs -> f (g xs))  
```

我们可以看到 ``mempty`` 不过就是 ``id``，而 ``mappend`` 其实是 function composition。

```haskell
ghci> fromDiffList (toDiffList [1,2,3,4] `mappend` toDiffList [1,2,3])  
[1,2,3,4,1,2,3]  
```

现在我们可以用 difference list 来加速我们的 ``gcdReverse``

```haskell
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

我们只要把 monoid 的型态从 ``[String]`` 改成 ``DiffList String``，并在使用 ``tell`` 的时候把普通的 list 用 ``toDiffList`` 转成 difference list 就可以了。

```haskell
ghci> mapM_ putStrLn . fromDiffList . snd . runWriter $ gcdReverse 110 34  
Finished with 2  
8 mod 2 = 0  
34 mod 8 = 2  
110 mod 34 = 8  
```

我们用 ``runWriter`` 来取出 ``gcdReverse 110 34`` 的结果，然后用 ``snd`` 取出 log，并用 ``fromDiffList`` 转回普通的 list 印出来。


### Comparing Performance

要体会 Difference List 能如何增进效率，考虑一个从某数数到零的 case。我们纪录的时候就像 ``gcdReverse`` 一样是反过来记的，所以在 log 中实际上是从零数到某个数。

```haskell
finalCountDown :: Int -> Writer (DiffList String) ()  
finalCountDown 0 = do  
    tell (toDiffList ["0"])  
finalCountDown x = do  
    finalCountDown (x-1)  
    tell (toDiffList [show x])  
```

如果我们喂 ``0``，他就只 log 0。如果喂其他正整数，他会先倒数到 ``0`` 然后 append 那些数到 log 中，所以如果我们调用 ``finalCountDown`` 并喂给他 ``100``，那 log 的最后一笔就会是 ``"100"``。

如果你把这个函数 load 进 GHCi 中并喂给他一个比较大的整数 ``500000``，你会看到他无停滞地从 ``0`` 开始数起：

```haskell
ghci> mapM_ putStrLn . fromDiffList . snd . runWriter $ finalCountDown 500000  
0  
1  
2  
```

但如果我们用普通的 list 而不用 difference list

```haskell
finalCountDown :: Int -> Writer [String] ()  
finalCountDown 0 = do  
    tell ["0"]  
finalCountDown x = do  
    finalCountDown (x-1)  
    tell [show x]  
```

并下同样的指令

```haskell
ghci> mapM_ putStrLn . snd . runWriter $ finalCountDown 500000  
```

我们会看到整个运算卡卡的。

当然这不是一个严谨的测试方法，但足以表显出 difference list 是比较有效率的写法。


## Reader Monad

![](revolver.png)

在讲 Applicative 的章节中，我们说过了 ``(->) r`` 的型态只是 ``Functor`` 的一个 instance。要将一个函数 ``f`` map over 一个函数 ``g``，基本上等价于一个函数，他可以接受原本 ``g`` 接受的参数，先套用 ``g`` 然后再把其结果丢给 ``f``。

```haskell
ghci> let f = (*5)  
ghci> let g = (+3)
ghci> (fmap f g) 8
```

我们已经见识过函数当作 applicative functors 的例子。这样能让我们对函数的结果直接进行操作。

```haskell
ghci> let f = (+) <$> (*2) <*> (+10)
ghci> f 3
19
```

``(+) <$> (*2) <*> (+10)`` 代表一个函数，他接受一个数值，分别把这数值交给 ``(*2)`` 跟 ``(+10)``。然后把结果加起来。例如说，如果我们喂 ``3`` 给这个函数，他会分别对 ``3`` 做 ``(*2)`` 跟 ``(+10)`` 的动作。而得到 ``6`` 跟 ``13``。然后调用 ``(+)``，而得到 ``19``。

其实 ``(->) r`` 不只是一个 functor 跟一个 applicative functor，他也是一个 monad。就如其他 monadic value 一般，一个函数也可以被想做是包含一个 context 的。这个 context 是说我们期待某个值，他还没出现，但我们知道我们会把他当作函数的参数，调用函数来得到结果。

我们已经见识到函数是怎样可以看作 functor 或是 applicative functors 了。再来让我们看看当作 ``Monad`` 的一个 instance 时会是什么样子。你可以在 ``Control.Monad.Instances`` 里面找到，他看起来像这样：

```haskell
instance Monad ((->) r) where  
    return x = \_ -> x  
    h >>= f = \w -> f (h w) w  
```

我们之前已经看过函数的 ``pure`` 实作了，而 ``return`` 差不多就是 ``pure``。他接受一个值并把他放进一个 minimal context 里面。而要让一个函数能够是某个定值的唯一方法就是让他完全忽略他的参数。

而 ``>>=`` 的实作看起来有点难以理解，我们可以仔细来看看。当我们使用 ``>>=`` 的时候，喂进去的是一个 monadic value，处理他的是一个函数，而吐出来的也是一个 monadic value。在这个情况下，当我们将一个函数喂进一个函数，吐出来的也是一个函数。这就是为什么我们在最外层使用了一个 lambda。在我们目前看过的实作中，``>>=`` 几乎都是用 lambda 将内部跟外部隔开来，然后在内部来使用 ``f``。这边也是一样的道理。要从一个函数得到一个结果，我们必须喂给他一些东西，这也是为什么我们先用 ``(h w)`` 取得结果，然后将他丢给 ``f``。而 ``f`` 回传一个 monadic value，在这边这个 monadic value 也就是一个函数。我们再把 ``w`` 喂给他。

如果你还不太懂 ``>>=`` 怎么写出来的，不要担心，因为接下来的范例会让你晓得这真的是一个简单的 Monad。我们造一个 ``do`` expression 来使用这个 Monad。

``` 
import Control.Monad.Instances  
  
addStuff :: Int -> Int  
addStuff = do  
  a <- (*2)  
  b <- (+10)  
  return (a+b)  
```

这跟我们之前写的 applicative expression 差不多，只差在他是运作在 monad 上。一个 ``do`` expression 的结果永远会是一个 monadic vlaue，这个也不例外。而这个 monadic value 其实是一个函数。只是在这边他接受一个数字，然后套用 ``(*2)``，把结果绑定到 ``a`` 上面。而 ``(+10)`` 也同用被套用到同样的参数。结果被绑定到 ``b`` 上。``return`` 就如其他 monad 一样，只是制作一个简单的 monadic value 而不会作多余的事情。这让整个函数的结果是 ``a+b``。如果我们试着跑跑看，会得到之前的结果。

```haskell
ghci> addStuff 3  
19  
```

其中 ``3`` 会被喂给 ``(*2)`` 跟 ``(+10)``。而且他也会被喂给 ``return (a+b)``，只是他会忽略掉 ``3`` 而永远回传 ``a+b`` 正因为如此，function monad 也被称作 reader monad。所有函数都从一个固定的地方读取。要写得更清楚一些，可以把 ``addStuff`` 改写如下：

```haskell
addStuff :: Int -> Int  
addStuff x = let  
    a = (*2) x  
    b = (+10) x  
    in a+b  
```

我们见识了把函数视作具有 context 的值很自然的可以表达成 reader monad。只要我们当作我们知道函数会回传什么值就好。他作的就是把所有的函数都黏在一起做成一个大的函数，然后把这个函数的参数都喂给全部组成的函数，这有点取出他们未来的值的意味。实作做完了然后 ``>>=`` 就会保证一切都能正常运作。

## State Monad

![](texas.png)

Haskell 是一个纯粹的语言，正因为如此，我们的程序是有一堆没办法改变全域状态或变量的函数所组成，他们只会作些处理并回传结果。这样的性质让我们很容易思考我们的程序在干嘛，因为我们不需要担心变量在某一个时间点的值是什么。然而，有一些领域的问题根本上就是依赖于随着时间而改变的状态。虽然我们也可以用 Haskell 写出这样的程序，但有时候写起来蛮痛苦的。这也是为什么 Haskell 要加进 State Monad 这个特性。这让我们在 Haskell 中可以容易地处理状态性的问题，并让其他部份的程序还是保持纯粹性。


当我们处理乱数的时候，我们的函数接受一个 random generator 并回传一个新的乱数跟一个新的 random generator。如果我们需要很多个乱数，我们可以用前一个函数回传的 random generator 继续做下去。当我们要写一个接受 ``StdGen`` 的函数并产生丢三个硬币结果的函数，我们会这样写：

```haskell
threeCoins :: StdGen -> (Bool, Bool, Bool)  
threeCoins gen =   
    let (firstCoin, newGen) = random gen  
        (secondCoin, newGen') = random newGen  
        (thirdCoin, newGen''') = random newGen'  
    in  (firstCoin, secondCoin, thirdCoin)  
```

他接受一个 ``gen`` 然后用 ``random gen`` 产生一个 ``Bool`` 型态的值以及新的 generator。要仿真丢第二个硬币的话，便使用新的 generator。在其他语言中，多半除了乱数之外不需要多回传一个 generator。那是因为我们可以对现有的进行修改。但 Haskell 是纯粹的语言，我们没办法那么做，所以我们必须要接受一个状态，产生结果然后回传一个新的状态，然后用新的状态来继续做下去。

一般来讲你应该不会喜欢这么写，在程序中有赤裸裸的状态，但我们又不想放弃 Haskell 的纯粹性质。这就是 State Monad 的好处了，他可以帮我们处理这些琐碎的事情，又让我们保持 Haskell 的纯粹性。

为了深入理解状态性的计算，我们先来看看应该给他们什么样的型态。我们会说一个状态性的计算是一个函数，他接受一个状态，回传一个值跟一个新的状态。写起来会像这样：

```haskell
s -> (a,s) 
```

``s`` 是状态的型态，而 ``a`` 是计算结果的型态。


    在其他的语言中，赋值大多是被当作会改变状态的操作。举例来说，当我们在命令式语言写 ``x = 5``，这通常代表的是把 ``5`` 指定给 ``x`` 这变量。而且这边 ``5`` 是一个 expression。
    
    如果你用函数语言的角度去思考，你可以把他想做是一个函数，接受一个状态，并回传结果跟新的状态。那新的状态代表所有已指定的值与新加入的变量。

这种改变状态的计算，除了想做是一个接受状态并回传结果跟新状态的函数外，也可以想做是具有 context 的值。
实际的值是结果。然而要得到结果，我们必须要给一个初始的状态，才能得到结果跟最后的状态。


### Stack and Stones

考虑现在我们要对一个堆叠的操作建立模型。你可以把东西推上堆叠顶端，或是把东西从顶端拿下来。如果你要的元素是在堆叠的底层的话，你必须要把他上面的东西都拿下来才能拿到他。

我们用一个 list 来代表我们的堆叠。而我们把 list 的头当作堆叠的顶端。为了正确的建立模型，我们要写两个函数：``pop`` 跟 ``push``。``pop`` 会接受一个堆叠，取下一个元素并回传一个新的堆叠，这个新的堆叠不包含取下的元素。``push`` 会接受一个元素，把他堆到堆叠中，并回传一个新的堆叠，其包含这个新的元素。

```haskell
type Stack = [Int]  
  
pop :: Stack -> (Int,Stack)  
pop (x:xs) = (x,xs)  

push :: Int -> Stack -> ((),Stack)  
push a xs = ((),a:xs)  
```

我们用 ``()`` 来当作 pushing 的结果，毕竟推上堆叠并不需要什么回传值，他的重点是在改变堆叠。注意到 ``push`` 跟 ``pop`` 都是改变状态的计算，可以从他们的型态看出来。

我们来写一段程序来仿真一个堆叠的操作。我们接受一个堆叠，把 ``3`` 推上去，然后取出两个元素。

```haskell
stackManip :: Stack -> (Int, Stack)  
stackManip stack = let  
    ((),newStack1) = push 3 stack  
    (a ,newStack2) = pop newStack1  
    in pop newStack2 
```

我们拿一个 ``stack`` 来作 ``push 3 stack`` 的动作，其结果是一个 tuple。tuple 的第一个部份是 ``()``，而第二个部份是新的堆叠，我们把他命名成 ``newStack1``。然后我们从 ``newStack1`` 上 pop 出一个数字。其结果是我们之前 push 上去的一个数字 ``a``，然后把这个更新的堆叠叫做 ``newStack2``。然后我们从 ``newStack2`` 上再 pop 出一个数字 ``b``，并得到 ``newStack3``。我们回传一个 tuple 跟最终的堆叠。

```haskell
ghci> stackManip [5,8,2,1]  
(5,[8,2,1])  
```

结果就是 ``5`` 跟新的堆叠 ``[8,2,1]``。注意到 ``stackManip`` 是一个会改变状态的操作。我们把一堆会改变状态的操作绑在一起操作，有没有觉得很耳熟的感觉。

``stackManip`` 的程序有点冗长，因为我们要写得太详细，必须把状态给每个操作，然后把新的状态再喂给下一个。如果我们可以不要这样作的话，那程序应该会长得像这样：

```haskell
stackManip = do  
    push 3  
    a <- pop  
    pop  
```


这就是 State Monad 在做的事。有了他，我们便可以免除于要把状态操作写得太明白的窘境。


### The State Monad

``Control.Monad.State`` 这个模块提供了一个 ``newtype`` 包起来的型态。

```haskell
newtype State s a = State { runState :: s -> (a,s) }  
```

一个 ``State s a`` 代表的是一个改变状态的操作，他操纵的状态为型态 ``s``，而产生的结果是 ``a``。

我们已经见识过什么是改变状态的操作，以及他们是可以被看成具有 context 的值。接着来看看他们 ``Monad`` 的 instance：

```haskell
instance Monad (State s) where  
    return x = State $ \s -> (x,s)  
    (State h) >>= f = State $ \s -> let (a, newState) = h s  
                                        (State g) = f a  
                                    in  g newState  
```

我们先来看看 ``return`` 那一行。我们 ``return`` 要作的事是接受一个值，并做出一个改变状态的操作，让他永远回传那个值。所以我们才做了一个 lambda 函数，``\s -> (x,s)``。我们把 ``x`` 当成是结果，并且状态仍然是 ``s``。这就是 ``return`` 要完成的 minimal context。

![](badge.png)

那 ``>>=`` 的实作呢？很明显的把改变状态的操作喂进 ``>>=`` 也必须要丢出另一个改变状态的操作。所以我们用 ``State`` 这个 ``newtype`` wrapper 来把一个 lambda 函数包住。这个 lambda 会是新的一个改变状态的操作。但里面的内容是什么？首先我们应该要从接受的操作取出结果。由于 lambda 是在一个大的操作中，所以我们可以喂给 ``h`` 我们现在的状态，也就是 ``s``。那会产生 ``(a, newState)``。到目前为止每次我们在实作 ``>>=`` 的时候，我们都会先从 monadic value 中取出结果，然后喂给 ``f`` 来得到新的 monadic value。在写 ``Writer`` 的时候，我们除了这样作还要确保 context 是用 ``mappend`` 把旧的 monoid value 跟新的接起来。在这边我们则是用 ``f a`` 得到一个新的操作 ``g``。现在我们有了新的操作跟新的状态（叫做 ``newState``），我们就把 ``newState`` 喂给 ``g``。结果便是一个 tuple，里面包含了最后的结果跟最终的状态。


有了 ``>>=``，我们便可以把两个操作黏在一起，只是第二个被放在一个函数中，专门接受第一个的结果。由于 ``pop`` 跟 ``push`` 已经是改变状态的操作了，我们可以把他们包在 ``State`` 中

```haskell
import Control.Monad.State  
  
pop :: State Stack Int  
pop = State $ \(x:xs) -> (x,xs)  

push :: Int -> State Stack ()  
push a = State $ \xs -> ((),a:xs)  
```

``pop`` 已经满足我们的条件，而 ``push`` 要先接受一个 ``Int`` 才会回传我们要的操作。所以我们可以改写先前的范例如下：

```haskell
import Control.Monad.State  
  
stackManip :: State Stack Int  
stackManip = do  
  push 3  
  a <- pop  
  pop  
```


看到我们是怎么把一个 ``push`` 跟两个 ``pop`` 黏成一个操作吗？当我们将他们从一个 ``newtype`` 取出，其实就是需要一个能喂进初始状态的函数：

```haskell
ghci> runState stackManip [5,8,2,1]  
(5,[8,2,1])  
```

我们不须绑定第二个 ``pop``，因为我们根本不会用到 ``a``，所以可以写成下面的样子：

```haskell
stackManip :: State Stack Int  
stackManip = do  
    push 3  
    pop  
    pop  
```

再来尝试另外一种方式，先从堆叠上取下一个数字，看看他是不是 ``5``，如果是的话就把他放回堆叠上，如果不是的话就堆上 ``3`` 跟 ``8``。

```haskell
stackStuff :: State Stack ()  
stackStuff = do  
    a <- pop  
    if a == 5  
        then push 5  
        else do  
            push 3  
            push 8 
```

很直觉吧！我们来看看初始的堆叠的样子。

```haskell
ghci> runState stackStuff [9,0,2,1,0]  
((),[8,3,0,2,1,0]) 
```

还记得我们说过 ``do`` 的结果会是一个 monadic value，而在 ``State`` monad 的 case，``do`` 也就是一个改变状态的函数。而由于 ``stackManip`` 跟 ``stackStuff`` 都是改变状态的计算，因此我们可以把他们黏在一起：

```haskell
moreStack :: State Stack ()  
moreStack = do  
    a <- stackManip  
    if a == 100  
        then stackStuff  
        else return ()  
```

如果 ``stackManip`` 的结果是 ``100``，我们就会跑 ``stackStuff``，如果不是的话就什么都不做。``return ()`` 不过就是什么是都不做，全部保持原样。

``Contorl.Monad.State`` 提供了一个 ``MonadState`` 的 typeclass，他有两个有用的函数，分别是 ``get`` 跟 ``put``。对于 ``State`` 来说，``get`` 的实作就像这样：

```haskell
get = State $ \s -> (s,s)
```

他只是取出现在的状态除此之外什么也不做。而 ``put`` 函数会接受一个状态并取代掉现有的状态。

```haskell
put newState = State $ \s -> ((),newState)  
```

有了这两个状态，我们便可以看到现在堆叠中有什么，或是把整个堆叠中的元素换掉。

```haskell
stackyStack :: State Stack ()  
stackyStack = do  
    stackNow <- get  
    if stackNow == [1,2,3]  
        then put [8,3,1]  
        else put [9,2,1]  
```

我们可以看看对于 ``State`` 而言，``>>=`` 的型态会是什么：

```haskell
(>>=) :: State s a -> (a -> State s b) -> State s b  
```

我们可以看到状态的型态都是 ``s``，而结果从型态 ``a`` 变成型态 ``b``。这代表我们可以把好几个改变状态的计算黏在一起，这些计算的结果可以都不一样，但状态的型态会是一样的。举例来说，对于 ``Maybe`` 而言，``>>=`` 的型态会是：

```haskell
(>>=) :: Maybe a -> (a -> Maybe b) -> Maybe b  
```

``Maybe`` 不变是有道理的，但如果用 ``>>=`` 来把两种不同的 monad 接起来是没道理的。但对于 state monad 而言，monad 其实是 ``State s``，所以如果 ``s`` 不一样，我们就要用 ``>>=`` 来把两个 monad 接起来。

### 随机性与 state monad

在章节的一开始，我们知道了在 Haskell 中要产生乱数的不方便。我们要拿一个产生器，并回传一个乱数跟一个新的产生器。接下来我们还一定要用新的产生器不可。但 State Monad 让我们可以方便一些。

``System.Random`` 中的 ``random`` 函数有下列的型态：

```haskell
random :: (RandomGen g, Random a) => g -> (a, g)  
```

代表他接受一个乱数产生器，并产生一个乱数跟一个新的产生器。很明显他是一个会改变状态的计算，所以我们可以用 ``newtype`` 把他包在一个 ``State`` 中，然后把他当作 monadic value 来操作。

```haskell
import System.Random  
import Control.Monad.State  
  
randomSt :: (RandomGen g, Random a) => State g a  
randomSt = State random  
```

这样我们要丢三个硬币的结果可以改写成这样：

```haskell
import System.Random  
import Control.Monad.State  
  
threeCoins :: State StdGen (Bool,Bool,Bool)  
threeCoins = do  
  a <- randomSt  
  b <- randomSt  
  c <- randomSt  
  return (a,b,c)  
```

``threeCoins`` 是一个改变状态的计算，他接受一个初始的乱数产生器，他会把他喂给 ``randomSt``，他会产生一个数字跟一个新的产生器，然后会一直传递下去。我们用 ``return (a,b,c)`` 来呈现 ``(a,b,c)``，这样并不会改变最近一个产生器的状态。

```haskell
ghci> runState threeCoins (mkStdGen 33)  
((True,False,True),680029187 2103410263)
```

要完成像这样要改变状态的任务便因此变得轻松了很多。

## Error Monad

我们知道 ``Maybe`` 是拿来赋予一个值具有可能失败的 context。一个值可能会是 ``Just something`` 或是一个 ``Nothing``。尽管这很有用，但当我们拿到了一个 ``Nothing``，我们只知道他失败了，但我们没办法塞进一些有用的信息，告诉我们究竟是在什么样的情况下失败了。

而 ``Either e a`` 则能让我们可以加入一个可能会发生错误的 context，还可以增加些有用的消息，这样能让我们知道究竟是什么东西出错了。一个 ``Either e a`` 的值可以是代表正确的 ``Right``，或是代表错误的 ``Left``，例如说：

```haskell
ghci> :t Right 4  
Right 4 :: (Num t) => Either a t  
ghci> :t Left "out of cheese error"  
Left "out of cheese error" :: Either [Char] b  
```

这就像是加强版的 ``Maybe``，他看起来实在很像一个 monad，毕竟他也可以当作是一个可能会发生错误的 context，只是多了些消息罢了。

在 ``Control.Monad.Error`` 里面有他的 ``Monad`` instance。

```haskell
instance (Error e) => Monad (Either e) where  
    return x = Right x   
    Right x >>= f = f x  
    Left err >>= f = Left err  
    fail msg = Left (strMsg msg)  
```

``return`` 就是建立起一个最小的 context，由于我们用 ``Right`` 代表正确的结果，所以他把值包在一个 ``Right`` constructor 里面。就像实作 ``Maybe`` 时的 ``return`` 一样。

``>>=`` 会检查两种可能的情况：也就是 ``Left`` 跟 ``Right``。如果进来的是 ``Right``，那我们就调用 ``f``，就像我们在写 ``Just`` 的时候一样，只是调用对应的函数。而在错误的情况下，``Left`` 会被传出来，而且里面保有描述失败的值。


``Either e`` 的 ``Monad`` instance 有一项额外的要求，就是包在 ``Left`` 中的型态，也就是 ``e``，必须是 ``Error`` typeclass 的 instance。``Error`` 这个 typeclass 描述一个可以被当作错误消息的型态。他定义了 ``strMsg`` 这个函数，他接受一个用字串表达的错误。一个明显的范例就是 ``String`` 型态，当他是 ``String`` 的时候，``strMsg`` 只不过回传他接受到的字串。

```haskell
ghci> :t strMsg  
strMsg :: (Error a) => String -> a  
ghci> strMsg "boom!" :: String  
"boom!"  
```

但因为我们通常在用 ``Either`` 来描述错误的时候，是用 ``String`` 来装错误消息，所以我们也不用担心这一点。当在 ``do`` 里面做 pattern match 失败的时候，``Left`` 的值会拿来代表失败。

总之来看看一个范例吧：

```haskell
ghci> Left "boom" >>= \x -> return (x+1)  
Left "boom"  
ghci> Right 100 >>= \x -> Left "no way!"  
Left "no way!" 
```


当我们用 ``>>=`` 来把一个 ``Left`` 喂进一个函数，函数的运算会被忽略而直接回传丢进去的 ``Left`` 值。当我们喂 ``Right`` 值给函数，函数就会被计算而得到结果，但函数还是产生了一个 ``Left`` 值。

当我们试着喂一个 ``Right`` 值给函数，而且函数也成功地计算，我们却碰到了一个奇怪的 type error。

```haskell
ghci> Right 3 >>= \x -> return (x + 100)  
  
<interactive>:1:0:  
  Ambiguous type variable `a' in the constraints:  
    `Error a' arising from a use of `it' at <interactive>:1:0-33  
    `Show a' arising from a use of `print' at <interactive>:1:0-33  
  Probable fix: add a type signature that fixes these type variable(s)  
```

Haskell 警告说他不知道要为 ``e`` 选择什么样的型态，尽管我们是要印出 ``Right`` 的值。这是因为 ``Error e`` 被限制成 ``Monad``。把 ``Either`` 当作 Monad 使用就会碰到这样的错误，你只要明确写出 type signature 就行了：

```haskell
ghci> Right 3 >>= \x -> return (x + 100) :: Either String Int  
Right 103  
```

这样就没问题了。


撇除这个小毛病，把 ``Either`` 当 Monad 使用就像使用 ``Maybe`` 一样。在前一章中，我们展示了 ``Maybe`` 的使用方式。你可以把前一章的范例用 ``Either`` 重写当作练习。

## 一些实用的 Moandic functions

在这个章节，我们会看看一些操作 monadic value 的函数。这样的函数通常我们称呼他们为 monadic function。其中有些你是第一次见到，但有些不过是 ``filter`` 或 ``foldl`` 的变形。让我们来看看吧！

### liftM 

![](wolf.png)

当我们开始学习 Monad 的时候，我们是先学习 functors，他代表可以被 map over 的事物。接着我们学了 functors 的加强版，也就是 applicative functors，他可以对 applicative values 做函数的套用，也可以把一个一般值放到一个缺省的 context 中。最后，我们介绍在 applicative functors 上更进一步的 monad，他让这些具有 context 的值可以被喂进一般函数中。 

也就是说每一个 monad 都是个 applicative functor，而每一个 applicative functor 也都是一个 functor。``Applicative`` typeclass 中有加入限制，让每一个 ``Applicative`` 都是 ``Functor``。但 ``Monad`` 却没有这样的限制，让每个 ``Monad`` 都是 ``Applicative``。这是因为 ``Monad`` 这个 typeclass 是在 ``Applicative`` 引入前就存在的缘故。

但即使每个 monad 都是一个 functor，但我们不需要依赖 ``Functor`` 的定义。那是因为我们有 ``liftM`` 这个函数。他会接受一个函数跟一个 monadic value，然后把函数 map over 那些 monadic value。所以他其实就是 ``fmap``，以下是他的型态：

```haskell
liftM :: (Monad m) => (a -> b) -> m a -> m b  
```

而这是 ``fmap`` 的型态：

```haskell
fmap :: (Functor f) => (a -> b) -> f a -> f b
```

如果 ``Functor`` 跟 ``Monad`` 的 instance 遵守 functor 跟 monad 的法则（到目前为止我们看过的 monad 都遵守），那这两个函数其实是等价的。这就像 ``pure`` 跟 ``return`` 其实是同一件事，只是一个在 ``Applicative`` 中，而另外一个在 ``Monad`` 里面，我们来试试看 ``liftM`` 吧：

```haskell
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

我们已经知道 ``fmap`` 是如何运作在 ``Maybe`` 上。而 ``liftM`` 又跟 ``fmap`` 等价。对于 ``Writer`` 型态的值而言，函数只有对他的第一个 component 做处理。而对于改变状态的计算，``fmap`` 跟 ``liftM`` 也都是产生另一个改变状态的计算。我们也看过了 ``(+100)`` 当作用在 ``pop`` 上会产生 ``(1, [2,3,4])``。

来看看 ``liftM`` 是如何被实作的：
```haskell
liftM :: (Monad m) => (a -> b) -> m a -> m b  
liftM f m = m >>= (\x -> return (f x)) 
```

或者用 ``do`` 来表示得清楚些

```haskell
liftM :: (Monad m) => (a -> b) -> m a -> m b  
liftM f m = do  
    x <- m  
    return (f x)  
```

我们喂一个 monadic value ``m`` 给函数，我们套用那个函数然后把结果放进一个缺省的 context。由于遵守 monad laws，这保证这操作不会改变 context，只会呈现最后的结果。我们可以看到实作中 ``liftM`` 也没有用到 ``Functor`` 的性质。这代表我们能只用 monad 提供给我们的就实作完 ``fmap``。这特性让我们可以得到 monad 比 functor 性质要强的结论。

``Applicative`` 让我们可以操作具有 context 的值就像操作一般的值一样。
就像这样：

```haskell
ghci> (+) <$> Just 3 <*> Just 5  
Just 8  
ghci> (+) <$> Just 3 <*> Nothing  
Nothing  
```

使用 applicative 的特性让事情变得很精简。
``<$>`` 不过就是 ``fmap``，而 ``<*>`` 只是一个具有下列型态的函数：

```haskell
(<*>) :: (Applicative f) => f (a -> b) -> f a -> f b  
```

他有点像 ``fmap``，只是函数本身有一个 context。我们必须把他从 context 中抽出，对 ``f a`` 做 map over 的东做，然后再放回 context 中。由于在 Haskel 中函数缺省都是 curried，我们便能用 ``<$>`` 以及 ``<*>`` 来让接受多个参数的函数也能接受 applicative 种类的值。

总之 ``<*>`` 跟 ``fmap`` 很类似，他也能只用 ``Monad`` 保证的性质实作出来。``ap`` 这个函数基本上就是 ``<*>``，只是他是限制在 ``Monad`` 上而不是 ``Applicative`` 上。这边是他的定义：

```haskell
ap :: (Monad m) => m (a -> b) -> m a -> m b  
ap mf m = do  
    f <- mf  
    x <- m  
    return (f x)  
```


``mf`` 是一个 monadic value，他的结果是一个函数。由于函数跟值都是放在 context 中，假设我们从 context 取出的函数叫 ``f``，从 context 取出的值叫 ``x``，我们把 ``x`` 喂给 ``f`` 然后再把结果放回 context。像这样：

```haskell
ghci> Just (+3) <*> Just 4  
Just 7  
ghci> Just (+3) `ap` Just 4  
Just 7  
ghci> [(+1),(+2),(+3)] <*> [10,11]  
[11,12,12,13,13,14]  
ghci> [(+1),(+2),(+3)] `ap` [10,11]  
[11,12,12,13,13,14]  
```


由于我们能用 ``Monad`` 提供的函数实作出 ``Applicative`` 的函数，因此我们看到 Monad 有比 applicative 强的性质。事实上，当我们知道一个型态是 monad 的时候，大多数会先定义出 ``Monad`` 的 instance，然后才定义 ``Applicative`` 的 instance。而且只要把 ``pure`` 定义成 ``return``，``<*>`` 定义成 ``ap`` 就行了。同样的，如果你已经有了 ``Monad`` 的 instance，你也可以简单的定义出 ``Functor``，只要把 ``fmap`` 定义成 ``liftM`` 就行了。

``liftA2`` 是一个方便的函数，他可以把两个 applicative 的值喂给一个函数。他的定义很简单：

```haskell
liftA2 :: (Applicative f) => (a -> b -> c) -> f a -> f b -> f c  
liftA2 f x y = f <$> x <*> y  
```

``liftM2`` 也是做差不多的事情，只是多了 ``Monad`` 的限制。在函式库中其实也有 ``liftM3``，``liftM4`` 跟 ``liftM5``。

我们看到了 monad 相较于 applicative 跟 functor 有比较强的性质。尽管 moand 有 functor 跟 applicative functor 的性质，但他们不见得有 ``Functor`` 跟 ``Applicative`` 的 instance 定义。所以我们查看了一些在 monad 中定义，且等价于 functor 或 applicative functor 所具有的函数。


### The join function

如果一个 monadic value 的结果是另一个 monadic value，也就是其中一个 monadic value 被包在另一个里面，你能够把他们变成一个普通的 monadic value 吗？就好像把他们打平一样。譬如说，我们有 ``Just (Just 9)``，我们能够把他变成 ``Just 9`` 吗？事实上是可以的，这也是 monad 的一个性质。也就是我要看的 ``join`` 函数，他的型态是这样：

```haskell
join :: (Monad m) => m (m a) -> m a  
```

他接受一个包在另一个 monadic value 中的 monadic value，然后会回给我们一个普通的 monadic value。这边有一些 ``Maybe`` 的范例：

```haskell
ghci> join (Just (Just 9))  
Just 9  
ghci> join (Just Nothing)  
Nothing  
ghci> join Nothing  
Nothing  
```

第一行是一个计算成功的结果包在另一个计算成功的结果，他们应该要能结合成为一个比较大的计算成功的结果。第二行则是一个 ``Nothing`` 包在一个 ``Just`` 中。我们之前在处理 ``Maybe`` 型态的值时，会用 ``<*>`` 或 ``>>=`` 把他们结合起来。输入必须都是 ``Just`` 时结果出来才会是 ``Just``。如果中间有任何的失败，结果就会是一个失败的结果。而第三行就是这样，我们尝试把失败的结果接合起来，结果也会是一个失败。

要 ``join`` 一个 list 也是很简单：

```haskell
ghci> join [[1,2,3],[4,5,6]]  
[1,2,3,4,5,6]  
```

你可以看到，对于 list 而言 ``join`` 不过就是 ``concat``。
而要 ``join`` 一个包在 ``Writer`` 中的 ``Writer``，
我们必须用 ``mappend``：

```haskell
ghci> runWriter $ join (Writer (Writer (1,"aaa"),"bbb"))  
(1,"bbbaaa")  
```

``"bbb"`` 先被加到 monoid 中，接着 ``"aaa"`` 被附加上去。你想要查看 ``Writer`` 中的值的话，必须先把值写进去才行。

要对 ``Either`` 做 ``join`` 跟对 ``Maybe`` 做 ``join`` 是很类似的：

```haskell
ghci> join (Right (Right 9)) :: Either String Int  
Right 9  
ghci> join (Right (Left "error")) :: Either String Int  
Left "error"  
ghci> join (Left "error") :: Either String Int  
Left "error"  
```

如果我们对一个包了另外一个改变状态的计算的进行改变状态的计算，要作 ``join`` 的动作会让外面的先被计算，然后才是计算里面的：

```haskell
ghci> runState (join (State $ \s -> (push 10,1:2:s))) [0,0,0]  
((),[10,1,2,0,0,0])  
```

这边的 lambda 函数接受一个状态，并把 ``2`` 跟 ``1`` 放到堆叠中，并把 ``push 10`` 当作他的结果。当对整个东西做 ``join`` 的时候，他会先把 ``2`` 跟 ``1`` 放到堆叠上，然后进行 ``push 10`` 的计算，因而把 ``10`` 放到堆叠的顶端。

``join`` 的实作像是这样：

```haskell
join :: (Monad m) => m (m a) -> m a  
join mm = do  
    m <- mm  
    m  
```

因为 ``mm`` 的结果会是一个 monadic value，我们单独用 ``m <- mm`` 拿取他的结果。这也可以说明 ``Maybe`` 只有当外层跟内层的值都是 ``Just`` 的时候才会是 ``Just``。如果把 ``mm`` 的值设成 ``Just (Just 8)`` 的话，他看起来会是这样：

```haskell
joinedMaybes :: Maybe Int  
joinedMaybes = do  
    m <- Just (Just 8)  
    m  
```

![](tipi.png)

最有趣的是对于一个 monadic value 而言，用 ``>>=`` 把他喂进一个函数其实等价于对 monad 做 mapping over 的动作，然后用 ``join`` 来把值从 nested 的状态变成扁平的状态。也就是说 ``m >>= f`` 其实就是 ``join (fmap f m)``。如果你仔细想想的话其实很明显。``>>=`` 的使用方式是，把一个 monadic value 喂进一个接受普通值的函数，但他却会回传 monadic value。如果我们 map over 一个 monadic value，我们会做成一个 monadic value 包了另外一个 monadic value。例如说，我们现在手上有 ``Just 9`` 跟 ``\x -> Just (x+1)``。如果我们把这个函数 map over ``Just 9``，我们会得到 ``Just (Just 10)``

事实上 ``m >>= f`` 永远等价于 ``join (fmap f m)`` 这性质非常有用。如果我们要定义自己的 ``Monad`` instance，要知道怎么把 nested monadic value 变成扁平比起要定义 ``>>=`` 是比较容易的一件事。


### filterM

``filter`` 函数是 Haskell 中不可或缺的要素。他接受一个断言(predicate)跟一个 list 来过滤掉断言为否的部份并回传一个新的 list。他的型态是这样：

```haskell
filter :: (a -> Bool) -> [a] -> [a]  
```

predicate 能接 list 中的一个元素并回传一个 ``Bool`` 型态的值。但如果 ``Bool`` 型态其实是一个 monadic value 呢？也就是他有一个 context。例如说除了 ``True`` 跟 ``False`` 之外还伴随一个 monoid，像是 ``["Accepted the number 5"]``，或 ``["3 is too small"]``。照前面所学的听起来是没问题，而且产出的 list 也会跟随 context，在这个例子中就是 log。所以如果 ``Bool`` 会回传伴随 context 的布林值，我们会认为最终的结果也会具有 context。要不然这些 context 都会在处理过程中遗失。

在 ``Control.Monad`` 中的 ``filterM`` 函数正是我们所需要的，他的型态如下：

```haskell
filterM :: (Monad m) => (a -> m Bool) -> [a] -> m [a]  
```

predicate 会回传一个 monadic value，他的结果会是 ``Bool`` 型态，由于他是 monadic value，他的 context 有可能会是任何 context，譬如说可能的失败，non-determinism，甚至其他的 context。一旦我们能保证 context 也会被保存在最后的结果中，结果也就是一个 monadic value。


我们来写一个接受 list 然后过滤掉小于 4 的函数。先尝试使用 ``filter`` 函数：

```haskell
ghci> filter (\x -> x < 4) [9,1,5,2,10,3]  
[1,2,3] 
```

很简单吧。接着我们在做个 predicate，除了表达 ``True`` 或 ``False`` 之外，还提供了一个 log。我们会用 ``Writer`` monad 来表达这件事：

```haskell
keepSmall :: Int -> Writer [String] Bool  
keepSmall x  
    | x < 4 = do  
        tell ["Keeping " ++ show x]  
        return True  
    | otherwise = do  
        tell [show x ++ " is too large, throwing it away"]  
        return False  
```

这个函数会回传 ``Writer [String] Bool`` 而不是一个单纯的 ``Bool``。他是一个 monadic predicate。如果扫到的数字小于 ``4`` 的话，我们就会回报要保存他，而且回传 ``return True``。

接着，我们把他跟一个 list 喂给 ``filterM``。由于 predicate 会回传 ``Writer``，所以结果仍会是一个 ``Writer`` 值。

```haskell
ghci> fst $ runWriter $ filterM keepSmall [9,1,5,2,10,3]  
[1,2,3]  
```

要检查 ``Writer`` 的结果，我们想要印出 log 看看里面有什么东西：

```haskell
ghci> mapM_ putStrLn $ snd $ runWriter $ filterM keepSmall [9,1,5,2,10,3]  
9 is too large, throwing it away  
Keeping 1  
5 is too large, throwing it away  
Keeping 2  
10 is too large, throwing it away  
Keeping 3  
```

提供 monadic predicate 给 ``filterM``，我们便能够做 filter 的动作，同时还能保有 monadic context。

一个比较炫的技巧是用 ``filterM`` 来产生一个 list 的 powerset。一个 powerset 就是一个集合所有子集所形成的集合。如果说我们的 list 是 ``[1,2,3]``，那他个 powerset 就会是：

```haskell
[1,2,3]  
[1,2]  
[1,3]  
[1]  
[2,3]  
[2]  
[3]  
[]  
```

换句话说，要产生一个 powerset 就是要列出所有要丢掉跟保留的组合。``[2,3]`` 只不过代表我们把 ``1`` 给丢掉而已。


我们要依赖 non-determinism 来写我们这产生 powerset 的函数。我们接受一个 list ``[1,2,3]`` 然后查看第一个元素，这个例子中是 ``1``，我们会问：我们要保留他呢？还是丢掉他呢？答案是我们都要做。所以我们会用一个 non-determinism 的 predicate 来过滤我的 list。也就是我们的 ``powerset`` 函数：

```haskell
powerset :: [a] -> [[a]]  
powerset xs = filterM (\x -> [True, False]) xs 
```

等等，我们已经写完了吗？没错，就这么简单，我们可以同时丢掉跟保留每个元素。只要我们用 non-deterministic predicate，那结果也就是一个 non-deterministic value，也便是一个 list 的 list。试着跑跑看：

```haskell
ghci> powerset [1,2,3]  
[[1,2,3],[1,2],[1,3],[1],[2,3],[2],[3],[]]  
```

这样的写法需要让你好好想一下，但如果你能接受 list 其实就是 non-deterministic value 的话，那要想通会比较容易一些。


### foldM

``foldl`` 的 monadic 的版本叫做 ``foldM``。如果你还有印象的话，``foldl`` 会接受一个 binary 函数，一个起始累加值跟一串 list，他会从左边开始用 binary 函数每次带进一个值来 fold。``foldM`` 也是做同样的事，只是他接受的这个 binary 函数会产生 monadic value。不意外的，他的结果也会是 monadic value。``foldl`` 的型态是：

```haskell
foldl :: (a -> b -> a) -> a -> [b] -> a 
```

而 ``foldM`` 的型态则是：

```haskell
foldM :: (Monad m) => (a -> b -> m a) -> a -> [b] -> m a  
```

binary 函数的回传值是 monadic，所以结果也会是 monadic。我们来试着把 list 的值用 fold 全部加起来：

```haskell
ghci> foldl (\acc x -> acc + x) 0 [2,8,3,1]  
14  
```

这边起始的累加值是 ``0``，首先 ``2`` 会被加进去，变成 ``2``。然后 ``8`` 被加进去变成 ``10``，直到我们没有值可以再加，那便是最终的结果。

但如果我们想额外加一个条件，也就是当碰到一个数字大于 ``9`` 时候，整个运算就算失败呢？一种合理的修改就是用一个 binary 函数，他会检查现在这个数是否大于 ``9``，如果是便引发失败，如果不是就继续。由于有失败的可能性，我们便需要这个 binary 函数回传一个 ``Maybe``，而不是一个普通的值。我们来看看这个函数：

```haskell
binSmalls :: Int -> Int -> Maybe Int  
binSmalls acc x  
    | x > 9     = Nothing  
    | otherwise = Just (acc + x)  
```

由于这边的 binary 函数是 monadic function，我们不能用普通的 ``foldl``，我们必须用 ``foldM``：

```haskell
ghci> foldM binSmalls 0 [2,8,3,1]  
Just 14  
ghci> foldM binSmalls 0 [2,11,3,1]  
Nothing  
```

由于这串 list 中有一个数值大于 ``9``，所以整个结果会是 ``Nothing``。另外你也可以尝试 fold 一个回传 ``Writer`` 的 binary 函数，他会在 fold 的过程中纪录你想纪录的信息。


### Making a safe RPN calculator

![](miner.png)

之前的章节我们实作了一个 RPN 计算机，但我们没有做错误的处理。他只有在输入是合法的时候才会运算正确。假如有东西出错了，整个程序便会当掉。我们在这章看到了要怎样把代码转换成 monadic 的版本，我们先尝适用 ``Maybe`` monad 来帮我们的 RPN 计算机加上些错误处理。

我们的 RPN 计算机接受一个像 ``"1 3 + 2 *"`` 这样的字串，把他断成 word，变成 ``["1","3","+","2","*"]`` 这样。然后用一个 binary 函数，跟一个空的堆叠，从左边开始或是将数值推进堆叠中，或是操作堆叠最上层的两个元素。

以下便是程序的核心部份：

```haskell
import Data.List  
  
solveRPN :: String -> Double  
solveRPN = head . foldl foldingFunction [] . words  
```

我们把输入变成一个字串的 list，从左边开始 fold，当堆叠中只剩下一个元素的时候，他便是我们要的答案。以下是我们的 folding 函数：

```haskell
foldingFunction :: [Double] -> String -> [Double]  
foldingFunction (x:y:ys) "*" = (x * y):ys  
foldingFunction (x:y:ys) "+" = (x + y):ys  
foldingFunction (x:y:ys) "-" = (y - x):ys  
foldingFunction xs numberString = read numberString:xs  
```

这边我们的累加元素是一个堆叠，我们用一个 ``Double`` 的 list 来表示他。当我们在做 folding 的过程，如果当前的元素是一个 operator，他会从堆叠上拿下两个元素，用 operator 施行运算然后把结果放回堆叠。如果当前的元素是一个表示成字串的数字，他会把字串转换成数字，并回传一个新的堆叠包含了转换后的数字。

我们首先把我们的 folding 函数加上处理错误的 case，所以他的型态会变成这样：

```haskell
foldingFunction :: [Double] -> String -> Maybe [Double]  
```

他不是回传一个 ``Just`` 的堆叠就是回传 ``Nothing``。

``reads`` 函数就像 ``read`` 一样，差别在于他回传一个 list。在成功读取的情况下 list 中只包含读取的那个元素。如果他失败了，他会回传一个空的 list。除了回传读取的元素，他也回传剩下读取失败的元素。他必须要看完整串输入，我们想把他弄成一个 ``readMaybe`` 的函数，好方便我们进行。

```haskell
readMaybe :: (Read a) => String -> Maybe a  
readMaybe st = case reads st of [(x,"")] -> Just x  
                                _ -> Nothing  
```

测试结果如下：

```haskell
ghci> readMaybe "1" :: Maybe Int  
Just 1  
ghci> readMaybe "GO TO HELL" :: Maybe Int  
Nothing  
```

看起来运作正常。我们再把他变成一个可以处理失败情况的 monadic 函数

```haskell
foldingFunction :: [Double] -> String -> Maybe [Double]  
foldingFunction (x:y:ys) "*" = return ((x * y):ys)  
foldingFunction (x:y:ys) "+" = return ((x + y):ys)  
foldingFunction (x:y:ys) "-" = return ((y - x):ys)  
foldingFunction xs numberString = liftM (:xs) (readMaybe numberString)  
```

前三种 case 跟前面的很像，只差在堆叠现在是包在 ``Just`` 里面（我们常常是用 ``return`` 来做到这件事，但其实我们也可以用 ``Just``）。在最后一种情况，我们用 ``readMaybe numberString`` 然后我们用 ``(:xs)`` map over 他。所以如果堆叠 ``xs`` 是 ``[1.0,2.0]`` 且 ``readMaybe numberString`` 产生 ``Just 3.0``，那结果便是 ``Just [3.0,1.0,2.0]``。如果 ``readyMaybe numberString`` 产生 ``Nothing`` 那结果便是 ``Nothing``。我们来试着跑跑看 folding 函数

```haskell
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

看起来正常运作。我们可以用他来写一个新的 ``solveRPN``。

```haskell
import Data.List  
  
solveRPN :: String -> Maybe Double  
solveRPN st = do  
  [result] <- foldM foldingFunction [] (words st)  
  return result  
```

我们仍是接受一个字串把他断成一串 word。然后我们用一个空的堆叠来作 folding 的动作，只差在我们用的是 ``foldM`` 而不是 ``foldl``。``foldM`` 的结果会是 ``Maybe``，``Maybe`` 里面包含了一个只有一个元素的 list。我们用 ``do`` expression 来取出值，把他绑定到 ``result`` 上。当 ``foldM`` 回传 ``Nothing`` 的时候，整个结果就变成 ``Nothing``。也特别注意我们有在 ``do`` 里面做 pattern match 的动作，所以如果 list 中不是只有一个元素的话，最后结果便会是 ``Nothing``。最后一行我们用 ``return result`` 来展示 RPN 计算的结果，把他包在一个 ``Maybe`` 里面。

```haskell
ghci> solveRPN "1 2 * 4 +"  
Just 6.0  
ghci> solveRPN "1 2 * 4 + 5 *"  
Just 30.0  
ghci> solveRPN "1 2 * 4"  
Nothing  
ghci> solveRPN "1 8 wharglbllargh"  
Nothing  
```

第一个例子会失败是因为 list 中不是只有一个元素，所以 ``do`` 里面的 pattern matching 失败了。第二个例子会失败是因为 ``readMaybe`` 回传了 ``Nothing``。

### Composing monadic functions

当我们介绍 monad law 的时候，我们说过 ``<=<`` 就像是函数合成一样，只差在一个是作用在普通函数 ``a -> b``。一个是作用在 monadic 函数 ``a -> m b``。

```haskell
ghci> let f = (+1) . (*100)  
ghci> f 4  
401  
ghci> let g = (\x -> return (x+1)) <=< (\x -> return (x*100))  
ghci> Just 4 >>= g  
Just 401  
```

在这个例子中我们合成了两个普通的函数，并喂给给他 ``4``。我们也合成了两个 monadic 函数并用 ``>>=`` 喂给他 ``Just 4``。

如果我们在 list 中有一大堆函数，我们可以把他们合成一个巨大的函数。用 ``id`` 当作累加的起点，``.`` 当作 binary 函数，用 fold 来作这件事。

```haskell
ghci> let f = foldr (.) id [(+1),(*100),(+1)]  
ghci> f 1  
201  
```

``f`` 接受一个数字，然后会帮他加 ``1``，乘以 ``100``，再加 ``1``。我们也可以将 monadic 函数用同样的方式做合成，只是不用 ``.`` 而用 ``<=<``，不用 ``id`` 而用 ``return``。我们不需要 ``foldM``，由于 ``<=<`` 只用 ``foldr`` 就足够了。

当我们在之前的章节介绍 list monad 的时候， 我们用他来解决一个骑士是否能在三步内走到另一点的问题。 那个函数叫做 ``moveKnight``， 他接受一个座标然后回传所有可能的下一步。 然后产生出所有可能三步的移动。

```haskell
in3 start = return start >>= moveKnight >>= moveKnight >>= moveKnight   
```

要检查我们是否能只用三步从 ``start`` 走到 ``end``，我们用下列函数

```haskell
canReachIn3 :: KnightPos -> KnightPos -> Bool  
canReachIn3 start end = end `elem` in3 start  
```

如果使用 monadic 版本的合成的话，我们也可以做一个类似的 ``in3``，但我们希望他不只有三步的版本，而希望有任意步的版本。如果你仔细观察 ``in3``，他只不过用 ``>>=`` 跟 ``moveKnight`` 把之前所有可能结果喂到下一步。把他一般化，就会像下面的样子：

```haskell
import Data.List  
  
inMany :: Int -> KnightPos -> [KnightPos]  
inMany x start = return start >>= foldr (<=<) return (replicate x moveKnight)  
```

首先我们用 ``replicate`` 来做出一个 list，里面有 ``x`` 份的 ``moveKnight``。然后我们把所有函数都合成起来，就会给我们从起点走 ``x`` 步内所有可能的的位置。然后我们只需要把起始位置喂给他就好了。

我们也可以一般化我们的 ``canReachIn3``：

```haskell
canReachIn :: Int -> KnightPos -> KnightPos -> Bool  
canReachIn x start end = end `elem` inMany x start  
```


## 定义自己的 Monad

![](spearhead.png)

在这一章节，我们会带你看看究竟一个型态是怎么被辨认，确认是一个 monad 而且正确定义出 ``Monad`` 的 instance。我们通常不会为了定义 monad 而定义。比较常发生的是，我们想要针对一个问题建立模型，却稍后发现我们定义的型态其实是一个 Monad，所以就定义一个 ``Monad`` 的 instance。

正如我们看到的，list 是被拿来当作 non-deterministic values。对于 ``[3,5,9]``，我们可以看作是一个 non-deterministic value，我们不能知道究竟是哪一个。当我们把一个 list 用 ``>>=`` 喂给一个函数，他就是把一串可能的选择都丢给函数，函数一个个去计算在那种情况下的结果，结果也便是一个 list。

如果我们把 ``[3,5,9]`` 看作是 ``3``,``5``,``9`` 各出现一次，但这边没有每一种数字出现的机率。如果我们把 non-deterministic 的值看作是 ``[3,5,9]``，但 ``3`` 出现的机率是 50%，``5`` 跟 ``9`` 出现的机率各是 25%呢？我们来试着用 Haskell 描述看看。

如果说 list 中的每一个元素都伴随着他出现的机率。那下面的形式就蛮合理的：

```haskell
[(3,0.5),(5,0.25),(9,0.25)]  
```

在数学上，机率通常不是用百分比表示，而是用介于 0 跟 1 的实数表示。0 代表不可能会发生，而 1 代表绝对会发生。但浮点数很有可能很快随着运算失去精准度，所以 Haskell 有提供有理数。他的型态是摆在 ``Data.Ratio`` 中，叫做 ``Rational``。要创造出一个 ``Rational``，我们会把他写成一个分数的形式。分子跟分母用 ``%`` 分隔。这边有几个例子：

```haskell
ghci> 1%4  
1 % 4  
ghci> 1%2 + 1%2  
1 % 1  
ghci> 1%3 + 5%4  
19 % 12  
```

第一行代表四分之一，第二行代表两个二分之一加起来变成一。而第三行我们把三分之一跟四分之五加起来变成十二分之十九。所以我们来用 ``Rational`` 取代浮点数来当作我们的机率值吧。

```haskell
ghci> [(3,1%2),(5,1%4),(9,1%4)]  
[(3,1 % 2),(5,1 % 4),(9,1 % 4)]  
```

所以 ``3`` 有二分之一的机会出现，而 ``5`` 跟 ``9`` 有四分之一的机会出现。


可以看到我们帮 list 加上了一些额外的 context。再我们继续深入之前，我们用一个 ``newtype`` 把他包起来，好让我们帮他写 instance。

```haskell
import Data.Ratio

newtype Prob a = Prob { getProb :: [(a,Rational)] } deriving Show  
```

接着我们想问，这是一个 functor 吗？list 是一个 functor，所以很有可能他也是一个 functor，毕竟我们只是在 list 上多加一些东西而已。在 list 的情况下，我们可以针对每个元素用函数做处理。这边我们也是用函数针对每个元素做处理，只是我们是输出机率值。所以我们就来写个 functor 的 instance 吧。

```haskell
instance Functor Prob where  
    fmap f (Prob xs) = Prob $ map (\(x,p) -> (f x,p)) xs 
```

我们可以用 pattern matching 的方式把 ``newtype`` 解开来，套用函数 ``f`` 之后再包回去。过程中不会动到机率值。

```haskell
ghci> fmap negate (Prob [(3,1%2),(5,1%4),(9,1%4)])  
Prob {getProb = [(-3,1 % 2),(-5,1 % 4),(-9,1 % 4)]}  
```

要注意机率的和永远是 ``1``。如果我们没有漏掉某种情形的话，没有道理他们加起来的值不为 ``1``。一个有 75% 机率是正面以及 50% 机率是反面的硬币根本没什么道理。

接着要问一个重要的问题，他是一个 monad 吗？我们知道 list 是一个 monad，所以他很有可能也是一个 monad。首先来想想 ``return``。他在 list 是怎么运作的？他接受一个普通的值并把他放到一个 list 中变成只有一个元素的 list。那在这边又如何？由于他是一个最小的 context，他也应该是一个元素的 list。那机率值呢？``return x`` 的值永远都是 ``x``，所以机率值不应该是 ``0``，而应该是 ``1``。


至于 ``>>=`` 呢？看起来有点复杂，所以我们换种方式来思考，我们知道 ``m >>= f`` 会等价于 ``join (fmap f m)``，所以我们来想要怎么把一串包含 probability list 的 list 弄平。举个例子，考虑一个 list，``'a'`` 跟 ``'b'`` 恰出现其中一个的机率为 25%，两个出现的机率相等。而 ``'c'`` 跟 ``'d'`` 恰出现其中一个的机率为 75%，两个出现的机率也是相等。这边有一个图将情形画出来。


![](prob.png)

每个字母发生的机率有多高呢？如果我们用四个盒子来代表每个字母，那每个盒子的机率为何？每个盒子的机率是他们所装有的机率值相乘的结果。``'a'`` 的机率是八分之一，``'b'`` 同样也是八分之一。八分之一是因为我们把二分之一跟四分之一相乘得到的结果。而 ``'c'`` 发生的机率是八分之三，是因为二分之一乘上四分之三。``'d'`` 同样也是八分之三。如果把所有的机率加起来，就会得到一，符合机率的规则。

来看看怎么用一个 list 表达我们要说明的东西：

```haskell
thisSituation :: Prob (Prob Char)  
thisSituation = Prob  
    [( Prob [('a',1%2),('b',1%2)] , 1%4 )  
    ,( Prob [('c',1%2),('d',1%2)] , 3%4 )  
    ]
```

注意到这边的型态是 ``Prob (Prob Char)``。所以我们要思考的是如何把一串包含机率 list 的 list 打平。如果能成功写出这样的逻辑，``>>=`` 不过就是 ``join (fmap f m)``，我们便得到了一个 monad。我们这边写了一个 ``flatten`` 来做这件事。

```haskell
flatten :: Prob (Prob a) -> Prob a  
flatten (Prob xs) = Prob $ concat $ map multAll xs  
    where multAll (Prob innerxs,p) = map (\(x,r) -> (x,p*r)) innerxs  
```

``multAll`` 接受一个 tuple，里面包含一个 probability list 跟一个伴随的机率值 ``p``，所以我们要作的事是把 list 里面的机率值都乘以 ``p``，并回传一个新的 tuple 包含新的 list 跟新的机率值。我们将 ``multAll`` map over 到我们的 probability list 上，我们就成功地打平了我们的 list。

现在我们就能定义我们的 ``Monad`` instance。

```haskell
instance Monad Prob where  
    return x = Prob [(x,1%1)]  
    m >>= f = flatten (fmap f m)  
    fail _ = Prob []  
```

![](ride.png)

由于我们已经把所有苦工的做完了，定义这个 instance 显得格外轻松。我们也定义了 ``fail``，我们定义他的方式跟定义 list 一样。如果在 ``do`` 中发生了失败的 pattern match，那就会调用 ``fail``。

检查我们定义的 instance 是否遵守 monad law 也是很重要的。monad law 的第一个定律是 ``return x >>= f`` 应该要等价于 ``f x``。要写出严格的证明会很麻烦，但我们可以观察到下列事实：首先用 ``return`` 做一个最小的 context，然后用 ``fmap`` 将一个函数 map over 这个 context，再将他打平。这样做出来的 probability list，每一个机率值都相当于将我们最初放到 minimal context 中的值乘上 ``1%1``。同样的逻辑，也可以看出 ``m >>= return`` 是等价于 ``m``。第三个定律是 ``f <=< (g <=< h)`` 应该要等价于 ``(f <=< g) <=< h``。我们可以从乘法有结合律的性质，以及 list monad 的特性上推出 probability monad 也符合这个定律。``1%2 * (1%3 * 1%5)`` 等于 ``(1%2 * 1%3) * 1%5``。

现在我们有了一个 monad，这样有什么好处呢？他可以帮助我们计算机率值。我们可以把机率事件看作是具有 context 的 value，而 probability monad 可以保证机率值能正确地被计算成最终的结果。

好比说我们现在有两个普通的硬币以及一个灌铅的硬币。灌铅的硬币十次中有九次会出现正面，只有一次会出现反面。如果我们一次丢掷这三个硬币，有多大的机会他们都会出现正面呢？让我们先来表达丢掷硬币这件事，分别丢的是灌铅的跟普通的硬币。


```haskell
data Coin = Heads | Tails deriving (Show, Eq)  

coin :: Prob Coin  
coin = Prob [(Heads,1%2),(Tails,1%2)]  

loadedCoin :: Prob Coin  
loadedCoin = Prob [(Heads,1%10),(Tails,9%10)]  
```

最后，来看看掷硬币的函数：

```haskell
import Data.List (all)  
  
flipThree :: Prob Bool  
flipThree = do  
  a <- coin  
  b <- coin  
  c <- loadedCoin  
  return (all (==Tails) [a,b,c])  
```

试着跑一下的话，我们会看到尽管我们用了不公平的硬币，三个反面的机率还是不高。

```haskell
ghci> getProb flipThree  
[(False,1 % 40),(False,9 % 40),(False,1 % 40),(False,9 % 40),  
 (False,1 % 40),(False,9 % 40),(False,1 % 40),(True,9 % 40)]  
```

同时出现正面的机率是四十分之九，差不多是 25%的机会。我们的 monad 并没有办法 join 所有都是 ``False`` 的情形，也就是所有硬币都是出现反面的情况。不过那不是个严重的问题，可以写个函数来将同样的结果变成一种结果，这就留给读者当作习题。

在这章节中，我们从提出问题到真的写出型态，并确认这个型态是一个 monad，写出他的 instance 并实际操作他。这是个很棒的经验。现在读者们应该对于 monad 有不少的了解才是。

