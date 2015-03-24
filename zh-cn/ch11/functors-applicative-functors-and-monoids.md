# Functors, Applicative Functors 与 Monoids

Haskell 的一些特色，像是纯粹性，高阶函数，algebraic data types，typeclasses，这些让我们可以从更高的角度来看到 polymorphism 这件事。不像 OOP 当中需要从庞大的型态阶层来思考。我们只需要看看手边的型态的行为，将他们跟适当地 typeclass 对应起来就可以了。像 ``Int`` 的行为跟很多东西很像。好比说他可以比较相不相等，可以从大到小排列，也可以将他们一一穷举出来。

Typeclass 的运用是很随意的。我们可以定义自己的资料型态，然后描述他可以怎样被操作，跟 typeclass 关联起来便定义了他的行为。由于 Haskell 强大的型态系统，这让我们只要读函数的型态宣告就可以知道很多资讯。typeclass 可以定义得很抽象很 general。我们之前有看过 typeclass 定义了可以比较两个东西是否相等，或是定义了可以比较两个东西的大小。这些是既抽象但又描述简洁的行为，但我们不会认为他们有什么特别之处，因为我们时常碰到他们。最近我们看过了 functor，基本上他们是一群可以被 map over 的物件。这是其中一个例子能够抽象但又漂亮地描述行为。在这一章中，我们会详加阐述 functors，并会提到比较强一些的版本，也就是 applicative functors。我们也会提到 monoids。


## 温习 Functors

![](frogtor.png)

我们已经在之前的章节提到 functors。如果你还没读那个章节，也许你应该先去看看。或是你直接假装你已经读过了。

来快速复习一下：Functors 是可以被 map over 的物件，像是 lists，``Maybe``，trees 等等。在 Haskell 中我们是用 ``Functor`` 这个 typeclass 来描述他。这个 typeclass 只有一个 method，叫做 ``fmap``，他的型态是 ``fmap :: (a -> b) ->  f a -> f b``。这型态说明了如果给我一个从 ``a`` 映到 ``b`` 的函数，以及一个装了 ``a`` 的盒子，我会回给你一个装了 ``b`` 的盒子。就好像用这个函数将每个元素都转成 ``b`` 一样

    *给一点建议*。这盒子的比喻尝试让你抓到些 functors 是如何运作的感觉。在之后我们也会用相同的比喻来比喻 applicative functors 跟 monads。在多数情况下这种比喻是恰当的，但不要过度引申，有些 functors 是不适用这个比喻的。一个比较正确的形容是 functors 是一个计算语境（computational context）。这个语境可能是这个 computation 可能带有值，或是有可能会失败（像 ``Maybe`` 跟 ``Either a``），或是他可能有多个值（像 lists），等等。

如果一个 type constructor 要是 ``Functor`` 的 instance，那他的 kind 必须是 ``* -> *``，这代表他必须刚好接受一个 type 当作 type parameter。像是 ``Maybe`` 可以是 Functor 的一个 instance，因为他接受一个 type parameter，来做成像是 ``Maybe Int``，或是 ``Maybe String``。如果一个 type constructor 接受两个参数，像是 ``Either``，我们必须给他两个 type parameter。所以我们不能这样写：``instance Functor Either where``，但我们可以写 ``instance Functor (Either a) where``，如果我们把 ``fmap`` 限缩成只是 ``Either a`` 的，那他的型态就是 ``fmap :: (b -> c) -> Either a b -> Either a c``。就像你看到的，``Either a`` 的是固定的一部分，因为 ``Either a`` 只恰好接受一个 type parameter，但 ``Either`` 则要接受两个 type parameters。这样 fmap 的型态变成 ``fmap :: (b -> c) -> Either b -> Either c``，这不太合理。

我们知道有许多型态都是 ``Functor`` 的 instance，像是 ``[]``，``Maybe``，``Either a`` 以及我们自己写的 ``Tree``。我们也看到了如何用一个函数 map 他们。在这一章节，我们再多举两个例子，也就是 ``IO`` 跟 ``(->) r``。

如果一个值的型态是 ``IO String``，他代表的是一个会被计算成 String 结果的 I/O action。我们可以用 do syntax 来把结果绑定到某个名称。我们之前把 I/O action 比喻做长了脚的盒子，会到真实世界帮我们取一些值回来。我们可以检视他们取了什么值，但一旦看过，我们必须要把值放回盒子中。用这个比喻，``IO`` 的行为就像是一个 functor。

我们来看看 ``IO`` 是怎么样的一个 ``Functor`` instance。当我们 ``fmap`` 用一个 function 来 map over I/O action 时，我们会想要拿回一个装着已经用 function 映射过值的 I/O action。

```haskell
instance Functor IO where
    fmap f action = do
        result <- action
        return (f result)
```

对一个 I/O action 做 map over 动作的结果仍会是一个 I/O action，所以我们才用 do syntax 来把两个 I/O action 黏成一个。在 ``fmap`` 的实作中，我们先执行了原本传进的 I/O action，并把结果绑定成 ``result``。然后我们写了 ``return (f result)``。``return`` 就如你所知道的，是一个只会回传包了你传给他东西的 I/O action。还有一个 do block 的回传值一定是他最后一个 I/O action 的回传值。这也是为什么我们需要 return。其实他只是回传包了 ``f result`` 的 I/O action。

我们可以再多实验一下来找到些感觉。来看看这段 code：

```haskell
main = do line <- getLine   
        let line' = reverse line  
        putStrLn $ "You said " ++ line' ++ " backwards!"  
        putStrLn $ "Yes, you really said" ++ line' ++ " backwards!"  
```

这程式要求使用者输入一行文字，然后印出一行反过来的。
我们可以用 ``fmap`` 来改写：

```haskell
main = do line <- fmap reverse getLine  
            putStrLn $ "You said " ++ line ++ " backwards!"  
            putStrLn $ "Yes, you really said" ++ line ++ " backwards!"  
```

![](alien.png)

就像我们用 ``fmap`` ``reverse`` 来 map over ``Just "blah"`` 会得到 ``Just "halb"``，我们也可以 ``fmap`` ``reverse`` 来 map over ``getLine``。``getLine`` 是一个 I/O action，他的 type 是 ``IO String``，而用 ``reverse`` 来 map over 他会回传一个取回一个字串并 ``reverse`` 他的 I/O action。就像我们 apply 一个 function 到一个 ``Maybe`` 一样，我们也可以 apply 一个 function 到一个 ``IO``，只是这个 ``IO`` 会跑去外面拿回某些值。然后我们把结果用 ``<-`` 绑定到某个名称，而这个名称绑定的值是已经 ``reverse`` 过了。

而 ``fmap (++"!") getLine`` 这个 I/O action 表现得就像 ``getLine``，只是他的结果多了一个 ``"!"`` 在最后。

如果我们限缩 ``fmap`` 到 ``IO`` 型态上，那 fmap 的型态是 ``fmap :: (a -> b) -> IO a -> IO b``。``fmap`` 接受一个函数跟一个 I/O action，并回传一个 I/O action 包含了已经 apply 过 function 的结果。

如果你曾经注意到你想要将一个 I/O action 绑定到一个名称上，只是为了要 apply 一个 function。你可以考虑使用 ``fmap``，那会更漂亮地表达这件事。或者你想要对 functor 中的资料做 transformation，你可以先将你要用的 function 写在 top level，或是把他作成一个 lambda function，甚至用 function composition。

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

正如你想的，``intersperse '-' . reverse . map toUpper`` 合成了一个 function，他接受一个字串，将他转成大写，然后反过来，再用 ``intersperse '-'`` 安插'-'。他是比较漂亮版本的 ``(\xs -> intersperse '-' (reverse (map toUpper xs)))``。

另一个 ``Functor`` 的案例是 ``(->) r``，只是我们先前没有注意到。你可能会困惑到底 ``(->) r`` 究竟代表什么？一个 ``r -> a`` 的型态可以写成 ``(->) r a``，就像是 ``2 + 3`` 可以写成 ``(+) 2 3`` 一样。我们可以从一个不同的角度来看待 ``(->) r a``，他其实只是一个接受两个参数的 type constructor，好比 ``Either``。但记住我们说过 ``Functor`` 只能接受一个 type constructor。这也是为什么 ``(->)`` 不是 ``Functor`` 的一个 instance，但 ``(->) r`` 则是。如果程式的语法允许的话，你也可以将 ``(->) r`` 写成 ``(r ->)``。就如 ``(2+)`` 代表的其实是 ``(+) 2``。至于细节是如何呢？我们可以看看 ``Control.Monad.Instances``。

    我们通常说一个接受任何东西以及回传随便一个东西的函数型态是 ``a -> b``。``r -> a`` 是同样意思，只是把符号代换了一下。

```haskell
instance Functor ((->) r) where  
    fmap f g = (\x -> f (g x))  
```

如果语法允许的话，他可以被写成

```haskell
instance Functor (r ->) where  
    fmap f g = (\x -> f (g x))  
```

但其实是不允许的，所以我们必须写成第一种的样子。

首先我们来看看 ``fmap`` 的型态。他的型态是 ``fmap :: (a -> b) -> f a -> f b``。我们把所有的 ``f`` 在心里代换成 ``(->) r``。则 ``fmap`` 的型态就变成 ``fmap :: (a -> b) -> ((->) r a) -> ((->) r b)``。接着我们把 ``(->) r a`` 跟 ``(->) r b`` 换成 ``r -> a`` 跟 ``r -> b``。则我们得到 ``fmap :: (a -> b) -> (r -> a) -> (r -> b)``。

从上面的结果看到将一个 function map over 一个 function 会得到另一个 function，就如 map over 一个 function 到 ``Maybe`` 会得到一个 ``Maybe``，而 map over 一个 function 到一个 list 会得到一个 list。而 ``fmap :: (a -> b) -> (r -> a) -> (r -> b)`` 告诉我们什么？他接受一个从 ``a`` 到 ``b`` 的 function，跟一个从 ``r`` 到 ``a`` 的 function，并回传一个从 ``r`` 到 ``b`` 的 function。这根本就是 function composition。把 ``r -> a`` 的输出接到 ``a -> b`` 的输入，的确是 function composition 在做的事。如果你再仔细看看 instance 的定义，会发现真的就是一个 function composition。

```haskell
instance Functor ((->) r) where  
    fmap = (.)  
```

这很明显就是把 ``fmap`` 当 composition 在用。可以用 ``:m + Control.Monad.Instances`` 把模组装载进来，并做一些尝试。

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

我们呼叫 ``fmap`` 的方式是 infix 的方式，这跟 ``.`` 很像。在第二行，我们把 ``(*3)`` map over 到 ``(+100)`` 上，这会回传一个先把输入值 ``(+100)`` 再 ``(*3)`` 的 function，我们再用 ``1`` 去呼叫他。

到这边为止盒子的比喻还适用吗？如果你硬是要解释的话还是解释得通。当我们将 ``fmap (+3)`` map over ``Just 3`` 的时候，对于 ``Maybe`` 我们很容易把他想成是装了值的盒子，我们只是对盒子里面的值 ``(+3)``。但对于 ``fmap (*3) (+100)`` 呢？你可以把 ``(+100)`` 想成是一个装了值的盒子。有点像把 I/O action 想成长了脚的盒子一样。对 ``(+100)`` 使用 ``fmap (*3)`` 会产生另一个表现得像 ``(+100)`` 的 function。只是在算出值之前，会再多计算 ``(*3)``。这样我们可以看出来 ``fmap`` 表现得就像 ``.`` 一样。

``fmap`` 等同于 function composition 这件事对我们来说并不是很实用，但至少是一个有趣的观点。这也让我们打开视野，看到盒子的比喻不是那么恰当，functors 其实比较像 computation。function 被 map over 到一个 computation 会产生经由那个 function 映射过后的 computation。

![](lifter.png)

在我们继续看 ``fmap`` 该遵守的规则之前，我们再看一次 ``fmap`` 的型态，他是 ``fmap :: (a -> b) -> f a -> f b``。很明显我们是在讨论 Functor，所以为了简洁，我们就不写 ``(Functor f) =>`` 的部份。当我们在学 curry 的时候，我们说过 Haskell 的 function 实际上只接受一个参数。一个型态是 ``a -> b -> c`` 的函数实际上是接受 ``a`` 然后回传 ``b -> c``，而 ``b -> c`` 实际上接受一个 ``b`` 然后回传一个 ``c``。如果我们用比较少的参数呼叫一个函数，他就会回传一个函数需要接受剩下的参数。所以 ``a -> b -> c`` 可以写成 ``a -> (b -> c)``。这样 curry 可以明显一些。

同样的，我们可以不要把 ``fmap`` 想成是一个接受 function 跟 functor 并回传一个 function 的 function。而是想成一个接受 function 并回传一个新的 function 的 function，回传的 function 接受一个 functor 并回传一个 functor。他接受 ``a -> b`` 并回传 ``f a -> f b``。这动作叫做 lifting。我们用 GHCI 的 ``:t`` 来做的实验。

```haskell
ghci> :t fmap (*2)  
fmap (*2) :: (Num a, Functor f) => f a -> f a  
ghci> :t fmap (replicate 3)  
fmap (replicate 3) :: (Functor f) => f a -> f [a]  
```

``fmap (*2)`` 接受一个 functor ``f``，并回传一个基于数字的 functor。那个 functor 可以是 list，可以是 ``Maybe``，可以是 ``Either String``。``fmap (replicate 3)`` 可以接受一个基于任何型态的 functor，并回传一个基于 list 的 functor。

    当我们提到 functor over numbers 的时候，你可以想像他是一个 functor 包含有许多数字在里面。前面一种说法其实比较正确，但后面一种说法比较容易让人理解。

这样的观察在我们只有绑定一个部份套用的函数，像是 ``fmap (++"!")``，的时候会显得更清楚，

你可以把 ``fmap`` 想做是一个函数，他接受另一个函数跟一个 functor，然后把函数对 functor 每一个元素做映射，或你可以想做他是一个函数，他接受一个函数并把他 lift 到可以在 functors 上面操作。两种想法都是正确的，而且在 Haskell 中是等价。

``fmap (replicate 3) :: (Functor f) => f a -> f [a]`` 这样的型态代表这个函数可以运作在任何 functor 上。至于确切的行为则要看究竟我们操作的是什么样的 functor。如果我们是用 ``fmap (replicate 3)`` 对一个 list 操作，那我们会选择 ``fmap`` 针对 list 的实作，也就是只是一个 ``map``。如果我们是碰到 ``Maybe a``。那他在碰到 ``Just`` 型态的时候，会对里面的值套用 ``replicate 3``。而碰到 ``Nothing`` 的时候就回传 ``Nothing``。

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


接下来我们来看看 functor laws。一个东西要成为 functor，必须要遵守某些定律。不管任何一个 functor 都被要求具有某些性质。他们必须是能被 map over 的。对他们呼叫 ``fmap`` 应该是要用一个函数 map 每一个元素，不多做任何事情。这些行为都被 functor laws 所描述。对于 ``Functor`` 的 instance 来说，总共两条定律应该被遵守。不过他们不会在 Haskell 中自动被检查，所以你必须自己确认这些条件。

functor law 的第一条说明，如果我们对 functor 做 map ``id``，那得到的新的 functor 应该要跟原来的一样。如果写得正式一点，他代表 ``fmap id = id``。基本上他就是说对 functor 呼叫 ``fmap id``，应该等同于对 functor 呼叫 ``id`` 一样。毕竟 ``id`` 只是 identity function，他只会把参数照原样丢出。他也可以被写成 ``\x -> x``。如果我们对 functor 的概念就是可以被 map over 的物件，那 ``fmap id = id`` 的性就显而易见。

我们来看看这个定律的几个案例：

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

如果我们看看 ``Maybe`` 的 ``fmap`` 的实作，我们不难发现第一定律为何被遵守。

```haskell
instance Functor Maybe where  
    fmap f (Just x) = Just (f x)  
    fmap f Nothing = Nothing  
```

我们可以想像在 ``f`` 的位置摆上 ``id``。我们看到 ``fmap id`` 拿到 ``Just x`` 的时候，结果只不过是 ``Just (id x)``，而 ``id`` 有只回传他拿到的东西，所以可以知道 ``Just (id x)`` 等价于 ``Just x``。所以说我们可以知道对 ``Maybe`` 中的 ``Just`` 用 ``id`` 去做 map over 的动作，会拿回一样的值。

而将 ``id`` map over ``Nothing`` 会拿回 ``Nothing`` 并不稀奇。所以从这两个 ``fmap`` 的实作，我们可以看到的确 ``fmap id = id`` 有被遵守。


![](justice.png)


*第二定律描述说先将两个函数合成并将结果 map over 一个 functor 的结果，应该跟先将第一个函数 map over 一个 functor，再将第二个函数 map over 那个 functor 的结果是一样的。*正式地写下来的话就是 ``fmap (f . g) = fmap f . fmap g``。或是用另外一种写法，对于任何一个 functor F，下面这个式子应该要被遵守：``fmap (f . g) F = fmap f (fmap g F)``。


如果我们能够证明某个型别遵守两个定律，那我们就可以保证他跟其他 functor 对于映射方面都拥有相同的性质。我们知道如果对他用 ``fmap``，我们知道不会有除了 mapping 以外的事会发生，而他就仅仅会表现成某个可以被 map over 的东西。也就是一个 functor。你可以再仔细检视 ``fmap`` 对于某些型别的实作来了解第二定律。正如我们先前对 ``Maybe`` 检视第一定律一般。

如果你需要的话，我们能在这边演练一下 ``Maybe`` 是如何遵守第二定律的。首先 ``fmap (f . g)`` 来 map over ``Nothing`` 的话，我们会得到 ``Nothing``。因为用任何函数来 ``fmap`` ``Nothing`` 的话都会回传 ``Nothing``。如果我们 ``fmap f (fmap g Nothing)``，我们会得到 ``Nothing``。可以看到当面对 ``Nothing`` 的时候，``Maybe`` 很显然是遵守第二定律的。
那对于 ``Just something`` 呢？如果我们使用 ``fmap (f . g) (Just x)`` 的话，从实作的程式码中我可以看到 ``Just ((f . g ) x)``，也就是 ``Just (f (g x))``。如果我们使用 ``fmap f (fmap g (Just x))`` 的话我们可以从实作知道 ``fmap g (Just x)`` 会是 ``Just (g x)``。``fmap f (fmap g (Just x))`` 跟 ``fmap f (Just (g x))`` 相等。而从实作上这又会相等于 ``Just (f (g x))``。

如果你不太理解这边的说明，别担心。只要确定你了解什么是函数合成就好。在多数的情况下你可以直觉地对应到这些型别表现得就像 containers 或函数一样。或是也可以换种方法，只要多尝试对型别中不同的值做操作你就可以看看型别是否有遵守定律。

我们来看一些经典的例子。这些型别建构子虽然是 ``Functor`` 的 instance，但实际上他们并不是 functor，因为他们并不遵守这些定律。我们来看看其中一个型别。

```haskell
data CMaybe a = CNothing | CJust Int a deriving (Show)      
```

C 这边代表的是计数器。他是一种看起来像是 ``Maybe a`` 的型别，只差在 ``Just`` 包含了两个 field 而不是一个。在 ``CJust`` 中的第一个 field 是 ``Int``，他是扮演计数器用的。而第二个 field 则为型别 ``a``，他是从型别参数来的，而他确切的型别当然会依据我们选定的 ``CMaybe a`` 而定。我们来对他作些操作来获得些操作上的直觉吧。

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


如果我们使用 ``CNothing``，就代表不含有 field。如果我们用的是 ``CJust``，那第一个 field 是整数，而第二个 field 可以为任何型别。我们来定义一个 ``Functor`` 的 instance，这样每次我们使用 ``fmap`` 的时候，函数会被套用在第二个 field，而第一个 field 会被加一。

```haskell
instance Functor CMaybe where  
    fmap f CNothing = CNothing  
    fmap f (CJust counter x) = CJust (counter+1) (f x)  
```

这种定义方式有点像是 ``Maybe`` 的定义方式，只差在当我们使用 ``fmap`` 的时候，如果碰到的不是空值，那我们不只会套用函数，还会把计数器加一。我们可以来看一些范例操作。


```haskell
ghci> fmap (++"ha") (CJust 0 "ho")  
CJust 1 "hoha"  
ghci> fmap (++"he") (fmap (++"ha") (CJust 0 "ho"))  
CJust 2 "hohahe"  
ghci> fmap (++"blah") CNothing  
CNothing  
```


这些会遵守 functor laws 吗？要知道有不遵守的情形，只要找到一个反例就好了。


```haskell
ghci> fmap id (CJust 0 "haha")  
CJust 1 "haha"  
ghci> id (CJust 0 "haha")  
CJust 0 "haha"  
```


我们知道 functor law 的第一定律描述当我们用 ``id`` 来 map over 一个 functor 的时候，他的结果应该跟只对 functor 呼叫 ``id`` 的结果一样。但我们可以看到这个例子中，这对于 ``CMaybe`` 并不遵守。尽管他的确是 ``Functor`` typeclass 的一个 instace。但他并不遵守 functor law 因此不是一个 functor。如果有人使用我们的 ``CMaybe`` 型别，把他当作 functor 用，那他就会期待 functor laws 会被遵守。但 ``CMaybe`` 并没办法满足，便会造成错误的程式。当我们使用一个 functor 的时候，函数合成跟 map over 的先后顺序不应该有影响。但对于 ``CMaybe`` 他是有影响的，因为他纪录了被 map over 的次数。如果我们希望 ``CMaybe`` 遵守 functor law，我们必须要让 ``Int`` 栏位在做 ``fmap`` 的时候维持不变。


乍看之下 functor laws 看起来不是很必要，也容易让人搞不懂，但我们知道如果一个型别遵守 functor laws，那我们就能对他作些基本的假设。如果遵守了 functor laws，我们知道对他做 ``fmap`` 不会做多余的事情，只是用一个函数做映射而已。这让写出来的程式码足够抽象也容易扩展。因为我们可以用定律来推论型别的行为。


所有在标准函式库中的 ``Functor`` 的 instance 都遵守这些定律，但你可以自己检查一遍。下一次你定义一个型别为 ``Functor`` 的 instance 的时候，花点时间确认他确实遵守 functor laws。一旦你操作过足够多的 functors 时，你就会获得直觉，知道他们会有什么样的性质跟行为。而且 functor laws 也会觉得显而易见。但就算没有这些直觉，你仍然可以一行一行地来找看看有没有反例让这些定律失效。


我们可以把 functor 看作输出具有 context 的值。例如说 ``Just 3`` 就是输出 ``3``，但他又带有一个可能没有值的 context。``[1,2,3]`` 输出三个值，``1``,``2`` 跟 ``3``，同时也带有可能有多个值或没有值的 context。``(+3)`` 则会带有一个依赖于参数的 context。


如果你把 functor 想做是输出值这件事，那你可以把 map over 一个 functor 这件事想成在 functor 输出的后面再多加一层转换。当我们做 ``fmap (+3) [1,2,3]`` 的时候，我们是把 ``(+3)`` 接到 ``[1,2,3]`` 后面，所以当我们检视任何一个 list 的输出的时候，``(+3)`` 也会被套用在上面。另一个例子是对函数做 map over。当我们做 ``fmap (+3) (*3)``，我们是把 ``(+3)`` 这个转换套用在 ``(*3)`` 后面。这样想的话会很自然就会把 ``fmap`` 跟函数合成关联起来（``fmap (+3) (*3)`` 等价于 ``(+3) . (*3)``，也等价于 ``\x -> ((x*3)+3)``），毕竟我们是接受一个函数 ``(*3)`` 然后套用 ``(+3)`` 转换。最后的结果仍然是一个函数，只是当我们喂给他一个数字的时候，他会先乘上三然后做转换加上三。这基本上就是函数合成在做的事。

## Applicative functors

![](present.png)

在这个章节中，我们会学到 applicative functors，也就是加强版的 functors，在 Haskell 中是用在 ``Control.Applicative`` 中的 ``Applicative`` 这个 typeclass 来定义的。

你还记得 Haskell 中函数预设就是 Curried 的，那代表接受多个参数的函数实际上是接受一个参数然后回传一个接受剩余参数的函数，以此类推。如果一个函数的型别是 ``a -> b -> c``，我们通常会说这个函数接受两个参数并回传 ``c``，但他实际上是接受 ``a`` 并回传一个 ``b -> c`` 的函数。这也是为什么我们可以用 ``(f x) y`` 的方式呼叫 ``f x y``。这个机制让我们可以 partially apply 一个函数，可以用比较少的参数呼叫他们。可以做成一个函数再喂给其他函数。

到目前为止，当我们要对 functor map over 一个函数的时候，我们用的函数都是只接受一个参数的。但如果我们要 map 一个接受两个参数的函数呢？我们来看几个具体的例子。如果我们有 ``Just 3`` 然后我们做 ``fmap (*) (Just 3)``，那我们会获得什么样的结果？从 ``Maybe`` 对 ``Functor`` 的 instance 实作来看，我们知道如果他是 ``Just something``，他会对在 ``Just`` 中的 ``something`` 做映射。因此当 ``fmap (*) (Just 3)`` 会得到 ``Just ((*) 3)``，也可以写做 ``Just (* 3)``。我们得到了一个包在 ``Just`` 中的函数。

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

如果我们 map ``compare`` 到一个包含许多字元的 list 呢？他的型别是 ``(Ord a) => a -> a -> Ordering``，我们会得到包含许多 ``Char -> Ordering`` 型别函数的 list，因为 ``compare`` 被 partially apply 到 list 中的字元。他不是包含许多 ``(Ord a) => a -> Ordering`` 的函数，因为第一个 ``a`` 碰到的型别是 ``Char``，所以第二个 ``a`` 也必须是 ``Char``。


我们看到如何用一个多参数的函数来 map functor，我们会得到一个包含了函数的 functor。那现在我们能对这个包含了函数的 functor 做什么呢？我们能用一个吃这些函数的函数来 map over 这个 functor，这些在 functor 中的函数都会被当作参数丢给我们的函数。

```haskell
ghci> let a = fmap (*) [1,2,3,4]  
ghci> :t a  
a :: [Integer -> Integer]  
ghci> fmap (\f -> f 9) a  
[9,18,27,36]  
```

但如果我们的有一个 functor 里面是 ``Just (3 *)`` 还有另一个 functor 里面是 ``Just 5``，但我们想要把第一个 ``Just (3 *)`` map over ``Just 5`` 呢？如果是普通的 functor，那就没救了。因为他们只允许 map 一个普通的函数。即使我们用 ``\f -> f 9`` 来 map 一个装了很多函数的 functor，我们也是使用了普通的函数。我们是无法单纯用 ``fmap`` 来把包在一个 functor 的函数 map 另一个包在 functor 中的值。我们能用模式匹配 ``Just`` 来把函数从里面抽出来，然后再 map ``Just 5``，但我们是希望有一个一般化的作法，对任何 functor 都有效。

我们来看看 ``Applicative`` 这个 typeclass。他位在 ``Control.Applicative`` 中，在其中定义了两个函数 ``pure`` 跟 ``<*>``。他并没有提供预设的实作，如果我们想使用他必须要为他们 applicative functor 的实作。typeclass 定义如下：

```haskell
class (Functor f) => Applicative f where  
    pure :: a -> f a  
    (<*>) :: f (a -> b) -> f a -> f b  
```

这简简单单的三行可以让我们学到不少。首先来看第一行。他开启了 ``Applicative`` 的定义，并加上 class contraint。描述了一个型别构造子要是 ``Applicative``，他必须也是 ``Functor``。这就是为什么我们说一个型别构造子属于 ``Applicative`` 的话，他也会是 ``Functor``，因此我们能对他使用 ``fmap``。

第一个定义的是 ``pure``。他的型别宣告是 ``pure :: a -> f a``。``f`` 代表 applicative functor 的 instance。由于 Haskell 有一个优秀的型别系统，其中函数又是将一些参数映射成结果，我们可以从型别宣告中读出许多讯息。``pure`` 应该要接受一个值，然后回传一个包含那个值的 applicative functor。我们这边是用盒子来作比喻，即使有一些比喻不完全符合现实的情况。尽管这样，``a -> f a`` 仍有许多丰富的资讯，他确实告诉我们他会接受一个值并回传一个 applicative functor，里面装有结果。

对于 ``pure`` 比较好的说法是把一个普通值放到一个预设的 context 下，一个最小的 context 但仍然包含这个值。

``<*>`` 也非常有趣。他的型别是 ``f (a -> b) -> f a -> f b``。这有让你联想到什么吗？没错！就是 ``fmap :: (a -> b) -> f a -> f b``。他有点像加强版的 ``fmap``。然而 ``fmap`` 接受一个函数跟一个 functor，然后套用 functor 之中的函数。``<*>`` 则是接受一个装有函数的 functor 跟另一个 functor，然后取出第一个 functor 中的函数将他对第二个 functor 中的值做 map。

我们来看看 ``Maybe`` 的 ``Applicative`` 实作：

```haskell
instance Applicative Maybe where  
    pure = Just  
    Nothing <*> _ = Nothing  
    (Just f) <*> something = fmap f something  
```

从 class 的定义我们可以看到 ``f`` 作为 applicative functor 会接受一个具体型别当作参数，所以我们是写成 ``instance Applicative Maybe where`` 而不是写成 ``instance Applicative (Maybe a) where``。

首先看到 ``pure``。他只不过是接受一个东西然后包成 applicative functor。我们写成 ``pure = Just`` 是因为 ``Just`` 不过就是一个普通函数。我们其实也可以写成 ``pure x = Just x``。

接着我们定义了 ``<*>``。我们无法从 ``Nothing`` 中抽出一个函数，因为 ``Nothing`` 并不包含一个函数。所以我们说如果我们要尝试从 ``Nothing`` 中取出一个函数，结果必定是 ``Nothing``。如果你看看 ``Applicative`` 的定义，你会看到他有 ``Functor`` 的限制，他代表 ``<*>`` 的两个参数都会是 functors。如果第一个参数不是 ``Nothing``，而是一个装了函数的 ``Just``，而且我们希望将这个函数对第二个参数做 map。这个也考虑到第二个参数是 ``Nothing`` 的情况，因为 ``fmap`` 任何一个函数至 ``Nothing`` 会回传 ``Nothing``。

对于 ``Maybe`` 而言，如果左边是 ``Just``，那 ``<*>`` 会从其中抽出了一个函数来 map 右边的值。如果有任何一个参数是 ``Nothing``。那结果便是 ``Nothing``。

来试试看吧！

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

我们看到 ``pure (+3)`` 跟 ``Just (+3)`` 在这个 case 下是一样的。如果你是在 applicative context 底下跟 ``Maybe`` 打交道的话请用 ``pure``，要不然就用 ``Just``。前四个输入展示了函数是如何被取出并做 map 的动作，但在这个 case 底下，他们同样也可以用 unwrap 函数来 map over functors。最后一行比较有趣，因为我们试着从 ``Nothing`` 取出函数并将他 map 到某个值。结果当然是 ``Nothing``。

对于普通的 functors，你可以用一个函数 map over 一个 functors，但你可能没办法拿到结果。而 applicative functors 则让你可以用单一一个函数操作好几个 functors。看看下面一段程式码：

```haskell
ghci> pure (+) <*> Just 3 <*> Just 5  
Just 8  
ghci> pure (+) <*> Just 3 <*> Nothing  
Nothing  
ghci> pure (+) <*> Nothing <*> Just 5  
Nothing  
```

![](whale.png)

究竟我们写了些什么？我们来一步步看一下。``<*>`` 是 left-associative，也就是说 ``pure (+) <*> Just 3 <*> Just 5`` 可以写成 ``(pure (+) <*> Just 3) <*> Just 5``。首先 ``+`` 是摆在一个 functor 中，在这边刚好他是一个 ``Maybe``。所以首先，我们有 ``pure (+)``，他等价于 ``Just (+)``。接下来由于 partial application 的关系，``Just (+) <*> Just 3`` 等价于 ``Just (3+)``。把一个 ``3`` 喂给 ``+`` 形成另一个只接受一个参数的函数，他的效果等于加上 3。最后 ``Just (3+) <*> Just 5`` 被运算，其结果是 ``Just 8``。


这样很棒吧！用 applicative style 的方式来使用 applicative functors。像是 ``pure f <*> x <*> y <*> ...`` 就让我们可以拿一个接受多个参数的函数，而且这些参数不一定是被包在 functor 中。就这样来套用在多个在 functor context 的值。这个函数可以吃任意多的参数，毕竟 ``<*>`` 只是做 partial application 而已。


如果我们考虑到 ``pure f <*> x`` 等于 ``fmap f x`` 的话，这样的用法就更方便了。这是 applicative laws 的其中一条。我们稍后会更仔细地检视这条定律。现在我们先依直觉来使用他。就像我们先前所说的，``pure`` 把一个值放进一个预设的 context 中。如果我们要把一个函数放在一个预设的 context，然后把他取出并套用在放在另一个 applicative functor 的值。我们会做的事就是把函数 map over 那个 applicative functor。但我们不会写成 ``pure f <*> x <*> y <*> ...``，而是写成 ``fmap f x <*> y <*> ...``。这也是为什么 ``Control.Applicative`` 会 export 一个函数 ``<$>``，他基本上就是中缀版的 ``fmap``。他是这么被定义的：

```haskell
(<$>) :: (Functor f) => (a -> b) -> f a -> f b  
f <$> x = fmap f x  
```
	
    
    要记住型别变数跟参数的名字还有值绑定的名称不冲突。``f`` 在函数的型别宣告中是型别变数，说明 ``f`` 应该要满足 ``Functor`` typeclass 的条件。而在函数本体中的 ``f`` 则表示一个函数，我们将他 map over x。我们同样用 ``f`` 来表示他们并代表他们是相同的东西。


``<$>`` 的使用显示了 applicative style 的好处。如果我们想要将 ``f`` 套用三个 applicative functor。我们可以写成 ``f <$> x <*> y <*> z``。如果参数不是 applicative functor 而是普通值的话。我们则写成 ``f x y z``。

我们再仔细看看他是如何运作的。我们有一个 ``Just "johntra"`` 跟 ``Just "volta"`` 这样的值，我们希望将他们结合成一个 ``String``，并且包含在 ``Maybe`` 中。我们会这样做：

```haskell
ghci> (++) <$> Just "johntra" <*> Just "volta"  
Just "johntravolta"  
```

可以将上面的跟下面这行比较一下：

```haskell
ghci> (++) "johntra" "volta"  
"johntravolta"  
```

可以将一个普通的函数套用在 applicative functor 上真不错。只要稍微写一些 ``<$>`` 跟 ``<*>`` 就可以把函数变成 applicative style，可以操作 applicatives 并回传 applicatives。


总之当我们在做 ``(++) <$> Just "johntra" <*> Just "volta"`` 时，首先我们将 ``(++)`` map over 到 ``Just "johntra"``，然后产生 ``Just ("johntra"++)``，其中 ``(++)`` 的型别为 ``(++) :: [a] -> [a] -> [a]``，``Just ("johntra"++)`` 的型别为 ``Maybe ([Char] -> [Char])``。注意到 ``(++)`` 是如何吃掉第一个参数，以及我们是怎么决定 ``a`` 是 ``Char`` 的。当我们做 ``Just ("johntra"++) <*> Just "volta"``，他接受一个包在 ``Just`` 中的函数，然后 map over ``Just "volta"``，产生了 ``Just "johntravolta"``。如果两个值中有任意一个为 ``Nothing``，那整个结果就会是 ``Nothing``。


到目前为止我们只有用 ``Maybe`` 当作我们的案例，你可能也会想说 applicative functor 差不多就等于 ``Maybe``。不过其实有许多其他 ``Applicative`` 的 instance。我们来看看有哪些。


List 也是 applicative functor。很惊讶吗？来看看我们是怎么定义 ``[]`` 为 ``Applicative`` 的 instance 的。

```haskell
instance Applicative [] where  
    pure x = [x]  
    fs <*> xs = [f x | f <- fs, x <- xs]  
```


早先我们说过 ``pure`` 是把一个值放进预设的 context 中。换种说法就是一个会产生那个值的最小 context。而对 list 而言最小 context 就是 ``[]``，但由于空的 list 并不包含一个值，所以我们没办法把他当作 ``pure``。这也是为什么 ``pure`` 其实是接受一个值然后回传一个包含单元素的 list。同样的，``Maybe`` 的最小 context 是 ``Nothing``，但他其实表示的是没有值。所以 ``pure`` 其实是被实作成 ``Just`` 的。

```haskell
ghci> pure "Hey" :: [String]  
["Hey"]  
ghci> pure "Hey" :: Maybe String  
Just "Hey"  
```

至于 ``<*>`` 呢？如果我们假定 ``<*>`` 的型别是限制在 list 上的话，我们会得到 ``(<*>) :: [a -> b] -> [a] -> [b]``。他是用 list comprehension 来实作的。``<*>`` 必须要从左边的参数取出函数，将他 map over 右边的参数。但左边的 list 有可能不包含任何函数，也可能包含一个函数，甚至是多个函数。而右边的 list 有可能包含多个值。这也是为什么我们用 list comprehension 的方式来从两个 list 取值。我们要对左右任意的组合都做套用的动作。而得到的结果就会是左右两者任意组合的结果。

```haskell
ghci> [(*0),(+100),(^2)] <*> [1,2,3]  
[0,0,0,101,102,103,1,4,9]  
```

左边的 list 包含三个函数，而右边的 list 有三个值。所以结果会是有九个元素的 list。在左边 list 中的每一个函数都被套用到右边的值。如果我们今天在 list 中的函数是接收两个参数的，我们也可以套用到两个 list 上。

```haskell
ghci> [(+),(*)] <*> [1,2] <*> [3,4]  
[4,5,5,6,3,4,6,8]  
```

由于 ``<*>`` 是 left-associative，也就是说 ``[(+),(*)] <*> [1,2]`` 会先运作，产生 ``[(1+),(2+),(1*),(2*)]``。由于左边的每一个函数都套用至右边的每一个值。也就产生 ``[(1+),(2+),(1*),(2*)] <*> [3,4]``，其便是最终结果。

list 的 applicative style 是相当有趣的：

```haskell
ghci> (++) <$> ["ha","heh","hmm"] <*> ["?","!","."]  
["ha?","ha!","ha.","heh?","heh!","heh.","hmm?","hmm!","hmm."] 
```

看看我们是如何将一个接受两个字串参数的函数套用到两个 applicative functor 上的，只要用适当的 applicative 运算子就可以达成。

你可以将 list 看作是一个 non-deterministic 的计算。而对于像 ``100`` 或是 ``"what"`` 这样的值则是 deterministic 的计算，只会有一个结果。而 ``[1,2,3]`` 则可以看作是没有确定究竟是哪一种结果。所以他代表的是所有可能的结果。当你在做 ``(+) <$> [1,2,3] <*> [4,5,6]``，你可以想做是把两个 non-deterministic 的计算做 ``+``，只是他会产生另一个 non-deterministic 的计算，而且结果更加不确定。


Applicative style 对于 list 而言是一个取代 list comprehension 的好方式。在第二章中，我们想要看到 ``[2,5,10]`` 跟 ``[8,10,11]`` 相乘的结果，所以我们这样做：

```haskell
ghci> [ x*y | x <- [2,5,10], y <- [8,10,11]]     
[16,20,22,40,50,55,80,100,110]     
```

我们只是从两个 list 中取出元素，并将一个函数套用在任何元素的组合上。这也可以用 applicative style 的方式来写：

```haskell
ghci> (*) <$> [2,5,10] <*> [8,10,11]  
[16,20,22,40,50,55,80,100,110]  
```

这写法对我来说比较清楚。可以清楚表达我们是要对两个 non-deterministic 的计算做 ``*``。如果我们想要所有相乘大于 50 可能的计算结果，我们会这样写：

```haskell
ghci> filter (>50) $ (*) <$> [2,5,10] <*> [8,10,11]  
[55,80,100,110]  
```

很容易看到 ``pure f <*> xs`` 等价于 ``fmap f xs``。而 ``pure f`` 就是 ``[f]``，而且 ``[f] <*> xs`` 可将左边的每个函数套用至右边的每个值。但左边其实只有一个函数，所以他做起来就像是 mapping。

另一个我们已经看过的 ``Applicative`` 的 instance 是 ``IO``，来看看他是怎么实作的：

```haskell
instance Applicative IO where  
    pure = return  
    a <*> b = do  
        f <- a  
        x <- b  
        return (f x)  
```

![](knight.png)

由于 ``pure`` 是把一个值放进最小的 context 中，所以将 ``return`` 定义成 ``pure`` 是很合理的。因为 ``return`` 也是做同样的事情。他做了一个不做任何事情的 I/O action，他可以产生某些值来作为结果，但他实际上并没有做任何 I/O 的动作，例如说印出结果到终端或是档案。

如果 ``<*>`` 被限定在 ``IO`` 上操作的话，他的型别会是 ``(<*>) :: IO (a -> b) -> IO a -> IO b``。他接受一个产生函数的 I/O action，还有另一个 I/O action，并从以上两者创造一个新的 I/O action，也就是把第二个参数喂给第一个参数。而得到回传的结果，然后放到新的 I/O action 中。我们用 do 的语法来实作他。你还记得的话 do 就是把好几个 I/O action 黏在一起，变成一个大的 I/O action。

而对于 ``Maybe`` 跟 ``[]`` 而言，我们可以把 ``<*>`` 想做是从左边的参数取出一个函数，然后套用到右边的参数上。至于 ``IO``，这种取出的类比方式仍然适用，但我们必须多加一个 sequencing 的概念，因为我们是从两个 I/O action 中取值，也是在 sequencing，把他们黏成一个。我们从第一个 I/O action 中取值，但要取出 I/O action 的结果，他必须要先被执行过。

考虑下面这个范例：

```haskell
myAction :: IO String  
myAction = do  
    a <- getLine  
    b <- getLine  
    return $ a ++ b  
```


这是一个提示使用者输入两行并产生将两行输入串接在一起结果的一个 I/O action。我们先把两个 ``getLine`` 黏在一起，然后用一个 ``return``，这是因为我们想要这个黏成的 I/O action 包含 ``a ++ b`` 的结果。我们也可以用 applicative style 的方式来描述：

```haskell
myAction :: IO String  
myAction = (++) <$> getLine <*> getLine  
```

我们先前的作法是将两个 I/O action 的结果喂给函数。还记得 ``getLine`` 的型别是 ``getLine :: IO String``。当我们对 applicative functor 使用 ``<*>`` 的时候，结果也会是 applicative functor。

如果我们再使用盒子的类比，我们可以把 ``getLine`` 想做是一个去真实世界中拿取字串的盒子。而 ``(++) <$> getLine <*> getLine`` 会创造一个比较大的盒子，这个大盒子会派两个盒子去终端拿取字串，并把结果串接起来放进自己的盒子中。

``(++) <$> getLine <*> getLine`` 的型别是 ``IO String``，他代表这个表达式式一个再普通不过的 I/O action，他里面也装着某种值。这也是为什么我们可以这样写：

```haskell
main = do  
    a <- (++) <$> getLine <*> getLine  
    putStrLn $ "The two lines concatenated turn out to be: " ++ a  
```

如果你发现你是在做 binding I/O action 的动作，而且在 binding 之后还呼叫一些函数，最后用 ``return`` 来将结果包起来。
那你可以考虑使用 applicative style，这样可以更简洁。

另一个 ``Applicative`` 的 instance 是 ``(->) r``。虽然他们通常是用在 code golf 的情况，但他们还是十分有趣的例子。所以我们还是来看一下他们是怎么被实作的。

	如果你忘记 ``(->) r`` 的意思，回去翻翻前一章节我们介绍 ``(->) r`` 作为一个 functor 的范例。

```haskell
instance Applicative ((->) r) where  
    pure x = (\_ -> x)  
    f <*> g = \x -> f x (g x)  
```

当我们用 ``pure`` 将一个值包成 applicative functor 的时候，他产生的结果永远都会是那个值。也就是最小的 context。那也是为什么对于 function 的 ``pure`` 实作来讲，他就是接受一个值，然后造一个函数永远回传那个值，不管他被喂了什么参数。如果你限定 ``pure`` 的型别至 ``(->) r`` 上，他就会是 ``pure :: a -> (r -> a)``。

```haskell
ghci> (pure 3) "blah"  
3  
```

由于 currying 的关系，函数套用是 left-associative，所以我们忽略掉括弧。

```haskell
ghci> pure 3 "blah"  
3  
```

而 ``<*>`` 的实作是比较不容易了解的，我们最好看一下怎么用 applicative style 的方式来使用作为 applicative functor 的 function。

```haskell
ghci> :t (+) <$> (+3) <*> (*100)  
(+) <$> (+3) <*> (*100) :: (Num a) => a -> a  
ghci> (+) <$> (+3) <*> (*100) $ 5  
508 
```

将两个 applicative functor 喂给 ``<*>`` 可以产生一个新的 applicative functor，所以如果我们丢给他两个函数，我们能得到一个新的函数。所以是怎么一回事呢？当我们做 ``(+) <$> (+3) <*> (*100)``，我们是在实作一个函数，他会将 ``(+3)`` 跟 ``(*100)`` 的结果再套用 ``+``。要看一个实际的范例的话，可以看一下 ``(+) <$> (+3) <*> (*100) $ 5`` 首先 ``5`` 被丢给 ``(+3)`` 跟 ``(*100)``，产生 ``8`` 跟 ``500``。然后 ``+`` 被套用到 ``8`` 跟 ``500``，得到 ``508``。


```haskell
ghci> (\x y z -> [x,y,z]) <$> (+3) <*> (*2) <*> (/2) $ 5  
[8.0,10.0,2.5]  
```

![](jazzb.png)

这边也一样。我们创建了一个函数，他会呼叫 ``\x y z -> [x,y,z]``，而丢的参数是 ``(+3)``, ``(*2)`` 跟 ``(/2)``。``5`` 被丢给以上三个函数，然后他们结果又接到 `` \x y z -> [x, y, z]``。


你可以将函数想做是装着最终结果的盒子，所以 ``k <$> f <*> g`` 会制造一个函数，他会将 ``f`` 跟 ``g`` 的结果丢给 ``k``。当我们做 ``(+) <$> Just 3 <*> Just 5``，我们是用 ``+`` 套用在一些可能有或可能没有的值上，所以结果也会是可能有或没有。当我们做 ``(+) <$> (+10) <*> (+5)``，我们是将 ``+`` 套用在 ``(+10)`` 跟 ``(+5)`` 的结果上，而结果也会是一个函数，当被喂给一个参数的时候会产生结果。


我们通常不会将函数当作 applicative 用，不过仍然值得当作练习。对于 ``(->) r`` 怎么定义成 ``Applicative`` 的并不是真的那么重要，所以如果你不是很懂的话也没关系。这只是让你获得一些操作上的直觉罢了。


一个我们之前还没碰过的 ``Applicative`` 的 instance 是 ``ZipList``，他是包含在 ``Control.Applicative`` 中。


对于 list 要作为一个 applicative functor 可以有多种方式。我们已经介绍过其中一种。如果套用 ``<*>``，左边是许多函数，而右边是许多值，那结果会是函数套用到值的所有组合。如果我们做 ``[(+3),(*2)] <*> [1,2]``。那 ``(+3)`` 会先套用至 ``1`` 跟 ``2``。接着 ``(*2)`` 套用至 ``1`` 跟 ``2``。而得到 ``[4,5,2,4]``。


然而 ``[(+3),(*2)] <*> [1,2]`` 也可以这样运作:把左边第一个函数套用至右边第一个值，接着左边第二个函数套用右边第二个值，以此类推。这样得到的会是 ``[4,4]``。或是 ``[1 + 3, 2 * 2]``。


由于一个型别不能对同一个 typeclass 定义两个 instance，所以才会定义了 ``ZipList a``，他只有一个构造子 ``ZipList``，他只包含一个栏位，他的型别是 list。

```haskell
instance Applicative ZipList where  
        pure x = ZipList (repeat x)  
        ZipList fs <*> ZipList xs = ZipList (zipWith (\f x -> f x) fs xs)  
```

``<*>`` 做的就是我们之前说的。他将第一个函数套用至第一个值，第二个函数套用第二个值。这也是 ``zipWith (\f x -> f x) fs xs`` 做的事。由于 ``zipWith`` 的特性，所以结果会跟 list 中比较短的那个一样长。


``pure`` 也值得我们讨论一下。他接受一个值，把他重复地放进一个 list 中。``pure "haha"`` 就会是 ``ZipList (["haha","haha","haha"...``。这可能会造成些混淆，毕竟我们说过 ``pure`` 是把一个值放进一个最小的 context 中。而你会想说无限长的 list 不可能会是一个最小的 context。但对于 zip list 来说这是很合理的，因为他必须在 list 的每个位置都有值。这也遵守了 ``pure f <*> xs`` 必须要等价于 ``fmap f xs`` 的特性。如果 ``pure 3`` 只是回传 ``ZipList [3]``，那 ``pure (*2) <*> ZipList [1,5,10]`` 就只会算出 ``ZipList [2]``，因为两个 zip list 算出结果的长度会是比较短的那个的长度。如果我们 zip 一个有限长的 list 以及一个无限长的 list，那结果的长会是有限长的 list 的长度。


那 zip list 是怎么用 applicative style 操作的呢？我们来看看，``ZipList a`` 型别并没有定义成 ``Show`` 的 instance，所以我们必须用 ``getZipList`` 函数来从 zip list 取出一个普通的 list。


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

    ``(,,)`` 函数跟 ``\x y z -> (x,y,z)`` 是等价的，而 ``(,)`` 跟 ``\x y -> (x,y)`` 是等价的。

除了 ``zipWith``，标准函式库中也有 ``zipWith3``, ``zipWith4`` 之类的函数，最多支援到 7。``zipWith`` 接受一个接受两个参数的函数，并把两个 list zip 起来。``zipWith3`` 则接受一个接受三个参数的函数，然后把三个 list zip 起来。以此类推。用 applicative style 的方式来操作 zip list 的话，我们就不需要对每个数量的 list 都定义一个独立的 zip 函数来 zip 他们。我们只需要用 applicative style 的方式来把任意数量的 list zip 起来就可以了。


``Control.Applicative`` 定义了一个函数叫做 ``liftA2``，他的型别是 ``liftA2 :: (Applicative f) => (a -> b -> c) -> f a -> f b -> f c``。他定义如下：


```haskell
liftA2 :: (Applicative f) => (a -> b -> c) -> f a -> f b -> f c  
liftA2 f a b = f <$> a <*> b  
```

并没有太难理解的东西，他不过就是对两个 applicatives 套用函数而已，而不用我们刚刚熟悉的 applicative style。我们提及他的理由只是要展示为什么 applicative functors 比起一般的普通 functor 要强。如果只是普通的 functor 的话，我们只能将一个函数 map over 这个 functor。但有了 applicative functor，我们可以对好多个 functor 套用一个函数。看看这个函数的型别，他会是 ``(a -> b -> c) -> (f a -> f b -> f c)``。当我们从这样的角度来看他的话，我们可以说 ``liftA2`` 接受一个普通的二元函数，并将他升级成一个函数可以运作在两个 functor 之上。

另外一个有趣的概念是，我们可以接受两个 applicative functor 并把他们结合成一个 applicative functor，这个新的将这两个 applicative functor 装在 list 中。举例来说，我们现在有 ``Just 3`` 跟 ``Just 4``。我们假设后者是一个只包含单元素的 list。

```haskell
ghci> fmap (\x -> [x]) (Just 4)  
Just [4]  
```

所以假设我们有 ``Just 3`` 跟 ``Just [4]``。我们有怎么得到 ``Just [3,4]`` 呢？很简单。

```haskell
ghci> liftA2 (:) (Just 3) (Just [4])  
Just [3,4]  
ghci> (:) <$> Just 3 <*> Just [4]  
Just [3,4]  
```

还记得 ``:`` 是一个函数，他接受一个元素跟一个 list，并回传一个新的 list，其中那个元素已经接在前面。现在我们有了 ``Just [3,4]``，我们能够将他跟 ``Just 2`` 绑在一起变成 ``Just [2,3,4]`` 吗？当然可以。我们可以将任意数量的 applicative 绑在一起变成一个 applicative，里面包含一个装有结果的 list。我们试着实作一个函数，他接受一串装有 applicative 的 list，然后回传一个 applicative 里面有一个装有结果的 list。我们称呼他为 ``sequenceA``。

```haskell
sequenceA :: (Applicative f) => [f a] -> f [a]  
sequenceA [] = pure []  
sequenceA (x:xs) = (:) <$> x <*> sequenceA xs  
```

居然用到了递回！首先我们来看一下他的型别。他将一串 applicative 的 list 转换成一个 applicative 装有一个 list。从这个资讯我们可以推测出边界条件。如果我们要将一个空的 list 变成一个装有 list 的 applicative。我们只要把这个空的 list 放进一个预设的 context。现在来看一下我们怎么用递回的。如果们有一个可以分成头跟尾的 list（``x`` 是一个 applicative 而 ``xs`` 是一串 applicatve），我们可以对尾巴呼叫 ``sequenceA``，便会得到一个装有 list 的 applicative。然后我们只要将在 ``x`` 中的值把他接到装有 list 的 applicative 前面就可以了。


所以如果我们做 ``sequenceA [Just 1, Just 2]``，也就是 ``(:) <$> Just 1 <*> sequenceA [Just 2]``。那会等价于 ``(:) <$> Just 1 <*> ((:) <$> Just 2 <*> sequenceA [])``。我们知道 ``sequenceA []`` 算出来会是 ``Just []``，所以运算式就变成 ``(:) <$> Just 1 <*> ((:) <$> Just 2 <*> Just [])``，也就是 ``(:) <$> Just 1 <*> Just [2]``，算出来就是 ``Just [1,2]``。


另一种实作 ``sequenceA`` 的方式是用 fold。要记得几乎任何需要走遍整个 list 并 accumulate 成一个结果的都可以用 fold 来实作。

```haskell
sequenceA :: (Applicative f) => [f a] -> f [a]  
sequenceA = foldr (liftA2 (:)) (pure [])  
```

我们从右往左走，并且起始的 accumulator 是用 ``pure []``。我们是用 ``liftA2 (:)`` 来结合 accumulator 跟 list 中最后的元素，而得到一个 applicative，里面装有一个单一元素的一个 list。然后我们再用 ``liftA2 (:)`` 来结合 accumulator 跟最后一个元素，直到我们只剩下 accumulator 为止，而得到一个 applicative，里面装有所有结果。


我们来试试看套用在不同 applicative 上。

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

很酷吧。当我们套用在 ``Maybe`` 上时，``sequenceA`` 创造一个新的 ``Maybe``，他包含了一个 list 装有所有结果。如果其中一个值是 ``Nothing``，那整个结果就会是 ``Nothing``。如果你有一串 ``Maybe`` 型别的值，但你只在乎当结果不包含任何 ``Nothing`` 的情况，这样的特性就很方便。


当套用在函数时，``sequenceA`` 接受装有一堆函数的 list，并回传一个回传 list 的函数。在我们的范例中，我们写了一个函数，他只接受一个数值作为参数，他会把他套用至 list 中的每一个函数，并回传一个包含结果的 list。``sequenceA [(+3),(+2),(+1)] 3`` 会将 ``3`` 喂给 ``(+3)``, ``(+2)`` 跟 ``(+1)``，然后将所有结果装在一个 list 中。


而 ``(+) <$> (+3) <*> (*2)`` 会创见一个接受单一参数的一函数，将他同时喂给 ``(+3)`` 跟 ``(*2)``，然后呼叫 ``+`` 来将两者加起来。同样的道理，``sequenceA [(+3),(*2)]`` 是制造一个接受单一参数的函数，他会将他喂给所有包含在 list 中的函数。但他最后不是呼叫 ``+``，而是呼叫 ``:`` 跟 ``pure []`` 来把结果接成一个 list，得到最后的结果。


当我们有一串函数，我们想要将相同的输入都喂给他们并检视结果的时候，``sequenceA`` 非常好用。例如说，我们手上有一个数值，但不知道他是否满足一串 predicate。一种实作的方式是像这样：

```haskell
ghci> map (\f -> f 7) [(>4),(<10),odd]  
[True,True,True]  
ghci> and $ map (\f -> f 7) [(>4),(<10),odd]  
True  
```


记住 ``and`` 接受一串布林值，并只有在全部都是 ``True`` 的时候才回传 ``True``。
另一种实作方式是用 ``sequenceA``：


```haskell
ghci> sequenceA [(>4),(<10),odd] 7  
[True,True,True]  
ghci> and $ sequenceA [(>4),(<10),odd] 7  
True  
```

``sequenceA [(>4),(<10),odd]`` 接受一个函数，他接受一个数值并将他喂给所有的 predicate，包含 ``[(>4),(<10),odd]``。然后回传一串布林值。他将一个型别为 ``(Num a) => [a -> Bool]`` 的 list 变成一个型别为 ``(Num a) => a -> [Bool]`` 的函数，很酷吧。


由于 list 要求里面元素的型别要一致，所以包含在 list 中的所有函数都是同样型别。你不能创造一个像是 ``[ord, (+3)]`` 这样的 list，因为 ``ord`` 接受一个字元并回传一个数值，然而 ``(+3)`` 接受一个数值并回传一个数值。


当跟 ``[]`` 一起使用的时候，``sequenceA`` 接受一串 list，并回传另一串 list。他实际上是创建一个包含所有可能组合的 list。为了方便说明，我们比较一下使用 ``sequenceA`` 跟 list comprehension 的差异：

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

这可能有点难以理解，但如果你多做点尝试，你会比较能看出来些眉目。假设我们在做 ``sequenceA [[1,2],[3,4]]``。要知道这是怎么回事，我们首先用 ``sequenceA`` 的定义 ``sequenceA (x:xs) = (:) <$> x <*> sequenceA xs`` 还有边界条件 ``sequenceA [] = pure []`` 来看看。你不需要实际计算，但他可以帮助你理解 ``sequenceA`` 是怎么运作在一串 list 上，毕竟这有点复杂。

    # 我们从 ``sequenceA [[1,2],[3,4]]`` 开始
    # 那可以被计算成 ``(:) <$> [1,2] <*> sequenceA [[3,4]]``
    # 计算内层的 ``sequenceA``，会得到 ``(:) <$> [1,2] <*> ((:) <$> [3,4] <*> sequenceA [])``
    # 我们碰到了边界条件，所以会是 ``(:) <$> [1,2] <*> ((:) <$> [3,4] <*> [[]])``
    # 现在我们计算 ``(:) <$> [3,4] <*> [[]] `` 的部份，我们会对左边 list 中的每一个值 (也就是 ``3`` 跟 ``4``) 跟右边的每一个值 (只有 ``[]``)套用 ``:``，而得到 ``[3:[], 4:[]]``，也就是 ``[[3],[4]]``。所以我们有 ``(:) <$> [1,2] <*> [[3],[4]]``
    # 而对于左边的每一个值(``1`` 跟 ``2``)以及右边可能的值（``[3]`` 跟 ``[4]``）我们套用 ``:`` 而得到 ``[1:[3], 1:[4], 2:[3], 2:[4]]``，他等于 ``[[1,3],[1,4],[2,3],[2,4]]``


计算 ``(+) <$> [1,2] <*> [4,5,6]`` 会得到一个 non-deterministic 的结果 ``x + y``，其中 ``x`` 代表 ``[1,2]`` 中的每一个值，而 ``y`` 代表 ``[4,5,6]`` 中的每一个值。我们用 list 来表示每一种可能的情形。同样的，当我们在做 ``sequence [[1,2],[3,4],[5,6],[7,8]]``，他的结果会是 non-deterministic 的 ``[x,y,z,w]``，其中 ``x`` 代表 ``[1,2]`` 中的每一个值，而 ``y`` 代表 ``[3,4]`` 中的每一个值。以此类推。我们用 list 代表 non-deterministic 的计算，每一个元素都是一个可能的情形。这也是为什么会用到 list of list。


当使用在 I/O action 上的时候，``sequenceA`` 跟 ``sequence`` 是等价的。他接受一串 I/O action 并回传一个 I/O action，这个 I/O action 会计算 list 中的每一个 I/O action，并把结果放在一个 list 中。要将型别为 ``[IO a]`` 的值转换成 ``IO [a]`` 的值，也就是会产生一串 list 的一个 I/O action，那这些 I/O action 必须要一个一个地被计算，毕竟对于这些 I/O action 你没办法不计算就得到结果。

```haskell
ghci> sequenceA [getLine, getLine, getLine]  
heyh  
ho  
woo  
["heyh","ho","woo"]  
```


就像普通的函数一样，applicative functors 也遵循一些定律。其中最重要的一个是我们之前提过的 ``pure f <*> x = fmap f x``。你可以证明一些我们之前介绍过的 applicative functor 遵守这个定律当作练习。其他的 functors law 有：

    # ``pure id <*> v = v``
    # ``pure (.) <*> u <*> v <*> w = u <*> (v <*> w)``
    # ``pure f <*> pure x = pure (f x)``
    # ``u <*> pure y = pure ($ y) <*> u``

我们不会一项一项地细看，那样会花费很大的篇幅而且对读者来说很无聊，但如果你有兴趣，你可以针对某些 instance 看看他们会不会遵守。


结论就是 applicative functor 不只是有趣而且实用， 他允许我们结合不同种类的计算，像是 I/O 计算，non-deterministic 的计算，有可能失败的计算等等。而使用 ``<$>`` 跟 ``<*>`` 我们可以将普通的函数来运作在任意数量的 applicative functors 上。



## 关键字"newtype"

![](maoi.png)

到目前为止，我们已经看过了如何用 ``data`` 关键字定义自己的 algebraic data type。我们也学习到了如何用 ``type`` 来定义 type synonyms。在这个章节中，我们会看一下如何使用 ``newtype`` 来从一个现有的型别中定义出新的型别，并说明我们为什么会想要那么做。


在之前的章节中，我们了解到其实 list 有很多种方式可以被视为一种 applicative functor。一中方式是定义 ``<*>`` 将左边的每一个值跟右边的每一个值组合，而得到各种组合的结果。


```haskell
ghci> [(+1),(*100),(*5)] <*> [1,2,3]  
[2,3,4,100,200,300,5,10,15]  
```

第二种方式是将 ``<*>`` 定义成将左边的第一个函数套用至右边的第一个值，然后将左边第二个函数套用至右边第二个值。以此类推。最终，这表现得有点像将两个 list 用一个拉链拉起来一样。但由于 list 已经被定义成 ``Applicaitive`` 的 instance 了，所以我们要怎么要让 list 可以被定义成第二种方式呢？如果你还记得我们说过我们是有很好的理由定义了 ``ZipList a``，其中他里面只包含一个值构造子跟只包含一个栏位。其实他的理由就是要让 ``ZipList`` 定义成用拉链的方式来表现 applicative 行为。我们只不过用 ``ZipList`` 这个构造子将他包起来，然后用 ``getZipList`` 来解开来。


```haskell
ghci> getZipList $ ZipList [(+1),(*100),(*5)] <*> ZipList [1,2,3]  
[2,200,15]  
```

所以这跟 newtype 这个关键字有什么关系呢？想想看我们是怎么宣告我们的 ``ZipList a`` 的，一种方式是像这样：


```haskell
data ZipList a = ZipList [a]      
```

也就是一个只有一个值构造子的型别而且那个构造子里面只有一个栏位。我们也可以用 record syntax 来定义一个解开的函数：

```haskell
data ZipList a = ZipList { getZipList :: [a] }      
```

这样听起来不错。这样我们就有两种方式来让一个型别来表现一个 typeclass，我们可以用 ``data`` 关键字来把一个型别包在另一个里面，然后再将他定义成第二种表现方式。

而在 Haskell 中 ``newtype`` 正是为了这种情形，我们想将一个型别包在另一个型别中。在实际的函式库中 ``ZipList a`` 是这样定义了：

```haskell
newtype ZipList a = ZipList { getZipList :: [a] }      
```

这边我们不用 ``data`` 关键字反而是用 ``newtype`` 关键字。这是为什么呢？第一个理由是 ``newtype`` 比较快速。如果你用 ``data`` 关键字来包一个型别的话，在你执行的时候会有一些包起来跟解开来的成本。但如果你用 ``newtype`` 的话，Haskell 会知道你只是要将一个现有的型别包成一个新的型别，你想要内部运作完全一样但只是要一个全新的型别而已。有了这个概念，Haskell 可以将包裹跟解开来的成本都去除掉。


那为什么我们不是一直使用 ``newtype`` 呢？当你用 ``newtype`` 来制作一个新的型别时，你只能定义单一一个值构造子，而且那个构造子只能有一个栏位。但使用 ``data`` 的话，你可以让那个型别有好几个值构造子，并且每个构造子可以有零个或多个栏位。

```haskell
data Profession = Fighter | Archer | Accountant  
  
data Race = Human | Elf | Orc | Goblin  

data PlayerCharacter = PlayerCharacter Race Profession  
```

当使用 ``newtype`` 的时候，你是被限制只能用一个值构造子跟单一栏位。


对于 ``newtype`` 我们也能使用 ``deriving`` 关键字。我们可以 derive 像是 ``Eq``, ``Ord``, ``Enum``, ``Bounded``, ``Show`` 跟 ``Read`` 的 instance。如果我们想要对新的型别做 derive，那原本的型别必须已经在那个 typeclass 中。这样很合理，毕竟 ``newtype`` 就是要将现有的型别包起来。如果我们按照下面的方式定义的话，我们就能对我们的型别做印出以及比较相等性的操作：


```haskell
newtype CharList = CharList { getCharList :: [Char] } deriving (Eq, Show)      
```

我们来跑跑看：

```haskell
ghci> CharList "this will be shown!"  
CharList {getCharList = "this will be shown!"}  
ghci> CharList "benny" == CharList "benny"  
True  
ghci> CharList "benny" == CharList "oisters"  
False  
```

对于这个 ``newtype``，他的值构造子有下列型别：

```haskell
CharList :: [Char] -> CharList      
```

他接受一个 ``[Char]`` 的值，例如 ``"my sharona"`` 并回传一个 ``CharList`` 的值。从上面我们使用 ``CharList`` 的值构造子的范例中，我们可以看到的确是这样。相反地，``getCharList`` 具有下列的型别。

```haskell
getCharList :: CharList -> [Char]      
```

他接受一个 ``CharList`` 的值并将他转成 ``[Char]``。你可以将这个想成包装跟解开的动作，但你也可以将他想成从一个型别转成另一个型别。


### Using newtype to make type class instances

有好几次我们想要让我们的型别属于某个 typeclass，但型别变数并没有符合我们想要的。要把 ``Maybe`` 定义成 ``Functor`` 的 instance 很容易，因为 ``Functor`` 这个 typeclass 被定义如下：

```haskell
class Functor f where  
    fmap :: (a -> b) -> f a -> f b  
```

我们先定义如下：

```haskell
instance Functor Maybe where       
```

然后我们实作 ``fmap``。当所有的型别变数被填上时，由于 ``Maybe`` 取代了 ``Functor`` 中 ``f`` 的位置，所以如果我们看看 ``fmap`` 运作在 ``Maybe`` 上时是什么样，他会像这样：


```haskell
fmap :: (a -> b) -> Maybe a -> Maybe b      
```

![](shamrock.png)

看起来不错吧？现在我们想要 tuple 成为 ``Functor`` 的一个 instance，所以当我们用 ``fmap`` 来 map over 一个 tuple 时，他会先套用到 tuple 中的第一个元素。这样当我们做 ``fmap (+3) (1,1)`` 会得到 ``(4,1)``。不过要定义出这样的 instance 有些困难。对于 ``Maybe``，我们只要写 ``instance Functor Maybe where``，这是因为对于只吃一个参数的型别构造子我们很容易定义成 ``Functor`` 的 instance。但对于 ``(a,b)`` 这样的就没办法。要绕过这样的困境，我们可以用 ``newtype`` 来重新定义我们的 tuple，这样第二个型别参数就代表了 tuple 中的第一个元素部份。

```haskell
newtype Pair b a = Pair { getPair :: (a,b) }      
```

现在我们可以将他定义成 ``Functor`` 的 instance，所以函数被 map over tuple 中的第一个部份。

```haskell
instance Functor (Pair c) where  
    fmap f (Pair (x,y)) = Pair (f x, y)  
```

正如你看到的，我们可以对 newtype 定义的型别做模式匹配。我们用模式匹配来拿到底层的 tuple，然后我们将 ``f`` 来套用至 tuple 的第一个部份，然后我们用 ``Pair`` 这个值构造子来将 tuple 转换成 ``Pair b a``。如果我们问 ``fmap`` 的型别究竟是什么，他会是：

```haskell
fmap :: (a -> b) -> Pair c a -> Pair c b      
```

我们说过 ``instance Functor (Pair c) where`` 跟 ``Pair c`` 取代了 ``Functor`` 中 ``f`` 的位置：

```haskell
class Functor f where  
    fmap :: (a -> b) -> f a -> f b  
```

如果我们将一个 tuple 转换成 ``Pair b a``，我们可以用 ``fmap`` 来 map over 第一个部份。

```haskell
ghci> getPair $ fmap (*100) (Pair (2,3))  
(200,3)  
ghci> getPair $ fmap reverse (Pair ("london calling", 3))  
("gnillac nodnol",3)  
```

### On newtype laziness

我们提到 ``newtype`` 一般来讲比 ``data`` 来得有效率。``newtype`` 能做的唯一一件事就是将现有的型别包成新的型别。这样 Haskell 在内部就能将新的型别的值用旧的方式来操作。只是要记住他们还是不同的型别。这代表 ``newtype`` 并不只是有效率，他也具备 lazy 的特性。我们来说明一下这是什么意思。


就像我们之前说得，Haskell 预设是具备 lazy 的特性，这代表只有当我们要将函数的结果印出来的时候计算才会发生。或者说，只有当我们真的需要结果的时候计算才会发生。在 Haskell 中 ``undefined`` 代表会造成错误的计算。如果我们试着计算他，也就是将他印到终端中，Haskell 会丢出错误。

```haskell
ghci> undefined  
*** Exception: Prelude.undefined  
```

然而，如果我们做一个 list，其中包含一些 ``undefined`` 的值，但却要求一个不是 ``undefined`` 的 head，那一切都会顺利地被计算，因为 Haskell 并不需要 list 中其他元素来得到结果。我们仅仅需要看到第一个元素而已。

```haskell
ghci> head [3,4,5,undefined,2,undefined]  
3  
```

现在们考虑下面的型别：

```haskell
data CoolBool = CoolBool { getCoolBool :: Bool }      
```

这是一个用 ``data`` 关键字定义的 algebraic data type。他有一个值建构子并只有一个型别为 ``Bool`` 的栏位。我们写一个函数来对 ``CoolBool`` 做模式匹配，并回传一个 ``"hello"`` 的值。他并不会管 ``CoolBool`` 中装的究竟是 ``True`` 或 ``False``。

```haskell
helloMe :: CoolBool -> String  
helloMe (CoolBool _) = "hello"  
```

这次我们不喂给这个函数一个普通的 ``CoolBool``，而是丢给他一个 ``undefined``。

``` 
ghci> helloMe undefined  
"*** Exception: Prelude.undefined  "
```

结果收到了一个 Exception。是什么造成这个 Exception 的呢？用 ``data`` 定义的型别可以有好几个值构造子（尽管 ``CoolBool`` 只有一个）所以当我们要看看喂给函数的值是否是 ``(CoolBool _)`` 的形式，Haskell 会需要做一些基本的计算来看看是哪个值构造子被用到。但当我们计算 ``undefined`` 的时候，就算是一点也会丢出 Exception。


我们不用 ``data`` 来定义 ``CoolBool`` 而用 ``newtype``：

```haskell
newtype CoolBool = CoolBool { getCoolBool :: Bool }      
```

我们不用修改 ``helloMe`` 函数，因为对于模式匹配使用 ``newtype`` 或 ``data`` 都是一样。我们再来将 ``undefined`` 喂给 ``helloMe``。

```haskell
ghci> helloMe undefined  
"hello" 
```

居然正常运作！为什么呢？正如我们说过得，当我们使用 ``newtype`` 的时候，Haskell 内部可以将新的型别用旧的型别来表示。他不必加入另一层 box 来包住旧有的型别。他只要注意他是不同的型别就好了。而且 Haskell 会知道 ``newtype`` 定义的型别一定只会有一个构造子，他不必计算喂给函数的值就能确定他是 ``(CoolBool _)`` 的形式，因为 ``newtype`` 只有一个可能的值跟单一栏位！


这样行为的差异可能没什么关系，但实际上他非常重要。因为他让我们认知到尽管从撰写程式的观点来看没什么差异，但他们的确是两种不同的机制。尽管 ``data`` 可以让你从无到有定义型别，``newtype`` 是从一个现有的型别做出来的。对 ``newtype`` 做模式匹配并不是像从盒子中取出东西，他比较像是将一个型别转换成另一个型别。


### type vs newtype vs data

到目前为止，你也许对于 ``type``,``data`` 跟 ``newtype`` 之间的差异还不是很了解，让我们快速复习一遍。


``type`` 关键字是让我们定义 type synonyms。他代表我们只是要给一个现有的型别另一个名字，假设我们这样做：

```haskell
type IntList = [Int]      
```

这样做可以允许我们用 ``IntList`` 的名称来指称 ``[Int]``。我们可以交换地使用他们。但我们并不会因此有一个 ``IntList`` 的值构造子。因为 ``[Int]`` 跟 ``IntList`` 只是两种指称同一个型别的方式。我们在指称的时候用哪一个并无所谓。

```haskell
ghci> ([1,2,3] :: IntList) ++ ([1,2,3] :: [Int])  
[1,2,3,1,2,3]  
```

当我们想要让 type signature 更清楚一些，给予我们更了解函数的 context 的时候，我们会定义 type synonyms。举例来说，当我们用一个型别为 ``[(String,String)]`` 的 association list 来代表一个电话簿的时候，我们可以定义一个 ``PhoneBook`` 的 type synonym，这样 type signature 会比较容易读。

 
``newtype`` 关键字将现有的型别包成一个新的型别，大部分是为了要让他们可以是特定 typeclass 的 instance 而这样做。当我们使用 ``newtype`` 来包裹一个现有的型别时，这个型别跟原有的型别是分开的。如果我们将下面的型别用 ``newtype`` 定义：

```haskell
newtype CharList = CharList { getCharList :: [Char] }      
```

我们不能用 ``++`` 来将 ``CharList`` 跟 ``[Char]`` 接在一起。我们也不能用 ``++`` 来将两个 ``CharList`` 接在一起，因为 ``++`` 只能套用在 list 上，而 ``CharList`` 并不是 list，尽管你会说他包含一个 list。但我们可以将两个 ``CharList`` 转成 list，将他们 ``++`` 然后再转回 ``CharList``。


当我们在 ``newtype`` 宣告中使用 record syntax 的时候，我们会得到将新的型别转成旧的型别的函数，也就是我们 ``newtype`` 的值构造子，以及一个函数将他的栏位取出。新的型别并不会被自动定义成原有型别所属的 typeclass 的一个 instance，所以我们必须自己来 derive 他们。


实际上你可以将 ``newtype`` 想成是只能定义一个构造子跟一个栏位的 ``data`` 宣告。如果你碰到这种情形，可以考虑使用 ``newtype``。


使用 ``data`` 关键字是为了定义自己的型别。他们可以在 algebraic data type 中放任意数量的构造子跟栏位。可以定义的东西从 list, ``Maybe`` 到 tree。

如果你只是希望你的 type signature 看起来比较干净，你可以只需要 type synonym。如果你想要将现有的型别包起来并定义成一个 type class 的 instance，你可以尝试使用 newtype。如果你想要定义完全新的型别，那你应该使用 ``data`` 关键字。



## Monoids


Haskell 中 typeclass 是用来表示一个型别之间共有的行为，是一种 interface。我们介绍过 ``Eq``，他定义型别是否可以比较相等性，以及 ``Ord``，他表示可以被排序的型别。还介绍了更有趣的像是 ``Functor`` 跟 ``Applicative``。


当我们定义一个型别时，我们会想说他应该要支援的行为。也就是表现的行为是什么，并且要让他属于哪些 typeclass。如果希望他可以比较相等与否，那我们就应该定义他成为 ``Eq`` 的一个 instance。如果我们想要看看型别是否是一种 functor，我们可以定义他是 ``Functor`` 的一个 instance。以此类推。


考虑 ``*`` 是一个将两个数值相乘的一个函数。如果我们将一个数值乘上 ``1``，那就会得到自身的数值。我们实际上是做 ``1 * x`` 或 ``x * 1`` 并没有差别。结果永远会是 ``x``。同样的，``++`` 是一个接受两个参数并回传新的值的一个函数。只是他不是相乘而是将两个 list 接在一起。而类似 ``*``，他也有一个特定的值，当他跟其他值使用 ``++`` 时会得到同样的值。那个值就是空的 list ``[]``。


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

看起来 ``*`` 之于 ``1`` 跟 ``++`` 之于 ``[]`` 有类似的性质：

    # 函数同样接受两个参数
    # 参数跟回传值是同样的型别
    # 同样存在某些值当套用二元函数时并不会改变其他值

关于这两种操作还有另一个比较难察觉的性质就是，当我们对这个二元函数对三个以上的值操作并化简，函数套用的顺序并不会影响到结果。不论是 ``(3 * 4) * 5`` 或是 ``3 * (4 * 5)``，两种方式都会得到 ``60``。而 ``++`` 也是相同的。

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

我们称呼这样的性质为结合律(associativity)。``*`` 遵守结合律，``++`` 也是。但 ``-`` 就不遵守。``(5 - 3) - 4`` 跟 ``5 - (3 - 4)`` 得到的结果是不同的。


注意到这些性质并具体地写下来，就可以得到 monoid。一个 monoid 是你有一个遵守结合律的二元函数还有一个可以相对于那个函数作为 identity 的值。当某个值相对于一个函数是一个 identity，他表示当我们将这个值丢给函数时，结果永远会是另外一边的那个值本身。``1`` 是相对于 ``*`` 的 identity，而 ``[]`` 是相对于 ``++`` 的 identity。在 Haskell 中还有许多其他的 monoid，这也是为什么我们定义了 ``Monoid`` 这个 typeclass。他描述了表现成 monoid 的那些型别。我们来看看这个 typeclass 是怎么被定义的：

```haskell
class Monoid m where  
    mempty :: m  
    mappend :: m -> m -> m  
    mconcat :: [m] -> m  
    mconcat = foldr mappend mempty  
```

![](balloondog.png)

``Monoid`` typeclass 被定义在 ``import Data.Monoid`` 中。我们来花些时间好好了解他。


首先我们看到只有具体型别才能定义成 ``Monoid`` 的 instance。由于在 typeclass 定义中的 ``m`` 并不接受任何型别参数。这跟 ``Functor`` 以及 ``Applicative`` 不同，他们要求他们的 instance 必须是一个接受单一型别参数的型别构造子。


第一个函数是 ``mempty``，由于他不接受任何参数，所以他并不是一个函数，而是一个 polymorphic 的常数。有点像是 ``Bounded`` 中的 ``minBound`` 一样。``mempty`` 表示一个特定 monoid 的 identity。


再来我们看到 ``mappend``，你可能已经猜到，他是一个接受两个相同型别的值的二元函数，并回传同样的型别。不过要注意的是他的名字不太符合他真正的意思，他的名字隐含了我们要将两个东西接在一起。尽管在 list 的情况下 ``++`` 的确将两个 list 接起来，但 ``*`` 则否。他只不过将两个数值做相乘。当我们再看到其他 ``Monoid`` 的 instance 时，我们会看到他们大部分都没有接起来的做，所以不要用接起来的概念来想像 ``mappend``，只要想像他们是接受两个 monoid 的值并回传另外一个就好了。


在 typeclass 定义中的最后一个函数是 ``mconcat``。他接受一串 monoid 值，并将他们用 ``mappend`` 简化成单一的值。他有一个预设的实作，就是从 ``mempty`` 作为起始值，然后用 ``mappend`` 来 fold。由于对于大部分的 instance 预设的实作就没什么问题，我们不会想要实作自己的 ``mconcat``。当我们定义一个型别属于 ``Monoid`` 的时候，多半实作 ``mempty`` 跟 ``mappend`` 就可以了。而 ``mconcat`` 就是因为对于一些 instance，有可能有比较有效率的方式来实作 ``mconcat``。不过大多数情况都不需要。



在我们继续接下去看几个 ``Monoid`` 的例子前，我们来看一下 monoid law。我们提过必须有一个值作为 identity 以及一个遵守结合律的二元函数当作前提。我们是可以定义一个 ``Monoid`` 的 instance 却不遵守这些定律的，但这样写出来的 instance 就没有用了，因为我们在使用 ``Monoid`` 的时候都是依靠这些定律才可以称作实质上的 monoid。所以我们必须确保他们遵守：

    # ``mempty `mappend` x = x``
    # ``x `mappend` mempty = x``
    # ``(x `mappend` y) `mappend` z = x `mappend` (y `mappend` z)``

前两个描述了 ``mempty`` 相对于 ``mappend`` 必须要表现成 identity。而第三个定律说了 ``mappend`` 必须要遵守结合律。也就是说我们做 ``mappend`` 顺序并不重要。Haskell 不会自己检查这些定律是否有被遵守。所以你必须自己小心地检查他们。



### Lists are monoids

没错，list 是一种 monoid。正如我们先前看到的，``++`` 跟空的 list ``[]`` 共同形成了一个 monoid。他的 instance 很简单：


```haskell
instance Monoid [a] where  
    mempty = []  
    mappend = (++)  
```

list 是 ``Monoid`` typeclass 的一个 instance，这跟他们装的元素的型别无关。注意到我们写 ``instance Monoid [a]`` 而非 ``instance Monoid []``，这是因为 ``Monoid`` 要求 instance 必须是具体型别。

我们试着跑跑看，得到我们预期中的结果：

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

注意到最后一行我们明白地标记出型别。这是因为如果只些 ``mempty`` 的话，GHCi 不会知道他是哪一个 instance 的 ``mempty``，所以我们必须清楚说出他是 list instance 的 mempty。我们可以使用一般化的型别 ``[a]``，因为空的 list 可以看作是属于任何型别。


由于 ``mconcat`` 有一个预设的实作，我们将某个型别定义成 ``Monoid`` 的型别时就可以自动地得到预设的实作。但对于 list 而言，``mconcat`` 其实就是 ``concat``。他接受一个装有 list 的 list，并把他用 ``++`` 来扁平化他。


list 的 instance 也遵守 monoid law。当我们有好几个 list 并且用 ``mappend`` 来把他们串起来，先后顺序并不是很重要，因为他们都是接在最后面。而且空的 list 也表现得如 identity 一样。注意到 monoid 并不要求 ``a `mappend` b`` 等于 ``b `mappend` a``。在 list 的情况下，他们明显不相等。

```haskell
ghci> "one" `mappend` "two"  
"onetwo"  
ghci> "two" `mappend` "one"  
"twoone"  
```

这样并没有关系。``3 * 5`` 跟 ``5 * 3`` 会相等只不过是乘法的性质而已，但没有保证所有 monoid 都要遵守。

### Product and Sum

我们已经描述过将数值表现成一种 monoid 的方式。只要将 ``*`` 当作二元函数而 ``1`` 当作 identity 就好了。而且这不是唯一一种方式，另一种方式是将 ``+`` 作为二元函数而 ``0`` 作为 identity。

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

他也遵守 monoid law，因为将 0 加上其他数值，都会是另外一者。而且加法也遵守结合律。所以现在我们有两种方式来将数值表现成 monoid，那要选哪一个呢？其实我们不必要强迫定下来，还记得当同一种型别有好几种表现成某个 typeclass 的方式时，我们可以用 ``newtype`` 来包裹现有的型别，然后再定义新的 instance。这样就行了。


``Data.Monoid`` 这个模组汇出了两种型别，``Product`` 跟 ``Sum``。``Product`` 定义如下：

```haskell
newtype Product a =  Product { getProduct :: a }  
    deriving (Eq, Ord, Read, Show, Bounded)  
```

简单易懂，就是一个单一型别参数的 ``newtype``，并 derive 一些性质。他的 ``Monoid`` 的 instance 长得像这样：

```haskell
instance Num a => Monoid (Product a) where  
    mempty = Product 1  
    Product x `mappend` Product y = Product (x * y)  
```

``mempty`` 只不过是将 ``1`` 包在 ``Product`` 中。``mappend`` 则对 ``Product`` 的构造子做模式匹配，将两个取出的数值相乘后再将结果放回去。就如你看到的，typeclass 定义前面有 ``Num a`` 的条件限制。所以他代表 ``Product a`` 对于所有属于 ``Num`` 的 ``a`` 是一个 ``Monoid``。要将 ``Product a`` 作为一个 monoid 使用，我们需要用 newtype 来做包裹跟解开的动作。

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

这当作 ``Monoid`` 的一个演练还不错，但并不会有人觉得这会比 ``3 * 9`` 跟 ``3 * 1`` 这种方式来做乘法要好。但我们稍后会说明尽管像这种显而易见的定义还是有他方便的地方。

``Sum`` 跟 ``Product`` 定义的方式类似，我们也可以用类似的方式操作：

```haskell
ghci> getSum $ Sum 2 `mappend` Sum 9  
11  
ghci> getSum $ mempty `mappend` Sum 3  
3  
ghci> getSum . mconcat . map Sum $ [1,2,3]  
6  
```


### Any and ALL

另一种可以有两种表示成 monoid 方式的型别是 ``Bool``。第一种方式是将 ``||`` 当作二元函数，而 ``False`` 作为 identity。这样的意思是只要有任何一个参数是 ``True`` 他就回传 ``True``，否则回传 ``False``。所以如果我们使用 ``False`` 作为 identity，他会在跟 ``False`` 做 OR 时回传 ``False``，跟 ``True`` 做 OR 时回传 ``True``。``Any`` 这个 newtype 是 ``Monoid`` 的一个 instance，并定义如下：

```haskell
newtype Any = Any { getAny :: Bool }  
    deriving (Eq, Ord, Read, Show, Bounded)  
```

他的 instance 长得像这样：

```haskell
instance Monoid Any where  
    mempty = Any False  
    Any x `mappend` Any y = Any (x || y)  
```

他叫做 ``Any`` 的理由是 ``x `mappend` y`` 当有任何一个是 ``True`` 时就会是 ``True``。就算是更多个用 ``mappend`` 串起来的 ``Any``，他也会在任何一个是 ``True`` 回传 ``True``。


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

另一种 ``Bool`` 表现成 ``Monoid`` 的方式是用 ``&&`` 作为二元函数，而 ``True`` 作为 identity。只有当所有都是 ``True`` 的时候才会回传 ``True``。下面是他的 newtype 定义：

```haskell
newtype All = All { getAll :: Bool }  
        deriving (Eq, Ord, Read, Show, Bounded)  
```

而这是他的 instance：

```haskell
instance Monoid All where  
        mempty = All True  
        All x `mappend` All y = All (x && y)  
```

当我们用 ``mappend`` 来串起 ``All`` 型别的值时，结果只有当所有 ``mappend`` 的值是 ``True`` 时才会是 ``True``：

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


就如乘法跟加法一样，我们通常宁愿用二元函数来操作他们也不会用 newtype 来将他们包起来。不会将他们包成 ``Any`` 或 ``All`` 然后用 ``mappend``，``mempty`` 或 ``mconcat`` 来操作。通常使用 ``or`` 跟 ``and``，他们接受一串 ``Bool``，并只有当任意一个或是所有都是 ``True`` 的时候才回传 ``True``。


###  The Ordering monoid


还记得 ``Ordering`` 型别吗?他是比较运算之后得到的结果，包含三个值：``LT``，``EQ`` 跟 ``GT``，分别代表小于，等于跟大于：

```haskell
ghci> 1 `compare` 2  
LT  
ghci> 2 `compare` 2  
EQ  
ghci> 3 `compare` 2  
GT  
```

针对 list，数值跟布林值而言，要找出 monoid 的行为只要去检视已经定义的函数，然后看看有没有展现出 monoid 的特性就可以了，但对于 ``Ordering``，我们就必须要更仔细一点才能看出来是否是一个 monoid，但其实他的 ``Monoid`` instance 还蛮直觉的：

```haskell
instance Monoid Ordering where  
    mempty = EQ  
    LT `mappend` _ = LT  
    EQ `mappend` y = y  
    GT `mappend` _ = GT  
```

![](bear.png)

这个 instance 定义如下：当我们用 ``mappend`` 两个 ``Ordering`` 型别的值时，左边的会被保留下来。除非左边的值是 ``EQ``，那我们就会保留右边的当作结果。而 identity 就是 ``EQ``。乍看之下有点随便，但实际上他是我们比较两个英文字时所用的方法。我们先比较两个字母是否相等，如果他们不一样，那我们就知道那一个字在字典中会在前面。而如果两个字母相等，那我们就继续比较下一个字母，以此类推。


举例来说，如果我们字典顺序地比较 ``"ox"`` 跟 ``"on"`` 的话。我们会先比较两个字的首个字母，看看他们是否相等，然后继续比较第二个字母。我们看到 ``'x'`` 是比 ``'n'`` 要来得大，所以我们就知道如何比较两个字了。而要了解为何 ``EQ`` 是 identity，我们可以注意到如果我们在两个字中间的同样位置塞入同样的字母，那他们之间的字典顺序并不会改变。``"oix"`` 仍然比 ``"oin"`` 要大。


很重要的一件事是在 ``Ordering`` 的 ``Monoid`` 定义里 ``x `mappend` y`` 并不等于 ``y `mappend` x``。因为除非第一个参数是 ``EQ``，不然结果就会是第一个参数。所以 ``LT `mappend` GT`` 等于 ``LT``，然而 ``GT `mappend` LT`` 等于 ``GT``。

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

所以这个 monoid 在什么情况下会有用呢？假设你要写一个比较两个字串长度的函数，并回传 ``Ordering``。而且当字串一样长的时候，我们不直接回传 ``EQ``，反而继续用字典顺序比较他们。一种实作的方式如下：


```haskell
lengthCompare :: String -> String -> Ordering  
lengthCompare x y = let a = length x `compare` length y   
                        b = x `compare` y  
                    in  if a == EQ then b else a  
```

我们称呼比较长度的结果为 ``a``，而比较字典顺序的结果为 ``b``，而当长度一样时，我们就回传字典顺序。

如果善用我们 ``Ordering`` 是一种 monoid 这项知识，我们可以把我们的函数写得更简单些：

```haskell
import Data.Monoid

lengthCompare :: String -> String -> Ordering  
lengthCompare x y = (length x `compare` length y) `mappend`  
                    (x `compare` y)  
```

我们可以试着跑跑看：

```haskell
ghci> lengthCompare "zen" "ants"  
LT  
ghci> lengthCompare "zen" "ant"  
GT  
```

要记住当我们使用 ``mappend``。他在左边不等于 ``EQ`` 的情况下都会回传左边的值。相反地则回传右边的值。这也是为什么我们将我们认为比较重要的顺序放在左边的参数。如果我们要继续延展这个函数，要让他们比较母音的顺序，并把这顺序列为第二重要，那我们可以这样修改他：

```haskell
import Data.Monoid  
  
lengthCompare :: String -> String -> Ordering  
lengthCompare x y = (length x `compare` length y) `mappend`  
                    (vowels x `compare` vowels y) `mappend`  
                    (x `compare` y)  
    where vowels = length . filter (`elem` "aeiou")  
```

我们写了一个辅助函数，他接受一个字串并回传他有多少母音。他是先用 filter 来把字母滤到剩下 ``"aeiou"``，然后再用 ``length`` 计算长度。

```haskell
ghci> lengthCompare "zen" "anna"  
LT  
ghci> lengthCompare "zen" "ana"  
LT  
ghci> lengthCompare "zen" "ann"  
GT  
```

在第一个例子中我们看到长度不同所以回传 ``LT``，明显地 ``"zen"`` 要短于 ``"anna"``。在第二个例子中，长度是一样的，但第二个字串有比较多的母音，所以结果仍然是 ``LT``。在第三个范例中，两个长度都相等，他们也有相同个数的母音，经由字典顺序比较后得到 ``"zen"`` 比较大。


``Ordering`` 的 monoid 允许我们用不同方式比较事物，并将这些顺序也定义了依重要度不同的一个顺序。


### Maybe the monoid


我们来看一下 ``Maybe a`` 是怎样有多种方式来表现成 ``Monoid`` 的，并且说明哪些是比较有用的。一种将 ``Maybe a`` 当作 monoid 的方式就是他的 ``a`` 也是一个 monoid，而我们将 ``mappend`` 实作成使用包在 ``Just`` 里面的值对应的 ``mappend``。并且用 ``Nothing`` 当作 identity。所以如果我 ``mappend`` 两个参数中有一个是 ``Nothing``。那结果就会是另一边的值。他的 instance 定义如下：	


```haskell
instance Monoid a => Monoid (Maybe a) where  
    mempty = Nothing  
    Nothing `mappend` m = m  
    m `mappend` Nothing = m  
    Just m1 `mappend` Just m2 = Just (m1 `mappend` m2)  
```

留意到 class constraint。他说明 ``Maybe a`` 只有在 ``a`` 是 ``Monoid`` 的情况下才会是一个 ``Monoid``。如果我们 ``mappend`` 某个东西跟 ``Nothing``。那结果就会是某个东西。如果我们 ``mappend`` 两个 ``Just``，那 ``Just`` 包住的结果就会 ``mappended`` 在一起并放回 ``Just``。我们能这么做是因为 class constraint 保证了在 ``Just`` 中的值是 ``Monoid``。

```haskell
ghci> Nothing `mappend` Just "andy"  
Just "andy"  
ghci> Just LT `mappend` Nothing  
Just LT  
ghci> Just (Sum 3) `mappend` Just (Sum 4)  
Just (Sum {getSum = 7})  
```

这当你在处理有可能失败的 monoid 的时候比较有用。有了这个 instance，我们就不必一一去检查他们是否失败，是否是 ``Nothing`` 或是 ``Just``，我们可以直接将他们当作普通的 monoid。


但如果在 ``Maybe`` 中的型别不是 ``Monoid`` 呢？注意到在先前的 instance 定义中，唯一有依赖于 monoid 限制的情况就是在 ``mappend`` 两个 ``Just`` 的时候。但如果我们不知道包在 ``Just`` 里面的值究竟是不是 monoid，我们根本无法用 ``mappend`` 操作他们，所以该怎么办呢？一种方式就是直接丢掉第二个值而留下第一个值。这就是 ``First a`` 存在的目的，而这是他的定义：

```haskell
newtype First a = First { getFirst :: Maybe a }  
    deriving (Eq, Ord, Read, Show) 
```

我们接受一个 ``Maybe a`` 并把他包成 newtype，``Monoid`` 的定义如下：

```haskell
instance Monoid (First a) where  
    mempty = First Nothing  
    First (Just x) `mappend` _ = First (Just x)  
    First Nothing `mappend` x = x  
```

正如我们说过得，``mempty`` 就是包在 ``First`` 中的 ``Nothing``。如果 ``mappend`` 的第一个参数是 ``Just``，我们就直接忽略第二个参数。如果第一个参数是 ``Nothing``，那我们就将第二个参数当作结果。并不管他究竟是 ``Just`` 或是 ``Nothing``：	

```haskell
ghci> getFirst $ First (Just 'a') `mappend` First (Just 'b')  
Just 'a'  
ghci> getFirst $ First Nothing `mappend` First (Just 'b')  
Just 'b'  
ghci> getFirst $ First (Just 'a') `mappend` First Nothing  
Just 'a'  
```

``First`` 在我们有一大串 ``Maybe`` 而且想知道他们之中就竟有没有 ``Just`` 的时候很有用。可以利用 ``mconcat``：

```haskell
ghci> getFirst . mconcat . map First $ [Nothing, Just 9, Just 10]  
Just 9  
```

如果我们希望定义一个 ``Maybe a`` 的 monoid，让他当 ``mappend`` 的两个参数都是 ``Just`` 的时候将第二个参数当作结果。``Data.Monoid`` 中有一个现成的 ``Last a``，他很像是 ``First a``，只差在 ``mappend`` 跟 ``mconcat`` 会保留最后一个非 ``Nothing`` 的值。

```haskell
ghci> getLast . mconcat . map Last $ [Nothing, Just 9, Just 10]  
Just 10  
ghci> getLast $ Last (Just "one") `mappend` Last (Just "two")  
Just "two" 
```


### Using monoids to fold data structures


另一种有趣的 monoid 使用方式就是让他来帮助我们 fold 一些资料结构。到目前为止我们只有 fold list。但 list 并不是唯一一种可以 fold 的资料结构。我们几乎可以 fold 任何一种资料结构。像是 tree 也是一种常见的可以 fold 的资料结构。


由于有太多种资料结构可以 fold 了，所以我们定义了 ``Foldable`` 这个 typeclass。就像 ``Functor`` 是定义可以 map over 的结构。``Foldable`` 是定义可以 fold 的结构。在 ``Data.Foldable`` 中有定义了一些有用的函数，但他们名称跟 ``Prelude`` 中的名称冲突。所以最好是用 qualified 的方式 import 他们：

```haskell
import qualified Foldable as F      
```

为了少打一些字，我们将他们 import qualified 成 ``F``。所以这个 typeclass 中定义了哪些函数呢？有 ``foldr``，``foldl``，``foldr1`` 跟 ``foldl1``。你会说我们已经知道这些函数了，他们有什么不一样的地方吗？我们来比较一下 ``Foldable`` 中的 ``foldr`` 跟 ``Prelude`` 中的 ``foldr`` 的型别异同：

```haskell
ghci> :t foldr  
foldr :: (a -> b -> b) -> b -> [a] -> b  
ghci> :t F.foldr  
F.foldr :: (F.Foldable t) => (a -> b -> b) -> b -> t a -> b  
```

尽管 ``foldr`` 接受一个 list 并将他 fold 起来，``Data.Foldable`` 中的 ``foldr`` 接受任何可以 fold 的型别。并不只是 list。
而两个 ``foldr`` 对于 list 的结果是相同的：

```haskell
ghci> foldr (*) 1 [1,2,3]  
6  
ghci> F.foldr (*) 1 [1,2,3]  
6  
```

那有哪些资料结构支援 fold 呢？首先我们有 ``Maybe``：

```haskell
ghci> F.foldl (+) 2 (Just 9)  
11  
ghci> F.foldr (||) False (Just True)  
True 
```


但 fold 一个 ``Maybe`` 并没什么新意。毕竟当他是 ``Just`` 的时候表现得像是只有单一元素的 list，而当他是 ``Nothing`` 的时候就像是空的 list 一样。所以我们来看一些比较复杂的资料结构。


还记得 Making Our Own Types and Typeclass 章节中的树状的资料结构吗？我们是这样定义的：

```haskell
data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Show, Read, Eq)      
```

我们说一棵树要不就是一棵空的树要不然就是一个包含值的节点，并且还指向另外两棵树。定义他之后，我们将他定义成 ``Functor`` 的 instance，因此可以 ``fmap`` 他。现在我们要将他定义成 ``Foldable`` 的 instance，这样我们就可以 fold 他。要定义成 ``Foldable`` 的一种方式就是实作 ``foldr``。但另一种比较简单的方式就是实作 ``foldMap``，他也属于 ``Foldable`` typeclass。``foldMap`` 的型别如下：

```haskell
foldMap :: (Monoid m, Foldable t) => (a -> m) -> t a -> m  
```

第一个参数是一个函数，这个函数接受 foldable 资料结构中包含的元素的型别，并回传一个 monoid。他第二个参数是一个 foldable 的结构，并包含型别 ``a`` 的元素。他将第一个函数来 map over 这个 foldable 的结构，因此得到一个包含 monoid 的 foldable 结构。然后用 ``mappend`` 来简化这些 monoid，最后得到单一的一个 monoid。这个函数听起来不太容易理解，但我们下面会看到他其实很容易实作。而且好消息是只要实作了这个函数就可以让我们的函数成为 ``Foldable``。所以我们只要实作某个型别的 ``foldMap``，我们就可以得到那个型别的 ``foldr`` 跟 ``foldl``。


这就是我们如何定义 ``Tree`` 成为 ``Foldable`` 的：

```haskell
instance F.Foldable Tree where  
    foldMap f Empty = mempty  
    foldMap f (Node x l r) = F.foldMap f l `mappend`  
                                f x           `mappend`  
                                F.foldMap f r  
```


![](accordion.png)

我们是这样思考的：如果我们写一个函数，他接受树中的一个元素并回传一个 monoid，那我们要怎么简化整棵树到只有单一一个 monoid？当我们在对树做 ``fmap`` 的时候，我们将那函数套用至节点上，并递回地套用至左子树以及右子树。这边我们不只是 map 一个函数而已，我们还要求要把结果用 ``mappend`` 简化成只有单一一个 monoid 值。首先我们考虑树为空的情形，一棵没有值也没有子树的情形。由于没有值我们也没办法将他套用上面转换成 monoid 的函数，所以当树为空的时候，结果应该要是 ``mempty``。


在非空节点的情形下比较有趣，他包含一个值跟两棵子树。在这种情况下，我们递回地做 ``foldMap``，用 ``f`` 来套用到左子树跟右子树上。要记住我们的 ``foldMap`` 只会得到单一的 monoid 值。我们也会套用 ``f`` 到节点中的值。这样我们就得到三个 monoid 值，有两个来自简化子树的结果，还有一个是套用 ``f`` 到节点中的值的结果。而我们需要将这三个值整合成单一个值。要达成这个目的我们使用 ``mappend``，而且自然地会想到照左子树，节点值以及右子树的顺序来简化。


注意到我们并不一定要提供一个将普通值转成 monoid 的函数。我们只是把他当作是 ``foldMap`` 的参数，我们要决定的只是如何套用那个函数，来把得到的 monoid 们简化成单一结果。


现在我们有树的 ``Foldable`` instance，而 ``foldr`` 跟 ``foldl`` 也有预设的实作了。考虑下面这棵树：


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

他的 root 是 ``5``，而他左边下来分别是 ``3``，再来是 ``1`` 跟 ``6``。而右边下来是 ``9``，再来是 ``8`` 跟 ``10``。有了 ``Foldable`` 的定义，我们就能像对 list 做 fold 一样对树做 fold：

```haskell
ghci> F.foldl (+) 0 testTree  
42  
ghci> F.foldl (*) 1 testTree  
64800  
```

``foldMap`` 不只是定义 ``Foldable`` 新的 instance 有用。他也对简化我们的结构至单一 monoid 值有用。举例来说，如果我们想要知道我们的树中有没有 ``3``，我们可以这样做：

```haskell
ghci> getAny $ F.foldMap (\x -> Any $ x == 3) testTree  
True 
```

这边 ``\x -> Any $ x == 3`` 是一个接受一个数值并回传一个 monoid 的函数，也就是一个包在 ``Any`` 中的 ``Bool``。``foldMap`` 将这个函数套用至树的每一个节点，并把结果用 ``mappend`` 简化成单一 monoid。如果我们这样做：

```haskell
ghci> getAny $ F.foldMap (\x -> Any $ x > 15) testTree  
False 
```

经过套用 lambda 之后我们所有的节点都会是 ``Any False``。但 ``mappend`` 必须要至少吃到一个 ``True`` 才能让最后的结果变成 ``True``。这也是为什么结果会是 ``False``，因为我们树中所有的值都大于 ``15``。


我们也能将 ``foldMap`` 配合 ``\x -> [x]`` 使用来将我们的树转成 list。经过套用那个函数后，所有节点都变成包含单一元素的 list。最后用 ``mappend`` 将这些单一元素的 list 转成一个装有全部元素的 list：

```haskell
ghci> F.foldMap (\x -> [x]) testTree  
[1,3,6,5,8,9,10] 
```

这个小技巧并不限于树而已，他可以被套用在任何 ``Foldable`` 上。

