# Types and Typeclasses

## Type

![](cow.png)

之前我们有说过 Haskell 是 Static Type，这表示在编译时期每个表达式的型别都已经确定下来，这提高了代码的安全性。若代码中有让布林值与数字相除的动作，就不会通过编译。这样的好处就是与其让进程在运行时崩溃，不如在编译时就找出可能的错误。Haskell 中所有东西都有型别，因此在编译的时候编译器可以做到很多事情。

与 Java 和 Pascal 不同，Haskell 支持型别推导。写下一个数字，你就没必要另告诉 Haskell 说"它是个数字"，它自己能推导出来。这样我们就不必在每个函数或表达式上都标明其型别了。在前面我们只简单涉及一下 Haskell 的型别方面的知识，但是理解这一型别系统对于 Haskell 的学习是至关重要的。

型别是每个表达式都有的某种标签，它标明了这一表达式所属的范畴。例如，表达式 ``True`` 是 ``boolean`` 型，``"hello"``是个字串，等等。

可以使用 ghci 来检测表达式的型别。使用 ``:t`` 命令后跟任何可用的表达式，即可得到该表达式的型别，先试一下：

```haskell
ghci> :t 'a'  
'a' :: Char  
ghci> :t True  
True :: Bool  
ghci> :t "HELLO!"  
"HELLO!" :: [Char]  
ghci> :t (True, 'a')  
(True, 'a') :: (Bool, Char)  
ghci> :t 4 == 5  
4 == 5 :: Bool
```

![](bomb.png)

可以看出，``:t`` 命令处理一个表达式的输出结果为表达式后跟 ``::`` 及其型别，``::`` 读作"它的型别为"。凡是明确的型别，其首字母必为大写。``'a'``, 如它的样子，是 ``Char`` 型别，易知是个字符 (character)。``True`` 是 ``Bool`` 型别，也靠谱。不过这又是啥，检测 ``"hello"`` 得一个  ``[Char]`` 这方括号表示一个 List，所以我们可以将其读作"一组字符的 List"。而与 List 不同，每个 Tuple 都是独立的型别，于是 ``(True,'a')`` 的型别是 ``(Bool,Char)``，而 ``('a','b','c')`` 的型别为 ``(Char,Char,Char)``。``4==5`` 一定回传 ``False``，所以它的型别为 Bool。

同样，函数也有型别。编写函数时，给它一个明确的型别声明是个好习惯，比较短的函数就不用多此一举了。还记得前面那个过滤大写字母的 List Comprehension 吗？给它加上型别声明便是这个样子：

```haskell
removeNonUppercase :: [Char] -> [Char]  
removeNonUppercase st = [ c | c <- st, c `elem` ['A'..'Z']]   
```

 ``removeNonUppercase`` 的型别为 ``[Char]->[Char]``，从它的参数和回传值的型别上可以看出，它将一个字串映射为另一个字串。``[Char]`` 与 ``String`` 是等价的，但使用 ``String`` 会更清晰：``removeNonUppercase :: String -> String``。编译器会自动检测出它的型别，我们还是标明了它的型别声明。要是多个参数的函数该怎样？如下便是一个将三个整数相加的简单函数。

```haskell
addThree :: Int -> Int -> Int -> Int  
addThree x y z = x + y + z  
```

参数之间由 ``->`` 分隔，而与回传值之间并无特殊差异。回传值是最后一项，参数就是前三项。稍后，我们将讲解为何只用 ``->`` 而不是 ``Int,Int,Int->Int`` 之类"更好看"的方式来分隔参数。

如果你打算给你编写的函数加上个型别声明却拿不准它的型别是啥，只要先不写型别声明，把函数体写出来，再使用 ``:t`` 命令测一下即可。函数也是表达式，所以 ``:t`` 对函数也是同样可用的。

如下是几个常见的型别：

**Int** 表示整数。7 可以是 Int，但 7.2 不可以。Int 是有界的，也就是说它由上限和下限。对 32 位的机器而言，上限一般是 ``2147483647``，下限是 ``-2147483648``。

**Integer** 表示...厄...也是整数，但它是无界的。这就意味着可以用它存放非常非常大的数，我是说非常大。它的效率不如 Int 高。

```haskell
factorial :: Integer -> Integer  
factorial n = product [1..n]  
```

```haskell
ghci> factorial 50  
30414093201713378043612608166064768844377641568960512000000000000  
```

**Float** 表示单精度的浮点数。

```haskell
circumference :: Float -> Float  
circumference r = 2 * pi * r  
```

```haskell
ghci> circumference 4.0  
25.132742  
```

**Double** 表示双精度的浮点数。

```haskell
circumference' :: Double -> Double  
circumference' r = 2 * pi * r  
```

```haskell
ghci> circumference' 4.0  
25.132741228718345  
```


**Bool** 表示布林值，它只有两种值：``True`` 和 ``False``。

**Char** 表示一个字符。一个字符由单引号括起，一组字符的 List 即为字串。

Tuple 的型别取决于它的长度及其中项的型别。注意，空 Tuple 同样也是个型别，它只有一种值：``()``。


## Type variables

你觉得 ``head`` 函数的型别是啥？它可以取任意型别的 List 的首项，是怎么做到的呢？我们查一下！

```haskell
ghci> :t head  
head :: [a] -> a  
```

![](box.png)

嗯! ``a`` 是啥？型别吗？想想我们在前面说过，凡是型别其首字母必大写，所以它不会是个型别。它是个型别变量，意味着 a 可以是任意的型别。这一点与其他语言中的泛型 (generic) 很相似，但在 Haskell 中要更为强大。它可以让我们轻而易举地写出型别无关的函数。使用到型别变量的函数被称作"多态函数 "，``head`` 函数的型别声明里标明了它可以取任意型别的 List 并回传其中的第一个元素。

在命名上，型别变量使用多个字符是合法的，不过约定俗成，通常都是使用单个字符，如 ``a``, ``b`` ,``c`` ,``d``...

还记得 ``fst``？我们查一下它的型别：

```haskell
ghci> :t fst  
fst :: (a, b) -> a  
```

可以看到``fst``取一个包含两个型别的 Tuple 作参数，并以第一个项的型别作为回传值。这便是 ``fst`` 可以处理一个含有两种型别项的 pair 的原因。注意，``a`` 和 ``b`` 是不同的型别变量，但它们不一定非得是不同的型别，它只是标明了首项的型别与回传值的型别相同。


## Typeclasses入门

![](classes.png)

型别定义行为的接口，如果一个型别属于某 Typeclass，那它必实现了该 Typeclass 所描述的行为。很多从 OOP 走过来的人们往往会把 Typeclass 当成面向对象语言中的 ``class`` 而感到疑惑，厄，它们不是一回事。易于理解起见，你可以把它看做是 Java 的 interface。

``==`` 函数的型别声明是怎样的？

```haskell
ghci> :t (==)  
(==) :: (Eq a) => a -> a -> Bool
```

    *Note*: 判断相等的==运算子是函数，``+-*/``之类的运算子也是同样。在缺省条件下，它们多为中缀函数。若要检查它的型别，就必须得用括号括起使之作为另一个函数，或者说以首码函数的形式调用它。

有意思。在这里我们见到个新东西：``=>`` 符号。它左边的部分叫做型别约束。我们可以这样阅读这段型别声明："相等函数取两个相同型别的值作为参数并回传一个布林值，而这两个参数的型别同在 Eq 类之中(即型别约束)"

**Eq** 这一 Typeclass 提供了判断相等性的接口，凡是可比较相等性的型别必属于 ``Eq`` class。

```haskell
ghci> 5 == 5   
True   
ghci> 5 /= 5   
False   
ghci> 'a' == 'a'   
True   
ghci> "Ho Ho" == "Ho Ho"   
True   
ghci> 3.432 == 3.432   
True 
```

``elem`` 函数的型别为: ``(Eq a)=>a->[a]->Bool``。这是它在检测值是否存在于一个 List 时使用到了==的缘故。

几个基本的 Typeclass：

**Eq** 包含可判断相等性的型别。提供实现的函数是 ``==`` 和 ``/=``。所以，只要一个函数有Eq类的型别限制，那么它就必定在定义中用到了 ``==`` 和 ``/=``。刚才说了，除函数以外的所有型别都属于 ``Eq``，所以它们都可以判断相等性。

**Ord** 包含可比较大小的型别。除了函数以外，我们目前所谈到的所有型别都属于 ``Ord`` 类。``Ord`` 包中包含了``<, >, <=, >=`` 之类用于比较大小的函数。``compare`` 函数取两个 ``Ord`` 类中的相同型别的值作参数，回传比较的结果。这个结果是如下三种型别之一：``GT, LT, EQ``。

```haskell
ghci> :t (>)  
(>) :: (Ord a) => a -> a -> Bool  
```

型别若要成为Ord的成员，必先加入Eq家族。

```haskell
ghci> "Abrakadabra" < "Zebra"  
True  
ghci> "Abrakadabra" `compare` "Zebra"  
LT  
ghci> 5 >= 2  
True  
ghci> 5 `compare` 3  
GT  
```

**Show** 的成员为可用字串表示的型别。目前为止，除函数以外的所有型别都是 ``Show`` 的成员。操作 Show Typeclass，最常用的函数表示 ``show``。它可以取任一Show的成员型别并将其转为字串。

```haskell
ghci> show 3  
"3"  
ghci> show 5.334  
"5.334"  
ghci> show True  
"True"  
```

**Read** 是与 ``Show`` 相反的 Typeclass。``read`` 函数可以将一个字串转为 ``Read`` 的某成员型别。

```haskell
ghci> read "True" || False  
True  
ghci> read "8.2" + 3.8  
12.0  
ghci> read "5" - 2  
3  
ghci> read "[1,2,3,4]" ++ [3]  
[1,2,3,4,3]  
```

一切良好，如上的所有型别都属于这一 Typeclass。尝试 ``read "4"`` 又会怎样？

```haskell
ghci> read "4"  
< interactive >:1:0:  
    Ambiguous type variable `a' in the constraint:  
      `Read a' arising from a use of `read' at <interactive>:1:0-7  
    Probable fix: add a type signature that fixes these type variable(s)  
```

ghci 跟我们说它搞不清楚我们想要的是什么样的回传值。注意调用 ``read`` 后跟的那部分，ghci 通过它来辨认其型别。若要一个 ``boolean`` 值，他就知道必须得回传一个 ``Bool`` 型别的值。但在这里它只知道我们要的型别属于 Read Typeclass，而不能明确到底是哪个。看一下 ``read`` 函数的型别声明吧：

```haskell
ghci> :t read  
read :: (Read a) => String -> a  
```
 
看，它的回传值属于 ReadTypeclass，但我们若用不到这个值，它就永远都不会得知该表达式的型别。所以我们需要在一个表达式后跟``::`` 的*型别注释*，以明确其型别。如下：

```haskell
ghci> read "5" :: Int  
5  
ghci> read "5" :: Float  
5.0  
ghci> (read "5" :: Float) * 4  
20.0  
ghci> read "[1,2,3,4]" :: [Int]  
[1,2,3,4]  
ghci> read "(3, 'a')" :: (Int, Char)  
(3, 'a')  
```

编译器可以辨认出大部分表达式的型别，但遇到 ``read "5"`` 的时候它就搞不清楚究竟该是 Int 还是 Float 了。只有经过运算，Haskell 才会明确其型别；同时由于 Haskell 是静态的，它还必须得在 编译前搞清楚所有值的型别。所以我们就最好提前给它打声招呼："嘿，这个表达式应该是这个型别，省的你认不出来！"

**Enum** 的成员都是连续的型别 -- 也就是可枚举。``Enum`` 类存在的主要好处就在于我们可以在 ``Range`` 中用到它的成员型别：每个值都有后继子 (successer) 和前置子 (predecesor)，分别可以通过 ``succ`` 函数和 ``pred`` 函数得到。该 Typeclass 包含的型别有：``()``, ``Bool``, ``Char``, ``Ordering``, ``Int``, ``Integer``, ``Float`` 和 ``Double``。

```haskell
ghci> ['a'..'e']  
"abcde"  
ghci> [LT .. GT]  
[LT,EQ,GT]  
ghci> [3 .. 5]  
[3,4,5]  
ghci> succ 'B'  
'C'  
```

**Bounded** 的成员都有一个上限和下限。

```haskell
ghci> minBound :: Int  
-2147483648  
ghci> maxBound :: Char  
'\1114111'  
ghci> maxBound :: Bool  
True  
ghci> minBound :: Bool  
False  
```

``minBound`` 和 ``maxBound`` 函数很有趣，它们的型别都是 ``(Bounded a) => a``。可以说，它们都是多态常量。

如果其中的项都属于 ``Bounded`` Typeclass，那么该 Tuple 也属于 ``Bounded``

```haskell
ghci> maxBound :: (Bool, Int, Char)  
(True,2147483647,'\1114111')  
```

**Num** 是表示数字的 Typeclass，它的成员型别都具有数字的特征。检查一个数字的型别：

```haskell
ghci> :t 20  
20 :: (Num t) => t  
```

看样子所有的数字都是多态常量，它可以作为所有 ``Num`` Typeclass中的成员型别。以上便是 ``Num`` Typeclass 中包含的所有型别，检测 `*` 运算子的型别，可以发现它可以处理一切的数字：

```haskell
ghci> :t (*)  
(*) :: (Num a) => a -> a -> a  
```

它只取两个相同型别的参数。所以 ``(5 :: Int) * (6 :: Integer)`` 会引发一个型别错误，而 ``5 * (6 :: Integer)`` 就不会有问题。

型别只有亲近 ``Show`` 和 ``Eq``，才可以加入 ``Num``。

**Integral** 同样是表示数字的 Typeclass。``Num`` 包含所有的数字：实数和整数。而 Integral 仅包含整数，其中的成员型别有 ``Int`` 和 ``Integer``。

**Floating** 仅包含浮点型别：``Float`` 和 ``Double``。

有个函数在处理数字时会非常有用，它便是 **fromIntegral**。其型别声明为： ``fromIntegral :: (Num b, Integral a) => a -> b``。从中可以看出，它取一个整数做参数并回传一个更加通用的数字，这在同时处理整数和浮点时会尤为有用。举例来说，``length`` 函数的型别声明为：``length :: [a] -> Int``，而非更通用的形式，如 ``length :: (Num b) => [a] -> b``。这应该是历史原因吧，反正我觉得挺蠢。如果取了一个 List 长度的值再给它加 3.2 就会报错，因为这是将浮点数和整数相加。面对这种情况，我们就用 ``fromIntegral (length [1,2,3,4]) + 3.2`` 来解决。

注意到，``fromIntegral`` 的型别声明中用到了多个型别约束。如你所见，只要将多个型别约束放到括号里用逗号隔开即可。

