# 函数的语法

## 模式匹配 (Pattern matching)

![](pattern.png)

本章讲的就是 Haskell 那套独特的语法结构，先从模式匹配开始。模式匹配通过检查数据的特定结构来检查其是否匹配，并按模式从中取得数据。

在定义函数时，你可以为不同的模式分别定义函数本身，这就让程式码更加简洁易读。你可以匹配一切数据型别 --- 数字，字元，List，元组，等等。我们弄个简单函数，让它检查我们传给它的数字是不是 7。

```haskell
lucky :: (Integral a) => a -> String  
lucky 7 = "LUCKY NUMBER SEVEN!"  
lucky x = "Sorry, you're out of luck, pal!"   
```

在呼叫 ``lucky`` 时，模式会从上至下进行检查，一旦有匹配，那对应的函数体就被应用了。这个模式中的唯一匹配是参数为 7，如果不是 7，就转到下一个模式，它匹配一切数值并将其绑定为 ``x`` 。这个函数完全可以使用 ``if`` 实现，不过我们若要个分辨 1 到 5 中的数字，而无视其它数的函数该怎么办？要是没有模式匹配的话，那可得好大一棵 ``if-else`` 树了！

```haskell
sayMe :: (Integral a) => a -> String  
sayMe 1 = "One!"  
sayMe 2 = "Two!"  
sayMe 3 = "Three!"  
sayMe 4 = "Four!"  
sayMe 5 = "Five!"  
sayMe x = "Not between 1 and 5"  
```

注意下，如果我们把最后匹配一切的那个模式挪到最前，它的结果就全都是 ``"Not between 1 and 5"`` 了。因为它自己匹配了一切数字，不给后面的模式留机会。

记得前面实现的那个阶乘函数么？当时是把 ``n`` 的阶乘定义成了 ``product [1..n]``。也可以写出像数学那样的递回实现，先说明 0 的阶乘是 1 ，再说明每个正整数的阶乘都是这个数与它前驱 (predecessor) 对应的阶乘的积。如下便是翻译到 Haskell 的样子：

```haskell
factorial :: (Integral a) => a -> a  
factorial 0 = 1  
factorial n = n * factorial (n - 1)  
```

这就是我们定义的第一个递回函数。递回在 Haskell 中十分重要，我们会在后面深入理解。如果拿一个数(如 3)呼叫 ``factorial`` 函数，这就是接下来的计算步骤：先计算 ``3*factorial 2``，``factorial 2`` 等于 ``2*factorial 1``，也就是 ``3*(2*(factorial 1))``。``factorial 1`` 等于 ``1*factorial 0``，好，得 ``3*(2*(1*factorial 0))``，递回在这里到头了，嗯 --- 我们在万能匹配前面有定义，0 的阶乘是 1。于是最终的结果等于 ``3*(2*(1*1))``。若是把第二个模式放在前面，它就会捕获包括 0 在内的一切数字，这一来我们的计算就永远都不会停止了。这便是为什么说模式的顺序是如此重要：它总是优先匹配最符合的那个，最后才是那个万能的。

模式匹配也会失败。假如这个函数：

```haskell
charName :: Char -> String  
charName 'a' = "Albert"  
charName 'b' = "Broseph"  
charName 'c' = "Cecil"  
```

拿个它没有考虑到的字元去呼叫它，你就会看到这个：

```haskell
ghci> charName 'a'  
"Albert"  
ghci> charName 'b'  
"Broseph"  
ghci> charName 'h'  
"*** Exception: tut.hs:(53,0)-(55,21): Non-exhaustive patterns in function charName  
```haskell
 
它告诉我们说，这个模式不够全面。因此，在定义模式时，一定要留一个万能匹配的模式，这样我们的程序就不会为了不可预料的输入而崩溃了。

对 Tuple 同样可以使用模式匹配。写个函数，将二维空间中的向量相加该如何？将它们的 ``x`` 项和 ``y`` 项分别相加就是了。如果不了解模式匹配，我们很可能会写出这样的程式码：

```haskell
addVectors :: (Num a) => (a, a) -> (a, a) -> (a, a)  
addVectors a b = (fst a + fst b, snd a + snd b)  
```haskell
 
嗯，可以运行。但有更好的方法，上模式匹配：

```haskell
addVectors :: (Num a) => (a, a) -> (a, a) -> (a, a)  
addVectors (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)  
```

there we go！好多了！注意，它已经是个万能的匹配了。两个 ``addVector`` 的型别都是 ``addVectors:: (Num a) => (a,a) -> (a,a) -> (a,a)``，我们就能够保证，两个参数都是序对 (Pair) 了。

``fst`` 和 ``snd`` 可以从序对中取出元素。三元组 (Tripple) 呢？嗯，没现成的函数，得自己动手：

```haskell
first :: (a, b, c) -> a  
first (x, _, _) = x  

second :: (a, b, c) -> b  
second (_, y, _) = y  
 
third :: (a, b, c) -> c  
third (_, _, z) = z  
```

这里的 ``_`` 就和 List Comprehension 中一样。表示我们不关心这部分的具体内容。

说到 List Comprehension，我想起来在 List Comprehension 中也能用模式匹配：

```haskell
ghci> let xs = [(1,3), (4,3), (2,4), (5,3), (5,6), (3,1)]  
ghci> [a+b | (a,b) <- xs]  
[4,7,6,8,11,4]   
```

一旦模式匹配失败，它就简单挪到下个元素。

对 List 本身也可以使用模式匹配。你可以用 ``[]`` 或 ``:`` 来匹配它。因为 ``[1,2,3]`` 本质就是 ``1:2:3:[]`` 的语法糖。你也可以使用前一种形式，像 ``x:xs`` 这样的模式可以将 List 的头部绑定为 ``x``，尾部绑定为 ``xs``。如果这 List 只有一个元素，那么 ``xs`` 就是一个空 List。

    *Note*：``x:xs`` 这模式的应用非常广泛，尤其是递回函数。不过它只能匹配长度大于等于 1 的 List。

如果你要把 List 的前三个元素都绑定到变数中，可以使用类似 ``x:y:z:xs`` 这样的形式。它只能匹配长度大于等于 3 的 List。

我们已经知道了对 List 做模式匹配的方法，就实现个我们自己的 ``head`` 函数。

```haskell
head' :: [a] -> a  
head' [] = error "Can't call head on an empty list, dummy!"  
head' (x:_) = x  
```

看看管不管用：

```haskell
ghci> head' [4,5,6]  
4  
ghci> head' "Hello"  
'H'  
```

漂亮！注意下，你若要绑定多个变数(用 ``_`` 也是如此)，我们必须用括号将其括起。同时注意下我们用的这个 ``error`` 函数，它可以生成一个运行时错误，用参数中的字串表示对错误的描述。它会直接导致程序崩溃，因此应谨慎使用。可是对一个空 List 取 ``head`` 真的不靠谱哇。

弄个简单函数，让它用非标准的英语给我们展示 List 的前几项。

```haskell
tell :: (Show a) => [a] -> String  
tell [] = "The list is empty"  
tell (x:[]) = "The list has one element: " ++ show x  
tell (x:y:[]) = "The list has two elements: " ++ show x ++ " and " ++ show y  
tell (x:y:_) = "This list is long. The first two elements are: " ++ show x ++ " and " ++ show y  
```

这个函数顾及了空 List，单元素 List，双元素 List 以及较长的 List，所以这个函数很安全。``(x:[])`` 与 ``(x:y:[])`` 也可以写作 ``[x]`` 和 ``[x,y]`` (有了语法糖，我们不必多加括号)。不过 ``(x:y:_)`` 这样的模式就不行了，因为它匹配的 List 长度不固定。

我们曾用 List Comprehension 实现过自己的 ``length`` 函数，现在用模式匹配和递回重新实现它：

```haskell
length' :: (Num b) => [a] -> b  
length' [] = 0  
length' (_:xs) = 1 + length' xs  
```

这与先前写的那个 ``factorial`` 函数很相似。先定义好未知输入的结果 --- 空 List，这也叫作边界条件。再在第二个模式中将这 List 分割为头部和尾部。说，List 的长度就是其尾部的长度加 1。匹配头部用的 ``_``，因为我们并不关心它的值。同时也应明确，我们顾及了 List 所有可能的模式：第一个模式匹配空 List，第二个匹配任意的非空 List。

看下拿 ``"ham"`` 呼叫 ``length'`` 会怎样。首先它会检查它是否为空 List。显然不是，于是进入下一模式。它匹配了第二个模式，把它分割为头部和尾部并无视掉头部的值，得长度就是 ``1+length' "am"``。ok。以此类推，``"am"`` 的 ``length`` 就是 ``1+length' "m"``。好，现在我们有了 ``1+(1+length' "m")``。``length' "m"`` 即 ``1+length ""`` (也就是 ``1+length' []`` )。根据定义，``length' []`` 等于 ``0``。最后得 ``1+(1+(1+0))``。

再实现 ``sum``。我们知道空 List 的和是 0，就把它定义为一个模式。我们也知道一个 List 的和就是头部加上尾部的和的和。写下来就成了：

```haskell
sum' :: (Num a) => [a] -> a  
sum' [] = 0  
sum' (x:xs) = x + sum' xs  
```

还有个东西叫做 ``as`` 模式，就是将一个名字和 ``@`` 置于模式前，可以在按模式分割什么东西时仍保留对其整体的引用。如这个模式 ``xs@(x:y:ys)``，它会匹配出与 ``x:y:ys`` 对应的东西，同时你也可以方便地通过 ``xs`` 得到整个 List，而不必在函数体中重复 ``x:y:ys``。看下这个 quick and dirty 的例子：

```haskell
capital :: String -> String  
capital "" = "Empty string, whoops!"  
capital all@(x:xs) = "The first letter of " ++ all ++ " is " ++ [x]  
```

```haskell
ghci> capital "Dracula"  
"The first letter of Dracula is D"  
```

我们使用 ``as`` 模式通常就是为了在较大的模式中保留对整体的引用，从而减少重复性的工作。

还有——你不可以在模式匹配中使用 ``++``。若有个模式是 ``(xs++ys)``，那么这个 List 该从什么地方分开呢？不靠谱吧。而 ``(xs++[x,y,z])`` 或只一个 ``(xs++[x])`` 或许还能说的过去，不过出于 List 的本质，这样写也是不可以的。

## 什么是 Guards

模式用来检查一个值是否合适并从中取值，而 guard 则用来检查一个值的某项属性是否为真。咋一听有点像是 ``if`` 语句，实际上也正是如此。不过处理多个条件分支时 guard 的可读性要高些，并且与模式匹配契合的很好。

![](guards.png)

在讲解它的语法前，我们先看一个用到 guard 的函数。它会依据你的 BMI 值 (body mass index，身体质量指数)来不同程度地侮辱你。BMI 值即为体重除以身高的平方。如果小于 18.5，就是太瘦；如果在 18.5 到 25 之间，就是正常；25 到 30 之间，超重；如果超过 30，肥胖。这就是那个函数(我们目前暂不为您计算 BMI，它只是直接取一个 BMI 值)。

```haskell
bmiTell :: (RealFloat a) => a -> String  
bmiTell bmi  
    | bmi <= 18.5 = "You're underweight, you emo, you!"  
    | bmi <= 25.0 = "You're supposedly normal. Pffft, I bet you're ugly!"  
    | bmi <= 30.0 = "You're fat! Lose some weight, fatty!"  
    | otherwise   = "You're a whale, congratulations!"  
```

guard 由跟在函数名及参数后面的竖线标志，通常他们都是靠右一个缩进排成一列。一个 guard 就是一个布尔表达式，如果为真，就使用其对应的函数体。如果为假，就送去见下一个 guard，如之继续。如果我们用 24.3 呼叫这个函数，它就会先检查它是否小于等于 18.5，显然不是，于是见下一个 guard。24.3 小于 25.0，因此通过了第二个 guard 的检查，就返回第二个字串。

在这里则是相当的简洁，不过不难想象这在命令式语言中又会是怎样的一棵 if-else 树。由于 if-else 的大树比较杂乱，若是出现问题会很难发现，guard 对此则十分清楚。

最后的那个 guard 往往都是 ``otherwise``，它的定义就是简单一个 ``otherwise = True`` ，捕获一切。这与模式很相像，只是模式检查的是匹配，而它们检查的是布尔表达式 。如果一个函数的所有 guard 都没有通过(而且没有提供 ``otherwise`` 作万能匹配)，就转入下一模式。这便是 guard 与模式契合的地方。如果始终没有找到合适的 guard 或模式，就会发生一个错误。

当然，guard 可以在含有任意数量参数的函数中使用。省得用户在使用这函数之前每次都自己计算 ``bmi``。我们修改下这个函数，让它取身高体重为我们计算。

```haskell
bmiTell :: (RealFloat a) => a -> a -> String  
bmiTell weight height  
    | weight / height ^ 2 <= 18.5 = "You're underweight, you emo, you!"  
    | weight / height ^ 2 <= 25.0 = "You're supposedly normal. Pffft, I bet you're ugly!"  
    | weight / height ^ 2 <= 30.0 = "You're fat! Lose some weight, fatty!"  
    | otherwise                 = "You're a whale, congratulations!"    
```

你可以测试自己胖不胖。

```haskell
ghci> bmiTell 85 1.90  
"You're supposedly normal. Pffft, I bet you're ugly!"  
```

运行的结果是我不太胖。不过程式却说我很丑。

要注意一点，函数的名字和参数的后面并没有 ``=``。许多初学者会造成语法错误，就是因为在后面加上了 ``=``。

另一个简单的例子：写个自己的 ``max`` 函数。应该还记得，它是取两个可比较的值，返回较大的那个。

```haskell
max' :: (Ord a) => a -> a -> a  
max' a b   
    | a > b     = a  
    | otherwise = b  
```

guard 也可以塞在一行里面。但这样会丧失可读性，因此是不被鼓励的。即使是较短的函数也是如此，不过出于展示，我们可以这样重写 ``max'``：

```haskell
max' :: (Ord a) => a -> a -> a  
max' a b | a > b = a | otherwise = b  
```

这样的写法根本一点都不容易读。

我们再来试试用 guard 实现我们自己的 ``compare`` 函数：

```haskell
myCompare :: (Ord a) => a -> a -> Ordering  
a `myCompare` b  
    | a > b     = GT  
    | a == b    = EQ  
    | otherwise = LT  
```

```haskell
ghci> 3 `myCompare` 2  
GT  
```

    *Note*：通过反单引号，我们不仅可以以中缀形式呼叫函数，也可以在定义函数的时候使用它。有时这样会更易读。


##  关键字 Where

前一节中我们写了这个 ``bmi`` 计算函数：

```haskell
bmiTell :: (RealFloat a) => a -> a -> String  
bmiTell weight height  
    | weight / height ^ 2 <= 18.5 = "You're underweight, you emo, you!"  
    | weight / height ^ 2 <= 25.0 = "You're supposedly normal. Pffft, I bet you're ugly!"  
    | weight / height ^ 2 <= 30.0 = "You're fat! Lose some weight, fatty!"  
    | otherwise                   = "You're a whale, congratulations!"  
```

注意，我们重复了 3 次。我们重复了 3 次。程式设计师的字典里不应该有"重复"这个词。既然发现有重复，那么给它一个名字来代替这三个表达式会更好些。嗯，我们可以这样修改：

```haskell
bmiTell :: (RealFloat a) => a -> a -> String  
bmiTell weight height  
    | bmi <= 18.5 = "You're underweight, you emo, you!"  
    | bmi <= 25.0 = "You're supposedly normal. Pffft, I bet you're ugly!"  
    | bmi <= 30.0 = "You're fat! Lose some weight, fatty!"  
    | otherwise   = "You're a whale, congratulations!"  
    where bmi = weight / height ^ 2  
```

我们的 ``where`` 关键字跟在 guard 后面(最好是与竖线缩进一致)，可以定义多个名字和函数。这些名字对每个 guard 都是可见的，这一来就避免了重复。如果我们打算换种方式计算 ``bmi``，只需进行一次修改就行了。通过命名，我们提升了程式码的可读性，并且由于 ``bmi`` 只计算了一次，函数的执行效率也有所提升。我们可以再做下修改：

```haskell
bmiTell :: (RealFloat a) => a -> a -> String  
bmiTell weight height  
    | bmi <= skinny = "You're underweight, you emo, you!"  
    | bmi <= normal = "You're supposedly normal. Pffft, I bet you're ugly!"  
    | bmi <= fat    = "You're fat! Lose some weight, fatty!"  
    | otherwise     = "You're a whale, congratulations!"  
    where bmi = weight / height ^ 2  
          skinny = 18.5  
          normal = 25.0  
          fat = 30.0  
```

函数在 ``where`` 绑定中定义的名字只对本函数可见，因此我们不必担心它会污染其他函数的命名空间。注意，其中的名字都是一列垂直排开，如果不这样规范，Haskell 就搞不清楚它们在哪个地方了。

``where`` 绑定不会在多个模式中共享。如果你在一个函数的多个模式中重复用到同一名字，就应该把它置于全局定义之中。

``where`` 绑定也可以使用*模式匹配*！前面那段程式码可以改成：

```haskell
...  
where bmi = weight / height ^ 2  
      (skinny, normal, fat) = (18.5, 25.0, 30.0)  
```

我们再搞个简单函数，让它告诉我们姓名的首字母：

```haskell
initials :: String -> String -> String  
initials firstname lastname = [f] ++ ". " ++ [l] ++ "."  
    where (f:_) = firstname  
          (l:_) = lastname  
```

我们完全按可以在函数的参数上直接使用模式匹配(这样更短更简洁)，在这里只是为了演示在 ``where`` 语句中同样可以使用模式匹配：

``where`` 绑定可以定义名字，也可以定义函数。保持健康的程式语言风格，我们搞个计算一组 ``bmi`` 的函数：

```haskell
calcBmis :: (RealFloat a) => [(a, a)] -> [a]  
calcBmis xs = [bmi w h | (w, h) <- xs] 
    where bmi weight height = weight / height ^ 2  
```

这就全了！在这里将 ``bmi`` 搞成一个函数，是因为我们不能依据参数直接进行计算，而必须先从传入函数的 List 中取出每个序对并计算对应的值。

``where`` 绑定还可以一层套一层地来使用。
有个常见的写法是，在定义一个函数的时候也写几个辅助函数摆在 ``where`` 绑定中。
而每个辅助函数也可以透过 ``where`` 拥有各自的辅助函数。




##  关键字 Let

``let`` 绑定与 ``where`` 绑定很相似。``where`` 绑定是在函数底部定义名字，对包括所有 guard 在内的整个函数可见。``let`` 绑定则是个表达式，允许你在任何位置定义局部变数，而对不同的 guard 不可见。正如 Haskell 中所有赋值结构一样，``let`` 绑定也可以使用模式匹配。看下它的实际应用！这是个依据半径和高度求圆柱体表面积的函数：

```haskell
cylinder :: (RealFloat a) => a -> a -> a  
cylinder r h = 
    let sideArea = 2 * pi * r * h  
        topArea = pi * r ^2  
    in  sideArea + 2 * topArea  
```

![](letitbe.png)

``let`` 的格式为 ``let [bindings] in [expressions]``。在 ``let`` 中绑定的名字仅对 ``in`` 部分可见。``let`` 里面定义的名字也得对齐到一列。不难看出，这用 ``where`` 绑定也可以做到。那么它俩有什么区别呢？看起来无非就是，``let`` 把绑定放在语句前面而 ``where`` 放在后面嘛。

不同之处在于，``let`` 绑定本身是个表达式，而 ``where`` 绑定则是个语法结构。还记得前面我们讲if语句时提到它是个表达式，因而可以随处安放？

```haskell
ghci> [if 5 > 3 then "Woo" else "Boo", if 'a' > 'b' then "Foo" else "Bar"]  
["Woo", "Bar"]  
ghci> 4 * (if 10 > 5 then 10 else 0) + 2  
42
```

用 ``let`` 绑定也可以实现：

```haskell
ghci> 4 * (let a = 9 in a + 1) + 2  
42  
```

``let`` 也可以定义局部函数：

```haskell
ghci> [let square x = x * x in (square 5, square 3, square 2)]  
[(25,9,4)]  
```

若要在一行中绑定多个名字，再将它们排成一列显然是不可以的。不过可以用分号将其分开。

```haskell
ghci> (let a = 100; b = 200; c = 300 in a*b*c, let foo="Hey "; bar = "there!" in foo ++ bar)  
(6000000,"Hey there!")  
```

最后那个绑定后面的分号不是必须的，不过加上也没关系。如我们前面所说，你可以在 ``let`` 绑定中使用模式匹配。这在从 Tuple 取值之类的操作中很方便。

```haskell
ghci> (let (a,b,c) = (1,2,3) in a+b+c) * 100  
600  
```

你也可以把 ``let`` 绑定放到 List Comprehension 中。我们重写下那个计算 ``bmi`` 值的函数，用个 ``let`` 替换掉原先的 ``where``。

```haskell
calcBmis :: (RealFloat a) => [(a, a)] -> [a]  
calcBmis xs = [bmi | (w, h) <- xs, let bmi = w / h ^ 2]  
```

List Comprehension 中 ``let`` 绑定的样子和限制条件差不多，只不过它做的不是过滤，而是绑定名字。``let`` 中绑定的名字在输出函数及限制条件中都可见。这一来我们就可以让我们的函数只返回胖子的 ``bmi`` 值：

```haskell
calcBmis :: (RealFloat a) => [(a, a)] -> [a]  
calcBmis xs = [bmi | (w, h) <- xs, let bmi = w / h ^ 2, bmi >= 25.0] 
```

在 ``(w, h) <- xs`` 这里无法使用 ``bmi`` 这名字，因为它在 ``let`` 绑定的前面。

在 List Comprehension 中我们忽略了 ``let`` 绑定的 ``in`` 部分，因为名字的可见性已经预先定义好了。不过，把一个 ``let...in`` 放到限制条件中也是可以的，这样名字只对这个限制条件可见。在 ghci 中 ``in`` 部分也可以省略，名字的定义就在整个交互中可见。

```haskell
ghci> let zoot x y z = x * y + z  
ghci> zoot 3 9 2  
29  
ghci> let boot x y z = x * y + z in boot 3 4 2  
14  
ghci> boot  
< interactive>:1:0: Not in scope: `boot'  
```

你说既然 ``let`` 已经这么好了，还要 ``where`` 干嘛呢？嗯，``let`` 是个表达式，定义域限制的相当小，因此不能在多个 guard 中使用。一些朋友更喜欢 ``where``，因为它是跟在函数体后面，把主函数体距离型别声明近一些会更易读。




##  Case expressions

![](case.png)

有命令式程式语言 (C, C++, Java, etc.) 的经验的同学一定会有所了解，很多命令式语言都提供了 ``case`` 语句。就是取一个变数，按照对变数的判断选择对应的程式码块。其中可能会存在一个万能匹配以处理未预料的情况。

Haskell 取了这一概念融合其中。如其名，``case`` 表达式就是，嗯，一种表达式。跟 ``if..else`` 和 ``let`` 一样的表达式。用它可以对变数的不同情况分别求值，还可以使用模式匹配。Hmm，取一个变数，对它模式匹配，执行对应的程式码块。好像在哪儿听过？啊，就是函数定义时参数的模式匹配！好吧，模式匹配本质上不过就是 ``case`` 语句的语法糖而已。这两段程式码就是完全等价的：

```haskell
head' :: [a] -> a  
head' [] = error "No head for empty lists!"  
head' (x:_) = x  
```

```haskell
head' :: [a] -> a  
head' xs = case xs of [] -> error "No head for empty lists!"  
                      (x:_) -> x  
```

看得出，_case_表达式的语法十分简单：

```haskell
case expression of pattern -> result  
                   pattern -> result  
                   pattern -> result  
                   ...  
```

expression 匹配合适的模式。
一如预期地，第一个模式若匹配，就执行第一个区块的程式码；否则就接下去比对下一个模式。如果到最后依然没有匹配的模式，就会产生运行时错误。

函数参数的模式匹配只能在定义函数时使用，而 ``case`` 表达式可以用在任何地方。例如：

```haskell
describeList :: [a] -> String  
describeList xs = "The list is " ++ case xs of [] -> "empty."  
                                               [x] -> "a singleton list."   
                                               xs -> "a longer list."  
```

这在表达式中作模式匹配很方便，由于模式匹配本质上就是 ``case`` 表达式的语法糖，那么写成这样也是等价的：

```haskell
describeList :: [a] -> String  
describeList xs = "The list is " ++ what xs  
    where what [] = "empty."  
          what [x] = "a singleton list."  
          what xs = "a longer list."  
```

