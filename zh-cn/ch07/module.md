# 模块 \(Modules\)

## 装载模块

![](../../.gitbook/assets/modules%20%281%29.png)

Haskell 中的模块是含有一组相关的函数，型别和型别类的组合。而 Haskell 进程的本质便是从主模块中引用其它模块并调用其中的函数来执行操作。这样可以把代码分成多块，只要一个模块足够的独立，它里面的函数便可以被不同的进程反复重用。这就让不同的代码各司其职，提高了代码的健壮性。

Haskell 的标准库就是一组模块，每个模块都含有一组功能相近或相关的函数和型别。有处理 List 的模块，有处理并发的模块，也有处理复数的模块，等等。目前为止我们谈及的所有函数,型别以及型别类都是 `Prelude` 模块的一部分，它缺省自动装载。在本章，我们看一下几个常用的模块，在开始浏览其中的函数之前，我们先得知道如何装载模块.

在 Haskell中，装载模块的语法为 `import`，这必须得在函数的定义之前，所以一般都是将它置于代码的顶部。无疑，一段代码中可以装载很多模块，只要将 `import` 语句分行写开即可。装载 `Data.List` 试下，它里面有很多实用的 List 处理函数.

执行 `import Data.List`，这样一来 `Data.List` 中包含的所有函数就都进入了全局命名空间。也就是说，你可以在代码的任意位置调用这些函数.`Data.List` 模块中有个 `nub` 函数，它可以筛掉一个 List 中的所有重复元素。用点号将 `length` 和 `nub` 组合: `length . nub`，即可得到一个与 `(\xs -> length (nub xs))` 等价的函数。

```haskell
import Data.List  

numUniques :: (Eq a) => [a] -> Int  
numUniques = length . nub
```

你也可以在 ghci 中装载模块，若要调用 `Data.List` 中的函数，就这样:

```haskell
ghci> :m Data.List
```

若要在 ghci 中装载多个模块，不必多次 `:m` 命令，一下就可以全部搞定:

```haskell
ghci> :m Data.List Data.Map Data.Set
```

而你的进程中若已经有包含的代码，就不必再用 `:m` 了.

如果你只用得到某模块的两个函数，大可仅包含它俩。若仅装载 `Data.List` 模块 `nub` 和 `sort`，就这样:

```haskell
import Data.List (nub, sort)
```

也可以只包含除去某函数之外的其它函数，这在避免多个模块中函数的命名冲突很有用。假设我们的代码中已经有了一个叫做 `nub` 的函数，而装入 `Data.List` 模块时就要把它里面的 `nub` 除掉.

```haskell
import Data.List hiding (nub)
```

避免命名冲突还有个方法，便是 `qualified import`，`Data.Map` 模块提供一了一个按键索值的数据结构，它里面有几个和 `Prelude` 模块重名的函数。如 `filter` 和 `null`，装入 `Data.Map` 模块之后再调用 `filter`，Haskell 就不知道它究竟是哪个函数。如下便是解决的方法:

```haskell
import qualified Data.Map
```

这样一来，再调用 `Data.Map` 中的 `filter` 函数，就必须得 `Data.Map.filter`，而 `filter` 依然是为我们熟悉喜爱的样子。但是要在每个函数前面都加 `个Data.Map` 实在是太烦人了! 那就给它起个别名，让它短些:

```haskell
import qualified Data.Map as M
```

好，再调用 `Data.Map` 模块的 `filter` 函数的话仅需 `M.filter` 就行了

要浏览所有的标准库模块，参考这个手册。翻阅标准库中的模块和函数是提升个人 Haskell 水平的重要途径。你也可以各个模块的源代码，这对 Haskell 的深入学习及掌握都是大有好处的.

检索函数或搜索函数字置就用 \[[http://www.Haskell.org/hoogle/](http://www.Haskell.org/hoogle/) Hoogle\]，相当了不起的 Haskell 搜索引擎! 你可以用函数名，模块名甚至型别声明来作为检索的条件.

## Data.List

显而易见，`Data.List` 是关于 List 操作的模块，它提供了一组非常有用的 List 处理函数。在前面我们已经见过了其中的几个函数\(如 `map` 和 `filter`\)，这是 `Prelude` 模块出于方便起见，导出了几个 `Data.List` 里的函数。因为这几个函数是直接引用自 `Data.List`，所以就无需使用 `qualified import`。在下面，我们来看看几个以前没见过的函数:

**intersperse** 取一个元素与 List 作参数，并将该元素置于 List 中每对元素的中间。如下是个例子:

```haskell
ghci> intersperse '.' "MONKEY"  
"M.O.N.K.E.Y"  
ghci> intersperse 0 [1,2,3,4,5,6]  
[1,0,2,0,3,0,4,0,5,0,6]
```

**intercalate** 取两个 List 作参数。它会将第一个 List 交叉插入第二个 List 中间，并返回一个 List.

```haskell
ghci> intercalate " " ["hey","there","guys"]  
"hey there guys"  
ghci> intercalate [0,0,0] [[1,2,3],[4,5,6],[7,8,9]]  
[1,2,3,0,0,0,4,5,6,0,0,0,7,8,9]
```

**transpose** 函数可以反转一组 List 的 List。你若把一组 List 的 List 看作是个 2D 的矩阵，那 `transpose` 的操作就是将其列为行。

```haskell
ghci> transpose [[1,2,3],[4,5,6],[7,8,9]]  
[[1,4,7],[2,5,8],[3,6,9]]  
ghci> transpose ["hey","there","guys"]  
["htg","ehu","yey","rs","e"]
```

假如有两个多项式 3x<sup>2</sup> + 5x + 9，10x<sup>3</sup> + 9 和 8x<sup>3</sup> + 5x<sup>2</sup> + x - 1，将其相加，我们可以列三个 List: `[0,3,5,9]`，`[10,0,0,9]` 和 `[8,5,1,-1]` 来表示。再用如下的方法取得结果.

```haskell
ghci> map sum $ transpose [[0,3,5,9],[10,0,0,9],[8,5,1,-1]]  
[18,8,6,17]
```

![](../../.gitbook/assets/legolists.png)

使用 `transpose` 处理这三个 List 之后，三次幂就到了第一行，二次幂到了第二行，以此类推。在用 `sum` 函数将其映射，即可得到正确的结果。

**foldl'** 和 **foldl1'** 是它们各自惰性实现的严格版本。在用 `fold` 处理较大的 List 时，经常会遇到堆栈溢出的问题。而这罪魁祸首就是 `fold` 的惰性: 在执行 `fold` 时，累加器的值并不会被立即更新，而是做一个"在必要时会取得所需的结果"的承诺。每过一遍累加器，这一行为就重复一次。而所有的这堆"承诺"最终就会塞满你的堆栈。严格的 `fold` 就不会有这一问题，它们不会作"承诺"，而是直接计算中间值的结果并继续执行下去。如果用惰性 `fold` 时经常遇到溢出错误，就应换用它们的严格版。

**concat** 把一组 List 连接为一个 List。

```haskell
ghci> concat ["foo","bar","car"]  
"foobarcar"  
ghci> concat [[3,4,5],[2,3,4],[2,1,1]]  
[3,4,5,2,3,4,2,1,1]
```

它相当于移除一级嵌套。若要彻底地连接其中的元素，你得 `concat` 它两次才行.

**concatMap** 函数与 `map` 一个 List 之后再 `concat` 它等价.

```haskell
ghci> concatMap (replicate 4) [1..3]  
[1,1,1,1,2,2,2,2,3,3,3,3]
```

**and** 取一组布尔值 List 作参数。只有其中的值全为 `True` 的情况下才会返回 `True`。

```haskell
ghci> and $ map (>4) [5,6,7,8]  
True  
ghci> and $ map (==4) [4,4,4,3,4]  
False
```

**or** 与 `and` 相似，一组布尔值 List 中若存在一个 `True` 它就返回 `True`.

```haskell
ghci> or $ map (==4) [2,3,4,5,6,1]  
True  
ghci> or $ map (>4) [1,2,3]  
False
```

**any** 和 **all** 取一个限制条件和一组布尔值 List 作参数，检查是否该 List 的某个元素或每个元素都符合该条件。通常较 `map` 一个 List 到 `and` 或 `or` 而言，使用 `any` 或 `all` 会更多些。

```haskell
ghci> any (==4) [2,3,5,6,1,4]  
True  
ghci> all (>4) [6,9,10]  
True  
ghci> all (`elem` ['A'..'Z']) "HEYGUYSwhatsup"  
False  
ghci> any (`elem` ['A'..'Z']) "HEYGUYSwhatsup"  
True
```

**iterate** 取一个函数和一个值作参数。它会用该值去调用该函数并用所得的结果再次调用该函数，产生一个无限的 List.

```haskell
ghci> take 10 $ iterate (*2) 1  
[1,2,4,8,16,32,64,128,256,512]  
ghci> take 3 $ iterate (++ "haha") "haha"  
["haha","hahahaha","hahahahahaha"]
```

**splitAt** 取一个 List 和数值作参数，将该 List 在特定的位置断开。返回一个包含两个 List 的二元组.

```haskell
ghci> splitAt 3 "heyman"  
("hey","man")  
ghci> splitAt 100 "heyman"  
("heyman","")  
ghci> splitAt (-3) "heyman"  
("","heyman")  
ghci> let (a,b) = splitAt 3 "foobar" in b ++ a  
"barfoo"
```

**takeWhile** 这一函数十分的实用。它从一个 List 中取元素，一旦遇到不符合条件的某元素就停止.

```haskell
ghci> takeWhile (>3) [6,5,4,3,2,1,2,3,4,5,4,3,2,1]  
[6,5,4]  
ghci> takeWhile (/=' ') "This is a sentence"  
"This"
```

如果要求所有三次方小于 10000 的数的和，用 `filter` 来过滤 `map (^3) [1..]` 所得结果中所有小于 10000 的数是不行的。因为对无限 List 执行的 `filter` 永远都不会停止。你已经知道了这个 List 是单增的，但 Haskell 不知道。所以应该这样：

```haskell
ghci> sum $ takeWhile (<10000) $ map (^3) [1..]  
53361
```

用 `(^3)` 处理一个无限 List，而一旦出现了大于等于 10000 的元素这个 List 就被切断了，sum 到一起也就轻而易举.

**dropWhile** 与此相似，不过它是扔掉符合条件的元素。一旦限制条件返回 `False`，它就返回 List 的余下部分。方便实用!

```haskell
ghci> dropWhile (/=' ') "This is a sentence"  
" is a sentence"  
ghci> dropWhile (<3) [1,2,2,2,3,4,5,4,3,2,1]  
[3,4,5,4,3,2,1]
```

给一 `Tuple` 组成的 List，这 Tuple 的首项表示股票价格，第二三四项分别表示年,月,日。我们想知道它是在哪天首次突破 $1000 的!

```haskell
ghci> let stock = [(994.4,2008,9,1),(995.2,2008,9,2),(999.2,2008,9,3),(1001.4,2008,9,4),(998.3,2008,9,5)]  
ghci> head (dropWhile (\(val,y,m,d) -> val < 1000) stock)  
(1001.4,2008,9,4)
```

**span** 与 `takeWhile` 有点像，只是它返回两个 List。第一个 List 与同参数调用 `takeWhile` 所得的结果相同，第二个 List 就是原 List 中余下的部分。

```haskell
ghci> let (fw, rest) = span (/=' ') "This is a sentence" in "First word:" ++ fw ++ ", the rest:" ++ rest  
"First word: This, the rest: is a sentence"
```

**span** 是在条件首次为 `False` 时断开 List，而 `break` 则是在条件首次为 `True` 时断开 `List`。`break p` 与 `span (not . p)` 是等价的.

```haskell
ghci> break (==4) [1,2,3,4,5,6,7]  
([1,2,3],[4,5,6,7])  
ghci> span (/=4) [1,2,3,4,5,6,7]  
([1,2,3],[4,5,6,7])
```

**break** 返回的第二个 List 就会以第一个符合条件的元素开头。

**sort** 可以排序一个 List，因为只有能够作比较的元素才可以被排序，所以这一 List 的元素必须是 Ord 型别类的实例型别。

```haskell
ghci> sort [8,5,3,2,1,6,4,2]  
[1,2,2,3,4,5,6,8]  
ghci> sort "This will be sorted soon"  
" Tbdeehiillnooorssstw"
```

**group** 取一个 List 作参数，并将其中相邻并相等的元素各自归类，组成一个个子 List.

```haskell
ghci> group [1,1,1,1,2,2,2,2,3,3,2,2,2,5,6,7]  
[[1,1,1,1],[2,2,2,2],[3,3],[2,2,2],[5],[6],[7]]
```

若在 `group` 一个 List 之前给它排序就可以得到每个元素在该 List 中的出现次数。

```haskell
ghci> map (\l@(x:xs) -> (x,length l)) . group . sort $ [1,1,1,1,2,2,2,2,3,3,2,2,2,5,6,7]  
[(1,4),(2,7),(3,2),(5,1),(6,1),(7,1)]
```

**inits** 和 **tails** 与 `init` 和 `tail` 相似，只是它们会递归地调用自身直到什么都不剩，看:

```haskell
ghci> inits "w00t"  
["","w","w0","w00","w00t"]  
ghci> tails "w00t"  
["w00t","00t","0t","t",""]  
ghci> let w = "w00t" in zip (inits w) (tails w)  
[("","w00t"),("w","00t"),("w0","0t"),("w00","t"),("w00t","")]
```

我们用 `fold` 实现一个搜索子 List 的函数:

```haskell
search :: (Eq a) => [a] -> [a] -> Bool  
search needle haystack =  
  let nlen = length needle  
  in foldl (\acc x -> if take nlen x == needle then True else acc) False (tails haystack)
```

首先，对搜索的 List 调用 `tails`，然后遍历每个 List 来检查它是不是我们想要的.

由此我们便实现了一个类似 **isInfixOf** 的函数，**isInfixOf** 从一个 List 中搜索一个子 List，若该 List 包含子 List，则返回 `True`.

```haskell
ghci> "cat" `isInfixOf` "im a cat burglar"  
True  
ghci> "Cat" `isInfixOf` "im a cat burglar"  
False  
ghci> "cats" `isInfixOf` "im a cat burglar"  
False
```

**isPrefixOf** 与 **isSuffixOf** 分别检查一个 List 是否以某子 List 开头或者结尾.

```haskell
ghci> "hey" `isPrefixOf` "hey there!"  
True  
ghci> "hey" `isPrefixOf` "oh hey there!"  
False  
ghci> "there!" `isSuffixOf` "oh hey there!"  
True  
ghci> "there!" `isSuffixOf` "oh hey there"  
False
```

**elem** 与 **notElem** 检查一个 List 是否包含某元素.

**partition** 取一个限制条件和 List 作参数，返回两个 List，第一个 List 中包含所有符合条件的元素，而第二个 List 中包含余下的.

```haskell
ghci> partition (`elem` ['A'..'Z']) "BOBsidneyMORGANeddy"  
("BOBMORGAN","sidneyeddy")  
ghci> partition (>3) [1,3,5,6,3,2,1,0,3,7]  
([5,6,7],[1,3,3,2,1,0,3])
```

了解这个与 `span` 和 `break` 的差异是很重要的.

```haskell
ghci> span (`elem` ['A'..'Z']) "BOBsidneyMORGANeddy"  
("BOB","sidneyMORGANeddy")
```

`span` 和 `break` 会在遇到第一个符合或不符合条件的元素处断开，而 `partition` 则会遍历整个 List。

**find** 取一个 List 和限制条件作参数，并返回首个符合该条件的元素，而这个元素是个 `Maybe` 值。在下章，我们将深入地探讨相关的算法和数据结构，但在这里你只需了解 `Maybe` 值是 `Just something` 或 `Nothing` 就够了。与一个 List 可以为空也可以包含多个元素相似，一个 `Maybe` 可以为空，也可以是单一元素。同样与 List 类似，一个 Int 型的 List 可以写作 `[Int]`，`Maybe`有个 Int 型可以写作 `Maybe Int`。先试一下 `find` 函数再说.

```haskell
ghci> find (>4) [1,2,3,4,5,6]  
Just 5  
ghci> find (>9) [1,2,3,4,5,6]  
Nothing  
ghci> :t find  
find :: (a -> Bool) -> [a] -> Maybe a
```

注意一下 `find` 的型别，它的返回结果为 `Maybe a`，这与 `[a]` 的写法有点像，只是 `Maybe` 型的值只能为空或者单一元素，而 List 可以为空,一个元素，也可以是多个元素.

想想前面那段找股票的代码，`head (dropWhile (\(val,y,m,d) -> val < 1000) stock)` 。但 `head` 并不安全! 如果我们的股票没涨过 $1000 会怎样? `dropWhile` 会返回一个空 List，而对空 List 取 `head` 就会引发一个错误。把它改成 `find (\(val,y,m,d) -> val > 1000) stock` 就安全多啦，若存在合适的结果就得到它, 像 `Just (1001.4,2008,9,4)`，若不存在合适的元素\(即我们的股票没有涨到过 $1000\)，就会得到一个 `Nothing`.

**elemIndex** 与 `elem` 相似，只是它返回的不是布尔值，它只是'可能' \(Maybe\)返回我们找的元素的索引，若这一元素不存在，就返回 `Nothing`。

```haskell
ghci> :t elemIndex  
elemIndex :: (Eq a) => a -> [a] -> Maybe Int  
ghci> 4 `elemIndex` [1,2,3,4,5,6]  
Just 3  
ghci> 10 `elemIndex` [1,2,3,4,5,6]  
Nothing
```

**elemIndices** 与 `elemIndex` 相似，只不过它返回的是 List，就不需要 `Maybe` 了。因为不存在用空 List 就可以表示，这就与 `Nothing` 相似了.

```haskell
ghci> ' ' `elemIndices` "Where are the spaces?"  
[5,9,13]
```

**findIndex** 与 `find` 相似，但它返回的是可能存在的首个符合该条件元素的索引。**findIndices** 会返回所有符合条件的索引.

```haskell
ghci> findIndex (==4) [5,3,2,1,6,4]  
Just 5  
ghci> findIndex (==7) [5,3,2,1,6,4]  
Nothing  
ghci> findIndices (`elem` ['A'..'Z']) "Where Are The Caps?"  
[0,6,10,14]
```

在前面，我们讲过了 `zip` 和 `zipWith`，它们只能将两个 List 组到一个二元组数或二参函数中，但若要组三个 List 该怎么办? 好说~ 有 `zip3`,`zip4`...,和 `zipWith3`, `zipWith4`...直到 7。这看起来像是个 hack，但工作良好。连着组 8 个 List 的情况很少遇到。还有个聪明办法可以组起无限多个 List，但限于我们目前的水平，就先不谈了.

```haskell
ghci> zipWith3 (\x y z -> x + y + z) [1,2,3] [4,5,2,2] [2,2,3]  
[7,9,8]  
ghci> zip4 [2,3,3] [2,2,2] [5,5,3] [2,2,2]  
[(2,2,5,2),(3,2,5,2),(3,2,3,2)]
```

与普通的 `zip` 操作相似，以返回的 List 中长度最短的那个为准.

在处理来自文件或其它地方的输入时，**lines** 会非常有用。它取一个字串作参数。并返回由其中的每一行组成的 List.

```haskell
ghci> lines "first line\nsecond line\nthird line"  
["first line","second line","third line"]
```

`'\n'` 表示unix下的换行符，在 Haskell 的字符中，反斜杠表示特殊字符.

**unlines** 是 `lines` 的反函数，它取一组字串的 List，并将其通过 `'\n'`合并到一块.

```haskell
ghci> unlines ["first line", "second line", "third line"]  
"first line\nsecond line\nthird line\n"
```

**words** 和 **unwords** 可以把一个字串分为一组单词或执行相反的操作，很有用.

```haskell
ghci> words "hey these are the words in this sentence"  
["hey","these","are","the","words","in","this","sentence"]  
ghci> words "hey these are the words in this\nsentence"  
["hey","these","are","the","words","in","this","sentence"]  
ghci> unwords ["hey","there","mate"]  
"hey there mate"
```

我们前面讲到了 **nub**，它可以将一个 List 中的重复元素全部筛掉，使该 List 的每个元素都如雪花般独一无二，'nub' 的含义就是'一小块'或'一部分'，用在这里觉得很古怪。我觉得，在函数的命名上应该用更确切的词语，而避免使用老掉牙的过时词汇.

```haskell
ghci> nub [1,2,3,4,3,2,1,2,3,4,3,2,1]  
[1,2,3,4]  
ghci> nub "Lots of words and stuff"  
"Lots fwrdanu"
```

**delete** 取一个元素和 List 作参数，会删掉该 List 中首次出现的这一元素.

```haskell
ghci> delete 'h' "hey there ghang!"  
"ey there ghang!"  
ghci> delete 'h' . delete 'h' $ "hey there ghang!"  
"ey tere ghang!"  
ghci> delete 'h' . delete 'h' . delete 'h' $ "hey there ghang!"  
"ey tere gang!"
```

**\\** 表示 List 的差集操作，这与集合的差集很相似，它会从左边 List 中的元素扣除存在于右边 List 中的元素一次.

```haskell
ghci> [1..10] \\ [2,5,9]  
[1,3,4,6,7,8,10]  
ghci> "Im a big baby" \\ "big"  
"Im a  baby"
```

**union** 与集合的并集也是很相似，它返回两个 List 的并集，即遍历第二个 List 若存在某元素不属于第一个 List，则追加到第一个 List。看，第二个 List 中的重复元素就都没了!

```haskell
ghci> "hey man" `union` "man what's up"  
"hey manwt'sup"  
ghci> [1..7] `union` [5..10]  
[1,2,3,4,5,6,7,8,9,10]
```

**intersect** 相当于集合的交集。它返回两个 List 的相同部分.

```haskell
ghci> [1..7] `intersect` [5..10]  
[5,6,7]
```

**insert** 可以将一个元素插入一个可排序的 List，并将其置于首个大于等于它的元素之前，如果使用 `insert` 来给一个排过序的 List 插入元素，返回的结果依然是排序的.

```haskell
ghci> insert 4 [1,2,3,5,6,7]  
[1,2,3,4,5,6,7]  
ghci> insert 'g' $ ['a'..'f'] ++ ['h'..'z']  
"abcdefghijklmnopqrstuvwxyz"  
ghci> insert 3 [1,2,4,3,2,1]  
[1,2,3,4,3,2,1]
```

`length`，`take`，`drop`，`splitAt`，`!!` 和 `replicate` 之类的函数有个共同点。那就是它们的参数中都有个 Int 值（或者返回Int值），我觉得使用 Intergal 或 Num 型别类会更好，但出于历史原因，修改这些会破坏掉许多既有的代码。在 `Data.List` 中包含了更通用的替代版，如: `genericLength，genericTake，genericDrop，genericSplitAt，genericIndex` 和 `genericReplicate`。`length` 的型别声明为 `length :: [a] -> Int`，而我们若要像这样求它的平均值，`let xs = [1..6] in sum xs / length xs` ，就会得到一个型别错误，因为 `/` 运算符不能对 Int 型使用! 而 `genericLength` 的型别声明则为 `genericLength :: (Num a) => [b] -> a`，Num 既可以是整数又可以是浮点数，`let xs = [1..6] in sum xs / genericLength xs` 这样再求平均数就不会有问题了.

`nub`, `delete`, `union`, `intsect` 和 `group` 函数也有各自的通用替代版 `nubBy`，`deleteBy`，`unionBy`，`intersectBy` 和 `groupBy`，它们的区别就是前一组函数使用 `(==)` 来测试是否相等，而带 `By` 的那组则取一个函数作参数来判定相等性，`group` 就与 `groupBy (==)` 等价.

假如有个记录某函数在每秒的值的 List，而我们要按照它小于零或者大于零的交界处将其分为一组子 List。如果用 `group`，它只能将相邻并相等的元素组到一起，而在这里我们的标准是它们是否互为相反数。`groupBy` 登场! 它取一个含两个参数的函数作为参数来判定相等性.

```haskell
ghci> let values = [-4.3,-2.4,-1.2,0.4,2.3,5.9,10.5,29.1,5.3,-2.4,-14.5,2.9,2.3]  
ghci> groupBy (\x y -> (x > 0) == (y > 0)) values  
[[-4.3,-2.4,-1.2],[0.4,2.3,5.9,10.5,29.1,5.3],[-2.4,-14.5],[2.9,2.3]]
```

这样一来我们就可以很清楚地看出哪部分是正数，哪部分是负数，这个判断相等性的函数会在两个元素同时大于零或同时小于零时返回 `True`。也可以写作 `\x y -> (x > 0) && (y > 0) || (x <= 0) && (y <= 0)`。但我觉得第一个写法的可读性更高。`Data.Function` 中还有个 `on` 函数可以让它的表达更清晰，其定义如下:

```haskell
on :: (b -> b -> c) -> (a -> b) -> a -> a -> c  
f `on` g = \x y -> f (g x) (g y)
```

执行 ``(==) `on` (> 0)`` 得到的函数就与 `\x y -> (x > 0) == (y > 0)` 基本等价。`on` 与带 `By` 的函数在一起会非常好用，你可以这样写:

```haskell
ghci> groupBy ((==) `on` (> 0)) values  
[[-4.3,-2.4,-1.2],[0.4,2.3,5.9,10.5,29.1,5.3],[-2.4,-14.5],[2.9,2.3]]
```

可读性很高! 你可以大声念出来: 按照元素是否大于零，给它分类！

同样，`sort`，`insert`，`maximum` 和 `min` 都有各自的通用版本。如 `groupBy` 类似，**sortBy**，**insertBy**，**maximumBy** 和 **minimumBy** 都取一个函数来比较两个元素的大小。像 `sortBy` 的型别声明为: `sortBy :: (a -> a -> Ordering) -> [a] -> [a]`。前面提过，`Ordering` 型别可以有三个值,`LT`，`EQ` 和 `GT`。`compare` 取两个 `Ord` 型别类的元素作参数，所以 `sort` 与 `sortBy compare` 等价.

List 是可以比较大小的，且比较的依据就是其中元素的大小。如果按照其子 List 的长度为标准当如何? 很好，你可能已经猜到了，`sortBy` 函数.

```haskell
ghci> let xs = [[5,4,5,4,4],[1,2,3],[3,5,4,3],[],[2],[2,2]]  
ghci> sortBy (compare `on` length) xs  
[[],[2],[2,2],[1,2,3],[3,5,4,3],[5,4,5,4,4]]
```

太绝了! ``compare `on` length``，乖乖，这简直就是英文! 如果你搞不清楚 `on` 在这里的原理，就可以认为它与 ``\x y -> length x `compare` length y`` 等价。通常，与带 `By` 的函数打交道时，若要判断相等性，则 ``(==) `on` something``。若要判定大小，则 ``compare `on` something``.

## Data.Char

如其名，`Data.Char` 模块包含了一组用于处理字符的函数。由于字串的本质就是一组字符的 List，所以往往会在 `filter` 或是 `map` 字串时用到它.

`Data.Char`模块中含有一系列用于判定字符范围的函数，如下:

![](../../.gitbook/assets/legochar.png)

**isControl** 判断一个字符是否是控制字符。 **isSpace** 判断一个字符是否是空格字符，包括空格，tab，换行符等. **isLower** 判断一个字符是否为小写. **isUper** 判断一个字符是否为大写。 **isAlpha** 判断一个字符是否为字母. **isAlphaNum** 判断一个字符是否为字母或数字. **isPrint** 判断一个字符是否是可打印的. **isDigit** 判断一个字符是否为数字. **isOctDigit** 判断一个字符是否为八进制数字. **isHexDigit** 判断一个字符是否为十六进制数字. **isLetter** 判断一个字符是否为字母. **isMark** 判断是否为 unicode 注音字符，你如果是法国人就会经常用到的. **isNumber** 判断一个字符是否为数字. **isPunctuation** 判断一个字符是否为标点符号. **isSymbol**判断一个字符是否为货币符号. **isSeperater** 判断一个字符是否为 unicode 空格或分隔符. **isAscii** 判断一个字符是否在 unicode 字母表的前 128 位。 **isLatin1** 判断一个字符是否在 unicode 字母表的前 256 位. **isAsciiUpper** 判断一个字符是否为大写的 ascii 字符. **isAsciiLower** 判断一个字符是否为小写的 ascii 字符.

以上所有判断函数的型别声明皆为 `Char -> Bool`，用到它们的绝大多数情况都无非就是过滤字串或类似操作。假设我们在写个进程，它需要一个由字符和数字组成的用户名。要实现对用户名的检验，我们可以结合使用 `Data.List` 模块的 `all` 函数与 `Data.Char` 的判断函数.

```haskell
ghci> all isAlphaNum "bobby283"  
True  
ghci> all isAlphaNum "eddy the fish!"  
False
```

Kewl~ 免得你忘记，`all` 函数取一个判断函数和一个 List 做参数，若该 List 的所有元素都符合条件，就返回 `True`.

也可以使用 `isSpace` 来实现 `Data.List` 的 `words` 函数.

```haskell
ghci> words "hey guys its me"  
["hey","guys","its","me"]  
ghci> groupBy ((==) `on` isSpace) "hey guys its me"  
["hey"," ","guys"," ","its"," ","me"]  
ghci>
```

Hmm，不错，有点 `words` 的样子了。只是还有空格在里面，恩，该怎么办? 我知道，用 `filter` 滤掉它们!

```haskell
ghci> filter (not . any isSpace) . groupBy ((==) `on` isSpace) $ "hey guys its me"  
["hey","guys","its","me"]
```

啊哈.

`Data.Char` 中也含有与 `Ordering` 相似的型别。`Ordering` 可以有三个值，`LT`，`GT` 和 `EQ`。这就是个枚举，它表示了两个元素作比较可能的结果. `GeneralCategory` 型别也是个枚举，它表示了一个字符可能所在的分类。而得到一个字符所在分类的主要方法就是使用 `generalCategory` 函数.它的型别为: `generalCategory :: Char -> GeneralCategory`。那 31 个分类就不在此一一列出了，试下这个函数先:

```haskell
ghci> generalCategory ' '  
Space  
ghci> generalCategory 'A'  
UppercaseLetter  
ghci> generalCategory 'a'  
LowercaseLetter  
ghci> generalCategory '.'  
OtherPunctuation  
ghci> generalCategory '9'  
DecimalNumber  
ghci> map generalCategory " \t\nA9?|"  
[Space,Control,Control,UppercaseLetter,DecimalNumber,OtherPunctuation,MathSymbol]
```

由于 `GeneralCategory` 型别是 `Eq` 型别类的一部分，使用类似 `generalCategory c == Space` 的代码也是可以的.

**toUpper** 将一个字符转为大写字母，若该字符不是小写字母，就按原值返回. **toLower** 将一个字符转为小写字母，若该字符不是大写字母，就按原值返回. **toTitle** 将一个字符转为 title-case，对大多数字元而言，title-case 就是大写. **digitToInt** 将一个字符转为 Int 值，而这一字符必须得在 `'1'..'9','a'..'f'`或`'A'..'F'` 的范围之内.

```haskell
ghci> map digitToInt "34538"  
[3,4,5,3,8]  
ghci> map digitToInt "FF85AB"  
[15,15,8,5,10,11]
```

`intToDigit` 是 `digitToInt` 的反函数。它取一个 `0` 到 `15` 的 `Int` 值作参数，并返回一个小写的字符.

```haskell
ghci> intToDigit 15  
'f'  
ghci> intToDigit 5  
'5'
```

**ord** 与 **char** 函数可以将字符与其对应的数字相互转换.

```haskell
ghci> ord 'a'  
97  
ghci> chr 97  
'a'  
ghci> map ord "abcdefgh"  
[97,98,99,100,101,102,103,104]
```

两个字符的 `ord` 值之差就是它们在 unicode 字符表上的距离.

_Caesar ciphar_ 是加密的基础算法，它将消息中的每个字符都按照特定的字母表进行替换。它的实现非常简单，我们这里就先不管字母表了.

```haskell
encode :: Int -> String -> String  
encode shift msg = 
  let ords = map ord msg  
      shifted = map (+ shift) ords  
  in map chr shifted
```

先将一个字串转为一组数字，然后给它加上某数，再转回去。如果你是标准的组合牛仔，大可将函数写为: `map (chr . (+ shift) . ord) msg`。试一下它的效果:

```haskell
ghci> encode 3 "Heeeeey"  
"Khhhhh|"  
ghci> encode 4 "Heeeeey"  
"Liiiii}"  
ghci> encode 1 "abcd"  
"bcde"  
ghci> encode 5 "Marry Christmas! Ho ho ho!"  
"Rfww~%Hmwnxyrfx&%Mt%mt%mt&"
```

不错。再简单地将它转成一组数字，减去某数后再转回来就是解密了.

```haskell
decode :: Int -> String -> String  
decode shift msg = encode (negate shift) msg
```

```haskell
ghci> encode 3 "Im a little teapot"  
"Lp#d#olwwoh#whdsrw"  
ghci> decode 3 "Lp#d#olwwoh#whdsrw"  
"Im a little teapot"  
ghci> decode 5 . encode 5 $ "This is a sentence"  
"This is a sentence"
```

## Data.Map

关联列表\(也叫做字典\)是按照键值对排列而没有特定顺序的一种 List。例如，我们用关联列表保存电话号码，号码就是值，人名就是键。我们并不关心它们的存储顺序，只要能按人名得到正确的号码就好.在 Haskell 中表示关联列表的最简单方法就是弄一个二元组的 List，而这二元组就首项为键，后项为值。如下便是个表示电话号码的关联列表:

```haskell
phoneBook = [("betty","555-2938") ,
             ("bonnie","452-2928") ,
             ("patsy","493-2928") ,
             ("lucille","205-2928") ,
             ("wendy","939-8282") ,
             ("penny","853-2492") ]
```

不理这貌似古怪的缩进，它就是一组二元组的 List 而已。话说对关联列表最常见的操作就是按键索值，我们就写个函数来实现它。

```haskell
findKey :: (Eq k) => k -> [(k,v)] -> v 
findKey key xs = snd . head . filter (\(k,v) -> key == k) $ xs
```

![](../../.gitbook/assets/legomap.png)

简洁漂亮。这个函数取一个键和 List 做参数，过滤这一 List 仅保留键匹配的项，并返回首个键值对。但若该关联列表中不存在这个键那会怎样? 哼，那就会在试图从空 List 中取 `head` 时引发一个运行时错误。无论如何也不能让进程就这么轻易地崩溃吧，所以就应该用 `Maybe` 型别。如果没找到相应的键，就返回 `Nothing`。而找到了就返回 `Just something`。而这 `something` 就是键对应的值。

```haskell
findKey :: (Eq k) => k -> [(k,v)] -> Maybe v 
findKey key [] = Nothing
findKey key ((k,v):xs) = 
     if key == k then 
         Just v 
     else 
         findKey key xs
```

看这型别声明，它取一个可判断相等性的键和一个关联列表做参数，可能 \(Maybe\) 得到一个值。听起来不错.这便是个标准的处理 List 的递归函数，边界条件，分割 List，递归调用，都有了 -- 经典的 `fold` 模式。 看看用 `fold` 怎样实现吧。

```haskell
findKey :: (Eq k) => k -> [(k,v)] -> Maybe v 
findKey key = foldr (\(k,v) acc -> if key == k then Just v else acc) Nothing
```

```text
*Note*: 通常，使用 ``fold`` 来替代类似的递归函数会更好些。用 ``fold`` 的代码让人一目了然，而看明白递归则得多花点脑子。
```

```haskell
ghci> findKey "penny" phoneBook 
Just "853-2492" 
ghci> findKey "betty" phoneBook 
Just "555-2938" 
ghci> findKey "wilma" phoneBook 
Nothing
```

如魔咒般灵验! 只要我们有这姑娘的号码就 `Just` 可以得到，否则就是 `Nothing`. 方才我们实现的函数便是 `Data.List` 模块的 `lookup`，如果要按键去寻找相应的值，它就必须得遍历整个 List，直到找到为止。而 `Data.Map` 模块提供了更高效的方式\(通过树实现\)，并提供了一组好用的函数。从现在开始，我们扔掉关联列表，改用map.由于`Data.Map`中的一些函数与Prelude和`Data.List` 模块存在命名冲突，所以我们使用 `qualified import`。`import qualified Data.Map as Map` 在代码中加上这句，并 `load` 到 ghci 中.继续前进，看看 `Data.Map` 是如何的一座宝库! 如下便是其中函数的一瞥:

**fromList** 取一个关联列表，返回一个与之等价的 Map。

```haskell
ghci> Map.fromList [("betty","555-2938"),("bonnie","452-2928"),("lucille","205-2928")] 
fromList [("betty","555-2938"),("bonnie","452-2928"),("lucille","205-2928")] 
ghci> Map.fromList [(1,2),(3,4),(3,2),(5,5)] 
fromList [(1,2),(3,2),(5,5)]
```

若其中存在重复的键,就将其忽略。如下即 `fromList` 的型别声明。

```haskell
Map.fromList :: (Ord k) => [(k,v)] -> Map.Map k v
```

这表示它取一组键值对的 List，并返回一个将 `k` 映射为 `v` 的 `map`。注意一下，当使用普通的关联列表时，只需要键的可判断相等性就行了。而在这里，它还必须得是可排序的。这在 `Data.Map` 模块中是强制的。因为它会按照某顺序将其组织在一棵树中.在处理键值对时，只要键的型别属于 `Ord` 型别类，就应该尽量使用`Data.Map`.`empty` 返回一个空 `map`.

```haskell
ghci> Map.empty 
fromList []
```

**insert** 取一个键，一个值和一个 `map` 做参数，给这个 `map` 插入新的键值对，并返回一个新的 `map`。

```haskell
ghci> Map.empty 
fromList [] 
ghci> Map.insert 3 100 Map.empty
fromList [(3,100)] 
ghci> Map.insert 5 600 (Map.insert 4 200 ( Map.insert 3 100  Map.empty)) 
fromList [(3,100),(4,200),(5,600)]
ghci> Map.insert 5 600 . Map.insert 4 200 . Map.insert 3 100 $ Map.empty 
fromList [(3,100),(4,200),(5,600)]
```

通过 `empty`，`insert` 与 `fold`，我们可以编写出自己的 `fromList`。

```haskell
fromList' :: (Ord k) => [(k,v)] -> Map.Map k v 
fromList' = foldr (\(k,v) acc -> Map.insert k v acc) Map.empty
```

简洁明了的 `fold`！ 从一个空的 `map` 开始，然后从右折叠，随着遍历不断地往 `map` 中插入新的键值对.

**null** 检查一个 `map` 是否为空.

```haskell
ghci> Map.null Map.empty 
True 
ghci> Map.null $ Map.fromList [(2,3),(5,5)] 
False
```

**size** 返回一个 `map` 的大小。

```haskell
ghci> Map.size Map.empty 
0 
ghci> Map.size $ Map.fromList [(2,4),(3,3),(4,2),(5,4),(6,4)] 
5
```

**singleton** 取一个键值对做参数,并返回一个只含有一个映射的 `map`.

```haskell
ghci> Map.singleton 3 9 
fromList [(3,9)] 
ghci> Map.insert 5 9 $ Map.singleton 3 9 
fromList [(3,9),(5,9)]
```

**lookup** 与 `Data.List` 的 `lookup` 很像,只是它的作用对象是 `map`，如果它找到键对应的值。就返回 `Just something`，否则返回 `Nothing`。

**member** 是个判断函数，它取一个键与 `map` 做参数，并返回该键是否存在于该 `map`。

```haskell
ghci> Map.member 3 $ Map.fromList [(3,6),(4,3),(6,9)] 
True 
ghci> Map.member 3 $ Map.fromList [(2,5),(4,5)] 
False
```

**map** 与 **filter** 与其对应的 `List` 版本很相似:

```haskell
ghci> Map.map (*100) $ Map.fromList [(1,1),(2,4),(3,9)] 
fromList [(1,100),(2,400),(3,900)] 
ghci> Map.filter isUpper $ Map.fromList [(1,'a'),(2,'A'),(3,'b'),(4,'B')] 
fromList [(2,'A'),(4,'B')]
```

`toList` 是 `fromList` 的反函数。

```haskell
ghci> Map.toList . Map.insert 9 2 $ Map.singleton 4 3 
[(4,3),(9,2)]
```

**keys** 与 **elems** 各自返回一组由键或值组成的 List，`keys` 与 `map fst . Map.toList` 等价，`elems` 与 `map snd . Map.toList`等价. `fromListWith` 是个很酷的小函数，它与 `fromList` 很像，只是它不会直接忽略掉重复键，而是交给一个函数来处理它们。假设一个姑娘可以有多个号码，而我们有个像这样的关联列表:

```haskell
phoneBook =   
    [("betty","555-2938")  
    ,("betty","342-2492")  
    ,("bonnie","452-2928")  
    ,("patsy","493-2928")  
    ,("patsy","943-2929")  
    ,("patsy","827-9162")  
    ,("lucille","205-2928")  
    ,("wendy","939-8282")  
    ,("penny","853-2492")  
    ,("penny","555-2111")  
    ]
```

如果用 `fromList` 来生成 `map`，我们会丢掉许多号码! 如下才是正确的做法:

```haskell
phoneBookToMap :: (Ord k) => [(k, String)] -> Map.Map k String  
phoneBookToMap xs = Map.fromListWith (\number1 number2 -> number1 ++ ", " ++ number2) xs
```

```haskell
ghci> Map.lookup "patsy" $ phoneBookToMap phoneBook 
"827-9162, 943-2929, 493-2928" 
ghci> Map.lookup "wendy" $ phoneBookToMap phoneBook
"939-8282" 
ghci> Map.lookup "betty" $ phoneBookToMap phoneBook 
"342-2492,555-2938"
```

一旦出现重复键，这个函数会将不同的值组在一起，同样，也可以缺省地将每个值放到一个单元素的 List 中，再用 `++` 将他们都连接在一起。

```haskell
phoneBookToMap :: (Ord k) => [(k,a)] -> Map.Map k [a] 
phoneBookToMap xs = Map.fromListWith (++) $ map (\(k,v) -> (k,[v])) xs 
ghci> Map.lookup "patsy" $ phoneBookToMap phoneBook 
["827-9162","943-2929","493-2928"]
```

很简洁! 它还有别的玩法，例如在遇到重复元素时，单选最大的那个值.

```haskell
ghci> Map.fromListWith max [(2,3),(2,5),(2,100),(3,29),(3,22),(3,11),(4,22),(4,15)] 
fromList [(2,100),(3,29),(4,22)]
```

或是将相同键的值都加在一起.

```haskell
ghci> Map.fromListWith (+) [(2,3),(2,5),(2,100),(3,29),(3,22),(3,11),(4,22),(4,15)] 
fromList [(2,108),(3,62),(4,37)]
```

**insertWith** 之于 `insert`，恰如 `fromListWith` 之于 `fromList`。它会将一个键值对插入一个 `map` 之中，而该 `map` 若已经包含这个键，就问问这个函数该怎么办。

```haskell
ghci> Map.insertWith (+) 3 100 $ Map.fromList [(3,4),(5,103),(6,339)] 
fromList [(3,104),(5,103),(6,339)]
```

`Data.Map` 里面还有不少函数，\[[http://www.haskell.org/ghc/docs/latest/html/libraries/containers/Data-Map.html](http://www.haskell.org/ghc/docs/latest/html/libraries/containers/Data-Map.html) 这个文档\]中的列表就很全了.

## Data.Set

![](../../.gitbook/assets/legosets.png)

`Data.Set` 模块提供了对数学中集合的处理。集合既像 List 也像 `Map`: 它里面的每个元素都是唯一的，且内部的数据由一棵树来组织\(这和 `Data.Map` 模块的 `map` 很像\)，必须得是可排序的。同样是插入,删除,判断从属关系之类的操作，使用集合要比 List 快得多。对一个集合而言，最常见的操作莫过于并集，判断从属或是将集合转为 List.

由于 `Data.Set` 模块与 `Prelude` 模块和 `Data.List` 模块中存在大量的命名冲突，所以我们使用 `qualified import`

将 `import` 语句至于代码之中:

```haskell
import qualified Data.Set as Set
```

然后在 ghci 中装载

假定我们有两个字串，要找出同时存在于两个字串的字符

```haskell
text1 = "I just had an anime dream. Anime... Reality... Are they so different?"  
text2 = "The old man left his garbage can out and now his trash is all over my lawn!"
```

**fromList** 函数同你想的一样，它取一个 List 作参数并将其转为一个集合

```haskell
ghci> let set1 = Set.fromList text1  
ghci> let set2 = Set.fromList text2  
ghci> set1  
fromList " .?AIRadefhijlmnorstuy"  
ghci> set2  
fromList " !Tabcdefghilmnorstuvwy"
```

如你所见，所有的元素都被排了序。而且每个元素都是唯一的。现在我们取它的交集看看它们共同包含的元素:

```haskell
ghci> Set.intersection set1 set2  
fromList " adefhilmnorstuy"
```

使用 `difference` 函数可以得到存在于第一个集合但不在第二个集合的元素

```haskell
ghci> Set.difference set1 set2  
fromList ".?AIRj"  
ghci> Set.difference set2 set1  
fromList "!Tbcgvw"
```

也可以使用 `union` 得到两个集合的并集

```haskell
ghci> Set.union set1 set2  
fromList " !.?AIRTabcdefghijlmnorstuvwy"
```

`null`，`size`，`member`，`empty`，`singleton`，`insert`，`delete` 这几个函数就跟你想的差不多啦

```haskell
ghci> Set.null Set.empty  
True  
ghci> Set.null $ Set.fromList [3,4,5,5,4,3]  
False  
ghci> Set.size $ Set.fromList [3,4,5,3,4,5]  
3  
ghci> Set.singleton 9  
fromList [9]  
ghci> Set.insert 4 $ Set.fromList [9,3,8,1]  
fromList [1,3,4,8,9]  
ghci> Set.insert 8 $ Set.fromList [5..10]  
fromList [5,6,7,8,9,10]  
ghci> Set.delete 4 $ Set.fromList [3,4,5,4,3,4,5]  
fromList [3,5]
```

也可以判断子集与真子集，如果集合 A 中的元素都属于集合 B，那么 A 就是 B 的子集, 如果 A 中的元素都属于 B 且 B 的元素比 A 多，那 A 就是 B 的真子集

```haskell
ghci> Set.fromList [2,3,4] `Set.isSubsetOf` Set.fromList [1,2,3,4,5]  
True  
ghci> Set.fromList [1,2,3,4,5] `Set.isSubsetOf` Set.fromList [1,2,3,4,5]  
True  
ghci> Set.fromList [1,2,3,4,5] `Set.isProperSubsetOf` Set.fromList [1,2,3,4,5]  
False  
ghci> Set.fromList [2,3,4,8] `Set.isSubsetOf` Set.fromList [1,2,3,4,5]  
False
```

对集合也可以执行 `map` 和 `filter`:

```haskell
ghci> Set.filter odd $ Set.fromList [3,4,5,6,7,2,3,4]  
fromList [3,5,7]  
ghci> Set.map (+1) $ Set.fromList [3,4,5,6,7,2,3,4]  
fromList [3,4,5,6,7,8]
```

集合有一常见用途，那就是先 `fromList` 删掉重复元素后再 `toList` 转回去。尽管 `Data.List` 模块的 `nub` 函数完全可以完成这一工作，但在对付大 List 时则会明显的力不从心。使用集合则会快很多，`nub` 函数只需 List 中的元素属于 `Eq` 型别类就行了，而若要使用集合，它必须得属于 `Ord` 型别类

```haskell
ghci> let setNub xs = Set.toList $ Set.fromList xs  
ghci> setNub "HEY WHATS CRACKALACKIN"  
" ACEHIKLNRSTWY"  
ghci> nub "HEY WHATS CRACKALACKIN"  
"HEY WATSCRKLIN"
```

在处理较大的 List 时，`setNub` 要比 `nub` 快，但也可以从中看出，`nub` 保留了 List 中元素的原有顺序，而 `setNub` 不。

## 建立自己的模块

我们已经见识过了几个很酷的模块，但怎样才能构造自己的模块呢? 几乎所有的编程语言都允许你将代码分成多个文件，Haskell 也不例外。在编程时，将功能相近的函数和型别至于同一模块中会是个很好的习惯。这样一来，你就可以轻松地一个 `import` 来重用其中的函数.

接下来我们将构造一个由计算机几何图形体积和面积组成的模块，先从新建一个 `Geometry.hs` 的文件开始.

在模块的开头定义模块的名称，如果文件名叫做 `Geometry.hs` 那它的名字就得是 `Geometry`。在声明出它含有的函数名之后就可以编写函数的实现啦，就这样写:

```haskell
module Geometry  
( sphereVolume  
, sphereArea  
, cubeVolume  
, cubeArea  
, cuboidArea  
, cuboidVolume  
) where
```

如你所见，我们提供了对球体,立方体和立方体的面积和体积的解法。继续进发，定义函数体:

```haskell
module Geometry  
( sphereVolume  
, sphereArea  
, cubeVolume  
, cubeArea  
, cuboidArea  
, cuboidVolume  
) where  

sphereVolume :: Float -> Float  
sphereVolume radius = (4.0 / 3.0) * pi * (radius ^ 3)  

sphereArea :: Float -> Float  
sphereArea radius = 4 * pi * (radius ^ 2)  

cubeVolume :: Float -> Float  
cubeVolume side = cuboidVolume side side side  

cubeArea :: Float -> Float  
cubeArea side = cuboidArea side side side  

cuboidVolume :: Float -> Float -> Float -> Float  
cuboidVolume a b c = rectangleArea a b * c  

cuboidArea :: Float -> Float -> Float -> Float  
cuboidArea a b c = rectangleArea a b * 2 + rectangleArea a c * 2 + rectangleArea c b * 2  

rectangleArea :: Float -> Float -> Float  
rectangleArea a b = a * b
```

![](../../.gitbook/assets/making_modules.png)

标准的几何公式。有几个地方需要注意一下，由于立方体只是长方体的特殊形式，所以在求它面积和体积的时候我们就将它当作是边长相等的长方体。在这里还定义了一个 `helper`函数，`rectangleArea` 它可以通过长方体的两条边计算出长方体的面积。它仅仅是简单的相乘而已，份量不大。但请注意我们可以在这一模块中调用这个函数，而它不会被导出! 因为我们这个模块只与三维图形打交道.

当构造一个模块的时候，我们通常只会导出那些行为相近的函数，而其内部的实现则是隐蔽的。如果有人用到了 `Geometry` 模块，就不需要关心它的内部实现是如何。我们作为编写者，完全可以随意修改这些函数甚至将其删掉，没有人会注意到里面的变动，因为我们并不把它们导出.

要使用我们的模块，只需:

```haskell
import Geometry
```

将 `Geometry.hs` 文件至于用到它的进程文件的同一目录之下.

模块也可以按照分层的结构来组织，每个模块都可以含有多个子模块。而子模块还可以有自己的子模块。我们可以把 `Geometry` 分成三个子模块，而一个模块对应各自的图形对象.

首先，建立一个 `Geometry` 文件夹，注意首字母要大写，在里面新建三个文件

如下就是各个文件的内容:

Sphere.hs

```haskell
module Geometry.Sphere  
( volume  
, area  
) where  

volume :: Float -> Float  
volume radius = (4.0 / 3.0) * pi * (radius ^ 3)  

area :: Float -> Float  
area radius = 4 * pi * (radius ^ 2)
```

Cuboid.hs

```haskell
module Geometry.Cuboid  
( volume  
, area  
) where  

volume :: Float -> Float -> Float -> Float  
volume a b c = rectangleArea a b * c  

area :: Float -> Float -> Float -> Float  
area a b c = rectangleArea a b * 2 + rectangleArea a c * 2 + rectangleArea c b * 2  

rectangleArea :: Float -> Float -> Float  
rectangleArea a b = a * b
```

Cube.hs

```haskell
module Geometry.Cube  
( volume  
, area  
) where  

import qualified Geometry.Cuboid as Cuboid  

volume :: Float -> Float  
volume side = Cuboid.volume side side side  

area :: Float -> Float  
area side = Cuboid.area side side side
```

好的! 先是 `Geometry.Sphere`。注意，我们将它置于 `Geometry` 文件夹之中并将它的名字定为 `Geometry.Sphere`。对 Cuboid 也是同样，也注意下，在三个模块中我们定义了许多名称相同的函数，因为所在模块不同，所以不会产生命名冲突。若要在 `Geometry.Cube` 使用 `Geometry.Cuboid` 中的函数，就不能直接 `import Geometry.Cuboid`，而必须得 `qualified import`。因为它们中间的函数名完全相同.

```haskell
import Geometry.Sphere
```

然后，调用 `area` 和 `volume`，就可以得到球体的面积和体积，而若要用到两个或更多此类模块，就必须得 `qualified import` 来避免重名。所以就得这样写:

```haskell
import qualified Geometry.Sphere as Sphere  
import qualified Geometry.Cuboid as Cuboid  
import qualified Geometry.Cube as Cube
```

然后就可以调用 `Sphere.area`，`Sphere.volume`，`Cuboid.area` 了，而每个函数都只计算其对应物体的面积和体积.

以后你若发现自己的代码体积庞大且函数众多，就应该试着找找目的相近的函数能否装入各自的模块，也方便日后的重用.

