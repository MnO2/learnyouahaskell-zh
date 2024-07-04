# 输入与输出

![](../../.gitbook/assets/dognap.png)

我们已经说明了 Haskell 是一个纯粹函数式语言。虽说在命令式语言中我们习惯给电脑执行一连串指令，在函数式语言中我们是用定义东西的方式进行。在 Haskell 中，一个函数不能改变状态，像是改变一个变量的内容。（当一个函数会改变状态，我们说这函数是有副作用的。）在 Haskell 中函数唯一可以做的事是根据我们给定的参数来算出结果。如果我们用同样的参数调用两次同一个函数，它会回传相同的结果。尽管这从命令式语言的角度来看是蛮大的限制，我们已经看过它可以达成多么酷的效果。在一个命令式语言中，编程语言没办法给你任何保证在一个简单如打印出几个数字的函数不会同时烧掉你的房子，绑架你的狗并刮伤你车子的烤漆。例如，当我们要建立一棵二元树的时候，我们并不插入一个节点来改变原有的树。由于我们无法改变状态，我们的函数实际上回传了一棵新的二元树。

函数无法改变状态的好处是它让我们促进了我们理解程序的容易度，但同时也造成了一个问题。假如说一个函数无法改变现实世界的状态，那它要如何打印出它所计算的结果？毕竟要告诉我们结果的话，它必须要改变输出设备的状态（譬如说屏幕），然后从屏幕传达到我们的脑，并改变我们心智的状态。

不要太早下结论，Haskell 实际上设计了一个非常聪明的系统来处理有副作用的函数，它漂亮地将我们的程序区分成纯粹跟非纯粹两部分。非纯粹的部分负责跟键盘还有屏幕沟通。有了这区分的机制，在跟外界沟通的同时，我们还是能够有效运用纯粹所带来的好处，像是惰性求值、容错性跟模块性。

## Hello, world!

![](../../.gitbook/assets/helloworld%20%281%29.png)

到目前为止我们都是将函数加载 GHCi 中来测试，像是标准函式库中的一些函式。但现在我们要做些不一样的，写一个真实跟世界交互的 Haskell 程序。当然不例外，我们会来写个 "hello world"。

现在，我们把下一行打到你熟悉的编辑器中

```haskell
main = putStrLn "hello, world"
```

我们定义了一个 `main`，并在里面以 `"hello, world"` 为参数调用了 `putStrLn`。看起来没什么大不了，但不久你就会发现它的奥妙。把这程序存成 `helloworld.hs`。

现在我们将做一件之前没做过的事：编译你的程序。打开你的终端并切换到包含 `helloworld.hs` 的目录，并输入下列指令。

```haskell
$ ghc --make helloworld
[1 of 1] Compiling Main                 ( helloworld.hs, hellowowlrd.o )
Linking helloworld ...
```

顺利的话你就会得到如上的消息，接着你便可以执行你的程序 `./helloworld`

```haskell
$ ./helloworld
hello, world
```

这就是我们第一个编译成功并打印出字串到屏幕的程序。很简单吧。

让我们来看一下我们究竟做了些什么，首先来看一下 `putStrLn` 函数的型态：

```haskell
ghci> :t putStrLn
putStrLn :: String -> IO ()
ghci> :t putStrLn "hello, world"
putStrLn "hello, world" :: IO ()
```

我们可以这么解读 `putStrLn` 的型态：`putStrLn` 接受一个字串并回传一个 I/O action，这 I/O action 包含了 `()` 的型态。（即空的 tuple，或者是 unit 型态）。一个 I/O action 是一个会造成副作用的动作，常是指读取输入或输出到屏幕，同时也代表会回传某些值。在屏幕打印出几个字串并没有什么有意义的回传值可言，所以这边用一个 `()` 来代表。

那究竟 I/O action 会在什么时候被触发呢？这就是 `main` 的功用所在。一个 I/O action 会在我们把它绑定到 `main` 这个名字并且执行程序的时候触发。

把整个程序限制在只能有一个 I/O action 看似是个极大的限制。这就是为什么我们需要 do 表示法来将所有 I/O action 绑成一个。来看看下面这个例子。

```haskell
main = do
    putStrLn "Hello, what's your name?"
    name <- getLine
    putStrLn ("Hey " ++ name ++ ", you rock!")
```

新的语法，有趣吧！它看起来就像一个命令式的程序。如果你编译并执行它，它便会照你预期的方式执行。我们写了一个 do 并且接着一连串指令，就像写个命令式程序一般，每一步都是一个 I/O action。将所有 I/O action 用 do 绑在一起变成了一个大的 I/O action。这个大的 I/O action 的型态是 `IO ()`，这完全是由最后一个 I/O action 所决定的。

这就是为什么 `main` 的型态永远都是 `main :: IO something`，其中 `something` 是某个具体的型态。按照惯例，我们通常不会把 `main` 的型态在程序中写出来。

另一个有趣的事情是第三行 `name <- getLine`。它看起来像是从输入读取一行并存到一个变量 `name` 之中。真的是这样吗？我们来看看 `getLine` 的型态吧

```haskell
ghci> :t getLine
getLine :: IO String
```

![](../../.gitbook/assets/luggage%20%281%29.png)

我们可以看到 `getLine` 是一个回传 `String` 的 I/O action。因为它会等用户输入某些字串，这很合理。那 `name <- getLine` 又是如何？你能这样解读它：执行一个 I/O action `getLine` 并将它的结果绑定到 `name` 这个名字。`getLine` 的型态是 `IO String`，所以 `name` 的型态会是 `String`。你能把 I/O action 想成是一个长了脚的盒子，它会跑到真实世界中替你做某些事，像是在墙壁上涂鸦，然后带回来某些数据。一旦它带了某些数据给你，打开盒子的唯一办法就是用 `<-`。而且如果我们要从 I/O action 拿出某些数据，就一定同时要在另一个 I/O action 中。这就是 Haskell 如何漂亮地分开纯粹跟不纯粹的程序的方法。`getLine` 在这样的意义下是不纯粹的，因为执行两次的时候它没办法保证会回传一样的值。这也是为什么它需要在一个 `IO` 的型态建构子中，那样我们才能在 I/O action 中取出数据。而且任何一段程序一旦依赖着 I/O 数据的话，那段程序也会被视为 I/O code。

但这不表示我们不能在纯粹的代码中使用 I/O action 回传的数据。只要我们绑定它到一个名字，我们便可以暂时地使用它。像在 `name <- getLine` 中 `name` 不过是一个普通字串，代表在盒子中的内容。我们能将这个普通的字串传给一个极度复杂的函数，并回传你一生会有多少财富。像是这样：

```haskell
main = do
    putStrLn "Hello, what's your name?"
    name <- getLine
    putStrLn $ "Read this carefully, because this is your future: " ++ tellFortune name
```

`tellFortune` 并不知道任何 I/O 有关的事，它的型态只不过是 `String -> String`。

再来看看这段代码吧，他是合法的吗?

```haskell
nameTag = "Hello, my name is " ++ getLine
```

如果你回答不是，恭喜你。如果你说是，你答错了。这么做不对的理由是 `++` 要求两个参数都必须是串列。他左边的参数是 `String`，也就是 `[Char]`。然而 `getLine` 的型态是 `IO String`。你不能串接一个字串跟 I/O action。我们必须先把 `String` 的值从 I/O action 中取出，而唯一可行的方法就是在 I/O action 中使用 `name <- getLine`。如果我们需要处理一些非纯粹的数据，那我们就要在非纯粹的环境中做。所以我们最好把 I/O 的部分缩减到最小的比例。

每个 I/O action 都有一个值封装在里面。这也是为什么我们之前的程序可以这么写：

```haskell
main = do
    foo <- putStrLn "Hello, what's your name?"
    name <- getLine
    putStrLn ("Hey " ++ name ++ ", you rock!")
```

然而，`foo` 只会有一个 `()` 的值，所以绑定到 `foo` 这个名字似乎是多余的。另外注意到我们并没有绑定最后一行的 `putStrLn` 给任何名字。那是因为在一个 do block 中，最后一个 action 不能绑定任何名字。我们在之后讲解 Monad 的时候会说明为什么。现在你可以先想成 do block 会自动从最后一个 action 取出值并绑定给他的结果。

除了最后一行之外，其他在 do 中没有绑定名字的其实也可以写成绑定的形式。所以 `putStrLn "BLAH"` 可以写成 `_ <- putStrLn "BLAH"`。但这没什么实际的意义，所以我们宁愿写成 `putStrLn something`。

初学者有时候会想错

```haskell
    name = getLine
```

以为这行会读取输入并给他绑定一个名字叫 `name` 但其实只是把 `getLine` 这个 I/O action 指定一个名字叫 `name` 罢了。记住，要从一个 I/O action 中取出值，你必须要在另一个 I/O action 中将他用 `<-` 绑定给一个名字。

I/O actions 只会在绑定给 `main` 的时候或是在另一个用 do 串起来的 I/O action 才会执行。你可以用 do 来串接 I/O actions，再用 do 来串接这些串接起来的 I/O actions。不过只有最外面的 I/O action 被指定给 main 才会触发执行。

喔对，其实还有另外一个情况。就是在 GHCi 中输入一个 I/O action 并按下 Enter 键，那也会被执行

```haskell
ghci> putStrLn "HEEY"
HEEY
```

就算我们只是在 GHCi 中打几个数字或是调用一个函数，按下 Enter 就会计算它并调用 `show`，再用 `putStrLn` 将字串打印出在终端上。

还记得 let binding 吗？如果不记得，回去温习一下这个章节。它们的形式是 `let bindings in expression`，其中 `bindings` 是 expression 中的名字、`expression` 则是被运用到这些名字的算式。我们也提到了 list comprehensions 中，`in` 的部份不是必需的。你能够在 do blocks 中使用 let bindings 如同在 list comprehensions 中使用它们一样，像这样：

```haskell
import Data.Char

main = do
    putStrLn "What's your first name?"
    firstName <- getLine
    putStrLn "What's your last name?"
    lastName <- getLine
    let bigFirstName = map toUpper firstName
        bigLastName = map toUpper lastName
    putStrLn $ "hey " ++ bigFirstName ++ " " ++ bigLastName ++ ", how are you?"
```

注意我们是怎么编排在 do block 中的 I/O actions，也注意到我们是怎么编排 let 跟其中的名字的，由于对齐在 Haskell 中并不会被无视，这么编排才是好的习惯。我们的程序用 `map toUpper firstName` 将 `"John"` 转成大写的 `"JOHN"`，并将大写的结果绑定到一个名字上，之后在输出的时候参考到了这个名字。

你也许会问究竟什么时候要用 `<-`，什么时候用 let bindings？记住，`<-` 是用来运算 I/O actions 并将他的结果绑定到名称。而 `map toUpper firstName` 并不是一个 I/O action。他只是一个纯粹的 expression。所以总结来说，当你要绑定 I/O actions 的结果时用 `<-`，而对于纯粹的 expression 使用 let bindings。对于错误的 `let firstName = getLine`，我们只不过是把 `getLine` 这个 I/O actions 给了一个不同的名字罢了。最后还是要用 `<-` 将结果取出。

现在我们来写一个会一行一行不断地读取输入，并将读进来的字反过来输出到屏幕上的程序。程序会在输入空白行的时候停止。

```haskell
main = do
    line <- getLine
    if null line
        then return ()
        else do
            putStrLn $ reverseWords line
            main

reverseWords :: String -> String
reverseWords = unwords . map reverse . words
```

在分析这段程序前，你可以执行看看来感受一下程序的运行。

首先，我们来看一下 `reverseWords`。他不过是一个普通的函数，假如接受了个字串 `"hey there man"`，他会先调用 `words` 来产生一个字的串列 `["hey", "there", "man"]`。然后用 `reverse` 来 map 整个串列，得到 `["yeh", "ereht", "nam"]`，接着用 `unwords` 来得到最终的结果 `"yeh ereht nam"`。这些用函数合成来简洁的表达。如果没有用函数合成，那就会写成丑丑的样子 `reverseWords st = unwords (map reverse (words st))`

那 `main` 又是怎么一回事呢？首先，我们用 `getLine` 从终端读取了一行，并把这行输入取名叫 `line`。然后接着一个条件式 expression。记住，在 Haskell 中 if 永远要伴随一个 else，这样每个 expression 才会有值。当 if 的条件是 true （也就是输入了一个空白行），我们便执行一个 I/O action，如果 if 的条件是 false，那 else 底下的 I/O action 被执行。这也就是说当 if 在一个 I/O do block 中的时候，长的样子是 `if condition then I/O action else I/O action`。

我们首先来看一下在 else 中发生了什么事。由于我们在 else 中只能有一个 I/O action，所以我们用 do 来将两个 I/O actions 绑成一个，你可以写成这样：

```haskell
else (do
    putStrLn $ reverseWords line
    main)
```

这样可以明显看到整个 do block 可以看作一个 I/O action，只是比较丑。但总之，在 do block 里面，我们依序调用了 `getLine` 以及 `reverseWords`，在那之后，我们递归调用了 `main`。由于 main 也是一个 I/O action，所以这不会造成任何问题。调用 `main` 也就代表我们回到程序的起点。

那假如 `null line` 的结果是 true 呢？也就是说 then 的区块被执行。我们看一下区块里面有 `then return ()`。如果你是从 C、Java 或 Python 过来的，你可能会认为 `return` 不过是作一样的事情便跳过这一段。但很重要的： `return` 在 Hakell 里面的意义跟其他语言的 `return` 完全不同！他们有相同的样貌，造成了许多人搞错，但确实他们是不一样的。在命令式语言中，`return` 通常结束 method 或 subroutine 的执行，并且回传某个值给调用者。在 Haskell 中，他的意义则是利用某个 pure value 造出 I/O action。用之前盒子的比喻来说，就是将一个 value 装进箱子里面。产生出的 I/O action 并没有作任何事，只不过将 value 包起来而已。所以在 I/O 的情况下来说，`return "haha"` 的型态是 `IO String`。将 pure value 包成 I/O action 有什么实质意义呢？为什么要弄成 `IO` 包起来的值？这是因为我们一定要在 else 中摆上某些 I/O action，所以我们才用 `return ()` 做了一个没作什么事情的 I/O action。

在 I/O do block 中放一个 `return` 并不会结束执行。像下面这个程序会执行到底。

```haskell
main = do
    return ()
    return "HAHAHA"
    line <- getLine
    return "BLAH BLAH BLAH"
    return 4
    putStrLn line
```

所有在程序中的 `return` 都是将 value 包成 I/O actions，而且由于我们没有将他们绑定名称，所以这些结果都被忽略。我们能用 `<-` 与 `return` 来达到绑定名称的目的。

```haskell
main = do
    a <- return "hell"
    b <- return "yeah!"
    putStrLn $ a ++ " " ++ b
```

可以看到 `return` 与 `<-` 作用相反。`return` 把 value 装进盒子中，而 `<-` 将 value 从盒子拿出来，并绑定一个名称。不过这么做是有些多余，因为你可以用 let bindings 来绑定

```haskell
main = do
    let a = "hell"
        b = "yeah"
    putStrLn $ a ++ " " ++ b
```

在 I/O do block 中需要 `return` 的原因大致上有两个：一个是我们需要一个什么事都不做的 I/O action，或是我们不希望这个 do block 形成的 I/O action 的结果值是这个 block 中的最后一个 I/O action，我们希望有一个不同的结果值，所以我们用 `return` 来作一个 I/O action 包了我们想要的结果放在 do block 的最后。

在我们接下去讲文件之前，让我们来看看有哪些实用的函数可以处理 I/O。

`putStr` 跟 `putStrLn` 几乎一模一样，都是接受一个字串当作参数，并回传一个 I/O action 打印出字串到终端上，只差在 `putStrLn` 会换行而 `putStr` 不会罢了。

```haskell
main = do putStr "Hey, "
          putStr "I'm "
          putStrLn "Andy!"
```

```haskell
$ runhaskell putstr_test.hs
Hey, I'm Andy!
```

他的 type signature 是 `putStr :: String -> IO ()`，所以是一个包在 I/O action 中的 unit。也就是空值，没有办法绑定他。

`putChar` 接受一个字符，并回传一个 I/O action 将他打印到终端上。

```haskell
main = do putChar 't'
          putChar 'e'
          putChar 'h'
```

```haskell
$ runhaskell putchar_test.hs
teh
```

`putStr` 实际上就是 `putChar` 递归定义出来的。`putStr` 的边界条件是空字串，所以假设我们打印一个空字串，那他只是回传一个什么都不做的 I/O action，像 `return ()`。如果打印的不是空字串，那就先用 `putChar` 打印出字串的第一个字符，然后再用 `putStr` 打印出字串剩下部份。

```haskell
putStr :: String -> IO ()
putStr [] = return ()
putStr (x:xs) = do
    putChar x
    putStr xs
```

看看我们如何在 I/O 中使用递归，就像我们在 pure code 中所做的一样。先定义一个边界条件，然后再思考剩下如何作。

`print` 接受任何是 `Show` typeclass 的 instance 的型态的值，这代表我们知道如何用字串表示他，调用 `show` 来将值变成字串然后将其输出到终端上。基本上，他就是 `putStrLn . show`。首先调用 `show` 然后把结果喂给 `putStrLn`，回传一个 I/O action 打印出我们的值。

```haskell
main = do print True
          print 2
          print "haha"
          print 3.2
          print [3,4,3]
```

```haskell
$ runhaskell print_test.hs
True
2
"haha"
3.2
[3,4,3]
```

就像你看到的，这是个很方便的函数。还记得我们提到 I/O actions 只有在 `main` 中才会被执行以及在 GHCI 中运算的事情吗？当我们用键盘打了些值，像 `3` 或 `[1,2,3]` 并按下 Enter，GHCI 实际上就是用了 `print` 来将这些值输出到终端。

```haskell
ghci> 3
3
ghci> print 3
3
ghci> map (++"!") ["hey","ho","woo"]
["hey!","ho!","woo!"]
ghci> print (map (++"!") ["hey", "ho", "woo"])
["hey!","ho!","woo!"]
```

当我们需要打印出字串，我们会用 `putStrLn`，因为我们不想要周围有引号，但对于输出值来说，`print` 才是最常用的。

`getChar` 是一个从输入读进一个字符的 I/O action，因此他的 type signature 是 `getChar :: IO Char`，代表一个 I/O action 的结果是 `Char`。注意由于缓冲区的关系，只有当 Enter 被按下的时候才会触发读取字符的行为。

```haskell
main = do
    c <- getChar
    if c /= ' '
        then do
            putChar c
            main
        else return ()
```

这程序看起来像是读取一个字符并检查他是否为一个空白。如果是的话便停止，如果不是的话便打印到终端上并重复之前的行为。在某种程度上来说也不能说错，只是结果不如你预期而已。来看看结果吧。

```haskell
$ runhaskell getchar_test.hs
hello sir
hello
```

上面的第二行是输入。我们输入了 `hello sir` 并按下了 Enter。由于缓冲区的关系，程序是在我们按了 Enter 后才执行而不是在某个输入字符的时候。一旦我们按下了 Enter，那他就把我们直到目前输入的一次做完。

`when` 这函数可以在 `Control.Monad` 中找到他 \(你必须 `import Contorl.Monad` 才能使用他\)。他在一个 do block 中看起来就像一个控制流程的 statement，但实际上他的确是一个普通的函数。他接受一个 boolean 值跟一个 I/O action。如果 boolean 值是 `True`，便回传我们传给他的 I/O action。如果 boolean 值是 `False`，便回传 `return ()`，即什么都不做的 I/O action。我们接下来用 `when` 来改写我们之前的程序。

```haskell
import Control.Monad

main = do
    c <- getChar
    when (c /= ' ') $ do
        putChar c
        main
```

就像你看到的，他可以将 `if something then do some I/O action else return ()` 这样的模式封装起来。

`sequence` 接受一串 I/O action，并回传一个会依序执行他们的 I/O action。运算的结果是包在一个 I/O action 的一连串 I/O action 的运算结果。他的 type signature 是 `sequence :: [IO a] -> IO [a]`

```haskell
main = do
    a <- getLine
    b <- getLine
    c <- getLine
    print [a,b,c]
```

其实可以写成

```haskell
main = do
    rs <- sequence [getLine, getLine, getLine]
    print rs
```

所以 `sequence [getLine, getLine, getLine]` 作成了一个执行 `getLine` 三次的 I/O action。如果我们对他绑定一个名字，结果便是这串结果的串列。也就是说，三个用户输入的东西组成的串列。

一个常见的使用方式是我们将 `print` 或 `putStrLn` 之类的函数 map 到串列上。`map print [1,2,3,4]` 这个动作并不会产生一个 I/O action，而是一串 I/O action，就像是 `[print 1, print 2, print 3, print 4]`。如果我们将一串 I/O action 变成一个 I/O action，我们必须用 `sequence`

```haskell
ghci> sequence (map print [1,2,3,4,5])
1
2
3
4
5
[(),(),(),(),()]
```

那 `[(),(),(),(),()]` 是怎么回事？当我们在 GHCI 中运算 I/O action，他会被执行并把结果打印出来，唯一例外是结果是 `()` 的时候不会被打印出。这也是为什么 `putStrLn "hehe"` 在 GHCI 中只会打印出 `hehe`（因为 `putStrLn "hehe"` 的结果是 `()`）。但当我们使用 `getLine` 时，由于 `getLine` 的型态是 `IO String`，所以结果会被打印出来。

由于对一个串列 map 一个回传 I/O action 的函数，然后再 sequence 他这个动作太常用了。所以有一些函数在函式库中 `mapM` 跟 `mapM_`。`mapM` 接受一个函数跟一个串列，将对串列用函数 map 然后 sequence 结果。`mapM_` 也作同样的事，只是他把运算的结果丢掉而已。在我们不关心 I/O action 结果的情况下，`mapM_` 是最常被使用的。

```haskell
ghci> mapM print [1,2,3]
1
2
3
[(),(),()]
ghci> mapM_ print [1,2,3]
1
2
3
```

`forever` 接受一个 I/O action 并回传一个永远作同一件事的 I/O action。你可以在 `Control.Monad` 中找到他。下面的程序会不断地要用户输入些东西，并把输入的东西转成大写输出到屏幕上。

```haskell
import Control.Monad
import Data.Char

main = forever $ do
    putStr "Give me some input: "
    l <- getLine
    putStrLn $ map toUpper l
```

在 `Control.Monad` 中的 `forM` 跟 `mapM` 的作用一样，只是参数的顺序相反而已。第一个参数是串列，而第二个则是函数。这有什么用？在一些有趣的情况下还是有用的：

```haskell
import Control.Monad

main = do
    colors <- forM [1,2,3,4] (\a -> do
        putStrLn $ "Which color do you associate with the number " ++ show a ++ "?"
        color <- getLine
        return color)
    putStrLn "The colors that you associate with 1, 2, 3 and 4 are: "
    mapM putStrLn colors
```

`(\a -> do ...)` 是接受一个数字并回传一个 I/O action 的函数。我们必须用括号括住他，不然 lambda 会贪心 match 的策略会把最后两个 I/O action 也算进去。注意我们在 do block 里面 `return color`。我们那么作是让 do block 的结果是我们选的颜色。实际上我们并不需那么作，因为 `getLine` 已经达到我们的目的。先 `color <- getLine` 再 `return color` 只不过是把值取出再包起来，其实是跟 `getLine` 效果相当。`forM` 产生一个 I/O action，我们把结果绑定到 `colors` 这名称。`colors` 是一个普通包含字串的串列。最后，我们用 `mapM putStrLn colors` 打印出所有颜色。

你可以把 `forM` 的意思想成将串列中的每个元素作成一个 I/O action。至于每个 I/O action 实际作什么就要看原本的元素是什么。然后，执行这些 I/O action 并将结果绑定到某个名称上。或是直接将结果忽略掉。

```haskell
$ runhaskell from_test.hs
Which color do you associate with the number 1?
white
Which color do you associate with the number 2?
blue
Which color do you associate with the number 3?
red
Which color do you associate with the number 4?
orange
The colors that you associate with 1, 2, 3 and 4 are:
white
blue
red
orange
```

其实我们也不是一定要用到 `forM`，只是用了 `forM` 程序会比较容易理解。正常来讲是我们需要在 map 跟 sequence 的时候定义 I/O action 的时候使用 `forM`，同样地，我们也可以将最后一行写成 `forM colors putStrLn`。

在这一节，我们学会了输入与输出的基础。我们也了解了什么是 I/O action，他们是如何帮助我们达成输入与输出的目的。这边重复一遍，I/O action 跟其他 Haskell 中的 value 没有两样。我们能够把他当参数传给函式，或是函式回传 I/O action。他们特别之处在于当他们是写在 `main` 里面或 GHCI 里面的时候，他们会被执行，也就是实际输出到你屏幕或输出音效的时候。每个 I/O action 也能包着一个从真实世界拿回来的值。

不要把像是 `putStrLn` 的函式想成接受字串并输出到屏幕。要想成一个函式接受字串并回传一个 I/O action。当 I/O action 被执行的时候，会漂亮地打印出你想要的东西。

## 文件与字符流

![](../../.gitbook/assets/streams.png)

`getChar` 是一个读取单一字符的 I/O action。`getLine` 是一个读取一行的 I/O action。这是两个非常直觉的函式，多数编程语言也有类似这两个函式的 statement 或 function。但现在我们来看看 _getContents_。`getContents` 是一个从标准输入读取直到 end-of-file 字符的 I/O action。他的型态是 `getContents :: IO String`。最酷的是 `getContents` 是惰性 I/O \(Lazy I/O\)。当我们写了 `foo <- getContents`，他并不会马上读取所有输入，将他们存在 memory 里面。他只有当你真的需要输入数据的时候才会读取。

当我们需要重导一个程序的输出到另一个程序的输入时，`getContents` 非常有用。假设我们有下面一个文本档：

```haskell
I'm a lil' teapot
What's with that airplane food, huh?
It's so small, tasteless
```

还记得我们介绍 `forever` 时写的小程序吗？会把所有输入的东西转成大写的那一个。为了防止你忘记了，这边再重复一遍。

```haskell
import Control.Monad
import Data.Char

main = forever $ do
    putStr "Give me some input: "
    l <- getLine
    putStrLn $ map toUpper l
```

将我们的程序存成 `capslocker.hs` 然后编译他。然后用 Unix 的 Pipe 将文本档喂给我们的程序。我们使用的是 GNU 的 cat，会将指定的文件输出到屏幕。

```haskell
$ ghc --make capslocker
[1 of 1] Compiling Main             ( capslocker.hs, capslocker.o )
Linking capslocker ...
$ cat haiku.txt
I'm a lil' teapot
What's with that airplane food, huh?
It's so small, tasteless
$ cat haiku.txt | ./capslocker
I'M A LIL' TEAPOT
WHAT'S WITH THAT AIRPLANE FOOD, HUH?
IT'S SO SMALL, TASTELESS
capslocker <stdin>: hGetLine: end of file
```

就如你看到的，我们是用 `|` 这符号来将某个程序的输出 piping 到另一个程序的输入。我们做的事相当于 run 我们的 capslocker，然后将 haiku 的内容用键盘打到终端上，最后再按 Ctrl-D 来代表 end-of-file。这就像执行 cat haiku.txt 后大喊，嘿，不要把内容打印到终端上，把内容塞到 capslocker！

我们用 `forever` 在做的事基本上就是将输入经过转换后变成输出。用 `getContents` 的话可以让我们的程序更加精炼。

```haskell
import Data.Char

main = do
    contents <- getContents
    putStr (map toUpper contents)
```

我们将 `getContents` 取回的字串绑定到 `contents`。然后用 `toUpper` map 到整个字串后打印到终端上。记住字串基本上就是一串惰性的串列 \(list\)，同时 `getContents` 也是惰性 I/O，他不会一口气读入内容然后将内容存在内存中。实际上，他会一行一行读入并输出大写的版本，这是因为输出才是真的需要输入的数据的时候。

```haskell
$ cat haiku.txt | ./capslocker
I'M A LIL' TEAPOT
WHAT'S WITH THAT AIRPLAN FOOD, HUH?
IT'S SO SMALL, TASTELESS
```

很好，程序运作正常。假如我们执行 capslocker 然后自己打几行字呢？

```haskell
$ ./capslocker
hey ho
HEY HO
lets go
LETS GO
```

按下 Ctrl-D 来离开环境。就像你看到的，程序是一行一行将我们的输入打印出来。当 `getContent` 的结果被绑定到 `contents` 的时候，他不是被表示成在内存中的一个字串，反而比较像是他有一天会是字串的一个承诺。当我们将 `toUpper` map 到 `contents` 的时候，便也是一个函数被承诺将会被 map 到内容上。最后 `putStr` 则要求先前的承诺说，给我一行大写的字串吧。实际上还没有任何一行被取出，所以便跟 `contents` 说，不如从终端那边取出些字串吧。这才是 `getContents` 真正从终端读入一行并把这一行交给程序的时候。程序便将这一行用 `toUpper` 处理并交给 `putStr`，`putStr` 则打印出他。之后 `putStr` 再说：我需要下一行。整个步骤便再重复一次，直到读到 end-of-file 为止。

接着我们来写个程序，读取输入，并只打印出少于十个字符的行。

```haskell
main = do
    contents <- getContents
    putStr (shortLinesOnly contents)

shortLinesOnly :: String -> String
shortLinesOnly input =
    let allLines = lines input
        shortLines = filter (\line -> length line < 10) allLines
        result = unlines shortLines
    in result
```

我们把 I/O 部份的代码弄得很短。由于程序的行为是接某些输入，作些处理然后输出。我们可以把他想成读取输入，调用一个函数，然后把函数的结果输出。

`shortLinesOnly` 的行为是这样：拿到一个字串，像是 `"short\nlooooooooooooooong\nshort again"`。这字串有三行，前后两行比较短，中间一行很常。他用 `lines` 把字串分成 `["short", "looooooooooooooong", "short again"]`，并把结果绑定成 `allLines`。然后过滤这些字串，只有少于十个字符的留下，`["short", "short again"]`，最后用 `unlines` 把这些字串用换行接起来，形成 `"short\nshort again"`

```haskell
i'm short
so am i
i am a loooooooooong line!!!
yeah i'm long so what hahahaha!!!!!!
short line
loooooooooooooooooooooooooooong
short
```

```haskell
$ ghc --make shortlinesonly
[1 of 1] Compiling Main             ( shortlinesonly.hs, shortlinesonly.o )
Linking shortlinesonly ...
$ cat shortlines.txt | ./shortlinesonly
i'm short
so am i
short
```

我们把 shortlines.txt 的内容经由 pipe 送给 shortlinesonly，结果就如你看到，我们只有得到比较短的行。

从输入那一些字串，经由一些转换然后输出这样的模式实在太常用了。常用到甚至建立了一个函数叫 **interact**。`interact` 接受一个 `String -> String` 的函数，并回传一个 I/O action。那个 I/O action 会读取一些输入，调用提供的函数，然后把函数的结果打印出来。所以我们的程序可以改写成这样。

```haskell
main = interact shortLinesOnly

shortLinesOnly :: String -> String
shortLinesOnly input =
    let allLines = lines input
        shortLines = filter (\line -> length line < 10) allLines
        result = unlines shortLines
    in result
```

我们甚至可以再让代码更短一些，像这样

```haskell
main = interact $ unlines . filter ((<10) . length) . lines
```

看吧，我们让程序缩到只剩一行了，很酷吧！

能应用 `interact` 的情况有几种，像是从输入 pipe 读进一些内容，然后丢出一些结果的程序；或是从用户获取一行一行的输入，然后丢回根据那一行运算的结果，再拿取另一行。这两者的差别主要是取决于用户使用他们的方式。

我们再来写另一个程序，它不断地读取一行行并告诉我们那一行字串是不是一个回文本串 \(palindrome\)。我们当然可以用 `getLine` 读取一行然后再调用 `main` 作同样的事。不过同样的事情可以用 `interact` 更简洁地达成。当使用 `interact` 的时候，想像你是将输入经有某些转换成输出。在这个情况当中，我们要将每一行输入转换成 `"palindrome"` 或 `"not a palindrome"`。所以我们必须写一个函数将 `"elephant\nABCBA\nwhatever"` 转换成 `not a palindrome\npalindrome\nnot a palindrome"`。来动手吧！

```haskell
respondPalindromes contents = unlines (map (\xs ->
    if isPalindrome xs then "palindrome" else "not a palindrome") (lines contents))
        where isPalindrome xs = xs == reverse xs
```

再来将程序改写成 point-free 的形式

```haskell
respondPalindromes = unlines . map (\xs ->
    if isPalindrome xs then "palindrome" else "not a palindrome") . lines
        where isPalindrome xs = xs == reverse xs
```

很直觉吧！首先将 `"elephant\nABCBA\nwhatever"` 变成 `["elephant", "ABCBA", "whatever"]` 然后将一个 lambda 函数 map 它，`["not a palindrome", "palindrome", "not a palindrome"]` 然后用 `unlines` 变成一行字串。接着

```haskell
main = interact respondPalindromes
```

来测试一下吧。

```haskell
$ runhaskell palindrome.hs
hehe
not a palindrome
ABCBA
palindrome
cookie
not a palindrome
```

即使我们的程序是把一大把字串转换成另一个，其实他表现得好像我们是一行一行做的。这是因为 Haskell 是惰性的，程序想要打印出第一行结果时，他必须要先有第一行输入。所以一旦我们给了第一行输入，他便打印出第一行结果。我们用 end-of-line 字符来结束程序。

我们也可以用 pipe 的方式将输入喂给程序。假设我们有这样一个文件。

```haskell
dogaroo
radar
rotor
madam
```

将他存为 `words.txt`，将他喂给程序后得到的结果

```haskell
$ cat words.txt | runhaskell palindromes.hs
not a palindrome
palindrome
palindrome
palindrome
```

再一次地提醒，我们得到的结果跟我们自己一个一个字打进输入的内容是一样的。我们看不到 `palindrome.hs` 输入的内容是因为内容来自于文件。

你应该大致了解 Lazy I/O 是如何运作，并能善用他的优点。他可以从输入转换成输出的角度方向思考。由于 Lazy I/O，没有输入在被用到之前是真的被读入。

到目前为止，我们的示范都是从终端读取某些东西或是打印出某些东西到终端。但如果我们想要读写文件呢？其实从某个角度来说我们已经作过这件事了。我们可以把读写终端想成读写文件。只是把文件命名成 `stdout` 跟 `stdin` 而已。他们分别代表标准输出跟标准输入。我们即将看到的读写文件跟读写终端并没什么不同。

首先来写一个程序，他会开启一个叫 girlfriend.txt 的文件，文件里面有 Avril Lavigne 的畅销名曲 Girlfriend，并将内容打印到终端上。接下来是 girlfriend.txt 的内容。

```haskell
Hey! Hey! You! You!
I don't like your girlfriend!
No way! No way!
I think you need a new one!
```

这则是我们的主程序。

```haskell
import System.IO

main = do
    handle <- openFile "girlfriend.txt" ReadMode
    contents <- hGetContents handle
    putStr contents
    hClose handle
```

执行他后得到的结果。

```haskell
$ runhaskell girlfriend.hs
Hey! Hey! You! You!
I don't like your girlfriend!
No way! No way!
I think you need a new one!
```

我们来一行行看一下程序。我们的程序用 do 把好几个 I/O action 绑在一起。在 do block 的第一行，我们注意到有一个新的函数叫 **openFile**。他的 type signature 是 `openFile :: FilePath -> IOMode -> IO Handle`。他说了 `openFile` 接受一个文件路径跟一个 `IOMode`，并回传一个 I/O action，他会打开一个文件并把文件关联到一个 handle。

`FilePath` 不过是 `String` 的 type synonym。

```haskell
type FilePath = String
```

`IOMode` 则是一个定义如下的型态

```haskell
data IOMode = ReadMode | WriteMode | AppendMode | ReadWriteMode
```

![](../../.gitbook/assets/file%20%281%29.png)

就像我们之前定义的型态，分别代表一个星期的七天。这个型态代表了我们想对打开的文件做什么。很简单吧。留意到我们的型态是 `IOMode` 而不是 `IO Mode`。`IO Mode` 代表的是一个 I/O action 包含了一个型态为 `Mode` 的值，但 `IOMode` 不过是一个阳春的 enumeration。

最后，他回传一个 I/O action 会将指定的文件用指定的模式打开。如果我们将 I/O action 绑定到某个东西，我们会得到一个 `Handle`。型态为 `Handle` 的值代表我们的文件在哪里。有了 handle 我们才知道要从哪个文件读取内容。想读取文件但不将文件绑定到 handle 上这样做是很蠢的。所以，我们将一个 handle 绑定到 `handle`。

接着一行，我们看到一个叫 **hGetContents** 的函数。他接了一个 `Handle`，所以他知道要从哪个文件读取内容并回传一个 `IO String`。一个包含了文件内容的 I/O action。这函数跟 `getContents` 差不多。唯一的差别是 `getContents` 会自动从标准输入读取内容（也就是终端），而 `hGetContents` 接了一个 file handle，这 file handle 告诉他读取哪个文件。除此之外，他们都是一样的。就像 `getContents`，`hGetContents` 不会把文件一次都拉到内存中，而是有必要才会读取。这非常酷，因为我们把 `contents` 当作是整个文件般用，但他实际上不在内存中。就算这是个很大的文件，`hGetContents` 也不会塞爆你的内存，而是只有必要的时候才会读取。

要留意文件的 handle 还有文件的内容两个概念的差异，在我们的程序中他们分别被绑定到 `handle` 跟 `contents` 两个名字。handle 是我们拿来区分文件的依据。如果你把整个文件系统想成一本厚厚的书，每个文件分别是其中的一个章节，handle 就像是书签一般标记了你现在正在阅读（或写入）哪一个章节，而内容则是章节本身。

我们使用 `putStr contents` 打印出内容到标准输出，然后我们用了 **hClose**。他接受一个 handle 然后回传一个关掉文件的 I/O action。在用了 `openFile` 之后，你必须自己把文件关掉。

要达到我们目的的另一种方式是使用 **withFile**，他的 type signature 是 `withFile :: FilePath -> IOMode -> (Handle -> IO a) -> IO a`。他接受一个文件路径，一个 `IOMode` 以及一个函数，这函数则接受一个 handle 跟一个 I/O action。`withFile` 最后回传一个会打开文件，对文件作某件事然后关掉文件的 I/O action。处理的结果是包在最后的 I/O action 中，这结果跟我们给的函数的回传是相同的。这听起来有些复杂，但其实很简单，特别是我们有 lambda，来看看我们用 `withFile` 改写前面程序的一个范例：

```haskell
import System.IO

main = do
    withFile "girlfriend.txt" ReadMode (\handle -> do
            contents <- hGetContents handle
            putStr contents)
```

正如你看到的，程序跟之前的看起来很像。`(\handle -> ... )` 是一个接受 handle 并回传 I/O action 的函数，他通常都是用 lambda 来表示。我们需要一个回传 I/O action 的函数的理由而不是一个本身作处理并关掉文件的 I/O action，是因为这样一来那个 I/O action 不会知道他是对哪个文件在做处理。用 `withFile` 的话，`withFile` 会打开文件并把 handle 传给我们给他的函数，之后他则拿到一个 I/O action，然后作成一个我们描述的 I/O action，最后关上文件。例如我们可以这样自己作一个 `withFile`：

```haskell
withFile' :: FilePath -> IOMode -> (Handle -> IO a) -> IO a
withFile' path mode f = do
    handle <- openFile path mode
    result <- f handle
    hClose handle
    return result
```

![](../../.gitbook/assets/edd.png)

我们知道要回传的是一个 I/O action，所以我们先放一个 do。首先我们打开文件，得到一个 handle。然后我们 apply `handle` 到我们的函数，并得到一个做事的 I/O action。我们绑定那个 I/O action 到 `result` 这个名字，关上 handle 并 `return result`。`return` 的作用把从 `f` 得到的结果包在 I/O action 中，这样一来 I/O action 中就包含了 `f handle` 得到的结果。如果 `f handle` 回传一个从标准输入读去数行并写到文件然后回传读入的行数的 I/O action，在 `withFile'` 的情形中，最后的 I/O action 就会包含读入的行数。

就像 `hGetContents` 对应 `getContents` 一样，只不过是针对某个文件。我们也有 **hGetLine**、**hPutStr**、**hPutStrLn**、**hGetChar** 等等。他们分别是少了 h 的那些函数的对应。只不过他们要多拿一个 handle 当参数，并且是针对特定文件而不是标准输出或标准输入。像是 `putStrLn` 是一个接受一个字串并回传一个打印出加了换行字符的字串的 I/O action 的函数。`hPutStrLn` 接受一个 handle 跟一个字串，回传一个打印出加了换行字符的字串到文件的 I/O action。以此类推，`hGetLine` 接受一个 handle 然后回传一个从文件读取一行的 I/O action。

读取文件并对他们的字串内容作些处理实在太常见了，常见到我们有三个函数来更进一步简化我们的工作。

**readFile** 的 type signature 是 `readFile :: FilePath -> IO String`。记住，`FilePath` 不过是 `String` 的一个别名。`readFile` 接受一个文件路径，回传一个惰性读取我们文件的 I/O action。然后将文件的内容绑定到某个字串。他比起先 `openFile`，绑定 handle，然后 `hGetContents` 要好用多了。这边是一个用 `readFile` 改写之前例子的范例：

```haskell
import System.IO

main = do
    contents <- readFile "girlfriend.txt"
    putStr contents
```

由于我们拿不到 handle，所以我们也无法关掉他。这件事 Haskell 的 `readFile` 在背后帮我们做了。

**writeFile** 的型态是 `writefile :: FilePath -> String -> IO ()`。他接受一个文件路径，以及一个要写到文件中的字串，并回传一个写入动作的 I/O action。如果这个文件已经存在了，他会先把文件内容都砍了再写入。下面示范了如何把 girlfriend.txt 的内容转成大写然后写入到 girlfriendcaps.txt 中

```haskell
import System.IO
import Data.Char

main = do
    contents <- readFile "girlfriend.txt"
    writeFile "girlfriendcaps.txt" (map toUpper contents)
```

```haskell
$ runhaskell girlfriendtocaps.hs
$ cat girlfriendcaps.txt
HEY! HEY! YOU! YOU!
I DON'T LIKE YOUR GIRLFRIEND!
NO WAY! NO WAY!
I THINK YOU NEED A NEW ONE!
```

**appendFile** 的型态很像 `writeFile`，只是 `appendFile` 并不会在文件存在时把文件内容砍掉而是接在后面。

假设我们有一个文件叫 todo.txt\`\`，里面每一行是一件要做的事情。现在我们写一个程序，从标准输入接受一行将他加到我们的 to-do list 中。

```haskell
import System.IO

main = do
    todoItem <- getLine
    appendFile "todo.txt" (todoItem ++ "\n")
```

```haskell
$ runhaskell appendtodo.hs
Iron the dishes
$ runhaskell appendtodo.hs
Dust the dog
$ runhaskell appendtodo.hs
Take salad out of the oven
$ cat todo.txt
Iron the dishes
Dust the dog
Take salad out of the oven
```

由于 `getLine` 回传的值不会有换行字符，我们需要在每一行最后加上 `"\n"`。

还有一件事，我们提到 `contents <- hGetContents handle` 是惰性 I/O，不会将文件一次都读到内存中。 所以像这样写的话：

```haskell
main = do
    withFile "something.txt" ReadMode (\handle -> do
        contents <- hGetContents handle
        putStr contents)
```

实际上像是用一个 pipe 把文件弄到标准输出。正如你可以把 list 想成 stream 一样，你也可以把文件想成 stream。他会每次读一行然后打印到终端上。你也许会问这个 pipe 究竟一次可以塞多少东西，读去硬盘的频率究竟是多少？对于文本档而言，缺省的 buffer 通常是 line-buffering。这代表一次被读进来的大小是一行。这也是为什么在这个 case 我们是一行一行处理。对于 binary file 而言，缺省的 buffer 是 block-buffering。这代表我们是一个 chunk 一个 chunk 去读得。而一个 chunk 的大小是根据操作系统不同而不同。

你能用 `hSetBuffering` 来控制 buffer 的行为。他接受一个 handle 跟一个 `BufferMode`，回传一个会设置 buffer 行为的 I/O action。`BufferMode` 是一个 enumeration 型态，他可能的值有：`NoBuffering`, `LineBuffering` 或 `BlockBuffering (Maybe Int)`。其中 `Maybe Int` 是表示一个 chunck 有几个 byte。如果他的值是 `Nothing`，则操作系统会帮你决定 chunk 的大小。`NoBuffering` 代表我们一次读一个 character。一般来说 `NoBuffering` 的表现很差，因为他访问硬盘的频率很高。

接下来是我们把之前的范例改写成用 2048 bytes 的 chunk 读取，而不是一行一行读。

```haskell
main = do
    withFile "something.txt" ReadMode (\handle -> do
        hSetBuffering handle $ BlockBuffering (Just 2048)
        contents <- hGetContents handle
        putStr contents)
```

用更大的 chunk 来读取对于减少访问硬盘的次数是有帮助的，特别是我们的文件其实是透过网络来访问。

我们也可以使用 **hFlush**，他接受一个 handle 并回传一个会 flush buffer 到文件的 I/O action。当我们使用 line-buffering 的时候，buffer 在每一行都会被 flush 到文件。当我们使用 block-buffering 的时候，是在我们读每一个 chunk 作 flush 的动作。flush 也会发生在关闭 handle 的时候。这代表当我们碰到换行字符的时候，读或写的动作都会停止并回报手边的数据。但我们能使用 `hFlush` 来强迫回报所有已经在 buffer 中的数据。经过 flushing 之后，数据也就能被其他程序看见。

把 block-buffering 的读取想成这样：你的马桶会在水箱有一加仑的水的时候自动冲水。所以你不断灌水进去直到一加仑，马桶就会自动冲水，在水里面的数据也就会被看到。但你也可以手动地按下冲水钮来冲水。他会让现有的水被冲走。冲水这个动作就是 `hFlush` 这个名字的含意。

我们已经写了一个将 item 加进 to-do list 里面的程序，现在我们想加进移除 item 的功能。我先把代码粘贴然后讲解他。我们会使用一些新面孔像是 `System.Directory` 以及 `System.IO` 里面的函数。

来看一下我们包含移除功能的程序:

```haskell
import System.IO
import System.Directory
import Data.List

main = do
    handle <- openFile "todo.txt" ReadMode
    (tempName, tempHandle) <- openTempFile "." "temp"
    contents <- hGetContents handle
    let todoTasks = lines contents
    let numberedTasks = zipWith (\n line -> show n ++ " - " ++ line) [0..] todoTasks
    putStrLn "These are your TO-DO items:"
    putStr $ unlines numberedTasks
    putStrLn "Which one do you want to delete?"
    numberString <- getLine
    let number = read numberString
    let newTodoItems = delete (todoTasks !! number) todoTasks
    hPutStr tempHandle $ unlines newTodoItems
    hClose handle
    hClose tempHandle
    removeFile "todo.txt"
    renameFile tempName "todo.txt"
```

一开始，我们用 read mode 打开 todo.txt，并把他绑定到 `handle`。

接着，我们使用了一个之前没用过在 `System.IO` 中的函数 **openTempFile**。他的名字浅显易懂。他接受一个暂存的文件夹跟一个样板文件名，然后打开一个暂存盘。我们使用 `"."` 当作我们的暂存文件夹，因为 `.` 在几乎任何操作系统中都代表了现在所在的文件夹。我们使用 `"temp"` 当作我们暂存盘的样板名，他代表暂存盘的名字会是 temp 接上某串随机字串。他回传一个创建暂存盘的 I/O action，然后那个 I/O action 的结果是一个 pair：暂存盘的名字跟一个 handle。我们当然可以随便开启一个 todo2.txt 这种名字的文件。但使用 `openTempFile` 会是比较好的作法，这样你不会不小心覆写任何文件。

我们不用 `getCurrentDirectory` 的来拿到现在所在文件夹而用 `"."` 的原因是 `.` 在 unix-like 系统跟 Windows 中都表示现在的文件夹。

然后，我们绑定 todo.txt 的内容成 `contents`。把字串断成一串字串，每个字串代表一行。`todoTasks` 就变成 `["Iron the dishes", "Dust the dog", "Take salad out of the oven"]`。我们用一个会把 3 跟 `"hey"` 变成 `"3 - hey"` 的函数，然后从 0 开始把这个串列 zip 起来。所以 `numberedTasks` 就是 `["0 - Iron the dishes", "1 - Dust the dog" ...`。我们用 `unlines` 把这个串列变成一行，然后打印到终端上。注意我们也有另一种作法，就是用 `mapM putStrLn numberedTasks`。

我们问用户他们想要删除哪一个并且等着他们输入一个数字。假设他们想要删除 1 号，那代表 `Dust the dog`，所以他们输入 `1`。于是 `numberString` 就代表 `"1"`。由于我们想要一个数字，而不是一个字串，所以我们用对 `1` 使用 `read`，并且绑定到 `number`。

还记得在 `Data.List` 中的 `delete` 跟 `!!` 吗？`!!` 回传某个 index 的元素，而 `delete` 删除在串列中第一个发现的元素，然后回传一个新的没有那个元素的串列。`(todoTasks !! number)` （number 代表 `1`） 回传 `"Dust the dog"`。我们把 `todoTasks` 去掉第一个 `"Dust the dog"` 后的串列绑定到 `newTodoItems`，然后用 `unlines` 变成一行然后写到我们所打开的暂存盘。旧有的文件并没有变动，而暂存盘包含砍掉那一行后的所有内容。

在我们关掉源文件跟暂存盘之后我们用 **removeFile** 来移除原本的文件。他接受一个文件路径并且删除文件。删除旧得 todo.txt 之后，我们用 **renameFile** 来将暂存盘重命名成 todo.txt。特别留意 `removeFile` 跟 `renameFile`（两个都在 `System.Directory` 中）接受的是文件路径，而不是 handle。

这就是我们要的，实际上我们可以用更少行写出同样的程序，但我们很小心地避免覆写任何文件，并询问操作系统我们可以把暂存盘摆在哪？让我们来执行看看。

```haskell
$ runhaskell deletetodo.hs
These are your TO-DO items:
0 - Iron the dishes
1 - Dust the dog
2 - Take salad out of the oven
Which one do you want to delete?
1

$ cat todo.txt
Iron the dishes
Take salad out of the oven

$ runhaskell deletetodo.hs
These are your TO-DO items:
0 - Iron the dishes
1 - Take salad out of the oven
Which one do you want to delete?
0

$ cat todo.txt
Take salad out of the oven
```

## 命令行参数

![](../../.gitbook/assets/arguments.png)

如果你想要写一个在终端里运行的程序，处理命令行参数是不可或缺的。幸运的是，利用 Haskell 的 Standard Libary 能让我们有效地处理命令行参数。

在之前的章节中，我们写了一个能将 to-do item 加进或移除 to-do list 的一个程序。但我们的写法有两个问题。第一个是我们把放 to-do list 的文件名称给写死了。我们擅自决定用户不会有很多个 to-do lists，就把文件命名为 todo.txt。

一种解决的方法是每次都询问用户他们想将他们的 to-do list 放进哪个文件。我们在用户要删除的时候也采用这种方式。这是一种可以运作的方式，但不太能被接受，因为他需要用户运行程序，等待程序询问才能回答。这被称为交互式的程序，但讨厌的地方在当你想要自动化执行程序的时候，好比说写成 script，这会让你的 script 写起来比较困难。

这也是为什么有时候让用户在执行的时候就告诉程序他们要什么会比较好，而不是让程序去问用户要什么。比较好的方式是让用户透过命令行参数告诉程序他们想要什么。

在 `System.Environment` 模块当中有两个很酷的 I/O actions，一个是 **getArgs**，他的 type 是 `getArgs :: IO [String]`，他是一个拿取命令行参数的 I/O action，并把结果放在包含的一个串列中。**getProgName** 的型态是 `getProgName :: IO String`，他则是一个 I/O action 包含了程序的名称。

我们来看一个展现他们功能的程序。

```haskell
import System.Environment
import Data.List

main = do
    args <- getArgs
    progName <- getProgName
    putStrLn "The arguments are:"
    mapM putStrLn args
    putStrLn "The program name is:"
    putStrLn progName
```

我们将 `getArgs` 跟 `progName` 分别绑定到 `args` 跟 `progName`。我们打印出 `The arguments are:` 以及在 `args` 中的每个参数。最后，我们打印出程序的名字。我们把程序编译成 `arg-test`。

```haskell
$ ./arg-test first second w00t "multi word arg"
The arguments are:
first
second
w00t
multi word arg
The program name is:
arg-test
```

知道了这些函数现在你能写几个很酷的命令行程序。在之前的章节，我们写了一个程序来加入待作事项，也写了另一个程序删除事项。现在我们要把两个程序合起来，他会根据命令行参数来决定该做的事情。我们也会让程序可以处理不同的文件，而不是只有 todo.txt

我们叫这程序 todo，他会作三件事：

```text
# 查看待作事项
# 加入待作事项
# 删除待作事项
```

我们暂不考虑不合法的输入这件事。

我们的程序要像这样运作：假如我们要加入 `Find the magic sword of power`，则我们会打 `todo add todo.txt "Find the magic sword of power"`。要查看事项我们则会打 `todo view todo.txt`，如果要移除事项二则会打 `todo remove todo.txt 2`

我们先作一个分发的 association list。他会把命令行参数当作 key，而对应的处理函数当作 value。这些函数的型态都是 `[String] -> IO ()`。他们会接受命令行参数的串列并回传对应的查看，加入以及删除的 I/O action。

```haskell
import System.Environment
import System.Directory
import System.IO
import Data.List

dispatch :: [(String, [String] -> IO ())]
dispatch =  [ ("add", add)
            , ("view", view)
            , ("remove", remove)
            ]
```

我们定义了 `main`，`add`，`view` 跟 `remove`，就从 `main` 开始讲吧：

```haskell
main = do
    (command:args) <- getArgs
    let (Just action) = lookup command dispatch
    action args
```

首先，我们取出参数并把他们绑定到 `(command:args)`。如果你还记得 pattern matching，这么做会把第一个参数绑定到 `command`，把其他的绑定到 `args`。如果我们像这样执行程序 `todo add todo.txt "Spank the monkey"`，`command` 会变成 `"add"`，而 `args` 会变成 `["todo.txt", "Spank the monkey"]`。

在下一行，我们在一个分派的串列中寻到我们的指令是哪个。由于 `"add"` 指向 `add`，我们的结果便是 `Just add`。我们再度使用了 pattern matching 来把我们的函数从 `Maybe` 中取出。但如果我们想要的指令不在分派的串列中呢？那样 lookup 就会回传 `Nothing`，但我们这边并不特别处理失败的情况，所以 pattern matching 会失败然后我们的程序就会当掉。

最后，我们用剩下的参数调用 `action` 这个函数。他会还传一个加入 item，显示所有 items 或者删除 item 的 I/O action。由于这个 I/O action 是在 `main` 的 do block 中，他最后会被执行。如果我们的 `action` 函数是 `add`，他就会被喂 `args` 然后回传一个加入 `Spank the monkey` 到 todo.txt 中的 I/O action。

我们剩下要做的就是实作 `add`，`view` 跟 `remove`，我们从 `add` 开始：

```haskell
add :: [String] -> IO ()
add [fileName, todoItem] = appendFile fileName (todoItem ++ "\n")
```

如果我们这样执行程序 `todo add todo.txt "Spank the monkey"`，则 `"add"` 会被绑定到 `command`，而 `["todo.txt", "Spank the monkey"]` 会被带到从 dispatch list 中拿到的函数。

由于我们不处理不合法的输入，我们只针对这两项作 pattern matching，然后回传一个附加一行到文件末尾的 I/O action。

接着，我们来实作查看串列。如果我们想要查看所有 items，我们会 `todo view todo.txt`。所以 `command` 会是 `"view"`，而 `args` 会是 `["todo.txt"]`。

```haskell
view :: [String] -> IO ()
view [fileName] = do
    contents <- readFile fileName
    let todoTasks = lines contents
    numberedTasks = zipWith (\n line -> show n ++ " - " ++ line) [0..] todoTasks
    putStr $ unlines numberedTasks
```

这跟我们之前删除文件的程序差不多，只是我们是在显示内容而已，

最后，我们要来实作 `remove`。他基本上跟之前写的只有删除功能的程序很像，所以如果你不知道删除是怎么做的，可以去看之前的解释。主要的差别是我们不写死 todo.txt，而是从参数取得。我们也不会提示用户要删除哪一号的 item，而是从参数取得。

```haskell
remove :: [String] -> IO ()
remove [fileName, numberString] = do
    handle <- openFile fileName ReadMode
    (tempName, tempHandle) <- openTempFile "." "temp"
    contents <- hGetContents handle
    let number = read numberString
        todoTasks = lines contents
        newTodoItems = delete (todoTasks !! number) todoTasks
    hPutStr tempHandle $ unlines newTodoItems
    hClose handle
    hClose tempHandle
    removeFile fileName
    renameFile tempName fileName
```

我们打开 `fileName` 的文件以及一个暂存。删除用户要我们删的那一行后，把文件内容写到暂存盘。砍掉原本的文件然后把暂存盘重命名成 `fileName`。

来看看完整的程序。

```haskell
import System.Environment
import System.Directory
import System.IO
import Data.List

dispatch :: [(String, [String] -> IO ())]
dispatch =  [ ("add", add)
            , ("view", view)
            , ("remove", remove)
            ]

main = do
    (command:args) <- getArgs
    let (Just action) = lookup command dispatch
    action args

add :: [String] -> IO ()
add [fileName, todoItem] = appendFile fileName (todoItem ++ "\n")

view :: [String] -> IO ()
view [fileName] = do
    contents <- readFile fileName
    let todoTasks = lines contents
        numberedTasks = zipWith (\n line -> show n ++ " - " ++ line) [0..] todoTasks
    putStr $ unlines numberedTasks

remove :: [String] -> IO ()
remove [fileName, numberString] = do
    handle <- openFile fileName ReadMode
    (tempName, tempHandle) <- openTempFile "." "temp"
    contents <- hGetContents handle
    let number = read numberString
        todoTasks = lines contents
        newTodoItems = delete (todoTasks !! number) todoTasks
    hPutStr tempHandle $ unlines newTodoItems
    hClose handle
    hClose tempHandle
    removeFile fileName
    renameFile tempName fileName
```

![](../../.gitbook/assets/salad%20%281%29.png)

总结我们的程序：我们做了一个 dispatch association，将指令对应到一些会接受命令行参数并回传 I/O action 的函数。我们知道用户下了什么命令，并根据那个命令从 dispatch list 取出对影的函数。我们用剩下的命令行参数调用哪些函数而得到一些作相对应事情的 I/O action。然后便执行那些 I/O action。

在其他编程语言，我们可能会用一个大的 switch case 来实作，但使用高端函数让我们可以要 dispatch list 给我们要的函数，并要那些函数给我们适当的 I/O action。

让我们看看执行结果。

```haskell
$ ./todo view todo.txt
0 - Iron the dishes
1 - Dust the dog
2 - Take salad out of the oven

$ ./todo add todo.txt "Pick up children from drycleaners"

$ ./todo view todo.txt
0 - Iron the dishes
1 - Dust the dog
2 - Take salad out of the oven
3 - Pick up children from drycleaners

$ ./todo remove todo.txt 2

$ ./todo view todo.txt
0 - Iron the dishes
1 - Dust the dog
2 - Pick up children from drycleaners
```

要再另外加新的选项也是很容易。只要在 dispatch list 加入新的会作你要的事情函数。你可以试试实作一个 `bump` 函数，接受一个文件跟一个 task number，他会回传一个把那个 task 搬到 to-do list 顶端的 I/O action。

对于不合法的输入你也可以让程序结束地漂亮一点。\(例如用户输入了 `todo UP YOURS HAHAHAHA`\)可以作一个回报错误的 I/O action \(例如 \`\`errorExist :: IO \(\)\)检查有没有不合法的输入，如果有便执行这个回报错误的 I/O action。我们之后会谈另一个可能，就是用 exception。

## 乱数

![](../../.gitbook/assets/random.png)

在许多情况下，你写程序会需要些随机的数据。或许你在制作一个游戏，在游戏中你需要掷骰子。或是你需要测试程序的测试数据。精准一点地说，我们需要 pseudo-random 的数据，我们知道真正的随机数据好比是一只猴子拿着起司跟奶油骑在单轮车上，任何事情都会发生。在这个章节，我们要看看如何让 Haskell 产生些 pseudo-random 的数据。

在大多数其他的编程语言中，会给你一些函数能让你拿到些随机乱数。每调用一次他就会拿到一个不同的数字。那在 Haskell 中是如何？要记住 Haskell 是一个纯粹函数式语言。代表任何东西都具有 referential transparency。那代表你喂给一个函数相同的参数，不管怎么调用都是回传相同的结果。这很新奇的原因是因为他让我们理解程序的方式不同，而且可以让我们延迟计算，直到我们真正需要他。如果我调用一个函数，我可以确定他不会乱来。我真正在乎的是他的结果。然而，这会造成在乱数的情况有点复杂。如果我有一个函数像这样：

```haskell
randomNumber :: (Num a) => a
randomNumber = 4
```

由于他永远回传 `4`，所以对于乱数的情形而言是没什么意义。就算 4 这个结果是掷骰子来的也没有意义。

其他的编程语言是怎么产生乱数的呢？他们可能随便拿取一些电脑的信息，像是现在的时间，你怎么移动你的鼠标，以及周围的声音。根据这些算出一个数值让他看起来好像随机的。那些要素算出来的结果可能在每个时间都不同，所以你会拿到不同的随机数字。

所以说在 Haskell 中，假如我们能作一个函数，他会接受一个具随机性的参数，然后根据那些信息还传一个数值。

在 `System.Random` 模块中。他包含所有满足我们需求的函数。让我们先来看其中一个，就是 **random**。他的型态是 `random :: (RandomGen g, Random a) => g -> (a, g)`。哇，出现了新的 typeclass。**RandomGen** typeclass 是指那些可以当作乱源的型态。而**Random** typeclass 则是可以装乱数的型态。一个布尔值可以是随机值，不是 `True` 就是 `False`。一个整数可以是随机的好多不同值。那你会问，函数可以是一个随机值吗？我不这么认为。如果我们试着翻译 `random` 的型态宣告，大概会是这样：他接受一个 random generator \(乱源所在\)，然后回传一个随机值以及一个新的 random generator。为什么他要回传一个新的 random generator 呢？就是下面我们要讲的。

要使用 `random` 函数， 我们必须要了解 random generator。 在 `System.Random` 中有一个很酷的型态，叫做 **StdGen**， 他是 `RandomGen` 的一个 instance。 我们可以自己手动作一个 `StdGen` 也可以告诉系统给我们一个现成的。

要自己做一个 random generator，要使用 **mkStdGen** 这个函数。他的型态是 `mkStdGen :: Int -> StdGen`。他接受一个整数，然后根据这个整数会给一个 random generator。让我们来试一下 `random` 以及 `mkStdGen`，用他们产生一个乱数吧。

```haskell
ghci> random (mkStdGen 100)
```

```haskell
<interactive>:1:0:
    Ambiguous type variable `a' in the constraint:
        `Random a' arising from a use of `random' at <interactive>:1:0-20
    Probable fix: add a type signature that fixes these type variable(s)  `
```

这是什么？由于 `random` 函数会回传 `Random` typeclass 中任何一种型态，所以我们必须告诉 Haskell 我们是要哪一种型态。不要忘了我们是回传 random value 跟 random generator 的一个 pair

```haskell
ghci> random (mkStdGen 100) :: (Int, StdGen)
(-1352021624,651872571 1655838864)
```

我们终于有了一个看起来像乱数的数字。tuple 的第一个部份是我们的乱数，而第二个部份是一个新的 random generator 的文本表示。如果我们用相同的 random generator 再调用 `random` 一遍呢？

```haskell
ghci> random (mkStdGen 100) :: (Int, StdGen)
(-1352021624,651872571 1655838864)
```

不易外地我们得到相同的结果。所以我们试试用不同的 random generator 作为我们的参数。

```haskell
ghci> random (mkStdGen 949494) :: (Int, StdGen)
(539963926,466647808 1655838864)
```

很好，我们拿到了不同的数字。我们可以用不同的型态标志来拿到不同型态的乱数

```haskell
ghci> random (mkStdGen 949488) :: (Float, StdGen)
(0.8938442,1597344447 1655838864)
ghci> random (mkStdGen 949488) :: (Bool, StdGen)
(False,1485632275 40692)
ghci> random (mkStdGen 949488) :: (Integer, StdGen)
(1691547873,1597344447 1655838864)
```

让我们写一个仿真丢三次铜板的函数。假如 `random` 不同时回传一个乱数以及一个新的 random generator，我们就必须让这函数接受三个 random generators 让他们每个回传一个掷铜板的结果。但那样听起来怪怪的，加入一个 generator 可以产生一个型态是 `Int` 的乱数，他应该可以产生掷三次铜板的结果（总共才八个组合）。这就是 `random` 为什么要回传一个新的 generator 的关键了。

我们将一个铜板表示成 `Bool`。`True` 代表反面，`False` 代表正面。

```haskell
threeCoins :: StdGen -> (Bool, Bool, Bool)
threeCoins gen =
    let (firstCoin, newGen) = random gen
    (secondCoin, newGen') = random newGen
    (thirdCoin, newGen'') = random newGen'
    in  (firstCoin, secondCoin, thirdCoin)
```

我们用我们拿来当参数的 generator 调用 `random` 并得到一个掷铜板的结果跟一个新的 generator。然后我们再用新的 generator 调用他一遍，来得到第二个掷铜板的结果。对于第三个掷铜板的结果也是如法炮制。如果我们一直都用同样的 generator，那所有的结果都会是相同的值。也就是不是 `(False, False, False)` 就是 `(True, True, True)`。

```haskell
ghci> threeCoins (mkStdGen 21)
(True,True,True)
ghci> threeCoins (mkStdGen 22)
(True,False,True)
ghci> threeCoins (mkStdGen 943)
(True,False,True)
ghci> threeCoins (mkStdGen 944)
(True,True,True)
```

留意我们不需要写 `random gen :: (Bool, StdGen)`。那是因为我们已经在函数的型态宣告那边就表明我们要的是布林。而 Haskell 可以推敲出我们要的是布尔值。

假如我们要的是掷四次？甚至五次呢？有一个函数叫 **randoms**，他接受一个 generator 并回传一个无穷串行。

```haskell
ghci> take 5 $ randoms (mkStdGen 11) :: [Int]
[-1807975507,545074951,-1015194702,-1622477312,-502893664]
ghci> take 5 $ randoms (mkStdGen 11) :: [Bool]
[True,True,True,True,False]
ghci> take 5 $ randoms (mkStdGen 11) :: [Float]
[7.904789e-2,0.62691015,0.26363158,0.12223756,0.38291094]
```

为什么 `randoms` 不另外多回传一个新的 generator 呢？我们可以这样地实作 `randoms`

```haskell
randoms' :: (RandomGen g, Random a) => g -> [a]
randoms' gen = let (value, newGen) = random gen in value:randoms' newGen
```

一个递归的定义。我们由现在的 generator 拿到一个乱数跟一个新的 generator，然后制作一个 list，list 的第一个值是那个乱数，而 list 的其余部份是根据新的 generator 产生出的其余乱数们。由于我们可能产生出无限的乱数，所以不可能回传一个新的 generator。

我们可以写一个函数，他会回传有限个乱数跟一个新的 generator

```haskell
finiteRandoms :: (RandomGen g, Random a, Num n, Eq n) => n -> g -> ([a], g)
finiteRandoms 0 gen = ([], gen)
finiteRandoms n gen =
    let (value, newGen) = random gen
        (restOfList, finalGen) = finiteRandoms (n-1) newGen
    in  (value:restOfList, finalGen)
```

又是一个递归的定义。我们说如果我们要 0 个乱数，我们便回传一个空的 list 跟原本给我们的 generator。对于其他数量的乱数，我们先拿一个乱数跟一个新的 generator。这一个乱数便是 list 的第一个数字。然后 list 中剩下的便是 n-1 个由新的 generator 产生出的乱数。然后我们回传整个 list 跟最后一个产生完 n-1 个乱数后 generator。

如果我们要的是在某个范围内的乱数呢？现在拿到的乱数要不是太大就是太小。如果我们想要的是骰子上的数字呢？**randomR** 能满足我们的需求。他的型态是 `randomR :: (RandomGen g, Random a) :: (a, a) -> g -> (a, g)`，代表他有点类似 `random`。只不过他的第一个参数是一对数目，定义了最后产生乱数的上界以及下界。

```haskell
ghci> randomR (1,6) (mkStdGen 359353)
(6,1494289578 40692)
ghci> randomR (1,6) (mkStdGen 35935335)
(3,1250031057 40692)
```

另外也有一个 **randomRs** 的函数，他会产生一连串在给定范围内的乱数：

```haskell
ghci> take 10 $ randomRs ('a','z') (mkStdGen 3) :: [Char]
"ndkxbvmomg"
```

这结果看起来像是一个安全性很好的密码。

你会问你自己，这一单元跟 I/O 有关系吗？到现在为止还没出现任何跟 I/O 有关的东西。到现在为止我们都是手动地做我们的 random generator。但那样的问题是，程序永远都会回传同样的乱数。这在真实世界中的程序是不能接受的。这也是为什么 `System.Random` 要提供 **getStdGen** 这个 I/O action，他的型态是 `IO StdGen`。当你的程序执行时，他会跟系统要一个 random generator，并存成一个 global generator。`getStdGen` 会替你拿那个 global random generator 并把他绑定到某个名称上。

这里有一个简单的产生随机字串的程序。

```haskell
import System.Random

main = do
    gen <- getStdGen
    putStr $ take 20 (randomRs ('a','z') gen)
```

```haskell
$ runhaskell random_string.hs
pybphhzzhuepknbykxhe
$ runhaskell random_string.hs
eiqgcxykivpudlsvvjpg
$ runhaskell random_string.hs
nzdceoconysdgcyqjruo
$ runhaskell random_string.hs
bakzhnnuzrkgvesqplrx
```

要当心当我们连续两次调用 `getStdGent` 的时候，实际上都会回传同样的 global generator。像这样：

```haskell
import System.Random

main = do
    gen <- getStdGen
    putStrLn $ take 20 (randomRs ('a','z') gen)
    gen2 <- getStdGen
    putStr $ take 20 (randomRs ('a','z') gen2)
```

你会打印出两次同样的字串。要能得到两个不同的字串是建立一个无限的 stream，然后拿前 20 个字当作第一个字串，拿下 20 个字当作第二个字串。要这么做，我们需要在 `Data.List` 中的 `splitAt` 函数。他会把一个 list 根据给定的 index 切成一个 tuple，tuple 的第一部份就是切断的前半，第二个部份就是切断的后半。

```haskell
import System.Random
import Data.List

main = do
    gen <- getStdGen
    let randomChars = randomRs ('a','z') gen
        (first20, rest) = splitAt 20 randomChars
        (second20, _) = splitAt 20 rest
    putStrLn first20
    putStr second20
```

另一种方法是用 **newStdGen** 这个 I/O action，他会把现有的 random generator 分成两个新的 generators。然后会把其中一个指定成 global generator，并回传另一个。

```haskell
import System.Random

main = do
    gen <- getStdGen
    putStrLn $ take 20 (randomRs ('a','z') gen)
    gen' <- newStdGen
    putStr $ take 20 (randomRs ('a','z') gen')
```

当我们绑定 `newStdGen` 的时候我们不只是会拿到一个新的 generator，global generator 也会被重新指定。所以再调用一次 `getStdGen` 并绑定到某个名称的话，我们就会拿到跟 `gen` 不一样的 generator。

这边有一个小程序会让用户猜数字：

```haskell
import System.Random
import Control.Monad(when)

main = do
    gen <- getStdGen
    askForNumber gen

askForNumber :: StdGen -> IO ()
askForNumber gen = do
    let (randNumber, newGen) = randomR (1,10) gen :: (Int, StdGen)
    putStr "Which number in the range from 1 to 10 am I thinking of? "
    numberString <- getLine
    when (not $ null numberString) $ do
        let number = read numberString
        if randNumber == number
            then putStrLn "You are correct!"
            else putStrLn $ "Sorry, it was " ++ show randNumber
            askForNumber newGen
```

![](../../.gitbook/assets/jackofdiamonds.png)

我们写了一个 `askForNumber` 的函数，他接受一个 random generator 并回传一个问用户要数字并回答是否正确的 I/O action。在那个函数里面，我们先根据从参数拿到的 generator 产生一个乱数以及一个新的 generator，分别叫他们为 `randomNumber` 跟 `newGen`。假设那个产生的数字是 `7`。则我们要求用户猜我们握有的数字是什么。我们用 `getLine` 来将结果绑定到 `numberString` 上。当用户输入 `7`，`numberString` 就会是 `"7"`。接下来，我们用 `when` 来检查用户输入的是否是空字串。如果是，那一个空的 I/O action `return ()` 就会被回传。基本上就等于是结束程序的意思。如果不是，那 I/O action 就会被执行。我们用 `read` 来把 `numberString` 转成一个数字，所以 `number` 便会是 `7`。

```text
如果用户给我们一些 ``read`` 没办法读取的输入（像是 ``"haha"``），我们的程序便会当掉并打印出错误消息。 如果你不希望你的程序当掉，就用 **reads**，当读取失败的时候他会回传一个空的 list。当成功的时候他就回传一个 tuple，第一个部份是我们想要的数字，第二个部份是读取失败的字串。
```

我们检查如果输入的数字跟我们随机产生的数字一样，便提示用户恰当的消息。然后再递归地调用 `askForNumber`，只是会拿到一个新的 generator。就像之前的 generator 一样，他会给我们一个新的 I/O action。

`main` 的组成很简单，就是由拿取一个 random generator 跟调用 `askForNumber` 组成罢了。

来看看我们的程序：

```haskell
$ runhaskell guess_the_number.hs
Which number in the range from 1 to 10 am I thinking of? 4
Sorry, it was 3
Which number in the range from 1 to 10 am I thinking of? 10
You are correct!
Which number in the range from 1 to 10 am I thinking of? 2
Sorry, it was 4
Which number in the range from 1 to 10 am I thinking of? 5
Sorry, it was 10
Which number in the range from 1 to 10 am I thinking of?
```

用另一种方式写的话像这样：

```haskell
import System.Random
import Control.Monad(when)

main = do
    gen <- getStdGen
    let (randNumber, _) = randomR (1,10) gen :: (Int, StdGen)
    putStr "Which number in the range from 1 to 10 am I thinking of? "
    numberString <- getLine
    when (not $ null numberString) $ do
        let number = read numberString
        if randNumber == number
            then putStrLn "You are correct!"
            else putStrLn $ "Sorry, it was " ++ show randNumber
        newStdGen
        main
```

他非常类似我们之前的版本，只是不是递归地调用，而是把所有的工作都在 `main` 里面做掉。在告诉用户他们猜得是否正确之后，便更新 global generator 然后再一次调用 `main`。两种策略都是有效但我比较喜欢第一种方式。因为他在 `main` 里面做的事比较少，并提供我们一个可以重复使用的函数。

## Bytestrings

![](../../.gitbook/assets/chainchomp%20%281%29.png)

List 是一种有用又酷的数据结构。到目前为止，我们几乎无处不使用他。有好几个函数是专门处理 List 的，而 Haskell 惰性的性质又让我们可以用 filter 跟 map 来替换其他语言中的 for loop 跟 while loop。也由于 evaluation 只会发生在需要的时候，像 infinite list 也对于 Haskell 不成问题（甚至是 infinite list of infinite list）。这也是为什么 list 能被用来表达 stream，像是读取标准输入或是读取文件。我们可以打开文件然后读取内容成字串，即便实际上我们是需要的时候才会真正取读取。

然而，用字串来处理文件有一个缺点：就是他很慢。就像你所知道的，`String` 是一个 `[Char]` 的 type synonym。`Char` 没有一个固定的大小，因为他可能由好几个 byte 组成，好比说 Unicode。再加上 list 是惰性的。如果你有一个 list 像 `[1,2,3,4]`，他只会在需要的时候被 evaluate。所以整个 list 其实比较像是一个"保证"你会有一个 list。要记住 `[1,2,3,4]` 不过是 `1:2:3:4:[]` 的一个 syntactic sugar。当 list 的第一个元素被 evaluated 的时候，剩余的部份 `2:3:4:[]` 一样也只是一个"保证"你会有一个 list，以此类推。以此类推。以此类推。所以你可以想像成 list 是保证在你需要的时候会给你第一个元素，以及保证你会有剩下的部份当你还需要更多的时候。其实不难说服你这样做并不是一个最有效率的作法。

这样额外的负担在大多数时候不会造成困扰，但当我们要读取一个很大的文件的时候就是个问题了。这也是为什么 Haskell 要有 `bytestrings`。Bytestrings 有点像 list，但他每一个元素都是一个 byte \(8 bits\)，而且他们惰性的程度也是不同。

Bytestrings 有两种：strict 跟 lazy。Strict bytestrings 放在 `Data.ByteString`，他们把惰性的性质完全拿掉。不会有所谓任何的「保证」，一个 strict bytestring 就代表一连串的 bytes。因此你不会有一个无限长的 strict bytestrings。如果你 evaluate 第一个 byte，你就必须 evalute 整个 bytestring。这么做的优点是他会比较少 overhaed，因为他没有 "Thunk"（也就是用 Haskell 术语来说的「保证」）。缺点就是他可能会快速消耗你的内存，因为你把他们一次都读进了内存。

另一种 bytestring 是放在 `Data.ByteString.Lazy` 中。他们具有惰性，但又不像 list 那么极端。就像我们之前说的，List 的 thunk 个数是跟 list 中有几个元素一模一样。这也是为什么他们速度没办法满足一些特殊需求。Lazy bytestrings 则用另一种作法，他们被存在 chunks 中（不要跟 Thunk 搞混），每一个 chunk 的大小是 64K。所以如果你 evaluate lazy bytestring 中的 byte，则前 64K 会被 evaluated。在那个 chunck 之后，就是一些「保证」会有剩余的 chunk。lazy bytestrings 有点像装了一堆大小为 64K 的 strict bytestrings 的 list。当你用 lazy bytestring 处理一个文件的时候，他是一个 chunk 一个 chunk 去读。这很棒是因为他不会让我们一下使用大量的内存，而且 64K 有很高的可能性能够装进你 CPU 的 L2 Cache。

如果你大概看过 `Data.ByteString.Lazy` 的文档，你会看到到他有一堆函数的名称跟 `Data.List` 中的函数名称相同，只是出现的 type signature 是 `ByteString` 而不是 `[a]`，是 `Word8` 而不是 `a`。同样名称的函数基本上表现的行为跟 list 中的差不多。因为名称是一样的，所以必须用 qualified import 才不会在装载进 GHCI 的时候造成冲突。

```haskell
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString as S
```

`B` 中有 lazy bytestrings 跟对应的函数，而 `S` 中则有 strict 的版本。大多数时候我们是用 lazy 的版本。

**pack** 函数的 type signature 是 `pack :: [Word8] -> ByteString`。代表他接受一串型态为 `Word8` 的 bytes，并回传一个 `ByteString`。你能想像一个 lazy 的 list，要让他稍微不 lazy 一些，所以让他对于 64K lazy。

那 `Word8` 型态又是怎么一回事？。他就像 `Int`，只是他的范围比较小，介于 0-255 之间。他代表一个 8-bit 的数字。就像 `Int` 一样，他是属于 `Num` 这个 typeclass。例如我们知道 `5` 是 polymorphic 的，他能够表现成任何数值型态。其实 `Word8` 他也能表示。

```haskell
ghci> B.pack [99,97,110]
Chunk "can" Empty
ghci> B.pack [98..120]
Chunk "bcdefghijklmnopqrstuvwx" Empty
```

正如你看到的，你其实不必特别在意 `Word8`，因为型态系统会选择正确的型态。如果你试着用比较大的数字，像是 `336`。那对于 `Word8` 他就会变成 `80`。

我们把一些数值打包成 `ByteString`，使他们可以塞进一个 chunk 里面。`Empty` 之于 `ByteString` 就像 `[]` 之于 list 一样。

**unpack** 是 `pack` 的相反，他把一个 bytestring 变成一个 byte list。

**fromChunks** 接受一串 strict 的 bytestrings 并把他变成一串 lazy bytestring。**toChunks** 接受一个 lazy bytestrings 并将他变成一串 strict bytestrings。

```haskell
ghci> B.fromChunks [S.pack [40,41,42], S.pack [43,44,45], S.pack [46,47,48]]
Chunk "()*" (Chunk "+,-" (Chunk "./0" Empty))
```

如果你有很多小的 strict bytestrings 而且不想先将他们 join 起来（会耗损 memory）这样的作法是不错的。

bytestring 版本的 `:` 叫做 **cons**。他接受一个 byte 跟一个 bytestring，并把这个 byte 放到 bytestring 的前端。他是 lazy 的操作，即使 bytestring 的第一个 chunk 不是满的，他也会添加一个 chunk。这也是为什么当你要插入很多 bytes 的时候最好用 strict 版本的 `cons`，也就是 **cons'**。

```haskell
ghci> B.cons 85 $ B.pack [80,81,82,84]
Chunk "U" (Chunk "PQRT" Empty)
ghci> B.cons' 85 $ B.pack [80,81,82,84]
Chunk "UPQRT" Empty
ghci> foldr B.cons B.empty [50..60]
Chunk "2" (Chunk "3" (Chunk "4" (Chunk "5" (Chunk "6" (Chunk "7" (Chunk "8" (Chunk "9" (Chunk ":" (Chunk ";" (Chunk "<"
Empty))))))))))
ghci> foldr B.cons' B.empty [50..60]
Chunk "23456789:;<" Empty
```

你可以看到 **empty** 制造了一个空的 bytestring。也注意到 `cons` 跟 `cons'` 的差异了吗？有了 `foldr`，我们逐步地把一串数字从右边开始，一个个放到 bytestring 的前头。当我们用 `cons`，我们则得到一个 byte 一个 chunk 的结果，并不是我们要的。

bytestring 模块有一大票很像 `Data.List` 中的函数。包括了 `head`，`tail`，`init`，`null`，`length`，`map`，`reverse`，`foldl`，`foldr`，`concat`，`takeWhile`，`filter`，等等。

他也有表现得跟 `System.IO` 中一样的函数，只有 `Strings` 被换成了 `ByteString` 而已。像是 `System.IO` 中的 `readFile`，他的型态是 `readFile :: FilePath -> IO String`，而 bytestring 模块中的 **readFile** 则是 `readFile :: FilePath -> IO ByteString`。小心，如果你用了 strict bytestring 来读取一个文件，他会把文件内容都读进内存中。而使用 lazy bytestring，他则会读取 chunks。

让我们来写一个简单的程序，他从命令行接受两个文件名，然后拷贝第一个文件内容成第二个文件。虽然 `System.Directory` 中已经有一个函数叫 `copyFile`，但我们想要实作自己的版本。

```haskell
import System.Environment
import qualified Data.ByteString.Lazy as B

main = do
    (fileName1:fileName2:_) <- getArgs
    copyFile fileName1 fileName2

copyFile :: FilePath -> FilePath -> IO ()
copyFile source dest = do
    contents <- B.readFile source
    B.writeFile dest contents
```

我们写了自己的函数，他接受两个 `FilePath`（记住 `FilePath` 不过是 `String` 的同义词。）并回传一个 I/O action，他会用 bytestring 拷贝第一个文件至另一个。在 `main` 函数中，我们做的只是拿到命令行参数然后调用那个函数来拿到一个 I/O action。

```haskell
$ runhaskell bytestringcopy.hs something.txt ../../something.txt
```

就算我们不用 bytestring 来写，程序最后也会长得像这样。差别在于我们会用 `B.readFile` 跟 `B.writeFile` 而不是 `readFile` 跟 `writeFile`。有很大的可能性，就是你只要 import 文件并在函数前加上 qualified 模块名，就可以把一个用正常 String 的程序改成用 ByteString。也有可能你是要反过来做，但那也不难。

当你需要更好的性能来读取许多数据，尝试用 bytestring，有很大的机会你会用很小的力气改进很多性能。我通常用正常 String 来写程序，然后在性能不好的时候把他们改成 ByteString。

## Exceptions \(例外\)

![](../../.gitbook/assets/timber%20%281%29.png)

所有的编程语言都有要处理失败的情形。这就是人生。不同的语言有不同的处理方式。在 C 里面，我们通常用非正常范围的回传值（像是 `-1` 或 null）来回传错误。Java 跟 C\#则倾向于使用 exception 来处理失败的情况。当一个 exception 被丢出的时候，控制流程就会跳到我们做一些清理动作的地方，做完清理后 exception 被重新丢出，这样一些处理错误的代码可以完成他们的工作。

Haskell 有一个很棒的型态系统。Algebraic data types 允许像是 `Maybe` 或 `Either` 这种型态，我们能用这些型态来代表一些可能有或没有的结果。在 C 里面，在失败的时候回传 `-1` 是很常见的事。但他只对写程序的人有意义。如果我们不小心，我们有可能把这些错误码当作正常值来处理，便造成一些混乱。Haskell 的型态系统赋予我们更安全的环境。一个 `a -> Maybe b` 的函数指出了他会产生一个包含 `b` 的 `Just`，或是回传 `Nothing`。这型态跟 `a -> b` 是不同的，如果我们试着将两个函数混用，compiler 便会警告我们。

尽管有表达力够强的型态来辅助失败的情形，Haskell 仍然支持 exception，因为 exception 在 I/O 的 contexts 下是比较合理的。在处理 I/O 的时候会有一堆奇奇怪怪的事情发生，环境是很不能被信赖的。像是打开文件。文件有可能被 lock 起来，也有可能文件被移除了，或是整个硬盘都被拔掉。所以直接跳到处理错误的代码是很合理的。

我们了解到 I/O code 会丢出 exception 是件合理的事。至于 pure code 呢？其实他也能丢出 Exception。想想看 `div` 跟 `head` 两个案例。他们的型态是 `(Integral a) => a -> a -> a` 以及 `[a] -> a`。`Maybe` 跟 `Either` 都没有在他们的回传型态中，但他们都有可能失败。`div` 有可能除以零，而 `head` 有可能你传给他一个空的 list。

```haskell
ghci> 4 `div` 0
*** Exception: divide by zero
ghci> head []
*** Exception: Prelude.head: empty list
```

![](../../.gitbook/assets/police.png)

pure code 能丢出 Exception，但 Exception 只能在 I/O section 中被接到（也就是在 `main` 的 do block 中）这是因为在 pure code 中你不知道什么东西什么时候会被 evaluate。因为 lazy 特性的缘故，程序没有一个特定的执行顺序，但 I/O code 有。

先前我们谈过为什么在 I/O 部份的程序要越少越好。程序的逻辑部份尽量都放在 pure 的部份，因为 pure 的特性就是他们的结果只会根据函数的参数不同而改变。当思考 pure function 的时候，你只需要考虑他回传什么，因为除此之外他不会有任何副作用。这会让事情简单许多。尽管 I/O 的部份是难以避免的（像是打开文件之类），但最好是把 I/O 部份降到最低。Pure functions 缺省是 lazy，那代表我们不知道他什么时候会被 evaluate，不过我们也不该知道。然而，一旦 pure functions 需要丢出 Exception，他们何时被 evaluate 就很重要了。那是因为我们只有在 I/O 的部份才能接到 Exception。这很糟糕，因为我们说过希望 I/O 的部份越少越好。但如果我们不接 Exception，我们的程序就会当掉。这问题有解决办法吗？答案是不要在 pure code 里面使用 Exception。利用 Haskell 的型态系统，尽量使用 `Either` 或 `Maybe` 之类的型态来表示可能失败的计算。

这也是为什么我们要来看看怎么使用 I/O Excetion。I/O Exception 是当我们在 `main` 里面跟外界沟通失败而丢出的 Exception。例如我们尝试打开一个文件，结果发现他已经被删掉或是其他状况。来看看一个尝试打开命令行参数所指定文件名称，并计算里面有多少行的程序。

```haskell
import System.Environment
import System.IO

main = do (fileName:_) <- getArgs
            contents <- readFile fileName
            putStrLn $ "The file has " ++ show (length (lines contents)) ++ " lines!"
```

一个很简单的程序。我们使用 `getArgs` I/O action，并绑定第一个 string 到 `fileName`。然后我们绑定文件内容到 `contents`。最后，我们用 `lines` 来取得 line 的 list，并计算 list 的长度，并用 `show` 来转换数字成 string。他如我们想像的工作，但当我们给的文件名称不存在的时候呢？

```haskell
$ runhaskell linecount.hs i_dont_exist.txt
linecount.hs: i_dont_exist.txt: openFile: does not exist (No such file or directory)
```

GHC 丢了错误消息给我们，告诉我们文件不存在。然后程序就挂掉了。假如我们希望打印出比较好一些的错误消息呢？一种方式就是在打开文件前检查他存不存在。用 `System.Directory` 中的 **doesFileExist**。

```haskell
import System.Environment
import System.IO
import System.Directory

main = do (fileName:_) <- getArgs
            fileExists <- doesFileExist fileName
            if fileExists
                then do contents <- readFile fileName
                    putStrLn $ "The file has " ++ show (length (lines contents)) ++ " lines!"
                else do putStrLn "The file doesn't exist!"
```

由于 `doesFileExist` 的型态是 `doesFileExist :: FilePath -> IO Bool`，所以我们要写成 `fileExists <- doesFileExist fileName`。那代表他回传含有一个布尔值告诉我们文件存不存在的 I/O action。`doesFileExist` 是不能直接在 if expression 中使用的。

另一个解法是使用 Exception。在这个情境下使用 Exception 是没问题的。文件不存在这个 Exception 是在 I/O 中被丢出，所以在 I/O 中接起来也没什么不对。

要这样使用 Exception，我们必须使用 `System.IO.Error` 中的 **catch** 函数。他的型态是 `catch :: IO a -> (IOError -> IO a) -> IO a`。他接受两个参数，第一个是一个 I/O action。像是他可以接受一个打开文件的 I/O action。第二个是 handler。如果第一个参数的 I/O action 丢出了 Exception，则他会被传给 handler，他会决定要作些什么。所以整个 I/O action 的结果不是如预期中做完第一个参数的 I/O action，就是 handler 处理的结果。

![](../../.gitbook/assets/puppy.png)

如果你对其他语言像是 Java, Python 中 try-catch 的形式很熟，那 `catch` 其实跟他们很像。第一个参数就是其他语言中的 try block。第二个参数就是其他语言中的 catch block。其中 handler 只有在 exception 被丢出时才会被执行。

handler 接受一个 `IOError` 型态的值，他代表的是一个 I/O exception 已经发生了。他也带有一些 exception 本身的信息。至于这型态在语言中使如何被实作则是要看编译器。这代表我们没办法用 pattern matching 的方式来查看 `IOError`。就像我们不能用 pattern matching 来查看 `IO something` 的内容。但我们能用一些 predicate 来查看他们。

我们来看看一个展示 `catch` 的程序

```haskell
import System.Environment
import System.IO
import System.IO.Error

main = toTry `catch` handler

toTry :: IO ()
toTry = do (fileName:_) <- getArgs
            contents <- readFile fileName
            putStrLn $ "The file has " ++ show (length (lines contents)) ++ " lines!"

handler :: IOError -> IO ()
handler e = putStrLn "Whoops, had some trouble!"
```

首先你看到我们可以在关键字周围加上 backticks 来把 `catch` 当作 infix function 用，因为他刚好接受两个参数。这样使用让可读性变好。``toTry `catch` handler`` 跟 `catch toTry handler` 是一模一样的。`toTry` 是一个 I/O action，而 `handler` 接受一个 `IOError`，并回传一个当 exception 发生时被执行的 I/O action。

来看看执行的结果。

```haskell
$ runhaskell count_lines.hs i_exist.txt
The file has 3 lines!

$ runhaskell count_lines.hs i_dont_exist.txt
Whoops, had some trouble!
```

在 handler 里面我们并没有检查我们拿到的是什么样的 `IOError`，我们只是打印出 `"Whoops, had some trouble!"`。接住任何种类的 Exception 就跟其他语言一样，在 Haskell 中也不是一个好的习惯。假如其他种类的 Exception 发生了，好比说我们送一个中断指令，而我们没有接到的话会发生什么事？这就是为什么我们要做跟其他语言一样的事：就是检查我们拿到的是什么样的 Exception。如果说是我们要的 Exception，那就做对应的处理。如果不是，我们再重新丢出 Exception。我们把我们的程序这样修改，只接住文件不存在的 Exception。

```haskell
import System.Environment
import System.IO
import System.IO.Error

main = toTry `catch` handler

toTry :: IO ()
toTry = do (fileName:_) <- getArgs
            contents <- readFile fileName
            putStrLn $ "The file has " ++ show (length (lines contents)) ++ " lines!"

handler :: IOError -> IO ()
handler e
    | isDoesNotExistError e = putStrLn "The file doesn't exist!"
    | otherwise = ioError e
```

除了 handler 以外其他东西都没变，我们只接住我们想要的 I/O exception。这边使用了 `System.IO.Error` 中的函数 **isDoesNotExistError** 跟 **ioError**。`isDoesNotExistError` 是一个运作在 `IOError` 上的 predicate ，他代表他接受一个 `IOError` 然后回传 `True` 或 `False`，他的型态是 `isDoesNotExistError :: IOError -> Bool`。我们用他来判断是否这个错误是文件不存在所造成的。我们这边使用 guard，但其实也可以用 if else。如果 exception 不是由于文件不存在所造成的，我们就用 `ioEroror` 重新丢出接到的 exception。他的型态是 `ioError :: IOException -> IO a`，所以他接受一个 `IOError` 然后产生一个会丢出 exception 的 I/O action。那个 I/O action 的型态是 `IO a`，但他其实不会产生任何结果，所以他可以被当作是 `IO anything`。

所以有可能在 `toTry` 里面丢出的 exception 并不是文件不存在造成的，而 ``toTry `catch` handler`` 会接住再丢出来，很酷吧。

程序里面有好几个运作在 `IOError` 上的 I/O action，当其中一个没有被 evaluate 成 `True` 时，就会掉到下一个 guard。这些 predicate 分别为：

* **isAlreadyExistsError**
* **isDoesNotExistError**
* **isFullError**
* **isEOFError**
* **isIllegalOperation**
* **isPermissionError**
* **isUserError**

大部分的意思都是显而易见的。当我们用了 **userError** 来丢出 exception 的时候，`isUserError` 被 evaluate 成 `True`。例如说，你可以写 `ioError $ userError "remote computer unplugged!"`，尽管用 `Either` 或 `Maybe` 来表示可能的错误会比自己丢出 exception 更好。

所以你可能写一个像这样的 handler

```haskell
handler :: IOError -> IO ()
handler e
    | isDoesNotExistError e = putStrLn "The file doesn't exist!"
    | isFullError e = freeSomeSpace
    | isIllegalOperation e = notifyCops
    | otherwise = ioError e
```

其中 `notifyCops` 跟 `freeSomeSpace` 是一些你定义的 I/O action。如果 exception 不是你要的，记得要把他们重新丢出，不然你的程序可能只会安静地当掉。

`System.IO.Error` 也提供了一些能询问 exception 性质的函数，像是哪些 handle 造成错误，或哪些文件名造成错误。这些函数都是 `ioe` 当开头。而且你可以在文档中看到一整串详细数据。假设我们想要打印出造成错误的文件名。我们不能直接打印出从 `getArgs` 那边拿到的 `fileName`，因为只有 `IOError` 被传进 handler 中，而 handler 并不知道其他事情。一个函数只依赖于他所被调用时的参数。这也是为什么我们会用 **ioeGetFileName** 这函数，他的型态是 `ioeGetFileName :: IOError -> Maybe FilePath`。他接受一个 `IOError` 并回传一个 `FilePath`（他是 `String` 的同义词。）基本上他做的事就是从 `IOError` 中抽出文件路径。我们来修改一下我们的程序。

```haskell
import System.Environment
import System.IO
import System.IO.Error

main = toTry `catch` handler

toTry :: IO ()
toTry = do (fileName:_) <- getArgs
    contents <- readFile fileName
    putStrLn $ "The file has " ++ show (length (lines contents)) ++ " lines!"

handler :: IOError -> IO ()
handler e
    | isDoesNotExistError e =
        case ioeGetFileName e of Just path -> putStrLn $ "Whoops! File does not exist at: " ++ path
                                 Nothing -> putStrLn "Whoops! File does not exist at unknown location!"
    | otherwise = ioError e
```

在 `isDoesNotExistError` 是 `True` 的 guard 里面，我们在 case expression 中用 `e` 来调用 `ioeGetFileName`，然后用 pattern matching 拆出 `Maybe` 中的值。当你想要用 pattern matching 却又不想要写一个新的函数的时候，case expression 是你的好朋友。

你不想只用一个 `catch` 来接你 I/O part 中的所有 exception。你可以只在特定地方用 `catch` 接 exception，或你可以用不同的 handler。像这样：

```haskell
main = do toTry `catch` handler1
          thenTryThis `catch` handler2
          launchRockets
```

这边 `toTry` 使用 `handler1` 当作 handler，而 `thenTryThis` 用了 `handler2`。`launchRockets` 并不是 `catch` 的参数，所以如果有任何一个 exception 被丢出都会让我们的程序当掉，除非 `launchRockets` 使用 `catch` 来处理 exception。当然 `toTry`，`thenTryThis` 跟 `launchRockets` 都是 I/O actions，而且被 do syntax 绑在一起。这很像其他语言中的 try-catch blocks，你可以把一小段程序用 try-catch 包住，你可以自己调整该包多少进去。

现在你知道如何处理 I/O exception 了。我们并没有提到如何从 pure code 中丢出 exception，这是因为正如我们先前提到的，Haskell 提供了更好的办法来处理错误。就算是在可能会失败的 I/O action 中，我也倾向用 `IO (Either a b)`，代表他们是 I/O action，但当他们被执行，他们结果的型态是 `Either a b`，意思是不是 `Left a` 就是 `Right b`。

