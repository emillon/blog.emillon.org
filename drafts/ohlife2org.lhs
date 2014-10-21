---
title: From OhLife to org-mode with Parsec
author: Etienne Millon
tags: haskell parsec org
---

A few weeks ago, [OhLife] shut down. It is a shame, as I really loved the
service. Every day, it sent you an email asking how you day was. You reply with
a few words, or a few sentences, and it uploaded it into a (private) archive.
But the really awesome feature is that the daily mail used to include a past
random entry. So, you can realize that time passes...

Anyway, the service shut down and I could export my data before. I use [org
mode] to organize several other things (TODOs! agenda! playlists!), so I will
try to use it for this from now on. Hosting snippets of my life in an external
service was scary anyway :).

So, let's write a conversion script. This is a good occasion to use [Parsec].

Parsing
-------

(Insert explanation that this is a litterate haskell file)

> import Control.Applicative ((<*), (*>), (<*>), (<$>))
> import Control.Monad
> import Control.Monad.RWS
> import Text.ParserCombinators.Parsec

To parse things, you usually have several options:

  - do it by hand. Split on separators, count character offsets, etc.
  - use regular expressions. Yeah, you have two problems. I read xkcd too.
  - use a LR parser generator such as `yacc` or `happy`.
  - use parser combinators.

Parser generators have several drawbacks. First, they require to use a separate
tool to generate code. Also, they tend to be quite slow. Finally, they are
somewhat arcane to debug: sometimes you change an innocuous rule and get 30
shift/reduce conflicts. Good luck with that.

On the contrary, parser combinators are nice because they are written within the
same implementation language as the rest of your code. They're just functions.
Suppose that a language has string litterals and integer litterals, and that
both are constants. To define the rule for constants, you'll just declare

~~~~ {.haskell}
constant = string_lit <|> integer_lit
~~~~

It's very declarative. `(<|>)` takes two parsers and returns a new parser which
can parse either. This property makes your code kind of look like the text
you're trying to parse.

For example, in the case of the OhLife export, the texts looks like:

    YYYY-MM-DD

    Entry

    YYYY-MM-DD

    Entry

And the (actual) code to parse it is:

> entry :: Parser Entry
> entry = do
>   d <- date
>   eol
>   eol
>   l <- body
>   return $ Entry d l

And to parse several of them, we use the following code. We can remove the `eof`
and the parser will parse till an error occur, but is is better to detect
errors.

> entries :: Parser [Entry]
> entries =
>     many1 entry <* eof

Of course we need to define the `Entry` data type.

> data Entry =
>     Entry { entryDate :: Date
>           , entryBody :: String
>           }
>         deriving (Show)

Parsing a date is quite easy but needs a bit of explaining.

> data Date =
>     Date { dateYear :: String
>          , dateMonth :: String
>          , dateDay :: String
>          }
>         deriving (Eq, Show)
>
> number :: Parser String
> number = many1 digit
>
> date :: Parser Date
> date =
>     Date <$> (number <* char '-')
>          <*> (number <* char '-')
>          <*> (number)

Remember what I said about arcane? Well, applicative style is also a bit
strange. The main characteristic is the use of `<$>` and `<*>`: if you want to
parse a `a`, a `b`, and a `c` and pass the results to a function `f`, you can
express this with `f <$> a <*> b <*> c`. This is equivalent to the following
monadic code (it's a generalization of `liftM`):

~~~~ {.haskell}
do
    ra <- a
    rb <- b
    rc <- c
    return $ f a b c
~~~~

The other trick is the use of `<*`. It parses both of its arguments, but returns
only the left one. So, it is a way to parse the dash without returning it.

End of line is tricky here because the data is dos-formatted. Plain `newline`
does not work, we have to detect `'\r'`.

> eol :: Parser ()
> eol = void $ string "\r\n"

Some parsers don't return anything. For example, the `emptyLine` just consumes
an empty line and only returns a unit value.

Parsing the body is just a matter of reading lines and joining them.

> body :: Parser String
> body =
>     unlines <$> many1 line

And a `line` is a sequence of characters that are not a new line.

> line :: Parser String
> line =
>     notFollowedBy date *> (many $ noneOf "\r\n") <* eol

There are several tricks here. First, `*>` and `<*` make it possible to ignore
the results of `notFollowedBy` and `eol`. And
TODO notFollowedBy

We can try this using a define a function that will run the parser on `stdin` and
print the result.

> parseStdinOrFail :: IO [Entry]
> parseStdinOrFail = do
>     c <- getContents
>     case parse entries "(stdin)" c of
>         Left e -> error $ "Error parsing input: " ++ show e
>         Right r -> return r
>
> parseAndPrintStdin :: IO ()
> parseAndPrintStdin = do
>     es <- parseStdinOrFail
>     mapM_ print es

Printing
--------

Here comes the second part: emitting a `.org` file that contains this data.
For the rest of my journal, I will be adding entries to a date tree. This is a
org-mode structure that looks like the following:

    * 2014
    ** 2014-10
    *** 2014-10-19
    **** Entry
    XXXX
    *** 2014-10-20
    **** Entry
    YYYY

The tricky part will of course be to emit the headers at the correct position.
Several approaches are possible: filling an intermediate data structure is of
course an option, but as the list is already sorted it's possible to just keep
track of the current month and year.

> emitEntries :: [Entry] -> M ()
> emitEntries =
>     mapM_ emitEntry

Here `M` is a monad we'll be using to keep track of the different effects. We
need the followning capabilities:

  - remember the last month and year that have been emitted
  - write lines

So, a variant of RWS (Read, Write, State) will work. We don't need the Read
part, so it can be `()`; the Write part is a list of Strings; and the State part
is a record containing a year and a month.

> type M a = RWS () [String] EmitState a
>
> data EmitState =
>     EmitState { esYear :: String
>               , esMonth :: String
>               }

Using this we can write the code to emit an entry:

> emitEntry :: Entry -> M ()
> emitEntry e = do
>     EmitState lastYear lastMonth <- get
>     let Date year month day = entryDate e
>     when (year > lastYear) $ do
>         tell ["* " ++ year]
>         modify $ \ s -> s { esYear = year }
>     when ((year, month) > (lastYear, lastMonth)) $ do
>         tell ["** " ++ year ++ "-" ++ month]
>         modify $ \ s -> s { esYear = year }
>         modify $ \ s -> s { esMonth = month }
>     tell [ "*** " ++ year ++ "-" ++ month ++ "-" ++ day
>          , "**** Entry"
>          , entryBody e
>          ]

Gluing it together
------------------

And finally our main function. To set the initial state, we use the fact that
the empty string is "less" than other strings, so the first test will always
write a year entry (same for the month entry).

> main :: IO ()
> main = do
>     es <- parseStdinOrFail
>     let initState = EmitState "" ""
>         ((), _, w) = runRWS (emitEntries es) () initState
>     mapM_ putStrLn w

[OhLife]: http://ohlife.com
[org mode]: http://orgmode.org/
[Parsec]: http://www.haskell.org/haskellwiki/Parsec
