# A simple password generator

```
gen-passwd - a password generator

Usage: gen-passwd [--short] [--special] [--count INT] [--delimiter ARG]
                  [--acrostic ARG]
  Print a wordlist-based password

Available options:
  --short                  Use the EFF list of short words
  --special                Use the EFF list of special words
  --count INT              The number of passwords (default: 1)
  --delimiter ARG          The delimiter to use (default: " ")
  --acrostic ARG           The first letters of the words form this
                           word. (default: "")
  -h,--help                Show this help text
```

![XKCD 936](https://imgs.xkcd.com/comics/password_strength.png)

You can find a longer explanation of this technique in 
[this EFF article](https://www.eff.org/deeplinks/2016/07/new-wordlists-random-passphrases),
along with [the word list](https://www.eff.org/files/2016/07/18/eff_large_wordlist.txt)
used by this program.
