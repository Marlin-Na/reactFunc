

# reactFunc


[![Travis-CI Build Status](https://travis-ci.org/Marlin-Na/reactFunc.svg?branch=master)](https://travis-ci.org/Marlin-Na/reactFunc)
[![Join the chat at https://gitter.im/reactFunc/Lobby](https://badges.gitter.im/reactFunc/Lobby.svg)](https://gitter.im/reactFunc/Lobby?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)


## Introduction

`reactFunc` is a tiny package that provide tools to build cacheable function
using reactive expressions.

For detail, see https://marlin-na.github.io/reactFunc/articles/reactFunc.html .

### Install

You can install it with:

```r
devtools::install_github("marlin-na/reactFunc")
```

### Usage



```r
library(reactFunc)
myfunc <- reactFunc(
    ARGV = alist(x = 6, y = ),
    a = x + 1,
    b = y - 3,
    ans = a() * b()
)
myfunc(x = 6, y = 9)
```

