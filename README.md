

# reactFunc


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
    a = reactArg(x) + 1
    b = reactArg(y) + 1
    c = a() + b()
)
```

