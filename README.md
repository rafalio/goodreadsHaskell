A Haskell API for getting data off of GoodReads. Downloads the xml with wreq and then parses it with HXT into a Haskell data type. From here on, you can do whatever you want with the data.

Example:

Get your API key and figure out your user ID. You can then create

```
config = GRRequestConfig {
  grDevKey = <yourDevKey>,
  grUserId = <yourUserId>
}
```

and query the first `n` pages of your 'read' book shelf as follows:

```
reviewsUpToPage config n
```
