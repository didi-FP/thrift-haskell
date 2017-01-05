thrift-haskell
==============

[![Build Status](https://travis-ci.org/didi-FP/thrift-haskell.svg)](https://travis-ci.org/didi-FP/thrift-haskell)

Haskell thrift codegen and runtime library, working in progress.

thrift-haskell-compiler
-----------------------

Following're some rules for IDL compiler:

+ Haskell module name will be camelized and capitalized path/filename relative to `CWD`, for example:

```
...
├── IDL
│   ├── rockstable.thrift
│   └── rockstable_master.thrift
...
```

If you run `thrift-haskell-compiler -o data ./IDL`, you will get your compiled files in:

```
├── Data
│   └── Idl
│       ├── Rockstable.hs
│       └── RockstableMaster.hs
├── IDL
│   ├── rockstable.thrift
│   └── rockstable_master.thrift
```

+ Identifier will be camelized (and capitalized if needed).

+ Type definitions will be compiled into `type` declarations.

```
typedef list<Tweet> TweetList
```

will be compiled to:

```
type TweetList = [Tweet]
```

+ Senum and Slist are deprecated, thus unsupportted.

+ Enum is very straightforward, except that hex numeric literals are not supportted(which follows the thrift spec AFAICT). 

```
enum TweetType {
    TWEET,
    RETWEET = 2,  
    DM = 0xa,     // this field's behavior is undefined
    REPLY
}
```

will compiled to:

```haskell
data TweetType = Tweet
               | Retweet
               | Dm
               | Reply
               deriving (Eq, Ord, Show, Thrift.Data, Thrift.Typeable,
                         Thrift.Generic, Thrift.Hashable)
instance Enum TweetType where
        fromEnum Tweet = 0
        fromEnum Retweet = 2
        fromEnum Dm = 4
        fromEnum Reply = 5
        toEnum 0 = Tweet
        toEnum 2 = Retweet
        toEnum 4 = Dm
        toEnum 5 = Reply

instance Thrift.Thrift TweetType where
        typeCode = Thrift.TypeCode Thrift.tcInt32
        defaultValue = Tweet
        toTValue Tweet = Thrift.TInt32 0
        toTValue Retweet = Thrift.TInt32 2
        toTValue Dm = Thrift.TInt32 4
        toTValue Reply = Thrift.TInt32 5
        fromTValue (Thrift.TInt32 0) = Tweet
        fromTValue (Thrift.TInt32 2) = Retweet
        fromTValue (Thrift.TInt32 4) = Dm
        fromTValue (Thrift.TInt32 5) = Reply
        fromTValue _ = error "bad enum value"
```

+ Struct field will be prepend with struct name to eliminate name collision, Every field **must** have a unique, positive integer identifier:

```
struct Tweet {
    1: required i32 userId;
    2: required string userName;
    3: required string text;
    4: optional Location loc;
    5: optional TweetType tweetType = TweetType.TWEET;
    16: optional string language = "english";
}
```

will compiled to:

```haskell
data Tweet = Tweet{tweetUserId :: Thrift.Int32,
                   tweetUserName :: Thrift.Text, tweetText :: Thrift.Text,
                   tweetLoc :: Maybe Location, tweetTweetType :: Maybe TweetType,
                   tweetLanguage :: Maybe Thrift.Text}
           deriving (Eq, Show, Thrift.Data, Thrift.Typeable, Thrift.Generic,
                     Thrift.Hashable)

instance Thrift.Thrift Tweet where
        typeCode = Thrift.TypeCode Thrift.tcStruct
        defaultValue
          = let tweetUserId = Thrift.defaultValue
                tweetUserName = Thrift.defaultValue
                tweetText = Thrift.defaultValue
                tweetLoc = Nothing
                tweetTweetType = Just Tweet
                tweetLanguage = Just "english"
              in Tweet{..}
        toTValue Tweet{..}
          = Thrift.TStruct
              (Thrift.catMaybes
                 [Just (1, Thrift.toTValue tweetUserId),
                  Just (2, Thrift.toTValue tweetUserName),
                  Just (3, Thrift.toTValue tweetText),
                  case tweetLoc of
                      Just x -> Just (4, Thrift.toTValue x)
                      _ -> Nothing,
                  case tweetTweetType of
                      Just x -> Just (5, Thrift.toTValue x)
                      _ -> Nothing,
                  case tweetLanguage of
                      Just x -> Just (16, Thrift.toTValue x)
                      _ -> Nothing])
        fromTValue (Thrift.TStruct x)
          = let m = Thrift.fromList x
                tweetUserId = Thrift.lookupRequired (fromIntegral 1) m
                tweetUserName = Thrift.lookupRequired (fromIntegral 2) m
                tweetText = Thrift.lookupRequired (fromIntegral 3) m
                tweetLoc = Thrift.lookupOptional Nothing (fromIntegral 4) m
                tweetTweetType
                  = Thrift.lookupOptional (Just Tweet) (fromIntegral 5) m
                tweetLanguage
                  = Thrift.lookupOptional (Just "english") (fromIntegral 16) m
              in Tweet{..}
        fromTValue _ = error "bad struct value"
```

Please notice that there exists a so called *default* field type, it's treated like `required` field in data declarations, eg. without `Maybe`. Because haskell really don't have a `undefined/nil` notation, **every field must be present as long as it is not optional**. During deserialization, if peer doesn't send a *default* field, a `defaultValue` will be used, for basic types those are:

```
bool: false
i8, i16, i32, i64, double : 0
string: ""
binary: "" 
MapType: []
SetType: []
ListType: []
```

This may seemed arbitrary, but you will set these *default* field anyway so the choice of defaults are not important here, and you can supply a default value using thift's syntax. Following is a table describe every possible scenario:

| field requireness  | data type | serialize | deserialize | deserialize when user supply a default value x |
| ------------------ | --------- | --------- | ----------- | ---------------------------------------------- |
| required                  | a       | always  | must present, otherwise error | ignore user supply value x |
| optional                  | Maybe a | if Just | not present == Nothing        | not present == Just x      |
| default(function return)  | a       | always  | not present == Thrift.def     | not present == x           |

The term *present* here means a field can be find by its identifier **AND** the type conversion is successful, if a field with same identifier but different type exists, we will simply call error. Note we won't do any implicit conversion, that means a `Int32` field x is **NOT** present even there is an `IntX` field x.
 
License
-------

Copyright (c) 2016, Winterland

All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

    * Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.

    * Redistributions in binary form must reproduce the above
      copyright notice, this list of conditions and the following
      disclaimer in the documentation and/or other materials provided
      with the distribution.

    * Neither the name of winterland1989 nor the names of other
      contributors may be used to endorse or promote products derived
      from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
"AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

