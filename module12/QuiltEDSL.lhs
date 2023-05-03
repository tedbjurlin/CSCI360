Module 12: Embedded domain-specific languages
=============================================

* Write your team names here:

Before starting to work on this module, the driver should:

  - Make sure you have `QuiltEDSL.lhs`, `quilt.cabal`, and `stack.yaml` downloaded.
  - Place them in a folder together.
  - Open a command prompt, navigate to the folder containing the files, and type `stack build`.  Leave this running in the background while starting to look at the rest of the module, since building may take a while the first time while `stack` downloads and builds necessary prerequisites.

Embedded Domain-Specific Languages
==================================

A *domain-specific* language (DSL) is a language that is designed to solve
problems in a particular domain---as opposed to a general-purpose
language.

In a traditional implementation of a DSL, we just write a standalone
parser, type checker, interpreter, and so on.  This is what you are
doing for Project 3.

+ Pro: this gives us total control over the language!
+ Con: we have to do all the work ourselves, from scratch!

An *embedded* (domain-specific) language (EDSL) piggybacks on an
existing "host" language, *i.e.* the language is really "just" a
library in the host language.  Then EDSL programs *are* programs in
the host language which use things from the library.

+ Pro: it is a lot less work, and we get a lot of stuff (*e.g.*
  parsing, typechecking) for free.
+ Con: design of the EDSL is constrained; we have to "shoehorn" the
  EDSL into the host language.

It is not particularly important to distinguish between libraries on
the one hand and EDSLs on the other.  They are on a spectrum.  In one
sense, any library can be considered an EDSL.  This means it makes
sense to apply tools of language design to thinking about library API
design.

Quilt as a Haskell EDSL
=======================

As we will see, Haskell makes a particularly good host language for
EDSLs (because of things like generally clean syntax, user-defined
operators, first-class functions, and many abstraction mechanisms such
as type classes).

The first question we should ask when designing any DSL: what are the types?
In Quilt, we had Booleans, Numbers, and Colors, all of which could
vary over the plane.

In our EDSL version, we will still be able to have normal Haskell
(non-varying) booleans, numbers, and so on, so it will be useful to
distinguish between single values and values that vary over the plane.

> {-# LANGUAGE FlexibleInstances    #-}
> {-# LANGUAGE TypeSynonymInstances #-}
> {-# LANGUAGE ViewPatterns         #-}
>
> module QuiltEDSL where
>
> import           Codec.Picture
> import           Data.Colour
> import           Data.Colour.Names
> import           Data.Colour.SRGB
> import           Data.Complex
> import           Data.Word
>
> type Color  = Colour Double   -- from the 'colour' library
> type Number = Double

This is what is known as a *shallow* embedding: everything deals
directly with the desired semantics.  So we just define a quilt as a
function that takes two `Double`s.  Note we can make it *polymorphic*:
a `Quilt a`, for some type `a`, is an `a` that varies over the plane.
So ultimately we will render a `Quilt Color`, but we have seen how it
is useful to also have things like `Quilt Bool`.

> type Quilt a = Number -> Number -> a

Here is some stuff the Quilt language has, that we'd
like to develop in this EDSL setting:

- arithmetic (on numbers or colors)
- `if` statements
- $x$, $y$
- `quilt`
- comparison operators
- color names
- Booleans
- numbers
- color literals (lists)

Let's start with the `quilt` operator.  In a standalone language
implementation, `quilt` is just arbitrary syntax that we parse, type
check, and interpret.  In our EDSL, `quilt` will be a *Haskell
function*.  Its *implementation* will be the `quilt` code from the
interpreter; its *type* should encode the typing rules for `quilt`.

A first try might be:

```
quilt :: Color -> Color -> Color -> Color -> Quilt Color
```
but this would only allow us to make quilts with four solid color
blocks.  The `quilt` operator was definitely more powerful than this!

A second try:
```
quilt :: Quilt Color -> Quilt Color -> Quilt Color -> Quilt Color -> Quilt Color
```

This is better, but still doesn't capture everything: recall we could also
use `quilt` on *e.g.* quilts of numbers, or quilts of booleans.

Here's the real type we want, and the implementation (lifted straight
from the interpreter):

> quilt :: Quilt a -> Quilt a -> Quilt a -> Quilt a -> Quilt a
> quilt q1 q2 q3 q4 = \x y ->
>   case (x < 0, y > 0) of   -- which quadrant are we in?
>     (True , True)  -> q1 (2*x + 1) (2*y - 1)  -- call the appropriate quilt
>     (True , False) -> q3 (2*x + 1) (2*y + 1)  -- with transformed coordinates
>     (False, True)  -> q2 (2*x - 1) (2*y - 1)
>     (False, False) -> q4 (2*x - 1) (2*y + 1)

`quilt` works on four `Quilt` values of *any* type `a` (as long as
they are all the same type).

However, note we can't write `quilt red green blue purple` like we
could in the original Quilt language.  That was handled by subtyping,
but Haskell doesn't have subtyping.  So we have to introduce functions
to do subtyping explicitly.  This is one tradeoff of doing Quilt as an
EDSL.

* Fill in the definitions of `solid`, `x`, `y`, and `mkGrey` below.
  For `mkGrey`, you can use the `sRGB` function to create a `Color`
  from three R, G, B values. Hints:

    - Remember that `Quilt a` is just shorthand for `Number -> Number -> a`.
    - Remember that to make a function of type `Number -> Number -> a` you
      can use a lambda expression, by writing e.g. `\x y -> ...` where the
      `...` can refer to `x` and `y`, and has type `a`.
    - To see whether your definitions type check, type `stack ghci` at a command prompt, which will try to load this module into GHCi.

> -- 'solid c' creates a Quilt which simply has the constant value c
> -- everywhere in the plane.
> solid :: a -> Quilt a
> solid c = \x y -> c
>
> -- 'x' is the Quilt which, for each location in the plane, has value
> -- equal to the x-coordinate.
> x :: Quilt Number
> x = \x y -> x
>
> -- 'y' is the Quilt which, for each location in the plane, has value
> -- equal to the y-coordinate.
> y :: Quilt Number
> y = \x y -> y
>
> -- At each location, given a number n, turn it into the gray color
> -- with RGB values all equal to n.
> mkGrey :: Quilt Number -> Quilt Color
> mkGrey q = \x y -> sRGB (q x y) (q x y) (q x y)

We can't use normal Haskell `if`, so we make our own `ifQ` function
which works on Quilts.  (Actually, we *could* use Haskell's `if
... then ... else` syntax with the `RebindableSyntax` extension, which
would allow us to redefine how `if ... then ... else` works! But we
won't go into that now.)

* Define a function `ifQ` with an appropriate type, and fill in its implementation.

> ifQ :: Quilt Bool -> Quilt a -> Quilt a -> Quilt a
> ifQ b a1 a2 = \x y ->
>   if b x y
>   then a1 x y
>   else a2 x y

We also can't use the `<` operator since it returns a `Bool`, and we
want a `Quilt Bool`.  So we make our own called `<.`.

> infixl 4 <.
>
> (<.) :: Ord a => Quilt a -> Quilt a -> Quilt Bool
> q1 <. q2 = \x y -> q1 x y < q2 x y

At this point we can now try things like
```
renderQuilt 256 "quilt.png" (ifQ (x <. y) (solid red) (solid blue))
```
Run `stack ghci` and simply paste the above expression into the GHCi prompt. Then view the resulting `quilt.png` file using your browser or some other image viewing program.  You may wish to try other expressions as well.

We also note that we get lots of cool stuff from Haskell for free,
like `let`-expressions and variables, recursive functions, ... try
playing with the following definition.  What does it do? (Try it!)

> quilterate :: Int -> Quilt a -> Quilt a
> quilterate 0 q = q
> quilterate n q = let q' = quilterate (n-1) q in quilt q' q' q' q'

Overloading
===========

`quilt` and `+` were both overloaded to work on multiple types.
However, they worked rather differently.

- `quilt` works for *any* type, and it does the same thing no matter
  which type is used.  This is called *parametric polymorphism*, and
  corresponds to *e.g.* Java generics.  It also coresponds to
  Haskell's polymorphism.  This is why the type for `quilt` above was
  `Quilt a -> ...`, indicating that `quilt` will work for *any*
  type `a`.

- Addition, on the other hand, only works on *specific* types, and it
  works in a different way specific to each one.  For example, on
  numbers it does normal addition; on colors it adds channels
  componentwise.  This is called *ad-hoc polymorphism*, and
  corresponds to Java interfaces, subclassing, and method
  overloading.  It also corresponds to Haskell *type classes*.

In Haskell, arithmetic is governed by the `Num` type class.  We can
get `+` and friends to work on things like colors just by making a new
*instance* of the `Num` class for `Color`.

> -- Apply a (Double -> Double) function to each component of a Color.
> mapColor :: (Double -> Double) -> Color -> Color
> mapColor f (toSRGB -> RGB r g b) = sRGB (f r) (f g) (f b)
>
> -- Combine two colors by combining each color channel separately,
> -- using the given function.
> zipColor :: (Double -> Double -> Double) -> Color -> Color -> Color
> zipColor (&) (toSRGB -> RGB r1 g1 b1) (toSRGB -> RGB r2 g2 b2)
>   = sRGB (r1 & r2) (g1 & g2) (b1 & b2)
>
> -- This 'instance' defines how the required 'Num' operations (+, *,
> -- -, abs, signum, and fromInteger) will work on the Color type.
> instance Num Color where
>   (+) = zipColor (+)
>   (-) = zipColor (-)
>   (*) = zipColor (*)
>   abs = mapColor abs
>   signum = mapColor signum
>
>   fromInteger i = sRGB i' i' i'
>     where i' = fromInteger i

* Your turn: make an instance of `Num` for `Quilt a`.

> -- Apply a function to the values of a Quilt at every point in the
> -- plane.
> mapQuilt :: (a -> b) -> Quilt a -> Quilt b
> mapQuilt f q = \x y -> f (q x y)
>
> -- Combine two Quilts using the given function to combine their
> -- values at each point in the plane.
> zipQuilt :: (a -> b -> c) -> Quilt a -> Quilt b -> Quilt c
> zipQuilt f q1 q2 = \x y -> (q1 x y) `f` (q2 x y)
>
> instance Num a => Num (Quilt a) where
>   (+) = zipQuilt (+)
>   (-) = zipQuilt (-)
>   (*) = zipQuilt (*)
>   abs = mapQuilt abs
>   signum = mapQuilt signum
>
>   fromInteger i = \_ _ -> fromIntegral i

Now you should be able to try things like
```
(ifQ (x <. y) (solid red) (solid blue)) + (ifQ (-x <. y) (solid green) (solid purple))
```

It's worth thinking carefully about how the `-x` works: it turns into
a call to the `negate` function of the `Num` class, which by default
is implemented as `negate x = fromInteger 0 - x`.  So it uses our
implementation of `(-)` for `Quilt`.  Note also that the central
addition is adding two `Quilt Color`s.  To do this, it first calls
`(+)` for `Quilt`, which uses `zipQuilt` to apply `(+)` to every point
in the quilts.  This `(+)` in turn is the version of `(+)` for `Color`.

> instance Fractional a => Fractional (Quilt a) where
>   (/) = zipQuilt (/)
>   recip = mapQuilt recip
>
>   fromRational r = \_ _ -> fromRational r

> instance Floating a => Floating (Quilt a) where
>   pi = \x y -> pi
>   exp = mapQuilt exp
>   log = mapQuilt log
>   sqrt = mapQuilt sqrt
>   (**) = zipQuilt (**)
>   logBase = zipQuilt logBase
>   sin = mapQuilt sin
>   cos = mapQuilt cos
>   tan = mapQuilt Prelude.tan
>   asin = mapQuilt asin
>   acos = mapQuilt acos
>   atan = mapQuilt atan
>   sinh = mapQuilt sinh
>   cosh = mapQuilt cosh
>   tanh = mapQuilt tanh
>   asinh = mapQuilt asinh
>   acosh = mapQuilt acosh
>   atanh = mapQuilt atanh

* Make instances of the `Fractional` and `Floating` type classes for
  `Quilt a`.  These instances will be similar to the instance for
  `Num`.  You will probably have to look up documentation for
  `Fractional` and `Floating`.

* You should now be able to render examples like
    - `mkGrey $ sin (8*pi*x)`
    - `mkGrey ((x + 1)/2) * solid red + mkGrey ((y+1)/2) * solid blue`

Notice how we have to use `mkGrey` and `solid`.

* What is the type of `((x+1)/2)`?

* What is the type of `mkGrey ((x+1)/2)`?

* What is the type of `red`?

* What is the type of `solid red`?

Now let's add some geometric transformations.

* Implement functions `tx` (translate X), `ty` (translate Y), `scale`
  (scale by a given factor), and `rot` (rotate by an angle in
  degrees) to do translation, scaling, and rotation, respectively.
  Part of the exercise is to figure out appropriate types for these
  functions.

> tx :: Double -> Quilt a -> Quilt a
> tx i q = \x y -> q (x + i) y

> ty :: Double -> Quilt a -> Quilt a
> ty i q = \x y -> q x (y + i)

> scale :: Double -> Quilt a -> Quilt a
> scale i q1 = \x y -> q1 (x*i) (y*i)

> rot :: Quilt Number -> Quilt a -> Quilt a
> rot a q1 = \x y -> q1 (x * (cos a x y) - y * (sin a x y)) (y * (cos a x y) + x * (sin a x y))

* Now translate the following example into our EDSL and make sure it
   produces an appropriate image.  Note, the syntax shown below is
   Quilt syntax, not valid Haskell syntax, so you cannot just paste
   the below into GHCi.  You will have to make a few modifications to adapt it to the EDSL.

```
let swirl = (let grate = -cos (x*20*pi)/2 + 0.5 in grate @ (20*(sin(50*sqrt(x*x + y*y)))))
in  swirl * yellow + (y+1)/2 * blue
```
let swirl = (let grate = -cos (x*20*pi)/2 + 0.5 in grate @ (20*(sin(50*sqrt(x*x + y*y)))))
in  swirl * yellow + (y+1)/2 * blue



By the power of EDSLs
=====================

* Try rendering `mystery` below.  For optimal viewing, translate it `0.5`
  units in the positive `x` direction first.

> z :: Quilt (Complex Double)
> z = (:+)
>
> fromComplex :: (Complex Double -> a) -> Quilt a
> fromComplex f = mapQuilt f z
>
> mysteryCount :: Quilt Int
> mysteryCount = fromComplex $ \c ->
>   length . take 100 . takeWhile ((< 2) . magnitude) . iterate (f c) $ 0
>   where
>     f c w = w*w + c
>
> mystery :: Quilt Color
> mystery = mkGrey $ mapQuilt pickColor mysteryCount
>   where
>     pickColor n = logBase 2 (fromIntegral n) / 7

* Explain why this is a good example of one of the benefits of using
  an EDSL.  Why would it have been difficult to produce this image using
  the non-embedded Quilt language?

* Play around and make at least two other cool example images.  Try to
  create images that take advantage of the EDSL, *i.e.* images that
  would have been difficult or impossible to make with project 3.

Rendering
=========

You can ignore the code below, it just does the work of rendering a
`Quilt Color` to an image file.

> renderQuilt :: Int -> FilePath -> Quilt Color -> IO ()
> renderQuilt qSize fn q = do
>   let q' r c = q (2*(fromIntegral r / fromIntegral qSize) - 1)
>                  (-(2*(fromIntegral c / fromIntegral qSize) - 1))
>       img    = ImageRGB8 $ generateImage (\r c -> toPixel $ q' r c) qSize qSize
>   savePngImage fn img
>
> toPixel :: Color -> PixelRGB8
> toPixel (toSRGB -> RGB r g b) = PixelRGB8 (conv r) (conv g) (conv b)
>   where
>     conv :: Double -> Word8
>     conv v = fromIntegral . clamp $ floor (v * 256)
>     clamp :: Int -> Int
>     clamp v
>       | v > 255   = 255
>       | v < 0     = 0
>       | otherwise = v
