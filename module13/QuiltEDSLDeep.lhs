Module 13: Deeply embedded EDSLs
================================

* Write your team names here: Ted Bjurlin, Thomas Manslaughter, Sarah Wright

We have already learned about the distinction between a standalone
domain-specific language and an embedded one.  There are also two types of
embedded domain-specific languages: *shallow* EDSLs work directly
with the semantics of the language; *deep* EDSLs construct abstract
syntax trees, which can be later interpreted or compiled.  A deep
EDSL is a bit more work to build (though still less work than a standalone
language implementation), but can have many benefits, some of which
we will explore today:

- It's possible to implement other phases (like error checking, optimization,
    or desugaring) before interpreting.
- One is not limited to a *single* semantics; it's possible to
  interpret the same AST in multiple ways (to compile or interpret it,
  analyze it, *etc.*).

> {-# LANGUAGE FlexibleInstances    #-}
> {-# LANGUAGE GADTs                #-}
> {-# LANGUAGE TypeSynonymInstances #-}
> {-# LANGUAGE ViewPatterns         #-}
>
> module QuiltEDSLDeep where
>
> import           Codec.Picture
> import           Data.Colour
> import           Data.Colour.Names
> import           Data.Colour.SRGB
> import           Data.Word

The Quilt AST
-------------

Here's the AST type we will use to represent Quilt programs (at least
for now; you are welcome to add features if you wish).

> type Color  = Colour Double
> type Number = Double
>
> data Coord where
>   X :: Coord
>   Y :: Coord
>
> data Quilt a where
>   QSolid :: a -> Quilt a
>   QCoord :: Coord -> Quilt Number
>   QGrey  :: Quilt Number -> Quilt Color
>   QIf    :: Quilt Bool -> Quilt a -> Quilt a -> Quilt a
>   QQuilt :: Quilt a -> Quilt a -> Quilt a -> Quilt a -> Quilt a
>   QMap   :: (a -> b) -> Quilt a -> Quilt b
>   QZip   :: (a -> b -> c) -> Quilt a -> Quilt b -> Quilt c
>   QRot   :: Quilt a -> Number -> Quilt a

`GADT` stands for *Generalized Algebraic Data Type*; the
*Generalized* refers to the way that constructors of the `Quilt` type
above do not always construct a `Quilt a`, but sometimes construct
something more specific such as a `Quilt Number` or `Quilt Color` (but
they still have to construct some sort of `Quilt`).  Note how the
types of the `Quilt` constructors essentially encode the type system
for the language.  For example, `QIf :: Quilt Bool -> Quilt a -> Quilt
a -> Quilt a` specifies that the first argument to `QIf` must be a
quilt of booleans; the branches must have the same type as each other;
and the result of the whole expression will be the same as the types
of the branches.

Note that `QRot` represents rotations, and only takes a `Number`
instead of a `Quilt Number`.  So we can only rotate by a single number
instead of by a number that varies over the plane.  The reason for
this restriction will become clear later; though note that we could
also add a constructor for generalized rotation taking a `Quilt
Number` if we wanted.

* Complete the function definitions below.  In the shallow
  version of the Quilt EDSL, these functions actually did
  some semantic work.  In this version, all they will do is construct
  ASTs.  *Note: if you are tempted to add a new constructor for `(<.)`,
  think again*.

> quilt :: Quilt a -> Quilt a -> Quilt a -> Quilt a -> Quilt a
> quilt = QQuilt
>
> solid :: a -> Quilt a
> solid = QSolid
>
> x :: Quilt Number
> x = QCoord X
>
> y :: Quilt Number
> y = QCoord Y
>
> mkGrey :: Quilt Number -> Quilt Color
> mkGrey = QGrey
>
> ifQ :: Quilt Bool -> Quilt a -> Quilt a -> Quilt a
> ifQ = QIf
>
> (<.) :: Ord a => Quilt a -> Quilt a -> Quilt Bool
> (<.) = QZip (<)
>
> mapQuilt :: (a -> b) -> Quilt a -> Quilt b
> mapQuilt = QMap
>
> zipQuilt :: (a -> b -> c) -> Quilt a -> Quilt b -> Quilt c
> zipQuilt = QZip
>
> rot :: Quilt a -> Number -> Quilt a
> rot = QRot

Below I have duplicated the code from last week for doing arithmetic
on colors and quilts, as well as the instances of `Fractional` and
`Floating` that you wrote for quilts.  Note that all this code is
*entirely unchanged* from last week: it all works in terms of
`solid`, `mapQuilt`, and `zipQuilt`, which you implemented above.

> mapColor :: (Double -> Double) -> Color -> Color
> mapColor f (toSRGB -> RGB r g b) = sRGB (f r) (f g) (f b)
>
> zipColor :: (Double -> Double -> Double) -> Color -> Color -> Color
> zipColor (&) (toSRGB -> RGB r1 g1 b1) (toSRGB -> RGB r2 g2 b2) = sRGB (r1 & r2) (g1 & g2) (b1 & b2)
>
> instance Num Color where
>   (+) = zipColor (+)
>   (-) = zipColor (-)
>   (*) = zipColor (*)
>   abs = mapColor abs
>   signum = mapColor signum
>   fromInteger i = sRGB i' i' i'
>     where i' = fromInteger i
>
> instance Num a => Num (Quilt a) where
>   (+)           = zipQuilt (+)
>   (-)           = zipQuilt (-)
>   (*)           = zipQuilt (*)
>   abs           = mapQuilt abs
>   signum        = mapQuilt signum
>   fromInteger i = solid (fromInteger i)
>
> instance Fractional a => Fractional (Quilt a) where
>   fromRational = solid . fromRational
>   (/) = zipQuilt (/)
>
> instance Floating a => Floating (Quilt a) where
>   pi    = solid pi
>   exp   = mapQuilt exp
>   log   = mapQuilt log
>   sin   = mapQuilt sin
>   cos   = mapQuilt cos
>   asin  = mapQuilt asin
>   acos  = mapQuilt acos
>   atan  = mapQuilt atan
>   sinh  = mapQuilt sinh
>   cosh  = mapQuilt cosh
>   asinh = mapQuilt asinh
>   acosh = mapQuilt acosh
>   atanh = mapQuilt atanh

![](../images/green.png)

Interpreting typed ASTs
-----------------------

* **ROTATE ROLES** and write the name of the new driver here: Thomas Manslaughter

Now that we can build up Quilt ASTs, we need a way to interpret them,
of course.

* Complete the implementation of `interp` below.

> type QuiltFun a = Double -> Double -> a
>
> interp :: Quilt a -> QuiltFun a
> interp (QSolid a) = \ x y -> a 
> interp (QCoord X) = \ x _ -> x
> interp (QCoord Y) = \ _ y -> y
> interp (QGrey q) = interp q

We got ere mate

Beautiful, isn't it?  Notice that `interp` doesn't have to return any
sort of `Either Error`---and not only that, it doesn't even have to
make any assumptions about the values produced by recursive calls to
`interp`!  When `interp` is called on a `Quilt Bool`, we get *actual*
`Bool` values out, or `Number` values from a `Quilt Number`, and so
on---the type of `interp` guarantees this.  In the past we had to use
some `Value` type and carefully keep track of our assumptions about how we
interpreted different types (*e.g.* interpreting `False` as `0` and
`True` as `1`), and there was always the possibility of a bug in our
type checker throwing things off.  But now there is no possibility of
error in type checking---since it's being done by the Haskell type
system---and no assumptions to keep track of in our interpreter.

* Write a smiley face here once you have read and understood the above
  paragraph:

* The below definition of `renderQuilt` is taken from our shallow
  EDSL.  Uncomment it (by removing the spaces from before it) and fix
  it to work with this new version of the EDSL.

    > renderQuilt :: Int -> FilePath -> Quilt Color -> IO ()
    > renderQuilt qSize fn q = do
    >   let q' r c = q (2*(fromIntegral r / fromIntegral qSize) - 1)
    >                  (-(2*(fromIntegral c / fromIntegral qSize) - 1))
    >       img    = ImageRGB8 $ generateImage (\r c -> toPixel $ q' r c) qSize qSize
    >   savePngImage fn img

* Try some examples to make sure everything works properly.

* Copy the `quilterate` function from the previous module.  How does
  it have to be modified to work with this new version of the EDSL?

* Try an example or two using `quilterate` to make sure it works.

![](../images/green.png)

What to do with an AST, part I: optimization
--------------------------------------------

* **ROTATE ROLES** and write the name of the new driver here:

So far, we have simply reimplemented the same functionality we already
had with our shallow EDSL.  Now that we build an AST, however, it
opens up many possibilities.

Consider the following function, which repeatedly rotates a quilt by 5
degrees:

> nudge :: Int -> Quilt a -> Quilt a
> nudge 0 q = q
> nudge n q = nudge (n-1) q `rot` 5

(This function is not very realistic, but it's not hard to imagine more
realistic things with similar characteristics.)

> nudgy :: Quilt Color
> nudgy = nudge 100 (ifQ (x <. y) (solid red) (solid blue))

* Render `nudgy`.  How long does it take?

* Why do you think it takes so long?

The point is that doing repeated rotations is silly: we should just do
one rotation instead. For example, rotating by 10 degrees and then
rotating by 20 degrees is the same as doing one rotation by 30 degrees.
The idea will be to write a function that transforms `Quilt` ASTs,
collapsing multiple consecutive rotations into one.

* Write a function `opt :: Quilt a -> Quilt a`.  It should collapse
  all consecutive `QRot` constructors into one `QRot` constructor,
  adding the rotations. (*Hint*: if you see two consecutive `QRot`
  constructors, collapse them, and then re-call `opt` on the result.)
  Be sure to also optimize `QRot` constructors buried somewhere inside
  an AST, not just ones at the very top level.

* Now modify `renderQuilt` to call `opt` before calling `interp`.

* Re-render `nudgy`.  Is it faster now?

* We can't make a real `Show` instance for `Quilt` since it contains
  functions.  However, `ghci` can do a decent job of showing values in
  a hacky way.  Try typing `:force nudgy` at the `ghci` prompt.

* Now define `nudgy' = opt nudgy` and then type `:force nudgy'`.

![](../images/green.png)

What to do with an AST, part II: analysis
-----------------------------------------

* **ROTATE ROLES** and write the name of the new driver here:

Once we have a Quilt AST we don't just have to interpret it.  We can
do anything else we like with it.

* As a simple example, complete the definition of `quiltSize` below.

> quiltSize :: Quilt a -> Int
> quiltSize = undefined

`quiltSize` should compute the number of `Quilt` constructors in an
AST.  For example, the size of `QSolid` is 1; the size of `QIf t q1
q2` is one more than the sum of the sizes of `t`, `q1`, and `q2`; and
so on.

* Try evaluating `quiltSize nudgy` and `quiltSize (opt nudgy)`.

Feedback
--------

* How long would you estimate that you spent working on this module?

* Were any parts particularly confusing or difficult?

* Were any parts particularly fun or interesting?

* Record here any other questions, comments, or suggestions for
  improvement.

Support code
------------

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
