data Shape = ||| A triangle, defined by base and height
             Triangle Double Double
           | ||| A rectangle, defined by height and width
             Rectangle Double Double
           | ||| A circle, defined by radius
             Circle Double

area : Shape -> Double
area (Triangle base height) = base * height / 2
area (Rectangle width height) = width * height
area (Circle radius) = pi * radius * radius

data Picture = Primitive Shape
             | Combine Picture Picture
             | Rotate Double Picture
             | Translate Double Double Picture

rectangle : Picture
rectangle = Primitive $ Rectangle 20 10

circle : Picture
circle = Primitive $ Circle 5

triangle : Picture
triangle = Primitive $ Triangle 10 10


testPicture : Picture
testPicture = Combine (Translate 5 5 rectangle)
                      (Combine (Translate 35 5 circle)
                               (Translate 15 25 triangle))

pictureArea : Picture -> Double
pictureArea (Primitive shape) = area shape
pictureArea (Combine pic1 pic2) = pictureArea pic1 + pictureArea pic2
pictureArea (Rotate angle pic) = pictureArea pic
pictureArea (Translate x y pic) = pictureArea pic
