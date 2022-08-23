library(pracma)

inters<-function(x1,y1,r1,x2,y2,r2){
  d = hypot(x2 - x1,y2 - y1)


  if (d < r1+r2){

    a = r1**2
    b = r2**2

    x = (a - b + d * d) / (2 * d)
    z = x * x
    y = sqrt(a - z)

    if (d <= abs(r2 - r1))
    {
      pi * min(a, b)
    }
    else {
      a * asin(y / r1) + b * asin(y / r2) - y * (x + sqrt(z + b - a))

    }
  }
  else
  { 0}
}

