
val x = 1
val y: x.type = x

def foo(z: Int): String = z match
  case _: x.type => "1"
  case a         => a.toString
