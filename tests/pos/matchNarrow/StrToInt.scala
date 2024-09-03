
val FOO: 1 = 1 // Try with singleton type or not!

def strToInt(x: Int): String = x match
  case 0   => "0"
  case FOO => "1" // Should not error!
  case y   => y.toString