package idris_runtime

import . "strconv"
import   "math/big"

//-------------------------------------------------------------------------------------------------
// Constant-like data used frequently
//-------------------------------------------------------------------------------------------------
var ConstBigZero = big.NewInt(0)
var ConstBigOne  = big.NewInt(1)

//-------------------------------------------------------------------------------------------------
// Data conversion functions
//-------------------------------------------------------------------------------------------------

func BoolToInt(isTrue bool) int {
  if isTrue {
    return 1
  } else {
    return 0
  }
}

func StringToInt(s string) int64 {
  value, _ := ParseInt(s, 0, 64)
  return value
}

func StringToFloat(s string) float64 {
  value, _ := ParseFloat(s, 64)
  return value
}

func BigIntFromString(n string) *big.Int {
  intResult, _ := big.NewInt(0).SetString(n, 0)
  return intResult
}
