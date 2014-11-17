package idris_runtime

import . "unicode/utf8"

//-------------------------------------------------------------------------------------------------
// Functions for UTF-8 string operations
//-------------------------------------------------------------------------------------------------

func Utf8Head(s string) rune {
  if len(s) > 0 {
    chr, _ := DecodeRuneInString(s)
    return chr
  } else {
    return 0
  }
}

func Utf8Tail(s string) string {
  _, offset := DecodeRuneInString(s)
  return s[offset:]
}

func Utf8AtIndex(s string, index int) rune {
  if len(s) > 0 {
    if index == 0 {
      chr, _ := DecodeRuneInString(s)
      return chr
    } else {
      i := 0
      for _, chr := range s {
        if i == index {
          return chr
        }
        i++
      }
    }
  }
  return 0
}

func Utf8Reverse(s string) string {
  offset := len(s)
  if offset == 0 {
    return ""
  }
  buf := make([]rune, offset)
  for _, chr := range s {
    offset--
    buf[offset] = chr
  }
  return string(buf[offset:])
}

