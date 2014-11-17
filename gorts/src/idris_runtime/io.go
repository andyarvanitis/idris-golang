package idris_runtime

import . "os"
import . "bufio"

//-------------------------------------------------------------------------------------------------
// Various IO functions
//-------------------------------------------------------------------------------------------------

func FileOpen(name string, mode string) *File {
  flags := 0
  for _, char := range mode { // TODO: these need some work
    switch char {
      case 'r': flags |= O_RDONLY
      case 'w': flags |= O_RDWR|O_TRUNC|O_CREATE
      case 'a': flags |= O_APPEND|O_CREATE
      case '+': flags |= O_RDWR
    }
    if flags & (O_RDWR|O_APPEND) != 0 {
      flags &^= O_RDONLY
    }
  }
  file, _ := OpenFile(name, flags, 0644)
  return file
}

func FileReadLine(file *File) string {
  // Save off current seek position
  offset, error := file.Seek(0, SEEK_CUR)
  if error == nil {
    reader := NewReader(file)
    line, error := reader.ReadString('\n')
    if error == nil {
      // Set seek position, since it's no longer correct
      file.Seek(offset + int64(len(line)), SEEK_SET)
      return line
    }
  }
  return ""
}

func FileEOF(file *File) int {
  info, error := file.Stat()
  if error == nil {
    size := info.Size()
    offset, error := file.Seek(0, SEEK_CUR)
    if error == nil {
      if offset == size {
        file.Seek(offset + 1, SEEK_SET)
      } else if offset > size {
        return 1
      }
    }
  }
  return 0
}
