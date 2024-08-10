// Copyright 2024 peter-jerry-ye
// SPDX-License-Identifier: MIT

priv enum State {
  ExpectValue
  ExpectString(mut ~state : ParseString, ~buffer : Buffer)
  ExpectObject(mut ~state : ParseObject)
  ExpectArray(mut ~state : ParseArray)
  ExpectTrue(mut ~expectation : ArrayView[Char])
  ExpectFalse(mut ~expectation : ArrayView[Char])
  ExpectNull(mut ~expectation : ArrayView[Char])
  ExpectNumber(mut ~state : ParseNumber, ~buffer : Buffer)
} derive(Show)

priv enum ParseString {
  ExpectCharPoint
  ExpectEscapedChar
  ExpectHex(mut ~len : Int, mut ~value : Int)
} derive(Show)

priv enum ParseNumber {
  ExpectDigit
  ExpectDigitOrFractionOrExponentOrEnd
  ExpectFractionOrExponentOrEnd
  ExpectFractionDigit
  ExpectFractionDigitOrExponentOrEnd
  ExpectExponentSignOrDigit
  ExpectExponentDigit
  ExpectExponentDigitOrEnd
} derive(Show)

priv enum ParseObject {
  ExpectEndOrFieldName
  ExpectEndOrNext
  ExpectFieldName
  ExpectFieldValue
} derive(Show)

priv enum ParseArray {
  ExpectEndOrNext
  ExpectEndOrValue
} derive(Show)

priv type! Err Unit

fn is_white_space(char : Char) -> Bool {
  match char {
    '\x20' | '\x0A' | '\x0D' | '\x09' => true
    _ => false
  }
}

/// Parses a JSON string and generates an iterator of JSON tokens.
/// 
/// # Parameters
/// 
/// - `string` : An iterator of characters representing the JSON string to be parsed.
/// 
/// # Returns
/// 
/// - An iterator of JSON tokens (`Iter[T]`).
/// 
/// # Examples
/// 
/// ```moonbit
/// test "parse" {
///   #|{
///   #|  "a":"hello",
///   #|  "x": [{"b": true}, null, 1.5, false, -0.27e+10]
///   #|}
///   .iter()
///   |> parse
///   |> collect
///   |> inspect!(
///     content=
///       #|[ObjectStart, String("a"), String("hello"), String("x"), ArrayStart, ObjectStart, String("b"), True, ObjectEnd, Null]
///     ,
///   )
/// }
/// ```
pub fn parse(string : Iter[Char]) -> Iter[T] {
  Iter::new(
    fn(f) {
      let true_str = ['t', 'r', 'u', 'e']
      let false_str = ['f', 'a', 'l', 's', 'e']
      let null_str = ['n', 'u', 'l', 'l']
      let stack = @deque.from_array([ExpectValue])
      for char in string {
        try {
          loop stack.back() {
            None => raise Err(())
            Some(ExpectValue) =>
              match char {
                't' => {
                  stack.pop_back_exn()
                  stack.push_back(ExpectTrue(expectation=true_str[1:]))
                }
                'f' => {
                  stack.pop_back_exn()
                  stack.push_back(ExpectFalse(expectation=false_str[1:]))
                }
                'n' => {
                  stack.pop_back_exn()
                  stack.push_back(ExpectNull(expectation=null_str[1:]))
                }
                '[' => {
                  stack.pop_back_exn()
                  match f(ArrayStart) {
                    IterEnd => raise Err(())
                    _ => ()
                  }
                  stack.push_back(ExpectArray(state=ExpectEndOrValue))
                }
                '{' => {
                  stack.pop_back_exn()
                  match f(ObjectStart) {
                    IterEnd => raise Err(())
                    _ => ()
                  }
                  stack.push_back(ExpectObject(state=ExpectEndOrFieldName))
                }
                '"' => {
                  stack.pop_back_exn()
                  stack.push_back(
                    ExpectString(state=ExpectCharPoint, buffer=Buffer::new()),
                  )
                }
                '-' => {
                  stack.pop_back_exn()
                  let buffer = Buffer::new()
                  buffer.write_char('-')
                  stack.push_back(ExpectNumber(state=ExpectDigit, ~buffer))
                }
                '0' => {
                  stack.pop_back_exn()
                  let buffer = Buffer::new()
                  buffer.write_char('0')
                  stack.push_back(
                    ExpectNumber(state=ExpectFractionOrExponentOrEnd, ~buffer),
                  )
                }
                _ =>
                  if char <= '9' && char >= '1' {
                    stack.pop_back_exn()
                    let buffer = Buffer::new()
                    buffer.write_char(char)
                    stack.push_back(
                      ExpectNumber(
                        state=ExpectDigitOrFractionOrExponentOrEnd,
                        ~buffer,
                      ),
                    )
                  } else if is_white_space(char).not() {
                    raise Err(())
                  }
              }
            Some(ExpectString(~state, ~buffer) as expect) =>
              match state {
                ExpectCharPoint =>
                  if char == '\\' {
                    expect.state = ExpectEscapedChar
                  } else if char == '"' {
                    match f(String(buffer.to_string())) {
                      IterEnd => raise Err(())
                      IterContinue => stack.pop_back_exn()
                    }
                  } else if char < '\x32' {
                    raise Err(())
                  } else {
                    buffer.write_char(char)
                  }
                ExpectEscapedChar =>
                  match char {
                    '"' => {
                      buffer.write_char('"')
                      expect.state = ExpectCharPoint
                    }
                    '\\' => {
                      buffer.write_char('\\')
                      expect.state = ExpectCharPoint
                    }
                    '/' => {
                      buffer.write_char('/')
                      expect.state = ExpectCharPoint
                    }
                    'b' => {
                      buffer.write_char('\b')
                      expect.state = ExpectCharPoint
                    }
                    'f' => {
                      buffer.write_char('\x0c')
                      expect.state = ExpectCharPoint
                    }
                    'n' => {
                      buffer.write_char('\n')
                      expect.state = ExpectCharPoint
                    }
                    'r' => {
                      buffer.write_char('\r')
                      expect.state = ExpectCharPoint
                    }
                    't' => {
                      buffer.write_char('\t')
                      expect.state = ExpectCharPoint
                    }
                    'u' => expect.state = ExpectHex(len=4, value=0)
                    _ => raise Err(())
                  }
                ExpectHex(..) as exp => {
                  if char >= '0' && char <= '9' {
                    exp.value = (exp.value << 4) | (char.to_int() - 0x30)
                  } else if char >= 'a' && char <= 'f' {
                    exp.value = (exp.value << 4) | (char.to_int() - 0x61 + 0xa)
                  } else if char >= 'A' && char <= 'F' {
                    exp.value = (exp.value << 4) | (char.to_int() - 0x41 + 0xa)
                  } else {
                    raise Err(())
                  }
                  exp.len -= 1
                  if exp.len == 0 {
                    expect.buffer.write_char(Char::from_int(exp.value))
                    expect.state = ExpectCharPoint
                  }
                }
              }
            Some(ExpectArray(~state) as expect) =>
              match state {
                ExpectEndOrValue =>
                  if char == ']' {
                    match f(ArrayEnd) {
                      IterEnd => raise Err(())
                      _ => stack.pop_back_exn()
                    }
                  } else if is_white_space(char).not() {
                    expect.state = ExpectEndOrNext
                    stack.push_back(ExpectValue)
                    continue stack.back()
                  }
                ExpectEndOrNext =>
                  if char == ']' {
                    match f(ArrayEnd) {
                      IterEnd => raise Err(())
                      _ => stack.pop_back_exn()
                    }
                  } else if char == ',' {
                    stack.push_back(ExpectValue)
                  } else if is_white_space(char).not() {
                    raise Err(())
                  }
              }
            Some(ExpectObject(~state) as expect) =>
              match state {
                ExpectEndOrFieldName =>
                  if char == '}' {
                    match f(ObjectEnd) {
                      IterEnd => raise Err(())
                      _ => stack.pop_back_exn()
                    }
                  } else if char == '"' {
                    expect.state = ExpectFieldValue
                    stack.push_back(
                      ExpectString(state=ExpectCharPoint, buffer=Buffer::new()),
                    )
                  } else if is_white_space(char).not() {
                    raise Err(())
                  }
                ExpectEndOrNext =>
                  if char == '}' {
                    match f(ObjectEnd) {
                      IterEnd => raise Err(())
                      _ => stack.pop_back_exn()
                    }
                  } else if char == ',' {
                    expect.state = ExpectFieldName
                  } else if is_white_space(char).not() {
                    raise Err(())
                  }
                ExpectFieldName =>
                  if char == '"' {
                    expect.state = ExpectFieldValue
                    stack.push_back(
                      ExpectString(state=ExpectCharPoint, buffer=Buffer::new()),
                    )
                  } else if is_white_space(char).not() {
                    raise Err(())
                  }
                ExpectFieldValue =>
                  if char == ':' {
                    expect.state = ExpectEndOrNext
                    stack.push_back(ExpectValue)
                  } else if is_white_space(char).not() {
                    raise Err(())
                  }
              }
            Some(ExpectTrue(~expectation) as expect) =>
              if expectation[0] == char {
                expect.expectation = expectation[1:]
                if expect.expectation.length() == 0 {
                  match f(True) {
                    IterEnd => raise Err(())
                    _ => stack.pop_back_exn()
                  }
                }
              } else {
                raise Err(())
              }
            Some(ExpectFalse(~expectation) as expect) =>
              if expectation[0] == char {
                expect.expectation = expectation[1:]
                if expect.expectation.length() == 0 {
                  match f(False) {
                    IterEnd => raise Err(())
                    _ => stack.pop_back_exn()
                  }
                }
              } else {
                raise Err(())
              }
            Some(ExpectNull(~expectation) as expect) =>
              if expectation[0] == char {
                expect.expectation = expectation[1:]
                if expect.expectation.length() == 0 {
                  match f(Null) {
                    IterEnd => raise Err(())
                    _ => stack.pop_back_exn()
                  }
                }
              } else {
                raise Err(())
              }
            Some(ExpectNumber(~state, ~buffer) as expect) =>
              match state {
                ExpectDigit =>
                  if char == '0' {
                    buffer.write_char('0')
                    expect.state = ExpectFractionOrExponentOrEnd
                  } else if char <= '9' && char >= '1' {
                    buffer.write_char(char)
                    expect.state = ExpectDigitOrFractionOrExponentOrEnd
                  } else {
                    raise Err(())
                  }
                ExpectDigitOrFractionOrExponentOrEnd =>
                  if char <= '9' && char >= '0' {
                    buffer.write_char(char)
                  } else if char == '.' {
                    buffer.write_char(char)
                    expect.state = ExpectFractionDigit
                  } else if char == 'e' || char == 'E' {
                    expect.state = ExpectExponentSignOrDigit
                  } else {
                    let number = try {
                      @strconv.parse_double!(buffer.to_string())
                    } catch {
                      _ => raise Err(())
                    }
                    match f(Number(number)) {
                      IterEnd => raise Err(())
                      _ => {
                        stack.pop_back_exn()
                        continue stack.back()
                      }
                    }
                  }
                ExpectFractionOrExponentOrEnd =>
                  if char == '.' {
                    buffer.write_char(char)
                    expect.state = ExpectFractionDigit
                  } else if char == 'e' || char == 'E' {
                    expect.state = ExpectExponentSignOrDigit
                  } else {
                    let number = try {
                      @strconv.parse_double!(buffer.to_string())
                    } catch {
                      _ => raise Err(())
                    }
                    match f(Number(number)) {
                      IterEnd => raise Err(())
                      _ => {
                        stack.pop_back_exn()
                        continue stack.back()
                      }
                    }
                  }
                ExpectFractionDigit =>
                  if char <= '9' && char >= '0' {
                    buffer.write_char(char)
                    expect.state = ExpectFractionDigitOrExponentOrEnd
                  } else {
                    raise Err(())
                  }
                ExpectFractionDigitOrExponentOrEnd =>
                  if char <= '9' && char >= '0' {
                    buffer.write_char(char)
                  } else if char == 'e' || char == 'E' {
                    buffer.write_char(char)
                    expect.state = ExpectExponentSignOrDigit
                  } else {
                    let number = try {
                      @strconv.parse_double!(buffer.to_string())
                    } catch {
                      _ => raise Err(())
                    }
                    match f(Number(number)) {
                      IterEnd => raise Err(())
                      _ => {
                        stack.pop_back_exn()
                        continue stack.back()
                      }
                    }
                  }
                ExpectExponentSignOrDigit =>
                  if char == '-' || char == '+' {
                    buffer.write_char(char)
                    expect.state = ExpectExponentDigit
                  } else if char <= '9' && char >= '0' {
                    buffer.write_char(char)
                    expect.state = ExpectExponentDigitOrEnd
                  } else {
                    raise Err(())
                  }
                ExpectExponentDigit =>
                  if char >= '0' && char <= '9' {
                    buffer.write_char(char)
                    expect.state = ExpectExponentDigitOrEnd
                  } else {
                    raise Err(())
                  }
                ExpectExponentDigitOrEnd =>
                  if char >= '0' && char <= '9' {
                    buffer.write_char(char)
                    expect.state = ExpectExponentDigitOrEnd
                  } else {
                    let number = try {
                      @strconv.parse_double!(buffer.to_string())
                    } catch {
                      _ => raise Err(())
                    }
                    match f(Number(number)) {
                      IterEnd => raise Err(())
                      _ => {
                        stack.pop_back_exn()
                        continue stack.back()
                      }
                    }
                  }
              }
          }
        } catch {
          _ => break IterEnd
        }
      } else {
        IterContinue
      }
    },
  )
}

test "parse" {
  #|{
  #|  "a":"hello",
  #|  "x": [{"b": true}, null, 1.5, false, -0.27e+10]
  #|}
  .iter()
  |> parse
  |> collect
  |> inspect!(
    content=
      #|[ObjectStart, String("a"), String("hello"), String("x"), ArrayStart, ObjectStart, String("b"), True, ObjectEnd, Null, Number(1.5), False, Number(-2.7e9), ArrayEnd, ObjectEnd]
    ,
  )
}
