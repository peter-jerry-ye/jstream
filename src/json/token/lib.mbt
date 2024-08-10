pub enum T {
  ObjectStart
  ObjectEnd
  ArrayStart
  ArrayEnd
  True
  False
  Null
  String(String)
  Number(Double)
} derive(Eq, Show)
