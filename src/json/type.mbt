// Copyright 2024 peter-jerry-ye
// SPDX-License-Identifier: MIT

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
