namespace Hex

def hexDigitToNat (c : Char) : Nat :=
  if '0' ≤ c ∧ c ≤ '9' then c.toNat - '0'.toNat
  else if 'a' ≤ c ∧ c ≤ 'f' then c.toNat - 'a'.toNat + 10
  else c.toNat - 'A'.toNat + 10

def hexPairToChar (c₁ c₂ : Char) : Char :=
  let d₁ := hexDigitToNat c₁
  let d₂ := hexDigitToNat c₂
  Char.ofNat (16 * d₁ + d₂)

partial def hexPairSequenceToString (s : String) : String :=
  let rec loop (s : String) (acc : String) : String :=
    if s.length < 2 then acc
    else loop (s.drop 2) (acc.push (hexPairToChar (s.get! 0) (s.get! (String.Pos.mk 1))))
  loop s ""

def decodeHex (s: String) : Option String := hexPairSequenceToString (s.drop 2)

def charToHex (c : Char) : String :=
  let d := c.toNat
  let d₁ := d / 16
  let d₂ := d % 16
  let c₁ := Char.ofNat (if d₁ < 10 then '0'.toNat + d₁ else 'a'.toNat + d₁ - 10)
  let c₂ := Char.ofNat (if d₂ < 10 then '0'.toNat + d₂ else 'a'.toNat + d₂ - 10)
  String.singleton c₁ ++ String.singleton c₂

partial def strToHex (s : String) : String :=
  let rec loop (s : String) (acc : String) : String :=
    if s.length == 0 then acc
    else loop (s.drop 1) (acc ++ charToHex (s.get! 0))
  "0x" ++ loop s ""

end Hex
