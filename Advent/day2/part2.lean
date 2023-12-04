namespace day2.part2

def data : IO <| List String :=
  IO.FS.Handle.mk (.mk "Advent/day2/data.txt") (.read) >>= IO.FS.Handle.readToEnd >>=
    fun s ↦ return s.trimRight.splitOn "\n"

structure Result where
  red : Nat
  green : Nat
  blue : Nat
  deriving Inhabited

instance : ToString Result where
  toString x := s!"⟨{x.red}, {x.green}, {x.blue}⟩"

instance : Add Result where
  add a b :=
    { red :=  a.red + b.red
      green := a.green + b.green
      blue := a.blue + b.blue }

def colourFromString (s : String) : Result :=
  let s' := s.splitOn " " |>.map String.trim
    match s' with
    | [m, "red"] => { red := m.toNat!, green := 0, blue := 0 }
    | [m, "green"] => { red := 0, green := m.toNat!, blue := 0 }
    | [m, "blue"] => { red := 0, green := 0, blue := m.toNat! }
    | _ => panic s!"wrong format of string, line 65, called with {s}"

def Result.fromString (s : String) : Result :=
  s.splitOn ","
    |>.map String.trimLeft
    |>.map colourFromString
    |>.foldl (·+·) ⟨0, 0, 0⟩

def Result.addUp (l : List Result) : Result :=
l.foldl (·+·) <| .mk 0 0 0

def processString (s : String) : List Result :=
  let temp := s.splitOn ":"
    match temp with
    | [_, snd] => snd.splitOn "; " |> .map Result.fromString
    | _ => panic "bad string"

def Result.max (l : List Result) : Result :=
  l.foldl
  (fun a b ↦
    { red := a.red.max b.red
      green := a.green.max b.green
      blue := a.blue.max b.blue }) ⟨0, 0, 0⟩

def Result.power (s : Result) : Nat := s.blue * s.red * s.green

def main : IO Unit := do
  let s ← data
  IO.println <|
    s.map processString |>.map Result.max |>.map Result.power |>.foldl (·+·) 0


end day2.part2
