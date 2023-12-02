
def data : IO <| List String :=
  IO.FS.Handle.mk (.mk "Advent/day2/data.txt") (.read) >>= IO.FS.Handle.readToEnd >>=
    fun s ↦ return s.splitOn "\n"

structure Result where
  red : Nat
  green : Nat
  blue : Nat
  deriving Repr, Inhabited

def Result.possible? (s : Result) : Bool :=
  s.red ≤ 12 && s.green ≤ 13 && s.blue ≤ 14

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

def processString (s : String) : Nat × List Bool :=
  let temp := s.splitOn ":"
    match temp with
    | [⟨'G'::'a'::'m'::'e'::' '::xs⟩, snd] =>
      ( (String.mk xs).toNat!,
        snd.splitOn "; " |>
          .map Result.fromString |>
          .map Result.possible? )
    | _ => panic "bad string"

def main : IO Unit := do
  let s ← data
  IO.println <|
    s.map processString |>.filter (fun x ↦ x.2.foldl (·&&·) true) |>.map (·.1)
      |>.foldl (·+·) 0

#eval main
