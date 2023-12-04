namespace day4.part1

def countElems (source : List Nat) (target : List Nat) :=
match target with
| [] => 0
| t::ts => if t ∈ source then 1 + countElems source ts else countElems source ts

def isWhiteSpace (s : List Char) : Bool := s.all (· = ' ')

def processLine (s : String) :=
  let s' := s.splitOn ":" |>.get! 1
  let s'' := s'.splitOn "| " |>.map String.trim  |>.map (·.splitOn " ")
    |>.map (fun x ↦ x.filter fun y ↦ ¬ y.isEmpty)
    |>.map (fun x ↦ x.map String.toNat!)
  s''

def main : IO Unit := do
  let file ← IO.FS.Handle.mk "Advent/day4/data.txt" .read
  let fileContent ← IO.FS.Handle.readToEnd file
  let lines := fileContent.splitOn "\n"
  let games := lines.map processLine
  IO.println <| games.map (fun g ↦ countElems (g.get! 0) (g.get! 1)) |>.map
    (fun x ↦ if x > 0 then 2 ^ x.pred else 0) |>.foldl (·+·) 0

end day4.part1
