namespace day4.part2

def countElems (source : List Nat) (target : List Nat) :=
match target with
| [] => 0
| t::ts => if t ∈ source then 1 + countElems source ts else countElems source ts

def isWhiteSpace (s : List Char) : Bool := s.all (· = ' ')

def processLine (s : String) : List Nat × List Nat :=
  let s' := s.splitOn ":" |>.get! 1
  let s'' := s'.splitOn "| " |>.map String.trim
    |>.map (·.splitOn " ")
    |>.map (fun x ↦ x.filter fun y ↦ ¬ y.isEmpty)
    |>.map (fun x ↦ x.map String.toNat!)
  (s''.get! 0, s''.get! 1)

def processGames (s : List String) : List <| Nat × (List Nat × List Nat) :=
s.enum.map fun ⟨linum, content⟩ ↦ (linum, processLine content)

def initialCount (s : List String) : List <| Nat × Nat :=
  (processGames s).map fun ⟨linum, _⟩ ↦ (linum, 1)

def winNumLookUp (s : List String) : List <| Nat × Nat :=
  (processGames s).map fun ⟨linum, content⟩ ↦
    (linum, countElems content.1 content.2)

/--
slow

but if we use `https://github.com/nomeata/lean4-memo-nat`

def calcNumOfGames (wins : List <| Nat × Nat) : Nat → Nat :=
```lean
NatMemo.memo fun n f ↦ 1 + show Nat from
  List.range n
    |>.filter (fun j ↦ (wins.get! j).2 + j + 1 > n)
    |>.map (fun i ↦ f i sorry)
    |>.foldl (. + .) 0
```
very fast
-/
unsafe def calcNumOfGames (wins : List <| Nat × Nat) : Nat → Nat
  | .zero => 1
  | .succ n =>
      1 + show Nat from List.range n.succ
          |>.filter (fun j ↦ (wins.get! j).2 + j > n)
          |>.map (calcNumOfGames wins)
          |>.foldl (. + .) 0

-- def exampleData :=
--   "Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53\n" ++
--   "Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19\n" ++
--   "Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1\n" ++
--   "Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83\n" ++
--   "Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36\n" ++
--   "Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11"

-- #eval (List.range 6 |>.map <| calcNumOfGames (winNumLookUp <| exampleData.splitOn "\n")).foldl (.+.) 0

unsafe def main : IO Unit := do
  let file ← IO.FS.Handle.mk "Advent/day4/data.txt" .read
  let fileContent ← IO.FS.Handle.readToEnd file
  let lines := fileContent.splitOn "\n"
  let winNums := winNumLookUp lines
  IO.println <| List.range 216 |>.map (calcNumOfGames winNums) |>.foldl (. + .) 0

-- #eval main
-- 5571760
end day4.part2
