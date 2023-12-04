namespace day3.part1

def _root_.Nat.sub' (m n : Nat) : Option Nat :=
if n ≤ m then .some (m - n) else .none

def optionCoord : Type := Option Nat × Option Nat

def adjacentCoords (pos : Nat × Nat) (length : Nat) :
  List (Nat × Nat) :=
  let left : optionCoord := (.some pos.1, pos.2.sub' 1)
  let right : optionCoord := (.some pos.1, .some <| pos.2 + length)
  let topLeft : optionCoord := (pos.1.sub' 1, pos.2.sub' 1)
  let topRight : optionCoord := (pos.1.sub' 1, .some <| pos.2 + length)
  let botLeft : optionCoord := (.some pos.1.succ, pos.2.sub' 1)
  let botRight : optionCoord := (.some pos.1.succ, some <| pos.2 + length)
  let top : List optionCoord :=
    (fun n ↦ (pos.1.sub' 1, .some (n + pos.2))) <$> List.range length
  let down : List optionCoord :=
    (fun n ↦ (.some pos.1.succ, .some (n + pos.2))) <$> List.range length
  left :: right :: topLeft :: topRight :: botLeft :: botRight ::
    (top.append down) |>.filterMap fun
      | ⟨.none, _⟩ => .none
      | ⟨_, .none⟩ => .none
      | ⟨.some a, .some b⟩ => .some ⟨a, b⟩

def data := IO.FS.Handle.mk (.mk "Advent/day3/data.txt") (.read)

def s : String := ".854...........................................................................362...........271...732........838.........24................"
def findNumericListOfChar (s : List Char) : List (List Char) :=
  s.groupBy (fun a b ↦ a.isDigit ∧ b.isDigit)

def accumulate (s : List Nat) (init : Nat) : List Nat :=
match s with
| [] => [init]
| x::l => init :: accumulate l (x + init)

def calcPosition (s : List (List Char)) : List (Nat × List Char) :=
  let lengths := s.map List.length
  accumulate lengths 0 |>.zip s

def listOfPositionWithLength (s : List (Nat × List Char)) :
  List (Nat × Nat × Nat) :=
s.filter (fun p ↦ p.2.head!.isDigit) |>.map
  fun ⟨length, content⟩ ↦ ⟨length, content.length, String.toNat! ⟨content⟩⟩

def processLine : String → List (Nat × Nat × Nat) :=
  listOfPositionWithLength ∘ calcPosition ∘ findNumericListOfChar ∘ String.data

def processLines' (s : List String) : List <| List ((Nat × Nat) × Nat × Nat) :=
  s.enum.map fun ⟨linum, content⟩ ↦
    processLine content |>.map
      fun ⟨pos, length, num⟩ ↦ ((linum, pos), length, num)

def processLines (s : List String) : List ((Nat × Nat) × Nat × Nat) :=
  processLines' s |>.foldl List.append []

def checkNeighbouring (s : List String) (position : (Nat × Nat) × Nat × Nat) :
    Bool × Nat :=
  (List.or <| let neighbours := adjacentCoords position.1 position.2.1
   let grid : List (List Char) := s.map String.data
   neighbours.filterMap fun xy ↦ grid.get? xy.1 >>= fun l ↦ l.get? xy.2 >>=
    fun c ↦ .some <| ((¬ c.isDigit) ∧ c ≠ '.'), position.2.2)

def main : IO Unit := do
  let d ← data
  let string ← IO.FS.Handle.readToEnd d
  let strings : List String := string.splitOn "\n"
  let processedData := processLines strings
  let result := processedData.map fun x ↦ checkNeighbouring strings x

  IO.println <| ((result.filter fun p => p.1).map fun p => p.2).foldl (. + .) 0

end day3.part1
