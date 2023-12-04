namespace day3.part2

def _root_.Nat.sub' (m n : Nat) : Option Nat :=
if n ≤ m then .some (m - n) else .none

def data : IO <| List String :=
  IO.FS.Handle.mk (.mk "advent/Advent/day3/data.txt") (.read) >>= fun d ↦
    (String.splitOn · "\n") <$> IO.FS.Handle.readToEnd d

def starPositions (grid : List <| List Char) : List (Nat × Nat) :=
  let grid' : List ((Nat × Nat) × Char) :=
    (grid.enum.map fun x ↦
      x.2.enum.map fun c ↦ ((x.1, c.1), c.2)).foldl List.append []
  grid'.filter (·.2 = '*') |>.map (·.1)

/--
.......
...*...
.......
-/
def createGrid (coord : Nat × Nat) : List <| List (Nat × Nat) :=
  let top : List (Nat × Nat) :=
    [ (coord.1.sub' 1, coord.2.sub' 3),
      (coord.1.sub' 1, coord.2.sub' 2),
      (coord.1.sub' 1, coord.2.sub' 1),
      (coord.1.sub' 1, .some coord.2),
      (coord.1.sub' 1, .some coord.2.succ),
      (coord.1.sub' 1, .some coord.2.succ.succ),
      (coord.1.sub' 1, .some coord.2.succ.succ.succ) ]
      |>.filterMap fun
      | ⟨.none, _⟩ => .none
      | ⟨_, .none⟩ => .none
      | ⟨.some x, .some y⟩ => .some (x, y)
  let bot : List (Nat × Nat) :=
    [ ((.some coord.1.succ : Option Nat), coord.2.sub' 3),
      ((.some coord.1.succ : Option Nat), coord.2.sub' 2),
      ((.some coord.1.succ : Option Nat), coord.2.sub' 1),
      ((.some coord.1.succ : Option Nat), .some coord.2),
      ((.some coord.1.succ : Option Nat), .some coord.2.succ),
      ((.some coord.1.succ : Option Nat), .some coord.2.succ.succ),
      ((.some coord.1.succ : Option Nat), .some coord.2.succ.succ.succ) ]
      |>.filterMap fun
      | ⟨.none, _⟩ => .none
      | ⟨_, .none⟩ => .none
      | ⟨.some x, .some y⟩ => .some (x, y)
  let left : List (Nat × Nat) :=
    [ ((.some coord.1 : Option Nat), coord.2.sub' 3),
      ((.some coord.1 : Option Nat), coord.2.sub' 2),
      ((.some coord.1 : Option Nat), coord.2.sub' 1) ]
      |>.filterMap fun
      | ⟨.none, _⟩ => .none
      | ⟨_, .none⟩ => .none
      | ⟨.some x, .some y⟩ => .some (x, y)
  let right : List (Nat × Nat) :=
    [ ((.some coord.1 : Option Nat), (.some coord.2.succ : Option Nat)),
      ((.some coord.1 : Option Nat), .some coord.2.succ.succ),
      ((.some coord.1 : Option Nat), .some coord.2.succ.succ.succ) ]
      |>.filterMap fun
      | ⟨.none, _⟩ => .none
      | ⟨_, .none⟩ => .none
      | ⟨.some x, .some y⟩ => .some (x, y)
  [top, left |>.append [coord] |>.append right, bot]

def stringOfGrid (coords : List <| List (Nat × Nat)) (grid : List (List Char)) :
    List (List Char) :=
  coords.map fun xys ↦ xys.filterMap fun xy ↦ grid.get? xy.1 >>= fun cs ↦
    cs.get? xy.2

def findStarPos (subgrid : List (List Char)) : Nat × Nat :=
  let middle := subgrid.get! 1
  (1, middle.length / 2)


def accumulate (s : List Nat) (init : Nat) : List Nat :=
match s with
| [] => [init]
| x::l => init :: accumulate l (x + init)

def calcPosition (s : List (List Char)) : List (Nat × List Char) :=
  let lengths := s.map List.length
  accumulate lengths 0 |>.zip s
/--
first coord position, second value, third length
-/
def findNumericListOfChar (s : List Char) : List (Nat × Nat × Nat) :=
  let s' := s.groupBy  (fun x y ↦ x.isDigit ∧ y.isDigit)
  calcPosition s' |>.filterMap fun x ↦ (x.1, ·, x.2.length) <$>
    (String.mk x.2 |>.toNat?)

def findNumericListOfListOfChar (s : List (List Char)) :
    List (List ((Nat × Nat) × Nat × Nat)) :=
  s.enum.map fun x ↦
    findNumericListOfChar x.2 |>.map fun ⟨pos, value⟩ ↦ ⟨⟨x.1, pos⟩, value⟩

def isAdj (x : Nat × Nat) (y : Nat × Nat) : Bool :=
(x.1.max y.1) - (x.1.min y.1) ≤ 1 &&
(x.2.max y.2) - (x.2.min y.2) ≤ 1

def isValid (starPos : Nat × Nat) (info : (Nat × Nat) × Nat × Nat) : Bool :=
  let poses : List (Nat × Nat) := List.range info.2.2 |>.map (info.1.1, · + info.1.2)
  poses.map (isAdj starPos) |>.or

def findAdjecentNumber' (s : List (List Char))  (starPos : Nat × Nat) :
    List Nat :=
  let s' := findNumericListOfListOfChar s
  ((s'.map fun xs ↦ xs.filter fun x ↦ isValid starPos x).map
    fun x ↦ x.map fun p ↦ p.2.1).foldl List.append []

def main : IO Unit := do
  let s ← data
  let grid : List (List Char) := s.map String.data
  let starPoses : List (Nat × Nat) := starPositions grid
  let gridsWithStar : List (List (List (Nat × Nat))) :=
    starPoses.map fun pos ↦ (createGrid pos)
  let stringsOfGrid := gridsWithStar.map (stringOfGrid · grid)
  let numbers := stringsOfGrid.map (fun g ↦ findAdjecentNumber' g (findStarPos g))
  IO.println <| numbers
    |>.filter (·.length = 2)
    |>.map (fun x ↦ x.foldl (·*·) 1) |>.foldl (·+·) 0


#eval main

end day3.part2
