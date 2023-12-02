namespace day1.part2

def processStringFst (s : String) : Nat :=
match s with
| ⟨[]⟩ => panic s!"wrong {s}"
| ⟨'0':: _⟩ => 0
| ⟨'1':: _⟩ => 1
| ⟨'2':: _⟩ => 2
| ⟨'3':: _⟩ => 3
| ⟨'4':: _⟩ => 4
| ⟨'5':: _⟩ => 5
| ⟨'6':: _⟩ => 6
| ⟨'7':: _⟩ => 7
| ⟨'8':: _⟩ => 8
| ⟨'9':: _⟩ => 9
| ⟨'z' :: 'e' :: 'r' :: 'o' :: _⟩ => 0
| ⟨'o' :: 'n' :: 'e' :: _⟩ => 1
| ⟨'t' :: 'w' :: 'o' :: _⟩ => 2
| ⟨'t' :: 'h' :: 'r' :: 'e' :: 'e' :: _⟩ => 3
| ⟨'f' :: 'o' :: 'u' :: 'r'  :: _⟩ => 4
| ⟨'f' :: 'i' :: 'v' :: 'e'  :: _⟩ => 5
| ⟨'s' :: 'i' :: 'x' :: _⟩ => 6
| ⟨'s' :: 'e' :: 'v' :: 'e' :: 'n' :: _⟩ => 7
| ⟨'e' :: 'i' :: 'g' :: 'h' :: 't' :: _⟩ => 8
| ⟨'n' :: 'i' :: 'n' :: 'e' :: _⟩ => 9
| ⟨_::rest⟩ => processStringFst <| .mk rest

def _root_.String.reverse (s : String) : String := .mk s.data.reverse

def processStringLastHelper (s : String) : Nat :=
match s with
| ⟨[]⟩ => panic s!"wrong {s}"
| ⟨'0':: _⟩ => 0
| ⟨'1':: _⟩ => 1
| ⟨'2':: _⟩ => 2
| ⟨'3':: _⟩ => 3
| ⟨'4':: _⟩ => 4
| ⟨'5':: _⟩ => 5
| ⟨'6':: _⟩ => 6
| ⟨'7':: _⟩ => 7
| ⟨'8':: _⟩ => 8
| ⟨'9':: _⟩ => 9
| ⟨'o' :: 'r' :: 'e' :: 'z' :: _⟩ => 0
| ⟨'e' :: 'n' :: 'o' :: _⟩ => 1
| ⟨'o' :: 'w' :: 't' :: _⟩ => 2
| ⟨'e' :: 'e' :: 'r' :: 'h' :: 't' :: _⟩ => 3
| ⟨'r' :: 'u' :: 'o' :: 'f'  :: _⟩ => 4
| ⟨'e' :: 'v' :: 'i' :: 'f'  :: _⟩ => 5
| ⟨'x' :: 'i' :: 's' :: _⟩ => 6
| ⟨'n' :: 'e' :: 'v' :: 'e' :: 's' :: _⟩ => 7
| ⟨'t' :: 'h' :: 'g' :: 'i' :: 'e' :: _⟩ => 8
| ⟨'e' :: 'n' :: 'i' :: 'n' :: _⟩ => 9
| ⟨_::rest⟩ => processStringLastHelper <| .mk rest

def processStringLast := processStringLastHelper ∘ .reverse

def processString (s : String) : Nat :=
let x := processStringFst s
let y := processStringLast s
(10 * x + y)

def data : IO <| List String :=
  IO.FS.Handle.mk (.mk "Advent/day1/data.txt") (.read) >>= IO.FS.Handle.readToEnd >>=
    fun s ↦ return s.splitOn "\n"

def main : IO Unit := do
  let s ← data
  IO.println <| (s.map processString).foldr (· + ·) 0

end day1.part2
