namespace day1_2

def processStringFst (s : String) : Option Nat :=
match s with
| ⟨[]⟩ => .none
| ⟨'0':: _⟩ => .some 0
| ⟨'1':: _⟩ => .some 1
| ⟨'2':: _⟩ => .some 2
| ⟨'3':: _⟩ => .some 3
| ⟨'4':: _⟩ => .some 4
| ⟨'5':: _⟩ => .some 5
| ⟨'6':: _⟩ => .some 6
| ⟨'7':: _⟩ => .some 7
| ⟨'8':: _⟩ => .some 8
| ⟨'9':: _⟩ => .some 9
| ⟨'z' :: 'e' :: 'r' :: 'o' :: _⟩ => .some 0
| ⟨'o' :: 'n' :: 'e' :: _⟩ => .some 1
| ⟨'t' :: 'w' :: 'o' :: _⟩ => .some 2
| ⟨'t' :: 'h' :: 'r' :: 'e' :: 'e' :: _⟩ => .some 3
| ⟨'f' :: 'o' :: 'u' :: 'r'  :: _⟩ => .some 4
| ⟨'f' :: 'i' :: 'v' :: 'e'  :: _⟩ => .some 5
| ⟨'s' :: 'i' :: 'x' :: _⟩ => .some 6
| ⟨'s' :: 'e' :: 'v' :: 'e' :: 'n' :: _⟩ => .some 7
| ⟨'e' :: 'i' :: 'g' :: 'h' :: 't' :: _⟩ => .some 8
| ⟨'n' :: 'i' :: 'n' :: 'e' :: _⟩ => .some 9
| ⟨_::rest⟩ => processStringFst <| .mk rest

def _root_.String.reverse (s : String) : String := .mk s.data.reverse

def processStringLastHelper (s : String) : Option Nat :=
match s with
| ⟨[]⟩ => .none
| ⟨'0':: _⟩ => .some 0
| ⟨'1':: _⟩ => .some 1
| ⟨'2':: _⟩ => .some 2
| ⟨'3':: _⟩ => .some 3
| ⟨'4':: _⟩ => .some 4
| ⟨'5':: _⟩ => .some 5
| ⟨'6':: _⟩ => .some 6
| ⟨'7':: _⟩ => .some 7
| ⟨'8':: _⟩ => .some 8
| ⟨'9':: _⟩ => .some 9
| ⟨'o' :: 'r' :: 'e' :: 'z' :: _⟩ => .some 0
| ⟨'e' :: 'n' :: 'o' :: _⟩ => .some 1
| ⟨'o' :: 'w' :: 't' :: _⟩ => .some 2
| ⟨'e' :: 'e' :: 'r' :: 'h' :: 't' :: _⟩ => .some 3
| ⟨'r' :: 'u' :: 'o' :: 'f'  :: _⟩ => .some 4
| ⟨'e' :: 'v' :: 'i' :: 'f'  :: _⟩ => .some 5
| ⟨'x' :: 'i' :: 's' :: _⟩ => .some 6
| ⟨'n' :: 'e' :: 'v' :: 'e' :: 's' :: _⟩ => .some 7
| ⟨'t' :: 'h' :: 'g' :: 'i' :: 'e' :: _⟩ => .some 8
| ⟨'e' :: 'n' :: 'i' :: 'n' :: _⟩ => .some 9
| ⟨_::rest⟩ => processStringLastHelper <| .mk rest

def processStringLast := processStringLastHelper ∘ .reverse

def processString (s : String) : Option Nat :=
let x := processStringFst s
let y := processStringLast s
x >>= fun x ↦ match y with |.none => .none | .some y => (10 * x + y)

def data : IO <| List String :=
  IO.FS.Handle.mk (.mk "Advent/day1/data.txt") (.read) >>= IO.FS.Handle.readToEnd >>=
    fun s ↦ return s.splitOn "\n"

instance : Add (Option Nat) :=
⟨fun a b ↦ match a, b with
| .some a, .some b => .some (a + b)
| none, .some b => .some b
| .some a, none => .some a
| .none, none => .none⟩

def main : IO Unit := do
  let s ← data
  IO.println <| (s.map processString).foldr (· + ·) .none

end day1_2
