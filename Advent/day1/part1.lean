namespace day1_1

def numbers : List Char := ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9']

def string2numberList' (s : String) (res : List Nat) : List Nat :=
match s with
| ⟨[]⟩ => res
| ⟨a :: as⟩ => string2numberList' (.mk as) <|
  if a ∈ numbers then (a.toNat - 48) :: res else res

def firstLastListFlip (s : List Nat) : Option Nat :=
match s with
| [] => .none
| [a] => .some <| 10 * a + a
| [a, b] => .some <| 10 * b + a
| n::_::ns => firstLastListFlip (n::ns)

def data := IO.FS.Handle.mk (.mk "Advent/day1/data.txt") (.read)

instance : Add (Option Nat) :=
⟨fun a b ↦ match a, b with
| .some a, .some b => .some (a + b)
| none, .some b => .some b
| .some a, none => .some a
| .none, none => .none⟩

def main : IO Unit := do
  let d ← data
  let strings ← IO.FS.Handle.readToEnd d

  IO.println <| ((strings.split (fun x ↦ x = '\n')).map (firstLastListFlip <| string2numberList' · [])).foldl
    (· + ·) (some 0)

end day1_1
