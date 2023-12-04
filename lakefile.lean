import Lake
open Lake DSL

require mathlib from git "https://github.com/leanprover-community/mathlib4"
-- require «memo-nat» from git "https://github.com/nomeata/lean4-memo-nat/"

package «advent» where
  -- add package configuration options here

lean_lib «Advent» where
  -- add library configuration options here

@[default_target]
lean_exe «advent» where
  root := `Main
  -- Enables the use of the Lean interpreter by the executable (e.g.,
  -- `runFrontend`) at the expense of increased binary size on Linux.
  -- Remove this line if you do not need such functionality.
  supportInterpreter := true
