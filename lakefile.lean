import Lake
open Lake DSL

package «Cartesi» where
  -- add package configuration options here

lean_lib «Cartesi» where
  -- add library configuration options here

@[default_target]
lean_exe «cartesi» where
  root := `Main
  -- Enables the use of the Lean interpreter by the executable (e.g.,
  -- `runFrontend`) at the expense of increased binary size on Linux.
  -- Remove this line if you do not need such functionality.
  supportInterpreter := true

require soda from git "https://github.com/algebraic-sofia/soda.git"
require requests from git "https://github.com/algebraic-sofia/requests.git"
require JSON from git "https://github.com/algebraic-sofia/JSON.git"
