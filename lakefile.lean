import Lake
open Lake DSL

package «Cartesi» where
  -- add package configuration options here

lean_lib «Cartesi» where
  -- add library configuration options here

require soda from git "https://github.com/algebraic-sofia/soda.git"
require requests from git "https://github.com/algebraic-sofia/requests.git"
require JSON from git "https://github.com/algebraic-sofia/JSON.git"
