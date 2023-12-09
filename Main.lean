import Cartesi
import Requests

open Cartesi

def handleAdvance (adv : Advance String) : App Status := do
  IO.println "Handling Advance"
  App.notice s!"Hello from Lean! :) {adv.payload}"
  pure Status.accept

def handleInspect (ins : Inspect String) : App Status := do
  IO.println "Handling Inspect"
  App.report s!"Hello from Lean! {ins.payload}"
  pure Status.accept

def app : Cartesi.App Unit :=
  Cartesi.cartesiHandler handleInspect handleAdvance

def main : IO Unit := do
  IO.println "Starting DApp"
  Cartesi.App.start app "0.0.0.0" "5004"
