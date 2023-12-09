import Requests
import JSON

import Cartesi.Hex

namespace Cartesi

structure AppState where
  host : String
  port : String

abbrev App := ReaderT AppState IO

def App.start (app: App Unit) (host: String) (port: String) : IO Unit := do
  let appState := AppState.mk host port
  app.run appState

class Serialize (t: Type) where
  serialize : t -> String

structure Hex where
  hex : String
  deriving Repr

-- | Class to serialize types to String
instance : Serialize String where
  serialize s := Hex.strToHex s

-- | Class to deserialize types from String
class Deserialize (t: Type) where
  deserialize : String -> Option t

instance : Deserialize String where
  deserialize s := Hex.decodeHex s

-- | Represents the end result of a request.
inductive Status
  | accept
  | reject
  deriving Repr

structure Address where
  address : String
  deriving Repr

-- | A request to advance the state of the application passing through the blockchain and going
-- to the Cartesi Rollup.
structure Advance (t: Type) where
  sender      : Address
  epochIndex  : Nat
  inputIndex  : Nat
  blockNumber : Nat
  timestamp   : Nat
  payload     : t
  deriving Repr

instance [Deserialize t] : FromJSON (Advance t) where
  fromJSON json := do
    let data : JSON ← json.find? "data"
    let metadata: JSON ← data.find? "metadata"
    let sender ← metadata.find? "msg_sender" <&> Address.mk
    let epochIndex ← metadata.find? "epoch_index"
    let inputIndex ← metadata.find? "input_index"
    let blockNumber ← metadata.find? "block_number"
    let timestamp ← metadata.find? "timestamp"
    let payload ← data.find? "payload" >>= Deserialize.deserialize
    return { sender, epochIndex, inputIndex, blockNumber, timestamp, payload }

-- | A request to read the state of the application.
structure Inspect (t: Type) where
  payload : t
  deriving Repr

instance [Deserialize t] : FromJSON (Inspect t) where
  fromJSON json := do
    let data : JSON ← json.find? "data"
    let payload ← data.find? "payload" >>= Deserialize.deserialize
    return { payload }

inductive Request (t: Type)
  | advance : Advance t -> Request t
  | inspect : Inspect t -> Request t
  deriving Repr

-- | A voucher is a combination of an address and a payload that is used to
-- interact with the blockchain.
structure Voucher (t: Type) where
  address : Address
  payload : t
  deriving Repr

instance [Serialize t] : ToJSON (Voucher t) where
  toJSON i := JSON.obj
    [ ("address", JSON.str i.address.address)
    , ("payload", JSON.str (Serialize.serialize i.payload))
    ]

-- | A notice is an arbitrary payload used for informational purposes.
structure Notice (t: Type) where
  payload : t
  deriving Repr

instance [Serialize t] : ToJSON (Notice t) where
  toJSON i := JSON.obj [("payload", JSON.str (Serialize.serialize i.payload))]

-- | Report is an application log that is not associated with a proof
structure Report (t: Type) where
  payload : t
  deriving Repr

instance [Serialize t] : ToJSON (Report t) where
  toJSON i := JSON.obj [("payload", JSON.str (Serialize.serialize i.payload))]

def Request.classify [Deserialize t] (json: JSON) : Option (Request t) :=
  match json.find? "request_type" with
  | some (JSON.str "advance_state") => Request.advance <$> (FromJSON.fromJSON json)
  | some (JSON.str "inspect_state") => Request.inspect <$> (FromJSON.fromJSON json)
  | _                               => none

partial def finish [Deserialize t] (status: Status) : IO (Request t) := do
  let conn ← Requests.Connection.create "127.0.0.1" "5004"
  let req := JSON.obj [("status", JSON.str "accept")]
  let req ← conn.post "/finish" (ToString.toString req)
  match req with
  | Except.error e => throw (IO.userError e)
  | Except.ok res  =>
    match JSON.parse res.body with
    | none     => finish status
    | some res =>
      match Request.classify res with
      | some res => pure res
      | none     => finish status

def cartesiHandler [Deserialize t] (handleInspect : (Inspect t) -> App Status) (handleAdvance: (Advance t) -> App Status) : App Unit := do
  let mut status := Status.accept
  repeat do
    let req ← finish status
    let res ← match req with
    | Request.inspect req => handleInspect req
    | Request.advance req => handleAdvance req
    status := res

def App.notice [Serialize t] (notice: t) : App Unit := do
  let state ← ReaderT.read
  let conn ← Requests.Connection.create state.host state.port
  let _ ← conn.post "/notice" (ToString.toString (ToJSON.toJSON (Notice.mk notice)))

def App.report [Serialize t] (report: t) : App Unit := do
  let state ← ReaderT.read
  let conn ← Requests.Connection.create state.host state.port
  let _ ← conn.post "/report" (ToString.toString (ToJSON.toJSON (Report.mk report)))

def App.voucher (voucher: Voucher String) : App Unit := do
  let state ← ReaderT.read
  let conn ← Requests.Connection.create state.host state.port
  let _ ← conn.post "/voucher" (ToString.toString (ToJSON.toJSON voucher))

end Cartesi
