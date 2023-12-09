
inductive Status where
  | accept
  | reject
  deriving Repr

instance : ToJSON Status where
  toJSON
    | Status.accept => JSON.str "accept"
    | Status.reject => JSON.str "reject"

structure Notice where
  payload : String

class ToJSON (α : Type u) where
  toJSON : α -> JSON

instance : ToJSON Notice where
  toJSON n := JSON.obj [("payload", JSON.str (strToHex n.payload))]

structure Report where
  payload : String

instance : ToJSON Report where
  toJSON n := JSON.obj [("payload", JSON.str (strToHex n.payload))]

class FromJSON (α : Type u) where
  fromJSON : JSON -> Option α

structure Inspect where
  payload : String
  deriving Repr

structure Advance where
  sender : String
  epochIndex : Nat
  inputIndex : Nat
  blockNumber : Nat
  timestamp : Nat
  payload : String
  deriving Repr

instance : FromJSON Advance where
  fromJSON json := do
    let data : JSON ← json.find? "data"
    let metadata: JSON ← data.find? "metadata"
    let sender ← metadata.find? "msg_sender"
    let epochIndex ← metadata.find? "epoch_index"
    let inputIndex ← metadata.find? "input_index"
    let blockNumber ← metadata.find? "block_number"
    let timestamp ← metadata.find? "timestamp"
    let payload ← data.find? "payload" >>= decodeHex
    return { sender, epochIndex, inputIndex, blockNumber, timestamp, payload }

instance : FromJSON Inspect where
  fromJSON json := do
    let data : JSON ← json.find? "data"
    let payload ← data.find? "payload" >>= decodeHex
    return { payload }

inductive Request
  | inspect : Inspect -> Request
  | advance : Advance -> Request
  deriving Repr

def classify (json: JSON) : Option Request :=
  match json.find? "request_type" with
  | some (JSON.str "advance_state") => Request.advance <$> (FromJSON.fromJSON json)
  | some (JSON.str "inspect_state") => Request.inspect <$> (FromJSON.fromJSON json)
  | _                               => none

partial def finish (status: Status) : IO Request := do
  let conn ← Requests.Connection.create "127.0.0.1" "5004"
  let req := JSON.obj [("status", JSON.str "accept")]
  let req ← conn.post "/finish" (ToString.toString req)
  match req with
  | Except.error e => throw (IO.userError e)
  | Except.ok res  =>
    match JSON.parse res.body with
    | none     => finish status
    | some res =>
      match classify res with
      | some res => pure res
      | none     => finish status

def cartesiHandler (handleInspect : Inspect -> IO Unit) (handleAdvance: Advance -> IO Unit) : IO Unit := do
  let mut a := Status.accept
  repeat do
    let req ← finish a
    match req with
    | Request.inspect req => handleInspect req
    | Request.advance req => handleAdvance req

end Cartesi
