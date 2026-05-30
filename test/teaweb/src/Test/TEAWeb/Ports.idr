||| EXT for `TEAWeb.Ports`.
|||
||| Ports is a thin naming layer over `Cmd.SendPort` / `Sub.Port`; the
||| FFI delivery is chez-untestable (it lands in the T6 demo). These
||| tests pin that the wrappers build the expected constructors so the
||| public API stays stable.
module Test.TEAWeb.Ports

import Hedgehog
import TEAWeb.Cmd
import TEAWeb.Sub
import TEAWeb.Ports

%default total

data Msg = MFromPort String

Eq Msg where
  MFromPort a == MFromPort b = a == b

Show Msg where
  show (MFromPort s) = "MFromPort " ++ show s

--------------------------------------------------------------------------------
-- Outbound: send builds a SendPort Cmd carrying name + payload.
--------------------------------------------------------------------------------

isSendPortFor : String -> String -> Cmd Msg -> Bool
isSendPortFor n p (SendPort n' p') = n == n' && p == p'
isSendPortFor _ _ _                = False

export
ext_send_builds_sendport : Property
ext_send_builds_sendport = withTests 1 . property $ do
  let cmd : Cmd Msg
      cmd = send "log" "hello"
  isSendPortFor "log" "hello" cmd === True

export
ext_send_flattens_to_one : Property
ext_send_flattens_to_one = withTests 1 . property $ do
  length (flatten (the (Cmd Msg) (send "log" "x"))) === 1

--------------------------------------------------------------------------------
-- Inbound: subscribe builds a Port Sub with the given cbId + name.
--------------------------------------------------------------------------------

export
ext_subscribe_cbId : Property
ext_subscribe_cbId = withTests 1 . property $ do
  subCallbackId (the (Sub Msg) (subscribe "p1" "ws" MFromPort)) === Just "p1"

export
ext_subscribeNamed_cbId : Property
ext_subscribeNamed_cbId = withTests 1 . property $ do
  subCallbackId (the (Sub Msg) (subscribeNamed "ws" MFromPort)) === Just "port:ws"

export
ext_subscribe_flattens_to_one : Property
ext_subscribe_flattens_to_one = withTests 1 . property $ do
  length (flatten (the (Sub Msg) (subscribe "p" "ws" MFromPort))) === 1

--------------------------------------------------------------------------------
-- Round-trip naming: a name sent and a name subscribed agree on the
-- wire string, so an app's send/receive pair line up.
--------------------------------------------------------------------------------

portNameOf : Sub Msg -> Maybe String
portNameOf (Port _ name _) = Just name
portNameOf _               = Nothing

export
ext_send_subscribe_share_name : Property
ext_send_subscribe_share_name = withTests 1 . property $ do
  let nm = "metrics"
  isSendPortFor nm "{}" (send nm "{}") === True
  portNameOf (subscribeNamed nm MFromPort) === Just nm

export
group : Group
group = MkGroup "TEAWeb.Ports"
  [ ("ext_send_builds_sendport",       ext_send_builds_sendport)
  , ("ext_send_flattens_to_one",       ext_send_flattens_to_one)
  , ("ext_subscribe_cbId",             ext_subscribe_cbId)
  , ("ext_subscribeNamed_cbId",        ext_subscribeNamed_cbId)
  , ("ext_subscribe_flattens_to_one",  ext_subscribe_flattens_to_one)
  , ("ext_send_subscribe_share_name",  ext_send_subscribe_share_name)
  ]
