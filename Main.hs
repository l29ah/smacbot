{-# LANGUAGE OverloadedStrings, Strict #-}

module Main where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad
import Data.List (uncons)
import qualified Data.Map as M
import Data.Maybe
import qualified Data.String.Class as S
import qualified Data.Text as T
import Llama
import Network.TLS
import Network.Xmpp
import Network.Xmpp.IM
import Network.Xmpp.Internal hiding (priority, status)
import Network.Xmpp.Extras.MUC
import System.Console.GetOpt
import System.Environment
import System.Log.Logger


passWordEnvVar = "SMACBOT_PASSWORD"

data Options = Options
	{ oUserName :: String
	, oPassWord :: String
	, oServer :: String
	, oResource :: String
	, oVerbose :: Bool
	, oNoTLSVerify :: Bool
	, oLlamaURL :: String
	} deriving (Eq, Show)

defaultOptions = Options
	{ oUserName = ""
	, oPassWord = ""
	, oServer = ""
	, oResource = "hsendxmpp"
	, oVerbose = False
	, oNoTLSVerify = False
	, oLlamaURL = "http://localhost:8080"
	}

options :: [OptDescr (Options -> Options)]
options =
	[ Option ['u']	["username"]	(ReqArg	(\str o -> o { oUserName = str }) "user")	"Use this username to authenticate to the server"
	, Option ['p']	["password"]	(ReqArg	(\str o -> o { oPassWord = str }) "password") $	"Use this password to authenticate to the server.\nThe password can also be provided via " ++ passWordEnvVar ++ " environment variable to avoid it leaking into process lists, and it will override the CLI option contents."
	, Option ['j']	["jserver"]	(ReqArg	(\str o -> o { oServer = str }) "server")	"Connect to this server"
	, Option ['r']	["resource"]	(ReqArg	(\str o -> o { oResource = str }) "res")	"Use resource res for the sender [default: 'hsendxmpp']"
	, Option ['v']	["verbose"]	(NoArg	(\o -> o { oVerbose = True }))			"Be verbose on what's happening on the wire"
	, Option ['n']	["no-tls-verify"]	(NoArg	(\o -> o { oNoTLSVerify = True }))	"Accept TLS certificates without verification"
	, Option ['l']	["llama-url"]	(ReqArg	(\str o -> o { oLlamaURL = str }) "url") $	"URL of llama-server to connect to for ^llama comand [default: '" ++ oLlamaURL defaultOptions ++ "']"
	]

getOpts :: IO (Options, [String])
getOpts = do
	args <- getArgs
	pn <- getProgName
	case getOpt Permute options args of
		(o,n,[]  ) -> return (foldl (flip id) defaultOptions o, n)
		(_,_,errs) -> ioError (userError (concat errs ++ usageInfo ("Usage: " ++ pn ++ " [options] <room1> [<room2> ...]") options))

handleRoom :: Options -> Session -> String -> IO ()
handleRoom opts sess room = do
	let parsedJid = parseJid room
	let (roomName, roomServer, _) = jidToTexts parsedJid
	let roomJid = fromJust $ jidFromTexts roomName roomServer $ Just $ S.toText $ oResource opts
	result <- joinMUCResult roomJid (Just $ def { mhrMaxStanzas = Just 0}) sess
	either (\err -> error $ show $ stanzaErrorText err) (const $ pure ()) result
	forever $ do
		msg <- getMessage sess
		let body = do
			imm <- getIM msg
			(h, _) <- uncons $ imBody imm
			pure h
		case body of
			Just body -> do
				case T.uncons $ bodyContent body of
					Just ('^', cmd) -> void $ forkIO $ case T.words cmd of
						"r":args -> do
							roomPeersPresences <- atomically $ getPeerEntities parsedJid sess
							let occupants = map fromNonempty $ catMaybes $ map resourcepart_ $ M.keys roomPeersPresences
							let answer = T.concat
								["Ready for "
								, T.unwords args
								, "? "
								, T.unwords occupants
								]
							sendMessage ((simpleIM parsedJid answer) { messageType = GroupChat }) sess
							pure ()
						"llama":args -> do
							llamaReply <- llamaTemplated (oLlamaURL opts) $ LlamaApplyTemplateRequest
								[ LlamaMessage System "Provie a short answer to the following:"
								, LlamaMessage User $ T.unwords args
								]
							fromMaybe (pure ()) $ do
								answer <- llamaReply
								pure $ void $ sendMessage ((simpleIM parsedJid answer) { messageType = GroupChat }) sess
						_ -> pure ()
					_-> pure ()
				pure ()
			_ -> pure ()

main :: IO ()
main = do
	(opts, [room]) <- getOpts
	when (oVerbose opts) $ updateGlobalLogger "Pontarius.Xmpp" $ setLevel DEBUG
	let server = if oServer opts == "" then error "no server specified" else oServer opts
	envPassWord <- lookupEnv passWordEnvVar
	let justEnvPassWord = fromMaybe "" envPassWord
	let passWord = if null justEnvPassWord then oPassWord opts else justEnvPassWord

	let authData = Just (fst $ fromJust (simpleAuth (S.toText $ oUserName opts) (S.toText passWord)), if null $ oResource opts then Nothing else Just $ S.toText $ oResource opts) :: AuthData
	let sessionConfiguration = if oNoTLSVerify opts
		then def { sessionStreamConfiguration = def { tlsParams = xmppDefaultParams { clientHooks = def { onServerCertificate = \_ _ _ _ -> pure [] } } } }
		else def
	eSess <- session server authData sessionConfiguration
	let sess = either (error . show) id $ eSess
	sendPresence presenceOnline sess

	handleRoom opts sess room

	sendPresence presenceOffline sess
	-- FIXME a workaround for https://github.com/l29ah/hsendxmpp/issues/1
	threadDelay 1000000
	endSession sess
