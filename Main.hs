{-# LANGUAGE OverloadedStrings, Strict #-}

module Main where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception
import Control.Monad
import Data.Foldable
import Data.IORef
import Data.List (uncons)
import qualified Data.Map as M
import Data.Maybe
import Data.Sequence (Seq(..))
import qualified Data.Sequence as Seq
import qualified Data.String.Class as S
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Llama
import Network.TLS
import Network.Xmpp
import Network.Xmpp.IM
import Network.Xmpp.Internal hiding (priority, status)
import Network.Xmpp.Extras.MUC
import System.Console.GetOpt
import System.Environment
import System.Log.Logger

import Paste

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

pushToCircular :: IORef (Seq a) -> a -> IO ()
pushToCircular ref msg = atomicModifyIORef' ref $ \log -> (
	case log of
		_ :<| init_ -> if Seq.length log >= 20 then init_ :|> msg else log :|> msg
		_ -> log :|> msg
	, ())

joinRoom opts sess room = do
	let myNickname = T.pack $ oResource opts
	let parsedJid = parseJid room
	let (roomName, roomServer, _) = jidToTexts parsedJid
	let roomJid = fromJust $ jidFromTexts roomName roomServer $ Just myNickname
	joinMUC roomJid (Just $ def { mhrMaxStanzas = Just 0 }) sess
	-- to handle the case of our JID being already connected to the MUC under a different nickname, as seen with biboumi
	-- XEP-0045, 7.6 Changing Nickname
	sendPresence (presTo presence roomJid) sess

handleRoom :: Options -> Session -> String -> IORef (Seq.Seq T.Text) -> IO ()
handleRoom opts sess room roomContext = do
	let myNickname = T.pack $ oResource opts
	let parsedJid = parseJid room
	forever $ do
		msg <- getMessage sess
		let say x = void $ sendMessage ((simpleIM parsedJid x) { messageType = GroupChat }) sess
		when (messageType msg == GroupChat) $ handle (\e -> say $ T.pack $ show (e :: SomeException)) $ do
			let body = do
				imm <- getIM msg
				(h, _) <- uncons $ imBody imm
				pure h
			case (body, messageFrom msg >>= resourcepart) of
				(Just body, Just resource) -> do
					-- got a new meaningful message, append to the log
					let logMsg = T.concat ["<", resource, "> ", bodyContent body]
					when (oVerbose opts) $ T.putStrLn logMsg
					pushToCircular roomContext logMsg
					when (resource /= myNickname) $ do -- ignore messages from yourself and without a body
						let reply txt = do
							when (oVerbose opts) $ T.putStrLn txt
							pasted <- paste txt
							say $ T.concat [resource, ": ", pasted]
						let onLlamaError = reply "llama-server is offline"
						let doLlama req = do
							llamaReply <- llamaTemplated (oLlamaURL opts) req
							maybe onLlamaError (reply . T.stripStart. snd . T.breakOnEnd "</think>") llamaReply
						case T.uncons $ bodyContent body of
							Just ('^', cmd) -> void $ forkIO $ case T.words cmd of
								"test":_ -> do
									say "passed"
								"r":args -> do
									roomPeersPresences <- atomically $ getPeerEntities parsedJid sess
									let occupants = map fromNonempty $ mapMaybe resourcepart_ $ M.keys roomPeersPresences
									let answer = T.concat
										["Ready for "
										, T.unwords args
										, "? "
										, T.unwords occupants
										]
									sendMessage ((simpleIM parsedJid answer) { messageType = GroupChat }) sess
									pure ()
								"llama":args -> do
									doLlama $ LlamaApplyTemplateRequest
										[ LlamaMessage System "Provide a short answer to the following:"
										, LlamaMessage User $ T.unwords args
										]
								"llamaraw":args -> do
									llamaReply <- llama (oLlamaURL opts) $ T.unwords args
									maybe onLlamaError reply llamaReply
								_ -> pure ()
							_-> pure ()
						when (T.isPrefixOf myNickname $ bodyContent body) $ do
							-- human-like call
							context <- readIORef roomContext
							let systemPrompt = T.concat	[ "You are a XMPP user "
											, myNickname
											, ". You are friendly, straight, informal, maybe ironic, but always informative. You will follow up to the last message, address the topic, and provide a ONE-LINE thoughtful and constructive response, without prepending your nickname. Try to helpfully surprise if you can."
											]
							doLlama $ LlamaApplyTemplateRequest $
								LlamaMessage System systemPrompt : map (LlamaMessage User) (toList context)
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
	roomContext <- newIORef Seq.Empty

	let sessionConfiguration = (if oNoTLSVerify opts
		then def { sessionStreamConfiguration = def { tlsParams = xmppDefaultParams { clientHooks = def { onServerCertificate = \_ _ _ _ -> pure [] } } } }
		else def)
		{ enableRoster = False
		, onConnectionClosed = \sess why -> do
			putStrLn $ "Disconnected (" ++ show why ++ "). Reconnecting..."
			attempts <- reconnect' sess
			putStrLn $ "Reconnected after " <> show attempts <> " attempts."
			-- mandatory initial presence
			void $ sendPresence presenceOnline sess
			joinRoom opts sess room
		}
	eSess <- session server authData sessionConfiguration
	let sess = either (error . show) id eSess
	-- mandatory initial presence
	sendPresence presenceOnline sess
	joinRoom opts sess room
	handleRoom opts sess room roomContext

	sendPresence presenceOffline sess
	-- FIXME a workaround for https://github.com/l29ah/hsendxmpp/issues/1
	threadDelay 1000000
	endSession sess
