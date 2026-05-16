-- SPDX-License-Identifier: Apache-2.0
module UmbraVox.TUI.RuntimeCommand
    ( RuntimeCommand(..)
    , commandForMenuItem
    , runRuntimeCommand
    ) where

import UmbraVox.TUI.Types
import UmbraVox.BuildProfile
    ( BuildPluginId(..), pluginEnabled
    , pluginUnavailableStatus
    )
import UmbraVox.TUI.Actions
    ( startExport, startImport, renameContact
    , sendCurrentMessage, quitApp, setStatus, openRegenKeyDialog
    )
import Data.IORef (readIORef, writeIORef, modifyIORef')
import Data.List (nub)
import UmbraVox.TUI.Handshake (genIdentity)
import UmbraVox.Plugin.Registry
    ( resolveEnable, enablePlugin, disablePlugin )
import qualified UmbraVox.Plugin.Registry as Registry
import UmbraVox.TUI.RuntimeEvent
    ( RuntimeEvent(..), applyRuntimeEvents )
import UmbraVox.TUI.RuntimeSettings
    ( toggleMDNSSetting, togglePersistentStorage
    , toggleSettingWithStatus, cycleConnectionMode
    , applyDisplayName, applyDBPath, applyRetentionDays
    , applyDebugLogPath, clearSelectedHistoryConfirmed
    )
import UmbraVox.TUI.RuntimeNetwork
    ( applyListenPort, connectGroupTargets, connectToPeer, startListenerOnPort )
import UmbraVox.App.Startup
    ( refreshPackagedPluginCatalog
    , refreshTransportProviderCatalog
    )
import UmbraVox.Storage.Anthony (saveSetting)

data RuntimeCommand
    = CmdOpenNewConversation
    | CmdOpenBrowse
    | CmdOpenVerify
    | CmdShowHelp
    | CmdShowAbout
    | CmdOpenSettings
    | CmdOpenKeys
    | CmdRenameContact
    | CmdToggleRichText
    | CmdInsertBold
    | CmdInsertItalic
    | CmdInsertColor
    | CmdInsertLink
    | CmdInsertEmoji
    | CmdSendCurrentMessage
    | CmdClearInput
    | CmdSetListenPort Int
    | CmdStartListener Int
    | CmdConnectPeer String (Maybe Int)
    | CmdConnectGroup [String]
    | CmdSetDisplayName String
    | CmdSetDBPath FilePath
    | CmdSetRetentionDays Int
    | CmdSetDebugLogPath FilePath
    | CmdClearSelectedHistory
    | CmdToggleMDNS
    | CmdTogglePEX
    | CmdTogglePersistentStorage
    | CmdToggleAutoSave
    | CmdToggleDebugLogging
    | CmdCycleConnectionMode
    | CmdTogglePlugin String
    | CmdExportChat
    | CmdImportChat
    | CmdOpenRegenKey
    | CmdOpenExportWarn
    | CmdOpenImportKey
    | CmdToggleKeyInfo
    | CmdQuit
    deriving stock (Eq, Show)

commandForMenuItem :: MenuTab -> Int -> Maybe RuntimeCommand
commandForMenuItem MenuHelp 0     = Just CmdShowHelp
commandForMenuItem MenuHelp 1     = Just CmdShowAbout
commandForMenuItem MenuIdentity 0 = Just CmdOpenRegenKey
commandForMenuItem MenuIdentity 1 = Just CmdOpenExportWarn
commandForMenuItem MenuIdentity 2 = Just CmdOpenImportKey
commandForMenuItem MenuIdentity 3 = Just CmdToggleKeyInfo
commandForMenuItem MenuPrefs idx  = case menuPrefsCommands !!? idx of
    Just cmd -> Just cmd
    Nothing -> Nothing
commandForMenuItem MenuQuit 0     = Just CmdQuit
commandForMenuItem _ _            = Nothing

menuPrefsCommands :: [RuntimeCommand]
menuPrefsCommands =
    [CmdOpenSettings, CmdToggleRichText]
    ++ (if pluginEnabled PluginChatTransfer then [CmdExportChat, CmdImportChat] else [])

(!!?) :: [a] -> Int -> Maybe a
(!!?) [] _ = Nothing
(!!?) (x:_) 0 = Just x
(!!?) (_:xs) n = (!!?) xs (n - 1)

runRuntimeCommand :: AppState -> RuntimeCommand -> IO ()
runRuntimeCommand st cmd =
    case cmd of
        CmdOpenNewConversation ->
            applyRuntimeEvents st [EventSetDialog (Just DlgNewConn)]
        CmdOpenBrowse          ->
            applyRuntimeEvents st [EventResetBrowse, EventSetDialog (Just DlgBrowse)]
        CmdOpenVerify          ->
            applyRuntimeEvents st [EventSetDialog (Just DlgVerify)]
        CmdShowHelp            ->
            applyRuntimeEvents st [EventSetDialog (Just DlgHelp)]
        CmdShowAbout           ->
            applyRuntimeEvents st [EventSetDialog (Just DlgAbout)]
        CmdOpenSettings        -> do
            refreshPackagedPluginCatalog (asConfig st)
            refreshTransportProviderCatalog (asConfig st)
            applyRuntimeEvents st [EventSetDialogTab 0, EventSetDialog (Just DlgSettings)]
        CmdOpenKeys            -> do
            mIk <- readIORef (cfgIdentity (asConfig st))
            case mIk of
                Nothing -> do
                    ik <- genIdentity
                    writeIORef (cfgIdentity (asConfig st)) (Just ik)
                Just _  -> pure ()
            applyRuntimeEvents st [EventSetDialog (Just DlgKeys)]
        CmdRenameContact       -> renameContact st
        CmdToggleRichText      -> do
            enabled <- readIORef (asRichText st)
            let newEnabled = not enabled
            writeIORef (asRichText st) newEnabled
            mDb <- readIORef (cfgAnthonyDB (asConfig st))
            case mDb of
                Just db -> saveSetting db "rich_text" (if newEnabled then "on" else "off")
                Nothing -> pure ()
            setStatus st (if newEnabled then "Rich text enabled" else "Rich text disabled")
        CmdInsertBold          -> do
            mSel <- readIORef (asSelectionStart st)
            case mSel of
                Just _ -> wrapSelectionMarkers st "**" "**"
                Nothing -> openFormatPrompt st "Bold text" (\val -> wrapSnippet st ("**" ++ val ++ "**"))
        CmdInsertItalic        -> do
            mSel <- readIORef (asSelectionStart st)
            case mSel of
                Just _ -> wrapSelectionMarkers st "*" "*"
                Nothing -> openFormatPrompt st "Italic text" (\val -> wrapSnippet st ("*" ++ val ++ "*"))
        CmdInsertColor         -> do
            mSel <- readIORef (asSelectionStart st)
            case mSel of
                Just _ -> openFormatPrompt st "Color for selection (example: red)" $
                    \colorName ->
                        if null colorName
                            then setStatus st "Color insert requires a color name"
                            else wrapSelectionMarkersColor st colorName
                Nothing -> openFormatPrompt st "Color|text (example: red|warning)" $
                    \val -> case break (== '|') val of
                        ([], _) -> setStatus st "Color insert expects color|text"
                        (_, []) -> setStatus st "Color insert expects color|text"
                        (colorName, _:txt) ->
                            wrapSnippet st ("<font color=\"" ++ colorName ++ "\">" ++ txt ++ "</font>")
        CmdInsertLink          -> do
            buf    <- readIORef (asInputBuf st)
            cursor <- readIORef (asInputCursor st)
            mSel   <- readIORef (asSelectionStart st)
            let selText = case mSel of
                    Just selStart ->
                        let lo = min selStart cursor
                            hi = max selStart cursor
                        in take (hi - lo) (drop lo buf)
                    Nothing -> ""
            writeIORef (asLinkText st) selText
            writeIORef (asLinkUrl st) ""
            writeIORef (asLinkFocus st) (if null selText then 0 else 1)
            writeIORef (asDialogMode st) (Just DlgInsertLink)
            writeIORef (asDialogScroll st) 0
        CmdInsertEmoji         -> do
            writeIORef (asEmojiSearch st) ""
            writeIORef (asEmojiCategory st) 0
            writeIORef (asEmojiPage st) 0
            applyRuntimeEvents st [EventSetDialog (Just DlgEmojiPicker)]
        CmdSendCurrentMessage  -> sendCurrentMessage st
        CmdClearInput          ->
            applyRuntimeEvents st [EventSetInput "", EventSetStatus "Input cleared"]
        CmdSetListenPort p     -> applyListenPort st p
        CmdStartListener p     -> startListenerOnPort st p "dialog"
        CmdConnectPeer h mPort -> connectToPeer st h mPort
        CmdConnectGroup peers  -> connectGroupTargets st peers
        CmdSetDisplayName val  -> applyDisplayName st val
        CmdSetDBPath val       -> applyDBPath st val
        CmdSetRetentionDays n  -> applyRetentionDays st n
        CmdSetDebugLogPath val -> applyDebugLogPath st val
        CmdClearSelectedHistory -> clearSelectedHistoryConfirmed st
        CmdToggleMDNS          -> toggleMDNSSetting st
        CmdTogglePEX           ->
            if not (pluginEnabled PluginPeerExchange)
                then applyRuntimeEvents st [EventSetStatus (pluginUnavailableStatus PluginPeerExchange)]
                else toggleSettingWithStatus st "settings.pex" (cfgPEXEnabled (asConfig st))
                    "Peer exchange enabled"
                    "Peer exchange disabled"
        CmdTogglePersistentStorage ->
            if not (pluginEnabled PluginPersistentStorage)
                then applyRuntimeEvents st [EventSetStatus (pluginUnavailableStatus PluginPersistentStorage)]
                else togglePersistentStorage st
        CmdToggleAutoSave      ->
            if not (pluginEnabled PluginPersistentStorage)
                then applyRuntimeEvents st [EventSetStatus (pluginUnavailableStatus PluginPersistentStorage)]
                else toggleSettingWithStatus st "settings.auto_save" (cfgAutoSaveMessages (asConfig st))
                    "Auto-save messages enabled"
                    "Auto-save messages disabled"
        CmdToggleDebugLogging  ->
            if not (pluginEnabled PluginRuntimeLogging)
                then applyRuntimeEvents st [EventSetStatus (pluginUnavailableStatus PluginRuntimeLogging)]
                else toggleSettingWithStatus st "settings.debug_logging" (cfgDebugLogging (asConfig st))
                    "Runtime debug logging enabled"
                    "Runtime debug logging disabled"
        CmdCycleConnectionMode ->
            cycleConnectionMode st
        CmdTogglePlugin pid    -> do
            reg <- readIORef (cfgPluginRegistry (asConfig st))
            if Registry.pluginEnabled pid reg
                then do
                    modifyIORef' (cfgPluginRegistry (asConfig st)) (disablePlugin pid)
                    applyRuntimeEvents st [EventSetStatus ("Plugin disabled: " ++ pid)]
                else
                    case resolveEnable pid reg of
                        Left err ->
                            applyRuntimeEvents st [EventSetStatus ("Plugin error: " ++ err)]
                        Right toEnable -> do
                            let newIds = filter (\p -> not (Registry.pluginEnabled p reg)) (nub toEnable)
                            modifyIORef' (cfgPluginRegistry (asConfig st))
                                (\r -> foldr enablePlugin r toEnable)
                            let msg = if null newIds || newIds == [pid]
                                    then "Plugin enabled: " ++ pid
                                    else "Plugin enabled: " ++ pid ++ " (+ " ++
                                         show (length newIds - 1) ++ " deps)"
                            applyRuntimeEvents st [EventSetStatus msg]
        CmdExportChat
            | pluginEnabled PluginChatTransfer -> startExport st
            | otherwise -> applyRuntimeEvents st [EventSetStatus (pluginUnavailableStatus PluginChatTransfer)]
        CmdImportChat
            | pluginEnabled PluginChatTransfer -> startImport st
            | otherwise -> applyRuntimeEvents st [EventSetStatus (pluginUnavailableStatus PluginChatTransfer)]
        CmdOpenRegenKey        -> openRegenKeyDialog st
        CmdOpenExportWarn      ->
            writeIORef (asRegenCheckbox st) False >>
            writeIORef (asDialogMode st) (Just DlgExportWarn) >>
            writeIORef (asDialogScroll st) 0
        CmdOpenImportKey       -> do
            writeIORef (asDialogBuf st) ""
            writeIORef (asDialogMode st) (Just DlgKeys)
            writeIORef (asDialogScroll st) 0
        CmdToggleKeyInfo       -> do
            shown <- readIORef (asShowIdentity st)
            let newShown = not shown
            writeIORef (asShowIdentity st) newShown
            setStatus st (if newShown then "Key info shown" else "Key info hidden")
        CmdQuit                -> quitApp st

openFormatPrompt :: AppState -> String -> (String -> IO ()) -> IO ()
openFormatPrompt st title cb = do
    writeIORef (asDialogBuf st) ""
    writeIORef (asDialogMode st) (Just (DlgPrompt title cb))

-- | Wrap the currently selected text with open/close markdown markers.
-- If no selection is active, insert empty markers at cursor.
wrapSelectionMarkers :: AppState -> String -> String -> IO ()
wrapSelectionMarkers st open close = do
    buf <- readIORef (asInputBuf st)
    cursor <- readIORef (asInputCursor st)
    mSel <- readIORef (asSelectionStart st)
    case mSel of
        Just selStart -> do
            let lo = min selStart cursor
                hi = max selStart cursor
                (pre, rest') = splitAt lo buf
                (mid, post) = splitAt (hi - lo) rest'
                newBuf = pre ++ open ++ mid ++ close ++ post
                newCursor = lo + length open + (hi - lo) + length close
            writeIORef (asRichText st) True
            writeIORef (asInputBuf st) newBuf
            writeIORef (asInputCursor st) newCursor
            writeIORef (asSelectionStart st) Nothing
            writeIORef (asInputScroll st) 0
            applyRuntimeEvents st [EventSetStatus "Applied formatting"]
        Nothing -> wrapSnippet st (open ++ close)

-- | Wrap selection with color font tag.
wrapSelectionMarkersColor :: AppState -> String -> IO ()
wrapSelectionMarkersColor st colorName =
    wrapSelectionMarkers st ("<font color=\"" ++ colorName ++ "\">") "</font>"

-- | Wrap selection as a markdown link with given URL.
wrapSelectionMarkersLink :: AppState -> String -> IO ()
wrapSelectionMarkersLink st url = do
    buf <- readIORef (asInputBuf st)
    cursor <- readIORef (asInputCursor st)
    mSel <- readIORef (asSelectionStart st)
    case mSel of
        Just selStart -> do
            let lo = min selStart cursor
                hi = max selStart cursor
                (pre, rest') = splitAt lo buf
                (mid, post) = splitAt (hi - lo) rest'
                newBuf = pre ++ "[" ++ mid ++ "](" ++ url ++ ")" ++ post
                newCursor = lo + 1 + (hi - lo) + 2 + length url + 1
            writeIORef (asRichText st) True
            writeIORef (asInputBuf st) newBuf
            writeIORef (asInputCursor st) newCursor
            writeIORef (asSelectionStart st) Nothing
            writeIORef (asInputScroll st) 0
            applyRuntimeEvents st [EventSetStatus "Applied link formatting"]
        Nothing -> wrapSnippet st "[](url)"

wrapSnippet :: AppState -> String -> IO ()
wrapSnippet st snippet = do
    buf <- readIORef (asInputBuf st)
    cursor <- readIORef (asInputCursor st)
    let (lhs, rhs) = splitAt cursor buf
        newBuf = lhs ++ snippet ++ rhs
        newCursor = length lhs + length snippet
    writeIORef (asRichText st) True
    writeIORef (asInputBuf st) newBuf
    writeIORef (asInputCursor st) newCursor
    writeIORef (asSelectionStart st) Nothing
    writeIORef (asInputScroll st) 0
    applyRuntimeEvents st [EventSetStatus "Inserted markdown snippet"]
