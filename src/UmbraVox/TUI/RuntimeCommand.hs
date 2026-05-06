-- SPDX-License-Identifier: Apache-2.0
module UmbraVox.TUI.RuntimeCommand
    ( RuntimeCommand(..)
    , commandForMenuItem
    , runRuntimeCommand
    ) where

import UmbraVox.TUI.Types
import UmbraVox.TUI.Actions
    ( startExport, startImport, renameContact
    , sendCurrentMessage, quitApp
    )
import Data.IORef (readIORef, writeIORef)
import UmbraVox.TUI.Handshake (genIdentity)
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

data RuntimeCommand
    = CmdOpenNewConversation
    | CmdOpenBrowse
    | CmdOpenVerify
    | CmdShowHelp
    | CmdShowAbout
    | CmdOpenSettings
    | CmdOpenKeys
    | CmdRenameContact
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
    | CmdExportChat
    | CmdImportChat
    | CmdQuit
    deriving stock (Eq, Show)

commandForMenuItem :: MenuTab -> Int -> Maybe RuntimeCommand
commandForMenuItem MenuHelp 0     = Just CmdShowHelp
commandForMenuItem MenuHelp 1     = Just CmdShowAbout
commandForMenuItem MenuContacts 0 = Just CmdOpenBrowse
commandForMenuItem MenuContacts 1 = Just CmdOpenVerify
commandForMenuItem MenuChat 0     = Just CmdOpenNewConversation
commandForMenuItem MenuChat 1     = Just CmdRenameContact
commandForMenuItem MenuChat 2     = Just CmdSendCurrentMessage
commandForMenuItem MenuChat 3     = Just CmdClearInput
commandForMenuItem MenuPrefs 0    = Just CmdOpenSettings
commandForMenuItem MenuPrefs 1    = Just CmdOpenKeys
commandForMenuItem MenuPrefs 2    = Just CmdToggleMDNS
commandForMenuItem MenuPrefs 3    = Just CmdExportChat
commandForMenuItem MenuPrefs 4    = Just CmdImportChat
commandForMenuItem MenuQuit 0     = Just CmdQuit
commandForMenuItem _ _            = Nothing

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
        CmdOpenSettings        ->
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
            toggleSettingWithStatus st "settings.pex" (cfgPEXEnabled (asConfig st))
                "Peer exchange enabled"
                "Peer exchange disabled"
        CmdTogglePersistentStorage ->
            togglePersistentStorage st
        CmdToggleAutoSave      ->
            toggleSettingWithStatus st "settings.auto_save" (cfgAutoSaveMessages (asConfig st))
                "Auto-save messages enabled"
                "Auto-save messages disabled"
        CmdToggleDebugLogging  ->
            toggleSettingWithStatus st "settings.debug_logging" (cfgDebugLogging (asConfig st))
                "Runtime debug logging enabled"
                "Runtime debug logging disabled"
        CmdCycleConnectionMode ->
            cycleConnectionMode st
        CmdExportChat          -> startExport st
        CmdImportChat          -> startImport st
        CmdQuit                -> quitApp st
