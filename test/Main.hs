module Main (main) where

import System.Environment (getArgs)
import System.Exit (exitFailure, exitSuccess)
import Data.List (find, intercalate)

import qualified Test.App.Startup as AppStartup
import qualified Test.Chat.API as ChatAPI
import qualified Test.Chat.Contacts as ChatContacts
import qualified Test.Chat.Message as ChatMessage
import qualified Test.Chat.Session as ChatSession
import qualified Test.Chat.Transaction as ChatTransaction
import qualified Test.Chat.WireEdge as ChatWireEdge
import qualified Test.Codegen as Codegen
import qualified Test.Consensus.Block as ConsensusBlock
import qualified Test.Consensus.ForkChoice as ConsensusForkChoice
import qualified Test.Consensus.LeaderElection as ConsensusLeaderElection
import qualified Test.Consensus.Ledger as ConsensusLedger
import qualified Test.Consensus.Mempool as ConsensusMempool
import qualified Test.Consensus.Nonce as ConsensusNonce
import qualified Test.Consensus.Protocol as ConsensusProtocol
import qualified Test.Consensus.Truncation as ConsensusTruncation
import qualified Test.Consensus.Types as ConsensusTypes
import qualified Test.Consensus.Validation as ConsensusValidation
import qualified Test.Crypto.AES as AES
import qualified Test.Crypto.BIP39 as BIP39
import qualified Test.Crypto.ChaCha20 as ChaCha20
import qualified Test.Crypto.Curve25519 as Curve25519
import qualified Test.Crypto.Differential as Differential
import qualified Test.Crypto.Differential.Primitives as DiffPrimitives
import qualified Test.Crypto.Differential.Negative as DiffNegative
import qualified Test.Crypto.Differential.Metamorphic as DiffMetamorphic
import qualified Test.Crypto.Ed25519 as Ed25519
import qualified Test.Crypto.Export as Export
import qualified Test.Crypto.GCM as GCM
import qualified Test.Crypto.HKDF as HKDF
import qualified Test.Crypto.HMAC as HMAC
import qualified Test.Crypto.Keccak as Keccak
import qualified Test.Crypto.KeyStore as KeyStore
import qualified Test.Crypto.RatchetPersist as RatchetPersist
import qualified Test.Crypto.SecureBytes as SecureBytes
import qualified Test.Crypto.MLKEM as MLKEM
import qualified Test.Crypto.PQWrapper as PQWrapper
import qualified Test.Crypto.PQXDH as PQXDH
import qualified Test.Crypto.Poly1305 as Poly1305
import qualified Test.Crypto.Random as Random
import qualified Test.Crypto.SHA256 as SHA256
import qualified Test.Crypto.SHA512 as SHA512
import qualified Test.CryptoIntegrity as CryptoIntegrity
import qualified Test.Crypto.Signal.DoubleRatchet as DoubleRatchet
import qualified Test.Crypto.Signal.SenderKeys as SenderKeys
import qualified Test.Crypto.Signal.Session as Session
import qualified Test.Crypto.Signal.X3DH as X3DH
import qualified Test.Crypto.StealthAddress as StealthAddress
import qualified Test.Crypto.VRF as VRF
import qualified Test.EndToEnd as EndToEnd
import qualified Test.EndToEnd2 as EndToEnd2
import qualified Test.Economics.Cycle as EconCycle
import qualified Test.Economics.Fees as EconFees
import qualified Test.Economics.Onboarding as EconOnboarding
import qualified Test.Economics.Penalty as EconPenalty
import qualified Test.Economics.Rewards as EconRewards
import qualified Test.Economics.Token as EconToken
import qualified Test.Equivalence as Equivalence
import qualified Test.Fuzz as Fuzz
import qualified Test.FuzzConnection as FuzzConnection
import qualified Test.FuzzInputs as FuzzInputs
import qualified Test.Hardening.Fault as HardeningFault
import qualified Test.Security.Adversarial as Adversarial
import qualified Test.Security.M11KeyMgmt as M11KeyMgmt
import qualified Test.Security.M11NoiseDH as M11NoiseDH
import qualified Test.Security.M11Protocol as M11Protocol
import qualified Test.Security.M11High as M11High
import qualified Test.Security.M11HighAuth as M11HighAuth
import qualified Test.Security.M11HighFS as M11HighFS
import qualified Test.Security.M11HighPQ as M11HighPQ
import qualified Test.Security.M11HighPQHash as M11HighPQHash
import qualified Test.Security.M11HighProto as M11HighProto
import qualified Test.Security.M11HighKeyImpl as M11HighKeyImpl
import qualified Test.Security.M11HighSymHash as M11HighSymHash
import qualified Test.Security.M11Medium as M11Medium
import qualified Test.Security.M11HighSC2 as M11HighSC2
import qualified Test.Security.M11SideChannel as M11SideChannel
import qualified Test.Security.M11Symmetric as M11Symmetric
import qualified Test.Security.M11Asymmetric as M11Asymmetric
import qualified Test.Security.M11HighAS2 as M11HighAS2
import qualified Test.Security.M11HighRemaining as M11HighRemaining
import qualified Test.Security.M11HighSMFSIA as M11HighSMFSIA
import qualified Test.Security.M11HighCrypto3 as M11HighCrypto3
import qualified Test.Security.M11HighKM3 as M11HighKM3
import qualified Test.Security.M11Final as M11Final
import qualified Test.Security.MCDC as MCDC
import qualified Test.Security.Regression as Regression
import qualified Test.Security.RegressionM7 as RegressionM7
import qualified Test.Security.RegressionM8 as RegressionM8
import qualified Test.Security.RegressionNet as RegressionNet
import qualified Test.Security.Unicode as Unicode
import qualified Test.Hardening.Recovery as HardeningRecovery
import qualified Test.Hardening.Soak as HardeningSoak
import qualified Test.Hardening.TCP as HardeningTCP
import qualified Test.Integration as Integration
import qualified Test.Network.MDNS as MDNS
import qualified Test.Network.Noise as Noise
import qualified Test.Network.PeerExchange as PeerExchange
import qualified Test.Network.Protocol as Protocol
import qualified Test.Network.Transport as Transport
import qualified Test.Network.Transport.Loopback as Loopback
import qualified Test.Network.TransportClass as TransportClass
import qualified Test.Network.TransportEdge as TransportEdge
import qualified Test.Network.UDP as UDP
import qualified Test.Network.Socks5Live as Socks5Live
import qualified Test.Protocol.CBOR as CBOR
import qualified Test.Protocol.MessageFormat as MessageFormat
import qualified Test.Protocol.QRCode as QRCode
import qualified Test.Protocol.SafetyNumber as SafetyNumber
import qualified Test.Protocol.WireFormat as WireFormat
import qualified Test.Security as Security
import qualified Test.App.RuntimeLog as AppRuntimeLog
import qualified Test.Plugin.Registry as PluginRegistry
import qualified Test.Plugin.EphemeralIntegration as EphemeralIntegration
import qualified Test.Storage.Anthony as Anthony
import qualified Test.Storage.Encryption as StorageEncryption
import qualified Test.Storage.ChainDB as ChainDB
import qualified Test.Storage.Checkpoint as Checkpoint
import qualified Test.Storage.Index as StorageIndex
import qualified Test.Storage.StateDB as StateDB
import qualified Test.TUI.Actions as TUIActions
import qualified Test.TUI.Handshake as TUIHandshake
import qualified Test.TUI.HandshakeEdge as TUIHandshakeEdge
import qualified Test.TUI.Input as TUIInput
import qualified Test.TUI.Paths as TUIPaths
import qualified Test.TUI.Render as TUIRender
import qualified Test.TUI.Types as TUITypes
import qualified Test.TUI.Sim.Chat as TUISimChat
import qualified Test.TUI.Sim.Contacts as TUISimContacts
import qualified Test.TUI.Sim.Dialogs as TUISimDialogs
import qualified Test.TUI.Sim.Focus as TUISimFocus
import qualified Test.TUI.Sim.Menu as TUISimMenu
import qualified Test.TUI.Sim.MessageFlow as TUISimMessageFlow
import qualified Test.TUI.Sim.Runtime as TUISimRuntime
import qualified Test.TUI.Sim.SettingsEdge as TUISimSettingsEdge
import qualified Test.TUI.Sim.Shortcuts as TUISimShortcuts
import qualified Test.TUI.Sim.Workflows as TUISimWorkflows
import qualified Test.Tools.Complexity as ToolsComplexity
import qualified Test.Tools.FStarVerify as ToolsFStarVerify
import qualified Test.Tools.FetchReferences as ToolsFetchReferences

data Suite = Suite
    { suiteName :: String
    , suiteAction :: IO Bool
    }

main :: IO ()
main = do
    args <- getArgs
    ok <- case args of
        ("startup-process-child":answer:_) -> AppStartup.runStartupProcessChild answer
        [] -> runSuiteArg "required"
        (x:_) -> runSuiteArg x
    if ok
        then exitSuccess
        else exitFailure

runSuiteArg :: String -> IO Bool
runSuiteArg suiteArg =
    case suiteArg of
        "required" -> runSuiteGroup "UmbraVox Messaging MVP Fast Gate" requiredSuites
        "core" -> runSuiteGroup "UmbraVox Core Messaging Suite" coreSuites
        "core-crypto" -> runSuiteGroup "UmbraVox Core Crypto Suite" coreCryptoSuites
        "core-network" -> runSuiteGroup "UmbraVox Core Network Suite" coreNetworkSuites
        "core-chat" -> runSuiteGroup "UmbraVox Core Chat Suite" coreChatSuites
        "core-tui" -> runSuiteGroup "UmbraVox Core TUI Suite" coreTUISuites
        "core-tools" -> runSuiteGroup "UmbraVox Core Tools Suite" coreToolsSuites
        "tcp" -> runSuiteGroup "UmbraVox TCP Hardening Suite" tcpSuites
        "fault" -> runSuiteGroup "UmbraVox Fault Hardening Suite" faultSuites
        "recovery" -> runSuiteGroup "UmbraVox Recovery Hardening Suite" recoverySuites
        "tui-sim" -> runSuiteGroup "UmbraVox TUI Simulation Suite" tuiSimSuites
        "regression" -> runSuiteGroup "UmbraVox Security Regression Suite"
            [Suite "regression" Regression.runTests]
        "regression-m7" -> runSuiteGroup "UmbraVox M7 Security Regression Suite"
            [Suite "regression-m7" RegressionM7.runTests]
        "regression-m8" -> runSuiteGroup "UmbraVox M8 Security Regression Suite"
            [Suite "regression-m8" RegressionM8.runTests]
        "regression-net" -> runSuiteGroup "UmbraVox Network/Protocol Regression Suite"
            [Suite "regression-net" RegressionNet.runTests]
        "m11-keymgmt" -> runSuiteGroup "UmbraVox M11 Key Management and Impl Bug Attack Suite"
            [Suite "m11-keymgmt" M11KeyMgmt.runTests]
        "m11-symmetric" -> runSuiteGroup "UmbraVox M11 Symmetric Crypto Attack Suite"
            [Suite "m11-symmetric" M11Symmetric.runTests]
        "m11-asymmetric" -> runSuiteGroup "UmbraVox M11 Asymmetric Crypto Attack Suite"
            [Suite "m11-asymmetric" M11Asymmetric.runTests]
        "m11-sidechannel" -> runSuiteGroup "UmbraVox M11 Side-Channel Attack Suite"
            [Suite "m11-sidechannel" M11SideChannel.runTests]
        "m11-protocol" -> runSuiteGroup "UmbraVox M11 Protocol-Level Attack Suite"
            [Suite "m11-protocol" M11Protocol.runTests]
        "m11-high" -> runSuiteGroup "UmbraVox M11 High-Priority Attack Suite"
            [Suite "m11-high" M11High.runTests]
        "m11-high-auth" -> runSuiteGroup "UmbraVox M11 High-Priority Identity and Auth Attack Suite"
            [Suite "m11-high-auth" M11HighAuth.runTests]
        "m11-noise-dh" -> runSuiteGroup "UmbraVox Noise IK DH-Leg Vector and Negative Tests"
            [Suite "m11-noise-dh" M11NoiseDH.runTests]
        "m11-high-fs" -> runSuiteGroup "UmbraVox M11 High-Priority Forward Secrecy and Metadata/Traffic Suite"
            [Suite "m11-high-fs" M11HighFS.runTests]
        "m11-high-proto" -> runSuiteGroup "UmbraVox M11 High-Priority Protocol Attack Suite (batch 2)"
            [Suite "m11-high-proto" M11HighProto.runTests]
        "m11-high-keyimpl" -> runSuiteGroup "UmbraVox M11 High-Priority Key Management and Impl Bug Suite"
            [Suite "m11-high-keyimpl" M11HighKeyImpl.runTests]
        "m11-high-symhash" -> runSuiteGroup "UmbraVox M11 High-Priority Symmetric Crypto and Hash Attack Suite"
            [Suite "m11-high-symhash" M11HighSymHash.runTests]
        "m11-high-pq" -> runSuiteGroup "UmbraVox M11 High-Priority Post-Quantum Attack Suite"
            [Suite "m11-high-pq" M11HighPQ.runTests]
        "m11-high-pqhash" -> runSuiteGroup "UmbraVox M11 High-Priority PQ + Hash Attack Suite"
            [Suite "m11-high-pqhash" M11HighPQHash.runTests]
        "m11-medium" -> runSuiteGroup "UmbraVox M11 Medium-Priority Attack Suite"
            [Suite "m11-medium" M11Medium.runTests]
        "m11-high-sc2" -> runSuiteGroup "UmbraVox M11 High-Priority Side-Channel Attack Suite (batch 2)"
            [Suite "m11-high-sc2" M11HighSC2.runTests]
        "m11-high-as2" -> runSuiteGroup "UmbraVox M11 High-Priority Asymmetric/KM/IB Attack Suite (batch 2)"
            [Suite "m11-high-as2" M11HighAS2.runTests]
        "m11-high-remaining" -> runSuiteGroup "UmbraVox M11 High-Priority Remaining Attack Suite"
            [Suite "m11-high-remaining" M11HighRemaining.runTests]
        "m11-high-smfsia" -> runSuiteGroup "UmbraVox M11 High-Priority State Machine/FS/Metadata/IA Suite"
            [Suite "m11-high-smfsia" M11HighSMFSIA.runTests]
        "m11-high-crypto3" -> runSuiteGroup "UmbraVox M11 High-Priority Crypto/Asymmetric Attack Suite (batch 3)"
            [Suite "m11-high-crypto3" M11HighCrypto3.runTests]
        "m11-high-km3" -> runSuiteGroup "UmbraVox M11 High-Priority Key Management and Impl Bug Suite (batch 3)"
            [Suite "m11-high-km3" M11HighKM3.runTests]
        "m11-final" -> runSuiteGroup "UmbraVox M11 Final Unimplemented Attack Tests"
            [Suite "m11-final" M11Final.runTests]
        "mcdc" -> runSuiteGroup "UmbraVox DO-178C DAL A MC/DC Gap-Fill Suite"
            [Suite "mcdc" MCDC.runTests]
        "integrity" -> runSuiteGroup "UmbraVox Integrity Suite" integritySuites
        "soak" -> runSuiteGroup "UmbraVox Soak Suite" soakSuites
        "differential" -> runSuiteGroup "UmbraVox Differential C vs Haskell Suite"
            [Suite "differential" Differential.runTests]
        "differential-oracle" -> runSuiteGroup "UmbraVox Multi-Oracle Differential Suite"
            [ Suite "differential-primitives" DiffPrimitives.differentialPrimitiveTests
            , Suite "differential-negative" DiffNegative.differentialNegativeTests
            , Suite "differential-metamorphic" DiffMetamorphic.metamorphicTests
            ]
        "unicode" -> runSuiteGroup "UmbraVox Unicode Exhaustive Suite"
            [Suite "unicode" Unicode.runTests]
        "deferred" -> runSuiteGroup "UmbraVox Deferred Stub Suite" deferredSuites
        "all" -> runSuiteGroup "UmbraVox Full Test Matrix"
            (requiredSuites ++ tuiSimSuites ++ integritySuites ++ soakSuites ++ deferredSuites)
        _ ->
            case find ((== suiteArg) . suiteName) allSuites of
                Just suite -> runSuiteGroup ("UmbraVox Suite: " ++ suiteArg) [suite]
                Nothing -> do
                    putStrLn $ "Unknown suite: " ++ suiteArg
                    putStrLn $ "Valid suites: " ++ intercalate ", " validSuiteArgs
                    putStrLn "You can also run any exact [suite] name shown by the grouped runners."
                    pure False

runSuiteGroup :: String -> [Suite] -> IO Bool
runSuiteGroup title suites = do
    putStrLn title
    putStrLn (replicate (length title) '=')
    putStrLn ""
    results <- mapM runNamed suites
    putStrLn ""
    if and results
        then putStrLn "All tests passed." >> pure True
        else putStrLn "SOME TESTS FAILED." >> pure False

runNamed :: Suite -> IO Bool
runNamed suite = do
    putStrLn $ "[suite] " ++ suiteName suite
    ok <- suiteAction suite
    putStrLn ""
    pure ok

coreSuites :: [Suite]
coreSuites =
    [ Suite "sha256" SHA256.runTests
    , Suite "sha512" SHA512.runTests
    , Suite "hmac" HMAC.runTests
    , Suite "hkdf" HKDF.runTests
    , Suite "aes" AES.runTests
    , Suite "gcm" GCM.runTests
    , Suite "curve25519" Curve25519.runTests
    , Suite "ed25519" Ed25519.runTests
    , Suite "chacha20" ChaCha20.runTests
    , Suite "x3dh" X3DH.runTests
    , Suite "double-ratchet" DoubleRatchet.runTests
    , Suite "ratchet-persist" RatchetPersist.runTests
    , Suite "secure-bytes" SecureBytes.runTests
    , Suite "mlkem" MLKEM.runTests
    , Suite "keccak" Keccak.runTests
    , Suite "poly1305" Poly1305.runTests
    , Suite "security" Security.runTests
    , Suite "fuzz" Fuzz.runTests
    , Suite "fuzz-inputs" FuzzInputs.runTests
    , Suite "integration" Integration.runTests
    , Suite "equivalence" Equivalence.runTests
    , Suite "transport-class" TransportClass.runTests
    , Suite "loopback-transport" Loopback.runTests
    , Suite "udp-transport" UDP.runTests
    , Suite "socks5-live" Socks5Live.runTests
    , Suite "cbor" CBOR.runTests
    , Suite "bip39" BIP39.runTests
    , Suite "export" Export.runTests
    , Suite "qrcode" QRCode.runTests
    , Suite "noise" Noise.runTests
    , Suite "chat-session" ChatSession.runTests
    , Suite "stealth-address" StealthAddress.runTests
    , Suite "peer-exchange" PeerExchange.runTests
    , Suite "safety-number" SafetyNumber.runTests
    , Suite "message-format" MessageFormat.runTests
    , Suite "wire-format" WireFormat.runTests
    , Suite "pqxdh" PQXDH.runTests
    , Suite "random" Random.runTests
    , Suite "vrf" VRF.runTests
    , Suite "pq-wrapper" PQWrapper.runTests
    , Suite "sender-keys" SenderKeys.runTests
    , Suite "signal-session" Session.runTests
    , Suite "mdns" MDNS.runTests
    , Suite "network-protocol" Protocol.runTests
    , Suite "chat-message" ChatMessage.runTests
    , Suite "chat-contacts" ChatContacts.runTests
    , Suite "chat-api" ChatAPI.runTests
    , Suite "codegen" Codegen.runTests
    , Suite "tui-types" TUITypes.runTests
    , Suite "tui-render" TUIRender.runTests
    , Suite "tui-input" TUIInput.runTests
    , Suite "tui-handshake" TUIHandshake.runTests
    , Suite "tui-actions" TUIActions.runTests
    , Suite "tui-paths" TUIPaths.runTests
    , Suite "tools-complexity" ToolsComplexity.runTests
    , Suite "tools-fstar-verify" ToolsFStarVerify.runTests
    , Suite "tools-fetch-references" ToolsFetchReferences.runTests
    , Suite "runtime-log" AppRuntimeLog.runTests
    , Suite "differential" Differential.runTests
    , Suite "adversarial" Adversarial.runTests
    , Suite "unicode" Unicode.runTests
    , Suite "regression" Regression.runTests
    , Suite "regression-m7" RegressionM7.runTests
    , Suite "regression-m8" RegressionM8.runTests
    , Suite "regression-net" RegressionNet.runTests
    , Suite "m11-keymgmt" M11KeyMgmt.runTests
    , Suite "m11-symmetric" M11Symmetric.runTests
    , Suite "m11-asymmetric" M11Asymmetric.runTests
    , Suite "m11-sidechannel" M11SideChannel.runTests
    , Suite "m11-protocol" M11Protocol.runTests
    , Suite "m11-high" M11High.runTests
    , Suite "m11-high-auth" M11HighAuth.runTests
    , Suite "m11-noise-dh" M11NoiseDH.runTests
    , Suite "m11-high-fs" M11HighFS.runTests
    , Suite "m11-high-proto" M11HighProto.runTests
    , Suite "m11-high-keyimpl" M11HighKeyImpl.runTests
    , Suite "m11-high-symhash" M11HighSymHash.runTests
    , Suite "m11-high-pq" M11HighPQ.runTests
    , Suite "m11-high-pqhash" M11HighPQHash.runTests
    , Suite "m11-medium" M11Medium.runTests
    , Suite "m11-high-sc2" M11HighSC2.runTests
    , Suite "m11-high-as2" M11HighAS2.runTests
    , Suite "m11-high-remaining" M11HighRemaining.runTests
    , Suite "m11-high-smfsia" M11HighSMFSIA.runTests
    , Suite "m11-high-crypto3" M11HighCrypto3.runTests
    , Suite "m11-high-km3" M11HighKM3.runTests
    , Suite "m11-final" M11Final.runTests
    , Suite "mcdc" MCDC.runTests
    ]

coreCryptoSuites :: [Suite]
coreCryptoSuites =
    [ Suite "sha256" SHA256.runTests
    , Suite "sha512" SHA512.runTests
    , Suite "hmac" HMAC.runTests
    , Suite "hkdf" HKDF.runTests
    , Suite "aes" AES.runTests
    , Suite "gcm" GCM.runTests
    , Suite "curve25519" Curve25519.runTests
    , Suite "ed25519" Ed25519.runTests
    , Suite "chacha20" ChaCha20.runTests
    , Suite "x3dh" X3DH.runTests
    , Suite "double-ratchet" DoubleRatchet.runTests
    , Suite "ratchet-persist" RatchetPersist.runTests
    , Suite "secure-bytes" SecureBytes.runTests
    , Suite "mlkem" MLKEM.runTests
    , Suite "keccak" Keccak.runTests
    , Suite "poly1305" Poly1305.runTests
    , Suite "bip39" BIP39.runTests
    , Suite "export" Export.runTests
    , Suite "pqxdh" PQXDH.runTests
    , Suite "random" Random.runTests
    , Suite "vrf" VRF.runTests
    , Suite "pq-wrapper" PQWrapper.runTests
    , Suite "sender-keys" SenderKeys.runTests
    , Suite "signal-session" Session.runTests
    , Suite "security" Security.runTests
    , Suite "differential" Differential.runTests
    , Suite "m11-symmetric" M11Symmetric.runTests
    , Suite "m11-asymmetric" M11Asymmetric.runTests
    , Suite "mcdc" MCDC.runTests
    ]

coreNetworkSuites :: [Suite]
coreNetworkSuites =
    [ Suite "transport-class" TransportClass.runTests
    , Suite "loopback-transport" Loopback.runTests
    , Suite "noise" Noise.runTests
    , Suite "peer-exchange" PeerExchange.runTests
    , Suite "mdns" MDNS.runTests
    , Suite "network-protocol" Protocol.runTests
    ]

coreChatSuites :: [Suite]
coreChatSuites =
    [ Suite "chat-session" ChatSession.runTests
    , Suite "chat-message" ChatMessage.runTests
    , Suite "chat-contacts" ChatContacts.runTests
    , Suite "chat-api" ChatAPI.runTests
    , Suite "message-format" MessageFormat.runTests
    , Suite "wire-format" WireFormat.runTests
    , Suite "cbor" CBOR.runTests
    , Suite "qrcode" QRCode.runTests
    , Suite "safety-number" SafetyNumber.runTests
    , Suite "integration" Integration.runTests
    , Suite "equivalence" Equivalence.runTests
    ]

coreTUISuites :: [Suite]
coreTUISuites =
    [ Suite "tui-types" TUITypes.runTests
    , Suite "tui-render" TUIRender.runTests
    , Suite "tui-input" TUIInput.runTests
    , Suite "tui-handshake" TUIHandshake.runTests
    , Suite "tui-actions" TUIActions.runTests
    , Suite "tui-paths" TUIPaths.runTests
    ]

coreToolsSuites :: [Suite]
coreToolsSuites =
    [ Suite "tools-complexity" ToolsComplexity.runTests
    , Suite "tools-fstar-verify" ToolsFStarVerify.runTests
    , Suite "tools-fetch-references" ToolsFetchReferences.runTests
    , Suite "codegen" Codegen.runTests
    , Suite "fuzz" Fuzz.runTests
    , Suite "fuzz-inputs" FuzzInputs.runTests
    ]

tcpSuites :: [Suite]
tcpSuites =
    [ Suite "transport" Transport.runTests
    , Suite "hardening-tcp" HardeningTCP.runTests
    ]

faultSuites :: [Suite]
faultSuites =
    [ Suite "fuzz-connection" FuzzConnection.runTests
    , Suite "transport-edge" TransportEdge.runTests
    , Suite "tui-handshake-edge" TUIHandshakeEdge.runTests
    , Suite "hardening-fault" HardeningFault.runTests
    ]

recoverySuites :: [Suite]
recoverySuites =
    [ Suite "app-startup" AppStartup.runTests
    , Suite "keystore" KeyStore.runTests
    , Suite "anthony" Anthony.runTests
    , Suite "storage-encryption" StorageEncryption.runTests
    , Suite "plugin-registry" PluginRegistry.runTests
    , Suite "ephemeral-integration" EphemeralIntegration.runTests
    , Suite "hardening-recovery" HardeningRecovery.runTests
    ]

soakSuites :: [Suite]
soakSuites =
    [ Suite "end-to-end" EndToEnd.runTests
    , Suite "end-to-end2" EndToEnd2.runTests
    , Suite "hardening-soak" HardeningSoak.runTests
    ]

tuiSimSuites :: [Suite]
tuiSimSuites =
    [ Suite "tui-sim-chat" TUISimChat.runTests
    , Suite "tui-sim-contacts" TUISimContacts.runTests
    , Suite "tui-sim-dialogs" TUISimDialogs.runTests
    , Suite "tui-sim-focus" TUISimFocus.runTests
    , Suite "tui-sim-menu" TUISimMenu.runTests
    , Suite "tui-sim-message-flow" TUISimMessageFlow.runTests
    , Suite "tui-sim-runtime" TUISimRuntime.runTests
    , Suite "tui-sim-settings-edge" TUISimSettingsEdge.runTests
    , Suite "tui-sim-shortcuts" TUISimShortcuts.runTests
    , Suite "tui-sim-workflows" TUISimWorkflows.runTests
    ]

integritySuites :: [Suite]
integritySuites =
    [ Suite "chat-wire-edge" ChatWireEdge.runTests
    , Suite "crypto-integrity" CryptoIntegrity.runTests
    ]

deferredSuites :: [Suite]
deferredSuites =
    [ Suite "chat-transaction" ChatTransaction.runTests
    , Suite "storage-chaindb" ChainDB.runTests
    , Suite "storage-statedb" StateDB.runTests
    , Suite "storage-index" StorageIndex.runTests
    , Suite "storage-checkpoint" Checkpoint.runTests
    , Suite "consensus-types" ConsensusTypes.runTests
    , Suite "consensus-block" ConsensusBlock.runTests
    , Suite "consensus-ledger" ConsensusLedger.runTests
    , Suite "consensus-mempool" ConsensusMempool.runTests
    , Suite "consensus-validation" ConsensusValidation.runTests
    , Suite "consensus-fork-choice" ConsensusForkChoice.runTests
    , Suite "consensus-nonce" ConsensusNonce.runTests
    , Suite "consensus-leader-election" ConsensusLeaderElection.runTests
    , Suite "consensus-protocol" ConsensusProtocol.runTests
    , Suite "consensus-truncation" ConsensusTruncation.runTests
    , Suite "economics-token" EconToken.runTests
    , Suite "economics-fees" EconFees.runTests
    , Suite "economics-rewards" EconRewards.runTests
    , Suite "economics-penalty" EconPenalty.runTests
    , Suite "economics-cycle" EconCycle.runTests
    , Suite "economics-onboarding" EconOnboarding.runTests
    ]

requiredSuites :: [Suite]
requiredSuites = coreSuites ++ tcpSuites ++ faultSuites ++ recoverySuites ++ integritySuites

allSuites :: [Suite]
allSuites =
    coreSuites ++ tcpSuites ++ faultSuites ++ recoverySuites ++
    tuiSimSuites ++ integritySuites ++ soakSuites ++ deferredSuites

validSuiteArgs :: [String]
validSuiteArgs =
    [ "required", "core", "core-crypto", "core-network", "core-chat"
    , "core-tui", "core-tools", "tcp", "fault", "recovery", "tui-sim"
    , "integrity", "soak", "deferred", "differential", "adversarial", "unicode"
    , "regression", "regression-m7", "regression-m8", "regression-net"
    , "m11-keymgmt", "m11-symmetric", "m11-asymmetric", "m11-sidechannel"
    , "m11-protocol", "m11-high", "m11-high-auth", "m11-noise-dh", "m11-high-fs", "m11-high-proto"
    , "m11-high-keyimpl", "m11-high-symhash", "m11-high-pq", "m11-high-pqhash", "m11-medium", "m11-high-sc2"
    , "m11-high-as2", "m11-high-remaining", "m11-high-smfsia", "m11-high-crypto3"
    , "m11-high-km3", "m11-final", "mcdc", "all"
    ]
