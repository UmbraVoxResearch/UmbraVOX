module Main (main) where

import System.Exit (exitFailure, exitSuccess)
import qualified Test.Crypto.SHA256 as SHA256
import qualified Test.Crypto.SHA512 as SHA512
import qualified Test.Crypto.HMAC as HMAC
import qualified Test.Crypto.HKDF as HKDF
import qualified Test.Crypto.AES as AES
import qualified Test.Crypto.GCM as GCM
import qualified Test.Crypto.Curve25519 as Curve25519
import qualified Test.Crypto.Ed25519 as Ed25519
import qualified Test.Crypto.ChaCha20 as ChaCha20
import qualified Test.Crypto.Signal.X3DH as X3DH
import qualified Test.Crypto.Signal.DoubleRatchet as DoubleRatchet
import qualified Test.Crypto.Keccak as Keccak
import qualified Test.Crypto.MLKEM as MLKEM
import qualified Test.Crypto.Poly1305 as Poly1305
import qualified Test.Security as Security
import qualified Test.Fuzz as Fuzz
import qualified Test.Integration as Integration
import qualified Test.Equivalence as Equivalence
import qualified Test.Network.Transport as Transport
import qualified Test.Network.TransportClass as TransportClass
import qualified Test.Network.Transport.Loopback as Loopback
import qualified Test.Network.TransportEdge as TransportEdge
import qualified Test.Protocol.CBOR as CBOR
import qualified Test.Crypto.BIP39 as BIP39
import qualified Test.Crypto.Export as Export
import qualified Test.Protocol.QRCode as QRCode
import qualified Test.Network.Noise as Noise
import qualified Test.Chat.Session as ChatSession
import qualified Test.Crypto.StealthAddress as StealthAddress
import qualified Test.Network.PeerExchange as PeerExchange
import qualified Test.Protocol.SafetyNumber as SafetyNumber
import qualified Test.Protocol.MessageFormat as MessageFormat
import qualified Test.Protocol.WireFormat as WireFormat
import qualified Test.Crypto.PQXDH as PQXDH
import qualified Test.Crypto.Random as Random
import qualified Test.Crypto.VRF as VRF
import qualified Test.Crypto.PQWrapper as PQWrapper
import qualified Test.Crypto.KeyStore as KeyStore
import qualified Test.Crypto.Signal.SenderKeys as SenderKeys
import qualified Test.Crypto.Signal.Session as Session
import qualified Test.Storage.Anthony as Anthony
import qualified Test.Storage.ChainDB as ChainDB
import qualified Test.Storage.StateDB as StateDB
import qualified Test.Storage.Index as StorageIndex
import qualified Test.Storage.Checkpoint as Checkpoint
import qualified Test.Network.MDNS as MDNS
import qualified Test.Network.Protocol as Protocol
import qualified Test.Chat.Message as ChatMessage
import qualified Test.Chat.Transaction as ChatTransaction
import qualified Test.Chat.Contacts as ChatContacts
import qualified Test.Chat.API as ChatAPI
import qualified Test.Codegen as Codegen
import qualified Test.TUI.Types as TUITypes
import qualified Test.TUI.Render as TUIRender
import qualified Test.TUI.Input as TUIInput
import qualified Test.TUI.Handshake as TUIHandshake
import qualified Test.TUI.HandshakeEdge as TUIHandshakeEdge
import qualified Test.TUI.Actions as TUIActions
import qualified Test.TUI.Paths as TUIPaths
import qualified Test.Tools.Complexity as ToolsComplexity
import qualified Test.Tools.FStarVerify as ToolsFStarVerify
import qualified Test.Tools.FetchReferences as ToolsFetchReferences
import qualified Test.Consensus.Types as ConsensusTypes
import qualified Test.Consensus.Block as ConsensusBlock
import qualified Test.Consensus.Ledger as ConsensusLedger
import qualified Test.Consensus.Mempool as ConsensusMempool
import qualified Test.Consensus.Validation as ConsensusValidation
import qualified Test.Consensus.ForkChoice as ConsensusForkChoice
import qualified Test.Consensus.Nonce as ConsensusNonce
import qualified Test.Consensus.LeaderElection as ConsensusLeaderElection
import qualified Test.Consensus.Protocol as ConsensusProtocol
import qualified Test.Consensus.Truncation as ConsensusTruncation
import qualified Test.Economics.Token as EconToken
import qualified Test.Economics.Fees as EconFees
import qualified Test.Economics.Rewards as EconRewards
import qualified Test.Economics.Penalty as EconPenalty
import qualified Test.Economics.Cycle as EconCycle
import qualified Test.Economics.Onboarding as EconOnboarding
import qualified Test.EndToEnd as EndToEnd
import qualified Test.EndToEnd2 as EndToEnd2

main :: IO ()
main = do
    putStrLn "UmbraVox Test Suite"
    putStrLn "==================="
    putStrLn ""

    sha256Pass <- SHA256.runTests
    putStrLn ""
    sha512Pass <- SHA512.runTests
    putStrLn ""
    hmacPass <- HMAC.runTests
    putStrLn ""
    hkdfPass <- HKDF.runTests
    putStrLn ""
    aesPass <- AES.runTests
    putStrLn ""
    gcmPass <- GCM.runTests
    putStrLn ""
    curve25519Pass <- Curve25519.runTests
    putStrLn ""
    ed25519Pass <- Ed25519.runTests
    putStrLn ""
    chacha20Pass <- ChaCha20.runTests
    putStrLn ""
    x3dhPass <- X3DH.runTests
    putStrLn ""
    doubleRatchetPass <- DoubleRatchet.runTests
    putStrLn ""
    mlkemPass <- MLKEM.runTests
    putStrLn ""
    keccakPass <- Keccak.runTests
    putStrLn ""
    poly1305Pass <- Poly1305.runTests
    putStrLn ""
    securityPass <- Security.runTests
    putStrLn ""
    fuzzPass <- Fuzz.runTests
    putStrLn ""
    integrationPass <- Integration.runTests
    putStrLn ""
    equivalencePass <- Equivalence.runTests
    putStrLn ""
    transportPass <- Transport.runTests
    putStrLn ""
    transportClassPass <- TransportClass.runTests
    putStrLn ""
    loopbackPass <- Loopback.runTests
    putStrLn ""
    transportEdgePass <- TransportEdge.runTests
    putStrLn ""
    cborPass <- CBOR.runTests
    putStrLn ""
    bip39Pass <- BIP39.runTests
    putStrLn ""
    exportPass <- Export.runTests
    putStrLn ""
    qrcodePass <- QRCode.runTests
    putStrLn ""
    noisePass <- Noise.runTests
    putStrLn ""
    chatSessionPass <- ChatSession.runTests
    putStrLn ""
    stealthPass <- StealthAddress.runTests
    putStrLn ""
    pexPass <- PeerExchange.runTests
    putStrLn ""
    safetyNumPass <- SafetyNumber.runTests
    putStrLn ""
    msgFmtPass <- MessageFormat.runTests
    putStrLn ""
    wireFmtPass <- WireFormat.runTests
    putStrLn ""
    pqxdhPass <- PQXDH.runTests
    putStrLn ""
    randomPass <- Random.runTests
    putStrLn ""
    vrfPass <- VRF.runTests
    putStrLn ""
    pqWrapperPass <- PQWrapper.runTests
    putStrLn ""
    keyStorePass <- KeyStore.runTests
    putStrLn ""
    senderKeysPass <- SenderKeys.runTests
    putStrLn ""
    sessionPass <- Session.runTests
    putStrLn ""
    anthonyPass <- Anthony.runTests
    putStrLn ""
    chainDBPass <- ChainDB.runTests
    putStrLn ""
    stateDBPass <- StateDB.runTests
    putStrLn ""
    storageIndexPass <- StorageIndex.runTests
    putStrLn ""
    checkpointPass <- Checkpoint.runTests
    putStrLn ""
    mdnsPass <- MDNS.runTests
    putStrLn ""
    protocolPass <- Protocol.runTests
    putStrLn ""
    chatMsgPass <- ChatMessage.runTests
    putStrLn ""
    chatTxPass <- ChatTransaction.runTests
    putStrLn ""
    chatContactsPass <- ChatContacts.runTests
    putStrLn ""
    chatAPIPass <- ChatAPI.runTests
    putStrLn ""
    codegenPass <- Codegen.runTests
    putStrLn ""
    tuiTypesPass <- TUITypes.runTests
    putStrLn ""
    tuiRenderPass <- TUIRender.runTests
    putStrLn ""
    tuiInputPass <- TUIInput.runTests
    putStrLn ""
    tuiHandshakePass <- TUIHandshake.runTests
    putStrLn ""
    tuiHandshakeEdgePass <- TUIHandshakeEdge.runTests
    putStrLn ""
    tuiActionsPass <- TUIActions.runTests
    putStrLn ""
    tuiPathsPass <- TUIPaths.runTests
    putStrLn ""
    toolsComplexityPass <- ToolsComplexity.runTests
    putStrLn ""
    toolsFStarVerifyPass <- ToolsFStarVerify.runTests
    putStrLn ""
    toolsFetchReferencesPass <- ToolsFetchReferences.runTests
    putStrLn ""
    consensusTypesPass <- ConsensusTypes.runTests
    putStrLn ""
    consensusBlockPass <- ConsensusBlock.runTests
    putStrLn ""
    consensusLedgerPass <- ConsensusLedger.runTests
    putStrLn ""
    consensusMempoolPass <- ConsensusMempool.runTests
    putStrLn ""
    consensusValidationPass <- ConsensusValidation.runTests
    putStrLn ""
    consensusForkChoicePass <- ConsensusForkChoice.runTests
    putStrLn ""
    consensusNoncePass <- ConsensusNonce.runTests
    putStrLn ""
    consensusLeaderElectionPass <- ConsensusLeaderElection.runTests
    putStrLn ""
    consensusProtocolPass <- ConsensusProtocol.runTests
    putStrLn ""
    consensusTruncationPass <- ConsensusTruncation.runTests
    putStrLn ""
    econTokenPass <- EconToken.runTests
    putStrLn ""
    econFeesPass <- EconFees.runTests
    putStrLn ""
    econRewardsPass <- EconRewards.runTests
    putStrLn ""
    econPenaltyPass <- EconPenalty.runTests
    putStrLn ""
    econCyclePass <- EconCycle.runTests
    putStrLn ""
    econOnboardingPass <- EconOnboarding.runTests
    putStrLn ""
    endToEndPass <- EndToEnd.runTests
    putStrLn ""
    endToEnd2Pass <- EndToEnd2.runTests

    putStrLn ""
    let allPass = sha256Pass && sha512Pass && hmacPass && hkdfPass && aesPass && gcmPass && curve25519Pass && ed25519Pass && chacha20Pass && x3dhPass && doubleRatchetPass && mlkemPass && keccakPass && poly1305Pass && securityPass && fuzzPass && integrationPass && equivalencePass && transportPass && transportClassPass && loopbackPass && transportEdgePass && cborPass && bip39Pass && exportPass && qrcodePass && noisePass && chatSessionPass && stealthPass && pexPass && safetyNumPass && msgFmtPass && wireFmtPass && pqxdhPass && randomPass && vrfPass && pqWrapperPass && keyStorePass && senderKeysPass && sessionPass && anthonyPass && chainDBPass && stateDBPass && storageIndexPass && checkpointPass && mdnsPass && protocolPass && chatMsgPass && chatTxPass && chatContactsPass && chatAPIPass && codegenPass && tuiTypesPass && tuiRenderPass && tuiInputPass && tuiHandshakePass && tuiHandshakeEdgePass && tuiActionsPass && tuiPathsPass && toolsComplexityPass && toolsFStarVerifyPass && toolsFetchReferencesPass && consensusTypesPass && consensusBlockPass && consensusLedgerPass && consensusMempoolPass && consensusValidationPass && consensusForkChoicePass && consensusNoncePass && consensusLeaderElectionPass && consensusProtocolPass && consensusTruncationPass && econTokenPass && econFeesPass && econRewardsPass && econPenaltyPass && econCyclePass && econOnboardingPass && endToEndPass && endToEnd2Pass
    if allPass
        then do
            putStrLn "All tests passed."
            exitSuccess
        else do
            putStrLn "SOME TESTS FAILED."
            exitFailure
