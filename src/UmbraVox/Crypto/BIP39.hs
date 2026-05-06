-- | BIP39 passphrase generation for encrypted exports.
--
-- Uses the first 512 words of the BIP39 English wordlist (9 bits per word).
-- A 6-word passphrase provides 54 bits of entropy from word selection.
module UmbraVox.Crypto.BIP39
    ( generatePassphrase
    , bip39Words
    ) where

import Data.Bits (shiftL, shiftR, (.|.), (.&.))
import qualified Data.ByteString as BS

import UmbraVox.Crypto.Random (randomBytes)

-- | Generate an n-word passphrase from the BIP39 wordlist.
-- Each word is selected uniformly from the 512-word list using
-- 2 random bytes per word (mod 512 for uniform distribution).
generatePassphrase :: Int -> IO String
generatePassphrase n
    | n <= 0    = pure ""
    | otherwise = do
        entropy <- randomBytes (n * 2)
        let indices = [ fromIntegral (getW16 entropy (i * 2)) `mod` wordCount
                      | i <- [0 .. n - 1] ]
            words'  = map (bip39Words !!) indices
        pure (unwords words')
  where
    wordCount :: Int
    wordCount = length bip39Words

    getW16 :: BS.ByteString -> Int -> Int
    getW16 bs off =
        (fromIntegral (BS.index bs off) `shiftL` 8)
        .|. fromIntegral (BS.index bs (off + 1))

-- | First 512 words of the BIP39 English wordlist (9 bits per word).
bip39Words :: [String]
bip39Words =
    [ "abandon", "ability", "able", "about", "above", "absent", "absorb", "abstract"
    , "absurd", "abuse", "access", "accident", "account", "accuse", "achieve", "acid"
    , "acoustic", "acquire", "across", "act", "action", "actor", "actress", "actual"
    , "adapt", "add", "addict", "address", "adjust", "admit", "adult", "advance"
    , "advice", "aerobic", "affair", "afford", "afraid", "again", "age", "agent"
    , "agree", "ahead", "aim", "air", "airport", "aisle", "alarm", "album"
    , "alcohol", "alert", "alien", "all", "alley", "allow", "almost", "alone"
    , "alpha", "already", "also", "alter", "always", "amateur", "amazing", "among"
    , "amount", "amused", "analyst", "anchor", "ancient", "anger", "angle", "angry"
    , "animal", "ankle", "announce", "annual", "another", "answer", "antenna", "antique"
    , "anxiety", "any", "apart", "apology", "appear", "apple", "approve", "april"
    , "arch", "arctic", "area", "arena", "argue", "arm", "armed", "armor"
    , "army", "around", "arrange", "arrest", "arrive", "arrow", "art", "artefact"
    , "artist", "artwork", "ask", "aspect", "assault", "asset", "assist", "assume"
    , "asthma", "athlete", "atom", "attack", "attend", "attitude", "attract", "auction"
    , "audit", "august", "aunt", "author", "auto", "autumn", "average", "avocado"
    , "avoid", "awake", "aware", "awesome", "awful", "awkward", "axis", "baby"
    , "bachelor", "bacon", "badge", "bag", "balance", "balcony", "ball", "bamboo"
    , "banana", "banner", "bar", "barely", "bargain", "barrel", "base", "basic"
    , "basket", "battle", "beach", "bean", "beauty", "because", "become", "beef"
    , "before", "begin", "behave", "behind", "believe", "below", "belt", "bench"
    , "benefit", "best", "betray", "better", "between", "beyond", "bicycle", "bid"
    , "bike", "bind", "biology", "bird", "birth", "bitter", "black", "blade"
    , "blame", "blanket", "blast", "bleak", "bless", "blind", "blood", "blossom"
    , "blow", "blue", "blur", "blush", "board", "boat", "body", "boil"
    , "bomb", "bone", "bonus", "book", "boost", "border", "boring", "borrow"
    , "boss", "bottom", "bounce", "box", "boy", "bracket", "brain", "brand"
    , "brass", "brave", "bread", "breeze", "brick", "bridge", "brief", "bright"
    , "bring", "brisk", "broccoli", "broken", "bronze", "broom", "brother", "brown"
    , "brush", "bubble", "buddy", "budget", "buffalo", "build", "bulb", "bulk"
    , "bullet", "bundle", "bunny", "burden", "burger", "burst", "bus", "business"
    , "busy", "butter", "buyer", "buzz", "cabbage", "cabin", "cable", "cactus"
    , "cage", "cake", "call", "calm", "camera", "camp", "can", "canal"
    , "cancel", "candy", "cannon", "canoe", "canvas", "canyon", "capable", "capital"
    , "captain", "car", "carbon", "card", "cargo", "carpet", "carry", "cart"
    , "case", "cash", "casino", "castle", "casual", "cat", "catalog", "catch"
    , "category", "cattle", "caught", "cause", "caution", "cave", "ceiling", "celery"
    , "cement", "census", "century", "cereal", "certain", "chair", "chalk", "champion"
    , "change", "chaos", "chapter", "charge", "chase", "cheap", "check", "cheese"
    , "chef", "cherry", "chest", "chicken", "chief", "child", "chimney", "choice"
    , "choose", "chronic", "chuckle", "chunk", "churn", "citizen", "city", "civil"
    , "claim", "clap", "clarify", "claw", "clay", "clean", "clerk", "clever"
    , "click", "client", "cliff", "climb", "clinic", "clip", "clock", "clog"
    , "close", "cloth", "cloud", "clown", "club", "clump", "cluster", "clutch"
    , "coach", "coast", "coconut", "code", "coffee", "coil", "coin", "collect"
    , "color", "column", "combine", "come", "comfort", "comic", "common", "company"
    , "concert", "conduct", "confirm", "congress", "connect", "consider", "control", "convince"
    , "cook", "cool", "copper", "copy", "coral", "core", "corn", "correct"
    , "cost", "cotton", "couch", "country", "couple", "course", "cousin", "cover"
    , "coyote", "crack", "cradle", "craft", "cram", "crane", "crash", "crater"
    , "crawl", "crazy", "cream", "credit", "creek", "crew", "cricket", "crime"
    , "crisp", "critic", "crop", "cross", "crouch", "crowd", "crucial", "cruel"
    , "cruise", "crumble", "crush", "cry", "crystal", "cube", "culture", "cup"
    , "cupboard", "curious", "current", "curtain", "curve", "cushion", "custom", "cute"
    , "cycle", "dad", "damage", "damp", "dance", "danger", "daring", "dash"
    , "daughter", "dawn", "day", "deal", "debate", "debris", "decade", "december"
    , "decide", "decline", "decorate", "decrease", "deer", "defense", "define", "defy"
    , "degree", "delay", "deliver", "demand", "demise", "denial", "dentist", "deny"
    , "depart", "depend", "deposit", "depth", "deputy", "derive", "describe", "desert"
    , "design", "desk", "despair", "destroy", "detail", "detect", "develop", "device"
    , "devote", "diagram", "dial", "diamond", "diary", "dice", "diesel", "diet"
    , "differ", "digital", "dignity", "dilemma", "dinner", "dinosaur", "direct", "dirt"
    , "disagree", "discover", "disease", "dish", "dismiss", "disorder", "display", "distance"
    , "divert", "divide", "divorce", "dizzy", "doctor", "document", "dog", "doll"
    , "dolphin", "domain", "donate", "donkey", "donor", "door", "dose", "double"
    , "dove", "draft", "dragon", "drama", "drastic", "draw", "dream", "dress"
    , "drift", "drill", "drink", "drip", "drive", "drop", "drum", "dry"
    , "duck", "dumb", "dune", "during", "dust", "dutch", "duty", "dwarf"
    ]
