module Gnut.Error
    ( errors
    , autherrors
    , randomError
    , randomAuthError
    ) where

import Data.Text (Text)
import System.Random (randomRIO)

import Gnut.Util

-- General - randomized - error messages
errors :: [Text]
errors =
    [ "CPU#0: Possible thermal failure (Gnut on fire?)"
    , "Error: Please insert more coins"
    , "Error: The stupidity. IT BUUUUUUUURNS"
    , "Oh fuck it. I give up. (I wish I could core dump.)"
    , "Go play in a void *"
    , "Could you do us a favour and dereference pointer 0x000000000? Much appreciated."
    , "You have to do that yourself, just run `dd if=/dev/null of=/dev/mem` as root."
    , "My /dev/urandom makes more sense than whatever you just wrote."
    , "It appears that you are a monoid in the category of Idiots."
    , "Did you use the wrong decryption key for your brain this morning or why are you only typing bullshit?"
    , "Let me guess, you're an Emacs user."
    , "Error: Can not fulfill request. (0x7a6d6269 - Datacenter infested by Zombies)"
    , "That's not how this works. That's not how any of this works."
    , "This is the 1000th Error! Contact D\x200b\&ean for your prize!"
    ]

-- Not-authorized messages
autherrors :: [Text]
autherrors =
    [ "I'm sorry Dave. I can't do that."
    , "You're #542523 in queue for this command. Est. waiting time: 125.3491 years"
    , "I'll get to it. Eventually. Maybe."
    , "You can't tell me what to do."
    , "How about No?"
    , "No. (❀◕‿◕)"
    , "You have to do that yourself, just run `dd if=/dev/null of=/dev/sda` as root."
    , "Diamonds are forever. Humans like you on the contrary die within a century."
    ]

randomError :: IO Text
randomError = randomElement errors

randomAuthError :: IO Text
randomAuthError = randomElement autherrors
