module Gnut.Plugin.Perlis
    ( plugin
    ) where

import Data.Text (Text)
import qualified Data.Text as T
import Control.Monad.Trans (liftIO)

import Gnut.IRC
import Gnut.MsgCommands
import Gnut.Util

plugin :: NotYetPlugin
plugin = makePlugin "Perlis" [quoteHook]

quoteHook :: Irc ()
quoteHook = onMsgCommand "^perlis" $ do
    idx   <- getMsgCommandText
    if T.null idx then do
        quote <- liftIO $ randomElement quotes
        noticeReply quote
    else do
        let idxnum = read $ T.unpack idx
        noticeReply (quotes !! (idxnum-1))

quotes :: [Text]
quotes =
    [ "We toast the Lisp programmer who pens his thoughts within nests of parentheses. — Alan Perlis"
    , "Both knowledge and wisdom extend man's reach. Knowledge led to computers, wisdom to chopsticks. — Alan Perlis"
    , "There is an appreciated substance to the phrase 'ALGOL-like' which is often used in arguments about programming, languages and computation. ALGOL appears to be a durable model, and even flourishes under surgery — be it explorative, plastic, or amputative. — Alan Perlis"
    , "The vision we have of conversational programming takes in much more than rapid turn around time and convenient debugging aids: our most interesting programs are never wrong and never final. [...] What is new is the requirement to make variable in our languages what we had previously taken as fixed. I do not refer to new data classes now, but to variables whose values are programs or parts of programs, syntax or parts of syntax, and regimes of control. — Alan Perlis"
    , "This language [LISP] induces humorous arguments among programmers, often being damned and praised for the same feature. — Alan Perlis"
    , "Programmers should never be satisfied with languages which permit them to program everything, but to program nothing of interest easily. — Alan Perlis"
    , "Computer science is a restless infant and its progress depends as much on shifts in point of view as on the orderly development of our current concepts. — Alan Perlis"
    , "One man's constant is another man's variable."
    , "Syntactic sugar causes cancer of the semi-colons."
    , "A programming language is low level when its programs require attention to the irrelevant. — Alan Perlis"
    , "If you have a procedure with 10 parameters, you probably missed some. — Alan Perlis"
    , "Every program has (at least) two purposes: the one for which it was written and another for which it wasn't. — Alan Perlis"
    , "A language that doesn't affect the way you think about programming, is not worth knowing. — Alan Perlis"
    , "Simplicity does not precede complexity, but follows it. — Alan Perlis"
    , "A picture is worth 10K words - but only those to describe the picture. Hardly any sets of 10K words can be adequately described with pictures. — Alan Perlis"
    , "Some programming languages manage to absorb change, but withstand progress. — Alan Perlis"
    , "You can measure a programmer's perspective by noting his attitude on the continuing vitality of FORTRAN. — Alan Perlis"
    , "LISP programmers know the value of everything and the cost of nothing. — Alan Perlis"
    , "It is easier to change the specification to fit the program than vice versa. — Alan Perlis"
    , "Fools ignore complexity. Pragmatists suffer it. Some can avoid it. Geniuses remove it. — Alan Perlis"
    , "In English every word can be verbed. Would that it were so in our programming languages. — Alan Perlis"
    , "Often it is means that justify ends: Goals advance technique and technique survives even when goal structures crumble. — Alan Perlis"
    , "The computing field is always in need of new cliches: Banality sooths our nerves. — Alan Perlis"
    , "A year spent in artificial intelligence is enough to make one believe in God. — Alan Perlis"
    , "Prolonged contact with the computer turns mathematicians into clerks and vice versa. — Alan Perlis"
    , "Don't have good ideas if you aren't willing to be responsible for them. — Alan Perlis"
    , "Dealing with failure is easy: Work hard to improve. Success is also easy to handle: You've solved the wrong problem. Work hard to improve. — Alan Perlis"
    , "You think you know when you learn, are more sure when you can write, even more when you can teach, but certain when you can program. — Alan Perlis"
    , "It goes against the grain of modern education to teach students to program. What fun is there to making plans, acquiring discipline in organizing thoughts, devoting attention to detail and, learning to be self-critical? — Alan Perlis"
    ]
