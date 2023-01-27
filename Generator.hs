import           Hopcroft (minimize)
import           NFAToDFA (convertNFAToDFA)
import           Types    (AcceptingStates, DFA, NFA)

generate :: (NFA, AcceptingStates) -> (DFA, AcceptingStates)
generate = minimize . convertNFAToDFA
