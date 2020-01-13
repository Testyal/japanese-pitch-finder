
module Dictionary.Search where 

import qualified Data.Vector as V
import Dictionary.Types

-- Filter for Dictionary -----------------------------------------------------------------------------------------------

{-| Filters a dictionary by its Japanese entries with a given query, returning the filtered dictionary. -}
filterEntries :: Dictionary -> String -> Dictionary
filterEntries dict query = V.filter (\entry -> japanese entry == query) dict

------------------------------------------------------------------------------------------------------------------------
