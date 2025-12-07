namespace Aoc

module Dict =
    open System.Collections.Generic

    let get_or (dict: Dictionary<'k, 'v>) key f =
        if dict.ContainsKey key then
            dict[key]
        else
            let result = f ()
            dict[key] <- result
            result
