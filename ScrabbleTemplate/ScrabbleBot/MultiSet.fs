module internal MultiSet
    type MultiSet<'a> when 'a : comparison = MS of Map<'a, uint32>

    let empty = MS Map.empty

    let isEmpty (MS s) =
        Map.isEmpty s

    let size (MS ms) =
        Map.fold(fun s a n -> s + n) 0u ms

    let contains a (MS s) =
        Map.containsKey(a) s

    let numItems a (MS s) =
        Map.tryFind a s |>
            function
            | None -> 0u
            | Some n -> n

    let add a n (MS s) =
        MS (Map.add a ((numItems a (MS s)) + n) s)

    let addSingle a (MS s) =
        MS (Map.add a (numItems a (MS s) + 1u) s)

    let remove a n (MS s) =
        if numItems a (MS s) > n then
            MS (Map.add a (numItems a (MS s) - n) s)
        else
            MS (Map.remove a s)

    let removeSingle a (MS s) =
        if (numItems a (MS s)) > 0u then
            (remove a 1u (MS s))
        else
            MS s

    let fold f acc (MS s) =
        Map.fold f acc s

    let foldBack f (MS s) acc =
        Map.foldBack f s acc

    let ofList lst =
        List.fold(fun s a -> (addSingle a s)) empty lst

    let toList s =
        fold (fun lst a n -> lst @ List.init (int n) (fun _ -> a)) List.Empty s

    let map f s =
        fold (fun b a n -> add (f a) n b) empty s

    let union s1 s2 =
        let (MS unionSet) = empty
        Map.fold(fun s a _ -> add a (max (numItems a s1) (numItems a s2)) s) empty unionSet

    let sum s1 s2 =
        let (MS sumSet) = empty
        Map.fold(fun s a _ -> add a ((numItems a s1) + (numItems a s2)) s) empty sumSet

    let subtract s1 s2 =
        let (MS subtractSet) = empty
        Map.fold(fun s a _ -> add a ((numItems a s1) - (numItems a s2)) s) empty subtractSet

    let intersection s1 s2 =
        let (MS interSet) = empty
        Map.fold(fun s a _ -> add a (min (numItems a s1) (numItems a s2)) s) empty interSet


