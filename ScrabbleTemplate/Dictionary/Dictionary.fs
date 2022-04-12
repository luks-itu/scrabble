module Dictionary

type Dictionary =
    | D of Map<char, Dictionary> * bool

let empty (u : unit) : Dictionary = D(Map.empty , false)

let rec insert (s: string) (D(map, endOfWord)) : Dictionary =
  match s with
    | "" -> D(map, true)
    | _  -> let currentC = s.[0]
            let c = Map.tryFind currentC map
            match c with
            | None       -> D(Map.add currentC ((empty ()) |> insert s.[1..]) map , endOfWord)
            | Some cDict -> D(Map.add currentC (insert s.[1..] cDict) map , endOfWord)

let rec lookup (s: string) (D(map, endOfWord)) : bool =
    match s with
    | "" -> endOfWord
    | _  -> let c = Map.tryFind s.[0] map
            match c with
            | None       -> false
            | Some cDict -> lookup s.[1..] cDict
          
let step (c: char) (D(map, endOfWord)) : (bool * Dictionary) option =
    match Map.tryFind c map with
    | None -> None
    | Some (D(_, b) as dict) -> Some (b, dict)