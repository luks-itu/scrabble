namespace FSkarp

open ScrabbleUtil
open ScrabbleUtil.ServerCommunication

open System.IO

open ScrabbleUtil.DebugPrint

// The RegEx module is only used to parse human input. It is not used for the final product.

module RegEx =
    open System.Text.RegularExpressions

    let (|Regex|_|) pattern input =
        let m = Regex.Match(input, pattern)
        if m.Success then Some(List.tail [ for g in m.Groups -> g.Value ])
        else None

    let parseMove ts =
        let pattern = @"([-]?[0-9]+[ ])([-]?[0-9]+[ ])([0-9]+)([A-Z]{1})([0-9]+)[ ]?" 
        Regex.Matches(ts, pattern) |>
        Seq.cast<Match> |> 
        Seq.map 
            (fun t -> 
                match t.Value with
                | Regex pattern [x; y; id; c; p] ->
                    ((x |> int, y |> int), (id |> uint32, (c |> char, p |> int)))
                | _ -> failwith "Failed (should never happen)") |>
        Seq.toList

 module Print =

    let printHand pieces hand =
        hand |>
        MultiSet.fold (fun _ x i -> forcePrint (sprintf "%d -> (%A, %d)\n" x (Map.find x pieces) i)) ()

module State = 
    // Make sure to keep your state localised in this module. It makes your life a whole lot easier.
    // Currently, it only keeps track of your hand, your player numer, your board, and your dictionary,
    // but it could, potentially, keep track of other useful
    // information, such as number of players, player turn, etc.

    //for currentBoard
    type pieces =  (char*int)


    type state = {
        board         : Parser.board
        dict          : ScrabbleUtil.Dictionary.Dict //ScrabbleUtil.Dictionary.Dict  <--- if you need it back
        playerNumber  : uint32
        hand          : MultiSet.MultiSet<uint32>
        numPlayer     : uint32
        playerTurn    : uint32
        currentBoard  : Map<coord,pieces>
        tiles         : Map<uint32, tile> //We assume tiles in the lookup table
    }

    let mkState b d pn h n t l =
        {board = b; dict = d;  playerNumber = pn; hand = h; numPlayer = n; playerTurn = t; currentBoard = Map.empty; tiles = l }

    let board st         = st.board
    let dict st          = st.dict
    let playerNumber st  = st.playerNumber
    let hand st          = st.hand
    let numPlayer st     = st.numPlayer 
    let playerTurn st    = st.playerTurn
    let currentBoard st  = st.currentBoard
    let tiles st         = st.tiles
    

module Scrabble =
    open System.Threading
    open MultiSet
    type pieces =  (char*int)
(*
    let wrapperVertical (input) (st:State.state) =
        match input with 
        |Some x -> Some x
        |None ->
            let tiles = toList st.currentBoard
            match tiles with
            |coord,pieces::tail ->
                let pos = coord
                let wordV = findExistingVerticalWord pos
                let current = makeNewWord wordV
                let skip = wordV.Length -1
                let gotRoom = hasRoomVertical pos (current.[skip..])
                match gotRoom with
                |true ->
                    let check  = 
*)

    let findExistingVerticalWord pos =
        failwith "not implemented"

    let makeNewWord wordOnBoard = 
        failwith "not implemented"

    let hasRoomVertical pos length =
        failwith "not implemented"
    
    let newInsert currentWordWithoutExistingTiles = 
        failwith "not implemented"


    let makeWord (c : char) (st : State.state) : string option =
        debugPrint (sprintf "current hand is %A \n" st.hand)
        let InitialC = c
       // debugPrint( sprintf "inside makeWord. now beginning \n")
        if st.currentBoard.IsEmpty then
            //debugPrint (sprintf "inside makemove. board still empty \n")
            let rec aux (c:char) (dict:Dictionary.Dict) (handlist:List<uint32>) (st:State.state) (currentWord:string) (previousDict:Dictionary.Dict) =
                let previousDict = dict
                //debugPrint (sprintf "current starting character is %A \n" c)
                let currentWord = currentWord+c.ToString()
                //debugPrint (sprintf "attempted word so far is %A \n" currentWord)
                let next = Dictionary.step c dict
                match next with
                |None ->
                    if currentWord.Length > 1 then
                        let hand = toList st.hand
                        //debugPrint (sprintf "hand length is %A \n"hand.Length)
                        let skipp = currentWord.Length
                        //debugPrint (sprintf "Skipp is  %A \n" skipp)
                        let newHand = hand[skipp..]
                        //debugPrint (sprintf "new hand length is   %A \n" newHand.Length)
                        let newC = currentWord.[currentWord.Length-2]
                        let currentWord = currentWord.Remove( currentWord.Length - 2);
                        aux newC previousDict newHand st currentWord previousDict
                    else
                        //debugPrint(sprintf "no words after %A . returning None \n" c) 
                        None
                            
                |Some (isWord,dictionary) ->
                    if currentWord.Length > 1 then
                        match isWord with
                        |true ->
                            //debugPrint(sprintf "it's a word! returning: %A because isWord is %A \n" currentWord isWord)  
                            Some currentWord
                        |false ->  
                            let newC = fst(((Map.find handlist[0] st.tiles).MinimumElement))
                            //debugPrint(sprintf "Was Not a word: currentWord is  %A , new character is %A  and isWord is %A \n" currentWord newC isWord) 
                            let newHandList = handlist[1..]
                            aux newC dictionary newHandList st currentWord previousDict
                    else
                    //debugPrint (sprintf "word was too short to be acceptable regardless of truth value. \n ")
                        let newC = fst(((Map.find handlist[0] st.tiles).MinimumElement))
                        let newHandList = handlist[1..]
                        aux newC dictionary newHandList st currentWord previousDict
            let hand = toList st.hand
            //function to remove tile from hand
            let rec delete lst x = 
                match lst with
                | head::tail when head = x -> tail
                | head::tail -> head :: (delete tail x)
                | []    -> []
            //for each tile on hand, try to create a word with the remaining hand
            let words =
                [for tile in hand do
                    let newC = fst(((Map.find tile st.tiles).MinimumElement))
                    let handlist = delete hand tile
                    let currentWord = ""
                    aux newC st.dict handlist st currentWord st.dict]

            //checking our results, returning first proper word. or none
            let rec helper words = 
                match words with
                |Some x::_ when Dictionary.lookup x st.dict = true -> Some x
                |Some _::tail -> helper tail
                |None::tail   -> helper tail
                |[]           -> None

            helper words
        else
            let firstWord = Dictionary.step c st.dict
            match firstWord with
            |None -> None
            |Some (isWord, d) -> 
                let rec aux (c:char) (dict:Dictionary.Dict) (handlist:List<uint32>) (st:State.state) (currentWord:string) (previousDict:Dictionary.Dict) =
                   // debugPrint (sprintf "current starting character is %A \n" c)
                    let previousDict = dict
                    let currentWord = currentWord+c.ToString()
                    //debugPrint (sprintf "attempted word so far is %A \n" currentWord)
                   // debugPrint (sprintf "attempted word so far is %A \n" currentWord)
                    let next = Dictionary.step c dict
                    match next with
                    |None ->
                        if currentWord.Length > 1 then
                            let hand = toList st.hand
                            //debugPrint (sprintf "hand length is %A \n"hand.Length)
                            let skipp = currentWord.Length
                            //debugPrint (sprintf "Skipp is  %A \n" skipp)
                            let newHand = hand[skipp..]
                            //debugPrint (sprintf "new hand length is   %A \n" newHand.Length)
                            let newC = currentWord.[currentWord.Length-2]
                            let currentWord = currentWord.Remove( currentWord.Length - 2);
                            aux newC previousDict newHand st currentWord previousDict
                        else
                            //debugPrint(sprintf "no words after %A . returning None \n" c) 
                            None
                        
                    |Some (isWord,dictionary) -> 
                        if currentWord.Length > 1 then
                            match isWord with
                            |true ->
                                //debugPrint(sprintf "it's a word! returning: %A because isWord is %A \n" currentWord isWord)  
                                Some currentWord
                            |false ->  
                                let newC = fst(((Map.find handlist[0] st.tiles).MinimumElement))
                                //debugPrint(sprintf "Was Not a word: currentWord is  %A , new character is %A  and isWord is %A \n" currentWord newC isWord) 
                                let newHandList = handlist[1..]
                                aux newC dictionary newHandList st currentWord previousDict
                        else
                            // debugPrint (sprintf "word was too short to be acceptable regardless of truth value. \n ")
                            let newC = fst(((Map.find handlist[0] st.tiles).MinimumElement))
                            let newHandList = handlist[1..]
                            aux newC dictionary newHandList st currentWord previousDict

                let hand = toList st.hand
                    //function to remove tile from hand
                let rec delete lst x = 
                    match lst with
                    | head::tail when head = x -> tail
                    | head::tail -> head :: (delete tail x)
                    | []    -> []
                //for each tile on hand, try to create a word with the remaining hand
                let words =
                    [for tile in hand do
                        let newC = fst(((Map.find tile st.tiles).MinimumElement))
                        let handlist = delete hand tile
                        let currentWord = InitialC.ToString()
                        aux newC d handlist st currentWord d]

                //checking our results, returning first proper word. or none
                let rec helper words = 
                    match words with
                    |Some x::_ when Dictionary.lookup x st.dict = true -> Some x
                    |Some _::tail -> helper tail
                    |None::tail   -> helper tail
                    |[]           -> None
               // debugPrint(sprintf "final word is %A \n  " (helper words))
                helper words        
  
    //too simple not to work
    let canMakeWord(c : char)  (st: State.state) =
       // debugPrint(sprintf "inside canMakeword. Testing with starting character: %A\n" c)
        match makeWord c st with
        | None -> false
        | Some x  -> 
           // debugPrint(sprintf "canMakeword is true! word is %A . Please note, this is just a check \n" x)
            true
    
    let checkAdjacentRight (pos : coord) (word : string) (c : char) (alignment : string) (st : State.state) : bool =
        debugPrint( sprintf "inside CheckAdjRight \n")
        if alignment = "vertical" then
            debugPrint (sprintf "inside AdjRight.vertical\n")
            let current = c.ToString()
            let position = (fst(pos)+1,snd(pos))
            debugPrint (sprintf "starting pos: %A\n" position)
            let rec aux (pos : coord) (current : string) (st : State.state) =
                let isOccupied = Map.tryFind pos st.currentBoard
                //debugPrint (sprintf "current: %A, pos: %A, isOccupied: %A\n" current position isOccupied)
                match isOccupied with
                | Some x -> 
                    let current = current+(fst(x)).ToString()
                    let position = (fst(pos)+1, snd(pos))
                    //debugPrint (sprintf "new pos: %A\n" position)
                    aux position current st
                | None   -> 
                        if current.Length > 1 then
                            debugPrint (sprintf "Exiting adjRight.vertical with: %A\n Current: %A\n" (Dictionary.lookup current st.dict) current)
                            Dictionary.lookup current st.dict
                        else
                            true
            aux position current st
        else
            debugPrint (sprintf "inside AdjRight.horisontal\n")
            let current = word
            let position = (fst(pos)+word.Length, snd(pos))
            let rec aux (pos : coord) (current : string) (st : State.state) =
                let isOccupied = Map.tryFind pos st.currentBoard
                //debugPrint (sprintf "current: %A, pos: %A, isOccupied: %A\n" current position isOccupied)
                match isOccupied with
                | Some x -> 
                    let current = current+(fst(x)).ToString()
                    let position = (fst(pos)+1,snd(pos))
                    aux position current st
                | None   -> 
                    if current.Length > 1 then
                        debugPrint (sprintf "Exiting adjRight.horisontal with: %A\n Current: %A\n" (Dictionary.lookup current st.dict) current)
                        Dictionary.lookup current st.dict
                    else
                        true
            aux position current st
    
    let checkAdjacentLeft (pos : coord) (word : string) (c : char) (alignment : string) (st : State.state) : bool =
        debugPrint (sprintf "Inside checkAdjLeft\n")
        if alignment = "vertical" then
            debugPrint (sprintf "Inside checkAdjLeft.vertical\n")
            let current = c.ToString()
            let position = (fst(pos)-1,snd(pos))
            let rec aux (pos : coord) (current : string) (st : State.state) =
                let isOccupied = Map.tryFind pos st.currentBoard
                //debugPrint (sprintf "current: %A, pos: %A, isOccupied: %A\n" current position isOccupied)
                match isOccupied with
                | Some x -> 
                    let current = (fst(x)).ToString()+current
                    let position = (fst(pos)-1,snd(pos))
                    aux position current st
                | None   -> 
                        if current.Length > 1 then
                            debugPrint (sprintf "Exiting adjLeft.vertical with: %A\n Current: %A\n" (Dictionary.lookup current st.dict) current)
                            Dictionary.lookup current st.dict
                        else
                            true
            aux position current st
        else
            debugPrint (sprintf "inside AdjLeft.horisontal\n")
            let current = word
            let position = (fst(pos)-1,snd(pos))
            let rec aux (pos : coord) (current : string) (st : State.state) =
                let isOccupied = Map.tryFind pos st.currentBoard
                //debugPrint (sprintf "current: %A, pos: %A, isOccupied: %A\n" current position isOccupied)
                match isOccupied with
                | Some x -> 
                    let current = (fst(x)).ToString()+current
                    let position = (fst(pos)-1,snd(pos))
                    aux position current st
                | None   ->
                    if current.Length > 1 then
                        debugPrint (sprintf "Exiting adjLeft.horisontal with: %A\n Current: %A\n" (Dictionary.lookup current st.dict) current)
                        Dictionary.lookup current st.dict
                    else
                        true
            aux position current st

    let checkAdjacentUp (pos : coord) (word : string) (c : char) (alignment : string) (st : State.state) : bool =
        debugPrint( sprintf "inside CheckAdjUP \n")
        if alignment = "vertical" then
            debugPrint( sprintf "inside CheckAdjUP.vertical \n")
            let current = word
            let position = (fst(pos), (snd(pos))-1)
            let rec aux (pos : coord) (current : string) (st : State.state) =
                let isOccupied = Map.tryFind pos st.currentBoard
                //debugPrint (sprintf "current: %A, pos: %A, isOccupied: %A\n" current position isOccupied)
                match isOccupied with
                | Some x -> 
                    let current = (fst(x)).ToString()+current
                    let position = (fst(pos),snd(pos)-1)
                    aux position current st
                | None   ->
                    if current.Length > 1 then
                        debugPrint (sprintf "Exiting adjUP.vertical with: %A\n Current: %A\n" (Dictionary.lookup current st.dict) current)
                        Dictionary.lookup current st.dict
                    else
                        true
                       
            aux position current st
        else
            debugPrint( sprintf "inside CheckAdjUP.horisontal \n")
            let current = c.ToString()
            let position = (fst(pos),snd(pos)-1)
            let rec aux (pos : coord) (current : string) (st : State.state) =
                let isOccupied = Map.tryFind pos st.currentBoard
                //debugPrint (sprintf "current: %A, pos: %A, isOccupied: %A\n" current position isOccupied)
                match isOccupied with
                | Some x -> 
                    let current = (fst(x)).ToString()+current
                    let position = (fst(pos),snd(pos)-1)
                    aux position current st
                | None   -> 
                    if current.Length > 1 then
                        debugPrint (sprintf "Exiting adjUP.horisontal with: %A\n Current: %A\n" (Dictionary.lookup current st.dict) current)
                        Dictionary.lookup current st.dict
                    else
                        true

            aux position current st
    
    let checkAdjacentDown (pos : coord) (word : string) (c : char) (alignment : string) (st : State.state) : bool =
        debugPrint( sprintf "inside CheckAdjDOWN \n")
        if alignment = "vertical" then
            debugPrint( sprintf "inside CheckAdjDOWN.vertical \n")
            let current = word
            let position = (fst(pos),snd(pos)+word.Length)
            let rec aux (pos : coord) (current : string) (st : State.state) =
                let isOccupied = Map.tryFind pos st.currentBoard
                //debugPrint (sprintf "current: %A, pos: %A, isOccupied: %A\n" current position isOccupied)
                match isOccupied with
                | Some x -> 
                    let current = current+(fst(x)).ToString()
                    let position = (fst(pos),snd(pos)+1)
                    aux position current st
                | None   -> 
                    if current.Length > 1 then
                        debugPrint (sprintf "Exiting adjDOWN.vertical with: %A\n" (Dictionary.lookup current st.dict))
                        Dictionary.lookup current st.dict
                    else
                        true

            aux position current st
        else 
            debugPrint( sprintf "inside CheckAdjDOWN.horisontal \n")
            let current = c.ToString()
            let position = (fst(pos),snd(pos)+1)
            let rec aux (pos : coord) (current : string) (st : State.state) =
                let isOccupied = Map.tryFind pos st.currentBoard
                //debugPrint (sprintf "current: %A, pos: %A, isOccupied: %A\n" current position isOccupied)
                match isOccupied with
                | Some x -> 
                    let current = current+(fst(x)).ToString()
                    let position = (fst(pos),snd(pos)+1)
                    aux position current st
                | None   -> 
                    if current.Length > 1 then
                        debugPrint (sprintf "Exiting adjDOWN.horisontal with: %A\n" (Dictionary.lookup current st.dict))
                        Dictionary.lookup current st.dict
                    else
                        true

            aux position current st
                
    let checkAdjacency (pos : coord) (word : string) (c :char) (alignment : string) (st : State.state) : bool = 
        if checkAdjacentRight pos word c alignment st && checkAdjacentLeft pos word c alignment st && 
            checkAdjacentDown pos word c alignment st && checkAdjacentUp pos word c alignment st then true
        else 
            false
        
    let wordCheckAdjacency (word : string) (pos : coord) (alignment : string) (st : State.state) : bool =
        // 1. boardWithWord = (board.insert word)
        // 2. for each position where we placed a new tile:
        //      2.1. check vertical
        //              2.1.1. get position of first tile in word (travel up)
        //              2.1.2. traverse downwards untill next space is empty while checking dictionary
        //                      - if we can't step the dictionary (returns none), word cannot be placed
        //              2.1.3. if the word we end with is a valid word (can be checked in the output of step), the word is valid
        //      2.2. check horizontal
        let chars = Seq.toList word
        if alignment = "vertical" then
            let rec aux chars pos (word: string) = 
                match chars with 
                | head::tail ->
                    let newPos = (fst(pos),snd(pos)+1) 
                    let newWord = word.[1..]
                    if (checkAdjacency pos word head "vertical" st) then aux chars[1..] newPos newWord else false
                | [] -> true
            aux chars pos word
        else
            let rec aux chars pos (word: string) = 
                match chars with 
                | head::tail ->
                    let newPos = (fst(pos)+1,snd(pos)) 
                    let newWord = word.[1..]
                    if (checkAdjacency pos word head "horisontal" st) then aux chars[1..] newPos newWord else false
                | [] -> true
            aux chars pos word

    let newBoard (st:State.state) (lst: option<list<(int * int) * (uint32 * (char * int))>>) =
        match lst with
        |None -> st.currentBoard
        |Some l -> 
            let rec aux l currentBoard = 
                match l with 
                |h::t   -> 
                    let currentBoard = Map.add (fst(h)) (snd(snd(h))) currentBoard
                    aux t currentBoard 
                |[] ->
                    debugPrint(sprintf "Returning the new board: %A \n" currentBoard) 
                    currentBoard
            aux l st.currentBoard
    
    let getWholeWordHorisontal (pos:coord) (currentBoard:Map<coord,pieces>) (st:State.state) = 
        let rec aux (pos:coord) (currentBoard:Map<coord,pieces>) (word:string) =
            let current = fst(Map.find pos currentBoard)
            let word = word+current.ToString()
            let posRight = (fst(pos)+1,snd(pos))
            let right = Map.tryFind posRight currentBoard
            match right with
            |Some _ -> aux posRight currentBoard word
            |None   -> 
                debugPrint( sprintf "inside check adjacency - checking horisontally. returning %A, it is %A \n" word (Dictionary.lookup word st.dict))
                if word.Length > 1 then Dictionary.lookup word st.dict else true
        aux pos currentBoard ""
    
    let checkAdjHorisontal (pos:coord) (currentBoard:Map<coord, pieces>) (st:State.state) =
        //debugPrint( sprintf "inside check adjacency - checking horisontally \n")
        let rec aux pos currentBoard =
            let posLeft = ((fst(pos)-1),snd(pos))
            let left = Map.tryFind posLeft currentBoard
            match left with 
            |Some _     -> aux posLeft currentBoard
            |None       -> getWholeWordHorisontal pos currentBoard st
        aux pos currentBoard

    let getWholeWordVertical (pos:coord) (currentBoard:Map<coord,pieces>) (st:State.state)=
        let rec aux (pos:coord) (currentBoard:Map<coord,pieces>) (word:string) =
            let current = fst(Map.find pos currentBoard)
            let word = word+current.ToString()
            let posBelow = (fst(pos), snd(pos)+1)
            let below = Map.tryFind posBelow currentBoard
            match below with 
            |Some _ -> aux posBelow currentBoard word
            |None   -> 
                debugPrint( sprintf "inside check adjacency vertical. returning %A, it is %A \n" word (Dictionary.lookup word st.dict))
                if word.Length > 1 then Dictionary.lookup word st.dict else true
        aux pos currentBoard ""
    
    let checkAdjVertical (pos:coord) (currentBoard:Map<coord,pieces>) (st:State.state) =
        //debugPrint( sprintf "inside check adjacency - checking vertically \n") 
        let rec aux pos currentBoard =
            let posAbove = (fst(pos), snd(pos)-1)
            let above = Map.tryFind posAbove currentBoard
            match above with
            |Some _ -> aux posAbove currentBoard
            |None -> getWholeWordVertical pos currentBoard st
        aux pos currentBoard

    let adjancency (pos:coord) lst st : bool =
        //debugPrint( sprintf "inside adjaNcency. About to create new board \n") 
        let currentBoard = newBoard st lst
        (checkAdjVertical pos currentBoard st && checkAdjHorisontal pos currentBoard st)

    let adjacencyFinal (pos:coord) (lst: option<list<(int * int) * (uint32 * (char * int))>>) st =
        let rec aux lst st =
            match lst with
            |None -> false
            |Some x -> 
                    let rec helper x = 
                        match x with 
                        |h::tail -> 
                            let coord = fst(h)
                            if (adjancency coord lst st) then helper tail else false
                        |[]     -> true
                    helper x
        (adjancency pos lst st) && (aux lst st)
    let canVertical (w:string option) (pos:coord) (st:State.state) =
        match w with
            |None  -> false
            |Some word ->
                let rec aux count pos =
                    if count = 0 then true
                    else
                        let next = fst(pos),snd(pos)+1
                        let nextExists = Map.tryFind next st.currentBoard
                        match nextExists with
                        |Some _ -> false
                        |None -> 
                            let count = count - 1
                            aux count next
                aux (word.Length) pos

    let canHorisontal (w:string option) (pos:coord) (st:State.state) =
        match w with
            |None  -> false
            |Some word ->
                let rec aux count pos =
                    if count = 0 then true
                    else
                        let next = fst(pos)+1,snd(pos)
                        let nextExists = Map.tryFind next st.currentBoard
                        match nextExists with
                        |Some _ -> false
                        |None -> 
                            let count = count - 1
                            aux count next
                aux (word.Length) pos

    let rec canInsertWordVertical (word : string option) (pos : coord as (x,y)) (st : State.state) =
        debugPrint(sprintf "Inside canInsertWordVertical. checking if can insert word: %A vertically, with starting position: %A \n" word pos)
        match word with
        | None -> false
        | Some w -> 
            let lengthOfInput = w.Length-1
            //if you reach this, then you have reached end of word without issues. can insert
            if (lengthOfInput = 0) then
                debugPrint (sprintf "inside canVertical. Returning true\n")
                true
            else
            // checking if can insert
                let newCoord = (x,(y+lengthOfInput))
                match Map.tryFind newCoord st.currentBoard with
                | Some x ->
                    debugPrint(sprintf "inside canVertical. Returning false\n")
                    false
                | None   -> canInsertWordVertical (Some w[0..(w.Length-2)]) newCoord st

    let rec canInsertWordHorisontal (word : string option) (pos : coord as (x,y)) (st : State.state) =
        //debugPrint(sprintf "Inside canInsertWordHorisontal. checking if can insert word: %A horisontally, with starting position: %A \n" word pos)
        match word with
        |None -> false
        |Some w -> 
            let lengthOfInput = w.Length-1
            if (lengthOfInput = 0) then
                debugPrint (sprintf "Can insert word horisontally \n")
                true 
            else
                let newCoord = ((x+lengthOfInput),y)
                match Map.tryFind newCoord st.currentBoard with
                | Some x ->
                    debugPrint (sprintf "Inside canInsertHorisontal. cannot insert word horisontally  \n") 
                    false
                | None   -> canInsertWordHorisontal (Some w[0..(w.Length-2)]) newCoord st

    //just a getter for the word, without the option type
    let getWord (c:char) (st:State.state) =
        let word = makeWord c st
        match word with
            |None -> ""
            |Some w ->
               // debugPrint(sprintf "inside getWord. just returning the word: %A  for another function \n" w)
                w
    //now returns tile on hand
    let getTileFromCharacter (c:char) (st:State.state) =
        let set = st.hand
        let lst = toList set
        let newL = List.map (fun x -> Map.find x st.tiles)
        let tiles = newL lst
        //debugPrint (sprintf "printing the tile list from hand %A\n" tiles)

        //let mylst = List.fold (fun acc x -> List.map( Map.find x) empt lst 
        let rec helper lst c : tile =
            match lst with
            |t:tile::tail when
                fst(t.MinimumElement) = c && t.Count = 1 ->
                   // debugPrint (sprintf "Count of tile t: %A\n" t)
                    t //fst(tile.MaximumElement),snd(tile.MaximumElement)
            |t:tile::tail when
                fst(t.MinimumElement) = c && t.Count > 1 ->
                   // debugPrint (sprintf "Count of tile t: %A\n" t)
                    t //fst(tile.MaximumElement),snd(tile.MaximumElement)            
            |x::rest ->
                helper rest c
            |[]      ->
                let set = Set.empty.Add(' ', 0)
                set
                                                                        
        helper tiles c
(*
        let (lst : list<uint32 * tile>) = st.tiles |> Map.toList
        let rec aux (lst: list<uint32 * tile>) =
            match lst with
            | ((n : uint32) , (tile : tile))::tail when fst(tile.MaximumElement) = c -> n
            | head::tail -> aux tail 
            | [] -> 0u
        aux lst
*)
        
    //getter for 
    let getIDValue (c:char) (st:State.state) =
        let (lst : list<uint32 * tile>) = st.tiles |> Map.toList
        match lst with
        | ((n : uint32) , t)::tail when fst(t.MinimumElement) = c -> snd(t.MinimumElement)
        |_ -> 0

    let getIDbasedOnTile (t:tile) (st:State.state) = 
        let lst = st.tiles |> Map.toList
        let rec aux (t:tile) (lst:list<uint32 * tile>) = 
            match lst with 
            |(id,tl)::tail when
                tl = t  -> id 
            |head::tail -> aux t tail
            |[]         -> 0u 
        aux t lst



    //give word, recieve tile list
    let getTilesFromWord (c:char) (st:State.state) (fstWord:bool) =
        if fstWord then 
            let word = getWord c (st: State.state)
            let charList = Seq.toList word
            let rec aux charList st  =
                match charList with
                | x::tail -> 
                    let tile = getTileFromCharacter x st
                    let id = getIDbasedOnTile tile st
                    let character = fst(tile.MinimumElement)
                    let value = snd(tile.MinimumElement)
                    (id,(character,value))::aux tail st
                | [] -> []
            aux charList st
        else 
            let word = getWord c (st: State.state)
            let w = word[1..]
            let charList = Seq.toList w
            let rec aux charList st  =
                match charList with
                | x::tail -> 
                    let tile = getTileFromCharacter x st
                    let id = getIDbasedOnTile tile st
                    let character = fst(tile.MinimumElement)
                    let value = snd(tile.MinimumElement)
                    (id,(character,value))::aux tail st
                | [] -> []
            aux charList st

    let coordList (word:string) (firstWord:bool) (direction:char) (pos:coord): (int*int) list = 
        let wordList = Seq.toList word
        if firstWord then
           // debugPrint (sprintf "Inside coordlist. Empty board\n" )

            let rec aux wordList count = 
                match wordList with 
                |head::tail -> (0+count,0)::aux tail (count+1)
                |[]         -> []
            aux wordList 0
        else
            //debugPrint (sprintf "Inside coordList.Non-empty board \n" )
            let wordList = wordList[1..]
            if direction = 'v' then 
           //     debugPrint (sprintf "Inside coordList. using 'V' \n")
                let rec aux wordList pos = 
                    match wordList with
                    |[]         -> []
                    |head::tail -> 
                        let newCoord = (fst(pos),(snd(pos)+1))
                        newCoord::aux tail newCoord

                aux wordList (fst(pos),snd(pos))
            else 
             //   debugPrint (sprintf "Inside coordList. using 'h' \n")
                let rec aux wordList pos = 
                    match wordList with
                    |[]         -> []
                    |head::tail -> 
                        let newCoord = (fst(pos)+1,(snd(pos)))
                        newCoord::aux tail newCoord

                aux wordList (fst(pos),snd(pos)) 
            
    let insertFirstWord (c:char) (st:State.state) (pos:coord) =
        let tiles =  getTilesFromWord c st true
        let coords = coordList (getWord c (st: State.state)) true 'v' (0,0)
        //we just insert vertically
        let ourMove coords tiles =
            Some (List.zip coords tiles)

        ourMove coords tiles 

    let insertWord (input : (((char) * (int * int)) * State.state)) =
        let c = fst(fst(input))
        let pos = snd(fst(input))
        let st = snd(input)
        //change from booleans. if can insert horisontal, then do so 
        //otherwise, insert vertically
        if st.currentBoard.IsEmpty then
            //debugPrint (sprintf "Inside insertWord. I claim that board is empty\n%A \n" (getWord c st)) 
            insertFirstWord c st pos
        else
            if c = ' ' then
                debugPrint (sprintf "Inside insertWord - could not construct word. swap instead \n")
                None

            else
                //debugPrint (sprintf "Inside insertWord. I claim that board is NOT empty \n" )
                let tiles =  getTilesFromWord c st false
                if canVertical (makeWord c st) pos st then 
                    //debugPrint (sprintf "trying to insert vertically (insert word)\n" )
                    let coords = coordList (getWord c (st: State.state)) false 'v' pos
                    debugPrint (sprintf "coords length: %A, tiles length: %A\n" coords.Length tiles.Length)
                    //debugPrint (sprintf "coords: %A\ntiles: %A\n" coords tiles)
                    //we just insert vertically 
                    let ourMove coords tiles =                  
                        debugPrint (sprintf "coords & tiles are %A" (Some (List.zip coords tiles)))
                        Some (List.zip coords tiles)
                    ourMove coords tiles
                else if canHorisontal(makeWord c st) pos st then 
                        //debugPrint (sprintf "trying to insert horisontally (insert word)\n" )
                        let coords = coordList (getWord c (st: State.state)) false 'h' pos
                        //we just insert vertically 
                        let ourMove coords tiles =
                            debugPrint (sprintf "coords & tiles are %A" (Some (List.zip coords tiles)))
                            Some (List.zip coords tiles)
                        ourMove coords tiles
                    else 
                        None
    

    let FindMove (st : State.state) =
       // debugPrint(sprintf "inside findMove. Checking if board is empty \n")

        if st.currentBoard.IsEmpty then
            debugPrint(sprintf "inside findMove. Board is empty. \n")
            //let handList = toList st.hand
            //let newC = Map.find handList[0] st.tiles
            //let c = fst(newC.MinimumElement)

            //debugPrint(sprintf "inside findMove.Starting char is: %A \n" c)
            //let (newHand : MultiSet<uint32>) =
            //MultiSet.removeSingle handList[0] st.hand
            //let newState = {st with hand = newHand}
            (' ', (0,0)), st
        else 
            debugPrint (sprintf "Board is not empty. will try to insert a word, using a single existing character on table \n" )
            let chars = State.currentBoard st |> Map.toSeq |> List.ofSeq; 
            // each entry in the list looks as follows: [(int*int), (char*int)] 
            // here (int*int) is coord, (char*int) is piece
            let rec aux (chars:list<coord * State.pieces>) st = 
                match chars with
                | (coord,pieces)::tail when
                    (canMakeWord (fst(pieces)) st) && ((getWord (fst(pieces)) st).Length > 1) &&
                    (
                        ((canVertical (makeWord (fst(pieces)) (st)) coord st) &&
                        (adjacencyFinal coord (insertWord ((fst(pieces),coord),st)) st))
                        ||
                        ((canHorisontal (makeWord (fst(pieces)) (st)) coord st) &&
                        (adjacencyFinal coord (insertWord ((fst(pieces),coord),st)) st))
                    )
                          -> (fst(pieces), coord),st 
                | _::tail -> aux(tail) st 
                | []      -> (' ',  (0,0)), st
            aux chars st
            
    let playGame cstream pieces (st : State.state) =

        let rec aux (st : State.state) =
            Print.printHand pieces (State.hand st)

            let nextTurn = 
                let next = st.playerTurn + 1u
                if next = (st.numPlayer+1u) then 1u else next     

            //check if our turn
            let ourTurn (playerNumber : uint32) (playerTurn : uint32) =  
                if playerNumber = playerTurn then true else false
            //only request and make move if our turn
            if (ourTurn st.playerNumber st.playerTurn) then 
                // remove the force print when you move on from manual input (or when you have learnt the format)
                forcePrint "Input move (format '(<x-coordinate> <y-coordinate> <piece id><character><point-value> )*', note the absence of space between the last inputs)\n\n"
                //let input = System.Console.ReadLine()
                debugPrint (sprintf "before query\n")
                let query = insertWord(FindMove(st))
                debugPrint (sprintf "query: %A\n" query)
                    
                debugPrint(sprintf "Current turn: %A, PlayerNumber: %A, PlayerTurn: %A\n" ourTurn st.playerNumber st.playerTurn)
                //method for setting next turn
                //debugPrint (sprintf "Player %d -> Server:\n%A\n" (State.playerNumber st) move) // keep the debug lines. They are useful.
                match query with 
                | None   -> send cstream (SMChange (toList st.hand))
                | Some q -> 
                    if q = [] then send cstream (SMChange (toList st.hand))
                    else send cstream (SMPlay q)
                
            let msg = recv cstream
            debugPrint (sprintf "Player %d <- Server:\nJust listening\n" (State.playerNumber st)) // keep the debug lines. They are useful.
            
            match msg with
            | RCM (CMPlaySuccess(ms, points, newPieces)) ->
                (* Successful play by you. Update your state (change turn, etc) *)
                let newBoard =
                    List.fold  (fun acc (c, k) ->
                        Map.add c (snd(k)) acc) st.currentBoard ms
                //greatest line of code EVER
                //removing tiles from hand
                let (newHand : MultiSet<uint32>) =
                    List.fold (fun acc (_, (k: uint32 * (char * int))) ->
                        MultiSet.removeSingle (fst(k)) acc) st.hand ms
                // adds newPieces to handset
                let (newHandSet : MultiSet<uint32>) =
                    List.fold (fun acc (x, k) ->
                        MultiSet.add x k acc) newHand newPieces
                //updates hand & board & turn
                let st' = {st with hand = newHandSet; currentBoard = newBoard; playerTurn = nextTurn}
                debugPrint(sprintf "Map of current board: %A\n" st'.currentBoard)
                
                aux st'
            | RCM (CMPlayed (pid, ms, points)) ->
                (* Successful play by other player. Update your state *)
                //updates state to reflect new move
                let newBoard =
                    List.fold  (fun acc (c, k) ->
                        Map.add c (snd(k)) acc) st.currentBoard ms
                let st' = {st with currentBoard = newBoard; playerTurn = nextTurn} 
                aux st'
            | RCM (CMChangeSuccess (newTiles)) ->
                let (newHand : MultiSet<uint32>) = MultiSet.empty         
                let (newHandSet : MultiSet<uint32>) =
                    List.fold (fun acc (x, k) ->
                        MultiSet.add x k acc) newHand newTiles
                //updates hand & board & turn
                let st' = {st with hand = newHandSet; playerTurn = nextTurn}
                aux st'
            | RCM (CMPassed player) ->
                if player <> st.playerNumber then
                    let st' = {st with playerTurn = nextTurn} 
                    aux st'
                else
                    aux st
            | RCM (CMTimeout player ) -> 
                let st' = {st with playerTurn = nextTurn} 
                aux st'
            | RCM (CMPlayFailed (pid, ms)) ->
                (* Failed play. Update your state *)
                    if pid = st.playerNumber then 
                        send cstream (SMChange (toList st.hand))
                        let st' = {st with playerTurn = nextTurn} 
                        aux st'
                    else
                        let st' = {st with playerTurn = nextTurn} 
                        aux st'
            | RCM (CMGameOver _) -> ()
            | RCM (CMChange(playerId, numberOfTiles)) ->
                if playerId <> st.playerNumber  then
                    let st' = {st with playerTurn = nextTurn} 
                    aux st'
                else
                    aux st
          
            | RCM (CMForfeit player) ->
                let newNumPlayer = st.numPlayer-1u
                let st' = {st with numPlayer = newNumPlayer}
                aux st' 
            | RGPE err ->
                let helper error =
                    match error with
                    | GPENotEnoughPieces (_ , p) ->
                        send cstream (SMChange (List.take (int p) (toList st.hand)))
                        let st' = {st with playerTurn = nextTurn} 
                        aux st'
                    | _ ->
                        debugPrint( sprintf "unknown error. Visit me on line 886")
                        //printfn "Gameplay Error:\n%A" err
                        send cstream (SMChange (toList st.hand))
                        let st' = {st with playerTurn = nextTurn} 
                        aux st'
                List.iter helper err
                //aux st
        aux st

    let startGame 
            (boardP : boardProg) 
            (dictf : bool -> Dictionary.Dict) 
            (numPlayers : uint32) 
            (playerNumber : uint32) 
            (playerTurn  : uint32) 
            (hand : (uint32 * uint32) list)
            (tiles : Map<uint32, tile>)
            (timeout : uint32 option) 
            (cstream : Stream) =
        debugPrint 
            (sprintf "Starting game!
                    number of players = %d
                    player id = %d
                    player turn = %d
                    hand =  %A
                    timeout = %A\n\n" numPlayers playerNumber playerTurn hand timeout)

        //let dict = dictf true // Uncomment if using a gaddag for your dictionary
        let dict = dictf false // Uncomment if using a trie for your dictionary
        let board = Parser.mkBoard boardP
        let handSet = List.fold (fun acc (x, k) -> MultiSet.add x k acc) MultiSet.empty hand

        fun () -> playGame cstream tiles (State.mkState board dict playerNumber handSet numPlayers playerTurn tiles)




    //program 
    //let move = insertWord(FindMove(st))

        
    //for each, check if use can use it as first character in word, using your available tiles in hand and the dictionary
    
    // if you can create word check if the necessary number of tiles are available horisontally or vertically

    //if so, insert the words from hand and return

    //otherwise, if false and end of list, return change request
    