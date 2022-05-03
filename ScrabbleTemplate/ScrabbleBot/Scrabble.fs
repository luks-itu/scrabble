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

    let makeWord c (st:State.state) : string option =
        debugPrint(sprintf "inside makeWord1. Current hand is : %A \n" (st.hand))
        debugPrint(sprintf "inside makeWord2. Current character which word starts with: %A\n" c)
        
        let handList = toList st.hand
        let remainingChars = handList.Length
        let rec aux (c : char) dict (currentWord : string) (handList:List<uint32>) (remainingChars:int) =
            debugPrint(sprintf "inside makeWord aux. Moving down tree. currently at character: %A\n" c )
            let current = currentWord+(c.ToString())
            match Dictionary.step c dict with
            | Some x -> 
                match fst(x) with
                | true ->
                    debugPrint(sprintf "found a word! it is: %A\n" current) 
                    Some current 
                | false ->
                    if (remainingChars = 0) then None 
                    else
                        let newC = Map.find handList[0] st.tiles
                        let c = fst(newC.MinimumElement)
                        let newHandList = handList[1..]
                        let remaining = remainingChars-1
                        aux c (snd(x)) current newHandList remaining
            | None ->
                    if (remainingChars = 0) then None
                    else
                        let newC = Map.find handList[0] st.tiles
                        let c = fst(newC.MinimumElement)
                        let newHandList = handList[1..]
                        let remaining = remainingChars-1
                        aux c dict current newHandList remaining
            
        aux c (st.dict) "" handList remainingChars

    
    let canMakeWord(c : char)  (st: State.state) =
        debugPrint(sprintf "inside canMakeword. ignore the next makeWord. Testing with starting character: %A" c)
        match makeWord c st with
        | None -> false
        | _  -> true

    let rec canInsertWordVertical (word : string option) (pos : coord as (x,y)) (st : State.state) =
        debugPrint(sprintf "Inside canInsertWordVertical. checking if can insert word: %A vertically, with starting position: %A \n" word pos)
        match word with
        | None -> false
        | Some w -> 
            let lengthOfInput = w.Length-1
            if (lengthOfInput = 0) then
                debugPrint (sprintf "inside canVertical. Returning true")
                true
            else
                let newCoord = ((x+lengthOfInput),y)
                match Map.tryFind newCoord st.currentBoard with
                | Some x ->
                    debugPrint(sprintf "inside canVertical. Returning false")
                    false
                | None   -> canInsertWordVertical (Some w[0..(w.Length-2)]) pos st

    let rec canInsertWordHorisontal (word : string option) (pos : coord as (x,y)) (st : State.state) =
        debugPrint(sprintf "Inside canInsertWordHorisontal. checking if can insert word: %A horisontally, with starting position: %A \n" word pos)
        match word with
        |None -> false
        |Some w -> 
            let lengthOfInput = w.Length-1
            if (lengthOfInput = 0) then true else
                    let newCoord = (x,(y+lengthOfInput))
                    match Map.tryFind newCoord st.currentBoard with
                    | Some x -> false
                    | None   -> canInsertWordHorisontal (Some w[0..(w.Length-2)]) pos st

    let getWord (c:char) (st:State.state) =
        let word = makeWord c st
        match word with
            |None -> ""
            |Some w ->
             debugPrint(sprintf "inside getWord. returning the word: %A" w)
             w
    
    let getID (c:char) (st:State.state) =
        let (lst : list<uint32 * tile>) = st.tiles |> Map.toList
        match lst with
        | ((n : uint32) , t)::tail when fst(t.MinimumElement) = c -> n
        |_ -> 0u
    
    let getIDValue (c:char) (st:State.state) =
        let (lst : list<uint32 * tile>) = st.tiles |> Map.toList
        match lst with
        | ((n : uint32) , t)::tail when fst(t.MinimumElement) = c -> snd(t.MinimumElement)
        |_ -> 0

    let insertFirstWord (c:char) (st:State.state) (pos:coord) = 
        debugPrint(sprintf "Inside insertFirstWord. trying with character: %A at position %A \n" c pos)
        if (canInsertWordVertical (makeWord c st) pos st) then 
            let word = getWord c st
            let w = word[0..]
            debugPrint( sprintf "word inside insertFirstWord is currently: %A\n" w)
            let rec aux (w : string) (pos : coord) =
                match w.Chars(0) with
                | ' ' -> ""
                | ch  ->
                 debugPrint( sprintf "inside where the magic happens\n")
                 fst(pos).ToString() + " " +
                         snd(pos).ToString() + " " +
                         (getID ch st).ToString() + " " +
                         ch.ToString() + " " +
                         (getIDValue ch st).ToString() + " " +
                         aux w[1..] (fst(pos)+1, snd(pos))
            debugPrint (sprintf "before startPos\n")                
            let startPos = fst(pos)+1, snd(pos) 
            debugPrint (sprintf "2. move as string: %A \n" (Some (aux w startPos)))
            Some (aux w startPos)
        else if (canInsertWordHorisontal (makeWord c st) pos st) then
            let word = getWord c st
            let w = word[0..]
            let rec aux (w : string) (pos : coord) =
                match w.Chars(0) with
                | ' ' -> ""
                | ch  -> fst(pos).ToString() + " " +
                            snd(pos).ToString() + " " +
                            (getID ch st).ToString() + " " +
                            ch.ToString() + " " +
                            (getIDValue ch st).ToString() + " " +
                            aux w[1..] (fst(pos), (snd(pos)+1))
            let startPos = fst(pos), snd(pos)+1 
            Some (aux w startPos)
        else
            None


        

    let insertWord (input : (((char) * (int * int)) * State.state))   =
        let c = fst(fst(input))
        let pos = snd(fst(input))
        let st = snd(input)
        //change from booleans. if can insert horisontal, then do so 
        //otherwise, insert vertically
        match st.currentBoard.TryFind (0,0) with 
        | None ->
            debugPrint (sprintf "Inside insertWord. I claim that board is empty \n" ) 
            insertFirstWord c st pos
        | Some x ->
            debugPrint (sprintf "Inside insertWord. I claim that board has tiles \n" )
            match c with
            | ' ' -> None
            | _ -> 
                if (canInsertWordVertical (makeWord c st) pos st) then
                    debugPrint(sprintf "inside insertWord. line 214 TRYING to vertically insert the word: %A at position: %A \n" (makeWord c st) pos )
                    let word = getWord c st
                    let w = word[1..]
                    let output = ""
                    let rec aux (w : string) (pos : coord) =
                        match w.Chars(0) with
                        | ' ' -> ""
                        | ch  -> fst(pos).ToString() + " " +
                                 snd(pos).ToString() + " " +
                                 (getID ch st).ToString() + " " +
                                 ch.ToString() + " " +
                                 (getIDValue ch st).ToString() + " " +
                                 aux w[1..] (fst(pos), (snd(pos)+1))
                    let startPos = fst(pos), snd(pos)+1 
                    Some (aux w startPos)
                else if (canInsertWordHorisontal (makeWord c st) pos st) then 
                    debugPrint (sprintf "inside insertWord.  Failed to insert vertically. line 230 TRYING horisontally insert word: %A at position %A with starting character %A, horisontally \n." (makeWord c st) pos c)
                    let word = getWord c st
                    let w = word[1..]
                    let output = ""
                    let rec aux (w : string) (pos : coord) =
                        match w.Chars(0) with
                        | ' ' -> ""
                        | ch  -> fst(pos).ToString() + " " +
                                 snd(pos).ToString() + " " +
                                 (getID ch st).ToString() + " " +
                                 ch.ToString() + " " +
                                 (getIDValue ch st).ToString() + " " +
                                 aux w[1..] (fst(pos)+1, snd(pos))            
                    let startPos = fst(pos)+1, snd(pos) 
                    Some (aux w startPos)
                else
                    debugPrint(sprintf "inside insertWord. i claim to fail to insert word %A at position: %A \n"  (makeWord c st) pos)
                    None
        //check if you should insert at all 
        //let updatedState = if c = ' ' then true else newState

        //if input is equal to " " for first tuple, dont change state but request new hand)
        //otherwise update state, with removing tile from hand and new coord on board and signal server correctly
        //st
        //remove tiles from hand, and insert on coord in currentBoard



    
    //if it works, it works
    let FindMove (st : State.state) =
        debugPrint(sprintf "inside findMove. Checking if board is empty \n")
        if st.currentBoard.IsEmpty then
            debugPrint(sprintf "Board is empty. im at line 259\n")
            let handList = toList st.hand
            let newC = Map.find handList[0] st.tiles
            let c = fst(newC.MinimumElement)
            debugPrint(sprintf "inside findMove.Starting char is: %A \n" c)
            let (newHand : MultiSet<uint32>) =
                MultiSet.removeSingle handList[0] st.hand
            let newState = {st with hand = newHand}
            (c, (0,0)), newState
            //let chars = State.currentBoard st |> Map.toSeq |> List.ofSeq; 
            
            (*match chars with
            | ((fst(chars)), pieces) :: tail when (canMakeWord c newState) &&
                                           (canInsertWordHorisontal (makeWord c newState) coord newState) ||
                                           canInsertWordVertical (makeWord c newState) coord newState
                                           -> (c, (coord)), newState
            |[]         ->  (c,  (0,0)), st

            *)
        
        else 
            debugPrint (sprintf "Board is not empty. will try to insert a word, using an existing start character on table \n" )
            let chars = State.currentBoard st |> Map.toSeq |> List.ofSeq; 
            // each entry in the list looks as follows: [(int*int), (char*int)] 
            // here (int*int) is coord, (char*int) is piece
            let rec aux (chars:list<coord * State.pieces>) st = 
                            match chars with
                            | (coord,pieces)::tail when
                                                (canMakeWord (fst(pieces))  st) &&
                                                ((canInsertWordVertical (makeWord (fst(pieces)) (st)) coord st) || (canInsertWordHorisontal (makeWord (fst(pieces)) (st)) coord st))
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
            (*
                let c  = 'O'
                let testHand = MultiSet.empty;
                let testHand = MultiSet.addSingle 1u testHand
                let testHand = MultiSet.addSingle 1u testHand
                let testHand = MultiSet.addSingle 1u testHand
                let testHand = MultiSet.addSingle 1u testHand
                let testst = {st with hand = testHand}
                debugPrint(sprintf "hand: %A \n" testHand)
                debugPrint(sprintf "tile1: %A \n" (st.tiles.TryFind(20u)))
                debugPrint(sprintf "makeword: %A \n" (makeWord c testst))
                debugPrint(sprintf "length of word: %A \n" (makeWord 'C' testst).Value)

*)

                //let input = System.Console.ReadLine()
                debugPrint (sprintf "before query\n")
                let query = insertWord(FindMove(st))
                debugPrint (sprintf "query: %A\n" query)

                
             

                //let move = RegEx.parseMove input //findMove(State)
                //debugPrint(sprintf "move: %A\n" (RegEx.parseMove (Some query)))
                    
                debugPrint(sprintf "Current turn: %A, PlayerNumber: %A, PlayerTurn: %A\n" ourTurn st.playerNumber st.playerTurn)
                //method for setting next turn

                //debugPrint (sprintf "Player %d -> Server:\n%A\n" (State.playerNumber st) move) // keep the debug lines. They are useful.
                match query with 
                | None   -> send cstream (CMChange)
                | Some q -> send cstream (SMPlay (RegEx.parseMove q))


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
                let st' = {st with hand = newHandSet;  playerTurn = nextTurn}
                aux st'
            | RCM (CMPlayFailed (pid, ms)) ->
                //ignore this evil comment
                (* Failed play. Update your state *)
                let st' = st // This state needs to be updated
                aux st'
            | RCM (CMGameOver _) -> ()
            | RCM a -> failwith (sprintf "not implmented: %A" a)
            | RGPE err -> printfn "Gameplay Error:\n%A" err; aux st


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
    