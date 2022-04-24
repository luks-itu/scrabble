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
    }

    let mkState b d pn h n t =
        {board = b; dict = d;  playerNumber = pn; hand = h; numPlayer = n; playerTurn = t; currentBoard = Map.empty  }

    let board st         = st.board
    let dict st          = st.dict
    let playerNumber st  = st.playerNumber
    let hand st          = st.hand
    let numPlayer st     = st.numPlayer 
    let playerTurn st    = st.playerTurn
    let currentBoard st  = st.currentBoard
    

module Scrabble =
    open System.Threading
    open MultiSet

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
                let input =  System.Console.ReadLine()
                let move = RegEx.parseMove input //findMove(State)
                    
                debugPrint(sprintf "Current turn: %A, PlayerNumber: %A, PlayerTurn: %A" ourTurn st.playerNumber st.playerTurn)
                //method for setting next turn

                debugPrint (sprintf "Player %d -> Server:\n%A\n" (State.playerNumber st) move) // keep the debug lines. They are useful.
        
                send cstream (SMPlay move)
            
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

        fun () -> playGame cstream tiles (State.mkState board dict playerNumber handSet numPlayers playerTurn)
(*
    let canMakeWord (c : char) (hand : MultiSet.MultiSet<uint32>) (st: State.state) =
        let rec aux c = 
            match c with
            | c -> Dictionary.step c
            | ' ' -> ' '

    let makeWord c state : string = "placeholder"

    let canInsertWord (word : string) (pos : coord) (st : State.state) = true

    //worst code EVER
    let FindMove (st : State.state) = 
        let chars = State.currentBoard st |> Map.toSeq |> List.ofSeq;
        let rec aux (chars:list<coord * State.pieces>) st = 
                        match chars with
                        |(coord,pieces)::tail  when (canMakeWord (fst(pieces)) (State.hand st) st) && (canInsertWord (makeWord (fst(pieces)) (State.hand st)) coord st) -> fst(pieces)
                        |_::tail -> aux(tail) st 
                        |[]      -> ' '
        aux chars st

*)
        
    //for each, check if use can use it as first character in word, using your available tiles in hand and the dictionary
    
    // if you can create word check if the necessary number of tiles are available horisontally or vertically

    //if so, insert the words from hand and return

    //otherwise, if false and end of list, return change request
    