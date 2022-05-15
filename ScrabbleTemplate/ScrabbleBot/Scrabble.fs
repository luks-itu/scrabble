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
        playerNumber  : uint32 //the bots number
        hand          : MultiSet.MultiSet<uint32>
        numPlayer     : uint32 //number of players participating
        playerTurn    : uint32 //whose turn
        currentBoard  : Map<coord,pieces>
        tiles         : Map<uint32, tile>  //We assume tiles in the lookup table
        remainingTiles : int  //number of tiles played in the game so far
        flip : int //used to determine if horisont or vertical (for solo play only)
    }

    let mkState b d pn h n t l =
        {board = b; dict = d;  playerNumber = pn; hand = h; numPlayer = n; playerTurn = t; currentBoard = Map.empty; tiles = l; remainingTiles = 0; flip = 0}

    let board st         = st.board
    let dict st          = st.dict
    let playerNumber st  = st.playerNumber
    let hand st          = st.hand
    let numPlayer st     = st.numPlayer 
    let playerTurn st    = st.playerTurn
    let currentBoard st  = st.currentBoard
    let tiles st         = st.tiles
    let remainingTiles st  = st.remainingTiles
    let flip st             = st.flip
    

module Scrabble =
    open System.Threading
    open MultiSet
    type pieces =  (char*int)



    let incrementFlip (st:State.state) = st.flip+1

        //used to check adjacencies - by pretending we have already inserted our word
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
       //                debugPrint(sprintf "Returning the new board: %A \n" currentBoard) 
                    currentBoard
            aux l st.currentBoard
        
        //check if the created word can fit the board horisontally
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
    
    //helper for horisontal adjacency check
    let getWholeWordHorisontal (pos:coord) (currentBoard:Map<coord,pieces>) (st:State.state) = 
        let rec aux (pos:coord) (currentBoard:Map<coord,pieces>) (word:string) =
            let current = fst(Map.find pos currentBoard)
            let word = word+current.ToString()
            let posRight = (fst(pos)+1,snd(pos))
            let right = Map.tryFind posRight currentBoard
            match right with
            |Some _ -> aux posRight currentBoard word
            |None   -> 
            //    debugPrint( sprintf "inside check adjacency - checking horisontally. returning %A, it is %A \n" word (Dictionary.lookup word st.dict))
                if word.Length > 1 then Dictionary.lookup word st.dict else true
        aux pos currentBoard ""
    
    //horisontal adjacency check
    let checkAdjHorisontal (pos:coord) (currentBoard:Map<coord, pieces>) (st:State.state) =
        //debugPrint( sprintf "inside check adjacency - checking horisontally \n")
        let rec aux pos currentBoard =
            let posLeft = ((fst(pos)-1),snd(pos))
            let left = Map.tryFind posLeft currentBoard
            match left with 
            |Some _     -> aux posLeft currentBoard
            |None       -> getWholeWordHorisontal pos currentBoard st
        aux pos currentBoard

    //helper for vertical adjacency check
    let getWholeWordVertical (pos:coord) (currentBoard:Map<coord,pieces>) (st:State.state)=
        let rec aux (pos:coord) (currentBoard:Map<coord,pieces>) (word:string) =
            let current = fst(Map.find pos currentBoard)
            let word = word+current.ToString()
            let posBelow = (fst(pos), snd(pos)+1)
            let below = Map.tryFind posBelow currentBoard
            match below with 
            |Some _ -> aux posBelow currentBoard word
            |None   -> 
             //   debugPrint( sprintf "inside check adjacency vertical. returning %A, it is %A \n" word (Dictionary.lookup word st.dict))
                if word.Length > 1 then Dictionary.lookup word st.dict else true
        aux pos currentBoard ""
    
    //vertical adjacency check
    let checkAdjVertical (pos:coord) (currentBoard:Map<coord,pieces>) (st:State.state) =
        //debugPrint( sprintf "inside check adjacency - checking vertically \n") 
        let rec aux pos currentBoard =
            let posAbove = (fst(pos), snd(pos)-1)
            let above = Map.tryFind posAbove currentBoard
            match above with
            |Some _ -> aux posAbove currentBoard
            |None -> getWholeWordVertical pos currentBoard st
        aux pos currentBoard

    //combines vertical and horisontal check. Used below
    let adjancency (pos:coord) lst st : bool =
        //debugPrint( sprintf "inside adjaNcency. About to create new board \n") 
        let currentBoard = newBoard st lst
        (checkAdjVertical pos currentBoard st && checkAdjHorisontal pos currentBoard st)


    //wrapper for adjacency checks. checks adjacencies for every character bot attempts to insert
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

    //make new word from existing word on board
    let makeNewWord (wordOnBoard:string) (st:State.state): string option = 

        //method for getting the last dictionary in the existing word
        let rec aux (wordOnBoard:string) (counter:int) (dictionary:Dictionary.Dict) = 
           // debugPrint( sprintf "WORD ON BOARD IS %A \n" wordOnBoard)
            let initial = Dictionary.step wordOnBoard[counter] dictionary
         //   debugPrint( sprintf "now trying %A"  wordOnBoard[counter])
            match initial with
            |None ->
          //      debugPrint( sprintf "NOOOO") 
                st.dict
            |Some (bool,dict) -> 
                if counter <> (wordOnBoard.Length-1) then aux wordOnBoard (counter+1) dict
                else
                    dict
    
        let startingDict = aux wordOnBoard 0 st.dict

        //hand to list
        let hand = toList st.hand
        //function to remove tile from hand
        let rec delete lst x = 
                match lst with
                | head::tail when head = x -> tail
                | head::tail -> head :: (delete tail x)
                | []    -> []

        //function for making word
        let rec insertion c dict handlist (st:State.state) currentWord wordOnBoard= 
            let currentWord = currentWord+c.ToString()
            let next = Dictionary.step c dict
            match next with
            |None -> 
                if (currentWord.Length > 1)  then
                    let hand = toList st.hand
                    //debugPrint (sprintf "hand length is %A \n"hand.Length)
                    let skipp = currentWord.Length
                    //debugPrint (sprintf "Skipp is  %A \n" skipp)
                    let newHand = hand[skipp..]
                    //debugPrint (sprintf "new hand length is   %A \n" newHand.Length)
                    let newC = currentWord.[currentWord.Length-2]
                    let currentWord = currentWord.Remove( currentWord.Length - 2);
                    insertion newC dict newHand st currentWord wordOnBoard 
                else
            //        debugPrint(sprintf "this happened")
                    None
            |Some (isWord,dictionary) -> 
                    match isWord with
                    |true ->
                    //    debugPrint( sprintf "i did it! word is %A \n" wordOnBoard+currentWord)
                        Some (wordOnBoard+currentWord)
                    |false ->  
                        let newC = fst(((Map.find handlist[0] st.tiles).MinimumElement))
                        let newHandList = handlist[1..]
                        insertion newC dictionary newHandList st currentWord wordOnBoard
                        
        //iterate over hand, try to find a word with the dict from the existing word on board
        let words = [for tile in hand do
                        let newC = fst(((Map.find tile st.tiles).MinimumElement))
                        let handlist = delete hand tile
                        let currentWord = ""
                        insertion newC startingDict handlist st currentWord wordOnBoard]

        let rec helper words = 
                    match words with
                    |Some x::_ when Dictionary.lookup x st.dict = true  ->
                        Some x
                    |Some _::tail -> helper tail
                    |None::tail   -> helper tail
                    |[]           -> None
        //debugPrint(sprintf "final word is %A \n  " (helper words))

        helper words        
    

    //given a charcter, returns corresponding tile
    let getTileFromCharacter (c:char) (st:State.state) =
        let set = st.hand
        let lst = toList set
        let newL = List.map (fun x -> x,(Map.find x st.tiles))
        let tiles = newL lst
        //debugPrint (sprintf "printing the tile list from hand %A\n" tiles)

        //let mylst = List.fold (fun acc x -> List.map( Map.find x) empt lst 
        let rec helper lst (c:char) : tile * State.state =
            match lst with
            |((id:uint),(t:tile))::tail when
                fst(t.MinimumElement) = c ->
                    let (newHand : MultiSet<uint32>) =
                        MultiSet.removeSingle id st.hand
                    let st' =  {st with hand = newHand}
                    t,st'   
            |x::rest ->
                helper rest c
            |[]      ->
                let set = Set.empty.Add(' ', 0)
                set,st
                                                                        
        helper tiles c

    //get corresponding id to a tile
    let getIDbasedOnTile (t:tile) (st:State.state) = 
        let lst = st.tiles |> Map.toList
        let rec aux (t:tile) (lst:list<uint32 * tile>) = 
            match lst with 
            |(id,tl)::tail when
                tl = t  -> id 
            |head::tail -> aux t tail
            |[]         -> 0u 
        aux t lst

    
    // returns created word without option type
    let getNewWord makeNewWord = 
        match makeNewWord with
        |None ->  ""
        |Some word -> word

    //get tile for each character in a word
    let getTilesOfWord (word:string) (st:State.state) = 
        let charList = Seq.toList word
        let rec aux charList st =
            match charList with 
                | x::tail -> 
                    let tile = fst(getTileFromCharacter x st)
                    let st = snd(getTileFromCharacter x st)
                    let id = getIDbasedOnTile tile st
                    let character = fst(tile.MinimumElement)
                    let value = snd(tile.MinimumElement)
                    (id,(character,value))::aux tail st
                | [] -> []
        aux charList st

    //create list of intended insertion to pass. Same return type as insertWord
    let newInsert currentWordWithoutExistingTiles st coordinates = 
        let tiles = getTilesOfWord currentWordWithoutExistingTiles st
        let ourMove = Some (List.zip coordinates tiles)
        ourMove


    //check if created word can fit the board vertically
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


    //used to keep track of no. tiles placed so far
    let updateRemainingTiles lst = 
        let rec aux lst count =
            match lst with
            |h::t -> aux t (count+1)
            |[] -> count
        aux lst 0

    //get coords of word to insert (for wrapperHorisontal)
    let coordListHorisontal (word:string) (pos:coord) : (int*int) list =
        let wordList = Seq.toList word
        let rec aux wordList pos = 
            match wordList with
            |[]         -> []
            |head::tail -> 
                let newCoord = (fst(pos)+1,(snd(pos)))
                newCoord::aux tail newCoord

        aux wordList (fst(pos),snd(pos))
    
    //get coords for word to insert (for wrapperVertical)
    let coordListVertical (word:string)  (pos:coord): (int*int) list = 
        let wordList = Seq.toList word
        let rec aux wordList pos = 
            match wordList with
            |[]         -> []
            |head::tail -> 
                let newCoord = (fst(pos),(snd(pos)+1))
                newCoord::aux tail newCoord

        aux wordList (fst(pos),snd(pos))

    //helper for finding existing word on board
    let getWordAndEndPositionV (pos:coord) (st:State.state) = 
        let rec aux (pos:coord) (st:State.state) (word:string) =
            let current = fst(Map.find pos st.currentBoard)
            let word = word+current.ToString()
            let posBelow = (fst(pos), snd(pos)+1)
            let below = Map.tryFind posBelow st.currentBoard
            match below with
            |Some _ -> aux posBelow st word
            |None ->  (word,pos)         //if word.Length > 1 then (word,pos) else ("",pos)
        aux pos st ""
    
    //returns word on board and its final position
    let findExistingVerticalWord (pos:coord) (st:State.state) :(string*coord) =
        let rec aux pos (st:State.state) =
            let posAbove = (fst(pos), snd(pos)-1)
            let above = Map.tryFind posAbove st.currentBoard
            match above with 
            |Some _ -> aux posAbove st
            |None -> getWordAndEndPositionV pos st 
        aux pos st


    //wrapper for creating vertical words using existing word on board.
    let wrapperVertical (input) (st:State.state) =
        match input with 
        |Some x -> Some x
        |None ->
            //get all tiles on board
            let tiles = Map.toList st.currentBoard
            let rec aux tiles =
                //match on coords
                match tiles with
                |(coord,_)::tail ->
                    //starting pos
                    let pos = coord
                    //find vertical word already on board
                    let wordV = fst(findExistingVerticalWord pos st)
                    //if only one letter, returns "". otherwise, keep going with found word
                    match wordV with
                    |""  -> aux tail
                    |_   ->
                        //find the end of existing vertical word on board
                        let wordVEnd = snd(findExistingVerticalWord pos st)

                        //find beginning of new word 
                        let newWordStart = (fst(wordVEnd),snd(wordVEnd)+1)

                        //create new word based on found word
                        let current = makeNewWord wordV st
                  //      debugPrint( sprintf "NEW WORD IS %A \n" current)
                   //     debugPrint( sprintf "NEW WORD IS %A \n" current)
                    //    debugPrint( sprintf "NEW WORD IS %A \n" current)
                        //check if succesful
                        match current with
                        |None -> aux tail
                        |Some x ->
                            //check if there is room for the new characters on the board
                            let skip = wordV.Length
                            let wholeWorld = getNewWord current
                            let actual =  wholeWorld.[skip..]
                            let optionActual = Some actual
                            let gotRoom = canVertical optionActual wordVEnd st
                            match gotRoom with
                            |true ->
                                //check if adjacent tiles also form words
                                let coordinates = coordListVertical actual wordVEnd
                                let insert = newInsert actual st coordinates
                                let check  = adjacencyFinal newWordStart insert st
                                match check with
                                |true -> newInsert actual st coordinates
                                |false -> aux tail
                            |false -> aux tail
                |[] -> None
            aux tiles
    
    // gets the word on the board and its end position
    let getWordAndEndPositionH pos st =
        let rec aux (pos:coord) (st:State.state) (word:string) =
            let current = fst(Map.find pos st.currentBoard)
            let word = word+current.ToString()
            let posRight = (fst(pos)+1, snd(pos))
            let below = Map.tryFind posRight st.currentBoard
            match below with
            |Some _ -> aux posRight st word
            |None ->  (word,pos)         //if word.Length > 1 then (word,pos) else ("",pos)
        aux pos st "" 

    
    //find horisontal word on board
    let findExistingHorisontalWord pos st = 
        let rec aux pos (st:State.state) =
            let posLeft = (fst(pos)-1, snd(pos))
            let above = Map.tryFind posLeft st.currentBoard
            match above with 
            |Some _ -> aux posLeft st
            |None -> getWordAndEndPositionH pos st 
        aux pos st
    
    //wrapper for checking for insertion of horisontal words, based on existing word on board
    let wrapperHorisontal (input) (st:State.state) =
        match input with 
        |Some x -> Some x
        |None ->
            //get all tiles on board
            let tiles = Map.toList st.currentBoard
            let rec aux tiles =
                //match on coords
                match tiles with
                |(coord,_)::tail ->
                    //starting pos
                    let pos = coord
                    let wordH = fst(findExistingHorisontalWord pos st)
                    match wordH with
                    |""  -> aux tail
                    |_   -> 
                        let wordHEnd = snd(findExistingVerticalWord pos st)
                        let newWordStart = (fst(wordHEnd)+1,snd(wordHEnd))
                        let current = makeNewWord wordH st
                        match current with
                        |None -> aux tail
                        |Some x ->
                            let skip = wordH.Length
                            let wholeWorld = getNewWord current
                            let actual =  wholeWorld.[skip..]
                            let optionActual = Some actual
                            let gotRoom = canHorisontal optionActual wordHEnd st
                            match gotRoom with
                            |true ->
                                let coordinates = coordListHorisontal actual wordHEnd
                                let insert = newInsert actual st coordinates
                                let check  = adjacencyFinal newWordStart insert st
                                match check with
                                |true -> newInsert actual st coordinates
                                |false -> aux tail
                            |false -> aux tail
                |[] -> None
            aux tiles

    //make word with existing character on board
    let makeWord (c : char) (st : State.state) : string option =
        //debugPrint (sprintf "current hand is %A \n" st.hand)
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
                    if currentWord.Length > 2 then
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
                        if currentWord.Length > 2 then
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
  
    //check if you found a word
    let canMakeWord(c : char)  (st: State.state) =
       // debugPrint(sprintf "inside canMakeword. Testing with starting character: %A\n" c)
        match makeWord c st with
        | None -> false
        | Some x  -> 
           // debugPrint(sprintf "canMakeword is true! word is %A . Please note, this is just a check \n" x)
            true

    //just a getter for the word, without the option type
    let getWord (c:char) (st:State.state) =
        let word = makeWord c st
        match word with
            |None -> ""
            |Some w ->
               // debugPrint(sprintf "inside getWord. just returning the word: %A  for another function \n" w)
                w


    //given word, receive list of tiles to play
    let getTilesFromWord (c:char) (st:State.state) (fstWord:bool) =
        if fstWord then 
            let word = getWord c (st: State.state)
            let charList = Seq.toList word
            let rec aux charList st  =
                match charList with
                | x::tail -> 
                    let tile = fst(getTileFromCharacter x st)
                    let st = snd(getTileFromCharacter x st)
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
                    let tile = fst(getTileFromCharacter x st)
                    let st = snd(getTileFromCharacter x st)
                    let id = getIDbasedOnTile tile st
                    let character = fst(tile.MinimumElement)
                    let value = snd(tile.MinimumElement)
                    (id,(character,value))::aux tail st
                | [] -> []
            aux charList st

    //get coords for word to insert
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

    //special case insertion of first word       
    let insertFirstWord (c:char) (st:State.state) (pos:coord) =
        let tiles =  getTilesFromWord c st true
        let coords = coordList (getWord c (st: State.state)) true 'v' (0,0)
        //we just insert vertically
        let ourMove coords tiles =
            Some (List.zip coords tiles)

        ourMove coords tiles 

    //main method for insertion
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
               // debugPrint (sprintf "Inside insertWord - could not construct word. swap instead \n")
                None

            else
                //debugPrint (sprintf "Inside insertWord. I claim that board is NOT empty \n" )
                let tiles =  getTilesFromWord c st false
                if canVertical (makeWord c st) pos st then 
                    //debugPrint (sprintf "trying to insert vertically (insert word)\n" )
                    let coords = coordList (getWord c (st: State.state)) false 'v' pos
                //    debugPrint (sprintf "coords length: %A, tiles length: %A\n" coords.Length tiles.Length)
                    //debugPrint (sprintf "coords: %A\ntiles: %A\n" coords tiles)
                    //we just insert vertically 
                    let ourMove coords tiles =                  
                    //   debugPrint (sprintf "coords & tiles are %A" (Some (List.zip coords tiles)))
                        Some (List.zip coords tiles)
                    ourMove coords tiles
                else if canHorisontal(makeWord c st) pos st then 
                        //debugPrint (sprintf "trying to insert horisontally (insert word)\n" )
                        let coords = coordList (getWord c (st: State.state)) false 'h' pos
                        //we just insert vertically 
                        let ourMove coords tiles =
                      //      debugPrint (sprintf "coords & tiles are %A" (Some (List.zip coords tiles)))
                            Some (List.zip coords tiles)
                        ourMove coords tiles
                else 
                    None
    
    //initial check to determine when/if to call insertWord
    let FindMove (st : State.state) =
       // debugPrint(sprintf "inside findMove. Checking if board is empty \n")

        if st.currentBoard.IsEmpty then
            (' ', (0,0)), st
        else 
          //  debugPrint (sprintf "Board is not empty. will try to insert a word, using a single existing character on table \n" )
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
    //main query method. Swaps between prioritising vertical and horisontal insertions of words based on existing words on board
    let swapper (st:State.state) = 
        if st.flip % 2 = 0 then wrapperHorisontal (wrapperVertical(insertWord(FindMove(st))) st) st 
        else wrapperVertical(wrapperHorisontal(insertWord(FindMove(st))) st) st
            
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
                debugPrint( sprintf "Input move (format '(<x-coordinate> <y-coordinate> <piece id><character><point-value> )*', note the absence of space between the last inputs)\n\n" )
                //let input = System.Console.ReadLine()
                debugPrint (sprintf "before query\n")
               
                //let query = wrapperHorisontal (wrapperVertical(insertWord(FindMove(st))) st) st 
                let query = swapper st
                
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

                let updatedRemaining = updateRemainingTiles ms + st.remainingTiles
                debugPrint( sprintf "current number of tiles played total: %A /n " updatedRemaining)
                //updates hand & board & turn
                let st' = {st with hand = newHandSet; currentBoard = newBoard; playerTurn = nextTurn; remainingTiles=updatedRemaining; flip=incrementFlip st}
                debugPrint(sprintf "Map of current board: %A\n" st'.currentBoard)
                
                aux st'
            | RCM (CMPlayed (pid, ms, points)) ->
                (* Successful play by other player. Update your state *)
                //updates state to reflect new move
                let newBoard =
                    List.fold  (fun acc (c, k) ->
                        Map.add c (snd(k)) acc) st.currentBoard ms
                let updatedRemaining = updateRemainingTiles ms + st.remainingTiles
                debugPrint( sprintf "current number of tiles played total: %A /n" updatedRemaining)
                let st' = {st with currentBoard = newBoard; playerTurn = nextTurn; remainingTiles = updatedRemaining} 
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
                        //let st' = {st with playerTurn = nextTurn} 
                        aux st
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
    