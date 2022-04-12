module internal Parser

    open Eval
    open ScrabbleUtil
    open StateMonad

    (*

    The interfaces for JParsec and FParsecLight are identical and the implementations should always produce the same output
    for successful parses although running times and error messages will differ. Please report any inconsistencies.

    *)

    open FParsecLight.TextParser             // Example parser combinator library. Use for CodeJudge.
    // open FParsecLight.TextParser     // Industrial parser-combinator library. Use for Scrabble Project.
    
    let pIntToChar  = pstring "IntToChar"
    let pPointValue = pstring "PointValue"

    let pCharToInt  = pstring "charToInt"
    let pToUpper    = pstring "toUpper"
    let pToLower    = pstring "toLower"
    let pCharValue  = pstring "charValue"

    let pTrue       = pstring "true"
    let pFalse      = pstring "false"
    let pIsDigit    = pstring "isDigit"
    let pIsLetter   = pstring "isLetter"
    let pIsVowel   = pstring "isVowel"

    let pif       = pstring "if"
    let pthen     = pstring "then"
    let pelse     = pstring "else"
    let pwhile    = pstring "while"
    let pdo       = pstring "do"
    let pdeclare  = pstring "declare"

    let whitespaceChar = satisfy System.Char.IsWhiteSpace <?> "whitespace"
    let pletter        = satisfy System.Char.IsLetter <?> "letter"
    let palphanumeric  = satisfy System.Char.IsLetterOrDigit <?> "alphanumeric"

    let spaces         = many whitespaceChar <?> "space"
    let spaces1        = many1 whitespaceChar <?> "space1"

    let (.>*>.) p1 p2 = p1 .>> spaces .>>. p2
    let (.>*>) p1 p2  = p1 .>> (spaces .>> p2)
    let (>*>.) p1 p2  = p1 .>> spaces >>. p2

    let parenthesise p = pstring "(" >*>. p .>*> pstring ")"

    let pid =
        let rec aux p =
            match p with
            | [] -> ""
            | h::t -> string(h) + aux(t)
        (pletter <|> pchar '_') .>>. many (palphanumeric <|> pchar '_') |>> fun(a,b) -> string(a)+aux(b)

    
    let unop op a = op >*>. a
    let binop op a b = a .>*> op .>*>. b 

    let TermParse, tref = createParserForwardedToRef<aExp>()
    let ProdParse, pref = createParserForwardedToRef<aExp>()
    let AtomParse, aref = createParserForwardedToRef<aExp>()

    let AddParse = binop (pchar '+') ProdParse TermParse |>> Add <?> "Add"
    let SubParse = binop (pchar '-') ProdParse TermParse |>> Sub <?> "Sub"
    do tref := choice [AddParse; SubParse; ProdParse]

    let MulParse = binop (pchar '*') AtomParse ProdParse |>> Mul <?> "Mul"
    let DivParse = binop (pchar '/') AtomParse ProdParse |>> Div <?> "Div"
    let ModParse = binop (pchar '%') AtomParse ProdParse |>> Mod <?> "Mod"
    do pref := choice [MulParse; DivParse; ModParse; AtomParse]

    let NParse   = pint32 |>> N <?> "Int"
    let ParParse = parenthesise TermParse
    let NegParse = unop (pchar '-') AtomParse |>> (fun f -> Mul (N(-1), f)) <?> "Neg"
    let PVParse = unop (pstring "pointValue") ParParse |>> PV <?> "PointValue"
    let VarParse = pid |>> V <?> "Var"
    
    let AexpParse = TermParse

    let CexpParse, cref = createParserForwardedToRef<cExp>()
    let CharParse = pchar ''' >>. (whitespaceChar <|> palphanumeric) .>> pchar ''' |>> C <?> "Char"
    let CVParse = unop (pstring "charValue") ParParse |>> CV <?> "CharValue" 
    let ToUpperParse = unop (pstring "toUpper") (parenthesise CexpParse) |>> ToUpper <?> "ToUpper"
    let ToLowerParse = unop (pstring "toLower") (parenthesise CexpParse) |>> ToLower <?> "ToLower"
    let IntToCharParse = unop (pstring "intToChar") ParParse |>> IntToChar <?> "IntToChar"
    let CharToIntParse = unop (pstring "charToInt") (parenthesise CexpParse) |>> CharToInt <?> "CharToInt"
    
    do aref := choice [CharToIntParse; NegParse; NParse; ParParse; PVParse; VarParse]
    do cref := choice [CVParse; IntToCharParse; ToLowerParse; ToUpperParse; CharParse]
    
    let BexpParse = pstring "not implemented"

    let stmntParse = pstring "not implemented"

(* These five types will move out of this file once you start working on the project *)
    (*type coord      = int * int
    type squareProg = Map<int, string>
    type boardProg  = {
            prog       : string;
            squares    : Map<int, squareProg>
            usedSquare : int
            center     : coord
    
            isInfinite : bool   // For pretty-printing purposes only
            ppSquare   : string // For pretty-printing purposes only
        }*)

    type word   = (char * int) list
    type squareFun = Eval.squareFun
    type square = Map<int, squareFun>

    let parseSquareProg _ = failwith "not implemented"

    let parseBoardProg _ = failwith "not implemented"

    type boardFun2 = coord -> StateMonad.Result<square option, StateMonad.Error>
    type board = {
        center        : coord
        defaultSquare : square
        squares       : boardFun2
    }

    let mkBoard : boardProg -> board = fun _ -> {center = (0,0); defaultSquare = Map.empty; squares = fun _ -> Success (Some Map.empty)}
