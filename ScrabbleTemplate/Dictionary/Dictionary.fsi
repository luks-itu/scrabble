module Dictionary

type Dictionary

    val empty : unit -> Dictionary
    val insert : s: string -> dict: Dictionary -> Dictionary
    val lookup : s: string -> dict: Dictionary -> bool
    val step : c: char -> dict: Dictionary -> (bool * Dictionary) option