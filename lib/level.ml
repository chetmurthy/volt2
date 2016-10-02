(*
 * This file is part of Bolt.
 * Copyright (C) 2009-2012 Xavier Clerc.
 *
 * Bolt is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation; either version 3 of the License, or
 * (at your option) any later version.
 *
 * Bolt is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *)


type error =
  | Invalid_level_string of string
  | Invalid_level_int of int

let string_of_error = function
  | Invalid_level_string s -> Printf.sprintf "invalid level string %S" s
  | Invalid_level_int i -> Printf.sprintf "invalid level int \"%d\"" i

exception Exception of error

let () =
  Printexc.register_printer
    (function
      | Exception error -> Some (string_of_error error)
      | _ -> None)

let fail error =
  raise (Exception error)

type t =
  | FATAL
  | ERROR
  | WARN
  | INFO0
  | INFO1
  | INFO2
  | INFO3
  | INFO4
  | INFO5
  | INFO6
  | INFO7
  | INFO8
  | INFO9
  | DEBUG0
  | DEBUG1
  | DEBUG2
  | DEBUG3
  | DEBUG4
  | DEBUG5
  | DEBUG6
  | DEBUG7
  | DEBUG8
  | DEBUG9
  | TRACE0
  | TRACE1
  | TRACE2
  | TRACE3
  | TRACE4
  | TRACE5
  | TRACE6
  | TRACE7
  | TRACE8
  | TRACE9

let levels = [
  FATAL ;
  ERROR ;
  WARN ;
  INFO0 ;
  INFO1 ;
  INFO2 ;
  INFO3 ;
  INFO4 ;
  INFO5 ;
  INFO6 ;
  INFO7 ;
  INFO8 ;
  INFO9 ;
  DEBUG0 ;
  DEBUG1 ;
  DEBUG2 ;
  DEBUG3 ;
  DEBUG4 ;
  DEBUG5 ;
  DEBUG6 ;
  DEBUG7 ;
  DEBUG8 ;
  DEBUG9 ;
  TRACE0 ;
  TRACE1 ;
  TRACE2 ;
  TRACE3 ;
  TRACE4 ;
  TRACE5 ;
  TRACE6 ;
  TRACE7 ;
  TRACE8 ;
  TRACE9 ;
]

let to_string = function
  | FATAL -> "FATAL"
  | ERROR -> "ERROR"
  | WARN -> "WARN"
  | INFO0 -> "INFO"
  | INFO1 -> "INFO1"
  | INFO2 -> "INFO2"
  | INFO3 -> "INFO3"
  | INFO4 -> "INFO4"
  | INFO5 -> "INFO5"
  | INFO6 -> "INFO6"
  | INFO7 -> "INFO7"
  | INFO8 -> "INFO8"
  | INFO9 -> "INFO9"
  | DEBUG0 -> "DEBUG"
  | DEBUG1 -> "DEBUG1"
  | DEBUG2 -> "DEBUG2"
  | DEBUG3 -> "DEBUG3"
  | DEBUG4 -> "DEBUG4"
  | DEBUG5 -> "DEBUG5"
  | DEBUG6 -> "DEBUG6"
  | DEBUG7 -> "DEBUG7"
  | DEBUG8 -> "DEBUG8"
  | DEBUG9 -> "DEBUG9"
  | TRACE0 -> "TRACE"
  | TRACE1 -> "TRACE1"
  | TRACE2 -> "TRACE2"
  | TRACE3 -> "TRACE3"
  | TRACE4 -> "TRACE4"
  | TRACE5 -> "TRACE5"
  | TRACE6 -> "TRACE6"
  | TRACE7 -> "TRACE7"
  | TRACE8 -> "TRACE8"
  | TRACE9 -> "TRACE9"

let of_string x =
  match String.uppercase x with
  | "FATAL" -> FATAL
  | "ERROR" -> ERROR
  | "WARN" -> WARN
  | "INFO" -> INFO0
  | "INFO1" -> INFO1
  | "INFO2" -> INFO2
  | "INFO3" -> INFO3
  | "INFO4" -> INFO4
  | "INFO5" -> INFO5
  | "INFO6" -> INFO6
  | "INFO7" -> INFO7
  | "INFO8" -> INFO8
  | "INFO9" -> INFO9
  | "DEBUG" -> DEBUG0
  | "DEBUG1" -> DEBUG1
  | "DEBUG2" -> DEBUG2
  | "DEBUG3" -> DEBUG3
  | "DEBUG4" -> DEBUG4
  | "DEBUG5" -> DEBUG5
  | "DEBUG6" -> DEBUG6
  | "DEBUG7" -> DEBUG7
  | "DEBUG8" -> DEBUG8
  | "DEBUG9" -> DEBUG9
  | "TRACE" -> TRACE0
  | "TRACE1" -> TRACE1
  | "TRACE2" -> TRACE2
  | "TRACE3" -> TRACE3
  | "TRACE4" -> TRACE4
  | "TRACE5" -> TRACE5
  | "TRACE6" -> TRACE6
  | "TRACE7" -> TRACE7
  | "TRACE8" -> TRACE8
  | "TRACE9" -> TRACE9
  | _ -> fail (Invalid_level_string x)

let to_int = function
  | FATAL -> 0
  | ERROR -> 1
  | WARN -> 2
  | INFO0 -> 3
  | INFO1 -> 4
  | INFO2 -> 5
  | INFO3 -> 6
  | INFO4 -> 7
  | INFO5 -> 8
  | INFO6 -> 9
  | INFO7 -> 10
  | INFO8 -> 11
  | INFO9 -> 12
  | DEBUG0 -> 13
  | DEBUG1 -> 14
  | DEBUG2 -> 15
  | DEBUG3 -> 16
  | DEBUG4 -> 17
  | DEBUG5 -> 18
  | DEBUG6 -> 19
  | DEBUG7 -> 20
  | DEBUG8 -> 21
  | DEBUG9 -> 22
  | TRACE0 -> 23
  | TRACE1 -> 24
  | TRACE2 -> 25
  | TRACE3 -> 26
  | TRACE4 -> 27
  | TRACE5 -> 28
  | TRACE6 -> 29
  | TRACE7 -> 30
  | TRACE8 -> 31
  | TRACE9 -> 32

let of_int = function
  | 0 -> FATAL
  | 1 -> ERROR
  | 2 -> WARN
  | 3 -> INFO0
  | 4 -> INFO1
  | 5 -> INFO2
  | 6 -> INFO3
  | 7 -> INFO4
  | 8 -> INFO5
  | 9 -> INFO6
  | 10 -> INFO7
  | 11 -> INFO8
  | 12 -> INFO9
  | 13 -> DEBUG0
  | 14 -> DEBUG1
  | 15 -> DEBUG2
  | 16 -> DEBUG3
  | 17 -> DEBUG4
  | 18 -> DEBUG5
  | 19 -> DEBUG6
  | 20 -> DEBUG7
  | 21 -> DEBUG8
  | 22 -> DEBUG9
  | 23 -> TRACE0
  | 24 -> TRACE1
  | 25 -> TRACE2
  | 26 -> TRACE3
  | 27 -> TRACE4
  | 28 -> TRACE5
  | 29 -> TRACE6
  | 30 -> TRACE7
  | 31 -> TRACE8
  | 32 -> TRACE9
  | x -> fail (Invalid_level_int x)

let info n = of_int ((to_int INFO0) + n)
let debug n = of_int ((to_int DEBUG0) + n)
let trace n = of_int ((to_int TRACE0) + n)
  
let unsafe_to_int (l : t) = (Obj.obj (Obj.repr l) : int)
