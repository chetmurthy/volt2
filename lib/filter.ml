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


(* Definitions *)

type t = Event.t -> bool

let _, register, register_unnamed, get =
  Utils.make_container_functions ()


(* Trivial filters *)

let all _ = true

let none _ = false


(* Time filters *)

let before t = fun e -> e.Event.relative < t

let after t = fun e -> e.Event.relative > t


(* Level filters *)

let trace0_or_below e = e.Event.level <= Level.TRACE0
let trace1_or_below e = e.Event.level <= Level.TRACE1
let trace2_or_below e = e.Event.level <= Level.TRACE2
let trace3_or_below e = e.Event.level <= Level.TRACE3
let trace4_or_below e = e.Event.level <= Level.TRACE4
let trace5_or_below e = e.Event.level <= Level.TRACE5
let trace6_or_below e = e.Event.level <= Level.TRACE6
let trace7_or_below e = e.Event.level <= Level.TRACE7
let trace8_or_below e = e.Event.level <= Level.TRACE8
let trace9_or_below e = e.Event.level <= Level.TRACE9

let debug0_or_below e = e.Event.level <= Level.DEBUG0
let debug1_or_below e = e.Event.level <= Level.DEBUG1
let debug2_or_below e = e.Event.level <= Level.DEBUG2
let debug3_or_below e = e.Event.level <= Level.DEBUG3
let debug4_or_below e = e.Event.level <= Level.DEBUG4
let debug5_or_below e = e.Event.level <= Level.DEBUG5
let debug6_or_below e = e.Event.level <= Level.DEBUG6
let debug7_or_below e = e.Event.level <= Level.DEBUG7
let debug8_or_below e = e.Event.level <= Level.DEBUG8
let debug9_or_below e = e.Event.level <= Level.DEBUG9

let info0_or_below e = e.Event.level <= Level.INFO0
let info1_or_below e = e.Event.level <= Level.INFO1
let info2_or_below e = e.Event.level <= Level.INFO2
let info3_or_below e = e.Event.level <= Level.INFO3
let info4_or_below e = e.Event.level <= Level.INFO4
let info5_or_below e = e.Event.level <= Level.INFO5
let info6_or_below e = e.Event.level <= Level.INFO6
let info7_or_below e = e.Event.level <= Level.INFO7
let info8_or_below e = e.Event.level <= Level.INFO8
let info9_or_below e = e.Event.level <= Level.INFO9

let warn_or_below e = e.Event.level <= Level.WARN

let error_or_below e = e.Event.level <= Level.ERROR

let fatal_or_below e = e.Event.level <= Level.FATAL

let trace0_or_above e = e.Event.level >= Level.TRACE0
let trace1_or_above e = e.Event.level >= Level.TRACE1
let trace2_or_above e = e.Event.level >= Level.TRACE2
let trace3_or_above e = e.Event.level >= Level.TRACE3
let trace4_or_above e = e.Event.level >= Level.TRACE4
let trace5_or_above e = e.Event.level >= Level.TRACE5
let trace6_or_above e = e.Event.level >= Level.TRACE6
let trace7_or_above e = e.Event.level >= Level.TRACE7
let trace8_or_above e = e.Event.level >= Level.TRACE8
let trace9_or_above e = e.Event.level >= Level.TRACE9

let debug0_or_above e = e.Event.level >= Level.DEBUG0
let debug1_or_above e = e.Event.level >= Level.DEBUG1
let debug2_or_above e = e.Event.level >= Level.DEBUG2
let debug3_or_above e = e.Event.level >= Level.DEBUG3
let debug4_or_above e = e.Event.level >= Level.DEBUG4
let debug5_or_above e = e.Event.level >= Level.DEBUG5
let debug6_or_above e = e.Event.level >= Level.DEBUG6
let debug7_or_above e = e.Event.level >= Level.DEBUG7
let debug8_or_above e = e.Event.level >= Level.DEBUG8
let debug9_or_above e = e.Event.level >= Level.DEBUG9

let info0_or_above e = e.Event.level >= Level.INFO0
let info1_or_above e = e.Event.level >= Level.INFO1
let info2_or_above e = e.Event.level >= Level.INFO2
let info3_or_above e = e.Event.level >= Level.INFO3
let info4_or_above e = e.Event.level >= Level.INFO4
let info5_or_above e = e.Event.level >= Level.INFO5
let info6_or_above e = e.Event.level >= Level.INFO6
let info7_or_above e = e.Event.level >= Level.INFO7
let info8_or_above e = e.Event.level >= Level.INFO8
let info9_or_above e = e.Event.level >= Level.INFO9

let warn_or_above e = e.Event.level >= Level.WARN

let error_or_above e = e.Event.level >= Level.ERROR

let fatal_or_above e = e.Event.level >= Level.FATAL

let level_below l e = e.Event.level < l

let level_above l e = e.Event.level > l

let level_equal l e = e.Event.level = l


(* Logger filters *)

let logger_equal f = fun e -> e.Event.logger = f

let logger_not_equal f = fun e -> e.Event.logger <> f


(** {6 File filters} *)

let file_defined e = e.Event.file <> "" && e.Event.file <> "<nofile>"

let file_undefined e = e.Event.file = "" || e.Event.file = "<nofile>"

let file_equal f = fun e -> e.Event.file = f

let file_not_equal f = fun e -> e.Event.file = f


(* Position filters *)

let line_defined e = e.Event.line > 0

let line_undefined e = e.Event.line <= 0

let column_defined e = e.Event.column > 0

let column_undefined e = e.Event.column <= 0


(* Message filters *)

let message_defined e = e.Event.message <> ""

let message_undefined e = e.Event.message = ""

let message_paje e = e.Event.message = !Utils.paje_t

let message_not_paje e = e.Event.message <> !Utils.paje_t

let message_daikon e = e.Event.message = !Utils.daikon_t

let message_not_daikon e = e.Event.message <> !Utils.daikon_t


(* Property filters *)

let properties_empty e = e.Event.properties = []

let properties_not_empty e = e.Event.properties <> []

let property_defined k = fun e -> List.mem_assoc k e.Event.properties

let property_undefined k = fun e -> not (List.mem_assoc k e.Event.properties)

let property_equal k v = fun e -> try (List.assoc k e.Event.properties) = v with Not_found -> false

let property_not_equal k v = fun e -> try (List.assoc k e.Event.properties) <> v with Not_found -> true

let property_equal_pred k p = fun e -> try p (List.assoc k e.Event.properties) with Not_found -> false

let property_not_equal_pred k p = fun e -> try not (p (List.assoc k e.Event.properties)) with Not_found -> true


(* Exception filters *)

let exception_some e = match e.Event.error with Some _ -> true | None -> false

let exception_none e = match e.Event.error with Some _ -> false | None -> true


(* Combinators over filters *)

let logand f1 f2 = fun e -> (f1 e) && (f2 e)

let (&&&) = logand

let logor f1 f2 = fun e -> (f1 e) || (f2 e)

let (|||) = logor

let logxor f1 f2 = fun e -> let r1 = f1 e and r2 = f2 e in (r1 && (not r2)) || ((not r1) && r2)

let (^^^) = logxor

let not f = fun e -> not (f e)

let for_all l =
  fun e ->
    List.for_all (fun f -> f e) l

let (!&&&) = for_all

let exists l =
  fun e ->
    List.exists (fun f -> f e) l

let (!|||) = exists


let () =
  List.iter
    (fun (x, y) -> register x y)
    [ "all",                  all ;
      "none",                 none ;
      "trace_or_below",       trace0_or_below ;
      "trace1_or_below",       trace1_or_below ;
      "trace2_or_below",       trace2_or_below ;
      "trace3_or_below",       trace3_or_below ;
      "trace4_or_below",       trace4_or_below ;
      "trace5_or_below",       trace5_or_below ;
      "trace6_or_below",       trace6_or_below ;
      "trace7_or_below",       trace7_or_below ;
      "trace8_or_below",       trace8_or_below ;
      "trace9_or_below",       trace9_or_below ;
      "debug_or_below",       debug0_or_below ;
      "debug1_or_below",       debug1_or_below ;
      "debug2_or_below",       debug2_or_below ;
      "debug3_or_below",       debug3_or_below ;
      "debug4_or_below",       debug4_or_below ;
      "debug5_or_below",       debug5_or_below ;
      "debug6_or_below",       debug6_or_below ;
      "debug7_or_below",       debug7_or_below ;
      "debug8_or_below",       debug8_or_below ;
      "debug9_or_below",       debug9_or_below ;
      "info_or_below",       info0_or_below ;
      "info1_or_below",       info1_or_below ;
      "info2_or_below",       info2_or_below ;
      "info3_or_below",       info3_or_below ;
      "info4_or_below",       info4_or_below ;
      "info5_or_below",       info5_or_below ;
      "info6_or_below",       info6_or_below ;
      "info7_or_below",       info7_or_below ;
      "info8_or_below",       info8_or_below ;
      "info9_or_below",       info9_or_below ;
      "warn_or_below",        warn_or_below ;
      "error_or_below",       error_or_below ;
      "fatal_or_below",       fatal_or_below ;
      "trace_or_above",       trace0_or_above ;
      "trace1_or_above",       trace1_or_above ;
      "trace2_or_above",       trace2_or_above ;
      "trace3_or_above",       trace3_or_above ;
      "trace4_or_above",       trace4_or_above ;
      "trace5_or_above",       trace5_or_above ;
      "trace6_or_above",       trace6_or_above ;
      "trace7_or_above",       trace7_or_above ;
      "trace8_or_above",       trace8_or_above ;
      "trace9_or_above",       trace9_or_above ;
      "debug_or_above",       debug0_or_above ;
      "debug1_or_above",       debug1_or_above ;
      "debug2_or_above",       debug2_or_above ;
      "debug3_or_above",       debug3_or_above ;
      "debug4_or_above",       debug4_or_above ;
      "debug5_or_above",       debug5_or_above ;
      "debug6_or_above",       debug6_or_above ;
      "debug7_or_above",       debug7_or_above ;
      "debug8_or_above",       debug8_or_above ;
      "debug9_or_above",       debug9_or_above ;
      "info_or_above",       info0_or_above ;
      "info1_or_above",       info1_or_above ;
      "info2_or_above",       info2_or_above ;
      "info3_or_above",       info3_or_above ;
      "info4_or_above",       info4_or_above ;
      "info5_or_above",       info5_or_above ;
      "info6_or_above",       info6_or_above ;
      "info7_or_above",       info7_or_above ;
      "info8_or_above",       info8_or_above ;
      "info9_or_above",       info9_or_above ;
      "warn_or_above",        warn_or_above ;
      "error_or_above",       error_or_above ;
      "fatal_or_above",       fatal_or_above ;
      "file_defined",         file_defined ;
      "file_undefined",       file_undefined ;
      "line_defined",         line_defined ;
      "line_undefined",       line_undefined ;
      "column_defined",       column_defined ;
      "column_undefined",     column_undefined ;
      "message_defined",      message_defined ;
      "message_undefined",    message_undefined ;
      "message_paje",         message_not_daikon ;
      "message_not_paje",     message_not_daikon ;
      "message_daikon",       message_not_daikon ;
      "message_not_daikon",   message_not_daikon ;
      "properties_empty",     properties_empty ;
      "properties_not_empty", properties_not_empty ;
      "exception_some",       exception_some ;
      "exception_none",       exception_none ]
