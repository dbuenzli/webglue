(*---------------------------------------------------------------------------
   Copyright 2011 Daniel C. Bünzli. All rights reserved.
   Distributed under a BSD license, see license at the end of the file.
   %%NAME%% release %%VERSION%%
  ---------------------------------------------------------------------------*)

(** S-expression IO from {!Ucutf} IO abstractions.

    [Seio] inputs S-expressions parsed according to {{:#def}this
    definition} from {!Ucutf} input abstractions.

    {e Version %%VERSION%% - %%EMAIL%% } *)

(** {1 S-expression type} *)

type 'a t = [ `Atom of string | `List of 'a t list ] * 'a
(** The type for annotated S-expressions. Strings are UTF-8 encoded. *)

(** {1:input Input} *)

type pos = int * int
(** The type for input positions. *)

type range = pos * pos
(** The type for input ranges. *)

type error = [
  | `Malformed_char
  | `Illegal_escape of int
  | `Illegal_char of int
  | `Mismatched_par
  | `Unclosed_quote
  | `Unclosed_par ]
(** The type for input errors. *)

val error_message : error -> string
(** [error_message e] is an english error message for [e]. *)

val input : ?err:(error -> pos -> unit) -> (range -> 'a) ->
  Ucutf.input -> 'a t list
(** [input err annot i] reads a list of S-expressions from [i] (the
    [<input>] production of the {{:#grammar}grammar}). The empty list
    [[]] is returned if no sexp was found.

    The [annot] function is used to tag S-expressions whenever they
    are constructed, it is given the range of characters spanned by
    the expression.

    On errors [err] is called (default does nothing) and parsing continues. *)


(** {1:input Output} *)

type dest = [
  | `Channel of out_channel | `Buffer of Buffer.t | `Fun of (int -> unit) ]
    (** The type for output destinations. For [`Buffer], the buffer won't
	be cleared. For [`Fun] the function is called with the output {e
	bytes} as [int]s. *)

type output
(** The type for output abstractions. *)

val make_output : dest -> output
(** [make_output dest] is an output abstraction writing to [dest]. *)

val output : output -> 'a t -> unit
(** [output o e] output [e] on [o]. Atoms are separated by a single
    white space character. *)

val print : Format.formatter -> 'a t -> unit
(** [print ppf e] pretty prints [e] on [ppf]. *)

val to_string : 'a t -> string
(** [to_string e] is like {!print}.

    {b Warning.} Not thread safe. Use {!print} for thread safety. *)

(** {1:full Full representation}

The full representation allows to keep comments and white space. It's usefull
to write filters that preserve the user's input. *)

(** S-expressions with comments and white space. *)
module Full : sig
  type 'a se = 'a t

  type 'a t = [
    | `Atom of string | `List of 'a t list | `White of string
    | `Comment of string ] * 'a
(** The type for annotated S-expressions with whitespace and
    comments. Strings are UTF-8 encoded. *)

  val input : ?err:(error -> pos -> unit) -> (range -> 'a) ->
    Ucutf.input -> 'a t list
(** [input_full tag i] is like {!Seio.input} but also includes whitespace and
    comments. *)

  val output : output -> 'a t -> unit
  (** [output o e] output [e] on [o]. A single white space character
      is introduced to separate two atoms iff there's no [`Comment] or
      [`White] to separate them. *)

  val print : Format.formatter -> 'a t -> unit
  (** [print ppf e] pretty prints [e] on [ppf]. *)

  val to_string : 'a t -> string
  (** [to_string e] is like {!print}.

      {b Warning.} Not thread safe. Use {!print} for thread safety. *)
end

(** {1:def S-expressions}

    An {e S-expression} is either an {e atom} or a {e list} of S-expressions.

    {2:atom Atoms}

    Atoms can be represented by tokens or quoted tokens, for example
    [abc] or ["abc"]. A token is any sequence of unicode characters
    except whitespace, ['"'], ['('], [')'], [';'] and ['\'].

    Given a token, its quoted form represents the same
    atom. However quoted tokens can represent atoms with characters
    that are forbidden in tokens : ["(hey; \" ho)"].

    In quoted tokens the sequences ["\""], ["\\"] and ["\n"] are
    respectively interpreted as a double quote character (['"'],
    [U+0022]), a backslash character (['\'], [U+005D]), or a line feed
    character (['\n'], [U+000A]). Any other character following a
    ['\'] is an illegal sequence of characters.

    {2:list Lists}

    Lists are delimited by left and right parentheses and their
    elements are S-expressions separated by whitespace, for example
    [(hop (hip hup) hap)]. The sequence [()] is the empty list.

    {2:comment Comments}

    Outside quoted tokens anything that follows a semi-colon ([';'],
    [U+003B]) is ignored until the next line feed character (['\n'],
    [U+000A]).

    {2:grammar The S-expression grammar}

    The grammar parsed by [Seio] on a stream of unicode
    characters is defined by:
{v
     <input> ::= <sexp>*
      <sexp> ::= <white> | <comment> | <token> | <qtoken> | <list>
      <list> ::= <LPAR> <sexp>* <RPAR>
     <token> ::= [^<tokenend>]+
    <qtoken> ::= <QUOT> <qseq>* <QUOT>
      <qseq> ::= [^<QUOT><BSLASH>] | <BSLASH> (<QUOT> | <BSLASH> | <N>)
     <white> ::= <whitechar>*
   <comment> ::= <SEMI> [^<LF>]* <LF>
  <tokenend> ::= <whitechar> | <QUOT> | <LPAR> | <RPAR> | <SEMI> | <BSLASH>
 <whitechar> ::= <SP> | <HT> | <CR> | <LF>
    <BSLASH> ::= U+005C  '\'
        <CR> ::= U+000D  '\r'
        <HT> ::= U+0009  '\t'
        <LF> ::= U+000A  '\n'
      <LPAR> ::= U+0028  '('
         <N> ::= U+004E  'n'
      <QUOT> ::= U+0022  '"'
      <RPAR> ::= U+0028  ')'
      <SEMI> ::= U+003B  ';'
        <SP> ::= U+0020  ' '
v}
*)

(*---------------------------------------------------------------------------
   Copyright 2011 Daniel C. Bünzli.
   All rights reserved.

   Redistribution and use in source and binary forms, with or without
   modification, are permitted provided that the following conditions
   are met:

   1. Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.

   2. Redistributions in binary form must reproduce the above
      copyright notice, this list of conditions and the following
      disclaimer in the documentation and/or other materials provided
      with the distribution.

   3. Neither the name of the Daniel C. Bünzli nor the names of
      contributors may be used to endorse or promote products derived
      from this software without specific prior written permission.

   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
   "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
   LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
   A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
   OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
   SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
   LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
   DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
   THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
   (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
   OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
  ---------------------------------------------------------------------------*)
